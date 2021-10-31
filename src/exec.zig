const std = @import("std");
const os = std.os;
const builtins = @import("builtins.zig").builtins;
const symtab = @import("symtab.zig");
const ast = @import("ast.zig");
const Node = ast.Node;
const AndOrCmdList = Node.AndOrCmdList;
const AndOrCmdListKind = AndOrCmdList.AndOrCmdListKind;
const Command = Node.Command;
const Word = ast.Word;
const Position = ast.Position;
const Range = ast.Range;
const IORedir = ast.IORedir;
const printError = std.debug.print;

const BoundedArray = std.BoundedArray([]const u8, 60);

pub fn simpleCommand(simple_command: *Command.SimpleCommand) !u8 {
    if (simple_command.name) |word_name| {
        var argv: BoundedArray = undefined;
        if (simple_command.args) |args| {
            argv = try BoundedArray.init(args.len + 1);
            for (args) |arg, i| {
                if (arg.cast(.STRING)) |a| {
                    argv.set(i + 1, a.str); // TODO consider others wordkinds
                }
            }
        } else {
            argv = try BoundedArray.init(1);
        }
        argv.set(0, word_name.cast(Word.WordKind.STRING).?.str);
        return try runProcess(argv.slice(), simple_command.io_redirs);
    }
    unreachable;
}

fn runProcess(argv: [][]const u8, io_redirs: ?[]*IORedir) !u8 {
    if (builtins.get(argv[0])) |builtin| {
        // TODO support redir on builtins
        return builtin(argv);
    }
    // TODO analize a way to not allocate too much memory

    // var buffer: [4096]u8 = undefined;
    // var fba = std.heap.FixedBufferAllocator.init(&buffer);
    // const allocator = &fba.allocator;
    var gpa = std.heap.GeneralPurposeAllocator(.{ .enable_memory_limit = true }){};
    gpa.setRequestedMemoryLimit(5000);

    const allocator = &gpa.allocator;
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.debug.print("Memory leaked.\n", .{});
    }

    var argvZ = try std.ArrayList(?[*:0]const u8).initCapacity(allocator, argv.len + 1);
    defer {
        for (argvZ.items) |arg| {
            if (arg) |value| allocator.destroy(value);
        }
        argvZ.deinit();
    }

    for (argv) |arg| {
        argvZ.appendAssumeCapacity(try std.mem.dupeZ(allocator, u8, arg));
    }

    const pid = std.os.fork() catch |err| {
        switch (err) {
            error.SystemResources => printError("kzh: could not fork, system resources unavaliable\n", .{}),
            else => printError("kzh: {}", .{err}),
        }
        return 1;
    };
    if (pid == 0) {
        // TODO put it on other place
        if (io_redirs) |io_redirections| {
            for (io_redirections) |io_redir| {
                var source_fd: os.fd_t = undefined;
                const dest_fd = processRedirection(io_redir, &source_fd) catch |err| {
                    switch (err) {
                        error.AccessDenied => printError("kzh: cannot create {s}: Permission denied\n", .{io_redir.name.cast(.STRING).?.str}), // TODO word string function
                        else => printError("kzh: {}", .{err}),
                    }
                    os.exit(1);
                };
                if (source_fd == dest_fd) continue;
                if (dest_fd == -1) {
                    printError("something wrong happened, better handling in the futureTM\n", .{});
                }
                os.dup2(dest_fd, source_fd) catch |err| {
                    printError("something wrong happened dup2, {}\n", .{err});
                };
            }
        }
        const args = try argvZ.toOwnedSliceSentinel(null);
        defer allocator.free(args);

        const envp = try symtab.global_symtab.dupeZ(allocator);
        defer allocator.free(envp);

        switch (std.os.execvpeZ(args[0].?, args, envp)) {
            error.FileNotFound => printError("kzh: {s}: not found\n", .{argv[0]}),
            error.AccessDenied => printError("kzh: {s}: cannot execute - Permission denied\n", .{argv[0]}),
            else => |err| printError("some problem happened: {}\n", .{err}),
        }
        os.exit(1); // if got here, then some problem happened, TODO proper handling
    } else {
        _ = std.os.waitpid(pid, 0);
        // printError("\nreturned process: {}\n", .{ret});
        return 0; // TODO figure out how to handle error of execution
    }
    return 1;
}

fn processRedirection(io_redir: *IORedir, source_fd: *os.fd_t) !os.fd_t {
    const filename = io_redir.name.cast(.STRING).?.str; // support other word types

    var dest_fd: os.fd_t = switch (io_redir.op) {
        .IO_LESS => try os.open(filename, os.O.CLOEXEC | os.O.RDONLY, 0),
        // .IO_DOUBLE_LESS, .IO_DOUBLE_LESS_DASH => createHereDocumentFd TODO
        .IO_GREAT, .IO_CLOBBER => try os.open(filename, os.O.WRONLY | os.system.O.CREAT | os.O.TRUNC, 0o644),
        .IO_DOUBLE_GREAT => try os.open(filename, os.O.WRONLY | os.O.CREAT | os.O.APPEND, 0o644),
        .IO_LESS_AND, .IO_GREAT_AND => std.fmt.parseInt(os.fd_t, filename, 10) catch -1,
        else => -1,
    };
    if (io_redir.io_num) |io_number| {
        source_fd.* = io_number;
    } else {
        switch (io_redir.op) {
            .IO_LESS, .IO_LESS_AND, .IO_DOUBLE_LESS, .IO_DOUBLE_LESS_DASH => source_fd.* = os.STDIN_FILENO,
            .IO_LESS_GREAT, .IO_GREAT, .IO_DOUBLE_GREAT, .IO_GREAT_AND, .IO_CLOBBER => source_fd.* = os.STDOUT_FILENO,
        }
    }
    return dest_fd;
}

test "Exec Simple Command" {
    // TODO
}
