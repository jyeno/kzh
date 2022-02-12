const std = @import("std");
const os = std.os;
const builtins = @import("builtins.zig").builtins;
const jobs = @import("jobs.zig");
const ast = @import("ast.zig");
const AndOrCmdList = ast.AndOrCmdList;
const AndOrCmdListKind = AndOrCmdList.AndOrCmdListKind;
const Command = ast.Command;
const Word = ast.Word;
const Position = ast.Position;
const Range = ast.Range;
const IORedir = ast.IORedir;
const printError = std.debug.print;

const BoundedArray = std.BoundedArray([]const u8, 60);

pub fn program(ctl: *jobs.JobController, prog: *ast.Program) !u32 {
    return try commandListArray(ctl, prog.body);
}

fn commandListArray(ctl: *jobs.JobController, cmd_list_array: []ast.CommandList) !u32 {
    // TODO improve and fix behaviour
    var last_status: u32 = 0;
    for (cmd_list_array) |cmd_list| {
        last_status = try andOrCmd(ctl, cmd_list.and_or_cmd_list);
    }
    return last_status;
}

fn andOrCmd(ctl: *jobs.JobController, and_or_cmd: ast.AndOrCmdList) anyerror!u32 {
    return switch (and_or_cmd.kind) {
        .PIPELINE => try pipeline(ctl, and_or_cmd.cast(.PIPELINE).?),
        .BINARY_OP => blk: {
            const binary_op = and_or_cmd.cast(.BINARY_OP).?;
            var last_status = try andOrCmd(ctl, binary_op.left);
            if ((binary_op.kind == .AND and last_status == 0) or
                (binary_op.kind == .OR and last_status != 0))
            {
                break :blk try andOrCmd(ctl, binary_op.right);
            }
            break :blk last_status;
        },
    };
}

fn pipeline(ctl: *jobs.JobController, pline: *ast.Pipeline) !u32 {
    std.debug.assert(pline.commands.len > 0);
    var last_status: u32 = 0;
    if (pline.commands.len == 1) {
        last_status = try command(ctl, pline.commands[0]);
    } else {
        // TODO consider non-allocating data, also if jobs.Process should be pointer or not
        // TODO solve memory leak
        var child_ctl = ctl.*;
        var job = try jobs.Job.initCapacity(&child_ctl, pline.commands.len);
        var next_stdin: ?os.fd_t = null;
        var cur_stdin: ?os.fd_t = null;
        var cur_stdout: ?os.fd_t = null;
        for (pline.commands) |cmd, index| {
            const is_last_cmd = index == (pline.commands.len - 1);
            if (!is_last_cmd) {
                std.debug.assert(next_stdin == null and cur_stdout == null);
                const fds = try os.pipe();
                next_stdin = fds[0];
                cur_stdout = fds[1];
            }
            const pid = try os.fork();
            if (pid == 0) {
                if (index > 0) {
                    if (next_stdin) |stdin| os.close(stdin);

                    if (cur_stdin.? != os.STDIN_FILENO) {
                        os.dup2(cur_stdin.?, os.STDIN_FILENO) catch {
                            os.exit(127);
                        };
                        os.close(cur_stdin.?);
                    }
                }

                if (!is_last_cmd and cur_stdout.? != os.STDOUT_FILENO) {
                    os.dup2(cur_stdout.?, os.STDOUT_FILENO) catch {
                        os.exit(127);
                    };
                    os.close(cur_stdout.?);
                }

                const result = command(&child_ctl, cmd) catch {
                    os.exit(127);
                };

                os.exit(@intCast(u8, result));
            }
            const proc = jobs.Process.init(pid);
            try job.addProcess(proc);

            if (cur_stdin) |stdin| os.close(stdin);

            if (cur_stdout) |stdout| {
                os.close(stdout);
                cur_stdout = null;
            }
            cur_stdin = next_stdin;
            next_stdin = null;
        }

        last_status = job.waitProcesses();
    }
    if (pline.has_bang) {
        last_status = if (last_status != 0) 0 else 1;
    }
    return last_status;
}

const ExpandedWordArray = std.ArrayList([]const u8);

fn command(ctl: *jobs.JobController, cmd: Command) anyerror!u32 {
    return switch (cmd.kind) {
        .SIMPLE_COMMAND => cmd: {
            const simple_command = cmd.cast(.SIMPLE_COMMAND).?;
            defer ctl.restoreFds();

            if (simple_command.name) |word_name| {
                if (simple_command.io_redirs) |io_redirections| {
                    applyProcRedirects(ctl, io_redirections) catch |err| {
                        std.debug.print("error {}\n", .{err});
                    };
                }
                var argv = try ExpandedWordArray.initCapacity(ctl.allocator, 1);
                if (simple_command.args) |args| {
                    _ = try expandWord(ctl, word_name, &argv);
                    try argv.ensureUnusedCapacity(args.len);
                    for (args) |word_arg| {
                        _ = try expandWord(ctl, word_arg, &argv);
                    }
                } else {
                    _ = try expandWord(ctl, word_name, &argv);
                }
                break :cmd try runProcess(ctl, argv.toOwnedSlice());
            }
            unreachable; // TODO include others possibilities of a simple command
        },
        .CMD_GROUP => cmd: {
            const cmd_group = cmd.cast(.CMD_GROUP).?;
            switch (cmd_group.kind) {
                .BRACE_GROUP => break :cmd try commandListArray(ctl, cmd_group.body),
                .SUBSHELL => {
                    const pid = try os.fork();
                    if (pid == 0) {
                        const result = commandListArray(ctl, cmd_group.body) catch 127;
                        os.exit(@intCast(u8, result));
                    }
                    const result = os.waitpid(pid, 0).status;
                    break :cmd result; // TODO add shell process
                },
            }
        },
        .FOR_DECL => {
            unreachable;
        },
        .CASE_DECL => {
            unreachable;
        },
        .LOOP_DECL => cmd: {
            // TODO integrate with ctl (jobcontroller)
            var result: u32 = 0;
            const loop_decl = cmd.cast(.LOOP_DECL).?;
            while (true) {
                const cond = try commandListArray(ctl, loop_decl.condition);
                if ((cond == 0 and loop_decl.kind == .WHILE) or
                    (cond != 0 and loop_decl.kind == .UNTIL))
                {
                    result = try commandListArray(ctl, loop_decl.body);
                } else {
                    break :cmd result;
                }
            }
        },
        .IF_DECL => cmd: {
            const if_decl = cmd.cast(.IF_DECL).?;
            const result = try commandListArray(ctl, if_decl.condition);
            if (result == 0) {
                break :cmd try commandListArray(ctl, if_decl.body);
            } else if (if_decl.else_decl) |else_decl| {
                break :cmd try command(ctl, else_decl);
            } else {
                break :cmd 0;
            }
        },
        .FUNC_DECL => cmd: {
            const func_decl = cmd.cast(.FUNC_DECL).?;
            // const cmd_body = cmd: {
            //     switch (func_decl.body.kind) {
            //         .SIMPLE_COMMAND => ,
            //         .IF_DECL => {},
            //         .LOOP_DECL => {},
            //         .CASE_DECL => {},
            //         .FOR_DECL => {},
            //         .CMD_GROUP => {},
            //         .FUNC_DECL => {},
            //     }
            // };
            // try ctl.putFunc(func_decl.name, cmd_body);
            // TODO make dupe for command types
            try ctl.putFunc(func_decl.name, func_decl.body);
            break :cmd 0;
        },
    };
}

const WordResult = union(enum) {
    empty: void,
    single_str: []const u8,
    multiple_str: []const []const u8,
};

// TODO move to expand_word.zig
fn expandWord(ctl: *jobs.JobController, word_arg: ast.Word, fields: *ExpandedWordArray) !u32 {
    // TODO evalTilde
    // TODO split fields
    // TODO expand pathnames
    // try splitFields(fields, word_ref);
    var result: u32 = 0;
    switch (try execWord(ctl, word_arg, &result)) {
        .multiple_str => |slice| {
            defer ctl.allocator.free(slice);
            std.debug.print("added slice: {s}\n", .{slice});
            try fields.appendSlice(slice);
        },
        .single_str => |str| try fields.append(str),
        else => unreachable,
    }
    return result;
}

fn execWord(ctl: *jobs.JobController, word_arg: ast.Word, result: *u32) anyerror!WordResult {
    return switch (word_arg.kind) {
        .STRING => WordResult{ .single_str = word_arg.cast(.STRING).?.str },
        .LIST => list: {
            // if double quoted, then merge all of the content into one []const u8
            // if not, then add all of them separatedly
            const word_list = word_arg.cast(.LIST).?;
            var word_array = std.ArrayList([]const u8).init(ctl.allocator);
            defer word_array.deinit();

            for (word_list.items) |word_item| {
                switch (try execWord(ctl, word_item, result)) {
                    .multiple_str => |slice| {
                        defer ctl.allocator.free(slice);
                        try word_array.appendSlice(slice);
                    },
                    .single_str => |str| try word_array.append(str),
                    else => unreachable, // TODO implement allocating empty string only when is_double_quoted
                }
            }

            if (word_list.is_double_quoted) {
                var intern_char_array = std.ArrayList(u8).init(ctl.allocator);
                defer intern_char_array.deinit();
                for (word_array.items) |str| {
                    try intern_char_array.ensureUnusedCapacity(str.len);
                    for (str) |char| intern_char_array.appendAssumeCapacity(char);
                }
                break :list WordResult{ .single_str = intern_char_array.toOwnedSlice() };
            } else {
                break :list WordResult{ .multiple_str = word_array.toOwnedSlice() };
            }
        },
        .PARAMETER => unreachable,
        .COMMAND => cmd: {
            const word_cmd = word_arg.cast(.COMMAND).?;
            const fds = try os.pipe();
            errdefer {
                os.close(fds[0]);
                os.close(fds[1]);
            }
            const pid = try os.fork();
            if (pid == 0) {
                os.close(fds[0]);
                if (fds[1] != os.STDOUT_FILENO) {
                    try os.dup2(fds[1], os.STDOUT_FILENO);
                    os.close(fds[1]);
                }
                // TODO implement traps
                if (word_cmd.program) |prog| {
                    const progResult = try ctl.run(prog);
                    os.exit(@intCast(u8, progResult));
                }
                os.exit(0);
            }
            os.close(fds[1]);

            var output_handle = std.fs.File{ .handle = fds[0] };
            // TODO improve it
            var data = try output_handle.reader().readUntilDelimiterOrEofAlloc(ctl.allocator, 0, 4096);
            if (data) |buffer| {
                result.* = os.waitpid(pid, 0).status;
                // trim newlines at end
                var end_size: usize = buffer.len;
                while (buffer[end_size - 1] == '\n') : (end_size -= 1) {}
                if (end_size < buffer.len) {
                    ctl.allocator.free(buffer[end_size..buffer.len]);
                }

                break :cmd WordResult{ .single_str = buffer[0..end_size] };
            } else {
                break :cmd .empty;
            }
        },
        .ARITHMETIC => unreachable,
    };
}

fn runProcess(ctl: *jobs.JobController, argv: []const []const u8) !u32 {
    defer ctl.allocator.free(argv);
    // std.debug.print("argv: {s}\n", .{argv});

    if (ctl.getFunc(argv[0])) |func_cmd| {
        return try command(ctl, func_cmd);
    } else if (builtins.get(argv[0])) |builtin| {
        return builtin(ctl, argv);
    }
    var argvZ = try std.ArrayList(?[*:0]const u8).initCapacity(ctl.allocator, argv.len);
    defer {
        var i: usize = 0;
        while (i < argvZ.items.len) : (i += 1) {
            if (argvZ.items[i]) |argZ| {
                ctl.allocator.destroy(argZ);
            }
        }
        argvZ.deinit();
    }

    for (argv) |arg| {
        argvZ.appendAssumeCapacity(try ctl.allocator.dupeZ(u8, arg));
    }

    const pid = std.os.fork() catch |err| {
        switch (err) {
            error.SystemResources => printError("kzh: could not fork, system resources unavaliable\n", .{}),
            else => printError("kzh: {}", .{err}),
        }
        return 1;
    };
    if (pid == 0) {
        const args = try argvZ.toOwnedSliceSentinel(null);
        defer ctl.allocator.free(args);

        const envp = try ctl.envp(ctl.allocator);
        defer ctl.allocator.free(envp);

        switch (std.os.execvpeZ(args[0].?, args, envp)) {
            error.FileNotFound => printError("kzh: {s}: not found\n", .{argv[0]}),
            error.AccessDenied => printError("kzh: {s}: cannot execute - Permission denied\n", .{argv[0]}),
            else => |err| printError("some problem happened: {}\n", .{err}),
        }
        os.exit(1);
    } else {
        _ = std.os.waitpid(pid, 0);
        // printError("\nreturned process: {}\n", .{ret});
        return 0; // TODO figure out how to handle error of execution
    }
}

fn applyProcRedirects(ctl: *jobs.JobController, io_redirs: []IORedir) !void {
    var arr = try std.ArrayList(jobs.SavedIOFd).initCapacity(ctl.allocator, io_redirs.len);
    defer arr.deinit();

    for (io_redirs) |io_redir| {
        var savedFd = jobs.SavedIOFd{};
        savedFd.saveApplyFd(ctl.allocator, io_redir) catch |err| {
            switch (err) {
                error.AccessDenied => printError("kzh: cannot create {s}: Permission denied\n", .{io_redir.name.cast(.STRING).?.str}), // TODO word string function
                error.SameFd => continue,
                else => printError("kzh: {}", .{err}),
            }
            os.exit(1);
        };
        arr.appendAssumeCapacity(savedFd);
    }
    ctl.saved_fds = arr.toOwnedSlice();
}
