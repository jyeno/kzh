const std = @import("std");
const mem = std.mem;
const Parser = @import("Parser.zig");
const kzhExit = @import("builtins/exit.zig").kzhExit;
const jobs = @import("jobs.zig");
const Option = @import("builtins.zig").Option;
const OptIterator = @import("builtins.zig").OptIterator;

const InitOptions = enum {
    COMMAND_ONLY,
    USE_STDIN,
};

const options = [_]Option(InitOptions){
    .{ .identifier = .COMMAND_ONLY, .short = 'c', .kind = .NEEDS_ARG },
    .{ .identifier = .USE_STDIN, .short = 's', .kind = .NO_ARG },
};

pub fn main() anyerror!u8 {
    const interative_mode = true;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.debug.print("Memory leaked.\n", .{});
    }
    // var allocator = gpa.allocator();
    var stack_alloc = std.heap.stackFallback(8096, gpa.allocator());
    var allocator = stack_alloc.get();

    var ctl = jobs.JobController.init(allocator);
    try initDefaultConf(&ctl, interative_mode);
    defer ctl.deinit();

    // TODO remove it and check arg by arg, also make it so that it stays simples
    var it = OptIterator(InitOptions).init(&options, &.{"kzh"}); //, "-c", "echo oi" }); // gets the args
    while (it.nextOpt() catch {
        return 1;
    }) |option| {
        switch (option.id) {
            .COMMAND_ONLY => return @intCast(u8, try internalExec(&ctl, option.value.?)),
            .USE_STDIN => {}, // TODO
        }
    }

    if (interative_mode) {
        try kzhLoop(&ctl);
    }
    return 0;
}

fn initDefaultConf(ctl: *jobs.JobController, interative_mode: bool) !void {
    errdefer ctl.deinit();
    _ = interative_mode;
    const keys_values = [_][]const []const u8{
        &.{ "autoload", "typeset", "-fu" },
        &.{ "functions", "typeset", "-f" },
        &.{ "hash", "alias", "-t" },
        &.{ "history", "fc", "-l" },
        &.{ "integer", "typeset", "-i" },
        &.{ "local", "typeset" },
        &.{ "login", "exec", "login" },
        &.{ "nohup", "nohup " },
        &.{ "r", "fc", "-s" },
        &.{ "stop", "kill", "-STOP" },
    };
    try ctl.aliases.ensureUnusedCapacity(keys_values.len);
    inline for (keys_values) |key_value| {
        try ctl.putAlias(key_value[0], key_value[1..]);
    }

    try ctl.env_vars.ensureUnusedCapacity(@intCast(u32, std.os.environ.len));
    for (std.os.environ) |env| {
        const equals_idx: ?usize = blk: {
            var index: usize = 0;
            while (env[index] != '=' and env[index] != 0) : (index += 1) {}
            // it cant be on the beginning and end of the env, if it is invalid therefore return null
            break :blk if (index != 0 and env[index] != 0) index else null;
        };
        if (equals_idx) |index| {
            const value = blk: {
                var end = index + 1;
                while (env[end] != 0) : (end += 1) {}
                break :blk env[index + 1 .. end];
            };
            try ctl.env_vars.put(env[0..index], value);
        }
    }
}

/// kzh main loop, used when the program is run in interactive mode
fn kzhLoop(ctl: *jobs.JobController) !void {
    // TODO consider usage of fallback allocator here
    var last_status: u32 = 0;
    while (true) {
        var algo: [256]u8 = undefined;
        const stdin = std.io.getStdIn().reader();
        std.debug.print("{}> ", .{last_status});

        if (try stdin.readUntilDelimiterOrEof(&algo, '\n')) |input| {
            // TODO do not return a pointer
            var parser = Parser.init(ctl.allocator, input);
            var program = parser.parse() catch |err| {
                std.debug.print("loop err: {}\n", .{err});
                continue;
            };
            defer parser.deinit();

            // TODO consider, should just use try here or should errors be treated inside of the job_ctl?
            last_status = try ctl.run(program);
        }
    }
}

fn internalExec(ctl: *jobs.JobController, input: []const u8) !u32 {
    var parser = Parser.init(ctl.allocator, input);
    var program = try parser.parse();
    defer parser.deinit();
    // TODO use kzhExec

    return try ctl.run(program);
}

test "Test All" {
    _ = @import("ast.zig");
    _ = @import("builtins.zig");
    _ = @import("jobs.zig");
    _ = @import("Parser.zig");
}
