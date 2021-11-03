const std = @import("std");
const Parser = @import("parse.zig").Parser;
const kzhExit = @import("builtins/exit.zig").kzhExit;
const symtab = @import("symtab.zig");
const jobs = @import("jobs.zig");

// TODO make general purpose allocatorglobal, then deinit it at kzhExit
pub fn main() anyerror!void {
    const interative_mode = true;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloca = &gpa.allocator;
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.debug.print("Memory leaked.\n", .{});
    }

    try init(alloca, interative_mode);
    defer {
        symtab.global_symtab.deinit();
        jobs.global_controller.deinit();
    }

    if (interative_mode) {
        kzhLoop(alloca) catch |err| switch (err) {
            else => std.debug.print("{}\n", .{err}),
        };
    }
}

fn init(allocator: *std.mem.Allocator, interative_mode: bool) !void {
    _ = interative_mode;

    try symtab.initGlobalSymbolTable(allocator);
    errdefer symtab.global_symtab.deinit();

    try jobs.initGlobalJobController(allocator);
}

/// kzh main loop, used when the program is run in interactive mode
fn kzhLoop(alloca: *std.mem.Allocator) !void {
    var job_ctl = jobs.global_controller;
    while (true) {
        var algo: [256]u8 = undefined;
        const stdin = std.io.getStdIn().reader();
        if (job_ctl.last_status == 0) {
            std.debug.print("> ", .{});
        } else {
            std.debug.print(">> ", .{});
        }
        if (try stdin.readUntilDelimiterOrEof(&algo, '\n')) |input| {
            var parser = Parser.init(alloca, input);
            var program = parser.parse() catch |err| {
                std.debug.print("loop err: {}\n", .{err});
                continue;
            };
            defer program.deinit(alloca);
            // program.print(1);

            // TODO consider, should just use try here or should errors be treated inside of the job_ctl?
            try job_ctl.run(program);
            // break;
        }
    }
}

test "Test All" {
    _ = @import("ast.zig");
    _ = @import("builtins.zig");
    _ = @import("jobs.zig");
    _ = @import("parse.zig");
    _ = @import("symtab.zig");
}
