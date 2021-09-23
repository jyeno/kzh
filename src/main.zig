const std = @import("std");
const Parser = @import("parse.zig").Parser;
const kzhExit = @import("builtins/exit.zig").kzhExit;
const executor = @import("exec.zig");

// TODO make general purpose allocatorglobal, then deinit it at kzhExit
pub fn main() anyerror!void {
    const interative_mode = true;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloca = &gpa.allocator;
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.debug.print("Memory leaked.\n", .{});
    }

    if (interative_mode) {
        kzhLoop(alloca) catch |err| switch (err) {
            else => std.debug.print("{}\n", .{err}),
        };
    }
}

pub fn kzhLoop(alloca: *std.mem.Allocator) !void {
    while (true) {
        var algo: [256]u8 = undefined;
        const stdin = std.io.getStdIn().reader();
        if (try stdin.readUntilDelimiterOrEof(&algo, '\n')) |input| {
            // TODO
            // maybe use buffer to now alloc a lot of times?
            // var buffer: [2046]u8 = undefined;
            // var fba = std.heap.FixedBufferAllocator.init(&buffer);
            var parser = Parser.init(alloca, input);
            var program = parser.parse() catch |err| {
                std.debug.print("err: {}\n", .{err});
                continue;
            };
            defer program.deinit(alloca);

            program.print();

            _ = executor.runProgram(program) catch |err| {
                std.debug.print("exec: {}\n", .{err});
            };
        }
    }
}
