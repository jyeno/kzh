const std = @import("std");
const Linenoise = @import("linenoise").Linenoise;

var linenoize: Linenoise = undefined;

pub fn main() anyerror!void {
    const interative_mode = true;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloca = &gpa.allocator;
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.debug.print("Memory leaked.\n", .{});
    }

    try initGlobals(alloca, interative_mode);

    defer linenoize.deinit();
    defer linenoize.history.save("ksh-history") catch |err| std.debug.print("Failed to save history, {}\n", .{err});

    if (interative_mode) {
        kzh_loop(alloca) catch |err| switch (err) {
            else => std.debug.print("{}\n", .{err}),
        };
    }
}

pub fn kzh_loop(alloca: *std.mem.Allocator) !void {
    while (true) {
        if (try linenoize.linenoise("PS1: ")) |input| {
            defer alloca.free(input);

            std.debug.print("input: {s}\n", .{input});

            linenoize.history.add(input) catch |err| switch (err) {
                else => std.debug.print("history: {}\n", .{err}),
            };
        }
    }
}

pub fn initGlobals(alloc: *std.mem.Allocator, interative_mode: bool) !void {
    if (interative_mode) {
        // linenoize setup
        linenoize = Linenoise.init(alloc);
        linenoize.multiline_mode = true;
        linenoize.history.load("ksh-history") catch |err| switch (err) {
            else => std.debug.print("Failed to load history, {}\n", .{err}),
        };
    }
}
