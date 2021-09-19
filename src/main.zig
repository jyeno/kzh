// TODO ter um sistema de hint que da hint das coisas que tem autocomplete
const std = @import("std");
const Linenoise = @import("linenoise").Linenoise;
const Parser = @import("parse.zig").Parser;
const SymTab = @import("symtab.zig");
const kzhExit = @import("builtins/exit.zig").kzhExit;

// var linenoize: Linenoise = undefined;

pub fn main() anyerror!void {
    const interative_mode = true;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloca = &gpa.allocator;
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.debug.print("Memory leaked.\n", .{});
    }
    // defer linenoize.deinit();

    // try initGlobals(alloca, interative_mode);
    // defer kzhExit([][]const u8);

    // defer linenoize.history.save("ksh-history") catch |err| std.debug.print("Failed to save history, {}\n", .{err});

    if (interative_mode) {
        kzhLoop(alloca) catch |err| switch (err) {
            else => std.debug.print("{}\n", .{err}),
        };
    }
}

pub fn kzhLoop(alloca: *std.mem.Allocator) !void {
    _ = alloca;
    while (true) {
        var algo: [256]u8 = undefined;
        const stdin = std.io.getStdIn().reader();
        if (try stdin.readUntilDelimiterOrEof(&algo, '\n')) |input| {
            // if (try linenoize.linenoise(SymTab.globalSymbolTable().local_lookup("PS1").?.str)) |input| {
            // defer alloca.free(input);

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

            // linenoize.history.add(input) catch |err| switch (err) {
            //     else => std.debug.print("history: {}\n", .{err}),
            // };
            break;
        }
    }
}

// pub fn initGlobals(alloc: *std.mem.Allocator, interative_mode: bool) !void {
//     try SymTab.initGlobalSymbolTable(alloc);
//     if (interative_mode) {
//         // linenoize setup
//         linenoize = Linenoise.init(alloc);
//         linenoize.multiline_mode = true;
//         linenoize.history.load("ksh-history") catch |err| switch (err) {
//             else => std.debug.print("Failed to load history, {}\n", .{err}),
//         };
//     }
// }
