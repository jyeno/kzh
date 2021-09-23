//! cd builtin
const std = @import("std");
const out = std.io.getStdOut();
const Option = @import("../builtins.zig").Option;
const OptIterator = @import("../builtins.zig").OptIterator;
const printError = std.debug.print;

const CdOptions = enum {
    LOGICAL,
    PHYSICAL,
};

const options = [_]Option(CdOptions){
    .{ .identifier = .LOGICAL, .short = 'L' },
    .{ .identifier = .PHYSICAL, .short = 'P' },
};

// TODO complete documentation of cd builtin and correct wrong behavior
//  cd [-LP] [dir]
///
/// Set the working directory to `dir`.
/// If the -L option is used or if the physical option isn't set (see the `set` builtin),
/// references to ‘..’ in dir are relative to the path used to get to the directory.
/// If the -P option (physical path) is used or if the physical option is set, ‘..’ is relative
/// to the filesystem directory tree. The PWD and OLDPWD parameters are updated to reflect the
/// current and old working directory, respectively.
pub fn kzhCd(args: [][]const u8) u8 {
    var it = OptIterator(CdOptions).init(options[0..], args);
    while (it.nextOpt() catch {
        return 1;
    }) |option| {
        switch (option.id) {
            .LOGICAL => {}, // TODO
            .PHYSICAL => {}, // TODO
        }
    }
    // TODO use symbol table to set OLD_PWD and PWD, and also get HOME
    var new_pwd: []const u8 = ".";
    if (it.nextArg()) |arg| {
        new_pwd = arg;
    }
    if (it.nextArg()) |arg| {
        _ = arg; // ignoring arg
        // TODO figure out how cd with two args work
        printError("kzh: cd: bad substitution\n", .{});
        return 1;
    }
    std.os.chdir(new_pwd) catch |err| switch (err) {
        // TODO: research correct numbers to return
        // TODO return error, treat it somewhere else
        error.AccessDenied => {
            printError("kzh: {s} - Permission denied\n", .{args});
            return 1;
        },
        error.SymLinkLoop => {
            printError("SymLink looping.\n", .{});
            return 1;
        },
        error.NameTooLong => {
            printError("Name is too long.\n", .{});
            return 1;
        },
        error.NotDir => {
            printError("Not a directory.\n", .{});
            return 1;
        },
        else => printError("cd err: {}\n", .{err}),
    };
    return 0;
}
