//! pwd builtin
const std = @import("std");
const Option = @import("../builtins.zig").Option;
const OptIterator = @import("../builtins.zig").OptIterator;
const symtab = @import("../symtab.zig");
const writer = std.io.getStdOut().writer();

const PwdOptions = enum { LOGICAL, PHYSICAL };

const options = [_]Option(PwdOptions){
    .{ .identifier = .LOGICAL, .short = 'L' },
    .{ .identifier = .PHYSICAL, .short = 'P' },
};

/// pwd [-LP]
///
/// Prints the current directory.
/// If the -L option is used or if the physical option isn't set (see `set` builtin),
/// the logical path is printed. If the -P option is used or the physical option is set,
/// the path determined from the filesystem is printed.
pub fn kzhPwd(args: []const []const u8) u8 {
    var resolve_symlinks = false; // default behavior, TODO get physical option

    var it = OptIterator(PwdOptions).init(options[0..], args);
    while (it.nextOpt() catch {
        return 1; // if invalid option, return error
    }) |option| {
        switch (option.id) {
            .PHYSICAL => resolve_symlinks = true,
            .LOGICAL => {},
        }
    }

    if (it.nextArg()) |arg| {
        _ = arg; // ignoring arg as ksh doesnt print it
        std.debug.print("kzh: pwd: too many arguments\n", .{}); // TODO error print universal
        return 1;
    }

    if (resolve_symlinks) {
        var cwdBuffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        const pwd = std.os.getcwd(&cwdBuffer);
        writer.print("{s}\n", .{pwd}) catch return 1;
    } else {
        // TODO check if variable is empty
        writer.print("{s}\n", .{symtab.global_symtab.lookup("PWD").?.str}) catch return 1;
    }

    return 0;
}
