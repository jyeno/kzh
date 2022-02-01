//! pwd builtin
const std = @import("std");
const JobController = @import("../jobs.zig").JobController;
const Option = @import("../builtins.zig").Option;
const OptIterator = @import("../builtins.zig").OptIterator;
const writer = std.io.getStdOut().writer();

const PwdOptions = enum { LOGICAL, PHYSICAL };

const options = [_]Option(PwdOptions){
    .{ .identifier = .LOGICAL, .short = 'L' },
    .{ .identifier = .PHYSICAL, .short = 'P' },
};

// TODO remove global symtab, use the one on ctl instead

/// pwd [-LP]
///
/// Prints the current directory.
/// If the -L option is used or if the physical option isn't set (see `set` builtin),
/// the logical path is printed. If the -P option is used or the physical option is set,
/// the path determined from the filesystem is printed.
pub fn kzhPwd(ctl: *JobController, args: []const []const u8) u8 {
    _ = ctl;
    var resolve_symlinks = false; // default behavior, TODO get physical option

    var it = OptIterator(PwdOptions).init(&options, args);
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

    var cwdBuffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var pwd: []const u8 = undefined;
    if (resolve_symlinks) {
        pwd = std.os.getcwd(&cwdBuffer) catch |err| {
            std.debug.print("pwd err: {}\n", .{err});
            return 1;
        };
    } else {
        // TODO tests
        if (ctl.lookupVar("PWD")) |value| {
            pwd = value;
        } else {
            var fba = std.heap.FixedBufferAllocator.init(&cwdBuffer);
            pwd = std.fs.path.resolve(fba.allocator(), &[_][]const u8{"."}) catch return 1;
        }
    }

    writer.print("{s}\n", .{pwd}) catch return 1;
    return 0;
}
