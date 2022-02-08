//! cd builtin
const std = @import("std");
const JobController = @import("../jobs.zig").JobController;
const Option = @import("../builtins.zig").Option;
const OptIterator = @import("../builtins.zig").OptIterator;
const writer = std.io.getStdOut().writer();
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
///
/// If the -P option (physical path) is used or if the physical option is set, ‘..’ is relative
/// to the filesystem directory tree. The PWD and OLDPWD parameters are updated to reflect the
/// current and old working directory, respectively.
pub fn kzhCd(ctl: *JobController, args: []const []const u8) u8 {
    var resolve_symlinks = false; // default behavior, TODO get physical option

    var it = OptIterator(CdOptions).init(&options, args);
    while (it.nextOpt() catch {
        return 1;
    }) |option| {
        switch (option.id) {
            .PHYSICAL => resolve_symlinks = true,
            .LOGICAL => {},
        }
    }

    var should_print_path = false;
    var buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);

    // TODO treat possibilities of error
    // TODO CDPATH
    const old_pwd = ctl.lookupVar("PWD").?;
    var new_pwd: []const u8 = ctl.lookupVar("HOME").?;
    if (it.nextArg()) |arg| {
        if (std.mem.eql(u8, arg, "-")) {
            if (ctl.lookupVar("OLDPWD")) |value| {
                new_pwd = value;
                should_print_path = true;
            } else {
                writer.print("-", .{}) catch return 1;
                return 0;
            }
        } else if (resolve_symlinks) {
            new_pwd = std.os.realpath(arg, &buffer) catch return 1;
        } else {
            new_pwd = std.fs.path.resolve(fba.allocator(), &[_][]const u8{arg}) catch return 1;
        }
    }

    if (it.nextArg()) |arg| {
        _ = arg; // TODO figure out how cd with two args work
        printError("kzh: cd: bad substitution\n", .{});
        return 1;
    }

    std.os.chdir(new_pwd) catch |err| {
        switch (err) {
            // TODO: research correct numbers to return
            // TODO consider return error, treat it somewhere else
            error.AccessDenied => printError("kzh: {s} - Permission denied\n", .{args}),
            error.SymLinkLoop => printError("SymLink looping.\n", .{}),
            error.NameTooLong => printError("Name is too long.\n", .{}),
            error.NotDir => printError("Not a directory.\n", .{}),
            else => printError("cd err: {}\n", .{err}),
        }
        return 1;
    };
    if (should_print_path) writer.print("{s}\n", .{new_pwd}) catch return 1;

    ctl.putVar("PWD", new_pwd) catch return 1;
    ctl.putVar("OLDPWD", old_pwd) catch return 1;

    return 0;
}
