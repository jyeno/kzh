//! exit builtin
const std = @import("std");
const JobController = @import("../jobs.zig").JobController;
// const out = std.io.getStdOut();

/// exit [status]
///
/// The shell exits with the specified exit status.
/// If status is not specified, the exit status is the current value of the $? parameter.
pub fn kzhExit(ctl: *JobController, args: []const []const u8) u8 {
    defer ctl.deinit();
    // TODO implement $? shell
    const exitValue = if (args.len == 1) 0 else std.fmt.parseInt(u8, args[1], 10) catch {
        // TODO have an standart place to print and just call kzhPrint or kzhPrintErr
        std.debug.print("kzh: {s}: bad number\n", .{args[1]});
        return 1;
    };
    std.os.exit(exitValue);
}
