//! true builtin
const JobController = @import("../jobs.zig").JobController;

/// true
///
/// A command that exits with a zero value.
pub fn kzhTrue(ctl: *JobController, args: []const []const u8) u8 {
    _ = ctl;
    _ = args;
    return 0;
}
