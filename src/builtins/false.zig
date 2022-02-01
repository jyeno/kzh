//! false builtin
const JobController = @import("../jobs.zig").JobController;

/// false
///
/// A command that exits with a non-zero status.
pub fn kzhFalse(ctl: *JobController, args: []const []const u8) u8 {
    _ = ctl;
    _ = args;
    return 1;
}
