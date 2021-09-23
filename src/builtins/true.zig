//! true builtin

/// true
///
/// A command that exits with a zero value.
pub fn kzhTrue(args: [][]const u8) u8 {
    _ = args;
    return 0;
}
