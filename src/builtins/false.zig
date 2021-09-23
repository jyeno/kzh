//! false builtin

/// false
///
/// A command that exits with a non-zero status.
pub fn kzhFalse(args: [][]const u8) u8 {
    _ = args;
    return 1;
}
