//! false builtin

/// false
///
/// A command that exits with a non-zero status.
pub fn kzhFalse(args: []const []const u8) u8 {
    _ = args;
    return 1;
}

test "builtin 'false'" {
    try @import("std").testing.expect(kzhFalse(&.{"false"}) == 1);
}
