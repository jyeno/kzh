//! true builtin

/// true
///
/// A command that exits with a zero value.
pub fn kzhTrue(args: []const []const u8) u8 {
    _ = args;
    return 0;
}

test "builtin 'true'" {
    try @import("std").testing.expect(kzhTrue(&.{"true"}) == 0);
}
