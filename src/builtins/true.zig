//! true builtin

/// true
///
/// A command that exits with a zero value.
pub fn kzhTrue(args: [][]const u8) u8 {
    _ = args;
    return 0;
}

test "builtin 'true'" {
    var argv = [_][]const u8{"true"};
    try @import("std").testing.expect(kzhTrue(argv[0..]) == 0);
}
