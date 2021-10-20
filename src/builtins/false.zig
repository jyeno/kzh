//! false builtin

/// false
///
/// A command that exits with a non-zero status.
pub fn kzhFalse(args: [][]const u8) u8 {
    _ = args;
    return 1;
}

test "builtin 'false'" {
    var argv = [_][]const u8{"false"};
    try @import("std").testing.expect(kzhFalse(argv[0..]) == 1);
}
