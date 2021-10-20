//! builtin builtin
const builtins = @import("../builtins.zig").builtins;
const printError = @import("std").debug.print;
const testing = @import("std").testing;

/// builtin command [arg ...]
///
/// Execute the built-in `command`.
pub fn kzhBuiltin(args: [][]const u8) u8 {
    if (args.len == 1) {
        return 0;
    }
    if (builtins.get(args[1])) |builtin| {
        return builtin(args[1..]);
    }
    printError("kzh: builtin: {s}: not a builtin\n", .{args[1]});
    return 1;
}

test "builtin 'builtin'" {
    var argv1 = [_][]const u8{"builtin"};
    try testing.expect(kzhBuiltin(argv1[0..]) == 0);

    var argv2 = [_][]const u8{ "builtin", "true" };
    try testing.expect(kzhBuiltin(argv2[0..]) == 0);

    var argv3 = [_][]const u8{ "builtin", "false" };
    try testing.expect(kzhBuiltin(argv3[0..]) == 1);

    // TODO dont print the error
    var argv4 = [_][]const u8{ "builtin", "notbuiltin" };
    try testing.expect(kzhBuiltin(argv4[0..]) == 1);
}
