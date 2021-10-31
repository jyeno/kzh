//! builtin builtin
const builtins = @import("../builtins.zig").builtins;
const printError = @import("std").debug.print;
const testing = @import("std").testing;

/// builtin command [arg ...]
///
/// Execute the built-in `command`.
pub fn kzhBuiltin(args: []const []const u8) u8 {
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
    try testing.expect(kzhBuiltin(&.{"builtin"}) == 0);
    try testing.expect(kzhBuiltin(&.{ "builtin", "true" }) == 0);
    try testing.expect(kzhBuiltin(&.{ "builtin", "false" }) == 1);
    // TODO dont print the error
    try testing.expect(kzhBuiltin(&.{ "builtin", "notbuiltin" }) == 1);
}
