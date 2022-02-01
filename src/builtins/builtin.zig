//! builtin builtin
const JobController = @import("../jobs.zig").JobController;
const builtins = @import("../builtins.zig").builtins;
const printError = @import("std").debug.print;
const testing = @import("std").testing;

/// builtin command [arg ...]
///
/// Execute the built-in `command`.
pub fn kzhBuiltin(ctl: *JobController, args: []const []const u8) u8 {
    if (args.len == 1) {
        return 0;
    }
    if (builtins.get(args[1])) |builtin| {
        return builtin(ctl, args[1..]);
    }
    printError("kzh: builtin: {s}: not a builtin\n", .{args[1]});
    return 1;
}

test "builtin 'builtin'" {
    var empty_ctl = JobController.init(testing.allocator);
    try testing.expect(kzhBuiltin(&empty_ctl, &.{"builtin"}) == 0);
    try testing.expect(kzhBuiltin(&empty_ctl, &.{ "builtin", "true" }) == 0);
    try testing.expect(kzhBuiltin(&empty_ctl, &.{ "builtin", "false" }) == 1);
    // TODO dont print the error
    try testing.expect(kzhBuiltin(&empty_ctl, &.{ "builtin", "notbuiltin" }) == 1);
}
