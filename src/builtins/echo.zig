const std = @import("std");
const Option = @import("../builtins.zig").Option;
const OptIterator = @import("../builtins.zig").OptIterator;
const out = std.io.getStdOut();

const EchoOptions = enum {
    SUPRESS_NEWLINE,
    ENABLE_BACKSLASH,
    DISABLE_BACKSLASH,
};

const options = [_]Option(EchoOptions){
    .{ .identifier = .SUPRESS_NEWLINE, .short = 'n' },
    .{ .identifier = .ENABLE_BACKSLASH, .short = 'e' },
    .{ .identifier = .DISABLE_BACKSLASH, .short = 'E' },
};

// TODO complete implementation and doc
///  echo [-Een] [arg ...]
///
/// Prints its arguments (separated by spaces) followed by a newline, to the standard output.
/// The options are provided for compatibility with BSD shell scripts.
/// The -n option suppresses the trailing newline, -e enables backslash interpretation (a no-op,
/// since this is normally done), and -E suppresses backslash interpretation.
pub fn kzhEcho(args: []const []const u8) u8 {
    var it = OptIterator(EchoOptions).init(options[0..], args);
    var enable_backslash = true; // default behavior
    var supress_newline = false;
    while (it.nextOpt() catch {
        return 1; // TODO on unknown option should print it not return as error, update builtin options or dont use it
    }) |option| {
        switch (option.id) {
            .SUPRESS_NEWLINE => supress_newline = true,
            .ENABLE_BACKSLASH => {}, // no-op
            .DISABLE_BACKSLASH => enable_backslash = false,
        }
    }
    var buf = std.io.bufferedWriter(out.writer());
    var writer = buf.writer();
    if (it.nextArg()) |arg| {
        writer.print("{s}", .{arg}) catch return 1;
    }
    while (it.nextArg()) |arg| {
        writer.print(" ", .{}) catch return 1;
        writer.print("{s}", .{arg}) catch return 1;
    }
    if (!supress_newline) {
        writer.print("\n", .{}) catch return 1;
    }

    buf.flush() catch return 1;

    // TODO consider if only returning 1 on error is a good idea
    return 0;
}
