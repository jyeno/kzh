//! builtin module, it provides a comptime string map with the builtins
//! functions of the shell, and also an interface to parsing of args
// TODO make tests for every builtin, when possible
// make an generalized error printer, it should print the error message, something like this:
// "kzh: {}: {}\n" where the first {} is the command called, and the second {} is the error
const std = @import("std");
const printError = std.debug.print;
const JobController = @import("jobs.zig").JobController;

pub const builtins = std.ComptimeStringMap(*const fn (*JobController, []const []const u8) u8, .{
    .{ "builtin", @import("builtins/builtin.zig").kzhBuiltin },
    .{ "cd", @import("builtins/cd.zig").kzhCd }, // not completed
    .{ "echo", @import("builtins/echo.zig").kzhEcho }, // not completed
    .{ "exit", @import("builtins/exit.zig").kzhExit }, // not completed
    .{ "false", @import("builtins/false.zig").kzhFalse },
    .{ "pwd", @import("builtins/pwd.zig").kzhPwd }, // not completed
    .{ "true", @import("builtins/true.zig").kzhTrue },
});

const OptType = enum { NEEDS_ARG, OPTIONAL_ARG, NO_ARG };

/// Creates an Option struct with given type (must be enum)
pub fn Option(comptime T: type) type {
    return struct {
        /// Identifier of the option, must be an enum, unique
        identifier: T,
        /// whenever it needs or have optional arg, or none
        /// defaults to OptType.NoArgs
        kind: OptType = .NO_ARG,
        /// short version of the option
        short: ?u8 = null,
    };
}

/// Creates an option token, that contains the identifier and respective value held
fn OptToken(comptime T: type) type {
    return struct {
        id: T,
        is_plus: bool = false,
        value: ?[]const u8 = null,
    };
}

/// Iterator of options (and args)
pub fn OptIterator(comptime T: type) type {
    return struct {
        opts: []const Opt,
        argv: []const []const u8,
        opt_index: u8 = 1,
        arg_index: u8 = 1,
        opt_pos: u8 = 0,
        only_args: bool = false,

        const Self = @This();
        const Opt = Option(T);
        const Token = OptToken(T);

        /// Initializes the OptIterator
        pub fn init(opts: []const Opt, argv: []const []const u8) Self {
            return Self{ .opts = opts, .argv = argv };
        }

        /// Returns next Option, or null if reached the end of argv,
        /// error if there is an invalid option, missing or invalid option arg
        pub fn nextOpt(self: *Self) !?Token {
            while (self.opt_index < self.argv.len and !self.only_args) : ({
                self.opt_index += 1;
                self.opt_pos = 0;
            }) {
                const opt = self.argv[self.opt_index];
                if (std.mem.eql(u8, "--", opt)) {
                    self.only_args = true;
                } else if (opt.len >= 2 and (opt[0] == '-' or opt[0] == '+')) {
                    return try self.nextShortOpt();
                }
            }

            return null;
        }

        /// Returns the next argument, or null if reached the end of argv
        pub fn nextArg(self: *Self) ?[]const u8 {
            while (self.arg_index < self.argv.len) : (self.arg_index += 1) {
                const arg = self.argv[self.arg_index];
                // TODO improve, make this cheaper
                if ((self.only_args and !std.mem.eql(u8, "--", arg)) or arg[0] != '-' or arg[0] == '+' or arg.len == 1) {
                    self.arg_index += 1;
                    return arg;
                } else if (self.isNextOptArg()) {
                    self.arg_index += 1;
                }
            }
            return null;
        }

        /// Returns next short option, or error if invalid option, invalid
        /// or missing option argument
        fn nextShortOpt(self: *Self) !Token {
            if (self.opt_pos == 0) self.opt_pos = 1;

            const opt_str = self.argv[self.opt_index];
            if (self.getShortOpt(opt_str[self.opt_pos])) |opt| {
                return switch (opt.kind) {
                    .NO_ARG => token: {
                        if (self.opt_pos == opt_str.len - 1) {
                            self.opt_pos = 0;
                            self.opt_index += 1;
                        } else {
                            self.opt_pos += 1;
                        }
                        break :token Token{ .id = opt.identifier, .is_plus = opt_str[0] == '+' };
                    },
                    .NEEDS_ARG => token: {
                        if (self.opt_pos == opt_str.len - 1) {
                            if (self.getNextOptArg()) |value| {
                                self.opt_pos = 0;
                                self.opt_index += 1;
                                break :token Token{ .id = opt.identifier, .value = value, .is_plus = opt_str[0] == '+' };
                            }
                        }

                        printError("{s}: -{c}: needs argument\n", .{ self.argv[0], opt_str[self.opt_index] });
                        break :token error.MissingOptionArg;
                    },
                    .OPTIONAL_ARG => token: {
                        const value = self.getNextOptArg();
                        if (self.opt_pos == opt_str.len - 1) {
                            self.opt_pos = 0;
                            self.opt_index += 1;
                        } else {
                            self.opt_pos += 1;
                        }
                        break :token Token{ .id = opt.identifier, .value = value, .is_plus = opt_str[0] == '+' };
                    },
                };
            }
            printError("kzh: {s}: -{c}: unknown option\n", .{ self.argv[0], opt_str[self.opt_index] });
            return error.InvalidOption;
        }

        /// Helper function, returns the option that matches given `short`
        /// or null if none
        fn getShortOpt(self: *Self, short: u8) ?Opt {
            for (self.opts) |opt| {
                if (opt.short != null and opt.short.? == short) {
                    return opt;
                }
            }
            return null;
        }

        /// Assumes that current argument index is an option,
        /// returns false if the next arg is not related to the
        /// option or reached end of argv, returns true otherwise
        fn isNextOptArg(self: *Self) bool {
            const opt_str = self.argv[self.arg_index];
            for (opt_str[1..]) |ch, i| {
                if (self.getShortOpt(ch)) |opt| {
                    if (opt.kind == .NO_ARG) continue;

                    // if at the end of current option string and the
                    // option type is not NoArgs, returns true
                    if (i + 1 == opt_str.len) {
                        return true;
                    }
                    break;
                }
            }
            return false;
        }

        /// Gets the next argument of the current option, null if none
        fn getNextOptArg(self: *Self) ?[]const u8 {
            // TODO support -L[n]
            if (self.opt_index + 1 >= self.argv.len) {
                return null;
            }
            const opt_arg_str = self.argv[self.opt_index + 1];

            return if (opt_arg_str[0] != '-' and opt_arg_str[0] != '+') opt_arg_str else null;
        }
    };
}

test "Test Builtins" {
    _ = @import("builtins/builtin.zig");
    _ = @import("builtins/cd.zig");
    _ = @import("builtins/echo.zig");
    _ = @import("builtins/exit.zig");
    _ = @import("builtins/false.zig");
    _ = @import("builtins/pwd.zig");
    _ = @import("builtins/true.zig");
}
