//! this module provides an AST for command parsing of kzh shell
//! Also, see 'GRAMMAR' on parse.zig
const std = @import("std");
const mem = std.mem;
const word = @import("ast/word.zig");
const command = @import("ast/command.zig");
pub usingnamespace word;
pub usingnamespace command;

// consider usage of this
// use buffered writer
const esc = "\x1B";
const csi = esc ++ "[";

pub fn create(allocator: mem.Allocator, comptime T: type, data: T) !*T {
    const ptr = try allocator.create(T);
    ptr.* = data;
    return ptr;
}

/// Representation of a 'program'
/// It has a body that contains one or more `CommandList`s.
pub const Program = struct {
    body: []CommandList,
};

/// Command List representation
pub const CommandList = struct {
    and_or_cmd_list: AndOrCmdList,
    is_async: bool = false,
};

/// And Or Command List representation
pub const AndOrCmdList = struct {
    impl: *anyopaque,
    kind: AndOrCmdListKind,

    /// And Or Command List type representation
    pub const AndOrCmdListKind = enum(u1) {
        /// command '|' command...
        PIPELINE,
        /// command '&&' or '||' command...
        BINARY_OP,

        pub fn Type(comptime self: AndOrCmdListKind) type {
            return switch (self) {
                .PIPELINE => Pipeline,
                .BINARY_OP => BinaryOp,
            };
        }
    };

    /// Casts given `base` (`AndOrCmdList`) pointer to `and_or_list_kind`, returns null if fail
    pub fn cast(and_or_cmd: *const AndOrCmdList, comptime and_or_kind: AndOrCmdListKind) ?*and_or_kind.Type() {
        if (and_or_cmd.kind == and_or_kind) {
            return @ptrCast(*and_or_kind.Type(), @alignCast(@alignOf(and_or_kind.Type()), and_or_cmd.impl));
        } else {
            return null;
        }
    }
};

/// Pipeline representation
pub const Pipeline = struct {
    commands: []command.Command,
    has_bang: bool,

    pub fn andOrCmd(self: *Pipeline) AndOrCmdList {
        return .{
            .impl = self,
            .kind = .PIPELINE,
        };
    }
};

/// Binary Operation representation
pub const BinaryOp = struct {
    left: AndOrCmdList,
    right: AndOrCmdList,
    kind: BinaryOpKind,

    pub const BinaryOpKind = enum(u1) {
        /// &&
        AND,
        /// ||
        OR,
    };

    pub fn andOrCmd(self: *BinaryOp) AndOrCmdList {
        return .{
            .impl = self,
            .kind = .BINARY_OP,
        };
    }
};
