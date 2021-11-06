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

/// Representation of a 'program'
/// It has a body that contains one or more `CommandList`s.
pub const Program = struct {
    // TODO make this not a array of pointers
    body: []*CommandList,

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *mem.Allocator, program: Program) !*Program {
        const prog = try allocator.create(Program);
        prog.* = program;
        return prog;
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self: *Program, allocator: *mem.Allocator) void {
        for (self.body) |command_list| {
            command_list.deinit(allocator);
        }
        allocator.free(self.body);
        allocator.destroy(self);
    }

    /// Prints the Program Representation
    pub fn print(self: *Program, spacing: usize) void {
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("program\n", .{});
        for (self.body) |command_list| {
            command_list.print(spacing + 2);
        }
        std.debug.print("\n", .{});
    }
};

/// Command List representation
pub const CommandList = struct {
    and_or_cmd_list: AndOrCmdList,
    is_async: bool = false,

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *mem.Allocator, command_list: CommandList) !*CommandList {
        const cmd_list = try allocator.create(CommandList);
        cmd_list.* = command_list;
        return cmd_list;
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self: *CommandList, allocator: *mem.Allocator) void {
        self.and_or_cmd_list.deinit(allocator);
        allocator.destroy(self);
    }

    /// Prints the Command List representation
    pub fn print(self: *CommandList, spacing: usize) void {
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("cmd_list is_async ({}):\n", .{self.is_async});
        self.and_or_cmd_list.print(spacing + 2);
    }
};

/// And Or Command List representation
pub const AndOrCmdList = struct {
    impl: *c_void,
    kind: AndOrCmdListKind,
    deinitFn: fn (*c_void, *mem.Allocator) void,
    printFn: fn (*c_void, usize) void,

    /// And Or Command List type representation
    pub const AndOrCmdListKind = enum(u1) {
        /// command '|' command...
        PIPELINE,
        /// command '&&' or '||' command...
        BINARY_OP,

        pub fn Type(self: AndOrCmdListKind) type {
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

    /// Calls the correct deinitializer of the `AndOrCmdList` type
    pub fn deinit(and_or_cmd: *const AndOrCmdList, allocator: *mem.Allocator) void {
        and_or_cmd.deinitFn(and_or_cmd.impl, allocator);
    }

    /// Calls the correct printer of the data representation of `AndOrCmdList`
    pub fn print(and_or_cmd: *const AndOrCmdList, spacing: usize) void {
        and_or_cmd.printFn(and_or_cmd.impl, spacing);
    }
};

/// Pipeline representation
pub const Pipeline = struct {
    commands: []command.Command,
    has_bang: bool,

    pub fn andOrCmd(self: *Pipeline) AndOrCmdList {
        return .{ .impl = self, .kind = .PIPELINE, .deinitFn = deinit, .printFn = print };
    }

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *mem.Allocator, pipeline: Pipeline) !AndOrCmdList {
        const node_pipeline = try allocator.create(Pipeline);
        node_pipeline.* = pipeline;
        return node_pipeline.andOrCmd();
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *mem.Allocator) void {
        const self = @ptrCast(*Pipeline, @alignCast(@alignOf(Pipeline), self_void));
        for (self.commands) |cmd| {
            cmd.deinit(allocator);
        }
        allocator.free(self.commands);
        allocator.destroy(self);
    }

    /// Prints the Pipeline representation
    pub fn print(self_void: *c_void, spacing: usize) void {
        const self = @ptrCast(*Pipeline, @alignCast(@alignOf(Pipeline), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("pipeline len ({}) has_bang ({}):\n", .{ self.commands.len, self.has_bang });
        for (self.commands) |cmd| {
            cmd.print(spacing + 2);
        }
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
        return .{ .impl = self, .kind = .BINARY_OP, .deinitFn = deinit, .printFn = print };
    }

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *mem.Allocator, binary_op: BinaryOp) !AndOrCmdList {
        const binary_operation = try allocator.create(BinaryOp);
        binary_operation.* = binary_op;
        return binary_operation.andOrCmd();
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *mem.Allocator) void {
        const self = @ptrCast(*BinaryOp, @alignCast(@alignOf(BinaryOp), self_void));
        self.left.deinit(allocator);
        self.right.deinit(allocator);
        allocator.destroy(self);
    }

    /// Prints the Binary Operation representation
    pub fn print(self_void: *c_void, spacing: usize) void {
        const self = @ptrCast(*BinaryOp, @alignCast(@alignOf(BinaryOp), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("binary_op {} left: {} right: {}\n", .{ self.kind, self.left, self.right });
    }
};
