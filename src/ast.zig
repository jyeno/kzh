//! this module provides an AST for command parsing of kzh shell
//! Also, see 'GRAMMAR' on parse.zig
const std = @import("std");
const mem = std.mem;
const word = @import("ast/word.zig");
pub usingnamespace word;

// consider usage of this
// use buffered writer
const esc = "\x1B";
const csi = esc ++ "[";

/// Representation of the position
pub const Position = struct {
    offset: u16 = 0,
    line: u16 = 1,
    column: u16 = 1,
};

/// Range of two positions
pub const Range = struct {
    begin: Position,
    end: Position,
};

/// Representation of a 'program'
/// It has a body that contains one or more `CommandList`s.
pub const Program = struct {
    body: []*CommandList,

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, program: Program) !*Program {
        const prog = try allocator.create(Program);
        prog.* = program;
        return prog;
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self: *Program, allocator: *std.mem.Allocator) void {
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
    separator_pos: ?Position = null,

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, command_list: CommandList) !*CommandList {
        const cmd_list = try allocator.create(CommandList);
        cmd_list.* = command_list;
        return cmd_list;
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self: *CommandList, allocator: *std.mem.Allocator) void {
        self.and_or_cmd_list.deinit(allocator);
        allocator.destroy(self);
    }

    /// Prints the Command List representation
    pub fn print(self: *CommandList, spacing: usize) void {
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("cmd_list is_async ({}) separator_pos ({}):\n", .{ self.is_async, self.separator_pos });
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
    pub fn deinit(and_or_cmd: *const AndOrCmdList, allocator: *std.mem.Allocator) void {
        and_or_cmd.deinitFn(and_or_cmd.impl, allocator);
    }

    /// Calls the correct printer of the data representation of `AndOrCmdList`
    pub fn print(and_or_cmd: *const AndOrCmdList, spacing: usize) void {
        and_or_cmd.printFn(and_or_cmd.impl, spacing);
    }
};

/// Pipeline representation
pub const Pipeline = struct {
    commands: []Command,
    has_bang: bool,
    bang_pos: ?Position,

    pub fn andOrCmd(self: *Pipeline) AndOrCmdList {
        return .{ .impl = self, .kind = .PIPELINE, .deinitFn = deinit, .printFn = print };
    }

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, pipeline: Pipeline) !*Pipeline {
        const node_pipeline = try allocator.create(Pipeline);
        node_pipeline.* = pipeline;
        return node_pipeline;
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *std.mem.Allocator) void {
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
        std.debug.print("pipeline len ({}) has_bang ({}) bang_pos ({}):\n", .{ self.commands.len, self.has_bang, self.bang_pos });
        for (self.commands) |cmd| {
            cmd.print(spacing + 2);
        }
    }

    // TODO pipe positions between each command
};

/// Binary Operation representation
pub const BinaryOp = struct {
    left: AndOrCmdList,
    right: AndOrCmdList,
    op_range: Range,
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
    pub fn create(allocator: *std.mem.Allocator, binary_op: BinaryOp) !*BinaryOp {
        const binary_operation = try allocator.create(BinaryOp);
        binary_operation.* = binary_op;
        // maybe andOrCmdList ?
        return binary_operation;
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *std.mem.Allocator) void {
        const self = @ptrCast(*BinaryOp, @alignCast(@alignOf(BinaryOp), self_void));
        self.left.deinit(allocator);
        self.right.deinit(allocator);
        allocator.destroy(self);
    }

    /// Prints the Binary Operation representation
    pub fn print(self_void: *c_void, spacing: usize) void {
        const self = @ptrCast(*BinaryOp, @alignCast(@alignOf(BinaryOp), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("binary_op ({}) {} left: {} right: {}\n", .{ self.op_range, self.kind, self.left, self.right });
    }
};

/// Command representation
pub const Command = struct {
    impl: *c_void,
    kind: CommandKind,
    deinitFn: fn (*c_void, *mem.Allocator) void,
    printFn: fn (*c_void, usize) void,

    /// Command type representation
    pub const CommandKind = enum {
        SIMPLE_COMMAND,
        // BRACE_GROUP,
        // SUBSHELL,
        // IF_CLAUSE,
        // FOR_CLAUSE,
        // LOOP_CLAUSE,
        // CASE_CLAUSE,
        // FUNCTION_DEF,

        pub fn Type(self: CommandKind) type {
            return switch (self) {
                .SIMPLE_COMMAND => SimpleCommand,
            };
        }
    };

    pub fn cast(cmd: *const Command, comptime cmd_kind: CommandKind) ?*cmd_kind.Type() {
        if (cmd.kind == cmd_kind) {
            return @ptrCast(*cmd_kind.Type(), @alignCast(@alignOf(cmd_kind.Type()), cmd.impl));
        } else {
            return null;
        }
    }

    pub fn deinit(cmd: *const Command, allocator: *std.mem.Allocator) void {
        cmd.deinitFn(cmd.impl, allocator);
    }

    pub fn print(cmd: *const Command, spacing: usize) void {
        cmd.printFn(cmd.impl, spacing);
    }
};

/// Simple Command representation
pub const SimpleCommand = struct {
    name: ?word.Word,
    args: ?[]word.Word = null,
    // maybe not allocate memory here
    io_redirs: ?[]IORedir = null,
    assigns: ?[]Assign = null,

    pub fn cmd(self: *SimpleCommand) Command {
        return .{ .impl = self, .kind = .SIMPLE_COMMAND, .deinitFn = deinit, .printFn = print };
    }

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, simple_command: SimpleCommand) !*SimpleCommand {
        const simple_cmd = try allocator.create(SimpleCommand);
        simple_cmd.* = simple_command;
        return simple_cmd;
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *std.mem.Allocator) void {
        const self = @ptrCast(*SimpleCommand, @alignCast(@alignOf(SimpleCommand), self_void));
        if (self.name) |word_name| {
            word_name.deinit(allocator);
        }
        if (self.args) |args| {
            for (args) |arg| {
                arg.deinit(allocator);
            }
            allocator.free(args);
        }
        if (self.assigns) |assignments| {
            for (assignments) |assign| {
                if (assign.value) |val| {
                    val.deinit(allocator);
                }
            }
            allocator.free(assignments);
        }
        if (self.io_redirs) |io_redirects| {
            for (io_redirects) |io_redir| {
                io_redir.name.deinit(allocator);
            }
            allocator.free(io_redirects);
        }
        allocator.destroy(self);
    }

    /// Prints the Simple Command representation
    pub fn print(self_void: *c_void, spacing: usize) void {
        var self = @ptrCast(*SimpleCommand, @alignCast(@alignOf(SimpleCommand), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("simple_command:\n", .{});
        if (self.io_redirs) |io_redirects| {
            for (io_redirects) |io_redir| {
                std.debug.print(csi ++ "{}C", .{spacing + 2});
                std.debug.print("io_redir op: {} name: {s} io_num: {} op_range: {}\n", .{ io_redir.op, io_redir.name.cast(word.Word.WordKind.STRING).?.str, io_redir.io_num, io_redir.op_range });
            }
        }
        if (self.assigns) |assignments| {
            for (assignments) |assign| {
                std.debug.print(csi ++ "{}C", .{spacing + 2});
                std.debug.print("assign name: {s} ({})  value:\n", .{ assign.name, assign.name_range });
                if (assign.value) |v| {
                    v.print(spacing + 4);
                } else {
                    std.debug.print("\"\"", .{});
                }
            }
        }
        if (self.name) |word_name| {
            word_name.print(spacing + 2);
        }
        if (self.args) |args| {
            for (args) |arg| {
                arg.print(spacing + 2);
            }
        }
    }

    /// Checks whenether the simple command is empty, returns true if it
    /// has no `name`, `io_redirs` and `assigns`, retuns false otherwise
    pub fn isEmpty(self: *SimpleCommand) bool {
        return self.name == null and self.io_redirs == null and self.assigns == null;
    }
};

/// Input/Output Redirection representation
pub const IORedir = struct {
    io_num: ?u8 = null,
    name: word.Word,
    here_doc: ?[]word.Word = null,
    io_num_pos: ?Position = null,
    op_range: Range,
    op: IORedirKind,

    /// Input/Output type representation
    pub const IORedirKind = enum {
        /// <
        IO_LESS,
        /// <<
        IO_DOUBLE_LESS,
        /// <&
        IO_LESS_AND,
        /// <<-
        IO_DOUBLE_LESS_DASH,
        /// <>
        IO_LESS_GREAT,
        /// >
        IO_GREAT,
        /// >>
        IO_DOUBLE_GREAT,
        /// >&
        IO_GREAT_AND,
        /// >|
        IO_CLOBBER,
    };

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, io_redir: IORedir) !*IORedir {
        const io_redirection = try allocator.create(IORedir);
        io_redirection.* = io_redir;
        return io_redirection;
    }
};

/// Assignment representation, name=value
pub const Assign = struct {
    name: []const u8,
    value: ?word.Word,
    name_range: Range,
    equal_pos: Position,

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, assign: Assign) !*Assign {
        const assignment = try allocator.create(Assign);
        assignment.* = assign;
        return assignment;
    }
};
