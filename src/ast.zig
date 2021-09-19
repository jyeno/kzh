//! this module provides an AST for command parsing of kzh shell

const std = @import("std");

/// Representation of the position
pub const Position = struct {
    offset: u16,
    line: u16,
    column: u16,
};

/// Range of two positions
pub const Range = struct {
    begin: Position,
    end: Position,
};

/// Base struct for Abstract Syntax Tree (AST), its type are representated by
/// the `NodeKind` enum
pub const Node = struct {
    /// type of the node
    kind: NodeKind,

    /// Node type representation, each type has its own struct, it is
    /// also made so to enable "casting" the node to its right type
    /// See `NodeKind.Type` and `cast`
    pub const NodeKind = enum {
        PROGRAM,
        COMMAND_LIST,
        AND_OR_LIST,
        COMMAND,
        WORD,

        /// Returns a node type based on its type.
        pub fn Type(self: NodeKind) type {
            return switch (self) {
                .PROGRAM => Program,
                .COMMAND_LIST => CommandList,
                .AND_OR_LIST => AndOrCmdList,
                .COMMAND => Command,
                .WORD => Word,
            };
        }
    };

    /// Retrieve node pointer based on the `NodeKind`, getting the parent pointer
    /// of the node or null otherwise.
    pub fn cast(base: *Node, comptime node_kind: NodeKind) ?*node_kind.Type() {
        if (base.kind == node_kind) {
            return @fieldParentPtr(node_kind.Type(), "node", base);
        }
        return null;
    }

    /// Representation of a 'program', see 'GRAMMAR' of insert_here_link
    /// It has a body that contains one or more `CommandList`s.
    pub const Program = struct {
        node: Node = .{ .kind = .PROGRAM },
        body: []*CommandList,

        pub fn deinit(self: *Program, allocator: *std.mem.Allocator) void {
            for (self.body) |command_list| {
                command_list.deinit(allocator);
            }
            allocator.free(self.body);
            allocator.destroy(self);
        }

        pub fn print(self: *Program) void {
            std.debug.print("program\n", .{});
            for (self.body) |command_list| {
                std.debug.print(" - ", .{});
                command_list.print();
            }
            std.debug.print("\n", .{});
        }
    };

    pub const CommandList = struct {
        node: Node = .{ .kind = .COMMAND_LIST },
        and_or_cmd_list: *AndOrCmdList,
        is_async: bool = false,
        separator_pos: ?Position = null,

        pub fn deinit(self: *CommandList, allocator: *std.mem.Allocator) void {
            self.and_or_cmd_list.deinit(allocator);
            allocator.destroy(self);
        }

        pub fn print(self: *CommandList) void {
            _ = self;
            std.debug.print("cmd_list is_async ({}) separator_pos ({}):\n", .{ self.is_async, self.separator_pos });
            self.and_or_cmd_list.print();
        }
    };

    pub const AndOrCmdList = struct {
        node: Node = .{ .kind = .AND_OR_LIST },
        kind: AndOrCmdListKind,

        pub const AndOrCmdListKind = enum(u1) {
            PIPELINE,
            BINARY_OP,

            pub fn Type(self: AndOrCmdListKind) type {
                return switch (self) {
                    .PIPELINE => Pipeline,
                    .BINARY_OP => BinaryOp,
                };
            }
        };

        pub fn cast(base: *AndOrCmdList, comptime and_or_list_kind: AndOrCmdListKind) ?*and_or_list_kind.Type() {
            if (base.kind == and_or_list_kind) {
                return @fieldParentPtr(and_or_list_kind.Type(), "and_or_cmd_list", base);
            }
            return null;
        }

        pub fn deinit(self: *AndOrCmdList, allocator: *std.mem.Allocator) void {
            if (self.cast(.PIPELINE)) |pipeline| {
                pipeline.deinit(allocator);
            } else if (self.cast(.BINARY_OP)) |binary_op| {
                binary_op.deinit(allocator);
            } else {
                unreachable;
            }
        }

        pub fn print(self: *AndOrCmdList) void {
            std.debug.print("   - ", .{});
            if (self.cast(.PIPELINE)) |pipeline| {
                pipeline.print();
            } else {
                unreachable;
            }
        }

        pub const Pipeline = struct {
            and_or_cmd_list: AndOrCmdList = .{ .kind = .PIPELINE },
            commands: []*Command,
            has_bang: bool = false,
            bang_pos: ?Position = null,

            pub fn deinit(self: *Pipeline, allocator: *std.mem.Allocator) void {
                for (self.commands) |cmd| {
                    cmd.deinit(allocator);
                }
                allocator.free(self.commands);
                allocator.destroy(self);
            }

            pub fn print(self: *Pipeline) void {
                std.debug.print("pipeline len ({}) has_bang ({}) bang_pos ({}):\n", .{ self.commands.len, self.has_bang, self.bang_pos });
                for (self.commands) |cmd| {
                    cmd.print();
                }
            }

            // TODO pipe positions between each command
        };

        pub const BinaryOp = struct {
            and_or_cmd_list: AndOrCmdList = .{ .kind = .BINARY_OP },
            left: *AndOrCmdList,
            right: *AndOrCmdList,
            op_range: Range,
            kind: BinaryOpKind,

            pub const BinaryOpKind = enum(u1) {
                AND,
                OR,
            };

            pub fn deinit(self: *BinaryOp, allocator: *std.mem.Allocator) void {
                self.left.deinit(allocator);
                allocator.destroy(self.left);
                self.right.deinit(allocator);
                allocator.destroy(self.right);
                allocator.destroy(self);
            }

            pub fn print(self: *BinaryOp) void {
                _ = self;
                unreachable;
            }
        };
    };

    pub const Command = struct {
        node: Node = .{ .kind = .COMMAND },
        kind: CommandKind,

        pub const CommandKind = enum {
            SIMPLE_COMMAND,
            // BRACE_GROUP,
            // SUBSHELL,
            // IF_CLAUSE,
            // FOR_CLAUSE,
            // LOOP_CLAUSE,
            // CASE_CLAUSE,
            // FUNCTION_CLAUSE,

            pub fn Type(self: CommandKind) type {
                return switch (self) {
                    .SIMPLE_COMMAND => SimpleCommand,
                };
            }
        };

        pub fn cast(base: *Command, comptime command_kind: CommandKind) ?*command_kind.Type() {
            if (base.kind == command_kind) {
                return @fieldParentPtr(command_kind.Type(), "command", base);
            }
            return null;
        }

        pub fn deinit(self: *Command, allocator: *std.mem.Allocator) void {
            if (self.cast(.SIMPLE_COMMAND)) |simple_command| {
                simple_command.deinit(allocator);
            } else {
                unreachable;
            }
        }

        pub fn print(self: *Command) void {
            std.debug.print("     - ", .{});
            if (self.cast(.SIMPLE_COMMAND)) |simple_command| {
                simple_command.print();
            } else {
                unreachable;
            }
        }

        pub const SimpleCommand = struct {
            command: Command = .{ .kind = .SIMPLE_COMMAND },
            name: ?*Word,
            args: ?[]*Word = null,
            io_redir: ?[]IORedir = null,
            assigns: ?[]Assign = null,

            pub fn isEmpty(self: *SimpleCommand) bool {
                return !(self.name != null or self.io_redir != null or self.assigns != null);
            }

            pub fn deinit(self: *SimpleCommand, allocator: *std.mem.Allocator) void {
                if (self.name) |word_name| allocator.destroy(word_name);
                if (self.args) |args| {
                    for (args) |arg| {
                        allocator.destroy(arg);
                    }
                    allocator.free(args);
                }
                // TODO deinit io_redir and assigns
                allocator.destroy(self);
            }

            pub fn print(self: *SimpleCommand) void {
                std.debug.print("simple_command:\n", .{});
                if (self.name) |word_name| {
                    word_name.print();
                }
                if (self.args) |args| {
                    for (args) |arg| {
                        arg.print();
                    }
                }
                // TODO others prints
            }
        };
    };
};

pub const Word = struct {
    node: Node = .{ .kind = .WORD },
    kind: WordKind,

    pub const WordKind = enum {
        STRING,
        // PARAMETER,
        // COMMAND,
        // ARITHMETIC,
        // LIST,

        pub fn Type(self: WordKind) type {
            return switch (self) {
                .STRING => WordString,
                // .PARAMETER => WordParameter,
                // .COMMAND => WordCommand, // is there right?
                // .ARITHMETIC => WordArith,
                // .LIST => WordList,
            };
        }
    };

    pub fn cast(base: *Word, comptime word_kind: WordKind) ?*word_kind.Type() {
        if (base.kind == word_kind) {
            return @fieldParentPtr(word_kind.Type(), "word", base);
        }
        return null;
    }

    pub fn print(self: *Word) void {
        std.debug.print("         ", .{});
        if (self.cast(.STRING)) |word_string| {
            word_string.print();
        } else {
            unreachable;
        }
    }

    pub const WordString = struct {
        word: Word = .{ .kind = .STRING },
        str: []const u8,
        is_single_quoted: bool = false,
        range: Range,

        pub fn print(self: *WordString) void {
            std.debug.print("word_string {s}  is_single_quoted ({}) range ({})\n", .{ self.str, self.is_single_quoted, self.range });
        }
    };
};

pub const IORedir = struct {
    io_num: i8 = -1,
    name: Word,
    here_doc: ?[]Word = null,
    io_num_pos: ?Position = null,
    op_range: Range,
    op: IORedirKind,

    pub const IORedirKind = enum {
        IO_LESS,
        IO_DLESS,
        IO_LESSAND,
        IO_DLESSDASH,
        IO_LESSGREAT,
        IO_GREAT,
        IO_DGREAT,
        IO_GREATAND,
        IO_CLOBBER,
    };
};

pub const Assign = struct {
    name: []const u8, // why not word?
    value: Word,
    name_range: Range,
    equal_pos: Position,
};
