//! this module provides an AST for command parsing of kzh shell
// TODO proper printer struct, so it can be properly printed

const std = @import("std");

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

    // TODO consider having an allocator to all its child
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
            std.debug.print("\nprogram\n", .{});
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
            } else if (self.cast(.BINARY_OP)) |binary_op| {
                binary_op.print();
            } else {
                unreachable;
            }
        }

        pub const Pipeline = struct {
            and_or_cmd_list: AndOrCmdList = .{ .kind = .PIPELINE },
            commands: []*Command,
            has_bang: bool,
            bang_pos: ?Position,

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
                self.right.deinit(allocator);
                allocator.destroy(self);
            }

            pub fn print(self: *BinaryOp) void {
                std.debug.print("binary_op ({}) {} left: {} right: {}\n", .{ self.op_range, self.kind, self.left, self.right });
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
            io_redirs: ?[]*IORedir = null,
            assigns: ?[]*Assign = null,

            pub fn isEmpty(self: *SimpleCommand) bool {
                return self.name == null and self.io_redirs == null and self.assigns == null;
            }

            pub fn deinit(self: *SimpleCommand, allocator: *std.mem.Allocator) void {
                // TODO proper word deinit
                if (self.name) |word_name| allocator.destroy(word_name);
                if (self.args) |args| {
                    for (args) |arg| {
                        allocator.destroy(arg);
                    }
                    allocator.free(args);
                }
                if (self.assigns) |assignments| {
                    for (assignments) |assign| {
                        // TODO deinit word
                        if (assign.value) |val| allocator.destroy(val);
                        allocator.destroy(assign);
                    }
                    allocator.free(assignments);
                }
                if (self.io_redirs) |io_redirects| {
                    for (io_redirects) |io_redir| {
                        // TODO proper io_redir deinit
                        allocator.destroy(io_redir.name);
                        allocator.destroy(io_redir);
                    }
                    allocator.free(io_redirects);
                }
                allocator.destroy(self);
            }

            pub fn print(self: *SimpleCommand) void {
                std.debug.print("simple_command:\n", .{});
                if (self.io_redirs) |io_redirects| {
                    for (io_redirects) |io_redir| {
                        _ = io_redir;
                        std.debug.print("         io_redir op: {} name: {s} io_num: {} op_range: {}\n", .{ io_redir.op, io_redir.name.cast(Word.WordKind.STRING).?.str, io_redir.io_num, io_redir.op_range });
                    }
                }
                if (self.assigns) |assignments| {
                    for (assignments) |assign| {
                        std.debug.print("         assign name: {s} ({})  value:\n   ", .{ assign.name, assign.name_range });
                        if (assign.value) |v| {
                            v.print();
                        } else {
                            std.debug.print("\"\"", .{});
                        }
                        std.debug.print("\n", .{});
                    }
                }
                if (self.name) |word_name| {
                    word_name.print();
                }
                if (self.args) |args| {
                    for (args) |arg| {
                        arg.print();
                    }
                }
            }
        };
    };
};

pub const Word = struct {
    node: Node = .{ .kind = .WORD },
    kind: WordKind,

    pub const WordKind = enum {
        STRING,
        PARAMETER,
        COMMAND,
        ARITHMETIC,
        LIST,

        pub fn Type(self: WordKind) type {
            return switch (self) {
                .STRING => WordString,
                .PARAMETER => WordParameter,
                .COMMAND => WordCommand,
                .ARITHMETIC => WordArithm,
                .LIST => WordList,
            };
        }
    };

    pub fn cast(base: *Word, comptime word_kind: WordKind) ?*word_kind.Type() {
        if (base.kind == word_kind) {
            return @fieldParentPtr(word_kind.Type(), "word", base);
        }
        return null;
    }

    pub fn deinit(self: *Word, allocator: *std.mem.Allocator) void {
        if (self.cast(.STRING)) |word_string| {
            word_string.deinit(allocator);
        } else if (self.cast(.COMMAND)) |word_command| {
            word_command.deinit(allocator);
        } else if (self.cast(.PARAMETER)) |word_parameter| {
            word_parameter.deinit(allocator);
        } else if (self.cast(.ARITHMETIC)) |word_arithm| {
            word_arithm.deinit(allocator);
        } else if (self.cast(.LIST)) |word_list| {
            word_list.deinit(allocator);
        } else {
            unreachable;
        }
    }

    pub fn print(self: *Word) void {
        std.debug.print("         ", .{});
        if (self.cast(.STRING)) |word_string| {
            word_string.print();
        } else if (self.cast(.COMMAND)) |word_command| {
            word_command.print();
        } else if (self.cast(.PARAMETER)) |word_parameter| {
            word_parameter.print();
        } else if (self.cast(.ARITHMETIC)) |word_arithm| {
            word_arithm.print();
        } else if (self.cast(.LIST)) |word_list| {
            word_list.print();
        } else {
            unreachable;
        }
    }

    pub const WordString = struct {
        word: Word = .{ .kind = .STRING },
        str: []const u8,
        is_single_quoted: bool = false,
        range: ?Range = null,

        pub fn deinit(self: *WordString, allocator: *std.mem.Allocator) void {
            allocator.destroy(self);
        }

        pub fn print(self: *WordString) void {
            std.debug.print("word_string {s}  is_single_quoted ({}) range ({})\n", .{ self.str, self.is_single_quoted, self.range });
        }
    };

    pub const WordParameter = struct {
        word: Word = .{ .kind = .PARAMETER },
        name: []const u8,
        op: ParameterOperation,
        /// only used when '-', '=', '?' and '+'
        has_colon: bool,
        arg: ?*Word = null,
        // TODO add positions

        pub const ParameterOperation = enum {
            PARAMETER_MINUS, // ${name:-[arg]}, arg is the default value
            PARAMETER_EQUAL, // ${name:=[arg]}, assign default value (arg)
            PARAMETER_PLUS, // ${name:+[arg]}, use alternative value
            PARAMETER_MAYBE, // ${name:?[arg]}, error if empty or undefined
            PARAMETER_LEADING_HASH, // ${#name}, string lenght of name
            PARAMETER_HASH, // ${name#[arg]}, remove smallest prefix pattern
            PARAMETER_DOUBLE_HASH, // ${name##[arg]}, remove largest prefix pattern
            PARAMETER_PERCENT, // ${name%[arg]}, remove smallest suffix pattern
            PARAMETER_DOUBLE_PERCENT, // ${name%%[arg]}, remove lagest suffix pattern
            PARAMETER_NO_OP, //$name , ${name}, no operation
        };

        pub fn deinit(self: *WordParameter, allocator: *std.mem.Allocator) void {
            if (self.arg) |word_arg| {
                word_arg.deinit(allocator);
            }
            allocator.destroy(self);
        }

        pub fn print(self: *WordParameter) void {
            std.debug.print("word_param ({}) name: {s} has_colon ({}) arg:", .{ self.op, self.name, self.has_colon });
            if (self.arg) |word_arg| {
                std.debug.print("\n   ", .{});
                word_arg.print();
            } else {
                std.debug.print(" null\n", .{});
            }
        }
    };

    pub const WordCommand = struct {
        word: Word = .{ .kind = .COMMAND },
        program: ?*Node.Program,
        is_back_quoted: bool,
        range: Range,

        pub fn deinit(self: *WordCommand, allocator: *std.mem.Allocator) void {
            if (self.program) |prog| {
                prog.deinit(allocator);
            }
            allocator.destroy(self);
        }

        pub fn print(self: *WordCommand) void {
            std.debug.print("word_command ({}) is_back_quoted: {}\n", .{ self.range, self.is_back_quoted });
            if (self.program) |prog| {
                std.debug.print("              program: ", .{});
                prog.print();
            }
        }
    };

    pub const WordArithm = struct {
        word: Word = .{ .kind = .ARITHMETIC },
        body: *Word,

        pub fn deinit(self: *WordArithm, allocator: *std.mem.Allocator) void {
            self.body.deinit(allocator);
            allocator.destroy(self);
        }

        pub fn print(self: *WordArithm) void {
            std.debug.print("word_arithm ({}) body:\n   ", .{self});
            self.body.print();
        }
    };

    pub const WordList = struct {
        word: Word = .{ .kind = .LIST },
        items: []*Word,
        is_double_quoted: bool,
        left_quote_pos: ?Position = null,
        right_quote_pos: ?Position = null,

        pub fn deinit(self: *WordList, allocator: *std.mem.Allocator) void {
            for (self.items) |item| {
                item.denit(allocator);
            }
            allocator.free(self.items);
            allocator.destroy(self);
        }

        pub fn print(self: *WordList) void {
            std.debug.print("word_list ({}) is_double_quoted: {} left_quote ({}) right_quote ({}):\n", .{ self.items.len, self.is_double_quoted, self.left_quote_pos, self.right_quote_pos });
            for (self.items) |item| {
                item.print();
            }
            std.debug.print("end_word_list\n", .{});
        }
    };
};

pub const IORedir = struct {
    io_num: ?u8 = null,
    name: *Word,
    here_doc: ?[]*Word = null,
    io_num_pos: ?Position = null,
    op_range: Range,
    op: IORedirKind,

    usingnamespace IORedirKind;

    pub const IORedirKind = enum {
        IO_LESS, // <
        IO_DOUBLE_LESS, // <<
        IO_LESS_AND, // <&
        IO_DOUBLE_LESS_DASH, // <<-
        IO_LESS_GREAT, // <>
        IO_GREAT, // >
        IO_DOUBLE_GREAT, // >>
        IO_GREAT_AND, // >&
        IO_CLOBBER, // >|
    };
};

pub const Assign = struct {
    name: []const u8,
    value: ?*Word,
    name_range: Range,
    equal_pos: Position,
};
