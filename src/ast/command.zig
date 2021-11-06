const std = @import("std");
const mem = std.mem;
const ast = @import("../ast.zig");
const CommandList = ast.CommandList;
const Word = @import("word.zig").Word;
const esc = "\x1B";
const csi = esc ++ "[";

/// Command representation
pub const Command = struct {
    impl: *c_void,
    kind: CommandKind,
    deinitFn: fn (*c_void, *mem.Allocator) void,
    printFn: fn (*c_void, usize) void,

    /// Command type representation
    pub const CommandKind = enum {
        SIMPLE_COMMAND,
        CMD_GROUP,
        IF_DECL,

        pub fn Type(self: CommandKind) type {
            return switch (self) {
                .SIMPLE_COMMAND => SimpleCommand,
                .CMD_GROUP => CmdGroup,
                .IF_DECL => IfDecl,
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

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(cmd: *const Command, allocator: *mem.Allocator) void {
        cmd.deinitFn(cmd.impl, allocator);
    }

    /// Prints the Simple Command representation
    pub fn print(cmd: *const Command, spacing: usize) void {
        cmd.printFn(cmd.impl, spacing);
    }
};

/// Simple Command representation
pub const SimpleCommand = struct {
    name: ?Word,
    args: ?[]Word = null,
    io_redirs: ?[]IORedir = null,
    assigns: ?[]Assign = null,

    pub fn cmd(self: *SimpleCommand) Command {
        return .{ .impl = self, .kind = .SIMPLE_COMMAND, .deinitFn = deinit, .printFn = print };
    }

    pub fn create(allocator: *mem.Allocator, simple_command: SimpleCommand) !Command {
        const simple_cmd = try allocator.create(SimpleCommand);
        simple_cmd.* = simple_command;
        return simple_cmd.cmd();
    }

    pub fn deinit(self_void: *c_void, allocator: *mem.Allocator) void {
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

    pub fn print(self_void: *c_void, spacing: usize) void {
        var self = @ptrCast(*SimpleCommand, @alignCast(@alignOf(SimpleCommand), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("simple_command:\n", .{});
        if (self.io_redirs) |io_redirects| {
            for (io_redirects) |io_redir| {
                std.debug.print(csi ++ "{}C", .{spacing + 2});
                std.debug.print("io_redir op: {} name: {s} io_num: {} op_range: {}\n", .{ io_redir.op, io_redir.name.cast(Word.WordKind.STRING).?.str, io_redir.io_num, io_redir.op_range });
                // TODO fix this
            }
        }
        if (self.assigns) |assignments| {
            for (assignments) |assign| {
                std.debug.print(csi ++ "{}C", .{spacing + 2});
                std.debug.print("assign name: {s} ({})  value:\n", .{ assign.name, assign.name_range });
                if (assign.value) |v| {
                    v.print(spacing + 2);
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

pub const CmdGroup = struct {
    body: []*CommandList,
    kind: GroupKind,

    pub const GroupKind = enum {
        BRACE_GROUP,
        SUBSHELL,
    };

    pub fn cmd(self: *CmdGroup) Command {
        return .{ .impl = self, .kind = .CMD_GROUP, .deinitFn = deinit, .printFn = print };
    }

    pub fn create(allocator: *mem.Allocator, cgroup: CmdGroup) !Command {
        const cmd_group = try allocator.create(CmdGroup);
        cmd_group.* = cgroup;
        return cmd_group.cmd();
    }

    pub fn deinit(self_void: *c_void, allocator: *mem.Allocator) void {
        const self = @ptrCast(*CmdGroup, @alignCast(@alignOf(CmdGroup), self_void));
        for (self.body) |cmd_list| {
            cmd_list.deinit(allocator);
        }
        allocator.destroy(self);
    }
    pub fn print(self_void: *c_void, spacing: usize) void {
        const self = @ptrCast(*CmdGroup, @alignCast(@alignOf(CmdGroup), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("{}:\n", .{self.kind});
        for (self.body) |cmd_list| {
            cmd_list.print(spacing + 2);
        }
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("end\n", .{});
    }
};

pub const IfDecl = struct {
    condition: []*CommandList,
    body: []*CommandList,
    else_decl: ?Command,

    pub fn cmd(self: *IfDecl) Command {
        return .{ .impl = self, .kind = .IF_DECL, .deinitFn = deinit, .printFn = print };
    }

    pub fn create(allocator: *mem.Allocator, ifdecl: IfDecl) !Command {
        const if_decl = try allocator.create(IfDecl);
        if_decl.* = ifdecl;
        return if_decl.cmd();
    }

    pub fn deinit(self_void: *c_void, allocator: *mem.Allocator) void {
        const self = @ptrCast(*IfDecl, @alignCast(@alignOf(IfDecl), self_void));
        for (self.condition) |cmd_list| {
            cmd_list.deinit(allocator);
        }
        allocator.free(self.condition);
        for (self.body) |cmd_list| {
            cmd_list.deinit(allocator);
        }
        allocator.free(self.body);
        if (self.else_decl) |else_part| else_part.deinit(allocator);
        allocator.destroy(self);
    }
    pub fn print(self_void: *c_void, spacing: usize) void {
        const self = @ptrCast(*IfDecl, @alignCast(@alignOf(IfDecl), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("if condition:\n", .{});
        for (self.condition) |cond| {
            cond.print(spacing + 5);
        }
        std.debug.print(csi ++ "{}C", .{spacing + 3});
        std.debug.print("body:\n", .{});
        for (self.body) |cmd_list| {
            cmd_list.print(spacing + 5);
        }
        std.debug.print(csi ++ "{}C", .{spacing + 3});
        std.debug.print("else ", .{});
        if (self.else_decl) |else_part| {
            std.debug.print("\n", .{});
            else_part.print(spacing + 5);
        } else {
            std.debug.print("null\n", .{});
        }
    }
};

/// Assignment representation, name=value
pub const Assign = struct {
    name: []const u8,
    value: ?Word,
    name_range: ast.Range,
    equal_pos: ast.Position,

    pub fn create(allocator: *mem.Allocator, assign: Assign) !*Assign {
        const assignment = try allocator.create(Assign);
        assignment.* = assign;
        return assignment;
    }
};

/// Input/Output Redirection representation
pub const IORedir = struct {
    io_num: ?u8 = null,
    name: Word,
    here_doc: ?[]Word = null,
    io_num_pos: ?ast.Position = null,
    op_range: ast.Range,
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

    pub fn create(allocator: *mem.Allocator, io_redir: IORedir) !*IORedir {
        const io_redirection = try allocator.create(IORedir);
        io_redirection.* = io_redir;
        return io_redirection;
    }
};
