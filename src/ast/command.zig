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
        FOR_DECL,
        LOOP_DECL,
        FUNC_DECL,

        pub fn Type(self: CommandKind) type {
            return switch (self) {
                .SIMPLE_COMMAND => SimpleCommand,
                .CMD_GROUP => CmdGroup,
                .IF_DECL => IfDecl,
                .FOR_DECL => ForDecl,
                .LOOP_DECL => LoopDecl,
                .FUNC_DECL => FuncDecl,
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
                std.debug.print("io_redir op: {} name: {s} io_num: {}\n", .{ io_redir.op, io_redir.name.cast(Word.WordKind.STRING).?.str, io_redir.io_num });
                // TODO fix this
            }
        }
        if (self.assigns) |assignments| {
            for (assignments) |assign| {
                std.debug.print(csi ++ "{}C", .{spacing + 2});
                std.debug.print("assign name: {s}  value:\n", .{assign.name});
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

pub const ForDecl = struct {
    name: []const u8,
    has_in: bool,
    list: ?[]Word,
    body: []*CommandList,

    pub fn cmd(self: *ForDecl) Command {
        return .{ .impl = self, .kind = .FOR_DECL, .deinitFn = deinit, .printFn = print };
    }

    pub fn create(allocator: *mem.Allocator, fordecl: ForDecl) !Command {
        const for_decl = try allocator.create(ForDecl);
        for_decl.* = fordecl;
        return for_decl.cmd();
    }

    pub fn deinit(self_void: *c_void, allocator: *mem.Allocator) void {
        const self = @ptrCast(*ForDecl, @alignCast(@alignOf(ForDecl), self_void));
        if (self.list) |word_list| {
            for (word_list) |word| {
                word.deinit(allocator);
            }
            allocator.free(word_list);
        }
        for (self.body) |cmd_list| {
            cmd_list.deinit(allocator);
        }
        allocator.free(self.body);
        allocator.destroy(self);
    }
    pub fn print(self_void: *c_void, spacing: usize) void {
        const self = @ptrCast(*ForDecl, @alignCast(@alignOf(ForDecl), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("FOR list:", .{});
        if (self.list) |word_list| {
            std.debug.print("\n", .{});
            for (word_list) |word_value| {
                word_value.print(spacing + 2);
            }
        } else {
            std.debug.print(" null\n", .{});
        }
        std.debug.print(csi ++ "{}C", .{spacing + 4});
        std.debug.print("body:\n", .{});
        for (self.body) |cmd_list| {
            cmd_list.print(spacing + 6);
        }
    }
};

pub const LoopDecl = struct {
    kind: LoopKind,
    condition: []*CommandList,
    body: []*CommandList,

    pub const LoopKind = enum {
        WHILE,
        UNTIL,
    };
    pub fn cmd(self: *LoopDecl) Command {
        return .{ .impl = self, .kind = .LOOP_DECL, .deinitFn = deinit, .printFn = print };
    }

    pub fn create(allocator: *mem.Allocator, loopdecl: LoopDecl) !Command {
        const loop_decl = try allocator.create(LoopDecl);
        loop_decl.* = loopdecl;
        return loop_decl.cmd();
    }

    pub fn deinit(self_void: *c_void, allocator: *mem.Allocator) void {
        const self = @ptrCast(*LoopDecl, @alignCast(@alignOf(LoopDecl), self_void));
        for (self.condition) |cmd_list| {
            cmd_list.deinit(allocator);
        }
        allocator.free(self.condition);
        for (self.body) |cmd_list| {
            cmd_list.deinit(allocator);
        }
        allocator.free(self.body);
        allocator.destroy(self);
    }

    pub fn print(self_void: *c_void, spacing: usize) void {
        const self = @ptrCast(*LoopDecl, @alignCast(@alignOf(LoopDecl), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("{}:\n", .{self.kind});
        std.debug.print(csi ++ "{}C", .{spacing + 2});
        std.debug.print("condition:\n", .{});
        for (self.condition) |cond| {
            cond.print(spacing + 4);
        }
        std.debug.print(csi ++ "{}C", .{spacing + 2});
        std.debug.print("body:\n", .{});
        for (self.body) |cmd_list| {
            cmd_list.print(spacing + 4);
        }
    }
};

pub const FuncDecl = struct {
    /// owner
    name: []const u8,
    body: Command,
    io_redirs: ?[]IORedir = null,

    pub fn cmd(self: *FuncDecl) Command {
        return .{ .impl = self, .kind = .FUNC_DECL, .deinitFn = deinit, .printFn = print };
    }

    pub fn create(allocator: *mem.Allocator, func_decl: FuncDecl) !Command {
        const func = try allocator.create(FuncDecl);
        func.* = func_decl;
        func.name = try allocator.dupe(u8, func.name);
        return func.cmd();
    }

    pub fn deinit(self_void: *c_void, allocator: *mem.Allocator) void {
        const self = @ptrCast(*FuncDecl, @alignCast(@alignOf(FuncDecl), self_void));
        self.body.deinit(allocator);
        if (self.io_redirs) |io_redirects| {
            for (io_redirects) |redir| {
                redir.name.deinit(allocator);
            }
            allocator.free(io_redirects);
        }
        allocator.free(self.name);
        allocator.destroy(self);
    }

    pub fn print(self_void: *c_void, spacing: usize) void {
        const self = @ptrCast(*FuncDecl, @alignCast(@alignOf(FuncDecl), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("func ({s} io_redirs ({s}) cmd:\n", .{ self.name, self.io_redirs });
        self.body.print(spacing + 2);
    }
};

/// Assignment representation, name=value
pub const Assign = struct {
    name: []const u8,
    value: ?Word,

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
