const std = @import("std");
const mem = std.mem;
const ast = @import("../ast.zig");
const CommandList = ast.CommandList;
const Word = @import("word.zig").Word;
const esc = "\x1B";
const csi = esc ++ "[";

/// Command representation
pub const Command = struct {
    impl: *anyopaque,
    kind: CommandKind,
    deinitFn: *const fn (*anyopaque, mem.Allocator) void,

    /// Command type representation
    pub const CommandKind = enum {
        SIMPLE_COMMAND,
        CMD_GROUP,
        IF_DECL,
        FOR_DECL,
        LOOP_DECL,
        CASE_DECL,
        FUNC_DECL,

        pub fn Type(comptime self: CommandKind) type {
            return switch (self) {
                .SIMPLE_COMMAND => SimpleCommand,
                .CMD_GROUP => CmdGroup,
                .IF_DECL => IfDecl,
                .FOR_DECL => ForDecl,
                .LOOP_DECL => LoopDecl,
                .CASE_DECL => CaseDecl,
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
    pub fn deinit(cmd: *const Command, allocator: mem.Allocator) void {
        cmd.deinitFn(cmd.impl, allocator);
    }
};

/// Simple Command representation
pub const SimpleCommand = struct {
    name: ?Word,
    args: ?[]Word = null,
    io_redirs: ?[]IORedir = null,
    assigns: ?[]Assign = null,

    pub fn cmd(self: *SimpleCommand) Command {
        return .{
            .impl = self,
            .kind = .SIMPLE_COMMAND,
            .deinitFn = deinit,
        };
    }

    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        const self = @ptrCast(*SimpleCommand, @alignCast(@alignOf(SimpleCommand), self_void));
        defer allocator.destroy(self);

        if (self.name) |word_name| {
            word_name.deinit(allocator);
        }
        if (self.args) |args| {
            defer allocator.free(args);
            for (args) |arg| {
                arg.deinit(allocator);
            }
        }
        if (self.assigns) |assignments| {
            defer allocator.free(assignments);
            for (assignments) |assign| {
                if (assign.value) |val| {
                    val.deinit(allocator);
                }
            }
        }
        if (self.io_redirs) |io_redirects| {
            defer allocator.free(io_redirects);
            for (io_redirects) |io_redir| {
                io_redir.name.deinit(allocator);
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
    body: []CommandList,
    kind: GroupKind,

    pub const GroupKind = enum {
        BRACE_GROUP,
        SUBSHELL,
    };

    pub fn cmd(self: *CmdGroup) Command {
        return .{
            .impl = self,
            .kind = .CMD_GROUP,
            .deinitFn = deinit,
        };
    }

    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        const self = @ptrCast(*CmdGroup, @alignCast(@alignOf(CmdGroup), self_void));
        defer {
            allocator.free(self.body);
            allocator.destroy(self);
        }

        for (self.body) |cmd_list| {
            cmd_list.deinit(allocator);
        }
    }
};

pub const IfDecl = struct {
    condition: []CommandList,
    body: []CommandList,
    else_decl: ?Command,

    pub fn cmd(self: *IfDecl) Command {
        return .{
            .impl = self,
            .kind = .IF_DECL,
            .deinitFn = deinit,
        };
    }

    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        const self = @ptrCast(*IfDecl, @alignCast(@alignOf(IfDecl), self_void));
        defer {
            allocator.free(self.condition);
            allocator.free(self.body);
            allocator.destroy(self);
        }

        for (self.condition) |cmd_list| {
            cmd_list.deinit(allocator);
        }
        for (self.body) |cmd_list| {
            cmd_list.deinit(allocator);
        }
        if (self.else_decl) |else_part| else_part.deinit(allocator);
    }
};

pub const ForDecl = struct {
    name: []const u8,
    has_in: bool,
    is_selection: bool,
    list: ?[]Word,
    body: []CommandList,

    pub fn cmd(self: *ForDecl) Command {
        return .{
            .impl = self,
            .kind = .FOR_DECL,
            .deinitFn = deinit,
        };
    }

    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        const self = @ptrCast(*ForDecl, @alignCast(@alignOf(ForDecl), self_void));
        defer {
            allocator.free(self.body);
            allocator.destroy(self);
        }
        if (self.list) |word_list| {
            defer allocator.free(word_list);
            for (word_list) |word| {
                word.deinit(allocator);
            }
        }
        for (self.body) |cmd_list| {
            cmd_list.deinit(allocator);
        }
    }
};

pub const LoopDecl = struct {
    kind: LoopKind,
    condition: []CommandList,
    body: []CommandList,

    pub const LoopKind = enum {
        WHILE,
        UNTIL,
    };

    pub fn cmd(self: *LoopDecl) Command {
        return .{
            .impl = self,
            .kind = .LOOP_DECL,
            .deinitFn = deinit,
        };
    }

    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        const self = @ptrCast(*LoopDecl, @alignCast(@alignOf(LoopDecl), self_void));
        defer {
            allocator.free(self.body);
            allocator.free(self.condition);
            allocator.destroy(self);
        }
        for (self.condition) |cmd_list| {
            cmd_list.deinit(allocator);
        }
        for (self.body) |cmd_list| {
            cmd_list.deinit(allocator);
        }
    }
};

pub const CaseDecl = struct {
    word: Word,
    items: []CaseItem,

    pub const CaseItem = struct {
        patterns: []Word,
        body: []CommandList,
    };

    pub fn cmd(self: *CaseDecl) Command {
        return .{
            .impl = self,
            .kind = .CASE_DECL,
            .deinitFn = deinit,
        };
    }

    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        const self = @ptrCast(*CaseDecl, @alignCast(@alignOf(CaseDecl), self_void));
        defer {
            allocator.free(self.items);
            allocator.destroy(self);
        }

        self.word.deinit(allocator);
        for (self.items) |item| {
            for (item.patterns) |pattern| {
                pattern.deinit(allocator);
            }
            allocator.free(item.patterns);
            for (item.body) |cmd_list| {
                cmd_list.deinit(allocator);
            }
            allocator.free(item.body);
        }
    }
};

pub const FuncDecl = struct {
    /// owner
    name: []const u8,
    body: Command,
    io_redirs: ?[]IORedir = null,

    pub fn cmd(self: *FuncDecl) Command {
        return .{
            .impl = self,
            .kind = .FUNC_DECL,
            .deinitFn = deinit,
        };
    }

    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        const self = @ptrCast(*FuncDecl, @alignCast(@alignOf(FuncDecl), self_void));
        defer {
            allocator.free(self.name);
            allocator.destroy(self);
        }
        self.body.deinit(allocator);
        if (self.io_redirs) |io_redirects| {
            defer allocator.free(io_redirects);
            for (io_redirects) |redir| {
                redir.name.deinit(allocator);
            }
        }
    }
};

/// Assignment representation, name=value
pub const Assign = struct {
    name: []const u8,
    value: ?Word,
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
};
