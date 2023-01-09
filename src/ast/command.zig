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
        };
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
        };
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
        };
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
        };
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
        };
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
        };
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
        };
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
