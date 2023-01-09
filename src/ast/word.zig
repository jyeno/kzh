const std = @import("std");
const mem = std.mem;
const ast = @import("../ast.zig");
const Program = ast.Program;

const esc = "\x1B";
const csi = esc ++ "[";

// make it file
// TODO way to convert to its kind?
pub const Word = struct {
    impl: *anyopaque,
    kind: WordKind,

    pub const WordKind = enum {
        STRING,
        PARAMETER,
        COMMAND,
        ARITHMETIC,
        LIST,

        pub fn Type(comptime self: WordKind) type {
            return switch (self) {
                .STRING => WordString,
                .PARAMETER => WordParameter,
                .COMMAND => WordCommand,
                .ARITHMETIC => WordArithm,
                .LIST => WordList,
            };
        }
    };

    pub fn cast(word: *const Word, comptime word_kind: WordKind) ?*word_kind.Type() {
        if (word.kind == word_kind) {
            return @ptrCast(*word_kind.Type(), @alignCast(@alignOf(word_kind.Type()), word.impl));
        } else {
            return null;
        }
    }
};

/// Word String representation
pub const WordString = struct {
    /// string slice
    str: []const u8,
    /// used to determine if the string should be expanded or not
    is_single_quoted: bool = false,

    pub fn word(self: *WordString) Word {
        return .{
            .impl = self,
            .kind = .STRING,
        };
    }
};

/// Word Parameter representation
pub const WordParameter = struct {
    /// Name of the word
    name: []const u8,
    /// Operation type
    op: ParameterOperation = .PARAMETER_NO_OP,
    /// Only used on '-', '=', '?' and '+' operations
    has_colon: bool = false,
    /// Optional args that are used by some operations
    arg: ?Word = null,

    /// Representation of the parameters operations
    pub const ParameterOperation = enum {
        /// ${name:-[arg]}, arg is the default value
        PARAMETER_MINUS,
        /// ${name:=[arg]}, assign default value (arg)
        PARAMETER_ASSIGN,
        /// ${name:+[arg]}, use alternative value
        PARAMETER_PLUS,
        /// ${name:?[arg]}, error if empty or undefined
        PARAMETER_MAYBE,
        /// ${#name}, string lenght of name
        PARAMETER_LEADING_HASH,
        /// ${name#[arg]}, remove smallest prefix pattern
        PARAMETER_HASH,
        /// ${name##[arg]}, remove largest prefix pattern
        PARAMETER_DOUBLE_HASH,
        /// ${name%[arg]}, remove smallest suffix pattern
        PARAMETER_PERCENT,
        /// ${name%%[arg]}, remove lagest suffix pattern
        PARAMETER_DOUBLE_PERCENT,
        ///$name , ${name}, no operation
        PARAMETER_NO_OP,
    };

    pub fn word(self: *WordParameter) Word {
        return .{
            .impl = self,
            .kind = .PARAMETER,
        };
    }
};

/// Word Command representation, $(program) or `program`
pub const WordCommand = struct {
    program: ?*Program,
    is_back_quoted: bool,

    pub fn word(self: *WordCommand) Word {
        return .{
            .impl = self,
            .kind = .COMMAND,
        };
    }
};

/// Word Arithmetic representation
pub const WordArithm = struct {
    body: Word,

    pub fn word(self: *WordArithm) Word {
        return .{
            .impl = self,
            .kind = .ARITHMETIC,
        };
    }
};

/// Word List representation
pub const WordList = struct {
    items: []Word,
    is_double_quoted: bool,

    pub fn word(self: *WordList) Word {
        return .{
            .impl = self,
            .kind = .LIST,
        };
    }
};
