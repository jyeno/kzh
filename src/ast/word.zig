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
    deinitFn: fn (*anyopaque, mem.Allocator) void,

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

    pub fn cast(word: *const Word, comptime word_kind: WordKind) ?*word_kind.Type() {
        if (word.kind == word_kind) {
            return @ptrCast(*word_kind.Type(), @alignCast(@alignOf(word_kind.Type()), word.impl));
        } else {
            return null;
        }
    }

    pub fn deinit(word: *const Word, allocator: mem.Allocator) void {
        word.deinitFn(word.impl, allocator);
    }
};

/// Word String representation
pub const WordString = struct {
    /// string slice
    str: []const u8,
    /// used to determine if the string should be expanded or not
    is_single_quoted: bool = false,

    pub fn word(self: *WordString) Word {
        return .{ .impl = self, .kind = .STRING, .deinitFn = deinit };
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        var self = @ptrCast(*WordString, @alignCast(@alignOf(WordString), self_void));
        allocator.destroy(self);
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
            .deinitFn = deinit,
        };
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        const self = @ptrCast(*WordParameter, @alignCast(@alignOf(WordParameter), self_void));
        defer allocator.destroy(self);

        if (self.arg) |word_arg| {
            word_arg.deinit(allocator);
        }
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
            .deinitFn = deinit,
        };
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        var self = @ptrCast(*WordCommand, @alignCast(@alignOf(WordCommand), self_void));
        if (self.program) |prog| {
            prog.deinit(allocator);
        }
        allocator.destroy(self);
    }
};

/// Word Arithmetic representation
pub const WordArithm = struct {
    body: Word,

    pub fn word(self: *WordArithm) Word {
        return .{
            .impl = self,
            .kind = .ARITHMETIC,
            .deinitFn = deinit,
        };
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        var self = @ptrCast(*WordArithm, @alignCast(@alignOf(WordArithm), self_void));
        self.body.deinit(allocator);
        allocator.destroy(self);
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
            .deinitFn = deinit,
        };
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *anyopaque, allocator: mem.Allocator) void {
        var self = @ptrCast(*WordList, @alignCast(@alignOf(WordList), self_void));
        for (self.items) |item| {
            item.deinit(allocator);
        }
        allocator.free(self.items);
        allocator.destroy(self);
    }
};
