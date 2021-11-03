const std = @import("std");
const ast = @import("../ast.zig");
const Program = ast.Program;
const Range = ast.Range;
const Position = ast.Position;

const esc = "\x1B";
const csi = esc ++ "[";

// make it file
// TODO way to convert to its kind?
pub const Word = struct {
    impl: *c_void,
    kind: WordKind,
    deinitFn: fn (*c_void, *std.mem.Allocator) void,
    printFn: fn (*c_void, usize) void,

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

    pub fn deinit(word: *const Word, allocator: *std.mem.Allocator) void {
        word.deinitFn(word.impl, allocator);
    }

    pub fn print(word: *const Word, spacing: usize) void {
        word.printFn(word.impl, spacing);
    }
};

/// Word String representation
pub const WordString = struct {
    /// string slice
    str: []const u8,
    /// used to determine if the string should be expanded or not
    is_single_quoted: bool = false,
    /// Position of the string
    range: ?Range = null,

    pub fn word(self: *WordString) Word {
        return .{ .impl = self, .kind = .STRING, .deinitFn = deinit, .printFn = print };
    }

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, word_string: WordString) !Word {
        const word_str = try allocator.create(WordString);
        word_str.* = word_string;
        return word_str.word();
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *std.mem.Allocator) void {
        var self = @ptrCast(*WordString, @alignCast(@alignOf(WordString), self_void));
        allocator.destroy(self);
    }

    /// Prints the Word String representation
    pub fn print(self_void: *c_void, spacing: usize) void {
        var self = @ptrCast(*WordString, @alignCast(@alignOf(WordString), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("word_string {s}  is_single_quoted ({}) range ({})\n", .{ self.str, self.is_single_quoted, self.range });
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
    // TODO add positions

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
        return .{ .impl = self, .kind = .PARAMETER, .deinitFn = deinit, .printFn = print };
    }

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, word_param: WordParameter) !Word {
        const word_param_expr = try allocator.create(WordParameter);
        word_param_expr.* = word_param;
        return word_param_expr.word();
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *std.mem.Allocator) void {
        var self = @ptrCast(*WordParameter, @alignCast(@alignOf(WordParameter), self_void));
        if (self.arg) |word_arg| {
            word_arg.deinit(allocator);
        }
        allocator.destroy(self);
    }

    /// Prints the Word Parameter representation
    pub fn print(self_void: *c_void, spacing: usize) void {
        var self = @ptrCast(*WordParameter, @alignCast(@alignOf(WordParameter), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("word_param ({}) name: {s} has_colon ({}) arg:", .{ self.op, self.name, self.has_colon });
        if (self.arg) |word_arg| {
            word_arg.print(spacing + 2);
        } else {
            std.debug.print(" null\n", .{});
        }
    }
};

/// Word Command representation, $(program) or `program`
pub const WordCommand = struct {
    program: ?*Program,
    is_back_quoted: bool,
    range: Range,

    // TODO maybe,  word() ?

    pub fn word(self: *WordCommand) Word {
        return .{ .impl = self, .kind = .COMMAND, .deinitFn = deinit, .printFn = print };
    }

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, word_command: WordCommand) !Word {
        const command = try allocator.create(WordCommand);
        command.* = word_command;
        return command.word();
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *std.mem.Allocator) void {
        var self = @ptrCast(*WordCommand, @alignCast(@alignOf(WordCommand), self_void));
        if (self.program) |prog| {
            prog.deinit(allocator);
        }
        allocator.destroy(self);
    }

    /// Prints the Word Command representation
    pub fn print(self_void: *c_void, spacing: usize) void {
        var self = @ptrCast(*WordCommand, @alignCast(@alignOf(WordCommand), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("word_command ({}) is_back_quoted: {}\n", .{ self.range, self.is_back_quoted });
        if (self.program) |prog| {
            std.debug.print(csi ++ "{}C", .{spacing});
            prog.print(spacing + 2);
        }
    }
};

/// Word Arithmetic representation
pub const WordArithm = struct {
    body: Word,

    pub fn word(self: *WordArithm) Word {
        return .{ .impl = self, .kind = .ARITHMETIC, .deinitFn = deinit, .printFn = print };
    }

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, word_arithm: WordArithm) !Word {
        const word_arithmetic = try allocator.create(WordArithm);
        word_arithmetic.* = word_arithm;
        return word_arithmetic.word();
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *std.mem.Allocator) void {
        var self = @ptrCast(*WordArithm, @alignCast(@alignOf(WordArithm), self_void));
        self.body.deinit(allocator);
        allocator.destroy(self);
    }

    /// Prints the Word Arithmetic representation
    pub fn print(self_void: *c_void, spacing: usize) void {
        var self = @ptrCast(*WordArithm, @alignCast(@alignOf(WordArithm), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("word_arithm ({}) body:\n   ", .{self});
        self.body.print(spacing + 2);
    }
};

/// Word List representation
pub const WordList = struct {
    items: []Word,
    is_double_quoted: bool,
    left_quote_pos: ?Position = null,
    right_quote_pos: ?Position = null,

    pub fn word(self: *WordList) Word {
        return .{ .impl = self, .kind = .LIST, .deinitFn = deinit, .printFn = print };
    }

    /// Initializes the memory using given `allocator`
    pub fn create(allocator: *std.mem.Allocator, word_list: WordList) !Word {
        const w_list = try allocator.create(WordList);
        w_list.* = word_list;
        return w_list.word();
    }

    /// Deinitializes the memory used, takes an `allocator`, it should be the one
    /// that was used to allocate the data
    pub fn deinit(self_void: *c_void, allocator: *std.mem.Allocator) void {
        var self = @ptrCast(*WordList, @alignCast(@alignOf(WordList), self_void));
        for (self.items) |item| {
            item.deinit(allocator);
        }
        allocator.free(self.items);
        allocator.destroy(self);
    }

    /// Prints the Word List representation
    pub fn print(self_void: *c_void, spacing: usize) void {
        var self = @ptrCast(*WordList, @alignCast(@alignOf(WordList), self_void));
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("word_list ({}) is_double_quoted: {} left_quote ({}) right_quote ({}):\n", .{ self.items.len, self.is_double_quoted, self.left_quote_pos, self.right_quote_pos });
        for (self.items) |item| {
            item.print(spacing + 2);
        }
        std.debug.print(csi ++ "{}C", .{spacing});
        std.debug.print("end_word_list\n", .{});
    }
};
