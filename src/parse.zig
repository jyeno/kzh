//! this module contains the parser of the cmdline
const std = @import("std");
const ast = @import("ast.zig");
const Node = ast.Node;
const AndOrCmdList = Node.AndOrCmdList;
const Command = Node.Command;
const Word = ast.Word;
const Position = ast.Position;
const Range = ast.Range;

/// Internal helper struct to parsing the tokens
pub const Parser = struct {
    /// source is not owned by the parser
    source: []const u8,
    allocator: *std.mem.Allocator,
    // TODO fix usage, currently it is used only the column property, and this is not right
    currentPos: Position = .{ .offset = 0, .line = 1, .column = 0 },
    currentSymbol: ?Symbol = null,

    const Self = @This();

    /// Initializes the parser
    pub fn init(allocator: *std.mem.Allocator, source: []const u8) Self {
        return Self{ .allocator = allocator, .source = source };
    }

    /// Generates an Abstract Syntax Tree based on the tokens received using
    /// recursive descent parsing, with the Visitor Pattern
    pub fn parse(self: *Self) !*Node.Program {
        // std.debug.print("parse\n", .{});
        return try self.program();
    }

    fn program(self: *Self) !*Node.Program {
        // std.debug.print("program\n", .{});
        var command_list_array = std.ArrayList(*Node.CommandList).init(self.allocator);
        defer command_list_array.deinit();
        errdefer command_list_array.deinit();

        while (!self.atEnd()) {
            try command_list_array.append(try self.commandList());
        }

        var prog = try self.allocator.create(Node.Program);
        prog.* = .{ .body = command_list_array.toOwnedSlice() };

        return prog;
    }

    fn commandList(self: *Self) !*Node.CommandList {
        // std.debug.print("cmdList\n", .{});
        var command_list = try self.allocator.create(Node.CommandList);
        command_list.* = .{ .and_or_cmd_list = try self.andOrCmdList() }; // is_async by now is false, after getting it to work I will fix it
        return command_list;
    }

    fn andOrCmdList(self: *Self) !*AndOrCmdList {
        // std.debug.print("andOrCmdList\n", .{});
        const pl = try self.pipeline();
        // TODO more things...
        return &pl.and_or_cmd_list;
    }

    fn pipeline(self: *Self) !*AndOrCmdList.Pipeline {
        // std.debug.print("pipeliness\n", .{});
        //TODO analyze possibility of using peek or similar to know how much allocation needs to be done
        var command_array = std.ArrayList(*Node.Command).init(self.allocator);
        defer command_array.deinit();

        // shouldnt do this line below
        try command_array.append(try self.command());
        while (self.consumeToken("|", null)) {
            try command_array.append(try self.command());
        }

        var pl = try self.allocator.create(AndOrCmdList.Pipeline);
        pl.* = .{ .commands = command_array.toOwnedSlice() };

        return pl;
    }

    fn command(self: *Self) !*Node.Command {
        // TODO compound command
        return &(try self.simpleCommand()).command;
    }

    fn simpleCommand(self: *Self) !*Command.SimpleCommand {
        // if has assignment or redir it has prefix
        // TODO above, has_prefix
        var cmd = try self.allocator.create(Command.SimpleCommand);
        // try self.cmdPrefix(cmd);
        cmd.* = .{ .name = try self.cmdName() };
        if (!cmd.isEmpty()) {
            // TODO fix cmdArgs
            try self.cmdArgs(cmd);
            return cmd;
        }

        return error.CommandEmpty;
    }

    // fn cmdPrefix(self: *Self, cmd: *Command.SimpleCommand) !void {
    //TODO
    // }

    fn cmdName(self: *Self) !?*Word {
        // TODO apply aliases
        // TODO apply keywords
        // TODO maybe make a function create to all the nodes, then it should allocate and return the allocation
        // the values would then be already initialized
        // return null;
        // const len = self.peekSizeWord(0);
        // var range: Range = undefined;
        // if (self.readToken(len, &range)) |str| {
        //     var word_string = try self.allocator.create(Word.WordString);
        //     word_string.* = .{ .str = str, .range = range };
        //     return &word_string.word;
        // } else {
        //     return null;
        // }
        // TODO fix it, make the above true
        return try self.word();
    }

    fn cmdArgs(self: *Self, cmd: *Command.SimpleCommand) !void {
        // TODO self.ioRedirect
        var word_array = std.ArrayList(*Word).init(self.allocator);
        defer word_array.deinit();
        while (try self.word()) |algo| {
            try word_array.append(algo);
        }
        cmd.args = word_array.toOwnedSlice();
    }

    fn word(self: *Self) !?*Word {
        const len = self.peekSizeWord(0);
        var range: Range = undefined;
        if (self.readToken(len, &range)) |str| {
            var word_string = try self.allocator.create(Word.WordString);
            word_string.* = .{ .str = str, .range = range };
            return &word_string.word;
        } else {
            return null;
        }
        // var wordNode = try self.allocator.create(WordString);
        // return &wordNode.word;
    }

    fn peekSizeWord(self: *Self, delim: u8) u8 {
        if (!self.isCurrentSymbol(Symbol.TOKEN)) {
            return 0;
        }
        var word_size: u8 = 0;

        // TODO maybe use for loop, figure out how to use the currentPos to get the right current index
        while (true) : (word_size += 1) {
            const string = self.peek(word_size + 1);
            if (string) |str| {
                const ch = str[word_size];
                switch (ch) {
                    '\n', ')' => return word_size,
                    '$', '`', '\'', '"', '\\' => return 0,
                    else => {},
                }
                if (isOperatorStart(ch) or std.ascii.isBlank(ch) or ch == delim) {
                    return word_size;
                }
            } else {
                return word_size;
            }
        }
    }

    fn peek(self: *Self, len: u16) ?[]const u8 {
        // TODO improve it
        const begin = self.currentPos.column;
        const end = begin + len;
        if (end > self.source.len) return null;
        return self.source[begin..end];
    }

    /// Peeks and returns a character
    fn peekChar(self: *Self) ?u8 {
        return if (self.peek(1)) |str| str[0] else null;
    }

    // fn compoundCommand(self: *Self) !*CompoundCommand {}
    fn read(self: *Self, len: u16) ?[]const u8 {
        // TODO broken
        const string = self.peek(len);
        if (string) |str| {
            // update currentPos
            for (str) |ch| {
                if (ch == '\n') {
                    self.currentPos.line += 1;
                    self.currentPos.column = 1;
                } else {
                    self.currentPos.column += 1;
                }
            }
            return str;
        } else {
            return null;
        }
    }

    /// Reads character based on current position and returns it,
    /// returns null if invalid
    fn readChar(self: *Self) ?u8 {
        return if (self.read(1)) |str| str[0] else null;
    }

    fn readToken(self: *Self, len: u8, range: ?*Range) ?[]const u8 {
        // std.debug.print("current symbol: {}\n", .{self.currentSymbol});
        if (!self.isCurrentSymbol(Symbol.TOKEN)) {
            return null;
        }

        const begin = self.currentPos;

        const str = self.read(len);

        if (range) |r| {
            r.begin = begin;
            r.end = self.currentPos;
        }

        self.resetCurrentSymbol();
        return str;
    }

    fn consumeToken(self: *Self, str: []const u8, range: ?*Range) bool {
        if (!self.isCurrentSymbol(Symbol.TOKEN)) {
            return false;
        }

        const begin = self.currentPos;

        if (str.len == 1 and std.ascii.isAlpha(str[0])) {
            if (self.peekChar() != str[0]) {
                return false;
            }
            _ = self.readChar();
        } else {
            // analyze correctly what is the buf->data
            // TODO get the str somehow
            const word_str = self.peek(self.peekSizeWord(0));
            if (!std.mem.eql(u8, str, word_str.?)) {
                return false;
            }
            _ = self.read(@intCast(u16, str.len));
        }

        if (range) |r| {
            r.begin = begin;
            r.end = self.currentPos;
        }

        self.resetCurrentSymbol();
        return true;
    }

    /// Representation of the symbols supported
    const Symbol = enum {
        /// end of file
        EOF,
        // NEWLINE,
        // AND,
        // OR,
        // DOUBLE_SEMICOLON,
        // DOUBLE_LESS,
        // DOUBLE_GREAT,
        // LESSAND,
        // GREATAND,
        // LESSGREAT,
        // DOUBLE_LESS_DASH,
        // CLOBBER,
        /// any valid string that is not any of the previous symbols
        TOKEN,
    };

    fn resetCurrentSymbol(self: *Self) void {
        self.currentSymbol = null;
    }

    fn isCurrentSymbol(self: *Self, symbol: Symbol) bool {
        if (self.currentSymbol == null) {
            self.readSymbol();
        }
        return self.currentSymbol.? == symbol;
    }

    fn readSymbol(self: *Self) void {
        const char = self.peekChar();
        // std.debug.print("peekch: {}\n", .{char});
        if (char) |c| {
            if (isOperatorStart(c)) {
                unreachable;
                // sets the symbol to the operator, not gonna do it now
            } else if (std.ascii.isBlank(c)) {
                _ = self.readChar();
                self.readSymbol();
            } else if (c == '#') {
                while (self.peekChar()) |ch| {
                    if (ch == '\n') {
                        break;
                    }
                    _ = self.readChar();
                }
                // _ = self.readChar(); // consuming the '\0' ?
                self.readSymbol();
            } else {
                self.currentSymbol = Symbol.TOKEN;
            }

            // maybe null will also be eof ?
            // std.debug.print("failed to read symbol. currentPos: {}\n", .{self.currentPos});
        } else {
            // TODO maybe wrong?
            self.currentSymbol = Symbol.EOF;
        }
    }

    fn isOperatorStart(c: u8) bool {
        return switch (c) {
            '<', '>', '|', '&', ';' => true,
            else => false,
        };
    }

    fn atEnd(self: *Self) bool {
        return if (self.currentSymbol) |sym| sym == Symbol.EOF else false;
    }
};

test "Simple Command" {
    // TODO
}

test "Simple Command with io redirection" {
    // TODO
}

test "Pipeline" {
    // TODO
}

test "List Termination" {
    // TODO
}

test "List Separation" {
    // TODO
}

test "Group Command" {
    // TODO
}

test "SubShell" {
    // TODO
}
