//! this module contains the parser of the cmdline
const std = @import("std");
const ast = @import("ast.zig");
const Node = ast.Node;
const AndOrCmdList = Node.AndOrCmdList;
const Command = Node.Command;
const Word = ast.Word;
const IORedir = ast.IORedir;
const Assign = ast.Assign;
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
        return try self.program();
    }

    fn program(self: *Self) !*Node.Program {
        var command_list_array = std.ArrayList(*Node.CommandList).init(self.allocator);
        defer command_list_array.deinit();

        while (!self.atEnd()) {
            try command_list_array.append(try self.commandList());
        }

        var prog = try self.allocator.create(Node.Program);
        prog.* = .{ .body = command_list_array.toOwnedSlice() };

        return prog;
    }

    fn commandList(self: *Self) !*Node.CommandList {
        const and_or_cmd_list = try self.andOrCmdList();

        var command_list = try self.allocator.create(Node.CommandList);
        command_list.* = .{ .and_or_cmd_list = and_or_cmd_list };

        const separator_pos = self.currentPos;
        if (self.separatorOperator()) |separator| {
            if (separator == '&') {
                command_list.is_async = true;
            }
            command_list.separator_pos = separator_pos;
            _ = self.readChar();
        }

        return command_list;
    }

    const errors = error{
        OutOfMemory,
        ExpectedCommand,
    };

    fn andOrCmdList(self: *Self) errors!*AndOrCmdList {
        const pl = try self.pipeline();
        // TODO error checking

        var op_range: Range = .{ .begin = .{}, .end = .{} };

        var bin_op_kind: AndOrCmdList.BinaryOp.BinaryOpKind = undefined;
        if (self.isOperator(.AND, &op_range)) {
            bin_op_kind = AndOrCmdList.BinaryOp.BinaryOpKind.AND;
        } else if (self.isOperator(.OR, &op_range)) {
            bin_op_kind = AndOrCmdList.BinaryOp.BinaryOpKind.OR;
        } else {
            return &pl.and_or_cmd_list;
        }

        // TODO error if and_or_right command is invalid
        const and_or_right = try self.andOrCmdList();
        var binary_op = try self.allocator.create(AndOrCmdList.BinaryOp);
        binary_op.* = .{ .left = &pl.and_or_cmd_list, .right = and_or_right, .op_range = op_range, .kind = bin_op_kind };

        return &binary_op.and_or_cmd_list;
    }

    fn pipeline(self: *Self) errors!*AndOrCmdList.Pipeline {
        //TODO analyze possibility of using peek or similar to know how much allocation needs to be done
        var command_array = std.ArrayList(*Node.Command).init(self.allocator);
        defer command_array.deinit();

        var range: Range = undefined;
        const has_bang = self.consumeToken("!", &range);
        var bang_pos: ?Position = null;
        if (has_bang) {
            bang_pos = range.begin;
        }

        try command_array.append(try self.command());

        while (self.consumeToken("|", null)) {
            const cmd = self.command() catch |err| switch (err) {
                error.ExpectedCommand => {
                    std.debug.print("expected command.\n", .{});
                    break;
                },
                else => return err,
            };
            try command_array.append(cmd);
        }

        var pl = try self.allocator.create(AndOrCmdList.Pipeline);
        pl.* = .{ .commands = command_array.toOwnedSlice(), .has_bang = has_bang, .bang_pos = bang_pos };

        return pl;
    }

    fn command(self: *Self) errors!*Node.Command {
        // TODO compound command
        const simple_command = try self.simpleCommand();
        return &simple_command.command;
    }

    // TODO make enter (do nothing) possible
    fn simpleCommand(self: *Self) errors!*Command.SimpleCommand {
        // if has assignment or redir it has prefix
        // TODO above, has_prefix
        var cmd = try self.allocator.create(Command.SimpleCommand);
        errdefer cmd.deinit(self.allocator);
        cmd.* = .{ .name = null };
        try self.cmdPrefix(cmd);
        cmd.name = try self.cmdName();
        if (!cmd.isEmpty()) {
            // TODO fix cmdArgs
            try self.cmdArgs(cmd);
            return cmd;
        }

        return error.ExpectedCommand;
    }

    fn cmdPrefix(self: *Self, cmd: *Command.SimpleCommand) errors!void {
        // TODO maybe function created at comptime that generates the code
        var io_redir_array = std.ArrayList(*IORedir).init(self.allocator);
        defer io_redir_array.deinit();

        while (try self.IORedirect()) |io_redir| {
            try io_redir_array.append(io_redir);
        }
        cmd.io_redirs = io_redir_array.toOwnedSlice();

        var assigns_array = std.ArrayList(*Assign).init(self.allocator);
        defer assigns_array.deinit();

        while (try self.assignmentWord()) |assign| {
            try assigns_array.append(assign);
        }
        cmd.assigns = assigns_array.toOwnedSlice();
    }

    fn cmdName(self: *Self) errors!?*Word {
        // TODO apply aliases
        // TODO apply keywords
        // TODO maybe make a function create to all the nodes, then it should allocate and return the allocation
        // the values would then be already initialized
        // TODO fix it, make the above true
        return try self.word();
    }

    fn cmdArgs(self: *Self, cmd: *Command.SimpleCommand) errors!void {
        var word_array = std.ArrayList(*Word).init(self.allocator);
        defer word_array.deinit();

        while (try self.word()) |word_ptr| {
            try word_array.append(word_ptr);
        }
        cmd.args = word_array.toOwnedSlice();

        var io_redir_array = std.ArrayList(*IORedir).init(self.allocator);
        defer io_redir_array.deinit();

        while (try self.IORedirect()) |io_redir| {
            try io_redir_array.append(io_redir);
        }
        cmd.io_redirs = io_redir_array.toOwnedSlice();
    }

    fn word(self: *Self) errors!?*Word {
        if (!self.isCurrentSymbol(.TOKEN)) {
            return null;
        }
        const len = self.peekSizeWord();
        var range: Range = undefined;
        if (self.readToken(len, &range)) |str| {
            var word_string = try self.allocator.create(Word.WordString);
            word_string.* = .{ .str = str, .range = range };
            return &word_string.word;
        } else {
            return null;
        }
    }

    fn assignmentWord(self: *Self) errors!?*Assign {
        if (!self.isCurrentSymbol(.TOKEN)) {
            return null;
        }

        // maybe have error her, maybe peekName could be called from others places? TODO
        const name_size = self.peekName();

        const string = self.peek(name_size + 1);
        if (string) |str| {
            if (name_size != 0 and str[name_size] == '=') {
                var name_range: Range = .{ .begin = .{}, .end = .{} };
                if (self.readToken(name_size, &name_range)) |name| {
                    const equal_pos = self.currentPos;
                    _ = self.readChar();
                    var assign = try self.allocator.create(Assign);
                    assign.* = .{ .name = name, .value = try self.word(), .name_range = name_range, .equal_pos = equal_pos };
                    return assign;
                }
            }
        }

        return null;
    }

    fn IORedirect(self: *Self) errors!?*IORedir {
        if (try self.IORedirFile()) |io_file| {
            return io_file;
        }

        if (try self.IORedirHere()) |io_here_doc| {
            return io_here_doc;
        }

        // maybe have an error here if io_number doesnt go to anyplace
        return null;
    }

    fn IORedirNumber(self: *Self) ?u8 {
        // TODO error reading, 'echo oi 2>&1' should have io_num as 2, it doesnt
        if (!self.isCurrentSymbol(.TOKEN)) {
            return null;
        }

        const redirNumOp = self.peek(2);
        if (redirNumOp) |numOp| {
            if (numOp[1] == '<' or numOp[1] == '>') {
                const number = self.read(1);
                self.resetCurrentSymbol();
                return std.fmt.parseInt(u8, number.?, 10) catch null;
            }
        }
        return null;
    }

    fn IORedirFile(self: *Self) errors!?*IORedir {
        const io_num_pos = self.currentPos;
        const io_number = self.IORedirNumber();

        var range = Range{ .begin = self.currentPos, .end = .{} };
        const operator = self.IORedirOp(&range);
        if (operator) |op| {
            const io_filename = try self.word(); // TODO improve it, making use of rule 2 of grammar
            if (io_filename) |filename| {
                var io_redir = try self.allocator.create(IORedir);
                io_redir.* = .{ .io_num = io_number, .io_num_pos = io_num_pos, .name = filename, .op_range = range, .op = op };
                return io_redir;
            }
        }

        return null;
    }

    fn IORedirHere(self: *Self) errors!?*IORedir {
        _ = self; // TODO
        return null;
    }

    const IORedirKind = IORedir.IORedirKind;

    fn IORedirOp(self: *Self, range: ?*Range) ?IORedirKind {
        if (self.consumeToken("<", range)) {
            return IORedirKind.IO_LESS;
        } else if (self.consumeToken(">", range)) {
            return IORedirKind.IO_GREAT;
        } else if (self.isOperator(.DOUBLE_GREAT, range)) {
            return IORedirKind.IO_DOUBLE_GREAT;
        } else if (self.isOperator(.LESS_AND, range)) {
            return IORedirKind.IO_LESS_AND;
        } else if (self.isOperator(.GREAT_AND, range)) {
            return IORedirKind.IO_GREAT_AND;
        } else if (self.isOperator(.LESS_GREAT, range)) {
            return IORedirKind.IO_LESS_GREAT;
        } else if (self.isOperator(.CLOBBER, range)) {
            return IORedirKind.IO_CLOBBER;
        } else {
            return null;
        }
    }

    fn peekSizeWord(self: *Self) u8 {
        if (!self.isCurrentSymbol(.TOKEN)) {
            return 0;
        }
        var word_size: u8 = 0;

        while (true) : (word_size += 1) {
            const string = self.peek(word_size + 1);
            if (string) |str| {
                const ch = str[word_size];
                switch (ch) {
                    '\n', ')' => return word_size,
                    '$', '`', '\'', '"', '\\' => return 0,
                    else => {},
                }
                if (isOperatorStart(ch) or std.ascii.isBlank(ch)) {
                    return word_size;
                }
            } else {
                return word_size;
            }
        }
    }

    /// A name in the shell is composition of underscores, digits
    /// and alphanumerics, the first character can not be a digit.
    fn peekName(self: *Self) u8 {
        if (!self.isCurrentSymbol(.TOKEN)) {
            return 0;
        }

        var name_size: u8 = 0;
        // TODO test, maybe add in_brace param bool
        // maybe raise an error if the first character is invalid
        while (true) : (name_size += 1) {
            const string = self.peek(name_size + 1);
            if (string) |str| {
                const ch = str[name_size];
                // maybe buggy, TODO test behavior
                if ((ch != '_' and !std.ascii.isAlNum(ch)) or std.ascii.isDigit(ch)) {
                    return name_size;
                }
            } else {
                return name_size;
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
            // TODO update currentPos
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
        if (!self.isCurrentSymbol(.TOKEN) or len == 0) {
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
        if (!self.isCurrentSymbol(.TOKEN)) {
            return false;
        }

        const begin = self.currentPos;

        if (str.len == 1 and !std.ascii.isAlpha(str[0])) {
            if (self.peekChar() != str[0]) {
                return false;
            }
            _ = self.readChar();
        } else {
            const word_str = self.peek(self.peekSizeWord());
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
        /// End Of File
        EOF,
        /// "\n"
        NEWLINE,
        /// "&&"
        AND,
        /// "||"
        OR,
        /// ";;"
        DOUBLE_SEMICOLON,
        /// "<<"
        DOUBLE_LESS,
        /// ">>"
        DOUBLE_GREAT,
        /// "<&
        LESS_AND,
        /// ">&"
        GREAT_AND,
        /// "<>"
        LESS_GREAT,
        /// ">|"
        CLOBBER,
        /// "<<-"
        DOUBLE_LESS_DASH,
        /// any valid string that is not any of the previous symbols
        TOKEN,
    };

    const operators = std.ComptimeStringMap(Symbol, .{
        .{ "&&", .AND },
        .{ "||", .OR },
        .{ ";;", .DOUBLE_SEMICOLON },
        .{ "<<", .DOUBLE_LESS },
        .{ ">>", .DOUBLE_GREAT },
        .{ "<&", .LESS_AND },
        .{ ">&", .GREAT_AND },
        .{ "<>", .LESS_GREAT },
        .{ ">|", .CLOBBER },
        .{ "<<-", .DOUBLE_LESS_DASH },
    });

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
        if (char) |c| {
            if (c == '\n') {
                self.currentSymbol = .NEWLINE;
            } else if (isOperatorStart(c)) {
                const string = blk: {
                    if (self.peek(3)) |str| break :blk str;

                    break :blk self.peek(2);
                };
                if (string) |str| {
                    const end: u2 = if (str[str.len - 1] == '-') 3 else 2; // TODO improve it
                    if (operators.get(str[0..end])) |sym| {
                        self.currentSymbol = sym;
                    }
                }
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
            }

            if (self.currentSymbol == null) self.currentSymbol = .TOKEN;
        } else {
            // TODO maybe wrong?
            self.currentSymbol = .EOF;
        }
    }

    fn isOperatorStart(c: u8) bool {
        return switch (c) {
            '<', '>', '|', '&', ';' => true,
            else => false,
        };
    }

    fn isOperator(self: *Self, sym: Symbol, range: ?*Range) bool {
        if (!self.isCurrentSymbol(sym)) {
            return false;
        }

        const begin: Position = self.currentPos;

        const operator_string_len: u8 = blk: {
            // TODO improve it
            if (self.peek(3)) |str| {
                if (str[2] != '-') {
                    if (operators.get(str[0..2]) != null) break :blk 2;
                } else { // if enter here it should be "<<-"
                    if (operators.get(str) != null) break :blk 3;
                }
            } else if (self.peek(2)) |str| {
                if (operators.get(str) != null) break :blk 2;
            }
            unreachable;
        };
        _ = self.read(operator_string_len);

        if (range) |r| {
            r.begin = begin;
            r.end = self.currentPos;
        }

        self.resetCurrentSymbol();
        return true;
    }

    fn separatorOperator(self: *Self) ?u8 {
        if (self.consumeToken("&", null)) {
            return '&';
        } else if (self.consumeToken(";", null)) {
            return ';';
        }
        return null;
    }

    fn atEnd(self: *Self) bool {
        return self.isCurrentSymbol(.EOF);
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
