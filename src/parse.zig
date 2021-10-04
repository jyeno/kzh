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

    /// Initializes the parser
    pub fn init(allocator: *std.mem.Allocator, source: []const u8) Parser {
        return Parser{ .allocator = allocator, .source = source };
    }

    /// Generates an Abstract Syntax Tree based on the tokens received using
    /// recursive descent parsing, with the Visitor Pattern
    pub fn parse(parser: *Parser) !*Node.Program {
        return try parser.program();
    }

    fn program(parser: *Parser) !*Node.Program {
        var command_list_array = std.ArrayList(*Node.CommandList).init(parser.allocator);
        defer command_list_array.deinit();

        while (!parser.atEnd()) {
            try command_list_array.append(try parser.commandList());
        }

        var prog = try parser.allocator.create(Node.Program);
        prog.* = .{ .body = command_list_array.toOwnedSlice() };

        return prog;
    }

    fn commandList(parser: *Parser) !*Node.CommandList {
        const and_or_cmd_list = try parser.andOrCmdList();

        var command_list = try parser.allocator.create(Node.CommandList);
        command_list.* = .{ .and_or_cmd_list = and_or_cmd_list };

        const separator_pos = parser.currentPos;
        if (parser.separatorOperator()) |separator| {
            if (separator == '&') {
                command_list.is_async = true;
            }
            command_list.separator_pos = separator_pos;
            _ = parser.readChar();
        }

        return command_list;
    }

    const errors = error{
        OutOfMemory,
        ExpectedCommand,
    };

    fn andOrCmdList(parser: *Parser) errors!*AndOrCmdList {
        const pl = try parser.pipeline();
        // TODO error checking

        var op_range: Range = .{ .begin = .{}, .end = .{} };

        var bin_op_kind: AndOrCmdList.BinaryOp.BinaryOpKind = undefined;
        if (parser.isOperator(.AND, &op_range)) {
            bin_op_kind = AndOrCmdList.BinaryOp.BinaryOpKind.AND;
        } else if (parser.isOperator(.OR, &op_range)) {
            bin_op_kind = AndOrCmdList.BinaryOp.BinaryOpKind.OR;
        } else {
            return &pl.and_or_cmd_list;
        }

        // TODO error if and_or_right command is invalid
        const and_or_right = try parser.andOrCmdList();
        var binary_op = try parser.allocator.create(AndOrCmdList.BinaryOp);
        binary_op.* = .{ .left = &pl.and_or_cmd_list, .right = and_or_right, .op_range = op_range, .kind = bin_op_kind };

        return &binary_op.and_or_cmd_list;
    }

    fn pipeline(parser: *Parser) errors!*AndOrCmdList.Pipeline {
        //TODO analyze possibility of using peek or similar to know how much allocation needs to be done
        var command_array = std.ArrayList(*Node.Command).init(parser.allocator);
        defer command_array.deinit();

        var range: Range = undefined;
        const has_bang = parser.consumeToken("!", &range);
        var bang_pos: ?Position = null;
        if (has_bang) {
            bang_pos = range.begin;
        }

        try command_array.append(try parser.command());

        while (parser.consumeToken("|", null)) {
            const cmd = parser.command() catch |err| switch (err) {
                error.ExpectedCommand => {
                    std.debug.print("expected command.\n", .{});
                    break;
                },
                else => return err,
            };
            try command_array.append(cmd);
        }

        var pl = try parser.allocator.create(AndOrCmdList.Pipeline);
        pl.* = .{ .commands = command_array.toOwnedSlice(), .has_bang = has_bang, .bang_pos = bang_pos };

        return pl;
    }

    fn command(parser: *Parser) errors!*Node.Command {
        // TODO compound command
        const simple_command = try parser.simpleCommand();
        return &simple_command.command;
    }

    // TODO make enter (do nothing) possible
    fn simpleCommand(parser: *Parser) errors!*Command.SimpleCommand {
        // if has assignment or redir it has prefix
        // TODO above, has_prefix
        var cmd = try parser.allocator.create(Command.SimpleCommand);
        errdefer cmd.deinit(parser.allocator);
        cmd.* = .{ .name = null };
        try parser.cmdPrefix(cmd);
        cmd.name = try parser.cmdName();
        if (!cmd.isEmpty()) {
            // TODO fix cmdArgs
            try parser.cmdArgs(cmd);
            return cmd;
        }

        return error.ExpectedCommand;
    }

    fn cmdPrefix(parser: *Parser, cmd: *Command.SimpleCommand) errors!void {
        // TODO maybe function created at comptime that generates the code
        var io_redir_array = std.ArrayList(*IORedir).init(parser.allocator);
        defer io_redir_array.deinit();

        while (try parser.IORedirect()) |io_redir| {
            try io_redir_array.append(io_redir);
        }
        cmd.io_redirs = io_redir_array.toOwnedSlice();

        var assigns_array = std.ArrayList(*Assign).init(parser.allocator);
        defer assigns_array.deinit();

        while (try parser.assignmentWord()) |assign| {
            try assigns_array.append(assign);
        }
        cmd.assigns = assigns_array.toOwnedSlice();
    }

    fn cmdName(parser: *Parser) errors!?*Word {
        // TODO apply aliases
        // TODO apply keywords
        // TODO maybe make a function create to all the nodes, then it should allocate and return the allocation
        // the values would then be already initialized
        // TODO fix it, make the above true
        return try parser.word();
    }

    fn cmdArgs(parser: *Parser, cmd: *Command.SimpleCommand) errors!void {
        var word_array = std.ArrayList(*Word).init(parser.allocator);
        defer word_array.deinit();
        var io_redir_array = std.ArrayList(*IORedir).init(parser.allocator);
        defer io_redir_array.deinit();
        var appended_redir = true;
        var appended_word = true;
        while (appended_redir or appended_word) {
            if (try parser.IORedirect()) |io_redir| {
                try io_redir_array.append(io_redir);
                appended_redir = true;
            } else {
                appended_redir = false;
            }
            if (try parser.word()) |word_ptr| {
                try word_array.append(word_ptr);
                appended_word = true;
            } else {
                appended_word = false;
            }
        }

        cmd.io_redirs = io_redir_array.toOwnedSlice();
        cmd.args = word_array.toOwnedSlice();
    }

    fn word(parser: *Parser) errors!?*Word {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return null;
        }
        const len = parser.peekSizeWord();
        var range: Range = undefined;
        if (parser.readToken(len, &range)) |str| {
            var word_string = try parser.allocator.create(Word.WordString);
            word_string.* = .{ .str = str, .range = range };
            return &word_string.word;
        } else {
            return null;
        }
    }

    fn assignmentWord(parser: *Parser) errors!?*Assign {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return null;
        }

        // maybe have error her, maybe peekName could be called from others places? TODO
        const name_size = parser.peekName();

        const string = parser.peek(name_size + 1);
        if (string) |str| {
            if (name_size != 0 and str[name_size] == '=') {
                var name_range: Range = .{ .begin = .{}, .end = .{} };
                if (parser.readToken(name_size, &name_range)) |name| {
                    const equal_pos = parser.currentPos;
                    _ = parser.readChar();
                    var assign = try parser.allocator.create(Assign);
                    assign.* = .{ .name = name, .value = try parser.word(), .name_range = name_range, .equal_pos = equal_pos };
                    return assign;
                }
            }
        }

        return null;
    }

    fn IORedirect(parser: *Parser) errors!?*IORedir {
        if (try parser.IORedirFile()) |io_file| {
            return io_file;
        }

        if (try parser.IORedirHere()) |io_here_doc| {
            return io_here_doc;
        }

        // maybe have an error here if io_number doesnt go to anyplace
        return null;
    }

    fn IORedirNumber(parser: *Parser) ?u8 {
        // TODO error reading, 'echo oi 2>&1' should have io_num as 2, it doesnt
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return null;
        }

        const redirNumOp = parser.peek(2);
        if (redirNumOp) |numOp| {
            if (numOp[1] == '<' or numOp[1] == '>') {
                const number = parser.read(1);
                parser.resetCurrentSymbol();
                return std.fmt.parseInt(u8, number.?, 10) catch null;
            }
        }
        return null;
    }

    fn IORedirFile(parser: *Parser) errors!?*IORedir {
        const io_num_pos = parser.currentPos;
        const io_number = parser.IORedirNumber();

        var range = Range{ .begin = parser.currentPos, .end = .{} };
        const operator = parser.IORedirOp(&range);
        if (operator) |op| {
            const io_filename = try parser.word(); // TODO improve it, making use of rule 2 of grammar
            if (io_filename) |filename| {
                var io_redir = try parser.allocator.create(IORedir);
                io_redir.* = .{ .io_num = io_number, .io_num_pos = io_num_pos, .name = filename, .op_range = range, .op = op };
                return io_redir;
            }
        }

        return null;
    }

    fn IORedirHere(parser: *Parser) errors!?*IORedir {
        _ = parser; // TODO
        return null;
    }

    const IORedirKind = IORedir.IORedirKind;

    fn IORedirOp(parser: *Parser, range: ?*Range) ?IORedirKind {
        if (parser.consumeToken("<", range)) {
            return IORedirKind.IO_LESS;
        } else if (parser.consumeToken(">", range)) {
            return IORedirKind.IO_GREAT;
        } else if (parser.isOperator(.DOUBLE_GREAT, range)) {
            return IORedirKind.IO_DOUBLE_GREAT;
        } else if (parser.isOperator(.LESS_AND, range)) {
            return IORedirKind.IO_LESS_AND;
        } else if (parser.isOperator(.GREAT_AND, range)) {
            return IORedirKind.IO_GREAT_AND;
        } else if (parser.isOperator(.LESS_GREAT, range)) {
            return IORedirKind.IO_LESS_GREAT;
        } else if (parser.isOperator(.CLOBBER, range)) {
            return IORedirKind.IO_CLOBBER;
        } else {
            return null;
        }
    }

    fn peekSizeWord(parser: *Parser) u8 {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return 0;
        }
        var word_size: u8 = 0;

        while (true) : (word_size += 1) {
            const string = parser.peek(word_size + 1);
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
    fn peekName(parser: *Parser) u8 {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return 0;
        }

        var name_size: u8 = 0;
        // TODO test, maybe add in_brace param bool
        // maybe raise an error if the first character is invalid
        while (true) : (name_size += 1) {
            const string = parser.peek(name_size + 1);
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

    fn peek(parser: *Parser, len: u16) ?[]const u8 {
        // TODO improve it
        const begin = parser.currentPos.column;
        const end = begin + len;
        if (end > parser.source.len) return null;
        return parser.source[begin..end];
    }

    /// Peeks and returns a character
    fn peekChar(parser: *Parser) ?u8 {
        return if (parser.peek(1)) |str| str[0] else null;
    }

    // fn compoundCommand(parser: *Parser) !*CompoundCommand {}

    fn read(parser: *Parser, len: u16) ?[]const u8 {
        // TODO broken
        const string = parser.peek(len);
        if (string) |str| {
            // TODO update currentPos
            for (str) |ch| {
                if (ch == '\n') {
                    parser.currentPos.line += 1;
                    parser.currentPos.column = 1;
                } else {
                    parser.currentPos.column += 1;
                }
            }
            return str;
        } else {
            return null;
        }
    }

    /// Reads character based on current position and returns it,
    /// returns null if invalid
    fn readChar(parser: *Parser) ?u8 {
        return if (parser.read(1)) |str| str[0] else null;
    }

    fn readToken(parser: *Parser, len: u8, range: ?*Range) ?[]const u8 {
        if (!parser.isCurrentSymbol(.TOKEN) or len == 0) {
            return null;
        }

        const begin = parser.currentPos;

        const str = parser.read(len);

        if (range) |r| {
            r.begin = begin;
            r.end = parser.currentPos;
        }

        parser.resetCurrentSymbol();
        return str;
    }

    fn consumeToken(parser: *Parser, str: []const u8, range: ?*Range) bool {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return false;
        }

        const begin = parser.currentPos;

        if (str.len == 1 and !std.ascii.isAlpha(str[0])) {
            if (parser.peekChar() != str[0]) {
                return false;
            }
            _ = parser.readChar();
        } else {
            const word_str = parser.peek(parser.peekSizeWord());
            if (!std.mem.eql(u8, str, word_str.?)) {
                return false;
            }
            _ = parser.read(@intCast(u16, str.len));
        }

        if (range) |r| {
            r.begin = begin;
            r.end = parser.currentPos;
        }

        parser.resetCurrentSymbol();
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

    fn resetCurrentSymbol(parser: *Parser) void {
        parser.currentSymbol = null;
    }

    fn isCurrentSymbol(parser: *Parser, symbol: Symbol) bool {
        if (parser.currentSymbol == null) {
            parser.readSymbol();
        }
        return parser.currentSymbol.? == symbol;
    }

    fn readSymbol(parser: *Parser) void {
        const char = parser.peekChar();
        if (char) |c| {
            if (c == '\n') {
                parser.currentSymbol = .NEWLINE;
            } else if (isOperatorStart(c)) {
                const string = blk: {
                    if (parser.peek(3)) |str| break :blk str;

                    break :blk parser.peek(2);
                };
                if (string) |str| {
                    const end: u2 = if (str[str.len - 1] == '-') 3 else 2; // TODO improve it
                    if (operators.get(str[0..end])) |sym| {
                        parser.currentSymbol = sym;
                    }
                }
            } else if (std.ascii.isBlank(c)) {
                _ = parser.readChar();
                parser.readSymbol();
            } else if (c == '#') {
                while (parser.peekChar()) |ch| {
                    if (ch == '\n') {
                        break;
                    }
                    _ = parser.readChar();
                }
                // _ = parser.readChar(); // consuming the '\0' ?
                parser.readSymbol();
            }

            if (parser.currentSymbol == null) parser.currentSymbol = .TOKEN;
        } else {
            // TODO maybe wrong?
            parser.currentSymbol = .EOF;
        }
    }

    fn isOperatorStart(c: u8) bool {
        return switch (c) {
            '<', '>', '|', '&', ';' => true,
            else => false,
        };
    }

    fn isOperator(parser: *Parser, sym: Symbol, range: ?*Range) bool {
        if (!parser.isCurrentSymbol(sym)) {
            return false;
        }

        const begin: Position = parser.currentPos;

        const operator_string_len: u8 = blk: {
            // TODO improve it
            if (parser.peek(3)) |str| {
                if (str[2] != '-') {
                    if (operators.get(str[0..2]) != null) break :blk 2;
                } else { // if enter here it should be "<<-"
                    if (operators.get(str) != null) break :blk 3;
                }
            } else if (parser.peek(2)) |str| {
                if (operators.get(str) != null) break :blk 2;
            }
            unreachable;
        };
        _ = parser.read(operator_string_len);

        if (range) |r| {
            r.begin = begin;
            r.end = parser.currentPos;
        }

        parser.resetCurrentSymbol();
        return true;
    }

    fn separatorOperator(parser: *Parser) ?u8 {
        if (parser.consumeToken("&", null)) {
            return '&';
        } else if (parser.consumeToken(";", null)) {
            return ';';
        }
        return null;
    }

    fn atEnd(parser: *Parser) bool {
        return parser.isCurrentSymbol(.EOF);
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
