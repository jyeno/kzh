//! this module contains the parser of the cmdline, Grammar from
//! https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
//!
//! -------------------------------------------------------
//!                     The grammar symbols
//! -------------------------------------------------------
//!
//! %token  WORD
//! %token  ASSIGNMENT_WORD
//! %token  NAME
//! %token  NEWLINE
//! %token  IO_NUMBER
//!
//! %token  AND_IF    OR_IF    DSEMI  TODO
//!          '&&'      '||'     ';;'
//!
//! %token  DLESS  DGREAT  LESSAND  GREATAND  LESSGREAT  DLESSDASH
//!          '<<'   '>>'    '<&'      '>&'      '<>'       '<<-'
//!
//! %token  CLOBBER
//!           '>|'
//!
//! The following are the reserved words. TODO
//! %token  If    Then    Else    Elif    Fi    Do    Done
//!        'if'  'then'  'else'  'elif'  'fi'  'do'  'done'
//!
//! %token  Case    Esac    While    Until    For
//!        'case'  'esac'  'while'  'until'  'for'
//!
//! These are reserved words, not operator tokens, and are
//! recognized when reserved words are recognized.
//!
//! %token  Lbrace    Rbrace    Bang
//!          '{'       '}'       '!'
//!
//! %token  In  TODO
//!        'in'

// TODO: maybe create parse/word.zig and parse/command.zig
// TODO: create errors instead of only returning null on some functions
const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const ast = @import("ast.zig");
const AndOrCmdList = ast.AndOrCmdList;
const Command = ast.Command;
const Word = ast.Word;
const IORedir = ast.IORedir;
const Assign = ast.Assign;
const Position = ast.Position;
const Range = ast.Range;

/// Internal helper struct to parsing the tokens
pub const Parser = struct {
    /// source is not owned by the parser
    source: []const u8,
    allocator: *mem.Allocator,
    // TODO fix usage, currently it is used only the column property, and this is not right
    currentPos: Position = .{ .offset = 0, .line = 1, .column = 0 },
    currentSymbol: ?Symbol = null,

    /// Initializes the parser
    pub fn init(allocator: *mem.Allocator, source: []const u8) Parser {
        return Parser{ .allocator = allocator, .source = source };
    }

    /// Generates an Abstract Syntax Tree based on the tokens received using
    /// recursive descent parsing, with the Visitor Pattern
    pub fn parse(parser: *Parser) !*ast.Program {
        return try parser.program();
    }

    /// complete_command  : list separator
    ///                   | list
    fn program(parser: *Parser) !*ast.Program {
        var command_list_array = std.ArrayList(*ast.CommandList).init(parser.allocator);
        defer command_list_array.deinit();

        // TODO separator
        while (try parser.commandList()) |cmd_list| {
            try command_list_array.append(cmd_list);
        }

        return try ast.Program.create(parser.allocator, .{ .body = command_list_array.toOwnedSlice() });
    }

    /// list  : list separator_op and_or
    ///       | and_or
    fn commandList(parser: *Parser) !?*ast.CommandList {
        // TODO fix behavior, improve logic
        if (try parser.andOrCmdList()) |and_or_cmd_list| {
            var command_list: ast.CommandList = .{ .and_or_cmd_list = and_or_cmd_list };

            const separator_pos = parser.currentPos;
            if (parser.separatorOperator()) |sep| {
                if (sep == '&') {
                    command_list.is_async = true;
                }
                command_list.separator_pos = separator_pos;
            }

            return try ast.CommandList.create(parser.allocator, command_list);
        }
        return null;
    }

    /// separator_op  : '&'
    ///               | ';'
    fn separatorOperator(parser: *Parser) ?u8 {
        if (parser.consumeToken("&", null)) {
            return '&';
        } else if (parser.consumeToken(";", null)) {
            return ';';
        }
        return null;
    }

    /// separator  : separator_op linebreak
    ///            | newline_list
    fn separator(parser: *Parser) ?u8 {
        if (parser.separatorOperator()) |sep| {
            parser.linebreak();
            return sep;
        }
        if (parser.consumeNewLine()) {
            return '\n';
        }
        return null;
    }

    const errors = error{
        OutOfMemory,
    };

    /// and_or  : pipeline
    ///         | and_or AND_IF linebreak pipeline
    ///         | and_or OR_IF  linebreak pipeline
    fn andOrCmdList(parser: *Parser) errors!?AndOrCmdList {
        if (try parser.pipeline()) |pl| {
            var op_range: Range = undefined;
            var bin_op_kind: ast.BinaryOp.BinaryOpKind = undefined;
            if (parser.isOperator(.AND, &op_range)) {
                bin_op_kind = ast.BinaryOp.BinaryOpKind.AND;
            } else if (parser.isOperator(.OR, &op_range)) {
                bin_op_kind = ast.BinaryOp.BinaryOpKind.OR;
            } else {
                return pl;
            }
            parser.linebreak();
            if (try parser.andOrCmdList()) |and_or_right| {
                return try ast.BinaryOp.create(parser.allocator, .{ .left = pl, .right = and_or_right, .op_range = op_range, .kind = bin_op_kind });
            } else {
                // TODO error if and_or_right command is invalid
                // or read newline
                return pl;
            }
        } else {
            return null;
        }
    }

    /// pipeline       : pipe_sequence
    ///                | Bang pipe_sequence
    /// pipe_sequence  : command
    ///                | pipe_sequence '|' linebreak command
    fn pipeline(parser: *Parser) errors!?AndOrCmdList {
        var command_array = std.ArrayList(ast.Command).init(parser.allocator);
        defer command_array.deinit();

        var range: Range = undefined;
        const has_bang = parser.consumeToken("!", &range);
        const bang_pos = if (has_bang) range.begin else null;

        if (try parser.command()) |cmd| {
            try command_array.append(cmd);
        } else {
            return null;
        }

        while (parser.consumeToken("|", null)) {
            parser.linebreak();
            if (try parser.command()) |cmd| {
                try command_array.append(cmd);
            } else {
                // TODO maybe have an error, see if needed
                break;
            }
        }

        return try ast.Pipeline.create(parser.allocator, .{ .commands = command_array.toOwnedSlice(), .has_bang = has_bang, .bang_pos = bang_pos });
    }

    /// command  : simple_command
    ///          | compound_command
    ///          | compound_command redirect_list TODO
    ///          | function_definition
    fn command(parser: *Parser) errors!?Command {
        if (try parser.compoundCommand()) |cmd| {
            // TODO redirects
            return cmd;
        } else if (try parser.funcDeclaration()) |func| {
            return func;
        } else if (try parser.simpleCommand()) |simple_cmd| {
            return simple_cmd;
        } else {
            return null;
        }
    }

    /// function_definition  : fname '(' ')' linebreak function_body
    /// function_body        : compound_command                /* Apply rule 9 */
    ///                      | compound_command redirect_list  /* Apply rule 9 */
    /// fname                : NAME                            /* Apply rule 8 */
    fn funcDeclaration(parser: *Parser) errors!?Command {
        const name_size = parser.peekNameSize();
        if (name_size == 0) {
            return null;
        } else {
            var n: usize = name_size + 1;
            while (parser.peek(n)) |strPeek| : (n += 1) {
                const char = strPeek[n - 1];
                if (char == '(') {
                    break;
                } else if (!std.ascii.isBlank(char)) {
                    return null;
                }
            } else {
                return null;
            }
        }
        // TODO check possibility of error
        const name = parser.readToken(name_size, null).?;

        if (!parser.consumeToken("(", null) or !parser.consumeToken(")", null)) {
            // TODO have some error
            return null;
        }
        parser.linebreak();
        if (try parser.compoundCommand()) |cmd| {
            var io_array = std.ArrayList(IORedir).init(parser.allocator);
            defer io_array.deinit();
            while (try parser.IORedirect()) |io_redir| {
                try io_array.append(io_redir);
            }
            if (io_array.items.len > 0) {
                return try ast.FuncDecl.create(parser.allocator, .{ .name = name, .body = cmd, .io_redirs = io_array.toOwnedSlice() });
            } else {
                return try ast.FuncDecl.create(parser.allocator, .{ .name = name, .body = cmd });
            }
        } else {
            return null;
        }
    }

    /// compound_command  : brace_group
    ///                   | subshell
    ///                   | for_clause
    ///                   | case_clause
    ///                   | if_clause
    ///                   | while_clause
    ///                   | until_clause
    fn compoundCommand(parser: *Parser) errors!?Command {
        if (try parser.cmdGroup()) |cmd_group| { // brace_group and subshell
            return cmd_group;
        } else if (try parser.forDeclaration()) |for_decl| {
            return for_decl;
        } else if (try parser.caseDeclaration()) |case_decl| {
            return case_decl;
        } else if (try parser.ifDeclaration()) |if_decl| {
            return if_decl;
        } else if (try parser.loopDeclaration()) |loop_decl| { // while and until
            return loop_decl;
        } else {
            return null;
        }
    }

    /// brace_group  : Lbrace compound_list Rbrace
    /// subshell     : '(' compound_list ')'
    fn cmdGroup(parser: *Parser) errors!?Command {
        var closing_char = "}";
        if (!parser.consumeToken("{", null)) {
            closing_char = ")";
            if (!parser.consumeToken("(", null)) {
                return null;
            }
        }
        if (try parser.compoundList()) |body| {
            // TODO read newline until lchar
            if (parser.consumeToken(closing_char, null)) {
                return try ast.CmdGroup.create(parser.allocator, .{ .body = body, .kind = if (closing_char[0] == '}') .BRACE_GROUP else .SUBSHELL });
            }
            for (body) |cmd_list| {
                cmd_list.deinit(parser.allocator);
            }
            parser.allocator.free(body);
        }
        return null;
    }

    /// compound_list  : linebreak term
    ///                | linebreak term separator
    /// term           : term separator and_or
    ///                | and_or
    fn compoundList(parser: *Parser) errors!?[]*ast.CommandList {
        parser.linebreak();
        var cmd_list_array = std.ArrayList(*ast.CommandList).init(parser.allocator);
        defer cmd_list_array.deinit();

        while (try parser.andOrCmdList()) |and_or_cmd| {
            const separator_pos = parser.currentPos;
            // TODO here_document
            if (parser.separator()) |sep| {
                try cmd_list_array.append(try ast.CommandList.create(parser.allocator, .{ .and_or_cmd_list = and_or_cmd, .is_async = sep == '&', .separator_pos = separator_pos }));
            } else {
                try cmd_list_array.append(try ast.CommandList.create(parser.allocator, .{ .and_or_cmd_list = and_or_cmd }));
            }
        }
        if (cmd_list_array.items.len > 0) {
            // TODO consider including this deinitialization and two parameters with token and range
            // if not token, then deinitializes and return null
            // for (body) |cmd_list| {
            //     cmd_list.deinit(parser.allocator);
            // }
            // parser.allocator.free(body);
            return cmd_list_array.toOwnedSlice();
        }
        return null;
    }

    /// for_clause  : For name do_group
    ///             | For name sequential_sep do_group
    ///             | For name linebreak in          sequential_sep do_group
    ///             | For name linebreak in wordlist sequential_sep do_group
    /// in          : In   /* Apply rule 6 */
    /// TODO get sequential_sep
    fn forDeclaration(parser: *Parser) errors!?Command {
        _ = parser;
        return null;
    }

    /// do_group : Do compound_list Done   /* Apply rule 6 */
    fn doGroup(parser: *Parser) errors!?[]*ast.CommandList {
        if (!parser.consumeToken("do", null)) {
            // TODO have error or read newline
            return null;
        }
        if (try parser.compoundList()) |body| {
            if (parser.consumeToken("done", null)) {
                return body;
            }
            // TODO consider have this logic on compoundList
            for (body) |cmd_list| {
                cmd_list.deinit(parser.allocator);
            }
            parser.allocator.free(body);
        }
        return null;
    }

    /// case_clause   : Case WORD linebreak in linebreak case_list    Esac
    ///               | Case WORD linebreak in linebreak case_list_ns Esac
    ///               | Case WORD linebreak in linebreak              Esac
    /// case_list_ns  : case_list case_item_ns
    ///               |           case_item_ns
    /// case_list     : case_list case_item
    ///               | case_item
    /// case_item_ns  : pattern ')' linebreak
    ///               | pattern ')' compound_list
    ///               | '(' pattern ')' linebreak
    ///               | '(' pattern ')' compound_list
    /// case_item     : pattern ')' linebreak     DSEMI linebreak
    ///               | pattern ')' compound_list DSEMI linebreak
    ///               | '(' pattern ')' linebreak     DSEMI linebreak
    ///               | '(' pattern ')' compound_list DSEMI linebreak
    /// pattern       : WORD             /* Apply rule 4 */
    ///               | pattern '|' WORD /* Do not apply rule 4 */
    fn caseDeclaration(parser: *Parser) errors!?Command {
        _ = parser;
        return null;
    }

    /// if_clause  : If compound_list Then compound_list else_part Fi
    ///            | If compound_list Then compound_list           Fi
    fn ifDeclaration(parser: *Parser) errors!?Command {
        if (!parser.consumeToken("if", null)) {
            return null;
        }
        // TODO correctly treat errors, readlines if needed
        if (try parser.compoundList()) |condition| {
            // TODO consider if should consume ";" if any
            if (parser.consumeToken("then", null)) {
                if (try parser.compoundList()) |body| {
                    const else_decl = try parser.elseDeclaration();
                    if (parser.consumeToken("fi", null)) {
                        return try ast.IfDecl.create(parser.allocator, .{ .condition = condition, .body = body, .else_decl = else_decl });
                    }
                }
            }
        }
        return null;
    }

    /// else_part  : Elif compound_list Then compound_list
    ///            | Elif compound_list Then compound_list else_part
    ///            | Else compound_list
    fn elseDeclaration(parser: *Parser) errors!?Command {
        // TODO have errors
        if (parser.consumeToken("elif", null)) {
            if (try parser.compoundList()) |condition| {
                if (parser.consumeToken("then", null)) {
                    if (try parser.compoundList()) |body| {
                        const else_decl = try parser.elseDeclaration();
                        return try ast.IfDecl.create(parser.allocator, .{ .condition = condition, .body = body, .else_decl = else_decl });
                    }
                }
            }
        } else if (parser.consumeToken("else", null)) {
            if (try parser.compoundList()) |body| {
                return try ast.CmdGroup.create(parser.allocator, .{ .body = body, .kind = .BRACE_GROUP });
            }
        }
        return null;
    }

    /// while_clause  : While compound_list do_group
    /// until_clause  : Until compound_list do_group
    fn loopDeclaration(parser: *Parser) errors!?Command {
        var loop_kind: ast.LoopDecl.LoopKind = undefined;
        if (parser.consumeToken("while", null)) {
            loop_kind = .WHILE;
        } else if (parser.consumeToken("until", null)) {
            loop_kind = .UNTIL;
        } else {
            return null;
        }

        if (try parser.compoundList()) |condition| {
            if (try parser.doGroup()) |body| {
                return try ast.LoopDecl.create(parser.allocator, .{ .condition = condition, .body = body, .kind = loop_kind });
            }
            for (condition) |cmd_list| {
                cmd_list.deinit(parser.allocator);
            }
            parser.allocator.free(condition);
        }
        return null;
    }

    /// simple_command  : cmd_prefix cmd_name cmd_suffix
    ///                 | cmd_prefix cmd_name
    ///                 | cmd_prefix
    ///                 | cmd_name cmd_suffix
    ///                 | cmd_name
    fn simpleCommand(parser: *Parser) errors!?ast.Command {
        var cmd: ast.SimpleCommand = undefined;
        try parser.cmdPrefix(&cmd);
        cmd.name = try parser.cmdName();
        if (!cmd.isEmpty()) {
            try parser.cmdArgs(&cmd);
            return try ast.SimpleCommand.create(parser.allocator, cmd);
        }
        return null;
    }

    /// cmd_prefix  : io_redirect
    ///             | cmd_prefix io_redirect
    ///             | ASSIGNMENT_WORD
    ///             | cmd_prefix ASSIGNMENT_WORD
    fn cmdPrefix(parser: *Parser, cmd: *ast.SimpleCommand) errors!void {
        var io_redir_array = std.ArrayList(IORedir).init(parser.allocator);
        defer io_redir_array.deinit();
        var assigns_array = std.ArrayList(Assign).init(parser.allocator);
        defer assigns_array.deinit();

        while (true) {
            if (try parser.IORedirect()) |io_redir| {
                try io_redir_array.append(io_redir);
            } else if (try parser.assignmentWord()) |assign| {
                try assigns_array.append(assign);
            } else {
                break;
            }
        }

        if (io_redir_array.items.len > 0) {
            cmd.io_redirs = io_redir_array.toOwnedSlice();
        }
        if (assigns_array.items.len > 0) {
            cmd.assigns = assigns_array.toOwnedSlice();
        }
    }

    // used only as a way to quickly verify keywords
    const keywords = std.ComptimeStringMap(void, .{
        .{ "if", void },
        .{ "then", void },
        .{ "else", void },
        .{ "elif", void },
        .{ "fi", void },
        .{ "do", void },
        .{ "done", void },
        .{ "case", void },
        .{ "esac", void },
        .{ "while", void },
        .{ "until", void },
        .{ "for", void },
    });

    /// name  : NAME        * Apply rule 5 *
    fn cmdName(parser: *Parser) errors!?Word {
        // TODO apply aliases
        // TODO apply keywords
        const word_size = parser.peekWordSize();
        if (word_size == 0) {
            return try parser.word();
        }
        if (parser.peek(word_size)) |strPeek| {
            if (keywords.get(strPeek)) |void_value| {
                _ = void_value; // do nothing
            } else {
                var range: Range = undefined;
                const str = parser.readToken(word_size, &range);
                return try ast.WordString.create(parser.allocator, .{ .str = str.?, .range = range });
            }
        }
        return null;
    }

    /// cmd_suffix  : io_redirect
    ///             | cmd_suffix io_redirect
    ///             | WORD
    ///             | cmd_suffix WORD
    fn cmdArgs(parser: *Parser, cmd: *ast.SimpleCommand) errors!void {
        var word_array = std.ArrayList(Word).init(parser.allocator);
        defer word_array.deinit();
        var io_redir_array = if (cmd.io_redirs) |io_redirs|
            std.ArrayList(IORedir).fromOwnedSlice(parser.allocator, io_redirs)
        else
            std.ArrayList(IORedir).init(parser.allocator);

        defer io_redir_array.deinit();

        while (true) {
            if (try parser.IORedirect()) |io_redir| {
                try io_redir_array.append(io_redir);
            } else if (try parser.word()) |word_ptr| {
                try word_array.append(word_ptr);
            } else {
                break;
            }
        }

        if (io_redir_array.items.len > 0) {
            cmd.io_redirs = io_redir_array.toOwnedSlice();
        } else {
            cmd.io_redirs = null;
        }
        if (word_array.items.len > 0) {
            cmd.args = word_array.toOwnedSlice();
        }
    }

    /// ASSIGNMENT_WORD
    fn assignmentWord(parser: *Parser) errors!?Assign {
        if (parser.isCurrentSymbol(.TOKEN)) {
            const name_size = parser.peekNameSize();
            if (parser.peek(name_size + 1)) |str| {
                if (name_size != 0 and str[name_size] == '=') {
                    var name_range: Range = undefined;
                    if (parser.readToken(name_size, &name_range)) |name| {
                        const equal_pos = parser.currentPos;
                        _ = parser.readChar();

                        const word_value = try parser.word();
                        return Assign{ .name = name, .value = word_value, .name_range = name_range, .equal_pos = equal_pos };
                    }
                }
            }
        }
        return null;
    }

    /// io_redirect  : io_file
    ///              | IO_NUMBER io_file
    ///              | io_here
    ///              | IO_NUMBER io_here
    fn IORedirect(parser: *Parser) errors!?IORedir {
        if (try parser.IORedirFile()) |io_file| {
            return io_file;
        } else if (try parser.IORedirHere()) |io_here_doc| {
            return io_here_doc;
        } else {
            // maybe have an error here if io_number doesnt go to anyplace
            return null;
        }
    }

    /// IO_NUMBER
    fn IORedirNumber(parser: *Parser) ?u8 {
        if (parser.isCurrentSymbol(.TOKEN)) {
            if (parser.peek(2)) |numOp| {
                if (numOp[1] == '<' or numOp[1] == '>') {
                    const number = parser.read(1).?;
                    parser.resetCurrentSymbol();
                    return std.fmt.parseInt(u8, number, 10) catch null;
                }
            }
        }
        return null;
    }

    /// io_file  : '<'       filename
    ///          | LESSAND   filename
    ///          | '>'       filename
    ///          | GREATAND  filename
    ///          | DGREAT    filename
    ///          | LESSGREAT filename
    ///          | CLOBBER   filename
    fn IORedirFile(parser: *Parser) errors!?IORedir {
        var io_num_pos: ?Position = parser.currentPos;
        const io_number = parser.IORedirNumber();
        if (io_number == null) io_num_pos = null;

        var range: Range = undefined;
        if (parser.IORedirOp(&range)) |op| {
            if (try parser.IORedirFilename()) |filename| {
                return IORedir{ .io_num = io_number, .io_num_pos = io_num_pos, .name = filename, .op_range = range, .op = op };
            }
        }

        return null;
    }

    /// filename  : WORD         * Apply rule 2 *
    fn IORedirFilename(parser: *Parser) !?Word {
        return try parser.word(); // TODO improve it, making use of rule 2 of grammar
    }

    /// io_here  : DLESS     here_end
    ///          | DLESSDASH here_end
    ///
    /// here_end  : WORD            * Apply rule 3 *
    fn IORedirHere(parser: *Parser) errors!?IORedir {
        _ = parser; // TODO
        return null;
    }

    /// Gets the token operator
    fn IORedirOp(parser: *Parser, range: ?*Range) ?IORedir.IORedirKind {
        if (parser.consumeToken("<", range)) {
            return .IO_LESS;
        } else if (parser.consumeToken(">", range)) {
            return .IO_GREAT;
        } else if (parser.isOperator(.DOUBLE_GREAT, range)) {
            return .IO_DOUBLE_GREAT;
        } else if (parser.isOperator(.LESS_AND, range)) {
            return .IO_LESS_AND;
        } else if (parser.isOperator(.GREAT_AND, range)) {
            return .IO_GREAT_AND;
        } else if (parser.isOperator(.LESS_GREAT, range)) {
            return .IO_LESS_GREAT;
        } else if (parser.isOperator(.CLOBBER, range)) {
            return .IO_CLOBBER;
        } else {
            return null;
        }
    }

    /// cmd_word  : WORD                   * Apply rule 7b *
    fn word(parser: *Parser) errors!?Word {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return null;
        }
        if (parser.peekChar()) |initialChar| {
            if (isOperatorStart(initialChar) or initialChar == ')') {
                return null;
            }
        } else {
            return null;
        }

        var word_array = std.ArrayList(Word).init(parser.allocator);
        defer word_array.deinit();

        // TODO improve logic
        var n: usize = 0;
        while (parser.peek(n + 1)) |strPeek| {
            const currentChar = strPeek[n];
            const word_value: ?Word = switch (currentChar) {
                '\n', ')', '}' => break,
                '$' => try parser.wordDollar(),
                '`' => try parser.wordBackQuotes(),
                '\'' => try parser.wordSingleQuotes(),
                '"' => try parser.wordDoubleQuotes(),
                else => null,
            };

            if (word_value) |w_value| {
                // TODO consider failure (wordFunc returning a null)
                try word_array.append(w_value);
                n = 0;
            } else {
                n += 1;
            }
            if (currentChar == '\\') {
                // TODO readnewline and \\
                n += 1;
            } else if (isOperatorStart(currentChar) or std.ascii.isBlank(currentChar)) {
                n -= 1;
                break;
            }
        }

        var range: Range = undefined;
        if (parser.readToken(n, &range)) |str| {
            try word_array.append(try ast.WordString.create(parser.allocator, .{ .str = str, .range = range }));
        }

        if (word_array.items.len == 0) {
            return null;
        } else if (word_array.items.len == 1) {
            return word_array.items[0];
        } else {
            return try ast.WordList.create(parser.allocator, .{ .items = word_array.toOwnedSlice(), .is_double_quoted = false });
        }
    }

    fn wordArithmetic(parser: *Parser) !?Word {
        _ = parser;
        unreachable;
    }

    /// Creates a word command determined by back quotes
    fn wordBackQuotes(parser: *Parser) !?Word {
        // TODO consider making this a generic function, to not repeat code
        // TODO check behavior when unclosed back quotes
        std.debug.assert(parser.readChar().? == '`');

        const word_size = parser.peekWordSizeUntil('`');
        var range: Range = undefined;
        if (parser.readToken(word_size, &range)) |buffer| {
            std.debug.assert(parser.readChar().? == '`');

            var subparser = Parser.init(parser.allocator, buffer);
            const sub_program = try subparser.parse();
            return try ast.WordCommand.create(parser.allocator, .{ .program = sub_program, .is_back_quoted = true, .range = range });
        }
        // TODO treat better the parsing error
        std.debug.print("back quotes not terminated\n", .{});
        return null;
    }

    fn wordCommand(parser: *Parser) !?Word {
        std.debug.assert(parser.readChar().? == '(');

        const word_size = parser.peekWordSizeUntil(')');
        var range: Range = undefined;
        if (parser.readToken(word_size, &range)) |buffer| {
            std.debug.assert(parser.readChar().? == ')');

            var subparser = Parser.init(parser.allocator, buffer);
            const sub_program = try subparser.parse();
            return try ast.WordCommand.create(parser.allocator, .{ .program = sub_program, .is_back_quoted = false, .range = range });
        }

        return null;
    }

    fn wordDollar(parser: *Parser) !?Word {
        std.debug.assert(parser.readChar().? == '$');

        if (parser.peekChar()) |currentChar| {
            var is_special_param = false;
            switch (currentChar) {
                '{' => return try parser.wordParameterExpression(),
                '(' => {
                    if (parser.peek(2)) |next| {
                        if (next[1] == '(') {
                            return try parser.wordArithmetic();
                        } else {
                            return try parser.wordCommand();
                        }
                    } else {
                        return null; // TODO read new line of input, see ksh behavior
                    }
                },
                '@', '*', '#', '?', '-', '$', '!', '0'...'9' => is_special_param = true,
                else => {}, // TODO see if there is some error here
            }

            const name_size = blk: {
                const size = parser.peekNameSize();
                if (size != 0) {
                    break :blk size;
                } else if (is_special_param) {
                    break :blk 1;
                } else unreachable;
            };

            // TODO range
            if (parser.readToken(name_size, null)) |name| {
                return try ast.WordParameter.create(parser.allocator, .{ .name = name });
            }
        }

        // TODO analize if should return null
        return null;
    }

    fn wordDoubleQuotes(parser: *Parser) !?Word {
        const lquote_pos = parser.currentPos;
        std.debug.assert(parser.readChar().? == '"');

        var word_array = std.ArrayList(Word).init(parser.allocator);
        defer word_array.deinit();
        while (parser.peekChar()) |currentChar| {
            const word_value: ?Word = switch (currentChar) {
                '"' => break,
                '`' => try parser.wordBackQuotes(),
                '$' => try parser.wordDollar(),
                else => null,
            };

            if (word_value) |w_value| {
                try word_array.append(w_value);
            } else {
                var n: usize = 0;
                while (parser.peek(n + 1)) |strPeek| : (n += 1) {
                    switch (strPeek[n]) {
                        '"', '`', '$' => break,
                        '\\' => {
                            if (parser.peek(n + 2)) |peek_buffer| {
                                const peek_next_ch = peek_buffer[n + 1];
                                switch (peek_next_ch) {
                                    '$', '`', '"', '\\' => n += 1,
                                    '\n' => {}, // TODO: fix behavior, should ignore peek_next_ch character
                                    else => {},
                                }
                            }
                        },
                        else => {},
                    }
                }
                const begin = parser.currentPos;
                if (parser.read(n)) |str| { // TODO consider re-usage of wordString
                    const word_string = try ast.WordString.create(parser.allocator, .{ .str = str, .range = .{ .begin = begin, .end = parser.currentPos } });
                    try word_array.append(word_string);
                }
            }
        }

        std.debug.assert(parser.readChar().? == '"');

        return try ast.WordList.create(parser.allocator, .{ .items = word_array.toOwnedSlice(), .is_double_quoted = true, .left_quote_pos = lquote_pos, .right_quote_pos = parser.currentPos });
    }

    fn wordList(parser: *Parser, word_function: fn (*Parser) errors!?Word) !?Word {
        var word_array = std.ArrayList(Word).init(parser.allocator);
        defer word_array.deinit();

        while (try word_function(parser)) |word_value| {
            try word_array.append(word_value);

            var n: usize = 0;
            while (parser.peek(n + 1)) |strPeek| : (n += 1) {
                const peekCh = strPeek[n];
                if (!std.ascii.isBlank(peekCh)) {
                    break;
                }
            }

            if (n > 0) {
                const begin = parser.currentPos;
                const str = parser.read(n).?;
                try word_array.append(try ast.WordString.create(parser.allocator, .{ .str = str, .range = .{ .begin = begin, .end = parser.currentPos } }));
            } else {
                break;
            }
        }

        if (word_array.items.len == 0) {
            return null;
        } else if (word_array.items.len == 1) {
            return word_array.items[0];
        } else {
            return try ast.WordList.create(parser.allocator, .{ .items = word_array.toOwnedSlice(), .is_double_quoted = false });
        }
    }

    const ParameterOp = ast.WordParameter.ParameterOperation;

    fn wordParameterExpression(parser: *Parser) !?Word {
        std.debug.assert(parser.readChar().? == '{');

        var param_op: ParameterOp = ParameterOp.PARAMETER_NO_OP;
        if (parser.peekChar()) |ch| {
            if (ch == '#') {
                _ = parser.readChar();
                param_op = .PARAMETER_LEADING_HASH;
            }
        }
        const name_size = parser.peekNameSize();
        if (name_size == 0) {
            return null;
        }
        const name = parser.readToken(name_size, null).?;
        var has_colon = false;
        var arg: ?Word = null;
        // TODO consider if reached the end of the stream
        if (param_op == .PARAMETER_NO_OP and parser.peekChar().? != '}') {
            var ch = parser.readChar().?;
            has_colon = ch == ':';
            if (has_colon) {
                ch = parser.readChar().?;
            }
            param_op = switch (ch) {
                '-' => ParameterOp.PARAMETER_MINUS,
                '=' => ParameterOp.PARAMETER_ASSIGN,
                '?' => ParameterOp.PARAMETER_MAYBE,
                '+' => ParameterOp.PARAMETER_PLUS,
                else => blk: {
                    if (has_colon) return null; // TODO have some error, see behavior of ksh

                    const peek_next_ch = parser.peekChar().?;
                    if (ch == peek_next_ch) {
                        _ = parser.readChar(); // consume doubled char
                        switch (ch) {
                            '%' => break :blk ParameterOp.PARAMETER_DOUBLE_PERCENT,
                            '#' => break :blk ParameterOp.PARAMETER_DOUBLE_HASH,
                            else => return null, // TODO have better handling here, see behavior of ksh
                        }
                    } else {
                        switch (ch) {
                            '%' => break :blk ParameterOp.PARAMETER_PERCENT,
                            '#' => break :blk ParameterOp.PARAMETER_HASH,
                            else => return null, // TODO have better handling here, see behavior of ksh
                        }
                    }
                    unreachable;
                },
            };

            arg = try parser.wordList(word);
        }

        std.debug.assert(parser.readChar().? == '}');

        return try ast.WordParameter.create(parser.allocator, .{ .name = name, .arg = arg, .op = param_op, .has_colon = has_colon });
    }

    fn wordSingleQuotes(parser: *Parser) !?Word {
        std.debug.assert(parser.readChar().? == '\'');

        const word_size = parser.peekWordSizeUntil('\'');
        var range: Range = undefined;
        if (parser.readToken(word_size, &range)) |str| {
            std.debug.assert(parser.readChar().? == '\'');

            return try ast.WordString.create(parser.allocator, .{ .str = str, .is_single_quoted = true, .range = range });
        }
        // TODO error or read new line

        return null;
    }

    fn wordString(parser: *Parser) !?Word {
        const len = parser.peekWordSize();
        var word_range: Range = undefined;
        if (parser.readToken(len, &word_range)) |str| {
            return try ast.WordString.create(parser.allocator, .{ .str = str, .range = word_range });
        }
        return null;
    }

    /// Peeks the word size until `endChar`, or 0 otherwise
    fn peekWordSizeUntil(parser: *Parser, endChar: u8) usize {
        // TODO read newline
        var n: usize = 0;
        while (parser.peek(n + 1)) |strPeek| : (n += 1) {
            if (strPeek[n] == endChar) {
                return n;
            }
        }
        return 0;
    }

    /// Peeks the size of the current word
    fn peekWordSize(parser: *Parser) u8 {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return 0;
        }

        var n: u8 = 0;
        while (parser.peek(n + 1)) |strPeek| : (n += 1) {
            const ch = strPeek[n];
            switch (ch) {
                '\n', ')', '}' => return n, // TODO this } maybe is wrong placed here, consider if it is worth it
                '$', '`', '\'', '"' => return 0,
                '\\' => n += 1,
                else => {
                    if (isOperatorStart(ch) or std.ascii.isBlank(ch)) {
                        break;
                    }
                },
            }
        }
        return n;
    }

    /// A name in the shell is composition of underscores, digits
    /// and alphanumerics, the first character can not be a digit
    fn peekNameSize(parser: *Parser) u8 {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return 0;
        }

        var n: u8 = 0;
        // TODO test, maybe add in_brace param bool
        while (parser.peek(n + 1)) |strPeek| : (n += 1) {
            const ch = strPeek[n];
            if ((ch != '_' and !std.ascii.isAlNum(ch)) or
                (n == 0 and std.ascii.isDigit(ch)))
            {
                break;
            }
        }
        return n;
    }

    /// Gets string until `len`, returns `null` if len plus
    /// current position is bigger than parser source size
    /// See `read`
    fn peek(parser: *Parser, len: usize) ?[]const u8 {
        const begin = parser.currentPos.line + parser.currentPos.column - 1;
        const end = begin + len;
        if (end > parser.source.len) return null;
        return parser.source[begin..end];
    }

    /// Peeks and returns a character
    fn peekChar(parser: *Parser) ?u8 {
        return if (parser.peek(1)) |str| str[0] else null;
    }

    /// Returns `null` if `len` plus current position is bigger
    /// than parser source size. If not, returns the string from
    /// current position until `len` modifying the current position
    /// to after the returned string
    fn read(parser: *Parser, len: usize) ?[]const u8 {
        const string = parser.peek(len);
        if (parser.peek(len)) |str| {
            // TODO fix behavior
            for (str) |_| {
                parser.currentPos.column += 1;
            }
        }
        return string;
    }

    /// Reads character based on current position and returns it,
    /// returns null if invalid.
    /// See `read`
    fn readChar(parser: *Parser) ?u8 {
        return if (parser.read(1)) |str| str[0] else null;
    }

    /// Reads token from current position until `len`, updating the `range`
    /// See `read`
    fn readToken(parser: *Parser, len: usize, range: ?*Range) ?[]const u8 {
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

    /// Consumes the current token if it matches the `str`, updating the
    /// `range` accordingly (if any), returns `true` if there is a match
    /// and `false` otherwise.
    fn consumeToken(parser: *Parser, str: []const u8, range: ?*Range) bool {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return false;
        }

        if (str.len == 1 and !std.ascii.isAlpha(str[0])) {
            if (parser.peekChar() != str[0]) {
                return false;
            }
            _ = parser.readToken(1, range);
        } else {
            const word_str = parser.peek(parser.peekWordSize()).?;
            if (!mem.eql(u8, str, word_str)) {
                return false;
            }
            _ = parser.readToken(str.len, range);
        }

        return true;
    }

    /// linebreak
    fn linebreak(parser: *Parser) void {
        while (parser.isNewLine()) {}
    }

    /// newline_list
    fn consumeNewLine(parser: *Parser) bool {
        if (!parser.isNewLine()) {
            return false;
        }
        parser.linebreak();
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

    /// Resets current symbol of the parser.
    fn resetCurrentSymbol(parser: *Parser) void {
        parser.currentSymbol = null;
    }

    /// Check whenether current symbol of the parser matches `symbol`.
    fn isCurrentSymbol(parser: *Parser, symbol: Symbol) bool {
        if (parser.currentSymbol == null) {
            parser.readSymbol();
        }
        return parser.currentSymbol.? == symbol;
    }

    /// Reads symbol and changes the current symbol of the parser.
    fn readSymbol(parser: *Parser) void {
        if (parser.peekChar()) |c| {
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
                return;
            } else if (c == '#') {
                while (parser.peekChar()) |ch| {
                    if (ch == '\n') {
                        break;
                    }
                    _ = parser.readChar();
                }
                parser.readSymbol(); // TODO consider use loop
                return;
            }

            if (parser.currentSymbol == null) parser.currentSymbol = .TOKEN;
        } else {
            // TODO maybe wrong?
            parser.currentSymbol = .EOF;
        }
    }

    /// Checks if `c` is the beginning of an operator.
    /// See `isOperator`
    fn isOperatorStart(c: u8) bool {
        return switch (c) {
            '<', '>', '|', '&', ';' => true,
            else => false,
        };
    }

    /// Checks current symbol of the parser and if it is an operator,
    /// updating `range` accordingly.
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

        _ = parser.read(operator_string_len); // consume operator

        if (range) |r| {
            r.begin = begin;
            r.end = parser.currentPos;
        }

        parser.resetCurrentSymbol();
        return true;
    }

    fn isNewLine(parser: *Parser) bool {
        if (!parser.isCurrentSymbol(.NEWLINE)) {
            return false;
        }
        std.debug.assert(parser.readChar().? == '\n');
        parser.resetCurrentSymbol();
        return true;
    }
};

test "SubShell" {
    // TODO
}

test "Group Command" {
    // TODO
}

test "Parse Command List Separator" {
    const command_string = "echo oi;print test; builtin pwd &";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);
    // program.print(1);

    const cmd_array = program.body;
    try testing.expect(cmd_array.len == 3);

    // TODO verify name of each command
    try testing.expect(cmd_array[0].is_async == false);
    const simple_command1 = cmd_array[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(mem.eql(u8, simple_command1.name.?.cast(.STRING).?.str, "echo"));

    try testing.expect(cmd_array[1].is_async == false);
    const simple_command2 = cmd_array[1].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(mem.eql(u8, simple_command2.name.?.cast(.STRING).?.str, "print"));

    try testing.expect(cmd_array[2].is_async == true);
    const simple_command3 = cmd_array[2].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(mem.eql(u8, simple_command3.name.?.cast(.STRING).?.str, "builtin"));
}

test "Parse And Or Cmd List" {
    const command_string = "pgrep kzh && echo kzh is running || echo kzh is not running";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);
    // program.print(1);

    try testing.expect(program.body.len == 1);

    const cmd_list = program.body[0];
    try testing.expect(cmd_list.is_async == false);

    const binary_op_left = cmd_list.and_or_cmd_list.cast(.BINARY_OP);
    try testing.expect(binary_op_left != null);
    try testing.expect(binary_op_left.?.kind == .AND);
    try testing.expect(binary_op_left.?.left.cast(.PIPELINE) != null);

    const binary_op_right = binary_op_left.?.right.cast(.BINARY_OP);
    try testing.expect(binary_op_right != null);
    try testing.expect(binary_op_right.?.kind == .OR);

    try testing.expect(binary_op_right.?.left.cast(.PIPELINE) != null);
    try testing.expect(binary_op_right.?.right.cast(.PIPELINE) != null);
}

test "Parse Pipeline" {
    const command_string = "head somefile | grep 'Hello World' | tr '\n' ' '";

    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);
    // program.print(1);

    const pipeline = program.body[0].and_or_cmd_list.cast(.PIPELINE).?;
    try testing.expect(pipeline.commands.len == 3);

    const cmd1 = pipeline.commands[0].cast(.SIMPLE_COMMAND).?;
    const cmd1_args = cmd1.args.?;
    try testing.expect(mem.eql(u8, cmd1.name.?.cast(.STRING).?.str, "head"));
    try testing.expect(mem.eql(u8, cmd1_args[0].cast(.STRING).?.str, "somefile"));

    const cmd2 = pipeline.commands[1].cast(.SIMPLE_COMMAND).?;
    const cmd2_args = cmd2.args.?;
    try testing.expect(mem.eql(u8, cmd2.name.?.cast(.STRING).?.str, "grep"));
    try testing.expect(mem.eql(u8, cmd2_args[0].cast(.STRING).?.str, "Hello World"));

    const cmd3 = pipeline.commands[2].cast(.SIMPLE_COMMAND).?;
    const cmd3_args = cmd3.args.?;
    try testing.expect(mem.eql(u8, cmd3.name.?.cast(.STRING).?.str, "tr"));
    try testing.expect(mem.eql(u8, cmd3_args[0].cast(.STRING).?.str, "\n"));
    try testing.expect(mem.eql(u8, cmd3_args[1].cast(.STRING).?.str, " "));

    // TODO test pipeline Bangs
}

test "Parse Simple Command" {
    const command_string = "echo hi";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);
    // program.print(1);

    const pipeline = program.body[0].and_or_cmd_list.cast(.PIPELINE).?;
    try testing.expect(pipeline.commands.len == 1);

    const simple_command = pipeline.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command.name != null);
    const simple_command_name = simple_command.name.?.cast(.STRING).?;
    try testing.expect(mem.eql(u8, simple_command_name.str, "echo"));

    try testing.expect(simple_command.args != null);
    const args = simple_command.args.?;
    try testing.expect(args.len == 1);
    try testing.expect(mem.eql(u8, args[0].cast(.STRING).?.str, "hi"));

    try testing.expect(simple_command.assigns == null);
    try testing.expect(simple_command.io_redirs == null);
}

test "Parse Simple Command with IO redirection" {
    const command_string1 = "ls >/dev/null 2>&1";
    var parser1 = Parser.init(testing.allocator, command_string1);
    const program1 = try parser1.parse();
    defer program1.deinit(testing.allocator);
    // program1.print(1);

    const simple_command1 = program1.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command1.name != null);
    try testing.expect(simple_command1.args == null);
    try testing.expect(simple_command1.assigns == null);

    try testing.expect(simple_command1.io_redirs != null);
    try testing.expect(simple_command1.io_redirs.?.len == 2);

    const io_redir1 = simple_command1.io_redirs.?[0];
    try testing.expect(io_redir1.io_num == null);
    try testing.expect(io_redir1.io_num_pos == null);
    try testing.expect(io_redir1.here_doc == null);
    try testing.expect(io_redir1.op == .IO_GREAT);
    try testing.expect(mem.eql(u8, io_redir1.name.cast(.STRING).?.str, "/dev/null"));

    const io_redir2 = simple_command1.io_redirs.?[1];
    try testing.expect(io_redir2.io_num != null);
    try testing.expect(io_redir2.io_num.? == 2);
    try testing.expect(io_redir2.here_doc == null);
    try testing.expect(io_redir2.op == .IO_GREAT_AND);
    try testing.expect(mem.eql(u8, io_redir2.name.cast(.STRING).?.str, "1"));
}

test "Parse Simple Command Assignments" {
    const command_string1 = "some=thing else=where ls";
    var parser1 = Parser.init(testing.allocator, command_string1);
    const program1 = try parser1.parse();
    defer program1.deinit(testing.allocator);
    // program1.print(1);

    const simple_command1 = program1.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command1.name != null);
    try testing.expect(simple_command1.args == null);
    try testing.expect(simple_command1.io_redirs == null);

    try testing.expect(simple_command1.assigns != null);
    const assigns1 = simple_command1.assigns.?;
    try testing.expect(assigns1.len == 2);
    try testing.expect(mem.eql(u8, assigns1[0].name, "some"));
    try testing.expect(assigns1[0].value != null);
    try testing.expect(mem.eql(u8, assigns1[0].value.?.cast(.STRING).?.str, "thing"));
    try testing.expect(mem.eql(u8, assigns1[1].name, "else"));
    try testing.expect(assigns1[1].value != null);
    try testing.expect(mem.eql(u8, assigns1[1].value.?.cast(.STRING).?.str, "where"));

    const command_string2 = "only=envvar";
    var parser2 = Parser.init(testing.allocator, command_string2);
    const program2 = try parser2.parse();
    defer program2.deinit(testing.allocator);
    // program2.print(1);

    const simple_command2 = program2.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command2.name == null);
    try testing.expect(simple_command2.args == null);
    try testing.expect(simple_command2.io_redirs == null);

    try testing.expect(simple_command2.assigns != null);
    const assigns2 = simple_command2.assigns.?;
    try testing.expect(assigns2.len == 1);
    try testing.expect(mem.eql(u8, assigns2[0].name, "only"));
    try testing.expect(assigns2[0].value != null);
    try testing.expect(mem.eql(u8, assigns2[0].value.?.cast(.STRING).?.str, "envvar"));
}

test "Parse Word Arithmetic" {
    // TODO
}

test "Parse Word Command" {
    const command_string1 = "ps_mem -p -s $(pgrep kzh)";
    var parser1 = Parser.init(testing.allocator, command_string1);
    const program1 = try parser1.parse();
    defer program1.deinit(testing.allocator);
    // program1.print(1);

    const simple_command1 = program1.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command1.name != null);
    try testing.expect(simple_command1.name.?.kind == .STRING);
    try testing.expect(mem.eql(u8, simple_command1.name.?.cast(.STRING).?.str, "ps_mem"));
    try testing.expect(simple_command1.args != null);

    const args1 = simple_command1.args.?;
    try testing.expect(args1[0].kind == .STRING);
    try testing.expect(mem.eql(u8, args1[0].cast(.STRING).?.str, "-p"));
    try testing.expect(args1[1].kind == .STRING);
    try testing.expect(mem.eql(u8, args1[1].cast(.STRING).?.str, "-s"));
    try testing.expect(args1[2].kind == .COMMAND);
    try testing.expect(args1[2].cast(.COMMAND).?.is_back_quoted == false);

    const sub_program1 = args1[2].cast(.COMMAND).?.program;
    try testing.expect(sub_program1 != null);

    const sub_simple_cmd1 = sub_program1.?.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(sub_simple_cmd1.name != null);
    try testing.expect(sub_simple_cmd1.name.?.kind == .STRING);
    try testing.expect(mem.eql(u8, sub_simple_cmd1.name.?.cast(.STRING).?.str, "pgrep"));
    try testing.expect(sub_simple_cmd1.args != null);

    const sub_args1 = sub_simple_cmd1.args.?;
    try testing.expect(args1[0].kind == .STRING);
    try testing.expect(mem.eql(u8, sub_args1[0].cast(.STRING).?.str, "kzh"));

    const command_string2 = "ls `pwd`";
    var parser2 = Parser.init(testing.allocator, command_string2);
    const program2 = try parser2.parse();
    defer program2.deinit(testing.allocator);
    // program2.print(1);

    const simple_command2 = program2.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command2.name != null);
    try testing.expect(simple_command2.name.?.kind == .STRING);
    try testing.expect(mem.eql(u8, simple_command2.name.?.cast(.STRING).?.str, "ls"));
    try testing.expect(simple_command2.args != null);

    const args2 = simple_command2.args.?;
    try testing.expect(args2[0].kind == .COMMAND);
    try testing.expect(args2[0].cast(.COMMAND).?.is_back_quoted == true);

    const sub_program2 = args2[0].cast(.COMMAND).?.program;
    try testing.expect(sub_program2 != null);

    const sub_simple_cmd2 = sub_program2.?.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(sub_simple_cmd2.name != null);
    try testing.expect(sub_simple_cmd2.name.?.kind == .STRING);
    try testing.expect(mem.eql(u8, sub_simple_cmd2.name.?.cast(.STRING).?.str, "pwd"));
}

test "Parse Word Parameter" {
    const command_string = "echo $HOME ${NOTHOME:-not home} ${VAR:=PWD} ${VAR:+HOME} ${NOTHOME:?VAR} ${#HOME} ${SOMESCRIPT1#.sh} ${SOMESCRIPT22##.sh} ${SOMESCRIPT333%.sh} ${S0MESCRIPT4%%.sh}";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);
    // program.print(1);

    const simple_command = program.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command.args != null);

    const args = simple_command.args.?;
    try testing.expect(args.len == 10);

    const parameter_names = [_][]const u8{ "HOME", "NOTHOME", "VAR", "VAR", "NOTHOME", "HOME", "SOMESCRIPT1", "SOMESCRIPT22", "SOMESCRIPT333", "S0MESCRIPT4" };
    var param_array: [10]*ast.WordParameter = undefined;
    for (args) |arg, index| {
        try testing.expect(arg.kind == .PARAMETER);
        const param = arg.cast(.PARAMETER).?;
        param_array[index] = param;
        try testing.expect(mem.eql(u8, param.name, parameter_names[index]));
    }

    try testing.expect(param_array[0].op == .PARAMETER_NO_OP);
    try testing.expect(param_array[0].has_colon == false);
    try testing.expect(param_array[0].arg == null);

    try testing.expect(param_array[1].op == .PARAMETER_MINUS);
    try testing.expect(param_array[1].has_colon == true);
    try testing.expect(param_array[1].arg != null);

    try testing.expect(param_array[2].op == .PARAMETER_ASSIGN);
    try testing.expect(param_array[2].has_colon == true);
    try testing.expect(param_array[2].arg != null);

    try testing.expect(param_array[3].op == .PARAMETER_PLUS);
    try testing.expect(param_array[3].has_colon == true);
    try testing.expect(param_array[3].arg != null);

    try testing.expect(param_array[4].op == .PARAMETER_MAYBE);
    try testing.expect(param_array[4].has_colon == true);
    try testing.expect(param_array[4].arg != null);

    try testing.expect(param_array[5].op == .PARAMETER_LEADING_HASH);
    try testing.expect(param_array[5].has_colon == false);
    try testing.expect(param_array[5].arg == null);

    try testing.expect(param_array[6].op == .PARAMETER_HASH);
    try testing.expect(param_array[6].has_colon == false);
    try testing.expect(param_array[6].arg != null);

    try testing.expect(param_array[7].op == .PARAMETER_DOUBLE_HASH);
    try testing.expect(param_array[7].has_colon == false);
    try testing.expect(param_array[7].arg != null);

    try testing.expect(param_array[8].op == .PARAMETER_PERCENT);
    try testing.expect(param_array[8].has_colon == false);
    try testing.expect(param_array[8].arg != null);

    try testing.expect(param_array[9].op == .PARAMETER_DOUBLE_PERCENT);
    try testing.expect(param_array[9].has_colon == false);
    try testing.expect(param_array[9].arg != null);
}

test "Parse Word Quotes and backslash" {
    const command_string = "echo \"home $HOME dir\" '$NOTVAR' word\\ with\\ spaces";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);
    // program.print(1);

    const simple_command = program.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command.args != null);

    const args = simple_command.args.?;
    try testing.expect(args[0].kind == .LIST);
    try testing.expect(args[1].kind == .STRING);
    try testing.expect(args[2].kind == .STRING);

    const word_arg1 = args[0].cast(.LIST).?;
    try testing.expect(word_arg1.is_double_quoted == true);
    try testing.expect(word_arg1.items.len == 3);

    try testing.expect(word_arg1.items[0].kind == .STRING);
    try testing.expect(mem.eql(u8, word_arg1.items[0].cast(.STRING).?.str, "home "));

    try testing.expect(word_arg1.items[1].kind == .PARAMETER);
    try testing.expect(word_arg1.items[1].cast(.PARAMETER).?.op == .PARAMETER_NO_OP);

    try testing.expect(word_arg1.items[2].kind == .STRING);
    try testing.expect(mem.eql(u8, word_arg1.items[2].cast(.STRING).?.str, " dir"));

    const word_arg2 = args[1].cast(.STRING).?;
    try testing.expect(word_arg2.is_single_quoted == true);
    try testing.expect(mem.eql(u8, word_arg2.str, "$NOTVAR"));

    const word_arg3 = args[2].cast(.STRING).?;
    try testing.expect(word_arg3.is_single_quoted == false);
    try testing.expect(mem.eql(u8, word_arg3.str, "word\\ with\\ spaces"));
}
