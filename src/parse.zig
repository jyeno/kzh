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
//! %token  In
//!        'in'

// TODO: maybe create parse/word.zig and parse/command.zig
// TODO: create errors instead of only returning null on some functions
const std = @import("std");
const mem = std.mem;
const ast = @import("ast.zig");
const AndOrCmdList = ast.AndOrCmdList;
const Command = ast.Command;
const Word = ast.Word;
const IORedir = ast.IORedir;
const Assign = ast.Assign;

/// Internal helper struct to parsing the tokens
pub const Parser = struct {
    /// source is not owned by the parser
    source: []const u8,
    allocator: *mem.Allocator,
    currentPos: usize = 0,
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

        while (try parser.commandList()) |cmd_list| {
            try command_list_array.append(cmd_list);
        }

        return try ast.create(parser.allocator, ast.Program, .{ .body = command_list_array.toOwnedSlice() });
    }

    /// list  : list separator_op and_or
    ///       | and_or
    fn commandList(parser: *Parser) !?*ast.CommandList {
        // TODO fix behavior, improve logic
        if (try parser.andOrCmdList()) |and_or_cmd_list| {
            var command_list: ast.CommandList = .{ .and_or_cmd_list = and_or_cmd_list };

            if (parser.separator()) |sep| {
                if (sep == '&') {
                    command_list.is_async = true;
                }
            }

            return try ast.create(parser.allocator, ast.CommandList, command_list);
        }
        return null;
    }

    /// separator_op  : '&'
    ///               | ';'
    fn separatorOperator(parser: *Parser) ?u8 {
        if (parser.consumeToken("&")) {
            return '&';
        } else if (parser.consumeToken(";")) {
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
        ExpectedToken,
    };

    /// and_or  : pipeline
    ///         | and_or AND_IF linebreak pipeline
    ///         | and_or OR_IF  linebreak pipeline
    fn andOrCmdList(parser: *Parser) errors!?AndOrCmdList {
        if (try parser.pipeline()) |pl| {
            var bin_op_kind: ast.BinaryOp.BinaryOpKind = undefined;
            if (parser.isOperator(.AND)) {
                bin_op_kind = ast.BinaryOp.BinaryOpKind.AND;
            } else if (parser.isOperator(.OR)) {
                bin_op_kind = ast.BinaryOp.BinaryOpKind.OR;
            } else {
                return pl;
            }
            parser.linebreak();
            if (try parser.andOrCmdList()) |and_or_right| {
                return (try ast.create(parser.allocator, ast.BinaryOp, .{ .left = pl, .right = and_or_right, .kind = bin_op_kind })).andOrCmd();
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

        const has_bang = parser.consumeToken("!");

        if (try parser.command()) |cmd| {
            try command_array.append(cmd);
        } else {
            return null;
        }

        while (parser.consumeToken("|")) {
            parser.linebreak();
            if (try parser.command()) |cmd| {
                try command_array.append(cmd);
            } else {
                // TODO maybe have an error, see if needed
                break;
            }
        }

        return (try ast.create(parser.allocator, ast.Pipeline, .{ .commands = command_array.toOwnedSlice(), .has_bang = has_bang })).andOrCmd();
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
        const has_func_keyword = parser.consumeToken("function");
        const name_size = parser.peekNameSize();
        if (name_size == 0) {
            return null;
        } else if (!has_func_keyword) {
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
        const name = parser.readToken(name_size).?;

        if (!has_func_keyword and (!parser.consumeToken("(") or !parser.consumeToken(")"))) {
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
                return (try ast.create(parser.allocator, ast.FuncDecl, .{ .name = name, .body = cmd, .io_redirs = io_array.toOwnedSlice() })).cmd();
            } else {
                return (try ast.create(parser.allocator, ast.FuncDecl, .{ .name = name, .body = cmd })).cmd();
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
        if (!parser.consumeToken("{")) {
            closing_char = ")";
            if (!parser.consumeToken("(")) {
                return null;
            }
        }
        if (try parser.compoundList(closing_char)) |body| {
            // TODO read newline until lchar
            const cmd_group_kind: ast.CmdGroup.GroupKind = if (closing_char[0] == '}') .BRACE_GROUP else .SUBSHELL;
            const cmd_group = try ast.create(parser.allocator, ast.CmdGroup, .{ .body = body, .kind = cmd_group_kind });
            return cmd_group.cmd();
        }
        return null;
    }

    /// compound_list  : linebreak term
    ///                | linebreak term separator
    /// term           : term separator and_or
    ///                | and_or
    fn compoundList(parser: *Parser, endToken: ?[]const u8) errors!?[]*ast.CommandList {
        // TODO read until endToken, if it is on the end, read new line
        parser.linebreak();
        var cmd_list_array = std.ArrayList(*ast.CommandList).init(parser.allocator);
        defer {
            for (cmd_list_array.items) |cmd_list| {
                cmd_list.deinit(parser.allocator);
            }
            cmd_list_array.deinit();
        }

        while (try parser.andOrCmdList()) |and_or_cmd| {
            // std.debug.print("\npeek 6: {s}\n", .{parser.peek(6)});
            // TODO here_document
            if (parser.separator()) |sep| {
                try cmd_list_array.append(try ast.create(parser.allocator, ast.CommandList, .{ .and_or_cmd_list = and_or_cmd, .is_async = sep == '&' }));
            } else {
                try cmd_list_array.append(try ast.create(parser.allocator, ast.CommandList, .{ .and_or_cmd_list = and_or_cmd }));
            }
        }
        if (cmd_list_array.items.len > 0) {
            if (endToken != null and !parser.consumeToken(endToken.?)) {
                // return error.ExpectedToken;
            }
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
        // TODO fix, still buggy
        var is_selection = false;
        if (!parser.consumeToken("for")) {
            if (!parser.consumeToken("select")) {
                return null;
            }
            is_selection = true;
        }
        const name_size = parser.peekNameSize();
        if (name_size == 0) {
            // TODO have error
            return null;
        }
        const name = parser.read(name_size).?;
        parser.linebreak();
        const has_in = parser.consumeToken("in");
        const for_list: ?[]Word = blk: {
            var word_array = std.ArrayList(Word).init(parser.allocator);
            defer word_array.deinit();
            if (has_in) {
                // wordlist
                while (try parser.word()) |word_val| {
                    try word_array.append(word_val);
                }
            }

            const has_sep = sequential_sep: {
                if (parser.consumeToken(";")) {
                    parser.linebreak();
                    break :sequential_sep true;
                }
                break :sequential_sep parser.consumeNewLine();
            };

            if (word_array.items.len > 0) {
                // TODO error if has_sep is false
                _ = has_sep;
                break :blk word_array.toOwnedSlice();
            } else {
                break :blk null;
            }
        };
        if (try parser.doGroup()) |body| {
            return (try ast.create(parser.allocator, ast.ForDecl, .{ .name = name, .has_in = has_in, .list = for_list, .body = body, .is_selection = is_selection })).cmd();
        }
        // TODO error if comes here
        return null;
    }

    /// do_group : Do compound_list Done   /* Apply rule 6 */
    fn doGroup(parser: *Parser) errors!?[]*ast.CommandList {
        if (!parser.consumeToken("do")) {
            return null;
        }
        if (try parser.compoundList("done")) |body| {
            // for (body) |cmdd| {
            //     std.debug.print("\ndone: {}\n\n", .{cmdd});
            // }
            return body;
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
        if (!parser.consumeToken("if")) {
            return null;
        }
        // TODO correctly treat errors, readlines if needed
        if (try parser.compoundList("then")) |condition| {
            errdefer {
                for (condition) |cmd_list| {
                    cmd_list.deinit(parser.allocator);
                }
                parser.allocator.free(condition);
            }
            if (try parser.compoundList(null)) |body| {
                const else_decl = try parser.elseDeclaration();

                if (parser.consumeToken("fi")) {
                    return (try ast.create(parser.allocator, ast.IfDecl, .{ .condition = condition, .body = body, .else_decl = else_decl })).cmd();
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
        if (parser.consumeToken("elif")) {
            if (try parser.compoundList("then")) |condition| {
                errdefer {
                    for (condition) |cmd_list| {
                        cmd_list.deinit(parser.allocator);
                    }
                    parser.allocator.free(condition);
                }
                if (try parser.compoundList(null)) |body| {
                    errdefer {
                        for (body) |cmd_list| {
                            cmd_list.deinit(parser.allocator);
                        }
                        parser.allocator.free(body);
                    }
                    const else_decl = try parser.elseDeclaration();
                    return (try ast.create(parser.allocator, ast.IfDecl, .{ .condition = condition, .body = body, .else_decl = else_decl })).cmd();
                }
            }
        } else if (parser.consumeToken("else")) {
            if (try parser.compoundList(null)) |body| {
                return (try ast.create(parser.allocator, ast.CmdGroup, .{ .body = body, .kind = .BRACE_GROUP })).cmd();
            }
        }
        return null;
    }

    /// while_clause  : While compound_list do_group
    /// until_clause  : Until compound_list do_group
    fn loopDeclaration(parser: *Parser) errors!?Command {
        var loop_kind: ast.LoopDecl.LoopKind = undefined;
        if (parser.consumeToken("while")) {
            loop_kind = .WHILE;
        } else if (parser.consumeToken("until")) {
            loop_kind = .UNTIL;
        } else {
            return null;
        }

        if (try parser.compoundList(null)) |condition| {
            errdefer {
                for (condition) |cmd_list| {
                    cmd_list.deinit(parser.allocator);
                }
                parser.allocator.free(condition);
            }
            // std.debug.print("peek before do group: {s}\n", .{parser.peek(15)});
            if (try parser.doGroup()) |body| {
                // std.debug.print("peek end loop: {s}\n", .{parser.peek(9)});
                return (try ast.create(parser.allocator, ast.LoopDecl, .{ .condition = condition, .body = body, .kind = loop_kind })).cmd();
            }
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
            return (try ast.create(parser.allocator, ast.SimpleCommand, cmd)).cmd();
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

    // used only as a way to quickly verify reserved keywords
    const reserved_keywords = std.ComptimeStringMap(void, .{
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
        .{ "function", void },
        .{ "in", void },
        .{ "select", void },
        .{ "time", void },
        .{ "!", void },
        .{ "(", void },
        .{ ")", void },
        .{ "((", void },
        .{ "))", void },
        .{ "[[", void },
        .{ "]]", void },
        .{ "{", void },
        .{ "}", void },
    });

    /// name  : NAME        * Apply rule 5 *
    fn cmdName(parser: *Parser) errors!?Word {
        // TODO apply aliases
        const word_size = parser.peekWordSize();
        if (word_size == 0) {
            return try parser.word();
        }
        if (parser.peek(word_size)) |strPeek| {
            if (reserved_keywords.get(strPeek)) |void_value| {
                _ = void_value; // do nothing
            } else {
                const str = parser.readToken(word_size);
                return (try ast.create(parser.allocator, ast.WordString, .{ .str = str.? })).word();
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
                    if (parser.readToken(name_size)) |name| {
                        _ = parser.readChar();
                        return Assign{ .name = name, .value = try parser.word() };
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
        const io_number = parser.IORedirNumber();
        if (parser.IORedirOp()) |op| {
            if (try parser.IORedirFilename()) |filename| {
                return IORedir{ .io_num = io_number, .name = filename, .op = op };
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
    fn IORedirOp(parser: *Parser) ?IORedir.IORedirKind {
        if (parser.consumeToken("<")) {
            return .IO_LESS;
        } else if (parser.consumeToken(">")) {
            return .IO_GREAT;
        } else if (parser.isOperator(.DOUBLE_GREAT)) {
            return .IO_DOUBLE_GREAT;
        } else if (parser.isOperator(.LESS_AND)) {
            return .IO_LESS_AND;
        } else if (parser.isOperator(.GREAT_AND)) {
            return .IO_GREAT_AND;
        } else if (parser.isOperator(.LESS_GREAT)) {
            return .IO_LESS_GREAT;
        } else if (parser.isOperator(.CLOBBER)) {
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
                '`' => try parser.wordCommand(true),
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

        if (parser.readToken(n)) |str| {
            try word_array.append((try ast.create(parser.allocator, ast.WordString, .{ .str = str })).word());
        }

        if (word_array.items.len == 0) {
            return null;
        } else if (word_array.items.len == 1) {
            return word_array.items[0];
        } else {
            return (try ast.create(parser.allocator, ast.WordList, .{ .items = word_array.toOwnedSlice(), .is_double_quoted = false })).word();
        }
    }

    fn wordArithmetic(parser: *Parser) !?Word {
        _ = parser;
        unreachable;
    }

    fn wordCommand(parser: *Parser, back_quotes: bool) !?Word {
        const open_close_ch = if (back_quotes) "``" else "()";
        std.debug.assert(parser.readChar().? == open_close_ch[0]);

        const word_size = parser.peekWordSizeUntil(open_close_ch[1]);
        if (parser.readToken(word_size)) |buffer| {
            std.debug.assert(parser.readChar().? == open_close_ch[1]);

            var subparser = Parser.init(parser.allocator, buffer);
            const sub_program = try subparser.parse();
            return (try ast.create(parser.allocator, ast.WordCommand, .{ .program = sub_program, .is_back_quoted = back_quotes })).word();
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
                            return try parser.wordCommand(false);
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

            if (parser.readToken(name_size)) |name| {
                return (try ast.create(parser.allocator, ast.WordParameter, .{ .name = name })).word();
            }
        }

        // TODO analize if should return null
        return null;
    }

    fn wordDoubleQuotes(parser: *Parser) !?Word {
        std.debug.assert(parser.readChar().? == '"');

        var word_array = std.ArrayList(Word).init(parser.allocator);
        defer word_array.deinit();
        while (parser.peekChar()) |currentChar| {
            const word_value: ?Word = switch (currentChar) {
                '"' => break,
                '`' => try parser.wordCommand(true),
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
                if (parser.read(n)) |str| { // TODO consider re-usage of wordString
                    const word_string = (try ast.create(parser.allocator, ast.WordString, .{ .str = str })).word();
                    try word_array.append(word_string);
                }
            }
        }

        std.debug.assert(parser.readChar().? == '"');
        return (try ast.create(parser.allocator, ast.WordList, .{ .items = word_array.toOwnedSlice(), .is_double_quoted = true })).word();
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
                const str = parser.read(n).?;
                try word_array.append((try ast.create(parser.allocator, ast.WordString, .{ .str = str })).word());
            } else {
                break;
            }
        }

        if (word_array.items.len == 0) {
            return null;
        } else if (word_array.items.len == 1) {
            return word_array.items[0];
        } else {
            return (try ast.create(parser.allocator, ast.WordList, .{ .items = word_array.toOwnedSlice(), .is_double_quoted = false })).word();
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
        const name = parser.readToken(name_size).?;
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

        return (try ast.create(parser.allocator, ast.WordParameter, .{ .name = name, .arg = arg, .op = param_op, .has_colon = has_colon })).word();
    }

    fn wordSingleQuotes(parser: *Parser) !?Word {
        std.debug.assert(parser.readChar().? == '\'');

        const word_size = parser.peekWordSizeUntil('\'');
        if (parser.readToken(word_size)) |str| {
            std.debug.assert(parser.readChar().? == '\'');

            return (try ast.create(parser.allocator, ast.WordString, .{ .str = str, .is_single_quoted = true })).word();
        }
        // TODO error or read new line

        return null;
    }

    fn wordString(parser: *Parser) !?Word {
        const len = parser.peekWordSize();
        if (parser.readToken(len)) |str| {
            return (try ast.create(parser.allocator, ast.WordString, .{ .str = str })).word();
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
        const begin = parser.currentPos;
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
                parser.currentPos += 1;
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

    /// Reads token from current position until `len`l reseting the current symbol
    /// See `read`
    fn readToken(parser: *Parser, len: usize) ?[]const u8 {
        if (!parser.isCurrentSymbol(.TOKEN) or len == 0) {
            return null;
        }
        const str = parser.read(len);
        parser.resetCurrentSymbol();
        return str;
    }

    /// Consumes the current token if it matches the `str`, returns
    /// `true` if there is a match and `false` otherwise.
    fn consumeToken(parser: *Parser, str: []const u8) bool {
        if (!parser.isCurrentSymbol(.TOKEN)) {
            return false;
        }

        if (str.len == 1 and !std.ascii.isAlpha(str[0])) {
            if (parser.peekChar() != str[0]) {
                return false;
            }
            _ = parser.readToken(1);
        } else {
            const word_str = parser.peek(parser.peekWordSize()).?;
            if (!mem.eql(u8, str, word_str)) {
                return false;
            }
            _ = parser.readToken(str.len);
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

    /// Checks current symbol of the parser and if it is an operator.
    fn isOperator(parser: *Parser, sym: Symbol) bool {
        if (!parser.isCurrentSymbol(sym)) {
            return false;
        }
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

test "Parse all tests" {
    _ = @import("parse/test_command.zig");
    _ = @import("parse/test_program.zig");
    _ = @import("parse/test_word.zig");
}
