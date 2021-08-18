const std = @import("std");

/// Creates tokens based on the shell syntax, the memory allocated is responsability of the caller
pub fn tokenize(allocator: *std.mem.Allocator, buffer: []const u8) ![]Token {
    // TODO see if it is overkill to allocate that much memory here
    var array = try std.ArrayList(Token).initCapacity(allocator, buffer.len + 1);
    defer array.deinit();

    var index: usize = 0;
    // maybe while true
    // TODO make a TokenIterator, it should return tokens one by one or all of them
    while (index < buffer.len) {
        while (index < buffer.len) : (index += 1) {
            switch (buffer[index]) {
                '#' => {
                    array.appendAssumeCapacity(.{ .type_ = TokenType.EOF, .data = null });
                    return array.toOwnedSlice();
                },
                ' ', '\t', '\n' => {},
                else => break,
            }
        }
        // TODO maybe have peek-y thing to have an enum that as like ${ or  $(
        // it could be possibly simpler later on, also it still misses some tokens
        // of a shell possibly
        var tokenType: TokenType = switch (buffer[index]) {
            '!' => TokenType.BANG,
            '|' => TokenType.PIPE,
            '&' => TokenType.AMPER,
            // TODO '>' => TokenType.OVERWRITE,
            // TODO '<' => TokenType.?,
            ';' => TokenType.SEMICOLON,
            '=' => TokenType.ASSIGN,
            '(' => TokenType.OPENING_PAREN,
            ')' => TokenType.CLOSING_PAREN,
            '{' => TokenType.OPENING_BRACKET,
            '}' => TokenType.CLOSING_BRACKET,
            '"' => TokenType.DOUBLE_QUOTE,
            '\'' => TokenType.SINGLE_QUOTE,
            '`' => TokenType.APOSTROPHE,
            '$' => TokenType.DOLAR,
            else => TokenType.STRING,
        };
        // TODO improve it
        if (tokenType == TokenType.DOLAR and index + 1 < buffer.len) {
            switch (buffer[index + 1]) {
                '{' => {
                    tokenType = TokenType.DOLAR_BRACKET;
                    index += 1;
                },
                '(' => {
                    tokenType = TokenType.DOLAR_PAREN;
                    index += 1;
                },
                else => {},
            }
        }
        if (tokenType != TokenType.STRING) {
            array.appendAssumeCapacity(.{ .type_ = tokenType, .data = null });
            index += 1;
        } else {
            const start = index;
            if (index >= buffer.len) break;

            while (index < buffer.len and tokenType == TokenType.STRING) : (index += 1) {
                switch (buffer[index]) {
                    '#', '!', '|', '&', ';', '=', '(', ')', '{', '}', '"', '\'', '`', '$', ' ', '\t', '\n' => break,
                    else => {},
                }
            }
            const end = index;
            array.appendAssumeCapacity(.{ .type_ = tokenType, .data = buffer[start..end] });
        }
    }
    array.appendAssumeCapacity(.{ .type_ = TokenType.EOF, .data = null });

    return array.toOwnedSlice();
}

// TODO probably misses some more tokens
pub const TokenType = enum {
    STRING, // anystring
    BANG, // !
    PIPE, // |
    AMPER, // &
    ASSIGN, // =
    SINGLE_QUOTE, // '
    DOUBLE_QUOTE, // "
    APOSTROPHE, // `
    SEMICOLON, // ;
    DOLAR, // $
    DOLAR_PAREN, // $(
    DOLAR_BRACKET, // ${
    OPENING_PAREN, // (
    CLOSING_PAREN, // )
    OPENING_BRACKET, // {
    CLOSING_BRACKET, // }
    EOF, // End Of File
};

pub const Token = struct {
    type_: TokenType,
    data: ?[]const u8,
};
