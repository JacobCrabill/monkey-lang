const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,

    // Identifiers & literals
    IDENT,
    INT,
    STRING,
    BUILTIN,

    // Operators
    EQUAL,
    PLUS,
    MINUS,
    STAR,
    SLASH,

    BANG,
    LT,
    GT,
    EQ,
    NEQ,

    // Delimiters
    COMMA,
    SEMI,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,
    EXIT,

    // Builtins
    //LEN,
};

pub const Token = struct {
    kind: TokenType = .ILLEGAL,
    literal: []const u8 = "",

    pub fn init(kind: TokenType, literal: []const u8) Token {
        return Token{
            .kind = kind,
            .literal = literal,
        };
    }
};

pub const Keywords = [_]Token{
    Token.init(.LET, "let"),
    Token.init(.FUNCTION, "fn"),
    Token.init(.IF, "if"),
    Token.init(.ELSE, "else"),
    Token.init(.RETURN, "return"),
    Token.init(.TRUE, "true"),
    Token.init(.FALSE, "false"),
    Token.init(.EXIT, "exit"),
};

pub const Builtins = [_]Token{
    Token.init(.BUILTIN, "len"),
    Token.init(.BUILTIN, "min"),
    Token.init(.BUILTIN, "max"),
};

/// Check for keywords and builtins and return the type, or identifier
pub fn lookupIdentifier(ident: []const u8) TokenType {
    for (Keywords) |keyword| {
        if (std.mem.eql(u8, ident, keyword.literal)) {
            return keyword.kind;
        }
    }

    for (Builtins) |builtin| {
        if (std.mem.eql(u8, ident, builtin.literal)) {
            return .BUILTIN;
        }
    }

    return .IDENT;
}
