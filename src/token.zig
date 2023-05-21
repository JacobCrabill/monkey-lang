const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,

    // Identifiers & literals
    IDENT,
    INT,

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
};

pub const Token = struct {
    kind: TokenType = .ILLEGAL,
    literal: []const u8 = undefined,

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
};

/// Check for keywords and return the keyword type, or identifier
pub fn lookupIdentifier(ident: []const u8) TokenType {
    for (Keywords) |keyword| {
        if (std.mem.eql(u8, ident, keyword.literal)) {
            return keyword.kind;
        }
    }

    return .IDENT;
}
