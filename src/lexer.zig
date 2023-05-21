const std = @import("std");
const Tokens = @import("token.zig");

const TokenType = Tokens.TokenType;
const Token = Tokens.Token;

const Lexer = struct {
    const Self = @This();
    input: []const u8 = undefined,
    cursor: usize = 0,
    read_cursor: usize = 0,
    ch: u8 = 0,

    // Create a new Lexer given input to tokenize
    pub fn init(input: []const u8) Lexer {
        var lex = Lexer{ .input = input };
        lex.readChar();
        return lex;
    }

    // Read and store the next characer in the input, advancing the cursor
    pub fn readChar(self: *Self) void {
        if (self.read_cursor >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_cursor];
        }
        self.cursor = self.read_cursor;
        self.read_cursor += 1;
    }

    pub fn peekChar(self: Self) u8 {
        if (self.read_cursor >= self.input.len)
            return 0;

        return self.input[self.read_cursor];
    }

    // Advance the cursor and produce the next Token
    pub fn nextToken(self: *Self) Token {
        self.skipWhitespace();

        // Immutable copy of current char
        const ch_s: []const u8 = self.currentString();

        //std.debug.print("@@@ Next Char: '{s}'\n", .{ch_s});
        var tok = Token.init(.ILLEGAL, ch_s);
        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    tok.kind = .EQ;
                    tok.literal = self.input[self.cursor .. self.read_cursor + 1];
                    self.readChar();
                } else {
                    tok.kind = .EQUAL;
                }
            },
            '+' => tok.kind = .PLUS,
            '-' => tok.kind = .MINUS,
            '*' => tok.kind = .STAR,
            '/' => tok.kind = .SLASH,
            ',' => tok.kind = .COMMA,
            ';' => tok.kind = .SEMI,
            '(' => tok.kind = .LPAREN,
            ')' => tok.kind = .RPAREN,
            '{' => tok.kind = .LBRACE,
            '}' => tok.kind = .RBRACE,
            '!' => {
                if (self.peekChar() == '=') {
                    tok.kind = .NEQ;
                    tok.literal = self.input[self.cursor .. self.read_cursor + 1];
                    self.readChar();
                } else {
                    tok.kind = .BANG;
                }
            },
            '<' => tok.kind = .LT,
            '>' => tok.kind = .GT,
            0 => tok.kind = .EOF,
            ' ' => {},
            else => {
                if (isLetter(self.ch)) {
                    tok.literal = self.readIdentifier();
                    tok.kind = Tokens.lookupIdentifier(tok.literal);
                    return tok;
                } else if (isDigit(self.ch)) {
                    tok.literal = self.readInteger();
                    tok.kind = .INT;
                    return tok;
                }
            },
        }

        self.readChar();
        return tok;
    }

    fn skipWhitespace(self: *Self) void {
        const ws_chars = " \t\n\r";
        while (std.mem.count(u8, ws_chars, &.{self.ch}) > 0) {
            self.readChar();
        }
    }
    fn readIdentifier(self: *Self) []const u8 {
        const cursor = self.cursor;
        while (isLetter(self.ch)) {
            self.readChar();
        }
        return self.input[cursor..self.cursor];
    }

    fn readInteger(self: *Self) []const u8 {
        const cursor = self.cursor;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        return self.input[cursor..self.cursor];
    }

    fn currentString(self: Self) []const u8 {
        if (self.cursor < self.input.len) {
            return self.input[self.cursor .. self.cursor + 1];
        } else {
            return "0";
        }
    }
};

pub fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ch == '_';
}

pub fn isDigit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

// --------------- Lexer Unit Tests ---------------

const expectEqual = std.testing.expectEqual;

test "simple tokens" {
    const input = "=+(){},;";

    const expected_types = [_]TokenType{
        .EQUAL,
        .PLUS,
        .LPAREN,
        .RPAREN,
        .LBRACE,
        .RBRACE,
        .COMMA,
        .SEMI,
    };

    var lex = Lexer.init(input);

    for (expected_types) |tok| {
        //std.log.info("{any}\n", .{tok});
        const t = lex.nextToken();

        errdefer {
            std.debug.print("Expected {any}, got {any}\n", .{ tok, t.kind });
        }
        try expectEqual(tok, t.kind);
    }
}

test "keywords and identifiers" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
        \\
    ;

    const expected_tokens = [_]Token{
        Token.init(.LET, "let"),
        Token.init(.IDENT, "five"),
        Token.init(.EQUAL, "="),
        Token.init(.INT, "5"),
        Token.init(.SEMI, ";"),
        Token.init(.LET, "let"),
        Token.init(.IDENT, "ten"),
        Token.init(.EQUAL, "="),
        Token.init(.INT, "10"),
        Token.init(.SEMI, ";"),
        Token.init(.LET, "let"),
        Token.init(.IDENT, "add"),
        Token.init(.EQUAL, "="),
        Token.init(.FUNCTION, "fn"),
        Token.init(.LPAREN, "("),
        Token.init(.IDENT, "x"),
        Token.init(.COMMA, ","),
        Token.init(.IDENT, "y"),
        Token.init(.RPAREN, ")"),
        Token.init(.LBRACE, "{"),
        Token.init(.IDENT, "x"),
        Token.init(.PLUS, "+"),
        Token.init(.IDENT, "y"),
        Token.init(.SEMI, ";"),
        Token.init(.RBRACE, "}"),
        Token.init(.SEMI, ";"),
        Token.init(.LET, "let"),
        Token.init(.IDENT, "result"),
        Token.init(.EQUAL, "="),
        Token.init(.IDENT, "add"),
        Token.init(.LPAREN, "("),
        Token.init(.IDENT, "five"),
        Token.init(.COMMA, ","),
        Token.init(.IDENT, "ten"),
        Token.init(.RPAREN, ")"),
        Token.init(.SEMI, ";"),
        Token.init(.BANG, "!"),
        Token.init(.MINUS, "-"),
        Token.init(.SLASH, "/"),
        Token.init(.STAR, "*"),
        Token.init(.INT, "5"),
        Token.init(.SEMI, ";"),
        Token.init(.INT, "5"),
        Token.init(.LT, "<"),
        Token.init(.INT, "10"),
        Token.init(.GT, ">"),
        Token.init(.INT, "5"),
        Token.init(.SEMI, ";"),
        Token.init(.IF, "if"),
        Token.init(.LPAREN, "("),
        Token.init(.INT, "5"),
        Token.init(.LT, "<"),
        Token.init(.INT, "10"),
        Token.init(.RPAREN, ")"),
        Token.init(.LBRACE, "{"),
        Token.init(.RETURN, "return"),
        Token.init(.TRUE, "true"),
        Token.init(.SEMI, ";"),
        Token.init(.RBRACE, "}"),
        Token.init(.ELSE, "else"),
        Token.init(.LBRACE, "{"),
        Token.init(.RETURN, "return"),
        Token.init(.FALSE, "false"),
        Token.init(.SEMI, ";"),
        Token.init(.RBRACE, "}"),
        Token.init(.INT, "10"),
        Token.init(.EQ, "=="),
        Token.init(.INT, "10"),
        Token.init(.SEMI, ";"),
        Token.init(.INT, "10"),
        Token.init(.NEQ, "!="),
        Token.init(.INT, "9"),
        Token.init(.SEMI, ";"),

        Token.init(.EOF, "0"),
    };

    var lex = Lexer.init(input);

    for (expected_tokens) |tok| {
        const t = lex.nextToken();
        //std.debug.print("expected:  {any}\n", .{tok});
        //std.debug.print("got token: {any}\n", .{t});
        errdefer {
            std.debug.print("Expected {any}, got {any}\n", .{ tok, t });
        }
        try std.testing.expect(compareTokens(tok, t));
    }
}

fn compareTokens(expected: Token, actual: Token) bool {
    return expected.kind == actual.kind and std.mem.eql(u8, expected.literal, actual.literal);
}
