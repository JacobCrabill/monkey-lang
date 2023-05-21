# Monkey Interpreter in Zig

An implementation of the Monkey programming language in Zig
Following Thorston Ball's ["Writing An Interpreter in Go"](interpreterbook.com)

## Status

The lexer and parser are working to some extent; to run tests, do:
- Lexer: `zig build test-lexer -fsummary`
- Parser: `zig build test-parser -fsummary`

Note: `-fsummary` forces the test summary to be displayed; otherwise silence means success.
