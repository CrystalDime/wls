const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "align", .keyword_align },
        .{ "module", .keyword_module },
        .{ "import", .keyword_import },
        .{ "export", .keyword_export },
        .{ "result", .keyword_export },
        .{ "call", .keyword_call },
        .{ "local", .keyword_local },
        .{ "loop", .keyword_loop },
        .{ "br_if", .keyword_br_if },
        .{ "br", .keyword_br },
        .{ "param", .keyword_param },
        .{ "block", .keyword_block },
        .{ "data", .keyword_data },
        .{ "global", .keyword_global },
        .{ "if", .keyword_if },
        .{ "else", .keyword_else },
        .{ "type", .keyword_type },
        .{ "func", .keyword_func },
        .{ "memory", .keyword_memory },
        .{ "start", .keyword_start },
        .{ "elem", .keyword_elem },
        .{ "offset", .keyword_offset },
        .{ "mut", .keyword_mut },
        .{ "i32", .keyword_i32 },
        .{ "i64", .keyword_i64 },
        .{ "f32", .keyword_f32 },
        .{ "f64", .keyword_f64 },
        .{ "anyfunc", .keyword_anyfunc },
        .{ "funcref", .keyword_funcref },
        .{ "extern", .keyword_extern },
        .{ "unreachable", .keyword_unreachable },
        .{ "nop", .keyword_nop },
        .{ "end", .keyword_end },
        .{ "table", .keyword_table },
        .{ "limits", .keyword_limits },
    });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const Tag = enum {
        identifier,
        string,
        float,
        integer,
        reserved,
        eof,
        comment_block,
        comment_line,
        l_paren,
        r_paren,
        keyword_align,
        keyword_module,
        keyword_import,
        keyword_export,
        keyword_call,
        keyword_local,
        keyword_loop,
        keyword_br_if,
        keyword_br,
        keyword_param,
        keyword_block,
        keyword_data,
        keyword_global,
        keyword_if,
        keyword_else,
        keyword_type,
        keyword_func,
        keyword_memory,
        keyword_start,
        keyword_elem,
        keyword_offset,
        keyword_mut,
        keyword_i32,
        keyword_i64,
        keyword_f32,
        keyword_f64,
        keyword_anyfunc,
        keyword_funcref,
        keyword_extern,
        keyword_unreachable,
        keyword_nop,
        keyword_end,
        keyword_table,
        keyword_limits,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .identifier,
                .string,
                .integer,
                .float,
                .eof,
                .reserved,
                .comment_block,
                .comment_line,
                => null,
                .l_paren => "(",
                .r_paren => ")",
                .keyword_align => "align",
                .keyword_module => "module",
                .keyword_import => "import",
                .keyword_export => "export",
                .keyword_call => "call",
                .keyword_local => "local",
                .keyword_loop => "loop",
                .keyword_br_if => "br_if",
                .keyword_br => "br",
                .keyword_param => "param",
                .keyword_block => "block",
                .keyword_data => "data",
                .keyword_global => "global",
                .keyword_if => "if",
                .keyword_else => "else",
                .keyword_type => "type",
                .keyword_func => "func",
                .keyword_memory => "memory",
                .keyword_start => "start",
                .keyword_elem => "elem",
                .keyword_offset => "offset",
                .keyword_mut => "mut",
                .keyword_i32 => "i32",
                .keyword_i64 => "i64",
                .keyword_f32 => "f32",
                .keyword_f64 => "f64",
                .keyword_anyfunc => "anyfunc",
                .keyword_funcref => "funcref",
                .keyword_extern => "extern",
                .keyword_unreachable => "unreachable",
                .keyword_nop => "nop",
                .keyword_end => "end",
                .keyword_table => "table",
                .keyword_limits => "limits",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .identifier => "an identifier",
                .string => "a string",
                .integer => "a number literal",
                .float => "a float literal",
                .eof => "EOF",
                .comment_block => "a block comment",
                .comment_line => "a line comment",
                .reserved => "reserved/invalid character",
                else => unreachable,
            };
        }
    };
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,
    pending_invalid_token: ?Token,

    /// For debugging purposes
    pub fn dump(self: *Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // Skip the UTF-8 BOM if present
        const src_start: usize = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0;
        return Tokenizer{
            .buffer = buffer,
            .index = src_start,
            .pending_invalid_token = null,
        };
    }

    const State = enum {
        start,
        identifier,
        string_literal,
        string_literal_backslash,
        integer,
        float,
        hexadecimal,
        hexadecimal_float,
        comment_line,
        comment_block,
        reserved,
    };

    /// This is a workaround to the fact that the tokenizer can queue up
    /// 'pending_invalid_token's when parsing literals, which means that we need
    /// to scan from the start of the current line to find a matching tag - just
    /// in case it was an invalid character generated during literal
    /// tokenization. Ideally this processing of this would be pushed to the AST
    /// parser or another later stage, both to give more useful error messages
    /// with that extra context and in order to be able to remove this
    /// workaround.
    pub fn findTagAtCurrentIndex(self: *Tokenizer, tag: Token.Tag) Token {
        if (tag == .reserved) {
            const target_index = self.index;
            var starting_index = target_index;
            while (starting_index > 0) {
                if (self.buffer[starting_index] == '\n') {
                    break;
                }
                starting_index -= 1;
            }

            self.index = starting_index;
            while (self.index <= target_index or self.pending_invalid_token != null) {
                const result = self.next();
                if (result.loc.start == target_index and result.tag == tag) {
                    return result;
                }
            }
            unreachable;
        } else {
            return self.next();
        }
    }

    pub fn next(self: *Tokenizer) Token {
        if (self.pending_invalid_token) |token| {
            self.pending_invalid_token = null;
            return token;
        }

        var state: State = .start;
        var result = Token{
            .tag = .eof,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        while (true) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                .start => switch (c) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            result.tag = .reserved;
                            result.loc.start = self.index;
                            self.index += 1;
                            result.loc.end = self.index;
                            return result;
                        }
                        break;
                    },
                    ' ', '\n', '\t', '\r' => result.loc.start = self.index + 1,
                    '"' => {
                        state = .string_literal;
                        result.tag = .string;
                    },
                    'a'...'z' => {
                        state = .identifier;
                        result.tag = .identifier;
                    },
                    '0'...'9' => {
                        state = .integer;
                        result.tag = .integer;
                    },
                    '-' => {
                        if (self.index + 1 < self.buffer.len and std.ascii.isDigit(self.buffer[self.index + 1])) {
                            state = .integer;
                            result.tag = .integer;
                        } else {
                            state = .reserved;
                            result.tag = .reserved;
                        }
                    },
                    '(' => {
                        if (self.index + 1 < self.buffer.len and self.buffer[self.index + 1] == ';') {
                            state = .comment_block;
                            result.tag = .comment_block;
                            self.index += 1;
                        } else {
                            result.tag = .l_paren;
                            self.index += 1;
                            break;
                        }
                    },
                    ')' => {
                        result.tag = .r_paren;
                        self.index += 1;
                        break;
                    },
                    ';' => {
                        if (self.index + 1 < self.buffer.len and self.buffer[self.index + 1] == ';') {
                            state = .comment_line;
                            result.tag = .comment_line;
                            self.index += 1;
                        } else {
                            state = .reserved;
                            result.tag = .reserved;
                        }
                    },
                    '$' => {
                        state = .identifier;
                        result.tag = .identifier;
                    },
                    else => {
                        state = .reserved;
                        result.tag = .reserved;
                    },
                },
                .identifier => switch (c) {
                    ' ', '\t', ')', 0 => {
                        if (Token.getKeyword(self.buffer[result.loc.start..self.index])) |tag| {
                            result.tag = tag;
                        }
                        break;
                    },
                    33...34 => {}, // Excludes the space and "
                    36...40 => {}, // Excludes (, )
                    42...43 => {}, // Excludes ,
                    45...57 => {}, // Excludes ;
                    59...126 => {}, // Excludes ( which is 40
                    else => {
                        result.tag = .reserved;
                        break;
                    },
                },
                .string_literal => switch (c) {
                    '\\' => {
                        state = .string_literal_backslash;
                    },
                    '"' => {
                        self.index += 1;
                        break;
                    },
                    0 => {
                        if (self.index == self.buffer.len) {
                            result.tag = .reserved;
                            break;
                        }
                    },
                    '\n' => {
                        result.tag = .reserved;
                        break;
                    },
                    else => {},
                },

                .string_literal_backslash => switch (c) {
                    0, '\n' => {
                        result.tag = .reserved;
                        break;
                    },
                    else => {
                        state = .string_literal;
                    },
                },
                .integer => switch (c) {
                    '0'...'9', '_' => {},
                    '.' => {
                        state = .float;
                        result.tag = .float;
                    },
                    'e', 'E' => {
                        state = .float;
                        result.tag = .float;
                        self.index += 1;
                    },
                    'x', 'X' => {
                        state = .hexadecimal;
                        result.tag = .float;
                        self.index += 1;
                    },
                    ' ', '"', ',', ')', 0 => { // Potentially new line needs to be inlcuded here. If there is a problem with parsing for ints, actually just take what reserved has.
                        break;
                    },
                    else => {
                        state = .reserved;
                        result.tag = .reserved;
                    },
                },
                .float => switch (c) {
                    '0'...'9', '_' => {},
                    'e', 'E' => {
                        self.index += 1;
                    },
                    else => break,
                },
                .hexadecimal => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                    '.' => {
                        state = .hexadecimal_float;
                        result.tag = .float;
                    },
                    'p', 'P' => {
                        state = .hexadecimal_float;
                        result.tag = .float;
                        self.index += 1;
                    },
                    else => break,
                },
                .hexadecimal_float => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                    'p', 'P' => {
                        self.index += 1;
                    },
                    else => break,
                },
                .comment_line => switch (c) {
                    0, '\n' => break,
                    else => {},
                },
                .comment_block => switch (c) {
                    '(' => {
                        if (self.index + 1 < self.buffer.len and self.buffer[self.index + 1] == ';') {
                            // Entering a nested block comment
                            self.index += 1;
                        }
                    },
                    ';' => {
                        if (self.index + 1 < self.buffer.len and self.buffer[self.index + 1] == ')') {
                            self.index += 1;
                            var nested_level: usize = 1;
                            while (self.index + 1 < self.buffer.len) : (self.index += 1) {
                                const next_char = self.buffer[self.index + 1];
                                if (next_char == '(' and self.index + 2 < self.buffer.len and self.buffer[self.index + 2] == ';') {
                                    // Entering a nested block comment
                                    nested_level += 1;
                                    self.index += 1;
                                } else if (next_char == ';' and self.index + 2 < self.buffer.len and self.buffer[self.index + 2] == ')') {
                                    // Exiting a block comment
                                    nested_level -= 1;
                                    self.index += 1;
                                    if (nested_level == 0) {
                                        self.index += 1;
                                        state = .start;
                                        result.loc.start = self.index;
                                        break;
                                    }
                                }
                            }
                        }
                    },
                    0 => break,
                    else => {},
                },
                .reserved => switch (c) {
                    ' ', '\n', '\t', '\r', '(', ')', 0 => break,
                    else => {},
                },
            }
        }

        if (result.tag == .eof) {
            if (self.pending_invalid_token) |token| {
                self.pending_invalid_token = null;
                return token;
            }
            result.loc.start = self.index;
        }

        result.loc.end = self.index;
        return result;
    }
};

test "keywords" {
    try testTokenize("if else", &.{ .keyword_if, .keyword_else });
}

test "line comment" {
    try testTokenize(
        \\;; line comment
        \\(
    , &.{
        .comment_line,
        .l_paren,
    });
}

test "module" {
    try testTokenize(
        \\(module)
    , &.{
        .l_paren,
        .keyword_module,
        .r_paren,
    });
}

test "block comment" {
    try testTokenize(
        \\(; block comment ;)
    , &.{.comment_block});
}

test "nested block comment" {
    try testTokenize(
        \\(; outer (; inner ;) outer ;)
    , &.{.comment_block});
}

test "integers" {
    try testTokenize("0", &.{.integer});
    try testTokenize("123", &.{.integer});
    try testTokenize("-456", &.{.integer});
}

test "floats" {
    try testTokenize("1.0", &.{.float});
    try testTokenize("-2.5", &.{.float});
    try testTokenize("3e-5", &.{.float});
    try testTokenize("4.2E+7", &.{.float});
}

test "hexadecimal" {
    try testTokenize("0x1", &.{.float});
    try testTokenize("0xABCD_EF01", &.{.float});
}

test "hexadecimal floats" {
    try testTokenize("0x1.2p3", &.{.float});
    try testTokenize("-0x2.ap-4", &.{.float});
}

test "strings" {
    try testTokenize("\"hello\"", &.{.string});
    try testTokenize("\"foo\\\"bar\"", &.{.string});
}

test "identifiers" {
    try testTokenize("$foo", &.{.identifier});
    try testTokenize("$foo_bar", &.{.identifier});
    try testTokenize("$foo-bar", &.{.identifier});
    try testTokenize("$foo.bar", &.{.identifier});
    try testTokenize("$123", &.{.identifier});
}

test "reserved tokens" {
    try testTokenize("0$foo", &.{.reserved});
    // TODO: Look into whether the below should truly be reserved or not idk
    //try testTokenize("\"a\"\"b\"", &.{.reserved});
}

test "parentheses" {
    try testTokenize("()", &.{ .l_paren, .r_paren });
    try testTokenize("(())", &.{ .l_paren, .l_paren, .r_paren, .r_paren });
}

test "whitespace" {
    try testTokenize(" \t\n\r", &.{});
    try testTokenize(" $foo ", &.{.identifier});
}

test "multiple tokens" {
    try testTokenize("if ($foo 123) else", &.{
        .keyword_if,
        .l_paren,
        .identifier,
        .integer,
        .r_paren,
        .keyword_else,
    });
}

test "invalid characters" {
    try testTokenize("#", &.{.reserved});
    try testTokenize("`", &.{.reserved});
}

test "invalid string" {
    try testTokenize("\"foo", &.{.reserved});
}

fn testTokenize(source: [:0]const u8, expected_token_tags: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected_token_tags) |expected_token_tag| {
        const token = tokenizer.next();
        if (token.tag != expected_token_tag) {
            std.debug.print("Expected {s}, but got ", .{@tagName(expected_token_tag)});
            tokenizer.dump(&token);
            return error.TestFailed;
        }
    }
    const last_token = tokenizer.next();
    if (last_token.tag != .eof) {
        std.debug.print("Expected EOF, but got ", .{});
        tokenizer.dump(&last_token);
        return error.TestFailed;
    }
    if (last_token.loc.start != source.len) {
        std.debug.print("Expected EOF at index {}, but got {}\n", .{ source.len, last_token.loc.start });
        tokenizer.dump(&last_token);
        return error.TestFailed;
    }
    if (last_token.loc.end != source.len) {
        std.debug.print("Expected EOF at index {}, but got {}\n", .{ source.len, last_token.loc.end });
        tokenizer.dump(&last_token);
        return error.TestFailed;
    }
}
