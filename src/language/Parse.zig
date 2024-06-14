//! Represents in-progress parsing, will be converted to an Ast after completion.

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
source: []const u8,
token_tags: []const Token.Tag,
token_starts: []const Ast.ByteOffset,
tok_i: TokenIndex,
errors: std.ArrayListUnmanaged(AstError),
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(Node.Index),
scratch: std.ArrayListUnmanaged(Node.Index),

const SmallSpan = union(enum) {
    zero_or_one: Node.Index,
    multi: Node.SubRange,
};

const Members = struct {
    len: usize,
    lhs: Node.Index,
    rhs: Node.Index,
    trailing: bool,

    fn toSpan(self: Members, p: *Parse) !Node.SubRange {
        if (self.len <= 2) {
            const nodes = [2]Node.Index{ self.lhs, self.rhs };
            return p.listToSpan(nodes[0..self.len]);
        } else {
            return Node.SubRange{ .start = self.lhs, .end = self.rhs };
        }
    }
};

fn listToSpan(p: *Parse, list: []const Node.Index) !Node.SubRange {
    try p.extra_data.appendSlice(p.gpa, list);
    return Node.SubRange{
        .start = @as(Node.Index, @intCast(p.extra_data.items.len - list.len)),
        .end = @as(Node.Index, @intCast(p.extra_data.items.len)),
    };
}

fn addNode(p: *Parse, elem: Ast.Node) Allocator.Error!Node.Index {
    const result = @as(Node.Index, @intCast(p.nodes.len));
    try p.nodes.append(p.gpa, elem);
    return result;
}

fn setNode(p: *Parse, i: usize, elem: Ast.Node) Node.Index {
    p.nodes.set(i, elem);
    return @as(Node.Index, @intCast(i));
}

fn reserveNode(p: *Parse, tag: Ast.Node.Tag) !usize {
    try p.nodes.resize(p.gpa, p.nodes.len + 1);
    p.nodes.items(.tag)[p.nodes.len - 1] = tag;
    return p.nodes.len - 1;
}

fn unreserveNode(p: *Parse, node_index: usize) void {
    if (p.nodes.len == node_index) {
        p.nodes.resize(p.gpa, p.nodes.len - 1) catch unreachable;
    } else {
        // There is zombie node left in the tree, let's make it as inoffensive as possible
        // (sadly there's no no-op node)
        p.nodes.items(.tag)[node_index] = .unreachable_literal;
        p.nodes.items(.main_token)[node_index] = p.tok_i;
    }
}

fn addExtra(p: *Parse, extra: anytype) Allocator.Error!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.extra_data.ensureUnusedCapacity(p.gpa, fields.len);
    const result = @as(u32, @intCast(p.extra_data.items.len));
    inline for (fields) |field| {
        comptime assert(field.type == Node.Index);
        p.extra_data.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

fn warnExpected(p: *Parse, expected_token: Token.Tag) error{OutOfMemory}!void {
    @setCold(true);
    try p.warnMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn warn(p: *Parse, error_tag: AstError.Tag) error{OutOfMemory}!void {
    @setCold(true);
    try p.warnMsg(.{ .tag = error_tag, .token = p.tok_i });
}

fn warnMsg(p: *Parse, msg: Ast.Error) error{OutOfMemory}!void {
    @setCold(true);
    switch (msg.tag) {
        .expected_semi_after_decl,
        .expected_semi_after_stmt,
        .expected_comma_after_field,
        .expected_comma_after_arg,
        .expected_comma_after_param,
        .expected_comma_after_initializer,
        .expected_comma_after_switch_prong,
        .expected_comma_after_for_operand,
        .expected_comma_after_capture,
        .expected_semi_or_else,
        .expected_semi_or_lbrace,
        .expected_token,
        .expected_block,
        .expected_block_or_assignment,
        .expected_block_or_expr,
        .expected_block_or_field,
        .expected_expr,
        .expected_expr_or_assignment,
        .expected_fn,
        .expected_inlinable,
        .expected_labelable,
        .expected_param_list,
        .expected_prefix_expr,
        .expected_primary_type_expr,
        .expected_pub_item,
        .expected_return_type,
        .expected_suffix_op,
        .expected_type_expr,
        .expected_var_decl,
        .expected_var_decl_or_fn,
        .expected_loop_payload,
        .expected_container,
        => if (msg.token != 0 and !p.tokensOnSameLine(msg.token - 1, msg.token)) {
            var copy = msg;
            copy.token_is_prev = true;
            copy.token -= 1;
            return p.errors.append(p.gpa, copy);
        },
        else => {},
    }
    try p.errors.append(p.gpa, msg);
}

fn fail(p: *Parse, tag: Ast.Error.Tag) error{ ParseError, OutOfMemory } {
    @setCold(true);
    return p.failMsg(.{ .tag = tag, .token = p.tok_i });
}

fn failExpected(p: *Parse, expected_token: Token.Tag) error{ ParseError, OutOfMemory } {
    @setCold(true);
    return p.failMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn failMsg(p: *Parse, msg: Ast.Error) error{ ParseError, OutOfMemory } {
    @setCold(true);
    try p.warnMsg(msg);
    return error.ParseError;
}

/// Root <- Module
pub fn parseRoot(p: *Parse) !void {
    // Root node must be index 0.
    p.nodes.appendAssumeCapacity(.{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });
    const module = try p.parseModule();
    if (p.token_tags[p.tok_i] != .eof) {
        try p.warnExpected(.eof);
    }

    p.nodes.items(.data)[0] = .{
        .lhs = module,
        .rhs = undefined,
    };
}

/// Module <- ModuleField*
fn parseModule(p: *Parse) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        const module_field = try p.parseModuleField();
        if (module_field == 0) break;
        try p.scratch.append(p.gpa, module_field);
    }

    const fields = p.scratch.items[scratch_top..];
    if (fields.len == 0) {
        return null_node;
    } else if (fields.len == 1) {
        return fields[0];
    } else {
        const span = try p.listToSpan(fields);
        return p.addNode(.{
            .tag = .module,
            .main_token = fields[0],
            .data = .{
                .lhs = span.start,
                .rhs = span.end,
            },
        });
    }
}

/// ModuleField
///     <- LPAREN KEYWORD_type TypeDef RPAREN
///      / LPAREN KEYWORD_import Import RPAREN
///      / LPAREN KEYWORD_func FuncDef RPAREN
///      / LPAREN KEYWORD_table TableDef RPAREN
///      / LPAREN KEYWORD_memory MemoryDef RPAREN
///      / LPAREN KEYWORD_global GlobalDef RPAREN
///      / LPAREN KEYWORD_export Export RPAREN
///      / LPAREN KEYWORD_start Start RPAREN
///      / LPAREN KEYWORD_elem Elem RPAREN
///      / LPAREN KEYWORD_data Data RPAREN
fn parseModuleField(p: *Parse) !Node.Index {
    _ = p.eatToken(.l_paren) orelse return null_node;
    _ = try p.expectToken(.r_paren);

    const tok = p.nextToken();
    switch (p.token_tags[tok]) {
        .keyword_type => {
            const type_def = try p.parseTypeDef();
            return p.addNode(.{
                .tag = .type_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = type_def,
                    .rhs = undefined,
                },
            });
        },
        .keyword_import => {
            const import_def = try p.parseImport();
            return p.addNode(.{
                .tag = .import_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = import_def,
                    .rhs = undefined,
                },
            });
        },
        .keyword_func => {
            const func_def = try p.parseFuncDef();
            return p.addNode(.{
                .tag = .func_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = func_def,
                    .rhs = undefined,
                },
            });
        },
        .keyword_table => {
            const table_def = try p.parseTableDef();
            return p.addNode(.{
                .tag = .table_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = table_def,
                    .rhs = undefined,
                },
            });
        },
        .keyword_memory => {
            const memory_def = try p.parseMemoryDef();
            return p.addNode(.{
                .tag = .memory_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = memory_def,
                    .rhs = undefined,
                },
            });
        },
        .keyword_global => {
            const global_def = try p.parseGlobalDef();
            return p.addNode(.{
                .tag = .global_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = global_def,
                    .rhs = undefined,
                },
            });
        },
        .keyword_export => {
            const export_def = try p.parseExport();
            return p.addNode(.{
                .tag = .export_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = export_def,
                    .rhs = undefined,
                },
            });
        },
        .keyword_start => {
            const start_def = try p.parseStart();
            return p.addNode(.{
                .tag = .start_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = start_def,
                    .rhs = undefined,
                },
            });
        },
        .keyword_elem => {
            const elem_def = try p.parseElem();
            return p.addNode(.{
                .tag = .elem_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = elem_def,
                    .rhs = undefined,
                },
            });
        },
        .keyword_data => {
            const data_def = try p.parseData();
            return p.addNode(.{
                .tag = .data_def,
                .main_token = tok - 1,
                .data = .{
                    .lhs = data_def,
                    .rhs = undefined,
                },
            });
        },
        else => return p.fail(.expected_module_field),
    }
}

/// TypeDef <- FuncType
fn parseTypeDef(p: *Parse) !Node.Index {
    return p.parseFuncType();
}

/// FuncType <- LPAREN KEYWORD_func LPAREN ValueType* RPAREN ValueType* RPAREN
fn parseFuncType(p: *Parse) !Node.Index {
    _ = try p.expectToken(.l_paren);
    _ = try p.expectToken(.keyword_func);
    _ = try p.expectToken(.l_paren);
    const param_types = try p.parseValueTypes();
    _ = try p.expectToken(.r_paren);
    const result_types = try p.parseValueTypes();
    _ = try p.expectToken(.r_paren);

    return p.addNode(.{
        .tag = .func_type,
        .main_token = p.tok_i - 1,
        .data = .{
            .lhs = param_types,
            .rhs = result_types,
        },
    });
}

/// ValueType <- KEYWORD_i32 / KEYWORD_i64 / KEYWORD_f32 / KEYWORD_f64 / KEYWORD_v128
fn parseValueType(p: *Parse) !Node.Index {
    const tok = p.nextToken();
    switch (p.token_tags[tok]) {
        .keyword_i32 => {
            return p.addNode(.{
                .tag = .value_type_i32,
                .main_token = tok - 1,
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
        },
        .keyword_i64 => {
            return p.addNode(.{
                .tag = .value_type_i64,
                .main_token = tok - 1,
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
        },
        .keyword_f32 => {
            return p.addNode(.{
                .tag = .value_type_f32,
                .main_token = tok - 1,
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
        },
        .keyword_f64 => {
            return p.addNode(.{
                .tag = .value_type_f64,
                .main_token = tok - 1,
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
        },
        else => return p.fail(.expected_value_type),
    }
}

/// ValueType*
fn parseValueTypes(p: *Parse) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        const value_type = p.parseValueType() catch |err| switch (err) {
            error.ParseError => break,
            else => |e| return e,
        };
        try p.scratch.append(p.gpa, value_type);
    }

    const value_types = p.scratch.items[scratch_top..];
    if (value_types.len == 0) {
        return null_node;
    } else if (value_types.len == 1) {
        return value_types[0];
    } else {
        const span = try p.listToSpan(value_types);
        return p.addNode(.{
            .tag = .value_type_tuple,
            .main_token = value_types[0],
            .data = .{
                .lhs = span.start,
                .rhs = span.end,
            },
        });
    }
}

/// Import <- LPAREN KEYWORD_import STRING STRING LPAREN ImportDesc RPAREN RPAREN
fn parseImport(p: *Parse) !Node.Index {
    _ = try p.expectToken(.l_paren);
    _ = try p.expectToken(.keyword_import);
    const module_str = try p.expectString();
    const field_str = try p.expectString();
    _ = try p.expectToken(.l_paren);
    const import_desc = try p.parseImportDesc();
    _ = try p.expectToken(.r_paren);
    _ = try p.expectToken(.r_paren);

    return p.addNode(.{
        .tag = .import,
        .main_token = p.tok_i - 1,
        .data = .{
            .lhs = try p.addExtra(.{
                .module_str = module_str,
                .field_str = field_str,
            }),
            .rhs = import_desc,
        },
    });
}

/// ImportDesc
///     <- KEYWORD_func TypeUse?
///      / KEYWORD_table TableType
///      / KEYWORD_memory MemoryType
///      / KEYWORD_global GlobalType
fn parseImportDesc(p: *Parse) !Node.Index {
    const tok = p.nextToken();
    switch (p.token_tags[tok]) {
        .keyword_func => {
            const type_use = try p.parseTypeUse();
            return p.addNode(.{
                .tag = .import_desc_func,
                .main_token = tok - 1,
                .data = .{
                    .lhs = type_use,
                    .rhs = undefined,
                },
            });
        },
        .keyword_table => {
            const table_type = try p.parseTableType();
            return p.addNode(.{
                .tag = .import_desc_table,
                .main_token = tok - 1,
                .data = .{
                    .lhs = table_type,
                    .rhs = undefined,
                },
            });
        },
        .keyword_memory => {
            const memory_type = try p.parseMemoryType();
            return p.addNode(.{
                .tag = .import_desc_memory,
                .main_token = tok - 1,
                .data = .{
                    .lhs = memory_type,
                    .rhs = undefined,
                },
            });
        },
        .keyword_global => {
            const global_type = try p.parseGlobalType();
            return p.addNode(.{
                .tag = .import_desc_global,
                .main_token = tok - 1,
                .data = .{
                    .lhs = global_type,
                    .rhs = undefined,
                },
            });
        },
        else => return p.fail(.expected_import_desc),
    }
}

/// TypeUse <- LPAREN KEYWORD_type NAT RPAREN
fn parseTypeUse(p: *Parse) !Node.Index {
    _ = p.eatToken(.l_paren) orelse return null_node;
    _ = try p.expectToken(.keyword_type);
    const type_index = try p.parseNat();
    _ = try p.expectToken(.r_paren);

    return p.addNode(.{
        .tag = .type_use,
        .main_token = p.tok_i - 1,
        .data = .{
            .lhs = type_index,
            .rhs = undefined,
        },
    });
}

/// TableType <- KEYWORD_funcref / ValueType LimitsOpt
fn parseTableType(p: *Parse) !Node.Index {
    if (p.eatToken(.keyword_funcref)) |_| {
        return p.addNode(.{
            .tag = .table_type_funcref,
            .main_token = p.tok_i - 1,
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        });
    }

    const value_type = try p.parseValueType();
    const limits = try p.parseLimitsOpt();

    return p.addNode(.{
        .tag = .table_type,
        .main_token = p.tok_i - 1,
        .data = .{
            .lhs = value_type,
            .rhs = limits,
        },
    });
}

/// MemoryType <- LimitsOpt
fn parseMemoryType(p: *Parse) !Node.Index {
    return p.parseLimitsOpt();
}

/// GlobalType <- ValueType Mut
fn parseGlobalType(p: *Parse) !Node.Index {
    const value_type = try p.parseValueType();
    const mut = try p.parseMut();

    return p.addNode(.{
        .tag = .global_type,
        .main_token = p.tok_i - 1,
        .data = .{
            .lhs = value_type,
            .rhs = mut,
        },
    });
}

/// Mut <- LPAREN KEYWORD_mut RPAREN
fn parseMut(p: *Parse) !Node.Index {
    _ = p.eatToken(.l_paren) orelse return null_node;
    _ = try p.expectToken(.keyword_mut);
    _ = try p.expectToken(.r_paren);

    return p.addNode(.{
        .tag = .mut,
        .main_token = p.tok_i - 1,
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });
}

/// LimitsOpt <- LPAREN KEYWORD_limits NAT NAT? RPAREN
fn parseLimitsOpt(p: *Parse) !Node.Index {
    _ = p.eatToken(.l_paren) orelse return null_node;
    _ = try p.expectToken(.keyword_limits);
    const min = try p.parseNat();
    const max = try p.parseNatOpt();
    _ = try p.expectToken(.r_paren);

    return p.addNode(.{
        .tag = .limits,
        .main_token = p.tok_i - 1,
        .data = .{
            .lhs = min,
            .rhs = max,
        },
    });
}

// ... (rest of the parsing functions for .wat grammar) ...

/// NAT <- DecimalNumber
fn parseNat(p: *Parse) !Node.Index {
    return p.parseDecimalNumber();
}

/// NAT?
fn parseNatOpt(p: *Parse) !Node.Index {
    return p.parseDecimalNumber() catch null_node;
}

/// STRING
fn expectString(p: *Parse) !Node.Index {
    return p.expectToken(.string);
}

/// DecimalNumber <-- change to float OMG
fn parseDecimalNumber(p: *Parse) !Node.Index {
    const number = p.eatToken(.float) orelse return p.fail(.expected_number);
    return p.addNode(.{
        .tag = .float,
        .main_token = number,
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });
}

/// Skips over doc comment tokens. Returns the first one, if any.
fn eatDocComments(p: *Parse) Allocator.Error!?TokenIndex {
    if (p.eatToken(.doc_comment)) |tok| {
        var first_line = tok;
        if (tok > 0 and tokensOnSameLine(p, tok - 1, tok)) {
            try p.warnMsg(.{
                .tag = .same_line_doc_comment,
                .token = tok,
            });
            first_line = p.eatToken(.doc_comment) orelse return null;
        }
        while (p.eatToken(.doc_comment)) |_| {}
        return first_line;
    }
    return null;
}

fn tokensOnSameLine(p: *Parse, token1: TokenIndex, token2: TokenIndex) bool {
    return std.mem.indexOfScalar(u8, p.source[p.token_starts[token1]..p.token_starts[token2]], '\n') == null;
}

fn eatToken(p: *Parse, tag: Token.Tag) ?TokenIndex {
    return if (p.token_tags[p.tok_i] == tag) p.nextToken() else null;
}

fn expectToken(p: *Parse, tag: Token.Tag) Error!TokenIndex {
    if (p.token_tags[p.tok_i] != tag) {
        return p.failMsg(.{
            .tag = .expected_token,
            .token = p.tok_i,
            .extra = .{ .expected_tag = tag },
        });
    }
    return p.nextToken();
}

fn nextToken(p: *Parse) TokenIndex {
    const result = p.tok_i;
    p.tok_i += 1;
    return result;
}

const wat = @import("Wat.zig");
const Ast = @import("Ast.zig");
const null_node: Node.Index = 0;
const Parse = @This();
const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Node = Ast.Node;
const AstError = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const Token = wat.Token;

test {
    _ = @import("parser_test.zig");
}
