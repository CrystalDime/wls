//! Represents in-progress parsing, will be converted to an Ast after completion.

pub const Error = error{ParseError} || Allocator.Error; // add ParseError

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
    const root_range = try module.toSpan(p);

    if (p.token_tags[p.tok_i] != .eof) {
        try p.warnExpected(.eof);
    }

    p.nodes.items(.data)[0] = .{
        .lhs = root_range.start,
        .rhs = root_range.end,
    };
}

/// Module <- ModuleField*
fn parseModule(p: *Parse) Allocator.Error!Members {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    _ = p.eatDocComments() catch {};
    _ = p.expectToken(.l_paren) catch {};
    _ = p.expectToken(.keyword_module) catch {};

    while (true) {
        const module_field = p.expectModuleField() catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ParseError => {
                continue;
            },
        };
        if (module_field == null_node) {
            // Terminal for the closing paren for the MODULE, commenting because I somehow forgot this.
            _ = p.expectToken(.r_paren) catch {};
            break;
        }
        try p.scratch.append(p.gpa, module_field);
    }

    const fields = p.scratch.items[scratch_top..];
    const trailing = false;
    switch (fields.len) {
        0 => return Members{
            .len = 0,
            .lhs = 0,
            .rhs = 0,
            .trailing = true,
        },
        1 => return Members{
            .len = 1,
            .lhs = fields[0],
            .rhs = 0,
            .trailing = trailing,
        },
        2 => return Members{
            .len = 2,
            .lhs = fields[0],
            .rhs = fields[1],
            .trailing = trailing,
        },
        else => {
            const span = try p.listToSpan(fields);
            return Members{
                .len = fields.len,
                .lhs = span.start,
                .rhs = span.end,
                .trailing = trailing,
            };
        },
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
fn expectModuleField(p: *Parse) !Node.Index {
    _ = p.eatDocComments() catch {};
    _ = p.eatToken(.l_paren) orelse return null_node;

    const tok = p.tok_i;
    switch (p.token_tags[tok]) {
        .keyword_type => {
            const type_def = try parseNat(p);
            return p.addNode(.{
                .tag = .type_def,
                .main_token = tok,
                .data = .{
                    .lhs = type_def,
                    .rhs = undefined,
                },
            });
        },
        // .keyword_import => {
        //     ////const import_def = try p.parseImport();
        //     //return p.addNode(.{
        //     //    .tag = .import_def,
        //     //    .main_token = tok - 1,
        //     //    .data = .{
        //     //        .lhs = import_def,
        //     //        .rhs = undefined,
        //     //    },
        //     //});
        // },
        // .keyword_func => {
        //     ////const func_def = try p.parseFuncDef();
        //     //return p.addNode(.{
        //     //    .tag = .func_def,
        //     //    .main_token = tok - 1,
        //     //    .data = .{
        //     //        .lhs = func_def,
        //     //        .rhs = undefined,
        //     //    },
        //     //});
        // },
        // .keyword_table => {
        //     // const table_def = try p.parseTableDef();
        //     // return p.addNode(.{
        //     //     .tag = .table_def,
        //     //     .main_token = tok - 1,
        //     //     .data = .{
        //     //         .lhs = table_def,
        //     //         .rhs = undefined,
        //     //     },
        //     // });
        // },
        .keyword_memory => {
            const memory_def = try p.parseMemoryDef();
            _ = p.expectToken(.r_paren) catch {};
            return memory_def;
        },
        // .keyword_global => {
        //     const global_def = try p.parseGlobalDef();
        //     return p.addNode(.{
        //         .tag = .global_def,
        //         .main_token = tok - 1,
        //         .data = .{
        //             .lhs = global_def,
        //             .rhs = undefined,
        //         },
        //     });
        // },
        // .keyword_export => {
        //     const export_def = try p.parseExport();
        //     return p.addNode(.{
        //         .tag = .export_def,
        //         .main_token = tok - 1,
        //         .data = .{
        //             .lhs = export_def,
        //             .rhs = undefined,
        //         },
        //     });
        // },
        // .keyword_start => {
        //     const start_def = try p.parseStart();
        //     return p.addNode(.{
        //         .tag = .start_def,
        //         .main_token = tok - 1,
        //         .data = .{
        //             .lhs = start_def,
        //             .rhs = undefined,
        //         },
        //     });
        // },
        // .keyword_elem => {
        //     const elem_def = try p.parseElem();
        //     return p.addNode(.{
        //         .tag = .elem_def,
        //         .main_token = tok - 1,
        //         .data = .{
        //             .lhs = elem_def,
        //             .rhs = undefined,
        //         },
        //     });
        // },
        // .keyword_data => {
        //     const data_def = try p.parseData();
        //     return p.addNode(.{
        //         .tag = .data_def,
        //         .main_token = tok - 1,
        //         .data = .{
        //             .lhs = data_def,
        //             .rhs = undefined,
        //         },
        //     });
        // },
        else => return p.failMsg(.{
            .tag = .expected_block_or_field, //TODO: OR maybe unexpected identifier?
            .token = p.tok_i,
        }),
    }
}

/// MemoryDef
///     Memory (Identifier) INTEGER (INTEGER)
fn parseMemoryDef(p: *Parse) !Node.Index {
    //TODO Go back and and the following comment to the ast tag definition
    // For a memory def the lhs is the identfier, the right hand side is the limit
    // For a limit the lhs is the min, and the rhs is the max

    const memoryDef = p.eatToken(.keyword_memory) orelse return null_node;

    const memorydef_index = try p.reserveNode(.memory_def);
    errdefer p.unreserveNode(memorydef_index);

    const optionalIdentifier = p.eatToken(.identifier);

    var identifierNode = null_node;
    if (optionalIdentifier) |tokenIndex| {
        identifierNode = try p.addNode(.{
            .tag = .identifier,
            .main_token = tokenIndex,
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        });
    }

    const limitdef_index = try p.reserveNode(.limits);
    errdefer p.unreserveNode(limitdef_index);

    const memorySizeMinIndex = try p.expectToken(.integer); //TODO Consider converting to a warn or maybe not?

    const memorySizeMinNode = try p.addNode(.{
        .tag = .number_literal,
        .main_token = memorySizeMinIndex,
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });

    // Max size is n optional portion of the memorydef
    const optionalMaxSize = p.eatToken(.integer); //TODO Consider converting to a warn or maybe not?

    var memorySizeMaxNode = null_node;
    if (optionalMaxSize) |tokenIndex| {
        memorySizeMaxNode = try p.addNode(.{
            .tag = .number_literal,
            .main_token = tokenIndex,
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        });
    }

    const limitdef_indexu32 = p.setNode(limitdef_index, .{
        .tag = .limits,
        .main_token = memorySizeMinIndex, // shrug idk
        .data = .{
            .lhs = memorySizeMinNode,
            .rhs = memorySizeMaxNode,
        },
    });

    return p.setNode(memorydef_index, .{
        .tag = .memory_def,
        .main_token = memoryDef,
        .data = .{
            .lhs = identifierNode,
            .rhs = limitdef_indexu32,
        },
    });
}

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
    if (p.eatToken(.comment_line) orelse p.eatToken(.comment_block)) |tok| {
        var first_comment = tok;
        if (tok > 0 and tokensOnSameLine(p, tok - 1, tok)) {
            try p.warnMsg(.{
                .tag = .same_line_doc_comment,
                .token = tok,
            });
            first_comment = p.eatToken(.comment_line) orelse p.eatToken(.comment_block) orelse return null;
        }
        while (p.eatToken(.comment_line) orelse p.eatToken(.comment_block)) |_| {}
        return first_comment;
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
