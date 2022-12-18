//! This file contains request types zls handles.
//! Note that the parameter types may be incomplete.
//! We only define what we actually use.

const std = @import("std");
const types = @import("types.zig");

/// Only check for the field's existence.
const Exists = struct {
    exists: bool,
};

fn Default(comptime T: type, comptime default_value: T) type {
    return struct {
        pub const value_type = T;
        pub const default = default_value;
        value: T,
    };
}

pub fn ErrorUnwrappedReturnOf(comptime func: anytype) type {
    return switch (@typeInfo(@TypeOf(func))) {
        .Fn, .BoundFn => |fn_info| switch (@typeInfo(fn_info.return_type.?)) {
            .ErrorUnion => |err_union| err_union.payload,
            else => |T| return T,
        },
        else => unreachable,
    };
}

fn Transform(comptime Original: type, comptime transform_fn: anytype) type {
    return struct {
        pub const original_type = Original;
        pub const transform = transform_fn;

        value: ErrorUnwrappedReturnOf(transform_fn),
    };
}

fn fromDynamicTreeInternal(arena: *std.heap.ArenaAllocator, value: std.json.Value, out: anytype) error{ MalformedJson, OutOfMemory }!void {
    const T = comptime std.meta.Child(@TypeOf(out));

    if (comptime std.meta.trait.is(.Struct)(T)) {
        if (value != .Object) return error.MalformedJson;

        var err = false;
        inline for (std.meta.fields(T)) |field| {
            const is_exists = field.type == Exists;

            const is_optional = comptime std.meta.trait.is(.Optional)(field.type);
            const actual_type = if (is_optional) std.meta.Child(field.type) else field.type;

            const is_struct = comptime std.meta.trait.is(.Struct)(actual_type);
            const is_default = comptime if (is_struct) std.meta.trait.hasDecls(actual_type, .{ "default", "value_type" }) else false;
            const is_transform = comptime if (is_struct) std.meta.trait.hasDecls(actual_type, .{ "original_type", "transform" }) else false;

            if (value.Object.get(field.name)) |json_field| {
                if (is_exists) {
                    @field(out, field.name) = Exists{ .exists = true };
                } else if (is_transform) {
                    var original_value: actual_type.original_type = undefined;
                    try fromDynamicTreeInternal(arena, json_field, &original_value);
                    @field(out, field.name) = actual_type{
                        .value = actual_type.transform(original_value) catch
                            return error.MalformedJson,
                    };
                } else if (is_default) {
                    try fromDynamicTreeInternal(arena, json_field, &@field(out, field.name).value);
                } else if (is_optional) {
                    if (json_field == .Null) {
                        @field(out, field.name) = null;
                    } else {
                        var actual_value: actual_type = undefined;
                        try fromDynamicTreeInternal(arena, json_field, &actual_value);
                        @field(out, field.name) = actual_value;
                    }
                } else {
                    try fromDynamicTreeInternal(arena, json_field, &@field(out, field.name));
                }
            } else {
                if (is_exists) {
                    @field(out, field.name) = Exists{ .exists = false };
                } else if (is_optional) {
                    @field(out, field.name) = null;
                } else if (is_default) {
                    @field(out, field.name) = actual_type{ .value = actual_type.default };
                } else {
                    err = true;
                }
            }
        }
        if (err) return error.MalformedJson;
    } else if (comptime (std.meta.trait.isSlice(T) and T != []const u8)) {
        if (value != .Array) return error.MalformedJson;
        const Child = std.meta.Child(T);

        if (value.Array.items.len == 0) {
            out.* = &[0]Child{};
        } else {
            var slice = try arena.allocator().alloc(Child, value.Array.items.len);
            for (value.Array.items) |arr_item, idx| {
                try fromDynamicTreeInternal(arena, arr_item, &slice[idx]);
            }
            out.* = slice;
        }
    } else if (T == std.json.Value) {
        out.* = value;
    } else if (comptime std.meta.trait.is(.Enum)(T)) {
        const info = @typeInfo(T).Enum;
        const TagType = info.tag_type;
        if (value != .Integer) return error.MalformedJson;
        out.* = std.meta.intToEnum(
            T,
            std.math.cast(TagType, value.Integer) orelse return error.MalformedJson,
        ) catch return error.MalformedJson;
    } else if (comptime std.meta.trait.is(.Int)(T)) {
        if (value != .Integer) return error.MalformedJson;
        out.* = std.math.cast(T, value.Integer) orelse return error.MalformedJson;
    } else switch (T) {
        bool => {
            if (value != .Bool) return error.MalformedJson;
            out.* = value.Bool;
        },
        f64 => {
            if (value != .Float) return error.MalformedJson;
            out.* = value.Float;
        },
        []const u8 => {
            if (value != .String) return error.MalformedJson;
            out.* = value.String;
        },
        else => @compileError("Invalid type " ++ @typeName(T)),
    }
}

pub fn fromDynamicTree(arena: *std.heap.ArenaAllocator, comptime T: type, value: std.json.Value) error{ MalformedJson, OutOfMemory }!T {
    var out: T = undefined;
    try fromDynamicTreeInternal(arena, value, &out);
    return out;
}

const MaybeStringArray = Default([]const []const u8, &.{});

pub const Initialize = struct {
    pub const ClientCapabilities = struct {
        workspace: ?struct {
            configuration: Default(bool, false),
            didChangeConfiguration: ?struct {
                dynamicRegistration: Default(bool, false), // NOTE: Should this be true? Seems like this critical feature should be nearly universal
            },
            workspaceFolders: Default(bool, false),
        },
        textDocument: ?struct {
            synchronization: ?struct {
                willSave: Default(bool, false),
                willSaveWaitUntil: Default(bool, false),
                didSave: Default(bool, false),
            },
            semanticTokens: Exists,
            inlayHint: Exists,
            hover: ?struct {
                contentFormat: MaybeStringArray,
            },
            completion: ?struct {
                completionItem: ?struct {
                    snippetSupport: Default(bool, false),
                    labelDetailsSupport: Default(bool, false),
                    documentationFormat: MaybeStringArray,
                },
            },
            documentHighlight: Exists,
        },
        general: ?struct {
            positionEncodings: MaybeStringArray,
        },
    };

    pub const ClientInfo = struct {
        name: []const u8,
        version: ?[]const u8,
    };

    params: struct {
        clientInfo: ?ClientInfo,
        capabilities: ClientCapabilities,
        workspaceFolders: ?[]const types.WorkspaceFolder,
    },
};

pub const WorkspaceFoldersChange = struct {
    params: struct {
        event: struct {
            added: []const types.WorkspaceFolder,
            removed: []const types.WorkspaceFolder,
        },
    },
};

pub const OpenDocument = struct {
    params: struct {
        textDocument: struct {
            uri: []const u8,
            text: []const u8,
        },
    },
};

const TextDocumentIdentifier = struct {
    uri: []const u8,
};

pub const ChangeDocument = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        contentChanges: []TextDocumentContentChangeEvent,
    },
};

pub const TextDocumentContentChangeEvent = struct {
    range: ?types.Range,
    text: []const u8,
};

const TextDocumentIdentifierRequest = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
    },
};

pub const SaveDocument = TextDocumentIdentifierRequest;
pub const CloseDocument = TextDocumentIdentifierRequest;
pub const SemanticTokensFull = TextDocumentIdentifierRequest;

const TextDocumentIdentifierPositionRequest = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        position: types.Position,
    },
};

pub const SaveReason = enum(u32) {
    Manual = 1,
    AfterDelay = 2,
    FocusOut = 3,
};

pub const WillSave = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        reason: SaveReason,
    },
};

pub const SignatureHelp = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        position: types.Position,
        context: ?struct {
            triggerKind: enum(u32) {
                invoked = 1,
                trigger_character = 2,
                content_change = 3,
            },
            triggerCharacter: ?[]const u8,
            isRetrigger: bool,
            activeSignatureHelp: ?types.SignatureHelp,
        },
    },
};

pub const Completion = TextDocumentIdentifierPositionRequest;
pub const GotoDefinition = TextDocumentIdentifierPositionRequest;
pub const GotoDeclaration = TextDocumentIdentifierPositionRequest;
pub const Hover = TextDocumentIdentifierPositionRequest;
pub const DocumentSymbols = TextDocumentIdentifierRequest;
pub const Formatting = TextDocumentIdentifierRequest;
pub const DocumentHighlight = TextDocumentIdentifierPositionRequest;
pub const Rename = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        position: types.Position,
        newName: []const u8,
    },
};

pub const References = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        position: types.Position,
        context: struct {
            includeDeclaration: bool,
        },
    },
};

pub const InlayHint = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        range: types.Range,
    },
};

pub const CodeAction = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        range: types.Range,
        context: struct {
            diagnostics: []types.Diagnostic,
        },
    },
};

pub const FoldingRange = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
    },
};

pub const SelectionRange = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        positions: []types.Position,
    },
};
