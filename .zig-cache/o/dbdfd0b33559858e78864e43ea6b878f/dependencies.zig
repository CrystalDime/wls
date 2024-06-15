pub const packages = struct {
    pub const @"1220102cb2c669d82184fb1dc5380193d37d68b54e8d75b76b2d155b9af7d7e2e76d" = struct {
        pub const build_root = "/home/delue/.cache/zig/p/1220102cb2c669d82184fb1dc5380193d37d68b54e8d75b76b2d155b9af7d7e2e76d";
        pub const build_zig = @import("1220102cb2c669d82184fb1dc5380193d37d68b54e8d75b76b2d155b9af7d7e2e76d");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"12209cde192558f8b3dc098ac2330fc2a14fdd211c5433afd33085af75caa9183147" = struct {
        pub const build_root = "/home/delue/.cache/zig/p/12209cde192558f8b3dc098ac2330fc2a14fdd211c5433afd33085af75caa9183147";
        pub const build_zig = @import("12209cde192558f8b3dc098ac2330fc2a14fdd211c5433afd33085af75caa9183147");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
};

pub const root_deps: []const struct { []const u8, []const u8 } = &.{
    .{ "known_folders", "12209cde192558f8b3dc098ac2330fc2a14fdd211c5433afd33085af75caa9183147" },
    .{ "diffz", "1220102cb2c669d82184fb1dc5380193d37d68b54e8d75b76b2d155b9af7d7e2e76d" },
};
