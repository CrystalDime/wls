pub const @"log.Level" = enum (u2) {
    err = 0,
    warn = 1,
    info = 2,
    debug = 3,
};
pub const log_level: @"log.Level" = .info;
pub const enable_failing_allocator: bool = false;
pub const enable_failing_allocator_likelihood: u32 = 256;
pub const use_gpa: bool = true;
