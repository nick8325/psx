const std = @import("std");
const utils = @import("utils.zig");

pub const MemoryError = error {
    UnknownAddress,
    UnalignedAccess
};

pub var ram = [_]u8{0} ** 0x200000;
pub var scratchpad = [_]u8{0} ** 0x400;
pub var bios = utils.readArray(u8, "../SCPH1002.bin");
pub var expansion = [_]u8{0} ** 0x100;
pub var io_ports = [_]u8{0} ** 0x1000;
pub var cache_control = @bitCast([4]u8, [1]u32{0});

const memory_map = .{
    // TODO: configurable RAM size
    RAM{.start = 0, .memory = ram[0..], .writable = true},
    RAM{.start = 0x200000, .memory = ram[0..], .writable = true},
    RAM{.start = 0x400000, .memory = ram[0..], .writable = true},
    RAM{.start = 0x600000, .memory = ram[0..], .writable = true},
    RAM{.start = 0x1f000000, .memory = expansion[0..], .writable = false},
    RAM{.start = 0x1f800000, .memory = scratchpad[0..], .writable = true},
    RAM{.start = 0x1f801000, .memory = io_ports[0..], .writable = true},
    RAM{.start = 0x1f802000, .memory = expansion[0..], .writable = false},
    RAM{.start = 0x1fc00000, .memory = bios[0..], .writable = true},
    RAM{.start = 0xfffe0130, .memory = cache_control[0..], .writable = true}
};

pub fn fetch(addr: u32) !u32 {
    return read(u32, addr);
}

pub fn read(comptime T: type, addr: u32) !T {
    if (addr % @sizeOf(T) != 0) {
        std.log.err("read of unaligned address {x} at size {}", .{addr, @sizeOf(T)});
        return error.UnalignedAccess;
    }

    inline for (memory_map) |block| {
        var start = block.start;
        if (start <= addr and addr < block.end())
            return block.read(T, addr);
    }

    std.log.err("read from unknown address {x}", .{addr});
    return error.UnknownAddress;
}

pub fn write(comptime T: type, addr: u32, value: T) !void {
    if (addr % @sizeOf(T) != 0) {
        std.log.err("write of unaligned address {x} at size {}", .{addr, @sizeOf(T)});
        return error.UnalignedAccess;
    }

    inline for (memory_map) |block| {
        var start = block.start;
        if (start <= addr and addr < block.end())
            return block.write(T, addr, value);
    }

    std.log.err("write to unknown address {x}", .{addr});
    return error.UnknownAddress;
}

const RAM = struct {
    start: u32,
    memory: []u8,
    writable: bool,

    fn read(self: RAM, comptime T: type, addr: u32) T {
        return std.mem.bytesAsSlice(T, self.memory)[(addr - self.start)/@sizeOf(T)];
    }

    fn write(self: RAM, comptime T: type, addr: u32, value: T) void {
        if (self.writable)
            std.mem.bytesAsSlice(T, self.memory)[(addr - self.start)/@sizeOf(T)] = value;
    }

    fn end(self: RAM) u32 {
        return self.start + @intCast(u32, self.memory.len);
    }
};

pub fn init() !void {
    try write(u32, 0x1f801000, 0x1f000000);
    try write(u32, 0x1f801004, 0x1f802000);
    try write(u32, 0x1f801008, 0x0013243f);
    try write(u32, 0x1f80100c, 0x00003022);
    try write(u32, 0x1f801010, 0x0013243f);
    try write(u32, 0x1f801014, 0x200931e1);
    try write(u32, 0x1f801018, 0x00020843);
    try write(u32, 0x1f80101c, 0x00070777);
    try write(u32, 0x1f801020, 0x00031125);
    try write(u32, 0x1f801060, 0x00000b88);
}
