const std = @import("std");
const utils = @import("utils.zig");

pub var ram = [_]u8{0} ** 0x200000;
pub var scratchpad = [_]u8{0} ** 0x400;
pub var bios = utils.readArray(u8, "../SCPH1002.bin");

pub const MemoryError = error {
    UnknownAddress,
    UnalignedAccess
};

const RAM = struct {
    start: u32,
    memory: []u8,

    fn read(self: RAM, comptime T: type, addr: u32) T {
        return std.mem.bytesAsSlice(T, self.memory)[(addr - self.start)/@sizeOf(T)];
    }

    fn write(self: RAM, comptime T: type, addr: u32, value: T) void {
        std.mem.bytesAsSlice(T, self.memory)[(addr - self.start)/@sizeOf(T)] = value;
    }

    fn end(self: RAM) u32 {
        return self.start + @intCast(u32, self.memory.len);
    }
};

const memory_map = .{
    RAM{.start = 0, .memory = ram[0..]},
    RAM{.start = 0x80000000, .memory = ram[0..]},
    RAM{.start = 0xa0000000, .memory = ram[0..]},
    RAM{.start = 0x1f800000, .memory = scratchpad[0..]},
    RAM{.start = 0x9f800000, .memory = scratchpad[0..]},
    RAM{.start = 0xbf800000, .memory = scratchpad[0..]},
    RAM{.start = 0x1fc00000, .memory = bios[0..]},
    RAM{.start = 0x9fc00000, .memory = bios[0..]},
    RAM{.start = 0xbfc00000, .memory = bios[0..]},
};

pub fn read(comptime T: type, addr: u32) !T {
    if (addr % @sizeOf(T) != 0) {
        std.log.err("read of aligned address {x} at size {}", .{addr, @sizeOf(T)});
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
        std.log.err("read of aligned address {x} at size {}", .{addr, @sizeOf(T)});
        return error.UnalignedAccess;
    }

    inline for (memory_map) |block| {
        var start = block.start;
        if (start <= addr and addr < block.end())
            return block.write(T, addr, value);
    }

    std.log.err("write to unknown address {x}", .{addr});
//    return error.UnknownAddress;
}
