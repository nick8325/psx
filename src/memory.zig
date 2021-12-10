const std = @import("std");
pub var ram = [_]u32{0} ** 0x80000;
pub var scratchpad = [_]u32{0} ** 0x100;

fn read_bios(comptime path: []const u8) [0x20000]u32 {
    const cstr = @embedFile(path);
    const bytes = @bitCast([0x80001]u8, cstr.*);
    return @bitCast([0x20000]u32, bytes[0..0x80000].*);
}

pub var bios = read_bios("../SCPH1002.bin");

fn abort(msg: anytype) noreturn {
    @setRuntimeSafety(true);
    std.log.err("{s}", .{msg});
    unreachable;
}

const RAM = struct {
    start: u32,
    memory: []u32,

    fn read(self: RAM, addr: u32) u32 {
        return self.memory[(addr - self.start) / 4];
    }

    fn write(self: RAM, addr: u32, value: u32) void {
        self.memory[(addr - self.start) / 4] = value;
    }

    fn end(self: RAM) u32 {
        return self.start + @intCast(u32, self.memory.len)*4;
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

pub fn read(addr: u32) u32 {
    std.debug.assert(addr % 4 == 0);

    inline for (memory_map) |block| {
        var start = block.start;
        if (start <= addr and addr < block.end())
            return block.read(addr);
    }

    abort("unknown read");
}

pub fn write(addr: u32, value: u32) void {
    std.debug.assert(addr % 4 == 0);

    inline for (memory_map) |block| {
        if (block.start <= addr and addr < block.end())
            return block.write(addr, value);
    }

    abort("unknown write");
}
