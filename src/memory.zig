const std = @import("std");
const bios_cstr = @embedFile("../SCPH1002.bin");
const bios_bytes = @bitCast([0x80001]u8, bios_cstr.*);
pub const bios = @bitCast([0x20000]u32, bios_bytes[0..0x80000].*);
pub var ram = [_]u32{0} ** 0x80000;

fn abort(msg: anytype) noreturn {
    @setRuntimeSafety(true);
    std.log.err("{s}", .{msg});
    unreachable;
}

pub fn read(in_addr: u32) u32 {
    var addr = in_addr;
    std.debug.assert(addr % 4 == 0);

    if (addr >= 0xa0000000)
        addr -= 0xa0000000
    else if (addr >= 0x80000000)
        addr -= 0x80000000;

    if (addr < 0x200000)
        return ram[addr / 4]
    else if (addr >= 0x1fc00000 and addr < 0x1fc80000)
        return bios[(addr - 0x1fc00000) / 4]
    else
        abort("unknown address");
}

pub fn write(in_addr: u32, value: u32) void {
    var addr = in_addr;
    std.debug.assert(addr % 4 == 0);

    if (addr >= 0xa0000000)
        addr -= 0xa0000000
    else if (addr >= 0x80000000)
        addr -= 0x80000000;

    if (addr < 0x200000)
        ram[addr / 4] = value
    else if (addr >= 0x1fc00000 and addr < 0x1fc80000)
        abort("write to bios")
    else
        abort("unknown address");
}
