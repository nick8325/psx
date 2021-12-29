const std = @import("std");
const utils = @import("utils.zig");

pub const MemoryError = error {
    UnknownAddress,
    UnalignedAccess
};

const page_size = 0x1000;

pub const Page = struct {
    val: usize,

    fn invalid() Page {
        return .{.val = 0};
    }

    fn valid(page: *[page_size]u8, writable: bool, io: bool) Page {
        const addr = @ptrToInt(page);

        std.debug.assert(addr % page_size == 0);
        return .{.val = addr | @as(usize, if (writable) 1 else 0) | @as(usize, if (io) 2 else 0)};
    }

    fn pointer(page: Page) ?*[page_size]u8 {
        if (page.val == 0) return null
        else return @intToPtr(*[page_size]u8, (page.val & ~@as(usize, 0b11)));
    }

    fn isWritable(page: Page) bool {
        return (page.val & 1) != 0;
    }

    fn isIo(page: Page) bool {
        return (page.val & 2) != 0;
    }
};

pub var page_table align(4096) = [_]Page{Page.invalid()} ** 0x100000;
pub var ram align(4096) = [_]u8{0} ** 0x200000;
pub var scratchpad align(4096) = [_]u8{0} ** 0x1000;
pub var bios align(4096) = utils.readArray(u8, "../SCPH1002.bin");
pub var expansion align(4096) = [_]u8{255} ** 0x1000;
pub var io_ports align(4096) = [_]u8{0} ** 0x1000;
pub var cache_control align(4096) = [_]u8{0} ** 0x1000;

// 4KB pages.
// Each page is address + readable bit + writable bit + "IO bit".
// If IO bit is set then separate IO handler gets invoked afterwards.

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
    RAM{.start = 0xfffe0000, .memory = cache_control[0..], .writable = true}
};

pub fn fetch(addr: u32) !u32 {
    std.debug.assert(addr % 4 == 0);

    const page = addr >> 12;
    const offset = addr & 0xfff;

    if (page_table[page].pointer()) |ptr|
        return std.mem.bytesAsSlice(u32, ptr)[offset/@sizeOf(u32)]
    else {
        std.log.err("fetch of unknown address {x}", .{addr});
        return error.UnknownAddress;
    }
}

pub fn read(comptime T: type, addr: u32) !T {
    if (addr % @sizeOf(T) != 0) {
        std.log.err("read of unaligned address {x} at size {}", .{addr, @sizeOf(T)});
        return error.UnalignedAccess;
    }

    const page = addr >> 12;
    const offset = addr & 0xfff;

    if (page_table[page].pointer()) |ptr|
        return std.mem.bytesAsSlice(T, ptr)[offset/@sizeOf(T)]
    else {
        std.log.err("read from unknown address {x}", .{addr});
        return error.UnknownAddress;
    }
}

pub fn write(comptime T: type, addr: u32, value: T) !void {
    if (addr % @sizeOf(T) != 0) {
        std.log.err("write of unaligned address {x} at size {}", .{addr, @sizeOf(T)});
        return error.UnalignedAccess;
    }

    const page = addr >> 12;
    const offset = addr & 0xfff;

    if (page_table[page].pointer()) |ptr| {
        if (page_table[page].isWritable())
            std.mem.bytesAsSlice(T, ptr)[offset/@sizeOf(T)] = value;
    } else {
        std.log.err("write to unknown address {x}", .{addr});
        return error.UnknownAddress;
    }
}

const RAM = struct {
    start: u32,
    memory: []u8,
    writable: bool,

    fn end(self: RAM) u32 {
        return self.start + @intCast(u32, self.memory.len);
    }
};

fn map(block: RAM, offset: u32) void {
    std.debug.assert(block.start % page_size == 0);
    std.debug.assert(block.end() % page_size == 0);

    var i: u32 = 0;
    while (i < block.end() - block.start) : (i += page_size) {
        const pageno = (offset + block.start + i) / page_size;
        const ptr = @ptrCast(*[page_size]u8, block.memory[i..i+page_size].ptr);
        page_table[pageno] = Page.valid(ptr, block.writable, false);
    }
}

pub fn init() !void {
    inline for (memory_map) |block| {
        map(block, 0);
        if (block.end() <= 0x20000000) {
            // Add the block to KSEG0 and KSEG1
            map(block, 0x80000000);
            map(block, 0xa0000000);
        }
    }

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
