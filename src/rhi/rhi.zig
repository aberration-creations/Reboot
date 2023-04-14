const glfw = @import("glfw");
const std = @import("std");
const RHIType = @import("rhi_types.zig");

const Allocator = std.mem.Allocator;

pub const RHI = union(enum) {
    const Self = @This();
    vulkan: *@import("vulkan/vulkan_renderer.zig"),

    pub inline fn initVulkan(allocator: Allocator) !Self {
        return Self{ .vulkan = try allocator.create(@import("vulkan/vulkan_renderer.zig")) };
    }

    pub fn deinitVulkan(self: Self, allocator: Allocator) void {
        allocator.destroy(self.vulkan);
    }

    pub fn deinit(self: RHI) void {
        switch (self) {
            inline else => |impl| impl.deinit(),
        }
    }

    pub fn getQueue(
        self: RHI,
        family: RHIType.QueueFamily,
    ) RHIType.Queue {
        switch (self) {
            inline else => |impl| return impl.getQueue(
                family,
            ),
        }
    }

    pub fn createContext(
        self: RHI,
        allocator: Allocator,
        app_name: [*:0]const u8,
        window: glfw.Window,
    ) anyerror!void {
        switch (self) {
            inline else => |impl| try impl.createContext(
                allocator,
                app_name,
                window,
            ),
        }
    }

    pub fn createSwapChain(
        self: RHI,
        allocator: Allocator,
    ) anyerror!void {
        switch (self) {
            inline else => |impl| try impl.createSwapChain(
                allocator,
            ),
        }
    }

    pub fn recreateSwapchain(
        self: RHI,
        allocator: Allocator,
    ) !void {
        switch (self) {
            inline else => |impl| try impl.recreateSwapchain(
                allocator,
            ),
        }
    }

    pub fn createImageViews(
        self: RHI,
        allocator: Allocator,
    ) anyerror!void {
        switch (self) {
            inline else => |impl| try impl.createImageViews(
                allocator,
            ),
        }
    }

    pub fn createCommandPool(
        self: RHI,
        queue: RHIType.Queue,
        bIsTransient: bool,
    ) anyerror!RHIType.CommandPool {
        switch (self) {
            inline else => |impl| return try impl.createCommandPool(
                queue,
                bIsTransient,
            ),
        }
    }

    pub fn destroyCommandPool(
        self: RHI,
        pool: RHIType.CommandPool,
    ) void {
        switch (self) {
            inline else => |impl| impl.destroyCommandPool(
                pool,
            ),
        }
    }

    pub fn createCommandBuffers(
        self: RHI,
        allocator: Allocator,
        pool: RHIType.CommandPool,
        bIsSecondary: bool,
        count: u32,
    ) anyerror![]RHIType.CommandBuffer {
        switch (self) {
            inline else => |impl| return try impl.createCommandBuffer(
                allocator,
                pool,
                bIsSecondary,
                count,
            ),
        }
    }

    pub fn destroyCommandBuffers(
        self: RHI,
        allocator: Allocator,
        pool: RHIType.CommandPool,
        count: u32,
        commandBuffers: []RHIType.CommandBuffer,
    ) void {
        switch (self) {
            inline else => |impl| impl.destroyCommandBuffers(
                allocator,
                pool,
                count,
                commandBuffers,
            ),
        }
    }

    pub fn beginCommandBuffer(
        self: RHI,
        commandBuffer: RHIType.CommandBuffer,
    ) !void {
        switch (self) {
            inline else => |impl| try impl.beginCommandBuffer(
                commandBuffer,
            ),
        }
    }

    pub fn endCommandBuffer(
        self: RHI,
        commandBuffer: RHIType.CommandBuffer,
    ) !void {
        switch (self) {
            inline else => |impl| try impl.endCommandBuffer(
                commandBuffer,
            ),
        }
    }

    pub fn createBuffer(
        self: RHI,
        allocator: Allocator,
        usage: RHIType.BufferUsage,
        size: u64,
        hostVisible: bool,
    ) anyerror!*RHIType.Buffer {
        switch (self) {
            inline else => |impl| return impl.createBuffer(
                allocator,
                usage,
                size,
                hostVisible,
            ),
        }
    }

    pub fn destroyBuffer(
        self: RHI,
        allocator: Allocator,
        buffer: *RHIType.Buffer,
    ) void {
        switch (self) {
            inline else => |impl| impl.destroyBuffer(
                allocator,
                buffer,
            ),
        }
    }
};
