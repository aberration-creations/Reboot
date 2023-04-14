const std = @import("std");
const glfw = @import("glfw");

const vk = @import("vk.zig");
const RHIType = @import("../rhi_types.zig");

const RHIInternalType = @import("structures.zig");

const GraphicsContext = @import("graphics_context.zig").GraphicsContext;
const Allocator = std.mem.Allocator;
const assert = @import("std").debug.assert;

pub const VulkanRenderer = @This();

context: GraphicsContext = undefined,
swapchain: vk.SwapchainKHR = undefined,
swapChainImages: std.ArrayList(vk.Image) = undefined,
swapChainImageFormat: vk.Format = undefined,
swapChainExtent: vk.Extent2D = undefined,
swapChainImageViews: std.ArrayList(vk.ImageView) = undefined,
surface: vk.SurfaceKHR = undefined,
window: glfw.Window = undefined,

present_queue_family_index: u32 = undefined,

pub fn deinit(self: *VulkanRenderer) void {
    if (self.context.vkd.queueWaitIdle(self.context.graphics_queue.handle)) {} else |err| switch (err) {
        error.DeviceLost => {
            std.debug.print("DeviceLost\n", .{});
        },
        error.OutOfDeviceMemory => {
            std.debug.print("OutOfDeviceMemory\n", .{});
        },
        error.OutOfHostMemory => {
            std.debug.print("OutOfHostMemory\n", .{});
        },
        error.Unknown => {
            std.debug.print("Unknown\n", .{});
        },
    }
    self.swapChainImages.deinit();
    self.cleanupSwapchain();
    self.swapChainImageViews.deinit();

    self.context.vki.destroySurfaceKHR(
        self.context.instance,
        self.surface,
        null,
    );
    self.context.deinit();
}

pub fn recreateSwapchain(
    self: *VulkanRenderer,
    allocator: Allocator,
) !void {
    var width: u32 = 0;
    var height: u32 = 0;
    while (width == 0 or height == 0) {
        const size = self.window.getFramebufferSize();
        glfw.waitEvents();
        width = size.width;
        height = size.height;
    }
    try self.context.vkd.deviceWaitIdle(self.context.dev);

    self.cleanupSwapchain();

    try self.createSwapChain(allocator);
    try self.createImageViews(allocator);
}

pub fn cleanupSwapchain(
    self: *VulkanRenderer,
) void {
    for (self.swapChainImageViews.items) |view| {
        self.context.vkd.destroyImageView(
            self.context.dev,
            view,
            null,
        );
    }
    self.context.vkd.destroySwapchainKHR(
        self.context.dev,
        self.swapchain,
        null,
    );
}

pub fn createContext(
    self: *VulkanRenderer,
    allocator: Allocator,
    app_name: [*:0]const u8,
    window: glfw.Window,
) anyerror!void {
    self.swapChainImages = std.ArrayList(vk.Image).init(allocator);
    self.swapChainImageViews = std.ArrayList(vk.ImageView).init(allocator);
    self.window = window;
    self.context = try GraphicsContext.init(
        allocator,
        app_name,
        window,
    );
    std.debug.print("Using device: {?s}\n", .{self.context.props.device_name});
    const result = glfw.createWindowSurface(
        self.context.instance,
        window,
        null,
        &self.surface,
    );
    if (result != 0) {
        std.debug.print("Failed to create window surface\n", .{});
        return error.failedToCreateSurface;
    }
}

pub fn getQueue(
    self: *VulkanRenderer,
    family: RHIType.QueueFamily,
) RHIType.Queue {
    if (family == .graphics) {
        return RHIType.Queue{
            .queue = RHIType.QueueType{
                .vulkan = RHIInternalType.VulkanQueue{
                    .queue = self.context.graphics_queue.handle,
                    .queueFamilyIndex = self.context.graphics_queue.family,
                },
            },
            .impl = RHIType.Api.vulkan,
        };
    } else if (family == .present) {
        return RHIType.Queue{
            .queue = RHIType.QueueType{
                .vulkan = RHIInternalType.VulkanQueue{
                    .queue = self.context.vkd.getDeviceQueue(self.context.dev, self.present_queue_family_index, 0),
                    .queueFamilyIndex = self.present_queue_family_index,
                },
            },
            .impl = RHIType.Api.vulkan,
        };
    }
    return RHIType.Queue{
        .queue = RHIType.QueueType{
            .vulkan = RHIInternalType.VulkanQueue{
                .queue = vk.Queue.null_handle,
                .queueFamilyIndex = 0xFFFFFFFF,
            },
        },
        .impl = RHIType.Api.vulkan,
    };
}

fn querySwapChainSupport(
    self: *VulkanRenderer,
    allocator: Allocator,
) RHIType.ErrorCode!RHIInternalType.SwapChainSupportDetails {
    const capabilities = self.context.vki.getPhysicalDeviceSurfaceCapabilitiesKHR(
        self.context.pdev,
        self.surface,
    ) catch |err| switch (err) {
        error.OutOfHostMemory => {
            std.debug.print("OutOfHostMemory\n", .{});
            return RHIType.ErrorCode.outOfHostMemory;
        },
        error.OutOfDeviceMemory => {
            std.debug.print("OutOfDeviceMemory\n", .{});
            return RHIType.ErrorCode.outOfDeviceMemory;
        },
        error.SurfaceLostKHR => {
            std.debug.print("SurfaceLostKHR\n", .{});
            return RHIType.ErrorCode.surfaceLost;
        },
        error.Unknown => {
            std.debug.print("Unknown\n", .{});
            return RHIType.ErrorCode.unknown;
        },
    };

    var formatCount: u32 = 0;
    _ = self.context.vki.getPhysicalDeviceSurfaceFormatsKHR(
        self.context.pdev,
        self.surface,
        &formatCount,
        null,
    ) catch |err| switch (err) {
        error.OutOfHostMemory => {
            std.debug.print("OutOfHostMemory\n", .{});
            return RHIType.ErrorCode.outOfHostMemory;
        },
        error.OutOfDeviceMemory => {
            std.debug.print("OutOfDeviceMemory\n", .{});
            return RHIType.ErrorCode.outOfDeviceMemory;
        },
        error.SurfaceLostKHR => {
            std.debug.print("SurfaceLostKHR\n", .{});
            return RHIType.ErrorCode.surfaceLost;
        },
        error.Unknown => {
            std.debug.print("Unknown\n", .{});
            return RHIType.ErrorCode.unknown;
        },
    };
    const surfaceFormats = allocator.alloc(vk.SurfaceFormatKHR, formatCount) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("Unknown\n", .{});
            return RHIType.ErrorCode.outOfMemory;
        },
    };
    //defer allocator.free(surfaceFormats);
    if (formatCount != 0) {
        _ = self.context.vki.getPhysicalDeviceSurfaceFormatsKHR(
            self.context.pdev,
            self.surface,
            &formatCount,
            surfaceFormats.ptr,
        ) catch |err| switch (err) {
            error.OutOfHostMemory => {
                std.debug.print("OutOfHostMemory\n", .{});
                return RHIType.ErrorCode.outOfHostMemory;
            },
            error.OutOfDeviceMemory => {
                std.debug.print("OutOfDeviceMemory\n", .{});
                return RHIType.ErrorCode.outOfDeviceMemory;
            },
            error.SurfaceLostKHR => {
                std.debug.print("SurfaceLostKHR\n", .{});
                return RHIType.ErrorCode.surfaceLost;
            },
            error.Unknown => {
                std.debug.print("Unknown\n", .{});
                return RHIType.ErrorCode.unknown;
            },
        };
    }

    var presentModeCount: u32 = 0;
    _ = self.context.vki.getPhysicalDeviceSurfacePresentModesKHR(
        self.context.pdev,
        self.surface,
        &presentModeCount,
        null,
    ) catch |err| switch (err) {
        error.OutOfHostMemory => {
            std.debug.print("OutOfHostMemory\n", .{});
            return RHIType.ErrorCode.outOfHostMemory;
        },
        error.OutOfDeviceMemory => {
            std.debug.print("OutOfDeviceMemory\n", .{});
            return RHIType.ErrorCode.outOfDeviceMemory;
        },
        error.SurfaceLostKHR => {
            std.debug.print("SurfaceLostKHR\n", .{});
            return RHIType.ErrorCode.surfaceLost;
        },
        error.Unknown => {
            std.debug.print("Unknown\n", .{});
            return RHIType.ErrorCode.unknown;
        },
    };
    const presentModes = allocator.alloc(vk.PresentModeKHR, presentModeCount) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("OutOfMemory\n", .{});
            return RHIType.ErrorCode.outOfMemory;
        },
    };
    //defer allocator.free(presentModes);
    if (presentModeCount != 0) {
        _ = self.context.vki.getPhysicalDeviceSurfacePresentModesKHR(
            self.context.pdev,
            self.surface,
            &formatCount,
            presentModes.ptr,
        ) catch |err| switch (err) {
            error.OutOfHostMemory => {
                std.debug.print("OutOfHostMemory\n", .{});
                return RHIType.ErrorCode.outOfHostMemory;
            },
            error.OutOfDeviceMemory => {
                std.debug.print("OutOfDeviceMemory\n", .{});
                return RHIType.ErrorCode.outOfDeviceMemory;
            },
            error.SurfaceLostKHR => {
                std.debug.print("SurfaceLostKHR\n", .{});
                return RHIType.ErrorCode.surfaceLost;
            },
            error.Unknown => {
                std.debug.print("Unknown\n", .{});
                return RHIType.ErrorCode.unknown;
            },
        };
    }

    return RHIInternalType.SwapChainSupportDetails{
        .capabilities = capabilities,
        .formats = surfaceFormats[0..formatCount],
        .presentModes = presentModes,
    };
}

fn chooseSwapSurfaceFormat(
    self: *VulkanRenderer,
    availableFormats: []vk.SurfaceFormatKHR,
) vk.SurfaceFormatKHR {
    _ = self;
    for (availableFormats) |f| {
        if (f.format == vk.Format.b8g8r8a8_srgb and f.color_space == vk.ColorSpaceKHR.srgb_nonlinear_khr) {
            return f;
        }
    }
    return availableFormats[0];
}

fn chooseSwapPresentMode(
    self: *VulkanRenderer,
    availablePresentModes: []vk.PresentModeKHR,
) vk.PresentModeKHR {
    _ = self;
    for (availablePresentModes) |p| {
        if (p == vk.PresentModeKHR.mailbox_khr) {
            return p;
        }
    }
    return vk.PresentModeKHR.fifo_khr;
}

fn chooseSwapExtent(
    self: *VulkanRenderer,
    capabilities: vk.SurfaceCapabilitiesKHR,
) vk.Extent2D {
    if (capabilities.current_extent.width != std.math.maxInt(u32)) {
        return capabilities.current_extent;
    } else {
        const size = glfw.Window.getFramebufferSize(self.window);
        var actualExtent = vk.Extent2D{
            .width = size.width,
            .height = size.height,
        };
        actualExtent.width = std.math.clamp(
            actualExtent.width,
            capabilities.min_image_extent.width,
            capabilities.max_image_extent.width,
        );
        actualExtent.height = std.math.clamp(
            actualExtent.height,
            capabilities.min_image_extent.height,
            capabilities.max_image_extent.height,
        );
        return actualExtent;
    }
}

fn findQueueFamilies(self: *VulkanRenderer, allocator: Allocator) RHIType.ErrorCode!RHIInternalType.QueueFamilyIndices {
    var indices = RHIInternalType.QueueFamilyIndices{
        .graphicsFamily = 0,
        .presentFamily = 0,
    };
    var queueFamilyCount: u32 = 0;
    self.context.vki.getPhysicalDeviceQueueFamilyProperties(
        self.context.pdev,
        &queueFamilyCount,
        null,
    );
    const queueFamilies = allocator.alloc(vk.QueueFamilyProperties, queueFamilyCount) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("OutOfMemory\n", .{});
            return RHIType.ErrorCode.outOfMemory;
        },
    };
    defer allocator.free(queueFamilies);

    var i: u32 = 0;
    var presentHasValue = false;
    var graphicsHasValue = false;
    for (queueFamilies) |qf| {
        if (qf.queue_flags.graphics_bit == true) {
            indices.graphicsFamily = i;
            graphicsHasValue = true;
        }
        var presentSupport: u32 = self.context.vki.getPhysicalDeviceSurfaceSupportKHR(self.context.pdev, i, self.surface) catch |err| switch (err) {
            error.OutOfHostMemory => {
                std.debug.print("OutOfHostMemory\n", .{});
                return RHIType.ErrorCode.outOfHostMemory;
            },
            error.OutOfDeviceMemory => {
                std.debug.print("OutOfDeviceMemory\n", .{});
                return RHIType.ErrorCode.outOfDeviceMemory;
            },
            error.SurfaceLostKHR => {
                std.debug.print("SurfaceLostKHR\n", .{});
                return RHIType.ErrorCode.surfaceLost;
            },
            error.Unknown => {
                std.debug.print("Unknown\n", .{});
                return RHIType.ErrorCode.unknown;
            },
        };

        if (presentSupport == 0) {
            indices.presentFamily = i;
            presentHasValue = true;
        }
        if (graphicsHasValue and presentHasValue) {
            break;
        }
        i += 1;
    }
    return indices;
}

pub fn createImageViews(self: *VulkanRenderer, allocator: Allocator) RHIType.ErrorCode!void {
    _ = allocator;
    self.swapChainImageViews.clearAndFree();
    self.swapChainImageViews.ensureTotalCapacity(self.swapChainImages.items.len) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("OutOfMemory\n", .{});
            return RHIType.ErrorCode.outOfMemory;
        },
    };

    for (self.swapChainImages.items) |img| {
        const createInfo = vk.ImageViewCreateInfo{
            .image = img,
            .view_type = .@"2d",
            .format = self.swapChainImageFormat,
            .components = .{
                .r = .identity,
                .g = .identity,
                .b = .identity,
                .a = .identity,
            },
            .subresource_range = .{
                .aspect_mask = .{
                    .color_bit = true,
                },
                .base_mip_level = 0,
                .level_count = 1,
                .base_array_layer = 0,
                .layer_count = 1,
            },
        };
        const temp = self.context.vkd.createImageView(
            self.context.dev,
            &createInfo,
            null,
        ) catch |err| switch (err) {
            error.OutOfHostMemory => {
                std.debug.print("OutOfHostMemory\n", .{});
                return RHIType.ErrorCode.outOfHostMemory;
            },
            error.OutOfDeviceMemory => {
                std.debug.print("OutOfDeviceMemory\n", .{});
                return RHIType.ErrorCode.outOfDeviceMemory;
            },
            error.InvalidOpaqueCaptureAddressKHR => {
                std.debug.print("InvalidOpaqueCaptureAddressKHR\n", .{});
                return RHIType.ErrorCode.invalidOpaqueCaptureAddress;
            },
            error.Unknown => {
                std.debug.print("Unknown\n", .{});
                return RHIType.ErrorCode.unknown;
            },
        };
        self.swapChainImageViews.append(temp) catch |err| switch (err) {
            error.OutOfMemory => {
                std.debug.print("OutOfMemory\n", .{});
                return RHIType.ErrorCode.outOfMemory;
            },
        };
    }
}

pub fn createSwapChain(
    self: *VulkanRenderer,
    allocator: Allocator,
) RHIType.ErrorCode!void {
    const swapChainSupport = try self.querySwapChainSupport(allocator);

    const surfaceFormat: vk.SurfaceFormatKHR = self.chooseSwapSurfaceFormat(swapChainSupport.formats);
    const presentMode: vk.PresentModeKHR = self.chooseSwapPresentMode(swapChainSupport.presentModes);
    const extent = self.chooseSwapExtent(swapChainSupport.capabilities);
    var imageCount: u32 = swapChainSupport.capabilities.min_image_count + 1;
    if (swapChainSupport.capabilities.max_image_count > 0 and
        imageCount > swapChainSupport.capabilities.max_image_count)
    {
        imageCount = swapChainSupport.capabilities.max_image_count;
    }

    var indices = try self.findQueueFamilies(allocator);
    const queueFamilyIndices = [_]u32{
        indices.graphicsFamily,
        indices.presentFamily,
    };

    self.present_queue_family_index = indices.presentFamily;

    var image_sharing_mode: vk.SharingMode = if (indices.graphicsFamily != indices.presentFamily)
        .concurrent
    else
        .exclusive;

    var queue_family_index_count: u32 = if (indices.graphicsFamily != indices.presentFamily)
        2
    else
        0;

    self.swapchain = self.context.vkd.createSwapchainKHR(
        self.context.dev,
        &.{
            .surface = self.surface,
            .min_image_count = imageCount,
            .image_format = surfaceFormat.format,
            .image_color_space = surfaceFormat.color_space,
            .image_extent = extent,
            .image_array_layers = 1,
            .image_usage = .{
                .color_attachment_bit = true,
            },
            .image_sharing_mode = image_sharing_mode,
            .queue_family_index_count = queue_family_index_count,
            .p_queue_family_indices = &queueFamilyIndices,
            .composite_alpha = .{
                .opaque_bit_khr = true,
            },
            .pre_transform = swapChainSupport.capabilities.current_transform,
            .present_mode = presentMode,
            .clipped = vk.TRUE,
        },
        null,
    ) catch |err| switch (err) {
        error.NativeWindowInUseKHR => {
            std.debug.print("NativeWindowInUseKHR\n", .{});
            return RHIType.ErrorCode.nativeWindowInUse;
        },
        error.InitializationFailed => {
            std.debug.print("InitializationFailed\n", .{});
            return RHIType.ErrorCode.initializationFailed;
        },
        error.DeviceLost => {
            std.debug.print("DeviceLost\n", .{});
            return RHIType.ErrorCode.deviceLost;
        },
        error.CompressionExhaustedEXT => {
            std.debug.print("CompressionExhaustedEXT\n", .{});
            return RHIType.ErrorCode.compressionExhausted;
        },
        error.OutOfDeviceMemory => {
            std.debug.print("OutOfDeviceMemory\n", .{});
            return RHIType.ErrorCode.outOfDeviceMemory;
        },
        error.OutOfHostMemory => {
            std.debug.print("OutOfHostMemory\n", .{});
            return RHIType.ErrorCode.outOfHostMemory;
        },
        error.SurfaceLostKHR => {
            std.debug.print("SurfaceLostKHR\n", .{});
            return RHIType.ErrorCode.surfaceLost;
        },
        error.Unknown => {
            std.debug.print("Unknown\n", .{});
            return RHIType.ErrorCode.unknown;
        },
    };

    _ = self.context.vkd.getSwapchainImagesKHR(
        self.context.dev,
        self.swapchain,
        &imageCount,
        null,
    ) catch |err| switch (err) {
        error.OutOfDeviceMemory => {
            std.debug.print("OutOfDeviceMemory\n", .{});
            return RHIType.ErrorCode.outOfDeviceMemory;
        },
        error.OutOfHostMemory => {
            std.debug.print("OutOfHostMemory\n", .{});
            return RHIType.ErrorCode.outOfHostMemory;
        },
        else => {
            std.debug.print("Unknown\n", .{});
            return RHIType.ErrorCode.unknown;
        },
    };
    self.swapChainImages.clearAndFree();
    self.swapChainImages.ensureTotalCapacity(imageCount) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("OutOfMemory\n", .{});
            return RHIType.ErrorCode.outOfMemory;
        },
    };
    const images = allocator.alloc(vk.Image, imageCount) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("OutOfMemory\n", .{});
            return RHIType.ErrorCode.outOfMemory;
        },
    };

    defer allocator.free(images);
    _ = self.context.vkd.getSwapchainImagesKHR(
        self.context.dev,
        self.swapchain,
        &imageCount,
        images.ptr,
    ) catch |err| switch (err) {
        error.OutOfDeviceMemory => {
            std.debug.print("OutOfDeviceMemory\n", .{});
            return RHIType.ErrorCode.outOfDeviceMemory;
        },
        error.OutOfHostMemory => {
            std.debug.print("OutOfHostMemory\n", .{});
            return RHIType.ErrorCode.outOfHostMemory;
        },
        else => {
            std.debug.print("Unknown\n", .{});
            return RHIType.ErrorCode.unknown;
        },
    };

    self.swapChainImages.appendSlice(images) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("OutOfMemory\n", .{});
            return RHIType.ErrorCode.outOfMemory;
        },
    };
    self.swapChainImageFormat = surfaceFormat.format;
    self.swapChainExtent = extent;
    allocator.free(swapChainSupport.formats);
    allocator.free(swapChainSupport.presentModes);
}

pub fn createCommandPool(
    self: *VulkanRenderer,
    queue: RHIType.Queue,
    transient: bool,
) anyerror!RHIType.CommandPool {
    _ = transient;
    assert(self.context.dev != vk.Device.null_handle);
    assert(queue.queue.vulkan.queue != vk.Queue.null_handle);

    const p = try self.context.vkd.createCommandPool(self.context.dev, &.{
        .flags = .{
            //.transient_bit = transient,
            .reset_command_buffer_bit = true,
        },
        .queue_family_index = queue.queue.vulkan.queueFamilyIndex,
    }, null);

    return RHIType.CommandPool{
        .commandPool = RHIType.PoolType{
            .vulkan = RHIInternalType.VulkanCommandPool{
                .commandPool = p,
            },
        },
        .impl = .vulkan,
    };
}

pub fn destroyCommandPool(
    self: *VulkanRenderer,
    pool: RHIType.CommandPool,
) void {
    assert(self.context.dev != vk.Device.null_handle);

    self.context.vkd.destroyCommandPool(
        self.context.dev,
        pool.commandPool.vulkan.commandPool,
        null,
    );
}

pub fn createCommandBuffer(
    self: *VulkanRenderer,
    allocator: Allocator,
    pool: RHIType.CommandPool,
    bIsSecondary: bool,
    count: u32,
) anyerror![]RHIType.CommandBuffer {
    assert(self.context.dev != vk.Device.null_handle);

    const cmdbufs = try allocator.alloc(vk.CommandBuffer, count);
    defer allocator.free(cmdbufs);

    const c = try allocator.alloc(RHIType.CommandBuffer, count);
    errdefer allocator.free(c);

    var level: vk.CommandBufferLevel = undefined;
    if (bIsSecondary) {
        level = .secondary;
    } else {
        level = .primary;
    }

    try self.context.vkd.allocateCommandBuffers(
        self.context.dev,
        &.{
            .command_pool = pool.commandPool.vulkan.commandPool,
            .level = level,
            .command_buffer_count = count,
        },
        cmdbufs.ptr,
    );

    for (cmdbufs, 0..) |cmdbuf, i| {
        c[i].commandBuffer = RHIType.CommandBufferType{
            .vulkan = RHIInternalType.VulkanCommandBuffer{
                .commandBuffer = cmdbuf,
            },
        };
        c[i].impl = .vulkan;
    }
    return c[0..count];
}

pub fn destroyCommandBuffers(
    self: *VulkanRenderer,
    allocator: Allocator,
    pool: RHIType.CommandPool,
    count: u32,
    commandBuffers: []RHIType.CommandBuffer,
) void {
    assert(self.context.dev != vk.Device.null_handle);

    for (0..count) |i| {
        self.context.vkd.freeCommandBuffers(
            self.context.dev,
            pool.commandPool.vulkan.commandPool,
            1,
            @ptrCast(
                [*]const vk.CommandBuffer,
                &commandBuffers[i].commandBuffer.vulkan.commandBuffer,
            ),
        );
    }
    allocator.free(commandBuffers);
}

pub fn beginCommandBuffer(
    self: *VulkanRenderer,
    commandBuffer: RHIType.CommandBuffer,
) !void {
    assert(commandBuffer.commandBuffer.vulkan.commandBuffer != vk.CommandBuffer.null_handle);

    try self.context.vkd.beginCommandBuffer(commandBuffer.commandBuffer.vulkan.commandBuffer, &.{
        .flags = .{ .simultaneous_use_bit = false },
        .p_inheritance_info = null,
    });
}

pub fn endCommandBuffer(
    self: *VulkanRenderer,
    commandBuffer: RHIType.CommandBuffer,
) !void {
    assert(commandBuffer.commandBuffer.vulkan.commandBuffer != vk.CommandBuffer.null_handle);

    try self.context.vkd.endCommandBuffer(commandBuffer.commandBuffer.vulkan.commandBuffer);
}

fn roundUp(value: u64, multiple: u64) u64 {
    return ((value + multiple - 1) / multiple) * multiple;
}

pub fn createBuffer(
    self: *VulkanRenderer,
    allocator: Allocator,
    usage: RHIType.BufferUsage,
    size: u64,
    hostVisible: bool,
) anyerror!*RHIType.Buffer {
    assert(self.context.dev != vk.Device.null_handle);
    var newSize = size;
    if (usage.uniformCbv) {
        newSize = roundUp(
            @max(size, 256),
            self.context.props.limits.min_uniform_buffer_offset_alignment,
        );
    }

    const buffer = try allocator.create(RHIType.Buffer);
    errdefer allocator.destroy(buffer);
    buffer.usage = usage;
    buffer.hostVisible = hostVisible;
    buffer.size = newSize;
    buffer.impl = .vulkan;

    const createInfo = vk.BufferCreateInfo{
        .s_type = .buffer_create_info,
        .p_next = null,
        .flags = .{},
        .size = newSize,
        .usage = .{
            .vertex_buffer_bit = usage.vertex,
            .index_buffer_bit = usage.index,
            .indirect_buffer_bit = usage.indirect,
            .transfer_src_bit = true,
            .transfer_dst_bit = true,
            .uniform_buffer_bit = usage.uniformCbv,
            .storage_buffer_bit = usage.storageSrv or usage.storageUav,
            .uniform_texel_buffer_bit = usage.uniformTexelSrv,
            .storage_texel_buffer_bit = usage.storageTexelUav,
        },
        .sharing_mode = .exclusive,
        .queue_family_index_count = 0,
        .p_queue_family_indices = null,
    };
    const b = try self.context.vkd.createBuffer(
        self.context.dev,
        &createInfo,
        null,
    );
    const memReqs = self.context.vkd.getBufferMemoryRequirements(
        self.context.dev,
        b,
    );
    var flags = vk.MemoryPropertyFlags{
        .device_local_bit = true,
        .host_visible_bit = hostVisible,
        .host_coherent_bit = hostVisible,
    };

    const memoryTypeIndex = try self.context.findMemoryTypeIndex(
        memReqs.memory_type_bits,
        flags,
    );

    const allocateInfo = vk.MemoryAllocateInfo{
        .allocation_size = memReqs.size,
        .memory_type_index = memoryTypeIndex,
    };
    const memory = try self.context.vkd.allocateMemory(
        self.context.dev,
        &allocateInfo,
        null,
    );
    try self.context.vkd.bindBufferMemory(
        self.context.dev,
        b,
        memory,
        0,
    );
    //TODO: refactor this to be callable on demand
    if (hostVisible) {
        const data = try self.context.vkd.mapMemory(
            self.context.dev,
            memory,
            0,
            vk.WHOLE_SIZE,
            .{},
        );
        buffer.cpuMappedAddress = @ptrCast(
            [*]u8,
            @alignCast(
                @alignOf(u8),
                data,
            ),
        );
    }
    buffer.buffer = RHIType.BufferType{
        .vulkan = RHIInternalType.VulkanBuffer{
            .buffer = b,
            .memory = memory,
        },
    };
    return buffer;
}

pub fn destroyBuffer(
    self: *VulkanRenderer,
    allocator: Allocator,
    buffer: *RHIType.Buffer,
) void {
    defer allocator.destroy(buffer);
    defer self.context.vkd.destroyBuffer(
        self.context.dev,
        buffer.buffer.vulkan.buffer,
        null,
    );
    defer self.context.vkd.freeMemory(
        self.context.dev,
        buffer.buffer.vulkan.memory,
        null,
    );
    defer self.context.vkd.unmapMemory(
        self.context.dev,
        buffer.buffer.vulkan.memory,
    );
}
