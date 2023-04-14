const std = @import("std");
const vk = @import("vulkan/structures.zig");

pub const Api = enum { vulkan, none };

pub const ErrorCode = error{
    failedToCreateSurface,
    failedToCreateSwapchain,
    outOfDeviceMemory,
    outOfHostMemory,
    outOfMemory,
    surfaceLost,
    invalidOpaqueCaptureAddress,
    nativeWindowInUse,
    initializationFailed,
    deviceLost,
    compressionExhausted,
    unknown,
};

pub const Extent2D = struct {
    width: u32,
    height: u32,
};

pub const Offset2D = struct {
    x: u32,
    y: u32,
};

pub const Rect2D = extern struct {
    offset: Offset2D,
    extent: Extent2D,
};

pub const Viewport = struct {
    rect: Rect2D,
    min_depth: f32,
    max_depth: f32,
};

pub const BufferUsage = packed struct(u32) {
    index: bool = false,
    vertex: bool = false,
    indirect: bool = false,
    transferSrc: bool = false,
    transferDst: bool = false,
    uniformCbv: bool = false,
    storageSrv: bool = false,
    storageUav: bool = false,
    uniformTexelSrv: bool = false,
    storageTexelUav: bool = false,
    unused: u22 = 0,

    comptime {
        std.debug.assert(@sizeOf(@This()) == @sizeOf(u32));
        std.debug.assert(@bitSizeOf(@This()) == @bitSizeOf(u32));
    }
};

pub const Format = enum {
    undefined,
    // 1 channel
    r8Unorm,
    r16Unorm,
    r16Uint,
    r16Float,
    r32Uint,
    r32Float,
    // 2 channel
    r8g8Unorm,
    r16g16Unorm,
    r16g16Float,
    r32g32Uint,
    r32g32Float,
    // 3 channel
    r8g8b8Unorm,
    r16g16b16Unorm,
    r16g16b16Float,
    r32g32b32Uint,
    r32g32b32Float,
    // 4 channel
    b8g8r8a8Unorm,
    r8g8b8a8Unorm,
    r16g16b16a16Unorm,
    r16g16b16a16Float,
    r32g32b32a32Uint,
    r32g32b32a32Float,
    // Depth/stencil
    d16Unorm,
    x8_d24UnormPack32,
    d32Float,
    s8Uint,
    d16UnormS8Uint,
    d24UnormS8Uint,
    d32FloatS8Uint,
};

pub const TextureUsage = enum {
    transferSrc,
    transferDst,
    sampledImage,
    storageImage,
    colorAttachment,
    depthStencilAttachment,
    resolveSrc,
    resolveDst,
    present,
};

pub const BufferType = union {
    vulkan: vk.VulkanBuffer,
    none: noreturn,
};

pub const Buffer = struct {
    usage: BufferUsage,
    size: u64,
    hostVisible: bool,
    vertexStride: u32,
    format: Format,
    firstElement: u64,
    elementCount: u64,
    structStride: u64,
    raw: bool,
    cpuMappedAddress: [*]u8,
    buffer: BufferType,
    impl: Api,
};

pub const QueueFamily = enum {
    graphics,
    copy,
    compute,
    present,
};

pub const QueueType = union {
    vulkan: vk.VulkanQueue,
    none: noreturn,
};

pub const Queue = struct {
    queue: QueueType,
    impl: Api,
};

pub const PoolType = union {
    vulkan: vk.VulkanCommandPool,
    none: noreturn,
};

pub const CommandPool = struct {
    commandPool: PoolType,
    impl: Api,
};

pub const CommandBufferType = union {
    vulkan: vk.VulkanCommandBuffer,
    none: noreturn,
};

pub const CommandBuffer = struct {
    commandBuffer: CommandBufferType,
    impl: Api,
};
