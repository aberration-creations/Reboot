const vk = @import("vk.zig");

pub const SwapChainSupportDetails = struct {
    capabilities: vk.SurfaceCapabilitiesKHR,
    formats: []vk.SurfaceFormatKHR,
    presentModes: []vk.PresentModeKHR,
};

pub const QueueFamilyIndices = struct {
    graphicsFamily: u32,
    presentFamily: u32,
};

pub const VulkanQueue = struct {
    queue: vk.Queue,
    queueFamilyIndex: u32,
};

pub const VulkanBuffer = struct {
    buffer: vk.Buffer,
    memory: vk.DeviceMemory,
    //commented for future use
    //bufferInfo: vk.DescriptorBufferInfo,
    //bufferView: vk.BufferView,
};

pub const VulkanCommandPool = struct {
    commandPool: vk.CommandPool,
};

pub const VulkanCommandBuffer = struct {
    commandBuffer: vk.CommandBuffer,
};
