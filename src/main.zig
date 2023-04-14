const std = @import("std");
const vk = @import("rhi/vulkan/vk.zig");
const RHI = @import("rhi/rhi.zig").RHI;
const glfw = @import("glfw");
const zaudio = @import("zaudio");
const resources = @import("resources");
const GraphicsContext = @import("rhi/vulkan/graphics_context.zig").GraphicsContext;

const RHIType = @import("rhi/rhi_types.zig");

const Allocator = std.mem.Allocator;

const app_name = "Reboot";

const isRelease = false;
const isMusic = true;

const Vertex = struct {
    const binding_description = vk.VertexInputBindingDescription{
        .binding = 0,
        .stride = @sizeOf(Vertex),
        .input_rate = .vertex,
    };

    const attribute_description = [_]vk.VertexInputAttributeDescription{
        .{
            .binding = 0,
            .location = 0,
            .format = .r32g32_sfloat,
            .offset = @offsetOf(Vertex, "pos"),
        },
        .{
            .binding = 0,
            .location = 1,
            .format = .r32g32b32_sfloat,
            .offset = @offsetOf(Vertex, "color"),
        },
    };

    pos: [2]f32,
    color: [3]f32,
};

const vertices = [_]Vertex{
    .{
        .pos = .{ -0.5, 0.9 },
        .color = .{ 1, 0, 0 },
    },
    .{
        .pos = .{ 0.5, -0.5 },
        .color = .{ 0, 1, 0 },
    },
    .{
        .pos = .{ -0.5, -0.5 },
        .color = .{ 0, 0, 1 },
    },
    .{
        .pos = .{ -0.5, 0.9 },
        .color = .{ 1, 0, 0 },
    },
    .{
        .pos = .{ 0.5, 0.9 },
        .color = .{ 0, 0, 1 },
    },
    .{
        .pos = .{ 0.5, -0.5 },
        .color = .{ 0, 1, 0 },
    },
};

const FrameData = packed struct {
    width: u32,
    height: u32,
    time: f32,
};

var frameData = FrameData{
    .time = 0,
    .width = 0,
    .height = 0,
};

const maxFramesInFlight = 8;
var framesInFlight: u32 = 0;
var currentFrame: u32 = 0;
var didResize = false;

var imageAvailableSemaphores: [maxFramesInFlight]vk.Semaphore = undefined;
var renderFinishedSemaphores: [maxFramesInFlight]vk.Semaphore = undefined;
var inFlightFences: [maxFramesInFlight]vk.Fence = undefined;

// for timing
var prevTimerNano: i128 = 0;
var prevMusicCursor: f32 = 0;
var currentTimeNano: i128 = 0;
var currentTimeSeconds: f64 = 0;

// music
var audioEngine: *zaudio.Engine = undefined;
var music: *zaudio.Sound = undefined;

fn seekAudio(delta: i64) void {
    const scaler = 44100;
    if (!isMusic or isRelease) {
        return;
    }
    var frame = @intCast(i64, music.getTimeInPcmFrames());
    frame += delta * scaler;
    if (frame < 0) {
        frame = 0;
    }
    music.seekToPcmFrame(@intCast(u64, frame)) catch {
        std.log.debug("seekToPcmFrame failed", .{});
    };
}

fn resizeCallback(window: glfw.Window, width: u32, height: u32) void {
    _ = height;
    _ = width;
    _ = window;
    didResize = true;
}

/// Default GLFW error handling callback
fn errorCallback(error_code: glfw.ErrorCode, description: [:0]const u8) void {
    std.log.err("glfw: {}: {s}\n", .{ error_code, description });
}

pub fn main() !void {
    //const allocator = std.heap.page_allocator;
    var gpa = std.heap.GeneralPurposeAllocator(.{
        //.safety = true,
    }){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked) @panic("TEST FAIL"); //fail test; can't try in defer as defer is executed after we return
    }
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    var cmdline = std.ArrayList(u32).init(allocator);
    defer cmdline.deinit();
    _ = args.skip();
    while (args.next()) |arg| {
        const num = try std.fmt.parseUnsigned(u32, arg, 10);
        try cmdline.append(num);
    }
    glfw.setErrorCallback(errorCallback);
    if (!glfw.init(.{})) {
        std.log.err("failed to initialize GLFW: {?s}", .{glfw.getErrorString()});
        std.process.exit(1);
    }
    defer glfw.terminate();

    const monitor = glfw.Monitor.getPrimary();
    var ex: RHIType.Extent2D = undefined;
    var window: glfw.Window = undefined;
    if (monitor) |m| {
        //const workArea = m.getWorkarea();
        ex = RHIType.Extent2D{
            .width = if (cmdline.items.len != 2) 1920 else cmdline.items[0],
            .height = if (cmdline.items.len != 2) 1080 else cmdline.items[1],
        };
        window = glfw.Window.create(
            ex.width,
            ex.height,
            app_name,
            if (isRelease) m else null,
            null,
            .{
                .client_api = .no_api,
                .decorated = !isRelease,
                .maximized = false,
                .srgb_capable = true,
            },
        ) orelse {
            std.log.err("failed to create GLFW window: {?s}", .{glfw.getErrorString()});
            std.process.exit(1);
        };
        window.setKeyCallback((struct {
            fn callback(_window: glfw.Window, key: glfw.Key, scancode: i32, action: glfw.Action, mods: glfw.Mods) void {
                _ = mods;
                _ = scancode;
                if (key == .escape and action == .press) {
                    std.debug.print("Quitting\n", .{});
                    _window.setShouldClose(true);
                } else if (key == .left and action == .press) {
                    //left
                    seekAudio(-1);
                } else if (key == .right and action == .press) {
                    //right
                    seekAudio(1);
                } else if (key == .up and action == .press) {
                    //up
                    seekAudio(-10);
                } else if (key == .down and action == .press) {
                    //down
                    seekAudio(10);
                } else if (key == .space and action == .press) {
                    //space
                }
            }
        }).callback);
    }

    defer window.destroy();
    if (isRelease) {
        window.setInputMode(.cursor, .hidden);
    }
    defer if (isRelease) {
        window.setInputMode(.cursor, .normal);
    };
    window.setFramebufferSizeCallback(resizeCallback);

    if (isMusic) {
        zaudio.init(allocator);
        audioEngine = try zaudio.Engine.create(null);
        music = try audioEngine.createSoundFromFile(
            "assets/audio.mp3",
            .{
                .flags = .{ .stream = true },
            },
        );
        music.setLooping(false);
    }
    defer if (isMusic) {
        music.destroy();
        audioEngine.destroy();
        zaudio.deinit();
    };

    var rhi = try RHI.initVulkan(allocator);
    defer rhi.deinitVulkan(allocator);
    defer rhi.deinit();

    try rhi.createContext(allocator, app_name, window);
    try rhi.createSwapChain(allocator);
    try rhi.createImageViews(allocator);

    framesInFlight = @intCast(u32, rhi.vulkan.swapChainImages.items.len);
    if (framesInFlight > maxFramesInFlight) {
        std.log.err("too many framesInFlight {} max supported {}", .{ framesInFlight, maxFramesInFlight });
    }

    try createSyncObjects(rhi);
    defer destroySyncObjects(rhi);

    const graphicsQueue = rhi.getQueue(.graphics);
    const presentQueue = rhi.getQueue(.present);

    const uboLayoutBinding = vk.DescriptorSetLayoutBinding{
        .binding = 0,
        .descriptor_type = .uniform_buffer,
        .descriptor_count = 1,
        .stage_flags = .{
            .vertex_bit = true,
            .fragment_bit = true,
        },
        .p_immutable_samplers = null,
    };

    const layoutInfo = vk.DescriptorSetLayoutCreateInfo{
        .binding_count = 1,
        .p_bindings = @ptrCast([*]const vk.DescriptorSetLayoutBinding, &uboLayoutBinding),
    };

    const dsl = try rhi.vulkan.context.vkd.createDescriptorSetLayout(
        rhi.vulkan.context.dev,
        &layoutInfo,
        null,
    );

    var descriptorSetLayouts: [maxFramesInFlight]vk.DescriptorSetLayout = [_]vk.DescriptorSetLayout{
        dsl, dsl, dsl, dsl,
        dsl, dsl, dsl, dsl,
    };

    defer rhi.vulkan.context.vkd.destroyDescriptorSetLayout(
        rhi.vulkan.context.dev,
        dsl,
        null,
    );

    const pLayout = vk.PipelineLayoutCreateInfo{
        .flags = .{},
        .set_layout_count = framesInFlight,
        .p_set_layouts = @ptrCast([*]const vk.DescriptorSetLayout, &descriptorSetLayouts[0]),
        .push_constant_range_count = 0,
        .p_push_constant_ranges = undefined,
    };
    const pipeline_layout = try rhi.vulkan.context.vkd.createPipelineLayout(
        rhi.vulkan.context.dev,
        &pLayout,
        null,
    );
    defer rhi.vulkan.context.vkd.destroyPipelineLayout(
        rhi.vulkan.context.dev,
        pipeline_layout,
        null,
    );

    var pipeline = try createPipeline(
        &rhi.vulkan.context,
        pipeline_layout,
        rhi,
    );
    defer rhi.vulkan.context.vkd.destroyPipeline(
        rhi.vulkan.context.dev,
        pipeline,
        null,
    );

    const typeCount = vk.DescriptorPoolSize{
        .type = .uniform_buffer,
        .descriptor_count = framesInFlight,
    };

    const descPoolInfo = vk.DescriptorPoolCreateInfo{
        .max_sets = framesInFlight,
        .pool_size_count = 1,
        .p_pool_sizes = @ptrCast([*]const vk.DescriptorPoolSize, &typeCount),
    };

    const descriptorPool = try rhi.vulkan.context.vkd.createDescriptorPool(
        rhi.vulkan.context.dev,
        &descPoolInfo,
        null,
    );
    defer rhi.vulkan.context.vkd.destroyDescriptorPool(
        rhi.vulkan.context.dev,
        descriptorPool,
        null,
    );

    const allocInfo = vk.DescriptorSetAllocateInfo{
        .descriptor_pool = descriptorPool,
        .descriptor_set_count = framesInFlight,
        .p_set_layouts = @ptrCast([*]const vk.DescriptorSetLayout, &descriptorSetLayouts),
    };

    var descriptorSet: [maxFramesInFlight]vk.DescriptorSet = undefined;
    try rhi.vulkan.context.vkd.allocateDescriptorSets(
        rhi.vulkan.context.dev,
        &allocInfo,
        @ptrCast([*]vk.DescriptorSet, &descriptorSet[0]),
    );

    const pool = try rhi.createCommandPool(graphicsQueue, false);

    defer rhi.destroyCommandPool(pool);

    const buffer = try rhi.createBuffer(
        allocator,
        .{ .vertex = true },
        @sizeOf(@TypeOf(vertices)),
        true,
    );
    defer rhi.destroyBuffer(allocator, buffer);
    @memcpy(
        buffer.cpuMappedAddress,
        @ptrCast([*]const u8, &vertices),
        @sizeOf(@TypeOf(vertices)),
    );

    var ubo: [maxFramesInFlight]*RHIType.Buffer = undefined;
    for (0..framesInFlight) |i| {
        ubo[i] = try rhi.createBuffer(
            allocator,
            .{ .uniformCbv = true },
            @sizeOf(@TypeOf(frameData)),
            true,
        );
        const bufferInfo = vk.DescriptorBufferInfo{
            .buffer = ubo[i].buffer.vulkan.buffer,
            .offset = 0,
            .range = @sizeOf(@TypeOf(frameData)),
        };
        const descriptorWrite = vk.WriteDescriptorSet{
            .dst_set = descriptorSet[i],
            .dst_binding = 0,
            .dst_array_element = 0,
            .descriptor_count = 1,
            .descriptor_type = .uniform_buffer,
            .p_buffer_info = @ptrCast([*]const vk.DescriptorBufferInfo, &bufferInfo),
            .p_image_info = undefined,
            .p_texel_buffer_view = undefined,
        };

        rhi.vulkan.context.vkd.updateDescriptorSets(
            rhi.vulkan.context.dev,
            1,
            @ptrCast([*]const vk.WriteDescriptorSet, &descriptorWrite),
            0,
            undefined,
        );
    }

    defer {
        for (0..framesInFlight) |i| {
            rhi.destroyBuffer(allocator, ubo[i]);
        }
    }

    const cmdbufs = try rhi.createCommandBuffers(
        allocator,
        pool,
        false,
        @intCast(u32, rhi.vulkan.swapChainImages.items.len),
    );

    defer rhi.destroyCommandBuffers(
        allocator,
        pool,
        @intCast(u32, cmdbufs.len),
        cmdbufs,
    );
    defer rhi.vulkan.context.vkd.queueWaitIdle(
        graphicsQueue.queue.vulkan.queue,
    ) catch |err| switch (err) {
        else => std.debug.print("Can't wait on queue idle\n", .{}),
    };

    if (isMusic) {
        try music.start();
    }
    if (!isRelease and isMusic) {
        var file = try std.fs.cwd().createFile("musicpos.txt", .{
            .read = true,
            .truncate = false,
        });
        defer file.close();
        var buf_reader = std.io.bufferedReader(file.reader());
        const stat = try file.stat();
        if (stat.size != 0) {
            const pcmFrame = try buf_reader.reader().readIntBig(u64);
            std.debug.print("\npcmframe begin {}\n", .{pcmFrame});
            try music.seekToPcmFrame(pcmFrame);
        } else {
            std.debug.print("\npcmframe begin none\n", .{});
            try music.seekToPcmFrame(0);
        }
    }

    while (!window.shouldClose()) {
        const device = rhi.vulkan.context.dev;
        const context = rhi.vulkan.context;
        const swapchain = rhi.vulkan.swapchain;

        _ = try context.vkd.waitForFences(
            device,
            1,
            @ptrCast([*]const vk.Fence, &inFlightFences[currentFrame]),
            vk.TRUE,
            std.math.maxInt(u64),
        );

        const imgIndex = try context.vkd.acquireNextImageKHR(
            device,
            swapchain,
            std.math.maxInt(u64),
            imageAvailableSemaphores[currentFrame],
            .null_handle,
        );

        if (imgIndex.result == .error_out_of_date_khr) {
            try rhi.recreateSwapchain(allocator);
            continue;
        } else if (imgIndex.result != .success and imgIndex.result != .suboptimal_khr) {
            std.debug.print("Failed to acquire swap chain image!\n", .{});
            std.process.exit(1);
        }

        // delicate timing code!
        if (isMusic) {
            if (isRelease and !music.isPlaying()) {
                // exit point for release
                std.os.exit(0);
            }
            // merges timer with cursor in music time for good precison
            // TODO: account for music buffer length
            // TODO: account for delay caused by frame buffering
            // (with this simple implementation hopefully the audio and frame buffering cancel each other out)
            var nextMusicCursor = try music.getCursorInSeconds();
            var nextTimerNano = std.time.nanoTimestamp();
            if (nextMusicCursor != prevMusicCursor) {
                currentTimeSeconds = @floatCast(f64, nextMusicCursor);
                currentTimeNano = @floatToInt(i128, currentTimeSeconds * 1000_000_000);
            } else {
                currentTimeNano += nextTimerNano - prevTimerNano;
                currentTimeSeconds = @intToFloat(f64, currentTimeNano) / 1000_000_000;
            }
            prevMusicCursor = nextMusicCursor;
            prevTimerNano = nextTimerNano;
        } else {
            // when no music, use std.time
            if (prevTimerNano <= 0) {
                prevTimerNano = std.time.nanoTimestamp();
            }
            var nextTimerNano = std.time.nanoTimestamp();
            currentTimeNano += nextTimerNano - prevTimerNano;
            currentTimeSeconds = @intToFloat(f64, currentTimeNano) / 1000_000_000;
            prevTimerNano = nextTimerNano;
        }

        frameData.time = @floatCast(f32, currentTimeSeconds);
        frameData.width = rhi.vulkan.swapChainExtent.width;
        frameData.height = rhi.vulkan.swapChainExtent.height;

        @memcpy(
            ubo[currentFrame].cpuMappedAddress,
            @ptrCast([*]const u8, &frameData),
            @sizeOf(@TypeOf(frameData)),
        );

        try context.vkd.resetFences(
            device,
            1,
            @ptrCast([*]const vk.Fence, &inFlightFences[currentFrame]),
        );

        try context.vkd.resetCommandBuffer(
            cmdbufs[currentFrame].commandBuffer.vulkan.commandBuffer,
            .{},
        );

        try render(
            &rhi.vulkan.context,
            rhi,
            cmdbufs[currentFrame],
            buffer,
            rhi.vulkan.swapChainExtent,
            pipeline,
            pipeline_layout,
            &descriptorSet,
            presentQueue,
            graphicsQueue,
        );

        if (context.vkd.queueSubmit(
            graphicsQueue.queue.vulkan.queue,
            1,
            &[_]vk.SubmitInfo{.{
                .wait_semaphore_count = 1,
                .p_wait_semaphores = @ptrCast(
                    [*]const vk.Semaphore,
                    &imageAvailableSemaphores[currentFrame],
                ),
                .p_wait_dst_stage_mask = &[_]vk.PipelineStageFlags{.{
                    .color_attachment_output_bit = true,
                }},
                .command_buffer_count = 1,
                .p_command_buffers = @ptrCast(
                    [*]const vk.CommandBuffer,
                    &cmdbufs[currentFrame].commandBuffer.vulkan.commandBuffer,
                ),
                .signal_semaphore_count = 1,
                .p_signal_semaphores = @ptrCast(
                    [*]const vk.Semaphore,
                    &renderFinishedSemaphores[currentFrame],
                ),
            }},
            inFlightFences[currentFrame],
        )) {} else |err| switch (err) {
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
        if (context.vkd.queuePresentKHR(
            graphicsQueue.queue.vulkan.queue,
            &.{
                .wait_semaphore_count = 1,
                .p_wait_semaphores = @ptrCast(
                    [*]const vk.Semaphore,
                    &renderFinishedSemaphores[currentFrame],
                ),
                .swapchain_count = 1,
                .p_swapchains = @ptrCast(
                    [*]const vk.SwapchainKHR,
                    &swapchain,
                ),
                .p_image_indices = @ptrCast(
                    [*]const u32,
                    &imgIndex.image_index,
                ),
            },
        )) |r| {
            if (r != .success) {}
        } else |err| switch (err) {
            error.DeviceLost => {
                std.debug.print("DeviceLost\n", .{});
                std.debug.print("Failed to present image!\n", .{});
                std.process.exit(1);
            },
            error.OutOfDateKHR => {
                std.debug.print("OutOfDateKHR\n", .{});
                try rhi.recreateSwapchain(allocator);
            },
            error.SurfaceLostKHR => {
                std.debug.print("SurfaceLostKHR\n", .{});
                try rhi.recreateSwapchain(allocator);
            },
            error.FullScreenExclusiveModeLostEXT => {
                std.debug.print("FullScreenExclusiveModeLostEXT\n", .{});
            },
            else => {
                std.debug.print("{}\n", .{err});
                std.debug.print("Failed to present image!\n", .{});
                std.process.exit(1);
            },
        }
        if (didResize) {
            std.debug.print("resizing\n", .{});
            try rhi.recreateSwapchain(allocator);
            didResize = false;
        }
        currentFrame = (currentFrame + 1) % framesInFlight;
        glfw.pollEvents();
    }

    if (!isRelease and isMusic) {
        var file = try std.fs.cwd().openFile("musicpos.txt", .{
            .mode = .write_only,
        });
        defer file.close();
        var buf_writer = std.io.bufferedWriter(file.writer());
        std.debug.print("pcmframes end {}\n", .{music.getTimeInPcmFrames()});
        try buf_writer.writer().writeInt(u64, music.getTimeInPcmFrames(), .Big);
        try buf_writer.flush();
    }
}

fn createSyncObjects(
    rhi: RHI,
) !void {
    const fenceInfo = vk.FenceCreateInfo{
        .flags = .{
            .signaled_bit = true,
        },
    };

    errdefer {
        std.debug.print("failed to create synchronization objects for a frame!\n", .{});
    }
    for (0..framesInFlight) |i| {
        imageAvailableSemaphores[i] = try rhi.vulkan.context.vkd.createSemaphore(
            rhi.vulkan.context.dev,
            &.{},
            null,
        );
        renderFinishedSemaphores[i] = try rhi.vulkan.context.vkd.createSemaphore(
            rhi.vulkan.context.dev,
            &.{},
            null,
        );
        inFlightFences[i] = try rhi.vulkan.context.vkd.createFence(
            rhi.vulkan.context.dev,
            &fenceInfo,
            null,
        );
    }
}

fn destroySyncObjects(
    rhi: RHI,
) void {
    for (0..framesInFlight) |i| {
        rhi.vulkan.context.vkd.destroySemaphore(
            rhi.vulkan.context.dev,
            imageAvailableSemaphores[i],
            null,
        );
        rhi.vulkan.context.vkd.destroySemaphore(
            rhi.vulkan.context.dev,
            renderFinishedSemaphores[i],
            null,
        );
        rhi.vulkan.context.vkd.destroyFence(
            rhi.vulkan.context.dev,
            inFlightFences[i],
            null,
        );
    }
}

fn render(
    gc: *const GraphicsContext,
    rhi: RHI,
    cmdbuf: RHIType.CommandBuffer,
    buffer: *RHIType.Buffer,
    extent: vk.Extent2D,
    pipeline: vk.Pipeline,
    pipelineLayout: vk.PipelineLayout,
    descriptorSets: []vk.DescriptorSet,
    presentQueue: RHIType.Queue,
    graphicsQueue: RHIType.Queue,
) !void {
    _ = graphicsQueue;
    _ = presentQueue;
    const clear = vk.ClearValue{
        .color = .{ .float_32 = .{ 0, 0, 0, 0 } },
    };

    const viewport = vk.Viewport{
        .x = 0,
        .y = 0,
        .width = @intToFloat(f32, extent.width),
        .height = @intToFloat(f32, extent.height),
        .min_depth = 0,
        .max_depth = 1,
    };

    const scissor = vk.Rect2D{
        .offset = .{ .x = 0, .y = 0 },
        .extent = extent,
    };

    try rhi.beginCommandBuffer(cmdbuf);

    gc.vkd.cmdSetViewport(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
        0,
        1,
        @ptrCast([*]const vk.Viewport, &viewport),
    );
    gc.vkd.cmdSetScissor(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
        0,
        1,
        @ptrCast([*]const vk.Rect2D, &scissor),
    );

    // This needs to be a separate definition - see https://github.com/ziglang/zig/issues/7627.
    const render_area = vk.Rect2D{
        .offset = .{ .x = 0, .y = 0 },
        .extent = extent,
    };

    const begin_memory_barrier = vk.ImageMemoryBarrier{
        .src_access_mask = .{},
        .dst_access_mask = .{ .color_attachment_write_bit = true },
        .src_queue_family_index = 0,
        .dst_queue_family_index = 0,
        .old_layout = .undefined,
        .new_layout = .color_attachment_optimal,
        .image = rhi.vulkan.swapChainImages.items[currentFrame],
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

    gc.vkd.cmdPipelineBarrier(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
        .{ .top_of_pipe_bit = true },
        .{ .color_attachment_output_bit = true },
        .{},
        0,
        undefined,
        0,
        undefined,
        1,
        @ptrCast([*]const vk.ImageMemoryBarrier, &begin_memory_barrier),
    );

    const color_attachment_info = vk.RenderingAttachmentInfoKHR{
        .image_view = rhi.vulkan.swapChainImageViews.items[currentFrame],
        .image_layout = .color_attachment_optimal,
        .load_op = .clear,
        .store_op = .store,
        .clear_value = clear,
        .resolve_mode = .{},
        .resolve_image_layout = .undefined,
    };

    const render_info = vk.RenderingInfoKHR{
        .render_area = render_area,
        .view_mask = 0,
        .layer_count = 1,
        .color_attachment_count = 1,
        .p_color_attachments = @ptrCast(
            [*]const vk.RenderingAttachmentInfoKHR,
            &color_attachment_info,
        ),
    };
    gc.vkd.cmdBeginRenderingKHR(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
        &render_info,
    );

    gc.vkd.cmdBindPipeline(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
        .graphics,
        pipeline,
    );

    gc.vkd.cmdBindDescriptorSets(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
        .graphics,
        pipelineLayout,
        0,
        1,
        @ptrCast([*]const vk.DescriptorSet, &descriptorSets[currentFrame]),
        0,
        undefined,
    );
    const offset = [_]vk.DeviceSize{0};
    gc.vkd.cmdBindVertexBuffers(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
        0,
        1,
        @ptrCast([*]const vk.Buffer, &buffer.buffer.vulkan.buffer),
        &offset,
    );

    gc.vkd.cmdDraw(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
        vertices.len,
        1,
        0,
        0,
    );

    gc.vkd.cmdEndRenderingKHR(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
    );

    const end_memory_barrier = vk.ImageMemoryBarrier{
        .src_access_mask = .{ .color_attachment_write_bit = true },
        .dst_access_mask = .{},
        .src_queue_family_index = 0,
        .dst_queue_family_index = 0,
        .old_layout = .color_attachment_optimal,
        .new_layout = .present_src_khr,
        .image = rhi.vulkan.swapChainImages.items[currentFrame],
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

    gc.vkd.cmdPipelineBarrier(
        cmdbuf.commandBuffer.vulkan.commandBuffer,
        .{ .color_attachment_output_bit = true },
        .{ .bottom_of_pipe_bit = true },
        .{},
        0,
        undefined,
        0,
        undefined,
        1,
        @ptrCast([*]const vk.ImageMemoryBarrier, &end_memory_barrier),
    );
    try rhi.endCommandBuffer(
        cmdbuf,
    );
}

fn createPipeline(
    gc: *const GraphicsContext,
    layout: vk.PipelineLayout,
    rhi: RHI,
) !vk.Pipeline {
    const vert = try gc.vkd.createShaderModule(
        gc.dev,
        &.{
            .flags = .{},
            .code_size = resources.triangle_vert.len,
            .p_code = @ptrCast([*]const u32, &resources.triangle_vert),
        },
        null,
    );
    defer gc.vkd.destroyShaderModule(
        gc.dev,
        vert,
        null,
    );

    const frag = try gc.vkd.createShaderModule(
        gc.dev,
        &.{
            .flags = .{},
            .code_size = resources.triangle_frag.len,
            .p_code = @ptrCast([*]const u32, &resources.triangle_frag),
        },
        null,
    );
    defer gc.vkd.destroyShaderModule(
        gc.dev,
        frag,
        null,
    );

    const pssci = [_]vk.PipelineShaderStageCreateInfo{
        .{
            .flags = .{},
            .stage = .{ .vertex_bit = true },
            .module = vert,
            .p_name = "main",
            .p_specialization_info = null,
        },
        .{
            .flags = .{},
            .stage = .{ .fragment_bit = true },
            .module = frag,
            .p_name = "main",
            .p_specialization_info = null,
        },
    };

    const pvisci = vk.PipelineVertexInputStateCreateInfo{
        .flags = .{},
        .vertex_binding_description_count = 1,
        .p_vertex_binding_descriptions = @ptrCast(
            [*]const vk.VertexInputBindingDescription,
            &Vertex.binding_description,
        ),
        .vertex_attribute_description_count = Vertex.attribute_description.len,
        .p_vertex_attribute_descriptions = &Vertex.attribute_description,
    };

    const piasci = vk.PipelineInputAssemblyStateCreateInfo{
        .flags = .{},
        .topology = .triangle_list,
        .primitive_restart_enable = vk.FALSE,
    };

    const pvsci = vk.PipelineViewportStateCreateInfo{
        .flags = .{},
        .viewport_count = 1,
        .p_viewports = undefined, // set in createCommandBuffers with cmdSetViewport
        .scissor_count = 1,
        .p_scissors = undefined, // set in createCommandBuffers with cmdSetScissor
    };

    const prsci = vk.PipelineRasterizationStateCreateInfo{
        .flags = .{},
        .depth_clamp_enable = vk.FALSE,
        .rasterizer_discard_enable = vk.FALSE,
        .polygon_mode = .fill,
        .cull_mode = .{ .back_bit = true },
        .front_face = .clockwise,
        .depth_bias_enable = vk.FALSE,
        .depth_bias_constant_factor = 0,
        .depth_bias_clamp = 0,
        .depth_bias_slope_factor = 0,
        .line_width = 1,
    };

    const pmsci = vk.PipelineMultisampleStateCreateInfo{
        .flags = .{},
        .rasterization_samples = .{ .@"1_bit" = true },
        .sample_shading_enable = vk.FALSE,
        .min_sample_shading = 1,
        .p_sample_mask = null,
        .alpha_to_coverage_enable = vk.FALSE,
        .alpha_to_one_enable = vk.FALSE,
    };

    const pcbas = vk.PipelineColorBlendAttachmentState{
        .blend_enable = vk.FALSE,
        .src_color_blend_factor = .one,
        .dst_color_blend_factor = .zero,
        .color_blend_op = .add,
        .src_alpha_blend_factor = .one,
        .dst_alpha_blend_factor = .zero,
        .alpha_blend_op = .add,
        .color_write_mask = .{ .r_bit = true, .g_bit = true, .b_bit = true, .a_bit = true },
    };

    const pcbsci = vk.PipelineColorBlendStateCreateInfo{
        .flags = .{},
        .logic_op_enable = vk.FALSE,
        .logic_op = .copy,
        .attachment_count = 1,
        .p_attachments = @ptrCast([*]const vk.PipelineColorBlendAttachmentState, &pcbas),
        .blend_constants = [_]f32{ 0, 0, 0, 0 },
    };

    const dynstate = [_]vk.DynamicState{ .viewport, .scissor };
    const pdsci = vk.PipelineDynamicStateCreateInfo{
        .flags = .{},
        .dynamic_state_count = dynstate.len,
        .p_dynamic_states = &dynstate,
    };

    const prci = vk.PipelineRenderingCreateInfo{
        .s_type = .pipeline_rendering_create_info,
        .color_attachment_count = 1,
        .view_mask = 0,
        .depth_attachment_format = .undefined,
        .stencil_attachment_format = .undefined,
        .p_color_attachment_formats = @ptrCast(
            [*]const vk.Format,
            &rhi.vulkan.swapChainImageFormat,
        ),
    };
    const gpci = vk.GraphicsPipelineCreateInfo{
        .flags = .{},
        .p_next = &prci,
        .stage_count = 2,
        .p_stages = &pssci,
        .p_vertex_input_state = &pvisci,
        .p_input_assembly_state = &piasci,
        .p_tessellation_state = null,
        .p_viewport_state = &pvsci,
        .p_rasterization_state = &prsci,
        .p_multisample_state = &pmsci,
        .p_depth_stencil_state = null,
        .p_color_blend_state = &pcbsci,
        .p_dynamic_state = &pdsci,
        .layout = layout,
        .subpass = 0,
        .base_pipeline_handle = .null_handle,
        .base_pipeline_index = -1,
    };

    var pipeline: vk.Pipeline = undefined;
    _ = try gc.vkd.createGraphicsPipelines(
        gc.dev,
        .null_handle,
        1,
        @ptrCast([*]const vk.GraphicsPipelineCreateInfo, &gpci),
        null,
        @ptrCast([*]vk.Pipeline, &pipeline),
    );
    return pipeline;
}
