const std = @import("std");
const glfw = @import("libs/mach-glfw/build.zig");
const vkgen = @import("libs/vulkan-zig/generator/index.zig");
const zaudio = @import("libs/zig-gamedev/libs/zaudio/build.zig");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const app = b.addExecutable(.{
        .name = "Harvester",
        .root_source_file = .{ .path = thisDir() ++ "/src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    app.install();

    // zaudio
    const zaudio_pkg = zaudio.Package.build(b, target, optimize, .{});
    app.addModule("zaudio", zaudio_pkg.zaudio);
    zaudio_pkg.link(app);

    // mach-glfw
    app.addModule("glfw", glfw.module(b));
    try glfw.link(b, app, .{});

    const gen = vkgen.VkGenerateStep.create(b, thisDir() ++ "/src/vk.xml");
    app.addModule("vulkan", gen.getModule());

    const shaders = vkgen.ShaderCompileStep.create(
        b,
        &[_][]const u8{ "glslc", "--target-env=vulkan1.2" },
        "-o",
    );
    shaders.add("triangle_vert", "src/shaders/triangle.vert", .{});
    shaders.add("triangle_frag", "src/shaders/triangle.frag", .{});
    app.addModule("resources", shaders.getModule());

    const run_cmd = app.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    var tests = b.addTest(.{ .root_source_file = .{ .path = "test.zig" }, .optimize = optimize });
    //tests.setBuildMode(mode);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&tests.step);
}

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}
