// Copyright 2024 Mitchell Kember. Subject to the MIT License.

//! This Lua C library implements a client for a simple protocol called NTSP
//! (Null-Terminated Socket Protocol). It uses SOCK_STREAM Unix domain sockets
//! with null-terminated requests and responses.
//!
//! Example usage from Lua:
//!
//!     ntsp = require("ntsp") -- loads ntsp.so from LUA_CPATH
//!     socket = ntsp.connect("foo.sock")
//!     print(socket:send("hi!")) -- prints the server's response, e.g. "hey!"
//!     socket:close()
//!
//! You can try out the same thing interactively using netcat:
//!
//!     $ nc -U foo.sock
//!     hi!^@hey!
//!
//! Here, the "^@" indicates typing CTRL-V CTRL-@ to produce a null character,
//! then CTRL-D to flush. The server's response gets printed right after.

const std = @import("std");

const c = @cImport({
    @cInclude("lua5.4/lauxlib.h");
    @cInclude("lua5.4/lua.h");
});

const library = [_]c.luaL_Reg{
    .{ .name = "connect", .func = l_connect },
    .{ .name = null, .func = null },
};

export fn luaopen_ntsp(L: *c.lua_State) c_int {
    // Would use the luaL_newlib macro but @cImport doesn't understand it.
    c.luaL_checkversion(L);
    c.lua_createtable(L, 0, library.len - 1);
    c.luaL_setfuncs(L, &library, 0);
    return 1;
}

// Opens a connection to the Unix domain socket at the given path. On success,
// returns a table with fields "fd" (the file descriptor), "send" (the method
// `l_send`), and "close" (the method `l_close`). On error, returns nil.
fn l_connect(L: ?*c.lua_State) callconv(.C) c_int {
    const path = std.mem.span(c.luaL_checklstring(L, 1, null));
    const stream = std.net.connectUnixSocket(path) catch |err|
        return fail(L, "connecting to socket", err);
    c.lua_createtable(L, 0, 3);
    c.lua_pushinteger(L, stream.handle);
    c.lua_setfield(L, -2, "fd");
    c.lua_pushcfunction(L, l_close);
    c.lua_setfield(L, -2, "close");
    c.lua_pushcfunction(L, l_send);
    c.lua_setfield(L, -2, "send");
    return 1;
}

// Method on the table returned by `l_connect`. Closes the socket, returning
// true on success and false on failure.
fn l_close(L: ?*c.lua_State) callconv(.C) c_int {
    c.luaL_checktype(L, 1, c.LUA_TTABLE);
    _ = c.lua_getfield(L, 1, "fd");
    const fd = c.luaL_checkinteger(L, -1);
    const stream = std.net.Stream{ .handle = @intCast(fd) };
    stream.close();
    c.lua_pushboolean(L, 1);
    return 1;
}

var response_buf: [100 * 1024]u8 = undefined;

// Method on the table returned by `l_connect`. Sends the string argument on the
// socket and returns the response, or nil on failure.
fn l_send(L: ?*c.lua_State) callconv(.C) c_int {
    c.luaL_checktype(L, 1, c.LUA_TTABLE);
    _ = c.lua_getfield(L, 1, "fd");
    const fd = c.luaL_checkinteger(L, -1);
    const stream = std.net.Stream{ .handle = @intCast(fd) };
    var request_len: usize = undefined;
    const request = c.lua_tolstring(L, 2, &request_len);
    std.debug.assert(request[request_len] == 0);
    stream.writeAll(request[0 .. request_len + 1]) catch |err|
        return fail(L, "writing to socket", err);
    var response = std.io.fixedBufferStream(&response_buf);
    stream.reader().streamUntilDelimiter(response.writer(), 0, response_buf.len - 1) catch |err|
        return fail(L, "reading from socket", err);
    response_buf[response.pos] = 0;
    _ = c.lua_pushstring(L, &response_buf);
    return 1;
}

fn fail(L: ?*c.lua_State, msg: []const u8, err: anytype) c_int {
    std.debug.print("ntsp.zig: {s}: {s}\n", .{ msg, @errorName(err) });
    c.lua_pushnil(L);
    return 1;
}
