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

fn l_connect(lua: ?*c.lua_State) callconv(.C) c_int {
    const path = c.luaL_checklstring(lua, 1, null);
    _ = path;
    // const fd = std.posix.socket(AF_UNIX, SOCK_STREAM, 0);
    // struct sockaddr_un addr;
    // memset(&addr, 0, sizeof addr);
    // addr.sun_family = AF_UNIX;
    // strcpy(addr.sun_path, path);
    // if (connect(fd, (const struct sockaddr *)&addr, sizeof addr) != 0) {
    //     perror(NULL);
    //     lua_pushnil(L);
    //     return 1;
    // }
    // lua_createtable(L, 0, 3);
    // lua_pushinteger(L, fd);
    // lua_setfield(L, -2, "fd");
    // lua_pushcfunction(L, l_close);
    // lua_setfield(L, -2, "close");
    // lua_pushcfunction(L, l_send);
    // lua_setfield(L, -2, "send");
    return 1;
}

const library = [_]c.luaL_Reg{
    .{ .name = "connect", .func = l_connect },
    .{ .name = null, .func = null },
};

export fn luaopen_ntsp(lua: *c.lua_State) c_int {
    c.luaL_checkversion(lua);
    c.lua_createtable(lua, 0, library.len - 1);
    c.luaL_setfuncs(lua, &library, 0);
    return 1;
}
