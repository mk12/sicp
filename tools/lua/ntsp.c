// Copyright 2022 Mitchell Kember. Subject to the MIT License.

// This Lua C library implements a client for a simple protocol called NTSP
// (Null-Terminated Socket Protocol). It uses SOCK_STREAM Unix domain sockets
// with null-terminated requests and responses.
//
// Example usage from Lua:
//
//     ntsp = require("ntsp") -- loads ntsp.so from LUA_CPATH
//     socket = ntsp.connect("foo.sock")
//     print(socket:send("hi!")) -- prints the server's response, e.g. "hey!"
//     socket:close()
//
// You can try out the same thing interactively using netcat:
//
//     $ nc -U foo.sock
//     hi!^@hey!
//
// Here, the "^@" indicates typing CTRL-V CTRL-@ to produce a null character,
// then CTRL-D to flush. The server's response gets printed right after.

#include <assert.h>
#include <lua5.4/lauxlib.h>
#include <lua5.4/lua.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

// Method on the table returned by `l_connect`. Closes the socket, returning
// true on success and false on failure.
static int l_close(lua_State *L) {
    luaL_checktype(L, 1, LUA_TTABLE);
    lua_getfield(L, 1, "fd");
    int fd = luaL_checkinteger(L, -1);
    bool ok = close(fd) == 0;
    if (!ok) {
        perror(NULL);
    }
    lua_pushboolean(L, ok);
    return 1;
}

// Method on the table returned by `l_connect`. Sends the string argument on the
// socket and returns the response, or nil on failure.
int l_send(lua_State *L) {
    luaL_checktype(L, 1, LUA_TTABLE);
    lua_getfield(L, 1, "fd");
    int fd = luaL_checkinteger(L, -1);
    size_t request_len;
    const char *request = lua_tolstring(L, 2, &request_len);
    assert(request[request_len] == '\0');
    request_len++;
    size_t sent = 0;
    while (sent < request_len) {
        ssize_t result = write(fd, request + sent, request_len - sent);
        if (result == -1) {
            perror(NULL);
            lua_pushboolean(L, false);
            return 1;
        }
        sent += result;
    }
    size_t buf_len = 1024;
    char *buf = malloc(buf_len);
    size_t received = 0;
    for (;;) {
        ssize_t result = read(fd, buf + received, buf_len - received);
        if (result == -1) {
            free(buf);
            perror(NULL);
            lua_pushboolean(L, false);
            return 1;
        }
        received += result;
        if (buf[received - 1] == '\0') {
            break;
        }
        if (received == buf_len) {
            buf_len *= 2;
            buf = realloc(buf, buf_len);
        }
    }
    lua_pushstring(L, buf);
    free(buf);
    return 1;
}

// Opens a connection to the Unix domain socket at the given path. On success,
// returns a table with fields "fd" (the file descriptor), "send" (the method
// `l_send`), and "close" (the method `l_close`). On error, returns nil.
int l_conect(lua_State *L) {
    const char *path = luaL_checklstring(L, 1, NULL);
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un addr;
    memset(&addr, 0, sizeof addr);
    addr.sun_family = AF_UNIX;
    strcpy(addr.sun_path, path);
    if (connect(fd, (const struct sockaddr *)&addr, sizeof addr) != 0) {
        perror(NULL);
        lua_pushnil(L);
        return 1;
    }
    lua_createtable(L, 0, 3);
    lua_pushinteger(L, fd);
    lua_setfield(L, -2, "fd");
    lua_pushcfunction(L, l_close);
    lua_setfield(L, -2, "close");
    lua_pushcfunction(L, l_send);
    lua_setfield(L, -2, "send");
    return 1;
}

static const luaL_Reg library[] = {{"connect", l_conect}, {NULL, NULL}};

LUALIB_API int luaopen_ntsp(lua_State *L) {
    luaL_newlib(L, library);
    return 1;
}
