// Copyright 2022 Mitchell Kember. Subject to the MIT License.

// This Lua C library provides Scheme syntax highlighting.
//
// Example usage from Lua:
//
//     schemehl = require("schemehl") -- loads schemehl.so from LUA_CPATH
//     print(schemehl.highlight("(define x 1)"))
//

#include <lua5.4/lauxlib.h>
#include <lua5.4/lua.h>
#include <math.h>

int l_highlight(lua_State *L) {
    double d = luaL_checknumber(L, 1); /* get argument */
    lua_pushnumber(L, sin(d));         /* push result */
    return 1;                          /* number of results */
}

static const luaL_Reg library[] = {{"highlight", l_highlight}, {NULL, NULL}};

LUALIB_API int luaopen_schemehl(lua_State *L) {
    luaL_newlib(L, library);
    return 1;
}
