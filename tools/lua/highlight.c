// Copyright 2022 Mitchell Kember. Subject to the MIT License.

#include <lua/lauxlib.h>
#include <lua/lua.h>
#include <math.h>

int l_sin(lua_State *L) {
    double d = luaL_checknumber(L, 1); /* get argument */
    lua_pushnumber(L, sin(d));         /* push result */
    return 1;                          /* number of results */
}

static const luaL_Reg fns[] = {{"sin", l_sin}, {NULL, NULL}};

LUALIB_API int luaopen_highlight(lua_State *L) {
    luaL_newlib(L, fns);
    lua_pushstring(L, "mytest1.0");
    lua_setfield(L, -2, "version");

    return 1;
}
