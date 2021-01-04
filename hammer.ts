async function f() {
    let conn = await Deno.connect({transport: "unix", path: "katex.sock"});
    for (let i = 0; i < 100; i++) {
        await Deno.writeAll(conn, new TextEncoder().encode("this is akfaskldjf\n"));
        for await (const chunk of Deno.iter(conn)) {
            console.log(`Got chunk: ${new TextDecoder().decode(chunk)}`);
            break;
        }
    }
}

let promises = [];
for (let i = 0; i < 40; i++) {
    promises.push(f());
}
await Promise.all(promises);
