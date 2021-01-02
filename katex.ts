// Copyright 2020 Mitchell Kember. Subject to the MIT License.

import { MuxAsyncIterator } from "https://deno.land/std/async/mod.ts";
import { signal } from "https://deno.land/std/signal/mod.ts";
import katex from "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.mjs";

// Unix domain socket file.
const SOCKET = "katex.sock";

// Unlinks the SOCKET file.
function cleanup() {
  try {
    Deno.removeSync(SOCKET);
  } catch {}
}

// Termination signals that we want to handle.
const SIGNUMS = [
  Deno.Signal.SIGHUP,
  Deno.Signal.SIGINT,
  Deno.Signal.SIGTERM,
];

// Upon receiving one of the signals in SIGNUMS, performs cleanup and exits.
async function handleSignals() {
  return Promise.all(SIGNUMS.map((sig) =>
    (async () => {
      await Deno.signal(sig);
      cleanup();
      // Simulate the exit status for this signal. Really we should re-raise and
      // let the default handler exit, but the only way to do this seems to be
      // `Deno.kill(Deno.pid, sig)` which requires --allow-run.
      Deno.exit(128 + sig);
    })()
  ));
}

// Accepts input on SOCKET and responds with pre-rendered KaTeX HTML.
async function serve() {
  const listener = Deno.listen({ transport: "unix", path: SOCKET });
  console.log(`katex.ts: listening on ${SOCKET}`);

  const decoder = new TextDecoder();
  const encoder = new TextEncoder();
  for await (const conn of listener) {
    let buffer = "";
    for await (const chunk of Deno.iter(conn)) {
      const parts = decoder.decode(chunk).split("\x00");
      parts[0] = buffer + parts[0];
      buffer = parts.pop()!;
      for (const part of parts) {
        await Deno.writeAll(
          conn,
          encoder.encode(
            katex.renderToString(part.slice(1), {
              displayMode: part.charAt(0) == "D",
              throwOnError: false,
            }),
          ),
        );
      }
    }
  }
}

try {
  await Promise.all([
    handleSignals(),
    serve(),
  ]);
} catch {
  cleanup();
}
