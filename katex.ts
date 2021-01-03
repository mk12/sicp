// Copyright 2020 Mitchell Kember. Subject to the MIT License.

import { MuxAsyncIterator } from "https://deno.land/std/async/mod.ts";
import { exists } from "https://deno.land/std/fs/mod.ts";
import { dirname, resolve } from "https://deno.land/std/path/mod.ts";
import { signal } from "https://deno.land/std/signal/mod.ts";
import katex from "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.mjs";

// Name of this script, used in logs.
const SCRIPT_NAME = "katex.ts";

// Prints the usage message using the given writing function.
function usage(write: (s: string) => void) {
  write(
    `
usage: deno run --unstable --allow-read --allow-write
       ${SCRIPT_NAME} [--help | --wait] SOCKET_FILE

Server that listens on the Unix domain socket SOCKET_FILE.

Responds to each null-terminated TeX string with a null-terminated KaTeX HTML
string. Prefix the input with "display:" to enable display mode.

Creates SOCKET_FILE.pid on startup, and exits the server if it is ever removed.
When run with the --wait option, simply waits for that file to be created. This
allows you to wait for a background server to start.
`.trim(),
  );
}

// Error that causes the program to clean up and exit.
class ExitError extends Error {
  constructor(readonly code: number) { super(); }
}

// Files to remove before exiting.
const filesToRemove: string[] = [];

// Removes files in filesToRemove if they exist.
async function cleanup(): Promise<void> {
  try {
    await Promise.allSettled(filesToRemove.map(f => Deno.remove(f)));
  } catch (ex) {
    if (!(ex instanceof Deno.errors.NotFound)) {
      throw ex;
    }
  }
}

// Termination signals that we want to handle.
const SIGNUMS = [
  Deno.Signal.SIGHUP,
  Deno.Signal.SIGINT,
  Deno.Signal.SIGTERM,
];

// Upon receiving one of the signals in SIGNUMS, throws ExitError.
async function handleSignals(): Promise<never> {
  return Promise.race(SIGNUMS.map((sig) =>
    (async () => {
      await Deno.signal(sig);
      // Simulate the exit status for this signal. Really we should re-raise and
      // let the default handler exit, but the only way to do this seems to be
      // `Deno.kill(Deno.pid, sig)` which requires --allow-run.
      throw new ExitError(128 + sig);
    })()
  ));
}

// Prefix used to request display mode.
const DISPLAY_PREFIX = "display:";

// Responds to null-terminated TeX messages with null-terminated KaTeX HTML.
async function serveKatex(listener: Deno.Listener): Promise<void> {
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();
  for await (const conn of listener) {
    let buffer = "";
    for await (const chunk of Deno.iter(conn)) {
      const parts = decoder.decode(chunk).split("\x00");
      parts[0] = buffer + parts[0];
      buffer = parts.pop()!;
      for (const part of parts) {
        const displayMode = part.startsWith(DISPLAY_PREFIX);
        const tex = displayMode ? part.slice(DISPLAY_PREFIX.length) : part;
        await Deno.writeAll(
          conn,
          encoder.encode(
            katex.renderToString(tex, { displayMode, throwOnError: false }),
          ),
        );
      }
    }
  }
}

// Waits for the given file to be created (if it doesn't already exist).
async function waitForFile(path: string): Promise<void> {
  if (await exists(path)) {
    return;
  }
  const absolute = resolve(path);
  for await (const event of Deno.watchFs(dirname(path), { recursive: false })) {
    if (event.kind === "create" && event.paths.includes(absolute)) {
      return;
    }
  }
}

// Throws ExitError(0) when the given file is removed.
async function whileFileExists(path: string): Promise<never> {
  for await (const event of Deno.watchFs(path)) {
    if (event.kind === "remove") {
      throw new ExitError(0);
    }
  }
  throw Error("watchFS stopped!");
}

// Starts the server on socketFile. Also writes the PID of the current process
// to pidFile. Throws ExitError if socketFile already exists, or if pidFile is
// removed while the server is running.
async function startServer(socketFile: string, pidFile: string): Promise<void> {
  // Deno.listen seems to automatically remove the file if it exists. We don't
  // want to do that because that likely means you're running two servers.
  if (await exists(socketFile) || await exists(pidFile)) {
    console.error(`error: ${socketFile} already exists`);
    throw new ExitError(1);
  }
  filesToRemove.push(socketFile);
  const listener = Deno.listen({ transport: "unix", path: socketFile });
  filesToRemove.push(pidFile);
  await Deno.writeFile(pidFile, new TextEncoder().encode(Deno.pid.toString()));
  console.log(`${SCRIPT_NAME}: listening on ${socketFile}`);
  return Promise.race([
    serveKatex(listener),
    whileFileExists(pidFile),
  ]);
}

async function main() {
  const args = [...Deno.args];
  if (args.includes("-h") || args.includes("--help")) {
    usage(console.log);
    return;
  }
  const waitIdx = args.indexOf("--wait");
  if (waitIdx >= 0) {
    args.splice(waitIdx, 1);
  }
  if (args.length != 1) {
    usage(console.error);
    Deno.exit(1);
  }
  const socketFile = args[0];
  const pidFile = `${socketFile}.pid`;
  if (waitIdx >= 0) {
    return waitForFile(pidFile);
  }
  let exitCode = 0;
  try {
    await Promise.race([
      handleSignals(),
      startServer(socketFile, pidFile),
    ]);
  } catch (ex) {
    if (ex instanceof ExitError) {
      exitCode = ex.code;
    } else {
      throw ex;
    }
  } finally {
    await cleanup();
  }
  Deno.exit(exitCode);
}

main();
