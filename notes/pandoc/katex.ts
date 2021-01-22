// Copyright 2021 Mitchell Kember. Subject to the MIT License.

import { exists } from "https://deno.land/std/fs/mod.ts";
import { dirname, resolve } from "https://deno.land/std/path/mod.ts";
import katex from "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.mjs";

// Name of this script, used in logs.
const SCRIPT_NAME = "katex.ts";

// ANSI color escape sequences.
const COLOR = !Deno.noColor && Deno.isatty(1) && Deno.isatty(2);
const RED = COLOR ? "\x1b[31;1m" : "";
const RESET = COLOR ? "\x1b[0m" : "";
const ERROR = `${RED}ERROR:${RESET}`;

// Prints the usage message using the given writing function.
function usage(write: (s: string) => void) {
  write(
    `
usage: deno run --unstable --allow-read --allow-write
       ${SCRIPT_NAME} [--help | --wait] SOCKET_FILE

Runs a server that renders TeX into KaTeX HTML.

The server listens on the Unix domain socket SOCKET_FILE (stream-oriented).
Requests and responses are null-terminated.
The request prefix "display:" enables display mode.
The response prefix "error:" indicates an error message.

The server automatically exits if SOCKET_FILE is removed.

When run with the --wait flag, blocks until a server is started on SOCKET_FILE.
`.trim(),
  );
}

// Error that causes the program to clean up and exit.
class ExitError extends Error {
  constructor(readonly code: number) {
    super();
  }
}

// Files to remove before exiting.
const filesToRemove: string[] = [];

// Removes files in filesToRemove if they exist.
async function cleanup(): Promise<void> {
  try {
    await Promise.allSettled(filesToRemove.map((f) => Deno.remove(f)));
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
function handleSignals(): Promise<never> {
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

// Request prefix used to request display mode.
const DISPLAY_PREFIX = "display:";
// Response prefix used to indicate an error message follows.
const ERROR_PREFIX = "error:";

// Responds to TeX requests with KaTeX HTML responses.
async function serveKatex(listener: Deno.Listener): Promise<void> {
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();

  async function handle(conn: Deno.Conn): Promise<void> {
    let buffer = "";
    for await (const chunk of Deno.iter(conn)) {
      const requests = decoder.decode(chunk).split("\x00");
      requests[0] = buffer + requests[0];
      buffer = requests.pop()!;
      for (const req of requests) {
        const displayMode = req.startsWith(DISPLAY_PREFIX);
        const tex = displayMode ? req.slice(DISPLAY_PREFIX.length) : req;
        const response = renderKatex(tex, displayMode);
        await Deno.writeAll(conn, encoder.encode(response + "\x00"));
      }
    }
  }

  // In order to serve clients concurrently, we do _not_ await handle(conn).
  for await (const conn of listener) {
    handle(conn).catch((ex) => {
      console.error(`${ERROR} ${ex}`);
    }).finally(() => {
      conn.close();
    });
  }
}

// Renders a TeX string to KaTeX HTML.
function renderKatex(tex: string, displayMode: boolean): string {
  let html;
  try {
    html = katex.renderToString(tex, {
      displayMode,
      throwOnError: true,
      strict: "error",
      macros: {
        "\\abs": "\\left\\lvert #1\\right\\rvert",
        "\\Fib": "\\text{Fib}",
      },
    });
  } catch (ex) {
    return ERROR_PREFIX + ex.toString();
  }
  // There is no need to include xmlns:
  // https://www.w3.org/TR/MathML3/chapter6.html#interf.html
  return html.replace(
    '<math xmlns="http://www.w3.org/1998/Math/MathML">',
    "<math>",
  );
}

// Waits for the given file to be created (if it doesn't already exist).
async function waitForFile(path: string): Promise<void> {
  if (await exists(path)) {
    return;
  }
  const absolute = resolve(path);
  for await (const event of Deno.watchFs(dirname(path), { recursive: false })) {
    if (event.kind === "modify" && event.paths.includes(absolute)) {
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
  throw Error("watchFs stopped!");
}

// Starts the server on socketFile. Throws ExitError if socketFile already
// exists, or if it is removed while the server is running.
async function startServer(socketFile: string): Promise<void> {
  if (await exists(socketFile)) {
    console.error(`
${ERROR} ${socketFile} already exists!
There must be another server running. To terminate it, run 'rm ${socketFile}'.
To find it, run 'pgrep -f ${socketFile}'. To leave it running and start a second
server, choose a different socket filename.
`.trim());
    throw new ExitError(1);
  }
  filesToRemove.push(socketFile);
  const listener = Deno.listen({ transport: "unix", path: socketFile });
  // The listen call creates socketFile, but for some reason it does not cause a
  // watchFs event in waitForFile. To force one, we touch the file and commit
  // the change with fsync (also ensuring whileFileExists starts up-to-date).
  const now = new Date();
  await Deno.utime(socketFile, now, now);
  const parent = await Deno.open(dirname(socketFile));
  await Deno.fsync(parent.rid);
  parent.close();
  console.log(`${SCRIPT_NAME}: listening on ${socketFile}`);
  return Promise.race([
    serveKatex(listener),
    whileFileExists(socketFile),
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
  if (waitIdx >= 0) {
    return waitForFile(socketFile);
  }
  let exitCode = 0;
  try {
    await Promise.race([
      handleSignals(),
      startServer(socketFile),
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
