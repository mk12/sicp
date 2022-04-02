// Copyright 2021 Mitchell Kember. Subject to the MIT License.

import { exists } from "https://deno.land/std/fs/mod.ts";
import { dirname, resolve } from "https://deno.land/std/path/mod.ts";
import katex from "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.mjs";
import { optimize } from "https://cdn.jsdelivr.net/gh/lumeland/svgo@v3.0.1/mod.js";

// Name of this script, used in logs.
const SCRIPT_NAME = "render.ts";

// ANSI color escape sequences.
const COLOR = !Deno.noColor && Deno.isatty(1) && Deno.isatty(2);
const RED = COLOR ? "\x1b[31;1m" : "";
const RESET = COLOR ? "\x1b[0m" : "";
const ERROR = `${RED}ERROR:${RESET}`;

// Prints the usage message using the given writing function.
function usage(write: (s: string) => void) {
  write(
    `
usage: deno run --unstable --allow-read --allow-write --allow-run
       ${SCRIPT_NAME} [--help | --wait] SOCKET_FILE

Runs a server that renders KaTeX math and svgbob diagrams.

The server listens on the Unix domain socket SOCKET_FILE (stream-oriented).
The server automatically exits if SOCKET_FILE is removed.
When run with the --wait flag, blocks until a server is started on SOCKET_FILE.

The protocol is as follows:

* Requests and responses are null-terminated.
* Responses contain HTML, or "error:" followed by an error message.
* KATEX request: "katex:", (then optionally "display:"), and then TeX input.
* SVGBOB request: "svgbob:", the diagram number, ":", and then ASCII art text.
`.trim(),
  );
}

// Helper to satisfy the typechecker.
function hasKey<T>(obj: T, key: any): key is keyof T {
  return key in obj;
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
      // `Deno.kill(Deno.pid, sig)`, and I can't get that to work properly.
      throw new ExitError(128 + sig);
    })()
  ));
}

// A simple request parser.
class Parser {
  rest: string;
  eaten?: string;

  constructor(private readonly request: string) {
    this.rest = request;
  }

  eat(label?: string): string | undefined {
    this.eaten = undefined;
    if (label === undefined) {
      const idx = this.rest.indexOf(":");
      if (idx >= 0) {
        this.eaten = this.rest.slice(0, idx);
        this.rest = this.rest.slice(idx + 1);
      }
    } else if (this.rest.startsWith(label + ":")) {
      this.eaten = label;
      this.rest = this.rest.slice(label.length + 1);
    }
    return this.eaten;
  }
}

// Response prefix used to indicate an error message follows.
const ERROR_PREFIX = "error:";

// Serves requests forever.
async function serve(listener: Deno.Listener): Promise<void> {
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();

  async function handle(conn: Deno.Conn): Promise<void> {
    let buffer = "";
    for await (const chunk of Deno.iter(conn)) {
      const requests = decoder.decode(chunk).split("\x00");
      requests[0] = buffer + requests[0];
      buffer = requests.pop()!;
      for (const req of requests) {
        const parser = new Parser(req);
        let response;
        if (parser.eat("katex")) {
          const display = !!parser.eat("display");
          response = renderKatex(parser.rest, display);
        } else if (parser.eat("svgbob") && parser.eat()) {
          response = await renderSvgbob(parseInt(parser.eaten!), parser.rest);
        } else {
          response = ERROR_PREFIX + "invalid request";
        }
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
        "\\powerset": "\\mathcal{P}(#1)",
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

// SVG markers used by svgbob.
const SVGBOB_MARKERS = {
  "arrow": `
<marker id="" viewBox="-2 -2 8 8" refX="4" refY="2" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
  <polygon points="0,0 0,4 4,2 0,0"></polygon>
</marker>`,

  "diamond": `
<marker id="" viewBox="-2 -2 8 8" refX="4" refY="2" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
  <polygon points="0,2 2,0 4,2 2,4 0,2"></polygon>
</marker>`,

  "circle": `
<marker id="" viewBox="0 0 8 8" refX="4" refY="4" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
  <circle cx="4" cy="4" r="2.5" stroke="none" fill="currentColor"></circle>
</marker>`,

  "open_circle": `
<marker id="" viewBox="0 0 8 8" refX="4" refY="4" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
  <circle cx="4" cy="4" r="2" stroke-width="1.5" style="fill: var(--bg);"></circle>
</marker>`,

  "big_open_circle": `
<marker id="" viewBox="0 0 8 8" refX="4" refY="4" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
  <circle cx="4" cy="4" r="3" style="fill: var(--bg);"></circle>
</marker>`,
};

// Returns an HTML id to use for a marker in an svgbob diagram.
function markerId(diagramNumber: number, markerName: string) {
  return `m${diagramNumber}${markerName[0]}`;
}

// Renders an ASCII diagram to SVG using svgbob and svgo.
async function renderSvgbob(number: number, diagram: string): Promise<string> {
  // We want 16px to match code text size (0.8em of 20px = 16px), so scale to
  // that from svgbob's default font size of 14. No need to pass the --font-size
  // flag since we remove all inline styles below and use style.css instead.
  const scaleFactor = 16 / 14;
  const p = Deno.run({
    cmd: ["svgbob", "--scale", scaleFactor.toString()],
    stdin: "piped",
    stdout: "piped",
  });
  await Deno.writeAll(p.stdin, new TextEncoder().encode(diagram));
  p.stdin.close();
  const [status, stdout] = await Promise.all([
    p.status(),
    p.output(),
  ]);
  p.close();
  if (status.code !== 0) {
    return ERROR_PREFIX + `svgbob exited with code ${status.code}`;
  }
  let markers = new Set<string>();
  const unopt = new TextDecoder().decode(stdout)
    .replace(
      /width="([0-9.]+)" height="([0-9.]+)"/,
      (_, w, h) => {
        w = Math.ceil(w);
        // svgbob seems to add extra padding on the bottom.
        h = Math.ceil(h) - 20;
        return `viewBox="0 0 ${w} ${h}" style="max-width: ${w}px;"`;
      },
    )
    .replace(/<style[\s\S]*<\/style>/, "")
    .replace(/<rect class="backdrop".*?<\/rect>/, "")
    .replace(
      /class="([^"]+)"/g,
      (_, classes) =>
        classes.split(" ").map((cl: string) => {
          let match;
          if (cl === "broken") {
            return `stroke-dasharray="8"`;
          } else if ((match = cl.match(/^start_marked_(.*)$/))) {
            markers.add(match[1]);
            return `marker-start="url(#${markerId(number, match[1])})"`;
          } else if ((match = cl.match(/^end_marked_(.*)$/))) {
            markers.add(match[1]);
            return `marker-end="url(#${markerId(number, match[1])})"`;
          }
        }).join(" "),
    )
    // Must do this after replacing all the classes.
    .replace(/^<svg /, `<svg class="diagram" `)
    .replace(/<defs>[\s\S]*<\/defs>/, () => {
      const defs = [];
      for (const marker of markers) {
        if (!hasKey(SVGBOB_MARKERS, marker)) {
          throw Error(`unexpected svgbob marker: ${marker}`);
        }
        const idAttr = `id="${markerId(number, marker)}"`;
        defs.push(SVGBOB_MARKERS[marker].replace(/id=""/, idAttr));
      }
      return `<defs>${defs.join("")}</defs>`;
    })
    // Scaling up to 16px results in all text being slighlty too far right.
    .replace(/<text x="([0-9.]+)"/g, (_, x) => `<text x="${x - 2}"`);
  const opt = optimize(unopt, {
    multipass: true,
    plugins: [
      "removeDoctype",
      "removeXMLNS",
      "removeXMLProcInst",
      "removeComments",
      "removeMetadata",
      "removeEditorsNSData",
      "cleanupAttrs",
      "inlineStyles",
      "minifyStyles",
      "removeUselessDefs",
      "cleanupNumericValues",
      "convertColors",
      "removeNonInheritableGroupAttrs",
      "removeUselessStrokeAndFill",
      "removeViewBox",
      "cleanupEnableBackground",
      "removeHiddenElems",
      "removeEmptyText",
      "convertShapeToPath",
      "convertEllipseToCircle",
      "moveElemsAttrsToGroup",
      "moveGroupAttrsToElems",
      "collapseGroups",
      "convertPathData",
      "convertTransform",
      "removeEmptyAttrs",
      "removeEmptyContainers",
      "mergePaths",
      "removeUnusedNS",
      "sortDefsChildren",
      "removeTitle",
      "removeDesc",
      // Don't rename IDs since they must be unique within the page.
      // "cleanupIDs",
      // Don't remove defaults since style.css determines defaults for .digram.
      // "removeUnknownsAndDefaults",
    ],
  }) as { data?: string };
  if (opt == undefined || opt.data == undefined) {
    return ERROR_PREFIX + "svgo produced: " + JSON.stringify(opt);
  }
  return opt.data;
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
    serve(listener),
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