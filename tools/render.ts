// Copyright 2021 Mitchell Kember. Subject to the MIT License.

import { writeAll } from "https://deno.land/std@0.216.0/io/write_all.ts";
import { iterateReader } from "https://deno.land/std@0.216.0/streams/mod.ts";
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
Usage: deno run --unstable --allow-{read,write}=SOCKET[,FIFO] --allow-run=svgbob
       ${SCRIPT_NAME} SOCKET [FIFO]

Run server that renders KaTeX math and svgbob diagrams

Arguments:
    SOCKET  Socket path. The server creates a stream-oriented Unix domain socket
            here to listen on. It exits automatically if SOCKET is removed.
    FIFO    Synchronization file. If provided, the sever signals FIFO (writes
            zero bytes) when it is ready to serve requests on SOCKET.

Requests:
    KaTeX   "katex:" [ "display:" ] tex_input "\\0"
    svgbob  "svgbob:" diagram_number ":" ascii_input "\\0"

Responses:
    success  html_output "\\0"
    error    "error:" error_message "\\0"
`.trim(),
  );
}

// Helper to satisfy the typechecker.
//
// deno-lint-ignore no-explicit-any
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
const SIGNALS = [
  { name: "SIGHUP", number: 1 },
  { name: "SIGINT", number: 2 },
  { name: "SIGTERM", number: 15 },
] as const;

// Adds listeners that exit upon receiving any of the signals in SIGNALS.
function addSignalListeners(): void {
  for (const { name, number } of SIGNALS) {
    Deno.addSignalListener(name, () => {
      cleanup().finally(() => {
        // Simulate the exit status for this signal. Really we should re-raise
        // and let the default handler exit, but the only way to do this seems
        // to be `Deno.kill(Deno.pid, sig)`, and I can't get that to work.
        Deno.exit(128 + number);
      });
    });
  }
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
    for await (const chunk of iterateReader(conn)) {
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
        await writeAll(conn, encoder.encode(response + "\x00"));
      }
    }
  }

  // In order to serve clients concurrently, we do _not_ await handle(conn).
  for await (const conn of listener) {
    handle(conn)
      .catch((ex) => {
        console.error(`${ERROR} ${ex}`);
      })
      .finally(() => {
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
      // Trust htmlExtension, in particular \htmlClass{...}{...}.
      trust: true,
      // Since htmlExtension is incompatible with strict mode, we have to
      // explicitly ingore errors for it.
      strict: (errorCode: string) =>
        errorCode == "htmlExtension" ? "ignore" : "error",
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
  arrow: `
<marker id="" viewBox="-2 -2 8 8" refX="4" refY="2" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
  <polygon points="0,0 0,4 4,2 0,0"></polygon>
</marker>`,

  diamond: `
<marker id="" viewBox="-2 -2 8 8" refX="4" refY="2" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
  <polygon points="0,2 2,0 4,2 2,4 0,2"></polygon>
</marker>`,

  circle: `
<marker id="" viewBox="0 0 8 8" refX="4" refY="4" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
  <circle cx="4" cy="4" r="2.5" stroke="none" fill="currentColor"></circle>
</marker>`,

  open_circle: `
<marker id="" viewBox="0 0 8 8" refX="4" refY="4" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
  <circle cx="4" cy="4" r="2" stroke-width="1.5" style="fill: var(--bg);"></circle>
</marker>`,

  big_open_circle: `
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
  const cmd = new Deno.Command("svgbob", {
    args: ["--scale", scaleFactor.toString()],
    stdin: "piped",
    stdout: "piped",
  });
  const child = cmd.spawn();
  const stdin = child.stdin.getWriter();
  await writeAll(stdin, new TextEncoder().encode(diagram));
  stdin.close();
  const { code, stdout } = await child.output();
  if (code !== 0) {
    return ERROR_PREFIX + `svgbob exited with code ${code}`;
  }
  const markers = new Set<string>();
  const unopt = new TextDecoder()
    .decode(stdout)
    .replace(/width="([0-9.]+)" height="([0-9.]+)"/, (_, w, h) => {
      w = Math.ceil(w);
      // svgbob seems to add extra padding on the bottom.
      h = Math.ceil(h) - 20;
      return `viewBox="0 0 ${w} ${h}" style="max-width: ${w}px;"`;
    })
    .replace(/<style[\s\S]*<\/style>/, "")
    .replace(/<rect class="backdrop".*?<\/rect>/, "")
    .replace(/class="([^"]+)"/g, (_, classes) =>
      classes
        .split(" ")
        .map((cl: string) => {
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
        })
        .join(" "))
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
      // Don't remove defaults since style.css determines defaults for .diagram.
      // "removeUnknownsAndDefaults",
    ],
  }) as { data?: string };
  if (opt == undefined || opt.data == undefined) {
    return ERROR_PREFIX + "svgo produced: " + JSON.stringify(opt);
  }
  return opt.data;
}

// Returns true if the file exists.
async function exists(path: string): Promise<boolean> {
  try {
    await Deno.lstat(path);
    return true;
  } catch (err) {
    if (err instanceof Deno.errors.NotFound) {
      return false;
    }
    throw err;
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

// If fifo is provided, signals it by opening it for writing and closing it.
async function maybeSignalFifo(fifo?: string): Promise<void> {
  if (fifo == undefined) {
    return;
  }
  try {
    (await Deno.open(fifo, { write: true })).close();
  } catch (ex) {
    if (ex instanceof Deno.errors.NotFound) {
      console.error(
        `
${ERROR} ${fifo} does not exist!
Try running 'mkfifo ${fifo}' first before starting the server.
`.trim(),
      );
      throw new ExitError(1);
    }
    throw ex;
  }
}

// Wraps a promise so that it never resolves.
async function neverResolve(promise: Promise<void>): Promise<void> {
  await promise;
  return Promise.race([]);
}

// Starts the server on socket and signals fifo if provided. Throws ExitError if
// socket already exists, or if it is removed while the server is running.
async function startServer(socket: string, fifo?: string): Promise<void> {
  if (await exists(socket)) {
    console.error(
      `
${ERROR} ${socket} already exists!
There must be another server running. To terminate it, run 'rm ${socket}'.
To find it, run 'pgrep -f ${socket}'. To leave it running and start a second
server, choose a different socket filename.
`.trim(),
    );
    throw new ExitError(1);
  }
  filesToRemove.push(socket);
  const listener = Deno.listen({ transport: "unix", path: socket });
  console.log(`${SCRIPT_NAME}: listening on ${socket}`);
  return Promise.race([
    neverResolve(maybeSignalFifo(fifo)),
    whileFileExists(socket),
    serve(listener),
  ]);
}

async function main() {
  addSignalListeners();
  const args = [...Deno.args];
  if (args.includes("-h") || args.includes("--help")) {
    usage(console.log);
    return;
  }
  if (args.length == 0 || args.length > 2) {
    usage(console.error);
    Deno.exit(1);
  }
  let exitCode = 0;
  try {
    await startServer(args[0], args[1]);
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
