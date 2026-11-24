// Deterministic source bundle. Never installs or starts Firefox or the bridge.
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { Script } from "node:vm";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const base = dirname(fileURLToPath(import.meta.url));
const modules = [
	"chatqueue-recurrence-core.mjs",
	"chatqueue-local-controller.mjs",
	"chatqueue-operational-client.mjs",
	"chatqueue-operational-runtime.mjs",
	"chatqueue-operational-browser.js",
];
const strip = (source) =>
	source
		.replace(/^import\s+\{[\s\S]*?\}\s+from\s+"\.\/[^"]+";\s*/gm, "")
		.replace(/\bexport (function|const)\b/g, "$1");
const source = modules
	.map((n) => strip(readFileSync(join(base, n), "utf8").trimEnd()))
	.join("\n");
if (/^import\s|^export\s/m.test(source))
	throw new Error("Unresolved static modules");
const digest = createHash("sha256").update(source).digest("hex").slice(0, 16);
const metadata = [
	"// ==UserScript==",
	"// @name         ChatQueue Broker Operational (klarkc/dotfiles)",
	"// @namespace    https://github.com/klarkc/dotfiles",
	"// @version      0.1.0-preactivation",
	"// @description  Explicitly gated broker executor; no configuration or automatic arm.",
	"// @match        https://chatgpt.com/g/*/c/*",
	"// @run-at       document-idle",
	"// @inject-into  content",
	"// @grant        GM_getValue",
	"// @grant        GM_setValue",
	"// @grant        GM_registerMenuCommand",
	"// @grant        GM_xmlhttpRequest",
	"// @connect      127.0.0.1",
	"// ==/UserScript==",
];
export const bundle =
	metadata.join("\n") +
	"\n// Source checksum: " +
	digest +
	'\n(() => {\n"use strict";\n' +
	source +
	"\n})();\n";
const output = join(base, "chatqueue-operational.user.js");
if (process.argv[1] === fileURLToPath(import.meta.url)) {
	new Script(bundle, { filename: "chatqueue-operational.user.js" });
	if (process.argv.includes("--check")) {
		if (existsSync(output) && readFileSync(output, "utf8") !== bundle)
			throw new Error("Generated operational userscript drift");
		process.stdout.write("PASS: operational source parses; no installation\n");
	} else {
		writeFileSync(output, bundle, { mode: 0o600 });
		process.stdout.write(
			"Generated ignored operational bundle; NO_GO, not installed\n",
		);
	}
}
