// Deterministic local userscript bundle. Does not install browser extensions.
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { Script } from "node:vm";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const base = dirname(fileURLToPath(import.meta.url));
const read = (name) => readFileSync(join(base, name), "utf8").trimEnd();
const core = read("chatqueue-recurrence-core.mjs").replace(
	/\bexport (function|const)\b/g,
	"$1",
);
const controller = read("chatqueue-local-controller.mjs")
	.replace(
		/import \{[\s\S]*?\} from "\.\/chatqueue-recurrence-core\.mjs";\s*/,
		"",
	)
	.replace(/\bexport function\b/g, "function");
const browser = read("chatqueue-local-browser.js");
if (/\bimport \{|\bexport (?:function|const)\b/.test(core + controller))
	throw new Error("Failed to strip static module imports/exports");
const digest = createHash("sha256")
	.update(core + "\n" + controller + "\n" + browser)
	.digest("hex")
	.slice(0, 16);
const metadata = [
	"// ==UserScript==",
	"// @name         ChatQueue Local LAB (klarkc/dotfiles)",
	"// @namespace    https://github.com/klarkc/dotfiles",
	"// @version      0.1.0-lab",
	"// @description  Original fail-closed recurrence lab; disabled on startup; no broker sends.",
	"// @match        https://chatgpt.com/c/*",
	"// @run-at       document-idle",
	"// @grant        GM_getValue",
	"// @grant        GM_setValue",
	"// @grant        GM_registerMenuCommand",
	"// ==/UserScript==",
];
export const bundle =
	metadata.join("\n") +
	"\n// Source checksum: " +
	digest +
	'\n(() => {\n"use strict";\n' +
	core +
	"\n" +
	controller +
	"\n" +
	browser +
	"\n})();\n";
const output = join(base, "chatqueue-local.user.js");
if (process.argv[1] === fileURLToPath(import.meta.url)) {
	new Script(bundle, { filename: "chatqueue-local.user.js" });
	if (process.argv.includes("--check")) {
		if (existsSync(output) && readFileSync(output, "utf8") !== bundle)
			throw new Error(
				"Generated userscript drift: run make firefox.ChatQueueLocalBuild",
			);
		process.stdout.write(
			"PASS: generated LAB source parses and matches existing local output\n",
		);
	} else {
		writeFileSync(output, bundle);
		process.stdout.write(
			"Generated ignored LAB-only userscript (not installed)\n",
		);
	}
}
