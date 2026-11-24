// Compile trusted operator-provisioned bindings from the pinned canonical
// prompt document. No secrets are printed. The output is always unreviewed.
import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { lstatSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, isAbsolute, join } from "node:path";
import { fileURLToPath } from "node:url";
import { AGENTS } from "./chatqueue-recurrence-core.mjs";
import {
	parseOperationalTemplates,
	validateOperationalConfig,
	OPERATIONAL_CONFIG_KEY,
} from "./chatqueue-operational-client.mjs";

const promptPath = "scheduling/prompts/permanent-four-agents-chatqueue.md";
export function compileOperationalConfig({
	markdown,
	promptBlob,
	bindings,
	bridge,
}) {
	const raw = Buffer.from(markdown, "utf8");
	const digest = createHash("sha1")
		.update("blob " + raw.length + "\0")
		.update(raw)
		.digest("hex");
	if (digest !== promptBlob) throw new Error("Prompt blob mismatch");
	const templates = parseOperationalTemplates(markdown);
	if (!Array.isArray(bindings) || bindings.length !== AGENTS.length)
		throw new Error("Four bindings required");
	const result = {
		version: 1,
		reviewed: false,
		promptBlob,
		bridge: { port: bridge?.port, capability: bridge?.capability },
		bindings: bindings.map((b) => ({
			agent: b.agent,
			path: b.path,
			modelLabel: b.modelLabel,
			prompt: templates[b.agent],
		})),
	};
	if (!validateOperationalConfig(result))
		throw new Error("Invalid private configuration");
	return result;
}
function privateFile(path) {
	if (!isAbsolute(path ?? ""))
		throw new Error("Absolute private file path required");
	const s = lstatSync(path),
		d = lstatSync(dirname(path));
	if (
		!s.isFile() ||
		!d.isDirectory() ||
		s.uid !== process.getuid() ||
		d.uid !== process.getuid() ||
		(s.mode & 0o077) !== 0 ||
		(d.mode & 0o077) !== 0
	)
		throw new Error("Private file required");
	return JSON.parse(readFileSync(path, "utf8"));
}
function main() {
	const [repo, head, bridgeFile] = process.argv.slice(2);
	if (!isAbsolute(repo ?? "") || !/^[0-9a-f]{40}$/.test(head ?? ""))
		throw new Error("Pinned checkout and HEAD required");
	const origin = process.env.CHATQUEUE_BROKER_ORIGIN;
	if (
		!origin ||
		!/^(?:git@[a-zA-Z0-9.-]+:|https:\/\/[a-zA-Z0-9.-]+\/)[a-zA-Z0-9_.-]+\/[a-zA-Z0-9_.-]+(?:\.git)?$/.test(
			origin,
		)
	)
		throw new Error("Trusted explicit origin required");
	const git = (...args) =>
		execFileSync("git", ["-C", repo, ...args], {
			encoding: "utf8",
			timeout: 15000,
			maxBuffer: 1024 * 1024,
			stdio: ["ignore", "pipe", "pipe"],
			env: {
				...process.env,
				GIT_NO_REPLACE_OBJECTS: "1",
				GIT_NO_LAZY_FETCH: "1",
				GIT_TERMINAL_PROMPT: "0",
				GIT_OPTIONAL_LOCKS: "0",
			},
		}).trimEnd();
	if (
		git("remote", "get-url", "origin") !== origin ||
		git("ls-remote", "--exit-code", "origin", "refs/heads/main") !==
			head + "\trefs/heads/main"
	)
		throw new Error("Trusted HEAD/origin not verified");
	const blob = git("rev-parse", "--verify", head + ":" + promptPath);
	// Retain exact trailing bytes for Git blob verification.
	const markdown = execFileSync("git", ["-C", repo, "cat-file", "blob", blob], {
		encoding: "utf8",
		timeout: 15000,
		maxBuffer: 1024 * 1024,
		stdio: ["ignore", "pipe", "pipe"],
		env: {
			...process.env,
			GIT_NO_REPLACE_OBJECTS: "1",
			GIT_NO_LAZY_FETCH: "1",
			GIT_OPTIONAL_LOCKS: "0",
		},
	});
	const bridge = privateFile(bridgeFile);
	const bindings = JSON.parse(process.env.CHATQUEUE_BROKER_BINDINGS ?? "null");
	const result = compileOperationalConfig({
		markdown,
		promptBlob: blob,
		bindings,
		bridge,
	});
	if (
		git("remote", "get-url", "origin") !== origin ||
		git("ls-remote", "--exit-code", "origin", "refs/heads/main") !==
			head + "\trefs/heads/main"
	)
		throw new Error("Prompt HEAD changed during provisioning");
	const output = join(dirname(bridgeFile), "operational-config.json");
	writeFileSync(
		output,
		JSON.stringify({ [OPERATIONAL_CONFIG_KEY]: result }) + "\n",
		{ mode: 0o600, flag: "wx" },
	);
	process.stdout.write(
		"Private unreviewed config generated; no installation, no arm, NO_GO\n",
	);
}
if (process.argv[1] === fileURLToPath(import.meta.url)) {
	try {
		main();
	} catch {
		process.stderr.write("Config compilation denied; no send authorization\n");
		process.exitCode = 2;
	}
}
