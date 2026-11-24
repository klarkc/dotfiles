// Read-only, original Git policy evidence for future ChatQueue integration.
// Not a scheduler, lease, manual GO, browser identity check or send authority.
// No network or filesystem side effects on import. Do not bundle into LAB.
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { isAbsolute } from "node:path";
import { fileURLToPath } from "node:url";
import { AGENTS } from "./chatqueue-recurrence-core.mjs";
import { parseOperationalTemplates } from "./chatqueue-operational-client.mjs";

const refs = ["refs/heads/main", "refs/heads/scheduling-runtime"];
const paths = [
	["main", "coordination/organization.json"],
	["main", "scheduling/state.json"],
	["main", "scheduling/watchdog-policy.json"],
	["runtime", "scheduling/runtime.json"],
];
const promptPath = "scheduling/prompts/permanent-four-agents-chatqueue.md";
const oid = (s) => typeof s === "string" && /^[0-9a-f]{40}$/.test(s);
const object = (s) => s && typeof s === "object" && !Array.isArray(s);
const deny = (reason) => ({
	status: "NO_GO",
	runtimeAuthorized: false,
	reasons: [reason],
});

export function parseRemoteHeads(raw) {
	const lines = raw.trim().split("\n");
	if (lines.length !== refs.length) throw new Error("Unexpected remote refs");
	const values = new Map();
	for (const line of lines) {
		const [sha, name, extra] = line.split("\t");
		if (
			!oid(sha) ||
			!refs.includes(name) ||
			values.has(name) ||
			extra !== undefined
		)
			throw new Error("Invalid or ambiguous remote ref");
		values.set(name, sha);
	}
	return { main: values.get(refs[0]), runtime: values.get(refs[1]) };
}

export function evaluateGitPolicy(agent, documents) {
	if (!AGENTS.includes(agent)) return deny("UNKNOWN_AGENT");
	if (
		!Array.isArray(documents) ||
		documents.length !== paths.length ||
		!documents.every(object)
	)
		return deny("INVALID_POLICY_DOCUMENT");
	const [org, state, watchdog, runtime] = documents;
	const reasons = [];
	const require = (ok, reason) => {
		if (!ok) reasons.push(reason);
	};
	const agents = org.permanent_agents;
	const exactRoles =
		Array.isArray(agents) &&
		agents.length === AGENTS.length &&
		AGENTS.every((id) => agents.filter((a) => a?.id === id).length === 1);
	require(org.mode === "FOUR_PERMANENT_AGENTS" &&
		org.max_permanent_agents === 4 &&
		exactRoles, "FOUR_ROLE_ORGANIZATION_NOT_VERIFIED");
	const role = exactRoles ? agents.find((a) => a.id === agent) : null;
	require(role?.heartbeat_authorized === true, "ROLE_NOT_AUTHORIZED");
	require(role?.heartbeat_active === true, "ROLE_PAUSED");
	const activation = org.activation;
	require(activation?.allowed_to_run_heartbeat ===
		true, "GLOBAL_HEARTBEAT_NOT_AUTHORIZED");
	require(activation?.chatqueue_production_heartbeat_resume_allowed ===
		true, "PRODUCTION_NO_GO");
	require(activation?.browser_scheduler ===
		"CHATQUEUE_FOR_FIREFOX_ONLY", "EXCLUSIVE_SCHEDULER_NOT_VERIFIED");
	require(activation?.chatqueue_original_1_0_1_resume_allowed ===
		false, "ORIGINAL_QUEUE_NOT_PROHIBITED");
	require(activation?.chatqueue_lab_runtime_installed ===
		false, "LAB_RUNTIME_NOT_ABSENT");
	require(state.enabled === false, "LEGACY_MAIN_GATE_NOT_OFF");
	require(watchdog.enabled === false, "LEGACY_WATCHDOG_GATE_NOT_OFF");
	require(runtime.enabled === false, "LEGACY_RUNTIME_GATE_NOT_OFF");
	return {
		status: reasons.length ? "NO_GO" : "GIT_POLICY_PASS_ONLY",
		runtimeAuthorized: false,
		reasons,
	};
}

// A trusted caller must explicitly pin the origin. Restrict to normal SSH
// or HTTPS repo URLs; never accept embedded credentials, query or fragments.
// No remote URL, command stderr, or raw document is returned.
function validOrigin(s) {
	return (
		typeof s === "string" &&
		/^(?:git@[a-zA-Z0-9.-]+:|https:\/\/[a-zA-Z0-9.-]+\/)[a-zA-Z0-9_.-]+\/[a-zA-Z0-9_.-]+(?:\.git)?$/.test(
			s,
		)
	);
}

// All reads are injected for deterministic offline tests. Read fresh remote
// heads twice around exact immutable blob reads. Missing local objects are a
// denial; this function does not fetch, mutate refs or retry a failed read.
export function collectGitPolicy({
	agent,
	expectedOrigin,
	readOrigin,
	readHeads,
	readBlob,
	includePrompt = false,
}) {
	if (!AGENTS.includes(agent)) return deny("UNKNOWN_AGENT");
	if (!validOrigin(expectedOrigin)) return deny("EXPECTED_ORIGIN_REQUIRED");
	try {
		if (readOrigin() !== expectedOrigin) return deny("ORIGIN_MISMATCH");
		const before = parseRemoteHeads(readHeads());
		const documents = [];
		const blobs = {};
		for (const [branch, path] of paths) {
			const item = readBlob(before[branch], path);
			if (!oid(item?.blob) || typeof item.text !== "string")
				throw new Error("Missing pinned blob");
			const doc = JSON.parse(item.text);
			if (!object(doc)) throw new Error("Invalid document");
			documents.push(doc);
			blobs[path] = item.blob;
		}
		let promptBlob, promptDigest;
		if (includePrompt) {
			const item = readBlob(before.main, promptPath);
			if (
				!oid(item?.blob) ||
				typeof item.text !== "string" ||
				!item.text.trim()
			)
				throw new Error("Missing pinned prompt document");
			promptBlob = item.blob;
			promptDigest = createHash("sha256")
				.update(parseOperationalTemplates(item.text)[agent])
				.digest("hex");
		}
		const after = parseRemoteHeads(readHeads());
		if (readOrigin() !== expectedOrigin)
			return deny("ORIGIN_CHANGED_DURING_READ");
		if (before.main !== after.main || before.runtime !== after.runtime)
			return deny("REMOTE_HEAD_CHANGED_DURING_READ");
		return {
			...evaluateGitPolicy(agent, documents),
			agent,
			heads: before,
			blobs,
			...(includePrompt ? { promptBlob, promptDigest } : {}),
		};
	} catch {
		// No raw exception: provider errors can contain private metadata.
		return deny("PINNED_POLICY_READ_UNVERIFIED");
	}
}

export function readGitPolicy({
	repo,
	agent,
	expectedOrigin,
	includePrompt = false,
}) {
	if (!isAbsolute(repo ?? "")) return deny("ABSOLUTE_REPO_REQUIRED");
	const git = (...args) =>
		execFileSync("git", ["-C", repo, ...args], {
			encoding: "utf8",
			timeout: 15_000,
			maxBuffer: 1024 * 1024,
			stdio: ["ignore", "pipe", "pipe"],
			env: {
				...process.env,
				GIT_TERMINAL_PROMPT: "0",
				GIT_NO_LAZY_FETCH: "1",
				GIT_NO_REPLACE_OBJECTS: "1",
				GIT_OPTIONAL_LOCKS: "0",
			},
		});
	return collectGitPolicy({
		agent,
		expectedOrigin,
		includePrompt,
		readOrigin: () => git("remote", "get-url", "origin").trim(),
		readHeads: () => git("ls-remote", "--exit-code", "origin", ...refs),
		readBlob: (commit, path) => {
			const blob = git("rev-parse", "--verify", commit + ":" + path).trim();
			if (!oid(blob)) throw new Error("Invalid blob");
			return { blob, text: git("cat-file", "blob", blob) };
		},
	});
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
	const result = readGitPolicy({
		repo: process.argv[2],
		agent: process.argv[3],
		expectedOrigin: process.env.CHATQUEUE_BROKER_ORIGIN,
		includePrompt: process.env.CHATQUEUE_INCLUDE_PROMPT_BLOB === "1",
	});
	process.stdout.write(JSON.stringify(result) + "\n");
	// Even PASS_ONLY is not permission to click or arm a browser.
	process.exitCode = result.status === "GIT_POLICY_PASS_ONLY" ? 0 : 2;
}
