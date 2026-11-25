import test from "node:test";
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtempSync, mkdirSync, writeFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname } from "node:path";
import { randomUUID } from "node:crypto";
import {
	parsePulse,
	verifyOwnPulsePair,
} from "../firefox/chatqueue-pulse-proof.mjs";
import {
	validatePulseQuery,
	pulseProofMatches,
	sanitizePulseProof,
} from "../firefox/chatqueue-pulse-proof-schema.mjs";
import {
	newPausedState,
	authorizeFirstTick,
	prepareSend,
	sendPreflightVerified,
} from "../firefox/chatqueue-recurrence-core.mjs";
import { collectGitPolicy } from "../firefox/chatqueue-git-policy.mjs";

const role = "coordinator";
const path = "monitoring/agent-pulses/" + role + ".md";
const attempt = "cqop-" + randomUUID();
const metadata = ({
	agent = role,
	status = "PROGRESS",
	run = "fixture-run-new",
	operation = "fixture-operation-new",
	delivery = attempt,
	style = "plain",
} = {}) => {
	const fields = {
		agent_id: agent,
		[style === "bullet" ? "state" : "status"]: status,
		run_id: run,
		operation_id: operation,
		...(delivery === null ? {} : { browser_delivery_id: delivery }),
	};
	return (
		"# Fixture pulse\n\n" +
		Object.entries(fields)
			.map(([k, v]) =>
				style === "bullet" ? "- " + k + ": \x60" + v + "\x60" : k + ": " + v,
			)
			.join("\n") +
		"\n"
	);
};
function fixture(t, { prior = "PROGRESS", absent = false } = {}) {
	const root = mkdtempSync(join(tmpdir(), "pulse-proof-test-"));
	t.after(() => rmSync(root, { recursive: true, force: true }));
	const env = {
		...process.env,
		GIT_CONFIG_NOSYSTEM: "1",
		GIT_CONFIG_GLOBAL: "/dev/null",
		GIT_TERMINAL_PROMPT: "0",
		GIT_NO_REPLACE_OBJECTS: "1",
	};
	for (const k of [
		"GIT_DIR",
		"GIT_WORK_TREE",
		"GIT_INDEX_FILE",
		"GIT_COMMON_DIR",
		"GIT_OBJECT_DIRECTORY",
		"GIT_ALTERNATE_OBJECT_DIRECTORIES",
		"GIT_CONFIG_COUNT",
		"GIT_CONFIG_PARAMETERS",
	])
		delete env[k];
	const git = (...args) =>
		execFileSync("git", ["-C", root, ...args], {
			encoding: "utf8",
			env,
			timeout: 10000,
			maxBuffer: 1024 * 1024,
			stdio: ["ignore", "pipe", "pipe"],
		});
	git("init", "--quiet", "--initial-branch=main", "--template=");
	git("config", "user.name", "Fixture");
	git("config", "user.email", "fixture@example.invalid");
	git("config", "commit.gpgsign", "false");
	git("config", "core.hooksPath", join(root, "no-hooks-fixture"));
	const put = (p, text) => {
		const f = join(root, p);
		mkdirSync(dirname(f), { recursive: true });
		writeFileSync(f, text);
	};
	const commit = (files, label) => {
		for (const [p, text] of Object.entries(files)) put(p, text);
		git("add", "--", ...Object.keys(files));
		git("commit", "--quiet", "-m", label);
		return head();
	};
	const head = () => git("rev-parse", "HEAD").trim();
	const baseline = commit(
		{
			"fixture.txt": "baseline\n",
			...(absent
				? {}
				: {
						[path]: metadata({
							status: prior,
							run: "fixture-run-old",
							operation: "fixture-op-old",
							delivery: null,
						}),
					}),
		},
		"baseline",
	);
	const readBlob = (commit, p) => {
		const blob = git("rev-parse", "--verify", commit + ":" + p).trim();
		return { blob, text: git("cat-file", "blob", blob) };
	};
	const query = { attemptId: attempt, baselineMain: baseline };
	const verify = (q = query, main = head(), agent = role) =>
		verifyOwnPulsePair({ agent, main, query: q, git, readBlob });
	const started = (extra = {}, files = {}) =>
		commit(
			{ [path]: metadata({ status: "STARTED", ...extra }), ...files },
			"started",
		);
	const close = (extra = {}, files = {}) =>
		commit(
			{ [path]: metadata({ status: "PROGRESS", ...extra }), ...files },
			"terminal",
		);
	return {
		root,
		git,
		commit,
		head,
		baseline,
		query,
		verify,
		started,
		close,
		readBlob,
	};
}
test("exact metadata supports both existing pulse layouts and rejects ambiguous or unscoped identity", () => {
	for (const style of ["plain", "bullet"]) {
		const p = parsePulse(metadata({ style, status: "WAITING" }));
		assert.equal(p.agent_id, role);
		assert.equal(p.status, "WAITING");
		assert.equal(p.browser_delivery_id, attempt);
	}
	for (const text of [
		metadata() + "\nstatus: IDLE\n",
		metadata() + "\n- state: \x60IDLE\x60\n",
		metadata({ agent: "other" }),
		metadata({ status: "DONE" }),
		metadata({ delivery: "wrong" }),
		metadata().replace("run_id:", "other_run_id:"),
		metadata().replace("agent_id:", " agent_id:"),
	])
		assert.throws(() => parsePulse(text));
});
test("broker invocation admission is independent of external effects; LAB contract remains strict", () => {
	const state = authorizeFirstTick(
		newPausedState(role),
		{
			manualGo: true,
			threeLegacyGatesOff: true,
			exclusiveScheduler: true,
			targetVerified: true,
			accountVerified: true,
			bootstrapSameChatVerified: true,
			noOtherGeneration: true,
			recurrenceE2EPassed: true,
		},
		100,
	);
	const base = {
		identityVerified: true,
		notGenerating: true,
		threeLegacyGatesOff: true,
	};
	const broker = {
		...base,
		browserDeliveryReconciled: true,
		externalEffectDisposition: "AGENT_MUST_RECONCILE",
	};
	assert.equal(sendPreflightVerified(broker, "broker"), true);
	assert.equal(sendPreflightVerified(broker, "lab"), false);
	assert.equal(
		prepareSend(state, attempt, 100, broker, "broker").phase,
		"PREPARED",
	);
	assert.throws(() =>
		prepareSend(
			state,
			attempt,
			100,
			{ ...base, noUnreconciledEffects: true },
			"broker",
		),
	);
	assert.throws(() =>
		prepareSend(
			state,
			attempt,
			100,
			{ ...broker, noUnreconciledEffects: true },
			"broker",
		),
	);
	assert.throws(() =>
		prepareSend(
			state,
			attempt,
			100,
			{ ...broker, browserDeliveryReconciled: false },
			"broker",
		),
	);
	assert.throws(() =>
		prepareSend(
			state,
			attempt,
			100,
			{ ...broker, externalEffectDisposition: "VERIFIED" },
			"broker",
		),
	);
	assert.throws(() => prepareSend(state, attempt, 100, broker, "other"));
	assert.equal(
		prepareSend(state, attempt, 100, { ...base, noUnreconciledEffects: true })
			.phase,
		"PREPARED",
	);
});
test("actual Git pair proves exact own path, blobs, same run/op/correlation, and current HEAD despite peer commits", (t) => {
	const f = fixture(t);
	const start = f.started({ style: "bullet" });
	f.commit(
		{ "monitoring/agent-pulses/platformmanager.md": "peer fixture\n" },
		"peer",
	);
	const end = f.close({ status: "WAITING" });
	f.commit({ "fixture.txt": "unrelated after terminal\n" }, "unrelated");
	const proof = f.verify();
	assert.ok(proof);
	assert.equal(proof.startedCommit, start);
	assert.equal(proof.terminalCommit, end);
	assert.equal(proof.main, f.head());
	assert.equal(proof.startedBlob, f.readBlob(start, path).blob);
	assert.equal(proof.terminalBlob, f.readBlob(end, path).blob);
	assert.equal(proof.runId, "fixture-run-new");
	assert.equal(proof.operationId, "fixture-operation-new");
	assert.equal(proof.terminalStatus, "WAITING");
	assert.equal(proof.providerEffectsVerified, false);
	assert.ok(pulseProofMatches(proof, role, f.query, f.head()));
});
test("pinned earlier supervised pair survives later own pulses only if exact ancestry is still verified", (t) => {
	const f = fixture(t);
	f.started();
	const end = f.close();
	f.started({
		run: "fixture-run-later",
		operation: "fixture-op-later",
		delivery: "cqop-" + randomUUID(),
	});
	assert.equal(f.verify(), null);
	const proof = f.verify({ ...f.query, terminalCommit: end });
	assert.equal(proof.terminalCommit, end);
	assert.equal(proof.main, f.head());
	assert.equal(f.verify({ ...f.query, terminalCommit: f.head() }), null);
	f.git("checkout", "--quiet", "--detach", f.baseline);
	assert.equal(f.verify({ ...f.query, terminalCommit: end }), null);
});
test("missing terminal, missing correlation, mixed run/op and mismatched role deny", async (t) => {
	for (const [name, start, close] of [
		["missing terminal", {}, null],
		["missing STARTED correlation", { delivery: null }, {}],
		["missing terminal correlation", {}, { delivery: null }],
		["wrong terminal correlation", {}, { delivery: "cqop-" + randomUUID() }],
		["different run", {}, { run: "different-run" }],
		["different op", {}, { operation: "different-op" }],
		["wrong role", {}, { agent: "businessmanager" }],
		["nonterminal", {}, { status: "STARTED", style: "bullet" }],
		["old run", { run: "fixture-run-old" }, { run: "fixture-run-old" }],
	])
		await t.test(name, (t) => {
			const f = fixture(t);
			f.started(start);
			if (close) f.close(close);
			assert.equal(f.verify(), null);
		});
});
test("reject mixed-path commits, intervening own changes, old baseline and stale unclosed STARTED", async (t) => {
	await t.test("mixed STARTED diff", (t) => {
		const f = fixture(t);
		f.started({}, { "fixture.txt": "mixed" });
		f.close();
		assert.equal(f.verify(), null);
	});
	await t.test("mixed terminal diff", (t) => {
		const f = fixture(t);
		f.started();
		f.close({}, { "fixture.txt": "mixed" });
		assert.equal(f.verify(), null);
	});
	await t.test("intervening own change", (t) => {
		const f = fixture(t);
		f.started();
		f.commit(
			{ [path]: metadata({ status: "STARTED", style: "bullet" }) },
			"own extra",
		);
		f.close();
		assert.equal(f.verify(), null);
	});
	await t.test("baseline after start", (t) => {
		const f = fixture(t);
		const s = f.started();
		f.close();
		assert.equal(f.verify({ ...f.query, baselineMain: s }), null);
	});
	await t.test("old run unclosed", (t) => {
		const f = fixture(t, { prior: "STARTED" });
		f.started();
		f.close();
		assert.equal(f.verify(), null);
	});
	await t.test("first pulse absent", (t) => {
		const f = fixture(t, { absent: true });
		f.started();
		f.close({ status: "IDLE" });
		assert.equal(f.verify().terminalStatus, "IDLE");
	});
});
test("query and returned evidence are strictly scoped and sanitized", (t) => {
	const f = fixture(t);
	f.started();
	f.close();
	const p = f.verify();
	assert.ok(validatePulseQuery(f.query));
	assert.ok(p);
	for (const q of [
		{ ...f.query, path: "anything" },
		{ ...f.query, baselineMain: "--all" },
		{ ...f.query, attemptId: "other" },
		null,
	])
		assert.equal(validatePulseQuery(q), false);
	assert.equal(
		sanitizePulseProof({ ...p, raw: "not returned" }).raw,
		undefined,
	);
	assert.equal(
		sanitizePulseProof({ ...p, providerEffectsVerified: true }),
		null,
	);
	assert.equal(
		pulseProofMatches(
			p,
			role,
			{ ...f.query, attemptId: "cqop-" + randomUUID() },
			f.head(),
		),
		false,
	);
	assert.equal(pulseProofMatches(p, role, f.query, "f".repeat(40)), false);
});
test("collector verifies pulse within fresh remote HEAD envelope and rejects drift", (t) => {
	const f = fixture(t);
	f.started();
	f.close();
	const main = f.head(),
		runtime = "b".repeat(40),
		origin = "git@example.invalid:fixture/broker.git";
	const org = {
		mode: "FOUR_PERMANENT_AGENTS",
		max_permanent_agents: 4,
		permanent_agents: [
			"coordinator",
			"platformmanager",
			"businessmanager",
			"qualitymanager",
		].map((id) => ({ id, heartbeat_active: true, heartbeat_authorized: true })),
		activation: {
			allowed_to_run_heartbeat: true,
			chatqueue_production_heartbeat_resume_allowed: true,
			browser_scheduler: "CHATQUEUE_FOR_FIREFOX_ONLY",
			chatqueue_original_1_0_1_resume_allowed: false,
			chatqueue_lab_runtime_installed: false,
		},
	};
	let reads = 0,
		proofRead = false,
		drift = false;
	const args = {
		agent: role,
		expectedOrigin: origin,
		readOrigin: () => origin,
		pulseQuery: f.query,
		readHeads: () => {
			reads++;
			if (reads === 2) assert.ok(proofRead);
			return (
				(drift && reads === 2 ? "e".repeat(40) : main) +
				"\trefs/heads/main\n" +
				runtime +
				"\trefs/heads/scheduling-runtime\n"
			);
		},
		readBlob: (_c, p) => ({
			blob: "a".repeat(40),
			text: JSON.stringify(
				p === "coordination/organization.json" ? org : { enabled: false },
			),
		}),
		readPulseProof: (h, a, q) => {
			assert.equal(h, main);
			assert.equal(a, role);
			assert.equal(reads, 1);
			proofRead = true;
			return f.verify(q, h, a);
		},
	};
	assert.equal(collectGitPolicy(args).pulseProof.terminalCommit, main);
	reads = 0;
	proofRead = false;
	drift = true;
	const denied = collectGitPolicy(args);
	assert.equal(denied.status, "NO_GO");
	assert.equal(denied.pulseProof, undefined);
});
