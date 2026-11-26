import test from "node:test";
import assert from "node:assert/strict";
import { createHash, randomUUID } from "node:crypto";
import {
	AGENTS,
	REPLY_COOLDOWN_MS,
	newPausedState,
} from "../firefox/chatqueue-recurrence-core.mjs";
import { compileOperationalConfig } from "../firefox/chatqueue-operational-config.mjs";
import {
	validateOperationalConfig,
	parseOperationalTemplates,
	freshOperationalEvidence,
	freshPausedOperationalEvidence,
	makeOperationalPolicyClient,
	operationalMessage,
} from "../firefox/chatqueue-operational-client.mjs";
import { makeOperationalRuntime } from "../firefox/chatqueue-operational-runtime.mjs";
import { startPolicyBridge } from "../firefox/chatqueue-policy-bridge.mjs";
import { collectGitPolicy } from "../firefox/chatqueue-git-policy.mjs";

const names = [
	"Coordinator",
	"PlatformManager",
	"BusinessManager",
	"QualityManager",
];
const doc = AGENTS.map(
	(id, i) =>
		"## " +
		names[i] +
		"\n\n\`\`\`text\nONE tick ONLY as permanent " +
		names[i] +
		" (agent_id=" +
		id +
		") in THIS SAME chat; require " +
		id.toUpperCase() +
		"_BOOTSTRAP_READY. No lanes.\n\nFollow NON_BLOCKING_HEARTBEATS_V1 + NO_DEADLOCK_V1.\n\nReport exact refs; STOP. ChatQueue triggers the next tick.\n\`\`\`\n",
).join("\n");
const oid = createHash("sha1")
	.update("blob " + Buffer.byteLength(doc) + "\0")
	.update(doc)
	.digest("hex");
const blob = "a".repeat(40);
const digestFor = (agent) =>
	createHash("sha256")
		.update(parseOperationalTemplates(doc)[agent])
		.digest("hex");
const token = "b".repeat(64);
const paths = [
	"coordination/organization.json",
	"scheduling/state.json",
	"scheduling/watchdog-policy.json",
	"scheduling/runtime.json",
];
const config = () =>
	compileOperationalConfig({
		markdown: doc,
		promptBlob: oid,
		bindings: AGENTS.map((id) => ({
			agent: id,
			path: "/g/example-project/c/example-" + id,
			modelLabel: "6 Extra alto",
		})),
		bridge: { port: 12345, capability: token },
	});
const permitted = (agent, nonce, now) => ({
	agent,
	nonce,
	status: "GIT_POLICY_PASS_ONLY",
	runtimeAuthorized: false,
	reasons: [],
	promptBlob: oid,
	promptDigest: digestFor(agent),
	heads: { main: blob, runtime: blob },
	blobs: Object.fromEntries(paths.map((p) => [p, blob])),
	startedAt: now,
	observedAt: now,
});
const syntheticProof = (agent, query, main = blob) => ({
	status: "OWN_GIT_PAIR_VERIFIED",
	agent,
	attemptId: query.attemptId,
	baselineMain: query.baselineMain,
	main,
	startedCommit: "1".repeat(40),
	startedBlob: "2".repeat(40),
	terminalCommit: query.terminalCommit ?? "3".repeat(40),
	terminalBlob: "4".repeat(40),
	runId: "synthetic-run",
	operationId: "synthetic-operation",
	terminalStatus: "PROGRESS",
	providerEffectsVerified: false,
});
const attest = {
	manualGo: true,
	accountModelVerified: true,
	originalQueueRetired: true,
	qaVerified: true,
};
const defer = () => {
	let resolve;
	const promise = new Promise((r) => (resolve = r));
	return { promise, resolve };
};

test("canonical templates compile into four exact private bindings, always unreviewed", () => {
	const c = config();
	assert.equal(c.reviewed, false);
	assert.equal(validateOperationalConfig(c), true);
	const templates = parseOperationalTemplates(doc);
	for (const b of c.bindings) assert.equal(b.prompt, templates[b.agent]);
	assert.throws(() => parseOperationalTemplates(doc + "\n" + doc));
	assert.throws(() =>
		compileOperationalConfig({
			markdown: doc,
			promptBlob: blob,
			bindings: c.bindings,
			bridge: c.bridge,
		}),
	);
	assert.equal(
		validateOperationalConfig({
			...c,
			bindings: [...c.bindings.slice(0, 3), c.bindings[0]],
		}),
		false,
	);
	assert.equal(
		validateOperationalConfig({
			...c,
			bindings: c.bindings.map((b, i) =>
				i ? b : { ...b, path: "/g/another-project/c/example" },
			),
		}),
		false,
	);
	assert.equal(
		validateOperationalConfig({
			...c,
			bindings: c.bindings.map((b, i) =>
				i ? b : { ...b, modelLabel: "Instant" },
			),
		}),
		false,
	);
});
test("unique correlation preserves canonical prompt verbatim and is not a provider run ID", () => {
	const b = config().bindings[0],
		attempt = "cqop-" + randomUUID();
	const msg = operationalMessage(b, attempt);
	assert.ok(msg.startsWith(b.prompt + "\n\n"));
	assert.ok(msg.endsWith(attempt));
	assert.throws(() => operationalMessage(b, "other"));
});
test("fresh evidence requires exact nonce, role, prompt blob, heads/blobs, bounds and evidence-only authority", () => {
	const now = 100000,
		nonce = "c".repeat(32),
		v = permitted("coordinator", nonce, now);
	const ctx = {
		agent: "coordinator",
		nonce,
		promptBlob: oid,
		promptDigest: digestFor("coordinator"),
		requestedAt: now,
		now,
	};
	assert.equal(freshOperationalEvidence(v, ctx), true);
	for (const patch of [
		{ nonce: "d".repeat(32) },
		{ agent: "platformmanager" },
		{ promptBlob: blob },
		{ promptDigest: "f".repeat(64) },
		{ heads: {} },
		{ blobs: {} },
		{ runtimeAuthorized: true },
		{ status: "NO_GO" },
		{ reasons: ["PRODUCTION_NO_GO"] },
		{ observedAt: now + 1 },
		{ startedAt: now - 1 },
	])
		assert.equal(freshOperationalEvidence({ ...v, ...patch }, ctx), false);
	assert.equal(freshOperationalEvidence(v, { ...ctx, now: now + 5001 }), false);
});
test("policy client uses authenticated loopback, rejects redirects and errors, never caches evidence", async () => {
	let calls = 0,
		now = 1000;
	const c = config(),
		nonce = "c".repeat(32);
	const client = makeOperationalPolicyClient({
		clock: () => now,
		randomNonce: () => nonce,
		request: async (opts) => {
			calls++;
			assert.equal(opts.url, "http://127.0.0.1:12345/v1/evidence");
			assert.equal(opts.anonymous, true);
			assert.equal(opts.headers.Authorization, "Bearer " + token);
			assert.deepEqual(JSON.parse(opts.data), { agent: "coordinator", nonce });
			const finalUrl = calls === 2 ? "http://example.invalid/" : opts.url;
			if (calls === 3) throw new Error("transport failure");
			return {
				status: 200,
				finalUrl,
				responseText: JSON.stringify(permitted("coordinator", nonce, now)),
			};
		},
	});
	assert.ok(await client(c, "coordinator"));
	assert.equal(await client(c, "coordinator"), null);
	assert.equal(await client(c, "coordinator"), null);
	assert.equal(calls, 3);
});
test("Git policy optional prompt blob is read within the two HEAD checks", () => {
	let promptRead = false,
		headReads = 0;
	const origin = "git@example.invalid:example/broker.git";
	const org = {
		mode: "FOUR_PERMANENT_AGENTS",
		max_permanent_agents: 4,
		permanent_agents: AGENTS.map((id) => ({
			id,
			heartbeat_authorized: true,
			heartbeat_active: true,
		})),
		activation: {
			allowed_to_run_heartbeat: true,
			chatqueue_production_heartbeat_resume_allowed: true,
			browser_scheduler: "CHATQUEUE_FOR_FIREFOX_ONLY",
			chatqueue_original_1_0_1_resume_allowed: false,
			chatqueue_lab_runtime_installed: false,
		},
	};
	const args = {
		agent: "coordinator",
		expectedOrigin: origin,
		readOrigin: () => origin,
		includePrompt: true,
		readHeads: () => {
			headReads++;
			if (headReads === 2) assert.ok(promptRead);
			return (
				blob +
				"\trefs/heads/main\n" +
				blob +
				"\trefs/heads/scheduling-runtime\n"
			);
		},
		readBlob: (_head, path) => {
			if (path === "scheduling/prompts/permanent-four-agents-chatqueue.md") {
				promptRead = true;
				return { blob: oid, text: doc };
			}
			return {
				blob,
				text: JSON.stringify(path === paths[0] ? org : { enabled: false }),
			};
		},
	};
	const r = collectGitPolicy(args);
	assert.equal(r.status, "GIT_POLICY_PASS_ONLY");
	assert.equal(r.promptBlob, oid);
	assert.equal(r.promptDigest, digestFor("coordinator"));
	assert.equal(r.runtimeAuthorized, false);
	assert.equal(Object.keys(r.blobs).length, 4);
	assert.equal(
		collectGitPolicy({ ...args, readBlob: () => null }).status,
		"NO_GO",
	);
});
test("actual loopback bridge transports prompt pin; client denies NO_GO", async () => {
	let go = false,
		reads = 0;
	const bridge = await startPolicyBridge({
		capability: token,
		readPolicy: async (agent, query) => {
			reads++;
			const v = permitted(agent, "c".repeat(32), Date.now());
			const withProof = {
				...v,
				...(query ? { pulseProof: syntheticProof(agent, query) } : {}),
			};
			return go
				? withProof
				: {
						...withProof,
						status: "NO_GO",
						reasons: ["ROLE_PAUSED", "PRODUCTION_NO_GO"],
					};
		},
	});
	try {
		const c = config();
		c.bridge.port = bridge.port;
		const client = makeOperationalPolicyClient({
			clock: Date.now,
			randomNonce: () => randomUUID().replace(/-/g, ""),
			request: async (opts) => {
				const res = await fetch(opts.url, {
					method: opts.method,
					headers: opts.headers,
					body: opts.data,
					redirect: "error",
				});
				return {
					status: res.status,
					finalUrl: res.url,
					responseText: await res.text(),
				};
			},
		});
		assert.equal(await client(c, "coordinator"), null);
		const pausedQuery = {
			attemptId: "cqop-" + randomUUID(),
			baselineMain: blob,
		};
		const paused = await client(
			c,
			"coordinator",
			pausedQuery,
			"paused-recovery",
		);
		assert.equal(paused.value.status, "NO_GO");
		assert.equal(paused.value.pulseProof.attemptId, pausedQuery.attemptId);
		assert.equal(
			freshOperationalEvidence(paused.value, {
				...paused.context,
				now: Date.now(),
			}),
			false,
		);
		go = true;
		assert.ok(await client(c, "coordinator"));
		assert.equal(reads, 3);
		const q = {
			attemptId: "cqop-" + randomUUID(),
			baselineMain: blob,
			terminalCommit: "3".repeat(40),
		};
		const result = await client(c, "coordinator", q);
		assert.equal(result.value.pulseProof.attemptId, q.attemptId);
		assert.equal(result.value.pulseProof.providerEffectsVerified, false);
		c.bindings[0].prompt = c.bindings[0].prompt.replace(
			"No lanes.",
			"Edited body.",
		);
		assert.equal(await client(c, "coordinator"), null);
		assert.equal(reads, 5);
	} finally {
		await bridge.stop();
	}
});

function fixture(agent = "coordinator", shared) {
	const base = shared ?? {
		data: new Map(),
		locks: new Map(),
		config: { ...config(), reviewed: true },
		now: 1000,
		clicks: [],
		timers: new Map(),
		reads: 0,
		allow: true,
	};
	const path = base.config.bindings.find((b) => b.agent === agent).path;
	const key = (k) => agent + ":" + k;
	const copy = (v) => (v === undefined ? null : structuredClone(v));
	let draft = "",
		route = true,
		ready = true,
		receipt = false;
	const lock = async (name, ifAvailable, fn) => {
		const prior = base.locks.get(name);
		if (prior && ifAvailable) return { status: "SKIP_IN_PROGRESS" };
		const d = defer();
		base.locks.set(name, d.promise);
		try {
			if (prior) await prior;
			return await fn();
		} finally {
			if (base.locks.get(name) === d.promise) base.locks.delete(name);
			d.resolve();
		}
	};
	const dom = {
		ready: (b, text) => route && ready && b.path === path && draft === text,
		receiptCount: () => (receipt ? 1 : 0),
		compose: async (content) => {
			draft = content;
		},
		click: (b, content) => {
			assert.ok(dom.ready(b, content));
			const s = base.data.get(key("STATE")).state;
			assert.equal(s.phase, "PREPARED");
			assert.ok(content.endsWith(s.attemptId));
			const journal = base.data.get(key("SUPERVISED"));
			if (journal?.attemptId === s.attemptId) {
				assert.equal(journal.phase, "PREPARED");
				assert.equal(journal.baselineMain, blob);
			}
			base.clicks.push({ agent, content, at: base.now, id: s.attemptId });
			draft = "";
			receipt = true;
		},
		unsentSnapshot: (b, content, prior) => {
			const latest = base.clicks.filter((c) => c.agent === agent).at(-1);
			return route &&
				ready &&
				b.path === path &&
				!receipt &&
				latest?.content === prior &&
				(draft === content || draft === "")
				? {
						draft: draft === content ? "exact" : "empty",
						reply: "Synthetic terminal result",
					}
				: null;
		},
		clearUnsentDraft: async (b, content, prior) => {
			assert.equal(dom.unsentSnapshot(b, content, prior)?.draft, "exact");
			draft = "";
			base.clears = (base.clears ?? 0) + 1;
		},
		waitSent: async () => ({ sent: true }),
		terminalSnapshot: (b) =>
			route && ready && b.path === path && !draft
				? "Synthetic terminal result"
				: null,
		waitTerminal: async (_b, _c, paused) => {
			if (await paused()) throw new Error("paused");
			receipt = false;
			return {
				observedFinalAssistantResponse: true,
				noActiveGeneration: true,
				receiptText: "Synthetic terminal result",
			};
		},
	};
	const readEvidence = async (_cfg, id, query = null, purpose = "send") => {
		base.reads++;
		if (base.readHook) await base.readHook(base.reads);
		if (!base.allow && !(purpose === "paused-recovery" && base.pausedRecovery))
			return null;
		const nonce = randomUUID().replace(/-/g, "");
		return {
			value: {
				...permitted(id, nonce, base.now),
				peerRolesPaused: base.peersPaused === true,
				...(purpose === "paused-recovery" && base.pausedRecovery
					? { status: "NO_GO", reasons: ["ROLE_PAUSED", "PRODUCTION_NO_GO"] }
					: {}),
				...(query && base.proofAllowed === true
					? {
							pulseProof: base.proofOverride
								? base.proofOverride(id, query)
								: syntheticProof(id, query),
						}
					: {}),
			},
			context: {
				agent: id,
				nonce,
				promptBlob: oid,
				promptDigest: digestFor(id),
				requestedAt: base.now,
				pulseQuery: query,
			},
		};
	};
	const runtime = makeOperationalRuntime({
		agent,
		path,
		getConfig: async () => copy(base.config),
		read: async (k) => copy(base.data.get(key(k))),
		write: async (k, v) => {
			if (base.writeHook) await base.writeHook(k, v);
			base.data.set(key(k), copy(v));
		},
		lock,
		clock: () => base.now,
		randomId: randomUUID,
		setTimer: (fn, ms) => {
			const id = randomUUID();
			base.timers.set(id, { fn, due: base.now + ms, agent });
			return id;
		},
		cancelTimer: (id) => base.timers.delete(id),
		readEvidence,
		dom,
	});
	const fire = async () => {
		const pair = [...base.timers.entries()]
			.filter(([, t]) => t.agent === agent)
			.sort((a, b) => a[1].due - b[1].due)[0];
		assert.ok(pair, "timer expected");
		const [id, t] = pair;
		base.timers.delete(id);
		base.now = Math.max(base.now, t.due);
		await t.fn();
	};
	return {
		runtime,
		base,
		dom,
		fire,
		key,
		path,
		setRoute: (v) => (route = v),
		setReady: (v) => (ready = v),
	};
}

test("boot inert, unreviewed config, missing GO and real-policy denial do not arm", async () => {
	const f = fixture();
	await f.runtime.boot();
	assert.equal(f.base.timers.size, 0);
	assert.equal(f.base.reads, 0);
	assert.equal((await f.runtime.status()).phase, "PAUSED");
	f.base.config.reviewed = false;
	assert.equal(await f.runtime.arm("supervised", attest), false);
	f.base.config.reviewed = true;
	assert.equal(
		await f.runtime.arm("supervised", { ...attest, manualGo: false }),
		false,
	);
	f.base.allow = false;
	assert.equal(await f.runtime.arm("supervised", attest), false);
	assert.equal(f.base.clicks.length, 0);
	assert.equal(f.base.timers.size, 0);
});
test("four independent roles, one supervised tick each, exact durable attempt, no automatic recurrence", async () => {
	const fs = [fixture()];
	for (const id of AGENTS.slice(1)) fs.push(fixture(id, fs[0].base));
	for (const f of fs) {
		assert.equal(await f.runtime.boot(), true);
		assert.equal(await f.runtime.arm("supervised", attest), true);
	}
	for (const f of fs) {
		await f.fire();
		const status = await f.runtime.status();
		assert.equal(status.phase, "COOLDOWN");
		assert.equal(status.stopped, true);
		assert.equal(status.cycle, 1);
		assert.equal(status.supervised, true);
	}
	const base = fs[0].base;
	assert.equal(base.clicks.length, 4);
	assert.equal(base.timers.size, 0);
	assert.equal(base.reads, 12);
	assert.deepEqual(new Set(base.clicks.map((c) => c.agent)), new Set(AGENTS));
	assert.equal(new Set(base.clicks.map((c) => c.id)).size, 4);
});
test("recurrence requires independently read supervised Git proof; respects reply cooldown", async () => {
	const f = fixture();
	await f.runtime.boot();
	assert.equal(await f.runtime.arm("recurrence", attest), false);
	await f.runtime.arm("supervised", attest);
	await f.fire();
	// A true legacy human flag must not grant recurrence without a Git pair.
	assert.equal(
		await f.runtime.arm("recurrence", {
			...attest,
			supervisedGitProofVerified: true,
		}),
		false,
	);
	assert.equal(f.base.timers.size, 0);
	f.base.proofAllowed = true;
	for (const patch of [
		{ attemptId: "cqop-" + randomUUID() },
		{ baselineMain: "5".repeat(40) },
		{ agent: "platformmanager" },
		{ main: "6".repeat(40) },
		{ terminalStatus: "STARTED" },
		{ providerEffectsVerified: true },
		{ startedCommit: null },
	]) {
		f.base.proofOverride = (agent, q) => ({
			...syntheticProof(agent, q),
			...patch,
		});
		assert.equal(await f.runtime.arm("recurrence", attest), false);
		assert.equal(f.base.timers.size, 0);
	}
	delete f.base.proofOverride;
	assert.equal(await f.runtime.arm("recurrence", attest), true);
	const journal = f.base.data.get(f.key("SUPERVISED"));
	assert.equal(journal.gitProof.attemptId, f.base.clicks[0].id);
	assert.equal(journal.gitProof.providerEffectsVerified, false);
	assert.equal(journal.gitProof.baselineMain, blob);
	const first = f.base.clicks[0];
	await f.fire();
	await f.fire();
	assert.equal(f.base.clicks.length, 3);
	for (let i = 1; i < 3; i++)
		assert.ok(
			f.base.clicks[i].at - f.base.clicks[i - 1].at >= REPLY_COOLDOWN_MS,
		);
	assert.equal(f.base.timers.size, 1);
	await f.runtime.pause();
	assert.equal(f.base.timers.size, 0);
	assert.equal((await f.runtime.status()).stopped, true);
	assert.ok(first.id);
});
test("fresh final policy denial after PREPARED preserves original attempt, no click/replay", async () => {
	const f = fixture();
	await f.runtime.boot();
	await f.runtime.arm("supervised", attest);
	f.base.readHook = async (n) => {
		if (n === 3) f.base.allow = false;
	};
	await f.fire();
	const s = await f.runtime.status();
	assert.equal(s.phase, "REVIEW");
	assert.equal(s.stopped, true);
	assert.ok(s.attemptId);
	assert.equal(f.base.clicks.length, 0);
	assert.equal(f.base.timers.size, 0);
	const dup = fixture("coordinator", f.base);
	await dup.runtime.boot();
	assert.equal((await dup.runtime.status()).attemptId, s.attemptId);
	assert.equal(await dup.runtime.arm("supervised", attest), false);
});
test("pause during final remote read fences old lease and preserves PREPARED", async () => {
	const f = fixture();
	await f.runtime.boot();
	await f.runtime.arm("supervised", attest);
	const entered = defer(),
		release = defer();
	f.base.readHook = async (n) => {
		if (n === 3) {
			entered.resolve();
			await release.promise;
		}
	};
	const running = f.fire();
	await entered.promise;
	const original = f.base.data.get(f.key("STATE")).state.attemptId;
	const dup = fixture("coordinator", f.base);
	await dup.runtime.pause();
	assert.equal((await dup.runtime.status()).stopped, true);
	release.resolve();
	await running;
	assert.equal(f.base.clicks.length, 0);
	assert.equal((await f.runtime.status()).phase, "REVIEW");
	assert.equal((await f.runtime.status()).attemptId, original);
});
test("click before pause may have occurred; duplicate reload does not replay or erase attempt", async () => {
	const f = fixture();
	await f.runtime.boot();
	await f.runtime.arm("supervised", attest);
	const entered = defer(),
		release = defer();
	f.dom.waitTerminal = async (_b, _c, paused) => {
		entered.resolve();
		await release.promise;
		if (await paused()) throw new Error("paused");
		return { observedFinalAssistantResponse: true, noActiveGeneration: true };
	};
	const running = f.fire();
	await entered.promise;
	const original = f.base.clicks[0].id;
	const dup = fixture("coordinator", f.base);
	assert.equal(await dup.runtime.boot(), false);
	release.resolve();
	await running;
	assert.equal(f.base.clicks.length, 1);
	assert.equal((await dup.runtime.status()).phase, "REVIEW");
	assert.equal((await dup.runtime.status()).attemptId, original);
	assert.equal(await dup.runtime.arm("supervised", attest), false);
});
test("stale timer, route drift and config replacement cannot click", async () => {
	for (const scenario of ["stale-timer", "route", "config"]) {
		const f = fixture();
		await f.runtime.boot();
		await f.runtime.arm("supervised", attest);
		const t = [...f.base.timers.values()][0];
		if (scenario === "stale-timer") {
			await f.runtime.pause();
			await t.fn();
		} else {
			if (scenario === "route") f.setRoute(false);
			else f.base.config = { ...f.base.config, reviewed: false };
			await f.fire();
		}
		assert.equal(f.base.clicks.length, 0, scenario);
	}
});
test("malformed durable state and changed role binding fail closed without erasing evidence", async () => {
	for (const state of [
		{ ...newPausedState("coordinator"), cycle: -1 },
		{ ...newPausedState("coordinator"), attemptId: "unexpected" },
	]) {
		const f = fixture();
		const original = { path: f.path, state };
		f.base.data.set(f.key("STATE"), structuredClone(original));
		await assert.rejects(f.runtime.boot());
		assert.deepEqual(f.base.data.get(f.key("STATE")), original);
		assert.equal(f.base.clicks.length, 0);
	}
	const f = fixture();
	const original = {
		path: "/g/example-project/c/old",
		state: newPausedState("coordinator"),
	};
	f.base.data.set(f.key("STATE"), structuredClone(original));
	await assert.rejects(f.runtime.boot());
	assert.deepEqual(f.base.data.get(f.key("STATE")), original);
});

test("recurrence rearm independently rechecks pinned proof; loss of proof before click blocks replay", async () => {
	const f = fixture();
	await f.runtime.boot();
	await f.runtime.arm("supervised", attest);
	await f.fire();
	f.base.proofAllowed = true;
	assert.equal(await f.runtime.arm("recurrence", attest), true);
	const terminal = f.base.data.get(f.key("SUPERVISED")).gitProof.terminalCommit;
	await f.fire();
	await f.runtime.pause();
	let queried;
	f.base.proofOverride = (agent, q) => {
		queried = q;
		return syntheticProof(agent, q);
	};
	assert.equal(await f.runtime.arm("recurrence", attest), true);
	assert.equal(queried.terminalCommit, terminal);
	const before = f.base.clicks.length;
	f.base.proofAllowed = false;
	await f.fire();
	assert.equal(f.base.clicks.length, before);
	assert.equal((await f.runtime.status()).stopped, true);
});
test("legacy uncorrelated supervised record cannot grant recurrence", async () => {
	const f = fixture();
	await f.runtime.boot();
	await f.runtime.arm("supervised", attest);
	await f.fire();
	const record = f.base.data.get(f.key("SUPERVISED"));
	delete record.baselineMain;
	delete record.phase;
	f.base.proofAllowed = true;
	assert.equal(await f.runtime.arm("recurrence", attest), false);
	assert.equal(f.base.timers.size, 0);
});

const recoveryAttest = {
	manualRecovery: true,
	accountModelVerified: true,
	oldDocumentsRetired: true,
	qaVerified: true,
};

test("paused evidence is disjoint from send admission and requires exact closed gates, pins and freshness", () => {
	const now = 10000,
		nonce = "c".repeat(32);
	const ctx = {
		agent: "platformmanager",
		nonce,
		promptBlob: oid,
		promptDigest: digestFor("platformmanager"),
		requestedAt: now,
		now,
	};
	const value = {
		...permitted(ctx.agent, nonce, now),
		status: "NO_GO",
		reasons: ["ROLE_PAUSED", "PRODUCTION_NO_GO"],
	};
	assert.equal(freshPausedOperationalEvidence(value, ctx), true);
	assert.equal(freshOperationalEvidence(value, ctx), false);
	for (const patch of [
		{ status: "GIT_POLICY_PASS_ONLY", reasons: [] },
		{ reasons: ["PRODUCTION_NO_GO"] },
		{
			reasons: [
				"ROLE_PAUSED",
				"PRODUCTION_NO_GO",
				"LEGACY_RUNTIME_GATE_NOT_OFF",
			],
		},
		{ reasons: ["ROLE_PAUSED", "ROLE_PAUSED"] },
		{ heads: {} },
		{ blobs: {} },
		{ promptDigest: "a".repeat(64) },
		{ nonce: "d".repeat(32) },
		{ runtimeAuthorized: true },
		{ agent: "businessmanager" },
	])
		assert.equal(
			freshPausedOperationalEvidence({ ...value, ...patch }, ctx),
			false,
		);
	assert.equal(
		freshPausedOperationalEvidence(value, { ...ctx, now: now + 5001 }),
		false,
	);
});

async function reviewedFixture() {
	const f = fixture("platformmanager");
	await f.runtime.boot();
	const terminal = f.dom.waitTerminal;
	f.dom.waitTerminal = async () => {
		throw new Error("Virtualized receipt missing");
	};
	await f.runtime.arm("supervised", attest);
	await f.fire();
	f.dom.waitTerminal = terminal;
	f.base.allow = false;
	f.base.pausedRecovery = true;
	f.base.proofAllowed = true;
	assert.equal((await f.runtime.status()).phase, "REVIEW");
	assert.equal(f.base.clicks.length, 1);
	return f;
}

test("exact supervised REVIEW reconciles under closed gates without a second send or arm", async () => {
	const f = await reviewedFixture();
	const original = structuredClone(f.base.data.get(f.key("STATE")).state);
	const gate = structuredClone(f.base.data.get(f.key("GATE")));
	assert.equal(
		await f.runtime.reconcileSupervisedReview({
			...recoveryAttest,
			manualRecovery: false,
		}),
		false,
	);
	assert.equal(await f.runtime.arm("recurrence", attest), false);
	f.base.proofAllowed = false;
	assert.equal(
		await f.runtime.reconcileSupervisedReview(recoveryAttest),
		false,
	);
	f.base.proofAllowed = true;
	f.base.proofOverride = (agent, query) => ({
		...syntheticProof(agent, query),
		attemptId: "cqop-" + randomUUID(),
	});
	assert.equal(
		await f.runtime.reconcileSupervisedReview(recoveryAttest),
		false,
	);
	delete f.base.proofOverride;
	const terminal = f.dom.waitTerminal;
	f.dom.waitTerminal = async () => ({
		observedFinalAssistantResponse: true,
		noActiveGeneration: true,
	});
	assert.equal(
		await f.runtime.reconcileSupervisedReview(recoveryAttest),
		false,
	);
	f.dom.waitTerminal = terminal;
	f.dom.terminalSnapshot = () => "Changed reply";
	assert.equal(
		await f.runtime.reconcileSupervisedReview(recoveryAttest),
		false,
	);
	f.dom.terminalSnapshot = () => "Synthetic terminal result";
	assert.deepEqual(f.base.data.get(f.key("STATE")).state, original);
	assert.equal(await f.runtime.reconcileSupervisedReview(recoveryAttest), true);
	const s = await f.runtime.status();
	const journal = f.base.data.get(f.key("SUPERVISED"));
	assert.equal(s.phase, "PAUSED");
	assert.equal(s.cycle, 1);
	assert.equal(s.attemptId, null);
	assert.equal(s.stopped, true);
	assert.equal(s.leased, false);
	assert.equal(s.supervisedGitProofStored, true);
	assert.equal(journal.attemptId, original.attemptId);
	assert.equal(journal.gitProof.providerEffectsVerified, false);
	assert.deepEqual(journal.reconciledReview.previousState, original);
	assert.deepEqual(f.base.data.get(f.key("GATE")), gate);
	assert.equal(f.base.timers.size, 0);
	assert.equal(f.base.clicks.length, 1);
	assert.equal(
		await f.runtime.reconcileSupervisedReview(recoveryAttest),
		false,
	);
	assert.equal(await f.runtime.arm("recurrence", attest), false);
});

test("partial reconciliation preserves original REVIEW and retries only its pinned pair", async () => {
	const f = await reviewedFixture();
	const original = structuredClone(f.base.data.get(f.key("STATE")).state);
	f.base.writeHook = async (key, value) => {
		if (key === "STATE" && value.state.phase === "PAUSED")
			throw new Error("Synthetic interrupted STATE write");
	};
	await assert.rejects(f.runtime.reconcileSupervisedReview(recoveryAttest));
	assert.deepEqual(f.base.data.get(f.key("STATE")).state, original);
	const saved = structuredClone(f.base.data.get(f.key("SUPERVISED")));
	assert.equal(saved.phase, "REPLY_TERMINAL");
	assert.equal(saved.attemptId, original.attemptId);
	delete f.base.writeHook;
	f.base.proofOverride = (agent, query) => ({
		...syntheticProof(agent, query),
		startedCommit: "5".repeat(40),
	});
	assert.equal(
		await f.runtime.reconcileSupervisedReview(recoveryAttest),
		false,
	);
	delete f.base.proofOverride;
	assert.equal(await f.runtime.reconcileSupervisedReview(recoveryAttest), true);
	assert.deepEqual(f.base.data.get(f.key("SUPERVISED")), saved);
	assert.equal((await f.runtime.status()).phase, "PAUSED");
	assert.equal(f.base.clicks.length, 1);
	assert.equal(f.base.timers.size, 0);
});

test("a config change during paused recovery keeps exact attempt in REVIEW", async () => {
	const f = await reviewedFixture();
	const original = structuredClone(f.base.data.get(f.key("STATE")));
	f.base.readHook = async () => {
		f.base.config.reviewed = false;
	};
	assert.equal(
		await f.runtime.reconcileSupervisedReview(recoveryAttest),
		false,
	);
	assert.deepEqual(f.base.data.get(f.key("STATE")), original);
	assert.equal(f.base.clicks.length, 1);
	assert.equal(f.base.timers.size, 0);
});

async function rolloverFixture() {
	const f = fixture();
	await f.runtime.boot();
	const previous = structuredClone(f.base.data.get(f.key("STATE")));
	f.base.config.reviewed = false;
	f.base.config.bindings[0].path = "/g/example-project/c/coordinator-successor";
	f.base.allow = false;
	f.base.pausedRecovery = true;
	const successor = fixture("coordinator", f.base);
	await assert.rejects(successor.runtime.boot());
	return { f: successor, previous };
}

test("explicit pristine paused rollover preserves state, changes only binding, never arms", async () => {
	const { f, previous } = await rolloverFixture();
	assert.equal((await f.runtime.status()).bindingMigrationRequired, true);
	assert.equal(
		await f.runtime.migratePristineBinding({
			...recoveryAttest,
			oldDocumentsRetired: false,
		}),
		false,
	);
	f.base.pausedRecovery = false;
	assert.equal(await f.runtime.migratePristineBinding(recoveryAttest), false);
	f.base.pausedRecovery = true;
	assert.deepEqual(f.base.data.get(f.key("STATE")), previous);
	assert.equal(await f.runtime.migratePristineBinding(recoveryAttest), true);
	assert.deepEqual(f.base.data.get(f.key("STATE")), {
		path: f.path,
		state: previous.state,
	});
	const status = await f.runtime.status();
	assert.equal(status.bindingMigrationRequired, false);
	assert.equal(status.phase, "PAUSED");
	assert.equal(status.stopped, true);
	assert.equal(status.leased, false);
	assert.equal(status.cycle, 0);
	assert.equal(f.base.clicks.length, 0);
	assert.equal(f.base.timers.size, 0);
	assert.equal(await f.runtime.arm("supervised", attest), false);
});

test("rollover rejects historical cycles, supervised journal, review, wrong project and reviewed config", async () => {
	for (const scenario of [
		"cycle",
		"journal",
		"review",
		"project",
		"reviewed",
		"model",
		"unknown-envelope",
	]) {
		const { f, previous } = await rolloverFixture();
		const candidate = structuredClone(previous);
		if (scenario === "cycle") candidate.state.cycle = 1;
		if (scenario === "journal")
			f.base.data.set(f.key("SUPERVISED"), {
				attemptId: "cqop-" + randomUUID(),
			});
		if (scenario === "review") {
			candidate.state.phase = "REVIEW";
			candidate.state.attemptId = "cqop-" + randomUUID();
		}
		if (scenario === "project") candidate.path = "/g/other-project/c/old";
		if (scenario === "reviewed") f.base.config.reviewed = true;
		if (scenario === "model") f.setReady(false);
		if (scenario === "unknown-envelope")
			candidate.history = ["synthetic prior record"];
		f.base.data.set(f.key("STATE"), candidate);
		assert.equal(
			await f.runtime.migratePristineBinding(recoveryAttest),
			false,
			scenario,
		);
		assert.deepEqual(f.base.data.get(f.key("STATE")), candidate);
		assert.equal(f.base.clicks.length, 0);
		assert.equal(f.base.timers.size, 0);
	}
});

const preclickAttest = {
	...recoveryAttest,
	precisePreclickSourceVerified: true,
};

async function preclickFixture() {
	const f = fixture("businessmanager");
	await f.runtime.boot();
	await f.runtime.arm("supervised", attest);
	await f.fire();
	f.base.proofAllowed = true;
	await f.runtime.arm("recurrence", attest);
	const denyAt = f.base.reads + 2;
	f.base.readHook = async (n) => {
		if (n >= denyAt) f.base.allow = false;
	};
	await f.fire();
	delete f.base.readHook;
	f.base.pausedRecovery = true;
	const state = f.base.data.get(f.key("STATE")).state;
	assert.equal(state.phase, "REVIEW");
	assert.match(state.pendingReason, /Final Git policy not proven/);
	assert.equal(f.base.clicks.length, 1);
	const q = { attemptId: f.base.clicks[0].id, baselineMain: blob };
	return { f, state: structuredClone(state), q };
}

test("one fresh pre-click reread can recover a transient denial without recomposition or duplicate click", async () => {
	const f = fixture();
	await f.runtime.boot();
	await f.runtime.arm("supervised", attest);
	let composed = 0;
	const compose = f.dom.compose;
	f.dom.compose = async (...a) => {
		composed++;
		return compose(...a);
	};
	f.base.readHook = async (n) => {
		if (n === 3) f.base.allow = false;
		if (n === 4) f.base.allow = true;
	};
	await f.fire();
	assert.equal(f.base.reads, 4);
	assert.equal(composed, 1);
	assert.equal(f.base.clicks.length, 1);
	assert.equal((await f.runtime.status()).phase, "COOLDOWN");
});

test("precise pre-click REVIEW recovery preserves full original state and prior proof, clears exact draft only, no click", async () => {
	const { f, state, q } = await preclickFixture();
	const journal = structuredClone(f.base.data.get(f.key("SUPERVISED")));
	const gate = structuredClone(f.base.data.get(f.key("GATE")));
	assert.equal(
		await f.runtime.reconcilePreclickReview(recoveryAttest, q),
		false,
	);
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, {
			...q,
			terminalCommit: "3".repeat(40),
		}),
		false,
	);
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, {
			...q,
			attemptId: state.attemptId,
		}),
		false,
	);
	f.base.pausedRecovery = false;
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, q),
		false,
	);
	f.base.pausedRecovery = true;
	const snapshot = f.dom.unsentSnapshot;
	f.dom.unsentSnapshot = () => null;
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, q),
		false,
	);
	f.dom.unsentSnapshot = snapshot;
	f.base.proofOverride = (agent, query) => ({
		...syntheticProof(agent, query),
		attemptId: "cqop-" + randomUUID(),
	});
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, q),
		false,
	);
	delete f.base.proofOverride;
	assert.deepEqual(f.base.data.get(f.key("STATE")).state, state);
	assert.equal(f.base.clears ?? 0, 0);
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, q),
		true,
	);
	const after = f.base.data.get(f.key("STATE")).state;
	assert.equal(after.phase, "PAUSED");
	assert.equal(after.cycle, state.cycle);
	assert.equal(after.attemptId, null);
	assert.equal(after.lastReplyAt, state.lastReplyAt);
	assert.equal(after.nextEligibleAt, null);
	assert.equal(after.preclickReviews.length, 1);
	assert.equal(after.preclickReviews[0].phase, "CANCELLED_BEFORE_CLICK");
	assert.deepEqual(after.preclickReviews[0].previousState, state);
	assert.equal(after.preclickReviews[0].priorGitProof.attemptId, q.attemptId);
	assert.equal(
		after.preclickReviews[0].priorGitProof.providerEffectsVerified,
		false,
	);
	assert.deepEqual(f.base.data.get(f.key("SUPERVISED")), journal);
	assert.deepEqual(f.base.data.get(f.key("GATE")), gate);
	assert.equal(f.base.clicks.length, 1);
	assert.equal(f.base.clears, 1);
	assert.equal(f.base.timers.size, 0);
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, q),
		false,
	);
	assert.equal(await f.runtime.arm("recurrence", attest), false);
});

test("pre-click recovery writes pending intent before draft clear and retries only same proof after interrupted final state write", async () => {
	const { f, state, q } = await preclickFixture();
	const clear = f.dom.clearUnsentDraft;
	f.dom.clearUnsentDraft = async (...args) => {
		const staged = f.base.data.get(f.key("STATE")).state;
		assert.equal(staged.phase, "REVIEW");
		assert.equal(staged.preclickReviews[0].phase, "DRAFT_CLEAR_PENDING");
		assert.deepEqual(staged.preclickReviews[0].previousState, state);
		return clear(...args);
	};
	f.base.writeHook = async (key, value) => {
		if (key === "STATE" && value.state.phase === "PAUSED")
			throw new Error("Synthetic interrupted finalize");
	};
	await assert.rejects(f.runtime.reconcilePreclickReview(preclickAttest, q));
	assert.equal(f.base.clears, 1);
	assert.equal((await f.runtime.status()).attemptId, state.attemptId);
	delete f.base.writeHook;
	f.base.proofOverride = (agent, query) => ({
		...syntheticProof(agent, query),
		terminalCommit: "5".repeat(40),
	});
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, q),
		false,
	);
	delete f.base.proofOverride;
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, q),
		true,
	);
	assert.equal(f.base.clears, 1);
	assert.equal(f.base.clicks.length, 1);
	assert.equal((await f.runtime.status()).phase, "PAUSED");
});

test("non-preclick REVIEW is never cancelled even with same draft or proof", async () => {
	const { f, state, q } = await preclickFixture();
	const altered = {
		...state,
		pendingReason:
			"Reconcile exact attempt " +
			state.attemptId +
			" before any retry (No exact user receipt)",
	};
	f.base.data.set(f.key("STATE"), { path: f.path, state: altered });
	assert.equal(
		await f.runtime.reconcilePreclickReview(preclickAttest, q),
		false,
	);
	assert.deepEqual(f.base.data.get(f.key("STATE")).state, altered);
	assert.equal(f.base.clears ?? 0, 0);
	assert.equal(f.base.clicks.length, 1);
});

const historyAttest = { ...recoveryAttest, oldDocumentsRetired: true };
async function historyFixture() {
	const old = fixture("platformmanager");
	await old.runtime.boot();
	const s = old.base.data.get(old.key("STATE")).state;
	const first = {
		attemptId: "cqop-" + randomUUID(),
		baselineMain: blob,
		terminalCommit: "3".repeat(40),
	};
	const latest = {
		attemptId: "cqop-" + randomUUID(),
		baselineMain: "5".repeat(40),
		terminalCommit: "8".repeat(40),
	};
	const proofFor = (id, query) => {
		if (query.attemptId === first.attemptId) return syntheticProof(id, first);
		if (query.attemptId === latest.attemptId)
			return {
				...syntheticProof(id, latest),
				startedCommit: "6".repeat(40),
				startedBlob: "7".repeat(40),
				terminalBlob: "9".repeat(40),
				runId: "latest-run",
				operationId: "latest-op",
				terminalStatus: "WAITING",
			};
		return null;
	};
	const previous = {
		path: old.path,
		state: {
			...s,
			cycle: 11,
			lastReplyAt: 900,
			preservedUnknown: { attempt: "synthetic-unknown", status: "UNKNOWN" },
		},
	};
	const journal = {
		phase: "REPLY_TERMINAL",
		path: old.path,
		promptBlob: oid,
		cycle: 1,
		attemptId: first.attemptId,
		baselineMain: first.baselineMain,
		lastReplyAt: 500,
		gitProof: proofFor("platformmanager", first),
		reconciledReview: { synthetic: true },
	};
	old.base.data.set(old.key("STATE"), structuredClone(previous));
	old.base.data.set(old.key("SUPERVISED"), structuredClone(journal));
	old.base.config.reviewed = false;
	old.base.config.bindings.find((b) => b.agent === "platformmanager").path =
		"/g/example-project/c/platformmanager-successor";
	old.base.allow = false;
	old.base.pausedRecovery = true;
	old.base.proofAllowed = true;
	old.base.proofOverride = proofFor;
	old.base.peersPaused = true;
	const f = fixture("platformmanager", old.base);
	const gate = structuredClone(f.base.data.get(f.key("GATE")));
	return { f, previous, journal, gate, latest, first, proofFor };
}

test("PM historical binding migration preserves cycle 11, full original envelope, journal and peer state; no send or arm", async () => {
	const { f, previous, journal, gate, latest } = await historyFixture();
	f.base.data.set("businessmanager:STATE", {
		phase: "REVIEW",
		opaque: "preserve",
	});
	const peer = structuredClone(f.base.data.get("businessmanager:STATE"));
	assert.equal(await f.runtime.migratePristineBinding(historyAttest), false);
	assert.equal(
		await f.runtime.migrateHistoricalBinding(historyAttest, latest),
		true,
	);
	const now = f.base.data.get(f.key("STATE"));
	assert.equal(now.path, f.path);
	assert.equal(now.state.cycle, 11);
	assert.equal(now.state.lastReplyAt, previous.state.lastReplyAt);
	assert.deepEqual(now.state.historyMigration.previous, previous);
	assert.deepEqual(now.state.preservedUnknown, previous.state.preservedUnknown);
	assert.deepEqual(f.base.data.get(f.key("SUPERVISED")), journal);
	assert.deepEqual(f.base.data.get(f.key("GATE")), gate);
	assert.deepEqual(f.base.data.get("businessmanager:STATE"), peer);
	assert.equal(
		await f.runtime.migrateHistoricalBinding(historyAttest, latest),
		true,
	);
	assert.deepEqual(f.base.data.get(f.key("STATE")), now);
	assert.equal(f.base.clicks.length, 0);
	assert.equal(f.base.timers.size, 0);
});

test("PM historical migration denies stale or wrong proof, non-idle DOM, active peers, old document or changed config", async () => {
	for (const mode of [
		"attestation",
		"dom",
		"peers",
		"proof",
		"newer",
		"review",
		"wrong-project",
		"duplicate",
		"reviewed",
		"prompt",
		"lock",
		"head-change",
	]) {
		const { f, previous, journal, latest, proofFor } = await historyFixture();
		let a = historyAttest,
			q = latest;
		if (mode === "attestation") a = { ...a, oldDocumentsRetired: false };
		if (mode === "dom") f.setReady(false);
		if (mode === "peers") f.base.peersPaused = false;
		if (mode === "proof") f.base.proofAllowed = false;
		if (mode === "newer") q = { ...latest, terminalCommit: "e".repeat(40) };
		if (mode === "review") {
			const s = f.base.data.get(f.key("STATE")).state;
			s.phase = "REVIEW";
			s.attemptId = latest.attemptId;
		}
		if (mode === "wrong-project")
			f.base.data.get(f.key("STATE")).path = "/g/other-project/c/old";
		if (mode === "duplicate") f.base.config.bindings[0].path = previous.path;
		if (mode === "reviewed") f.base.config.reviewed = true;
		if (mode === "prompt") f.base.config.promptBlob = "e".repeat(40);
		if (mode === "lock")
			f.base.locks.set("CQOP_V1:ROLE:platformmanager", Promise.resolve());
		if (mode === "head-change")
			f.base.proofOverride = (id, query) => ({
				...proofFor(id, query),
				main: "e".repeat(40),
			});
		const before = structuredClone(f.base.data.get(f.key("STATE")));
		assert.equal(await f.runtime.migrateHistoricalBinding(a, q), false, mode);
		assert.deepEqual(f.base.data.get(f.key("STATE")), before, mode);
		assert.deepEqual(f.base.data.get(f.key("SUPERVISED")), journal, mode);
		assert.equal(f.base.clicks.length, 0, mode);
	}
});

test("PM historical migration resumes only exact witnessed pending transaction", async () => {
	const { f, latest, journal } = await historyFixture();
	f.base.writeHook = async (key, value) => {
		if (key === "STATE" && value.path === f.path)
			throw new Error("interrupted final migration");
	};
	await assert.rejects(
		f.runtime.migrateHistoricalBinding(historyAttest, latest),
	);
	const staged = structuredClone(f.base.data.get(f.key("STATE")));
	assert.equal(staged.state.historyMigration.phase, "PREPARED");
	delete f.base.writeHook;
	assert.equal(
		await f.runtime.migrateHistoricalBinding(historyAttest, {
			...latest,
			terminalCommit: "e".repeat(40),
		}),
		false,
	);
	assert.deepEqual(f.base.data.get(f.key("STATE")), staged);
	assert.equal(
		await f.runtime.migrateHistoricalBinding(historyAttest, latest),
		true,
	);
	assert.deepEqual(f.base.data.get(f.key("SUPERVISED")), journal);
	assert.equal(f.base.clicks.length, 0);
});

test("PM bounded rearm never overwrites original supervised proof and only enables later recurrence with new exact Git pair", async () => {
	const { f, latest, journal, proofFor } = await historyFixture();
	assert.equal(
		await f.runtime.migrateHistoricalBinding(historyAttest, latest),
		true,
	);
	f.base.config.reviewed = true;
	f.base.allow = true;
	f.base.pausedRecovery = false;
	assert.equal(await f.runtime.arm("supervised", attest), false);
	assert.equal(await f.runtime.arm("recurrence", attest), false);
	assert.equal(await f.runtime.arm("bounded", attest), true);
	assert.equal(await f.runtime.arm("bounded", attest), false);
	await f.fire();
	const s = await f.runtime.status();
	assert.equal(s.cycle, 12);
	assert.equal(s.stopped, true);
	assert.equal(f.base.clicks.length, 1);
	assert.equal(f.base.timers.size, 0);
	assert.deepEqual(f.base.data.get(f.key("SUPERVISED")), journal);
	assert.equal(await f.runtime.arm("recurrence", attest), false);
	const a = f.base.clicks[0].id;
	f.base.proofOverride = (id, q) =>
		q.attemptId === a
			? {
					...syntheticProof(id, q),
					startedCommit: "b".repeat(40),
					startedBlob: "c".repeat(40),
					terminalCommit: "d".repeat(40),
					terminalBlob: "e".repeat(40),
					runId: "bounded-run",
					operationId: "bounded-op",
				}
			: proofFor(id, q);
	assert.equal(await f.runtime.arm("bounded", attest), false);
	assert.equal(await f.runtime.arm("recurrence", attest), true);
	assert.deepEqual(f.base.data.get(f.key("SUPERVISED")), journal);
	await f.runtime.pause();
});

test("PM bounded rearm fails closed on final policy denial and reload; no second bounded attempt", async () => {
	const { f, latest, journal } = await historyFixture();
	assert.equal(
		await f.runtime.migrateHistoricalBinding(historyAttest, latest),
		true,
	);
	f.base.config.reviewed = true;
	f.base.allow = true;
	f.base.pausedRecovery = false;
	assert.equal(await f.runtime.arm("bounded", attest), true);
	f.base.peersPaused = false;
	await f.fire();
	assert.equal(f.base.clicks.length, 0);
	assert.equal(f.base.timers.size, 0);
	f.base.peersPaused = true;
	const reloaded = fixture("platformmanager", f.base);
	await reloaded.runtime.boot();
	assert.equal(await reloaded.runtime.arm("bounded", attest), false);
	assert.equal(await reloaded.runtime.arm("recurrence", attest), false);
	assert.deepEqual(f.base.data.get(f.key("SUPERVISED")), journal);
});

test("PM historical migration refuses incomplete audit and cannot fall back to overwriting original journal", async () => {
	for (const mode of [
		"deleted",
		"journal",
		"target",
		"pending",
		"prior-state",
		"prompt",
	]) {
		const { f, latest, journal } = await historyFixture();
		assert.equal(
			await f.runtime.migrateHistoricalBinding(historyAttest, latest),
			true,
		);
		const state = f.base.data.get(f.key("STATE")).state;
		if (mode === "deleted") delete state.historyMigration;
		if (mode === "journal")
			f.base.data.get(f.key("SUPERVISED")).reconciledReview = { changed: true };
		if (mode === "target")
			state.historyMigration.targetPath = "/g/example-project/c/wrong";
		if (mode === "pending") state.historyMigration.phase = "PREPARED";
		if (mode === "prior-state")
			state.historyMigration.previous.state.preservedUnknown.status = "LOST";
		if (mode === "prompt") f.base.config.promptBlob = "e".repeat(40);
		const protectedJournal = structuredClone(
			f.base.data.get(f.key("SUPERVISED")),
		);
		f.base.config.reviewed = true;
		f.base.allow = true;
		f.base.pausedRecovery = false;
		await assert.rejects(
			f.runtime.arm("supervised", attest),
			/Historical binding/,
			mode,
		);
		await assert.rejects(
			f.runtime.arm("bounded", attest),
			/Historical binding/,
			mode,
		);
		assert.deepEqual(f.base.data.get(f.key("SUPERVISED")), protectedJournal);
		if (mode !== "journal") assert.deepEqual(protectedJournal, journal);
		assert.equal(f.base.timers.size, 0);
		assert.equal(f.base.clicks.length, 0);
	}
});
