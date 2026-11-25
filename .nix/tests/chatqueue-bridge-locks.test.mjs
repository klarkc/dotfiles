import test from "node:test";
import assert from "node:assert/strict";
import { randomBytes, createHash } from "node:crypto";
import { mkdtemp, rm, mkdir, chmod } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import {
	makeBridgeLockTable,
	acquireBridgeGenerationGuard,
} from "../firefox/chatqueue-bridge-locks.mjs";
import { makeBridgeLockClient } from "../firefox/chatqueue-bridge-lock-client.mjs";
import { startPolicyBridge } from "../firefox/chatqueue-policy-bridge.mjs";
const token = () => randomBytes(16).toString("hex");
const digest = (s) => createHash("sha256").update(s).digest("hex");
const coordinator = "coordinator",
	peer = "platformmanager";
const resource = (kind, agent = coordinator) =>
	kind === "POLICY_READ"
		? "CQOP_V1:POLICY_READ"
		: "CQOP_V1:" + kind + ":" + agent;
const delay = (ms) => new Promise((r) => setTimeout(r, ms));
const deferred = () => {
	let resolve;
	const promise = new Promise((r) => (resolve = r));
	return { promise, resolve };
};
const rpc = (table, action, fields = {}, agent = coordinator) =>
	table.handle({ action, agent, nonce: token(), ...fields })?.status;
test("fixed-scope table: independent roles, same role exclusivity, global policy mutex, exact release and no owner replay", () => {
	const t = makeBridgeLockTable(),
		a = token(),
		b = token(),
		c = token(),
		d = token();
	assert.equal(rpc(t, "acquire", { kind: "ROLE", owner: a }), "GRANTED");
	assert.equal(rpc(t, "acquire", { kind: "ROLE", owner: b }), "BUSY");
	assert.equal(rpc(t, "acquire", { kind: "ROLE", owner: b }, peer), "GRANTED");
	assert.equal(rpc(t, "acquire", { kind: "FENCE", owner: c }), "GRANTED");
	assert.equal(rpc(t, "acquire", { kind: "POLICY_READ", owner: d }), "GRANTED");
	assert.equal(
		rpc(t, "acquire", { kind: "POLICY_READ", owner: token() }, peer),
		"BUSY",
	);
	assert.equal(rpc(t, "release", { kind: "ROLE", owner: a }, peer), "DENIED");
	assert.equal(rpc(t, "release", { kind: "FENCE", owner: a }), "DENIED");
	assert.equal(rpc(t, "release", { kind: "ROLE", owner: a }), "RELEASED");
	assert.equal(rpc(t, "acquire", { kind: "ROLE", owner: a }), "DENIED");
	assert.equal(rpc(t, "release", { kind: "ROLE", owner: b }, peer), "RELEASED");
	assert.equal(rpc(t, "release", { kind: "FENCE", owner: c }), "RELEASED");
	assert.equal(
		rpc(t, "release", { kind: "POLICY_READ", owner: d }),
		"RELEASED",
	);
	assert.deepEqual(t.status(), { holders: 0, pending: 0, closed: false });
	assert.equal(t.close(), true);
	assert.equal(rpc(t, "acquire", { kind: "ROLE", owner: token() }), undefined);
});
test("witness blocks stale reads and pending writes; only correct lock owner/transaction can commit", () => {
	const t = makeBridgeLockTable(),
		o = token(),
		tr = token(),
		before = digest("null"),
		after = digest('{"stopped":true}');
	assert.equal(rpc(t, "check", { key: "GATE", digest: before }), "MATCH");
	assert.equal(
		rpc(t, "begin", { key: "GATE", owner: o, transaction: tr, digest: after }),
		"DENIED",
	);
	assert.equal(rpc(t, "acquire", { kind: "ROLE", owner: o }), "GRANTED");
	assert.equal(
		rpc(t, "begin", { key: "GATE", owner: o, transaction: tr, digest: after }),
		"DENIED",
	);
	const f = token();
	assert.equal(rpc(t, "acquire", { kind: "FENCE", owner: f }), "GRANTED");
	const fields = { key: "GATE", owner: f, transaction: tr, digest: after };
	assert.equal(rpc(t, "begin", fields), "BEGUN");
	assert.equal(rpc(t, "check", { key: "GATE", digest: before }), "PENDING");
	assert.equal(rpc(t, "check", { key: "GATE", digest: after }), "PENDING");
	assert.equal(rpc(t, "release", { kind: "FENCE", owner: f }), "DENIED");
	assert.equal(rpc(t, "commit", { ...fields, transaction: token() }), "DENIED");
	assert.equal(rpc(t, "commit", { ...fields, digest: before }), "DENIED");
	assert.equal(rpc(t, "commit", fields), "COMMITTED");
	assert.equal(rpc(t, "check", { key: "GATE", digest: before }), "STALE");
	assert.equal(rpc(t, "check", { key: "GATE", digest: after }), "MATCH");
	assert.equal(rpc(t, "release", { kind: "FENCE", owner: f }), "RELEASED");
	assert.equal(rpc(t, "release", { kind: "ROLE", owner: o }), "RELEASED");
	assert.equal(t.close(), true);
});
async function harness(t) {
	const table = makeBridgeLockTable(),
		capability = token() + token();
	const b = await startPolicyBridge({
		capability,
		lockTable: table,
		readPolicy: () => assert.fail("lock request must not read Git"),
	});
	t.after(() => b.stop());
	const bridge = { port: b.port, capability };
	const request = async (opts) => {
		const r = await fetch(opts.url, {
			method: opts.method,
			headers: opts.headers,
			body: opts.data,
			redirect: "error",
		});
		return { status: r.status, finalUrl: r.url, responseText: await r.text() };
	};
	const gm = new Map();
	const client = (
		agent = coordinator,
		{ transport = request, read, write } = {},
	) =>
		makeBridgeLockClient({
			agent,
			bridge,
			request: transport,
			randomNonce: token,
			clock: Date.now,
			sleep: () => delay(1),
			digest: async (s) => digest(s),
			readGM:
				read ??
				(async (key) => structuredClone(gm.get(agent + ":" + key) ?? null)),
			writeGM:
				write ??
				(async (key, value) => {
					gm.set(agent + ":" + key, structuredClone(value));
				}),
		});
	return { table, bridge, b, request, gm, client };
}
test("actual authenticated HTTP lock and GM witnesses serialize two documents, fence pause before click", async (t) => {
	const h = await harness(t),
		a = h.client(),
		b = h.client(),
		entered = deferred(),
		release = deferred(),
		order = [];
	const pause = a.lock(resource("FENCE"), false, async () => {
		await a.write("GATE", { stopped: true });
		entered.resolve();
		await release.promise;
		order.push("pause-ack");
	});
	await entered.promise;
	let clicked = false;
	const send = b.lock(resource("ROLE"), true, () =>
		b.lock(resource("FENCE"), false, async () => {
			const gate = await b.read("GATE");
			order.push("final-check");
			if (!gate.stopped) clicked = true;
		}),
	);
	await delay(20);
	assert.deepEqual(order, []);
	release.resolve();
	await pause;
	await send;
	assert.deepEqual(order, ["pause-ack", "final-check"]);
	assert.equal(clicked, false);
	assert.deepEqual(h.table.status(), { holders: 0, pending: 0, closed: false });
});
test("actual HTTP serializes click first and pause second; same-role duplicate cannot take long lock", async (t) => {
	const h = await harness(t),
		a = h.client(),
		b = h.client(),
		entered = deferred(),
		release = deferred(),
		order = [];
	const send = a.lock(resource("ROLE"), true, () =>
		a.lock(resource("FENCE"), false, async () => {
			await a.write("GATE", { stopped: false });
			await a.write("STATE", {
				attemptId: "synthetic-original",
				phase: "PREPARED",
			});
			entered.resolve();
			await release.promise;
			assert.equal((await a.read("GATE")).stopped, false);
			order.push("click");
		}),
	);
	await entered.promise;
	assert.deepEqual(
		await b.lock(resource("ROLE"), true, () => assert.fail("duplicate")),
		{ status: "SKIP_IN_PROGRESS" },
	);
	const pause = b.lock(resource("FENCE"), false, async () => {
		await b.write("GATE", { stopped: true });
		order.push("pause-ack");
	});
	await delay(20);
	assert.deepEqual(order, []);
	release.resolve();
	await send;
	await pause;
	assert.deepEqual(order, ["click", "pause-ack"]);
	assert.equal((await b.read("STATE")).attemptId, "synthetic-original");
});
test("stale GM cache cannot certify an old unstopped gate or old attempt", async (t) => {
	const h = await harness(t),
		a = h.client(),
		b = h.client(coordinator, { read: async () => ({ stopped: false }) });
	await a.lock(resource("FENCE"), false, () =>
		a.write("GATE", { stopped: true }),
	);
	await assert.rejects(b.read("GATE"), /unverified/);
	assert.equal(b.poisoned(), true);
	await assert.rejects(
		b.lock(resource("FENCE"), false, () => assert.fail("poisoned client")),
		/reconciliation/,
	);
	assert.equal((await a.read("GATE")).stopped, true);
});
test("lost acquire response never invokes callback, never retries uncertain grant, never frees by time", async (t) => {
	const h = await harness(t);
	let calls = 0;
	const a = h.client(coordinator, {
		transport: async (opts) => {
			calls++;
			await h.request(opts);
			throw new Error("lost response");
		},
	});
	await assert.rejects(
		a.lock(resource("ROLE"), true, () => assert.fail("no grant receipt")),
		/unverified/,
	);
	assert.equal(calls, 1);
	assert.equal(a.poisoned(), true);
	assert.equal(h.table.status().holders, 1);
	const b = h.client();
	assert.deepEqual(
		await b.lock(resource("ROLE"), true, () => assert.fail("orphan grant")),
		{ status: "SKIP_IN_PROGRESS" },
	);
	assert.equal(await h.b.stop(), false);
});
test("unknown GM write keeps pending witness and refuses release even if callback unwinds", async (t) => {
	const h = await harness(t),
		a = h.client(coordinator, {
			write: async () => {
				throw new Error("storage result unknown");
			},
		});
	await assert.rejects(
		a.lock(resource("FENCE"), false, () => a.write("GATE", { stopped: true })),
		/unverified/,
	);
	assert.equal(a.poisoned(), true);
	assert.equal(h.table.status().holders, 1);
	assert.equal(h.table.status().pending, 1);
	const b = h.client();
	await assert.rejects(b.read("GATE"), /unverified/);
	assert.equal(await h.b.stop(), false);
});
test("lost release response poisons old client; callback finished before any later grant", async (t) => {
	const h = await harness(t),
		a = h.client(coordinator, {
			transport: async (opts) => {
				const r = await h.request(opts);
				if (JSON.parse(opts.data).action === "release")
					throw new Error("lost release ack");
				return r;
			},
		});
	let done = false;
	await assert.rejects(
		a.lock(resource("ROLE"), true, async () => {
			done = true;
		}),
		/unverified/,
	);
	assert.ok(done);
	assert.equal(a.poisoned(), true);
	const b = h.client();
	assert.equal(await b.lock(resource("ROLE"), true, () => 42), 42);
});
test("HTTP scope refuses unknown action, arbitrary resource, wrong capability and body", async (t) => {
	const h = await harness(t),
		q = {
			action: "acquire",
			agent: coordinator,
			nonce: token(),
			owner: token(),
			kind: "ROLE",
		};
	const send = async (q, cap = h.bridge.capability) =>
		fetch("http://127.0.0.1:" + h.bridge.port + "/v1/lock", {
			method: "POST",
			headers: {
				"Content-Type": "application/json",
				Authorization: "Bearer " + cap,
			},
			body: JSON.stringify(q),
		});
	for (const v of [
		{ ...q, action: "steal" },
		{ ...q, kind: "arbitrary" },
		{ ...q, agent: "unknown" },
		{ ...q, path: "private" },
		{ ...q, owner: "bad" },
	])
		assert.equal((await send(v)).status, 400);
	assert.equal((await send(q, "0".repeat(64))).status, 403);
	assert.equal(h.table.status().holders, 0);
});
test("generation guard is atomic; unclean shutdown retains it, no automatic reclaim; clean shutdown releases", async (t) => {
	const root = await mkdtemp(join(tmpdir(), "cqop-guard-fixture-"));
	t.after(() => rm(root, { recursive: true, force: true }));
	const guard = await acquireBridgeGenerationGuard(root);
	await assert.rejects(acquireBridgeGenerationGuard(root));
	assert.equal(await guard.release(false), false);
	await assert.rejects(acquireBridgeGenerationGuard(root));
	// Synthetic explicit recovery only; no browser or real lock exists.
	await rm(join(root, "chatqueue-operational-bridge-active"), {
		recursive: true,
	});
	const replacement = await acquireBridgeGenerationGuard(root);
	assert.equal(await replacement.release(true), true);
	const third = await acquireBridgeGenerationGuard(root);
	assert.equal(await third.release(true), true);
	const insecure = join(root, "insecure");
	await mkdir(insecure, { mode: 0o755 });
	await chmod(insecure, 0o755);
	await assert.rejects(acquireBridgeGenerationGuard(insecure));
});
