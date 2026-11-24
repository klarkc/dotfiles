import test from "node:test";
import assert from "node:assert/strict";
import { randomBytes } from "node:crypto";
import { request } from "node:http";
import {
	startPolicyBridge,
	createPrivatePolicyCache,
} from "../firefox/chatqueue-policy-bridge.mjs";

const capability = randomBytes(32).toString("hex");
const nonce = "a".repeat(32);
const body = JSON.stringify({ agent: "coordinator", nonce });
const paths = [
	"coordination/organization.json",
	"scheduling/state.json",
	"scheduling/watchdog-policy.json",
	"scheduling/runtime.json",
];
const good = {
	agent: "coordinator",
	status: "GIT_POLICY_PASS_ONLY",
	runtimeAuthorized: false,
	reasons: [],
	heads: { main: "b".repeat(40), runtime: "c".repeat(40) },
	blobs: Object.fromEntries(paths.map((p) => [p, "d".repeat(40)])),
};
const withBridge = async (readPolicy, fn) => {
	const b = await startPolicyBridge({ capability, readPolicy });
	try {
		await fn(b.port);
	} finally {
		await b.stop();
	}
};
const send = (port, opts = {}) =>
	new Promise((resolve, reject) => {
		const text = opts.body ?? body;
		const req = request(
			{
				host: "127.0.0.1",
				port,
				path: opts.path ?? "/v1/evidence",
				method: opts.method ?? "POST",
				headers: {
					"Content-Type": "application/json",
					"Content-Length": Buffer.byteLength(text),
					Authorization: "Bearer " + capability,
					...opts.headers,
				},
			},
			(res) => {
				let raw = "";
				res.on("data", (chunk) => (raw += chunk));
				res.on("end", () =>
					resolve({
						code: res.statusCode,
						headers: res.headers,
						value: JSON.parse(raw),
					}),
				);
			},
		);
		req.on("error", reject);
		req.end(text);
	});
test("passive authenticated request echoes nonce, never authorizes runtime, strips raw data", async () => {
	let reads = 0;
	await withBridge(
		async (agent) => {
			reads++;
			assert.equal(agent, "coordinator");
			return { ...good, raw: "private payload must be omitted" };
		},
		async (port) => {
			assert.equal(reads, 0);
			const r = await send(port);
			assert.equal(r.code, 200);
			assert.equal(r.value.status, "GIT_POLICY_PASS_ONLY");
			assert.equal(r.value.runtimeAuthorized, false);
			assert.equal(r.value.agent, "coordinator");
			assert.equal(r.value.nonce, nonce);
			assert.deepEqual(r.value.heads, good.heads);
			assert.deepEqual(r.value.blobs, good.blobs);
			assert.ok(r.value.observedAt >= r.value.startedAt);
			assert.equal(r.value.raw, undefined);
			assert.equal(r.headers["cache-control"], "no-store");
			assert.equal(r.headers["access-control-allow-origin"], undefined);
			assert.equal(reads, 1);
		},
	);
});
test("wrong method, host, path, token and malformed bodies never read Git", async () => {
	await withBridge(
		() => assert.fail("unauthorized Git access"),
		async (port) => {
			for (const opts of [
				{ method: "GET" },
				{ method: "OPTIONS" },
				{ path: "/v1/evidence?x=1" },
				{ headers: { Host: "example.invalid" } },
				{ headers: { Authorization: "Bearer " + "0".repeat(64) } },
				{ headers: { Authorization: "" } },
			])
				assert.equal((await send(port, opts)).code, 403);
			for (const opts of [
				{ body: "{}" },
				{ body: "x".repeat(257) },
				{ body: JSON.stringify({ agent: "other", nonce }) },
				{
					body: JSON.stringify({
						agent: "coordinator",
						nonce,
						repo: "/example/private",
					}),
				},
				{ body: JSON.stringify({ agent: "coordinator", nonce: "short" }) },
				{ headers: { "Content-Type": "text/plain" } },
			])
				assert.equal((await send(port, opts)).code, 400);
		},
	);
});
test("malformed or overly permissive evidence is a denial; provider errors sanitized", async () => {
	for (const value of [
		{ ...good, runtimeAuthorized: true },
		{ ...good, heads: undefined },
		{ ...good, agent: "platformmanager" },
		{ ...good, reasons: ["OTHER"] },
		{ ...good, status: "UNKNOWN" },
		{ ...good, status: "NO_GO", reasons: [] },
		{ ...good, status: "NO_GO", reasons: Array(25).fill("OTHER") },
	])
		await withBridge(
			() => value,
			async (port) => {
				const r = await send(port);
				assert.equal(r.value.status, "NO_GO");
				assert.equal(r.value.runtimeAuthorized, false);
				assert.deepEqual(r.value.reasons, ["INVALID_POLICY_EVIDENCE"]);
			},
		);
	await withBridge(
		() => {
			throw new Error("private-provider-error");
		},
		async (port) => {
			const r = await send(port);
			assert.deepEqual(r.value.reasons, ["BRIDGE_POLICY_READ_UNVERIFIED"]);
			assert.ok(!JSON.stringify(r).includes("private-provider-error"));
		},
	);
});
test("concurrent request is refused, no implicit polling or queued second read", async () => {
	let release, entered;
	const pending = new Promise((r) => (release = r));
	const ready = new Promise((r) => (entered = r));
	let reads = 0;
	await withBridge(
		async () => {
			reads++;
			entered();
			await pending;
			return good;
		},
		async (port) => {
			const first = send(port);
			await ready;
			try {
				const second = await send(port, {
					body: JSON.stringify({
						agent: "businessmanager",
						nonce: "e".repeat(32),
					}),
				});
				assert.equal(second.code, 503);
				assert.equal(second.value.runtimeAuthorized, false);
				assert.equal(reads, 1);
			} finally {
				release();
			}
			assert.equal((await first).value.status, "GIT_POLICY_PASS_ONLY");
			assert.equal(reads, 1);
		},
	);
});
test("reject invalid private cache configuration before creating a cache", async () => {
	await assert.rejects(
		createPrivatePolicyCache({
			runtimeDir: "relative",
			expectedOrigin: "git@example.invalid:example/broker.git",
		}),
	);
	await assert.rejects(
		createPrivatePolicyCache({
			runtimeDir: "/example/runtime",
			expectedOrigin: "file:///example/broker",
		}),
	);
	await assert.rejects(
		createPrivatePolicyCache({
			runtimeDir: "/tmp",
			expectedOrigin: "git@example.invalid:example/broker.git",
		}),
	);
	await assert.rejects(
		startPolicyBridge({ capability: "short", readPolicy: () => good }),
	);
});

test("shutdown stops admission, waits for outstanding read, and does not replay", async () => {
	let release, entered;
	const pending = new Promise((r) => (release = r));
	const ready = new Promise((r) => (entered = r));
	let reads = 0;
	const b = await startPolicyBridge({
		capability,
		readPolicy: async () => {
			reads++;
			entered();
			await pending;
			return good;
		},
	});
	const first = send(b.port).catch(() => null);
	await ready;
	let stopped = false;
	const shutdown = b.stop().then(() => {
		stopped = true;
	});
	await new Promise((r) => setImmediate(r));
	assert.equal(stopped, false);
	release();
	await shutdown;
	await first;
	assert.equal(reads, 1);
});
