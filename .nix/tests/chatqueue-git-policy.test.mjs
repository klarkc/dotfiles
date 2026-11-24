import test from "node:test";
import assert from "node:assert/strict";
import { AGENTS } from "../firefox/chatqueue-recurrence-core.mjs";
import {
	collectGitPolicy,
	evaluateGitPolicy,
	parseRemoteHeads,
	readGitPolicy,
} from "../firefox/chatqueue-git-policy.mjs";

const expectedOrigin = "git@example.invalid:example/broker.git";
const main = "a".repeat(40);
const runtime = "b".repeat(40);
const heads =
	main + "\trefs/heads/main\n" + runtime + "\trefs/heads/scheduling-runtime\n";

function documents() {
	return [
		{
			mode: "FOUR_PERMANENT_AGENTS",
			max_permanent_agents: 4,
			permanent_agents: AGENTS.map((id) => ({
				id,
				heartbeat_authorized: true,
				heartbeat_active: id === "coordinator",
			})),
			activation: {
				allowed_to_run_heartbeat: true,
				chatqueue_production_heartbeat_resume_allowed: true,
				browser_scheduler: "CHATQUEUE_FOR_FIREFOX_ONLY",
				chatqueue_original_1_0_1_resume_allowed: false,
				chatqueue_lab_runtime_installed: false,
			},
		},
		{ enabled: false },
		{ enabled: false },
		{ enabled: false },
	];
}
function fixture(overrides = {}) {
	const docs = documents();
	const calls = [];
	const names = [
		"coordination/organization.json",
		"scheduling/state.json",
		"scheduling/watchdog-policy.json",
		"scheduling/runtime.json",
	];
	const f = {
		agent: "coordinator",
		expectedOrigin,
		readOrigin: () => expectedOrigin,
		readHeads: () => heads,
		readBlob: (sha, path) => {
			const i = names.indexOf(path);
			assert.notEqual(i, -1);
			assert.equal(sha, i === 3 ? runtime : main);
			calls.push(path);
			return { blob: String(i + 1).repeat(40), text: JSON.stringify(docs[i]) };
		},
		...overrides,
	};
	return { f, docs, calls };
}
test("malformed policy input fails closed before inspecting fields", () => {
	for (const input of [
		undefined,
		null,
		{},
		[],
		[{}],
		[null, {}, {}, {}],
		[...documents(), {}],
	]) {
		assert.deepEqual(evaluateGitPolicy("coordinator", input), {
			status: "NO_GO",
			runtimeAuthorized: false,
			reasons: ["INVALID_POLICY_DOCUMENT"],
		});
	}
});
test("Git policy pass is only evidence, not runtime authority", () => {
	const { f, calls } = fixture();
	const r = collectGitPolicy(f);
	assert.equal(r.status, "GIT_POLICY_PASS_ONLY");
	assert.equal(r.runtimeAuthorized, false);
	assert.deepEqual(r.heads, { main, runtime });
	assert.equal(Object.keys(r.blobs).length, 4);
	assert.equal(calls.length, 4);
	assert.deepEqual(r.reasons, []);
});
test("NO_GO and role pause remain separate from historical grant", () => {
	const d = documents();
	d[0].activation.chatqueue_production_heartbeat_resume_allowed = false;
	d[0].permanent_agents[0].heartbeat_active = false;
	assert.deepEqual(evaluateGitPolicy("coordinator", d).reasons, [
		"ROLE_PAUSED",
		"PRODUCTION_NO_GO",
	]);
});
test("all three legacy gates require literal false, never truthy or absent", () => {
	for (let i = 1; i < 4; i++) {
		for (const value of [true, undefined, null, 0, "false"]) {
			const d = documents();
			d[i].enabled = value;
			assert.equal(evaluateGitPolicy("coordinator", d).status, "NO_GO");
		}
	}
});
test("missing authorization, wrong scheduler, original or LAB runtime deny", () => {
	for (const [key, value] of [
		["allowed_to_run_heartbeat", "true"],
		["chatqueue_production_heartbeat_resume_allowed", undefined],
		["browser_scheduler", "OTHER"],
		["chatqueue_original_1_0_1_resume_allowed", true],
		["chatqueue_lab_runtime_installed", undefined],
	]) {
		const d = documents();
		d[0].activation[key] = value;
		assert.equal(evaluateGitPolicy("coordinator", d).status, "NO_GO", key);
	}
});
test("wrong identity, duplicated role, fifth role and missing role grant deny", () => {
	const d = documents();
	assert.equal(evaluateGitPolicy("unknown", d).status, "NO_GO");
	assert.equal(evaluateGitPolicy("platformmanager", d).status, "NO_GO");
	d[0].permanent_agents[0].heartbeat_authorized = false;
	assert.equal(evaluateGitPolicy("coordinator", d).status, "NO_GO");
	const duplicate = documents();
	duplicate[0].permanent_agents[1] = duplicate[0].permanent_agents[0];
	assert.equal(evaluateGitPolicy("coordinator", duplicate).status, "NO_GO");
	const extra = documents();
	extra[0].permanent_agents.push({ id: "other" });
	assert.equal(evaluateGitPolicy("coordinator", extra).status, "NO_GO");
});
test("remote head parser refuses missing, duplicate, extra, malformed refs", () => {
	assert.deepEqual(parseRemoteHeads(heads), { main, runtime });
	for (const raw of [
		"",
		main + "\trefs/heads/main\n",
		heads + heads,
		heads.replace(runtime, "bad"),
		heads.replace("scheduling-runtime", "main"),
		heads + main + "\trefs/heads/extra\n",
	]) {
		assert.throws(() => parseRemoteHeads(raw));
	}
});
test("origin must be explicit and match before any remote read", () => {
	for (const wrong of [
		undefined,
		"",
		"https://user:secret@example.invalid/a/b.git",
		"file:///tmp/repo",
		"https://example.invalid/a/b.git?secret=1",
	]) {
		const { f } = fixture({
			expectedOrigin: wrong,
			readOrigin: () => assert.fail("must not read"),
		});
		assert.equal(collectGitPolicy(f).reasons[0], "EXPECTED_ORIGIN_REQUIRED");
	}
	const { f } = fixture({
		readOrigin: () => "git@example.invalid:wrong/broker.git",
		readHeads: () => assert.fail("must not read heads"),
	});
	assert.equal(collectGitPolicy(f).reasons[0], "ORIGIN_MISMATCH");
});
test("changed main, runtime or origin during read invalidates snapshot", () => {
	for (const sha of [main, runtime]) {
		let count = 0;
		const { f } = fixture({
			readHeads: () =>
				++count === 1 ? heads : heads.replace(sha, "c".repeat(40)),
		});
		assert.deepEqual(collectGitPolicy(f), {
			status: "NO_GO",
			runtimeAuthorized: false,
			reasons: ["REMOTE_HEAD_CHANGED_DURING_READ"],
		});
	}
	let count = 0;
	const { f } = fixture({
		readOrigin: () =>
			++count === 1 ? expectedOrigin : "git@example.invalid:wrong/broker.git",
	});
	assert.equal(collectGitPolicy(f).reasons[0], "ORIGIN_CHANGED_DURING_READ");
});
test("malformed/missing blob and provider error never leak raw data", () => {
	for (const readBlob of [
		() => {
			throw new Error("private-provider-error");
		},
		() => ({ blob: "bad", text: "{}" }),
		() => ({ blob: "1".repeat(40), text: "invalid" }),
		() => ({ blob: "1".repeat(40), text: "null" }),
	]) {
		const { f } = fixture({ readBlob });
		const r = collectGitPolicy(f);
		assert.deepEqual(r, {
			status: "NO_GO",
			runtimeAuthorized: false,
			reasons: ["PINNED_POLICY_READ_UNVERIFIED"],
		});
		assert.ok(!JSON.stringify(r).includes("private-provider-error"));
	}
});
test("CLI adapter refuses relative repo paths without touching Git", () => {
	assert.deepEqual(
		readGitPolicy({ repo: "relative", agent: "coordinator", expectedOrigin }),
		{
			status: "NO_GO",
			runtimeAuthorized: false,
			reasons: ["ABSOLUTE_REPO_REQUIRED"],
		},
	);
});
