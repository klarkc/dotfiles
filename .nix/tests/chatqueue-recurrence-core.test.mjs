import test from "node:test";
import assert from "node:assert/strict";
import {
	AGENTS,
	PHASES,
	REPLY_COOLDOWN_MS,
	newPausedState,
	authorizeFirstTick,
	prepareSend,
	recordSent,
	recordReplyTerminal,
	markUncertain,
	recoverAfterRestart,
	due,
} from "../firefox/chatqueue-recurrence-core.mjs";

const gates = {
	manualGo: true,
	threeLegacyGatesOff: true,
	exclusiveScheduler: true,
	targetVerified: true,
	accountVerified: true,
	bootstrapSameChatVerified: true,
	noOtherGeneration: true,
	recurrenceE2EPassed: true,
};
const preflight = {
	identityVerified: true,
	notGenerating: true,
	threeLegacyGatesOff: true,
	noUnreconciledEffects: true,
};
const first = (agent = "coordinator", at = 0) =>
	authorizeFirstTick(newPausedState(agent), gates, at);
const accepted = (agent = "coordinator", at = 0, id = "run-00001") =>
	recordSent(prepareSend(first(agent, at), id, at, preflight), id, at + 20);

test("exactly four roles: no fifth agent", () => {
	assert.deepEqual(AGENTS, [
		"coordinator",
		"platformmanager",
		"businessmanager",
		"qualitymanager",
	]);
	assert.throws(() => newPausedState("watchdog"));
});

test("paused by default, manual GO requires every gate", () => {
	const s = newPausedState("coordinator");
	assert.equal(s.phase, PHASES.PAUSED);
	assert.equal(due(s, 100_000), false);
	for (const key of Object.keys(gates)) {
		assert.throws(() => authorizeFirstTick(s, { ...gates, [key]: false }, 0));
	}
	assert.equal(first().phase, PHASES.READY);
});

test("sending records PREPARED before UI; duplicate send rejected", () => {
	const prepared = prepareSend(first(), "run-00001", 0, preflight);
	assert.equal(prepared.phase, PHASES.PREPARED);
	assert.equal(prepared.nextEligibleAt, null);
	assert.throws(() => prepareSend(prepared, "run-00002", 1, preflight));
	const sending = recordSent(prepared, "run-00001", 10);
	assert.equal(sending.phase, PHASES.AWAITING_REPLY);
	assert.throws(() => recordSent(sending, "run-00001", 11));
});

test("a sent event never schedules a recurrence", () => {
	const s = accepted();
	assert.equal(s.nextEligibleAt, null);
	assert.equal(due(s, 1_000_000), false);
	assert.throws(() => prepareSend(s, "run-00002", 1_000_000, preflight));
	assert.throws(() =>
		recordReplyTerminal(s, "run-00001", 90, { noActiveGeneration: true }),
	);
});

test("reply observed 9 minutes later schedules 6 minutes AFTER terminal", () => {
	const sent = accepted();
	const finished = recordReplyTerminal(sent, "run-00001", 540_000, {
		observedFinalAssistantResponse: true,
		noActiveGeneration: true,
	});
	assert.equal(finished.phase, PHASES.COOLDOWN);
	assert.equal(finished.nextEligibleAt, 540_000 + REPLY_COOLDOWN_MS);
	assert.equal(due(finished, 899_999), false);
	assert.equal(due(finished, 900_000), true);
	const next = prepareSend(finished, "run-00002", 900_000, preflight);
	assert.equal(next.cycle, 2);
	assert.equal(next.attemptId, "run-00002");
});

test("require independent final reply observation, not just absence of Stop", () => {
	assert.throws(() =>
		recordReplyTerminal(accepted(), "run-00001", 120, {
			observedFinalAssistantResponse: false,
			noActiveGeneration: true,
		}),
	);
	assert.throws(() =>
		recordReplyTerminal(accepted(), "run-00001", 120, {
			observedFinalAssistantResponse: true,
			noActiveGeneration: false,
		}),
	);
});

test("wrong attempt and duplicate terminal are rejected", () => {
	const s = accepted();
	assert.throws(() =>
		recordReplyTerminal(s, "run-00002", 100, {
			observedFinalAssistantResponse: true,
			noActiveGeneration: true,
		}),
	);
	const t = recordReplyTerminal(s, "run-00001", 100, {
		observedFinalAssistantResponse: true,
		noActiveGeneration: true,
	});
	assert.throws(() =>
		recordReplyTerminal(t, "run-00001", 200, {
			observedFinalAssistantResponse: true,
			noActiveGeneration: true,
		}),
	);
});

test("unreconciled external effects and busy generation block preparation", () => {
	for (const key of Object.keys(preflight))
		assert.throws(() =>
			prepareSend(first(), "run-00001", 0, { ...preflight, [key]: false }),
		);
});

test("unknown sends become REVIEW without re-enqueue", () => {
	const s = markUncertain(accepted(), "Provider delivery unknown");
	assert.equal(s.phase, PHASES.REVIEW);
	assert.equal(s.attemptId, "run-00001");
	assert.equal(s.nextEligibleAt, null);
	assert.equal(due(s, 999_999_999), false);
	assert.throws(() => prepareSend(s, "run-00002", 999_999, preflight));
});

test("restart around delivery fails closed; never infer terminal by timeout", () => {
	for (const s of [
		prepareSend(first(), "run-00001", 0, preflight),
		accepted(),
	]) {
		const r = recoverAfterRestart(s);
		assert.equal(r.phase, PHASES.REVIEW);
		assert.equal(r.attemptId, "run-00001");
		assert.equal(due(r, 999_999_999), false);
	}
	const fresh = recoverAfterRestart(first());
	assert.equal(fresh.phase, PHASES.PAUSED);
	const terminal = recordReplyTerminal(accepted(), "run-00001", 100, {
		observedFinalAssistantResponse: true,
		noActiveGeneration: true,
	});
	assert.equal(recoverAfterRestart(terminal).phase, PHASES.PAUSED);
});

test("each agent state is independent; no cross-tab shared cursor", () => {
	const a = accepted("coordinator");
	const b = accepted("qualitymanager", 3_000, "run-00002");
	assert.equal(a.agentId, "coordinator");
	assert.equal(b.agentId, "qualitymanager");
	assert.equal(a.cycle, b.cycle);
	assert.notEqual(a.attemptId, b.attemptId);
	const completeA = recordReplyTerminal(a, "run-00001", 200_000, {
		observedFinalAssistantResponse: true,
		noActiveGeneration: true,
	});
	assert.equal(completeA.phase, PHASES.COOLDOWN);
	assert.equal(b.phase, PHASES.AWAITING_REPLY);
});
