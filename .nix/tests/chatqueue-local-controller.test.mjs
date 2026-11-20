import test from "node:test";
import assert from "node:assert/strict";
import { makeLocalController } from "../firefox/chatqueue-local-controller.mjs";
import {
	newPausedState,
	PHASES,
	REPLY_COOLDOWN_MS,
} from "../firefox/chatqueue-recurrence-core.mjs";

const completeGates = {
	manualGo: true,
	threeLegacyGatesOff: true,
	exclusiveScheduler: true,
	targetVerified: true,
	accountVerified: true,
	bootstrapSameChatVerified: true,
	noOtherGeneration: true,
	recurrenceE2EPassed: true,
};
function rig() {
	const state = new Map();
	const events = [];
	const held = new Set();
	let now = 0,
		stopped = false;
	const transport = {
		preflight: async () => ({
			identityVerified: true,
			notGenerating: true,
			threeLegacyGatesOff: true,
			noUnreconciledEffects: true,
		}),
		compose: async () => {
			events.push("COMPOSE");
		},
		deliver: async () => {
			events.push("CLICK");
			return { sent: true };
		},
		replyTerminal: async () => {
			events.push("TERMINAL");
			now += 540000;
			return {
				observedFinalAssistantResponse: true,
				noActiveGeneration: true,
			};
		},
	};
	const controller = makeLocalController({
		read: async (agent) => state.get(agent),
		write: async (agent, data) => {
			events.push("SAVE:" + data.phase);
			state.set(agent, data);
		},
		withLock: async (key, fn) => {
			if (held.has(key)) return { status: "SKIP_IN_PROGRESS" };
			held.add(key);
			try {
				return await fn();
			} finally {
				held.delete(key);
			}
		},
		clock: () => now,
		paused: async () => stopped,
		transport,
	});
	return {
		state,
		events,
		controller,
		transport,
		get now() {
			return now;
		},
		set now(value) {
			now = value;
		},
		stop: () => {
			stopped = true;
		},
		unstop: () => {
			stopped = false;
		},
	};
}
test("browser import has no jobs; first boot is always paused", async () => {
	const r = rig();
	const s = await r.controller.boot("coordinator");
	assert.equal(s.phase, PHASES.PAUSED);
	assert.deepEqual(r.events, ["SAVE:PAUSED"]);
	assert.equal(
		(await r.controller.tick("coordinator", "LAB", "attempt-1234")).status,
		"PAUSED",
	);
});
test("arming requires all independent gates; no send before manual GO", async () => {
	const r = rig();
	await r.controller.boot("qualitymanager");
	for (const key of Object.keys(completeGates)) {
		await assert.rejects(
			r.controller.arm("qualitymanager", { ...completeGates, [key]: false }),
		);
	}
	assert.equal(r.state.get("qualitymanager").phase, PHASES.PAUSED);
	await r.controller.arm("qualitymanager", completeGates);
	assert.equal(r.state.get("qualitymanager").phase, PHASES.READY);
	assert.equal(r.events.includes("CLICK"), false);
});
test("PREPARED persists BEFORE browser composer mutation and click", async () => {
	const r = rig();
	await r.controller.boot("coordinator");
	await r.controller.arm("coordinator", completeGates);
	const result = await r.controller.tick(
		"coordinator",
		"LAB test",
		"attempt-1234",
	);
	assert.equal(result.status, "COOLDOWN");
	const ixPrepared = r.events.indexOf("SAVE:PREPARED");
	const ixCompose = r.events.indexOf("COMPOSE");
	const ixClick = r.events.indexOf("CLICK");
	const ixSent = r.events.indexOf("SAVE:AWAITING_REPLY");
	const ixTerminal = r.events.indexOf("TERMINAL");
	assert.ok(ixPrepared >= 0 && ixPrepared < ixCompose && ixCompose < ixClick);
	assert.ok(ixClick < ixSent && ixSent < ixTerminal);
	assert.equal(result.nextEligibleAt, 540000 + REPLY_COOLDOWN_MS);
});
test("a verified sent never retries while generation is running", async () => {
	const r = rig();
	await r.controller.boot("coordinator");
	await r.controller.arm("coordinator", completeGates);
	const replyPromise = new Promise((resolve) => {
		r.transport.replyTerminal = async () =>
			await new Promise((done) => {
				resolve(() =>
					done({
						observedFinalAssistantResponse: true,
						noActiveGeneration: true,
					}),
				);
			});
	});
	const running = r.controller.tick("coordinator", "LAB", "attempt-1234");
	const release = await replyPromise;
	const competing = await r.controller.tick(
		"coordinator",
		"LAB",
		"attempt-5678",
	);
	assert.equal(competing.status, "SKIP_IN_PROGRESS");
	assert.equal(r.events.filter((x) => x === "CLICK").length, 1);
	release();
	assert.equal((await running).status, "COOLDOWN");
});
test("unconfirmed delivery marks REVIEW and never retries automatically", async () => {
	const r = rig();
	r.transport.deliver = async () => ({ sent: false });
	await r.controller.boot("coordinator");
	await r.controller.arm("coordinator", completeGates);
	assert.equal(
		(await r.controller.tick("coordinator", "LAB", "attempt-1234")).status,
		"REVIEW",
	);
	assert.equal(r.state.get("coordinator").attemptId, "attempt-1234");
	assert.equal(
		(await r.controller.tick("coordinator", "LAB", "attempt-5678")).status,
		"REVIEW",
	);
});
test("crash after PREPARED and crash after sent require review on boot", async () => {
	for (const crashAfter of ["compose", "deliver"]) {
		const r = rig();
		await r.controller.boot("coordinator");
		await r.controller.arm("coordinator", completeGates);
		let interrupt = false;
		if (crashAfter === "compose")
			r.transport.compose = async () => {
				interrupt = true;
				throw Error("tab closed");
			};
		if (crashAfter === "deliver")
			r.transport.deliver = async () => {
				interrupt = true;
				throw Error("post-click uncertain");
			};
		const result = await r.controller.tick(
			"coordinator",
			"LAB",
			"attempt-1234",
		);
		assert.equal(result.status, "REVIEW");
		assert.ok(interrupt);
		assert.equal((await r.controller.boot("coordinator")).phase, PHASES.REVIEW);
	}
});
test("fresh preflight failure does not consume eligibility", async () => {
	const r = rig();
	r.transport.preflight = async () => ({ identityVerified: false });
	await r.controller.boot("coordinator");
	await r.controller.arm("coordinator", completeGates);
	assert.equal(
		(await r.controller.tick("coordinator", "LAB", "attempt-1234")).status,
		"SKIP_NOT_READY",
	);
	assert.equal(r.state.get("coordinator").phase, PHASES.READY);
	assert.equal(r.events.includes("CLICK"), false);
});
test("manual pause request prevents initial send", async () => {
	const r = rig();
	await r.controller.boot("coordinator");
	await r.controller.arm("coordinator", completeGates);
	r.stop();
	assert.equal(
		(await r.controller.tick("coordinator", "LAB", "attempt-1234")).status,
		"PAUSED",
	);
	assert.equal(r.events.includes("CLICK"), false);
});
test("manual pause during composing moves attempt into REVIEW", async () => {
	const r = rig();
	await r.controller.boot("coordinator");
	await r.controller.arm("coordinator", completeGates);
	r.transport.compose = async () => {
		r.stop();
	};
	assert.equal(
		(await r.controller.tick("coordinator", "LAB", "attempt-1234")).status,
		"REVIEW",
	);
	assert.equal(r.events.includes("CLICK"), false);
});
test("a restarted cooldown cannot send without renewed manual GO", async () => {
	const r = rig();
	await r.controller.boot("coordinator");
	await r.controller.arm("coordinator", completeGates);
	assert.equal(
		(await r.controller.tick("coordinator", "LAB", "attempt-1234")).status,
		"COOLDOWN",
	);
	assert.equal((await r.controller.boot("coordinator")).phase, PHASES.PAUSED);
	r.now += 999999;
	assert.equal(
		(await r.controller.tick("coordinator", "LAB", "attempt-5678")).status,
		"PAUSED",
	);
});
test("multiple roles have separate durable state, no cross-send", async () => {
	const r = rig();
	for (const agent of ["coordinator", "qualitymanager"]) {
		await r.controller.boot(agent);
		await r.controller.arm(agent, completeGates);
	}
	assert.equal(
		(await r.controller.tick("coordinator", "LAB A", "attempt-1234")).status,
		"COOLDOWN",
	);
	assert.equal(r.state.get("qualitymanager").phase, PHASES.READY);
	assert.equal(
		(await r.controller.tick("qualitymanager", "LAB B", "attempt-5678")).status,
		"COOLDOWN",
	);
	assert.equal(r.events.filter((x) => x === "CLICK").length, 2);
});
