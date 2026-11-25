// Original browser-neutral controller. No network, timers, send or UI on import.
// All side effects are explicit injected functions; must run under a single
// exclusive per-conversation lock with a durable state store.
import {
	AGENTS,
	PHASES,
	authorizeFirstTick,
	due,
	markUncertain,
	newPausedState,
	prepareSend,
	recordReplyTerminal,
	recordSent,
	recoverAfterRestart,
	sendPreflightVerified,
} from "./chatqueue-recurrence-core.mjs";

function assertAgent(agent) {
	if (!AGENTS.includes(agent)) throw new Error("Unrecognized agent");
}
function requireFunction(value, key) {
	if (typeof value !== "function") throw new Error("Missing " + key);
}
export function makeLocalController({
	read,
	write,
	withLock,
	clock,
	transport,
	paused,
	admission = "lab",
}) {
	if (!["lab", "broker"].includes(admission))
		throw new Error("Unknown admission scope");
	for (const [key, fn] of Object.entries({
		read,
		write,
		withLock,
		clock,
		paused,
	}))
		requireFunction(fn, key);
	for (const name of ["preflight", "compose", "deliver", "replyTerminal"])
		requireFunction(transport?.[name], "transport." + name);

	const exclusive = (agent, fn) => withLock("chatqueue-local:" + agent, fn);

	// Boot is intentionally fail-closed. A browser reload is NOT consent to
	// resume unattended work; a possibly-sent attempt is frozen in REVIEW.
	async function boot(agent) {
		assertAgent(agent);
		return exclusive(agent, async () => {
			const before = (await read(agent)) ?? newPausedState(agent);
			if (before.agentId !== agent) throw new Error("State-agent mismatch");
			const after = recoverAfterRestart(before);
			await write(agent, after);
			return after;
		});
	}
	async function arm(agent, gates) {
		assertAgent(agent);
		return exclusive(agent, async () => {
			const before = (await read(agent)) ?? newPausedState(agent);
			if (before.agentId !== agent) throw new Error("State-agent mismatch");
			if (await paused(agent)) throw new Error("Queue pause requested");
			const next = authorizeFirstTick(before, gates, clock());
			await write(agent, next);
			return next;
		});
	}
	async function tick(agent, content, attemptId) {
		assertAgent(agent);
		if (typeof content !== "string" || !content.trim())
			throw new Error("Empty message");
		return exclusive(agent, async () => {
			const state = await read(agent);
			if (!state || state.agentId !== agent) return { status: "NO_STATE" };
			if ((await paused(agent)) || state.phase === PHASES.PAUSED)
				return { status: "PAUSED" };
			if (state.phase === PHASES.REVIEW) return { status: "REVIEW" };
			if (!due(state, clock())) return { status: "NOT_DUE" };

			const preflight = await transport.preflight(agent, content);
			if (!sendPreflightVerified(preflight, admission))
				return { status: "SKIP_NOT_READY" };

			const prepared = prepareSend(
				state,
				attemptId,
				clock(),
				preflight,
				admission,
			);
			await write(agent, prepared); // MUST happen before DOM writes/clicks.
			try {
				if (await paused(agent)) throw new Error("Paused before composing");
				await transport.compose(agent, content);
				if (await paused(agent)) throw new Error("Paused before send");
				const receipt = await transport.deliver(agent, content, attemptId);
				if (receipt?.sent !== true)
					throw new Error("Delivery proof uncertain; operator review required");
				const waiting = recordSent(prepared, attemptId, clock());
				await write(agent, waiting);
				const proof = await transport.replyTerminal(agent, attemptId);
				if (await paused(agent))
					throw new Error("Queue paused during reply; manual reconciliation");
				const next = recordReplyTerminal(waiting, attemptId, clock(), proof);
				await write(agent, next);
				return { status: "COOLDOWN", nextEligibleAt: next.nextEligibleAt };
			} catch (error) {
				const reason =
					"Reconcile exact attempt " +
					attemptId +
					" before any retry (" +
					String(error?.message ?? error).slice(0, 160) +
					")";
				const review = markUncertain((await read(agent)) ?? prepared, reason);
				await write(agent, review);
				return { status: "REVIEW", attemptId };
			}
		});
	}
	return { boot, arm, tick };
}
