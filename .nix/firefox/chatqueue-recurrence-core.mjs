// Original, portable recurrence state machine owned by klarkc/dotfiles.
// This is NOT wired to any browser action. No extension package is patched.
// Caller must persist each transition before acting on external UI.
export const AGENTS = Object.freeze([
	"coordinator",
	"platformmanager",
	"businessmanager",
	"qualitymanager",
]);
export const REPLY_COOLDOWN_MS = 360_000;
export const PHASES = Object.freeze({
	PAUSED: "PAUSED",
	READY: "READY",
	PREPARED: "PREPARED",
	AWAITING_REPLY: "AWAITING_REPLY",
	COOLDOWN: "COOLDOWN",
	REVIEW: "REVIEW",
});
const required = (condition, message) => {
	if (!condition) throw new Error(message);
};
const validTime = (n) => Number.isSafeInteger(n) && n >= 0;
const validId = (value) =>
	typeof value === "string" && /^[a-zA-Z0-9_.:-]{8,128}$/.test(value);

export function newPausedState(agentId) {
	required(AGENTS.includes(agentId), "Unknown permanent agent");
	return {
		version: 1,
		agentId,
		phase: PHASES.PAUSED,
		cycle: 0,
		attemptId: null,
		nextEligibleAt: null,
		lastReplyAt: null,
		pendingReason: "No manual GO; four broker heartbeats remain paused",
	};
}

// This is a functional decision model, not runtime authorization.
export function authorizeFirstTick(state, gates, now) {
	required(validTime(now), "Invalid time");
	required(state.phase === PHASES.PAUSED, "Not paused");
	const ready =
		gates?.manualGo === true &&
		gates?.threeLegacyGatesOff === true &&
		gates?.exclusiveScheduler === true &&
		gates?.targetVerified === true &&
		gates?.accountVerified === true &&
		gates?.bootstrapSameChatVerified === true &&
		gates?.noOtherGeneration === true &&
		gates?.recurrenceE2EPassed === true;
	required(ready, "Missing manual GO or a preactivation proof");
	return {
		...state,
		phase: PHASES.READY,
		nextEligibleAt: now,
		pendingReason: null,
	};
}

export function prepareSend(state, attemptId, now, preflight) {
	required(validTime(now), "Invalid time");
	required(validId(attemptId), "Missing stable attempt ID");
	required(
		[PHASES.READY, PHASES.COOLDOWN].includes(state.phase),
		"An old send is incomplete or review is required",
	);
	required(
		state.nextEligibleAt !== null && now >= state.nextEligibleAt,
		"Not eligible until after terminal reply and cooldown",
	);
	required(
		preflight?.identityVerified &&
			preflight?.notGenerating &&
			preflight?.threeLegacyGatesOff &&
			preflight?.noUnreconciledEffects,
		"Fresh send preflight not proven",
	);
	return {
		...state,
		phase: PHASES.PREPARED,
		cycle: state.cycle + 1,
		attemptId,
		nextEligibleAt: null,
		pendingReason: "Persist PREPARED before any UI send",
	};
}

export function recordSent(state, attemptId, now) {
	required(validTime(now), "Invalid time");
	required(
		state.phase === PHASES.PREPARED && state.attemptId === attemptId,
		"Unexpected or duplicate send acknowledgement",
	);
	return {
		...state,
		phase: PHASES.AWAITING_REPLY,
		pendingReason: "sent is not a completed assistant reply",
	};
}

export function recordReplyTerminal(state, attemptId, now, proof) {
	required(validTime(now), "Invalid time");
	required(
		state.phase === PHASES.AWAITING_REPLY && state.attemptId === attemptId,
		"Reply does not match current attempt",
	);
	required(
		proof?.observedFinalAssistantResponse === true &&
			proof?.noActiveGeneration === true,
		"A sent event or absent Stop button alone is insufficient",
	);
	return {
		...state,
		phase: PHASES.COOLDOWN,
		attemptId: null,
		lastReplyAt: now,
		nextEligibleAt: now + REPLY_COOLDOWN_MS,
		pendingReason: "Wait 360 seconds after verified reply completion",
	};
}

export function markUncertain(state, reason) {
	required(
		typeof reason === "string" && reason.trim(),
		"A review must include an actionable reason",
	);
	return {
		...state,
		phase: PHASES.REVIEW,
		nextEligibleAt: null,
		pendingReason: reason,
	};
}

export function recoverAfterRestart(state) {
	// Never infer a previous send's outcome merely from elapsed time.
	if ([PHASES.PREPARED, PHASES.AWAITING_REPLY].includes(state.phase))
		return markUncertain(
			state,
			"Browser restarted around a send: reconcile exact attempt before rearming",
		);
	if ([PHASES.READY, PHASES.COOLDOWN].includes(state.phase))
		return {
			...state,
			phase: PHASES.PAUSED,
			nextEligibleAt: null,
			pendingReason:
				"Reverify target, account, bootstrap, and manual GO after restart",
		};
	return { ...state };
}

export function due(state, now) {
	required(validTime(now), "Invalid time");
	return (
		[PHASES.READY, PHASES.COOLDOWN].includes(state.phase) &&
		state.nextEligibleAt !== null &&
		now >= state.nextEligibleAt
	);
}
