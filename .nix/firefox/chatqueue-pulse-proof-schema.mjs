// Browser-neutral, strict shape validation. No Git authority is implied by a shape.
import { AGENTS } from "./chatqueue-recurrence-core.mjs";
const pulseOid = (s) => typeof s === "string" && /^[0-9a-f]{40}$/.test(s);
const pulseId = (s) =>
	typeof s === "string" && /^[a-zA-Z0-9_.:-]{1,256}$/.test(s);
const pulseAttempt = (s) =>
	typeof s === "string" &&
	/^cqop-[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(s);
const pulseTerminal = (s) => ["PROGRESS", "WAITING", "IDLE"].includes(s);

export function validatePulseQuery(q) {
	return (
		!!q &&
		typeof q === "object" &&
		!Array.isArray(q) &&
		[
			"attemptId,baselineMain",
			"attemptId,baselineMain,terminalCommit",
		].includes(Object.keys(q).sort().join(",")) &&
		pulseAttempt(q.attemptId) &&
		pulseOid(q.baselineMain) &&
		(q.terminalCommit === undefined || pulseOid(q.terminalCommit))
	);
}

export function sanitizePulseProof(p) {
	if (
		!p ||
		p.status !== "OWN_GIT_PAIR_VERIFIED" ||
		!AGENTS.includes(p.agent) ||
		!validatePulseQuery({
			attemptId: p.attemptId,
			baselineMain: p.baselineMain,
			terminalCommit: p.terminalCommit,
		}) ||
		![
			"main",
			"startedCommit",
			"startedBlob",
			"terminalCommit",
			"terminalBlob",
		].every((k) => pulseOid(p[k])) ||
		p.startedCommit === p.terminalCommit ||
		p.startedBlob === p.terminalBlob ||
		!pulseId(p.runId) ||
		!pulseId(p.operationId) ||
		!pulseTerminal(p.terminalStatus) ||
		p.providerEffectsVerified !== false
	)
		return null;
	return Object.fromEntries(
		[
			"status",
			"agent",
			"attemptId",
			"baselineMain",
			"main",
			"startedCommit",
			"startedBlob",
			"terminalCommit",
			"terminalBlob",
			"runId",
			"operationId",
			"terminalStatus",
			"providerEffectsVerified",
		].map((k) => [k, p[k]]),
	);
}

export function pulseProofMatches(value, agent, query, main) {
	const proof = sanitizePulseProof(value);
	return (
		!!proof &&
		validatePulseQuery(query) &&
		proof.agent === agent &&
		proof.main === main &&
		proof.attemptId === query.attemptId &&
		proof.baselineMain === query.baselineMain &&
		(query.terminalCommit === undefined ||
			proof.terminalCommit === query.terminalCommit)
	);
}
