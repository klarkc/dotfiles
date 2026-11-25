// Shared wire schema. Only fixed role-scoped resources and state witnesses.
// All authority stays in the authenticated local bridge; no page callbacks.
import { AGENTS } from "./chatqueue-recurrence-core.mjs";
export const BRIDGE_LOCK_PROTOCOL = "CQOP_LOCK_V1";
const hex32 = (s) => typeof s === "string" && /^[a-f0-9]{32}$/.test(s);
const hex64 = (s) => typeof s === "string" && /^[a-f0-9]{64}$/.test(s);
export const lockTypes = ["ROLE", "FENCE", "POLICY_READ"];
export const witnessKeys = ["GATE", "STATE", "SUPERVISED"];
export function lockResource(agent, kind) {
	if (!AGENTS.includes(agent) || !lockTypes.includes(kind))
		throw new Error("Unknown lock");
	return kind === "POLICY_READ" ? kind : agent + ":" + kind;
}
export const witnessLock = (key) => (key === "GATE" ? "FENCE" : "ROLE");
export function validLockInput(q) {
	if (
		!q ||
		typeof q !== "object" ||
		Array.isArray(q) ||
		!AGENTS.includes(q.agent) ||
		!hex32(q.nonce)
	)
		return false;
	const shape = Object.keys(q).sort().join(",");
	switch (q.action) {
		case "acquire":
		case "release":
			return (
				shape === "action,agent,kind,nonce,owner" &&
				lockTypes.includes(q.kind) &&
				hex32(q.owner)
			);
		case "check":
			return (
				shape === "action,agent,digest,key,nonce" &&
				witnessKeys.includes(q.key) &&
				hex64(q.digest)
			);
		case "begin":
		case "commit":
			return (
				shape === "action,agent,digest,key,nonce,owner,transaction" &&
				witnessKeys.includes(q.key) &&
				hex64(q.digest) &&
				hex32(q.owner) &&
				hex32(q.transaction)
			);
		default:
			return false;
	}
}
