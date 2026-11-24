// Browser-neutral validation of private bindings and fresh bridge evidence.
// A valid evidence packet is not authority to arm or click.
import { AGENTS } from "./chatqueue-recurrence-core.mjs";
const policyPaths = [
	"coordination/organization.json",
	"scheduling/state.json",
	"scheduling/watchdog-policy.json",
	"scheduling/runtime.json",
];
const oid = (s) => typeof s === "string" && /^[a-f0-9]{40}$/.test(s);
const validNonce = (s) => typeof s === "string" && /^[a-f0-9]{32,64}$/.test(s);
const roleNames = {
	coordinator: "Coordinator",
	platformmanager: "PlatformManager",
	businessmanager: "BusinessManager",
	qualitymanager: "QualityManager",
};
export const OPERATIONAL_MAX_EVIDENCE_AGE_MS = 5000;
export const OPERATIONAL_REQUEST_TIMEOUT_MS = 90000;
export const OPERATIONAL_CONFIG_KEY = "CQOP_V1:CONFIG";
export const OPERATIONAL_PATH = /^\/g\/[a-zA-Z0-9-]+\/c\/[a-zA-Z0-9-]+$/;

export function parseOperationalTemplates(markdown) {
	if (typeof markdown !== "string") throw new Error("Missing prompt document");
	const result = {};
	for (const id of AGENTS) {
		const section = new RegExp(
			"^## " +
				roleNames[id] +
				"\\s*\\n\\s*\`\`\`text\\s*\\n([\\s\\S]*?)\\n\`\`\`\\s*$",
			"gm",
		);
		const matches = [...markdown.matchAll(section)];
		if (matches.length !== 1) throw new Error("Ambiguous role template");
		const text = matches[0][1];
		if (
			!text.startsWith(
				"ONE tick ONLY as permanent " +
					roleNames[id] +
					" (agent_id=" +
					id +
					")",
			) ||
			!text.includes("require " + id.toUpperCase() + "_BOOTSTRAP_READY.") ||
			!text.includes("NON_BLOCKING_HEARTBEATS_V1 + NO_DEADLOCK_V1") ||
			!text.endsWith("STOP. ChatQueue triggers the next tick.") ||
			text.length > 10000
		)
			throw new Error("Unexpected role template");
		result[id] = text;
	}
	return result;
}

export function validateOperationalConfig(config) {
	if (
		!config ||
		config.version !== 1 ||
		typeof config.reviewed !== "boolean" ||
		!oid(config.promptBlob) ||
		!Number.isInteger(config.bridge?.port) ||
		config.bridge.port < 1024 ||
		config.bridge.port > 65535 ||
		typeof config.bridge.capability !== "string" ||
		!/^[a-f0-9]{64}$/.test(config.bridge.capability) ||
		!Array.isArray(config.bindings) ||
		config.bindings.length !== AGENTS.length
	)
		return false;
	const bindings = config.bindings;
	const paths = bindings.map((b) => b?.path);
	if (
		new Set(paths).size !== AGENTS.length ||
		!paths.every((p) => typeof p === "string" && OPERATIONAL_PATH.test(p)) ||
		new Set(paths.map((p) => p.split("/")[2])).size !== 1
	)
		return false;
	return AGENTS.every((id) => {
		const matches = bindings.filter((b) => b?.agent === id);
		if (matches.length !== 1) return false;
		const b = matches[0];
		return (
			typeof b.modelLabel === "string" &&
			/^(?:6\s+)?Extra (?:alto|High)$/i.test(b.modelLabel) &&
			typeof b.prompt === "string" &&
			b.prompt.length <= 10000 &&
			b.prompt.startsWith(
				"ONE tick ONLY as permanent " +
					roleNames[id] +
					" (agent_id=" +
					id +
					")",
			) &&
			b.prompt.includes("require " + id.toUpperCase() + "_BOOTSTRAP_READY.") &&
			b.prompt.endsWith("STOP. ChatQueue triggers the next tick.")
		);
	});
}

export function operationalMessage(binding, attemptId) {
	if (!/^cqop-[a-f0-9-]{36}$/.test(attemptId))
		throw new Error("Invalid attempt identity");
	// Keep the canonical template verbatim; the suffix is only a transport
	// correlation value. It must NOT become a provider run_id/operation_id.
	return (
		binding.prompt +
		"\n\nBrowser delivery correlation only (not run_id or operation_id): " +
		attemptId
	);
}

export function freshOperationalEvidence(
	value,
	{ agent, nonce, promptBlob, promptDigest, requestedAt, now },
) {
	return (
		!!value &&
		AGENTS.includes(agent) &&
		validNonce(nonce) &&
		value.agent === agent &&
		value.nonce === nonce &&
		value.status === "GIT_POLICY_PASS_ONLY" &&
		value.runtimeAuthorized === false &&
		Array.isArray(value.reasons) &&
		value.reasons.length === 0 &&
		oid(promptBlob) &&
		value.promptBlob === promptBlob &&
		validNonce(promptDigest) &&
		promptDigest.length === 64 &&
		value.promptDigest === promptDigest &&
		oid(value.heads?.main) &&
		oid(value.heads?.runtime) &&
		policyPaths.every((path) => oid(value.blobs?.[path])) &&
		Number.isSafeInteger(requestedAt) &&
		Number.isSafeInteger(now) &&
		Number.isSafeInteger(value.startedAt) &&
		Number.isSafeInteger(value.observedAt) &&
		requestedAt <= value.startedAt &&
		value.startedAt <= value.observedAt &&
		value.observedAt <= now &&
		now - requestedAt <= OPERATIONAL_REQUEST_TIMEOUT_MS &&
		now - value.observedAt <= OPERATIONAL_MAX_EVIDENCE_AGE_MS
	);
}

async function sha256(text) {
	const bytes = await crypto.subtle.digest(
		"SHA-256",
		new TextEncoder().encode(text),
	);
	return [...new Uint8Array(bytes)]
		.map((b) => b.toString(16).padStart(2, "0"))
		.join("");
}
export function makeOperationalPolicyClient({ request, clock, randomNonce }) {
	return async function readEvidence(config, agent) {
		if (!validateOperationalConfig(config) || !AGENTS.includes(agent))
			return null;
		let promptDigest;
		try {
			promptDigest = await sha256(
				config.bindings.find((b) => b.agent === agent).prompt,
			);
		} catch {
			return null;
		}
		const nonce = randomNonce();
		if (!validNonce(nonce)) return null;
		const requestedAt = clock();
		const url = "http://127.0.0.1:" + config.bridge.port + "/v1/evidence";
		try {
			const response = await request({
				url,
				method: "POST",
				anonymous: true,
				timeout: OPERATIONAL_REQUEST_TIMEOUT_MS,
				headers: {
					"Content-Type": "application/json",
					Authorization: "Bearer " + config.bridge.capability,
				},
				data: JSON.stringify({ agent, nonce }),
			});
			if (
				response?.status !== 200 ||
				response.finalUrl !== url ||
				typeof response.responseText !== "string" ||
				response.responseText.length > 8192
			)
				return null;
			const value = JSON.parse(response.responseText);
			const context = {
				agent,
				nonce,
				promptBlob: config.promptBlob,
				promptDigest,
				requestedAt,
			};
			return freshOperationalEvidence(value, { ...context, now: clock() })
				? { value, context }
				: null;
		} catch {
			return null;
		}
	};
}
