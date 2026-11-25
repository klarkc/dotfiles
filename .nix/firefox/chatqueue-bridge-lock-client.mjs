// Browser-neutral authenticated lock and GM state witness adapter.
// No Web Locks, unsafeWindow, exported functions, page storage or TTL leases.
import { AGENTS } from "./chatqueue-recurrence-core.mjs";
import {
	BRIDGE_LOCK_PROTOCOL,
	validLockInput,
	lockTypes,
	witnessKeys,
	witnessLock,
} from "./chatqueue-bridge-lock-protocol.mjs";

export function makeBridgeLockClient({
	agent,
	bridge,
	request,
	randomNonce,
	clock,
	sleep,
	readGM,
	writeGM,
	digest,
}) {
	if (
		!AGENTS.includes(agent) ||
		!Number.isInteger(bridge?.port) ||
		bridge.port < 1024 ||
		bridge.port > 65535 ||
		!/^[a-f0-9]{64}$/.test(bridge.capability ?? "")
	)
		throw new Error("Invalid lock binding");
	const endpoint = "http://127.0.0.1:" + bridge.port + "/v1/lock";
	const held = new Map();
	let poisoned = false;
	const fail = () => {
		poisoned = true;
		throw new Error(
			"Bridge lock/state evidence unverified; stop and reconcile",
		);
	};
	const ensure = () => {
		if (poisoned) throw new Error("Bridge lock client requires reconciliation");
	};
	async function rpc(action, fields, allowPoisoned = false) {
		if (!allowPoisoned) ensure();
		const q = { action, agent, nonce: randomNonce(), ...fields };
		if (!validLockInput(q)) return fail();
		try {
			const r = await request({
				url: endpoint,
				method: "POST",
				anonymous: true,
				timeout: 10000,
				headers: {
					"Content-Type": "application/json",
					Authorization: "Bearer " + bridge.capability,
				},
				data: JSON.stringify(q),
			});
			if (
				r?.status !== 200 ||
				r.finalUrl !== endpoint ||
				typeof r.responseText !== "string" ||
				r.responseText.length > 1024
			)
				return fail();
			const v = JSON.parse(r.responseText);
			if (
				v.protocol !== BRIDGE_LOCK_PROTOCOL ||
				v.agent !== agent ||
				v.action !== action ||
				v.nonce !== q.nonce ||
				typeof v.status !== "string"
			)
				return fail();
			return v.status;
		} catch {
			return fail();
		}
	}
	function kindOf(name) {
		const k =
			name === "CQOP_V1:POLICY_READ"
				? "POLICY_READ"
				: name === "CQOP_V1:ROLE:" + agent
					? "ROLE"
					: name === "CQOP_V1:FENCE:" + agent
						? "FENCE"
						: null;
		if (!lockTypes.includes(k)) throw new Error("Invalid lock name");
		return k;
	}
	async function lock(name, ifAvailable, fn) {
		ensure();
		const kind = kindOf(name),
			owner = randomNonce(),
			deadline = clock() + 120000;
		for (let tries = 0; ; tries++) {
			if (tries >= 1200) return fail();
			const outcome = await rpc("acquire", { kind, owner });
			if (outcome === "GRANTED") break;
			if (outcome !== "BUSY") return fail();
			if (ifAvailable) return { status: "SKIP_IN_PROGRESS" };
			// Retrying a proven BUSY only. Never retry unknown grants.
			if (clock() >= deadline) return fail();
			await sleep(100);
			ensure();
		}
		if (held.has(kind)) return fail();
		held.set(kind, owner);
		try {
			ensure();
			return await fn();
		} finally {
			// The callback has stopped making effects before release. Remove
			// the local owner BEFORE waiting for acknowledgement; a second
			// document can acquire immediately after a proven server release.
			held.delete(kind);
			const outcome = await rpc("release", { kind, owner }, true);
			if (outcome !== "RELEASED") fail();
		}
	}
	async function read(key) {
		ensure();
		if (!witnessKeys.includes(key)) return fail();
		// Actual GM background storage remains the durable journal. A stale
		// per-tab cache or an in-flight write cannot certify state.
		for (let i = 0; i < 5; i++) {
			const value = (await readGM(key)) ?? null;
			const hash = await digest(JSON.stringify(value));
			const outcome = await rpc("check", { key, digest: hash });
			if (outcome === "MATCH") {
				ensure();
				return value;
			}
			if (outcome !== "STALE" && outcome !== "PENDING") return fail();
			await sleep(100);
		}
		return fail();
	}
	async function write(key, value) {
		ensure();
		if (!witnessKeys.includes(key)) return fail();
		const owner = held.get(witnessLock(key));
		if (!owner) return fail();
		const transaction = randomNonce(),
			hash = await digest(JSON.stringify(value));
		const fields = { key, owner, transaction, digest: hash };
		if ((await rpc("begin", fields)) !== "BEGUN") return fail();
		// No recovery/retry after an ambiguous write. The pending witness
		// prevents release and causes all other readers to fail closed.
		try {
			await writeGM(key, value);
			if ((await digest(JSON.stringify((await readGM(key)) ?? null))) !== hash)
				return fail();
			if ((await rpc("commit", fields)) !== "COMMITTED") return fail();
		} catch {
			return fail();
		}
	}
	return { lock, read, write, poisoned: () => poisoned };
}
