// Passive single-process mutual exclusion. No expiry, steal, replay or
// automatic release on socket loss: an old document may still be able to click.
// A lost grant / pending write is an explicit orphan, not evidence of absence.
import { mkdir, rmdir, lstat } from "node:fs/promises";
import { isAbsolute, join } from "node:path";
import {
	BRIDGE_LOCK_PROTOCOL,
	validLockInput,
	lockResource,
	witnessLock,
} from "./chatqueue-bridge-lock-protocol.mjs";

export function makeBridgeLockTable() {
	const holders = new Map(),
		witnesses = new Map(),
		pending = new Map(),
		usedOwners = new Set();
	let closed = false;
	const key = (q) => q.agent + ":" + q.key;
	const owns = (q, kind) => {
		const h = holders.get(lockResource(q.agent, kind));
		return h?.agent === q.agent && h.owner === q.owner;
	};
	const answer = (q, status) => ({
		protocol: BRIDGE_LOCK_PROTOCOL,
		nonce: q.nonce,
		agent: q.agent,
		action: q.action,
		status,
	});
	return {
		handle(q) {
			if (closed || !validLockInput(q)) return null;
			const k = key(q);
			switch (q.action) {
				case "acquire": {
					const r = lockResource(q.agent, q.kind);
					if (holders.has(r)) return answer(q, "BUSY");
					if (usedOwners.has(q.owner)) return answer(q, "DENIED");
					usedOwners.add(q.owner);
					holders.set(r, { agent: q.agent, owner: q.owner });
					return answer(q, "GRANTED");
				}
				case "release": {
					if (
						!owns(q, q.kind) ||
						[...pending.values()].some(
							(p) => p.agent === q.agent && p.owner === q.owner,
						)
					)
						return answer(q, "DENIED");
					holders.delete(lockResource(q.agent, q.kind));
					return answer(q, "RELEASED");
				}
				case "check": {
					if (pending.has(k)) return answer(q, "PENDING");
					// First observation is from a freshly loaded, explicitly
					// provisioned GM store. Subsequent reads must match the
					// last acknowledged background write, not stale tab cache.
					if (!witnesses.has(k)) witnesses.set(k, q.digest);
					return answer(q, witnesses.get(k) === q.digest ? "MATCH" : "STALE");
				}
				case "begin": {
					if (!owns(q, witnessLock(q.key)) || pending.has(k))
						return answer(q, "DENIED");
					pending.set(k, {
						agent: q.agent,
						owner: q.owner,
						transaction: q.transaction,
						digest: q.digest,
					});
					return answer(q, "BEGUN");
				}
				case "commit": {
					const p = pending.get(k);
					if (
						!owns(q, witnessLock(q.key)) ||
						p?.owner !== q.owner ||
						p.transaction !== q.transaction ||
						p.digest !== q.digest
					)
						return answer(q, "DENIED");
					witnesses.set(k, q.digest);
					pending.delete(k);
					return answer(q, "COMMITTED");
				}
				default:
					return null;
			}
		},
		// Closing does not assert that a held lock is released. The caller
		// MUST retain the process-generation guard when clean is false.
		close() {
			closed = true;
			return holders.size === 0 && pending.size === 0;
		},
		status() {
			return { holders: holders.size, pending: pending.size, closed };
		},
	};
}

// The production CLI must hold this non-expiring, private single-instance
// guard. SIGKILL/uncertain close leaves it behind. Never auto-reclaim it based
// on PID, clock or network loss. Operator recovery requires all old browser
// processes gone and retained GM attempt reconciliation. No auto restart.
export async function acquireBridgeGenerationGuard(runtimeDir) {
	if (!isAbsolute(runtimeDir ?? ""))
		throw new Error("Private runtime directory required");
	const stat = await lstat(runtimeDir);
	if (
		!stat.isDirectory() ||
		stat.uid !== process.getuid() ||
		(stat.mode & 0o077) !== 0
	)
		throw new Error("Private runtime directory required");
	const path = join(runtimeDir, "chatqueue-operational-bridge-active");
	await mkdir(path, { mode: 0o700 });
	let released = false;
	return {
		async release(clean) {
			if (released || clean !== true) return false;
			await rmdir(path);
			released = true;
			return true;
		},
	};
}
