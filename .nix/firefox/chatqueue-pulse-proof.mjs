// Read-only Git evidence. Never infer provider effects from a pulse or a
// terminal browser answer. Inputs are fixed role and validated object IDs.
import { AGENTS } from "./chatqueue-recurrence-core.mjs";
import {
	validatePulseQuery,
	sanitizePulseProof,
} from "./chatqueue-pulse-proof-schema.mjs";

const oid = (s) => typeof s === "string" && /^[0-9a-f]{40}$/.test(s);
const id = (s) => typeof s === "string" && /^[a-zA-Z0-9_.:-]{1,256}$/.test(s);
const attempt = (s) =>
	typeof s === "string" &&
	/^cqop-[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(s);
const terminal = (s) => ["PROGRESS", "WAITING", "IDLE"].includes(s);

export function parsePulse(text) {
	if (typeof text !== "string" || text.length > 1024 * 1024)
		throw new Error("Invalid pulse");
	const fields = new Map();
	for (const line of text.split(/\r?\n/)) {
		const m =
			/^(?:- )?(agent_id|run_id|operation_id|status|state|browser_delivery_id):[ \t]*(.*?)\s*$/.exec(
				line,
			);
		if (!m) continue;
		const key = m[1] === "state" ? "status" : m[1];
		if (fields.has(key)) throw new Error("Ambiguous pulse field");
		const v = m[2].replace(/^\x60([^\x60]+)\x60$/, "$1");
		fields.set(key, v);
	}
	const p = Object.fromEntries(fields);
	if (
		!AGENTS.includes(p.agent_id) ||
		!id(p.run_id) ||
		!id(p.operation_id) ||
		!(p.status === "STARTED" || terminal(p.status)) ||
		(p.browser_delivery_id !== undefined && !attempt(p.browser_delivery_id))
	)
		throw new Error("Unverified pulse metadata");
	return p;
}

// Called only within the collector's before/after remote HEAD checks. The
// trusted local Git adapter supplies read-only operations. A new supervised
// run has exactly two own-pulse changes in the baseline..terminal interval.
// Other agents may commit between them. Later rearm pins the already-proven
// terminal commit and rechecks its reachability from the fresh main.
export function verifyOwnPulsePair({ agent, main, query, git, readBlob }) {
	if (!AGENTS.includes(agent) || !oid(main) || !validatePulseQuery(query))
		return null;
	try {
		const path = "monitoring/agent-pulses/" + agent + ".md";
		const isAncestor = (a, b) => {
			git("merge-base", "--is-ancestor", a, b);
		};
		const lines = (s) => (s.trim() ? s.trim().split("\n") : []);
		const read = (commit, allowAbsent = false) => {
			const rows = lines(git("ls-tree", commit, "--", path));
			if (allowAbsent && rows.length === 0) return null;
			if (rows.length !== 1) throw new Error("Missing pulse");
			const m = /^100644 blob ([0-9a-f]{40})\t(.+)$/.exec(rows[0]);
			if (!m || m[2] !== path) throw new Error("Invalid pulse tree entry");
			const item = readBlob(commit, path);
			if (item.blob !== m[1]) throw new Error("Pulse blob mismatch");
			const meta = parsePulse(item.text);
			if (meta.agent_id !== agent) throw new Error("Wrong pulse identity");
			return { blob: item.blob, meta };
		};
		const end = query.terminalCommit ?? main;
		isAncestor(query.baselineMain, end);
		isAncestor(end, main);
		const changes = lines(
			git(
				"log",
				"--first-parent",
				"--full-history",
				"--format=%H",
				"--max-count=3",
				query.baselineMain + ".." + end,
				"--",
				path,
			),
		);
		if (changes.length !== 2 || !changes.every(oid)) return null;
		const [terminalCommit, startedCommit] = changes;
		if (query.terminalCommit && terminalCommit !== query.terminalCommit)
			return null;
		const singleParent = (commit) => {
			const parent = git("show", "-s", "--format=%P", commit).trim();
			if (!oid(parent))
				throw new Error("Pulse commit must have exactly one parent");
			const names = lines(
				git(
					"diff-tree",
					"--no-commit-id",
					"--name-only",
					"--no-renames",
					"-r",
					parent,
					commit,
				),
			);
			if (names.length !== 1 || names[0] !== path)
				throw new Error("Pulse commit changes other paths");
			return parent;
		};
		const sp = singleParent(startedCommit),
			tp = singleParent(terminalCommit);
		isAncestor(query.baselineMain, sp);
		isAncestor(startedCommit, tp);
		isAncestor(terminalCommit, main);
		const baseline = read(query.baselineMain, true);
		const beforeStart = read(sp, true);
		if (
			(baseline?.blob ?? null) !== (beforeStart?.blob ?? null) ||
			(baseline && !terminal(baseline.meta.status))
		)
			return null;
		const started = read(startedCommit),
			closed = read(terminalCommit);
		if (
			read(tp).blob !== started.blob ||
			read(end).blob !== closed.blob ||
			started.meta.status !== "STARTED" ||
			!terminal(closed.meta.status) ||
			started.meta.browser_delivery_id !== query.attemptId ||
			closed.meta.browser_delivery_id !== query.attemptId ||
			started.meta.run_id !== closed.meta.run_id ||
			started.meta.operation_id !== closed.meta.operation_id ||
			started.meta.run_id === baseline?.meta.run_id
		)
			return null;
		return sanitizePulseProof({
			status: "OWN_GIT_PAIR_VERIFIED",
			agent,
			attemptId: query.attemptId,
			baselineMain: query.baselineMain,
			main,
			startedCommit,
			startedBlob: started.blob,
			terminalCommit,
			terminalBlob: closed.blob,
			runId: started.meta.run_id,
			operationId: started.meta.operation_id,
			terminalStatus: closed.meta.status,
			providerEffectsVerified: false,
		});
	} catch {
		return null;
	}
}
