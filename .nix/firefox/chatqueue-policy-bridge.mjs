// Passive, local-only Git policy evidence transport. No browser, timer,
// scheduler, install, or send authority. Nothing runs on import.
import { createServer } from "node:http";
import { randomBytes, timingSafeEqual } from "node:crypto";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { lstat, mkdtemp, rm, writeFile } from "node:fs/promises";
import { isAbsolute, join } from "node:path";
import { fileURLToPath } from "node:url";
import { AGENTS } from "./chatqueue-recurrence-core.mjs";
import {
	validatePulseQuery,
	sanitizePulseProof,
} from "./chatqueue-pulse-proof-schema.mjs";

const exec = promisify(execFile);
const tokenPattern = /^[0-9a-f]{64}$/;
const noncePattern = /^[0-9a-f]{32,64}$/;
const oidPattern = /^[0-9a-f]{40}$/;
const refspecs = [
	"refs/heads/main:refs/remotes/origin/main",
	"refs/heads/scheduling-runtime:refs/remotes/origin/scheduling-runtime",
];
const policyPaths = [
	"coordination/organization.json",
	"scheduling/state.json",
	"scheduling/watchdog-policy.json",
	"scheduling/runtime.json",
];
const policySource = fileURLToPath(
	new URL("./chatqueue-git-policy.mjs", import.meta.url),
);
const denied = (reason) => ({
	status: "NO_GO",
	runtimeAuthorized: false,
	reasons: [reason],
});
const trustedOrigin = (s) =>
	typeof s === "string" &&
	/^(?:git@[a-zA-Z0-9.-]+:|https:\/\/[a-zA-Z0-9.-]+\/)[a-zA-Z0-9_.-]+\/[a-zA-Z0-9_.-]+(?:\.git)?$/.test(
		s,
	);

function sanitize(agent, value) {
	if (
		!value ||
		value.runtimeAuthorized !== false ||
		!["NO_GO", "GIT_POLICY_PASS_ONLY"].includes(value.status) ||
		!Array.isArray(value.reasons) ||
		value.reasons.length > 24 ||
		(value.status === "NO_GO" && value.reasons.length === 0) ||
		!value.reasons.every(
			(s) => typeof s === "string" && /^[A-Z_]{1,80}$/.test(s),
		)
	) {
		return denied("INVALID_POLICY_EVIDENCE");
	}
	// A positive result needs every exact blob and both remote HEADs.
	// A denial may omit heads/blobs (e.g. network or malformed data).
	const pinned =
		value.agent === agent &&
		oidPattern.test(value.heads?.main ?? "") &&
		oidPattern.test(value.heads?.runtime ?? "") &&
		policyPaths.every((path) => oidPattern.test(value.blobs?.[path] ?? ""));
	if (
		value.status === "GIT_POLICY_PASS_ONLY" &&
		(!pinned || value.reasons.length !== 0)
	)
		return denied("INVALID_POLICY_EVIDENCE");
	const result = {
		status: value.status,
		runtimeAuthorized: false,
		reasons: [...value.reasons],
	};
	if (pinned) {
		result.heads = { main: value.heads.main, runtime: value.heads.runtime };
		if (oidPattern.test(value.promptBlob ?? ""))
			result.promptBlob = value.promptBlob;
		if (
			typeof value.promptDigest === "string" &&
			/^[0-9a-f]{64}$/.test(value.promptDigest)
		)
			result.promptDigest = value.promptDigest;
		const proof = sanitizePulseProof(value.pulseProof);
		if (proof && proof.agent === agent && proof.main === value.heads.main)
			result.pulseProof = proof;
		result.blobs = Object.fromEntries(
			policyPaths.map((path) => [path, value.blobs[path]]),
		);
	}
	return result;
}

export async function startPolicyBridge({
	capability,
	readPolicy,
	clock = Date.now,
}) {
	if (
		typeof capability !== "string" ||
		!tokenPattern.test(capability) ||
		typeof readPolicy !== "function"
	)
		throw new Error("Invalid bridge configuration");
	const token = Buffer.from(capability, "hex");
	let activeRead = null;
	let stopped = false;
	let port = null;
	const server = createServer(
		{ requestTimeout: 10_000, headersTimeout: 5_000, maxHeaderSize: 4096 },
		async (req, res) => {
			const send = (code, payload) => {
				if (res.destroyed || res.writableEnded) return;
				res.writeHead(code, {
					"Content-Type": "application/json",
					"Cache-Control": "no-store",
					"X-Content-Type-Options": "nosniff",
					Connection: "close",
				});
				res.end(JSON.stringify(payload));
			};
			const reject = (code, reason) => send(code, denied(reason));
			// No CORS, no URL token, no arbitrary repository/path/command from client.
			if (
				stopped ||
				req.socket.remoteAddress !== "127.0.0.1" ||
				req.headers.host !== "127.0.0.1:" + port ||
				req.method !== "POST" ||
				req.url !== "/v1/evidence"
			) {
				reject(403, "BRIDGE_REQUEST_DENIED");
				return;
			}
			const auth = req.headers.authorization;
			const supplied =
				typeof auth === "string" && auth.startsWith("Bearer ")
					? auth.slice(7)
					: "";
			if (
				!tokenPattern.test(supplied) ||
				!timingSafeEqual(token, Buffer.from(supplied, "hex"))
			) {
				reject(403, "BRIDGE_REQUEST_DENIED");
				return;
			}
			const length = Number(req.headers["content-length"]);
			if (
				req.headers["content-type"] !== "application/json" ||
				req.headers["transfer-encoding"] !== undefined ||
				!Number.isSafeInteger(length) ||
				length < 1 ||
				length > 384
			) {
				reject(400, "BRIDGE_INVALID_BODY");
				return;
			}
			let input;
			try {
				let body = "";
				for await (const chunk of req) {
					body += chunk.toString("utf8");
					if (Buffer.byteLength(body) > 384) throw new Error("Oversize body");
				}
				if (Buffer.byteLength(body) !== length)
					throw new Error("Body length mismatch");
				input = JSON.parse(body);
				if (
					!input ||
					Array.isArray(input) ||
					!["agent,nonce", "agent,nonce,pulseQuery"].includes(
						Object.keys(input).sort().join(","),
					) ||
					(input.pulseQuery !== undefined &&
						!validatePulseQuery(input.pulseQuery)) ||
					!AGENTS.includes(input.agent) ||
					typeof input.nonce !== "string" ||
					!noncePattern.test(input.nonce)
				)
					throw new Error("Invalid request shape");
			} catch {
				reject(400, "BRIDGE_INVALID_BODY");
				return;
			}
			if (activeRead || stopped) {
				reject(503, "BRIDGE_BUSY_OR_STOPPED");
				return;
			}
			const startedAt = clock();
			const pending = Promise.resolve().then(() =>
				readPolicy(input.agent, input.pulseQuery ?? null),
			);
			activeRead = pending;
			try {
				const evidence = sanitize(input.agent, await pending);
				if (stopped) {
					reject(503, "BRIDGE_BUSY_OR_STOPPED");
					return;
				}
				send(200, {
					agent: input.agent,
					nonce: input.nonce,
					startedAt,
					observedAt: clock(),
					...evidence,
				});
			} catch {
				send(200, {
					agent: input.agent,
					nonce: input.nonce,
					startedAt,
					observedAt: clock(),
					...denied("BRIDGE_POLICY_READ_UNVERIFIED"),
				});
			} finally {
				if (activeRead === pending) activeRead = null;
			}
		},
	);
	server.maxConnections = 8;
	await new Promise((resolve, reject) => {
		server.once("error", reject);
		server.listen(0, "127.0.0.1", resolve);
	});
	port = server.address().port;
	return {
		port,
		async stop() {
			stopped = true;
			const pending = activeRead;
			await new Promise((resolve) => {
				server.close(resolve);
				server.closeAllConnections();
			});
			if (pending) await pending.catch(() => {});
		},
	};
}

// Only a generated private bare cache receives local Git updates. All remote
// operations are ordinary reads. The operator's working checkout is never used.
// Local account/Git configuration is trusted; no SSH/TLS bypass or credentials
// in the bridge response. This is not a sandbox against the local account.
export async function createPrivatePolicyCache({ runtimeDir, expectedOrigin }) {
	if (!isAbsolute(runtimeDir ?? "") || !trustedOrigin(expectedOrigin))
		throw new Error("Private runtime directory and exact Git origin required");
	const stat = await lstat(runtimeDir);
	if (
		!stat.isDirectory() ||
		stat.uid !== process.getuid() ||
		(stat.mode & 0o077) !== 0
	)
		throw new Error("Runtime directory must be owned and private");
	const root = await mkdtemp(join(runtimeDir, "chatqueue-policy-"));
	const cache = join(root, "cache.git");
	const env = { ...process.env };
	for (const key of [
		"GIT_DIR",
		"GIT_WORK_TREE",
		"GIT_INDEX_FILE",
		"GIT_COMMON_DIR",
		"GIT_OBJECT_DIRECTORY",
		"GIT_ALTERNATE_OBJECT_DIRECTORIES",
	])
		delete env[key];
	Object.assign(env, {
		GIT_TERMINAL_PROMPT: "0",
		GIT_NO_LAZY_FETCH: "1",
		GIT_NO_REPLACE_OBJECTS: "1",
		GIT_OPTIONAL_LOCKS: "0",
		GIT_ALLOW_PROTOCOL: "ssh:https",
	});
	const git = (...args) =>
		exec("git", args, {
			env,
			encoding: "utf8",
			timeout: 30_000,
			maxBuffer: 1024 * 1024,
		});
	let closed = false;
	let reading = false;
	try {
		await git("init", "--bare", "--template=", cache);
		await git("-C", cache, "remote", "add", "origin", expectedOrigin);
	} catch {
		await rm(root, { recursive: true, force: true });
		throw new Error("Cannot initialize private policy cache");
	}
	return {
		root,
		async readPolicy(agent, pulseQuery = null) {
			if (
				closed ||
				reading ||
				!AGENTS.includes(agent) ||
				(pulseQuery !== null && !validatePulseQuery(pulseQuery))
			)
				return denied("CACHE_NOT_READY");
			reading = true;
			try {
				const { stdout: origin } = await git(
					"-C",
					cache,
					"remote",
					"get-url",
					"--all",
					"origin",
				);
				if (origin.trim() !== expectedOrigin)
					return denied("CACHE_ORIGIN_MISMATCH");
				// No force, tag/submodule fetch, auto maintenance or working tree.
				await git(
					"-C",
					cache,
					"fetch",
					"--no-tags",
					"--no-recurse-submodules",
					"--no-auto-gc",
					"--no-write-fetch-head",
					"origin",
					...refspecs,
				);
				const options = {
					env: {
						...env,
						CHATQUEUE_BROKER_ORIGIN: expectedOrigin,
						CHATQUEUE_INCLUDE_PROMPT_BLOB: "1",
						CHATQUEUE_PULSE_QUERY: JSON.stringify(pulseQuery),
					},
					encoding: "utf8",
					timeout: 45_000,
					maxBuffer: 1024 * 1024,
				};
				let stdout;
				try {
					({ stdout } = await exec(
						process.execPath,
						[policySource, cache, agent],
						options,
					));
				} catch (error) {
					if (error?.code !== 2) return denied("CACHE_POLICY_READ_UNVERIFIED");
					stdout = error.stdout;
				}
				return { agent, ...sanitize(agent, JSON.parse(stdout)) };
			} catch {
				return denied("CACHE_REFRESH_UNVERIFIED");
			} finally {
				reading = false;
			}
		},
		async close() {
			if (reading) throw new Error("Cannot remove a cache with an active read");
			closed = true;
			await rm(root, { recursive: true, force: true });
		},
	};
}

// Explicit start only. The generated credential is never printed. Do not
// invoke this from bootstrap, make all or a scheduler. No automatic browser
// configuration/installation is included.
async function main() {
	if (process.argv[2] !== "--serve")
		throw new Error("Explicit --serve required");
	const cache = await createPrivatePolicyCache({
		runtimeDir: process.env.XDG_RUNTIME_DIR,
		expectedOrigin: process.env.CHATQUEUE_BROKER_ORIGIN,
	});
	let bridge;
	try {
		const capability = randomBytes(32).toString("hex");
		bridge = await startPolicyBridge({
			capability,
			readPolicy: cache.readPolicy,
		});
		const settings = join(cache.root, "bridge.json");
		await writeFile(
			settings,
			JSON.stringify({ version: 1, port: bridge.port, capability }) + "\n",
			{ mode: 0o600, flag: "wx" },
		);
		process.stdout.write(
			"Passive policy bridge ready; private settings generated in runtime directory.\n",
		);
		await new Promise((resolve) => {
			process.once("SIGINT", resolve);
			process.once("SIGTERM", resolve);
		});
	} finally {
		if (bridge) await bridge.stop();
		// Do not delete a cache while an outstanding read is in progress.
		// A refused cleanup leaves only this private generated runtime directory.
		await cache.close();
	}
}
if (process.argv[1] === fileURLToPath(import.meta.url)) {
	main().catch(() => {
		process.stderr.write(
			"Policy bridge failed closed; no send authorization.\n",
		);
		process.exitCode = 2;
	});
}
