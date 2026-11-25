// Operational integration of the tested recurrence controller and fresh policy
// evidence. Dependencies are injected; no browser or timer runs on import.
import {
	PHASES,
	REPLY_COOLDOWN_MS,
	authorizeFirstTick,
	recoverAfterRestart,
} from "./chatqueue-recurrence-core.mjs";
import { makeLocalController } from "./chatqueue-local-controller.mjs";
import {
	validateOperationalConfig,
	freshOperationalEvidence,
	operationalMessage,
} from "./chatqueue-operational-client.mjs";
import {
	validatePulseQuery,
	sanitizePulseProof,
	pulseProofMatches,
} from "./chatqueue-pulse-proof-schema.mjs";

const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
const opValidTime = (value) => Number.isSafeInteger(value) && value >= 0;
const validAttempt = (value) =>
	typeof value === "string" && /^cqop-[0-9a-f-]{36}$/.test(value);
const validState = (s, agent) => {
	if (
		!s ||
		s.version !== 1 ||
		s.agentId !== agent ||
		!Object.values(PHASES).includes(s.phase) ||
		!opValidTime(s.cycle) ||
		!(s.lastReplyAt === null || opValidTime(s.lastReplyAt))
	)
		return false;
	if ([PHASES.PREPARED, PHASES.AWAITING_REPLY, PHASES.REVIEW].includes(s.phase))
		return validAttempt(s.attemptId) && s.nextEligibleAt === null;
	if (s.attemptId !== null) return false;
	if (s.phase === PHASES.PAUSED) return s.nextEligibleAt === null;
	return (
		opValidTime(s.nextEligibleAt) &&
		(s.phase !== PHASES.COOLDOWN ||
			(opValidTime(s.lastReplyAt) &&
				s.nextEligibleAt >= s.lastReplyAt + REPLY_COOLDOWN_MS))
	);
};

export function makeOperationalRuntime({
	agent,
	path,
	getConfig,
	read,
	write,
	lock,
	clock,
	randomId,
	setTimer,
	cancelTimer,
	readEvidence,
	dom,
}) {
	let lease = null;
	let timer = null;
	let booted = false;
	let active = null;
	const exclusive = (fn) => lock("CQOP_V1:ROLE:" + agent, true, fn);
	const fence = (fn) => lock("CQOP_V1:FENCE:" + agent, false, fn);
	const writeChecked = async (key, value) => {
		await write(key, value);
		if (!same(await read(key), value))
			throw new Error("Storage readback unverified");
	};
	const stopTimer = () => {
		if (timer !== null) cancelTimer(timer);
		timer = null;
	};
	const binding = (config) =>
		validateOperationalConfig(config)
			? config.bindings.find((b) => b.agent === agent && b.path === path)
			: null;
	const configCurrent = async (l) => {
		const current = await getConfig();
		return (
			!!l &&
			lease === l &&
			!!binding(current) &&
			current.reviewed === true &&
			same(current, l.config)
		);
	};
	const paused = async () => {
		const l = lease;
		if (!l) return true;
		const gate = await read("GATE");
		return (
			lease !== l ||
			!same(gate, l.gate) ||
			gate?.stopped !== false ||
			!(await configCurrent(l))
		);
	};
	const revoke = async () => {
		lease = null;
		stopTimer();
		await fence(async () => {
			lease = null;
			stopTimer();
			await writeChecked("GATE", {
				epoch: randomId(),
				stopped: true,
				armId: null,
			});
		});
	};
	const stopOwn = async (l) => {
		if (lease !== l) return;
		lease = null;
		stopTimer();
		await fence(async () => {
			if (same(await read("GATE"), l.gate))
				await writeChecked("GATE", {
					epoch: randomId(),
					stopped: true,
					armId: null,
				});
		});
	};
	const readState = async () => {
		const envelope = await read("STATE");
		if (!envelope) return null;
		if (envelope.path !== path || !validState(envelope.state, agent))
			throw new Error("Prior role binding requires manual reconciliation");
		return envelope.state;
	};
	const writeState = async (state) => {
		if (!validState(state, agent)) throw new Error("Invalid role state");
		await writeChecked("STATE", { path, state });
	};
	const currentEvidence = async (l) => {
		if (lease !== l || (await paused())) return null;
		const packet = await lock("CQOP_V1:POLICY_READ", false, async () => {
			if (lease !== l || (await paused())) return null;
			return await readEvidence(l.config, agent, l.proofQuery ?? null);
		});
		if (lease !== l || (await paused())) return null;
		return packet;
	};
	const validPacket = (packet, pulseQuery = null) =>
		!!packet &&
		packet.context?.agent === agent &&
		freshOperationalEvidence(packet.value, {
			...packet.context,
			pulseQuery,
			now: clock(),
		});
	const supervisedQuery = (journal, state, config) => {
		if (
			!journal ||
			journal.phase !== "REPLY_TERMINAL" ||
			journal.path !== path ||
			journal.promptBlob !== config.promptBlob ||
			!Number.isSafeInteger(journal.cycle) ||
			journal.cycle < 1 ||
			!state ||
			journal.cycle > state.cycle ||
			!opValidTime(journal.lastReplyAt) ||
			!opValidTime(state.lastReplyAt) ||
			journal.lastReplyAt > state.lastReplyAt
		)
			return null;
		const q = {
			attemptId: journal.attemptId,
			baselineMain: journal.baselineMain,
			...(journal.gitProof
				? { terminalCommit: journal.gitProof.terminalCommit }
				: {}),
		};
		if (
			!validatePulseQuery(q) ||
			(journal.gitProof &&
				(!sanitizePulseProof(journal.gitProof) ||
					!pulseProofMatches(
						journal.gitProof,
						agent,
						q,
						journal.gitProof.main,
					)))
		)
			return null;
		return q;
	};
	const transport = {
		preflight: async (_agent, content) => {
			const l = active?.lease;
			const p = l && (await currentEvidence(l));
			const state = await readState();
			const priorBrowserTerminal =
				!!state &&
				[PHASES.READY, PHASES.COOLDOWN].includes(state.phase) &&
				state.attemptId === null;
			const valid =
				!!l &&
				priorBrowserTerminal &&
				validPacket(p, l?.proofQuery ?? null) &&
				!(await paused()) &&
				active?.lease === l &&
				content === active.content &&
				dom.ready(l.binding, "");
			return {
				identityVerified: valid,
				notGenerating: valid,
				threeLegacyGatesOff: valid,
				// The controller checks delivery state separately; the agent
				// must reconcile any external UNKNOWN under the canonical contract.
				browserDeliveryReconciled: priorBrowserTerminal,
				externalEffectDisposition: "AGENT_MUST_RECONCILE",
			};
		},
		compose: async (_agent, content) => {
			const l = active?.lease;
			if (
				!l ||
				lease !== l ||
				(await paused()) ||
				content !== active.content ||
				!dom.ready(l.binding, "")
			)
				throw new Error("Compose admission denied");
			await dom.compose(content);
			if ((await paused()) || !dom.ready(l.binding, content))
				throw new Error("Draft changed or pause requested");
		},
		deliver: async (_agent, content, attemptId) => {
			const l = active?.lease;
			if (!l || lease !== l || content !== active.content)
				throw new Error("Attempt mismatch");
			// No remote IO while holding the short fence. Recheck freshness inside it.
			const packet = await currentEvidence(l);
			if (!validPacket(packet, l.proofQuery ?? null))
				throw new Error("Final Git policy not proven");
			await fence(async () => {
				const state = await readState();
				if (
					state?.phase !== PHASES.PREPARED ||
					state.attemptId !== attemptId ||
					(await paused()) ||
					lease !== l ||
					!validPacket(packet, l.proofQuery ?? null) ||
					!dom.ready(l.binding, content) ||
					dom.receiptCount(content) !== 0
				)
					throw new Error("Final send admission denied");
				if (l.mode === "supervised") {
					// Persist the exact final Git baseline before crossing the click
					// boundary. A restart or uncertain send cannot manufacture proof.
					await writeChecked("SUPERVISED", {
						phase: "PREPARED",
						path,
						promptBlob: l.config.promptBlob,
						cycle: state.cycle,
						attemptId,
						baselineMain: packet.value.heads.main,
						lastReplyAt: null,
					});
				}
				// The durable write may have yielded to Pause or invalidated freshness.
				const current = await readState();
				if (
					current?.phase !== PHASES.PREPARED ||
					current.attemptId !== attemptId ||
					(await paused()) ||
					lease !== l ||
					!validPacket(packet, l.proofQuery ?? null) ||
					!dom.ready(l.binding, content) ||
					dom.receiptCount(content) !== 0
				)
					throw new Error("Final send admission changed");
				// No await between final checks and the synchronous click.
				dom.click(l.binding, content);
			});
			return await dom.waitSent(
				content,
				async () => lease !== l || (await paused()),
			);
		},
		replyTerminal: async () => {
			const attempt = active;
			const l = attempt?.lease;
			if (!l || lease !== l) throw new Error("Attempt absent");
			return await dom.waitTerminal(
				l.binding,
				attempt.content,
				async () => lease !== l || (await paused()),
			);
		},
	};
	const controller = makeLocalController({
		read: readState,
		write: (_agent, state) => writeState(state),
		withLock: (_key, fn) => exclusive(fn),
		clock,
		transport,
		paused,
		admission: "broker",
	});
	async function boot() {
		const cfg = await getConfig();
		if (!binding(cfg)) return false;
		await revoke();
		const result = await controller.boot(agent);
		booted = !!result && result.agentId === agent;
		return booted;
	}
	async function schedule(delay, l) {
		if (!l || lease !== l) return;
		await fence(async () => {
			if (lease !== l || (await paused())) return;
			stopTimer();
			const handle = setTimer(
				async () => {
					if (timer !== handle) return;
					timer = null;
					let attempt = null;
					try {
						if (lease !== l || (await paused()) || lease !== l || active)
							return;
						const attemptId = "cqop-" + randomId();
						attempt = {
							lease: l,
							content: operationalMessage(l.binding, attemptId),
						};
						active = attempt;
						const result = await controller.tick(
							agent,
							attempt.content,
							attemptId,
						);
						if (result?.status !== "COOLDOWN") {
							await stopOwn(l);
							return;
						}
						if (l.mode === "supervised") {
							await exclusive(() =>
								fence(async () => {
									const state = await readState();
									const journal = await read("SUPERVISED");
									if (
										(await paused()) ||
										lease !== l ||
										state?.phase !== PHASES.COOLDOWN ||
										journal?.phase !== "PREPARED" ||
										journal.attemptId !== attemptId ||
										journal.cycle !== state.cycle ||
										journal.path !== path ||
										journal.promptBlob !== l.config.promptBlob
									)
										return;
									// Receipt is only conversational evidence, not Git proof.
									await writeChecked("SUPERVISED", {
										...journal,
										phase: "REPLY_TERMINAL",
										lastReplyAt: state.lastReplyAt,
									});
								}),
							);
							await stopOwn(l);
						} else {
							if (active === attempt) active = null;
							await schedule(Math.max(0, result.nextEligibleAt - clock()), l);
						}
					} catch {
						await stopOwn(l);
					} finally {
						if (active === attempt) active = null;
					}
				},
				Math.max(1, delay),
			);
			timer = handle;
		});
	}
	async function arm(mode, attestation) {
		// Only an explicit signed-menu action calls this; neither boot nor
		// config/proof retrieval can arm. The separate remote GO is required.
		if (
			!booted ||
			active ||
			!["supervised", "recurrence"].includes(mode) ||
			attestation?.manualGo !== true ||
			attestation?.accountModelVerified !== true ||
			attestation?.originalQueueRetired !== true ||
			attestation?.qaVerified !== true
		)
			return false;
		const config = await getConfig();
		const b = binding(config);
		if (!b || config.reviewed !== true || !dom.ready(b, "", true)) return false;
		const before = await read("GATE");
		if (before?.stopped !== true || typeof before.epoch !== "string")
			return false;
		const journal = mode === "recurrence" ? await read("SUPERVISED") : null;
		const query =
			mode === "recurrence"
				? supervisedQuery(journal, await readState(), config)
				: null;
		if (mode === "recurrence" && !query) return false;
		const evidence = await lock("CQOP_V1:POLICY_READ", false, () =>
			readEvidence(config, agent, query),
		);
		if (!validPacket(evidence, query)) return false;
		const armed = await exclusive(async () => {
			if (
				!same(await getConfig(), config) ||
				!same(await read("GATE"), before) ||
				!dom.ready(b, "", true) ||
				!validPacket(evidence, query)
			)
				return false;
			let state = await readState();
			if (!state) return false;
			if (mode === "recurrence") {
				if (
					!same(await read("SUPERVISED"), journal) ||
					!same(supervisedQuery(journal, state, config), query)
				)
					return false;
				const proof = sanitizePulseProof(evidence.value.pulseProof);
				if (!pulseProofMatches(proof, agent, query, evidence.value.heads.main))
					return false;
				// A fresh, independently read Git pair is mandatory. Preserve it
				// for future rearm, which rechecks ancestry and exact commits.
				await writeChecked("SUPERVISED", { ...journal, gitProof: proof });
			}
			if (state.phase === PHASES.COOLDOWN) {
				// Preserve the original cooldown even after manual rearm.
				const eligible = state.nextEligibleAt;
				state = recoverAfterRestart(state);
				if (
					state.lastReplyAt === null ||
					eligible < state.lastReplyAt + REPLY_COOLDOWN_MS
				)
					return false;
			}
			if (state.phase !== PHASES.PAUSED) return false;
			const notBefore = Math.max(
				clock(),
				state.nextEligibleAt ?? 0,
				Number.isSafeInteger(state.lastReplyAt)
					? state.lastReplyAt + REPLY_COOLDOWN_MS
					: 0,
			);
			const ready = authorizeFirstTick(
				state,
				{
					manualGo: true,
					threeLegacyGatesOff: true,
					exclusiveScheduler: true,
					targetVerified: true,
					accountVerified: true,
					bootstrapSameChatVerified: true,
					noOtherGeneration: true,
					recurrenceE2EPassed: true,
				},
				clock(),
			);
			await writeState({ ...ready, nextEligibleAt: notBefore });
			return await fence(async () => {
				if (
					!same(await read("GATE"), before) ||
					!same(await getConfig(), config) ||
					(mode === "recurrence" &&
						!same(await read("SUPERVISED"), {
							...journal,
							gitProof: sanitizePulseProof(evidence.value.pulseProof),
						})) ||
					!validPacket(evidence, query) ||
					!dom.ready(b, "", true)
				) {
					await writeState(recoverAfterRestart(ready));
					return false;
				}
				const gate = { epoch: before.epoch, stopped: false, armId: randomId() };
				await writeChecked("GATE", gate);
				const proofQuery =
					mode === "recurrence"
						? {
								...query,
								terminalCommit: evidence.value.pulseProof.terminalCommit,
							}
						: null;
				const l = { gate, config, binding: b, mode, proofQuery };
				lease = l;
				if (await paused()) {
					lease = null;
					await writeChecked("GATE", {
						epoch: randomId(),
						stopped: true,
						armId: null,
					});
					return false;
				}
				return l;
			});
		});
		if (!armed || armed !== lease) return false;
		const state = await readState();
		await schedule(Math.max(0, state.nextEligibleAt - clock()), armed);
		return true;
	}
	async function pause() {
		await revoke();
		await controller.boot(agent);
	}
	async function status() {
		const cfg = await getConfig();
		const state = await readState();
		const journal = await read("SUPERVISED");
		return {
			configured: !!binding(cfg),
			reviewed: cfg?.reviewed === true,
			phase: state?.phase ?? "NONE",
			cycle: state?.cycle ?? 0,
			attemptId: state?.attemptId ?? null,
			stopped: (await read("GATE"))?.stopped !== false,
			leased: !!lease,
			supervised: journal?.phase === "REPLY_TERMINAL",
			supervisedAttemptId: journal?.attemptId ?? null,
			supervisedGitProofStored: !!sanitizePulseProof(journal?.gitProof),
		};
	}
	return { boot, arm, pause, status };
}
