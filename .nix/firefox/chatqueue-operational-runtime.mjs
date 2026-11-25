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
	freshPausedOperationalEvidence,
	OPERATIONAL_PATH,
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
			let packet = await currentEvidence(l);
			if (!validPacket(packet, l.proofQuery ?? null)) {
				// At most one fresh policy reread, still before any click.
				// No recomposition, cached admission, second attempt or replay.
				const s = await readState();
				if (
					s?.phase === PHASES.PREPARED &&
					s.attemptId === attemptId &&
					lease === l &&
					!(await paused()) &&
					dom.ready(l.binding, content) &&
					dom.receiptCount(content) === 0
				)
					packet = await currentEvidence(l);
			}
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
	// Recovery is a distinct, explicitly invoked no-send path. A fresh
	// paused policy packet is NOT accepted by arm/prepare/click.
	const validPausedPacket = (packet, query = null) =>
		!!packet &&
		packet.context?.agent === agent &&
		freshPausedOperationalEvidence(packet.value, {
			...packet.context,
			pulseQuery: query,
			now: clock(),
		});
	const recoveryAttested = (a) =>
		a?.manualRecovery === true &&
		a?.accountModelVerified === true &&
		a?.qaVerified === true;
	const locallyStopped = (gate) =>
		!lease &&
		!active &&
		timer === null &&
		gate?.stopped === true &&
		gate.armId === null &&
		typeof gate.epoch === "string";
	const sameStopped = async (config, gate) =>
		locallyStopped(gate) &&
		same(await getConfig(), config) &&
		same(await read("GATE"), gate) &&
		locallyStopped(gate);
	const readPausedPacket = (config, query = null) =>
		lock("CQOP_V1:POLICY_READ", false, () =>
			readEvidence(config, agent, query, "paused-recovery"),
		);

	async function reconcileSupervisedReview(attestation) {
		if (!booted || !recoveryAttested(attestation)) return false;
		return (
			(await exclusive(async () => {
				const config = await getConfig(),
					b = binding(config);
				const gate = await read("GATE");
				const state = await readState();
				const journal = await read("SUPERVISED");
				if (
					!b ||
					!locallyStopped(gate) ||
					state?.phase !== PHASES.REVIEW ||
					state.cycle < 1 ||
					!opValidTime(clock()) ||
					(state.lastReplyAt !== null && state.lastReplyAt > clock()) ||
					!journal ||
					journal.path !== path ||
					journal.promptBlob !== config.promptBlob ||
					journal.cycle !== state.cycle ||
					journal.attemptId !== state.attemptId
				)
					return false;

				// The journal is written first. A crash before STATE commits can
				// only retry this same review, with the original exact Git pair.
				const pending =
					journal.phase === "PREPARED" &&
					journal.lastReplyAt === null &&
					!journal.gitProof &&
					!journal.reconciledReview;
				const priorProof = sanitizePulseProof(journal.gitProof);
				const partial =
					journal.phase === "REPLY_TERMINAL" &&
					journal.reconciledReview?.method ===
						"EXACT_RECEIPT_AND_OWN_GIT_PAIR" &&
					same(journal.reconciledReview.previousState, state) &&
					opValidTime(journal.lastReplyAt) &&
					journal.lastReplyAt <= clock() &&
					(state.lastReplyAt === null ||
						journal.lastReplyAt >= state.lastReplyAt) &&
					!!priorProof;
				if (!pending && !partial) return false;
				const query = {
					attemptId: journal.attemptId,
					baselineMain: journal.baselineMain,
					...(partial ? { terminalCommit: priorProof.terminalCommit } : {}),
				};
				if (!validatePulseQuery(query)) return false;
				const content = operationalMessage(b, state.attemptId);
				const observed = await dom.waitTerminal(
					b,
					content,
					async () => !(await sameStopped(config, gate)),
					15000,
				);
				if (
					observed?.observedFinalAssistantResponse !== true ||
					observed.noActiveGeneration !== true ||
					typeof observed.receiptText !== "string" ||
					!observed.receiptText
				)
					return false;
				const packet = await readPausedPacket(config, query);
				if (!validPausedPacket(packet, query)) return false;
				const proof = sanitizePulseProof(packet.value.pulseProof);
				if (!pulseProofMatches(proof, agent, query, packet.value.heads.main))
					return false;
				if (partial && !same({ ...proof, main: priorProof.main }, priorProof))
					return false;
				return await fence(async () => {
					const stillVerified = async () =>
						(await sameStopped(config, gate)) &&
						same(await readState(), state) &&
						validPausedPacket(packet, query) &&
						dom.terminalSnapshot(b, content) === observed.receiptText;
					if (
						!(await stillVerified()) ||
						!same(await read("SUPERVISED"), journal)
					)
						return false;
					const resolved = pending
						? {
								...journal,
								phase: "REPLY_TERMINAL",
								lastReplyAt: clock(),
								gitProof: proof,
								reconciledReview: {
									method: "EXACT_RECEIPT_AND_OWN_GIT_PAIR",
									previousState: state,
								},
							}
						: journal;
					if (pending) await writeChecked("SUPERVISED", resolved);
					if (
						!(await stillVerified()) ||
						!same(await read("SUPERVISED"), resolved)
					)
						return false;
					await writeState({
						...state,
						phase: PHASES.PAUSED,
						attemptId: null,
						nextEligibleAt: null,
						lastReplyAt: resolved.lastReplyAt,
						pendingReason:
							"Exact supervised review reconciled; separate manual GO required",
					});
					return true;
				});
			})) === true
		);
	}

	// This is deliberately not generic REVIEW recovery. The exact installed
	// predecessor emits this reason only before entering the click fence.
	// A live draft corroborates that path; a current OWN Git pair and exact
	// prior terminal receipt exclude a newer accepted invocation.
	async function reconcilePreclickReview(attestation, priorQuery) {
		if (
			!booted ||
			!recoveryAttested(attestation) ||
			attestation.precisePreclickSourceVerified !== true ||
			!validatePulseQuery(priorQuery) ||
			priorQuery.terminalCommit !== undefined
		)
			return false;
		return (
			(await exclusive(async () => {
				const config = await getConfig(),
					b = binding(config);
				const gate = await read("GATE"),
					state = await readState();
				const journal = await read("SUPERVISED");
				if (
					!b ||
					!locallyStopped(gate) ||
					state?.phase !== PHASES.REVIEW ||
					state.cycle < 2 ||
					!opValidTime(state.lastReplyAt) ||
					state.lastReplyAt > clock() ||
					state.pendingReason !==
						"Reconcile exact attempt " +
							state.attemptId +
							" before any retry (Final Git policy not proven)" ||
					priorQuery.attemptId === state.attemptId ||
					!supervisedQuery(journal, state, config) ||
					!sanitizePulseProof(journal.gitProof) ||
					journal.cycle >= state.cycle
				)
					return false;
				const { preclickReviews = [], ...original } = state;
				if (
					!Array.isArray(preclickReviews) ||
					preclickReviews.length >= 64 ||
					preclickReviews.some(
						(r, i) =>
							r?.phase !== "CANCELLED_BEFORE_CLICK" &&
							!(
								i === preclickReviews.length - 1 &&
								r?.phase === "DRAFT_CLEAR_PENDING"
							),
					)
				)
					return false;
				const pending =
					preclickReviews.at(-1)?.phase === "DRAFT_CLEAR_PENDING"
						? preclickReviews.at(-1)
						: null;
				if (
					pending &&
					(!same(pending.previousState, original) ||
						!same(pending.query, priorQuery) ||
						!sanitizePulseProof(pending.priorGitProof))
				)
					return false;
				if (
					preclickReviews.some(
						(r) =>
							r !== pending && r?.previousState?.attemptId === state.attemptId,
					)
				)
					return false;
				const content = operationalMessage(b, state.attemptId);
				const prior = operationalMessage(b, priorQuery.attemptId);
				const snapshot = dom.unsentSnapshot(b, content, prior);
				if (
					!snapshot ||
					(!pending && snapshot.draft !== "exact") ||
					!["exact", "empty"].includes(snapshot.draft)
				)
					return false;
				const packet = await readPausedPacket(config, priorQuery);
				if (!validPausedPacket(packet, priorQuery)) return false;
				const proof = sanitizePulseProof(packet.value.pulseProof);
				if (
					!pulseProofMatches(proof, agent, priorQuery, packet.value.heads.main)
				)
					return false;
				if (
					pending &&
					!same(
						{ ...proof, main: pending.priorGitProof.main },
						pending.priorGitProof,
					)
				)
					return false;
				return await fence(async () => {
					let staged = state;
					const verified = async () =>
						(await sameStopped(config, gate)) &&
						same(await readState(), staged) &&
						same(await read("SUPERVISED"), journal) &&
						validPausedPacket(packet, priorQuery);
					if (
						!(await verified()) ||
						!same(dom.unsentSnapshot(b, content, prior), snapshot)
					)
						return false;
					const record = pending ?? {
						method: "EXACT_PRECLICK_REASON_DRAFT_AND_PRIOR_OWN_GIT_PAIR",
						phase: "DRAFT_CLEAR_PENDING",
						previousState: original,
						query: priorQuery,
						priorGitProof: proof,
					};
					const history = pending
						? preclickReviews.slice(0, -1)
						: preclickReviews;
					if (!pending) {
						staged = { ...state, preclickReviews: [...history, record] };
						await writeState(staged);
					}
					if (
						!(await verified()) ||
						!same(dom.unsentSnapshot(b, content, prior), snapshot)
					)
						return false;
					// Persist cancellation intent before removing only the exact
					// unsent draft. Interrupted writes/clears leave REVIEW and the
					// same attempt/query; no automatic retry of uncertain writes.
					if (snapshot.draft === "exact")
						await dom.clearUnsentDraft(b, content, prior);
					const after = dom.unsentSnapshot(b, content, prior);
					if (
						!(await verified()) ||
						!after ||
						after.draft !== "empty" ||
						after.reply !== snapshot.reply ||
						!same(dom.unsentSnapshot(b, content, prior), after)
					)
						return false;
					await writeState({
						...staged,
						preclickReviews: [
							...history,
							{ ...record, phase: "CANCELLED_BEFORE_CLICK" },
						],
						phase: PHASES.PAUSED,
						attemptId: null,
						nextEligibleAt: null,
						pendingReason:
							"Exact pre-click attempt cancelled without send; separate manual GO required",
					});
					return true;
				});
			})) === true
		);
	}

	async function migratePristineBinding(attestation) {
		if (
			!recoveryAttested(attestation) ||
			attestation.oldDocumentsRetired !== true
		)
			return false;
		return (
			(await exclusive(async () => {
				const config = await getConfig(),
					b = binding(config);
				const gate = await read("GATE");
				const previous = await read("STATE");
				const s = previous?.state;
				// Deliberately scoped to a never-prepared, zero-cycle journal.
				// Any attempt/history or old supervised proof needs a distinct
				// reviewed migration. Never discard it or inherit its authority.
				if (
					!b ||
					config.reviewed !== false ||
					!locallyStopped(gate) ||
					!OPERATIONAL_PATH.test(previous?.path ?? "") ||
					Object.keys(previous).sort().join(",") !== "path,state" ||
					previous.path === path ||
					previous.path.split("/")[2] !== path.split("/")[2] ||
					config.bindings.some((item) => item.path === previous.path) ||
					!validState(s, agent) ||
					s.phase !== PHASES.PAUSED ||
					s.cycle !== 0 ||
					s.attemptId !== null ||
					s.lastReplyAt !== null ||
					(await read("SUPERVISED")) !== null ||
					!dom.ready(b, "", true)
				)
					return false;
				const packet = await readPausedPacket(config);
				if (!validPausedPacket(packet)) return false;
				return await fence(async () => {
					if (
						!(await sameStopped(config, gate)) ||
						!same(await read("STATE"), previous) ||
						(await read("SUPERVISED")) !== null ||
						!validPausedPacket(packet) ||
						!dom.ready(b, "", true)
					)
						return false;
					// Preserve the entire existing state; only change its private
					// binding envelope, under witnessed ROLE/FENCE locks.
					await writeState(s);
					booted = true;
					return true;
				});
			})) === true
		);
	}

	async function pause() {
		await revoke();
		await controller.boot(agent);
	}
	async function status() {
		const cfg = await getConfig();
		const envelope = await read("STATE");
		const state = envelope?.state ?? null;
		if (envelope && !validState(state, agent))
			throw new Error("Invalid role state");
		const bindingMigrationRequired = !!envelope && envelope.path !== path;
		const journal = await read("SUPERVISED");
		return {
			configured: !!binding(cfg),
			bindingMigrationRequired,
			pendingReason:
				typeof state?.pendingReason === "string"
					? state.pendingReason.slice(0, 256)
					: null,
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
	return {
		boot,
		arm,
		pause,
		status,
		reconcileSupervisedReview,
		reconcilePreclickReview,
		migratePristineBinding,
	};
}
