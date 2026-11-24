// Browser adapter for a LAB-ONLY, opt-in Violentmonkey userscript.
// The signed ChatQueue XPI remains untouched and its global queue MUST stay
// paused. Production agent prompts are deliberately rejected in this release.
(() => {
	"use strict";
	const VERSION = "0.1.0-lab";
	const MAX_CYCLES = 2;
	const LAB_PREFIX = "CQLOCAL LAB:";
	const origin = "https://chatgpt.com";
	const url = location.origin + location.pathname;
	if (
		location.origin !== origin ||
		!/^\/c\/[a-zA-Z0-9-]+$/.test(location.pathname)
	)
		return;
	if (
		typeof GM_getValue !== "function" ||
		typeof GM_setValue !== "function" ||
		typeof GM_registerMenuCommand !== "function"
	)
		return;
	const storageKey = (kind) => "CQLOCAL_V1:" + url + ":" + kind;
	const read = async (kind) => await GM_getValue(storageKey(kind), null);
	const write = async (kind, value) =>
		await GM_setValue(storageKey(kind), value);
	const locks = navigator.locks;
	let timer = null;
	let currentRole = null;
	let beforeAssistant = new Set();
	let sentUser = null;
	const sleep = (ms) => new Promise((done) => setTimeout(done, ms));
	const normalized = (text) =>
		String(text || "")
			.replace(/\s+/g, " ")
			.trim();
	const visible = (el) =>
		!!el &&
		!el.hidden &&
		el.getAttribute?.("aria-hidden") !== "true" &&
		(typeof el.getClientRects !== "function" || el.getClientRects().length > 0);
	// Live Firefox ChatGPT uses virtualized turns; generic old role nodes are absent.
	// Global user bubbles are duplicated, so only inspect bubbles inside a turn.
	const messageNodes = (role) => {
		const legacy = [
			...document.querySelectorAll('[data-message-author-role="' + role + '"]'),
		].filter(visible);
		const modern = [];
		for (const turn of document.querySelectorAll("[data-virtualized-turn]")) {
			if (!visible(turn)) continue;
			if (role === "user") {
				const bubbles = [
					...turn.querySelectorAll("[data-user-message-bubble]"),
				].filter(
					(el) =>
						visible(el) &&
						!el.parentElement?.closest("[data-user-message-bubble]"),
				);
				if (bubbles.length === 1) modern.push(bubbles[0]);
			} else if (role === "assistant") {
				const headings = [
					...turn.querySelectorAll('[data-conversation-role="assistant"]'),
				];
				if (headings.length !== 1) continue;
				const group = headings[0].closest("div.group.flex.flex-col");
				if (group && turn.contains(group) && visible(group)) modern.push(group);
			}
		}
		// Mixed dialects are ambiguous. Never silently select one.
		if (legacy.length && modern.length) return [];
		return legacy.length ? legacy : modern;
	};
	const stopButton = () =>
		[
			...document.querySelectorAll(
				'button[data-testid="stop-button"],button[aria-label*="Stop"],button[aria-label*="Parar"],button[aria-label*="Interromper"]',
			),
		].some(visible);
	// Never equate a six-second text pause with response completion.
	// In ChatGPT this is deliberately conservative: if the current assistant
	// turn exposes no corroborating end-of-turn action, fail closed in REVIEW.
	// The cq-local-terminal-receipt marker exists ONLY in our loopback fixture.
	const completedTurnWitness = (turn) => {
		if (!turn?.querySelector) return false;
		const receipt = turn.querySelector(
			'[data-testid="copy-turn-action-button"],' +
				'button[aria-label="Copy"],button[aria-label="Copiar"],' +
				'[data-testid="cq-local-terminal-receipt"]',
		);
		return visible(receipt) && !receipt.disabled;
	};
	const composer = () => {
		const active = [
			...document.querySelectorAll("form[data-chatgpt-composer]"),
		].filter(visible);
		// Live ChatGPT also renders an invisible duplicate form.
		return active.length === 1 ? active[0] : null;
	};
	const composeBox = () =>
		composer()?.querySelector(
			'[contenteditable="true"][role="textbox"],[contenteditable="true"],textarea',
		);
	const sendButton = () => {
		// The authenticated ChatGPT editor has NO submit button when blank:
		// it appears only after composing a draft. Accept either a uniquely
		// identified send button or a native form submit, never voice/dictation.
		const options = [
			...(composer()?.querySelectorAll(
				'button[data-testid="send-button"],button[type="submit"]',
			) || []),
		];
		if (options.length !== 1) return null;
		const btn = options[0];
		if (
			!visible(btn) ||
			btn.disabled ||
			/stop|cancel|pause|parar/i.test(btn.getAttribute("aria-label") || "")
		)
			return null;
		return btn;
	};
	const accountReady = () =>
		!document.querySelector(
			'[data-testid="login-button"],a[href*="/auth/login"]',
		);
	const manualSeedReady = () => {
		const users = messageNodes("user");
		const assistants = messageNodes("assistant");
		const user = users.at(-1);
		const assistant = assistants.at(-1);
		const userTurn = user?.closest?.("[data-virtualized-turn]");
		const assistantTurn = assistant?.closest?.("[data-virtualized-turn]");
		return (
			!!user &&
			!!assistant &&
			users.length === assistants.length &&
			!(!userTurn && assistantTurn) &&
			!(!assistantTurn && userTurn) &&
			(!userTurn || userTurn === assistantTurn) &&
			normalized(user.innerText ?? user.textContent).startsWith(LAB_PREFIX) &&
			completedTurnWitness(assistant)
		);
	};
	const targetReady = () =>
		location.origin + location.pathname === url &&
		!stopButton() &&
		accountReady() &&
		!!composer() &&
		!!composeBox() &&
		!normalized(composeBox()?.innerText ?? composeBox()?.value) &&
		manualSeedReady();

	async function withLock(_name, callback) {
		if (!locks?.request) throw new Error("Firefox LockManager unavailable");
		return await locks.request(
			url + ":chatqueue-local",
			{ ifAvailable: true, mode: "exclusive" },
			async (lock) => {
				if (!lock) return { status: "SKIP_IN_PROGRESS" };
				return await callback();
			},
		);
	}
	const configMatches = async (agent, content) => {
		const config = await read("CONFIG");
		return (
			config?.lab === true &&
			config.target === url &&
			config.agentId === agent &&
			config.text === content &&
			content.startsWith(LAB_PREFIX)
		);
	};
	const transport = {
		preflight: async (agent, content) => ({
			identityVerified:
				(await configMatches(agent, content)) &&
				targetReady() &&
				currentRole === agent,
			notGenerating: !stopButton(),
			threeLegacyGatesOff: true, // LAB assertion; never use for production.
			noUnreconciledEffects: true, // LAB-only, no broker integrations.
		}),
		compose: async (_agent, content) => {
			if (!targetReady()) throw new Error("Composer not ready");
			const box = composeBox();
			box.focus();
			if (box.tagName === "TEXTAREA") {
				const setter = Object.getOwnPropertyDescriptor(
					HTMLTextAreaElement.prototype,
					"value",
				)?.set;
				if (!setter) throw new Error("Textarea setter unavailable");
				setter.call(box, content);
				box.dispatchEvent(new Event("input", { bubbles: true }));
			} else {
				const inserted = document.execCommand("insertText", false, content);
				if (!inserted) {
					// Firefox may reject execCommand on an unfocused background tab.
					// Change only this empty, isolated LAB composer, then emit input.
					// A real app may still reject it; the verified send-button gate
					// below must fail closed if its editor state was not updated.
					if (normalized(box.innerText ?? box.textContent))
						throw new Error("Refusing to replace existing draft");
					box.textContent = content;
					box.dispatchEvent(
						new InputEvent("input", {
							bubbles: true,
							composed: true,
							inputType: "insertText",
							data: content,
						}),
					);
				}
			}
			await sleep(400);
			const actual = normalized(box.innerText ?? box.value ?? box.textContent);
			if (actual !== normalized(content))
				throw new Error("Composer text verification failed");
		},
		deliver: async (agent, content) => {
			if (!(await configMatches(agent, content)) || (await read("STOP")))
				throw new Error("Configuration changed or LAB paused before click");
			if (
				stopButton() ||
				location.origin + location.pathname !== url ||
				!accountReady()
			)
				throw new Error("Conversation changed or model busy");
			const box = composeBox();
			if (
				!box ||
				normalized(box.innerText ?? box.value ?? box.textContent) !==
					normalized(content)
			)
				throw new Error("Composer changed before click");
			const btn = sendButton();
			if (!btn) throw new Error("Send button ambiguous or unavailable");
			beforeAssistant = new Set(messageNodes("assistant"));
			const priorUsers = new Set(messageNodes("user"));
			btn.click(); // Exactly once, AFTER durable PREPARED.
			for (let n = 0; n < 50; n++) {
				await sleep(500);
				const newUser = messageNodes("user").find(
					(item) =>
						!priorUsers.has(item) &&
						normalized(item.innerText ?? item.textContent) ===
							normalized(content),
				);
				if (newUser) {
					sentUser = newUser;
					return { sent: true };
				}
			}
			// No replay. A delivery that cannot be proved is REVIEW.
			throw new Error("Post-click user message not independently observed");
		},
		replyTerminal: async () => {
			if (!sentUser) throw new Error("No verified current user message");
			let stableSince = 0,
				prev = "";
			const deadline = Date.now() + 2 * 60 * 60 * 1000;
			while (Date.now() < deadline) {
				if (location.origin + location.pathname !== url)
					throw new Error("Chat switched during reply");
				if (await read("STOP"))
					throw new Error("LAB paused during uncertain reply");
				if (
					sentUser?.isConnected === false ||
					messageNodes("user").at(-1) !== sentUser
				)
					throw new Error("Current user turn disappeared or was superseded");
				const next = messageNodes("assistant").filter(
					(el) =>
						!beforeAssistant.has(el) &&
						!!normalized(el.innerText ?? el.textContent),
				);
				const latest = next.at(-1);
				// An earlier turn's visible Copy button must never certify this send.
				const userTurn = sentUser.closest?.("[data-virtualized-turn]");
				const replyTurn = latest?.closest?.("[data-virtualized-turn]");
				if (
					latest &&
					((userTurn && userTurn !== replyTurn) || (!userTurn && replyTurn))
				)
					throw new Error("Assistant receipt is not in sent user turn");
				const text = normalized(latest?.innerText ?? latest?.textContent);
				if (latest && !stopButton() && completedTurnWitness(latest)) {
					if (text !== prev) {
						prev = text;
						stableSince = Date.now();
					} else if (stableSince && Date.now() - stableSince >= 6000) {
						return {
							observedFinalAssistantResponse: true,
							noActiveGeneration: true,
						};
					}
				} else {
					stableSince = 0;
				}
				await sleep(500);
			}
			throw new Error("No corroborated terminal reply before deadline");
		},
	};
	const controller = makeLocalController({
		read: async (agent) => await read("STATE:" + agent),
		write: async (agent, state) => await write("STATE:" + agent, state),
		withLock,
		clock: () => Date.now(),
		paused: async () => !!(await read("STOP")),
		transport,
	});
	const clearTimer = () => {
		if (timer !== null) {
			clearTimeout(timer);
			timer = null;
		}
	};
	async function schedule(ms = 1500) {
		clearTimer();
		timer = setTimeout(async () => {
			timer = null;
			const config = await read("CONFIG");
			if (
				!config?.lab ||
				config.target !== url ||
				config.agentId !== currentRole
			)
				return;
			if (await read("STOP")) return;
			const state = await read("STATE:" + currentRole);
			if (!state || state.cycle >= MAX_CYCLES) {
				await write("STOP", true);
				return;
			}
			try {
				const res = await controller.tick(
					currentRole,
					config.text,
					"lab-" + crypto.randomUUID(),
				);
				if (res?.status === "COOLDOWN") {
					const latest = await read("STATE:" + currentRole);
					if (latest?.cycle >= MAX_CYCLES) await write("STOP", true);
					else await schedule(Math.max(2000, res.nextEligibleAt - Date.now()));
				} else if (
					res?.status === "SKIP_NOT_READY" ||
					res?.status === "NOT_DUE"
				) {
					await schedule(30000);
				} else {
					await write("STOP", true);
				}
			} catch {
				await write("STOP", true); // never automatically retry ambiguous failures
			}
		}, ms);
	}
	const menu = (text, handler) =>
		GM_registerMenuCommand("ChatQueue Local " + VERSION + " - " + text, () => {
			Promise.resolve()
				.then(handler)
				.catch(() =>
					window.alert(
						"ChatQueue Local: operation denied; see stored status. No automatic resend.",
					),
				);
		});
	menu("Status (no send)", async () => {
		const config = await read("CONFIG");
		const state = config?.agentId
			? await read("STATE:" + config.agentId)
			: null;
		window.alert(
			"LAB ONLY. ChatQueue original must stay PAUSED. " +
				"Configured=" +
				!!config +
				"; phase=" +
				(state?.phase ?? "NONE") +
				"; stopped=" +
				!!(await read("STOP")) +
				"; cycles=" +
				(state?.cycle ?? 0) +
				"; activeComposerUnique=" +
				!!composer() +
				"; manualSeedReady=" +
				manualSeedReady() +
				"; editorPresent=" +
				!!composeBox() +
				"; sendReady=" +
				!!sendButton() +
				"; stopPresent=" +
				stopButton() +
				"; assistantTurnCount=" +
				messageNodes("assistant").length +
				"; lastTurnHasCompletionAction=" +
				completedTurnWitness(messageNodes("assistant").at(-1)),
		);
	});
	menu("Configure LAB here (no send)", async () => {
		if (
			!window.confirm(
				"Configure a harmless LAB-only test on THIS chat? No broker agent prompts.",
			)
		)
			return;
		const agent = window.prompt(
			"LAB role (one of 4 permanent role names):",
			"coordinator",
		);
		if (!AGENTS.includes(agent)) return;
		const content = window.prompt(
			"Paste harmless text beginning exactly 'CQLOCAL LAB:' (no secrets):",
			"CQLOCAL LAB: Respond with LAB_ACK.",
		);
		if (!content?.startsWith(LAB_PREFIX) || content.length > 300) return;
		const configured = await withLock("configure", async () => {
			const previous = await read("CONFIG");
			if (previous?.agentId && previous.agentId !== agent) return false;
			const prior = await read("STATE:" + agent);
			// Never reset a pending or ambiguous attempt, even when STOP is true.
			if (
				(await read("STOP")) !== true ||
				(prior && ![PHASES.PAUSED, PHASES.COOLDOWN].includes(prior.phase))
			)
				return false;
			await write("STOP", true);
			await write("CONFIG", {
				lab: true,
				target: url,
				agentId: agent,
				text: content,
			});
			await write("STATE:" + agent, newPausedState(agent));
			currentRole = agent;
			clearTimer();
			return true;
		});
		if (configured !== true) {
			window.alert("LAB configuration refused: active or uncertain attempt.");
			return;
		}
		window.alert("Configured LAB-only; no sending until manual Arm LAB.");
	});
	menu("Arm LAB after preflight (max 2 sends)", async () => {
		const config = await read("CONFIG");
		if (
			!config?.lab ||
			config?.target !== url ||
			!AGENTS.includes(config?.agentId) ||
			!config.text.startsWith(LAB_PREFIX)
		)
			return;
		if (!locks?.request) return;
		if (
			!window.confirm(
				"LAB ONLY: upstream ChatQueue queue PAUSED, correct isolated test chat, " +
					"no draft/busy model, signed Violentmonkey active? Never use agent chats.",
			)
		)
			return;
		const armed = await withLock("arm", async () => {
			// Re-read the binding under the same per-conversation lock as tick.
			if (
				(await read("STOP")) !== true ||
				!(await configMatches(config.agentId, config.text))
			)
				return false;
			const state = await read("STATE:" + config.agentId);
			if (!state || state.agentId !== config.agentId) return false;
			const gates = {
				manualGo: true,
				threeLegacyGatesOff: true,
				exclusiveScheduler: true,
				targetVerified: targetReady(),
				accountVerified: accountReady(),
				bootstrapSameChatVerified: true, // LAB only, not broker certification.
				noOtherGeneration: !stopButton(),
				recurrenceE2EPassed: true, // LAB test path ONLY, not an operational claim.
			};
			// Persist READY before allowing a send. A denied lock changes nothing.
			const next = authorizeFirstTick(state, gates, Date.now());
			try {
				await write("STATE:" + config.agentId, next);
				await write("STOP", false);
				currentRole = config.agentId;
				return true;
			} catch (error) {
				await write("STOP", true);
				throw error;
			}
		});
		if (armed === true) await schedule();
		else window.alert("LAB arm refused: active lock or stale configuration.");
	});
	menu("Pause LAB immediately", async () => {
		await write("STOP", true);
		clearTimer();
		window.alert(
			"LAB paused; in-flight attempt requires REVIEW after uncertainty.",
		);
	});
	// Import is inert. Browser startup/reload never arms a job.
	Promise.resolve()
		.then(async () => {
			await write("STOP", true);
			const config = await read("CONFIG");
			if (
				config?.lab &&
				config.target === url &&
				AGENTS.includes(config.agentId)
			) {
				currentRole = config.agentId;
				await controller.boot(currentRole);
			}
		})
		.catch(() => {});
})();
