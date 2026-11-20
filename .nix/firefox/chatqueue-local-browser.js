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
	const messageNodes = (role) => [
		...document.querySelectorAll('[data-message-author-role="' + role + '"]'),
	];
	const stopButton = () =>
		[
			...document.querySelectorAll(
				'button[data-testid="stop-button"],button[aria-label*="Stop"],button[aria-label*="Parar"],button[aria-label*="Interromper"]',
			),
		].some(visible);
	const composer = () => document.querySelector("form[data-chatgpt-composer]");
	const composeBox = () =>
		composer()?.querySelector(
			'[contenteditable="true"][role="textbox"],[contenteditable="true"],textarea',
		);
	const sendButton = () => {
		const options = [
			...(composer()?.querySelectorAll('button[type="submit"]') || []),
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
	const userMessageSeen = (text) => {
		const sample = normalized(text).slice(0, 40);
		return messageNodes("user").some((node) =>
			normalized(node.innerText ?? node.textContent).includes(sample),
		);
	};
	const accountReady = () =>
		!document.querySelector(
			'[data-testid="login-button"],a[href*="/auth/login"]',
		);
	const targetReady = () =>
		location.origin + location.pathname === url &&
		!stopButton() &&
		accountReady() &&
		!!composer() &&
		composer().querySelectorAll('button[type="submit"]').length === 1 &&
		!!composeBox() &&
		!normalized(composeBox()?.innerText ?? composeBox()?.value);

	async function withLock(name, callback) {
		if (!locks?.request) throw new Error("Firefox LockManager unavailable");
		return await locks.request(
			url + ":" + name,
			{ ifAvailable: true, mode: "exclusive" },
			async (lock) => {
				if (!lock) return { status: "SKIP_IN_PROGRESS" };
				return await callback();
			},
		);
	}
	const transport = {
		preflight: async () => ({
			identityVerified: targetReady() && currentRole !== null,
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
		deliver: async (_agent, content) => {
			if (stopButton() || location.origin + location.pathname !== url)
				throw new Error("Conversation changed or model busy");
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
						normalized(item.innerText ?? item.textContent).includes(
							normalized(content).slice(0, 40),
						),
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
				const next = messageNodes("assistant").filter(
					(el) =>
						!beforeAssistant.has(el) &&
						!!normalized(el.innerText ?? el.textContent),
				);
				const latest = next.at(-1);
				const text = normalized(latest?.innerText ?? latest?.textContent);
				if (latest && !stopButton()) {
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
			throw new Error("No terminal reply proof after 2-hour limit");
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
				(state?.cycle ?? 0),
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
		const previous = await read("CONFIG");
		if (previous?.agentId && previous.agentId !== agent) return;
		const prior = await read("STATE:" + agent);
		if (prior?.phase === PHASES.REVIEW) return;
		await write("STOP", true);
		await write("CONFIG", {
			lab: true,
			target: url,
			agentId: agent,
			text: content,
		});
		await write("STATE:" + agent, newPausedState(agent));
		currentRole = agent;
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
		currentRole = config.agentId;
		await write("STOP", false);
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
		await controller.arm(currentRole, gates);
		await schedule();
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
