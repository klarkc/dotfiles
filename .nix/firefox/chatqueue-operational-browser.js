// Separate production adapter; no automatic arming, no embedded configuration.
// Only signed Violentmonkey's privileged isolated storage and menu are used.
(() => {
	"use strict";
	const origin = "https://chatgpt.com";
	if (
		location.origin !== origin ||
		!OPERATIONAL_PATH.test(location.pathname) ||
		location.search ||
		location.hash ||
		typeof GM_getValue !== "function" ||
		typeof GM_setValue !== "function" ||
		typeof GM_registerMenuCommand !== "function" ||
		typeof GM_xmlhttpRequest !== "function" ||
		!navigator.locks?.request
	)
		return;
	const path = location.pathname;
	const getConfig = async () => await GM_getValue(OPERATIONAL_CONFIG_KEY, null);
	const visible = (e) =>
		!!e &&
		!e.hidden &&
		e.getAttribute?.("aria-hidden") !== "true" &&
		e.getClientRects().length > 0;
	const norm = (s) =>
		String(s ?? "")
			.replace(/\s+/g, " ")
			.trim();
	const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
	const route = () =>
		location.origin === origin &&
		location.pathname === path &&
		!location.search &&
		!location.hash;
	const busy = () =>
		[
			...document.querySelectorAll(
				'button[data-testid="stop-button"],button[aria-label*="Stop"],button[aria-label*="Parar"],button[aria-label*="Interromper"]',
			),
		].some(visible);
	const composer = () => {
		const forms = [
			...document.querySelectorAll("form[data-chatgpt-composer]"),
		].filter(visible);
		if (forms.length !== 1) return null;
		const boxes = [
			...forms[0].querySelectorAll(
				'[contenteditable="true"][role="textbox"],textarea',
			),
		].filter(visible);
		return boxes.length === 1 ? { form: forms[0], box: boxes[0] } : null;
	};
	const textOf = (el) =>
		norm(
			el?.tagName === "TEXTAREA"
				? el.value
				: (el?.innerText ?? el?.textContent),
		);
	const turns = () =>
		[...document.querySelectorAll("[data-virtualized-turn]")].filter(visible);
	const user = (t) => {
		const rows = [
			...(t?.querySelectorAll("[data-user-message-bubble]") ?? []),
		].filter(
			(e) =>
				visible(e) && !e.parentElement?.closest("[data-user-message-bubble]"),
		);
		return rows.length === 1 ? rows[0] : null;
	};
	const assistant = (t) => {
		const headings = [
			...(t?.querySelectorAll('[data-conversation-role="assistant"]') ?? []),
		];
		if (headings.length !== 1) return null;
		const a = headings[0].closest("div.group.flex.flex-col");
		return a && t.contains(a) && visible(a) ? a : null;
	};
	const terminal = (a) => {
		const copy = a?.querySelector(
			'button[data-testid="copy-turn-action-button"],button[aria-label="Copy"],button[aria-label="Copiar"]',
		);
		return visible(copy) && !copy.disabled;
	};
	const receiptMatches = (message) =>
		turns().filter((t) => textOf(user(t)) === norm(message));
	const receipt = (message) => {
		const matches = receiptMatches(message);
		return matches.length === 1 ? matches[0] : null;
	};
	const bootstrap = (b) =>
		turns().some((t) => {
			const a = assistant(t);
			if (!terminal(a)) return false;
			// The modern group includes both sides; require the exact assistant
			// portion, never a mention of a marker in the user's bootstrap prompt.
			const heading = a.querySelector('[data-conversation-role="assistant"]');
			if (!heading) return false;
			const text = (a.innerText ?? "").split(
				/ChatGPT disse:\s*|ChatGPT said:\s*/,
			);
			const reply =
				text.length > 1
					? text.at(-1)
					: [...a.children]
							.filter(
								(e) =>
									!e.contains(user(t)) &&
									!e.querySelector("[data-user-message-bubble]"),
							)
							.map((e) => e.innerText ?? "")
							.join("\n");
			return reply
				.split("\n")
				.some(
					(line) => line.trim() === b.agent.toUpperCase() + "_BOOTSTRAP_READY",
				);
		});
	const ready = (b, draft, requireBootstrap = false) => {
		const c = composer();
		const ts = turns(),
			last = ts.at(-1);
		const models = [...(c?.form.querySelectorAll("button") ?? [])].filter(
			(e) =>
				visible(e) &&
				/^(?:6\s+)?Extra (?:alto|High)$/i.test(norm(e.textContent)),
		);
		return (
			route() &&
			b.path === path &&
			!busy() &&
			!document.querySelector(
				'[data-testid="login-button"],a[href*="/auth/login"]',
			) &&
			!!c &&
			models.length === 1 &&
			norm(models[0].textContent) === norm(b.modelLabel) &&
			textOf(c.box) === norm(draft) &&
			!!user(last) &&
			terminal(assistant(last)) &&
			(!requireBootstrap || bootstrap(b))
		);
	};
	const dom = {
		ready,
		receiptCount: (message) => receiptMatches(message).length,
		async compose(content) {
			const c = composer();
			if (!route() || busy() || !c || textOf(c.box))
				throw new Error("Composer not empty");
			c.box.focus();
			if (c.box.tagName === "TEXTAREA") {
				const set = Object.getOwnPropertyDescriptor(
					HTMLTextAreaElement.prototype,
					"value",
				)?.set;
				if (!set) throw new Error("Textarea setter unavailable");
				set.call(c.box, content);
				c.box.dispatchEvent(new Event("input", { bubbles: true }));
			} else {
				if (!document.execCommand("insertText", false, content)) {
					if (textOf(c.box)) throw new Error("Refusing to replace draft");
					c.box.textContent = content;
					c.box.dispatchEvent(
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
			if (textOf(c.box) !== norm(content)) throw new Error("Draft mismatch");
		},
		click(b, content) {
			if (!ready(b, content) || receiptMatches(content).length !== 0)
				throw new Error("Final DOM mismatch");
			const c = composer();
			const btn = [
				...c.form.querySelectorAll(
					'button[data-testid="send-button"],button[type="submit"]',
				),
			].filter((e) => visible(e) && !e.disabled);
			if (
				btn.length !== 1 ||
				/stop|cancel|pause|parar/i.test(btn[0].getAttribute("aria-label") ?? "")
			)
				throw new Error("Send ambiguous");
			btn[0].click();
		},
		async waitSent(content, paused) {
			for (let i = 0; i < 50; i++) {
				await sleep(500);
				if (!route() || (await paused()))
					throw new Error("Delivery interrupted");
				const t = receipt(content);
				if (t && turns().at(-1) === t) return { sent: true };
			}
			throw new Error("No exact user receipt");
		},
		async waitTerminal(b, content, paused) {
			const deadline = Date.now() + 2 * 60 * 60 * 1000;
			let previous = "",
				stable = 0;
			while (Date.now() < deadline) {
				if (!route() || (await paused())) throw new Error("Reply interrupted");
				const t = receipt(content);
				if (!t || turns().at(-1) !== t)
					throw new Error("Attempt turn no longer unique/latest");
				const a = assistant(t),
					text = textOf(a);
				if (!busy() && text && terminal(a) && ready(b, "")) {
					if (text !== previous) {
						previous = text;
						stable = Date.now();
					} else if (stable && Date.now() - stable >= 6000)
						return {
							observedFinalAssistantResponse: true,
							noActiveGeneration: true,
						};
				} else {
					stable = 0;
					previous = "";
				}
				await sleep(500);
			}
			throw new Error("Terminal reply unverified");
		},
	};
	const lock = async (name, ifAvailable, fn) =>
		await navigator.locks.request(
			name,
			{ mode: "exclusive", ...(ifAvailable ? { ifAvailable: true } : {}) },
			(l) => (l ? fn() : { status: "SKIP_IN_PROGRESS" }),
		);
	const request = (opts) =>
		new Promise((resolve, reject) => {
			GM_xmlhttpRequest({
				...opts,
				onload: resolve,
				onerror: () => reject(new Error("Policy read failed")),
				ontimeout: () => reject(new Error("Policy read timeout")),
				onabort: () => reject(new Error("Policy read aborted")),
			});
		});
	const readEvidence = makeOperationalPolicyClient({
		request,
		clock: Date.now,
		randomNonce: () => crypto.randomUUID().replace(/-/g, ""),
	});
	Promise.resolve()
		.then(async () => {
			const config = await getConfig();
			if (!validateOperationalConfig(config)) return;
			const binding = config.bindings.find((b) => b.path === path);
			if (!binding) return;
			const prefix = "CQOP_V1:" + binding.agent + ":";
			const runtime = makeOperationalRuntime({
				agent: binding.agent,
				path,
				getConfig,
				read: async (key) => await GM_getValue(prefix + key, null),
				write: async (key, value) => await GM_setValue(prefix + key, value),
				lock,
				clock: Date.now,
				randomId: () => crypto.randomUUID(),
				setTimer: setTimeout,
				cancelTimer: clearTimeout,
				readEvidence,
				dom,
			});
			const boot = runtime.boot();
			const menu = (title, fn) =>
				GM_registerMenuCommand("ChatQueue Broker - " + title, () =>
					Promise.resolve()
						.then(fn)
						.catch(() =>
							window.alert(
								"Operation denied. Check status. No automatic resend.",
							),
						),
				);
			menu("Status (no send)", async () => {
				await boot;
				const s = await runtime.status();
				window.alert(JSON.stringify(s));
			});
			menu("Check policy (no send)", async () => {
				await boot;
				const cfg = await getConfig();
				const p = await lock("CQOP_V1:POLICY_READ", false, () =>
					readEvidence(cfg, binding.agent),
				);
				window.alert(
					p
						? "Git policy evidence only; no permission to send."
						: "NO_GO or unverified policy; no send.",
				);
			});
			menu("Pause this role", async () => {
				await runtime.pause();
				window.alert("Pause acknowledged; pending attempts are not replayed.");
			});
			const arm = async (mode) => {
				if (!(await boot)) return;
				const cfg = await getConfig();
				if (cfg?.reviewed !== true) {
					window.alert("Operational QA/config not approved. NO_GO.");
					return;
				}
				if (
					!window.confirm(
						"Explicit " +
							mode +
							" GO for " +
							binding.agent +
							" only? Confirm signed host and exact account/workspace/model, completed same-chat bootstrap, original queue retired, independent operational QA, all gates checked. " +
							(mode === "recurrence"
								? "Confirm OWN STARTED and terminal Git proof of the supervised tick."
								: "Only ONE supervised send."),
					)
				)
					return;
				const ok = await runtime.arm(mode, {
					manualGo: true,
					accountModelVerified: true,
					originalQueueRetired: true,
					qaVerified: true,
					supervisedGitProofVerified: mode === "recurrence",
				});
				if (!ok) window.alert("Arm refused; no automatic retry.");
			};
			menu("Arm supervised (one tick)", () => arm("supervised"));
			menu("Arm recurrence (after verified supervised Git proof)", () =>
				arm("recurrence"),
			);
		})
		.catch(() => {});
})();
