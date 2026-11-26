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
		typeof GM === "undefined" ||
		typeof GM.getValue !== "function" ||
		typeof GM.setValue !== "function" ||
		typeof GM_registerMenuCommand !== "function" ||
		typeof GM_xmlhttpRequest !== "function"
	)
		return;
	const path = location.pathname;
	const getConfig = async () => await GM.getValue(OPERATIONAL_CONFIG_KEY, null);
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
	// Expanded/line-clamped plain user messages may include an ellipsis and
	// a localized expansion control in the bubble. Only project an exact,
	// unique leaf text body when every other nonblank text node is accounted
	// for by that observed UI structure. Never strip arbitrary suffix text.
	const userMessageText = (t) => {
		const bubble = user(t);
		if (!bubble) return null;
		const bodies = [...bubble.querySelectorAll("div.whitespace-pre-wrap")];
		if (bodies.length === 0) return textOf(bubble);
		const body = bodies[0];
		if (bodies.length !== 1 || !visible(body) || body.children.length !== 0)
			return null;
		const text = textOf(body);
		if (!text || norm(body.textContent) !== text) return null;
		const controls = [...bubble.querySelectorAll("button,[role=button]")];
		if (
			controls.length > 1 ||
			controls.some(
				(e) =>
					e.tagName !== "BUTTON" ||
					!visible(e) ||
					e.contains(body) ||
					!/^(?:Mostrar (?:mais|menos)|Show (?:more|less))$/i.test(textOf(e)),
			) ||
			bubble.querySelector(
				"img,video,audio,iframe,object,embed,canvas,input,textarea,[contenteditable=true]",
			)
		)
			return null;
		const walker = document.createTreeWalker(bubble, NodeFilter.SHOW_TEXT);
		let ellipses = 0;
		while (walker.nextNode()) {
			const node = walker.currentNode;
			const part = norm(node.nodeValue);
			if (!part || body.contains(node)) continue;
			if (controls.length === 1 && controls[0].contains(node)) continue;
			const parent = node.parentElement;
			if (
				controls.length === 1 &&
				parent?.tagName === "SPAN" &&
				parent.getAttribute("aria-hidden") === "true" &&
				/^(?:\.\.\.|…)$/.test(part) &&
				++ellipses === 1
			)
				continue;
			return null;
		}
		return text;
	};
	const headingsIn = (t) => [
		...(t?.querySelectorAll('[data-conversation-role="assistant"]') ?? []),
	];
	const assistant = (t) => {
		const headings = headingsIn(t),
			bubble = user(t);
		if (
			!headings.length ||
			!bubble ||
			headings.some(
				(h) =>
					bubble.contains(h) ||
					!(
						bubble.compareDocumentPosition(h) & Node.DOCUMENT_POSITION_FOLLOWING
					),
			)
		)
			return null;
		// Several hidden reasoning/tool headings can precede the one visible
		// final heading, but all must belong to the same unique turn group.
		const a = headings[0].closest("div.group.flex.flex-col");
		if (
			!a ||
			!t.contains(a) ||
			!visible(a) ||
			headings.some((h) => h.closest("div.group.flex.flex-col") !== a)
		)
			return null;
		const active = headings.filter(visible);
		return active.length === 1 && active[0] === headings.at(-1) ? a : null;
	};
	const assistantReplyNodes = (a) => {
		const h = headingsIn(a).at(-1);
		if (!h || !visible(h) || !h.parentElement || !a.contains(h.parentElement))
			return [];
		const siblings = [...h.parentElement.children].slice(
			[...h.parentElement.children].indexOf(h) + 1,
		);
		const roleOrUser = "[data-conversation-role],[data-user-message-bubble]";
		if (
			siblings.some((e) => e.matches(roleOrUser) || e.querySelector(roleOrUser))
		)
			return [];
		return siblings.filter(
			(e) => visible(e) && !e.matches("button,[role=button]"),
		);
	};
	const assistantReply = (a) =>
		assistantReplyNodes(a)
			.map((e) => e.innerText ?? "")
			.join("\n")
			.trim();
	const terminal = (a) => {
		const copies = [
			...(a?.querySelectorAll(
				'button[data-testid="copy-turn-action-button"],button[aria-label="Copy"],button[aria-label="Copiar"]',
			) ?? []),
		].filter(visible);
		const h = headingsIn(a).at(-1),
			nodes = assistantReplyNodes(a);
		const copy = copies[0];
		return (
			copies.length === 1 &&
			!copy.disabled &&
			!!h &&
			!!(h.compareDocumentPosition(copy) & Node.DOCUMENT_POSITION_FOLLOWING) &&
			!copy.closest("[data-user-message-bubble]") &&
			nodes.length > 0 &&
			!nodes.some((e) => e.contains(copy)) &&
			!!assistantReply(a)
		);
	};
	const receiptMatches = (message) =>
		turns().filter((t) => userMessageText(t) === norm(message));
	const receipt = (message) => {
		const matches = receiptMatches(message);
		return matches.length === 1 ? matches[0] : null;
	};
	const bootstrap = (b) =>
		turns().some((t) => {
			const a = assistant(t);
			// Only the structural visible assistant reply can contain a
			// bootstrap marker. Neither user text nor hidden reasoning counts.
			return (
				terminal(a) &&
				assistantReply(a)
					.split("\n")
					.some(
						(line) =>
							line.trim() === b.agent.toUpperCase() + "_BOOTSTRAP_READY",
					)
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
	// A virtualized turn can temporarily leave the DOM. Absence is not a
	// terminal witness; an explicit duplicate or superseding turn is fatal.
	const attemptTurn = (content) => {
		const ts = turns();
		const matches = ts.filter((t) => userMessageText(t) === norm(content));
		if (
			matches.length > 1 ||
			(matches.length === 1 && matches[0] !== ts.at(-1))
		)
			throw new Error("Attempt turn no longer unique/latest");
		return matches[0] ?? null;
	};
	const terminalSnapshot = (b, content) => {
		const t = attemptTurn(content);
		const a = assistant(t);
		return t && terminal(a) && ready(b, "")
			? norm(assistantReply(a)) || null
			: null;
	};
	const unsentSnapshot = (b, content, prior) => {
		if (receiptMatches(content).length !== 0) return null;
		const draft = ready(b, content) ? "exact" : ready(b, "") ? "empty" : null;
		const t = attemptTurn(prior),
			a = assistant(t);
		const reply = terminal(a) ? norm(assistantReply(a)) : "";
		return draft && reply ? { draft, reply } : null;
	};
	const dom = {
		ready,
		unsentSnapshot,
		async clearUnsentDraft(b, content, prior) {
			const before = unsentSnapshot(b, content, prior);
			if (before?.draft !== "exact")
				throw new Error("Exact unsent draft not proven");
			const c = composer();
			c.box.focus();
			if (unsentSnapshot(b, content, prior)?.draft !== "exact")
				throw new Error("Draft changed before clear");
			if (c.box.tagName === "TEXTAREA") {
				const set = Object.getOwnPropertyDescriptor(
					HTMLTextAreaElement.prototype,
					"value",
				)?.set;
				if (!set) throw new Error("Textarea setter unavailable");
				set.call(c.box, "");
				c.box.dispatchEvent(new Event("input", { bubbles: true }));
			} else {
				const range = document.createRange(),
					sel = window.getSelection();
				if (!sel) throw new Error("Selection unavailable");
				range.selectNodeContents(c.box);
				sel.removeAllRanges();
				sel.addRange(range);
				if (!document.execCommand("delete", false))
					throw new Error("Exact draft clear unverified");
			}
			await sleep(400);
			const after = unsentSnapshot(b, content, prior);
			if (after?.draft !== "empty" || after.reply !== before.reply)
				throw new Error("Draft clear readback unverified");
		},
		terminalSnapshot,
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
		async waitTerminal(b, content, paused, maxWait = Infinity) {
			const deadline = Date.now() + 2 * 60 * 60 * 1000;
			const until = Math.min(deadline, Date.now() + maxWait);
			let previous = "",
				stable = null,
				missingIdleSince = null;
			while (Date.now() < until) {
				if (!route() || (await paused())) throw new Error("Reply interrupted");
				const t = attemptTurn(content);
				const now = Date.now();
				if (!t && !busy()) {
					missingIdleSince ??= now;
					if (now - missingIdleSince >= 30000)
						throw new Error("Exact attempt receipt absent after idle grace");
				} else missingIdleSince = null;
				const text = t ? terminalSnapshot(b, content) : null;
				if (text) {
					if (text !== previous || stable === null) {
						previous = text;
						stable = Date.now();
					} else if (Date.now() - stable >= 6000)
						return {
							observedFinalAssistantResponse: true,
							noActiveGeneration: true,
							receiptText: text,
						};
				} else {
					stable = null;
					previous = "";
				}
				await sleep(500);
			}
			throw new Error("Terminal reply unverified");
		},
	};
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
			const locks = makeBridgeLockClient({
				agent: binding.agent,
				bridge: config.bridge,
				request,
				randomNonce: () => crypto.randomUUID().replace(/-/g, ""),
				clock: Date.now,
				sleep,
				digest: sha256,
				readGM: async (key) => await GM.getValue(prefix + key, null),
				writeGM: async (key, value) => await GM.setValue(prefix + key, value),
			});
			const lock = locks.lock;
			const runtime = makeOperationalRuntime({
				agent: binding.agent,
				path,
				getConfig,
				read: locks.read,
				write: locks.write,
				lock,
				clock: Date.now,
				randomId: () => crypto.randomUUID(),
				setTimer: setTimeout,
				cancelTimer: clearTimeout,
				readEvidence,
				dom,
			});
			const boot = runtime.boot().catch(() => false);
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
								? "The executor must independently verify OWN STARTED and terminal Git proof of the supervised tick."
								: mode === "bounded"
									? "Only ONE bounded send, preserving the original supervised proof; separate Git verification and GO before recurrence."
									: "Only ONE supervised send."),
					)
				)
					return;
				const ok = await runtime.arm(mode, {
					manualGo: true,
					accountModelVerified: true,
					originalQueueRetired: true,
					qaVerified: true,
				});
				if (!ok) window.alert("Arm refused; no automatic retry.");
			};
			const recoveryAttestation = () => ({
				manualRecovery: true,
				accountModelVerified: true,
				oldDocumentsRetired: true,
				qaVerified: true,
			});
			menu("Reconcile supervised REVIEW (no send)", async () => {
				await boot;
				if (
					!window.confirm(
						"Reconcile only the exact supervised attempt in this chat? Confirm stopped roles, correct account/model and independent QA. No send, arm or provider-effect claim.",
					)
				)
					return;
				const ok = await runtime.reconcileSupervisedReview(
					recoveryAttestation(),
				);
				window.alert(
					ok
						? "Exact attempt reconciled; still paused. Separate GO required."
						: "REVIEW retained; exact receipt or proof not verified. No send.",
				);
			});
			menu("Migrate pristine paused binding (no send)", async () => {
				await boot;
				if (
					!window.confirm(
						"Migrate this role's pristine paused journal to the current chat? Confirm old documents closed, exact new account/model/bootstrap and independent QA. No send or arm.",
					)
				)
					return;
				const ok = await runtime.migratePristineBinding(recoveryAttestation());
				window.alert(
					ok
						? "Pristine binding migrated; still paused. Separate GO required."
						: "Binding unchanged; migration conditions not proven. No send.",
				);
			});
			menu("Reconcile pre-click REVIEW (no send)", async () => {
				await boot;
				if (
					!window.confirm(
						"Cancel only a proven pre-click REVIEW? Confirm exact predecessor source/reason, stopped roles, correct account/model and independent QA. Only the exact unsent draft may be cleared; no send, arm or replay.",
					)
				)
					return;
				const raw = window.prompt(
					"Prior completed OWN invocation evidence: JSON with attemptId and baselineMain only. No terminalCommit; the pair must be the latest own Git pulse.",
				);
				if (!raw) return;
				let query;
				try {
					query = JSON.parse(raw);
				} catch {
					return;
				}
				const ok = await runtime.reconcilePreclickReview(
					{ ...recoveryAttestation(), precisePreclickSourceVerified: true },
					query,
				);
				window.alert(
					ok
						? "Exact pre-click attempt cancelled; still paused. Separate GO required."
						: "REVIEW retained; pre-click evidence not verified. No send.",
				);
			});
			if (binding.agent === "platformmanager") {
				menu("Migrate historical PM binding (no send)", async () => {
					await boot;
					if (
						!window.confirm(
							"Migrate only the stopped PlatformManager history? Independently verify retirement of the old documents/leases, exact new account/model/bootstrap and source QA. Preserve all proofs; no send or arm.",
						)
					)
						return;
					const raw = window.prompt(
						"Latest completed OWN invocation evidence: JSON with attemptId, baselineMain and terminalCommit. Verify exact native journal and Git evidence first.",
					);
					if (!raw) return;
					let query;
					try {
						query = JSON.parse(raw);
					} catch {
						return;
					}
					const ok = await runtime.migrateHistoricalBinding(
						recoveryAttestation(),
						query,
					);
					window.alert(
						ok
							? "Historical binding migrated; still paused. Reload while paused and use separate GO."
							: "Historical binding unchanged or pending; no send. Check evidence.",
					);
				});
				menu("Arm bounded history (one tick)", () => arm("bounded"));
			}
			menu("Arm supervised (one tick)", () => arm("supervised"));
			menu("Arm recurrence (requires verified supervised Git pair)", () =>
				arm("recurrence"),
			);
		})
		.catch(() => {});
})();
