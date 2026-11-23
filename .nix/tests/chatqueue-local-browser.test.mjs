import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { Script } from "node:vm";
import { bundle } from "../firefox/build-chatqueue-local.mjs";

const root = dirname(fileURLToPath(import.meta.url));
const base = join(root, "../firefox");

const source = readFileSync(join(base, "chatqueue-local-browser.js"), "utf8");
const script = new Script(bundle, { filename: "chatqueue-local.user.js" });
const flush = async () => {
	for (let n = 0; n < 6; n++)
		await new Promise((resolve) => setImmediate(resolve));
};
function sandbox(pathname = "/c/example-chat-123") {
	const menus = [];
	const values = new Map();
	const timers = [];
	const alerts = [];
	let confirms = true;
	const responses = [];
	const context = {
		location: { origin: "https://chatgpt.com", pathname },
		navigator: { locks: undefined },
		window: {
			confirm: () => confirms,
			prompt: () => responses.shift() ?? null,
			alert: (text) => alerts.push(String(text)),
		},
		document: { querySelector: () => null, querySelectorAll: () => [] },
		GM_getValue: (key, fallback) => values.get(key) ?? fallback,
		GM_setValue: (key, value) => {
			values.set(key, value);
		},
		GM_registerMenuCommand: (name, callback) => menus.push({ name, callback }),
		setTimeout: (fn, ms) => {
			timers.push({ fn, ms });
			return timers.length;
		},
		clearTimeout: () => {},
	};
	const run = () => script.runInNewContext(context);
	const runMenu = async (label) => {
		const menu = menus.find((entry) => entry.name.includes(label));
		assert.ok(menu, "menu present: " + label);
		menu.callback();
		await flush();
	};
	return {
		menus,
		values,
		timers,
		alerts,
		responses,
		context,
		run,
		runMenu,
		reject: () => {
			confirms = false;
		},
	};
}

test("generated LAB userscript has no import, export, remote includes or unapproved grants", () => {
	assert.ok(bundle.startsWith("// ==UserScript=="));
	assert.match(bundle, /@match\s+https:\/\/chatgpt\.com\/c\/\*/);
	assert.match(bundle, /@version\s+0\.1\.0-lab/);
	assert.match(bundle, /@grant\s+GM_getValue/);
	assert.doesNotMatch(bundle, /@grant\s+GM_xmlhttpRequest/);
	assert.doesNotMatch(bundle, /@require|@downloadURL|@updateURL/);
	assert.doesNotMatch(bundle, /\b(?:import|export)\s+\{/);
	assert.doesNotMatch(source, /\bfetch\s*\(|browser\.storage|chrome\.storage/);
	assert.doesNotMatch(source, /\bGM_xmlhttpRequest\b/);
});
test("script import registers four menus, defaults STOP and has zero scheduled jobs", async () => {
	const s = sandbox();
	s.run();
	await flush();
	assert.equal(s.menus.length, 4);
	assert.equal(s.timers.length, 0);
	assert.equal(
		[...s.values].filter(([key, val]) => key.endsWith(":STOP") && val === true)
			.length,
		1,
	);
	assert.equal(s.alerts.length, 0);
});
test("non-chat pages get no menus and no automatic send", async () => {
	const s = sandbox("/home");
	s.run();
	await flush();
	assert.equal(s.menus.length, 0);
	assert.equal(s.values.size, 0);
	assert.equal(s.timers.length, 0);
});
test("new unpersisted ChatGPT home route is not eligible for LAB", async () => {
	const s = sandbox("/");
	s.run();
	await flush();
	assert.equal(s.menus.length, 0);
	assert.equal(s.values.size, 0);
	assert.equal(s.timers.length, 0);
});

test("configuring benign LAB text never sends or starts a timer", async () => {
	const s = sandbox();
	s.run();
	await flush();
	s.responses.push("coordinator", "CQLOCAL LAB: Respond only LAB_ACK.");
	await s.runMenu("Configure LAB");
	assert.equal(s.timers.length, 0);
	const cfg = [...s.values].find(([key]) => key.endsWith(":CONFIG"))?.[1];
	assert.equal(cfg.lab, true);
	assert.equal(cfg.agentId, "coordinator");
	assert.equal(cfg.text, "CQLOCAL LAB: Respond only LAB_ACK.");
	assert.equal([...s.values].find(([key]) => key.endsWith(":STOP"))[1], true);
});
test("normal agent text is rejected as LAB config", async () => {
	const s = sandbox();
	s.run();
	await flush();
	s.responses.push("coordinator", "ONE tick ONLY as permanent Coordinator");
	await s.runMenu("Configure LAB");
	assert.equal(
		[...s.values].some(([key]) => key.endsWith(":CONFIG")),
		false,
	);
});
test("without browser LockManager, arming LAB never starts or sends", async () => {
	const s = sandbox();
	s.run();
	await flush();
	s.responses.push("coordinator", "CQLOCAL LAB: Short response.");
	await s.runMenu("Configure LAB");
	await s.runMenu("Arm LAB");
	assert.equal(s.timers.length, 0);
	assert.equal([...s.values].find(([key]) => key.endsWith(":STOP"))[1], true);
});
test("pausing is local and does not invoke ChatQueue XPI or schedule anything", async () => {
	const s = sandbox();
	s.run();
	await flush();
	await s.runMenu("Pause LAB");
	assert.equal(s.timers.length, 0);
	assert.equal(
		s.alerts.some((line) => line.includes("paused")),
		true,
	);
});

test("synthetic browser DOM: two sends only, each after a terminal + cooldown", async () => {
	const store = new Map();
	const held = new Set();
	const menus = [];
	const timers = [];
	const clicks = [];
	const seededReceipt = {
		disabled: false,
		hidden: false,
		getAttribute: () => null,
		getClientRects: () => [1],
	};
	const users = [{ innerText: "CQLOCAL LAB: Manual seed." }],
		assistants = [
			{
				innerText: "LAB_ACK",
				querySelector: () => seededReceipt,
			},
		];
	let clock = 0,
		nextTimer = 1,
		nextUUID = 1;
	const docBox = {
		tagName: "DIV",
		innerText: "",
		focus() {},
	};
	const send = {
		disabled: false,
		hidden: false,
		getAttribute: () => "",
		getClientRects: () => [1],
		click() {
			clicks.push(clock);
			users.push({ innerText: docBox.innerText });
			assistants.push({
				innerText: "LAB_ACK_" + clicks.length,
				querySelector: () => ({
					disabled: false,
					hidden: false,
					getAttribute: () => null,
					getClientRects: () => [1],
				}),
			});
			docBox.innerText = "";
		},
	};
	const form = {
		getClientRects: () => [1],
		getAttribute: () => null,
		querySelector: () => docBox,
		querySelectorAll: () => (docBox.innerText ? [send] : []),
	};
	const DocDate = class extends Date {
		static now() {
			return clock;
		}
	};
	const responses = ["coordinator", "CQLOCAL LAB: Respond only LAB_ACK."];
	const context = {
		location: { origin: "https://chatgpt.com", pathname: "/c/lab-synthetic" },
		window: {
			confirm: () => true,
			prompt: () => responses.shift(),
			alert() {},
		},
		navigator: {
			locks: {
				async request(key, _options, callback) {
					if (held.has(key)) return callback(null);
					held.add(key);
					try {
						return await callback({ name: key });
					} finally {
						held.delete(key);
					}
				},
			},
		},
		document: {
			querySelector(selector) {
				return selector === "form[data-chatgpt-composer]" ? form : null;
			},
			querySelectorAll(selector) {
				if (selector === "form[data-chatgpt-composer]") return [form];
				if (selector.includes('author-role="user"')) return users;
				if (selector.includes('author-role="assistant"')) return assistants;
				return [];
			},
			execCommand(_name, _unused, value) {
				docBox.innerText = value;
				return true;
			},
		},
		Date: DocDate,
		crypto: {
			randomUUID() {
				return "simulated-000" + nextUUID++;
			},
		},
		GM_getValue: (key, fallback) => store.get(key) ?? fallback,
		GM_setValue: (key, value) => {
			store.set(key, value);
		},
		GM_registerMenuCommand: (name, callback) => menus.push({ name, callback }),
		setTimeout: (fn, ms) => {
			const id = nextTimer++;
			timers.push({ id, when: clock + ms, fn });
			return id;
		},
		clearTimeout: (id) => {
			const idx = timers.findIndex((x) => x.id === id);
			if (idx >= 0) timers.splice(idx, 1);
		},
	};
	script.runInNewContext(context);
	await flush();
	const runMenu = async (label) => {
		const item = menus.find((entry) => entry.name.includes(label));
		assert.ok(item);
		item.callback();
		await flush();
	};
	await runMenu("Configure LAB");
	await runMenu("Arm LAB");
	let steps = 0;
	while (timers.length && steps < 140) {
		timers.sort((a, b) => a.when - b.when);
		const next = timers.shift();
		clock = next.when;
		next.fn();
		await flush();
		steps++;
	}
	assert.ok(steps < 140, "simulated timer queue must terminate");
	assert.equal(clicks.length, 2);
	assert.ok(
		clicks[1] - clicks[0] >= 360_000,
		"next send must happen six minutes AFTER terminal evidence",
	);
	const state = [...store].find(([key]) =>
		key.endsWith(":STATE:coordinator"),
	)?.[1];
	assert.equal(state.cycle, 2);
	assert.equal(state.phase, "COOLDOWN");
	assert.equal([...store].find(([key]) => key.endsWith(":STOP"))?.[1], true);
});

test("LAB Status reports only DOM structural booleans, no message content", async () => {
	const s = sandbox();
	s.run();
	await flush();
	await s.runMenu("Status (no send)");
	assert.equal(s.alerts.length, 1);
	assert.match(s.alerts[0], /lastTurnHasCompletionAction=false/);
	assert.match(s.alerts[0], /assistantTurnCount=0/);
	assert.doesNotMatch(s.alerts[0], /CQLOCAL LAB: Respond/);
	assert.equal(s.timers.length, 0);
});
test("ARM with unverified editor refuses scheduling and restores STOP", async () => {
	const s = sandbox();
	s.context.navigator.locks = {
		async request(_name, _opts, fn) {
			return fn({ name: "local-test-lock" });
		},
	};
	s.run();
	await flush();
	s.responses.push("coordinator", "CQLOCAL LAB: harmless.");
	await s.runMenu("Configure LAB");
	await s.runMenu("Arm LAB");
	assert.equal([...s.values].find(([k]) => k.endsWith(":STOP"))?.[1], true);
	assert.equal(s.timers.length, 0);
});
