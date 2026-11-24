#!/usr/bin/env python3
"""Opt-in browser E2E against loopback-only fixture. Never visits ChatGPT.

The original LAB userscript is rehosted IN MEMORY for localhost and runs with
accelerated waits to validate browser DOM integration. It never installs to
Firefox/Violentmonkey and never uses the user's Firefox profile.
"""
from __future__ import annotations

import contextlib
import http.server
import json
import re
import shutil
import subprocess
import threading
import time
from pathlib import Path
from urllib.parse import urlparse

from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.support.ui import WebDriverWait

ROOT = Path(__file__).resolve().parents[1] / "firefox"
BUILDER = ROOT / "build-chatqueue-local.mjs"
BUNDLE = ROOT / "chatqueue-local.user.js"
BASE_COOLDOWN = 360_000
LAB_COOLDOWN_MS = 3_000
LAB_STABILITY_MS = 1_000
LAB_TERMINAL_DEADLINE_MS = 8_000
SELENIUM_TIMEOUT = 18

PAGE = """
<!doctype html><html><head><meta charset="utf-8"><title>Private Local ChatQueue LAB</title>
<style>
body {font:16px sans-serif; margin:20px} form {margin-top:10px}
[contenteditable] {min-height:32px; border:1px solid #999; padding:8px; width:70%}
article {padding:6px; margin:4px; background:#efefef}
</style></head><body>
<h1>Local test only — no ChatGPT or browser profile data</h1>
<div id="messages">
<div data-virtualized-turn>
  <div data-user-message-bubble>CQLOCAL LAB: Manual seed.</div>
  <div class="group flex flex-col">
    <h4 data-conversation-role="assistant">Assistant</h4>
    <p>LAB_ACK</p>
    <button type="button" aria-label="Copiar">Copiar</button>
  </div>
</div>
</div>
<!-- The authenticated page has a second bubble OUTSIDE the current turn. -->
<div data-user-message-bubble style="position:absolute; left:-9999px">
  CQLOCAL LAB: Manual seed.
</div>
<form data-chatgpt-composer hidden>
  <div contenteditable="true" role="textbox"></div>
</form>
<form id="active-composer" data-chatgpt-composer>
<div contenteditable="true" role="textbox" aria-label="Test composer"></div>
<!-- Like ChatGPT: no Send button exists until a draft is composed. -->
</form>
<button type="button" id="stop" data-testid="stop-button" style="display:none">Stop</button>
<script>
(() => {
  const form = document.querySelector("#active-composer");
  const input = form.querySelector("[contenteditable]");
  const stop = document.querySelector("#stop");
  const box = document.querySelector("#messages");
  const send = document.createElement("button");
  send.type = "button";
  send.setAttribute("data-testid", "send-button");
  send.setAttribute("aria-label", "Send test");
  send.textContent = "Send test";
  send.addEventListener("click", () => form.requestSubmit());
  const toggleSend = () => {
    if (input.innerText.trim()) {
      if (!send.isConnected) form.appendChild(send);
    } else if (send.isConnected) {
      send.remove();
    }
  };
  input.addEventListener("input", toggleSend);
  const id = location.pathname.split("/").pop();
  if (id.includes("ambiguous-form")) {
    document.querySelector("form[hidden]").hidden = false;
  }
  if (id.includes("no-manual-seed")) {
    document.querySelector("#messages").replaceChildren();
  }
  if (id.includes("unanswered-latest")) {
    const pending = document.createElement("div");
    pending.setAttribute("data-virtualized-turn", "");
    const unpaired = document.createElement("div");
    unpaired.setAttribute("data-user-message-bubble", "");
    unpaired.textContent = "CQLOCAL LAB: latest unanswered user message.";
    pending.appendChild(unpaired);
    box.appendChild(pending);
  }
  const delayMs = id.includes("restart") ? 20000 : 800;
  const key = "FIXTURE_EVENTS:" + location.pathname;
  window.__fixture = {
    get events() { return JSON.parse(localStorage.getItem(key) || "[]"); }
  };
  form.addEventListener("submit", (event) => {
    event.preventDefault();
    if (stop.style.display !== "none") return;
    const body = input.innerText.trim();
    if (!body.startsWith("CQLOCAL LAB:")) return;
    const turn = document.createElement("div");
    turn.setAttribute("data-virtualized-turn", "");
    const row = document.createElement("div");
    row.setAttribute("data-user-message-bubble", "");
    row.textContent = body;
    turn.appendChild(row);
    box.appendChild(turn);
    input.textContent = "";
    toggleSend();
    stop.style.display = "block";
    const sentAt = Date.now();
    const previous = window.__fixture.events;
    previous.push({ type: "send", at: sentAt, body: body });
    localStorage.setItem(key, JSON.stringify(previous));
    setTimeout(() => {
      const answer = document.createElement("div");
      answer.className = "group flex flex-col";
      const role = document.createElement("h4");
      role.setAttribute("data-conversation-role", "assistant");
      role.textContent = "assistant";
      answer.appendChild(role);
      const text = document.createElement("p");
      text.textContent = "LAB_ACK";
      answer.appendChild(text);
      if (!id.includes("no-receipt")) {
        const receipt = document.createElement("button");
        receipt.type = "button";
        receipt.setAttribute("aria-label", "Copiar");
        receipt.textContent = "Copiar";
        answer.appendChild(receipt);
      }
      turn.appendChild(answer);
      stop.style.display = "none";
      const previous = window.__fixture.events;
      previous.push({ type: "reply", at: Date.now(), body: "LAB_ACK" });
      localStorage.setItem(key, JSON.stringify(previous));
    }, delayMs);
  });
})();
</script></body></html>
"""


def assert_all_local(url: str) -> None:
    parsed = urlparse(url)
    if parsed.scheme != "http" or parsed.hostname != "127.0.0.1":
        raise AssertionError("Only loopback test URLs permitted")
    if not re.fullmatch(r"/c/[A-Za-z0-9-]+", parsed.path):
        raise AssertionError("Unexpected test page URL")


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self) -> None:
        parsed = urlparse(self.path)
        if re.fullmatch(r"/c/[A-Za-z0-9-]+", parsed.path):
            body = PAGE.encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.send_header("Cache-Control", "no-store")
            self.end_headers()
            self.wfile.write(body)
        else:
            self.send_error(404)

    def log_message(self, fmt: str, *args: object) -> None:
        return


BOOTSTRAP = """
window.__CQTEST = {
  menus: [],
  prompts: [],
  confirms: true,
  alerts: [],
};
window.GM_getValue = (key, fallback) => {
  const raw = window.localStorage.getItem(key);
  return raw === null ? fallback : JSON.parse(raw);
};
window.GM_setValue = (key, value) => {
  window.localStorage.setItem(key, JSON.stringify(value));
};
window.GM_registerMenuCommand = (name, callback) => {
  window.__CQTEST.menus.push({ name, callback });
};
window.prompt = () => window.__CQTEST.prompts.shift() ?? null;
window.confirm = () => window.__CQTEST.confirms;
window.alert = (value) => window.__CQTEST.alerts.push(String(value));
window.eval(arguments[0]);
"""


def render_local_bundle(port: int) -> str:
    subprocess.run(["node", str(BUILDER), "--check"], check=True, capture_output=True)
    bundle = BUNDLE.read_text()
    old_origin = 'const origin = "https://chatgpt.com";'
    old_cooldown = "const REPLY_COOLDOWN_MS = 360_000;"
    old_stability = "Date.now() - stableSince >= 6000"
    old_deadline = "const deadline = Date.now() + 2 * 60 * 60 * 1000;"
    for needle in (old_origin, old_cooldown, old_stability, old_deadline):
        if bundle.count(needle) != 1:
            raise AssertionError("Source changed; explicit fixture patch must be reviewed")
    bundle = bundle.replace(old_origin, 'const origin = "http://127.0.0.1:' + str(port) + '";')
    bundle = bundle.replace(old_cooldown, 'const REPLY_COOLDOWN_MS = ' + str(LAB_COOLDOWN_MS) + ';')
    bundle = bundle.replace(old_stability, "Date.now() - stableSince >= " + str(LAB_STABILITY_MS))
    bundle = bundle.replace(old_deadline, "const deadline = Date.now() + " + str(LAB_TERMINAL_DEADLINE_MS) + ";")
    return bundle


def count_events(driver: webdriver.Firefox, kind: str) -> int:
    return driver.execute_script(
        "return window.__fixture.events.filter(e => e.type === arguments[0]).length;",
        kind,
    )


def events(driver: webdriver.Firefox) -> list[dict]:
    return driver.execute_script("return window.__fixture.events;")


def phase(driver: webdriver.Firefox) -> str | None:
    return driver.execute_script(
        "return JSON.parse(localStorage.getItem('CQLOCAL_V1:' + "
        "location.origin + location.pathname + ':STATE:coordinator') || 'null')?.phase || null;"
    )


def boot(driver: webdriver.Firefox, bundle: str) -> None:
    assert_all_local(driver.current_url)
    if not driver.execute_script("return !!navigator.locks?.request;"):
        raise AssertionError("Firefox LockManager unavailable in loopback")
    driver.execute_script(BOOTSTRAP, bundle)
    WebDriverWait(driver, 10).until(
        lambda d: d.execute_script("return window.__CQTEST?.menus?.length === 4;")
    )
    # Browser startup must not arm a timer or send.
    assert count_events(driver, "send") == 0, "Unexpected initial send"


def menu(driver: webdriver.Firefox, substring: str) -> None:
    driver.execute_script(
        "const item = window.__CQTEST.menus.find(m => m.name.includes(arguments[0]));"
        "if (!item) throw new Error('Missing test menu'); item.callback();",
        substring,
    )


def configure(driver: webdriver.Firefox, role: str, content: str = "CQLOCAL LAB: Respond only LAB_ACK.") -> None:
    driver.execute_script(
        "window.__CQTEST.prompts = [arguments[0], arguments[1]];",
        role, content
    )
    menu(driver, "Configure LAB")
    WebDriverWait(driver, 10).until(
        lambda d: d.execute_script(
            "return !!localStorage.getItem('CQLOCAL_V1:' + location.origin + "
            "location.pathname + ':CONFIG');"
        )
    )


def arm(driver: webdriver.Firefox) -> None:
    menu(driver, "Arm LAB")


def run_four_tab_case(driver: webdriver.Firefox, port: int, bundle: str) -> None:
    roles = ("coordinator", "platformmanager", "businessmanager", "qualitymanager")
    tabs = {}
    for index, role in enumerate(roles):
        if index:
            driver.switch_to.new_window("tab")
        driver.get(f"http://127.0.0.1:{port}/c/{role}-lab")
        boot(driver, bundle)
        configure(driver, role, "CQLOCAL LAB: Synthetic role " + role)
        tabs[role] = driver.current_window_handle
    for role, tab in tabs.items():
        driver.switch_to.window(tab)
        arm(driver)
        WebDriverWait(driver, 8).until(lambda d: d.execute_script(
            "const url = location.origin + location.pathname;"
            "const c=JSON.parse(localStorage.getItem('CQLOCAL_V1:' + url + ':CONFIG') || 'null');"
            "const s=JSON.parse(localStorage.getItem('CQLOCAL_V1:' + url + ':STATE:' + c?.agentId) || 'null');"
            "return c?.agentId === arguments[0] && s?.phase === 'READY';", role))
    for role, tab in tabs.items():
        driver.switch_to.window(tab)
        WebDriverWait(driver, 45).until(lambda d: count_events(d, "reply") >= 2)
        data = events(driver)
        sends = [e for e in data if e["type"] == "send"]
        replies = [e for e in data if e["type"] == "reply"]
        assert len(sends) == len(replies) == 2, "Duplicate or missing send/reply"
        assert sends[1]["at"] - replies[0]["at"] >= LAB_COOLDOWN_MS
        assert all(e["body"] == "CQLOCAL LAB: Synthetic role " + role for e in sends)
        WebDriverWait(driver, 10).until(
            lambda d: d.execute_script(
                "return JSON.parse(localStorage.getItem('CQLOCAL_V1:' + location.origin + location.pathname + ':STOP') || 'false') === true;"
            )
        )
        print(f"PASS: role fixture {role}: 2 sends, 2 terminal replies, isolated content/cooldown/STOP", flush=True)
    for role in roles[1:]:
        driver.switch_to.window(tabs[role])
        driver.close()
    driver.switch_to.window(tabs[roles[0]])
    print("PASS: four distinct synthetic role tabs; no real agent conversations", flush=True)


def run_duplicate_tab_case(driver: webdriver.Firefox, port: int, bundle: str) -> None:
    original_tab = driver.current_window_handle
    driver.switch_to.new_window("tab")
    url = f"http://127.0.0.1:{port}/c/restart-duplicate-lab"
    driver.get(url)
    boot(driver, bundle)
    configure(driver, "coordinator")
    first = driver.current_window_handle
    arm(driver)
    WebDriverWait(driver, 12).until(lambda d: count_events(d, "send") == 1)
    WebDriverWait(driver, 8).until(lambda d: phase(d) == "AWAITING_REPLY")
    before = driver.execute_script(
        "return JSON.parse(localStorage.getItem('CQLOCAL_V1:' + location.origin + location.pathname + ':STATE:coordinator')).attemptId;"
    )
    assert before
    driver.switch_to.new_window("tab")
    driver.get(url)
    driver.execute_script(BOOTSTRAP, bundle)
    WebDriverWait(driver, 10).until(
        lambda d: d.execute_script(
            "return window.__CQTEST?.menus?.length === 4 && JSON.parse(localStorage.getItem('CQLOCAL_V1:' + location.origin + location.pathname + ':STOP') || 'false') === true;"
        )
    )
    # Same URL, same storage, second document. Neither reconfigure nor arm
    # may supersede the in-flight attempt even though the second DOM is idle.
    driver.execute_script("window.__CQTEST.prompts = ['coordinator', 'CQLOCAL LAB: Replacement.'];")
    menu(driver, "Configure LAB")
    WebDriverWait(driver, 10).until(
        lambda d: d.execute_script("return window.__CQTEST.alerts.some(a => a.includes('refused'));")
    )
    menu(driver, "Arm LAB")
    WebDriverWait(driver, 12).until(lambda d: phase(d) == "REVIEW")
    assert count_events(driver, "send") == 1
    after = driver.execute_script(
        "return JSON.parse(localStorage.getItem('CQLOCAL_V1:' + location.origin + location.pathname + ':STATE:coordinator')).attemptId;"
    )
    assert before == after, "Original uncertain attempt ID was lost"
    driver.close()
    driver.switch_to.window(first)
    assert count_events(driver, "send") == 1
    driver.close()
    driver.switch_to.window(original_tab)
    print("PASS: duplicate same-conversation tab revokes older lease, preserves attempt in REVIEW, zero replay", flush=True)


def run_restart_case(driver: webdriver.Firefox, port: int, bundle: str) -> None:
    original_tab = driver.current_window_handle
    driver.switch_to.new_window("tab")
    url = "http://127.0.0.1:" + str(port) + "/c/restart-lab"
    driver.get(url)
    boot(driver, bundle)
    configure(driver, "coordinator")
    arm(driver)
    WebDriverWait(driver, 12).until(
        lambda d: count_events(d, "send") >= 1
    )
    # Restart mid-reply. Source stores AWAITING_REPLY, so reload must freeze it
    # in REVIEW and never replay the already-delivered request.
    driver.refresh()
    driver.execute_script(BOOTSTRAP, bundle)
    WebDriverWait(driver, 10).until(lambda d: phase(d) == "REVIEW")
    data = events(driver)
    assert sum(1 for e in data if e["type"] == "send") == 1
    assert driver.execute_script(
        "return JSON.parse(localStorage.getItem('CQLOCAL_V1:' + "
        "location.origin + location.pathname + ':STOP'));"
    ) is True
    print("PASS: REAL Firefox headless — reload during answer freezes REVIEW with original send, no replay", flush=True)
    driver.close()
    driver.switch_to.window(original_tab)


def run_no_receipt_case(driver: webdriver.Firefox, port: int, bundle: str) -> None:
    original_tab = driver.current_window_handle
    driver.switch_to.new_window("tab")
    url = "http://127.0.0.1:" + str(port) + "/c/no-receipt-lab"
    driver.get(url)
    boot(driver, bundle)
    configure(driver, "coordinator")
    arm(driver)
    WebDriverWait(driver, 12).until(lambda d: count_events(d, "send") == 1)
    WebDriverWait(driver, 12).until(lambda d: count_events(d, "reply") == 1)
    # Even though the text becomes stable and the Stop button disappears,
    # without a corroborating terminal receipt there must be NO second send.
    WebDriverWait(driver, LAB_TERMINAL_DEADLINE_MS / 1000 + 6).until(
        lambda d: phase(d) == "REVIEW"
    )
    assert count_events(driver, "send") == 1, "Unsafe retry without terminal receipt"
    assert driver.execute_script(
        "return JSON.parse(localStorage.getItem('CQLOCAL_V1:' + "
        "location.origin + location.pathname + ':STOP'));"
    ) is True
    print("PASS: REAL Firefox headless — text stable and Stop hidden but no receipt => REVIEW, zero retries", flush=True)
    driver.close()
    driver.switch_to.window(original_tab)


def run_preflight_denial_case(
    driver: webdriver.Firefox, port: int, bundle: str, path: str
) -> None:
    original_tab = driver.current_window_handle
    driver.switch_to.new_window("tab")
    driver.get("http://127.0.0.1:" + str(port) + "/c/" + path)
    boot(driver, bundle)
    configure(driver, "coordinator")
    arm(driver)
    WebDriverWait(driver, 8).until(
        lambda d: d.execute_script(
            "return window.__CQTEST.alerts.some(a => a.includes('operation denied') || a.includes('arm refused')) && "
            "JSON.parse(localStorage.getItem('CQLOCAL_V1:' + location.origin + location.pathname + ':STOP') || 'false') === true;"
        )
    )
    assert count_events(driver, "send") == 0
    print(
        "PASS: REAL Firefox headless - denied " + path +
        " without sending; STOP restored",
        flush=True,
    )
    driver.close()
    driver.switch_to.window(original_tab)


def run_manual_pause_case(driver: webdriver.Firefox, port: int, bundle: str) -> None:
    original_tab = driver.current_window_handle
    driver.switch_to.new_window("tab")
    url = "http://127.0.0.1:" + str(port) + "/c/restart-pause-lab"
    driver.get(url)
    boot(driver, bundle)
    configure(driver, "coordinator")
    arm(driver)
    WebDriverWait(driver, 12).until(lambda d: count_events(d, "send") == 1)
    menu(driver, "Pause LAB")
    WebDriverWait(driver, 10).until(lambda d: phase(d) == "REVIEW")
    assert count_events(driver, "send") == 1, "Pause must not trigger retry"
    print("PASS: REAL Firefox headless — manual pause during answer => REVIEW, zero retries", flush=True)
    driver.close()
    driver.switch_to.window(original_tab)


def main() -> int:
    if shutil.which("geckodriver") is None:
        raise SystemExit("Missing geckodriver; run pinned Nix devshell target")
    handler = Handler
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    port = server.server_address[1]
    bundle = render_local_bundle(port)
    options = webdriver.FirefoxOptions()
    options.binary_location = "/usr/bin/firefox"
    options.add_argument("-headless")
    for pref, value in {
        "app.update.enabled": False,
        "browser.shell.checkDefaultBrowser": False,
        "datareporting.healthreport.uploadEnabled": False,
        "toolkit.telemetry.enabled": False,
        "browser.startup.homepage": "about:blank",
    }.items():
        options.set_preference(pref, value)
    service = Service(executable_path=shutil.which("geckodriver"))
    driver = None
    try:
        driver = webdriver.Firefox(options=options, service=service)
        driver.set_page_load_timeout(20)
        run_four_tab_case(driver, port, bundle)
        run_duplicate_tab_case(driver, port, bundle)
        run_restart_case(driver, port, bundle)
        run_no_receipt_case(driver, port, bundle)
        run_preflight_denial_case(driver, port, bundle, "ambiguous-form-lab")
        run_preflight_denial_case(driver, port, bundle, "no-manual-seed-lab")
        run_preflight_denial_case(driver, port, bundle, "unanswered-latest-lab")
        run_manual_pause_case(driver, port, bundle)
        print("PASS: isolated browser E2E; no ChatGPT, account, profile or broker agents accessed", flush=True)
        return 0
    finally:
        if driver:
            with contextlib.suppress(Exception):
                driver.quit()
        server.shutdown()
        server.server_close()
        thread.join(timeout=5)


if __name__ == "__main__":
    raise SystemExit(main())
