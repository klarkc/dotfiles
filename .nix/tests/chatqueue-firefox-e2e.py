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
SELENIUM_TIMEOUT = 18

PAGE = """
<!doctype html><html><head><meta charset="utf-8"><title>Private Local ChatQueue LAB</title>
<style>
body {font:16px sans-serif; margin:20px} form {margin-top:10px}
[contenteditable] {min-height:32px; border:1px solid #999; padding:8px; width:70%}
article {padding:6px; margin:4px; background:#efefef}
</style></head><body>
<h1>Local test only — no ChatGPT or browser profile data</h1>
<div id="messages"></div>
<form data-chatgpt-composer>
<div contenteditable="true" role="textbox" aria-label="Test composer"></div>
<button type="submit" aria-label="Send test">Send test</button>
</form>
<button type="button" id="stop" data-testid="stop-button" style="display:none">Stop</button>
<script>
(() => {
  const form = document.querySelector("form");
  const input = document.querySelector("[contenteditable]");
  const stop = document.querySelector("#stop");
  const box = document.querySelector("#messages");
  const id = location.pathname.split("/").pop();
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
    const row = document.createElement("article");
    row.setAttribute("data-message-author-role", "user");
    row.textContent = body;
    box.appendChild(row);
    input.textContent = "";
    stop.style.display = "block";
    const sentAt = Date.now();
    const previous = window.__fixture.events;
    previous.push({ type: "send", at: sentAt, body: body });
    localStorage.setItem(key, JSON.stringify(previous));
    setTimeout(() => {
      const answer = document.createElement("article");
      answer.setAttribute("data-message-author-role", "assistant");
      answer.textContent = "LAB_ACK";
      box.appendChild(answer);
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
    for needle in (old_origin, old_cooldown, old_stability):
        if bundle.count(needle) != 1:
            raise AssertionError("Source changed; explicit fixture patch must be reviewed")
    bundle = bundle.replace(old_origin, 'const origin = "http://127.0.0.1:' + str(port) + '";')
    bundle = bundle.replace(old_cooldown, 'const REPLY_COOLDOWN_MS = ' + str(LAB_COOLDOWN_MS) + ';')
    bundle = bundle.replace(old_stability, "Date.now() - stableSince >= " + str(LAB_STABILITY_MS))
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


def configure(driver: webdriver.Firefox, role: str) -> None:
    driver.execute_script(
        "window.__CQTEST.prompts = [arguments[0], arguments[1]];",
        role, "CQLOCAL LAB: Respond only LAB_ACK."
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


def run_two_tab_case(driver: webdriver.Firefox, port: int, bundle: str) -> None:
    a = "http://127.0.0.1:" + str(port) + "/c/a-lab"
    b = "http://127.0.0.1:" + str(port) + "/c/b-lab"
    driver.get(a)
    boot(driver, bundle)
    configure(driver, "coordinator")
    a_tab = driver.current_window_handle

    driver.switch_to.new_window("tab")
    driver.get(b)
    boot(driver, bundle)
    configure(driver, "qualitymanager")
    b_tab = driver.current_window_handle

    # Do not arm both unless all local fixture preflight conditions hold.
    arm(driver)
    WebDriverWait(driver, 8).until(lambda d: d.execute_script(
        "const c=JSON.parse(localStorage.getItem('CQLOCAL_V1:' + location.origin + location.pathname + ':CONFIG') || 'null'); "
        "return JSON.parse(localStorage.getItem('CQLOCAL_V1:' + location.origin + location.pathname + ':STATE:' + c?.agentId) || 'null')?.phase === 'READY';"))
    print("LAB_DRIVER_QUALITYMANAGER_ARMED", flush=True)
    driver.switch_to.window(a_tab)
    arm(driver)
    WebDriverWait(driver, 8).until(lambda d: d.execute_script(
        "const c=JSON.parse(localStorage.getItem('CQLOCAL_V1:' + location.origin + location.pathname + ':CONFIG') || 'null'); "
        "return JSON.parse(localStorage.getItem('CQLOCAL_V1:' + location.origin + location.pathname + ':STATE:' + c?.agentId) || 'null')?.phase === 'READY';"))
    print("LAB_DRIVER_COORDINATOR_ARMED", flush=True)

    for tab in (a_tab, b_tab):
        driver.switch_to.window(tab)
        try:
            WebDriverWait(driver, SELENIUM_TIMEOUT).until(
                lambda d: count_events(d, "send") >= 2
            )
        except Exception:
            diagnostic = driver.execute_script("""
              const url = location.origin + location.pathname;
              return { pathname: location.pathname,
                events: window.__fixture.events,
                status: (() => { const cfg = JSON.parse(localStorage.getItem('CQLOCAL_V1:' + url + ':CONFIG') || 'null'); return JSON.parse(localStorage.getItem('CQLOCAL_V1:' + url + ':STATE:' + cfg?.agentId) || 'null'); })(),
                stop: localStorage.getItem('CQLOCAL_V1:' + url + ':STOP'),
                config: JSON.parse(localStorage.getItem('CQLOCAL_V1:' + url + ':CONFIG') || 'null'),
                alerts: window.__CQTEST?.alerts ?? [],
                buttons: [...document.querySelectorAll('button')].map(b => ({label: b.getAttribute('aria-label'), hidden: b.getClientRects().length === 0}))
              };
            """)
            print("LAB_DIAGNOSTIC:", json.dumps(diagnostic, sort_keys=True), flush=True)
            raise
        WebDriverWait(driver, SELENIUM_TIMEOUT).until(
            lambda d: count_events(d, "reply") >= 2
        )
        data = events(driver)
        sends = [e for e in data if e["type"] == "send"]
        replies = [e for e in data if e["type"] == "reply"]
        assert len(sends) == len(replies) == 2, "Duplicate or missing send/reply"
        assert sends[1]["at"] - replies[0]["at"] >= LAB_COOLDOWN_MS, (
            "Cooldown counted from send rather than terminal reply"
        )
        assert all(e["body"].startswith("CQLOCAL LAB:") for e in sends)
        WebDriverWait(driver, 10).until(
            lambda d: d.execute_script(
                "const key = 'CQLOCAL_V1:' + location.origin + location.pathname + ':STOP';"
                "return JSON.parse(localStorage.getItem(key) || 'false') === true;"
            )
        )
    print("PASS: REAL Firefox headless on 127.0.0.1 — two tabs, 2 replies each, reply-based cooldown, STOP", flush=True)
    driver.switch_to.window(b_tab)
    driver.close()
    driver.switch_to.window(a_tab)
    # Keep one window open; the next test opens an isolated restart tab.


def run_restart_case(driver: webdriver.Firefox, port: int, bundle: str) -> None:
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
        run_two_tab_case(driver, port, bundle)
        run_restart_case(driver, port, bundle)
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
