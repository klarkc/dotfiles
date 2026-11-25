#!/usr/bin/env python3
"""Opt-in signed host integration, only a disposable Firefox profile.

Requires a locally available signed Violentmonkey XPI and explicit native
installer confirmation. No access to a live profile, ChatGPT, actual broker
or real credentials. Loopback-only fixture, actual GM APIs and Node lock
endpoint; synthetic policy evidence, DOM and shortened timing.
"""
import argparse
import contextlib
import http.server
import importlib.util
import json
import re
import shutil
import sys
import threading
import time
from pathlib import Path
from urllib.parse import urlsplit

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException

sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location(
    "operational_fixture", Path(__file__).with_name("chatqueue-operational-firefox-e2e.py")
)
fixture = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixture)
fixture.COOLDOWN = 6000


def run(args):
    assert args.signed_xpi.is_file() and args.signed_xpi.suffix == ".xpi"
    assert args.confirm_file.is_absolute() and not args.confirm_file.exists()
    assert args.confirm_file.parent.is_dir()
    assert shutil.which("geckodriver")
    bundle = None

    class Handler(fixture.Handler):
        def do_GET(self):
            if urlsplit(self.path).path == "/cqop-signed-fixture.user.js":
                self.respond(200, bundle.encode(), "application/javascript; charset=utf-8")
            else:
                super().do_GET()

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    port = server.server_address[1]
    origin = f"http://127.0.0.1:{port}"
    bundle = fixture.render_bundle(port)

    def replace(old, new):
        nonlocal bundle
        assert bundle.count(old) == 1, old
        bundle = bundle.replace(old, new)

    replace(
        "// @name         ChatQueue Broker Operational (klarkc/dotfiles)",
        "// @name         CQOP SIGNED BRIDGE LOCK FIXTURE",
    )
    replace(
        "// @match        https://chatgpt.com/g/*/c/*",
        f"// @match        {origin}/g/example-project/c/*",
    )
    cfg = {
        "version": 1,
        "reviewed": False,
        "promptBlob": fixture.PROMPT_BLOB,
        "bridge": {"port": port, "capability": fixture.TOKEN},
        "bindings": [
            {
                "agent": r,
                "path": f"/g/example-project/c/example-{r}",
                "modelLabel": "6 Extra alto",
                "prompt": fixture.PROMPTS[r],
            }
            for r in fixture.ROLES
        ],
    }
    # Only the disposable test copy gains these synthetic provision/menu
    # buttons. GM values and capability never leave the privileged realm.
    anchor = "const path = location.pathname;"
    injection = r"""
    const fixtureControls=document.createElement("div");
    fixtureControls.id="cqop-signed-fixture-controls";
    document.body.appendChild(fixtureControls);
    const fixtureButton=(label,fn)=>{
        const b=document.createElement("button");
        b.type="button";b.textContent=label;
        b.addEventListener("click",()=>Promise.resolve().then(fn).catch(()=>window.alert("Fixture callback denied")));
        fixtureControls.appendChild(b);
    };
    fixtureButton("Provision synthetic config",async()=>{
        if(await GM.getValue(OPERATIONAL_CONFIG_KEY,null))return;
        await GM.setValue(OPERATIONAL_CONFIG_KEY,FIXTURE_CONFIG);
        location.reload();
    });
    fixtureButton("Prepare synthetic successor",async()=>{
        const cfg=await GM.getValue(OPERATIONAL_CONFIG_KEY,null);
        if(!cfg||cfg.reviewed||!cfg.bindings.some(b=>b.agent==="coordinator"&&b.path===location.pathname))return;
        await GM.setValue(OPERATIONAL_CONFIG_KEY,{
            ...cfg, bindings:cfg.bindings.map(b=>b.agent==="coordinator"?{...b,path:"/g/example-project/c/example-coordinator-successor"}:b)
        });
        window.alert("Synthetic successor configured; no send");
    });
    fixtureButton("Review synthetic config",async()=>{
        const cfg=await GM.getValue(OPERATIONAL_CONFIG_KEY,null);
        if(!cfg||cfg.reviewed)return;
        await GM.setValue(OPERATIONAL_CONFIG_KEY,{...cfg,reviewed:true});
        location.reload();
    });
    const fixtureMenuRegister=(label,callback)=>{
        GM_registerMenuCommand(label,callback);
        fixtureButton(label,callback);
    };
""".replace("FIXTURE_CONFIG", json.dumps(cfg, ensure_ascii=False))
    replace(anchor, anchor + "\n" + injection)
    replace(
        'GM_registerMenuCommand("ChatQueue Broker - " + title, () =>',
        'fixtureMenuRegister("ChatQueue Broker - " + title, () =>',
    )
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    options = webdriver.FirefoxOptions()
    options.binary_location = "/usr/bin/firefox"
    for k, v in {
        "app.update.enabled": False,
        "browser.shell.checkDefaultBrowser": False,
        "datareporting.healthreport.uploadEnabled": False,
        "toolkit.telemetry.enabled": False,
        "browser.startup.homepage": "about:blank",
        "extensions.update.enabled": False,
    }.items():
        options.set_preference(k, v)
    driver = None

    def wait(fn, seconds=20):
        return WebDriverWait(driver, seconds).until(fn)

    def count(kind):
        return fixture.count(driver, kind)

    def button(label):
        return driver.find_element(
            By.XPATH, "//div[@id='cqop-signed-fixture-controls']/button[contains(., " + json.dumps(label) + ")]"
        )

    def accept_alert():
        a = wait(EC.alert_is_present(), 20)
        text = a.text
        a.accept()
        return text

    def status():
        button("Status (no send)").click()
        raw = accept_alert()
        if not raw.startswith("{"):
            raise AssertionError("Status refused: " + raw[:160])
        return json.loads(raw)

    def wait_status(predicate, seconds=20):
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            s = status()
            if predicate(s):
                return s
            time.sleep(.2)
        raise AssertionError("Status condition not met")

    def open_role(role, new=False):
        if new:
            driver.switch_to.new_window("tab")
        driver.get(f"{origin}/g/example-project/c/example-{role}")
        wait(lambda _: driver.find_elements(By.ID, "cqop-signed-fixture-controls"))

    def await_menus():
        wait(lambda _: driver.find_elements(By.XPATH, "//button[contains(.,'Status (no send)')]"))

    def arm(mode):
        button("Arm " + mode).click()
        assert "Explicit " + mode + " GO" in accept_alert()

    def proof(role, s):
        attempt = s["supervisedAttemptId"]
        assert re.fullmatch(r"cqop-[0-9a-f-]{36}", attempt)
        assert attempt in fixture.events(driver)[0]["body"]
        with fixture.MUTEX:
            fixture.PROOFS[role] = {
                "status": "OWN_GIT_PAIR_VERIFIED", "agent": role,
                "attemptId": attempt, "baselineMain": "a" * 40,
                "main": "a" * 40, "startedCommit": "1" * 40, "startedBlob": "2" * 40,
                "terminalCommit": "3" * 40, "terminalBlob": "4" * 40,
                "runId": "fixture-run", "operationId": "fixture-operation",
                "terminalStatus": "PROGRESS", "providerEffectsVerified": False,
            }
        return attempt

    try:
        driver = webdriver.Firefox(options=options, service=Service(executable_path=shutil.which("geckodriver")))
        driver.set_page_load_timeout(6)
        addon = driver.install_addon(str(args.signed_xpi), temporary=False)

        def signed(d):
            p = Path(d.capabilities["moz:profile"]) / "extensions.json"
            try:
                return any(
                    a.get("id") == addon and a.get("signedState") == 2 and a.get("active")
                    and a.get("defaultLocale", {}).get("name") == "Violentmonkey"
                    for a in json.loads(p.read_text())["addons"]
                )
            except Exception:
                return False

        wait(signed)
        print(f"Signed host installed in disposable Firefox PID {driver.capabilities['moz:processID']}; confirm native fixture installer only.", flush=True)
        try:
            driver.get(origin + "/cqop-signed-fixture.user.js")
        except TimeoutException:
            pass
        deadline = time.monotonic() + 180
        while not args.confirm_file.exists():
            if time.monotonic() > deadline:
                raise TimeoutError("Native installer confirmation missing")
            time.sleep(.25)
        driver.switch_to.new_window("tab")
        open_role("coordinator")
        assert count("send") == 0
        button("Provision synthetic config").click()
        await_menus()
        s = status()
        assert s["phase"] == "PAUSED" and s["stopped"] and not s["reviewed"], s
        button("Arm supervised").click()
        assert "not approved" in accept_alert() and count("send") == 0
        button("Check policy").click()
        assert "evidence only" in accept_alert()
        print("PASS: signed GM config, actual privileged HTTP/lock RPC, inert boot and unreviewed denial", flush=True)
        # The old document is retired by navigation; only a synthetic
        # pristine config is edited via actual privileged GM storage.
        button("Prepare synthetic successor").click()
        assert "Synthetic successor configured" in accept_alert()
        with fixture.MUTEX:
            fixture.PAUSED_ROLES.add("coordinator")
        driver.get(origin + "/g/example-project/c/example-coordinator-successor")
        await_menus()
        before = status()
        assert before["bindingMigrationRequired"] and before["cycle"] == 0 and before["stopped"], before
        button("Migrate pristine paused binding").click()
        assert "Migrate this role" in accept_alert()
        assert "Pristine binding migrated" in accept_alert()
        after = status()
        assert not after["bindingMigrationRequired"] and after["phase"] == "PAUSED" and after["cycle"] == 0 and after["stopped"] and not after["leased"], after
        assert count("send") == 0
        driver.refresh()
        await_menus()
        assert not status()["bindingMigrationRequired"]
        with fixture.MUTEX:
            fixture.PAUSED_ROLES.remove("coordinator")
        print("PASS: signed GM pristine rollover, witnessed state, zero sends", flush=True)
        button("Review synthetic config").click()
        await_menus()
        assert status()["reviewed"] is True

        tabs = {}
        for i, role in enumerate(fixture.ROLES):
            if i:
                open_role(role, new=True)
                await_menus()
            s = status()
            assert s["cycle"] == 0 and s["stopped"], s
            tabs[role] = driver.current_window_handle
            if role == "qualitymanager":
                driver.execute_script("localStorage.setItem('CQOP_DELAY:qualitymanager','5500')")
            arm("supervised")
            wait(lambda _: count("send") == 1)
            if role == "qualitymanager":
                s = wait_status(lambda s: s["phase"] == "AWAITING_REPLY")
                attempt = s["attemptId"]
                driver.execute_script("""
                    const m=document.getElementById('messages'),duplicate=m.lastElementChild.cloneNode(true);
                    duplicate.id='fixture-duplicate';m.append(duplicate);
                """)
                s = wait_status(lambda s: s["phase"] == "REVIEW" and s["stopped"])
                assert s["attemptId"] == attempt
                driver.execute_script("document.getElementById('fixture-duplicate').remove()")
                wait(lambda _: count("reply") == 1)
                assert proof(role, s) == attempt
                with fixture.MUTEX:
                    fixture.PAUSED_ROLES.add(role)
                button("Reconcile supervised REVIEW").click()
                assert "Reconcile only the exact" in accept_alert()
                assert "Exact attempt reconciled" in accept_alert()
                s = status()
                assert s["phase"] == "PAUSED" and s["stopped"] and not s["leased"] and s["attemptId"] is None
                assert s["supervisedAttemptId"] == attempt and s["supervisedGitProofStored"]
                with fixture.MUTEX:
                    fixture.PAUSED_ROLES.remove(role)
                driver.execute_script("localStorage.removeItem('CQOP_DELAY:qualitymanager')")
                print("PASS: signed GM paused REVIEW reconciliation of same attempt, no resend", flush=True)
            else:
                wait(lambda _: count("reply") == 1)
                s = wait_status(lambda s: s["phase"] == "COOLDOWN" and s["stopped"] and s["supervised"])
                assert s["supervisedGitProofStored"] is False
            assert s["cycle"] == 1
            assert count("send") == 1
            assert not driver.execute_script("return Object.keys(localStorage).some(k=>k.startsWith('CQOP_V1:'))")
            if role == "coordinator":
                arm("recurrence")
                assert "Arm refused" in accept_alert() and count("send") == 1
            proof(role, s)
            print(f"PASS: signed {role}, one supervised delivery, durable GM terminal, no page storage", flush=True)

        role = "coordinator"
        driver.switch_to.window(tabs[role])
        arm("recurrence")
        wait(lambda _: count("send") == 2 and count("reply") == 2)
        s = wait_status(lambda s: s["phase"] == "COOLDOWN" and s["cycle"] == 2)
        assert s["supervisedGitProofStored"] is True
        button("Pause this role").click()
        assert "Pause acknowledged" in accept_alert()
        assert status()["stopped"] and count("send") == 2
        events = fixture.events(driver)
        assert events[2]["at"] - events[1]["at"] >= fixture.COOLDOWN
        print("PASS: signed missing-proof refusal, distinct recurrence, cooldown, acknowledged Pause", flush=True)

        role = "qualitymanager"
        driver.switch_to.window(tabs[role])
        prior = status()["supervisedAttemptId"]
        with fixture.MUTEX:
            fixture.DENY_AT[role] = fixture.COUNTS[role] + 3
        arm("recurrence")
        s = wait_status(lambda s: s["phase"] == "REVIEW" and s["stopped"], 30)
        assert s["cycle"] == 2 and "Final Git policy not proven" in s["pendingReason"]
        failed = s["attemptId"]
        assert failed and failed != prior and count("send") == 1
        with fixture.MUTEX:
            fixture.PAUSED_ROLES.add(role)
        button("Reconcile pre-click REVIEW").click()
        assert "Cancel only a proven pre-click" in accept_alert()
        prompt = wait(EC.alert_is_present(), 20)
        assert "Prior completed OWN invocation" in prompt.text
        prompt.send_keys(json.dumps({"attemptId": prior, "baselineMain": "a"*40}))
        prompt.accept()
        assert "Exact pre-click attempt cancelled" in accept_alert()
        s = status()
        assert s["phase"] == "PAUSED" and s["stopped"] and not s["leased"] and s["attemptId"] is None
        assert s["cycle"] == 2 and s["supervisedAttemptId"] == prior and s["supervisedGitProofStored"]
        assert count("send") == 1
        assert driver.execute_script("return document.querySelector('[role=textbox]').innerText.trim()") == ""
        driver.refresh()
        await_menus()
        assert status()["phase"] == "PAUSED" and status()["cycle"] == 2 and count("send") == 1
        assert not driver.execute_script("return Object.keys(localStorage).some(k=>k.startsWith('CQOP_V1:'))")
        with fixture.MUTEX:
            fixture.PAUSED_ROLES.remove(role)
        print("PASS: signed GM pre-click cancellation, exact draft removed, prior proof retained, no resend after reload", flush=True)

        role = "platformmanager"
        driver.switch_to.window(tabs[role])
        driver.execute_script("localStorage.setItem('CQOP_DELAY:platformmanager','20000')")
        arm("recurrence")
        wait(lambda _: count("send") == 2)
        s = wait_status(lambda s: s["phase"] == "AWAITING_REPLY")
        attempt = s["attemptId"]
        assert attempt
        open_role(role, new=True)
        await_menus()
        s = wait_status(lambda s: s["phase"] == "REVIEW" and s["stopped"], 25)
        assert s["attemptId"] == attempt and count("send") == 2
        driver.refresh()
        await_menus()
        s = wait_status(lambda s: s["phase"] == "REVIEW" and s["stopped"])
        assert s["attemptId"] == attempt and count("send") == 2
        print("PASS: real cross-document GM and bridge lock, duplicate/reload REVIEW, exact attempt preserved, zero replay", flush=True)
        # Destruction of the original document can leave a long lock
        # orphaned. Never release it on timeout or replay its attempt.
        role = "businessmanager"
        driver.switch_to.window(tabs[role])
        driver.execute_script("localStorage.setItem('CQOP_DELAY:businessmanager','20000')")
        arm("recurrence")
        wait(lambda _: count("send") == 2)
        s = wait_status(lambda s: s["phase"] == "AWAITING_REPLY")
        attempt = s["attemptId"]
        driver.refresh()
        await_menus()
        s = wait_status(lambda s: s["stopped"] and s["phase"] in ("AWAITING_REPLY", "REVIEW"), 25)
        assert s["attemptId"] == attempt and count("send") == 2
        time.sleep(1)
        assert count("send") == 2
        print("PASS: same-document in-flight reload preserves original attempt and stops; orphan may need explicit recovery", flush=True)
        print("PASS SIGNED FIXTURE ONLY: actual GM/HTTP/lock/client; synthetic DOM, prompts, Git evidence and clocks. No GO.", flush=True)
    finally:
        if driver:
            with contextlib.suppress(Exception):
                driver.quit()
        server.shutdown()
        server.server_close()
        thread.join(timeout=5)
        print("Disposable signed profile and listeners closed", flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--signed-xpi", type=Path, required=True)
    parser.add_argument("--confirm-file", type=Path, required=True)
    args = parser.parse_args()
    with fixture.lock_fixture_bridge():
        run(args)
