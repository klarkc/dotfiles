#!/usr/bin/env python3
"""Real isolated Firefox, synthetic four-role DOM and HTTP evidence only.

GM APIs are fixture shims (not a signed extension). No live account or broker
agent is visited, no operational installation or authorization is implied.
"""
from __future__ import annotations

import contextlib
import hashlib
import http.server
import json
import re
import secrets
import shutil
import subprocess
import threading
import time
from pathlib import Path
from urllib.parse import urlparse

from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.support.ui import WebDriverWait

ROOT = Path(__file__).resolve().parents[1] / "firefox"
ROLES = ("coordinator", "platformmanager", "businessmanager", "qualitymanager")
NAMES = ("Coordinator", "PlatformManager", "BusinessManager", "QualityManager")
PROMPTS = {
    role: f"ONE tick ONLY as permanent {name} (agent_id={role}) in THIS SAME chat; require {role.upper()}_BOOTSTRAP_READY. No lanes.\n\nFollow NON_BLOCKING_HEARTBEATS_V1 + NO_DEADLOCK_V1.\n\nReport exact refs; STOP. ChatQueue triggers the next tick."
    for role, name in zip(ROLES, NAMES)
}
PROMPT_DOC = "\n".join(
    f"## {name}\n\n\x60\x60\x60text\n{PROMPTS[role]}\n\x60\x60\x60\n"
    for role, name in zip(ROLES, NAMES)
)
PROMPT_BLOB = hashlib.sha1(
    f"blob {len(PROMPT_DOC.encode())}\0".encode() + PROMPT_DOC.encode()
).hexdigest()
TOKEN = secrets.token_hex(32)
COOLDOWN = 3000
COUNTS = dict.fromkeys(ROLES, 0)
DENY_AT = {}
PAUSED_ROLES = set()
PROOFS = {}  # Synthetic externally supplied proof; never infer it from DOM completion.
MUTEX = threading.Lock()
LOCK_PORT = None


@contextlib.contextmanager
def lock_fixture_bridge():
    """Actual Node lock endpoint; Python only proxies fixture same-origin POSTs."""
    global LOCK_PORT
    assert LOCK_PORT is None
    node = shutil.which("node")
    assert node
    source = """
import {startPolicyBridge} from './chatqueue-policy-bridge.mjs';
import {makeBridgeLockTable} from './chatqueue-bridge-locks.mjs';
const chunks=[];
for await(const c of process.stdin){chunks.push(c);}
const capability=Buffer.concat(chunks).toString('utf8');
const bridge=await startPolicyBridge({capability,readPolicy:()=>{throw Error("No real Git in fixture")},lockTable:makeBridgeLockTable()});
console.log(bridge.port);
process.once("SIGTERM",async()=>{const clean=await bridge.stop();process.exitCode=clean?0:2;});
"""
    proc = subprocess.Popen(
        [node, "--input-type=module", "-e", source], cwd=ROOT,
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        text=True,
    )
    try:
        proc.stdin.write(TOKEN)
        proc.stdin.close()
        import select
        assert select.select([proc.stdout], [], [], 15)[0], "Lock fixture startup timeout"
        LOCK_PORT = int(proc.stdout.readline().strip())
        assert 1024 <= LOCK_PORT <= 65535
        yield
    finally:
        LOCK_PORT = None
        if proc.poll() is None:
            proc.terminate()
            try:
                proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait(timeout=5)
        # A failed test may deliberately leave an orphan; do not mistake
        # fixture process cleanup for safe operational generation recovery.
        proc.stdout.close()
        proc.stderr.close()

PAGE = """
<!doctype html><html><head><meta charset="utf-8"><title>Operational synthetic fixture</title>
<style>[contenteditable]{min-height:32px;border:1px solid #999;white-space:pre-wrap}article{padding:4px}</style></head><body>
<div id="messages"><article data-virtualized-turn>
<div data-user-message-bubble>Manual synthetic bootstrap.</div>
<div class="group flex flex-col"><h4 data-conversation-role="assistant">Assistant</h4>
<p id="marker"></p><button type="button" aria-label="Copy">Copy</button></div></article></div>
<form data-chatgpt-composer hidden><div contenteditable="true" role="textbox"></div></form>
<form id="composer" data-chatgpt-composer><div contenteditable="true" role="textbox"></div>
<button type="button" id="model">6 Extra alto</button></form>
<button id="stop" type="button" data-testid="stop-button" style="display:none">Stop</button>
<script>
(() => {
 const role=location.pathname.split("/").at(-1).replace("example-","").replace("-successor","");
 document.getElementById("marker").textContent=role.toUpperCase()+"_BOOTSTRAP_READY";
 const messages=document.getElementById("messages"),form=document.getElementById("composer"),box=form.querySelector("[contenteditable]"),stop=document.getElementById("stop");
 const send=document.createElement("button");send.type="button";send.dataset.testid="send-button";send.textContent="Send";
 send.addEventListener("click",()=>form.requestSubmit());
 const toggle=()=>{if(box.textContent.trim()){if(!send.isConnected)form.append(send)}else send.remove()};
 box.addEventListener("input",toggle);
 // The real long-prompt DOM includes a distinct text leaf, an aria-hidden
 // ellipsis and localized expansion control. Hidden reasoning headings and
 // the visible final heading share one conversation turn group.
 const expandedUser=(body)=>{
  const bubble=document.createElement("div");bubble.setAttribute("data-user-message-bubble","");
  const wrapper=document.createElement("div"),content=document.createElement("div");
  content.className="whitespace-pre-wrap";content.textContent=body;
  const ellipsis=document.createElement("span");ellipsis.setAttribute("aria-hidden","true");ellipsis.textContent="…";
  const expand=document.createElement("button");expand.type="button";expand.textContent="Mostrar mais";
  wrapper.append(content,ellipsis,expand);bubble.append(wrapper);return bubble;
 };
 const appendReply=(group,body,hiddenCount)=>{
  for(let i=0;i<hiddenCount;i++){const wrapper=document.createElement("div");wrapper.hidden=true;
   const h=document.createElement("h4");h.dataset.conversationRole="assistant";h.textContent="Reasoning";
   wrapper.append(h);group.append(wrapper);}
  const final=document.createElement("div"),heading=document.createElement("h4"),reply=document.createElement("div");
  heading.dataset.conversationRole="assistant";heading.textContent="ChatGPT disse:";reply.textContent=body;final.append(heading,reply);
  const actions=document.createElement("div"),copy=document.createElement("button");copy.type="button";copy.setAttribute("aria-label","Copiar");copy.textContent="Copiar";actions.append(copy);
  group.append(final,actions);
 };
 const key="CQOP_FIXTURE_EVENTS:"+role;
 const events=()=>JSON.parse(localStorage.getItem(key)||"[]");
 const record=(type,body)=>localStorage.setItem(key,JSON.stringify([...events(),{type,body,at:Date.now()}]));
 window.__opFixture={events,expandedUser,appendReply};
 form.addEventListener("submit",e=>{
  e.preventDefault();if(stop.style.display!=="none")return;
  const body=box.innerText.trim();
  if(!body.startsWith("ONE tick ONLY")||!body.includes("agent_id="+role)||!body.includes("Browser delivery correlation only"))return;
  const turn=document.createElement("article");turn.setAttribute("data-virtualized-turn","");
  const group=document.createElement("div");group.className="group flex flex-col";
  group.append(expandedUser(body));turn.append(group);messages.append(turn);
  box.textContent="";toggle();stop.style.display="block";record("send",body);
  setTimeout(()=>{
   appendReply(group,"Synthetic terminal result",3);
   stop.style.display="none";record("reply","Synthetic terminal result");
  },Number(localStorage.getItem("CQOP_DELAY:"+role)||800));
 });
})();
</script></body></html>
"""


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if urlparse(self.path).path not in [
            f"/g/example-project/c/example-{r}" for r in ROLES
        ] + ["/g/example-project/c/example-coordinator-successor"]:
            self.send_error(404)
            return
        self.respond(200, PAGE.encode(), "text/html; charset=utf-8")

    def do_POST(self):
        if self.path not in ("/v1/evidence", "/v1/lock") or self.headers.get("Authorization") != "Bearer " + TOKEN:
            self.send_error(403)
            return
        try:
            length = int(self.headers["Content-Length"])
            assert 0 < length <= 384
            value = json.loads(self.rfile.read(length))
            role, nonce = value["agent"], value["nonce"]
            assert role in ROLES and re.fullmatch(r"[0-9a-f]{32,64}", nonce)
            if self.path == "/v1/lock":
                assert LOCK_PORT is not None
                from urllib.request import Request, urlopen
                from urllib.error import HTTPError
                req = Request(
                    f"http://127.0.0.1:{LOCK_PORT}/v1/lock",
                    data=json.dumps(value).encode(),
                    headers={"Authorization": "Bearer " + TOKEN, "Content-Type": "application/json"},
                    method="POST",
                )
                try:
                    with urlopen(req, timeout=5) as response:
                        self.respond(response.status, response.read(2048), "application/json")
                except HTTPError as exc:
                    self.respond(exc.code, exc.read(2048), "application/json")
                return
            query = value.get("pulseQuery")
            if query is not None:
                assert set(query) in ({"attemptId", "baselineMain"}, {"attemptId", "baselineMain", "terminalCommit"})
                assert re.fullmatch(r"cqop-[0-9a-f-]{36}", query["attemptId"])
                assert re.fullmatch(r"[0-9a-f]{40}", query["baselineMain"])

        except Exception:
            self.send_error(400)
            return
        with MUTEX:
            COUNTS[role] += 1
            paused = role in PAUSED_ROLES
            denied = paused or COUNTS[role] >= DENY_AT.get(role, 10**9)
            proof = PROOFS.get(role) if query else None
            if proof and (
                proof["attemptId"] != query["attemptId"]
                or proof["baselineMain"] != query["baselineMain"]
                or ("terminalCommit" in query and proof["terminalCommit"] != query["terminalCommit"])
            ):
                proof = None
        now = int(time.time() * 1000)
        result = {
            "agent": role,
            "nonce": nonce,
            "status": "NO_GO" if denied else "GIT_POLICY_PASS_ONLY",
            "runtimeAuthorized": False,
            "reasons": (["ROLE_PAUSED", "PRODUCTION_NO_GO"] if paused
                        else ["PRODUCTION_NO_GO"] if denied else []),
            "startedAt": now,
            "observedAt": now,
            "promptBlob": PROMPT_BLOB,
            "promptDigest": hashlib.sha256(PROMPTS[role].encode()).hexdigest(),
            "heads": {"main": "a" * 40, "runtime": "b" * 40},
            "blobs": {
                p: "c" * 40
                for p in (
                    "coordination/organization.json",
                    "scheduling/state.json",
                    "scheduling/watchdog-policy.json",
                    "scheduling/runtime.json",
                )
            },
        }
        if proof is not None:
            result["pulseProof"] = proof
        self.respond(200, json.dumps(result).encode(), "application/json")

    def respond(self, code, body, content_type):
        self.send_response(code)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *_):
        pass


SHIM = """
window.__opTest={menus:[],alerts:[],confirm:true};
window.GM={getValue:async(key,fallback)=>{const raw=localStorage.getItem(key);return raw===null?fallback:JSON.parse(raw)},setValue:async(key,value)=>localStorage.setItem(key,JSON.stringify(value))};
window.GM_registerMenuCommand=(name,callback)=>window.__opTest.menus.push({name,callback});
window.GM_xmlhttpRequest=opts=>fetch(opts.url,{method:opts.method,headers:opts.headers,body:opts.data,redirect:"error",credentials:"omit"})
 .then(async response=>opts.onload({status:response.status,finalUrl:response.url,responseText:await response.text()})).catch(opts.onerror);
window.confirm=()=>window.__opTest.confirm;
window.prompt=()=>window.__opTest.prompt??null;
window.alert=v=>window.__opTest.alerts.push(String(v));
window.eval(arguments[0]);
"""


def render_bundle(port):
    subprocess.run(["node", str(ROOT / "build-chatqueue-operational.mjs"), "--check"], check=True)
    source = (ROOT / "chatqueue-operational.user.js").read_text()
    replacements = {
        'const origin = "https://chatgpt.com";': f'const origin = "http://127.0.0.1:{port}";',
        "const REPLY_COOLDOWN_MS = 360_000;": f"const REPLY_COOLDOWN_MS = {COOLDOWN};",
        "Date.now() - stable >= 6000": "Date.now() - stable >= 1000",
        "const deadline = Date.now() + 2 * 60 * 60 * 1000;": "const deadline = Date.now() + 15000;",
    }
    # Formatting is intentionally not part of the test's semantics; use exact
    # source replacements except whitespace for the two elapsed-time bounds.
    replacements = {k: v for k, v in replacements.items() if "stable" not in k and "deadline" not in k}
    for old, new in replacements.items():
        assert source.count(old) == 1, old
        source = source.replace(old, new)
    for pattern, value in [
        (r"Date\.now\(\)\s*-\s*stable\s*>=\s*6000", "Date.now() - stable >= 1000"),
        (r"const deadline\s*=\s*Date\.now\(\)\s*\+\s*2\s*\*\s*60\s*\*\s*60\s*\*\s*1000;", "const deadline = Date.now() + 15000;"),
    ]:
        source, n = re.subn(pattern, value, source)
        assert n == 1, pattern
    return source


def state(driver, role):
    return driver.execute_script(
        "return JSON.parse(localStorage.getItem('CQOP_V1:'+arguments[0]+':STATE')||'null')?.state",
        role,
    )


def stopped(driver, role):
    return driver.execute_script(
        "return JSON.parse(localStorage.getItem('CQOP_V1:'+arguments[0]+':GATE')||'null')?.stopped===true",
        role,
    )


def events(driver):
    return driver.execute_script("return window.__opFixture.events()")


def count(driver, kind):
    return sum(e["type"] == kind for e in events(driver))


def menu(driver, name):
    driver.execute_script(
        "const m=window.__opTest.menus.find(x=>x.name.includes(arguments[0]));if(!m)throw Error('Menu absent');m.callback();",
        name,
    )


def boot(driver, bundle, role, config=None, expected_path=None):
    u = urlparse(driver.current_url)
    assert u.scheme == "http" and u.hostname == "127.0.0.1"
    assert u.path == (expected_path or f"/g/example-project/c/example-{role}")
    if config is not None:
        driver.execute_script("localStorage.setItem('CQOP_V1:CONFIG',JSON.stringify(arguments[0]))", config)
    driver.execute_script(SHIM, bundle)
    WebDriverWait(driver, 10).until(
        lambda d: d.execute_script("return window.__opTest?.menus?.length===8") and stopped(d, role)
    )


def check_receipt_dom(driver, binding):
    """Only the read-only source prefix is evaluated in the isolated fixture."""
    src = (ROOT / "chatqueue-operational-browser.js").read_text()
    pure = src[src.index("\tconst visible ="):src.index("\tconst dom = {")]
    assert ".click(" not in pure and "GM." not in pure and "execCommand" not in pure
    checks = r"""
      const binding=arguments[0],origin=location.origin,path=location.pathname;
    """ + pure + r"""
      const marker=document.getElementById("marker"),saved=marker.textContent;
      const token=binding.agent.toUpperCase()+"_BOOTSTRAP_READY",prompt="Synthetic exact prompt.\n"+token;
      const messages=document.getElementById("messages"),turn=document.createElement("article");
      const group=document.createElement("div");group.className="group flex flex-col";turn.setAttribute("data-virtualized-turn","");
      const bubble=window.__opFixture.expandedUser(prompt),wrapper=bubble.firstElementChild;
      group.append(bubble);window.__opFixture.appendReply(group,"Synthetic terminal result",3);
      turn.append(group);messages.append(turn);marker.textContent="No bootstrap";
      try {
        const result={};
        result.exact=receiptMatches(prompt).length===1&&!!terminalSnapshot(binding,prompt);
        result.userMarkerDenied=!ready(binding,"",true);
        const reply=group.querySelectorAll('[data-conversation-role="assistant"]')[3].nextElementSibling;
        reply.textContent=token;
        result.assistantMarkerAccepted=ready(binding,"",true);
        reply.textContent="Synthetic terminal result";
        const extra=document.createElement("span");extra.textContent="Unexpected extra text";wrapper.append(extra);
        result.extraTextDenied=receiptMatches(prompt).length===0;
        extra.remove();
        const extraLeaf=document.createElement("div");extraLeaf.className="whitespace-pre-wrap";extraLeaf.textContent=prompt;wrapper.append(extraLeaf);
        result.ambiguousBodyDenied=receiptMatches(prompt).length===0;
        extraLeaf.remove();
        const unknownControl=document.createElement("button");wrapper.append(unknownControl);
        result.unknownControlDenied=receiptMatches(prompt).length===0;unknownControl.remove();
        const media=document.createElement("img");wrapper.append(media);
        result.extraMediaDenied=receiptMatches(prompt).length===0;media.remove();
        const extraHeading=group.querySelector('[data-conversation-role="assistant"]');
        extraHeading.parentElement.hidden=false;
        result.multipleVisibleDenied=!terminalSnapshot(binding,prompt);
        extraHeading.parentElement.hidden=true;
        const other=document.createElement("div");other.className="group flex flex-col";
        window.__opFixture.appendReply(other,"Unrelated response",0);turn.append(other);
        result.otherGroupDenied=!terminalSnapshot(binding,prompt);
        other.remove();
        const copy=group.querySelector('button[aria-label="Copiar"]'),actions=copy.parentElement;
        reply.append(copy);
        result.copyInsideReplyDenied=!terminalSnapshot(binding,prompt);
        actions.append(copy);
        const duplicateCopy=copy.cloneNode(true);actions.append(duplicateCopy);
        result.duplicateCopyDenied=!terminalSnapshot(binding,prompt);duplicateCopy.remove();
        const extraBubble=turn.cloneNode(true);messages.append(extraBubble);
        let duplicateDenied=false;try{terminalSnapshot(binding,prompt)}catch{duplicateDenied=true;}
        result.duplicateTurnDenied=duplicateDenied;extraBubble.remove();
        group.append(bubble);
        result.reorderedUserDenied=!terminalSnapshot(binding,prompt);
        group.prepend(bubble);
        result.exactRestored=!!terminalSnapshot(binding,prompt);
        return result;
      } finally {turn.remove();marker.textContent=saved;}
    """
    result = driver.execute_script(checks, binding)
    assert result and all(result.values()), result
    print("PASS: exact expanded user body + one visible final heading; reject extra text, ambiguity, user bootstrap, unrelated/duplicate copy or turn", flush=True)


def main():
    if not shutil.which("geckodriver"):
        raise SystemExit("Use pinned Nix test target")
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    port = server.server_address[1]
    bundle = render_bundle(port)
    cfg = {
        "version": 1,
        "reviewed": False,
        "promptBlob": PROMPT_BLOB,
        "bridge": {"port": port, "capability": TOKEN},
        "bindings": [
            {
                "agent": r,
                "path": f"/g/example-project/c/example-{r}",
                "modelLabel": "6 Extra alto",
                "prompt": PROMPTS[r],
            }
            for r in ROLES
        ],
    }
    options = webdriver.FirefoxOptions()
    options.binary_location = "/usr/bin/firefox"
    options.add_argument("-headless")
    for pref, val in {
        "app.update.enabled": False,
        "browser.shell.checkDefaultBrowser": False,
        "datareporting.healthreport.uploadEnabled": False,
        "toolkit.telemetry.enabled": False,
        "browser.startup.homepage": "about:blank",
    }.items():
        options.set_preference(pref, val)
    driver = None
    try:
        driver = webdriver.Firefox(options=options, service=Service(executable_path=shutil.which("geckodriver")))
        driver.set_page_load_timeout(20)
        tabs = {}
        for index, role in enumerate(ROLES):
            if index:
                driver.switch_to.new_window("tab")
            driver.get(f"http://127.0.0.1:{port}/g/example-project/c/example-{role}")
            boot(driver, bundle, role, cfg if index == 0 else None)
            tabs[role] = driver.current_window_handle
            assert count(driver, "send") == 0
        check_receipt_dom(driver, cfg["bindings"][-1])
        menu(driver, "Arm supervised")
        WebDriverWait(driver, 8).until(lambda d: d.execute_script("return window.__opTest.alerts.some(a=>a.includes('not approved'))"))
        assert count(driver, "send") == 0
        # Retire the old pristine document, provision a different exact route,
        # and migrate only its witnessed zero-cycle state while Git is closed.
        role = ROLES[0]
        driver.switch_to.window(tabs[role])
        previous = state(driver, role)
        assert previous["cycle"] == 0 and previous["phase"] == "PAUSED"
        successor = "/g/example-project/c/example-coordinator-successor"
        cfg["bindings"][0]["path"] = successor
        with MUTEX:
            PAUSED_ROLES.add(role)
        driver.get(f"http://127.0.0.1:{port}" + successor)
        boot(driver, bundle, role, cfg, expected_path=successor)
        menu(driver, "Migrate pristine paused binding")
        WebDriverWait(driver, 12).until(lambda d: d.execute_script(
            "return window.__opTest.alerts.some(a=>a.includes('Pristine binding migrated'))"
        ))
        envelope = driver.execute_script(
            "return JSON.parse(localStorage.getItem('CQOP_V1:coordinator:STATE'))"
        )
        assert envelope == {"path": successor, "state": previous}
        assert count(driver, "send") == 0 and stopped(driver, role)
        driver.refresh()
        boot(driver, bundle, role, expected_path=successor)
        with MUTEX:
            PAUSED_ROLES.remove(role)
        print("PASS: pristine stopped rollover under witnessed locks, no send", flush=True)
        cfg["reviewed"] = True
        driver.execute_script("localStorage.setItem('CQOP_V1:CONFIG',JSON.stringify(arguments[0]))", cfg)
        for role in ROLES:
            driver.switch_to.window(tabs[role])
            if role in ("platformmanager", "qualitymanager"):
                driver.execute_script("localStorage.setItem('CQOP_DELAY:'+arguments[0],'4500')", role)
            menu(driver, "Arm supervised")
            WebDriverWait(driver, 15).until(lambda d: count(d, "send") == 1)
            if role == "platformmanager":
                WebDriverWait(driver, 8).until(lambda d: state(d, role)["phase"] == "AWAITING_REPLY")
                driver.execute_script("""
                    const m=document.getElementById('messages'),t=m.lastElementChild;
                    t.remove();setTimeout(()=>m.append(t),1600);
                """)
            if role == "qualitymanager":
                # An explicit duplicate is not transient absence: it must
                # stop the attempt. Restore the exact original receipt, then
                # reconcile with synthetic independently supplied Git proof.
                WebDriverWait(driver, 8).until(lambda d: state(d, role)["phase"] == "AWAITING_REPLY")
                driver.execute_script("""
                    const m=document.getElementById('messages');
                    const duplicate=m.lastElementChild.cloneNode(true);
                    duplicate.id='fixture-duplicate';m.append(duplicate);
                """)
                WebDriverWait(driver, 12).until(lambda d: state(d, role)["phase"] == "REVIEW" and stopped(d, role))
                original = state(driver, role)
                driver.execute_script("document.getElementById('fixture-duplicate').remove()")
                WebDriverWait(driver, 12).until(lambda d: count(d, "reply") == 1)
                journal = driver.execute_script(
                    "return JSON.parse(localStorage.getItem('CQOP_V1:qualitymanager:SUPERVISED'))"
                )
                with MUTEX:
                    PAUSED_ROLES.add(role)
                    PROOFS[role] = {
                        "status": "OWN_GIT_PAIR_VERIFIED", "agent": role,
                        "attemptId": journal["attemptId"], "baselineMain": journal["baselineMain"],
                        "main": "a" * 40, "startedCommit": "1" * 40, "startedBlob": "2" * 40,
                        "terminalCommit": "3" * 40, "terminalBlob": "4" * 40,
                        "runId": "synthetic-run", "operationId": "synthetic-operation",
                        "terminalStatus": "PROGRESS", "providerEffectsVerified": False,
                    }
                menu(driver, "Reconcile supervised REVIEW")
                WebDriverWait(driver, 18).until(lambda d: state(d, role)["phase"] == "PAUSED" and stopped(d, role))
                saved = driver.execute_script(
                    "return JSON.parse(localStorage.getItem('CQOP_V1:qualitymanager:SUPERVISED'))"
                )
                assert saved["reconciledReview"]["previousState"] == original
                assert saved["attemptId"] == original["attemptId"]
                assert saved["gitProof"]["providerEffectsVerified"] is False
                assert count(driver, "send") == 1
                with MUTEX:
                    PAUSED_ROLES.remove(role)
                driver.execute_script("localStorage.removeItem('CQOP_DELAY:'+arguments[0])", role)
                print("PASS: duplicate stops exact attempt; paused native-menu reconciliation preserves REVIEW evidence, no resend", flush=True)
            else:
                WebDriverWait(driver, 15).until(lambda d: state(d, role)["phase"] == "COOLDOWN" and stopped(d, role))
            if role == "platformmanager":
                driver.execute_script("localStorage.removeItem('CQOP_DELAY:'+arguments[0])", role)
                print("PASS: virtualized receipt temporarily absent during generation, exact terminal recovered", flush=True)
            assert count(driver, "reply") == 1
            assert events(driver)[0]["body"].startswith(PROMPTS[role])
            assert COUNTS[role] == (4 if role in ("coordinator", "qualitymanager") else 3)
            print(f"PASS: {role} one supervised send, terminal, expected policy reads, STOP", flush=True)

        role = ROLES[0]
        driver.switch_to.window(tabs[role])
        # A completed supervised browser reply alone must not start recurrence.
        menu(driver, "Arm recurrence")
        WebDriverWait(driver, 8).until(
            lambda d: d.execute_script("return window.__opTest.alerts.some(a=>a.includes('Arm refused'))")
        )
        assert stopped(driver, role) and count(driver, "send") == 1
        print("PASS: recurrence refused without independently supplied synthetic Git evidence", flush=True)
        for r in ROLES:
            driver.switch_to.window(tabs[r])
            journal = driver.execute_script(
                "return JSON.parse(localStorage.getItem('CQOP_V1:'+arguments[0]+':SUPERVISED'))",
                r,
            )
            assert journal["phase"] == "REPLY_TERMINAL"
            assert journal["baselineMain"] == "a" * 40
            assert journal["attemptId"] in events(driver)[0]["body"]
            with MUTEX:
                PROOFS[r] = {
                    "status": "OWN_GIT_PAIR_VERIFIED", "agent": r,
                    "attemptId": journal["attemptId"], "baselineMain": journal["baselineMain"],
                    "main": "a" * 40, "startedCommit": "1" * 40, "startedBlob": "2" * 40,
                    "terminalCommit": "3" * 40, "terminalBlob": "4" * 40,
                    "runId": "synthetic-run", "operationId": "synthetic-operation",
                    "terminalStatus": "PROGRESS", "providerEffectsVerified": False,
                }
        driver.switch_to.window(tabs[role])
        menu(driver, "Arm recurrence")
        WebDriverWait(driver, 18).until(lambda d: count(d, "send") == 2 and count(d, "reply") == 2 and state(d, role)["phase"] == "COOLDOWN")
        menu(driver, "Pause this role")
        WebDriverWait(driver, 10).until(lambda d: stopped(d, role))
        ev = events(driver)
        assert ev[2]["at"] - ev[1]["at"] >= COOLDOWN
        assert count(driver, "send") == 2
        assert ev[0]["body"] != ev[2]["body"]
        print("PASS: recurrence after independent synthetic Git-pair proof, exact distinct correlation and cooldown", flush=True)

        role = ROLES[1]
        driver.switch_to.window(tabs[role])
        with MUTEX:
            DENY_AT[role] = COUNTS[role] + 3
        menu(driver, "Arm recurrence")
        WebDriverWait(driver, 18).until(lambda d: state(d, role)["phase"] == "REVIEW" and stopped(d, role))
        original = state(driver, role)
        assert original["attemptId"] and count(driver, "send") == 1
        journal = driver.execute_script(
            "return JSON.parse(localStorage.getItem('CQOP_V1:'+arguments[0]+':SUPERVISED'))", role)
        query = {"attemptId": journal["attemptId"], "baselineMain": journal["baselineMain"]}
        driver.execute_script("window.__opTest.prompt=JSON.stringify(arguments[0])", query)
        with MUTEX:
            PAUSED_ROLES.add(role)
        menu(driver, "Reconcile pre-click REVIEW")
        WebDriverWait(driver, 18).until(lambda d: state(d, role)["phase"] == "PAUSED" and stopped(d, role))
        cancelled = state(driver, role)
        assert cancelled["preclickReviews"][0]["previousState"] == original
        assert cancelled["preclickReviews"][0]["phase"] == "CANCELLED_BEFORE_CLICK"
        assert cancelled["attemptId"] is None and count(driver, "send") == 1
        assert driver.execute_script("return document.querySelector('[role=textbox]').innerText.trim()")== ""
        driver.refresh()
        boot(driver, bundle, role)
        assert state(driver, role) == cancelled and count(driver, "send") == 1
        with MUTEX:
            PAUSED_ROLES.remove(role)
        print("PASS: pre-click denial, exact no-send draft cancellation and preserved REVIEW evidence after reload", flush=True)

        role = ROLES[2]
        driver.switch_to.window(tabs[role])
        driver.execute_script("localStorage.setItem('CQOP_DELAY:'+arguments[0],'20000')", role)
        menu(driver, "Arm recurrence")
        WebDriverWait(driver, 15).until(lambda d: count(d, "send") == 2 and state(d, role)["phase"] == "AWAITING_REPLY")
        original = state(driver, role)["attemptId"]
        driver.switch_to.new_window("tab")
        driver.get(f"http://127.0.0.1:{port}/g/example-project/c/example-{role}")
        boot(driver, bundle, role)
        WebDriverWait(driver, 12).until(lambda d: state(d, role)["phase"] == "REVIEW")
        assert state(driver, role)["attemptId"] == original and count(driver, "send") == 2
        print("PASS: same-role duplicate revokes lease, exact attempt preserved, no replay", flush=True)
        print("PASS: isolated Firefox operational integration; synthetic HTTP and GM shims only", flush=True)
        return 0
    finally:
        if driver:
            with contextlib.suppress(Exception):
                driver.quit()
        server.shutdown()
        server.server_close()
        thread.join(timeout=5)


if __name__ == "__main__":
    with lock_fixture_bridge():
        raise SystemExit(main())
