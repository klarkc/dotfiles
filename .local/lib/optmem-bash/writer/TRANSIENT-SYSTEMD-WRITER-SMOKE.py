#!/usr/bin/env python3
"""Run writer IPC as a collected, never-installed user service on synthetic state."""
from pathlib import Path
import json,os,socket,subprocess,sys,time,uuid
HERE=Path(__file__).resolve().parent
uid=os.getuid()
root=Path(f"/run/user/{uid}")
assert root.is_dir() and root.stat().st_uid==uid and not(root.stat().st_mode & 0o077)
cases=sorted((HERE/"synthetic-cases").glob("test_acknowledged_receipt_is_durable_idempotent-*"),
             key=lambda p:p.stat().st_mtime,reverse=True)
assert cases
case=cases[0]
for name in ("memory","provenance","collector"):
 assert (case/name).is_dir()
uid_name=uuid.uuid4().hex[:12]
unit="optmem-writer-sandbox-"+uid_name
socket_dir=root/("optmem-stage-writer-"+uid_name)
socket_dir.mkdir(mode=0o700)
path=socket_dir/"writer.sock"
log=case/("writer-systemd-"+uid_name+".log")
env=os.environ.copy()
env["XDG_RUNTIME_DIR"]=str(root)
env["DBUS_SESSION_BUS_ADDRESS"]="unix:path="+str(root/"bus")
args=["systemd-run","--user","--no-block","--collect","--unit="+unit,
 "--property=Type=exec","--property=ProtectHome=read-only",
 "--property=ProtectSystem=strict","--property=NoNewPrivileges=yes",
 "--property=PrivateTmp=yes","--property=MemoryMax=256M",
 "--property=RestrictAddressFamilies=AF_UNIX",
 "--property=IPAddressDeny=any",
 "--property=ReadWritePaths="+str(case)+" "+str(socket_dir),
 "--property=InaccessiblePaths="+str(Path.home()/".ssh")+" "+str(Path.home()/".gnupg"),
 "--property=WorkingDirectory="+str(HERE),
 "--property=UMask=0077",
 "--property=StandardOutput=append:"+str(log),
 "--property=StandardError=append:"+str(log),
 sys.executable,"-B",str(HERE/"optmem_writer_ipc.py"),
 "--socket",str(path),"--memory-dir",str(case/"memory"),
 "--sidecar",str(case/"provenance"),"--source-state",str(case/"collector")]
try:
 start=subprocess.run(args,capture_output=True,text=True,env=env,timeout=9)
 if start.returncode:raise RuntimeError("Transient systemd writer failed to start")
 for _ in range(60):
  if path.exists():break
  time.sleep(0.1)
 if not path.exists():
  status=subprocess.run(["systemctl","--user","show",unit+".service",
    "-p","Result","-p","ExecMainStatus","-p","ActiveState"],
    env=env,capture_output=True,text=True,timeout=8)
  diag={"main_unit_state":status.stdout.strip().replace("\n",";"),
        "log_tail":log.read_text(errors="replace")[-1600:] if log.exists() else "none"}
  raise RuntimeError("Transient writer socket unavailable: "+json.dumps(diag))
 assert path.stat().st_mode & 0o077==0
 with socket.socket(socket.AF_UNIX,socket.SOCK_STREAM) as client:
  client.settimeout(7)
  client.connect(str(path))
  client.sendall(b'{"operation":"admit","feature":"git status"}\n')
  reply=b""
  while b"\n" not in reply and len(reply)<1000:
   part=client.recv(1000)
   if not part:break
   reply+=part
 parsed=json.loads(reply.strip())
 assert parsed["status"]=="ACCEPTED" and parsed["result"]=="ALREADY_ACKNOWLEDGED"
 assert parsed["index"]==0
 print(json.dumps({"systemd_transient_writer":"PASS","request":"ACKNOWLEDGED_EXISTING",
  "memory_entries_unchanged":(case/"memory/LOG.txt").stat().st_size==320,
  "socket_permissions":"0600","sandbox_protect_home":"read-only",
  "systemd_unit_installed":False,"canonical_store_touched":False}))
finally:
 subprocess.run(["systemctl","--user","stop",unit+".service"],env=env,
  capture_output=True,timeout=9)
 for _ in range(30):
  if not path.exists():break
  time.sleep(0.1)
 if path.exists():
  raise RuntimeError("Transient IPC socket not removed by service stop")
 socket_dir.rmdir()
