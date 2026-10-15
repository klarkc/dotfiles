#!/usr/bin/env python3
"""Stage-only Unix-socket OptMem writer authority.

No command strings, raw shell lines or arbitrary filesystem paths are accepted
from clients. The server's fixed startup configuration owns memory and provenance.
"""
from pathlib import Path
import argparse
import json
import os
import re
import signal
import socket
import stat
import struct
import sys

import optmem_writer_isolated as writer

STOP=False
MAX_REQUEST_BYTES=1024

def stop_requested(signum,frame):
    global STOP
    STOP=True

def safe_server_path(path: Path):
    path=Path(path)
    if not path.is_absolute() or path.exists() or path.is_symlink():
        raise ValueError("Fresh absolute Unix socket path required")
    runtime=Path(f"/run/user/{os.getuid()}")
    if path.parent.parent==runtime:
        # Ephemeral *dedicated* socket dir, never the whole user runtime.
        # Strict systemd sandbox can grant write only to this child.
        if (runtime.is_symlink() or runtime.stat().st_uid!=os.getuid()
                or runtime.stat().st_mode & 0o077):
            raise ValueError("Unsafe user runtime parent")
        directory=path.parent
        if (directory.is_symlink() or directory.stat().st_uid!=os.getuid()
                or directory.stat().st_mode & 0o077):
            raise ValueError("Unsafe dedicated socket directory")
        if not re.fullmatch(r"optmem-stage-writer-[a-z0-9_-]{1,45}",directory.name):
            raise ValueError("Only stage-specific socket directories are allowed")
        if path.name!="writer.sock":
            raise ValueError("Only fixed stage IPC socket names are allowed")
    elif path.resolve().is_relative_to(writer.HERE):
        writer.private_dir(path.parent)
    else:
        raise ValueError("Writer socket must be within private staging or user runtime")
    if len(os.fsencode(str(path)))>104:
        raise ValueError("AF_UNIX path too long; use the user runtime directory")
    return path

def process_request(data: bytes, memory: Path, sidecar: Path, source_state: Path) -> dict:
    if len(data)>MAX_REQUEST_BYTES or not data.endswith(b"\n") or data.count(b"\n")!=1:
        return {"status":"REFUSED","reason":"invalid_request_bounds"}
    try:
        msg=json.loads(data[:-1])
    except (UnicodeError,ValueError,TypeError):
        return {"status":"REFUSED","reason":"invalid_request_format"}
    if not isinstance(msg,dict) or set(msg)!={"operation","feature"}:
        return {"status":"REFUSED","reason":"unsupported_shape"}
    if msg["operation"]!="admit" or not isinstance(msg["feature"],str):
        return {"status":"REFUSED","reason":"unsupported_operation"}
    if not re.fullmatch(r"[a-z0-9_-]+(?: [a-z0-9_-]+)?(?: usage)?",msg["feature"]):
        return {"status":"REFUSED","reason":"invalid_feature"}
    try:
        result=writer.commit_and_acknowledge(memory,sidecar,source_state,msg["feature"])
    except (RuntimeError,ValueError,InterruptedError,OSError):
        return {"status":"DEFERRED","reason":"writer_or_provenance_gate"}
    return {"status":"ACCEPTED","result":result["outcome"],"index":result["index"]}

def main(argv=None):
    cli=argparse.ArgumentParser(description="Staged OptMem private AF_UNIX writer")
    cli.add_argument("--socket",type=Path,required=True)
    cli.add_argument("--memory-dir",type=Path,required=True)
    cli.add_argument("--sidecar",type=Path,required=True)
    cli.add_argument("--source-state",type=Path,required=True)
    args=cli.parse_args(argv)
    os.umask(0o077)
    path=safe_server_path(args.socket)
    mem=writer.private_dir(args.memory_dir)
    side=writer.private_dir(args.sidecar)
    src=writer.private_dir(args.source_state)
    writer.log_path(mem)
    # Preflight invariants before exposing the IPC endpoint.
    writer.verify(mem,side)
    signal.signal(signal.SIGTERM,stop_requested)
    signal.signal(signal.SIGINT,stop_requested)
    srv=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM)
    try:
        srv.bind(str(path))
        os.chmod(path,0o600)
        srv.listen(8)
        srv.settimeout(0.5)
        while not STOP:
            try:conn,_=srv.accept()
            except socket.timeout:continue
            with conn:
                conn.settimeout(7)
                try:
                    raw_peer=conn.getsockopt(socket.SOL_SOCKET,socket.SO_PEERCRED,12)
                    peer_pid,peer_uid,peer_gid=struct.unpack("3i",raw_peer)
                    if peer_uid!=os.getuid():
                        reply={"status":"REFUSED","reason":"peer_identity"}
                    else:
                        blob=b""
                        while not blob.endswith(b"\n") and len(blob)<=MAX_REQUEST_BYTES:
                            chunk=conn.recv(MAX_REQUEST_BYTES+1-len(blob))
                            if not chunk:break
                            blob+=chunk
                        reply=process_request(blob,mem,side,src)
                except (OSError,ValueError):
                    reply={"status":"DEFERRED","reason":"ipc_error"}
                try:
                    conn.sendall((json.dumps(reply,separators=(",",":"))+"\n").encode())
                except OSError:
                    pass
    finally:
        srv.close()
        if path.exists() and stat.S_ISSOCK(path.lstat().st_mode):
            path.unlink()
    return 0

if __name__=="__main__":
    raise SystemExit(main())
