import fcntl
import os
from pathlib import Path
import re
import shlex
import stat
import subprocess


def profile_path():
    return Path.home() / ".profile_override"


def private_file(path):
    path = Path(path)
    info = path.lstat()
    if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValueError("Private file must be regular, owned by this user and mode 0600 or stricter")
    return path


def load_profile(required=()):
    private_file(profile_path())
    inherited = ("HOME", "PATH", "USER", "LOGNAME", "SHELL", "LANG", "LC_ALL",
                 "XDG_RUNTIME_DIR", "XDG_CONFIG_HOME", "XDG_STATE_HOME", "XDG_CACHE_HOME")
    result = subprocess.run(
        ["bash", "--noprofile", "--norc", "-c",
         'set +x; set -a; source "$HOME/.profile_override" >/dev/null 2>&1 || exit 2; exec env -0'],
        env={key: os.environ[key] for key in inherited if key in os.environ},
        stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, check=False,
    )
    if result.returncode:
        raise ValueError("Could not load .profile_override; check its shell syntax locally")
    values = {}
    for entry in result.stdout.split(b"\0"):
        key, separator, value = entry.partition(b"=")
        if separator:
            values[os.fsdecode(key)] = os.fsdecode(value)
    missing = [key for key in required if not values.get(key)]
    if missing:
        raise ValueError("Missing .profile_override variables: " + ", ".join(missing))
    return values


def append_profile(updates):
    if any(not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", key) or not isinstance(value, str)
           or any(c in value for c in ("\0", "\n", "\r")) for key, value in updates.items()):
        raise ValueError("Invalid private environment update")
    path = private_file(profile_path())
    descriptor = os.open(path, os.O_WRONLY | os.O_APPEND | os.O_NOFOLLOW)
    try:
        fcntl.flock(descriptor, fcntl.LOCK_EX)
        info = os.fstat(descriptor)
        if (info.st_dev, info.st_ino) != (path.lstat().st_dev, path.lstat().st_ino):
            raise ValueError("Private environment changed during update")
        values = load_profile()
        if any(values.get(key) and values[key] != value for key, value in updates.items()):
            raise ValueError("Private setting already differs; refusing to overwrite it")
        additions = {key: value for key, value in updates.items() if not values.get(key)}
        if additions:
            body = "\n" + "".join(f"export {key}={shlex.quote(value)}\n" for key, value in additions.items())
            with os.fdopen(os.dup(descriptor), "a") as stream:
                stream.write(body)
                stream.flush()
                os.fsync(stream.fileno())
        return {**values, **updates}
    finally:
        os.close(descriptor)
