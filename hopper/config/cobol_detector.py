"""hopper/config/cobol_detector.py - GnuCOBOL compiler detection"""

import os
import shutil
import subprocess
from dataclasses import dataclass
from typing import Optional


@dataclass
class CobolCompiler:
    name: str
    path: str
    version: Optional[str] = None

    def display_name(self) -> str:
        return f"{self.name} ({self.version})" if self.version else self.name


KNOWN_COMPILERS = [
    ("cobc",     "GnuCOBOL",       ["--version"], lambda out: out.split("\n")[0].strip()),
    ("gnucobol", "GnuCOBOL (alt)", ["--version"], lambda out: out.split("\n")[0].strip()),
]


def _get_version(path, args, parser):
    try:
        r = subprocess.run([path] + args, capture_output=True, text=True, timeout=5)
        out = r.stdout or r.stderr
        return parser(out) if out else None
    except Exception:
        return None


def detect_cobol_compilers() -> list[CobolCompiler]:
    detected, seen = [], set()
    for cmd, display, vargs, vparser in KNOWN_COMPILERS:
        path = shutil.which(cmd)
        if path and path not in seen:
            seen.add(path)
            detected.append(CobolCompiler(display, path, _get_version(path, vargs, vparser)))
    return detected


def get_default_compiler() -> Optional[CobolCompiler]:
    compilers = detect_cobol_compilers()
    return compilers[0] if compilers else None


def is_valid_compiler(path: str) -> bool:
    if not path:
        return False
    if os.path.isabs(path):
        return os.path.isfile(path) and os.access(path, os.X_OK)
    return shutil.which(path) is not None
