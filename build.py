"""
build.py — HOPPER packaging helper
====================================
Cleans previous build artefacts, then calls PyInstaller.

Usage:
    python3 build.py
"""

import importlib.util
import os
import pathlib
import platform
import shutil
import sys

APP_NAME    = "HOPPER"
ENTRY_POINT = "hopper/main.py"
SCRIPT_DIR  = pathlib.Path(__file__).parent.resolve()


def _inject_venv_paths() -> None:
    venv_dir = SCRIPT_DIR / "venv"
    if not venv_dir.exists():
        return
    py_ver = f"python{sys.version_info.major}.{sys.version_info.minor}"
    site_pkgs = venv_dir / "lib" / py_ver / "site-packages"
    if site_pkgs.exists() and str(site_pkgs) not in sys.path:
        sys.path.insert(0, str(site_pkgs))
        print(f"[build] Added venv site-packages to path: {site_pkgs}")


_inject_venv_paths()

import PyInstaller.__main__  # noqa: E402


def _find_module_add_data(module_name: str, sep: str = ":") -> list[str]:
    spec = importlib.util.find_spec(module_name)
    if spec is None:
        print(f"[build] WARNING: '{module_name}' not found — skipping.")
        return []
    if spec.submodule_search_locations:
        pkg_dir = str(pathlib.Path(spec.origin).parent)
        return ["--add-data", f"{pkg_dir}{sep}{module_name}"]
    return ["--add-data", f"{spec.origin}{sep}."]


def clean_build_dirs() -> None:
    print("Cleaning build directories...")
    for d in ["build", "dist"]:
        if os.path.exists(d):
            shutil.rmtree(d)


def get_args() -> list[str]:
    system = platform.system()
    sep    = ";" if system == "Windows" else ":"

    args = [
        "--name",   APP_NAME,
        "--clean",
        "--noconfirm",
        "--windowed",
        "--hidden-import", "PyQt6",
        *_find_module_add_data("pyte",       sep),
        *_find_module_add_data("ptyprocess", sep),
        "--add-data", f"images{sep}images",
        "--add-data", f"examples{sep}examples",
    ]

    if system == "Darwin":
        args += ["--target-architecture", "universal2"]
    elif system == "Windows":
        args += ["--icon", f"images{sep}hopper_icon.png"]

    args.append(ENTRY_POINT)
    return args


def build() -> None:
    clean_build_dirs()
    args = get_args()
    print(f"Building {APP_NAME} for {platform.system()} …")
    print()
    try:
        PyInstaller.__main__.run(args)
        print()
        print(f"Build complete!  →  dist/{APP_NAME}/")
    except Exception as exc:
        print(f"Build failed: {exc}")
        sys.exit(1)


if __name__ == "__main__":
    build()
