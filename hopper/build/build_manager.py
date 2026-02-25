# hopper/build/build_manager.py - Compile + Run workflow for GnuCOBOL

import shlex
from pathlib import Path
from typing import Optional

from PyQt6.QtCore import QObject, QProcess, pyqtSignal

from hopper.config.settings import Settings


class BuildManager(QObject):
    """Orchestrates Save → Compile → Run for COBOL sources via cobc.

    GnuCOBOL command:
        cobc -free -x [-Wall] -o <output> <source>
        cobc -fixed -x [-Wall] -o <output> <source>

    Note: cobc uses standard ``-o path`` (with space), unlike FPC.
    """

    build_started  = pyqtSignal(str)
    build_finished = pyqtSignal(int, str)
    run_started    = pyqtSignal(str)

    def __init__(self, settings: Settings, terminal=None, parent=None):
        super().__init__(parent)
        self.settings = settings
        self.terminal = terminal
        self._process: Optional[QProcess] = None
        self._output_buffer: list[str] = []
        self._pending_run: Optional[str] = None
        self.last_binary: Optional[str] = None

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def compile(self, source_path: str) -> None:
        self._pending_run = None
        self._start_compile(source_path)

    def compile_and_run(self, source_path: str) -> None:
        output_path = self._get_output_path(source_path)
        self._pending_run = output_path
        self._start_compile(source_path)

    def run(self, executable_path: str) -> None:
        if not self.terminal:
            return
        self.run_started.emit(executable_path)
        quoted = shlex.quote(executable_path)
        self.terminal.write(f"{quoted}\n".encode())

    def clean(self, source_path: str) -> None:
        """Delete the binary and any cobc-produced object (if any)."""
        output_path = self._get_output_path(source_path)
        removed = []
        for path in [Path(output_path), Path(source_path).with_suffix(".o")]:
            if path.exists():
                path.unlink(); removed.append(path.name)
        msg = f"\r\n\033[1;33m[HOPPER] Cleaned: {', '.join(removed) if removed else 'nothing to clean.'}\033[0m\r\n"
        if self.terminal:
            self.terminal.on_data_received(msg.encode())

    # ------------------------------------------------------------------
    # Internals
    # ------------------------------------------------------------------

    def _get_output_path(self, source_path: str) -> str:
        stem = Path(source_path).stem
        output_dir = self.settings.get("build", "output_dir") or "."
        source_dir = Path(source_path).parent
        out_dir = Path(output_dir)
        if not out_dir.is_absolute():
            out_dir = source_dir / out_dir
        out_dir.mkdir(parents=True, exist_ok=True)
        return str(out_dir / stem)

    def _build_command(self, source_path: str):
        compiler   = self.settings.get("build", "compiler_path") or "cobc"
        flags_str  = self.settings.get("build", "compiler_flags") or ""
        fmt        = self.settings.get("build", "source_format") or "free"
        flags      = flags_str.split() if flags_str.strip() else []
        output_path = self._get_output_path(source_path)
        # cobc uses standard -o path (with space, unlike FPC's -o<path>)
        args = [f"-{fmt}", "-x"] + flags + ["-o", output_path, source_path]
        return compiler, args, output_path

    def _start_compile(self, source_path: str) -> None:
        if self._process and self._process.state() != QProcess.ProcessState.NotRunning:
            return
        compiler, args, output_path = self._build_command(source_path)
        cmd_str = compiler + " " + " ".join(args)
        self._output_buffer = []
        self._current_output_path = output_path
        if self.terminal:
            self.terminal.on_data_received(
                f"\r\n\033[1;34m[HOPPER] Compiling...\033[0m\r\n$ {cmd_str}\r\n".encode()
            )
        self.build_started.emit(cmd_str)
        self._process = QProcess(self)
        self._process.setProgram(compiler)
        self._process.setArguments(args)
        self._process.readyReadStandardOutput.connect(self._on_stdout)
        self._process.readyReadStandardError.connect(self._on_stderr)
        self._process.finished.connect(self._on_compile_finished)
        self._process.start()

    def _on_stdout(self):
        data = bytes(self._process.readAllStandardOutput())
        self._output_buffer.append(data.decode("utf-8", errors="replace"))
        if self.terminal: self.terminal.on_data_received(data)

    def _on_stderr(self):
        data = bytes(self._process.readAllStandardError())
        self._output_buffer.append(data.decode("utf-8", errors="replace"))
        if self.terminal: self.terminal.on_data_received(data)

    def _on_compile_finished(self, exit_code: int, _exit_status) -> None:
        output_text = "".join(self._output_buffer)
        if exit_code == 0:
            self.last_binary = self._current_output_path
            if self.terminal:
                self.terminal.on_data_received(b"\r\n\033[1;32m[HOPPER] Build successful.\033[0m\r\n")
            pending = self._pending_run; self._pending_run = None
            if pending: self.run(pending)
        else:
            if self.terminal:
                self.terminal.on_data_received(
                    f"\r\n\033[1;31m[HOPPER] Build failed (exit {exit_code}).\033[0m\r\n".encode()
                )
            self._pending_run = None
        self.build_finished.emit(exit_code, output_text)
