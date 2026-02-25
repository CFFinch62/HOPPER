# hopper/terminal/terminal_widget.py - Terminal display widget (pyte + PTY)

import os

import pyte
from PyQt6.QtWidgets import QWidget, QMenu
from PyQt6.QtCore import Qt, QTimer, QPoint
from PyQt6.QtGui import (
    QPainter, QFont, QColor, QFontMetrics, QKeyEvent, QBrush,
    QMouseEvent, QGuiApplication,
)

from hopper.terminal.pty_process import PTYProcess
from hopper.config.settings import Settings


class TerminalWidget(QWidget):
    """PTY-backed terminal panel using pyte for VT100 emulation."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.settings = Settings()
        self.cols = 80
        self.rows = 24
        self.screen = pyte.Screen(self.cols, self.rows)
        self.stream = pyte.Stream(self.screen)
        shell = os.environ.get("SHELL", "/bin/bash")
        self.pty = PTYProcess(command=[shell])
        self.pty.data_received.connect(self.on_data_received)
        self.pty.process_exited.connect(self.on_process_exited)
        self.setup_font()
        self.cursor_visible = True
        self.cursor_timer = QTimer(self)
        self.cursor_timer.timeout.connect(self.toggle_cursor)
        self.cursor_timer.start(500)
        self._sel_start: tuple[int, int] | None = None
        self._sel_end:   tuple[int, int] | None = None
        self._selecting = False
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setAttribute(Qt.WidgetAttribute.WA_InputMethodEnabled, True)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_context_menu)
        self.pty.start()

    def setup_font(self):
        family = self.settings.get("editor", "font_family") or "Monospace"
        size   = self.settings.get("editor", "font_size") or 14
        self.font = QFont(family, size)
        self.font.setStyleHint(QFont.StyleHint.Monospace)
        self.fm = QFontMetrics(self.font)
        self.char_width  = self.fm.horizontalAdvance("W")
        self.char_height = self.fm.height()
        self.resize_terminal()

    def resizeEvent(self, event):
        self.resize_terminal(); super().resizeEvent(event)

    def resize_terminal(self):
        new_cols = max(1, self.width() // self.char_width)
        new_rows = max(1, self.height() // self.char_height)
        if new_cols != self.cols or new_rows != self.rows:
            self.cols, self.rows = new_cols, new_rows
            self.screen.resize(self.rows, self.cols)
            self.pty.resize(self.rows, self.cols)
            self.update()

    def on_data_received(self, data: bytes):
        try:
            self.stream.feed(data.decode("utf-8", errors="replace")); self.update()
        except Exception:
            pass

    def on_process_exited(self, exit_code: int):
        self.stream.feed(f"\r\n[Process exited with code {exit_code}]\r\n"); self.update()

    def write(self, data: bytes):
        self.pty.write(data)

    def keyPressEvent(self, event: QKeyEvent):
        key  = event.key(); text = event.text()
        mods = event.modifiers()
        if key == Qt.Key.Key_Return:    self.write(b"\r")
        elif key == Qt.Key.Key_Backspace: self.write(b"\x7f")
        elif key == Qt.Key.Key_Tab:     self.write(b"\t")
        elif key == Qt.Key.Key_Up:      self.write(b"\x1b[A")
        elif key == Qt.Key.Key_Down:    self.write(b"\x1b[B")
        elif key == Qt.Key.Key_Right:   self.write(b"\x1b[C")
        elif key == Qt.Key.Key_Left:    self.write(b"\x1b[D")
        elif mods & Qt.KeyboardModifier.ControlModifier and key == Qt.Key.Key_C:
            if self._get_sel_range() is not None: self.copy_selection()
            else: self.write(b"\x03")
        elif mods & Qt.KeyboardModifier.ControlModifier and key == Qt.Key.Key_D:
            self.write(b"\x04")
        elif text: self.write(text.encode("utf-8"))

    def _pixel_to_cell(self, pos: QPoint):
        col = max(0, min(self.cols-1, pos.x()//self.char_width))
        row = max(0, min(self.rows-1, pos.y()//self.char_height))
        return (row, col)

    def mousePressEvent(self, event: QMouseEvent):
        self.setFocus(Qt.FocusReason.MouseFocusReason)
        if event.button() == Qt.MouseButton.LeftButton:
            cell = self._pixel_to_cell(event.pos())
            self._sel_start = self._sel_end = cell; self._selecting = True; self.update()
        event.accept()

    def mouseMoveEvent(self, event: QMouseEvent):
        if self._selecting and event.buttons() & Qt.MouseButton.LeftButton:
            self._sel_end = self._pixel_to_cell(event.pos()); self.update()

    def mouseReleaseEvent(self, event: QMouseEvent):
        if event.button() == Qt.MouseButton.LeftButton:
            self._selecting = False
            if self._sel_start == self._sel_end:
                self._sel_start = self._sel_end = None
            self.update()

    def _get_sel_range(self):
        if self._sel_start is None or self._sel_end is None: return None
        s, e = self._sel_start, self._sel_end
        if (s[0], s[1]) > (e[0], e[1]): s, e = e, s
        return s, e

    def _cell_in_selection(self, row, col):
        sel = self._get_sel_range()
        if sel is None: return False
        (sr, sc), (er, ec) = sel
        if row < sr or row > er: return False
        if row == sr and col < sc: return False
        if row == er and col > ec: return False
        return True

    def copy_selection(self):
        sel = self._get_sel_range()
        if sel is None: return
        (sr, sc), (er, ec) = sel
        lines = []
        for row in range(sr, er+1):
            row_chars = self.screen.buffer[row]
            c0 = sc if row == sr else 0
            c1 = ec if row == er else self.cols-1
            lines.append("".join(row_chars[c].data for c in range(c0, c1+1)).rstrip())
        text = "\n".join(lines)
        if text: QGuiApplication.clipboard().setText(text)

    def _show_context_menu(self, point: QPoint):
        menu = QMenu(self)
        copy_act = menu.addAction("Copy")
        copy_act.setEnabled(self._get_sel_range() is not None)
        copy_act.triggered.connect(self.copy_selection)
        menu.exec(self.mapToGlobal(point))

    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setFont(self.font)
        bg = QColor("#0f0f0f"); fg = QColor("#cccccc")
        sel_color = QColor(0, 120, 200, 120)
        painter.fillRect(self.rect(), bg)
        for y in range(self.rows):
            row_chars = self.screen.buffer[y]
            for x in range(self.cols):
                char_data = row_chars[x]
                if self._cell_in_selection(y, x):
                    painter.fillRect(x*self.char_width, y*self.char_height,
                                     self.char_width, self.char_height, sel_color)
                painter.setPen(fg)
                painter.drawText(x*self.char_width, y*self.char_height+self.fm.ascent(), char_data.data)
        if self.cursor_visible and self.pty.running:
            cx, cy = self.screen.cursor.x, self.screen.cursor.y
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(QColor(200, 200, 200, 100)))
            painter.drawRect(cx*self.char_width, cy*self.char_height, self.char_width, self.char_height)

    def toggle_cursor(self):
        self.cursor_visible = not self.cursor_visible
        cx, cy = self.screen.cursor.x, self.screen.cursor.y
        self.update(cx*self.char_width, cy*self.char_height, self.char_width, self.char_height)

    def restart(self):
        self.pty.terminate_process(); self.pty.wait()
        self.screen.reset(); self._sel_start = self._sel_end = None
        shell = os.environ.get("SHELL", "/bin/bash")
        self.pty = PTYProcess(command=[shell])
        self.pty.data_received.connect(self.on_data_received)
        self.pty.process_exited.connect(self.on_process_exited)
        self.pty.start(); self.pty.resize(self.rows, self.cols); self.update()

    def clear(self):
        self.screen.reset(); self._sel_start = self._sel_end = None; self.update()

    def interrupt(self): self.write(b"\x03")

    def focusInEvent(self, event):
        self.cursor_visible = True; self.update(); super().focusInEvent(event)
