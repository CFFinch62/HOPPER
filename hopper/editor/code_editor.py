# hopper/editor/code_editor.py - Code editor widget with line numbers

from __future__ import annotations

from PyQt6.QtWidgets import QPlainTextEdit, QWidget, QTextEdit
from PyQt6.QtCore import Qt, QRect, QSize
from PyQt6.QtGui import QColor, QPainter, QTextFormat, QFont, QKeyEvent

from hopper.editor.highlighter import CobolHighlighter
from hopper.config.settings import Settings
from hopper.config.themes import Theme, get_theme


class LineNumberArea(QWidget):
    def __init__(self, editor):
        super().__init__(editor)
        self.codeEditor = editor

    def sizeHint(self):
        return QSize(self.codeEditor.line_number_area_width(), 0)

    def paintEvent(self, event):
        self.codeEditor.lineNumberAreaPaintEvent(event)


class CodeEditor(QPlainTextEdit):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.settings = Settings()
        self._theme: Theme = get_theme(self.settings.get("theme") or "dark")
        self._source_format = self.settings.get("build", "source_format") or "free"

        self.line_number_area = LineNumberArea(self)
        self.highlighter = CobolHighlighter(self.document(), self._theme)
        self.highlighter.set_source_format(self._source_format)

        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self.update_line_number_area)
        self.cursorPositionChanged.connect(self.highlight_current_line)

        self.update_line_number_area_width(0)
        self.highlight_current_line()
        self.setup_font()
        self.setup_editor()

    def set_theme(self, theme: Theme) -> None:
        self._theme = theme
        self.highlighter.set_theme(theme)
        self.highlight_current_line()
        self.line_number_area.update()

    def set_source_format(self, fmt: str) -> None:
        self._source_format = fmt
        self.highlighter.set_source_format(fmt)

    def setup_font(self):
        family = self.settings.get("editor", "font_family") or "Monospace"
        size   = self.settings.get("editor", "font_size") or 14
        font = QFont(family, size)
        font.setStyleHint(QFont.StyleHint.Monospace)
        font.setFixedPitch(True)
        self.setFont(font)

    def setup_editor(self):
        self.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        tab_width = self.settings.get("editor", "tab_width") or 4
        self.setTabStopDistance(self.fontMetrics().horizontalAdvance(' ') * tab_width)

    def keyPressEvent(self, event: QKeyEvent):
        if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            cursor = self.textCursor()
            line_text = cursor.block().text()
            indent = ""
            for ch in line_text:
                if ch in (' ', '\t'):
                    indent += ch
                else:
                    break
            super().keyPressEvent(event)
            if indent:
                self.insertPlainText(indent)
        else:
            super().keyPressEvent(event)

    # ── Line number gutter ─────────────────────────────────────────

    def line_number_area_width(self):
        digits = max(1, len(str(self.blockCount())))
        return 3 + self.fontMetrics().horizontalAdvance('9') * digits

    def update_line_number_area_width(self, _):
        self.setViewportMargins(self.line_number_area_width(), 0, 0, 0)

    def update_line_number_area(self, rect, dy):
        if dy:
            self.line_number_area.scroll(0, dy)
        else:
            self.line_number_area.update(0, rect.y(), self.line_number_area.width(), rect.height())
        if rect.contains(self.viewport().rect()):
            self.update_line_number_area_width(0)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        cr = self.contentsRect()
        self.line_number_area.setGeometry(
            QRect(cr.left(), cr.top(), self.line_number_area_width(), cr.height())
        )

    def highlight_current_line(self):
        extra = []
        if not self.isReadOnly():
            sel = QTextEdit.ExtraSelection()
            sel.format.setBackground(QColor(self._theme.current_line))
            sel.format.setProperty(QTextFormat.Property.FullWidthSelection, True)
            sel.cursor = self.textCursor()
            sel.cursor.clearSelection()
            extra.append(sel)
        self.setExtraSelections(extra)

    def lineNumberAreaPaintEvent(self, event):
        painter = QPainter(self.line_number_area)
        painter.fillRect(event.rect(), QColor(self._theme.line_number_bg))
        block = self.firstVisibleBlock()
        bn = block.blockNumber()
        top = round(self.blockBoundingGeometry(block).translated(self.contentOffset()).top())
        bottom = top + round(self.blockBoundingRect(block).height())
        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                painter.setPen(QColor(self._theme.line_number_fg))
                painter.drawText(0, top, self.line_number_area.width(),
                                 self.fontMetrics().height(), Qt.AlignmentFlag.AlignRight, str(bn + 1))
            block = block.next()
            top = bottom
            bottom = top + round(self.blockBoundingRect(block).height())
            bn += 1
