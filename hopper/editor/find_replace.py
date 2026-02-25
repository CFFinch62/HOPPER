# hopper/editor/find_replace.py

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QLineEdit,
    QPushButton, QCheckBox, QMessageBox,
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QTextCursor, QTextDocument


class FindReplaceDialog(QDialog):
    def __init__(self, editor, parent=None):
        super().__init__(parent)
        self.editor = editor
        self.setWindowTitle("Find / Replace")
        self.setFixedWidth(420)
        self.setWindowFlags(self.windowFlags() | Qt.WindowType.WindowStaysOnTopHint)
        ly = QVBoxLayout(self)

        h = QHBoxLayout(); h.addWidget(QLabel("Find:"))
        self.find_input = QLineEdit(); self.find_input.returnPressed.connect(self.find_next)
        h.addWidget(self.find_input); ly.addLayout(h)

        h = QHBoxLayout(); h.addWidget(QLabel("Replace:"))
        self.replace_input = QLineEdit(); h.addWidget(self.replace_input); ly.addLayout(h)

        self.case_check = QCheckBox("Case Sensitive"); ly.addWidget(self.case_check)

        bl = QHBoxLayout()
        for label, slot in [("Find Next", self.find_next), ("Find Previous", self.find_previous),
                             ("Replace", self.replace), ("Replace All", self.replace_all)]:
            btn = QPushButton(label); btn.clicked.connect(slot); bl.addWidget(btn)
        ly.addLayout(bl)

    def _flags(self, backward=False):
        f = QTextDocument.FindFlag(0)
        if self.case_check.isChecked():
            f |= QTextDocument.FindFlag.FindCaseSensitively
        if backward:
            f |= QTextDocument.FindFlag.FindBackward
        return f

    def find_next(self):
        text = self.find_input.text()
        if not text: return
        if not self.editor.find(text, self._flags()):
            c = self.editor.textCursor()
            c.movePosition(QTextCursor.MoveOperation.Start)
            self.editor.setTextCursor(c)
            if not self.editor.find(text, self._flags()):
                QMessageBox.information(self, "Find", "Text not found.")

    def find_previous(self):
        text = self.find_input.text()
        if not text: return
        if not self.editor.find(text, self._flags(backward=True)):
            c = self.editor.textCursor()
            c.movePosition(QTextCursor.MoveOperation.End)
            self.editor.setTextCursor(c)
            if not self.editor.find(text, self._flags(backward=True)):
                QMessageBox.information(self, "Find", "Text not found.")

    def replace(self):
        s = self.find_input.text(); r = self.replace_input.text()
        c = self.editor.textCursor(); sel = c.selectedText()
        match = sel.lower() == s.lower() if not self.case_check.isChecked() else sel == s
        if c.hasSelection() and match:
            c.insertText(r)
        self.find_next()

    def replace_all(self):
        text = self.find_input.text()
        if not text: return
        rep = self.replace_input.text(); count = 0
        c = self.editor.textCursor(); c.beginEditBlock()
        c.movePosition(QTextCursor.MoveOperation.Start)
        self.editor.setTextCursor(c)
        while self.editor.find(text, self._flags()):
            self.editor.textCursor().insertText(rep); count += 1
        c.endEditBlock()
        QMessageBox.information(self, "Replace All", f"Replaced {count} occurrence(s).")
