# hopper/editor/tab_widget.py

from __future__ import annotations
from pathlib import Path
from PyQt6.QtWidgets import QTabWidget, QMessageBox
from PyQt6.QtCore import pyqtSignal
from hopper.editor.code_editor import CodeEditor


class EditorTabWidget(QTabWidget):
    tab_changed = pyqtSignal(int)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setTabsClosable(True)
        self.setMovable(True)
        self.tabCloseRequested.connect(self.close_tab)
        self.currentChanged.connect(self.tab_changed)

    def new_file(self) -> CodeEditor:
        editor = CodeEditor()
        idx = self.addTab(editor, "untitled.cob")
        self.setCurrentIndex(idx)
        editor.modificationChanged.connect(lambda m: self.update_tab_title(editor, m))
        editor.setFocus()
        return editor

    def open_file(self, path, content: str) -> CodeEditor:
        path = Path(path)
        editor = CodeEditor()
        editor.setPlainText(content)
        editor.document().setModified(False)
        editor.setProperty("file_path", str(path))
        idx = self.addTab(editor, path.name)
        self.setCurrentIndex(idx)
        editor.modificationChanged.connect(lambda m: self.update_tab_title(editor, m))
        editor.setFocus()
        return editor

    def close_tab(self, index: int):
        editor = self.widget(index)
        if editor and editor.document().isModified():
            r = QMessageBox.question(self, "Unsaved Changes",
                f"Save '{self.tabText(index).rstrip('*')}'?",
                QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                return
        self.removeTab(index)

    def update_tab_title(self, editor, modified: bool):
        idx = self.indexOf(editor)
        if idx == -1:
            return
        title = self.tabText(idx)
        if modified and not title.endswith("*"):
            self.setTabText(idx, title + "*")
        elif not modified and title.endswith("*"):
            self.setTabText(idx, title[:-1])

    def get_current_editor(self) -> CodeEditor | None:
        return self.currentWidget()
