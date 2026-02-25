# hopper/config/settings_dialog.py - Preferences dialog

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTabWidget, QWidget, QLabel,
    QSpinBox, QCheckBox, QComboBox, QDialogButtonBox, QLineEdit,
    QFileDialog, QPushButton, QGroupBox, QMessageBox, QRadioButton,
    QButtonGroup,
)
from PyQt6.QtCore import Qt

from hopper.config.cobol_detector import detect_cobol_compilers, is_valid_compiler


class SettingsDialog(QDialog):
    def __init__(self, parent, settings):
        super().__init__(parent)
        self.settings = settings
        self.setWindowTitle("Preferences")
        self.resize(520, 460)

        layout = QVBoxLayout(self)
        self.tabs = QTabWidget()
        layout.addWidget(self.tabs)

        self._create_editor_tab()
        self._create_build_tab()
        self._create_appearance_tab()

        bb = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        bb.accepted.connect(self.accept)
        bb.rejected.connect(self.reject)
        layout.addWidget(bb)
        self._load_values()

    # ── Editor tab ────────────────────────────────────────────────────

    def _create_editor_tab(self):
        tab = QWidget()
        ly = QVBoxLayout(tab)

        h = QHBoxLayout(); h.addWidget(QLabel("Font Family:"))
        self.font_family_edit = QLineEdit(); h.addWidget(self.font_family_edit)
        ly.addLayout(h)

        h = QHBoxLayout(); h.addWidget(QLabel("Font Size:"))
        self.font_size_spin = QSpinBox(); self.font_size_spin.setRange(8, 72)
        h.addWidget(self.font_size_spin); ly.addLayout(h)

        h = QHBoxLayout(); h.addWidget(QLabel("Tab Width:"))
        self.tab_width_spin = QSpinBox(); self.tab_width_spin.setRange(1, 16)
        h.addWidget(self.tab_width_spin); ly.addLayout(h)

        self.line_numbers_check = QCheckBox("Show Line Numbers")
        ly.addWidget(self.line_numbers_check)
        ly.addStretch()
        self.tabs.addTab(tab, "Editor")

    # ── Build tab ─────────────────────────────────────────────────────

    def _create_build_tab(self):
        tab = QWidget()
        ly = QVBoxLayout(tab)

        # Compiler group
        cg = QGroupBox("COBOL Compiler")
        cg_ly = QVBoxLayout(cg)

        h = QHBoxLayout(); h.addWidget(QLabel("Compiler:"))
        self.compiler_combo = QComboBox(); self.compiler_combo.setMinimumWidth(260)
        self.compiler_combo.currentIndexChanged.connect(self._on_compiler_changed)
        h.addWidget(self.compiler_combo)
        refresh_btn = QPushButton("Refresh")
        refresh_btn.clicked.connect(self._refresh_compilers)
        h.addWidget(refresh_btn); cg_ly.addLayout(h)

        h = QHBoxLayout()
        self.custom_path_check = QCheckBox("Use custom path:")
        self.custom_path_check.toggled.connect(self._on_custom_toggled)
        h.addWidget(self.custom_path_check)
        self.compiler_path_edit = QLineEdit(); self.compiler_path_edit.setEnabled(False)
        self.compiler_path_edit.setPlaceholderText("Path to cobc executable")
        h.addWidget(self.compiler_path_edit)
        browse_btn = QPushButton("Browse…"); browse_btn.clicked.connect(self._browse_compiler)
        h.addWidget(browse_btn); cg_ly.addLayout(h)
        ly.addWidget(cg)

        # Source format
        sf_group = QGroupBox("Source Format")
        sf_ly = QHBoxLayout(sf_group)
        self.free_radio  = QRadioButton("Free-form  (-free)")
        self.fixed_radio = QRadioButton("Fixed-form (-fixed)")
        self._fmt_group = QButtonGroup()
        self._fmt_group.addButton(self.free_radio,  0)
        self._fmt_group.addButton(self.fixed_radio, 1)
        sf_ly.addWidget(self.free_radio); sf_ly.addWidget(self.fixed_radio)
        ly.addWidget(sf_group)

        h = QHBoxLayout(); h.addWidget(QLabel("Compiler Flags:"))
        self.compiler_flags_edit = QLineEdit(); self.compiler_flags_edit.setPlaceholderText("-Wall")
        h.addWidget(self.compiler_flags_edit); ly.addLayout(h)

        h = QHBoxLayout(); h.addWidget(QLabel("Output Directory:"))
        self.output_dir_edit = QLineEdit(); self.output_dir_edit.setPlaceholderText(".")
        h.addWidget(self.output_dir_edit)
        od_btn = QPushButton("Browse…"); od_btn.clicked.connect(self._browse_output_dir)
        h.addWidget(od_btn); ly.addLayout(h)

        ly.addStretch()
        self.tabs.addTab(tab, "Build")
        self._refresh_compilers()

    def _create_appearance_tab(self):
        tab = QWidget(); ly = QVBoxLayout(tab)
        h = QHBoxLayout(); h.addWidget(QLabel("Theme:"))
        self.theme_combo = QComboBox(); self.theme_combo.addItems(["dark", "light"])
        h.addWidget(self.theme_combo); ly.addLayout(h)
        ly.addStretch()
        self.tabs.addTab(tab, "Appearance")

    # ── Compiler helpers ──────────────────────────────────────────────

    def _refresh_compilers(self):
        self.compiler_combo.clear()
        self._detected = detect_cobol_compilers()
        if not self._detected:
            self.compiler_combo.addItem("No compilers found", "")
            self.compiler_combo.setEnabled(False)
            self.custom_path_check.setChecked(True)
        else:
            self.compiler_combo.setEnabled(True)
            for c in self._detected:
                self.compiler_combo.addItem(c.display_name(), c.path)

    def _on_compiler_changed(self, idx):
        if not self.custom_path_check.isChecked() and idx >= 0:
            path = self.compiler_combo.currentData()
            if path:
                self.compiler_path_edit.setText(path)

    def _on_custom_toggled(self, checked):
        self.compiler_path_edit.setEnabled(checked)
        self.compiler_combo.setEnabled(not checked)
        if not checked and self.compiler_combo.currentData():
            self.compiler_path_edit.setText(self.compiler_combo.currentData())

    def _browse_compiler(self):
        path, _ = QFileDialog.getOpenFileName(self, "Select COBOL Compiler Executable")
        if path:
            self.compiler_path_edit.setText(path)
            self.custom_path_check.setChecked(True)

    def _browse_output_dir(self):
        d = QFileDialog.getExistingDirectory(self, "Select Output Directory")
        if d:
            self.output_dir_edit.setText(d)

    # ── Load / Save ───────────────────────────────────────────────────

    def _load_values(self):
        self.font_family_edit.setText(self.settings.get("editor", "font_family") or "")
        self.font_size_spin.setValue(self.settings.get("editor", "font_size") or 14)
        self.tab_width_spin.setValue(self.settings.get("editor", "tab_width") or 4)
        self.line_numbers_check.setChecked(bool(self.settings.get("editor", "show_line_numbers")))

        saved_path = self.settings.get("build", "compiler_path") or ""
        self.compiler_path_edit.setText(saved_path)
        found = False
        for i in range(self.compiler_combo.count()):
            if self.compiler_combo.itemData(i) == saved_path:
                self.compiler_combo.setCurrentIndex(i); found = True; break
        if not found and saved_path:
            self.custom_path_check.setChecked(True)

        fmt = self.settings.get("build", "source_format") or "free"
        if fmt == "fixed":
            self.fixed_radio.setChecked(True)
        else:
            self.free_radio.setChecked(True)

        self.compiler_flags_edit.setText(self.settings.get("build", "compiler_flags") or "")
        self.output_dir_edit.setText(self.settings.get("build", "output_dir") or ".")
        idx = self.theme_combo.findText(self.settings.get("theme") or "dark")
        if idx >= 0:
            self.theme_combo.setCurrentIndex(idx)

    def _save_values(self):
        self.settings.set("editor", "font_family", self.font_family_edit.text())
        self.settings.set("editor", "font_size",   self.font_size_spin.value())
        self.settings.set("editor", "tab_width",   self.tab_width_spin.value())
        self.settings.set("editor", "show_line_numbers", self.line_numbers_check.isChecked())
        self.settings.set("build", "compiler_path",  self.compiler_path_edit.text())
        self.settings.set("build", "source_format",  "fixed" if self.fixed_radio.isChecked() else "free")
        self.settings.set("build", "compiler_flags", self.compiler_flags_edit.text())
        self.settings.set("build", "output_dir",     self.output_dir_edit.text() or ".")
        self.settings.set("theme", self.theme_combo.currentText())
        self.settings.save()

    def accept(self):
        path = self.compiler_path_edit.text()
        if path and not is_valid_compiler(path):
            r = QMessageBox.warning(self, "Invalid Compiler",
                f"'{path}' does not appear to be a valid executable.\nSave anyway?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if r == QMessageBox.StandardButton.No:
                return
        self._save_values()
        super().accept()
