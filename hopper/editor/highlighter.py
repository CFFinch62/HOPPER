# hopper/editor/highlighter.py - COBOL syntax highlighter

from __future__ import annotations

from PyQt6.QtGui import QSyntaxHighlighter, QTextCharFormat, QColor, QFont
from PyQt6.QtCore import QRegularExpression

from hopper.config.themes import Theme, get_theme

_STATE_NORMAL = 0

# COBOL is case-insensitive throughout
_CI = QRegularExpression.PatternOption.CaseInsensitiveOption


class CobolHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for GnuCOBOL (free-form and fixed-form sources).

    Rule groups are applied in order; later rules overwrite earlier ones.
    """

    def __init__(self, document, theme: Theme | None = None):
        super().__init__(document)
        self._theme = theme or get_theme("dark")
        self._source_format = "free"   # "free" or "fixed"
        self.highlighting_rules: list[tuple] = []
        self._build_rules(self._theme)

    def set_theme(self, theme: Theme) -> None:
        self._theme = theme
        self._build_rules(theme)
        self.rehighlight()

    def set_source_format(self, fmt: str) -> None:
        self._source_format = fmt
        self._build_rules(self._theme)
        self.rehighlight()

    # ------------------------------------------------------------------
    # Rule construction
    # ------------------------------------------------------------------

    def _build_rules(self, theme: Theme) -> None:
        self.highlighting_rules.clear()

        # ── 1. Numbers ─────────────────────────────────────────────
        num_fmt = QTextCharFormat()
        num_fmt.setForeground(QColor(theme.number))
        self.highlighting_rules.append((
            QRegularExpression(r"[+-]?\b[0-9]+(\.[0-9]+)?\b"),
            num_fmt,
        ))

        # ── 2. Division headers (boldest, takes priority) ───────────
        div_fmt = QTextCharFormat()
        div_fmt.setForeground(QColor(theme.division))
        div_fmt.setFontWeight(QFont.Weight.Bold)
        for phrase in [
            r"IDENTIFICATION\s+DIVISION",
            r"ENVIRONMENT\s+DIVISION",
            r"DATA\s+DIVISION",
            r"PROCEDURE\s+DIVISION",
        ]:
            self.highlighting_rules.append((
                QRegularExpression(phrase, _CI), div_fmt
            ))

        # ── 3. Section/Paragraph headers → division color ───────────
        section_fmt = QTextCharFormat()
        section_fmt.setForeground(QColor(theme.division))
        for phrase in [
            r"WORKING-STORAGE\s+SECTION",
            r"FILE\s+SECTION",
            r"LOCAL-STORAGE\s+SECTION",
            r"LINKAGE\s+SECTION",
            r"CONFIGURATION\s+SECTION",
            r"INPUT-OUTPUT\s+SECTION",
        ]:
            self.highlighting_rules.append((
                QRegularExpression(phrase, _CI), section_fmt
            ))

        # ── 4. Type / Picture keywords ──────────────────────────────
        type_fmt = QTextCharFormat()
        type_fmt.setForeground(QColor(theme.type_keyword))
        for word in [
            "PICTURE", "PIC", "USAGE", "COMP-5", "COMP-4", "COMP-3",
            "COMP-2", "COMP-1", "COMP", "BINARY", "PACKED-DECIMAL",
            "INDEX", "POINTER", "PROCEDURE-POINTER", "FUNCTION-POINTER",
            "OCCURS", "TIMES", "INDEXED", "DEPENDING", "ON",
            "REDEFINES", "RENAMES", "RENAME", "VALUE", "VALUES",
        ]:
            self.highlighting_rules.append((
                QRegularExpression(rf"\b{word}\b", _CI), type_fmt
            ))

        # ── 5. Control / Verb keywords (bold) ──────────────────────
        kw_fmt = QTextCharFormat()
        kw_fmt.setForeground(QColor(theme.keyword))
        kw_fmt.setFontWeight(QFont.Weight.Bold)
        for word in [
            "MOVE", "COMPUTE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE",
            "REMAINDER", "GIVING", "TO", "FROM", "INTO", "BY",
            "PERFORM", "VARYING", "UNTIL", "THRU", "THROUGH", "AFTER",
            "IF", "THEN", "ELSE", "END-IF", "EVALUATE", "WHEN", "OTHER",
            "END-EVALUATE", "CALL", "USING", "RETURNING", "STOP", "GOBACK",
            "EXIT", "OPEN", "CLOSE", "READ", "WRITE", "REWRITE", "DELETE",
            "START", "ACCEPT", "DISPLAY", "UPON", "INITIALIZE", "SET",
            "INSPECT", "TALLYING", "REPLACING", "SORT", "MERGE", "RELEASE",
            "RETURN", "STRING", "UNSTRING", "REFERENCE", "CONTENT",
            "PROGRAM-ID", "AUTHOR", "DATE-WRITTEN",
            "ENVIRONMENT", "IDENTIFICATION", "DATA", "PROCEDURE",
            "SECTION", "DIVISION", "PARAGRAPH",
            "NOT", "AND", "OR", "EQUAL", "GREATER", "LESS", "THAN",
            "IS", "ARE", "ZERO", "ZEROS", "ZEROES", "SPACE", "SPACES",
            "HIGH-VALUE", "HIGH-VALUES", "LOW-VALUE", "LOW-VALUES",
            "QUOTE", "QUOTES", "TRUE", "FALSE", "NULL", "NULLS",
            "WITH", "NO", "ADVANCING", "AT", "END", "ALL", "LEADING",
        ]:
            self.highlighting_rules.append((
                QRegularExpression(rf"\b{word}\b", _CI), kw_fmt
            ))

        # ── 6. FUNCTION built-ins ───────────────────────────────────
        fn_fmt = QTextCharFormat()
        fn_fmt.setForeground(QColor(theme.function_name))
        self.highlighting_rules.append((
            QRegularExpression(r"\bFUNCTION\s+[A-Z][A-Z0-9\-]*\b", _CI),
            fn_fmt,
        ))

        # ── 7. Strings ──────────────────────────────────────────────
        str_fmt = QTextCharFormat()
        str_fmt.setForeground(QColor(theme.string))
        self.highlighting_rules.append((QRegularExpression(r"'[^']*'"), str_fmt))
        self.highlighting_rules.append((QRegularExpression(r'"[^"]*"'), str_fmt))

        # ── 8. Fixed-form column 7 comments  ( *  in col 7 ) ────────
        # NOTE: applied before line comments so *> overrides it if needed.
        if self._source_format == "fixed":
            fixed_seq_fmt = QTextCharFormat()
            fixed_seq_fmt.setForeground(QColor(theme.comment))
            fixed_seq_fmt.setFontItalic(True)
            # Col 7 star-comment: line starts with at least 6 chars then *
            self.highlighting_rules.append((
                QRegularExpression(r"^.{6}\*.*"),
                fixed_seq_fmt,
            ))

        # ── 9. Line comments (*> anywhere, or standalone * in free mode) ─
        cmt_fmt = QTextCharFormat()
        cmt_fmt.setForeground(QColor(theme.comment))
        cmt_fmt.setFontItalic(True)
        self.highlighting_rules.append((QRegularExpression(r"\*>.*"), cmt_fmt))
        # Free-form: line starting with * (legacy style)
        self.highlighting_rules.append((QRegularExpression(r"^\s*\*(?!>).*"), cmt_fmt))

        # Store comment format for paintEvent overrides haven't reached yet
        self._comment_fmt = cmt_fmt

    # ------------------------------------------------------------------
    # Qt override
    # ------------------------------------------------------------------

    def highlightBlock(self, text: str) -> None:
        for pattern, fmt in self.highlighting_rules:
            it = pattern.globalMatch(text)
            while it.hasNext():
                m = it.next()
                self.setFormat(m.capturedStart(), m.capturedLength(), fmt)

        self.setCurrentBlockState(_STATE_NORMAL)
