from typing import Optional, List, Dict, Any
from pathlib import Path
import json
import sys

class ErrorLevel:
    """Enum-like class for error severity levels."""
    ERROR = "ERROR"
    WARNING = "WARNING"
    INFO = "INFO"

class ErrorFormatter:
    """Formats error messages with rich context, color, and structured output."""
    def __init__(self, use_color: bool = True, context_lines: int = 2, locale: str = "en"):
        self.use_color = use_color and sys.stdout.isatty()  # Only use color in terminals
        self.context_lines = max(0, context_lines)  # Lines before/after error
        self.locale = locale
        self._line_cache: Dict[str, List[str]] = {}  # Cache source lines
        self._translations = {
            "en": {
                "file": "File",
                "line": "Line",
                "column": "Column",
                "unknown": "<unknown>",
                "invalid_line": "Invalid line number",
                "invalid_column": "Invalid column number",
            },
            # Add other languages as needed
        }

    def _get_translation(self, key: str) -> str:
        """Retrieve localized string."""
        return self._translations.get(self.locale, self._translations["en"]).get(key, key)

    def _split_lines(self, source: str) -> List[str]:
        """Split source into lines, caching result."""
        source_hash = hash(source)
        if source_hash not in self._line_cache:
            self._line_cache[source_hash] = source.splitlines()
        return self._line_cache[source_hash]

    def _ansi(self, text: str, code: str) -> str:
        """Apply ANSI color if enabled."""
        return f"\033[{code}m{text}\033[0m" if self.use_color else text

    def format_error(
        self,
        error_type: str,
        message: str,
        filename: str,
        source: str,
        line: int,
        column: int,
        end_line: Optional[int] = None,
        end_column: Optional[int] = None,
        level: str = ErrorLevel.ERROR,
        error_code: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> str:
        """Formats an error with context, color, and optional structured output."""
        # Validate inputs
        filename = str(Path(filename)) if filename else "<unknown>"
        source = source or ""
        line = max(1, line)
        column = max(1, column)
        end_line = end_line or line
        end_column = end_column or column

        # Get source lines
        lines = self._split_lines(source)
        max_lines = len(lines)

        # Validate line and column
        if line > max_lines or line < 1:
            return (
                f"{self._ansi(level, '31' if level == ErrorLevel.ERROR else '33')}: {error_type}: {message}\n"
                f"  {self._get_translation('file')}: {filename}\n"
                f"  {self._get_translation('line')}: {line}, {self._get_translation('column')}: {column}\n"
                f"  {self._get_translation('invalid_line')}"
            )

        # Prepare context window
        start_context = max(1, line - self.context_lines)
        end_context = min(max_lines, (end_line or line) + self.context_lines)
        max_line_num_width = len(str(end_context))

        # Build output
        output = [
            f"{self._ansi(level, '31' if level == ErrorLevel.ERROR else '33')}: {error_type}: {message}",
            f"  {self._get_translation('file')}: {filename}",
            f"  {self._get_translation('line')}: {line}, {self._get_translation('column')}: {column}"
        ]

        # Add code snippet
        for i in range(start_context - 1, end_context):
            if i >= max_lines:
                break
            code_line = lines[i].rstrip() or " "
            line_num = i + 1
            prefix = f"{line_num:>{max_line_num_width}} | "
            if line_num >= line and line_num <= end_line:
                output.append(f"{self._ansi(prefix, '36')}{self._ansi(code_line, '1')}")
                # Add marker
                if line_num == line:
                    marker_start = column - 1
                    marker_len = (end_column - column + 1) if line_num == end_line else len(code_line) - marker_start
                    marker = " " * marker_start + "^" * max(1, marker_len)
                    output.append(f"{' ' * max_line_num_width} | {self._ansi(marker, '31')}")
            else:
                output.append(f"{prefix}{code_line}")

        # Add error code and metadata
        if error_code:
            output.append(f"  Error Code: {error_code}")
        if metadata:
            output.append(f"  Metadata: {json.dumps(metadata, indent=2)}")

        return "\n".join(output)

    def format_json(
        self,
        error_type: str,
        message: str,
        filename: str,
        source: str,
        line: int,
        column: int,
        end_line: Optional[int] = None,
        end_column: Optional[int] = None,
        level: str = ErrorLevel.ERROR,
        error_code: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> str:
        """Formats error as JSON for IDE/tool integration."""
        return json.dumps({
            "level": level,
            "type": error_type,
            "message": message,
            "filename": str(Path(filename)) if filename else "<unknown>",
            "line": max(1, line),
            "column": max(1, column),
            "end_line": end_line or max(1, line),
            "end_column": end_column or max(1, column),
            "code_snippet": self._split_lines(source)[max(0, line - 1):line] if line <= len(self._split_lines(source)) else [],
            "error_code": error_code,
            "metadata": metadata or {}
        }, indent=2)

def format_error(
    error_type: str,
    message: str,
    filename: str,
    source: str,
    line: int,
    column: int,
    end_line: Optional[int] = None,
    end_column: Optional[int] = None,
    level: str = ErrorLevel.ERROR,
    error_code: Optional[str] = None,
    metadata: Optional[Dict[str, Any]] = None
) -> str:
    """Compatibility wrapper for existing calls."""
    formatter = ErrorFormatter()
    return formatter.format_error(
        error_type, message, filename, source, line, column,
        end_line, end_column, level, error_code, metadata
    )