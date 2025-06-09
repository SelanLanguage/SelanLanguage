import re
from typing import List, Tuple, Optional
from .token import Token, TokenType, token_types_list
from .error import format_error

class Lexer:
    """Tokenizes XenonLang source code into a list of Tokens with support for advanced constructs."""
    def __init__(self, code: str, filename: str):
        self.code = code
        self.filename = filename
        self.pos = 0
        self.line = 1
        self.column = 1
        self.token_list: List[Token] = []
        self.state: str = 'DEFAULT'  # States: DEFAULT, RAW_STRING, BLOCK_COMMENT
        self.raw_string_start_line: int = 0
        self.raw_string_start_column: int = 0

        # Prioritized token order (longer patterns first, then keywords, then single-char)
        self.token_priority = [
            # Multi-character operators
            'EQUAL', 'NOT_EQUAL', 'LESS_EQUAL', 'GREATER_EQUAL', 'NULL_COALESCE', 'ELVIS',
            'INCREMENT', 'DECREMENT', 'PLUS_ASSIGN', 'MINUS_ASSIGN', 'MULTIPLY_ASSIGN',
            'DIVIDE_ASSIGN', 'MODULO_ASSIGN', 'AND', 'OR', 'SHL', 'SHR', 'BIT_AND',
            'BIT_OR', 'BIT_XOR', 'ARROW', 'NULLABLE',
            # New operators
            'DEREF', 'REFERENCE',
            # Keywords
            'IF', 'ELSE', 'WHEN', 'WHILE', 'FOR', 'DO', 'SWITCH', 'CASE', 'DEFAULT',
            'BREAK', 'CONTINUE', 'RETURN', 'TRY', 'CATCH', 'FINALLY', 'THROW',
            'FUNCTION', 'CLASS', 'STRUCT', 'DATA', 'SEALED', 'OBJECT', 'COMPANION',
            'INTERFACE', 'ENUM', 'VAR', 'VAL', 'CONST', 'STATIC', 'PUBLIC', 'PRIVATE',
            'PROTECTED', 'INTERNAL', 'ABSTRACT', 'NEW', 'THIS', 'SUPER', 'INSTANCEOF',
            'LAMBDA', 'IMPORT', 'FROM', 'AS', 'PACKAGE', 'NAMESPACE', 'EXPORT',
            'ASYNC', 'AWAIT', 'YIELD', 'ALLOC', 'FREE', 'TRUE', 'FALSE', 'NULL',
            'PRINT', 'INPUT', 'READLINE',
            # Types
            'INT', 'FLOAT', 'DOUBLE', 'BOOLEAN', 'STRING_TYPE', 'CHAR_TYPE', 'BYTE',
            'UNIT', 'NOTHING', 'ANY',
            # Literals (handled separately)
            'NUMBER', 'STRING', 'RAW_STRING', 'CHAR', 'HEX', 'BYTE_LITERAL',
            # Single-character operators and delimiters
            'ASSIGN', 'LESS', 'GREATER', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE',
            'MODULO', 'NOT', 'BIT_NOT', 'SEMICOLON', 'COLON', 'COMMA', 'DOT',
            'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE', 'LBRACKET', 'RBRACKET',
            # Special tokens
            'ANNOTATION', 'COMMENT', 'BLOCK_COMMENT', 'SPACE',
            # Identifiers (must be last)
            'VARIABLE'
        ]

        # Compile regex patterns
        self.patterns: List[Tuple[TokenType, re.Pattern]] = [
            (token_types_list[name], re.compile(r'^' + token_types_list[name].regex))
            for name in self.token_priority
            if name in token_types_list
        ]

        # Specialized patterns for literals
        self.number_pattern = re.compile(r'^-?(?:\d+\.\d*[eE][+-]?\d+|\d+\.\d+|\d+)(?!\w)')
        self.string_pattern = re.compile(r'^"([^"\\]*(?:\\.[^"\\]*)*)"')
        self.raw_string_pattern = re.compile(r'^"""')
        self.char_pattern = re.compile(r"^'([^'\\]|\\.)'")
        self.hex_pattern = re.compile(r'^0[xX][0-9a-fA-F]+(?!\w)')
        self.byte_pattern = re.compile(r'^0b[0-1]+(?!\w)')
        self.variable_pattern = re.compile(r'^[a-zA-Z_][a-zA-Z0-9_]*(?!\w)')
        self.annotation_pattern = re.compile(r'^@[a-zA-Z_][a-zA-Z0-9_]*(?!\w)')

    def update_position(self, value: str) -> None:
        """Updates line and column numbers based on the token value."""
        lines = value.split('\n')
        self.line += len(lines) - 1
        self.column = len(lines[-1]) + 1 if len(lines) > 1 else self.column + len(value)
        self.pos += len(value)

    def lex_analysis(self) -> List[Token]:
        """Performs lexical analysis, returning tokens excluding SPACE and COMMENT."""
        while self.pos < len(self.code):
            if not self.next_token():
                break
        return [token for token in self.token_list if token.type.name not in ('SPACE', 'COMMENT', 'BLOCK_COMMENT')]

    def next_token(self) -> bool:
        """Matches the next token based on the current state."""
        if self.pos >= len(self.code):
            return False

        substring = self.code[self.pos:]

        # State: RAW_STRING
        if self.state == 'RAW_STRING':
            return self.handle_raw_string(substring)

        # State: BLOCK_COMMENT
        if self.state == 'BLOCK_COMMENT':
            return self.handle_block_comment(substring)

        # State: DEFAULT
        # Skip whitespace
        whitespace_match = re.match(r'^\s+', substring)
        if whitespace_match:
            value = whitespace_match.group(0)
            token = Token(token_types_list['SPACE'], value, self.pos, self.line, self.column)
            self.token_list.append(token)
            self.update_position(value)
            return True

        # Try specialized literal patterns first
        if token := self.try_literals(substring):
            self.token_list.append(token)
            return True

        # Try annotation
        if annotation_match := self.annotation_pattern.match(substring):
            value = annotation_match.group(0)
            token = Token(token_types_list['ANNOTATION'], value, self.pos, self.line, self.column)
            self.token_list.append(token)
            self.update_position(value)
            return True

        # Try general token patterns
        for token_type, pattern in self.patterns:
            if token_type.name in ('NUMBER', 'STRING', 'RAW_STRING', 'CHAR', 'HEX', 'BYTE_LITERAL', 'VARIABLE', 'ANNOTATION'):
                continue  # Handled separately
            if match := pattern.match(substring):
                value = match.group(0)
                if token_type.name == 'RAW_STRING':
                    self.state = 'RAW_STRING'
                    self.raw_string_start_line = self.line
                    self.raw_string_start_column = self.column
                elif token_type.name == 'BLOCK_COMMENT':
                    self.state = 'BLOCK_COMMENT'
                token = Token(token_type, value, self.pos, self.line, self.column)
                self.token_list.append(token)
                self.update_position(value)
                return True

        # Unrecognized character
        char = self.code[self.pos]
        raise SyntaxError(format_error(
            "SyntaxError",
            f"Unexpected character '{char}'",
            self.filename,
            self.code,
            self.line,
            self.column
        ))

    def try_literals(self, substring: str) -> Optional[Token]:
        """Attempts to match literal tokens (numbers, strings, chars, variables)."""
        # Number
        if match := self.number_pattern.match(substring):
            value = match.group(0)
            try:
                float(value)  # Validate number format
            except ValueError:
                raise SyntaxError(format_error(
                    "SyntaxError",
                    f"Invalid number format: {value}",
                    self.filename,
                    self.code,
                    self.line,
                    self.column
                ))
            token = Token(token_types_list['NUMBER'], value, self.pos, self.line, self.column)
            self.update_position(value)
            return token

        # String
        if match := self.string_pattern.match(substring):
            value = match.group(0)
            token = Token(token_types_list['STRING'], value, self.pos, self.line, self.column)
            self.update_position(value)
            return token

        # Raw string start
        if match := self.raw_string_pattern.match(substring):
            value = match.group(0)
            self.state = 'RAW_STRING'
            self.raw_string_start_line = self.line
            self.raw_string_start_column = self.column
            token = Token(token_types_list['RAW_STRING'], value, self.pos, self.line, self.column)
            self.update_position(value)
            return token

        # Char
        if match := self.char_pattern.match(substring):
            value = match.group(0)
            if len(value) > 3 and value[1] != '\\':
                raise SyntaxError(format_error(
                    "SyntaxError",
                    f"Invalid char literal: {value}",
                    self.filename,
                    self.code,
                    self.line,
                    self.column
                ))
            token = Token(token_types_list['CHAR'], value, self.pos, self.line, self.column)
            self.update_position(value)
            return token

        # Hex
        if match := self.hex_pattern.match(substring):
            value = match.group(0)
            try:
                int(value[2:], 16)  # Validate hex
            except ValueError:
                raise SyntaxError(format_error(
                    "SyntaxError",
                    f"Invalid hex literal: {value}",
                    self.filename,
                    self.code,
                    self.line,
                    self.column
                ))
            token = Token(token_types_list['HEX'], value, self.pos, self.line, self.column)
            self.update_position(value)
            return token

        # Byte
        if match := self.byte_pattern.match(substring):
            value = match.group(0)
            try:
                int(value[2:], 2)  # Validate binary
            except ValueError:
                raise SyntaxError(format_error(
                    "SyntaxError",
                    f"Invalid byte literal: {value}",
                    self.filename,
                    self.code,
                    self.line,
                    self.column
                ))
            token = Token(token_types_list['BYTE_LITERAL'], value, self.pos, self.line, self.column)
            self.update_position(value)
            return token

        # Variable/Identifier
        if match := self.variable_pattern.match(substring):
            value = match.group(0)
            # Check if it's a keyword
            for token_type, _ in self.patterns:
                if token_type.name in self.token_priority and token_type.regex == value:
                    token = Token(token_type, value, self.pos, self.line, self.column)
                    break
            else:
                token = Token(token_types_list['VARIABLE'], value, self.pos, self.line, self.column)
            self.update_position(value)
            return token

        return None

    def handle_raw_string(self, substring: str) -> bool:
        """Handles raw string content until closing triple quotes."""
        end_match = re.search(r'"""', substring)
        if end_match:
            value = substring[:end_match.end()]
            token = Token(token_types_list['RAW_STRING'], value, self.pos, self.line, self.column)
            self.token_list.append(token)
            self.update_position(value)
            self.state = 'DEFAULT'
            return True
        else:
            # Unclosed raw string
            raise SyntaxError(format_error(
                "SyntaxError",
                "Unclosed raw string starting at line " + str(self.raw_string_start_line),
                self.filename,
                self.code,
                self.raw_string_start_line,
                self.raw_string_start_column
            ))

    def handle_block_comment(self, substring: str) -> bool:
        """Handles block comment content until closing */."""
        end_match = re.search(r'\*/', substring)
        if end_match:
            value = substring[:end_match.end()]
            token = Token(token_types_list['BLOCK_COMMENT'], value, self.pos, self.line, self.column)
            self.token_list.append(token)
            self.update_position(value)
            self.state = 'DEFAULT'
            return True
        else:
            # Unclosed block comment
            raise SyntaxError(format_error(
                "SyntaxError",
                "Unclosed block comment",
                self.filename,
                self.code,
                self.line,
                self.column
            ))