import re
from typing import List, Optional, Dict, Tuple, Pattern
from dataclasses import dataclass
from enum import Enum
import logging
from .token import Token, TokenType, token_types_list, token_types
from .error import format_error

# Configure logging to write to tokens.log
logging.basicConfig(level=logging.INFO, filename='tokens.log', filemode='w')
logger = logging.getLogger(__name__)

class LexerError(Exception):
    """Custom exception for lexer errors."""
    pass

class TokenCategory(Enum):
    """Categories for token types to improve organization and validation."""
    KEYWORD = "keyword"
    OPERATOR = "operator"
    PUNCTUATION = "punctuation"
    LITERAL = "literal"
    IDENTIFIER = "identifier"
    TYPE = "type"
    WHITESPACE = "whitespace"
    COMMENT = "comment"

@dataclass
class TokenSpec:
    """Specification for a token type, including regex and category."""
    name: str
    token_type: TokenType
    regex: str
    category: TokenCategory
    skip: bool = False  # Whether to skip this token in output

class Lexer:
    """Tokenizes source code into a list of Tokens for a strongly-typed language with Python-like imports."""
    
    # Token specifications in order of precedence
    TOKEN_SPECS: List[TokenSpec] = [
        # Keywords
        TokenSpec("IF", token_types_list["IF"], r"if\b", TokenCategory.KEYWORD),
        TokenSpec("ELSE", token_types_list["ELSE"], r"else\b", TokenCategory.KEYWORD),
        TokenSpec("WHILE", token_types_list["WHILE"], r"while\b", TokenCategory.KEYWORD),
        TokenSpec("FOR", token_types_list["FOR"], r"for\b", TokenCategory.KEYWORD),
        TokenSpec("DO", token_types_list["DO"], r"do\b", TokenCategory.KEYWORD),
        TokenSpec("SWITCH", token_types_list["SWITCH"], r"switch\b", TokenCategory.KEYWORD),
        TokenSpec("CASE", token_types_list["CASE"], r"case\b", TokenCategory.KEYWORD),
        TokenSpec("DEFAULT", token_types_list["DEFAULT"], r"default\b", TokenCategory.KEYWORD),
        TokenSpec("BREAK", token_types_list["BREAK"], r"break\b", TokenCategory.KEYWORD),
        TokenSpec("CONTINUE", token_types_list["CONTINUE"], r"continue\b", TokenCategory.KEYWORD),
        TokenSpec("RETURN", token_types_list["RETURN"], r"return\b", TokenCategory.KEYWORD),
        TokenSpec("TRY", token_types_list["TRY"], r"try\b", TokenCategory.KEYWORD),
        TokenSpec("CATCH", token_types_list["CATCH"], r"catch\b", TokenCategory.KEYWORD),
        TokenSpec("FINALLY", token_types_list["FINALLY"], r"finally\b", TokenCategory.KEYWORD),
        TokenSpec("THROW", token_types_list["THROW"], r"throw\b", TokenCategory.KEYWORD),
        TokenSpec("FUNCTION", token_types_list["FUNCTION"], r"fun\b", TokenCategory.KEYWORD),
        TokenSpec("CLASS", token_types_list["CLASS"], r"class\b", TokenCategory.KEYWORD),
        TokenSpec("INTERFACE", token_types_list["INTERFACE"], r"interface\b", TokenCategory.KEYWORD),
        TokenSpec("ENUM", token_types_list["ENUM"], r"enum\b", TokenCategory.KEYWORD),
        TokenSpec("VAR", token_types_list["VAR"], r"var\b", TokenCategory.KEYWORD),
        TokenSpec("VAL", token_types_list["VAL"], r"val\b", TokenCategory.KEYWORD),
        TokenSpec("CONST", token_types_list["CONST"], r"const\b", TokenCategory.KEYWORD),
        TokenSpec("STATIC", token_types_list["STATIC"], r"static\b", TokenCategory.KEYWORD),
        TokenSpec("PUBLIC", token_types_list["PUBLIC"], r"public\b", TokenCategory.KEYWORD),
        TokenSpec("PRIVATE", token_types_list["PRIVATE"], r"private\b", TokenCategory.KEYWORD),
        TokenSpec("PROTECTED", token_types_list["PROTECTED"], r"protected\b", TokenCategory.KEYWORD),
        TokenSpec("INTERNAL", token_types_list["INTERNAL"], r"internal\b", TokenCategory.KEYWORD),
        TokenSpec("NEW", token_types_list["NEW"], r"new\b", TokenCategory.KEYWORD),
        TokenSpec("THIS", token_types_list["THIS"], r"this\b", TokenCategory.KEYWORD),
        TokenSpec("SUPER", token_types_list["SUPER"], r"super\b", TokenCategory.KEYWORD),
        TokenSpec("INSTANCEOF", token_types_list["INSTANCEOF"], r"instanceof\b", TokenCategory.KEYWORD),
        TokenSpec("LAMBDA", token_types_list["LAMBDA"], r"lambda\b", TokenCategory.KEYWORD),
        TokenSpec("IMPORT", token_types_list["IMPORT"], r"import\b", TokenCategory.KEYWORD),
        TokenSpec("FROM", token_types_list["FROM"], r"from\b", TokenCategory.KEYWORD),
        TokenSpec("AS", token_types_list["AS"], r"as\b", TokenCategory.KEYWORD),
        TokenSpec("TRUE", token_types_list["TRUE"], r"true\b", TokenCategory.LITERAL),
        TokenSpec("FALSE", token_types_list["FALSE"], r"false\b", TokenCategory.LITERAL),
        TokenSpec("NULL", token_types_list["NULL"], r"null\b", TokenCategory.LITERAL),
        TokenSpec("PRINT", token_types_list["PRINT"], r"print\b", TokenCategory.KEYWORD),
        TokenSpec("INPUT", token_types_list["INPUT"], r"input\b", TokenCategory.KEYWORD),
        
        # Types
        TokenSpec("INT", token_types_list["INT"], r"int\b", TokenCategory.TYPE),
        TokenSpec("FLOAT", token_types_list["FLOAT"], r"float\b", TokenCategory.TYPE),
        TokenSpec("DOUBLE", token_types_list["DOUBLE"], r"double\b", TokenCategory.TYPE),
        TokenSpec("BOOLEAN", token_types_list["BOOLEAN"], r"boolean\b", TokenCategory.TYPE),
        TokenSpec("STRING_TYPE", token_types_list["STRING_TYPE"], r"string\b", TokenCategory.TYPE),
        TokenSpec("VOID", token_types_list["VOID"], r"void\b", TokenCategory.TYPE),
        TokenSpec("ANY", token_types_list["ANY"], r"any\b", TokenCategory.TYPE),
        
        # Operators
        TokenSpec("EQUAL", token_types_list["EQUAL"], r"==", TokenCategory.OPERATOR),
        TokenSpec("NOT_EQUAL", token_types_list["NOT_EQUAL"], r"!=", TokenCategory.OPERATOR),
        TokenSpec("LESS_EQUAL", token_types_list["LESS_EQUAL"], r"<=", TokenCategory.OPERATOR),
        TokenSpec("GREATER_EQUAL", token_types_list["GREATER_EQUAL"], r">=", TokenCategory.OPERATOR),
        TokenSpec("NULL_COALESCE", token_types_list["NULL_COALESCE"], r"\?\?", TokenCategory.OPERATOR),
        TokenSpec("ELVIS", token_types_list["ELVIS"], r"\?:", TokenCategory.OPERATOR),
        TokenSpec("INCREMENT", token_types_list["INCREMENT"], r"\+\+", TokenCategory.OPERATOR),
        TokenSpec("DECREMENT", token_types_list["DECREMENT"], r"--", TokenCategory.OPERATOR),
        TokenSpec("AND", token_types_list["AND"], r"and\b", TokenCategory.OPERATOR),
        TokenSpec("OR", token_types_list["OR"], r"or\b", TokenCategory.OPERATOR),
        TokenSpec("SHL", token_types_list["SHL"], r"<<", TokenCategory.OPERATOR),
        TokenSpec("SHR", token_types_list["SHR"], r">>", TokenCategory.OPERATOR),
        TokenSpec("BIT_AND", token_types_list["BIT_AND"], r"&", TokenCategory.OPERATOR),
        TokenSpec("BIT_OR", token_types_list["BIT_OR"], r"\|", TokenCategory.OPERATOR),
        TokenSpec("BIT_XOR", token_types_list["BIT_XOR"], r"\^", TokenCategory.OPERATOR),
        TokenSpec("ASSIGN", token_types_list["ASSIGN"], r"=", TokenCategory.OPERATOR),
        TokenSpec("LESS", token_types_list["LESS"], r"<", TokenCategory.OPERATOR),
        TokenSpec("GREATER", token_types_list["GREATER"], r">", TokenCategory.OPERATOR),
        TokenSpec("PLUS", token_types_list["PLUS"], r"\+", TokenCategory.OPERATOR),
        TokenSpec("MINUS", token_types_list["MINUS"], r"-", TokenCategory.OPERATOR),
        TokenSpec("MULTIPLY", token_types_list["MULTIPLY"], r"\*", TokenCategory.OPERATOR),
        TokenSpec("DIVIDE", token_types_list["DIVIDE"], r"/", TokenCategory.OPERATOR),
        TokenSpec("MODULO", token_types_list["MODULO"], r"%", TokenCategory.OPERATOR),
        TokenSpec("NOT", token_types_list["NOT"], r"!", TokenCategory.OPERATOR),
        TokenSpec("BIT_NOT", token_types_list["BIT_NOT"], r"~", TokenCategory.OPERATOR),
        
        # Punctuation
        TokenSpec("SEMICOLON", token_types_list["SEMICOLON"], r";", TokenCategory.PUNCTUATION),
        TokenSpec("COLON", token_types_list["COLON"], r":", TokenCategory.PUNCTUATION),
        TokenSpec("COMMA", token_types_list["COMMA"], r",", TokenCategory.PUNCTUATION),
        TokenSpec("DOT", token_types_list["DOT"], r"\.", TokenCategory.PUNCTUATION),
        TokenSpec("ARROW", token_types_list["ARROW"], r"->", TokenCategory.PUNCTUATION),
        TokenSpec("NULLABLE", token_types_list["NULLABLE"], r"\?", TokenCategory.PUNCTUATION),
        TokenSpec("LPAREN", token_types_list["LPAREN"], r"\(", TokenCategory.PUNCTUATION),
        TokenSpec("RPAREN", token_types_list["RPAREN"], r"\)", TokenCategory.PUNCTUATION),
        TokenSpec("LBRACE", token_types_list["LBRACE"], r"\{", TokenCategory.PUNCTUATION),
        TokenSpec("RBRACE", token_types_list["RBRACE"], r"\}", TokenCategory.PUNCTUATION),
        TokenSpec("LBRACKET", token_types_list["LBRACKET"], r"\[", TokenCategory.PUNCTUATION),
        TokenSpec("RBRACKET", token_types_list["RBRACKET"], r"\]", TokenCategory.PUNCTUATION),
        
        # Literals
        TokenSpec("NUMBER", token_types_list["NUMBER"], r"-?\d+(\.\d+)?([eE][+-]?\d+)?", TokenCategory.LITERAL),
        TokenSpec("STRING", token_types_list["STRING"], r'"[^"\n]*"', TokenCategory.LITERAL),  # Enforce double quotes
        TokenSpec("CHAR", token_types_list["CHAR"], r"'[^'\n]'", TokenCategory.LITERAL),
        
        # Whitespace and Comments
        TokenSpec("SPACE", token_types_list["SPACE"], r"[ \t\r\n]+", TokenCategory.WHITESPACE, skip=True),
        TokenSpec("COMMENT", token_types_list["COMMENT"], r"#.*?$|/\*[\s\S]*?\*/", TokenCategory.COMMENT, skip=True),
        
        # Identifiers (must be last to avoid matching keywords)
        TokenSpec("VARIABLE", token_types_list["VARIABLE"], r"[a-zA-Z_][a-zA-Z0-9_]*", TokenCategory.IDENTIFIER),
    ]

    def __init__(self, code: str, filename: str, debug: bool = False):
        self.code = code.rstrip() + "\n"  # Normalize input with trailing newline
        self.filename = filename
        self.pos = 0
        self.line = 1
        self.column = 1
        self.token_list: List[Token] = []
        self.debug = debug
        self.compiled_patterns: List[Tuple[TokenSpec, Pattern]] = [
            (spec, re.compile(r'^' + spec.regex, re.MULTILINE))
            for spec in self.TOKEN_SPECS
        ]

    def update_position(self, value: str) -> None:
        """Updates line and column numbers based on the token value."""
        lines = value.split('\n')
        for i, line in enumerate(lines):
            if i < len(lines) - 1:
                self.line += 1
                self.column = 1
            else:
                self.column += len(line)
        self.pos += len(value)

    def lex_analysis(self) -> List[Token]:
        """Tokenizes the source code and writes tokens to tokens.log."""
        try:
            while self.next_token():
                pass
            self.token_list.append(Token(token_types_list["EOF"], "", self.pos, self.line, self.column))
            with open('tokens.log', 'w', encoding='utf-8') as f:
                for token in self.token_list:
                    f.write(f"Token: type={token.type.name}, value={token.value!r}, line={token.line}, column={token.column}\n")
            # Validate tokens before returning
            self.validate_tokens()
            return [token for token in self.token_list if not self._is_skipped_token(token)]
        except LexerError as e:
            raise SyntaxError(str(e)) from e

    def next_token(self) -> bool:
        """Attempts to match the next token from the current position."""
        if self.pos >= len(self.code):
            return False

        substring = self.code[self.pos:]
        for spec, pattern in self.compiled_patterns:
            match = pattern.match(substring)
            if match:
                value = match.group(0)
                token = Token(spec.token_type, value, self.pos, self.line, self.column)
                self.token_list.append(token)
                if self.debug:
                    logger.info(f"Token created: {spec.name}('{value}') at {self.line}:{self.column}")
                self.update_position(value)
                return True

        char = self.code[self.pos]
        raise LexerError(format_error(
            "SyntaxError",
            f"Unexpected character '{char}'",
            self.filename,
            self.code,
            self.line,
            self.column,
            token_length=1
        ))

    def _is_skipped_token(self, token: Token) -> bool:
        """Determines if a token should be skipped in the output."""
        return any(spec.skip and spec.name == token.type.name for spec in self.TOKEN_SPECS)

    def validate_tokens(self) -> None:
        """Validates the token list for common issues (e.g., unclosed strings, empty identifiers)."""
        for token in self.token_list:
            if token.type.name == "STRING":
                if not token.value.startswith('"') or not token.value.endswith('"'):
                    raise LexerError(format_error(
                        "SyntaxError",
                        f"Unclosed or malformed string literal: {token.value}",
                        self.filename,
                        self.code,
                        token.line,
                        token.column,
                        token_length=len(token.value)
                    ))
            if token.type.name == "CHAR" and len(token.value) != 3:
                raise LexerError(format_error(
                    "SyntaxError",
                    "Invalid character literal",
                    self.filename,
                    self.code,
                    token.line,
                    token.column,
                    token_length=len(token.value)
                ))
            if token.type.name == "VARIABLE" and not token.value.strip():
                raise LexerError(format_error(
                    "SyntaxError",
                    "Empty variable identifier",
                    self.filename,
                    self.code,
                    token.line,
                    token.column,
                    token_length=1
                ))

    @classmethod
    def add_token_type(cls, name: str, regex: str, category: TokenCategory, skip: bool = False) -> None:
        """Dynamically adds a new token type to the lexer."""
        if name in token_types_list:
            raise ValueError(f"Token type '{name}' already exists")
        token_type = TokenType(name, regex)
        token_types_list[name] = token_type
        token_types[name] = regex
        cls.TOKEN_SPECS.append(TokenSpec(name, token_type, regex, category, skip))
        cls.TOKEN_SPECS.sort(key=lambda spec: (-len(spec.regex), spec.name))  # Prioritize longer patterns