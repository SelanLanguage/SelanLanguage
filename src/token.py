from typing import Dict

class TokenType:
    """Represents a token type with a name and regex pattern."""
    def __init__(self, name: str, regex: str):
        self.name = name
        self.regex = regex

    def __repr__(self):
        return f"TokenType(name='{self.name}', regex=r'{self.regex}')"

class Token:
    """Represents a token with type, value, position, and metadata."""
    def __init__(self, type: TokenType, value: str, position: int, line: int = 1, column: int = 1, is_nullable: bool = False):
        self.type = type
        self.value = value
        self.position = position
        self.line = line
        self.column = column
        self.is_nullable = is_nullable

    def __repr__(self):
        return f"Token(type={self.type.name}, value='{self.value}', pos={self.position}, line={self.line}, col={self.column}, nullable={self.is_nullable})"

# Token types for XenonLang, categorized for clarity and aligned with lexer/interpreter
token_types_list: Dict[str, TokenType] = {
    # === Literals ===
    'NUMBER': TokenType('NUMBER', r'-?(?:\d+\.\d+[eE][+-]?\d+|\d+\.\d+|\d+)(?!\w)'),  # Integers, floats, scientific notation
    'STRING': TokenType('STRING', r'"([^"\\]*(?:\\.[^"\\]*)*)"'),  # Double-quoted strings with escapes
    'RAW_STRING': TokenType('RAW_STRING', r'"""(?:[^"]|"[^"]|""[^"])*"""'),  # Multi-line raw strings
    'CHAR': TokenType('CHAR', r"'([^'\\]|\\.)'"),  # Single character literals
    'BYTE': TokenType('BYTE', r'0b[01]+(?!\w)'),  # Binary literals (e.g., 0b1010)
    'HEX': TokenType('HEX', r'0x[0-9a-fA-F]+(?!\w)'),  # Hexadecimal literals
    'TRUE': TokenType('TRUE', r'true'),  # Boolean true
    'FALSE': TokenType('FALSE', r'false'),  # Boolean false
    'NULL': TokenType('NULL', r'null'),  # Null literal

    # === Identifiers ===
    'VARIABLE': TokenType('VARIABLE', r'[a-zA-Z_][a-zA-Z0-9_]*(?!\w)'),  # Keywords handled by lexer

    # === Operators ===
    'ASSIGN': TokenType('ASSIGN', r'='),
    'EQUAL': TokenType('EQUAL', r'=='),
    'NOT_EQUAL': TokenType('NOT_EQUAL', r'!='),
    'LESS': TokenType('LESS', r'<'),
    'GREATER': TokenType('GREATER', r'>'),
    'LESS_EQUAL': TokenType('LESS_EQUAL', r'<='),
    'GREATER_EQUAL': TokenType('GREATER_EQUAL', r'>='),
    'PLUS': TokenType('PLUS', r'\+'),
    'MINUS': TokenType('MINUS', r'-'),
    'MULTIPLY': TokenType('MULTIPLY', r'\*'),
    'DIVIDE': TokenType('DIVIDE', r'/'),
    'MODULO': TokenType('MODULO', r'%'),
    'AND': TokenType('AND', r'&&'),
    'OR': TokenType('OR', r'\|\|'),
    'NOT': TokenType('NOT', r'!'),
    'BIT_AND': TokenType('BIT_AND', r'&'),
    'BIT_OR': TokenType('BIT_OR', r'\|'),
    'BIT_XOR': TokenType('BIT_XOR', r'\^'),
    'BIT_NOT': TokenType('BIT_NOT', r'~'),
    'SHL': TokenType('SHL', r'<<'),
    'SHR': TokenType('SHR', r'>>'),
    'NULL_COALESCE': TokenType('NULL_COALESCE', r'\?\?'),
    'ELVIS': TokenType('ELVIS', r'\?:'),
    'INCREMENT': TokenType('INCREMENT', r'\+\+'),
    'DECREMENT': TokenType('DECREMENT', r'--'),
    'PLUS_ASSIGN': TokenType('PLUS_ASSIGN', r'\+='),
    'MINUS_ASSIGN': TokenType('MINUS_ASSIGN', r'-='),
    'MULTIPLY_ASSIGN': TokenType('MULTIPLY_ASSIGN', r'\*='),
    'DIVIDE_ASSIGN': TokenType('DIVIDE_ASSIGN', r'/='),
    'MODULO_ASSIGN': TokenType('MODULO_ASSIGN', r'%='),
    'BIT_AND_ASSIGN': TokenType('BIT_AND_ASSIGN', r'&='), 
    'BIT_OR_ASSIGN': TokenType('BIT_OR_ASSIGN', r'\|='),
    'BIT_XOR_ASSIGN': TokenType('BIT_XOR_ASSIGN', r'\^='),

    # === Delimiters ===
    'SEMICOLON': TokenType('SEMICOLON', r';'),
    'COLON': TokenType('COLON', r':'),
    'COMMA': TokenType('COMMA', r','),
    'DOT': TokenType('DOT', r'\.'),
    'ARROW': TokenType('ARROW', r'->'),
    'DOUBLE_COLON': TokenType('DOUBLE_COLON', r'::'),
    'ANNOTATION': TokenType('ANNOTATION', r'@[a-zA-Z_][a-zA-Z0-9_]*(?!\w)'),
    'NULLABLE': TokenType('NULLABLE', r'\?'),

    # === Brackets ===
    'LPAREN': TokenType('LPAREN', r'\('),
    'RPAREN': TokenType('RPAREN', r'\)'),
    'LBRACE': TokenType('LBRACE', r'{'),
    'RBRACE': TokenType('RBRACE', r'}'),
    'LBRACKET': TokenType('LBRACKET', r'\['),
    'RBRACKET': TokenType('RBRACKET', r'\]'),

    # === Control Flow Keywords ===
    'IF': TokenType('IF', r'if'),
    'ELSE': TokenType('ELSE', r'else'),
    'WHEN': TokenType('WHEN', r'when'),
    'WHILE': TokenType('WHILE', r'while'),
    'FOR': TokenType('FOR', r'for'),
    'DO': TokenType('DO', r'do'),
    'SWITCH': TokenType('SWITCH', r'switch'),
    'CASE': TokenType('CASE', r'case'),
    'DEFAULT': TokenType('DEFAULT', r'default'),
    'BREAK': TokenType('BREAK', r'break'),
    'CONTINUE': TokenType('CONTINUE', r'continue'),
    'RETURN': TokenType('RETURN', r'return'),
    'YIELD': TokenType('YIELD', r'yield'),
    'TRY': TokenType('TRY', r'try'),
    'CATCH': TokenType('CATCH', r'catch'),
    'FINALLY': TokenType('FINALLY', r'finally'),
    'THROW': TokenType('THROW', r'throw'),
    'ASSERT': TokenType('ASSERT', r'assert'),
    'PRINT': TokenType('PRINT', r'print'), 

    # === Definition Keywords ===
    'FUNCTION': TokenType('FUNCTION', r'fun'),
    'CLASS': TokenType('CLASS', r'class'),
    'INTERFACE': TokenType('INTERFACE', r'interface'),
    'ENUM': TokenType('ENUM', r'enum'),
    'STRUCT': TokenType('STRUCT', r'struct'),
    'DATA': TokenType('DATA', r'data'),
    'SEALED': TokenType('SEALED', r'sealed'),
    'ABSTRACT': TokenType('ABSTRACT', r'abstract'),
    'VAR': TokenType('VAR', r'var'),
    'VAL': TokenType('VAL', r'val'),
    'CONST': TokenType('CONST', r'const'),
    'STATIC': TokenType('STATIC', r'static'),
    'NEW': TokenType('NEW', r'new'),
    'INLINE': TokenType('INLINE', r'inline'),
    'COMPANION': TokenType('COMPANION', r'companion'),
    'OBJECT': TokenType('OBJECT', r'object'),
    'LAMBDA': TokenType('LAMBDA', r'lambda'),

    # === Access Modifiers ===
    'PUBLIC': TokenType('PUBLIC', r'public'),
    'PRIVATE': TokenType('PRIVATE', r'private'),
    'PROTECTED': TokenType('PROTECTED', r'protected'),
    'INTERNAL': TokenType('INTERNAL', r'internal'),

    # === Type Declarations ===
    'INT': TokenType('INT', r'Int'),
    'FLOAT': TokenType('FLOAT', r'Float'),
    'DOUBLE': TokenType('DOUBLE', r'Double'),
    'BOOLEAN': TokenType('BOOLEAN', r'Boolean'),
    'STRING_TYPE': TokenType('STRING_TYPE', r'String'),
    'VOID': TokenType('VOID', r'void'),
    'ANY': TokenType('ANY', r'Any'),
    'UNIT': TokenType('UNIT', r'Unit'),
    'NOTHING': TokenType('NOTHING', r'Nothing'),
    'LIST': TokenType('LIST', r'List'),
    'MAP': TokenType('MAP', r'Map'),
    'SET': TokenType('SET', r'Set'),
    'ARRAY': TokenType('ARRAY', r'Array'),

    # === Memory Management ===
    'DEREF': TokenType('DEREF', r'\*'),  
    'ALLOC': TokenType('ALLOC', r'alloc'),
    'FREE': TokenType('FREE', r'free'),
    'REFERENCE': TokenType('REFERENCE', r'&'),

    # === Input/Output ===
    'INPUT': TokenType('INPUT', r'input'),
    'READLINE': TokenType('READLINE', r'readline'),

    # === Modules and Imports ===
    'IMPORT': TokenType('IMPORT', r'import'),
    'FROM': TokenType('FROM', r'from'),
    'AS': TokenType('AS', r'as'),
    'PACKAGE': TokenType('PACKAGE', r'package'),
    'NAMESPACE': TokenType('NAMESPACE', r'namespace'),
    'EXPORT': TokenType('EXPORT', r'export'),

    # === Async and Networking ===
    'ASYNC': TokenType('ASYNC', r'async'),
    'AWAIT': TokenType('AWAIT', r'await'),
    'DATETIME': TokenType('DATETIME', r'datetime'),
    'TIMEDELTA': TokenType('TIMEDELTA', r'timedelta'),
    'NOW': TokenType('NOW', r'now'),
    'SOCKET': TokenType('SOCKET', r'socket'),
    'HTTP': TokenType('HTTP', r'http'),

    # === Other Keywords ===
    'THIS': TokenType('THIS', r'this'),
    'SUPER': TokenType('SUPER', r'super'),
    'INSTANCEOF': TokenType('INSTANCEOF', r'instanceof'),

    # === Whitespace and Comments ===
    'SPACE': TokenType('SPACE', r'[ \n\t\r]+'),
    'COMMENT': TokenType('COMMENT', r'(?:#|//).*'),
    'BLOCK_COMMENT': TokenType('BLOCK_COMMENT', r'/\*[\s\S]*?\*/'),
}

# Lookup dictionary for compatibility with parser.py
token_types: Dict[str, TokenType] = {name: token_type for name, token_type in token_types_list.items()}