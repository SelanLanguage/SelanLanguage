from typing import Dict

class TokenType:
    def __init__(self, name: str, regex: str):
        self.name = name
        self.regex = regex

    def __repr__(self):
        return f"TokenType(name='{self.name}', regex=r'{self.regex}')"

class Token:
    def __init__(self, type: TokenType, value: str, position: int, line: int = 1, column: int = 1, is_nullable: bool = False):
        self.type = type
        self.value = value
        self.position = position
        self.line = line
        self.column = column
        self.is_nullable = is_nullable

    def __repr__(self):
        return f"Token(type={self.type.name}, value='{self.value}', pos={self.position}, line={self.line}, col={self.column}, nullable={self.is_nullable})"

# Token types definition
token_types_list: Dict[str, TokenType] = {
    # Literals
    'NUMBER': TokenType("NUMBER", r'-?\d+(\.\d+)?([eE][+-]?\d+)?'),
    'STRING': TokenType("STRING", r'\"[^\"\n]*\"'),  # Double-quoted strings only
    'CHAR': TokenType("CHAR", r'\'[^\'\n]\''),
    'VARIABLE': TokenType("VARIABLE", r'\b[a-zA-Z_][a-zA-Z0-9_]*\b(?!\s*(?:class|fun|var|val|const|if|else|while|do|for|switch|case|default|try|catch|finally|throw|return|break|continue|print|input|new|true|false|null|import|from|as|interface|enum|instanceof|public|private|protected|internal|int|float|double|boolean|string|void|any|static|this|super|lambda))'),
    'TRUE': TokenType("TRUE", r'\btrue\b'),
    'FALSE': TokenType("FALSE", r'\bfalse\b'),
    'NULL': TokenType("NULL", r'\bnull\b'),

    # Operators
    'ASSIGN': TokenType("ASSIGN", r'='),
    'EQUAL': TokenType("EQUAL", r'=='),
    'NOT_EQUAL': TokenType("NOT_EQUAL", r'!='),
    'LESS': TokenType("LESS", r'<'),
    'GREATER': TokenType("GREATER", r'>'),
    'LESS_EQUAL': TokenType("LESS_EQUAL", r'<='),
    'GREATER_EQUAL': TokenType("GREATER_EQUAL", r'>='),
    'PLUS': TokenType("PLUS", r'\+'),
    'MINUS': TokenType("MINUS", r'-'),
    'MULTIPLY': TokenType("MULTIPLY", r'\*'),
    'DIVIDE': TokenType("DIVIDE", r'/'),
    'MODULO': TokenType("MODULO", r'%'),
    'AND': TokenType("AND", r'&&'),
    'OR': TokenType("OR", r'\|\|'),
    'NOT': TokenType("NOT", r'!'),
    'BIT_AND': TokenType("BIT_AND", r'&'),
    'BIT_OR': TokenType("BIT_OR", r'\|'),
    'BIT_XOR': TokenType("BIT_XOR", r'\^'),
    'BIT_NOT': TokenType("BIT_NOT", r'~'),
    'SHL': TokenType("SHL", r'<<'),
    'SHR': TokenType("SHR", r'>>'),
    'NULL_COALESCE': TokenType("NULL_COALESCE", r'\?\?'),
    'ELVIS': TokenType("ELVIS", r'\?:'),
    'INCREMENT': TokenType("INCREMENT", r'\+\+'),
    'DECREMENT': TokenType("DECREMENT", r'--'),

    # Delimiters
    'SEMICOLON': TokenType("SEMICOLON", r';'),
    'COLON': TokenType("COLON", r':'),
    'COMMA': TokenType("COMMA", r','),
    'DOT': TokenType("DOT", r'\.'),
    'ARROW': TokenType("ARROW", r'->'),

    # Brackets
    'LPAREN': TokenType("LPAREN", r'\('),
    'RPAREN': TokenType("RPAREN", r'\)'),
    'LBRACE': TokenType("LBRACE", r'\{'),
    'RBRACE': TokenType("RBRACE", r'\}'),
    'LBRACKET': TokenType("LBRACKET", r'\['),
    'RBRACKET': TokenType("RBRACKET", r'\]'),

    # Keywords (Control Flow)
    'IF': TokenType("IF", r'\bif\b'),
    'ELSE': TokenType("ELSE", r'\belse\b'),
    'WHILE': TokenType("WHILE", r'\bwhile\b'),
    'FOR': TokenType("FOR", r'\bfor\b'),
    'DO': TokenType("DO", r'\bdo\b'),
    'SWITCH': TokenType("SWITCH", r'\bswitch\b'),
    'CASE': TokenType("CASE", r'\bcase\b'),
    'DEFAULT': TokenType("DEFAULT", r'\bdefault\b'),
    'BREAK': TokenType("BREAK", r'\bbreak\b'),
    'CONTINUE': TokenType("CONTINUE", r'\bcontinue\b'),
    'RETURN': TokenType("RETURN", r'\breturn\b'),
    'TRY': TokenType("TRY", r'\btry\b'),
    'CATCH': TokenType("CATCH", r'\bcatch\b'),
    'FINALLY': TokenType("FINALLY", r'\bfinally\b'),
    'THROW': TokenType("THROW", r'\bthrow\b'),

    # Keywords (Definitions)
    'PRINT': TokenType("PRINT", r'\bprint\b'),
    'INPUT': TokenType("INPUT", r'\binput\b'),
    'FUNCTION': TokenType("FUNCTION", r'\bfun\b'),
    'CLASS': TokenType("CLASS", r'\bclass\b'),
    'INTERFACE': TokenType("INTERFACE", r'\binterface\b'),
    'ENUM': TokenType("ENUM", r'\benum\b'),
    'VAR': TokenType("VAR", r'\bvar\b'),
    'VAL': TokenType("VAL", r'\bval\b'),
    'CONST': TokenType("CONST", r'\bconst\b'),
    'STATIC': TokenType("STATIC", r'\bstatic\b'),
    'NEW': TokenType("NEW", r'\bnew\b'),

    # Access Modifiers
    'PUBLIC': TokenType("PUBLIC", r'\bpublic\b'),
    'PRIVATE': TokenType("PRIVATE", r'\bprivate\b'),
    'PROTECTED': TokenType("PROTECTED", r'\bprotected\b'),
    'INTERNAL': TokenType("INTERNAL", r'\binternal\b'),

    # Type Declarations
    'INT': TokenType("INT", r'\bint\b'),
    'FLOAT': TokenType("FLOAT", r'\bfloat\b'),
    'DOUBLE': TokenType("DOUBLE", r'\bdouble\b'),
    'BOOLEAN': TokenType("BOOLEAN", r'\bboolean\b'),
    'STRING_TYPE': TokenType("STRING_TYPE", r'\bstring\b'),
    'VOID': TokenType("VOID", r'\bvoid\b'),
    'ANY': TokenType("ANY", r'\bany\b'),
    'NULLABLE': TokenType("NULLABLE", r'\?'),

    # Other Keywords
    'IMPORT': TokenType("IMPORT", r'\bimport\b'),
    'FROM': TokenType("FROM", r'\bfrom\b'),
    'AS': TokenType("AS", r'\bas\b'),
    'THIS': TokenType("THIS", r'\bthis\b'),
    'SUPER': TokenType("SUPER", r'\bsuper\b'),
    'INSTANCEOF': TokenType("INSTANCEOF", r'\binstanceof\b'),
    'LAMBDA': TokenType("LAMBDA", r'\blambda\b'),

    # Whitespace and Comments
    'SPACE': TokenType("SPACE", r'[ \n\t\r]+'),
    'COMMENT': TokenType("COMMENT", r'#.*'),
    'EOF': TokenType("EOF", r''),
}

# Create a lookup dictionary for quick access
token_types: Dict[str, str] = {name: token_type.regex for name, token_type in token_types_list.items()}