# token.py
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

# Extended token types list covering Python, Kotlin, and additional features
token_types_list = {
    # === Literals ===
    'NUMBER': TokenType("NUMBER", r'-?\d+(\.\d+)?([eE][+-]?\d+)?'),  # Integers, decimals, scientific notation
    'STRING': TokenType("STRING", r'\"[^\"\n]*\"|\'[^\'\n]*\''),  # Double or single-quoted strings
    'CHAR': TokenType("CHAR", r'\'[^\']?\''),  # Single character literals
    'RAW_STRING': TokenType("RAW_STRING", r'r\"[^\"]*\"|r\'[^\']*\'|\"\"\"[^\"]*\"\"\"'),  # Raw strings and multi-line (Python/Kotlin style)
    'BYTE': TokenType("BYTE", r'\b0b[01]+\b'),  # Binary literals (e.g., 0b1010)
    'HEX': TokenType("HEX", r'\b0x[0-9a-fA-F]+\b'),  # Hexadecimal literals
    'TRUE': TokenType("TRUE", r'\btrue\b'),  # Boolean true
    'FALSE': TokenType("FALSE", r'\bfalse\b'),  # Boolean false
    'NULL': TokenType("NULL", r'\bnull\b'),  # Null literal

    # === Identifiers ===
   'VARIABLE': TokenType("VARIABLE", r'\b[a-zA-Z_][a-zA-Z0-9_]*\b(?!\s*(?:class|fun|var|val|const|if|else|while|do|for|switch|case|default|try|catch|finally|throw|return|break|continue|print|new|true|false|null|import|from|as|interface|enum|instanceof|public|private|protected|internal|int|float|double|boolean|string|void|any|static|this|super|lambda|struct|pointer|deref|alloc|free|async|await|yield|data|sealed|inline|companion|object|package|namespace|export|input|readline|datetime|timedelta|socket|http|print))'),

    # === Operators ===
    'ASSIGN': TokenType("ASSIGN", r'='),  # Assignment
    'EQUAL': TokenType("EQUAL", r'=='),  # Equality comparison
    'NOT_EQUAL': TokenType("NOT_EQUAL", r'!='),  # Inequality comparison
    'LESS': TokenType("LESS", r'<'),  # Less than
    'GREATER': TokenType("GREATER", r'>'),  # Greater than
    'LESS_EQUAL': TokenType("LESS_EQUAL", r'<='),  # Less than or equal
    'GREATER_EQUAL': TokenType("GREATER_EQUAL", r'>='),  # Greater than or equal
    'PLUS': TokenType("PLUS", r'\+'),  # Addition
    'MINUS': TokenType("MINUS", r'-'),  # Subtraction
    'MULTIPLY': TokenType("MULTIPLY", r'\*'),  # Multiplication
    'DIVIDE': TokenType("DIVIDE", r'/'),  # Division
    'MODULO': TokenType("MODULO", r'%'),  # Modulo
    'AND': TokenType("AND", r'&&'),  # Logical AND
    'OR': TokenType("OR", r'\|\|'),  # Logical OR
    'NOT': TokenType("NOT", r'!'),  # Logical NOT
    'BIT_AND': TokenType("BIT_AND", r'&'),  # Bitwise AND
    'BIT_OR': TokenType("BIT_OR", r'\|'),  # Bitwise OR
    'BIT_XOR': TokenType("BIT_XOR", r'\^'),  # Bitwise XOR
    'BIT_NOT': TokenType("BIT_NOT", r'~'),  # Bitwise NOT
    'SHL': TokenType("SHL", r'<<'),  # Shift left
    'SHR': TokenType("SHR", r'>>'),  # Shift right
    'ROTL': TokenType("ROTL", r'<<>'),  # Bitwise rotate left
    'ROTR': TokenType("ROTR", r'>>='),  # Bitwise rotate right
    'NULL_COALESCE': TokenType("NULL_COALESCE", r'\?\?'),  # Null coalescing (Kotlin)
    'ELVIS': TokenType("ELVIS", r'\?:'),  # Elvis operator (Kotlin)
    'INCREMENT': TokenType("INCREMENT", r'\+\+'),  # Increment
    'DECREMENT': TokenType("DECREMENT", r'--'),  # Decrement
    'PLUS_ASSIGN': TokenType("PLUS_ASSIGN", r'\+='),  # += operator
    'MINUS_ASSIGN': TokenType("MINUS_ASSIGN", r'-='),  # -= operator
    'MULTIPLY_ASSIGN': TokenType("MULTIPLY_ASSIGN", r'\*='),  # *= operator
    'DIVIDE_ASSIGN': TokenType("DIVIDE_ASSIGN", r'/='),  # /= operator
    'MODULO_ASSIGN': TokenType("MODULO_ASSIGN", r'%='),  # %= operator
    'BIT_AND_ASSIGN': TokenType("BIT_AND_ASSIGN", r'&='),  # &= operator
    'BIT_OR_ASSIGN': TokenType("BIT_OR_ASSIGN", r'\|='),  # |= operator
    'BIT_XOR_ASSIGN': TokenType("BIT_XOR_ASSIGN", r'\^='),  # ^= operator

    # === Delimiters ===
    'SEMICOLON': TokenType("SEMICOLON", r';'),  # Statement terminator
    'COLON': TokenType("COLON", r':'),  # Type annotations, inheritance
    'COMMA': TokenType("COMMA", r','),  # Separator
    'DOT': TokenType("DOT", r'\.'),  # Member access
    'ARROW': TokenType("ARROW", r'->'),  # Lambda or function types
    'DOUBLE_COLON': TokenType("DOUBLE_COLON", r'::'),  # Kotlin-style class reference
    'AT': TokenType("AT", r'@'),  # Annotations/decorators
    'QUESTION_MARK': TokenType("QUESTION_MARK", r'\?'),  # Nullable types or ternary-like ops

    # === Brackets ===
    'LPAREN': TokenType("LPAREN", r'\('),  # Left parenthesis
    'RPAREN': TokenType("RPAREN", r'\)'),  # Right parenthesis
    'LBRACE': TokenType("LBRACE", r'\{'),  # Left brace
    'RBRACE': TokenType("RBRACE", r'\}'),  # Right brace
    'LBRACKET': TokenType("LBRACKET", r'\['),  # Left bracket
    'RBRACKET': TokenType("RBRACKET", r'\]'),  # Right bracket

    # === Keywords (Control Flow) ===
    'IF': TokenType("IF", r'\bif\b'),  # If statement
    'ELSE': TokenType("ELSE", r'\belse\b'),  # Else statement
    'WHEN': TokenType("WHEN", r'\bwhen\b'),  # Kotlin-style when (like switch)
    'WHILE': TokenType("WHILE", r'\bwhile\b'),  # While loop
    'FOR': TokenType("FOR", r'\bfor\b'),  # For loop
    'DO': TokenType("DO", r'\bdo\b'),  # Do-while loop
    'SWITCH': TokenType("SWITCH", r'\bswitch\b'),  # Switch statement
    'CASE': TokenType("CASE", r'\bcase\b'),  # Case in switch
    'DEFAULT': TokenType("DEFAULT", r'\bdefault\b'),  # Default in switch
    'BREAK': TokenType("BREAK", r'\bbreak\b'),  # Break statement
    'CONTINUE': TokenType("CONTINUE", r'\bcontinue\b'),  # Continue statement
    'RETURN': TokenType("RETURN", r'\breturn\b'),  # Return statement
    'YIELD': TokenType("YIELD", r'\byield\b'),  # Generator yield (Python)
    'TRY': TokenType("TRY", r'\btry\b'),  # Try block
    'CATCH': TokenType("CATCH", r'\bcatch\b'),  # Catch block
    'FINALLY': TokenType("FINALLY", r'\bfinally\b'),  # Finally block
    'THROW': TokenType("THROW", r'\bthrow\b'),  # Throw exception
    'ASSERT': TokenType("ASSERT", r'\bassert\b'),  # Assert statement (Python)

    # === Keywords (Definitions) ===
    'FUNCTION': TokenType("FUNCTION", r'\bfun\b'),  # Function definition
    'CLASS': TokenType("CLASS", r'\bclass\b'),  # Class definition
    'INTERFACE': TokenType("INTERFACE", r'\binterface\b'),  # Interface definition
    'ENUM': TokenType("ENUM", r'\benum\b'),  # Enum definition
    'STRUCT': TokenType("STRUCT", r'\bstruct\b'),  # Struct definition
    'DATA': TokenType("DATA", r'\bdata\b'),  # Kotlin data class
    'SEALED': TokenType("SEALED", r'\bsealed\b'),  # Kotlin sealed class
    'ABSTRACT': TokenType("ABSTRACT", r'\babstract\b'),  # Abstract class/method
    'VAR': TokenType("VAR", r'\bvar\b'),  # Mutable variable
    'VAL': TokenType("VAL", r'\bval\b'),  # Immutable variable
    'CONST': TokenType("CONST", r'\bconst\b'),  # Constant declaration
    'STATIC': TokenType("STATIC", r'\bstatic\b'),  # Static member
    'NEW': TokenType("NEW", r'\bnew\b'),  # Object instantiation
    'INLINE': TokenType("INLINE", r'\binline\b'),  # Kotlin inline functions
    'COMPANION': TokenType("COMPANION", r'\bcompanion\b'),  # Kotlin companion object
    'OBJECT': TokenType("OBJECT", r'\bobject\b'),  # Kotlin object declaration
    'LAMBDA': TokenType("LAMBDA", r'\blambda\b'),  # Lambda expression

    # === Access Modifiers ===
    'PUBLIC': TokenType("PUBLIC", r'\bpublic\b'),  # Public access
    'PRIVATE': TokenType("PRIVATE", r'\bprivate\b'),  # Private access
    'PROTECTED': TokenType("PROTECTED", r'\bprotected\b'),  # Protected access
    'INTERNAL': TokenType("INTERNAL", r'\binternal\b'),  # Internal access

    # === Type Declarations ===
    'INT': TokenType("INT", r'\bint\b'),  # Integer type
    'FLOAT': TokenType("FLOAT", r'\bfloat\b'),  # Float type
    'DOUBLE': TokenType("DOUBLE", r'\bdouble\b'),  # Double type
    'BOOLEAN': TokenType("BOOLEAN", r'\bboolean\b'),  # Boolean type
    'STRING_TYPE': TokenType("STRING_TYPE", r'\bstring\b'),  # String type
    'VOID': TokenType("VOID", r'\bvoid\b'),  # Void type
    'ANY': TokenType("ANY", r'\bany\b'),  # Any type
    'UNIT': TokenType("UNIT", r'\bUnit\b'),  # Kotlin Unit type
    'NOTHING': TokenType("NOTHING", r'\bNothing\b'),  # Kotlin Nothing type
    'LIST': TokenType("LIST", r'\bList\b'),  # List type
    'MAP': TokenType("MAP", r'\bMap\b'),  # Map type
    'SET': TokenType("SET", r'\bSet\b'),  # Set type
    'ARRAY': TokenType("ARRAY", r'\bArray\b'),  # Array type
    'NULLABLE': TokenType("NULLABLE", r'\?'),  # Nullable type modifier

    # === Memory Management ===
    'POINTER': TokenType("POINTER", r'\*'),  # Pointer type (e.g., *int)
    'DEREF': TokenType("DEREF", r'\*'),  # Dereference operator
    'ALLOC': TokenType("ALLOC", r'\balloc\b'),  # Memory allocation
    'FREE': TokenType("FREE", r'\bfree\b'),  # Memory deallocation
    'REFERENCE': TokenType("REFERENCE", r'&'),  # Reference operator

    # === Input/Output ===
    'INPUT': TokenType("INPUT", r'\binput\b'),  # Input function (Python-style)
    'READLINE': TokenType("READLINE", r'\breadline\b'),  # Read line input

    # === Modules and Imports ===
    'IMPORT': TokenType("IMPORT", r'\bimport\b'),  # Import statement
    'FROM': TokenType("FROM", r'\bfrom\b'),  # From clause
    'AS': TokenType("AS", r'\bas\b'),  # Alias for imports
    'PACKAGE': TokenType("PACKAGE", r'\bpackage\b'),  # Package declaration
    'NAMESPACE': TokenType("NAMESPACE", r'\bnamespace\b'),  # Namespace declaration
    'EXPORT': TokenType("EXPORT", r'\bexport\b'),  # Export for modules

    # === Time and Networking ===
    'DATETIME': TokenType("DATETIME", r'\bdatetime\b'),  # Datetime type/function
    'TIMEDELTA': TokenType("TIMEDELTA", r'\btimedelta\b'),  # Timedelta type
    'NOW': TokenType("NOW", r'\bnow\b'),  # Current time function
    'SOCKET': TokenType("SOCKET", r'\bsocket\b'),  # Socket operations
    'HTTP': TokenType("HTTP", r'\bhttp\b'),  # HTTP operations
    'ASYNC': TokenType("ASYNC", r'\basync\b'),  # Async function (Kotlin/Python)
    'AWAIT': TokenType("AWAIT", r'\bawait\b'),  # Await for coroutines

    # === Other Keywords ===
    'THIS': TokenType("THIS", r'\bthis\b'),  # This reference
    'SUPER': TokenType("SUPER", r'\bsuper\b'),  # Super reference
    'INSTANCEOF': TokenType("INSTANCEOF", r'\binstanceof\b'),  # Instanceof operator
    'ANNOTATION': TokenType("ANNOTATION", r'@\b[a-zA-Z_][a-zA-Z0-9_]*\b'),  # Annotations (Kotlin-style)
    'DECORATOR': TokenType("DECORATOR", r'@\b[a-zA-Z_][a-zA-Z0-9_]*\b'),  # Python-style decorators

    # === Whitespace and Comments ===
    'SPACE': TokenType("SPACE", r'[ \n\t\r]+'),  # Whitespace
    'COMMENT': TokenType("COMMENT", r'#.*'),  # Single-line comment
    'MULTILINE_COMMENT': TokenType("MULTILINE_COMMENT", r'/\*[\s\S]*?\*/'),  # Multi-line comment
}

# Create a lookup dictionary for quick access
token_types: Dict[str, TokenType] = {name: token_type for name, token_type in token_types_list.items()}
