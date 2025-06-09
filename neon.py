import sys
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter
# from src.compiler import Compiler

def interpret(filename):
    """Interprets a Neon source file."""
    if not filename.endswith(".ne"):
        print("Error: Expected a file with .ne extension.")
        sys.exit(1)
    
    try:
        with open(filename, 'r', encoding='utf-8') as file:
            source = file.read()
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
        sys.exit(1)

    try:
        lexer = Lexer(source, filename)
        tokens = lexer.lex_analysis()
        parser = Parser(tokens, source, filename)
        root_node = parser.parse()
        interpreter = Interpreter(source, filename)
        interpreter.interpret(root_node)
    except Exception as e:
        print(e)
        sys.exit(1)

def compile_code():
    """Placeholder for compilation functionality."""
    print("🔧 Compilation not implemented yet.")

def main():
    """Main entry point for the Neon compiler."""
    if len(sys.argv) >= 3 and sys.argv[1] == "-i":
        filename = sys.argv[2]
        interpret(filename)
    elif len(sys.argv) == 1:
        compile_code()
    else:
        print("Usage:")
        print("  Interpret: python neon.py -i <filename.ne>")
        print("  Compile:   python neon.py")
        sys.exit(1)

if __name__ == "__main__":
    main()