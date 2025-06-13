import sys
from selanc.src.lexer import Lexer
from selanc.src.parser import Parser
from selanc.src.interpreter import Interpreter
# from selanc.src.compiler import Compiler

def interpret(filename):
    """Interprets a Neon source file."""
    if not filename.endswith(".selan") and not filename.endswith(".se"):
        print("Error: Expected a file with .selan or .se extension.")
        sys.exit(1)
    
    try:
        with open(filename, 'r', encoding='utf-8') as file:
            source = file.read()
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
        sys.exit(1)

    lexer = Lexer(source, filename)
    tokens = lexer.lex_analysis()
    parser = Parser(tokens, source, filename)
    root_node = parser.parse()
    interpreter = Interpreter(source, filename)
    interpreter.interpret(root_node)

def compile_code():
    """Placeholder for compilation functionality."""
    print("🔧 Compilation not implemented yet.")

def main():
    """Main entry point for the Selan compiler."""
    if len(sys.argv) >= 3 and sys.argv[1] == "-i":
        filename = sys.argv[2]
        try:
            interpret(filename)
        except (SyntaxError, RuntimeError, TypeError) as e:
            print(str(e))
            sys.exit(1)
        except KeyboardInterrupt:
            print("Error: Program interrupted by user")
            sys.exit(1)
        except FileNotFoundError:
            print(f"Error: File '{filename}' not found.")
            sys.exit(1)
    elif len(sys.argv) == 1:
        compile_code()
    else:
        print("Usage:")
        print("  Interpret: python selan.py -i <filename.selan>")
        print("  Compile:   python selan.py -c <filename.selan>")
        sys.exit(1)

if __name__ == "__main__":
    main()