from abc import ABC
from typing import List, Optional, Any, Dict, Union, Callable
import sys
import logging
from dataclasses import dataclass
from enum import Enum
from .token import Token, TokenType, token_types_list
from .ast import *
from .error import format_error

# Configure logging for detailed debugging
logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

class TypeCategory(Enum):
    """Categories for type tokens to enhance type checking."""
    PRIMITIVE = "primitive"
    CLASS = "class"
    INTERFACE = "interface"
    ANY = "any"
    VOID = "void"

@dataclass
class VariableInfo:
    """Metadata for a variable, including value, type, and properties."""
    value: Any
    type_token: Optional[Token]
    is_const: bool = False
    is_nullable: bool = False

@dataclass
class ClassInstance:
    """Represents a class instance with its members and class name."""
    class_name: str
    members: Dict[str, VariableInfo]
    superclass: Optional["ClassInstance"] = None

class InterpreterError(Exception):
    """Custom exception for interpreter errors."""
    pass

class Interpreter:
    """Interprets an AST for the Selan language, supporting classes, methods, and type checking."""
    
    def __init__(self, source: str, filename: str, debug: bool = True):
        """Initialize the interpreter with source code and filename."""
        self.source = source
        self.filename = filename
        self.scopes: List[Dict[str, VariableInfo]] = [{}]  # Stack of variable scopes
        self.functions: Dict[str, FunctionDefNode] = {}  # Function definitions
        self.classes: Dict[str, ClassNode] = {}  # Class definitions
        self.interfaces: Dict[str, InterfaceNode] = {}  # Interface definitions
        self.enums: Dict[str, EnumNode] = {}  # Enum definitions
        self.imports: Dict[str, str] = {}  # Import mappings
        self.current_class: Optional[str] = None  # Current class being interpreted
        self.super_class_stack: List[str] = []  # Stack of superclasses
        self.current_instance: Optional[ClassInstance] = None  # Current class instance
        self.debug = debug
        self.type_map: Dict[str, TypeCategory] = {
            "int": TypeCategory.PRIMITIVE,
            "float": TypeCategory.PRIMITIVE,
            "double": TypeCategory.PRIMITIVE,
            "boolean": TypeCategory.PRIMITIVE,
            "string": TypeCategory.PRIMITIVE,
            "str": TypeCategory.PRIMITIVE,  # Alias for string
            "void": TypeCategory.VOID,
            "any": TypeCategory.ANY,
        }

    # --- Scope Management ---
    def push_scope(self) -> None:
        """Push a new scope onto the stack."""
        self.scopes.append({})
        if self.debug:
            logger.debug(f"Pushed new scope: {len(self.scopes)} scopes active")

    def pop_scope(self) -> None:
        """Pop the current scope from the stack."""
        if len(self.scopes) > 1:
            self.scopes.pop()
            if self.debug:
                logger.debug(f"Popped scope: {len(self.scopes)} scopes remain")
        else:
            raise InterpreterError("Cannot pop global scope")

    def get_variable(self, name: str, line: int, column: int) -> Any:
        """Retrieve a variable’s value from instance or scopes."""
        if name == "this" and self.current_instance:
            return self.current_instance
        if self.current_instance and name in self.current_instance.members:
            value = self.current_instance.members[name].value
            if self.debug:
                logger.debug(f"Accessed instance member {name} = {value}")
            return value
        for scope in reversed(self.scopes):
            if name in scope:
                value = scope[name].value
                if self.debug:
                    logger.debug(f"Accessed scope variable {name} = {value}")
                return value
        raise InterpreterError(format_error(
            "RuntimeError",
            f"Undefined variable: {name}",
            self.filename,
            self.source,
            line,
            column,
            token_length=len(name)
        ))

    def set_variable(self, name: str, value: Any, type_token: Optional[Token], is_const: bool, is_nullable: bool, line: int, column: int) -> Any:
        """Set a variable’s value with type checking."""
        if self.current_instance and name in self.current_instance.members:
            if self.current_instance.members[name].is_const:
                raise InterpreterError(format_error(
                    "RuntimeError",
                    f"Cannot reassign const member {name}",
                    self.filename,
                    self.source,
                    line,
                    column,
                    token_length=len(name)
                ))
            if type_token or self.current_instance.members[name].type_token:
                self.check_type(value, type_token or self.current_instance.members[name].type_token, line, column)
            self.current_instance.members[name] = VariableInfo(value, type_token, is_const, is_nullable)
            if self.debug:
                logger.debug(f"Set instance member {name} = {value}")
            return value
        for scope in reversed(self.scopes):
            if name in scope:
                if scope[name].is_const:
                    raise InterpreterError(format_error(
                        "RuntimeError",
                        f"Cannot reassign const variable {name}",
                        self.filename,
                        self.source,
                        line,
                        column,
                        token_length=len(name)
                    ))
                if type_token or scope[name].type_token:
                    self.check_type(value, type_token or scope[name].type_token, line, column)
                scope[name] = VariableInfo(value, type_token, is_const, is_nullable)
                if self.debug:
                    logger.debug(f"Set scope variable {name} = {value}")
                return value
        if type_token:
            self.check_type(value, type_token, line, column)
        self.scopes[-1][name] = VariableInfo(value, type_token, is_const, is_nullable)
        if self.debug:
            logger.debug(f"Set new variable {name} = {value} in current scope")
        return value

    # --- Type Checking ---
    def check_type(self, value: Any, type_token: Token, line: int, column: int) -> None:
        """Validate that a value matches the expected type."""
        if not isinstance(type_token, Token):
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Invalid type token: {type(type_token).__name__}",
                self.filename,
                self.source,
                line,
                column,
                token_length=1
            ))
        expected_type = type_token.value
        is_nullable = getattr(type_token, 'is_nullable', False)
        if self.debug:
            logger.debug(f"Checking type: value={value} ({type(value).__name__}), expected={expected_type}, nullable={is_nullable}")

        if expected_type == 'str':
            expected_type = 'string'

        if value is None:
            if is_nullable or expected_type == 'void':
                return
            raise InterpreterError(format_error(
                "TypeError",
                f"Expected {expected_type}, got None",
                self.filename,
                self.source,
                line,
                column,
                token_length=len(expected_type)
            ))

        type_checks = {
            'int': lambda v: isinstance(v, int),
            'float': lambda v: isinstance(v, (int, float)),
            'double': lambda v: isinstance(v, (int, float)),
            'string': lambda v: isinstance(v, str),
            'boolean': lambda v: isinstance(v, bool),
            'void': lambda v: v is None,
            'any': lambda v: True,
        }

        if expected_type in type_checks:
            if type_checks[expected_type](value):
                return
            raise InterpreterError(format_error(
                "TypeError",
                f"Expected {expected_type}, got {type(value).__name__}",
                self.filename,
                self.source,
                line,
                column,
                token_length=len(expected_type)
            ))
        elif expected_type in self.classes:
            if isinstance(value, ClassInstance) and (value.class_name == expected_type or self._is_subclass(value.class_name, expected_type)):
                return
            raise InterpreterError(format_error(
                "TypeError",
                f"Expected class {expected_type}, got {type(value).__name__}",
                self.filename,
                self.source,
                line,
                column,
                token_length=len(expected_type)
            ))
        else:
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Unknown type: {expected_type}",
                self.filename,
                self.source,
                line,
                column,
                token_length=len(expected_type)
            ))

    def _is_subclass(self, class_name: str, parent_name: str) -> bool:
        """Check if a class is a subclass of another."""
        if class_name == parent_name:
            return True
        class_def = self.classes.get(class_name)
        if not class_def or not class_def.superclass:
            return False
        return self._is_subclass(class_def.superclass.variable.value, parent_name)

    def check_modifiers(self, modifiers: List[Token], line: int, column: int) -> None:
        """Validate access modifiers based on context."""
        valid_modifiers = {'public', 'private', 'protected', 'internal', 'static'}
        for mod in modifiers:
            if mod.value not in valid_modifiers:
                raise InterpreterError(format_error(
                    "RuntimeError",
                    f"Invalid modifier: {mod.value}",
                    self.filename,
                    self.source,
                    line,
                    column,
                    token_length=len(mod.value)
                ))
        if not self.current_class:
            for mod in modifiers:
                if mod.value in ('private', 'protected'):
                    raise InterpreterError(format_error(
                        "RuntimeError",
                        f"Modifier '{mod.value}' is only allowed inside classes",
                        self.filename,
                        self.source,
                        line,
                        column,
                        token_length=len(mod.value)
                    ))
        else:
            for mod in modifiers:
                if mod.value == 'protected' and self.current_class not in self.super_class_stack:
                    raise InterpreterError(format_error(
                        "RuntimeError",
                        "Protected access is only allowed within the class or its subclasses",
                        self.filename,
                        self.source,
                        line,
                        column,
                        token_length=len(mod.value)
                    ))

    # --- AST Debugging and Validation ---
    def dump_ast(self, node: ExpressionNode, indent: int = 0) -> str:
        """Dump the AST structure for debugging with line and column info."""
        result = []
        indent_str = "  " * indent
        line = getattr(node, 'line', 'unknown')
        column = getattr(node, 'column', 'unknown')
        if isinstance(node, ProgramNode):
            result.append(f"{indent_str}{type(node).__name__}: (line={line}, col={column})")
            for imp in node.imports:
                result.append(self.dump_ast(imp, indent + 1))
            for stmt in node.statements:
                result.append(self.dump_ast(stmt, indent + 1))
        elif isinstance(node, BlockNode):
            result.append(f"{indent_str}{type(node).__name__}: (line={line}, col={column})")
            for stmt in node.statements:
                result.append(self.dump_ast(stmt, indent + 1))
        elif isinstance(node, FunctionDefNode):
            result.append(f"{indent_str}{type(node).__name__}: {node.name.value} (return_type={node.return_type.value if node.return_type else 'None'}, line={line}, col={column})")
            result.append(self.dump_ast(node.body, indent + 1))
        elif isinstance(node, ReturnNode):
            expr = getattr(node, 'expression', None)
            result.append(f"{indent_str}{type(node).__name__}: expr={type(expr).__name__ if expr else 'None'} (line={line}, col={column})")
            if expr:
                result.append(self.dump_ast(expr, indent + 1))
        elif isinstance(node, StringNode):
            result.append(f"{indent_str}{type(node).__name__}: value={node.value} (line={line}, col={column})")
        elif isinstance(node, VariableNode):
            result.append(f"{indent_str}{type(node).__name__}: value={node.variable.value!r} (line={line}, col={column})")
        elif isinstance(node, (ClassNode, VarDeclarationNode, PrintNode, NewNode, MemberCallNode)):
            result.append(f"{indent_str}{type(node).__name__}: (line={line}, col={column})")
            for attr in ['name', 'variable', 'expression', 'obj', 'method']:
                if hasattr(node, attr):
                    val = getattr(node, attr)
                    if val:
                        result.append(self.dump_ast(val, indent + 1))
            for attr in ['members', 'args']:
                if hasattr(node, attr):
                    for item in getattr(node, attr, []):
                        result.append(self.dump_ast(item, indent + 1))
        else:
            value = getattr(node, 'value', '')
            result.append(f"{indent_str}{type(node).__name__}: value={value!r} (line={line}, col={column})")
        return "\n".join(result)

    def validate_ast(self, node: ExpressionNode) -> None:
        """Validate the AST structure before interpretation."""
        if self.debug:
            logger.debug(f"Validating AST node: {type(node).__name__}")
        if isinstance(node, FunctionDefNode):
            if not isinstance(node.body, (BlockNode, ReturnNode)):
                raise InterpreterError(f"Invalid AST: Function {node.name.value} body must be BlockNode or ReturnNode, got {type(node.body).__name__}")
            if node.return_type and node.return_type.value != 'void':
                has_valid_return = False
                if isinstance(node.body, ReturnNode):
                    if not node.body.expression:
                        raise InterpreterError(f"Invalid AST: Function {node.name.value} with return type {node.return_type.value} has no return expression")
                    if isinstance(node.body.expression, VariableNode):
                        if not node.body.expression.variable.value.strip():
                            logger.error(f"Invalid return expression in {node.name.value}:\n{self.dump_ast(node.body)}")
                            raise InterpreterError(f"Invalid AST: Function {node.name.value} return expression is an empty or invalid VariableNode")
                        # Allow non-empty VariableNode for now, validate during interpretation
                    if node.return_type.value == 'string' and not isinstance(node.body.expression, (StringNode, VariableNode)):
                        logger.error(f"Invalid return expression type in {node.name.value}:\n{self.dump_ast(node.body)}")
                        raise InterpreterError(f"Invalid AST: Function {node.name.value} expected string return, got {type(node.body.expression).__name__}")
                    has_valid_return = True
                elif isinstance(node.body, BlockNode):
                    for stmt in node.body.statements:
                        if isinstance(stmt, ReturnNode):
                            if not stmt.expression:
                                raise InterpreterError(f"Invalid AST: Function {node.name.value} with return type {node.return_type.value} has no return expression")
                            if isinstance(stmt.expression, VariableNode):
                                if not stmt.expression.variable.value.strip():
                                    logger.error(f"Invalid return expression in {node.name.value}:\n{self.dump_ast(stmt)}")
                                    raise InterpreterError(f"Invalid AST: Function {node.name.value} return expression is an empty or invalid VariableNode")
                                # Allow non-empty VariableNode for now
                            if node.return_type.value == 'string' and not isinstance(stmt.expression, (StringNode, VariableNode)):
                                logger.error(f"Invalid return expression type in {node.name.value}:\n{self.dump_ast(stmt)}")
                                raise InterpreterError(f"Invalid AST: Function {node.name.value} expected string return, got {type(stmt.expression).__name__}")
                            has_valid_return = True
                            break
                    if not has_valid_return:
                        logger.error(f"No valid return statement in {node.name.value}:\n{self.dump_ast(node.body)}")
                        raise InterpreterError(f"Invalid AST: Function {node.name.value} with return type {node.return_type.value} must contain a valid return statement")
                self.validate_ast(node.body)
        elif isinstance(node, BlockNode):
            for stmt in node.statements:
                self.validate_ast(stmt)
        elif isinstance(node, ReturnNode):
            if node.expression:
                self.validate_ast(node.expression)
        elif isinstance(node, (ProgramNode, ClassNode)):
            for attr in ['statements', 'members']:
                if hasattr(node, attr):
                    for item in getattr(node, attr, []):
                        self.validate_ast(item)
        elif isinstance(node, (VarDeclarationNode, PrintNode, NewNode, MemberCallNode)):
            for attr in ['expr', 'expression', 'obj', 'method']:
                if hasattr(node, attr):
                    val = getattr(node, attr)
                    if val:
                        self.validate_ast(val)
            for attr in ['args']:
                if hasattr(node, attr):
                    for item in getattr(node, attr, []):
                        self.validate_ast(item)

    # --- Main Interpretation ---
    def interpret(self, node: ExpressionNode) -> Any:
        """Main entry point for interpreting an AST node."""
        try:
            if self.debug:
                logger.debug(f"Validating program AST")
                logger.debug(f"Full Program AST:\n{self.dump_ast(node)}")
                self.validate_ast(node)
            method_map = {
                ProgramNode: self.interpret_program,
                ImportNode: self.interpret_import,
                NumberNode: self.interpret_number,
                StringNode: self.interpret_string,
                CharNode: self.interpret_char,
                BooleanNode: self.interpret_boolean,
                NullNode: lambda n: None,
                VariableNode: self.interpret_variable,
                AssignNode: self.interpret_assign,
                VarDeclarationNode: self.interpret_var_declaration,
                ValDeclarationNode: self.interpret_val_declaration,
                ConstDeclarationNode: self.interpret_const_declaration,
                BinaryOperationNode: self.interpret_binary_op,
                UnaryOperationNode: self.interpret_unary_op,
                NullCoalesceNode: self.interpret_null_coalesce,
                ElvisNode: self.interpret_elvis,
                IfNode: self.interpret_if,
                WhileNode: self.interpret_while,
                DoWhileNode: self.interpret_do_while,
                ForNode: self.interpret_for,
                SwitchNode: self.interpret_switch,
                TryNode: self.interpret_try,
                ThrowNode: self.interpret_throw,
                FunctionDefNode: self.interpret_function_def,
                FunctionCallNode: self.interpret_function_call,
                MemberCallNode: self.interpret_member_call,
                LambdaNode: self.interpret_lambda,
                ClassNode: self.interpret_class_def,
                InterfaceNode: self.interpret_interface_def,
                EnumNode: self.interpret_enum_def,
                ReturnNode: self.interpret_return,
                PrintNode: self.interpret_print,
                InputNode: self.interpret_input,
                InstanceOfNode: self.interpret_instanceof,
                NewNode: self.interpret_new,
                BlockNode: self.interpret_block,
                BreakNode: lambda n: n,
                ContinueNode: lambda n: n,
                ThisNode: self.interpret_this,
                SuperNode: self.interpret_super,
            }
            for node_type, handler in method_map.items():
                if isinstance(node, node_type):
                    if self.debug:
                        logger.debug(f"Interpreting node: {type(node).__name__}")
                    return handler(node)
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Unknown node type: {type(node).__name__}",
                self.filename,
                self.source,
                getattr(node, 'line', 1),
                getattr(node, 'column', 1),
                token_length=1
            ))
        except InterpreterError as e:
            print(str(e))
            sys.exit(1)

    # --- Program and Imports ---
    def interpret_program(self, node: ProgramNode) -> Any:
        """Interpret a program node, handling imports and statements."""
        if self.debug:
            logger.debug(f"Program AST:\n{self.dump_ast(node)}")
        for imp in node.imports:
            self.interpret_import(imp)
        last_result = None
        for stmt in node.statements:
            result = self.interpret(stmt)
            if isinstance(result, (ReturnNode, BreakNode, ContinueNode)):
                if isinstance(result, ReturnNode):
                    return self.interpret_return(result)
                return result
            last_result = result
        return last_result

    def interpret_import(self, node: ImportNode) -> None:
        """Handle Python-style imports."""
        module_name = '.'.join(t.value for t in node.module)
        if node.names:
            for name in node.names:
                self.imports[name.value] = f"{module_name}.{name.value}"
        elif node.alias:
            self.imports[node.alias.value] = module_name
        else:
            self.imports[module_name] = module_name
        if self.debug:
            logger.debug(f"Imported: {self.imports}")

    # --- Literals ---
    def interpret_number(self, node: NumberNode) -> Union[int, float]:
        """Interpret a number literal."""
        value = node.value
        return float(value) if '.' in value or 'e' in value.lower() else int(value)

    def interpret_string(self, node: StringNode) -> str:
        """Interpret a string literal."""
        return node.value[1:-1]

    def interpret_char(self, node: CharNode) -> str:
        """Interpret a character literal."""
        return node.value[1:-1]

    def interpret_boolean(self, node: BooleanNode) -> bool:
        """Interpret a boolean literal."""
        return node.value.lower() == 'true'

    # --- Variables and Assignments ---
    def interpret_variable(self, node: VariableNode) -> Any:
        """Interpret a variable reference."""
        if not node.variable.value.strip():
            if self.debug:
                logger.error(f"Invalid VariableNode with empty value:\n{self.dump_ast(node)}")
            raise InterpreterError(format_error(
                "RuntimeError",
                "Invalid variable reference: empty identifier, likely a parser error for string literal",
                self.filename,
                self.source,
                node.variable.line,
                node.variable.column,
                token_length=1
            ))
        return self.get_variable(node.variable.value, node.variable.line, node.variable.column)

    def interpret_assign(self, node: AssignNode) -> Any:
        """Interpret an assignment operation."""
        value = self.interpret(node.expression)
        if isinstance(node.variable, VariableNode):
            return self.set_variable(
                node.variable.variable.value,
                value,
                None,
                False,
                True,
                node.variable.variable.line,
                node.variable.variable.column
            )
        raise InterpreterError(format_error(
            "RuntimeError",
            "Invalid assignment target",
            self.filename,
            self.source,
            node.token.line,
            node.token.column,
            token_length=1
        ))

    def interpret_var_declaration(self, node: VarDeclarationNode) -> Any:
        """Interpret a variable declaration."""
        value = self.interpret(node.expr) if node.expr else None
        self.check_modifiers(node.modifiers, node.variable.variable.line, node.variable.variable.column)
        if node.type_token:
            self.check_type(value, node.type_token, node.variable.variable.line, node.variable.variable.column + len(node.variable.variable.value) + 2)
        return self.set_variable(
            node.variable.variable.value,
            value,
            node.type_token,
            False,
            node.type_token.is_nullable if node.type_token else True,
            node.variable.variable.line,
            node.variable.variable.column
        )

    def interpret_val_declaration(self, node: ValDeclarationNode) -> Any:
        """Interpret a val (immutable) declaration."""
        if not node.expr and 'CONST' not in [mod.value for mod in node.modifiers]:
            raise InterpreterError(format_error(
                "RuntimeError",
                "Val declaration requires an initializer unless const",
                self.filename,
                self.source,
                node.variable.variable.line,
                node.variable.variable.column,
                token_length=len(node.variable.variable.value)
            ))
        value = self.interpret(node.expr) if node.expr else None
        self.check_modifiers(node.modifiers, node.variable.variable.line, node.variable.variable.column)
        if node.type_token:
            self.check_type(value, node.type_token, node.variable.variable.line, node.variable.variable.column + len(node.variable.variable.value) + 2)
        return self.set_variable(
            node.variable.variable.value,
            value,
            node.type_token,
            True,
            node.type_token.is_nullable if node.type_token else True,
            node.variable.variable.line,
            node.variable.variable.column
        )

    def interpret_const_declaration(self, node: ConstDeclarationNode) -> Any:
        """Interpret a const declaration."""
        if not node.expr:
            raise InterpreterError(format_error(
                "RuntimeError",
                "Const declaration requires an initializer",
                self.filename,
                self.source,
                node.variable.variable.line,
                node.variable.variable.column,
                token_length=len(node.variable.variable.value)
            ))
        value = self.interpret(node.expr)
        self.check_modifiers(node.modifiers, node.variable.variable.line, node.variable.variable.column)
        if node.type_token:
            self.check_type(value, node.type_token, node.variable.variable.line, node.variable.variable.column + len(node.variable.variable.value) + 2)
        return self.set_variable(
            node.variable.variable.value,
            value,
            node.type_token,
            True,
            node.type_token.is_nullable if node.type_token else True,
            node.variable.variable.line,
            node.variable.variable.column
        )

    # --- Operators ---
    def interpret_binary_op(self, node: BinaryOperationNode) -> Any:
        """Interpret a binary operation."""
        left = self.interpret(node.left_node)
        right = self.interpret(node.right_node)
        op = node.operator.type.name
        operations = {
            'PLUS': lambda l, r: str(l) + str(r) if isinstance(l, str) or isinstance(r, str) else l + r,
            'MINUS': lambda l, r: l - r,
            'MULTIPLY': lambda l, r: l * r,
            'DIVIDE': lambda l, r: l / r if r != 0 else self.raise_division_by_zero(node.operator),
            'MODULO': lambda l, r: l % r,
            'EQUAL': lambda l, r: l == r,
            'NOT_EQUAL': lambda l, r: l != r,
            'LESS': lambda l, r: l < r,
            'GREATER': lambda l, r: l > r,
            'LESS_EQUAL': lambda l, r: l <= r,
            'GREATER_EQUAL': lambda l, r: l >= r,
            'AND': lambda l, r: bool(l) and bool(r),
            'OR': lambda l, r: bool(l) or bool(r),
            'BIT_AND': lambda l, r: l & r if isinstance(l, int) and isinstance(r, int) else self.raise_bitwise_error(op, node.operator),
            'BIT_OR': lambda l, r: l | r if isinstance(l, int) and isinstance(r, int) else self.raise_bitwise_error(op, node.operator),
            'BIT_XOR': lambda l, r: l ^ r if isinstance(l, int) and isinstance(r, int) else self.raise_bitwise_error(op, node.operator),
            'SHL': lambda l, r: l << r if isinstance(l, int) and isinstance(r, int) else self.raise_bitwise_error(op, node.operator),
            'SHR': lambda l, r: l >> r if isinstance(l, int) and isinstance(r, int) else self.raise_bitwise_error(op, node.operator),
        }
        if op in operations:
            try:
                result = operations[op](left, right)
                if self.debug:
                    logger.debug(f"Binary op {op}: {left} {op} {right} = {result}")
                return result
            except TypeError as e:
                raise InterpreterError(format_error(
                    "TypeError",
                    f"Invalid types for {op}: {type(left).__name__}, {type(right).__name__}",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column,
                    token_length=1
                ))
        raise InterpreterError(format_error(
            "RuntimeError",
            f"Unknown operator: {op}",
            self.filename,
            self.source,
            node.operator.line,
            node.operator.column,
            token_length=1
        ))

    def raise_division_by_zero(self, token: Token) -> None:
        """Raise an error for division by zero."""
        raise InterpreterError(format_error(
            "RuntimeError",
            "Division by zero",
            self.filename,
            self.source,
            token.line,
            token.column,
            token_length=1
        ))

    def raise_bitwise_error(self, op: str, token: Token) -> None:
        """Raise an error for invalid bitwise operation."""
        raise InterpreterError(format_error(
            "TypeError",
            f"Bitwise {op} requires integers",
            self.filename,
            self.source,
            token.line,
            token.column,
            token_length=1
        ))

    def interpret_unary_op(self, node: UnaryOperationNode) -> Any:
        """Interpret a unary operation."""
        operand = self.interpret(node.operand)
        op = node.operator.type.name
        if node.is_postfix:
            if not isinstance(node.operand, VariableNode):
                raise InterpreterError(format_error(
                    "RuntimeError",
                    "Postfix operator requires a variable",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column,
                    token_length=1
                ))
            var_name = node.operand.variable.value
            old_value = self.get_variable(var_name, node.operand.variable.line, node.operand.variable.column)
            if not isinstance(old_value, (int, float)):
                raise InterpreterError(format_error(
                    "TypeError",
                    f"Postfix {op} requires numeric value, got {type(old_value).__name__}",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column,
                    token_length=1
                ))
            new_value = old_value + 1 if op == 'INCREMENT' else old_value - 1
            self.set_variable(var_name, new_value, None, False, True, node.operand.variable.line, node.operand.variable.column)
            return old_value
        operations = {
            'MINUS': lambda v: -v,
            'NOT': lambda v: not v,
            'BIT_NOT': lambda v: ~v if isinstance(v, int) else self.raise_bitwise_error('NOT', node.operator),
            'INCREMENT': lambda v: v + 1 if isinstance(v, (int, float)) else self.raise_increment_error(node.operator),
            'DECREMENT': lambda v: v - 1 if isinstance(v, (int, float)) else self.raise_increment_error(node.operator),
        }
        if op in operations:
            try:
                result = operations[op](operand)
                if self.debug:
                    logger.debug(f"Unary op {op}: {operand} = {result}")
                return result
            except TypeError as e:
                raise InterpreterError(format_error(
                    "TypeError",
                    f"Invalid type for {op}: {type(operand).__name__}",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column,
                    token_length=1
                ))
        raise InterpreterError(format_error(
            "RuntimeError",
            f"Unknown unary operator: {op}",
            self.filename,
            self.source,
            node.operator.line,
            node.operator.column,
            token_length=1
        ))

    def raise_increment_error(self, token: Token) -> None:
        """Raise an error for invalid increment/decrement operation."""
        raise InterpreterError(format_error(
            "TypeError",
            "Increment/decrement requires numeric value",
            self.filename,
            self.source,
            token.line,
            token.column,
            token_length=1
        ))

    def interpret_null_coalesce(self, node: NullCoalesceNode) -> Any:
        """Interpret a null-coalescing operation (??)."""
        left = self.interpret(node.left_node)
        return left if left is not None else self.interpret(node.right_node)

    def interpret_elvis(self, node: ElvisNode) -> Any:
        """Interpret an Elvis operation (?:)."""
        left = self.interpret(node.left_node)
        return left if left is not None else self.interpret(node.right_node)

    # --- Control Structures ---
    def interpret_if(self, node: IfNode) -> Any:
        """Interpret an if statement."""
        condition = self.interpret(node.condition)
        if not isinstance(condition, bool):
            raise InterpreterError(format_error(
                "TypeError",
                f"Condition must be boolean, got {type(condition).__name__}",
                self.filename,
                self.source,
                getattr(node.condition, 'line', 1),
                getattr(node.condition, 'column', 1),
                token_length=1
            ))
        if condition:
            return self.interpret(node.then_branch)
        elif node.else_branch:
            return self.interpret(node.else_branch)
        return None

    def interpret_while(self, node: WhileNode) -> Any:
        """Interpret a while loop."""
        while True:
            condition = self.interpret(node.condition)
            if not isinstance(condition, bool):
                raise InterpreterError(format_error(
                    "TypeError",
                    f"Condition must be boolean, got {type(condition).__name__}",
                    self.filename,
                    self.source,
                    getattr(node.condition, 'line', 1),
                    getattr(node.condition, 'column', 1),
                    token_length=1
                ))
            if not condition:
                break
            result = self.interpret(node.body)
            if isinstance(result, BreakNode):
                break
            elif isinstance(result, ReturnNode):
                return self.interpret_return(result)
            elif isinstance(result, ContinueNode):
                continue
        return None

    def interpret_do_while(self, node: DoWhileNode) -> Any:
        """Interpret a do-while loop."""
        while True:
            result = self.interpret(node.body)
            if isinstance(result, BreakNode):
                break
            elif isinstance(result, ReturnNode):
                return self.interpret_return(result)
            elif isinstance(result, ContinueNode):
                continue
            condition = self.interpret(node.condition)
            if not isinstance(condition, bool):
                raise InterpreterError(format_error(
                    "TypeError",
                    f"Condition must be boolean, got {type(condition).__name__}",
                    self.filename,
                    self.source,
                    getattr(node.condition, 'line', 1),
                    getattr(node.condition, 'column', 1),
                    token_length=1
                ))
            if not condition:
                break
        return None

    def interpret_for(self, node: ForNode) -> Any:
        """Interpret a for loop."""
        if node.init:
            self.interpret(node.init)
        self.push_scope()
        try:
            while node.cond is None or self.interpret(node.cond):
                result = self.interpret(node.body)
                if isinstance(result, BreakNode):
                    break
                elif isinstance(result, ReturnNode):
                    return self.interpret_return(result)
                elif isinstance(result, ContinueNode):
                    if node.step:
                        self.interpret(node.step)
                    continue
                if node.step:
                    self.interpret(node.step)
            return None
        finally:
            self.pop_scope()

    def interpret_switch(self, node: SwitchNode) -> Any:
        """Interpret a switch statement."""
        value = self.interpret(node.expression)
        for case in node.cases:
            case_value = self.interpret(case.value)
            if value == case_value:
                return self.interpret(case.body)
        if node.default:
            return self.interpret(node.default)
        return None

    def interpret_try(self, node: TryNode) -> Any:
        """Interpret a try-catch-finally block."""
        try:
            return self.interpret(node.try_block)
        except InterpreterError as e:
            for catch in node.catches:
                self.push_scope()
                try:
                    self.set_variable(
                        catch.exception_var.variable.value,
                        str(e),
                        catch.type_token,
                        False,
                        True,
                        catch.exception_var.variable.line,
                        catch.exception_var.variable.column
                    )
                    return self.interpret(catch.body)
                finally:
                    self.pop_scope()
            raise
        finally:
            if node.finally_block:
                self.interpret(node.finally_block)

    def interpret_throw(self, node: ThrowNode) -> None:
        """Interpret a throw statement."""
        value = self.interpret(node.expression)
        raise InterpreterError(format_error(
            "RuntimeError",
            str(value),
            self.filename,
            self.source,
            getattr(node.expression, 'line', 1),
            getattr(node.expression, 'column', 1),
            token_length=1
        ))

    # --- Functions and Methods ---
    def interpret_function_def(self, node: FunctionDefNode) -> None:
        """Interpret a function definition."""
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        self.functions[node.name.value] = node
        if self.debug:
            logger.debug(f"Defined function {node.name.value}")

    def interpret_function_call(self, node: FunctionCallNode) -> Any:
        """Interpret a function call."""
        func_name = node.func.variable.value
        if func_name not in self.functions:
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Undefined function: {func_name}",
                self.filename,
                self.source,
                node.func.variable.line,
                node.func.variable.column,
                token_length=len(func_name)
            ))
        func = self.functions[func_name]
        self.check_modifiers(func.modifiers, node.func.variable.line, node.func.variable.column)
        args = [self.interpret(arg) for arg in node.args]
        if len(args) != len(func.params):
            raise InterpreterError(format_error(
                "TypeError",
                f"Expected {len(func.params)} arguments, got {len(args)}",
                self.filename,
                self.source,
                node.func.variable.line,
                node.func.variable.column,
                token_length=1
            ))
        self.push_scope()
        try:
            for param, arg in zip(func.params, args):
                if param.type_token:
                    self.check_type(arg, param.type_token, param.name.line, param.name.column)
                if arg is None and not param.is_nullable:
                    raise InterpreterError(format_error(
                        "RuntimeError",
                        f"Parameter {param.name.value} is not nullable",
                        self.filename,
                        self.source,
                        param.name.line,
                        param.name.column,
                        token_length=len(param.name.value)
                    ))
                self.set_variable(
                    param.name.value,
                    arg,
                    param.type_token,
                    False,
                    param.is_nullable,
                    param.name.line,
                    param.name.column
                )
            result = self.interpret(func.body)
            if isinstance(result, ReturnNode):
                return_value = self.interpret_return(result)
                if func.return_type:
                    self.check_type(return_value, func.return_type, func.name.line, func.name.column)
                return return_value
            if func.return_type and func.return_type.value != 'void':
                raise InterpreterError(format_error(
                    "RuntimeError",
                    f"Function {func_name} must return a value of type {func.return_type.value}",
                    self.filename,
                    self.source,
                    node.func.variable.line,
                    node.func.variable.column,
                    token_length=len(func_name)
                ))
            return None
        finally:
            self.pop_scope()

    def interpret_member_call(self, node: MemberCallNode) -> Any:
        """Interpret a member method call on an object."""
        if self.debug:
            logger.debug(f"Interpreting member call: {node.method.variable.value} on object")
        obj = self.interpret(node.obj)
        if not isinstance(obj, ClassInstance):
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Member call requires a class instance, got {type(obj).__name__}",
                self.filename,
                self.source,
                getattr(node.obj, 'line', node.method.variable.line),
                getattr(node.obj, 'column', node.method.variable.column),
                token_length=len(node.method.variable.value)
            ))
        class_name = obj.class_name
        if self.debug:
            logger.debug(f"Calling method {node.method.variable.value} on instance of {class_name}")
        if class_name not in self.classes:
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Undefined class: {class_name}",
                self.filename,
                self.source,
                node.method.variable.line,
                node.method.variable.column,
                token_length=len(class_name)
            ))
        class_def = self.classes[class_name]
        method_name = node.method.variable.value
        method = None
        for member in class_def.members:
            if isinstance(member, FunctionDefNode) and member.name.value == method_name:
                method = member
                break
        if not method and class_def.superclass:
            superclass_name = class_def.superclass.variable.value
            superclass_def = self.classes.get(superclass_name)
            if superclass_def:
                for member in superclass_def.members:
                    if isinstance(member, FunctionDefNode) and member.name.value == method_name:
                        method = member
                        break
        if not method:
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Undefined method {method_name} in class {class_name}",
                self.filename,
                self.source,
                node.method.variable.line,
                node.method.variable.column,
                token_length=len(method_name)
            ))
        self.check_modifiers(method.modifiers, node.method.variable.line, node.method.variable.column)
        args = [self.interpret(arg) for arg in node.args]
        if len(args) != len(method.params):
            raise InterpreterError(format_error(
                "TypeError",
                f"Method {method_name} expected {len(method.params)} arguments, got {len(args)}",
                self.filename,
                self.source,
                node.method.variable.line,
                node.method.variable.column,
                token_length=1
            ))
        prev_instance = self.current_instance
        self.current_instance = obj
        self.push_scope()
        try:
            for param, arg in zip(method.params, args):
                if param.type_token:
                    self.check_type(arg, param.type_token, param.name.line, param.name.column)
                if arg is None and not param.is_nullable:
                    raise InterpreterError(format_error(
                        "RuntimeError",
                        f"Parameter {param.name.value} is not nullable",
                        self.filename,
                        self.source,
                        param.name.line,
                        param.name.column,
                        token_length=len(param.name.value)
                    ))
                self.set_variable(
                    param.name.value,
                    arg,
                    param.type_token,
                    False,
                    param.is_nullable,
                    param.name.line,
                    param.name.column
                )
            self.set_variable("this", obj, None, False, False, node.method.variable.line, node.method.variable.column)
            if self.debug:
                logger.debug(f"Executing method {method_name} body (type: {type(method.body).__name__})")
                logger.debug(f"Method body AST:\n{self.dump_ast(method.body)}")
                if isinstance(method.body, BlockNode):
                    logger.debug(f"Block contains {len(method.body.statements)} statements")
                    for i, stmt in enumerate(method.body.statements):
                        logger.debug(f"Statement {i+1}: {type(stmt).__name__}")
            if isinstance(method.body, ReturnNode):
                if self.debug:
                    logger.debug(f"Method body is a ReturnNode, interpreting directly")
                return_value = self.interpret_return(method.body)
                if method.return_type:
                    self.check_type(return_value, method.return_type, node.method.variable.line, node.method.variable.column)
                return return_value
            result = self.interpret(method.body)
            if self.debug:
                logger.debug(f"Method {method_name} body returned: {result} (type: {type(result).__name__})")
            if isinstance(result, ReturnNode):
                return_value = self.interpret_return(result)
                if self.debug:
                    logger.debug(f"Return value from method {method_name}: {return_value} (type: {type(return_value).__name__})")
                if method.return_type:
                    self.check_type(return_value, method.return_type, node.method.variable.line, node.method.variable.column)
                return return_value
            if method.return_type and method.return_type.value != 'void':
                if self.debug:
                    logger.error(f"Method {method_name} did not return a value; expected {method.return_type.value}")
                    logger.error(f"Method body AST (re-dumped):\n{self.dump_ast(method.body)}")
                raise InterpreterError(format_error(
                    "RuntimeError",
                    f"Method {method_name} must return a value of type {method.return_type.value}",
                    self.filename,
                    self.source,
                    node.method.variable.line,
                    node.method.variable.column,
                    token_length=len(method_name)
                ))
            return None
        except InterpreterError as e:
            if self.debug:
                logger.error(f"InterpreterError in method {method_name}: {str(e)}")
            raise
        except Exception as e:
            if self.debug:
                logger.error(f"Unexpected error in method {method_name}: {str(e)}")
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Error executing method {method_name}: {str(e)}",
                self.filename,
                self.source,
                node.method.variable.line,
                node.method.variable.column,
                token_length=len(method_name)
            ))
        finally:
            self.pop_scope()
            self.current_instance = prev_instance

    def interpret_lambda(self, node: LambdaNode) -> Callable:
        """Interpret a lambda expression."""
        def lambda_func(*args):
            if len(args) != len(node.params):
                raise InterpreterError(format_error(
                    "TypeError",
                    f"Expected {len(node.params)} arguments, got {len(args)}",
                    self.filename,
                    self.source,
                    node.params[0].line if node.params else 1,
                    node.params[0].column if node.params else 1,
                    token_length=1
                ))
            self.push_scope()
            try:
                for param, arg in zip(node.params, args):
                    if param.type_token:
                        self.check_type(arg, param.type_token, param.name.line, param.name.column)
                    if arg is None and not param.is_nullable:
                        raise InterpreterError(format_error(
                            "RuntimeError",
                            f"Parameter {param.name.value} is not nullable",
                            self.filename,
                            self.source,
                            param.name.line,
                            param.name.column,
                            token_length=len(param.name.value)
                        ))
                    self.set_variable(
                        param.name.value,
                        arg,
                        param.type_token,
                        False,
                        param.is_nullable,
                        param.name.line,
                        param.name.column
                    )
                result = self.interpret(node.body)
                if isinstance(result, ReturnNode):
                    return self.interpret_return(result)
                return result
            finally:
                self.pop_scope()
        if self.debug:
            logger.debug(f"Created lambda function with {len(node.params)} parameters")
        return lambda_func

    # --- Classes and Instances ---
    def interpret_class_def(self, node: ClassNode) -> None:
        """Interpret a class definition."""
        class_name = node.name.value
        if self.debug:
            logger.debug(f"Defining class {class_name}")
        prev_class = self.current_class
        self.current_class = class_name
        self.classes[class_name] = node
        self.type_map[class_name] = TypeCategory.CLASS
        if node.superclass:
            superclass_name = node.superclass.variable.value
            if superclass_name not in self.classes:
                raise InterpreterError(format_error(
                    "RuntimeError",
                    f"Undefined superclass: {superclass_name}",
                    self.filename,
                    self.source,
                    node.superclass.variable.line,
                    node.superclass.variable.column,
                    token_length=len(superclass_name)
                ))
            self.super_class_stack.append(superclass_name)
        for interface in node.interfaces:
            interface_name = interface.variable.value
            if interface_name not in self.interfaces:
                raise InterpreterError(format_error(
                    "RuntimeError",
                    f"Undefined interface: {interface_name}",
                    self.filename,
                    self.source,
                    interface.variable.line,
                    interface.variable.column,
                    token_length=len(interface_name)
                ))
        self.push_scope()
        try:
            for member in node.members:
                if isinstance(member, (VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode, FunctionDefNode)):
                    self.interpret(member)
        finally:
            self.pop_scope()
            self.current_class = prev_class
            if node.superclass:
                self.super_class_stack.pop()

    def interpret_interface_def(self, node: InterfaceNode) -> None:
        """Interpret an interface definition."""
        self.interfaces[node.name.value] = node
        self.type_map[node.name.value] = TypeCategory.INTERFACE
        if self.debug:
            logger.debug(f"Defined interface {node.name.value}")

    def interpret_enum_def(self, node: EnumNode) -> None:
        """Interpret an enum definition."""
        self.enums[node.name.value] = node
        self.push_scope()
        try:
            for value in node.values:
                self.set_variable(
                    value.value,
                    value.value,
                    None,
                    True,
                    False,
                    value.line,
                    value.column
                )
            for member in node.members:
                self.interpret(member)
        finally:
            self.pop_scope()
        if self.debug:
            logger.debug(f"Defined enum {node.name.value}")

    def interpret_new(self, node: NewNode) -> ClassInstance:
        """Interpret a new instance creation."""
        class_name = node.type_token.value
        if class_name not in self.classes:
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Undefined class: {class_name}",
                self.filename,
                self.source,
                node.type_token.line,
                node.type_token.column,
                token_length=len(class_name)
            ))
        class_def = self.classes[class_name]
        args = [self.interpret(arg) for arg in node.args]
        members: Dict[str, VariableInfo] = {}
        superclass_instance = None
        if class_def.superclass:
            superclass_name = class_def.superclass.variable.value
            superclass_def = self.classes[superclass_name]
            superclass_instance = self.interpret_new(NewNode(
                Token(TokenType.VARIABLE, superclass_name, node.type_token.position, node.type_token.line, node.type_token.column),
                args
            ))
        instance = ClassInstance(class_name, members, superclass_instance)
        prev_instance = self.current_instance
        self.current_instance = instance
        self.push_scope()
        try:
            self.set_variable("this", instance, None, False, False, node.type_token.line, node.type_token.column)
            if self.debug:
                logger.debug(f"Creating instance of {class_name}")
            for member in class_def.members:
                if isinstance(member, (VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode)):
                    value = self.interpret(member.expr) if member.expr else None
                    if member.type_token:
                        self.check_type(value, member.type_token, member.variable.variable.line, member.variable.variable.column)
                    members[member.variable.variable.value] = VariableInfo(
                        value,
                        member.type_token,
                        isinstance(member, ConstDeclarationNode),
                        member.type_token.is_nullable if member.type_token else True
                    )
                    if self.debug:
                        logger.debug(f"Set member {member.variable.variable.value} = {value}")
            return instance
        finally:
            self.pop_scope()
            self.current_instance = prev_instance

    # --- Miscellaneous ---
    def interpret_return(self, node: ReturnNode) -> Any:
        """Interpret a return statement."""
        if node.expression:
            if isinstance(node.expression, VariableNode) and not node.expression.variable.value.strip():
                if self.debug:
                    logger.error(f"Invalid return expression in ReturnNode:\n{self.dump_ast(node)}")
                raise InterpreterError(format_error(
                    "RuntimeError",
                    "Invalid return expression: empty variable reference, expected string literal",
                    self.filename,
                    self.source,
                    getattr(node, 'line', 1),
                    getattr(node, 'column', 1),
                    token_length=1
                ))
            value = self.interpret(node.expression)
        else:
            value = None
        if self.debug:
            logger.debug(f"Returning value: {value} (type: {type(value).__name__})")
        return value

    def interpret_print(self, node: PrintNode) -> None:
        """Interpret a print statement."""
        value = self.interpret(node.expression)
        print(value)
        if self.debug:
            logger.debug(f"Printed: {value}")

    def interpret_input(self, node: InputNode) -> str:
        """Interpret an input statement."""
        if node.prompt:
            prompt = self.interpret(node.prompt)
            if not isinstance(prompt, str):
                raise InterpreterError(format_error(
                    "TypeError",
                    f"Prompt must be a string, got {type(prompt).__name__}",
                    self.filename,
                    self.source,
                    getattr(node.prompt, 'line', node.token.line),
                    getattr(node.prompt, 'column', node.token.column),
                    token_length=1
                ))
            print(prompt, end='', flush=True)
        try:
            user_input = input()
            if user_input.strip() == "":
                raise InterpreterError(format_error(
                    "RuntimeError",
                    "Empty input is not allowed",
                    self.filename,
                    self.source,
                    node.token.line,
                    node.token.column,
                    token_length=len(node.token.value)
                ))
            return user_input
        except (KeyboardInterrupt, EOFError):
            raise InterpreterError(format_error(
                "RuntimeError",
                "Input interrupted or terminated",
                self.filename,
                self.source,
                node.token.line,
                node.token.column,
                token_length=1
            ))

    def interpret_instanceof(self, node: InstanceOfNode) -> bool:
        """Interpret an instanceof check."""
        value = self.interpret(node.expression)
        type_name = node.type_token.value
        if self.debug:
            logger.debug(f"Checking instanceof: value={value}, type={type_name}")
        if isinstance(value, ClassInstance):
            return self._is_subclass(value.class_name, type_name)
        type_checks = {
            'int': lambda v: isinstance(v, int),
            'float': lambda v: isinstance(v, float),
            'double': lambda v: isinstance(v, float),
            'boolean': lambda v: isinstance(v, bool),
            'string': lambda v: isinstance(v, str),
            'any': lambda v: True,
        }
        return type_checks.get(type_name, lambda v: False)(value)

    def interpret_block(self, node: BlockNode) -> Any:
        """Interpret a block of statements."""
        self.push_scope()
        try:
            last_result = None
            if self.debug:
                logger.debug(f"Interpreting block with {len(node.statements)} statements")
                logger.debug(f"Block AST:\n{self.dump_ast(node)}")
            if not node.statements:
                if self.debug:
                    logger.debug("Block is empty, returning None")
                return None
            for i, stmt in enumerate(node.statements):
                if self.debug:
                    logger.debug(f"Interpreting statement {i+1}/{len(node.statements)}: {type(stmt).__name__} at line {getattr(stmt, 'line', 'unknown')}, column {getattr(stmt, 'column', 'unknown')}")
                    logger.debug(f"Statement AST:\n{self.dump_ast(stmt)}")
                try:
                    result = self.interpret(stmt)
                except Exception as e:
                    if self.debug:
                        logger.error(f"Error interpreting statement {i+1} ({type(stmt).__name__}): {str(e)}")
                    raise InterpreterError(format_error(
                        "RuntimeError",
                        f"Error interpreting statement {type(stmt).__name__}: {str(e)}",
                        self.filename,
                        self.source,
                        getattr(stmt, 'line', 1),
                        getattr(stmt, 'column', 1),
                        token_length=1
                    ))
                if self.debug:
                    logger.debug(f"Statement {i+1} result: {result} (type: {type(result).__name__})")
                if isinstance(result, (ReturnNode, BreakNode, ContinueNode)):
                    if self.debug:
                        logger.debug(f"Block statement {i+1} returned control node: {type(result).__name__}")
                    if isinstance(result, ReturnNode):
                        return_value = self.interpret_return(result)
                        if self.debug:
                            logger.debug(f"Propagating return value from block: {return_value} (type: {type(return_value).__name__})")
                        return return_value
                    return result
                last_result = result
            if self.debug:
                logger.debug(f"Block completed with last result: {last_result} (type: {type(last_result).__name__})")
            return last_result
        except InterpreterError as e:
            if self.debug:
                logger.error(f"InterpreterError in block: {str(e)}")
            raise
        except Exception as e:
            if self.debug:
                logger.error(f"Unexpected error in block: {str(e)}")
            raise InterpreterError(format_error(
                "RuntimeError",
                f"Error in block: {str(e)}",
                self.filename,
                self.source,
                getattr(node, 'line', 1),
                getattr(node, 'column', 1),
                token_length=1
            ))
        finally:
            self.pop_scope()

    def interpret_this(self, node: ThisNode) -> ClassInstance:
        """Interpret a this reference."""
        if not self.current_instance:
            raise InterpreterError(format_error(
                "RuntimeError",
                "'this' can only be used inside a class",
                self.filename,
                self.source,
                node.token.line,
                node.token.column,
                token_length=len(node.token.value)
            ))
        return self.current_instance

    def interpret_super(self, node: SuperNode) -> ClassInstance:
        """Interpret a super reference."""
        if not self.current_instance or not self.current_instance.superclass:
            raise InterpreterError(format_error(
                "RuntimeError",
                "'super' can only be used inside a class with a superclass",
                self.filename,
                self.source,
                node.token.line,
                node.token.column,
                token_length=len(node.token.value)
            ))
        return self.current_instance.superclass