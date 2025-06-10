import asyncio
import math
import random
from abc import ABC, abstractmethod
from typing import Any, List, Optional, Callable, Dict, Union, Coroutine, Set, Tuple
from .token import Token, TokenType, token_types
from .ast import *
from .error import format_error
import sys
import time
import logging
from datetime import datetime, timedelta

# Configure logging for debugging
logging.basicConfig(level=logging.DEBUG, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class MemoryRef:
    """
    Represents a memory reference for pointers in NeonLang.
    Stores a value, its type, and validity status for pointer operations.
    Supports low-level memory management constructs.
    """
    def __init__(self, value: Any, type_token: Callable[[bool], Token]):
        self.value = value
        self.type_token = type_token
        self.is_valid: bool = True

    def __repr__(self) -> str:
        return f"MemoryRef(value={self.value}, type={self.type_token(True).value}, valid={self.is_valid})"

class ClassInstance:
    """
    Represents an instance of a class, struct, or object in NeonLang.
    Contains fields and methods, enabling object-oriented programming.
    Supports encapsulation, inheritance, and polymorphism.
    """
    def __init__(self, class_name: str, fields: Dict[str, Any], methods: Dict[str, FunctionDefNode]):
        self.class_name: str = class_name
        self.fields: Dict[str, Any] = fields
        self.methods: Dict[str, FunctionDefNode] = methods

    def __repr__(self) -> str:
        return f"ClassInstance(class={self.class_name}, fields={self.fields})"

class EnumInstance:
    """
    Represents an enum value in NeonLang.
    Stores the enum name and its specific value for pattern matching and type safety.
    """
    def __init__(self, enum_name: str, value: str):
        self.enum_name: str = enum_name
        self.value: str = value

    def __repr__(self) -> str:
        return f"EnumInstance({self.enum_name}.{self.value})"

class LambdaFunc:
    """
    Represents a lambda function with captured scope.
    Supports functional programming constructs like closures and higher-order functions.
    """
    def __init__(self, node: LambdaNode, scope: Dict[str, Any]):
        self.node: LambdaNode = node
        self.captured_scope: Dict[str, Any] = scope.copy()

    def __repr__(self) -> str:
        return f"LambdaFunc(params={len(self.node.params)}, body={self.node.body})"

class Generator:
    """
    Represents a generator for yield expressions in NeonLang.
    Enables iterator-based programming for lazy evaluation and streaming data.
    """
    def __init__(self, iterator):
        self.iterator = iterator

    def __iter__(self):
        return self.iterator

class Interpreter:
    """
    Interprets NeonLang AST with comprehensive support for:
    - Variables, functions, classes, structs, and enums
    - Memory management (pointers, alloc/free)
    - Asynchronous programming (async/await)
    - Module system (packages, namespaces, imports, exports)
    - Built-in functions and operator overloading
    - List comprehensions and generators
    - Robust error handling and debugging
    """
    def __init__(self, source: str, filename: str):
        """
        Initialize the interpreter with source code and filename.
        Sets up scopes, definitions, built-in functions, and runtime state.
        """
        self.source: str = source
        self.filename: str = filename
        # Scope management
        self.global_scope: Dict[str, Any] = {}
        self.scopes: List[Dict[str, Any]] = [self.global_scope]
        self.const_vars: Set[str] = set()
        # Definitions
        self.functions: Dict[str, FunctionDefNode] = {}
        self.classes: Dict[str, Union[ClassNode, StructNode, DataClassNode, SealedClassNode, ObjectNode]] = {}
        self.interfaces: Dict[str, InterfaceNode] = {}
        self.enums: Dict[str, EnumNode] = {}
        # Built-in functions
        self.built_ins: Dict[str, Callable[..., Any]] = {
            'len': self.builtin_len,
            'sqrt': self.builtin_sqrt,
            'random': self.builtin_random,
            'time': self.builtin_time,
            'to_string': self.builtin_to_string,
            'range': self.builtin_range,
            'abs': self.builtin_abs,
            'join': self.builtin_join,
        }
        # Module system
        self.package: Optional[str] = None
        self.namespace: Optional[str] = None
        self.exports: List[str] = []
        self.imports: Dict[str, str] = {}
        # Runtime state
        self.current_class: Optional[str] = None
        self.super_class_stack: List[str] = []
        self.memory: Dict[int, MemoryRef] = {}
        self.next_mem_addr: int = 0
        # Coroutine state
        self.async_context: bool = False
        # Operator overloading
        self.operator_overloads: Dict[Tuple[str, str], FunctionDefNode] = {}
        # Debugging
        self.debug_mode: bool = False
        logger.info(f"Interpreter initialized for {filename}")

    def enable_debug_mode(self) -> None:
        """
        Enable debug mode for detailed logging of AST traversal.
        Useful for diagnosing issues like missing PrintNode execution.
        """
        self.debug_mode = True
        logger.info("Debug mode enabled")

    def push_scope(self) -> None:
        """
        Push a new scope onto the stack for blocks, functions, or classes.
        Ensures variable isolation and proper scoping.
        """
        self.scopes.append({})
        logger.debug(f"Pushed new scope. Total scopes: {len(self.scopes)}")

    def pop_scope(self) -> None:
        """
        Pop the current scope from the stack.
        Prevents popping the global scope to maintain program state.
        """
        if len(self.scopes) > 1:
            self.scopes.pop()
            logger.debug(f"Popped scope. Total scopes: {len(self.scopes)}")
        else:
            raise RuntimeError("Cannot pop global scope")

    def get_variable(self, name: str, line: int, column: int) -> Any:
        """
        Retrieve a variable from the current or outer scopes.
        Raises NameError if the variable is undefined.
        """
        for scope in reversed(self.scopes):
            if name in scope:
                logger.debug(f"Found variable '{name}' in scope")
                return scope[name]
        raise RuntimeError(format_error(
            "NameError",
            f"Undefined variable: {name}",
            self.filename,
            self.source,
            line,
            column
        ))

    def set_variable(self, name: str, value: Any, line: int, column: int, is_const: bool = False) -> Any:
        """
        Set a variable in the appropriate scope.
        Prevents reassignment of const variables and logs updates.
        """
        if is_const and name in self.const_vars:
            raise RuntimeError(format_error(
                "TypeError",
                f"Cannot reassign const variable: {name}",
                self.filename,
                self.source,
                line,
                column
            ))
        for scope in reversed(self.scopes):
            if name in scope:
                if name in self.const_vars:
                    raise RuntimeError(format_error(
                        "TypeError",
                        f"Cannot reassign const variable: {name}",
                        self.filename,
                        self.source,
                        line,
                        column
                    ))
                scope[name] = value
                logger.debug(f"Updated variable '{name}' to {value}")
                return value
        self.scopes[-1][name] = value
        if is_const:
            self.const_vars.add(name)
        logger.debug(f"Set new variable '{name}' to {value}, const={is_const}")
        return value

    def check_type(self, value: Any, type_token: Token, line: int, column: int, is_nullable: bool = False) -> None:
        """
        Verify that a value matches the expected type.
        Supports basic types, collections, pointers, and custom types (classes/enums).
        """
        logger.debug(f"Checking type: value={value}, expected={type_token.value}, nullable={is_nullable}")
        if value is None:
            if is_nullable or type_token.value == 'Nothing':
                return
            raise TypeError(format_error(
                "TypeError",
                f"Expected {type_token.value}, got None",
                self.filename,
                self.source,
                line,
                column
            ))
        expected_type = type_token.value
        if expected_type == 'Int':
            if not isinstance(value, int):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected Int, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type in ('Float', 'Double'):
            if not isinstance(value, (int, float)):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected {expected_type}, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type == 'String':
            if not isinstance(value, str):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected String, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type == 'Char':
            if not (isinstance(value, str) and len(value) == 1):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected Char, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type == 'Byte':
            if not (isinstance(value, int) and 0 <= value <= 255):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected Byte, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type == 'Boolean':
            if not isinstance(value, bool):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected Boolean, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type == 'Unit':
            if value is not None:
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected Unit, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type == 'Nothing':
            raise TypeError(format_error(
                "TypeError",
                f"Expected Nothing, got {type(value).__name__}",
                self.filename,
                self.source,
                line,
                column
            ))
        elif expected_type == 'Any':
            return
        elif expected_type.startswith('List<'):
            if not isinstance(value, list):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected List, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type.startswith('Map<'):
            if not isinstance(value, dict):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected Map, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type.startswith('Set<'):
            if not isinstance(value, set):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected Set, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type.startswith('Array<'):
            if not isinstance(value, list):
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected Array, got {type(value).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        elif expected_type.endswith('*'):
            base_type = expected_type[:-1]
            if not isinstance(value, int) or value not in self.memory:
                raise TypeError(format_error(
                    "TypeError",
                    f"Expected pointer to {base_type}, got invalid pointer",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
            mem_ref = self.memory[value]
            if not mem_ref.is_valid:
                raise RuntimeError(format_error(
                    "RuntimeError",
                    f"Dereferencing invalid pointer",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
            if base_type and mem_ref.value is not None:
                self.check_type(
                    mem_ref.value,
                    Token(value=base_type, type=TokenType.VARIABLE, line=line, column=column),
                    line,
                    column,
                    mem_ref.type_token(True).is_nullable
                )
        elif expected_type in self.classes or expected_type in self.enums:
            if isinstance(value, ClassInstance) and value.class_name == expected_type:
                return
            if isinstance(value, EnumInstance) and value.enum_name == expected_type:
                return
            raise TypeError(format_error(
                "TypeError",
                f"Expected {expected_type}, got {type(value).__name__}",
                self.filename,
                self.source,
                line,
                column
            ))
        else:
            raise RuntimeError(format_error(
                "RuntimeError",
                f"Unknown type: {expected_type}",
                self.filename,
                self.source,
                line,
                column
            ))

    def check_modifiers(self, modifiers: List[Token], line: int, column: int) -> None:
        """
        Validate access modifiers for classes, methods, and fields.
        Ensures modifiers like 'private' or 'protected' are used correctly.
        """
        for mod in modifiers:
            if mod.value in ('private', 'protected') and not self.current_class:
                raise RuntimeError(format_error(
                    "SyntaxError",
                    f"Modifier '{mod.value}' is only allowed inside classes",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
            if mod.value == 'protected':
                if not any(self.current_class in super_class for super_class in self.super_class_stack):
                    raise RuntimeError(format_error(
                        "TypeError",
                        "Protected access is only allowed within the class or its subclasses",
                        self.filename,
                        self.source,
                        line,
                        column
                    ))
            if mod.value in ('abstract', 'sealed', 'data', 'object') and not isinstance(
                self.classes.get(self.current_class),
                (ClassNode, SealedClassNode, DataClassNode, ObjectNode)
            ):
                raise RuntimeError(format_error(
                    "SyntaxError",
                    f"Modifier '{mod.value}' is only allowed for classes",
                    self.filename,
                    self.source,
                    line,
                    column
                ))

    def interpret(self, node: Union[ExpressionNode, StatementNode]) -> Optional[Any]:
        """
        Main dispatch method for interpreting AST nodes.
        Routes to specific interpret methods based on node type.
        Includes debug logging for AST traversal.
        """
        try:
            if self.debug_mode:
                logger.debug(f"Interpreting node: {type(node).__name__} at line {getattr(node, 'line', 1)}")
            if isinstance(node, ProgramNode):
                return self.interpret_program(node)
            elif isinstance(node, PackageNode):
                return self.interpret_package(node)
            elif isinstance(node, NamespaceNode):
                return self.interpret_namespace(node)
            elif isinstance(node, ExportNode):
                return self.interpret_export(node)
            elif isinstance(node, ImportNode):
                return self.interpret_import(node)
            elif isinstance(node, NumberNode):
                return self.interpret_number(node)
            elif isinstance(node, StringNode):
                return self.interpret_string(node)
            elif isinstance(node, RawStringNode):
                return self.interpret_raw_string(node)
            elif isinstance(node, CharNode):
                return self.interpret_char(node)
            elif isinstance(node, ByteNode):
                return self.interpret_byte(node)
            elif isinstance(node, HexNode):
                return self.interpret_hex(node)
            elif isinstance(node, BooleanNode):
                return self.interpret_boolean(node)
            elif isinstance(node, NullNode):
                return self.interpret_null(node)
            elif isinstance(node, VariableNode):
                return self.interpret_variable(node)
            elif isinstance(node, AssignNode):
                return self.interpret_assign(node)
            elif isinstance(node, VarDeclarationNode):
                return self.interpret_var_declaration(node)
            elif isinstance(node, ValDeclarationNode):
                return self.interpret_val_declaration(node)
            elif isinstance(node, ConstDeclarationNode):
                return self.interpret_const_declaration(node)
            elif isinstance(node, BinaryOperationNode):
                return self.interpret_binary_op(node)
            elif isinstance(node, UnaryOperationNode):
                return self.interpret_unary_op(node)
            elif isinstance(node, NullCoalesceNode):
                return self.interpret_null_coalesce(node)
            elif isinstance(node, ElvisNode):
                return self.interpret_elvis(node)
            elif isinstance(node, IfNode):
                return self.interpret_if(node)
            elif isinstance(node, WhenNode):
                return self.interpret_when(node)
            elif isinstance(node, WhileNode):
                return self.interpret_while(node)
            elif isinstance(node, DoWhileNode):
                return self.interpret_do_while(node)
            elif isinstance(node, ForNode):
                return self.interpret_for(node)
            elif isinstance(node, SwitchNode):
                return self.interpret_switch(node)
            elif isinstance(node, BreakNode):
                return node
            elif isinstance(node, ContinueNode):
                return node
            elif isinstance(node, TryNode):
                return self.interpret_try(node)
            elif isinstance(node, ThrowNode):
                return self.interpret_throw(node)
            elif isinstance(node, FunctionDefNode):
                return self.interpret_function_def(node)
            elif isinstance(node, FunctionCallNode):
                return self.interpret_function_call(node)
            elif isinstance(node, MemberCallNode):
                return self.interpret_member_call(node)
            elif isinstance(node, LambdaNode):
                return self.interpret_lambda(node)
            elif isinstance(node, ListComprehensionNode):
                return self.interpret_list_comprehension(node)
            elif isinstance(node, ClassNode):
                return self.interpret_class_def(node)
            elif isinstance(node, StructNode):
                return self.interpret_struct_def(node)
            elif isinstance(node, DataClassNode):
                return self.interpret_data_class_def(node)
            elif isinstance(node, SealedClassNode):
                return self.interpret_sealed_class_def(node)
            elif isinstance(node, ObjectNode):
                return self.interpret_object_def(node)
            elif isinstance(node, CompanionObjectNode):
                return self.interpret_companion_object(node)
            elif isinstance(node, InterfaceNode):
                return self.interpret_interface_def(node)
            elif isinstance(node, EnumNode):
                return self.interpret_enum_def(node)
            elif isinstance(node, ReturnNode):
                return self.interpret_return(node)
            elif isinstance(node, PrintNode):
                return self.interpret_print(node)
            elif isinstance(node, InputNode):
                return self.interpret_input(node)
            elif isinstance(node, ReadLineNode):
                return self.interpret_read_line(node)
            elif isinstance(node, InstanceOfNode):
                return self.interpret_instanceof(node)
            elif isinstance(node, NewNode):
                return self.interpret_new(node)
            elif isinstance(node, AllocNode):
                return self.interpret_alloc(node)
            elif isinstance(node, FreeNode):
                return self.interpret_free(node)
            elif isinstance(node, DerefNode):
                return self.interpret_deref(node)
            elif isinstance(node, ReferenceNode):
                return self.interpret_reference(node)
            elif isinstance(node, ThisNode):
                return self.interpret_this(node)
            elif isinstance(node, SuperNode):
                return self.interpret_super(node)
            elif isinstance(node, YieldNode):
                return self.interpret_yield(node)
            elif isinstance(node, AwaitNode):
                return self.interpret_await(node)
            elif isinstance(node, AnnotationNode):
                return node
            elif isinstance(node, BlockNode):
                return self.interpret_block(node)
            else:
                raise RuntimeError(format_error(
                    "TypeError",
                    f"Unknown node type: {type(node).__name__}",
                    self.filename,
                    self.source,
                    getattr(node, 'line', 1),
                    getattr(node, 'column', 1)
                ))
        except Exception as e:
            logger.error(f"Error interpreting node {type(node).__name__}: {str(e)}")
            raise RuntimeError(format_error(
                "RuntimeError",
                str(e),
                self.filename,
                self.source,
                getattr(node, 'line', 1),
                getattr(node, 'column', 1)
            ))

    def interpret_program(self, node: ProgramNode) -> Optional[Any]:
        """
        Interpret a program node, handling package, namespace, imports, statements, and exports.
        Executes all top-level statements sequentially.
        """
        logger.info("Interpreting program")
        if node.package:
            self.interpret_package(node.package)
        if node.namespace:
            self.interpret_namespace(node.namespace)
        for imp in node.imports or []:
            self.interpret_import(imp)
        for stmt in node.statements:
            result = self.interpret(stmt)
            if self.debug_mode:
                logger.debug(f"Program statement result: {result}")
            if isinstance(result, ReturnNode):
                return self.interpret_return(result)
        for exp in node.exports or []:
            self.interpret_export(exp)
        return None

    def interpret_package(self, node: PackageNode) -> None:
        """
        Set the package name for the module.
        Defines the module's organizational structure.
        """
        self.package = '.'.join(t.value for t in node.package)
        logger.debug(f"Set package: {self.package}")

    def interpret_namespace(self, node: NamespaceNode) -> None:
        """
        Set the namespace for the module.
        Scopes identifiers to avoid naming conflicts.
        """
        self.namespace = '.'.join(t.value for t in node.namespace)
        logger.debug(f"Set namespace: {self.namespace}")

    def interpret_export(self, node: ExportNode) -> None:
        """
        Export specified names to make them available to other modules.
        Validates that exported names exist in the global scope.
        """
        for name in node.names:
            if name.value not in self.global_scope:
                raise RuntimeError(format_error(
                    "NameError",
                    f"Cannot export undefined name: {name.value}",
                    self.filename,
                    self.source,
                    name.line,
                    name.column
                ))
            self.exports.append(name.value)
            logger.debug(f"Exported: {name.value}")

    def interpret_import(self, node: ImportNode) -> None:
        """
        Handle import statements, mapping module names or aliases.
        Supports selective imports and aliasing for module access.
        """
        module_name = '.'.join(t.value for t in node.module)
        if node.names:
            for name in node.names:
                self.imports[name.value] = f"{module_name}.{name.value}"
                logger.debug(f"Imported {name.value} from {module_name}")
        elif node.alias:
            self.imports[node.alias.value] = module_name
            logger.debug(f"Imported {module_name} as {node.alias.value}")
        else:
            self.imports[module_name] = module_name
            logger.debug(f"Imported module: {module_name}")

    def interpret_number(self, node: NumberNode) -> Union[int, float]:
        """
        Convert a number node to an int or float based on its string representation.
        Handles decimal and scientific notation.
        """
        value = node.value
        logger.debug(f"Interpreting number: {value}")
        if '.' in value or 'e' in value.lower():
            return float(value)
        return int(value)

    def interpret_string(self, node: StringNode) -> str:
        """
        Interpret a string node, removing surrounding quotes.
        Ensures proper string literal evaluation.
        """
        value = node.value[1:-1]  # Strip quotes
        logger.debug(f"Interpreted string: {value}")
        return value

    def interpret_raw_string(self, node: RawStringNode) -> str:
        """
        Interpret a raw string node, removing triple quotes.
        Preserves content without escape sequence processing.
        """
        value = node.value[3:-3]  # Strip """ quotes
        logger.debug(f"Interpreted raw string: {value}")
        return value

    def interpret_char(self, node: CharNode) -> str:
        """
        Interpret a char node, extracting the single character.
        Validates single-character content.
        """
        value = node.value[1:-1]  # Strip quotes
        if len(value) != 1:
            raise RuntimeError(format_error(
                "TypeError",
                f"Char literal must be a single character, got '{value}'",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        logger.debug(f"Interpreted char: {value}")
        return value

    def interpret_byte(self, node: ByteNode) -> int:
        """
        Interpret a byte node, converting binary string to int.
        Ensures value fits within 0-255 range.
        """
        value = int(node.value[2:], 2)  # e.g., 0b1010
        if not 0 <= value <= 255:
            raise RuntimeError(format_error(
                "TypeError",
                f"Byte value out of range: {value}",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        logger.debug(f"Interpreted byte: {value}")
        return value

    def interpret_hex(self, node: HexNode) -> int:
        """
        Interpret a hex node, converting hex string to int.
        Supports hexadecimal literals.
        """
        value = int(node.value[2:], 16)  # e.g., 0xFF
        logger.debug(f"Interpreted hex: {value}")
        return value

    def interpret_boolean(self, node: BooleanNode) -> bool:
        """
        Interpret a boolean node, converting to Python bool.
        Maps 'true'/'false' strings to boolean values.
        """
        value = node.value == 'true'
        logger.debug(f"Interpreted boolean: {value}")
        return value

    def interpret_null(self, node: NullNode) -> None:
        """
        Interpret a null node, returning None.
        Represents absence of a value.
        """
        logger.debug("Interpreted null")
        return None

    def interpret_variable(self, node: VariableNode) -> Any:
        """
        Retrieve the value of a variable node.
        Looks up variable in current scope chain.
        """
        value = self.get_variable(node.variable.value, node.variable.line, node.variable.column)
        logger.debug(f"Interpreted variable {node.variable.value}: {value}")
        return value

    def interpret_assign(self, node: AssignNode) -> Optional[Any]:
        """
        Handle assignment, including compound assignments and pointer dereferences.
        Supports operators like =, +=, -=, etc.
        """
        value = self.interpret(node.expression)
        if isinstance(node.target, VariableNode):
            if node.token.type != TokenType.ASSIGN:
                old_value = self.get_variable(node.target.variable.value, node.target.variable.line, node.target.variable.column)
                value = self.perform_operation(old_value, value, node.token.type, node.token.line, node.token.column)
            result = self.set_variable(
                node.target.variable.value,
                value,
                node.target.variable.line,
                node.target.variable.column
            )
            logger.debug(f"Assigned {node.target.variable.value} = {value}")
            return result
        elif isinstance(node.target, DerefNode):
            addr = self.interpret(node.target)
            if not isinstance(addr, int) or addr not in self.memory:
                raise RuntimeError(format_error(
                    "RuntimeError",
                    f"Invalid pointer address: {addr}",
                    self.filename,
                    self.source,
                    node.target.line,
                    node.target.column
                ))
            mem_ref = self.memory[addr]
            if not mem_ref.is_valid:
                raise RuntimeError(format_error(
                    "RuntimeError",
                    f"Dereferencing invalid pointer",
                    self.filename,
                    self.source,
                    node.target.line,
                    node.target.column
                ))
            if node.token.type != TokenType.ASSIGN:
                old_value = mem_ref.value
                value = self.perform_operation(old_value, value, node.token.type, node.token.line, node.token.column)
            mem_ref.value = value
            logger.debug(f"Dereferenced assignment at {addr}: {value}")
            return value
        else:
            raise RuntimeError(format_error(
                "TypeError",
                f"Invalid assignment target",
                self.filename,
                self.source,
                node.target.line,
                node.target.column
            ))

    def interpret_var_declaration(self, node: VarDeclarationNode) -> Any:
        """
        Declare a mutable variable with optional type and initializer.
        Enforces type checking if type is specified.
        """
        value = self.interpret(node.expr) if node.expr else None
        if node.type_token:
            self.check_type(value, node.type_token(True), node.var_token.line, node.var_token.column, is_nullable=True)
        self.check_modifiers(node.modifiers, node.var_token.line, node.var_token.column)
        result = self.set_variable(node.variable.variable.value, value, node.var_token.line, node.var_token.column)
        logger.debug(f"Declared var {node.variable.variable.value} = {value}")
        return result

    def interpret_val_declaration(self, node: ValDeclarationNode) -> Any:
        """
        Declare an immutable variable with optional type and initializer.
        Marks variable as constant to prevent reassignment.
        """
        value = self.interpret(node.expr) if node.expr else None
        if node.type_token:
            self.check_type(value, node.type_token(True), node.val_token.line, node.val_token.column, is_nullable=True)
        self.check_modifiers(node.modifiers, node.val_token.line, node.val_token.column)
        result = self.set_variable(
            node.variable.variable.value,
            value,
            node.val_token.line,
            node.val_token.column,
            is_const=True
        )
        logger.debug(f"Declared val {node.variable.variable.value} = {value}")
        return result

    def interpret_const_declaration(self, node: ConstDeclarationNode) -> Any:
        """
        Declare a constant with required initializer.
        Enforces type checking and immutability.
        """
        if not node.expr:
            raise RuntimeError(format_error(
                "SyntaxError",
                f"Const declaration requires an initializer",
                self.filename,
                self.source,
                node.const_token.line,
                node.const_token.column
            ))
        value = self.interpret(node.expr)
        if node.type_token:
            self.check_type(value, node.type_token(True), node.const_token.line, node.const_token.column, is_nullable=True)
        self.check_modifiers(node.modifiers, node.const_token.line, node.const_token.column)
        result = self.set_variable(
            node.variable.variable.value,
            value,
            node.const_token.line,
            node.const_token.column,
            is_const=True
        )
        logger.debug(f"Declared const {node.variable.variable.value} = {value}")
        return result

    def perform_operation(self, left: Any, right: Any, op: TokenType, line: int, column: int) -> Any:
        """
        Perform compound assignment operations (e.g., +=, -=).
        Validates operand types and handles division by zero.
        """
        try:
            if op == TokenType.PLUS_ASSIGN:
                return left + right
            elif op == TokenType.MINUS_ASSIGN:
                return left - right
            elif op == TokenType.MULTIPLY_ASSIGN:
                return left * right
            elif op == TokenType.DIVIDE_ASSIGN:
                if right == 0:
                    raise RuntimeError(format_error(
                        "RuntimeError",
                        "Division by zero",
                        self.filename,
                        self.source,
                        line,
                        column
                    ))
                return left / right
            elif op == TokenType.MODULO_ASSIGN:
                return left % right
            else:
                raise RuntimeError(format_error(
                    "RuntimeError",
                    f"Unknown operator: {op}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        except TypeError:
            raise RuntimeError(format_error(
                "TypeError",
                f"Invalid types for operation {op}",
                self.filename,
                self.source,
                line,
                column
            ))

    def interpret_binary_op(self, node: BinaryOperationNode) -> Any:
        """
        Interpret binary operations, including arithmetic, logical, and bitwise.
        Supports operator overloading for class instances.
        """
        left = self.interpret(node.left_node)
        right = self.interpret(node.right_node)
        op = node.operator.type
        # Check for operator overloading
        if isinstance(left, ClassInstance):
            op_key = (left.class_name, op.name)
            if op_key in self.operator_overloads:
                func = self.operator_overloads[op_key]
                return self.call_overloaded_operator(func, left, right, node.operator.line, node.operator.column)
        try:
            if op == TokenType.PLUS:
                if isinstance(left, str) or isinstance(right, str):
                    return str(left) + str(right)
                return left + right
            elif op == TokenType.MINUS:
                return left - right
            elif op == TokenType.MULTIPLY:
                return left * right
            elif op == TokenType.DIVIDE:
                if right == 0:
                    raise RuntimeError(format_error(
                        "RuntimeError",
                        "Division by zero",
                        self.filename,
                        self.source,
                        node.operator.line,
                        node.operator.column
                    ))
                return left / right
            elif op == TokenType.MODULO:
                return left % right
            elif op == TokenType.EQUAL:
                return left == right
            elif op == TokenType.NOT_EQUAL:
                return left != right
            elif op == TokenType.LESS:
                return left < right
            elif op == TokenType.GREATER:
                return left > right
            elif op == TokenType.LESS_EQUAL:
                return left <= right
            elif op == TokenType.GREATER_EQUAL:
                return left >= right
            elif op == TokenType.AND:
                return bool(left) and bool(right)
            elif op == TokenType.OR:
                return bool(left) or bool(right)
            elif op == TokenType.BIT_AND:
                if isinstance(left, int) and isinstance(right, int):
                    return left & right
                raise TypeError(format_error(
                    "TypeError",
                    "Bitwise AND requires integers",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
            elif op == TokenType.BIT_OR:
                if isinstance(left, int) and isinstance(right, int):
                    return left | right
                raise TypeError(format_error(
                    "TypeError",
                    "Bitwise OR requires integers",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
            elif op == TokenType.BIT_XOR:
                if isinstance(left, int) and isinstance(right, int):
                    return left ^ right
                raise TypeError(format_error(
                    "TypeError",
                    "Bitwise XOR requires integers",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
            elif op == TokenType.SHL:
                if isinstance(left, int) and isinstance(right, int):
                    return left << right
                raise TypeError(format_error(
                    "TypeError",
                    "Left shift requires integers",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
            elif op == TokenType.SHR:
                if isinstance(left, int) and isinstance(right, int):
                    return left >> right
                raise TypeError(format_error(
                    "TypeError",
                    "Right shift requires integers",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
            else:
                raise RuntimeError(format_error(
                    "RuntimeError",
                    f"Unknown operator: {op}",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
        except TypeError as e:
            raise RuntimeError(format_error(
                "TypeError",
                f"Type error in operation {op}: {str(e)}",
                self.filename,
                self.source,
                node.operator.line,
                node.operator.column
            ))

    def call_overloaded_operator(self, func: FunctionDefNode, left: Any, right: Any, line: int, column: int) -> Any:
        """
        Call an overloaded operator method defined in a class.
        Sets up 'this' and parameter in a new scope.
        """
        self.push_scope()
        try:
            if len(func.params) != 1:
                raise TypeError(format_error(
                    "TypeError",
                    f"Operator overload expects 1 parameter, got {len(func.params)}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
            param = func.params[0]
            self.set_variable(param.name.value, right, param.name.line, param.name.column)
            self.set_variable('this', left, line, column)
            result = self.interpret(func.body)
            if isinstance(result, ReturnNode):
                return self.interpret_return(result)
            return result
        finally:
            self.pop_scope()

    def interpret_unary_op(self, node: UnaryOperationNode) -> Any:
        """
        Interpret unary operations, including prefix and postfix operators.
        Supports negation, logical NOT, bitwise NOT, and increment/decrement.
        """
        operand = self.interpret(node.operand)
        op = node.operator.type
        if node.is_postfix:
            if not isinstance(node.operand, VariableNode):
                raise RuntimeError(format_error(
                    "SyntaxError",
                    "Postfix operator requires a variable",
                    self.filename,
                    self.source,
                    node.line,
                    node.column
                ))
            var_name = node.operand.variable.value
            old_value = self.get_variable(var_name, node.operand.variable.line, node.operand.variable.column)
            if not isinstance(old_value, (int, float)):
                raise TypeError(format_error(
                    "TypeError",
                    f"Postfix operator requires numeric value, got {type(old_value).__name__}",
                    self.filename,
                    self.source,
                    node.line,
                    node.column
                ))
            if op == TokenType.INCREMENT:
                self.set_variable(var_name, old_value + 1, node.line, node.column)
                return old_value
            elif op == TokenType.DECREMENT:
                self.set_variable(var_name, old_value - 1, node.line, node.column)
                return old_value
        try:
            if op == TokenType.MINUS:
                return -operand
            elif op == TokenType.NOTATION:
                return not operand
            elif op == TokenType.BIT_NOT:
                if isinstance(operand, int):
                    return ~operand
                raise TypeError(format_error(
                    "TypeError",
                    "Bitwise NOT requires an integer",
                    self.filename,
                    self.source,
                    node.line,
                    node.column
                ))
            elif op == TokenType.INCREMENT:
                if not isinstance(operand, (int, float)):
                    raise TypeError(format_error(
                        "TypeError",
                        f"Increment requires numeric value, got {type(operand).__name__}",
                        self.filename,
                        self.source,
                        node.line,
                        node.column
                    ))
                return operand + 1
            elif op == TokenType.DECREMENT:
                if not isinstance(operand, (int, float)):
                    raise TypeError(format_error(
                        "TypeError",
                        f"Decrement requires numeric value, got {type(operand).__name__}",
                        self.filename,
                        self.source,
                        node.line,
                        node.column
                    ))
                return operand - 1
            else:
                raise RuntimeError(format_error(
                    "RuntimeError",
                    f"Unknown unary operator: {op}",
                    self.filename,
                    self.source,
                    node.line,
                    node.column
                ))
        except TypeError as e:
            raise RuntimeError(format_error(
                "TypeError",
                f"Type error in unary operation {op}: {str(e)}",
                self.filename,
                self.source,
                node.line,
                node.column
            ))

    def interpret_null_coalesce(self, node: NullCoalesceNode) -> Any:
        """
        Interpret null-coalescing operator (??).
        Returns left operand if non-null, otherwise right operand.
        """
        left = self.interpret(node.left_node)
        result = left if left is not None else self.interpret(node.right_node)
        logger.debug(f"Null coalesce: {left} ?? {result}")
        return result

    def interpret_elvis(self, node: ElvisNode) -> Any:
        """
        Interpret Elvis operator (?:).
        Returns left operand if non-null, otherwise right operand.
        """
        left = self.interpret(node.left_node)
        result = left if left is not None else self.interpret(node.right_node)
        logger.debug(f"Elvis: {left} ?: {result}")
        return result

    def interpret_if(self, node: IfNode) -> Any:
        """
        Interpret if statement, evaluating condition and executing appropriate branch.
        Ensures condition evaluates to a boolean.
        """
        condition = self.interpret(node.condition)
        if not isinstance(condition, bool):
            raise TypeError(format_error(
                "TypeError",
                f"Condition must be boolean, got {type(condition).__name__}",
                self.filename,
                self.source,
                node.condition.line,
                node.condition.column
            ))
        if condition:
            result = self.interpret(node.then_branch)
        elif node.el:
            result = self.interpret(node.el_branch)
        else:
            result = None
        logger.debug(f"If result: {result}")
        return result

    def interpret_when(self, node: WhenNode) -> Optional[Any]:
        """
        Interpret when statement, matching conditions or executing else branch.
        Supports pattern matching without an expression.
        """
        expr_value = self.interpret(node.expression) if node.expression else None
        for case in node.cases:
            for cond in case.conditions:
                cond_value = self.interpret(cond)
                if expr_value is None or cond_value == expr_value:
                    result = self.interpret(case.body[0])
                    logger.debug(f"When matched: {cond_value}, result={result}")
                    return result
        if node.el_branch:
            result = self.interpret(node.el_branch)
            logger.debug(f"When else: {result}")
            return result
        logger.debug("When no match")
        return None

    def interpret_while(self, node: WhileNode) -> Optional[Any]:
        """
        Interpret while loop, handling break, continue, and return statements.
        Ensures condition evaluates to a boolean.
        """
        while True:
            condition = self.interpret(node.condition)
            if not isinstance(condition, bool):
                raise TypeError(format_error(
                    "TypeError",
                    f"Condition must be boolean, got {type(condition).__name__}",
                    self.filename,
                    self.source,
                    node.condition.line,
                    node.condition.column
                ))
            if not condition:
                break
            result = self.interpret(node.body)
            if isinstance(result, BreakNode):
                break
            elif isinstance(result, ReturnNode):
                return result
            elif isinstance(result, ContinueNode):
                continue
        logger.debug("While loop exited")
        return None

    def interpret_do_while(self, node: DoWhileNode) -> Optional[Any]:
        """
        Interpret do-while loop, ensuring at least one iteration.
        Handles break, continue, and return statements.
        """
        while True:
            result = self.interpret(node.body)
            if isinstance(result, BreakNode):
                break
            elif isinstance(result, ReturnNode):
                return result
            elif isinstance(result, ContinueNode):
                continue
            condition = self.interpret(node.condition)
            if not isinstance(condition, bool):
                raise TypeError(format_error(
                    "TypeError",
                    f"Condition must be boolean, got {type(condition).__name__}",
                    self.filename,
                    self.source,
                    node.condition.line,
                    node.condition.column
                ))
            if not condition:
                break
        logger.debug("Do-while loop exited")
        return None

    def interpret_for(self, node: ForNode) -> Optional[Any]:
        """
        Interpret for loop, managing initialization, condition, step, and scope.
        Handles break, continue, and return statements.
        """
        self.push_scope()
        try:
            if node.init:
                self.interpret(node.init)
            while node.condition is None or self.interpret(node.condition):
                result = self.interpret(node.body)
                if isinstance(result, BreakNode):
                    break
                elif isinstance(result, ReturnNode):
                    return result
                elif isinstance(result, ContinueNode):
                    if node.step:
                        self.interpret(node.step)
                    continue
                if node.step:
                    self.interpret(node.step)
            logger.debug("For loop exited")
            return None
        finally:
            self.pop_scope()

    def interpret_switch(self, node: SwitchNode) -> Optional[Any]:
        """
        Interpret switch statement, matching cases or executing default branch.
        Evaluates expression and compares with case values.
        """
        value = self.interpret(node.expression)
        for case in node.choices:
            case_value = self.interpret(case.value)
            if value == case_value:
                result = self.interpret(case.body[0])
                logger.debug(f"Switch matched: {case_value}, result={result}")
                return result
        if node.default:
            result = self.interpret(node.default)
            logger.debug(f"Switch default: {result}")
            return result
        logger.debug("Switch no match")
        return None

    def interpret_try(self, node: TryNode) -> Optional[Any]:
        """
        Interpret try-catch-finally block, handling exceptions.
        Executes finally block regardless of outcome.
        """
        try:
            return self.interpret(node.body)
        except RuntimeError as e:
            for catch in node.catches:
                self.push_scope()
                try:
                    self.set_variable(
                        catch.exception_var.variable.value,
                        str(e),
                        catch.variable.line,
                        catch.variable.column
                    )
                    if catch.type_token:
                        self.check_type(
                            str(e),
                            catch.type_token(True),
                            catch.exception_var.line,
                            catch.exception_var.column
                        )
                    result = self.interpret(catch.body)
                    logger.debug(f"Caught exception: {str(e)}, result={result}")
                    return result
                finally:
                    self.pop_scope()
            raise
        finally:
            if node.finally_block:
                self.interpret(node.finally_block)
                logger.debug("Executed finally block")

    def interpret_throw(self, node: ThrowNode) -> None:
        """
        Interpret throw statement, raising an exception.
        Converts expression value to string for error message.
        """
        value = self.interpret(node.expression)
        logger.error(f"Throwing: {value}")
        raise RuntimeError(format_error(
            "RuntimeError",
            str(value),
            self.filename,
            self.source,
            node.expression.line,
            node.expression.column
        ))

    def interpret_function_def(self, node: FunctionDefNode) -> None:
        """
        Define a function, storing it for later calls.
        Supports async functions and access modifiers.
        """
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        if 'async' in [m.value for m in node.modifiers]:
            node.is_async = True
        self.functions[node.name.value] = node
        logger.debug(f"Defined function: {node.name.value}")

    def interpret_function_call(self, node: FunctionCallNode) -> Any:
        """
        Call a function, handling built-in and user-defined functions.
        Validates argument count and types, supports async functions.
        """
        func_name = node.func.variable.value
        args = [self.interpret(arg) for arg in node.args]
        if func_name in self.built_ins:
            result = self.built_ins[func_name](*args, line=node.func.line, column=node.func.column)
            logger.debug(f"Called built-in {func_name}: {result}")
            return result
        if func_name not in self.functions:
            raise TypeError(format_error(
                "NameError",
                f"Undefined function: {func_name}",
                self.filename,
                self.source,
                node.func.line,
                node.func.column
            ))
        func = self.functions[func_name]
        self.check_modifiers(func.modifiers, node.func.line, node.func.column)
        if len(args) != len(func.params):
            raise TypeError(format_error(
                "TypeError",
                f"Expected {len(func.params)} arguments, got {len(args)}",
                self.filename,
                self.source,
                node.func.line,
                node.func.column
            ))
        self.push_scope()
        try:
            for param, arg in zip(func.params, args):
                if param.type_token:
                    self.check_type(arg, param.type_token(True), param.name.line, param.name.column, param.is_nullable)
                if arg is None and not param.is_nullable:
                    raise TypeError(format_error(
                        "TypeError",
                        f"Parameter {param.name.value} is not nullable",
                        self.filename,
                        self.source,
                        param.name.line,
                        param.name.column
                    ))
                self.set_variable(param.name.value, arg, param.name.line, param.name.column)
            result = self.interpret(func.body)
            if isinstance(result, ReturnNode):
                return_value = self.interpret_return(result)
                if func.return_type:
                    self.check_type(return_value, func.return_type(True), node.func.line, node.func.column)
                if getattr(func, 'is_async', False):
                    return asyncio.ensure_future(self.async_wrapper(return_value))
                logger.debug(f"Function {func_name} returned: {return_value}")
                return return_value
            if func.return_type and func.return_type(True).value != 'Unit':
                raise RuntimeError(format_error(
                    "TypeError",
                    f"Function {func_name} must return a value of type {func.return_type(True).value}",
                    self.filename,
                    self.source,
                    node.func.line,
                    node.func.column
                ))
            return None
        finally:
            self.pop_scope()

    async def async_wrapper(self, value: Any) -> Any:
        """
        Wrap synchronous results for async functions.
        Awaits coroutines if necessary.
        """
        if isinstance(value, Coroutine):
            result = await value
            logger.debug(f"Async wrapper: {result}")
            return result
        return value

    def interpret_member_call(self, node: MemberCallNode) -> Any:
        """
        Call a method on an object instance.
        Validates method existence, arguments, and access modifiers.
        """
        obj = self.interpret(node.obj)
        if not isinstance(obj, ClassInstance):
            raise TypeError(format_error(
                "TypeError",
                f"Member call requires an object, got {type(obj).__name__}",
                self.filename,
                self.source,
                node.method.line,
                node.method.column
            ))
        method_name = node.method.value
        if method_name not in obj.methods:
            raise RuntimeError(format_error(
                "NameError",
                f"Undefined method {method_name} in class {obj.class_name}",
                self.filename,
                self.source,
                node.method.line,
                node.method.column
            ))
        method = obj.methods[method_name]
        self.check_modifiers(method.modifiers, node.method.line, node.method.column)
        args = [self.interpret(arg) for arg in node.args]
        if len(args) != len(method.params):
            raise TypeError(format_error(
                "TypeError",
                f"Expected {len(method.params)} arguments, got {len(args)}",
                self.filename,
                self.source,
                node.method.line,
                node.method.column
            ))
        self.push_scope()
        try:
            self.set_variable('this', obj, node.method.line, node.method.column)
            for param, arg in zip(method.params, args):
                if param.type_token:
                    self.check_type(arg, param.type_token(True), param.name.line, param.name.column, param.is_nullable)
                if arg is None and not param.is_nullable:
                    raise TypeError(format_error(
                        "TypeError",
                        f"Parameter {param.name.value} is not nullable",
                        self.filename,
                        self.source,
                        param.name.line,
                        param.name.column
                    ))
                self.set_variable(param.name.value, arg, param.name.line, param.name.column)
            result = self.interpret(method.body)
            if isinstance(result, ReturnNode):
                return_value = self.interpret_return(result)
                if method.return_type:
                    self.check_type(return_value, method.return_type(True), node.method.line, node.method.column)
                logger.debug(f"Method {method_name} returned: {return_value}")
                return return_value
            if method.return_type and method.return_type(True).value != 'Unit':
                raise RuntimeError(format_error(
                    "TypeError",
                    f"Method {method_name} must return a value of type {method.return_type(True).value}",
                    self.filename,
                    self.source,
                    node.method.line,
                    node.method.column
                ))
            return None
        finally:
            self.pop_scope()

    def interpret_lambda(self, node: LambdaNode) -> LambdaFunc:
        """
        Interpret a lambda expression, capturing the current scope.
        Returns a LambdaFunc object for later invocation.
        """
        lambda_func = LambdaFunc(node, self.scopes[-1])
        logger.debug(f"Created lambda with {len(node.params)} params")
        return lambda_func

    def interpret_list_comprehension(self, node: ListComprehensionNode) -> List[Any]:
        """
        Interpret a list comprehension node.
        Evaluates expression for each iteration over the iterable.
        """
        result = []
        iterable = self.interpret(node.iterable)
        if not isinstance(iterable, (list, set, dict)):
            raise TypeError(format_error(
                "TypeError",
                f"Expected iterable, got {type(iterable).__name__}",
                self.filename,
                self.source,
                node.iterable.line,
                node.iterable.column
            ))
        self.push_scope()
        try:
            for item in iterable:
                self.set_variable(node.variable.value, item, node.variable.line, node.variable.column)
                if node.condition:
                    if not self.interpret(node.condition):
                        continue
                value = self.interpret(node.expression)
                result.append(value)
            logger.debug(f"List comprehension result: {result}")
            return result
        finally:
            self.pop_scope()

    def interpret_class_def(self, node: ClassNode) -> None:
        """
        Define a class, storing its fields and methods.
        Supports inheritance and operator overloading.
        """
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        self.classes[node.name.value] = node
        self.current_class = node.name.value
        self.super_class_stack.append(node.name.value)
        try:
            fields = {}
            methods = {}
            for member in node.body:
                if isinstance(member, (VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode)):
                    value = self.interpret(member)
                    fields[member.variable.variable.value] = value
                elif isinstance(member, FunctionDefNode):
                    if 'operator' in [m.value for m in member.modifiers]:
                        op_type = member.name.value
                        self.operator_overloads[(node.name.value, op_type)] = member
                    methods[member.name.value] = member
            self.global_scope[node.name.value] = lambda: ClassInstance(node.name.value, fields.copy(), methods.copy())
            logger.debug(f"Defined class: {node.name.value}")
        finally:
            self.super_class_stack.pop()
            self.current_class = self.super_class_stack[-1] if self.super_class_stack else None

    def interpret_struct_def(self, node: StructNode) -> None:
        """
        Define a struct, similar to a class but with value semantics.
        Stores fields and methods.
        """
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        self.classes[node.name.value] = node
        fields = {}
        methods = {}
        for member in node.body:
            if isinstance(member, (VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode)):
                value = self.interpret(member)
                fields[member.variable.variable.value] = value
            elif isinstance(member, FunctionDefNode):
                methods[member.name.value] = member
        self.global_scope[node.name.value] = lambda: ClassInstance(node.name.value, fields.copy(), methods.copy())
        logger.debug(f"Defined struct: {node.name.value}")

    def interpret_data_class_def(self, node: DataClassNode) -> None:
        """
        Define a data class with automatic equality and to_string methods.
        Stores fields and user-defined methods.
        """
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        self.classes[node.name.value] = node
        fields = {}
        methods = {}
        for member in node.body:
            if isinstance(member, (VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode)):
                value = self.interpret(member)
                fields[member.variable.variable.value] = value
            elif isinstance(member, FunctionDefNode):
                methods[member.name.value] = member
        self.global_scope[node.name.value] = lambda: ClassInstance(node.name.value, fields.copy(), methods.copy())
        logger.debug(f"Defined data class: {node.name.value}")

    def interpret_sealed_class_def(self, node: SealedClassNode) -> None:
        """
        Define a sealed class, restricting subclassing.
        Stores fields and methods.
        """
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        self.classes[node.name.value] = node
        fields = {}
        methods = {}
        for member in node.body:
            if isinstance(member, (VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode)):
                value = self.interpret(member)
                fields[member.variable.variable.value] = value
            elif isinstance(member, FunctionDefNode):
                methods[member.name.value] = member
        self.global_scope[node.name.value] = lambda: ClassInstance(node.name.value, fields.copy(), methods.copy())
        logger.debug(f"Defined sealed class: {node.name.value}")

    def interpret_object_def(self, node: ObjectNode) -> None:
        """
        Define a singleton object, similar to a class with one instance.
        Stores fields and methods.
        """
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        self.classes[node.name.value] = node
        fields = {}
        methods = {}
        for member in node.body:
            if isinstance(member, (VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode)):
                value = self.interpret(member)
                fields[member.variable.variable.value] = value
            elif isinstance(member, FunctionDefNode):
                methods[member.name.value] = member
        instance = ClassInstance(node.name.value, fields, methods)
        self.global_scope[node.name.value] = instance
        logger.debug(f"Defined object: {node.name.value}")

    def interpret_companion_object(self, node: CompanionObjectNode) -> None:
        """
        Define a companion object for a singleton associated with a class.
        Stores static fields and fields.
        """
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        fields = {}
        methods = {}
        for member in node.body:
            if isinstance(member, (VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode)):
                value = self.interpret(member)
                fields[member.variable.variable.value] = value
            elif isinstance(member, FunctionDefNode):
                methods[member.name.value] = member
        companion_name = f"{self.current_class}.Companion"
        instance = ClassInstance(companion_name, fields, fields)
        self.global_scope[companion_name] = instance
        logger.debug(f"Defined companion object: {companion_name}")
    
    def interpret_interface_def(self, node: node: InterfaceNode) -> None:
        """
        Define an interface, storing it for type checking and implementation.
        Contains method signatures.
        """
        self.interfaces[node.name.value] = node
        logger.debug(f"Registered interface: {node.name}")
        """
    
    def interpret_enum_def(self, node: node: EnumNode) -> None:
        """
        Define an enum, storing its values.
        Supports pattern matching and type safety.
        """
        self.enums[node.name.value] = node
        for value in node.values:
            self.global_scope[f"{node.name.value}.{value.value}"] = EnumInstance(node.name.value, value.value)
            logger.debug(f"Defined enum: {node.name.value} with value {value.value}")
        """

         def interpret_return(self, node: ReturnNode) -> Optional[Any]:
        """
        Interpret return statement, unwrapping the expression value.
        Propagates return through the call stack.
        """
        if node.expression:
            value = self.interpret(node.expression)
            logger.debug(f"Return value: {value}")
            return value
        logger.debug("Return None")
        return None

    def interpret_print(self, node: PrintNode) -> None:
        """
        Interpret print statement, outputting the evaluated expression to console.
        Ensures output is flushed immediately for real-time display.
        """
        value = self.interpret(node.expression)
        print(value, flush=True)
        logger.debug(f"Printed: {value}")

    def interpret_input(self, node: InputNode) -> str:
        """
        Interpret input statement, reading a line from standard input.
        Optionally displays a prompt.
        """
        if node.prompt:
            prompt = self.interpret(node.prompt)
            print(prompt, end='', flush=True)
        value = input()
        logger.debug(f"Input received: {value}")
        return value

    def interpret_read_line(self, node: ReadLineNode) -> str:
        """
        Interpret read_line statement, reading a line from standard input.
        Similar to input but without a prompt.
        """
        value = input()
        logger.debug(f"Read line: {value}")
        return value

    def interpret_instanceof(self, node: InstanceOfNode) -> bool:
        """
        Interpret instanceof operator, checking if an object is an instance of a type.
        Supports classes, interfaces, and enums.
        """
        value = self.interpret(node.expression)
        type_name = node.type_token.value
        if type_name in self.classes:
            return isinstance(value, ClassInstance) and value.class_name == type_name
        elif type_name in self.enums:
            return isinstance(value, EnumInstance) and value.enum_name == type_name
        elif type_name in self.interfaces:
            # Check if value's class implements the interface (simplified)
            if isinstance(value, ClassInstance):
                class_def = self.classes.get(value.class_name)
                return class_def and any(i.name.value == type_name for i in getattr(class_def, 'interfaces', []))
        raise RuntimeError(format_error(
            "TypeError",
            f"Unknown type for instanceof: {type_name}",
            self.filename,
            self.source,
            node.type_token.line,
            node.type_token.column
        ))

    def interpret_new(self, node: NewNode) -> Any:
        """
        Interpret new expression, creating a new instance of a class or struct.
        Initializes fields and calls constructor if defined.
        """
        class_name = node.class_name.value
        if class_name not in self.classes:
            raise RuntimeError(format_error(
                "NameError",
                f"Undefined class: {class_name}",
                self.filename,
                self.source,
                node.class_name.line,
                node.class_name.column
            ))
        class_def = self.classes[class_name]
        args = [self.interpret(arg) for arg in node.args]
        instance = self.global_scope[class_name]()
        if isinstance(class_def, ObjectNode):
            return instance
        # Call constructor if defined
        if 'init' in instance.methods:
            constructor = instance.methods['init']
            if len(args) != len(constructor.params):
                raise TypeError(format_error(
                    "TypeError",
                    f"Constructor expects {len(constructor.params)} arguments, got {len(args)}",
                    self.filename,
                    self.source,
                    node.class_name.line,
                    node.class_name.column
                ))
            self.push_scope()
            try:
                self.set_variable('this', instance, node.class_name.line, node.class_name.column)
                for param, arg in zip(constructor.params, args):
                    if param.type_token:
                        self.check_type(arg, param.type_token(True), param.name.line, param.name.column, param.is_nullable)
                    self.set_variable(param.name.value, arg, param.name.line, param.name.column)
                self.interpret(constructor.body)
            finally:
                self.pop_scope()
        logger.debug(f"Created instance of {class_name}")
        return instance

    def interpret_alloc(self, node: AllocNode) -> int:
        """
        Interpret alloc expression, allocating memory for a pointer.
        Returns a memory address (simulated as an integer).
        """
        type_token = node.type_token(True)
        addr = self.next_mem_addr
        self.memory[addr] = MemoryRef(None, lambda _: type_token)
        self.next_mem_addr += 1
        logger.debug(f"Allocated memory at address {addr}")
        return addr

    def interpret_free(self, node: FreeNode) -> None:
        """
        Interpret free statement, marking a pointer as invalid.
        Prevents use-after-free errors.
        """
        addr = self.interpret(node.expression)
        if not isinstance(addr, int) or addr not in self.memory:
            raise RuntimeError(format_error(
                "RuntimeError",
                f"Invalid pointer address: {addr}",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        self.memory[addr].is_valid = False
        logger.debug(f"Freed memory at address {addr}")

    def interpret_deref(self, node: DerefNode) -> Any:
        """
        Interpret dereference operator, accessing the value at a pointer address.
        Validates pointer validity.
        """
        addr = self.interpret(node.expression)
        if not isinstance(addr, int) or addr not in self.memory:
            raise RuntimeError(format_error(
                "RuntimeError",
                f"Invalid pointer address: {addr}",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        mem_ref = self.memory[addr]
        if not mem_ref.is_valid:
            raise RuntimeError(format_error(
                "RuntimeError",
                f"Dereferencing invalid pointer",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        logger.debug(f"Dereferenced pointer at {addr}: {mem_ref.value}")
        return mem_ref.value

    def interpret_reference(self, node: ReferenceNode) -> int:
        """
        Interpret reference operator, creating a pointer to a variable.
        Allocates memory for the variable's value.
        """
        if not isinstance(node.expression, VariableNode):
            raise RuntimeError(format_error(
                "SyntaxError",
                f"Reference operator requires a variable",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        var_name = node.expression.variable.value
        value = self.get_variable(var_name, node.expression.variable.line, node.expression.variable.column)
        addr = self.next_mem_addr
        self.memory[addr] = MemoryRef(value, lambda _: Token(value='Any', type=TokenType.VARIABLE, line=node.line, column=node.column))
        self.next_mem_addr += 1
        logger.debug(f"Created reference at {addr} for {var_name}")
        return addr

    def interpret_this(self, node: ThisNode) -> Any:
        """
        Interpret this keyword, returning the current object instance.
        Only valid within class methods.
        """
        if not self.current_class:
            raise RuntimeError(format_error(
                "SyntaxError",
                f"'this' is only allowed inside class methods",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        this = self.get_variable('this', node.line, node.column)
        logger.debug(f"Resolved 'this': {this}")
        return this

    def interpret_super(self, node: SuperNode) -> Any:
        """
        Interpret super keyword, accessing the superclass instance.
        Only valid within subclass methods.
        """
        if not self.current_class or not self.super_class_stack:
            raise RuntimeError(format_error(
                "SyntaxError",
                f"'super' is only allowed inside subclass methods",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        # Simplified: Assume single inheritance
        class_def = self.classes.get(self.current_class)
        if not class_def or not getattr(class_def, 'superclass', None):
            raise RuntimeError(format_error(
                "TypeError",
                f"No superclass for {self.current_class}",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        this = self.get_variable('this', node.line, node.column)
        logger.debug(f"Resolved 'super' for {self.current_class}")
        return this  # Super calls access the same instance

    def interpret_yield(self, node: YieldNode) -> Any:
        """
        Interpret yield expression, producing a value for a generator.
        Returns the evaluated expression.
        """
        value = self.interpret(node.expression)
        logger.debug(f"Yielded: {value}")
        return value

    def interpret_await(self, node: AwaitNode) -> Any:
        """
        Interpret await expression, resolving a coroutine.
        Only valid within async functions.
        """
        if not self.async_context:
            raise RuntimeError(format_error(
                "SyntaxError",
                f"'await' is only allowed inside async functions",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        coroutine = self.interpret(node.expression)
        if not isinstance(coroutine, Coroutine):
            raise TypeError(format_error(
                "TypeError",
                f"Expected coroutine, got {type(coroutine).__name__}",
                self.filename,
                self.source,
                node.line,
                node.column
            ))
        result = asyncio.get_event_loop().run_until_complete(coroutine)
        logger.debug(f"Awaited result: {result}")
        return result

    def interpret_block(self, node: BlockNode) -> Optional[Any]:
        """
        Interpret a block of statements, executing each in a new scope.
        Returns the last statement's value or None.
        """
        self.push_scope()
        try:
            result = None
            for stmt in node.statements:
                result = self.interpret(stmt)
                if isinstance(result, (ReturnNode, BreakNode, ContinueNode)):
                    return result
            logger.debug(f"Block result: {result}")
            return result
        finally:
            self.pop_scope()

    # Built-in Functions
    def builtin_len(self, *args, line: int, column: int) -> int:
        """
        Built-in len function, returning the length of a collection or string.
        """
        if len(args) != 1:
            raise TypeError(format_error(
                "TypeError",
                f"len expects 1 argument, got {len(args)}",
                self.filename,
                self.source,
                line,
                column
            ))
        value = args[0]
        if isinstance(value, (str, list, dict, set)):
            return len(value)
        raise TypeError(format_error(
            "TypeError",
            f"len not supported for type {type(value).__name__}",
            self.filename,
            self.source,
            line,
            column
        ))

    def builtin_sqrt(self, *args, line: int, column: int) -> float:
        """
        Built-in sqrt function, returning the square root of a number.
        """
        if len(args) != 1:
            raise TypeError(format_error(
                "TypeError",
                f"sqrt expects 1 argument, got {len(args)}",
                self.filename,
                self.source,
                line,
                column
            ))
        if not isinstance(args[0], (int, float)):
            raise TypeError(format_error(
                "TypeError",
                f"sqrt expects a number, got {type(args[0]).__name__}",
                self.filename,
                self.source,
                line,
                column
            ))
        if args[0] < 0:
            raise ValueError(format_error(
                "ValueError",
                "sqrt of negative number",
                self.filename,
                self.source,
                line,
                column
            ))
        return math.sqrt(args[0])

    def builtin_random(self, *args, line: int, column: int) -> float:
        """
        Built-in random function, returning a random float between 0 and 1.
        """
        if len(args) != 0:
            raise TypeError(format_error(
                "TypeError",
                f"random expects 0 arguments, got {len(args)}",
                self.filename,
                self.source,
                line,
                column
            ))
        return random.random()

    def builtin_time(self, *args, line: int, column: int) -> float:
        """
        Built-in time function, returning the current time in seconds since epoch.
        """
        if len(args) != 0:
            raise TypeError(format_error(
                "TypeError",
                f"time expects 0 arguments, got {len(args)}",
                self.filename,
                self.source,
                line,
                column
            ))
        return time.time()

    def builtin_to_string(self, *args, line: int, column: int) -> str:
        """
        Built-in to_string function, converting a value to its string representation.
        """
        if len(args) != 1:
            raise TypeError(format_error(
                "TypeError",
                f"to_string expects 1 argument, got {len(args)}",
                self.filename,
                self.source,
                line,
                column
            ))
        return str(args[0])

    def builtin_range(self, *args, line: int, column: int) -> List[int]:
        """
        Built-in range function, returning a list of integers.
        Supports start, stop, and step arguments.
        """
        if len(args) < 1 or len(args) > 3:
            raise TypeError(format_error(
                "TypeError",
                f"range expects 1-3 arguments, got {len(args)}",
                self.filename,
                self.source,
                line,
                column
            ))
        for arg in args:
            if not isinstance(arg, int):
                raise TypeError(format_error(
                    "TypeError",
                    f"range arguments must be integers, got {type(arg).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        if len(args) == 1:
            return list(range(args[0]))
        elif len(args) == 2:
            return list(range(args[0], args[1]))
        else:
            if args[2] == 0:
                raise ValueError(format_error(
                    "ValueError",
                    "range step cannot be zero",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
            return list(range(args[0], args[1], args[2]))

    def builtin_abs(self, *args, line: int, column: int) -> Union[int, float]:
        """
        Built-in abs function, returning the absolute value of a number.
        """
        if len(args) != 1:
            raise TypeError(format_error(
                "TypeError",
                f"abs expects 1 argument, got {len(args)}",
                self.filename,
                self.source,
                line,
                column
            ))
        if not isinstance(args[0], (int, float)):
            raise TypeError(format_error(
                "TypeError",
                f"abs expects a number, got {type(args[0]).__name__}",
                self.filename,
                self.source,
                line,
                column
            ))
        return abs(args[0])

    def builtin_join(self, *args, line: int, column: int) -> str:
        """
        Built-in join function, concatenating a list of strings with a separator.
        """
        if len(args) != 2:
            raise TypeError(format_error(
                "TypeError",
                f"join expects 2 arguments, got {len(args)}",
                self.filename,
                self.source,
                line,
                column
            ))
        separator, iterable = args
        if not isinstance(separator, str):
            raise TypeError(format_error(
                "TypeError",
                f"join separator must be a string, got {type(separator).__name__}",
                self.filename,
                self.source,
                line,
                column
            ))
        if not isinstance(iterable, (list, tuple)):
            raise TypeError(format_error(
                "TypeError",
                f"join iterable must be a list or tuple, got {type(iterable).__name__}",
                self.filename,
                self.source,
                line,
                column
            ))
        for item in iterable:
            if not isinstance(item, str):
                raise TypeError(format_error(
                    "TypeError",
                    f"join elements must be strings, got {type(item).__name__}",
                    self.filename,
                    self.source,
                    line,
                    column
                ))
        return separator.join(iterable)   