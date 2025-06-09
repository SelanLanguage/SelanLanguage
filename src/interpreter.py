import asyncio
from abc import ABC
from typing import Any, List, Optional, Callable, Dict, Union, Coroutine
from .token import Token, TokenType, token_types
from .ast import *
from .error import format_error
import sys
import random

class MemoryRef:
    """Represents a memory reference for pointers."""
    def __init__(self, value: Any, type_token: Callable[[bool], Token]):
        self.value = value
        self.type_token = type_token
        self.is_valid = True

class ClassInstance:
    """Represents an instance of a class, struct, or object."""
    def __init__(self, class_name: str, fields: Dict[str, Any], methods: Dict[str, FunctionDefNode]):
        self.class_name = class_name
        self.fields = fields
        self.methods = methods

class EnumInstance:
    """Represents an enum value."""
    def __init__(self, enum_name: str, value: str):
        self.enum_name = enum_name
        self.value = value

class LambdaFunc:
    """Represents a lambda function with captured scope."""
    def __init__(self, node: LambdaNode, scope: Dict[str, Any]):
        self.node = node
        self.captured_scope = scope.copy()

class Generator:
    """Represents a generator for yield expressions."""
    def __init__(self, iterator):
        self.iterator = iterator

class Interpreter:
    """Interprets NeonLang AST with full support for all constructs."""
    def __init__(self, source: str, filename: str):
        self.source = source
        self.filename = filename
        # Scope management
        self.global_scope: Dict[str, Any] = {}
        self.scopes: List[Dict[str, Any]] = [self.global_scope]
        self.const_vars: set = set()  # Track immutable variables
        # Definitions
        self.functions: Dict[str, FunctionDefNode] = {}
        self.classes: Dict[str, Union[ClassNode, StructNode, DataClassNode, SealedClassNode, ObjectNode]] = {}
        self.interfaces: Dict[str, InterfaceNode] = {}
        self.enums: Dict[str, EnumNode] = {}
        # Module system
        self.package: Optional[str] = None
        self.namespace: Optional[str] = None
        self.exports: List[str] = []
        self.imports: Dict[str, str] = {}
        # Runtime state
        self.current_class: Optional[str] = None
        self.super_class_stack: List[str] = []
        self.memory: Dict[int, MemoryRef] = {}  # Simulated heap
        self.next_mem_addr = 0
        # Coroutine state
        self.async_context: bool = False

    def push_scope(self):
        """Push a new scope onto the stack."""
        self.scopes.append({})

    def pop_scope(self):
        """Pop the current scope from the stack."""
        if len(self.scopes) > 1:
            self.scopes.pop()
        else:
            raise RuntimeError("Cannot pop global scope")

    def get_variable(self, name: str, line: int, column: int) -> Any:
        """Retrieve a variable from the current or outer scopes."""
        for scope in reversed(self.scopes):
            if name in scope:
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
        """Set a variable in the appropriate scope."""
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
                return value
        self.scopes[-1][name] = value
        if is_const:
            self.const_vars.add(name)
        return value

    def check_type(self, value: Any, type_token: Token, line: int, column: int, is_nullable: bool = False):
        """Verify that a value matches the expected type."""
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
        elif expected_type.endswith('*'):  # Pointer type
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
                raise RuntimeError(format_error("RuntimeError", f"Dereferencing invalid pointer", self.filename, self.source, line, column))
            # Recursively check the base type if needed
            if base_type and mem_ref.value is not None:
                self.check_type(mem_ref.value, Token(value=base_type, type=TokenType.VARIABLE, line=line, column=column), line, column, mem_ref.type_token.is_nullable)
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

    def check_modifiers(self, modifiers: List[Token], line: int, column: int):
        """Validate access modifiers."""
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
            if mod.value in ('abstract', 'sealed', 'data', 'object') and not isinstance(self.classes.get(self.current_class), (ClassNode, SealedClassNode, DataClassNode, ObjectNode)):
                raise RuntimeError(format_error(
                    "SyntaxError",
                    f"Modifier '{mod.value}' is only allowed for classes",
                    self.filename,
                    self.source,
                    line,
                    column
                ))

    def interpret(self, node: Union[ExpressionNode, StatementNode]) -> Optional[Any]:
        """Main dispatch method for interpreting AST nodes."""
        try:
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
                return node.value[1:-1]
            elif isinstance(node, RawStringNode):
                return node.value[3:-3]  # Remove triple quotes
            elif isinstance(node, CharNode):
                return node.value[1]
            elif isinstance(node, ByteNode):
                return int(node.value[2:], 16)  # e.g., 0xFF
            elif isinstance(node, HexNode):
                return int(node.value[2:], 16)
            elif isinstance(node, BooleanNode):
                return node.value == 'true'
            elif isinstance(node, NullNode):
                return None
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
                return node  # Annotations are stored as metadata
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
            raise RuntimeError(format_error(
                "RuntimeError",
                str(e),
                self.filename,
                self.source,
                getattr(node, 'line', 1),
                getattr(node, 'column', 1)
            ))

    def interpret_program(self, node: ProgramNode) -> Optional[Any]:
        """Interpret a program with package, imports."""
        if node.package:
            self.interpret_package(node.package)
        if node.namespace:
            self.interpret_namespace(node.namespace)
            for imp in node.imports:
                self.interpret_import(imp)
            for stmt in node.statements:
                result = self.interpret(stmt)
                if isinstance(result, ReturnNode):
                    return self.interpret_return(result)
            for exp in node.exports or []:
                self.interpret_export(exp)
        return None

    def interpret_package(self, node: PackageNode) -> None:
        """Set the package name."""
        self.package = '.'.join(t.value for t in node.package)

    def interpret_namespace(self, node: NamespaceNode) -> None:
        """Set the namespace."""
        self.namespace = '.'.join(t.value for t in node.namespace)

    def interpret_export(self, node: ExportNode) -> None:
        """Export specified names."""
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

    def interpret_import(self, node: ImportNode) -> None:
        """Handle import statements."""
        module_name = '.'.join(t.value for t in node.module)
        if node.names:
            for name in node.names:
                self.imports[name.value] = f"{module_name}.{name.value}"
        elif node.alias:
            self.imports[node.alias.value] = module_name
        else:
            self.imports[module_name] = module_name

    def interpret_number(self, node: NumberNode) -> Union[int, float]:
        """Convert number node to int or float."""
        value = node.value
        if '.' in value or 'e' in value.lower():
            return float(value)
        return int(value)

    def interpret_variable(self, node: VariableNode) -> Any:
        """Retrieve variable value."""
        return self.get_variable(node.variable.value, node.variable.line, node.variable.column)

    def interpret_assign(self, node: AssignNode) -> Optional[Any]:
        """Handle assignments, including pointer dereferences."""
        value = self.interpret(node.expression)
        if isinstance(node.target, VariableNode):
            if node.token.type != TokenType.ASSIGN:
                old_value = self.get_variable(node.target.variable.value, node.target.variable.line, node.target.variable.column)
                value = self.perform_operation(old_value, value, node.token.type, node.token.line, node.token.column)
            return self.set_variable(
                node.target.variable.value,
                value,
                node.target.variable.line,
                node.target.variable.column
            )
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
        """Declare a mutable variable."""
        value = self.interpret(node.expr) if node.expr else None
        if node.type_token:
            self.check_type(value, node.type_token(True), node.var_token.line, node.var_token.column, is_nullable=True)
        self.check_modifiers(node.modifiers, node.var_token.line, node.var_token.column)
        return self.set_variable(node.variable.variable.value, value, node.var_token.line, node.var_token.column)

    def interpret_val_declaration(self, node: ValDeclarationNode) -> Any:
        """Declare an immutable variable."""
        value = self.interpret(node.expr) if node.expr else None
        if node.type_token:
            self.check_type(value, node.type_token(True), node.val_token.line, node.val_token.column, is_nullable=True)
        self.check_modifiers(node.modifiers, node.val_token.line, node.val_token.column)
        return self.set_variable(node.variable.variable.value, value, node.val_token.line, node.val_token.column, is_const=True)

    def interpret_const_declaration(self, node: ConstDeclarationNode) -> Any:
        """Declare a constant."""
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
        return self.set_variable(node.variable.variable.value, value, node.const_token.line, node.const_token.column, is_const=True)

    def perform_operation(self, left: Any, right: Any, op: TokenType, line: int, column: int) -> Any:
        """Perform binary operation for compound assignments."""
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
        """Interpret binary operations."""
        left = self.interpret(node.left_node)
        right = self.interpret(node.right_node)
        op = node.operator.type
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

    def interpret_unary_op(self, node: UnaryOperationNode) -> Any:
        """Interpret unary operations."""
        operand = self.interpret(node.operand)
        op = node.operator.type
        if node.is_postfix:
            if not isinstance(node.operand, VariableNode):
                raise RuntimeError(format_error(
                    "SyntaxError",
                    "Postfix operator requires a variable",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
            var_name = node.operand.variable.value
            old_value = self.get_variable(var_name, node.operand.variable.line, node.operand.variable.column)
            if not isinstance(old_value, (int, float)):
                raise TypeError(format_error(
                    "TypeError",
                    f"Postfix operator requires numeric value, got {type(old_value).__name__}",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
            if op == TokenType.INCREMENT:
                self.set_variable(var_name, old_value + 1, node.operand.variable.line, node.operand.variable.column)
                return old_value
            elif op == TokenType.DECREMENT:
                self.set_variable(var_name, old_value - 1, node.operand.variable.line, node.operand.variable.column)
                return old_value
        try:
            if op == TokenType.MINUS:
                return -operand
            elif op == TokenType.NOT:
                return not operand
            elif op == TokenType.BIT_NOT:
                if isinstance(operand, int):
                    return ~operand
                raise TypeError(format_error(
                    "TypeError",
                    "Bitwise NOT requires an integer",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
            elif op == TokenType.INCREMENT:
                if not isinstance(operand, (int, float)):
                    raise TypeError(format_error(
                        "TypeError",
                        f"Increment requires numeric value, got {type(operand).__name__}",
                        self.filename,
                        self.source,
                        node.operator.line,
                        node.operator.column
                    ))
                return operand + 1
            elif op == TokenType.DECREMENT:
                if not isinstance(operand, (int, float)):
                    raise TypeError(format_error(
                        "TypeError",
                        f"Decrement requires numeric value, got {type(operand).__name__}",
                        self.filename,
                        self.source,
                        node.operator.line,
                        node.operator.column
                    ))
                return operand - 1
            else:
                raise RuntimeError(format_error(
                    "RuntimeError",
                    f"Unknown unary operator: {op}",
                    self.filename,
                    self.source,
                    node.operator.line,
                    node.operator.column
                ))
        except TypeError as e:
            raise RuntimeError(format_error(
                "TypeError",
                f"Type error in unary operation {op}: {str(e)}",
                self.filename,
                self.source,
                node.operator.line,
                node.operator.column
            ))

    def interpret_null_coalesce(self, node: NullCoalesceNode) -> Any:
        """Interpret null-coalescing operator (??)."""
        left = self.interpret(node.left_node)
        return left if left is not None else self.interpret(node.right_node)

    def interpret_elvis(self, node: ElvisNode) -> Any:
        """Interpret Elvis operator (?:)."""
        left = self.interpret(node.left_node)
        return left if left is not None else self.interpret(node.right_node)

    def interpret_if(self, node: IfNode) -> Any:
        """Interpret if statement."""
        condition = self.interpret(node.condition)
        if not isinstance(condition, bool):
            raise TypeError(format_error(
                "TypeError",
                f"Condition must be boolean, got {type(condition).__name__}",
                self.filename,
                self.source,
                getattr(node.condition, 'line', 1),
                getattr(node.condition, 'column', 1)
            ))
        if condition:
            return self.interpret(node.then_branch)
        elif node.else_branch:
            return self.interpret(node.else_branch)
        return None

    def interpret_when(self, node: WhenNode) -> Any:
        """Interpret when statement."""
        expr_value = self.interpret(node.expression) if node.expression else None
        for case in node.cases:
            for cond in case.conditions:
                cond_value = self.interpret(cond)
                if expr_value is None or cond_value == expr_value:
                    return self.interpret(case.body)
        if node.else_branch:
            return self.interpret(node.else_branch)
        return None

    def interpret_while(self, node: WhileNode) -> Any:
        """Interpret while loop."""
        while True:
            condition = self.interpret(node.condition)
            if not isinstance(condition, bool):
                raise TypeError(format_error(
                    "TypeError",
                    f"Condition must be boolean, got {type(condition).__name__}",
                    self.filename,
                    self.source,
                    getattr(node.condition, 'line', 1),
                    getattr(node.condition, 'column', 1)
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
        return None

    def interpret_do_while(self, node: DoWhileNode) -> Any:
        """Interpret do-while loop."""
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
                    getattr(node.condition, 'line', 1),
                    getattr(node.condition, 'column', 1)
                ))
            if not condition:
                break
        return None

    def interpret_for(self, node: ForNode) -> Any:
        """Interpret for loop."""
        self.push_scope()
        try:
            if node.init:
                self.interpret(node.init)
            while node.cond is None or self.interpret(node.cond):
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
            return None
        finally:
            self.pop_scope()

    def interpret_switch(self, node: SwitchNode) -> Any:
        """Interpret switch statement."""
        value = self.interpret(node.expression)
        for case in node.cases:
            case_value = self.interpret(case.value)
            if value == case_value:
                return self.interpret(case.body)
        if node.default:
            return self.interpret(node.default)
        return None

    def interpret_try(self, node: TryNode) -> Any:
        """Interpret try-catch-finally."""
        try:
            return self.interpret(node.try_block)
        except RuntimeError as e:
            for catch in node.catches:
                self.push_scope()
                try:
                    self.set_variable(catch.exception_var.variable.value, str(e), catch.exception_var.variable.line, catch.exception_var.variable.column)
                    if catch.type_token:
                        self.check_type(str(e), catch.type_token(True), catch.exception_var.variable.line, catch.exception_var.variable.column)
                    return self.interpret(catch.body)
                finally:
                    self.pop_scope()
            raise
        finally:
            if node.finally_block:
                self.interpret(node.finally_block)

    def interpret_throw(self, node: ThrowNode) -> None:
        """Interpret throw statement."""
        value = self.interpret(node.expression)
        raise RuntimeError(format_error(
            "RuntimeError",
            str(value),
            self.filename,
            self.source,
            getattr(node.expression, 'line', 1),
            getattr(node.expression, 'column', 1)
        ))

    def interpret_function_def(self, node: FunctionDefNode) -> None:
        """Define a function."""
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        if 'async' in [m.value for m in node.modifiers]:
            node.is_async = True
        self.functions[node.name.value] = node
        return None

    def interpret_function_call(self, node: FunctionCallNode) -> Any:
        """Call a function."""
        func_name = node.func.variable.value
        if func_name not in self.functions:
            raise RuntimeError(format_error(
                "NameError",
                f"Undefined function: {func_name}",
                self.filename,
                self.source,
                node.func.variable.line,
                node.func.variable.column
            ))
        func = self.functions[func_name]
        self.check_modifiers(func.modifiers, node.func.variable.line, node.func.variable.column)
        args = [self.interpret(arg) for arg in node.args]
        if len(args) != len(func.params):
            raise TypeError(format_error(
                "TypeError",
                f"Expected {len(func.params)} arguments, got {len(args)}",
                self.filename,
                self.source,
                node.func.variable.line,
                node.func.variable.column
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
                    self.check_type(return_value, func.return_type(True), node.func.variable.line, node.func.variable.column)
                if getattr(func, 'is_async', False):
                    return asyncio.ensure_future(self.async_wrapper(return_value))
                return return_value
            if func.return_type and func.return_type(True).value != 'Unit':
                raise RuntimeError(format_error(
                    "TypeError",
                    f"Function {func_name} must return a value of type {func.return_type(True).value}",
                    self.filename,
                    self.source,
                    node.func.variable.line,
                    node.func.variable.column
                ))
            return None
        finally:
            self.pop_scope()

    async def async_wrapper(self, value: Any) -> Any:
        """Wrap synchronous results for async functions."""
        if isinstance(value, Coroutine):
            return await value
        return value

    def interpret_member_call(self, node: MemberCallNode) -> Any:
        """Call a method on an object."""
        obj = self.interpret(node.obj)
        if not isinstance(obj, ClassInstance):
            raise TypeError(format_error(
                "TypeError",
                f"Member call requires an object, got {type(obj).__name__}",
                self.filename,
                self.source,
                node.method.variable.line,
                node.method.variable.column
            ))
        method_name = node.method.variable.value
        if method_name not in obj.methods:
            raise RuntimeError(format_error(
                "NameError",
                f"Undefined method {method_name} in class {obj.class_name}",
                self.filename,
                self.source,
                node.method.variable.line,
                node.method.variable.column
            ))
        method = obj.methods[method_name]
        self.check_modifiers(method.modifiers, node.method.variable.line, node.method.variable.column)
        args = [self.interpret(arg) for arg in node.args]
        if len(args) != len(method.params):
            raise TypeError(format_error(
                "TypeError",
                f"Expected {len(method.params)} arguments, got {len(args)}",
                self.filename,
                self.source,
                node.method.variable.line,
                node.method.variable.column
            ))
        self.push_scope()
        try:
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
            self.set_variable('this', obj, node.method.variable.line, node.method.variable.column)
            result = self.interpret(method.body)
            if isinstance(result, ReturnNode):
                return_value = self.interpret_return(result)
                if method.return_type:
                    self.check_type(return_value, method.return_type(True), node.method.variable.line, node.method.variable.column)
                return return_value
            return None
        finally:
            self.pop_scope()

    def interpret_lambda(self, node: LambdaNode) -> LambdaFunc:
        """Create a lambda function."""
        return LambdaFunc(node, self.scopes[-1])

    def interpret_class_def(self, node: ClassNode) -> None:
        """Define a class."""
        prev_class = self.current_class
        self.current_class = node.name.value
        self.classes[node.name.value] = node
        if node.superclass:
            superclass_name = node.superclass.variable.value
            if superclass_name not in self.classes:
                raise RuntimeError(format_error(
                    "NameError",
                    f"Undefined superclass: {superclass_name}",
                    self.filename,
                    self.source,
                    node.superclass.variable.line,
                    node.superclass.variable.column
                ))
            self.super_class_stack.append(superclass_name)
        for interface in node.interfaces:
            interface_name = interface.variable.value
            if interface_name not in self.interfaces:
                raise RuntimeError(format_error(
                    "NameError",
                    f"Undefined interface: {interface_name}",
                    self.filename,
                    self.source,
                    interface.variable.line,
                    interface.variable.column
                ))
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        for member in node.members:
            self.interpret(member)
        self.current_class = prev_class
        if node.superclass:
            self.super_class_stack.pop()

    def interpret_struct_def(self, node: StructNode) -> None:
        """Define a struct."""
        self.classes[node.name.value] = node
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        prev_class = self.current_class
        self.current_class = node.name.value
        for member in node.members:
            self.interpret(member)
        self.current_class = prev_class

    def interpret_data_class_def(self, node: DataClassNode) -> None:
        """Define a data class."""
        self.classes[node.name.value] = node
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        prev_class = self.current_class
        self.current_class = node.name.value
        for member in node.members:
            self.interpret(member)
        self.current_class = prev_class

    def interpret_sealed_class_def(self, node: SealedClassNode) -> None:
        """Define a sealed class."""
        self.classes[node.name.value] = node
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        prev_class = self.current_class
        self.current_class = node.name.value
        if node.superclass:
            superclass_name = node.superclass.variable.value
            if superclass_name not in self.classes:
                raise RuntimeError(format_error(
                    "NameError",
                    f"Undefined superclass: {superclass_name}",
                    self.filename,
                    self.source,
                    node.superclass.variable.line,
                    node.superclass.variable.column
                ))
            self.super_class_stack.append(superclass_name)
        for interface in node.interfaces:
            interface_name = interface.variable.value
            if interface_name not in self.interfaces:
                raise RuntimeError(format_error(
                    "NameError",
                    f"Undefined interface: {interface_name}",
                    self.filename,
                    self.source,
                    interface.variable.line,
                    interface.variable.column
                ))
        for member in node.members:
            self.interpret(member)
        self.current_class = prev_class
        if node.superclass:
            self.super_class_stack.pop()

    def interpret_object_def(self, node: ObjectNode) -> None:
        """Define a singleton object."""
        self.classes[node.name.value] = node
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)
        prev_class = self.current_class
        self.current_class = node.name.value
        instance = ClassInstance(node.name.value, {}, {})
        self.set_variable(node.name.value, instance, node.name.line, node.name.column, is_const=True)
        for member in node.members:
            self.interpret(member)
        self.current_class = prev_class

    def interpret_companion_object(self, node: CompanionObjectNode) -> None:
        """Define a companion object."""
        if not self.current_class:
            raise RuntimeError(format_error(
                "SyntaxError",
                "Companion object must be defined inside a class",
                self.filename,
                self.source,
                getattr(node, 'line', 1),
                getattr(node, 'column', 1)
            ))
        for member in node.members:
            self.interpret(member)

    def interpret_interface_def(self, node: InterfaceNode) -> None:
        """Define an interface."""
        self.interfaces[node.name.value] = node
        self.check_modifiers(node.modifiers, node.name.line, node.name.column)

    def interpret_enum_def(self, node: EnumNode) -> None:
        """Define an enum."""
        self.enums[node.name.value] = node
        for value in node.values:
            enum_instance = EnumInstance(node.name.value, value.value)
            self.set_variable(value.value, enum_instance, value.line, value.column, is_const=True)
        prev_class = self.current_class
        self.current_class = node.name.value
        for member in node.members:
            self.interpret(member)
        self.current_class = prev_class

    def interpret_return(self, node: ReturnNode) -> Any:
        """Interpret return statement."""
        return self.interpret(node.expression) if node.expression else None

    def interpret_print(self, node: PrintNode) -> None:
        """Interpret print statement."""
        value = self.interpret(node.expression)
        print(value)

    def interpret_input(self, node: InputNode) -> str:
        """Interpret input statement."""
        if node.prompt:
            prompt = self.interpret(node.prompt)
            return input(str(prompt))
        return input()

    def interpret_read_line(self, node: ReadLineNode) -> str:
        """Interpret readline statement."""
        return input()

    def interpret_instanceof(self, node: InstanceOfNode) -> bool:
        """Interpret instanceof operator."""
        value = self.interpret(node.expression)
        type_name = node.type_token.value
        if type_name == 'Int':
            return isinstance(value, int)
        elif type_name in ('Float', 'Double'):
            return isinstance(value, float)
        elif type_name == 'Boolean':
            return isinstance(value, bool)
        elif type_name == 'String':
            return isinstance(value, str)
        elif type_name == 'Char':
            return isinstance(value, str) and len(value) == 1
        elif type_name == 'Byte':
            return isinstance(value, int) and 0 <= value <= 255
        elif type_name == 'Any':
            return True
        elif type_name in self.classes:
            return isinstance(value, ClassInstance) and value.class_name == type_name
        elif type_name in self.enums:
            return isinstance(value, EnumInstance) and value.enum_name == type_name
        elif type_name.endswith('*'):
            return isinstance(value, int) and value in self.memory
        return False

    def interpret_new(self, node: NewNode) -> ClassInstance:
        """Create a new instance."""
        class_name = node.type_token.value
        if class_name not in self.classes:
            raise RuntimeError(format_error(
                "NameError",
                f"Undefined class: {class_name}",
                self.filename,
                self.source,
                node.type_token.line,
                node.type_token.column
            ))
        class_def = self.classes[class_name]
        args = [self.interpret(arg) for arg in node.args]
        fields = {}
        methods = {}
        self.push_scope()
        try:
            self.set_variable('this', ClassInstance(class_name, fields, methods), node.type_token.line, node.type_token.column)
            for member in class_def.members:
                if isinstance(member, (VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode)):
                    value = self.interpret(member)
                    fields[member.variable.variable.value] = value
                elif isinstance(member, FunctionDefNode):
                    methods[member.name.value] = member
            instance = ClassInstance(class_name, fields, methods)
            self.set_variable('this', instance, node.type_token.line, node.type_token.column)
            return instance
        finally:
            self.pop_scope()

    def interpret_alloc(self, node: AllocNode) -> int:
        """Allocate memory and return a pointer."""
        type_token = node.type_token(True)
        addr = self.next_mem_addr
        self.next_mem_addr += 1
        self.memory[addr] = MemoryRef(None, node.type_token)
        return addr

    def interpret_free(self, node: FreeNode) -> None:
        """Free allocated memory."""
        addr = self.interpret(node.expression)
        if not isinstance(addr, int) or addr not in self.memory:
            raise RuntimeError(format_error(
                "RuntimeError",
                f"Invalid pointer address: {addr}",
                self.filename,
                self.source,
                node.expression.line,
                node.expression.column
            ))
        self.memory[addr].is_valid = False
        del self.memory[addr]

    def interpret_deref(self, node: DerefNode) -> Any:
        """Dereference a pointer."""
        addr = self.interpret(node.expression)
        if not isinstance(addr, int) or addr not in self.memory:
            raise RuntimeError(format_error(
                "RuntimeError",
                f"Invalid pointer address: {addr}",
                self.filename,
                self.source,
                node.expression.line,
                node.expression.column
            ))
        mem_ref = self.memory[addr]
        if not mem_ref.is_valid:
            raise RuntimeError(format_error(
                "RuntimeError",
                f"Dereferencing invalid pointer",
                self.filename,
                self.source,
                node.expression.line,
                node.expression.column
            ))
        return mem_ref.value

    def interpret_reference(self, node: ReferenceNode) -> int:
        """Get the address of a variable."""
        if not isinstance(node.expression, VariableNode):
            raise RuntimeError(format_error(
                "SyntaxError",
                f"Reference operator requires a variable",
                self.filename,
                self.source,
                node.expression.line,
                node.expression.column
            ))
        var_name = node.expression.variable.value
        value = self.get_variable(var_name, node.expression.variable.line, node.expression.variable.column)
        addr = self.next_mem_addr
        self.next_mem_addr += 1
        self.memory[addr] = MemoryRef(value, lambda x: Token(value='Any', type=TokenType.VARIABLE, line=node.expression.line, column=node.expression.column))
        return addr

    def interpret_this(self, node: ThisNode) -> ClassInstance:
        """Interpret this keyword."""
        if not self.current_class:
            raise RuntimeError(format_error(
                "SyntaxError",
                "'this' can only be used inside a class",
                self.filename,
                self.source,
                node.token.line,
                node.token.column
            ))
        return self.get_variable('this', node.token.line, node.token.column)

    def interpret_super(self, node: SuperNode) -> ClassInstance:
        """Interpret super keyword."""
        if not self.current_class or not self.super_class_stack:
            raise RuntimeError(format_error(
                "SyntaxError",
                "'super' can only be used inside a class with a superclass",
                self.filename,
                self.source,
                node.token.line,
                node.token.column
            ))
        this_obj = self.get_variable('this', node.token.line, node.token.column)
        superclass_name = self.super_class_stack[-1]
        return ClassInstance(superclass_name, this_obj.fields.copy(), this_obj.methods.copy())

    def interpret_yield(self, node: YieldNode) -> Generator:
        """Interpret yield expression."""
        value = self.interpret(node.expression) if node.expression else None
        def generator():
            yield value
        return Generator(generator())

    def interpret_await(self, node: AwaitNode) -> Any:
        """Interpret await expression."""
        if not self.async_context:
            raise RuntimeError(format_error(
                "SyntaxError",
                "'await' can only be used inside async functions",
                self.filename,
                self.source,
                node.expression.line,
                node.expression.column
            ))
        value = self.interpret(node.expression)
        if not isinstance(value, Coroutine):
            raise TypeError(format_error(
                "TypeError",
                f"'await' requires a coroutine, got {type(value).__name__}",
                self.filename,
                self.source,
                node.expression.line,
                node.expression.column
            ))
        async def run():
            return await value
        return asyncio.run(run())

    def interpret_block(self, node: BlockNode) -> Any:
        """Interpret a block of statements."""
        self.push_scope()
        try:
            for stmt in node.statements:
                result = self.interpret(stmt)
                if isinstance(result, (ReturnNode, BreakNode, ContinueNode)):
                    return result
            return None
        finally:
            self.pop_scope()