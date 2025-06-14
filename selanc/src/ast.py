from abc import ABC
from typing import List, Optional, Any
from .token import Token, TokenType

class ExpressionNode(ABC):
    """Base class for all AST nodes with line and column tracking."""
    def __init__(self, line: int = 1, column: int = 1):
        self.line: int = line
        self.column: int = column

class FileNode(ExpressionNode):
    def __init__(self, file_obj: Any, token: Token):
        super().__init__(token.line, token.column)
        self.file_obj = file_obj  # Holds Python file object
        self.token = token

    def __repr__(self):
        return f"FileNode(file_obj={self.file_obj}, line={self.line}, col={self.column})"

# Include all other nodes from the provided ast.py without changes
class NumberNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"NumberNode(value={self.value}, line={self.line}, col={self.column})"

class StringNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value[1:-1]  # Strip quotes, e.g., "Unknown" -> Unknown
        if not self.value and token.value not in ('""', ''):
            raise ValueError(f"Invalid string literal at line {self.line}, col {self.column}: {token.value}")

    def __repr__(self):
        return f"StringNode(value={self.value!r}, line={self.line}, col={self.column})"

class CharNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value[1:-1]  # Strip single quotes, e.g., 'a' -> a

    def __repr__(self):
        return f"CharNode(value={self.value!r}, line={self.line}, col={self.column})"

class BooleanNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"BooleanNode(value={self.value}, line={self.line}, col={self.column})"

class NullNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"NullNode(value={self.value}, line={self.line}, col={self.column})"

class UnaryOperationNode(ExpressionNode):
    def __init__(self, operator: Token, operand: ExpressionNode, is_postfix: bool = False):
        super().__init__(operator.line, operator.column)
        self.operator = operator
        self.operand = operand
        self.is_postfix = is_postfix

    def __repr__(self):
        return f"UnaryOperationNode(operator={self.operator}, operand={self.operand}, is_postfix={self.is_postfix}, line={self.line}, col={self.column})"

class BinaryOperationNode(ExpressionNode):
    def __init__(self, operator: Token, left_node: ExpressionNode, right_node: ExpressionNode):
        super().__init__(operator.line, operator.column)
        self.operator = operator
        self.left_node = left_node
        self.right_node = right_node

    def __repr__(self):
        return f"BinaryOperationNode({self.operator}, {self.left_node}, {self.right_node}, line={self.line}, col={self.column})"

class NullCoalesceNode(ExpressionNode):
    def __init__(self, left_node: ExpressionNode, right_node: ExpressionNode):
        super().__init__(left_node.line, left_node.column)
        self.left_node = left_node
        self.right_node = right_node

    def __repr__(self):
        return f"NullCoalesceNode({self.left_node}, {self.right_node}, line={self.line}, col={self.column})"

class ElvisNode(ExpressionNode):
    def __init__(self, left_node: ExpressionNode, right_node: ExpressionNode):
        super().__init__(left_node.line, left_node.column)
        self.left_node = left_node
        self.right_node = right_node

    def __repr__(self):
        return f"ElvisNode({self.left_node}, {self.right_node}, line={self.line}, col={self.column})"

class VariableNode(ExpressionNode):
    def __init__(self, variable: Token):
        super().__init__(variable.line, variable.column)
        self.variable = variable
        if not variable.value.strip():
            raise ValueError(f"Empty variable identifier at line {self.line}, col {self.column}")

    def __repr__(self):
        return f"VariableNode(value={self.variable.value}, line={self.line}, col={self.column})"

class AssignNode(ExpressionNode):
    def __init__(self, token: Token, variable: VariableNode, expression: ExpressionNode):
        super().__init__(token.line, token.column)
        self.token = token
        self.variable = variable
        self.expression = expression

    def __repr__(self):
        return f"AssignNode({self.token}, {self.variable}, {self.expression}, line={self.line}, col={self.column})"

class VarDeclarationNode(ExpressionNode):
    def __init__(self, var_token: Token, variable: VariableNode, type_token: Optional[Token], expr: Optional[ExpressionNode], modifiers: List[Token] = None):
        super().__init__(var_token.line, var_token.column)
        self.var_token = var_token
        self.variable = variable
        self.type_token = type_token
        self.expr = expr
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"VarDeclarationNode({self.var_token}, {self.variable}, {self.type_token}, {self.expr}, modifiers={self.modifiers}, line={self.line}, col={self.column})"

class ValDeclarationNode(ExpressionNode):
    def __init__(self, val_token: Token, variable: VariableNode, type_token: Optional[Token], expr: Optional[ExpressionNode], modifiers: List[Token] = None):
        super().__init__(val_token.line, val_token.column)
        self.val_token = val_token
        self.variable = variable
        self.type_token = type_token
        self.expr = expr
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"ValDeclarationNode({self.val_token}, {self.variable}, {self.type_token}, {self.expr}, modifiers={self.modifiers}, line={self.line}, col={self.column})"

class ConstDeclarationNode(ExpressionNode):
    def __init__(self, const_token: Token, variable: VariableNode, type_token: Optional[Token], expr: Optional[ExpressionNode], modifiers: List[Token] = None):
        super().__init__(const_token.line, const_token.column)
        self.const_token = const_token
        self.variable = variable
        self.type_token = type_token
        self.expr = expr
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"ConstDeclarationNode({self.const_token}, {self.variable}, {self.type_token}, {self.expr}, modifiers={self.modifiers}, line={self.line}, col={self.column})"

class IfNode(ExpressionNode):
    def __init__(self, condition: ExpressionNode, then_branch: 'BlockNode', else_branch: Optional['BlockNode']):
        super().__init__(condition.line, condition.column)
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

    def __repr__(self):
        return f"IfNode({self.condition}, {self.then_branch}, {self.else_branch}, line={self.line}, col={self.column})"

class WhileNode(ExpressionNode):
    def __init__(self, condition: ExpressionNode, body: 'BlockNode'):
        super().__init__(condition.line, condition.column)
        self.condition = condition
        self.body = body

    def __repr__(self):
        return f"WhileNode({self.condition}, {self.body}, line={self.line}, col={self.column})"

class DoWhileNode(ExpressionNode):
    def __init__(self, body: 'BlockNode', condition: ExpressionNode):
        super().__init__(body.line, body.column)
        self.body = body
        self.condition = condition

    def __repr__(self):
        return f"DoWhileNode({self.body}, {self.condition}, line={self.line}, col={self.column})"

class ForNode(ExpressionNode):
    def __init__(self, init: Optional[ExpressionNode], cond: Optional[ExpressionNode], step: Optional[ExpressionNode], body: 'BlockNode'):
        super().__init__(init.line if init else body.line, init.column if init else body.column)
        self.init = init
        self.cond = cond
        self.step = step
        self.body = body

    def __repr__(self):
        return f"ForNode({self.init}, {self.cond}, {self.step}, {self.body}, line={self.line}, col={self.column})"

class SwitchNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode, cases: List['CaseNode'], default: Optional['BlockNode']):
        super().__init__(expression.line, expression.column)
        self.expression = expression
        self.cases = cases
        self.default = default

    def __repr__(self):
        return f"SwitchNode({self.expression}, {self.cases}, {self.default}, line={self.line}, col={self.column})"

class CaseNode(ExpressionNode):
    def __init__(self, value: ExpressionNode, body: ExpressionNode):
        super().__init__(value.line, value.column)
        self.value = value
        self.body = body

    def __repr__(self):
        return f"CaseNode({self.value}, {self.body}, line={self.line}, col={self.column})"

class BreakNode(ExpressionNode):
    def __init__(self, line: int = 1, column: int = 1):
        super().__init__(line, column)

    def __repr__(self):
        return f"BreakNode(line={self.line}, col={self.column})"

class ContinueNode(ExpressionNode):
    def __init__(self, line: int = 1, column: int = 1):
        super().__init__(line, column)

    def __repr__(self):
        return f"ContinueNode(line={self.line}, col={self.column})"

class TryNode(ExpressionNode):
    def __init__(self, try_block: 'BlockNode', catches: List['CatchNode'], finally_block: Optional['BlockNode']):
        super().__init__(try_block.line, try_block.column)
        self.try_block = try_block
        self.catches = catches
        self.finally_block = finally_block

    def __repr__(self):
        return f"TryNode({self.try_block}, {self.catches}, {self.finally_block}, line={self.line}, col={self.column})"

class CatchNode(ExpressionNode):
    def __init__(self, exception_var: VariableNode, type_token: Optional[Token], body: 'BlockNode'):
        super().__init__(exception_var.line, exception_var.column)
        self.exception_var = exception_var
        self.type_token = type_token
        self.body = body

    def __repr__(self):
        return f"CatchNode({self.exception_var}, {self.type_token}, {self.body}, line={self.line}, col={self.column})"

class ThrowNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode):
        super().__init__(expression.line, expression.column)
        self.expression = expression

    def __repr__(self):
        return f"ThrowNode({self.expression}, line={self.line}, col={self.column})"

class FunctionCallNode(ExpressionNode):
    def __init__(self, func: VariableNode, args: List[ExpressionNode]):
        super().__init__(func.line, func.column)
        self.func = func
        self.args = args

    def __repr__(self):
        return f"FunctionCallNode({self.func}, {self.args}, line={self.line}, col={self.column})"

class FunctionDefNode(ExpressionNode):
    def __init__(self, name: Token, params: List['ParameterNode'], return_type: Optional[Token], body: 'BlockNode', modifiers: List[Token] = None):
        super().__init__(name.line, name.column)
        self.name = name
        self.params = params
        self.return_type = return_type
        self.body = body
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"FunctionDefNode({self.name}, {self.params}, {self.return_type}, {self.body}, modifiers={self.modifiers}, line={self.line}, col={self.column})"

class ParameterNode(ExpressionNode):
    def __init__(self, name: Token, type_token: Optional[Token], is_nullable: bool = False):
        super().__init__(name.line, name.column)
        self.name = name
        self.type_token = type_token
        self.is_nullable = is_nullable

    def __repr__(self):
        return f"ParameterNode({self.name}, {self.type_token}, nullable={self.is_nullable}, line={self.line}, col={self.column})"

class LambdaNode(ExpressionNode):
    def __init__(self, params: List['ParameterNode'], body: ExpressionNode):
        super().__init__(params[0].line if params else body.line, params[0].column if params else body.column)
        self.params = params
        self.body = body

    def __repr__(self):
        return f"LambdaNode({self.params}, {self.body}, line={self.line}, col={self.column})"

class ClassNode(ExpressionNode):
    def __init__(self, name: Token, superclass: Optional[VariableNode], interfaces: List[VariableNode], members: List[ExpressionNode], modifiers: List[Token] = None):
        super().__init__(name.line, name.column)
        self.name = name
        self.superclass = superclass
        self.interfaces = interfaces
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"ClassNode({self.name}, {self.superclass}, {self.interfaces}, {self.members}, modifiers={self.modifiers}, line={self.line}, col={self.column})"

class InterfaceNode(ExpressionNode):
    def __init__(self, name: Token, members: List[ExpressionNode], modifiers: List[Token] = None):
        super().__init__(name.line, name.column)
        self.name = name
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"InterfaceNode({self.name}, {self.members}, modifiers={self.modifiers}, line={self.line}, col={self.column})"

class EnumNode(ExpressionNode):
    def __init__(self, name: Token, values: List[Token], members: List[ExpressionNode]):
        super().__init__(name.line, name.column)
        self.name = name
        self.values = values
        self.members = members

    def __repr__(self):
        return f"EnumNode({self.name}, {self.values}, {self.members}, line={self.line}, col={self.column})"

class ReturnNode(ExpressionNode):
    def __init__(self, expression: Optional[ExpressionNode], line: int = 1, column: int = 1):
        super().__init__(expression.line if expression else line, expression.column if expression else column)
        self.expression = expression

    def __repr__(self):
        return f"ReturnNode({self.expression}, line={self.line}, col={self.column})"

class PrintNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode):
        super().__init__(expression.line, expression.column)
        self.expression = expression

    def __repr__(self):
        return f"PrintNode({self.expression}, line={self.line}, col={self.column})"

class InputNode(ExpressionNode):
    def __init__(self, token: Token, prompt: Optional[ExpressionNode] = None):
        super().__init__(token.line, token.column)
        self.token = token
        self.prompt = prompt

    def __repr__(self):
        return f"InputNode(token={self.token}, prompt={self.prompt}, line={self.line}, col={self.column})"

class InstanceOfNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode, type_token: Token):
        super().__init__(expression.line, expression.column)
        self.expression = expression
        self.type_token = type_token

    def __repr__(self):
        return f"InstanceOfNode({self.expression}, {self.type_token}, line={self.line}, col={self.column})"

class NewNode(ExpressionNode):
    def __init__(self, type_token: Token, args: List[ExpressionNode]):
        super().__init__(type_token.line, type_token.column)
        self.type_token = type_token
        self.args = args

    def __repr__(self):
        return f"NewNode({self.type_token}, {self.args}, line={self.line}, col={self.column})"

class ThisNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"ThisNode(value={self.value}, line={self.line}, col={self.column})"

class SuperNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"SuperNode(value={self.value}, line={self.line}, col={self.column})"

class MemberCallNode(ExpressionNode):
    def __init__(self, obj: ExpressionNode, method: VariableNode, args: List[ExpressionNode]):
        super().__init__(obj.line, obj.column)
        self.obj = obj
        self.method = method
        self.args = args

    def __repr__(self):
        return f"MemberCallNode(obj={self.obj}, method={self.method}, args={self.args}, line={self.line}, col={self.column})"

class ImportNode(ExpressionNode):
    def __init__(self, module: List[Token], names: List[Token] = None, line: int = 1, column: int = 1):
        super().__init__(module[0].line if module else line, module[0].column if module else column)
        self.module = module
        self.names = names or []
        self.alias = None  # Simplified, assuming alias not used

    def __repr__(self):
        return f"ImportNode(module={self.module}, names={self.names}, alias={self.alias}, line={self.line}, col={self.column})"

class BlockNode(ExpressionNode):
    def __init__(self, statements: List[ExpressionNode], line: int = 1, column: int = 1):
        super().__init__(statements[0].line if statements else line, statements[0].column if statements else column)
        self.statements = statements

    def __repr__(self):
        return f"BlockNode({self.statements}, line={self.line}, col={self.column})"

class StatementsNode(ExpressionNode):
    def __init__(self, line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.code_strings: List[ExpressionNode] = []

    def add_node(self, node: ExpressionNode):
        self.code_strings.append(node)
        if self.line == 1 and self.column == 1 and hasattr(node, 'line') and hasattr(node, 'column'):
            self.line = node.line
            self.column = node.column

    def __repr__(self):
        return f"StatementsNode({self.code_strings}, line={self.line}, col={self.column})"

class ProgramNode(ExpressionNode):
    def __init__(self, imports: List['ImportNode'], statements: List[ExpressionNode]):
        super().__init__(imports[0].line if imports else statements[0].line if statements else 1,
                        imports[0].column if imports else statements[0].column if statements else 1)
        self.imports = imports
        self.statements = statements

    def __repr__(self):
        return f"ProgramNode({self.imports}, {self.statements}, line={self.line}, col={self.column})"