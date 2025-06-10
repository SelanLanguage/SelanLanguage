from abc import ABC
from typing import List, Optional, Callable
from .token import Token

class Node(ABC):
    """Base class for all AST nodes."""
    def __init__(self, line: int = 1, column: int = 1):
        self.line = line
        self.column = column

class ExpressionNode(Node):
    """Base class for expression nodes (e.g., literals, operations)."""
    pass

class StatementNode(Node):
    """Base class for statement nodes (e.g., declarations, control flow)."""
    def __init__(self, annotations: List['AnnotationNode'] = None, line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.annotations = annotations or []

class NumberNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"NumberNode(value={self.value}, line={self.line}, column={self.column})"

class StringNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"StringNode(value={self.value}, line={self.line}, column={self.column})"

class RawStringNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"RawStringNode(value={self.value}, line={self.line}, column={self.column})"

class CharNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"CharNode(value={self.value}, line={self.line}, column={self.column})"

class ByteNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"ByteNode(value={self.value}, line={self.line}, column={self.column})"

class HexNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"HexNode(value={self.value}, line={self.line}, column={self.column})"

class BooleanNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"BooleanNode(value={self.value}, line={self.line}, column={self.column})"

class NullNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"NullNode(value={self.value}, line={self.line}, column={self.column})"

class UnaryOperationNode(ExpressionNode):
    def __init__(self, operator: Token, operand: ExpressionNode, is_postfix: bool = False):
        super().__init__(operator.line, operator.column)
        self.operator = operator
        self.operand = operand
        self.is_postfix = is_postfix

    def __repr__(self):
        return f"UnaryOperationNode(operator={self.operator.value}, operand={self.operand}, is_postfix={self.is_postfix}, line={self.line}, column={self.column})"

class BinaryOperationNode(ExpressionNode):
    def __init__(self, operator: Token, left_node: ExpressionNode, right_node: ExpressionNode):
        super().__init__(operator.line, operator.column)
        self.operator = operator
        self.left_node = left_node
        self.right_node = right_node

    def __repr__(self):
        return f"BinaryOperationNode(operator={self.operator.value}, left={self.left_node}, right={self.right_node}, line={self.line}, column={self.column})"

class NullCoalesceNode(ExpressionNode):
    def __init__(self, left_node: ExpressionNode, right_node: ExpressionNode, line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.left_node = left_node
        self.right_node = right_node

    def __repr__(self):
        return f"NullCoalesceNode(left={self.left_node}, right={self.right_node}, line={self.line}, column={self.column})"

class ElvisNode(ExpressionNode):
    def __init__(self, left_node: ExpressionNode, right_node: ExpressionNode, line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.left_node = left_node
        self.right_node = right_node

    def __repr__(self):
        return f"ElvisNode(left={self.left_node}, right={self.right_node}, line={self.line}, column={self.column})"

class VariableNode(ExpressionNode):
    def __init__(self, variable: Token):
        super().__init__(variable.line, variable.column)
        self.variable = variable

    def __repr__(self):
        return f"VariableNode(name={self.variable.value}, line={self.line}, column={self.column})"

class AssignNode(ExpressionNode):
    def __init__(self, token: Token, target: ExpressionNode, expression: ExpressionNode):
        super().__init__(token.line, token.column)
        self.token = token
        self.target = target
        self.expression = expression

    def __repr__(self):
        return f"AssignNode(operator={self.token.value}, target={self.target}, value={self.expression}, line={self.line}, column={self.column})"

class VarDeclarationNode(StatementNode):
    def __init__(self, var_token: Token, variable: VariableNode, 
                 type_token: Optional[Callable[[bool], Token]] = None, 
                 expr: Optional[ExpressionNode] = None, 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations, var_token.line, var_token.column)
        self.var_token = var_token
        self.variable = variable
        self.type_token = type_token
        self.expr = expr
        self.modifiers = modifiers or []
        self.annotations = annotations or []

    def __repr__(self):
        return f"VarDeclarationNode(name={self.variable}, type={self.type_token(True).value if self.type_token else None}, expr={self.expr}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ValDeclarationNode(StatementNode):
    def __init__(self, val_token: Token, variable: VariableNode, 
                 type_token: Optional[Callable[[bool], Token]] = None, 
                 expr: Optional[ExpressionNode] = None, 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations, val_token.line, val_token.column)
        self.val_token = val_token
        self.variable = variable
        self.type_token = type_token
        self.expr = expr
        self.modifiers = modifiers or []
        self.annotations = annotations or []

    def __repr__(self):
        return f"ValDeclarationNode(name={self.variable}, type={self.type_token(True).value if self.type_token else None}, expr={self.expr}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ConstDeclarationNode(StatementNode):
    def __init__(self, const_token: Token, variable: VariableNode, 
                 type_token: Optional[Callable[[bool], Token]] = None, 
                 expr: Optional[ExpressionNode] = None, 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations, const_token.line, const_token.column)
        self.const_token = const_token
        self.variable = variable
        self.type_token = type_token
        self.expr = expr
        self.modifiers = modifiers or []
        self.annotations = annotations or []

    def __repr__(self):
        return f"ConstDeclarationNode(name={self.variable}, type={self.type_token(True).value if self.type_token else None}, expr={self.expr}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class IfNode(StatementNode):
    def __init__(self, condition: ExpressionNode, then_branch: 'BlockNode', 
                 else_branch: Optional['BlockNode'] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

    def __repr__(self):
        return f"IfNode(condition={self.condition}, then={self.then_branch}, else={self.else_branch}, annotations={self.annotations}, line={self.line}, column={self.column})"

class WhenNode(StatementNode):
    def __init__(self, expression: Optional[ExpressionNode], 
                 cases: List['WhenCaseNode'], 
                 else_branch: Optional['BlockNode'] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.expression = expression
        self.cases = cases
        self.else_branch = else_branch

    def __repr__(self):
        return f"WhenNode(expr={self.expression}, cases={self.cases}, else={self.else_branch}, annotations={self.annotations}, line={self.line}, column={self.column})"

class WhenCaseNode(Node):
    def __init__(self, conditions: List[ExpressionNode], body: 'BlockNode', line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.conditions = conditions
        self.body = body

    def __repr__(self):
        return f"WhenCaseNode(conditions={self.conditions}, body={self.body}, line={self.line}, column={self.column})"

class WhileNode(StatementNode):
    def __init__(self, condition: ExpressionNode, body: 'BlockNode', 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.condition = condition
        self.body = body

    def __repr__(self):
        return f"WhileNode(condition={self.condition}, body={self.body}, annotations={self.annotations}, line={self.line}, column={self.column})"

class DoWhileNode(StatementNode):
    def __init__(self, body: 'BlockNode', condition: ExpressionNode, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.body = body
        self.condition = condition

    def __repr__(self):
        return f"DoWhileNode(body={self.body}, condition={self.condition}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ForNode(StatementNode):
    def __init__(self, init: Optional[StatementNode], 
                 cond: Optional[ExpressionNode], 
                 step: Optional[ExpressionNode], 
                 body: 'BlockNode', 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.init = init
        self.cond = cond
        self.step = step
        self.body = body

    def __repr__(self):
        return f"ForNode(init={self.init}, cond={self.cond}, step={self.step}, body={self.body}, annotations={self.annotations}, line={self.line}, column={self.column})"

class SwitchNode(StatementNode):
    def __init__(self, expression: ExpressionNode, cases: List['CaseNode'], 
                 default: Optional['BlockNode'] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.expression = expression
        self.cases = cases
        self.default = default

    def __repr__(self):
        return f"SwitchNode(expr={self.expression}, cases={self.cases}, default={self.default}, annotations={self.annotations}, line={self.line}, column={self.column})"

class CaseNode(Node):
    def __init__(self, value: ExpressionNode, body: 'BlockNode', line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.value = value
        self.body = body

    def __repr__(self):
        return f"CaseNode(value={self.value}, body={self.body}, line={self.line}, column={self.column})"

class BreakNode(StatementNode):
    def __init__(self, annotations: Optional[List['AnnotationNode']] = None, line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)

    def __repr__(self):
        return f"BreakNode(annotations={self.annotations}, line={self.line}, column={self.column})"

class ContinueNode(StatementNode):
    def __init__(self, annotations: Optional[List['AnnotationNode']] = None, line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)

    def __repr__(self):
        return f"ContinueNode(annotations={self.annotations}, line={self.line}, column={self.column})"

class TryNode(StatementNode):
    def __init__(self, try_block: 'BlockNode', catches: List['CatchNode'], 
                 finally_block: Optional['BlockNode'] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.try_block = try_block
        self.catches = catches
        self.finally_block = finally_block

    def __repr__(self):
        return f"TryNode(try={self.try_block}, catches={self.catches}, finally={self.finally_block}, annotations={self.annotations}, line={self.line}, column={self.column})"

class CatchNode(Node):
    def __init__(self, exception_var: VariableNode, 
                 body: 'BlockNode', 
                 type_token: Optional[Callable[[bool], Token]] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.exception_var = exception_var
        self.type_token = type_token
        self.body = body

    def __repr__(self):
        return f"CatchNode(var={self.exception_var}, type={self.type_token(True).value if self.type_token else None}, body={self.body}, line={self.line}, column={self.column})"

class ThrowNode(StatementNode):
    def __init__(self, expression: ExpressionNode, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.expression = expression

    def __repr__(self):
        return f"ThrowNode(expr={self.expression}, annotations={self.annotations}, line={self.line}, column={self.column})"

class FunctionCallNode(ExpressionNode):
    def __init__(self, func: VariableNode, args: List[ExpressionNode], line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.func = func
        self.args = args

    def __repr__(self):
        return f"FunctionCallNode(func={self.func}, args={self.args}, line={self.line}, column={self.column})"

class FunctionDefNode(StatementNode):
    def __init__(self, name: Token, params: List['ParamNode'], body: 'BlockNode', 
                 return_type: Optional[Callable[[bool], Token]] = None, 
                 modifiers: List[Token] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.name = name
        self.params = params
        self.return_type = return_type
        self.body = body
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"FunctionDefNode(name={self.name.value}, params={self.params}, return_type={self.return_type(True).value if self.return_type else None}, body={self.body}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ParamNode(Node):
    def __init__(self, name: Token, type_token: Optional[Callable[[bool], Token]] = None, 
                 is_nullable: Optional[bool] = False, line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.name = name
        self.type_token = type_token
        self.is_nullable = is_nullable

    def __repr__(self):
        return f"ParamNode(name={self.name.value}, type={self.type_token(True).value if self.type_token else None}, nullable={self.is_nullable}, line={self.line}, column={self.column})"

class LambdaNode(ExpressionNode):
    def __init__(self, params: List[ParamNode], body: ExpressionNode, line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.params = params
        self.body = body

    def __repr__(self):
        return f"LambdaNode(params={self.params}, body={self.body}, line={self.line}, column={self.column})"

class ListComprehensionNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode, variable: Token, 
                 iterable: ExpressionNode, condition: Optional[ExpressionNode] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.expression = expression
        self.variable = variable
        self.iterable = iterable
        self.condition = condition

    def __repr__(self):
        return f"ListComprehensionNode(expression={self.expression}, variable={self.variable.value}, iterable={self.iterable}, condition={self.condition}, line={self.line}, column={self.column})"

class StructNode(StatementNode):
    def __init__(self, name: Token, members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.name = name
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"StructNode(name={self.name.value}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class DataClassNode(StatementNode):
    def __init__(self, name: Token, members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.name = name
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"DataClassNode(name={self.name.value}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class SealedClassNode(StatementNode):
    def __init__(self, name: Token, superclass: Optional[VariableNode], 
                 interfaces: List[VariableNode], members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.name = name
        self.superclass = superclass
        self.interfaces = interfaces
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"SealedClassNode(name={self.name.value}, superclass={self.superclass}, interfaces={self.interfaces}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ObjectNode(StatementNode):
    def __init__(self, name: Token, members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.name = name
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"ObjectNode(name={self.name.value}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class CompanionObjectNode(StatementNode):
    def __init__(self, members: List[StatementNode], 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.members = members

    def __repr__(self):
        return f"CompanionObjectNode(members={self.members}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ClassNode(StatementNode):
    def __init__(self, name: Token, superclass: Optional[VariableNode], 
                 interfaces: List[VariableNode], members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.name = name
        self.superclass = superclass
        self.interfaces = interfaces
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"ClassNode(name={self.name.value}, superclass={self.superclass}, interfaces={self.interfaces}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class InterfaceNode(StatementNode):
    def __init__(self, name: Token, members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.name = name
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"InterfaceNode(name={self.name.value}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations}, line={self.line}, column={self.column})"

class EnumNode(StatementNode):
    def __init__(self, name: Token, values: List[Token], 
                 members: List[StatementNode], 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.name = name
        self.values = values
        self.members = members

    def __repr__(self):
        return f"EnumNode(name={self.name.value}, values={self.values}, members={self.members}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ReturnNode(StatementNode):
    def __init__(self, expression: Optional[ExpressionNode] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.expression = expression

    def __repr__(self):
        return f"ReturnNode(expr={self.expression}, annotations={self.annotations}, line={self.line}, column={self.column})"

class PrintNode(StatementNode):
    def __init__(self, expression: ExpressionNode, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.expression = expression

    def __repr__(self):
        return f"PrintNode(expr={self.expression}, annotations={self.annotations}, line={self.line}, column={self.column})"

class InputNode(StatementNode):
    def __init__(self, prompt: Optional[ExpressionNode] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.prompt = prompt

    def __repr__(self):
        return f"InputNode(prompt={self.prompt}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ReadLineNode(StatementNode):
    def __init__(self, annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)

    def __repr__(self):
        return f"ReadLineNode(annotations={self.annotations}, line={self.line}, column={self.column})"

class InstanceOfNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode, type_token: Token):
        super().__init__(type_token.line, type_token.column)
        self.expression = expression
        self.type_token = type_token

    def __repr__(self):
        return f"InstanceOfNode(expr={self.expression}, type={self.type_token.value}, line={self.line}, column={self.column})"

class NewNode(ExpressionNode):
    def __init__(self, type_token: Token, args: List[ExpressionNode]):
        super().__init__(type_token.line, type_token.column)
        self.type_token = type_token
        self.args = args

    def __repr__(self):
        return f"NewNode(type={self.type_token.value}, args={self.args}, line={self.line}, column={self.column})"

class AllocNode(ExpressionNode):
    def __init__(self, type_token: Callable[[bool], Token], line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.type_token = type_token

    def __repr__(self):
        return f"AllocNode(type={self.type_token(True).value}, line={self.line}, column={self.column})"

class FreeNode(StatementNode):
    def __init__(self, expression: ExpressionNode, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.expression = expression

    def __repr__(self):
        return f"FreeNode(expr={self.expression}, annotations={self.annotations}, line={self.line}, column={self.column})"

class DerefNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode, line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.expression = expression

    def __repr__(self):
        return f"DerefNode(expr={self.expression}, line={self.line}, column={self.column})"

class ReferenceNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode, line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.expression = expression

    def __repr__(self):
        return f"ReferenceNode(expr={self.expression}, line={self.line}, column={self.column})"

class ThisNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"ThisNode(value={self.value}, line={self.line}, column={self.column})"

class SuperNode(ExpressionNode):
    def __init__(self, token: Token):
        super().__init__(token.line, token.column)
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"SuperNode(value={self.value}, line={self.line}, column={self.column})"

class MemberCallNode(ExpressionNode):
    def __init__(self, obj: ExpressionNode, method: VariableNode, 
                 args: List[ExpressionNode], line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.obj = obj
        self.method = method
        self.args = args

    def __repr__(self):
        return f"MemberCallNode(obj={self.obj}, method={self.method}, args={self.args}, line={self.line}, column={self.column})"

class ImportNode(StatementNode):
    def __init__(self, module: List[Token], names: List[Token] = None, 
                 alias: Optional[Token] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.module = module
        self.names = names or []
        self.alias = alias

    def __repr__(self):
        return f"ImportNode(module={self.module}, names={self.names}, alias={self.alias}, annotations={self.annotations}, line={self.line}, column={self.column})"

class PackageNode(StatementNode):
    def __init__(self, package: List[Token], 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.package = package

    def __repr__(self):
        return f"PackageNode(package={self.package}, annotations={self.annotations}, line={self.line}, column={self.column})"

class NamespaceNode(StatementNode):
    def __init__(self, namespace: List[Token], 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.namespace = namespace

    def __repr__(self):
        return f"NamespaceNode(namespace={self.namespace}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ExportNode(StatementNode):
    def __init__(self, names: List[Token], 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.names = names

    def __repr__(self):
        return f"ExportNode(names={self.names}, annotations={self.annotations}, line={self.line}, column={self.column})"

class YieldNode(ExpressionNode):
    def __init__(self, expression: Optional[ExpressionNode] = None, line: int = 1, column: int = 1):
        super().__init__(line, column)
        self.expression = expression

    def __repr__(self):
        return f"YieldNode(expr={self.expression}, line={self.line}, column={self.column})"

class AwaitNode(StatementNode):
    def __init__(self, expression: ExpressionNode, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.expression = expression

    def __repr__(self):
        return f"AwaitNode(expr={self.expression}, annotations={self.annotations}, line={self.line}, column={self.column})"

class AnnotationNode(StatementNode):
    def __init__(self, name: Token, args: List[str], 
                 annotations: List['AnnotationNode'] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.name = name
        self.args = args

    def __repr__(self):
        return f"AnnotationNode(name={self.name.value}, args={self.args}, annotations={self.annotations}, line={self.line}, column={self.column})"

class BlockNode(StatementNode):
    def __init__(self, statements: List[StatementNode], 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.statements = statements

    def __repr__(self):
        return f"BlockNode(statements={self.statements}, annotations={self.annotations}, line={self.line}, column={self.column})"

class ProgramNode(StatementNode):
    def __init__(self, imports: List[ImportNode], statements: List[StatementNode], 
                 package: Optional['PackageNode'] = None, 
                 namespace: Optional['NamespaceNode'] = None, 
                 exports: Optional[List['ExportNode']] = None, 
                 annotations: Optional[List['AnnotationNode']] = None, 
                 line: int = 1, column: int = 1):
        super().__init__(annotations, line, column)
        self.imports = imports or []
        self.statements = statements
        self.package = package
        self.namespace = namespace
        self.exports = exports or []

    def __repr__(self):
        return f"ProgramNode(imports={self.imports}, statements={self.statements}, package={self.package}, namespace={self.namespace}, exports={self.exports}, annotations={self.annotations}, line={self.line}, column={self.column})"