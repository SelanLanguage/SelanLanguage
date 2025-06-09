from abc import ABC
from typing import List, Optional, Callable
from .token import Token

class Node(ABC):
    """Base class for all AST nodes."""
    pass

class ExpressionNode(Node):
    """Base class for expression nodes (e.g., literals, operations)."""
    pass

class StatementNode(Node):
    """Base class for statement nodes (e.g., declarations, control flow)."""
    def __init__(self, annotations: List['AnnotationNode'] = None):
        self.annotations = annotations or []

class NumberNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"NumberNode(value={self.value})"

class StringNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"StringNode(value={self.value})"

class RawStringNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"RawStringNode(value={self.value})"

class CharNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"CharNode(value={self.value})"

class ByteNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"ByteNode(value={self.value})"

class HexNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"HexNode(value={self.value})"

class BooleanNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"BooleanNode(value={self.value})"

class NullNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"NullNode(value={self.value})"

class UnaryOperationNode(ExpressionNode):
    def __init__(self, operator: Token, operand: ExpressionNode, is_postfix: bool = False):
        self.operator = operator
        self.operand = operand
        self.is_postfix = is_postfix

    def __repr__(self):
        return f"UnaryOperationNode(operator={self.operator.value}, operand={self.operand}, is_postfix={self.is_postfix})"

class BinaryOperationNode(ExpressionNode):
    def __init__(self, operator: Token, left_node: ExpressionNode, right_node: ExpressionNode):
        self.operator = operator
        self.left_node = left_node
        self.right_node = right_node

    def __repr__(self):
        return f"BinaryOperationNode(operator={self.operator.value}, left={self.left_node}, right={self.right_node})"

class NullCoalesceNode(ExpressionNode):
    def __init__(self, left_node: ExpressionNode, right_node: ExpressionNode):
        self.left_node = left_node
        self.right_node = right_node

    def __repr__(self):
        return f"NullCoalesceNode(left={self.left_node}, right={self.right_node})"

class ElvisNode(ExpressionNode):
    def __init__(self, left_node: ExpressionNode, right_node: ExpressionNode):
        self.left_node = left_node
        self.right_node = right_node

    def __repr__(self):
        return f"ElvisNode(left={self.left_node}, right={self.right_node})"

class VariableNode(ExpressionNode):
    def __init__(self, variable: Token):
        self.variable = variable

    def __repr__(self):
        return f"VariableNode(name={self.variable.value})"

class AssignNode(ExpressionNode):
    def __init__(self, token: Token, target: ExpressionNode, expression: ExpressionNode):
        self.token = token
        self.target = target
        self.expression = expression

    def __repr__(self):
        return f"AssignNode(operator={self.token.value}, target={self.target}, value={self.expression})"

class VarDeclarationNode(StatementNode):
    def __init__(self, var_token: Token, variable: VariableNode, 
                 type_token: Optional[Callable[[bool], Token]], 
                 expr: Optional[ExpressionNode], 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.var_token = var_token
        self.variable = variable
        self.type_token = type_token
        self.expr = expr
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"VarDeclarationNode(name={self.variable}, type={self.type_token(True) if self.type_token else None}, expr={self.expr}, modifiers={self.modifiers}, annotations={self.annotations})"

class ValDeclarationNode(StatementNode):
    def __init__(self, val_token: Token, variable: VariableNode, 
                 type_token: Optional[Callable[[bool], Token]], 
                 expr: Optional[ExpressionNode], 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.val_token = val_token
        self.variable = variable
        self.type_token = type_token
        self.expr = expr
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"ValDeclarationNode(name={self.variable}, type={self.type_token(True) if self.type_token else None}, expr={self.expr}, modifiers={self.modifiers}, annotations={self.annotations})"

class ConstDeclarationNode(StatementNode):
    def __init__(self, const_token: Token, variable: VariableNode, 
                 type_token: Optional[Callable[[bool], Token]], 
                 expr: Optional[ExpressionNode], 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.const_token = const_token
        self.variable = variable
        self.type_token = type_token
        self.expr = expr
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"ConstDeclarationNode(name={self.variable}, type={self.type_token(True) if self.type_token else None}, expr={self.expr}, modifiers={self.modifiers}, annotations={self.annotations})"

class IfNode(StatementNode):
    def __init__(self, condition: ExpressionNode, then_branch: 'BlockNode', 
                 else_branch: Optional['BlockNode'], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

    def __repr__(self):
        return f"IfNode(condition={self.condition}, then={self.then_branch}, else={self.else_branch}, annotations={self.annotations})"

class WhenNode(StatementNode):
    def __init__(self, expression: Optional[ExpressionNode], 
                 cases: List['WhenCaseNode'], 
                 else_branch: Optional['BlockNode'], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.expression = expression
        self.cases = cases
        self.else_branch = else_branch

    def __repr__(self):
        return f"WhenNode(expr={self.expression}, cases={self.cases}, else={self.else_branch}, annotations={self.annotations})"

class WhenCaseNode(Node):
    def __init__(self, conditions: List[ExpressionNode], body: 'BlockNode'):
        self.conditions = conditions
        self.body = body

    def __repr__(self):
        return f"WhenCaseNode(conditions={self.conditions}, body={self.body})"

class WhileNode(StatementNode):
    def __init__(self, condition: ExpressionNode, body: 'BlockNode', 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.condition = condition
        self.body = body

    def __repr__(self):
        return f"WhileNode(condition={self.condition}, body={self.body}, annotations={self.annotations})"

class DoWhileNode(StatementNode):
    def __init__(self, body: 'BlockNode', condition: ExpressionNode, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.body = body
        self.condition = condition

    def __repr__(self):
        return f"DoWhileNode(body={self.body}, condition={self.condition}, annotations={self.annotations})"

class ForNode(StatementNode):
    def __init__(self, init: Optional[StatementNode], 
                 cond: Optional[ExpressionNode], 
                 step: Optional[ExpressionNode], 
                 body: 'BlockNode', 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.init = init
        self.cond = cond
        self.step = step
        self.body = body

    def __repr__(self):
        return f"ForNode(init={self.init}, cond={self.cond}, step={self.step}, body={self.body}, annotations={self.annotations})"

class SwitchNode(StatementNode):
    def __init__(self, expression: ExpressionNode, cases: List['CaseNode'], 
                 default: Optional['BlockNode'], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.expression = expression
        self.cases = cases
        self.default = default

    def __repr__(self):
        return f"SwitchNode(expr={self.expression}, cases={self.cases}, default={self.default}, annotations={self.annotations})"

class CaseNode(Node):
    def __init__(self, value: ExpressionNode, body: 'BlockNode'):
        self.value = value
        self.body = body

    def __repr__(self):
        return f"CaseNode(value={self.value}, body={self.body})"

class BreakNode(StatementNode):
    def __init__(self, annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)

    def __repr__(self):
        return f"BreakNode(annotations={self.annotations})"

class ContinueNode(StatementNode):
    def __init__(self, annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)

    def __repr__(self):
        return f"ContinueNode(annotations={self.annotations})"

class TryNode(StatementNode):
    def __init__(self, try_block: 'BlockNode', catches: List['CatchNode'], 
                 finally_block: Optional['BlockNode'], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.try_block = try_block
        self.catches = catches
        self.finally_block = finally_block

    def __repr__(self):
        return f"TryNode(try={self.try_block}, catches={self.catches}, finally={self.finally_block}, annotations={self.annotations})"

class CatchNode(Node):
    def __init__(self, exception_var: VariableNode, 
                 type_token: Optional[Callable[[bool], Token]], 
                 body: 'BlockNode'):
        self.exception_var = exception_var
        self.type_token = type_token
        self.body = body

    def __repr__(self):
        return f"CatchNode(var={self.exception_var}, type={self.type_token(True) if self.type_token else None}, body={self.body})"

class ThrowNode(StatementNode):
    def __init__(self, expression: ExpressionNode, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.expression = expression

    def __repr__(self):
        return f"ThrowNode(expr={self.expression}, annotations={self.annotations})"

class FunctionCallNode(ExpressionNode):
    def __init__(self, func: VariableNode, args: List[ExpressionNode]):
        self.func = func
        self.args = args

    def __repr__(self):
        return f"FunctionCallNode(func={self.func}, args={self.args})"

class FunctionDefNode(StatementNode):
    def __init__(self, name: Token, params: List['ParamNode'], 
                 return_type: Optional[Callable[[bool], Token]], 
                 body: 'BlockNode', 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.name = name
        self.params = params
        self.return_type = return_type
        self.body = body
        self.modifiers = modifiers or []

    def __repr__(self):
        return (f"FunctionDefNode(name={self.name.value}, params={self.params}, "
                f"return_type={self.return_type(True) if self.return_type else None}, "
                f"body={self.body}, modifiers={self.modifiers}, annotations={self.annotations})")

class ParamNode(Node):
    def __init__(self, name: Token, type_token: Optional[Callable[[bool], Token]], 
                 is_nullable: Optional[bool] = False):
        self.name = name
        self.type_token = type_token
        self.is_nullable = is_nullable

    def __repr__(self):
        return f"ParamNode(name={self.name.value}, type={self.type_token(True) if self.type_token else None}, nullable={self.is_nullable})"

class LambdaNode(ExpressionNode):
    def __init__(self, params: List[ParamNode], body: ExpressionNode):
        self.params = params
        self.body = body

    def __repr__(self):
        return f"LambdaNode(params={self.params}, body={self.body})"

class StructNode(StatementNode):
    def __init__(self, name: Token, members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.name = name
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"StructNode(name={self.name.value}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations})"

class DataClassNode(StatementNode):
    def __init__(self, name: Token, members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.name = name
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"DataClassNode(name={self.name.value}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations})"

class SealedClassNode(StatementNode):
    def __init__(self, name: Token, superclass: Optional[VariableNode], 
                 interfaces: List[VariableNode], members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.name = name
        self.superclass = superclass
        self.interfaces = interfaces
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"SealedClassNode(name={self.name.value}, superclass={self.superclass}, interfaces={self.interfaces}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations})"

class ObjectNode(StatementNode):
    def __init__(self, name: Token, members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.name = name
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"ObjectNode(name={self.name.value}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations})"

class CompanionObjectNode(StatementNode):
    def __init__(self, members: List[StatementNode], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.members = members

    def __repr__(self):
        return f"CompanionObjectNode(members={self.members}, annotations={self.annotations})"

class ClassNode(StatementNode):
    def __init__(self, name: Token, superclass: Optional[VariableNode], 
                 interfaces: List[VariableNode], members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.name = name
        self.superclass = superclass
        self.interfaces = interfaces
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"ClassNode(name={self.name.value}, superclass={self.superclass}, interfaces={self.interfaces}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations})"

class InterfaceNode(StatementNode):
    def __init__(self, name: Token, members: List[StatementNode], 
                 modifiers: List[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.name = name
        self.members = members
        self.modifiers = modifiers or []

    def __repr__(self):
        return f"InterfaceNode(name={self.name.value}, members={self.members}, modifiers={self.modifiers}, annotations={self.annotations})"

class EnumNode(StatementNode):
    def __init__(self, name: Token, values: List[Token], 
                 members: List[StatementNode], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.name = name
        self.values = values
        self.members = members

    def __repr__(self):
        return f"EnumNode(name={self.name.value}, values={self.values}, members={self.members}, annotations={self.annotations})"

class ReturnNode(StatementNode):
    def __init__(self, expression: Optional[ExpressionNode], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.expression = expression

    def __repr__(self):
        return f"ReturnNode(expr={self.expression}, annotations={self.annotations})"

class PrintNode(StatementNode):
    def __init__(self, expression: ExpressionNode, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.expression = expression

    def __repr__(self):
        return f"PrintNode(expr={self.expression}, annotations={self.annotations})"

class InputNode(StatementNode):
    def __init__(self, prompt: Optional[ExpressionNode], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.prompt = prompt

    def __repr__(self):
        return f"InputNode(prompt={self.prompt}, annotations={self.annotations})"

class ReadLineNode(StatementNode):
    def __init__(self, annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)

    def __repr__(self):
        return f"ReadLineNode(annotations={self.annotations})"

class InstanceOfNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode, type_token: Token):
        self.expression = expression
        self.type_token = type_token

    def __repr__(self):
        return f"InstanceOfNode(expr={self.expression}, type={self.type_token.value})"

class NewNode(ExpressionNode):
    def __init__(self, type_token: Token, args: List[ExpressionNode]):
        self.type_token = type_token
        self.args = args

    def __repr__(self):
        return f"NewNode(type={self.type_token.value}, args={self.args})"

class AllocNode(ExpressionNode):
    def __init__(self, type_token: Callable[[bool], Token]):
        self.type_token = type_token

    def __repr__(self):
        return f"AllocNode(type={self.type_token(True).value})"

class FreeNode(StatementNode):
    def __init__(self, expression: ExpressionNode, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.expression = expression

    def __repr__(self):
        return f"FreeNode(expr={self.expression}, annotations={self.annotations})"

class DerefNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode):
        self.expression = expression

    def __repr__(self):
        return f"DerefNode(expr={self.expression})"

class ReferenceNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode):
        self.expression = expression

    def __repr__(self):
        return f"ReferenceNode(expr={self.expression})"

class ThisNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"ThisNode(value={self.value})"

class SuperNode(ExpressionNode):
    def __init__(self, token: Token):
        self.token = token
        self.value = token.value

    def __repr__(self):
        return f"SuperNode(value={self.value})"

class MemberCallNode(ExpressionNode):
    def __init__(self, obj: ExpressionNode, method: VariableNode, 
                 args: List[ExpressionNode]):
        self.obj = obj
        self.method = method
        self.args = args

    def __repr__(self):
        return f"MemberCallNode(obj={self.obj}, method={self.method}, args={self.args})"

class ImportNode(StatementNode):
    def __init__(self, module: List[Token], names: List[Token] = None, 
                 alias: Optional[Token] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.module = module
        self.names = names or []
        self.alias = alias

    def __repr__(self):
        return f"ImportNode(module={self.module}, names={self.names}, alias={self.alias}, annotations={self.annotations})"

class PackageNode(StatementNode):
    def __init__(self, package: List[Token], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.package = package

    def __repr__(self):
        return f"PackageNode(package={self.package}, annotations={self.annotations})"

class NamespaceNode(StatementNode):
    def __init__(self, namespace: List[Token], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.namespace = namespace

    def __repr__(self):
        return f"NamespaceNode(namespace={self.namespace}, annotations={self.annotations})"

class ExportNode(StatementNode):
    def __init__(self, names: List[Token], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.names = names

    def __repr__(self):
        return f"ExportNode(names={self.names}, annotations={self.annotations})"

class YieldNode(ExpressionNode):
    def __init__(self, expression: Optional[ExpressionNode]):
        self.expression = expression

    def __repr__(self):
        return f"YieldNode(expr={self.expression})"

class AwaitNode(ExpressionNode):
    def __init__(self, expression: ExpressionNode):
        self.expression = expression

    def __repr__(self):
        return f"AwaitNode(expr={self.expression})"

class AnnotationNode(StatementNode):
    def __init__(self, name: Token, args: List[ExpressionNode], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.name = name
        self.args = args

    def __repr__(self):
        return f"AnnotationNode(name={self.name.value}, args={self.args}, annotations={self.annotations})"

class BlockNode(StatementNode):
    def __init__(self, statements: List[StatementNode], 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.statements = statements

    def __repr__(self):
        return f"BlockNode(statements={self.statements}, annotations={self.annotations})"

class ProgramNode(StatementNode):
    def __init__(self, imports: List[ImportNode], statements: List[StatementNode], 
                 package: Optional[PackageNode] = None, 
                 namespace: Optional[NamespaceNode] = None, 
                 exports: List[ExportNode] = None, 
                 annotations: List['AnnotationNode'] = None):
        super().__init__(annotations)
        self.imports = imports
        self.statements = statements
        self.package = package
        self.namespace = namespace
        self.exports = exports or []

    def __repr__(self):
        return f"ProgramNode(imports={self.imports}, statements={self.statements}, package={self.package}, namespace={self.namespace}, exports={self.exports}, annotations={self.annotations})"