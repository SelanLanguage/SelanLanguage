from abc import ABC
from typing import List, Optional, Tuple
from .token import Token, TokenType, token_types_list
from .ast import (
    ExpressionNode, ProgramNode, ImportNode, VariableNode, ClassNode,
    FunctionDefNode, ParameterNode, NewNode, MemberCallNode, PrintNode,
    VarDeclarationNode, ValDeclarationNode, ConstDeclarationNode,
    ReturnNode, BlockNode, BreakNode, ContinueNode, IfNode, WhileNode,
    DoWhileNode, ForNode, SwitchNode, CaseNode, TryNode, CatchNode,
    ThrowNode, InterfaceNode, EnumNode, NumberNode, StringNode,
    CharNode, BooleanNode, NullNode, ThisNode, SuperNode, AssignNode,
    BinaryOperationNode, UnaryOperationNode, NullCoalesceNode, ElvisNode,
    InputNode, LambdaNode, FunctionCallNode, InstanceOfNode
)
from .error import format_error
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

class Parser:
    """Parses a list of tokens into an AST for a strongly-typed language with Python-like imports."""
    
    def __init__(self, tokens: List[Token], source: str, filename: str, debug: bool = True):
        self.tokens = tokens
        self.source = source
        self.filename = filename
        self.pos = 0
        self.current_token: Optional[Token] = tokens[0] if tokens else None
        self.debug = debug
        if self.debug:
            logger.debug(f"Parser initialized with {len(tokens)} tokens")

    def advance(self) -> None:
        """Advances to the next token."""
        self.pos += 1
        self.current_token = self.tokens[self.pos] if self.pos < len(self.tokens) else None
        if self.debug:
            logger.debug(f"Advanced to token: {self.current_token.type.name if self.current_token else 'None'}")

    def match(self, *token_types: TokenType) -> bool:
        """Checks if the current token matches any of the given token types and advances if matched."""
        if self.current_token and self.current_token.type in token_types:
            if self.debug:
                logger.debug(f"Matched token: {self.current_token.type.name}")
            self.advance()
            return True
        return False

    def expect(self, token_type_name: str) -> Token:
        """Expects a token of the given type, advances, and returns it; raises SyntaxError if not found."""
        if not self.current_token:
            raise SyntaxError(format_error(
                "SyntaxError",
                f"Expected {token_type_name}, got EOF",
                self.filename,
                self.source,
                self.tokens[-1].line if self.tokens else 1,
                self.tokens[-1].column if self.tokens else 1,
                token_length=1
            ))
        if self.current_token.type.name == token_type_name:
            token = self.current_token
            self.advance()
            return token
        raise SyntaxError(format_error(
            "SyntaxError",
            f"Expected {token_type_name}, got {self.current_token.type.name}",
            self.filename,
            self.source,
            self.current_token.line,
            self.current_token.column,
            token_length=len(self.current_token.value) if self.current_token.value else 1
        ))

    def expect_one_of(self, token_type_names: Tuple[str, ...]) -> Token:
        """Expects one of the specified token types."""
        if not self.current_token:
            raise SyntaxError(format_error(
                "SyntaxError",
                f"Expected one of {token_type_names}, got EOF",
                self.filename,
                self.source,
                self.tokens[-1].line if self.tokens else 1,
                self.tokens[-1].column if self.tokens else 1,
                token_length=1
            ))
        if self.current_token.type.name in token_type_names:
            token = self.current_token
            self.advance()
            return token
        raise SyntaxError(format_error(
            "SyntaxError",
            f"Expected one of {token_type_names}, got {self.current_token.type.name}",
            self.filename,
            self.source,
            self.current_token.line,
            self.current_token.column,
            token_length=len(self.current_token.value) if self.current_token.value else 1
        ))

    def parse(self) -> ProgramNode:
        """Parses the entire program into a ProgramNode with imports and statements."""
        return self.parse_program()

    def parse_program(self) -> ProgramNode:
        """Parses imports and statements into a ProgramNode."""
        imports = []
        statements = []
        while self.current_token and self.current_token.type.name != 'EOF':
            try:
                if self.match(token_types_list['IMPORT']):
                    imports.append(self.parse_import())
                else:
                    statements.append(self.parse_statement())
            except SyntaxError as e:
                print(str(e))
                self.current_token = None
                break
        if self.debug:
            logger.debug(f"Parsed program: {len(imports)} imports, {len(statements)} statements")
        return ProgramNode(imports, statements)

    def parse_import(self) -> ImportNode:
        """Parses Python-style imports."""
        module = [self.expect('VARIABLE')]
        while self.current_token and self.current_token.type.name == 'DOT':
            self.advance()
            module.append(self.expect('VARIABLE'))
        names = []
        alias = None
        if self.current_token and self.current_token.type.name == 'FROM':
            self.advance()
            names = [self.expect('VARIABLE')]
            while self.current_token and self.current_token.type.name == 'COMMA':
                self.advance()
                names.append(self.expect('VARIABLE'))
            if self.current_token and self.current_token.type.name == 'AS':
                self.advance()
                alias = self.expect('VARIABLE')
        elif self.current_token and self.current_token.type.name == 'AS':
            self.advance()
            alias = self.expect('VARIABLE')
        self.expect('SEMICOLON')
        return ImportNode(module, names, alias)

    def parse_statement(self) -> ExpressionNode:
        """Parses a single statement."""
        if not self.current_token:
            raise SyntaxError(format_error(
                "SyntaxError",
                "Unexpected end of input",
                self.filename,
                self.source,
                self.tokens[-1].line if self.tokens else 1,
                self.tokens[-1].column if self.tokens else 1,
                token_length=1
            ))
        
        modifiers = []
        while self.current_token and self.current_token.type.name in ('PUBLIC', 'PRIVATE', 'PROTECTED', 'INTERNAL', 'STATIC'):
            modifiers.append(self.current_token)
            self.advance()

        if self.current_token.type.name in ('VAR', 'VAL', 'CONST'):
            return self.parse_declaration(modifiers)
        elif self.current_token.type.name == 'FUNCTION':
            return self.parse_function_def(modifiers)
        elif self.current_token.type.name == 'CLASS':
            return self.parse_class_def(modifiers)
        elif self.current_token.type.name == 'INTERFACE':
            return self.parse_interface_def(modifiers)
        elif self.current_token.type.name == 'ENUM':
            return self.parse_enum_def(modifiers)
        elif self.current_token.type.name == 'IF':
            return self.parse_if_statement()
        elif self.current_token.type.name == 'WHILE':
            return self.parse_while_statement()
        elif self.current_token.type.name == 'DO':
            return self.parse_do_while_statement()
        elif self.current_token.type.name == 'FOR':
            return self.parse_for_statement()
        elif self.current_token.type.name == 'SWITCH':
            return self.parse_switch_statement()
        elif self.current_token.type.name == 'TRY':
            return self.parse_try_statement()
        elif self.current_token.type.name == 'THROW':
            return self.parse_throw_statement()
        elif self.current_token.type.name == 'RETURN':
            return self.parse_return_statement()
        elif self.current_token.type.name == 'BREAK':
            self.advance()
            self.expect('SEMICOLON')
            return BreakNode()
        elif self.current_token.type.name == 'CONTINUE':
            self.advance()
            self.expect('SEMICOLON')
            return ContinueNode()
        elif self.current_token.type.name == 'PRINT':
            return self.parse_print_statement()
        elif self.current_token.type.name == 'INPUT':
            return self.parse_input_statement()
        elif self.current_token.type.name == 'LBRACE':
            return self.parse_block()
        elif modifiers:
            raise SyntaxError(format_error(
                "SyntaxError",
                f"Expected declaration after modifiers, got {self.current_token.type.name}",
                self.filename,
                self.source,
                self.current_token.line,
                self.current_token.column,
                token_length=len(self.current_token.value) if self.current_token.value else 1
            ))
        else:
            expr = self.parse_expression()
            self.expect('SEMICOLON')
            return expr

    def parse_input_statement(self) -> InputNode:
        """Parses an input statement like Python's input([prompt])."""
        input_token = self.expect('INPUT')
        self.expect('LPAREN')
        prompt = None
        if self.current_token and self.current_token.type.name != 'RPAREN':
            prompt = self.parse_expression()
        self.expect('RPAREN')
        self.expect('SEMICOLON')
        return InputNode(input_token, prompt)

    def parse_declaration(self, modifiers: List[Token] = None) -> ExpressionNode:
        """Parses variable declarations (var, val, const)."""
        if not modifiers:
            modifiers = []
        decl_type = self.current_token.type.name
        decl_token = self.expect(decl_type)
        variable = VariableNode(self.expect('VARIABLE'))
        type_token = None
        if self.current_token and self.current_token.type.name == 'COLON':
            self.advance()
            type_token = self.parse_type()
        expr = None
        if self.current_token and self.current_token.type.name == 'ASSIGN':
            self.advance()
            expr = self.parse_expression()
        self.expect('SEMICOLON')
        if decl_type == 'VAR':
            return VarDeclarationNode(decl_token, variable, type_token, expr, modifiers)
        elif decl_type == 'VAL':
            if not expr and 'CONST' not in [mod.value for mod in modifiers]:
                raise SyntaxError(format_error(
                    "SyntaxError",
                    "Val declaration requires an initializer unless const",
                    self.filename,
                    self.source,
                    decl_token.line,
                    decl_token.column,
                    token_length=len(decl_token.value)
                ))
            return ValDeclarationNode(decl_token, variable, type_token, expr, modifiers)
        else:  # CONST
            if not expr:
                raise SyntaxError(format_error(
                    "SyntaxError",
                    "Const declaration requires an initializer",
                    self.filename,
                    self.source,
                    decl_token.line,
                    decl_token.column,
                    token_length=len(decl_token.value)
                ))
            return ConstDeclarationNode(decl_token, variable, type_token, expr, modifiers)

    def parse_type(self) -> Token:
        """Parses a type annotation, returning a Token with is_nullable set appropriately."""
        if not self.current_token:
            raise SyntaxError(format_error(
                "SyntaxError",
                "Expected type, got EOF",
                self.filename,
                self.source,
                self.tokens[-1].line if self.tokens else 1,
                self.tokens[-1].column if self.tokens else 1,
                token_length=1
            ))
        type_token = self.current_token
        if type_token.type.name in ('INT', 'FLOAT', 'DOUBLE', 'BOOLEAN', 'STRING_TYPE', 'VOID', 'ANY'):
            self.advance()
            is_nullable = self.current_token and self.current_token.type.name == 'NULLABLE'
            if is_nullable:
                self.advance()
            return Token(type_token.type, type_token.value, type_token.position, type_token.line, type_token.column, is_nullable)
        elif type_token.type.name == 'VARIABLE' and type_token.value == 'str':
            type_token = Token(token_types_list['STRING_TYPE'], 'string', type_token.position, type_token.line, type_token.column)
            self.advance()
            is_nullable = self.current_token and self.current_token.type.name == 'NULLABLE'
            if is_nullable:
                self.advance()
            return Token(type_token.type, type_token.value, type_token.position, type_token.line, type_token.column, is_nullable)
        elif type_token.type.name == 'VARIABLE':
            self.advance()
            is_nullable = self.current_token and self.current_token.type.name == 'NULLABLE'
            if is_nullable:
                self.advance()
            return Token(type_token.type, type_token.value, type_token.position, type_token.line, type_token.column, is_nullable)
        raise SyntaxError(format_error(
            "SyntaxError",
            f"Expected type, got {type_token.type.name}",
            self.filename,
            self.source,
            type_token.line,
            type_token.column,
            token_length=len(type_token.value) if type_token.value else 1
        ))

    def parse_if_statement(self) -> IfNode:
        """Parses an if statement."""
        self.expect('IF')
        self.expect('LPAREN')
        condition = self.parse_expression()
        self.expect('RPAREN')
        then_branch = self.parse_block()
        else_branch = None
        if self.current_token and self.current_token.type.name == 'ELSE':
            self.advance()
            else_branch = self.parse_block()
        return IfNode(condition, then_branch, else_branch)

    def parse_while_statement(self) -> WhileNode:
        """Parses a while statement."""
        self.expect('WHILE')
        self.expect('LPAREN')
        condition = self.parse_expression()
        self.expect('RPAREN')
        body = self.parse_block()
        return WhileNode(condition, body)

    def parse_do_while_statement(self) -> DoWhileNode:
        """Parses a do-while statement."""
        self.expect('DO')
        body = self.parse_block()
        self.expect('WHILE')
        self.expect('LPAREN')
        condition = self.parse_expression()
        self.expect('RPAREN')
        self.expect('SEMICOLON')
        return DoWhileNode(body, condition)

    def parse_for_statement(self) -> ForNode:
        """Parses a for statement."""
        self.expect('FOR')
        self.expect('LPAREN')
        init = self.parse_statement() if self.current_token and self.current_token.type.name != 'SEMICOLON' else None
        if self.current_token and self.current_token.type.name == 'SEMICOLON':
            self.advance()
        cond = self.parse_expression() if self.current_token and self.current_token.type.name != 'SEMICOLON' else None
        self.expect('SEMICOLON')
        step = self.parse_expression() if self.current_token and self.current_token.type.name != 'RPAREN' else None
        self.expect('RPAREN')
        body = self.parse_block()
        return ForNode(init, cond, step, body)

    def parse_switch_statement(self) -> SwitchNode:
        """Parses a switch statement."""
        self.expect('SWITCH')
        self.expect('LPAREN')
        expression = self.parse_expression()
        self.expect('RPAREN')
        self.expect('LBRACE')
        cases = []
        default = None
        while self.current_token and self.current_token.type.name != 'RBRACE':
            if self.current_token.type.name == 'CASE':
                self.advance()
                value = self.parse_expression()
                self.expect('COLON')
                body = self.parse_block()
                cases.append(CaseNode(value, body))
            elif self.current_token.type.name == 'DEFAULT':
                self.advance()
                self.expect('COLON')
                default = self.parse_block()
            else:
                raise SyntaxError(format_error(
                    "SyntaxError",
                    f"Expected 'case' or 'default', got {self.current_token.type.name}",
                    self.filename,
                    self.source,
                    self.current_token.line,
                    self.current_token.column,
                    token_length=len(self.current_token.value) if self.current_token.value else 1
                ))
        self.expect('RBRACE')
        return SwitchNode(expression, cases, default)

    def parse_try_statement(self) -> TryNode:
        """Parses a try-catch-finally statement."""
        self.expect('TRY')
        try_block = self.parse_block()
        catches = []
        while self.current_token and self.current_token.type.name == 'CATCH':
            self.advance()
            self.expect('LPAREN')
            exception_var = VariableNode(self.expect('VARIABLE'))
            type_token = None
            if self.current_token and self.current_token.type.name == 'COLON':
                self.advance()
                type_token = self.parse_type()
            self.expect('RPAREN')
            body = self.parse_block()
            catches.append(CatchNode(exception_var, type_token, body))
        finally_block = None
        if self.current_token and self.current_token.type.name == 'FINALLY':
            self.advance()
            finally_block = self.parse_block()
        return TryNode(try_block, catches, finally_block)

    def parse_throw_statement(self) -> ThrowNode:
        """Parses a throw statement."""
        self.expect('THROW')
        expr = self.parse_expression()
        self.expect('SEMICOLON')
        return ThrowNode(expr)

    def parse_function_def(self, modifiers: List[Token] = None) -> FunctionDefNode:
        """Parses a function definition."""
        if not modifiers:
            modifiers = []
        self.expect('FUNCTION')
        name = self.expect('VARIABLE')
        self.expect('LPAREN')
        params = []
        if self.current_token and self.current_token.type.name != 'RPAREN':
            params.append(self.parse_param())
            while self.current_token and self.current_token.type.name == 'COMMA':
                self.advance()
                params.append(self.parse_param())
        self.expect('RPAREN')
        return_type = None
        if self.current_token and self.current_token.type.name == 'COLON':
            self.advance()
            return_type = self.parse_type()
        body = self.parse_block()
        return FunctionDefNode(name, params, return_type, body, modifiers)

    def parse_param(self) -> ParameterNode:
        """Parses a function parameter."""
        name = self.expect('VARIABLE')
        type_token = None
        is_nullable = False
        if self.current_token and self.current_token.type.name == 'COLON':
            self.advance()
            type_token = self.parse_type()
            is_nullable = type_token.is_nullable
        return ParameterNode(name, type_token, is_nullable)

    def parse_class_def(self, modifiers: List[Token] = None) -> ClassNode:
        """Parses a class definition."""
        if not modifiers:
            modifiers = []
        self.expect('CLASS')
        name = self.expect('VARIABLE')
        superclass = None
        interfaces = []
        if self.current_token and self.current_token.type.name == 'COLON':
            self.advance()
            superclass = VariableNode(self.expect('VARIABLE'))
            while self.current_token and self.current_token.type.name == 'COMMA':
                self.advance()
                interfaces.append(VariableNode(self.expect('VARIABLE')))
        self.expect('LBRACE')
        members = []
        while self.current_token and self.current_token.type.name != 'RBRACE':
            member_modifiers = []
            while self.current_token and self.current_token.type.name in ('PUBLIC', 'PRIVATE', 'PROTECTED', 'INTERNAL', 'STATIC'):
                member_modifiers.append(self.current_token)
                self.advance()
            if self.current_token.type.name in ('VAR', 'VAL', 'CONST'):
                members.append(self.parse_declaration(member_modifiers))
            elif self.current_token.type.name == 'FUNCTION':
                members.append(self.parse_function_def(member_modifiers))
            else:
                raise SyntaxError(format_error(
                    "SyntaxError",
                    f"Expected member declaration, got {self.current_token.type.name}",
                    self.filename,
                    self.source,
                    self.current_token.line,
                    self.current_token.column,
                    token_length=len(self.current_token.value) if self.current_token.value else 1
                ))
        self.expect('RBRACE')
        return ClassNode(name, superclass, interfaces, members, modifiers)

    def parse_interface_def(self, modifiers: List[Token] = None) -> InterfaceNode:
        """Parses an interface definition."""
        if not modifiers:
            modifiers = []
        self.expect('INTERFACE')
        name = self.expect('VARIABLE')
        self.expect('LBRACE')
        members = []
        while self.current_token and self.current_token.type.name != 'RBRACE':
            member_modifiers = []
            while self.current_token and self.current_token.type.name in ('PUBLIC', 'PRIVATE', 'PROTECTED', 'INTERNAL', 'STATIC'):
                member_modifiers.append(self.current_token)
                self.advance()
            if self.current_token.type.name == 'FUNCTION':
                members.append(self.parse_function_def(member_modifiers))
            else:
                raise SyntaxError(format_error(
                    "SyntaxError",
                    f"Expected function declaration in interface, got {self.current_token.type.name}",
                    self.filename,
                    self.source,
                    self.current_token.line,
                    self.current_token.column,
                    token_length=len(self.current_token.value) if self.current_token.value else 1
                ))
        self.expect('RBRACE')
        return InterfaceNode(name, members, modifiers)

    def parse_enum_def(self, modifiers: List[Token] = None) -> EnumNode:
        """Parses an enum definition."""
        if not modifiers:
            modifiers = []
        self.expect('ENUM')
        name = self.expect('VARIABLE')
        self.expect('LBRACE')
        values = []
        members = []
        while self.current_token and self.current_token.type.name != 'RBRACE':
            if self.current_token.type.name == 'VARIABLE':
                values.append(self.current_token)
                self.advance()
                if self.current_token and self.current_token.type.name == 'COMMA':
                    self.advance()
                elif self.current_token and self.current_token.type.name != 'RBRACE':
                    raise SyntaxError(format_error(
                        "SyntaxError",
                        "Expected comma or '}' after enum value",
                        self.filename,
                        self.source,
                        self.current_token.line,
                        self.current_token.column,
                        token_length=len(self.current_token.value) if self.current_token.value else 1
                    ))
            else:
                members.append(self.parse_statement())
        self.expect('RBRACE')
        return EnumNode(name, values, members)

    def parse_return_statement(self) -> ReturnNode:
        """Parses a return statement."""
        self.expect('RETURN')
        expr = self.parse_expression() if self.current_token and self.current_token.type.name != 'SEMICOLON' else None
        self.expect('SEMICOLON')
        return ReturnNode(expr)

    def parse_print_statement(self) -> PrintNode:
        """Parses a print statement."""
        print_token = self.expect('PRINT')
        self.expect('LPAREN')
        expr = self.parse_expression()
        self.expect('RPAREN')
        self.expect('SEMICOLON')
        return PrintNode(expr)

    def parse_block(self) -> BlockNode:
        """Parses a block of statements."""
        self.expect('LBRACE')
        statements = []
        while self.current_token and self.current_token.type.name != 'RBRACE':
            statements.append(self.parse_statement())
        self.expect('RBRACE')
        return BlockNode(statements)

    def parse_expression(self) -> ExpressionNode:
        """Parses an expression."""
        return self.parse_null_coalesce()

    def parse_null_coalesce(self) -> ExpressionNode:
        """Parses a null-coalescing expression (??)."""
        expr = self.parse_elvis()
        while self.current_token and self.current_token.type.name == 'NULL_COALESCE':
            self.advance()
            right = self.parse_elvis()
            expr = NullCoalesceNode(expr, right)
        return expr

    def parse_elvis(self) -> ExpressionNode:
        """Parses an Elvis expression (?:)."""
        expr = self.parse_assignment()
        while self.current_token and self.current_token.type.name == 'ELVIS':
            self.advance()
            right = self.parse_assignment()
            expr = ElvisNode(expr, right)
        return expr

    def parse_assignment(self) -> ExpressionNode:
        """Parses an assignment expression."""
        expr = self.parse_equality()
        if self.current_token and self.current_token.type.name == 'ASSIGN':
            token = self.current_token
            self.advance()
            if not isinstance(expr, (VariableNode, MemberCallNode)):
                raise SyntaxError(format_error(
                    "SyntaxError",
                    "Invalid assignment target",
                    self.filename,
                    self.source,
                    token.line,
                    token.column,
                    token_length=len(token.value)
                ))
            value = self.parse_expression()
            return AssignNode(token, expr, value)
        return expr

    def parse_equality(self) -> ExpressionNode:
        """Parses an equality expression (==, !=)."""
        expr = self.parse_comparison()
        while self.current_token and self.current_token.type.name in ('EQUAL', 'NOT_EQUAL'):
            token = self.current_token
            self.advance()
            right = self.parse_comparison()
            expr = BinaryOperationNode(token, expr, right)
        return expr

    def parse_comparison(self) -> ExpressionNode:
        """Parses a comparison expression (<, >, <=, >=)."""
        expr = self.parse_term()
        while self.current_token and self.current_token.type.name in ('LESS', 'GREATER', 'LESS_EQUAL', 'GREATER_EQUAL'):
            token = self.current_token
            self.advance()
            right = self.parse_term()
            expr = BinaryOperationNode(token, expr, right)
        return expr

    def parse_term(self) -> ExpressionNode:
        """Parses a term expression (+, -)."""
        expr = self.parse_factor()
        while self.current_token and self.current_token.type.name in ('PLUS', 'MINUS'):
            token = self.current_token
            self.advance()
            right = self.parse_factor()
            expr = BinaryOperationNode(token, expr, right)
        return expr

    def parse_factor(self) -> ExpressionNode:
        """Parses a factor expression (*, /, %)."""
        expr = self.parse_unary()
        while self.current_token and self.current_token.type.name in ('MULTIPLY', 'DIVIDE', 'MODULO'):
            token = self.current_token
            self.advance()
            right = self.parse_unary()
            expr = BinaryOperationNode(token, expr, right)
        return expr

    def parse_unary(self) -> ExpressionNode:
        """Parses a unary expression (-, !, ~)."""
        if self.current_token and self.current_token.type.name in ('MINUS', 'NOT', 'BIT_NOT'):
            token = self.current_token
            self.advance()
            operand = self.parse_unary()
            return UnaryOperationNode(token, operand)
        return self.parse_postfix()

    def parse_postfix(self) -> ExpressionNode:
        """Parses postfix expressions (++, --, .method())."""
        expr = self.parse_instanceof()
        while self.current_token and self.current_token.type.name in ('INCREMENT', 'DECREMENT', 'DOT'):
            if self.current_token.type.name in ('INCREMENT', 'DECREMENT'):
                token = self.current_token
                self.advance()
                expr = UnaryOperationNode(token, expr, is_postfix=True)
            elif self.current_token.type.name == 'DOT':
                self.advance()
                method = VariableNode(self.expect('VARIABLE'))
                if self.current_token and self.current_token.type.name == 'LPAREN':
                    self.expect('LPAREN')
                    args = []
                    if self.current_token and self.current_token.type.name != 'RPAREN':
                        args.append(self.parse_expression())
                        while self.current_token and self.current_token.type.name == 'COMMA':
                            self.advance()
                            args.append(self.parse_expression())
                    self.expect('RPAREN')
                    expr = MemberCallNode(expr, method, args)
                else:
                    raise SyntaxError(format_error(
                        "SyntaxError",
                        "Member access without method call not supported",
                        self.filename,
                        self.source,
                        method.variable.line,
                        method.variable.column,
                        token_length=len(method.variable.value)
                    ))
        return expr

    def parse_instanceof(self) -> ExpressionNode:
        """Parses an instanceof expression."""
        expr = self.parse_new()
        if self.current_token and self.current_token.type.name == 'INSTANCEOF':
            self.advance()
            type_token = self.expect_one_of(('INT', 'FLOAT', 'DOUBLE', 'BOOLEAN', 'STRING_TYPE', 'ANY', 'VARIABLE'))
            return InstanceOfNode(expr, type_token)
        return expr

    def parse_new(self) -> ExpressionNode:
        """Parses a new expression (object creation)."""
        if self.current_token and self.current_token.type.name == 'NEW':
            self.advance()
            type_token = self.expect('VARIABLE')
            self.expect('LPAREN')
            args = []
            if self.current_token and self.current_token.type.name != 'RPAREN':
                args.append(self.parse_expression())
                while self.current_token and self.current_token.type.name == 'COMMA':
                    self.advance()
                    args.append(self.parse_expression())
            self.expect('RPAREN')
            return NewNode(type_token, args)
        return self.parse_lambda()

    def parse_lambda(self) -> ExpressionNode:
        """Parses a lambda expression."""
        if self.current_token and self.current_token.type.name == 'LAMBDA':
            self.advance()
            self.expect('LPAREN')
            params = []
            if self.current_token and self.current_token.type.name != 'RPAREN':
                params.append(self.parse_param())
                while self.current_token and self.current_token.type.name == 'COMMA':
                    self.advance()
                    params.append(self.parse_param())
            self.expect('RPAREN')
            self.expect('ARROW')
            body = self.parse_expression()
            return LambdaNode(params, body)
        return self.parse_primary()

    def parse_primary(self) -> ExpressionNode:
        if not self.current_token:
            raise SyntaxError(format_error(
                "SyntaxError",
                "Expected expression, got EOF",
                self.filename,
                self.source,
                self.tokens[-1].line if self.tokens else 1,
                self.tokens[-1].column if self.tokens else 1,
                token_length=1
            ))

        token = self.current_token
        if self.debug:
            logger.debug(f"Parsing primary token: type={token.type.name}, value={token.value!r}, line={token.line}, column={token.column}")
        self.advance()

        if token.type.name == 'NUMBER':
            return NumberNode(token)
        elif token.type.name == 'STRING':
            if not token.value.startswith('"') or not token.value.endswith('"'):
                raise SyntaxError(format_error(
                    "SyntaxError",
                    f"Invalid string literal: {token.value}",
                    self.filename,
                    self.source,
                    token.line,
                    token.column,
                    token_length=len(token.value) if token.value else 1
                ))
            return StringNode(token)
        elif token.type.name == 'CHAR':
            return CharNode(token)
        elif token.type.name in ('TRUE', 'FALSE'):
            return BooleanNode(token)
        elif token.type.name == 'NULL':
            return NullNode(token)
        elif token.type.name == 'THIS':
            return ThisNode(token)
        elif token.type.name == 'SUPER':
            return SuperNode(token)
        elif token.type.name == 'INPUT':
            self.expect('LPAREN')
            prompt = None
            if self.current_token and self.current_token.type.name != 'RPAREN':
                prompt = self.parse_expression()
            self.expect('RPAREN')
            return InputNode(token, prompt)
        elif token.type.name == 'VARIABLE':
            if not token.value.strip():
                raise SyntaxError(format_error(
                    "SyntaxError",
                    "Empty variable identifier",
                    self.filename,
                    self.source,
                    token.line,
                    token.column,
                    token_length=1
                ))
            keyword_values = {
                'print', 'if', 'else', 'while', 'for', 'do', 'switch', 'case', 'default', 'return', 'fun',
                'class', 'interface', 'enum', 'var', 'val', 'const', 'break', 'continue', 'try', 'catch',
                'finally', 'throw', 'true', 'false', 'null', 'public', 'private', 'protected', 'internal',
                'static', 'new', 'this', 'super', 'instanceof', 'lambda', 'import', 'from', 'as', 'input'
            }
            if token.value.lower() in keyword_values:
                raise SyntaxError(format_error(
                    "SyntaxError",
                    f"Unexpected keyword '{token.value}' used as variable",
                    self.filename,
                    self.source,
                    token.line,
                    token.column,
                    token_length=len(token.value)
                ))
            if self.current_token and self.current_token.type.name == 'LPAREN':
                return self.parse_function_call(token)
            return VariableNode(token)
        elif token.type.name == 'LPAREN':
            expr = self.parse_expression()
            self.expect('RPAREN')
            return expr
        raise SyntaxError(format_error(
            "SyntaxError",
            f"Unexpected token {token.type.name}",
            self.filename,
            self.source,
            token.line,
            token.column,
            token_length=len(token.value) if token.value else 1
        ))

    def parse_function_call(self, func_token: Token) -> FunctionCallNode:
        """Parses a function call."""
        self.expect('LPAREN')
        args = []
        if self.current_token and self.current_token.type.name != 'RPAREN':
            args.append(self.parse_expression())
            while self.current_token and self.current_token.type.name == 'COMMA':
                self.advance()
                args.append(self.parse_expression())
        self.expect('RPAREN')
        return FunctionCallNode(VariableNode(func_token), args)