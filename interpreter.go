package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// TokenType represents the type of a token
type TokenType string

const (
	TOKEN_PRINT      TokenType = "PRINT"
	TOKEN_VAR        TokenType = "VAR"
	TOKEN_INPUT      TokenType = "INPUT"
	TOKEN_FOR        TokenType = "FOR"
	TOKEN_IF         TokenType = "IF"
	TOKEN_ELSE       TokenType = "ELSE"
	TOKEN_STRING     TokenType = "STRING"
	TOKEN_INT        TokenType = "INT"
	TOKEN_IDENT      TokenType = "IDENT"
	TOKEN_NUMBER     TokenType = "NUMBER"
	TOKEN_PLUS       TokenType = "PLUS"
	TOKEN_MINUS      TokenType = "MINUS"
	TOKEN_MOD        TokenType = "MOD"
	TOKEN_EQ         TokenType = "EQ"
	TOKEN_SEMI       TokenType = "SEMI"
	TOKEN_COLON      TokenType = "COLON"
	TOKEN_LPAREN     TokenType = "LPAREN"
	TOKEN_RPAREN     TokenType = "RPAREN"
	TOKEN_LBRACE     TokenType = "LBRACE"
	TOKEN_RBRACE     TokenType = "RBRACE"
	TOKEN_LT         TokenType = "LT"
	TOKEN_GT         TokenType = "GT"
	TOKEN_STRING_LIT TokenType = "STRING_LIT"
	TOKEN_EOF        TokenType = "EOF"
)

// Token represents a single token
type Token struct {
	Type    TokenType
	Literal string
	Line    int
}

// Lexer holds the state of the lexical analyzer
type Lexer struct {
	input   string
	pos     int
	line    int
	tokens  []Token
}

// NewLexer creates a new lexer
func NewLexer(input string) *Lexer {
	return &Lexer{input: input, pos: 0, line: 1}
}

// NextToken returns the next token from the input
func (l *Lexer) NextToken() Token {
	l.skipWhitespace()
	if l.pos >= len(l.input) {
		return Token{Type: TOKEN_EOF, Line: l.line}
	}

	ch := l.input[l.pos]
	switch ch {
	case '"':
		return l.readString()
	case ':':
		l.pos++
		return Token{Type: TOKEN_COLON, Literal: ":", Line: l.line}
	case ';':
		l.pos++
		return Token{Type: TOKEN_SEMI, Literal: ";", Line: l.line}
	case '(':
		l.pos++
		return Token{Type: TOKEN_LPAREN, Literal: "(", Line: l.line}
	case ')':
		l.pos++
		return Token{Type: TOKEN_RPAREN, Literal: ")", Line: l.line}
	case '{':
		l.pos++
		return Token{Type: TOKEN_LBRACE, Literal: "{", Line: l.line}
	case '}':
		l.pos++
		return Token{Type: TOKEN_RBRACE, Literal: "}", Line: l.line}
	case '+':
		l.pos++
		return Token{Type: TOKEN_PLUS, Literal: "+", Line: l.line}
	case '-':
		l.pos++
		return Token{Type: TOKEN_MINUS, Literal: "-", Line: l.line}
	case '%':
		l.pos++
		return Token{Type: TOKEN_MOD, Literal: "%", Line: l.line}
	case '<':
		l.pos++
		return Token{Type: TOKEN_LT, Literal: "<", Line: l.line}
	case '>':
		l.pos++
		return Token{Type: TOKEN_GT, Literal: ">", Line: l.line}
	case '=':
		if l.pos+1 < len(l.input) && l.input[l.pos+1] == '=' {
			l.pos += 2
			return Token{Type: TOKEN_EQ, Literal: "==", Line: l.line}
		}
	}

	if isLetter(ch) {
		return l.readIdentifierOrKeyword()
	}
	if isDigit(ch) {
		return l.readNumber()
	}

	return Token{Type: TOKEN_EOF, Line: l.line}
}

// readString reads a string literal
func (l *Lexer) readString() Token {
	start := l.pos
	l.pos++
	for l.pos < len(l.input) && l.input[l.pos] != '"' {
		l.pos++
	}
	if l.pos < len(l.input) {
		l.pos++ // Skip closing quote
	}
	return Token{Type: TOKEN_STRING_LIT, Literal: l.input[start+1 : l.pos-1], Line: l.line}
}

// readIdentifierOrKeyword reads an identifier or keyword
func (l *Lexer) readIdentifierOrKeyword() Token {
	start := l.pos
	for l.pos < len(l.input) && (isLetter(l.input[l.pos]) || isDigit(l.input[l.pos])) {
		l.pos++
	}
	literal := l.input[start:l.pos]
	switch literal {
	case "print":
		return Token{Type: TOKEN_PRINT, Literal: literal, Line: l.line}
	case "var":
		return Token{Type: TOKEN_VAR, Literal: literal, Line: l.line}
	case "input":
		return Token{Type: TOKEN_INPUT, Literal: literal, Line: l.line}
	case "for":
		return Token{Type: TOKEN_FOR, Literal: literal, Line: l.line}
	case "if":
		return Token{Type: TOKEN_IF, Literal: literal, Line: l.line}
	case "else":
		return Token{Type: TOKEN_ELSE, Literal: literal, Line: l.line}
	case "string":
		return Token{Type: TOKEN_STRING, Literal: literal, Line: l.line}
	case "int":
		return Token{Type: TOKEN_INT, Literal: literal, Line: l.line}
	default:
		return Token{Type: TOKEN_IDENT, Literal: literal, Line: l.line}
	}
}

// readNumber reads a number
func (l *Lexer) readNumber() Token {
	start := l.pos
	for l.pos < len(l.input) && isDigit(l.input[l.pos]) {
		l.pos++
	}
	return Token{Type: TOKEN_NUMBER, Literal: l.input[start:l.pos], Line: l.line}
}

// skipWhitespace skips whitespace and comments
func (l *Lexer) skipWhitespace() {
	for l.pos < len(l.input) {
		if l.input[l.pos] == ' ' || l.input[l.pos] == '\t' {
			l.pos++
		} else if l.input[l.pos] == '\n' {
			l.pos++
			l.line++
		} else if l.pos < len(l.input)-1 && l.input[l.pos] == '#' {
			for l.pos < len(l.input) && l.input[l.pos] != '\n' {
				l.pos++
			}
		} else {
			break
		}
	}
}

func isLetter(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}

// AST Node Types
type Node interface {
	node()
}

type Program struct {
	Statements []Statement
}

func (p *Program) node() {}

type Statement interface {
	Node
	statement()
}

type PrintStatement struct {
	Expression Expression
}

func (p *PrintStatement) node()      {}
func (p *PrintStatement) statement() {}

type VarStatement struct {
	Name  string
	Type  string
	Value Expression
}

func (v *VarStatement) node()      {}
func (v *VarStatement) statement() {}

type ForStatement struct {
	Init      Statement
	Condition Expression
	Increment Expression
	Body      []Statement
}

func (f *ForStatement) node()      {}
func (f *ForStatement) statement() {}

type IfStatement struct {
	Condition   Expression
	ThenBranch  []Statement
	ElseBranch  []Statement
}

func (i *IfStatement) node()      {}
func (i *IfStatement) statement() {}

type Expression interface {
	Node
	expression()
}

type BinaryExpression struct {
	Left     Expression
	Operator string
	Right    Expression
}

func (b *BinaryExpression) node()      {}
func (b *BinaryExpression) expression() {}

type LiteralExpression struct {
	Value string
}

func (l *LiteralExpression) node()      {}
func (l *LiteralExpression) expression() {}

type VariableExpression struct {
	Name string
}

func (v *VariableExpression) node()      {}
func (v *VariableExpression) expression() {}

type InputExpression struct {
	Prompt string
}

func (i *InputExpression) node()      {}
func (i *InputExpression) expression() {}

// Parser holds the state of the parser
type Parser struct {
	lexer     *Lexer
	tokens    []Token
	pos       int
}

// NewParser creates a new parser
func NewParser(lexer *Lexer) *Parser {
	p := &Parser{lexer: lexer}
	p.tokens = make([]Token, 0)
	for {
		token := p.lexer.NextToken()
		p.tokens = append(p.tokens, token)
		if token.Type == TOKEN_EOF {
			break
		}
	}
	return p
}

// ParseProgram parses the entire program
func (p *Parser) ParseProgram() *Program {
	program := &Program{Statements: []Statement{}}
	for p.pos < len(p.tokens) && p.tokens[p.pos].Type != TOKEN_EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
	}
	return program
}

// parseStatement parses a single statement
func (p *Parser) parseStatement() Statement {
	token := p.tokens[p.pos]
	switch token.Type {
	case TOKEN_PRINT:
		return p.parsePrintStatement()
	case TOKEN_VAR:
		return p.parseVarStatement()
	case TOKEN_FOR:
		return p.parseForStatement()
	case TOKEN_IF:
		return p.parseIfStatement()
	default:
		p.pos++
		return nil
	}
}

// parsePrintStatement parses a print statement
func (p *Parser) parsePrintStatement() *PrintStatement {
	p.pos++ // Skip 'print'
	p.expect(TOKEN_LPAREN)
	expr := p.parseExpression()
	p.expect(TOKEN_RPAREN)
	p.expect(TOKEN_SEMI)
	return &PrintStatement{Expression: expr}
}

// parseVarStatement parses a variable declaration
func (p *Parser) parseVarStatement() *VarStatement {
	p.pos++ // Skip 'var'
	name := p.expect(TOKEN_IDENT).Literal
	p.expect(TOKEN_COLON)
	typ := p.expect(TOKEN_STRING, TOKEN_INT).Literal
	var value Expression
	if p.tokens[p.pos].Type == TOKEN_EQ {
		p.pos++ // Skip '='
		value = p.parseExpression()
	}
	p.expect(TOKEN_SEMI)
	return &VarStatement{Name: name, Type: typ, Value: value}
}

// parseForStatement parses a for loop
func (p *Parser) parseForStatement() *ForStatement {
	p.pos++ // Skip 'for'
	p.expect(TOKEN_LPAREN)
	init := p.parseStatement()
	condition := p.parseExpression()
	p.expect(TOKEN_SEMI)
	increment := p.parseExpression()
	p.expect(TOKEN_RPAREN)
	p.expect(TOKEN_LBRACE)
	body := []Statement{}
	for p.tokens[p.pos].Type != TOKEN_RBRACE && p.tokens[p.pos].Type != TOKEN_EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			body = append(body, stmt)
		}
	}
	p.expect(TOKEN_RBRACE)
	return &ForStatement{Init: init, Condition: condition, Increment: increment, Body: body}
}

// parseIfStatement parses an if statement
func (p *Parser) parseIfStatement() *IfStatement {
	p.pos++ // Skip 'if'
	p.expect(TOKEN_LPAREN)
	condition := p.parseExpression()
	p.expect(TOKEN_RPAREN)
	p.expect(TOKEN_LBRACE)
	thenBranch := []Statement{}
	for p.tokens[p.pos].Type != TOKEN_RBRACE && p.tokens[p.pos].Type != TOKEN_EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			thenBranch = append(thenBranch, stmt)
		}
	}
	p.expect(TOKEN_RBRACE)
	var elseBranch []Statement
	if p.pos < len(p.tokens) && p.tokens[p.pos].Type == TOKEN_ELSE {
		p.pos++ // Skip 'else'
		p.expect(TOKEN_LBRACE)
		elseBranch = []Statement{}
		for p.tokens[p.pos].Type != TOKEN_RBRACE && p.tokens[p.pos].Type != TOKEN_EOF {
			stmt := p.parseStatement()
			if stmt != nil {
				elseBranch = append(elseBranch, stmt)
			}
		}
		p.expect(TOKEN_RBRACE)
	}
	return &IfStatement{Condition: condition, ThenBranch: thenBranch, ElseBranch: elseBranch}
}

// parseExpression parses an expression
func (p *Parser) parseExpression() Expression {
	expr := p.parsePrimary()
	for p.pos < len(p.tokens) && (p.tokens[p.pos].Type == TOKEN_PLUS || p.tokens[p.pos].Type == TOKEN_EQ ||
		p.tokens[p.pos].Type == TOKEN_LT || p.tokens[p.pos].Type == TOKEN_GT || p.tokens[p.pos].Type == TOKEN_MOD) {
		op := p.tokens[p.pos].Literal
		p.pos++
		right := p.parsePrimary()
		expr = &BinaryExpression{Left: expr, Operator: op, Right: right}
	}
	return expr
}

// parsePrimary parses primary expressions
func (p *Parser) parsePrimary() Expression {
	token := p.tokens[p.pos]
	switch token.Type {
	case TOKEN_STRING_LIT:
		p.pos++
		return &LiteralExpression{Value: token.Literal}
	case TOKEN_NUMBER:
		p.pos++
		return &LiteralExpression{Value: token.Literal}
	case TOKEN_IDENT:
		p.pos++
		return &VariableExpression{Name: token.Literal}
	case TOKEN_INPUT:
		p.pos++
		p.expect(TOKEN_LPAREN)
		prompt := p.expect(TOKEN_STRING_LIT).Literal
		p.expect(TOKEN_RPAREN)
		return &InputExpression{Prompt: prompt}
	case TOKEN_MINUS:
		p.pos++
		expr := p.parsePrimary()
		return &BinaryExpression{Left: &LiteralExpression{Value: "0"}, Operator: "-", Right: expr}
	default:
		p.pos++
		return nil
	}
}

// expect checks if the current token matches the expected type
func (p *Parser) expect(types ...TokenType) Token {
	if p.pos >= len(p.tokens) {
		return Token{Type: TOKEN_EOF}
	}
	token := p.tokens[p.pos]
	for _, t := range types {
		if token.Type == t {
			p.pos++
			return token
		}
	}
	return Token{Type: TOKEN_EOF}
}

// Interpreter holds the state of the interpreter
type Interpreter struct {
	variables map[string]interface{}
}

// NewInterpreter creates a new interpreter
func NewInterpreter() *Interpreter {
	return &Interpreter{variables: make(map[string]interface{})}
}

// Interpret runs the program
func (i *Interpreter) Interpret(program *Program) {
	for _, stmt := range program.Statements {
		i.executeStatement(stmt)
	}
}

// executeStatement executes a statement
func (i *Interpreter) executeStatement(stmt Statement) {
	switch s := stmt.(type) {
	case *PrintStatement:
		value := i.evaluateExpression(s.Expression)
		fmt.Println(value)
	case *VarStatement:
		if s.Value != nil {
			value := i.evaluateExpression(s.Value)
			i.variables[s.Name] = value
		} else {
			if s.Type == "int" {
				i.variables[s.Name] = 0
			} else {
				i.variables[s.Name] = ""
			}
		}
	case *ForStatement:
		i.executeStatement(s.Init)
		for {
			cond := i.evaluateExpression(s.Condition)
			if !cond.(bool) {
				break
			}
			for _, bodyStmt := range s.Body {
				i.executeStatement(bodyStmt)
			}
			i.evaluateExpression(s.Increment)
		}
	case *IfStatement:
		cond := i.evaluateExpression(s.Condition)
		if cond.(bool) {
			for _, stmt := range s.ThenBranch {
				i.executeStatement(stmt)
			}
		} else {
			for _, stmt := range s.ElseBranch {
				i.executeStatement(stmt)
			}
		}
	}
}

// evaluateExpression evaluates an expression
func (i *Interpreter) evaluateExpression(expr Expression) interface{} {
	switch e := expr.(type) {
	case *LiteralExpression:
		if strings.Contains(e.Value, "\"") {
			return strings.Trim(e.Value, "\"")
		}
		num, err := strconv.Atoi(e.Value)
		if err == nil {
			return num
		}
		return e.Value
	case *VariableExpression:
		return i.variables[e.Name]
	case *InputExpression:
		fmt.Print(e.Prompt)
		reader := bufio.NewReader(os.Stdin)
		input, _ := reader.ReadString('\n')
		return strings.TrimSpace(input)
	case *BinaryExpression:
		left := i.evaluateExpression(e.Left)
		right := i.evaluateExpression(e.Right)
		switch e.Operator {
		case "+":
			switch l := left.(type) {
			case string:
				return l + right.(string)
			case int:
				return l + right.(int)
			}
		case "-":
			return left.(int) - right.(int)
		case "%":
			return left.(int) % right.(int)
		case "==":
			return left == right
		case "<":
			return left.(int) < right.(int)
		case ">":
			return left.(int) > right.(int)
		}
	}
	return nil
}

func main() {
	input := `
	# Basic Print Statement
	print("Hello, World, from Neon!");

	var x: string = input("Enter number>>> ");
	print("Your number is : " + x);

	var x: string = input("Enter string>>> ");
	print("Result: " + x);

	for (var i = 0; i <= 10; i = i + 2) {
		print("Even: " + i);
	}

	for (var i = 0; i < 72 + 1; i = i + 1) {
		if (i % 2 == 0) {
			print("Even number: " + i);
			print("Hello from Neon");
		} else {
			print("Odd number: " + i);
		}
	}

	var num: int = -3;
	if (num > 0) {
		print("Positive");
	} else {
		print("Negative or zero");
	}

	var num: int = 7;
	if (num > 0) {
		print("Number is positive");
	}
	`

	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	interpreter := NewInterpreter()
	interpreter.Interpret(program)
}