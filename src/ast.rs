/*
primary_expression
    : IDENTIFIER
    | CONSTANT
    | STRING_LITERAL
    | '(' expression ')'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum PrimaryExpression {
    Identifier(String),
    Constant(String),
    StringLiteral(String),
    ParenthesizedExpression(Box<Expression>),
}

/*
postfix_expression
    : primary_expression
    | postfix_expression '[' expression ']'
    | postfix_expression '(' ')'
    | postfix_expression '(' argument_expression_list ')'
    | postfix_expression '.' IDENTIFIER
    | postfix_expression PTR_OP IDENTIFIER
    | postfix_expression INC_OP
    | postfix_expression DEC_OP
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum PostfixExpression {
    PrimaryExpression(PrimaryExpression),
    ArrayAccess(Box<(PostfixExpression, Expression)>),
    FunctionCall(Box<(PostfixExpression,)>),
    FunctionCallArgument(Box<(PostfixExpression, ArgumentExpressionList)>),
    MemberAccess(Box<(PostfixExpression, String)>),
    PointerAccess(Box<(PostfixExpression, String)>),
    Increment(Box<(PostfixExpression,)>),
    Decrement(Box<(PostfixExpression,)>),
}

/*
argument_expression_list
    : assignment_expression
    | argument_expression_list ',' assignment_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ArgumentExpressionList {
    Single(Box<(AssignmentExpression,)>),
    Multiple(Box<(ArgumentExpressionList, AssignmentExpression,)>),
}

/*
unary_expression
    : postfix_expression
    | INC_OP unary_expression
    | DEC_OP unary_expression
    | unary_operator cast_expression
    | SIZEOF unary_expression
    | SIZEOF '(' type_name ')'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryExpression {
    PostfixExpression(Box<(PostfixExpression,)>),
    Increment(Box<(UnaryExpression,)>),
    Decrement(Box<(UnaryExpression,)>),
    UnaryOperator(Box<(UnaryOperator, CastExpression,)>),
    SizeOfExpression(Box<(UnaryExpression,)>),
    SizeOfType(Box<(TypeName,)>),
}

/*
unary_operator
    : '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | '!'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    And,
    Mul,
    Add,
    Sub,
    BitwiseNot,
    Not,
}

/*
cast_expression
    : unary_expression
    | '(' type_name ')' cast_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum CastExpression {
    UnaryExpression(Box<(UnaryExpression,)>),
    Cast(Box<(TypeName, CastExpression,)>),
}

/*
multiplicative_expression
    : cast_expression
    | multiplicative_expression '*' cast_expression
    | multiplicative_expression '/' cast_expression
    | multiplicative_expression '%' cast_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum MultiplicativeExpression {
    CastExpression(Box<(CastExpression,)>),
    Multiply(Box<(MultiplicativeExpression, CastExpression,)>),
    Divide(Box<(MultiplicativeExpression, CastExpression)>),
    Modulus(Box<(MultiplicativeExpression, CastExpression)>),
}

/*
additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression
    | additive_expression '-' multiplicative_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum AdditiveExpression {
    MultiplicativeExpression(Box<(MultiplicativeExpression,)>),
    Add(Box<(AdditiveExpression, MultiplicativeExpression)>),
    Subtract(Box<(AdditiveExpression, MultiplicativeExpression)>),
}

/*
shift_expression
    : additive_expression
    | shift_expression LEFT_OP additive_expression
    | shift_expression RIGHT_OP additive_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ShiftExpression {
    AdditiveExpression(Box<(AdditiveExpression,)>),
    LeftShift(Box<(ShiftExpression, AdditiveExpression,)>),
    RightShift(Box<(ShiftExpression, AdditiveExpression,)>),
}

/*
relational_expression
    : shift_expression
    | relational_expression '<' shift_expression
    | relational_expression '>' shift_expression
    | relational_expression LE_OP shift_expression
    | relational_expression GE_OP shift_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum RelationalExpression {
    ShiftExpression(Box<(ShiftExpression,)>),
    LessThan(Box<(RelationalExpression, ShiftExpression,)>),
    GreaterThan(Box<(RelationalExpression, ShiftExpression,)>),
    LessThanOrEqual(Box<(RelationalExpression, ShiftExpression,)>),
    GreaterThanOrEqual(Box<(RelationalExpression, ShiftExpression,)>),
}

/*
equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression
    | equality_expression NE_OP relational_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum EqualityExpression {
    RelationalExpression(Box<(RelationalExpression,)>),
    Equal(Box<(EqualityExpression, RelationalExpression,)>),
    NotEqual(Box<(EqualityExpression, RelationalExpression,)>),
}

/*
and_expression
    : equality_expression
    | and_expression '&' equality_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum AndExpression {
    EqualityExpression(Box<(EqualityExpression,)>),
    And(Box<(AndExpression, EqualityExpression,)>),
}
/*
exclusive_or_expression
    : and_expression
    | exclusive_or_expression '^' and_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ExclusiveOrExpression {
    AndExpression(Box<(AndExpression,)>),
    Xor(Box<(ExclusiveOrExpression, AndExpression,)>),
}

/*
inclusive_or_expression
    : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum InclusiveOrExpression {
    ExclusiveOrExpression(Box<(ExclusiveOrExpression,)>),
    Or(Box<(InclusiveOrExpression, ExclusiveOrExpression,)>),
}

/*
logical_and_expression
    : inclusive_or_expression
    | logical_and_expression AND_OP inclusive_or_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum LogicalAndExpression {
    InclusiveOrExpression(Box<(InclusiveOrExpression,)>),
    And(Box<(LogicalAndExpression, InclusiveOrExpression,)>),
}

/*
logical_or_expression
    : logical_and_expression
    | logical_or_expression OR_OP logical_and_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum LogicalOrExpression {
    LogicalAndExpression(Box<(LogicalAndExpression,)>),
    Or(Box<(LogicalOrExpression, LogicalAndExpression,)>),
}

/*
conditional_expression
    : logical_or_expression
    | logical_or_expression '?' expression ':' conditional_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ConditionalExpression {
    LogicalOrExpression(Box<(LogicalOrExpression,)>),
    Conditional(Box<(LogicalOrExpression, Expression, ConditionalExpression,)>),
}

/*
assignment_expression
    : conditional_expression
    | unary_expression assignment_operator assignment_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentExpression {
    ConditionalExpression(Box<(ConditionalExpression,)>),
    Assignment(Box<(UnaryExpression, AssignmentOperator, AssignmentExpression,)>),
}




/*
assignment_operator
    : '='
    | MUL_ASSIGN
    | DIV_ASSIGN
    | MOD_ASSIGN
    | ADD_ASSIGN
    | SUB_ASSIGN
    | LEFT_ASSIGN
    | RIGHT_ASSIGN
    | AND_ASSIGN
    | XOR_ASSIGN
    | OR_ASSIGN
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentOperator {
    Assign,
    MultiplyAssign,
    DivideAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    LeftShiftAssign,
    RightShiftAssign,
    AndAssign,
    XORAssign,
    OrAssign,
}

/*
expression
    : assignment_expression
    | expression ',' assignment_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    AssignmentExpression(Box<(AssignmentExpression,)>),
    Multiple(Box<(Expression, AssignmentExpression,)>),
}

/*
constant_expression
    : conditional_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ConstantExpression {
    ConditionalExpression(Box<(ConditionalExpression,)>),
}

/*
declaration
    : declaration_specifiers ';'
    | declaration_specifiers init_declarator_list ';'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    DeclarationSpecifiers(Box<(DeclarationSpecifiers,)>),
    DeclarationSpecifiersWithList(Box<(DeclarationSpecifiers, InitDeclaratorList,)>),
}

/*
declaration_specifiers
    : storage_class_specifier
    | storage_class_specifier declaration_specifiers
    | type_specifier
    | type_specifier declaration_specifiers
    | type_qualifier
    | type_qualifier declaration_specifiers
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum DeclarationSpecifiers {
    StorageClassSpecifier(Box<(StorageClassSpecifier,)>),
    StorageClassSpecifierRec(Box<(StorageClassSpecifier, DeclarationSpecifiers,)>),
    TypeSpecifier(Box<(TypeSpecifier,)>),
    TypeSpecifierRec(Box<(TypeSpecifier, DeclarationSpecifiers,)>),
    TypeQualifier(Box<(TypeQualifier,)>),
    TypeQualifierRec(Box<(TypeQualifier, DeclarationSpecifiers,)>),
}

/*
init_declarator_list
    : init_declarator
    | init_declarator_list ',' init_declarator
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum InitDeclaratorList {
    Single(Box<(InitDeclarator,)>),
    Multiple(Box<(InitDeclaratorList, InitDeclarator,)>),
}

/*
init_declarator
    : declarator
    | declarator '=' initializer
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum InitDeclarator {
    Declarator(Box<(Declarator,)>),
    DeclaratorWithInitializer(Box<(Declarator, Initializer,)>),
}

/*
storage_class_specifier
    : TYPEDEF
    | EXTERN
    | STATIC
    | AUTO
    | REGISTER
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum StorageClassSpecifier {
    TypeDef,
    Extern,
    Static,
    Auto,
    Register,
}

/*
type_specifier
    : VOID
    | CHAR
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    | SIGNED
    | UNSIGNED
    | struct_or_union_specifier
    | enum_specifier
    | TYPE_NAME
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    StructOrUnionSpecifier(Box<(StructOrUnionSpecifier,)>),
    EnumSpecifier(Box<(EnumSpecifier,)>),
    TypeName(Box<(String,)>),
}







/*
struct_or_union_specifier
    : struct_or_union IDENTIFIER '{' struct_declaration_list '}'
    | struct_or_union '{' struct_declaration_list '}'
    | struct_or_union IDENTIFIER
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum StructOrUnionSpecifier {
    Named(Box<(StructOrUnion, String, StructDeclarationList,)>),
    Anonymous(Box<(StructOrUnion, StructDeclarationList,)>),
    NamedOnly(Box<(StructOrUnion, String,)>),
}

/*
struct_or_union
    : STRUCT
    | UNION
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum StructOrUnion {
    Struct,
    Union,
}

/*
struct_declaration_list
    : struct_declaration
    | struct_declaration_list struct_declaration
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum StructDeclarationList {
    Single(Box<(StructDeclaration,)>),
    Multiple(Box<(StructDeclarationList, StructDeclaration,)>),
}

/*
struct_declaration
    : specifier_qualifier_list struct_declarator_list ';'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum StructDeclaration {
    SpecifierQualifierList(Box<(SpecifierQualifierList, StructDeclaratorList,)>),
}

/*
specifier_qualifier_list
    : type_specifier specifier_qualifier_list
    | type_specifier
    | type_qualifier specifier_qualifier_list
    | type_qualifier
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum SpecifierQualifierList {
    TypeSpecifierSingle(Box<(TypeSpecifier,)>),
    TypeSpecifierMultiple(Box<(TypeSpecifier, SpecifierQualifierList,)>),
    TypeQualifierSingle(Box<(TypeQualifier,)>),
    TypeQualifierMultiple(Box<(TypeQualifier, SpecifierQualifierList,)>),
}

/*
struct_declarator_list
    : struct_declarator
    | struct_declarator_list ',' struct_declarator
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum StructDeclaratorList {
    Single(Box<(StructDeclarator,)>),
    Multiple(Box<(StructDeclaratorList, StructDeclarator,)>),
}

/*
struct_declarator
    : declarator
    | ':' constant_expression
    | declarator ':' constant_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum StructDeclarator {
    Declarator(Box<(Declarator,)>),
    BitField(Box<(ConstantExpression,)>),
    DeclaratorWithBitField(Box<(Declarator, ConstantExpression,)>),
}

/*
enum_specifier
    : ENUM '{' enumerator_list '}'
    | ENUM IDENTIFIER '{' enumerator_list '}'
    | ENUM IDENTIFIER
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum EnumSpecifier {
    Named(Box<(String, EnumeratorList,)>),
    Anonymous(Box<(EnumeratorList,)>),
    NamedOnly(Box<(String,)>),
}

/*
enumerator_list
    : enumerator
    | enumerator_list ',' enumerator
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum EnumeratorList {
    Single(Box<(Enumerator,)>),
    EnumeratorList(Box<(EnumeratorList, Enumerator,)>),
}

/*
enumerator
    : IDENTIFIER
    | IDENTIFIER '=' constant_expression
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum Enumerator {
    Identifier(Box<(String,)>),
    IdentifierWithValue(Box<(String, ConstantExpression,)>),
}

/*
type_qualifier
    : CONST
    | VOLATILE
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum TypeQualifier {
    Const,
    Volatile,
}

/*
declarator
    : pointer direct_declarator
    | direct_declarator
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum Declarator {
    PointerWithDirect(Box<(Pointer, DirectDeclarator,)>),
    Direct(Box<(DirectDeclarator,)>),
}

/*
direct_declarator
    : IDENTIFIER
    | '(' declarator ')'
    | direct_declarator '[' constant_expression ']'
    | direct_declarator '[' ']'
    | direct_declarator '(' parameter_type_list ')'
    | direct_declarator '(' identifier_list ')'
    | direct_declarator '(' ')'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum DirectDeclarator {
    Identifier(Box<(String,)>),
    Parenthesized(Box<(Declarator,)>),
    ArrayWithSize(Box<(DirectDeclarator, ConstantExpression,)>),
    ArrayWithoutSize(Box<(DirectDeclarator,)>),
    FunctionWithParams(Box<(DirectDeclarator, ParameterTypeList,)>),
    FunctionWithIdentifiers(Box<(DirectDeclarator, IdentifierList,)>),
    FunctionWithoutParams(Box<(DirectDeclarator,)>),
}

/*
pointer
    : '*'
    | '*' type_qualifier_list
    | '*' pointer
    | '*' type_qualifier_list pointer
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum Pointer {
    Base,
    WithTypeQualifiers(Box<(TypeQualifierList,)>),
    Recursive(Box<(Pointer,)>),
    WithTypeQualifiersAndPointer(Box<(TypeQualifierList, Pointer,)>),
}

/*
type_qualifier_list
    : type_qualifier
    | type_qualifier_list type_qualifier
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum TypeQualifierList {
    Single(Box<(TypeQualifier,)>),
    Multiple(Box<(TypeQualifierList, TypeQualifier,)>),
}

/*
parameter_type_list
    : parameter_list
    | parameter_list ',' ELLIPSIS
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ParameterTypeList {
    List(Box<(ParameterList,)>),
    ListWithEllipsis(Box<(ParameterList,)>),
}

/*
parameter_list
    : parameter_declaration
    | parameter_list ',' parameter_declaration
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ParameterList {
    Single(Box<(ParameterDeclaration,)>),
    Multiple(Box<(ParameterList, ParameterDeclaration,)>),
}

/*
parameter_declaration
    : declaration_specifiers declarator
    | declaration_specifiers abstract_declarator
    | declaration_specifiers
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ParameterDeclaration {
    WithDeclarator(Box<(DeclarationSpecifiers, Declarator,)>),
    WithAbstractDeclarator(Box<(DeclarationSpecifiers, AbstractDeclarator,)>),
    OnlySpecifiers(Box<(DeclarationSpecifiers,)>),
}

/*
identifier_list
    : IDENTIFIER
    | identifier_list ',' IDENTIFIER
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum IdentifierList {
    Single(Box<(String,)>),
    Multiple(Box<(IdentifierList, String,)>),
}

/*
type_name
    : specifier_qualifier_list
    | specifier_qualifier_list abstract_declarator
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum TypeName {
    SpecifierQualifierList(Box<(SpecifierQualifierList,)>),
    SpecifierQualifierListWithAbstract(Box<(SpecifierQualifierList, AbstractDeclarator,)>),
}
/*
abstract_declarator
    : pointer
    | direct_abstract_declarator
    | pointer direct_abstract_declarator
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum AbstractDeclarator {
    PointerOnly(Box<(Pointer,)>),
    DirectAbstractDeclarator(Box<(DirectAbstractDeclarator,)>),
    PointerWithDirect(Box<(Pointer, DirectAbstractDeclarator,)>),
}

/*
direct_abstract_declarator
    : '(' abstract_declarator ')'
    | '[' ']'
    | '[' constant_expression ']'
    | direct_abstract_declarator '[' ']'
    | direct_abstract_declarator '[' constant_expression ']'
    | '(' ')'
    | '(' parameter_type_list ')'
    | direct_abstract_declarator '(' ')'
    | direct_abstract_declarator '(' parameter_type_list ')'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum DirectAbstractDeclarator {
    Parenthesized(Box<(AbstractDeclarator,)>),
    ArrayBase,
    ArrayConstExpression(Box<(ConstantExpression,)>),
    RecArrayBase(Box<(DirectAbstractDeclarator,)>),
    RecArrayConstExpression(Box<(DirectAbstractDeclarator, ConstantExpression,)>),
    FunctionBase,
    FunctionParameterTypeList(Box<(ParameterTypeList,)>),
    RecFunctionBase(Box<(DirectAbstractDeclarator,)>),
    RecFunctionParameterTypeList(Box<(DirectAbstractDeclarator, ParameterTypeList,)>),
}

/*
initializer
    : assignment_expression
    | '{' initializer_list '}'
    | '{' initializer_list ',' '}'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum Initializer {
    AssignmentExpression(Box<(AssignmentExpression,)>),
    List(Box<(InitializerList,)>),
    ListWithTrailingComma(Box<(InitializerList,)>),
}

/*
initializer_list
    : initializer
    | initializer_list ',' initializer
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum InitializerList {
    Single(Box<(Initializer,)>),
    Multiple(Box<(InitializerList, Initializer,)>),
}

/*
statement
    : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Labeled(Box<(LabeledStatement,)>),
    Compound(Box<(CompoundStatement,)>),
    Expression(Box<(ExpressionStatement,)>),
    Selection(Box<(SelectionStatement,)>),
    Iteration(Box<(IterationStatement,)>),
    Jump(Box<(JumpStatement,)>),
}

/*
labeled_statement
    : IDENTIFIER ':' statement
    | CASE constant_expression ':' statement
    | DEFAULT ':' statement
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum LabeledStatement {
    Identifier(Box<(String, Statement,)>),
    Case(Box<(ConstantExpression, Statement,)>),
    Default(Box<(Statement,)>),
}

/*
compound_statement
    : '{' '}'
    | '{' statement_list '}'
    | '{' declaration_list '}'
    | '{' declaration_list statement_list '}'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum CompoundStatement {
    Empty,
    StatementList(Box<(StatementList,)>),
    DeclarationList(Box<(DeclarationList,)>),
    DeclarationAndStatementList(Box<(DeclarationList, StatementList,)>),
}

/*
declaration_list
    : declaration
    | declaration_list declaration
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum DeclarationList {
    Single(Box<(Declaration,)>),
    Multiple(Box<(DeclarationList, Declaration,)>),
}

/*
statement_list
    : statement
    | statement_list statement
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum StatementList {
    Single(Box<(Statement,)>),
    Multiple(Box<(StatementList, Statement,)>),
}

/*
expression_statement
    : ';'
    | expression ';'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionStatement {
    Empty,
    Expression(Box<(Expression,)>),
}

/*
selection_statement
    : IF '(' expression ')' statement
    | IF '(' expression ')' statement ELSE statement
    | SWITCH '(' expression ')' statement
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum SelectionStatement {
    If(Box<(Expression, Statement,)>),
    IfElse(Box<(Expression, CompoundStatement, CompoundStatement,)>),
    Switch(Box<(Expression, Statement,)>),
}

/*
iteration_statement
    : WHILE '(' expression ')' statement
    | DO statement WHILE '(' expression ')' ';'
    | FOR '(' expression_statement expression_statement ')' statement
    | FOR '(' expression_statement expression_statement expression ')' statement
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum IterationStatement {
    While(Box<(Expression, Statement,)>),
    DoWhile(Box<(Statement, Expression,)>),
    ForWithoutExpression(Box<(ExpressionStatement, ExpressionStatement, Statement,)>),
    ForWithExpression(Box<(ExpressionStatement, ExpressionStatement, Expression, Statement,)>),
}

/*
jump_statement
    : GOTO IDENTIFIER ';'
    | CONTINUE ';'
    | BREAK ';'
    | RETURN ';'
    | RETURN expression ';'
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum JumpStatement {
    Goto(Box<(String,)>),
    Continue,
    Break,
    ReturnEmpty,
    Return(Box<(Expression,)>),
}

/*
translation_unit
    : external_declaration
    | translation_unit external_declaration
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum TranslationUnit {
    Single(Box<(ExternalDeclaration,)>),
    Multiple(Box<(TranslationUnit, ExternalDeclaration,)>),
}

/*
external_declaration
    : function_definition
    | declaration
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ExternalDeclaration {
    FunctionDefinition(Box<(FunctionDefinition,)>),
    Declaration(Box<(Declaration,)>),
}

/*
function_definition
    : declaration_specifiers declarator declaration_list compound_statement
    | declaration_specifiers declarator compound_statement
    | declarator declaration_list compound_statement
    | declarator compound_statement
    ;
*/
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionDefinition {
    Full(Box<(DeclarationSpecifiers, Declarator, DeclarationList, CompoundStatement,)>),
    WithDeclarations(Box<(DeclarationSpecifiers, Declarator, CompoundStatement,)>),
    WithDeclarationsAndDefinitions(Box<(Declarator, DeclarationList, CompoundStatement,)>),
    Simple(Box<(Declarator, CompoundStatement,)>),
}