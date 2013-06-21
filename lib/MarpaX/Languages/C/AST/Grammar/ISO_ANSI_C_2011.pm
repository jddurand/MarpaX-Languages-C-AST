use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011::Actions;

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    return $self;
}

sub deref {
    my $self = shift;
    return [ @{$_[0]} ];
}

sub deref_and_bless_declaration {
    my $self = shift;
    return bless $self->deref(@_), 'C::AST::declaration';
}

sub deref_and_bless_declarator {
    my $self = shift;
    return bless $self->deref(@_), 'C::AST::declarator';
}

sub deref_and_bless_compoundStatement {
    my $self = shift;
    return bless $self->deref(@_), 'C::AST::compoundStatement';
}

package MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011;

# ABSTRACT: ISO ANSI C 2011 grammar written in Marpa BNF

# VERSION

=head1 DESCRIPTION

This modules returns describes the ISO ANSI C 2011 C grammar written in Marpa BNF, as of L<http://www.quut.com/c/ANSI-C-grammar-y-2011.html> and L<http://www.quut.com/c/ANSI-C-grammar-l.html>.

=head1 SYNOPSIS

    use strict;
    use warnings FATAL => 'all';
    use MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011;

    my $grammar = MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011->new();

    my $grammar_content = $grammar->content();
    my $grammar_option = $grammar->grammar_option();
    my $recce_option = $grammar->recce_option();

=head1 SUBROUTINES/METHODS

=head2 new()

Instance a new object. Takes no argument.

=cut

sub new {
  my ($class) = @_;

  my $self  = {
    _content => do { local $/; <DATA> },
    _grammar_option => {action_object  => sprintf('%s::%s', __PACKAGE__, 'Actions')},
    _recce_option => {ranking_method => 'high_rule_only'},
  };
  bless($self, $class);

  return $self;
}

=head2 content()

Returns the content of the grammar. Takes no argument.

=cut

sub content {
    my ($self) = @_;
    return $self->{_content};
}

=head2 grammar_option()

Returns recommended option for Marpa::R2::Scanless::G->new(), returned as a reference to a hash.

=cut

sub grammar_option {
    my ($self) = @_;
    return $self->{_grammar_option};
}

=head2 recce_option()

Returns recommended option for Marpa::R2::Scanless::R->new(), returned as a reference to a hash.

=cut

sub recce_option {
    my ($self) = @_;
    return $self->{_recce_option};
}

1;
__DATA__
################################################################################################################
#                                                    grammar
#
# 2011 ISO C, as of http://www.quut.com/c/ANSI-C-grammar-l.html
#                   http://www.quut.com/c/ANSI-C-grammar-y-2011.html
#
################################################################################################################

#
# Defaults
#
:default ::= action => [values] bless => ::lhs

# Except for the functionDefinition compoundStatement
# we always associate LPAREN and LCURLY to enterScope,
# RPAREN and RCURLY to exitScope
event 'lparen$'                = completed <lparen>
event 'rparen$'                = completed <rparen>
event 'lcurly$'                = completed <lcurly>
event 'lcurlyMaybeEnterScope$' = completed <lcurlyMaybeEnterScope>
event 'rcurly$'                = completed <rcurly>
lparen                ::= LPAREN                action => deref
rparen                ::= RPAREN                action => deref
lcurly                ::= LCURLY                action => deref
lcurlyMaybeEnterScope ::= LCURLY                action => deref
rcurly                ::= RCURLY                action => deref

#
# G1 (grammar), c.f. http://www.quut.com/c/ANSI-C-grammar-y-2011.html
#
:start ::= translationUnit

event 'primaryExpressionIdentifier$' = completed <primaryExpressionIdentifier>
primaryExpressionIdentifier
	::= IDENTIFIER         action => deref

primaryExpression
	::= primaryExpressionIdentifier
	| constant
	| string
	| lparen expression rparen
	| genericSelection

constant
	::= I_CONSTANT         # includes character_constant
	| F_CONSTANT
	| ENUMERATION_CONSTANT # after it has been defined as such

event 'enumerationConstantIdentifier$' = completed <enumerationConstantIdentifier>
enumerationConstantIdentifier  # before it has been defined as such
	::= IDENTIFIER        action => deref

enumerationConstant            # before it has been defined as such
	::= enumerationConstantIdentifier

string
	::= STRING_LITERAL
	| FUNC_NAME

genericSelection
	::= GENERIC lparen assignmentExpression COMMA genericAssocList rparen

genericAssocList
	::= genericAssociation
	| genericAssocList COMMA genericAssociation

genericAssociation
	::= typeName COLON assignmentExpression
	| DEFAULT COLON assignmentExpression

postfixExpression
	::= primaryExpression
	| postfixExpression LBRACKET expression RBRACKET
	| postfixExpression lparen rparen
	| postfixExpression lparen argumentExpressionList rparen
	| postfixExpression DOT IDENTIFIER
	| postfixExpression PTR_OP IDENTIFIER
	| postfixExpression INC_OP
	| postfixExpression DEC_OP
	| lparen typeName rparen lcurly initializerList rcurly
	| lparen typeName rparen lcurly initializerList COMMA rcurly

argumentExpressionList
	::= assignmentExpression
	| argumentExpressionList COMMA assignmentExpression

unaryExpression
	::= postfixExpression
	| INC_OP unaryExpression
	| DEC_OP unaryExpression
	| unaryOperator castExpression
	| SIZEOF unaryExpression
	| SIZEOF lparen typeName rparen
	| ALIGNOF lparen typeName rparen

unaryOperator
	::= AMPERSAND
	| STAR
	| PLUS
	| HYPHEN
	| TILDE
	| EXCLAMATION

castExpression
	::= unaryExpression
	| lparen typeName rparen castExpression

multiplicativeExpression
	::= castExpression
	| multiplicativeExpression STAR castExpression
	| multiplicativeExpression SLASH castExpression
	| multiplicativeExpression PERCENT castExpression

additiveExpression
	::= multiplicativeExpression
	| additiveExpression PLUS multiplicativeExpression
	| additiveExpression HYPHEN multiplicativeExpression

shiftExpression
	::= additiveExpression
	| shiftExpression LEFT_OP additiveExpression
	| shiftExpression RIGHT_OP additiveExpression

relationalExpression
	::= shiftExpression
	| relationalExpression LESS_THAN shiftExpression
	| relationalExpression GREATER_THAN shiftExpression
	| relationalExpression LE_OP shiftExpression
	| relationalExpression GE_OP shiftExpression

equalityExpression
	::= relationalExpression
	| equalityExpression EQ_OP relationalExpression
	| equalityExpression NE_OP relationalExpression

andExpression
	::= equalityExpression
	| andExpression AMPERSAND equalityExpression

exclusiveOrExpression
	::= andExpression
	| exclusiveOrExpression CARET andExpression

inclusiveOrExpression
	::= exclusiveOrExpression
	| inclusiveOrExpression VERTICAL_BAR exclusiveOrExpression

logicalAndExpression
	::= inclusiveOrExpression
	| logicalAndExpression AND_OP inclusiveOrExpression

logicalOrExpression
	::= logicalAndExpression
	| logicalOrExpression OR_OP logicalAndExpression

conditionalExpression
	::= logicalOrExpression
	| logicalOrExpression QUESTION_MARK expression COLON conditionalExpression

assignmentExpression
	::= conditionalExpression
	| unaryExpression assignmentOperator assignmentExpression

assignmentOperator
	::= EQUAL
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

expression
	::= assignmentExpression
	| expression COMMA assignmentExpression

constantExpression
	::= conditionalExpression # with constraints

# ###############################################################################################
# A directDeclarator introduces a typedefName only when it eventually participates in the grammar
# rule:
# declaration ::= declarationSpecifiers initDeclaratorList SEMICOLON
# ###############################################################################################
event 'declarationCheck01declarationSpecifiers$' = completed <declarationCheck01declarationSpecifiers>
declarationCheck01declarationSpecifiers ::= declarationSpecifiers action => deref

event 'declarationCheck01initDeclaratorList$' = completed <declarationCheck01initDeclaratorList>
declarationCheck01initDeclaratorList    ::= initDeclaratorList    action => deref

event 'declarationCheck01$' = completed <declarationCheck01>
declarationCheck01 ::= declarationCheck01declarationSpecifiers declarationCheck01initDeclaratorList SEMICOLON

declaration
	::= declarationSpecifiers SEMICOLON
	| declarationCheck01                                      action => deref_and_bless_declaration
	| staticAssertDeclaration

declarationSpecifiers
	::= storageClassSpecifier declarationSpecifiers
	| storageClassSpecifier
	| typeSpecifier declarationSpecifiers
	| typeSpecifier
	| typeQualifier declarationSpecifiers
	| typeQualifier
	| functionSpecifier declarationSpecifiers
	| functionSpecifier
	| alignmentSpecifier declarationSpecifiers
	| alignmentSpecifier

initDeclaratorList
	::= initDeclarator
	| initDeclaratorList COMMA initDeclarator

initDeclarator
	::= declarator EQUAL initializer
	| declarator

event 'storageClassSpecifierTypedef$' = completed <storageClassSpecifierTypedef>
storageClassSpecifierTypedef
	::= TYPEDEF            action => deref

storageClassSpecifier
	::= storageClassSpecifierTypedef # identifiers must be flagged as TYPEDEF_NAME
	| EXTERN
	| STATIC
	| THREAD_LOCAL
	| AUTO
	| REGISTER

typeSpecifier
	::= VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| BOOL
	| COMPLEX
	| IMAGINARY	  	# non-mandated extension
	| atomicTypeSpecifier
	| structOrUnionSpecifier
	| enumSpecifier
	| TYPEDEF_NAME		# after it has been defined as such

structOrUnionSpecifier
	::= structOrUnion lcurly structDeclarationList rcurly
	| structOrUnion IDENTIFIER lcurly structDeclarationList rcurly
	| structOrUnion IDENTIFIER

structOrUnion
	::= STRUCT
	| UNION

structDeclarationList
	::= structDeclaration
	| structDeclarationList structDeclaration

structDeclaration
	::= specifierQualifierList SEMICOLON	# for anonymous struct/union
	| specifierQualifierList structDeclaratorList SEMICOLON
	| staticAssertDeclaration

specifierQualifierList
	::= typeSpecifier specifierQualifierList
	| typeSpecifier
	| typeQualifier specifierQualifierList
	| typeQualifier

structDeclaratorList
	::= structDeclarator
	| structDeclaratorList COMMA structDeclarator

structDeclarator
	::= COLON constantExpression
	| declarator COLON constantExpression
	| declarator

enumSpecifier
	::= ENUM lcurly enumeratorList rcurly
	| ENUM lcurly enumeratorList COMMA rcurly
	| ENUM IDENTIFIER lcurly enumeratorList rcurly
	| ENUM IDENTIFIER lcurly enumeratorList COMMA rcurly
	| ENUM IDENTIFIER

enumeratorList
	::= enumerator
	| enumeratorList COMMA enumerator

enumerator	# identifiers must be flagged as ENUMERATION_CONSTANT
	::= enumerationConstant EQUAL constantExpression
	| enumerationConstant

atomicTypeSpecifier
	::= ATOMIC lparen typeName rparen

typeQualifier
	::= CONST
	| RESTRICT
	| VOLATILE
	| ATOMIC

functionSpecifier
	::= INLINE
	| NORETURN

alignmentSpecifier
	::= ALIGNAS lparen typeName rparen
	| ALIGNAS lparen constantExpression rparen

declarator
	::= pointer directDeclarator
	| directDeclarator

event 'directDeclaratorIdentifier$' = completed <directDeclaratorIdentifier>
directDeclaratorIdentifier
	::= IDENTIFIER          action => deref

directDeclarator
	::= directDeclaratorIdentifier
	| lparen declarator rparen
	| directDeclarator LBRACKET RBRACKET
	| directDeclarator LBRACKET STAR RBRACKET
	| directDeclarator LBRACKET STATIC typeQualifierList assignmentExpression RBRACKET
	| directDeclarator LBRACKET STATIC assignmentExpression RBRACKET
	| directDeclarator LBRACKET typeQualifierList STAR RBRACKET
	| directDeclarator LBRACKET typeQualifierList STATIC assignmentExpression RBRACKET
	| directDeclarator LBRACKET typeQualifierList assignmentExpression RBRACKET
	| directDeclarator LBRACKET typeQualifierList RBRACKET
	| directDeclarator LBRACKET assignmentExpression RBRACKET
	| directDeclarator lparen parameterTypeList rparen
	| directDeclarator lparen rparen
	| directDeclarator lparen identifierList rparen

pointer
	::= STAR typeQualifierList pointer
	| STAR typeQualifierList
	| STAR pointer
	| STAR

typeQualifierList
	::= typeQualifier
	| typeQualifierList typeQualifier

parameterTypeList
	::= parameterList COMMA ELLIPSIS
	| parameterList

parameterList
	::= parameterDeclaration
	| parameterList COMMA parameterDeclaration

parameterDeclaration
	::= declarationSpecifiers declarator
	| declarationSpecifiers abstractDeclarator
	| declarationSpecifiers

identifierList
	::= IDENTIFIER
	| identifierList COMMA IDENTIFIER

typeName
	::= specifierQualifierList abstractDeclarator
	| specifierQualifierList

abstractDeclarator
	::= pointer directAbstractDeclarator
	| pointer
	| directAbstractDeclarator

directAbstractDeclarator
	::= lparen abstractDeclarator rparen
	| LBRACKET RBRACKET
	| LBRACKET STAR RBRACKET
	| LBRACKET STATIC typeQualifierList assignmentExpression RBRACKET
	| LBRACKET STATIC assignmentExpression RBRACKET
	| LBRACKET typeQualifierList STATIC assignmentExpression RBRACKET
	| LBRACKET typeQualifierList assignmentExpression RBRACKET
	| LBRACKET typeQualifierList RBRACKET
	| LBRACKET assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET RBRACKET
	| directAbstractDeclarator LBRACKET STAR RBRACKET
	| directAbstractDeclarator LBRACKET STATIC typeQualifierList assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET STATIC assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET typeQualifierList assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET typeQualifierList STATIC assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET typeQualifierList RBRACKET
	| directAbstractDeclarator LBRACKET assignmentExpression RBRACKET
	| lparen rparen
	| lparen parameterTypeList rparen
	| directAbstractDeclarator lparen rparen
	| directAbstractDeclarator lparen parameterTypeList rparen

initializer
	::= lcurly initializerList rcurly
	| lcurly initializerList COMMA rcurly
	| assignmentExpression

initializerList
	::= designation initializer
	| initializer
	| initializerList COMMA designation initializer
	| initializerList COMMA initializer

designation
	::= designatorList EQUAL

designatorList
	::= designator
	| designatorList designator

designator
	::= LBRACKET constantExpression RBRACKET
	| DOT IDENTIFIER

staticAssertDeclaration
	::= STATIC_ASSERT lparen constantExpression COMMA STRING_LITERAL rparen SEMICOLON

statement
	::= labeledStatement
	| compoundStatement
	| expressionStatement
	| selectionStatement
	| iterationStatement
	| jumpStatement

labeledStatement
	::= IDENTIFIER COLON statement
	| CASE constantExpression COLON statement
	| DEFAULT COLON statement

compoundStatement
	::= lcurly rcurly
	| lcurly blockItemList rcurly

compoundStatementWithMaybeEnterScope
	::= lcurlyMaybeEnterScope rcurly                  action => deref_and_bless_compoundStatement
	| lcurlyMaybeEnterScope blockItemList rcurly      action => deref_and_bless_compoundStatement

blockItemList
	::= blockItem
	| blockItemList blockItem

blockItem
	::= declaration
	| statement

expressionStatement
	::= SEMICOLON
	| expression SEMICOLON

selectionStatement
	::= IF lparen expression rparen statement ELSE statement
	| IF lparen expression rparen statement rank => 1
	| SWITCH lparen expression rparen statement

iterationStatement
	::= WHILE lparen expression rparen statement
	| DO statement WHILE lparen expression rparen SEMICOLON
	| FOR lparen expressionStatement expressionStatement rparen statement
	| FOR lparen expressionStatement expressionStatement expression rparen statement
	| FOR lparen declaration expressionStatement rparen statement
	| FOR lparen declaration expressionStatement expression rparen statement

jumpStatement
	::= GOTO IDENTIFIER SEMICOLON
	| CONTINUE SEMICOLON
	| BREAK SEMICOLON
	| RETURN SEMICOLON
	| RETURN expression SEMICOLON

translationUnit
	::= externalDeclaration
	| translationUnit externalDeclaration

externalDeclaration
	::= functionDefinition
	| declaration

fileScopeDeclarator ::= declarator            action => deref_and_bless_declarator

event 'reenterScope[]' = nulled <reenterScope>
reenterScope ::=

event '^functionDefinition' = predicted <functionDefinition>
functionDefinition
	::= declarationSpecifiers fileScopeDeclarator (<reenterScope>) declarationList compoundStatementWithMaybeEnterScope
	| declarationSpecifiers fileScopeDeclarator (<reenterScope>) compoundStatementWithMaybeEnterScope

declarationList
	::= declaration
	| declarationList declaration

#
# G0 (tokens), c.f. http://www.quut.com/c/ANSI-C-grammar-l.html
#

# Intermediary tokens

O          ~ [0-7]
O_any      ~ O*
D          ~ [0-9]
D_any      ~ D*
D_many     ~ D+
NZ         ~ [1-9]
L          ~ [a-zA-Z_]
A          ~ [a-zA-Z_0-9]
A_any      ~ A*
H          ~ [a-fA-F0-9]
H_any      ~ H*
H_many     ~ H+
HP         ~ '0' [xX]
SIGN_maybe ~ [+-]
SIGN_maybe ~
E          ~ [Ee] SIGN_maybe D_many
E_maybe    ~ E
E_maybe    ~
P          ~ [Pp] SIGN_maybe D_many
FS         ~ [fFlL]
FS_maybe   ~ FS
FS_maybe   ~
LL         ~ 'll' | 'LL' | [lL]
LL_maybe   ~ LL
LL_maybe   ~
U          ~ [uU]
U_maybe    ~ U
U_maybe    ~
IS         ~ U LL_maybe | LL U_maybe
IS_maybe   ~ IS
IS_maybe   ~
CP         ~ [uUL]
CP_maybe   ~ CP
CP_maybe   ~
SP         ~ 'u8' | [uUL]
SP_maybe   ~ SP
SP_maybe   ~
ES_AFTERBS ~ [\'\"\?\\abfnrtv]
           | O
           | O O
           | O O O
           | 'x' H_many
ES         ~ BS ES_AFTERBS
WS         ~ [ \t\v\n\f]
WS_any     ~ WS*
# Lexemes

:lexeme ~ <AUTO>          priority => -1
AUTO          ~ 'auto'
:lexeme ~ <BREAK>         priority => -2
BREAK         ~ 'break'
:lexeme ~ <CASE>          priority => -3
CASE          ~ 'case'
:lexeme ~ <CHAR>          priority => -4
CHAR          ~ 'char'
:lexeme ~ <CONST>         priority => -5
CONST         ~ 'const'
:lexeme ~ <CONTINUE>      priority => -6
CONTINUE      ~ 'continue'
:lexeme ~ <DEFAULT>       priority => -7
DEFAULT       ~ 'default'
:lexeme ~ <DO>            priority => -8
DO            ~ 'do'
:lexeme ~ <DOUBLE>        priority => -9
DOUBLE        ~ 'double'
:lexeme ~ <ELSE>          priority => -10
ELSE          ~ 'else'
:lexeme ~ <ENUM>          priority => -11
ENUM          ~ 'enum'
:lexeme ~ <EXTERN>        priority => -12
EXTERN        ~ 'extern'
:lexeme ~ <FLOAT>         priority => -13
FLOAT         ~ 'float'
:lexeme ~ <FOR>           priority => -14
FOR           ~ 'for'
:lexeme ~ <GOTO>          priority => -15
GOTO          ~ 'goto'
:lexeme ~ <IF>            priority => -16
IF            ~ 'if'
:lexeme ~ <INLINE>        priority => -17
INLINE        ~ 'inline'
:lexeme ~ <INT>           priority => -18
INT           ~ 'int'
:lexeme ~ <LONG>          priority => -19
LONG          ~ 'long'
:lexeme ~ <REGISTER>      priority => -20
REGISTER      ~ 'register'
:lexeme ~ <RESTRICT>      priority => -21
RESTRICT      ~ 'restrict'
:lexeme ~ <RETURN>        priority => -22
RETURN        ~ 'return'
:lexeme ~ <SHORT>         priority => -23
SHORT         ~ 'short'
:lexeme ~ <SIGNED>        priority => -24
SIGNED        ~ 'signed'
:lexeme ~ <SIZEOF>        priority => -25
SIZEOF        ~ 'sizeof'
:lexeme ~ <STATIC>        priority => -26
STATIC        ~ 'static'
:lexeme ~ <STRUCT>        priority => -27
STRUCT        ~ 'struct'
:lexeme ~ <SWITCH>        priority => -28
SWITCH        ~ 'switch'
:lexeme ~ <TYPEDEF>       priority => -29
TYPEDEF       ~ 'typedef'
:lexeme ~ <UNION>         priority => -30
UNION         ~ 'union'
:lexeme ~ <UNSIGNED>      priority => -31
UNSIGNED      ~ 'unsigned'
:lexeme ~ <VOID>          priority => -32
VOID          ~ 'void'
:lexeme ~ <VOLATILE>      priority => -33
VOLATILE      ~ 'volatile'
:lexeme ~ <WHILE>         priority => -34
WHILE         ~ 'while'
:lexeme ~ <ALIGNAS>       priority => -35
ALIGNAS       ~ '_Alignas'
:lexeme ~ <ALIGNOF>       priority => -36
ALIGNOF       ~ '_Alignof'
:lexeme ~ <ATOMIC>        priority => -37
ATOMIC        ~ '_Atomic'
:lexeme ~ <BOOL>          priority => -38
BOOL          ~ '_Bool'
:lexeme ~ <COMPLEX>       priority => -39
COMPLEX       ~ '_Complex'
:lexeme ~ <GENERIC>       priority => -40
GENERIC       ~ '_Generic'
:lexeme ~ <IMAGINARY>     priority => -41
IMAGINARY     ~ '_Imaginary'
:lexeme ~ <NORETURN>      priority => -42
NORETURN      ~ '_Noreturn'
:lexeme ~ <STATIC_ASSERT> priority => -43
STATIC_ASSERT ~ '_Static_assert'
:lexeme ~ <THREAD_LOCAL>  priority => -44
THREAD_LOCAL  ~ '_Thread_local'
:lexeme ~ <FUNC_NAME>     priority => -45
FUNC_NAME     ~ '__func__'

#
## DETERMINED AT RUN TIME
#
:lexeme ~ <TYPEDEF_NAME>         priority => -100 pause => before
:lexeme ~ <ENUMERATION_CONSTANT> priority => -100 pause => before
:lexeme ~ <IDENTIFIER>           priority => -100 pause => before
TYPEDEF_NAME         ~ L A_any
ENUMERATION_CONSTANT ~ L A_any
IDENTIFIER           ~ L A_any

:lexeme ~ <I_CONSTANT>         priority => -101
I_CONSTANT ~ HP H_many IS_maybe
           | NZ D_any IS_maybe
           | '0' O_any IS_maybe
           | CP_maybe QUOTE I_CONSTANT_INSIDE_many QUOTE

:lexeme ~ <F_CONSTANT>         priority => -102
F_CONSTANT ~ D_many E FS_maybe
           | D_any '.' D_many E_maybe FS_maybe
           | D_many '.' E_maybe FS_maybe
           | HP H_many P FS_maybe
           | HP H_any '.' H_many P FS_maybe
           | HP H_many '.' P FS_maybe

:lexeme ~ <STRING_LITERAL>         priority => -103
STRING_LITERAL ~ STRING_LITERAL_UNIT+

:lexeme ~ <ELLIPSIS>      priority => -104
ELLIPSIS     ~ '...'
:lexeme ~ <RIGHT_ASSIGN>  priority => -105
RIGHT_ASSIGN ~ '>>='
:lexeme ~ <LEFT_ASSIGN>   priority => -106
LEFT_ASSIGN  ~ '<<='
:lexeme ~ <ADD_ASSIGN>    priority => -107
ADD_ASSIGN   ~ '+='
:lexeme ~ <SUB_ASSIGN>    priority => -108
SUB_ASSIGN   ~ '-='
:lexeme ~ <MUL_ASSIGN>    priority => -109
MUL_ASSIGN   ~ '*='
:lexeme ~ <DIV_ASSIGN>    priority => -110
DIV_ASSIGN   ~ '/='
:lexeme ~ <MOD_ASSIGN>    priority => -111
MOD_ASSIGN   ~ '%='
:lexeme ~ <AND_ASSIGN>    priority => -112
AND_ASSIGN   ~ '&='
:lexeme ~ <XOR_ASSIGN>    priority => -113
XOR_ASSIGN   ~ '^='
:lexeme ~ <OR_ASSIGN>     priority => -114
OR_ASSIGN    ~ '|='
:lexeme ~ <RIGHT_OP>      priority => -115
RIGHT_OP     ~ '>>'
:lexeme ~ <LEFT_OP>       priority => -116
LEFT_OP      ~ '<<'
:lexeme ~ <INC_OP>        priority => -117
INC_OP       ~ '++'
:lexeme ~ <DEC_OP>        priority => -118
DEC_OP       ~ '--'
:lexeme ~ <PTR_OP>        priority => -119
PTR_OP       ~ '->'
:lexeme ~ <AND_OP>        priority => -120
AND_OP       ~ '&&'
:lexeme ~ <OR_OP>         priority => -121
OR_OP        ~ '||'
:lexeme ~ <LE_OP>         priority => -122
LE_OP        ~ '<='
:lexeme ~ <GE_OP>         priority => -123
GE_OP        ~ '>='
:lexeme ~ <EQ_OP>         priority => -124
EQ_OP        ~ '=='
:lexeme ~ <NE_OP>         priority => -125
NE_OP        ~ '!='
:lexeme ~ <SEMICOLON>     priority => -126
SEMICOLON                     ~ ';'
:lexeme ~ <LCURLY>        priority => -127
LCURLY                       ~ '{' | '<%'
:lexeme ~ <RCURLY>        priority => -128
RCURLY                       ~ '}' | '%>'
:lexeme ~ <COMMA>         priority => -129
COMMA                     ~ ','
:lexeme ~ <COLON>         priority => -130
COLON                      ~ ':'
:lexeme ~ <EQUAL>         priority => -131
EQUAL       ~ '='
:lexeme ~ <LPAREN>        priority => -132
LPAREN                ~ '('
:lexeme ~ <RPAREN>        priority => -133
RPAREN                      ~ ')'
:lexeme ~ <LBRACKET>      priority => -134
LBRACKET      ~ '[' | '<:'
:lexeme ~ <RBRACKET>      priority => -135
RBRACKET      ~ ']' | ':>'
:lexeme ~ <DOT>           priority => -136
DOT           ~ '.'
:lexeme ~ <AMPERSAND>     priority => -137
AMPERSAND     ~ '&'
:lexeme ~ <EXCLAMATION>   priority => -138
EXCLAMATION ~ '!'
:lexeme ~ <TILDE>         priority => -139
TILDE ~ '~'
:lexeme ~ <HYPHEN>        priority => -140
HYPHEN ~ '-'
:lexeme ~ <PLUS>          priority => -141
PLUS ~ '+'
:lexeme ~ <STAR>          priority => -142
STAR ~ '*'
:lexeme ~ <SLASH>         priority => -143
SLASH ~ '/'
:lexeme ~ <PERCENT>       priority => -144
PERCENT ~ '%'
:lexeme ~ <LESS_THAN>     priority => -145
LESS_THAN ~ '<'
:lexeme ~ <GREATER_THAN>  priority => -146
GREATER_THAN ~ '>'
:lexeme ~ <CARET>         priority => -147
CARET ~ '^'
:lexeme ~ <VERTICAL_BAR>  priority => -148
VERTICAL_BAR ~ '|'
:lexeme ~ <QUESTION_MARK> priority => -149
QUESTION_MARK ~ '?'

#
# Discard of a C comment, c.f. https://gist.github.com/jeffreykegler/5015057
#
<C style comment> ~ '/*' <comment interior> '*/'
<comment interior> ~
    <optional non stars>
    <optional star prefixed segments>
    <optional pre final stars>
<optional non stars> ~ [^*]*
<optional star prefixed segments> ~ <star prefixed segment>*
<star prefixed segment> ~ <stars> [^/*] <optional star free text>
<stars> ~ [*]+
<optional star free text> ~ [^*]*
<optional pre final stars> ~ [*]*

#
# Discard of a C++ comment
#
<Cplusplus style comment> ~ '//' <Cplusplus comment interior>
<Cplusplus comment interior> ~ [^\n]*

#
# Internal tokens added
#
QUOTE     ~ [']
I_CONSTANT_INSIDE ~ [^'\\\n]
I_CONSTANT_INSIDE ~ ES
I_CONSTANT_INSIDE_many ~ I_CONSTANT_INSIDE+
STRING_LITERAL_INSIDE ~ [^"\\\n]
STRING_LITERAL_INSIDE ~ ES
STRING_LITERAL_INSIDE_any ~ STRING_LITERAL_INSIDE*
STRING_LITERAL_UNIT ~ SP_maybe '"' STRING_LITERAL_INSIDE_any '"' WS_any
BS         ~ '\'
ANYTHING_ELSE   ~ [.]

:discard ~ <Cplusplus style comment>
:discard ~ <C style comment>
:discard ~ WS            # whitespace separates tokens
:discard ~ ANYTHING_ELSE # discard bad characters
