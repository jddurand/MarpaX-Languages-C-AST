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
    _grammar_option => {action_object  => sprintf('%s::%s', __PACKAGE__, 'Actions')},
    _recce_option => {ranking_method => 'high_rule_only'},
  };
  #
  # Rework the grammar to have a systematic pause on EVERY lexeme.
  # This is needed for scopes.
  # And this will allow to do a tracing for the c2ast command-line -;
  #
  $self->{_content} = '';
  while (defined($_ = <DATA>)) {
      if (/^\s*:lexeme\b/ && ! /\bpause\b/) {
	  substr($_, -1, 1) = " pause => after\n";
      }
      $self->{_content} .= $_;
  }

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
lexeme default = action => [start,length,value]

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
	| LPAREN expression RPAREN
	| genericSelection
        | gccStatementExpression


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
	::= GENERIC LPAREN assignmentExpression COMMA genericAssocList RPAREN

genericAssocList
	::= genericAssociation
	| genericAssocList COMMA genericAssociation

genericAssociation
	::= typeName COLON assignmentExpression
	| DEFAULT COLON assignmentExpression

postfixExpression
	::= primaryExpression
	| postfixExpression LBRACKET expression RBRACKET
	| postfixExpression LPAREN RPAREN
	| postfixExpression LPAREN argumentExpressionList RPAREN
        | gccBuiltinVaStart
        | gccBuiltinVaEnd
        | gccBuiltinVaArg
        | gccBuiltinOffsetof
	| postfixExpression DOT IDENTIFIER
	| postfixExpression PTR_OP IDENTIFIER
	| postfixExpression INC_OP
	| postfixExpression DEC_OP
	| LPAREN typeName RPAREN LCURLY initializerList RCURLY
	| LPAREN typeName RPAREN LCURLY initializerList COMMA RCURLY

argumentExpressionList
	::= assignmentExpression
	| argumentExpressionList COMMA assignmentExpression
	| argumentExpressionList COMMA

gccAlignofExpression ::= GCC_ALIGNOF unaryExpression
                       | GCC_ALIGNOF LPAREN typeName RPAREN

unaryExpression
	::= postfixExpression
	| INC_OP unaryExpression
	| DEC_OP unaryExpression
	| unaryOperator castExpression
	| SIZEOF unaryExpression
	| SIZEOF LPAREN typeName RPAREN
	| ALIGNOF LPAREN typeName RPAREN
        | gccAlignofExpression
        | gccExtensionSpecifier castExpression

unaryOperator
	::= AMPERSAND
	| STAR
	| PLUS
	| HYPHEN
	| TILDE
	| EXCLAMATION

castExpression
	::= unaryExpression
	| LPAREN typeName RPAREN castExpression

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
	| logicalOrExpression QUESTION_MARK COLON conditionalExpression          # GCC Extension

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
event 'declarationCheckdeclarationSpecifiers$' = completed <declarationCheckdeclarationSpecifiers>
declarationCheckdeclarationSpecifiers ::= declarationSpecifiers action => deref

event 'declarationCheckinitDeclaratorList$' = completed <declarationCheckinitDeclaratorList>
declarationCheckinitDeclaratorList    ::= initDeclaratorList    action => deref

event '^declarationCheck' = predicted <declarationCheck>
event 'declarationCheck$' = completed <declarationCheck>
declarationCheck ::= declarationCheckdeclarationSpecifiers declarationCheckinitDeclaratorList SEMICOLON action => deref

declaration
	::= declarationSpecifiers SEMICOLON
	| declarationCheck
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
	| gccDeclarationSpecifier declarationSpecifiers
	| gccDeclarationSpecifier
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
        | msvsDeclspec

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
	| LABEL
	| COMPLEX
	| IMAGINARY	  	# non-mandated extension
	| atomicTypeSpecifier
	| structOrUnionSpecifier
	| enumSpecifier
	| TYPEDEF_NAME		# after it has been defined as such
        | msvsBuiltinType
        | gccBuiltinType

event 'structContextStart[]' = nulled <structContextStart>
structContextStart ::=

event 'structContextEnd[]' = nulled <structContextEnd>
structContextEnd ::=

structOrUnionSpecifier
	::= structOrUnion gccAttributeList LCURLY <structContextStart> structDeclarationList RCURLY <structContextEnd>
	| structOrUnion gccAttributeList IDENTIFIER LCURLY <structContextStart> structDeclarationList RCURLY <structContextEnd>
	| structOrUnion gccAttributeList IDENTIFIER

structOrUnion
	::= STRUCT
	| UNION

structDeclarationList
	::= structDeclaration
	| structDeclarationList structDeclaration

structDeclaration
	::= specifierQualifierList SEMICOLON	# for anonymous struct/union
	| specifierQualifierList structDeclaratorList SEMICOLON
	| SEMICOLON                             # GCC extension
        | preprocessorDirective

specifierQualifierList
	::= typeSpecifier specifierQualifierList
	| typeSpecifier
	| typeQualifier specifierQualifierList
	| typeQualifier
	| gccDeclarationSpecifier specifierQualifierList
	| gccDeclarationSpecifier

structDeclaratorList
	::= structDeclarator
	| structDeclaratorList COMMA structDeclarator

structDeclarator
	::= COLON constantExpression
	| declarator COLON constantExpression gccAttributeList
	| declarator

enumSpecifier
	::= ENUM gccAttributeList LCURLY enumeratorList RCURLY
	| ENUM gccAttributeList LCURLY enumeratorList COMMA RCURLY
	| ENUM gccAttributeList IDENTIFIER LCURLY enumeratorList RCURLY
	| ENUM gccAttributeList IDENTIFIER LCURLY enumeratorList COMMA RCURLY
	| ENUM gccAttributeList IDENTIFIER

enumeratorList
	::= enumerator
	| enumeratorList COMMA enumerator

enumerator	# identifiers must be flagged as ENUMERATION_CONSTANT
	::= enumerationConstant EQUAL constantExpression
	| enumerationConstant

atomicTypeSpecifier
	::= ATOMIC LPAREN typeName RPAREN

typeQualifier
	::= CONST
	| RESTRICT
	| VOLATILE
	| ATOMIC
	| MSVS_W64
	| MSVS_PTR32
	| MSVS_PTR64

functionSpecifier
	::= INLINE
	| NORETURN
        | msvsFunctionSpecifier

alignmentSpecifier
	::= ALIGNAS LPAREN typeName RPAREN
	| ALIGNAS LPAREN constantExpression RPAREN

msvsAttributeList ::= msvsAttribute*

declarator
	::= pointer msvsAttributeList directDeclarator gccAsmExpressionMaybe gccAttributeList
	| msvsAttributeList directDeclarator gccAsmExpressionMaybe gccAttributeList

event 'directDeclaratorIdentifier$' = completed <directDeclaratorIdentifier>
directDeclaratorIdentifier
	::= IDENTIFIER          action => deref

gccAttributeList ::= gccAttribute*
directDeclarator
	::= directDeclaratorIdentifier
	| LPAREN gccAttributeList declarator RPAREN
	| directDeclarator LBRACKET RBRACKET
	| directDeclarator LBRACKET STAR RBRACKET
	| directDeclarator LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET
	| directDeclarator LBRACKET STATIC assignmentExpression RBRACKET
	| directDeclarator LBRACKET gccArrayTypeModifierList STAR RBRACKET
	| directDeclarator LBRACKET gccArrayTypeModifierList STATIC assignmentExpression RBRACKET
	| directDeclarator LBRACKET gccArrayTypeModifierList assignmentExpression RBRACKET
	| directDeclarator LBRACKET gccArrayTypeModifierList RBRACKET
	| directDeclarator LBRACKET assignmentExpression RBRACKET
	| directDeclarator LPAREN_SCOPE parameterTypeList RPAREN_SCOPE
	| directDeclarator LPAREN_SCOPE RPAREN_SCOPE
	| directDeclarator LPAREN_SCOPE identifierList RPAREN_SCOPE

pointerQualifier ::= typeQualifier | gccAttribute

pointerQualifierList ::= pointerQualifier
                       | pointerQualifierList pointerQualifier
pointer
	::= msvsAttributeList STAR pointerQualifierList pointer
	| msvsAttributeList STAR pointerQualifierList
	| msvsAttributeList STAR pointer
	| msvsAttributeList STAR

gccArrayTypeModifierList ::= gccArrayTypeModifier
                           | gccArrayTypeModifierList gccArrayTypeModifier
#typeQualifierList
#	::= typeQualifier
#	| typeQualifierList typeQualifier

parameterTypeList
	::= parameterList COMMA ELLIPSIS
	| parameterList

parameterList
	::= parameterDeclaration
	| parameterList COMMA parameterDeclaration

event 'parameterDeclarationdeclarationSpecifiers$' = completed <parameterDeclarationdeclarationSpecifiers>
parameterDeclarationdeclarationSpecifiers ::= declarationSpecifiers action => deref

event '^parameterDeclarationCheck' = predicted <parameterDeclarationCheck>
event 'parameterDeclarationCheck$' = completed <parameterDeclarationCheck>
parameterDeclarationCheck ::= parameterDeclarationdeclarationSpecifiers declarator action => deref

parameterDeclaration
	::= parameterDeclarationCheck
	| declarationSpecifiers abstractDeclarator
	| declarationSpecifiers

identifierList
	::= IDENTIFIER
	| identifierList COMMA IDENTIFIER

typeName
	::= specifierQualifierList abstractDeclarator
	| specifierQualifierList

gccAsmExpressionMaybe ::= gccAsmExpression
                        | gccEmptyRule

abstractDeclarator
	::= pointer msvsAttributeList directAbstractDeclarator gccAsmExpressionMaybe gccAttributeList
	| pointer msvsAttributeList
	| directAbstractDeclarator gccAsmExpressionMaybe gccAttributeList

directAbstractDeclarator
	::= LPAREN gccAttributeList abstractDeclarator RPAREN
	| LBRACKET RBRACKET
	| LBRACKET STAR RBRACKET
	| LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET
	| LBRACKET STATIC assignmentExpression RBRACKET
	| LBRACKET gccArrayTypeModifierList STATIC assignmentExpression RBRACKET
	| LBRACKET gccArrayTypeModifierList assignmentExpression RBRACKET
	| LBRACKET gccArrayTypeModifierList RBRACKET
	| LBRACKET assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET RBRACKET
	| directAbstractDeclarator LBRACKET STAR RBRACKET
	| directAbstractDeclarator LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET STATIC assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET gccArrayTypeModifierList assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET gccArrayTypeModifierList STATIC assignmentExpression RBRACKET
	| directAbstractDeclarator LBRACKET gccArrayTypeModifierList RBRACKET
	| directAbstractDeclarator LBRACKET assignmentExpression RBRACKET
	| LPAREN_SCOPE RPAREN_SCOPE
	| LPAREN_SCOPE parameterTypeList RPAREN_SCOPE
	| directAbstractDeclarator LPAREN_SCOPE RPAREN_SCOPE
	| directAbstractDeclarator LPAREN_SCOPE parameterTypeList RPAREN_SCOPE

initializer
	::= LCURLY initializerList RCURLY
	| LCURLY initializerList COMMA RCURLY
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
        | LBRACKET constantExpression ELLIPSIS constantExpression RBRACKET # GCC Extension

staticAssertDeclaration
	::= STATIC_ASSERT LPAREN constantExpression COMMA STRING_LITERAL RPAREN SEMICOLON

statement
	::= labeledStatement
	| compoundStatement
	| expressionStatement
	| selectionStatement
	| iterationStatement
	| jumpStatement
        | msvsAsmStatement
        | gccAsmStatement
        | preprocessorDirective

labeledStatement
	::= IDENTIFIER COLON statement
	| CASE constantExpression COLON statement
	| DEFAULT COLON statement

compoundStatement
	::= LCURLY_SCOPE RCURLY_SCOPE
	| LCURLY_SCOPE blockItemList RCURLY_SCOPE

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
	::= IF LPAREN expression RPAREN statement ELSE statement
	| IF LPAREN expression RPAREN statement rank => 1
	| SWITCH LPAREN expression RPAREN statement

iterationStatement
	::= WHILE LPAREN expression RPAREN statement
	| DO statement WHILE LPAREN expression RPAREN SEMICOLON
	| FOR LPAREN expressionStatement expressionStatement RPAREN statement
	| FOR LPAREN expressionStatement expressionStatement expression RPAREN statement
	| FOR LPAREN declaration expressionStatement RPAREN statement
	| FOR LPAREN declaration expressionStatement expression RPAREN statement

jumpStatement
	::= GOTO IDENTIFIER SEMICOLON
	| CONTINUE SEMICOLON
	| BREAK SEMICOLON
	| RETURN SEMICOLON
	| RETURN expression SEMICOLON

event 'translationUnit$' = completed <translationUnit>
translationUnit
	::= externalDeclaration
	| translationUnit externalDeclaration

event '^externalDeclaration' = predicted <externalDeclaration>
externalDeclaration
	::= functionDefinition
	| declaration
        | preprocessorDirective

compoundStatementReenterScope ::= LCURLY RCURLY_SCOPE                   action => deref_and_bless_compoundStatement
	                        | LCURLY blockItemList RCURLY_SCOPE     action => deref_and_bless_compoundStatement

functionDefinition
	::= functionDefinitionCheck1
	| functionDefinitionCheck2

event 'fileScopeDeclarator$' = completed <fileScopeDeclarator>
fileScopeDeclarator ::= declarator action => deref_and_bless_declarator

event '^functionDefinitionCheck1' = predicted <functionDefinitionCheck1>
event 'functionDefinitionCheck1$' = completed <functionDefinitionCheck1>
functionDefinitionCheck1 ::= functionDefinitionCheck1declarationSpecifiers fileScopeDeclarator functionDefinitionCheck1declarationList compoundStatementReenterScope action => deref

event '^functionDefinitionCheck2' = predicted <functionDefinitionCheck2>
event 'functionDefinitionCheck2$' = completed <functionDefinitionCheck2>
functionDefinitionCheck2 ::= functionDefinitionCheck2declarationSpecifiers fileScopeDeclarator                                         compoundStatementReenterScope action => deref

event 'functionDefinitionCheck1declarationSpecifiers$' = completed <functionDefinitionCheck1declarationSpecifiers>
functionDefinitionCheck1declarationSpecifiers ::= declarationSpecifiers action => deref

event 'functionDefinitionCheck2declarationSpecifiers$' = completed <functionDefinitionCheck2declarationSpecifiers>
functionDefinitionCheck2declarationSpecifiers ::= declarationSpecifiers action => deref

event 'functionDefinitionCheck1declarationList$' = completed <functionDefinitionCheck1declarationList>
functionDefinitionCheck1declarationList ::= declarationList action => deref

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
WS_many    ~ WS+

# Lexemes
:lexeme ~ <AUTO>          priority => -1
AUTO          ~ 'auto'
:lexeme ~ <BREAK>         priority => -2
BREAK         ~ 'break'
:lexeme ~ <CASE>          priority => -3
CASE          ~ 'case'
:lexeme ~ <CHAR>          priority => -4
CHAR          ~ 'char'
:lexeme ~ <CONST>         priority => -5 pause => after
CONST         ~ 'const'
CONST         ~ '__const'
CONST         ~ 'const__'
CONST         ~ '__const__'
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
INLINE        ~ '__inline__'
INLINE        ~ 'inline__'
INLINE        ~ '__inline'
:lexeme ~ <INT>           priority => -18
INT           ~ 'int'
:lexeme ~ <LONG>          priority => -19
LONG          ~ 'long'
:lexeme ~ <REGISTER>      priority => -20
REGISTER      ~ 'register'
:lexeme ~ <RESTRICT>      priority => -21
RESTRICT      ~ 'restrict'
RESTRICT      ~ '__restrict'
RESTRICT      ~ 'restrict__'
RESTRICT      ~ '__restrict__'              # gcc
:lexeme ~ <RETURN>        priority => -22
RETURN        ~ 'return'
:lexeme ~ <SHORT>         priority => -23
SHORT         ~ 'short'
:lexeme ~ <SIGNED>        priority => -24
SIGNED        ~ 'signed'
SIGNED        ~ '__signed'
SIGNED        ~ '__signed__'
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
UNSIGNED      ~ '__unsigned'
UNSIGNED      ~ '__unsigned__'
:lexeme ~ <VOID>          priority => -32
VOID          ~ 'void'
:lexeme ~ <VOLATILE>      priority => -33
VOLATILE      ~ 'volatile'
VOLATILE      ~ '__volatile'
VOLATILE      ~ 'volatile__'
VOLATILE      ~ '__volatile__'
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
:lexeme ~ <LABEL>          priority => -38
LABEL          ~ '__label__'
:lexeme ~ <COMPLEX>       priority => -39
COMPLEX       ~ '_Complex'
COMPLEX       ~ '__complex'
COMPLEX       ~ '__complex__'
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
THREAD_LOCAL  ~ '__thread' # gcc
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
:lexeme ~ <LCURLY_SCOPE>        priority => -127
LCURLY_SCOPE                       ~ '{' | '<%'
:lexeme ~ <RCURLY>        priority => -128
RCURLY                       ~ '}' | '%>'
:lexeme ~ <RCURLY_SCOPE>        priority => -128
RCURLY_SCOPE                       ~ '}' | '%>'
:lexeme ~ <COMMA>         priority => -129
COMMA                     ~ ','
:lexeme ~ <COLON>         priority => -130
COLON                      ~ ':'
:lexeme ~ <EQUAL>         priority => -131
EQUAL       ~ '='
:lexeme ~ <LPAREN>        priority => -132
LPAREN                ~ '('
:lexeme ~ <LPAREN_SCOPE>        priority => -132
LPAREN_SCOPE                ~ '('
:lexeme ~ <RPAREN>        priority => -133
RPAREN                      ~ ')'
:lexeme ~ <RPAREN_SCOPE>        priority => -133
RPAREN_SCOPE                      ~ ')'
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
# Discard simple preprocessor directives (on one line - cpp output persist to get some of them)
#
#<Cpp style directive start> ~ '#'
#<Cpp style directive interior single line> ~ [^\n]*
#<Cpp style directive> ~ <Cpp style directive start> <Cpp style directive interior single line>

#
# Discard of some simple annotation directives
#
<MSVS annotation directive start> ~ '[source_annotation_attribute'
<MSVS annotation directive interior single line> ~ [^\n]*
<MSVS annotation directive> ~ <MSVS annotation directive start> <MSVS annotation directive interior single line>

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
#:discard ~ <Cpp style directive>
:discard ~ <MSVS annotation directive>
:discard ~ WS_many       # whitespace separates tokens
:discard ~ ANYTHING_ELSE # discard bad characters

#
# GCC C LEXEMES
#
:lexeme ~ <GCC_ATTRIBUTE>            priority => -60
GCC_ATTRIBUTE        ~ '__attribute__'
GCC_ATTRIBUTE        ~ '__attribute'
:lexeme ~ <GCC_EXTENSION>            priority => -60
GCC_EXTENSION        ~ '__extension__'
GCC_EXTENSION        ~ '__extension'
:lexeme ~ <GCC_ASM>                  priority => -60
GCC_ASM              ~ 'asm__'
GCC_ASM              ~ '__asm'
GCC_ASM              ~ '__asm__'
:lexeme ~ <GCC_BUILTIN_VA_START>     priority => -60
GCC_BUILTIN_VA_START ~ '__builtin_va_start'
:lexeme ~ <GCC_BUILTIN_VA_END>       priority => -60
GCC_BUILTIN_VA_END   ~ '__builtin_va_end'
:lexeme ~ <GCC_BUILTIN_VA_ARG>       priority => -60
GCC_BUILTIN_VA_ARG   ~ '__builtin_va_arg'
:lexeme ~ <GCC_TYPEOF>               priority => -60
GCC_TYPEOF           ~ 'typeof'
GCC_TYPEOF           ~ '__typeof'
GCC_TYPEOF           ~ '__typeof__'
:lexeme ~ <GCC_BUILTIN_OFFSETOF>     priority => -60
GCC_BUILTIN_OFFSETOF ~ '__builtin_offsetof'
:lexeme ~ <GCC_BUILTIN_VA_LIST>      priority => -60
GCC_BUILTIN_VA_LIST ~ '__builtin_va_list'
:lexeme ~ <GCC_ALIAS>              priority => -60
GCC_ALIAS ~ '__alias__'
GCC_ALIAS ~ 'alias__'
GCC_ALIAS ~ '__alias'
GCC_ALIAS ~ 'alias'
:lexeme ~ <GCC_ALIGNOF>              priority => -60
GCC_ALIGNOF ~ '__alignof__'
GCC_ALIGNOF ~ 'alignof__'
GCC_ALIGNOF ~ '__alignof'
GCC_ALIGNOF ~ 'alignof'
:lexeme ~ <GCC_ALIGNED>              priority => -60
GCC_ALIGNED ~ '__aligned__'
GCC_ALIGNED ~ 'aligned__'
GCC_ALIGNED ~ '__aligned'
GCC_ALIGNED ~ 'aligned'
:lexeme ~ <GCC_NOTHROW>              priority => -60
GCC_NOTHROW ~ '__nothrow__'
GCC_NOTHROW ~ 'nothrow__'
GCC_NOTHROW ~ '__nothrow'
GCC_NOTHROW ~ 'nothrow'
:lexeme ~ <GCC_MAY_ALIAS>            priority => -60
GCC_MAY_ALIAS ~ '__may_alias__'
GCC_MAY_ALIAS ~ 'may_alias__'
GCC_MAY_ALIAS ~ '__may_alias'
GCC_MAY_ALIAS ~ 'may_alias'
:lexeme ~ <GCC_TLS_MODEL>            priority => -60
GCC_TLS_MODEL ~ '__tls_model__'
GCC_TLS_MODEL ~ 'tls_model__'
GCC_TLS_MODEL ~ '__tls_model'
GCC_TLS_MODEL ~ 'tls_model'
:lexeme ~ <GCC_TLS_MODEL_TYPE>       priority => -60
GCC_TLS_MODEL_TYPE ~ 'global-dynamic'
GCC_TLS_MODEL_TYPE ~ 'local-dynamic'
GCC_TLS_MODEL_TYPE ~ 'initial-exec'
GCC_TLS_MODEL_TYPE ~ 'local-exec'
:lexeme ~ <GCC_LEAF>                 priority => -60
GCC_LEAF ~ '__leaf__'
GCC_LEAF ~ 'leaf__'
GCC_LEAF ~ '__leaf'
GCC_LEAF ~ 'leaf'
:lexeme ~ <GCC_NONNULL>              priority => -60
GCC_NONNULL ~ '__nonnull__'
GCC_NONNULL ~ 'nonnull__'
GCC_NONNULL ~ '__nonnull'
GCC_NONNULL ~ 'nonnull'
:lexeme ~ <GCC_CLEANUP>              priority => -60
GCC_CLEANUP ~ '__cleanup__'
GCC_CLEANUP ~ 'cleanup__'
GCC_CLEANUP ~ '__cleanup'
GCC_CLEANUP ~ 'cleanup'
:lexeme ~ <GCC_NEAR>                 priority => -60
GCC_NEAR ~ '__near__'
GCC_NEAR ~ '__near'
GCC_NEAR ~ 'near__'
GCC_NEAR ~ 'near'
:lexeme ~ <GCC_DLLEXPORT>                 priority => -60
GCC_DLLEXPORT ~ '__dllexport__'
GCC_DLLEXPORT ~ '__dllexport'
GCC_DLLEXPORT ~ 'dllexport__'
GCC_DLLEXPORT ~ 'dllexport'
:lexeme ~ <GCC_USED>                 priority => -60
GCC_USED ~ '__used__'
GCC_USED ~ '__used'
GCC_USED ~ 'used__'
GCC_USED ~ 'used'
:lexeme ~ <GCC_FAR>                  priority => -60
GCC_FAR ~ '__far__'
GCC_FAR ~ '__far'
GCC_FAR ~ 'far__'
GCC_FAR ~ 'far'
:lexeme ~ <GCC_VISIBILITY>           priority => -60
GCC_VISIBILITY ~ '__visibility__'
GCC_VISIBILITY ~ '__visibility'
GCC_VISIBILITY ~ 'visibility__'
GCC_VISIBILITY ~ 'visibility'
:lexeme ~ <GCC_WEAK>           priority => -60
GCC_WEAK ~ '__weak__'
GCC_WEAK ~ '__weak'
GCC_WEAK ~ 'weak__'
GCC_WEAK ~ 'weak'
:lexeme ~ <GCC_ALWAYS_INLINE>           priority => -60
GCC_ALWAYS_INLINE ~ '__always_inline__'
GCC_ALWAYS_INLINE ~ '__always_inline'
GCC_ALWAYS_INLINE ~ 'always_inline__'
GCC_ALWAYS_INLINE ~ 'always_inline'
:lexeme ~ <GCC_NOINLINE>           priority => -60
GCC_NOINLINE ~ '__noinline__'
GCC_NOINLINE ~ '__noinline'
GCC_NOINLINE ~ 'noinline__'
GCC_NOINLINE ~ 'noinline'
:lexeme ~ <GCC_VECTOR_SIZE>           priority => -60
GCC_VECTOR_SIZE ~ '__vector_size__'
GCC_VECTOR_SIZE ~ '__vector_size'
GCC_VECTOR_SIZE ~ 'vector_size__'
GCC_VECTOR_SIZE ~ 'vector_size'
:lexeme ~ <GCC_DEPRECATED>           priority => -60
GCC_DEPRECATED ~ '__deprecated__'
GCC_DEPRECATED ~ '__deprecated'
GCC_DEPRECATED ~ 'deprecated__'
GCC_DEPRECATED ~ 'deprecated'
:lexeme ~ <GCC_NO_CHECK_MEMORY_USAGE>           priority => -60
GCC_NO_CHECK_MEMORY_USAGE ~ '__no_check_memory_usage__'
GCC_NO_CHECK_MEMORY_USAGE ~ '__no_check_memory_usage'
GCC_NO_CHECK_MEMORY_USAGE ~ 'no_check_memory_usage__'
GCC_NO_CHECK_MEMORY_USAGE ~ 'no_check_memory_usage'
:lexeme ~ <GCC_REGPARM>           priority => -60
GCC_REGPARM ~ '__regparm__'
GCC_REGPARM ~ '__regparm'
GCC_REGPARM ~ 'regparm__'
GCC_REGPARM ~ 'regparm'
:lexeme ~ <GCC_STDCALL>           priority => -60
GCC_STDCALL ~ '__stdcall__'
GCC_STDCALL ~ '__stdcall'
GCC_STDCALL ~ 'stdcall__'
GCC_STDCALL ~ 'stdcall'
:lexeme ~ <GCC_CDECL>           priority => -60
GCC_CDECL ~ '__cdecl__'
GCC_CDECL ~ '__cdecl'
GCC_CDECL ~ 'cdecl__'
GCC_CDECL ~ 'cdecl'
:lexeme ~ <GCC_LONG_CALL>           priority => -60
GCC_LONG_CALL ~ '__long_call__'
GCC_LONG_CALL ~ '__long_call'
GCC_LONG_CALL ~ 'long_call__'
GCC_LONG_CALL ~ 'long_call'
:lexeme ~ <GCC_LONGCALL>           priority => -60
GCC_LONGCALL ~ '__longcall__'
GCC_LONGCALL ~ '__longcall'
GCC_LONGCALL ~ 'longcall__'
GCC_LONGCALL ~ 'longcall'
:lexeme ~ <GCC_SHORT_CALL>           priority => -60
GCC_SHORT_CALL ~ '__short_call__'
GCC_SHORT_CALL ~ '__short_call'
GCC_SHORT_CALL ~ 'short_call__'
GCC_SHORT_CALL ~ 'short_call'
:lexeme ~ <GCC_DLLIMPORT>           priority => -60
GCC_DLLIMPORT ~ '__dllimport__'
GCC_DLLIMPORT ~ '__dllimport'
GCC_DLLIMPORT ~ 'dllimport__'
GCC_DLLIMPORT ~ 'dllimport'
:lexeme ~ <GCC_EXCEPTION>           priority => -60
GCC_EXCEPTION ~ '__exception__'
GCC_EXCEPTION ~ '__exception'
GCC_EXCEPTION ~ 'exception__'
GCC_EXCEPTION ~ 'exception'
:lexeme ~ <GCC_FUNCTION_VECTOR>           priority => -60
GCC_FUNCTION_VECTOR ~ '__function_vector__'
GCC_FUNCTION_VECTOR ~ '__function_vector'
GCC_FUNCTION_VECTOR ~ 'function_vector__'
GCC_FUNCTION_VECTOR ~ 'function_vector'
:lexeme ~ <GCC_INTERRUPT>           priority => -60
GCC_INTERRUPT ~ '__interrupt__'
GCC_INTERRUPT ~ '__interrupt'
GCC_INTERRUPT ~ 'interrupt__'
GCC_INTERRUPT ~ 'interrupt'
:lexeme ~ <GCC_INTERRUPT_HANDLER>           priority => -60
GCC_INTERRUPT_HANDLER ~ '__interrupt_handler__'
GCC_INTERRUPT_HANDLER ~ '__interrupt_handler'
GCC_INTERRUPT_HANDLER ~ 'interrupt_handler__'
GCC_INTERRUPT_HANDLER ~ 'interrupt_handler'
:lexeme ~ <GCC_SP_SWITCH>           priority => -60
GCC_SP_SWITCH ~ '__sp_switch__'
GCC_SP_SWITCH ~ '__sp_switch'
GCC_SP_SWITCH ~ 'sp_switch__'
GCC_SP_SWITCH ~ 'sp_switch'
:lexeme ~ <GCC_TRAP_EXIT>           priority => -60
GCC_TRAP_EXIT ~ '__trap_exit__'
GCC_TRAP_EXIT ~ '__trap_exit'
GCC_TRAP_EXIT ~ 'trap_exit__'
GCC_TRAP_EXIT ~ 'trap_exit'
:lexeme ~ <GCC_EIGHTBIT_DATA>           priority => -60
GCC_EIGHTBIT_DATA ~ '__eightbit_data__'
GCC_EIGHTBIT_DATA ~ '__eightbit_data'
GCC_EIGHTBIT_DATA ~ 'eightbit_data__'
GCC_EIGHTBIT_DATA ~ 'eightbit_data'
:lexeme ~ <GCC_FORMAT>           priority => -60
GCC_FORMAT ~ '__format__'
GCC_FORMAT ~ '__format'
GCC_FORMAT ~ 'format__'
GCC_FORMAT ~ 'format'
:lexeme ~ <GCC_FORMAT_ARG>           priority => -60
GCC_FORMAT_ARG ~ '__format_arg__'
GCC_FORMAT_ARG ~ '__format_arg'
GCC_FORMAT_ARG ~ 'format_arg__'
GCC_FORMAT_ARG ~ 'format_arg'
:lexeme ~ <GCC_TINY_DATA>           priority => -60
GCC_TINY_DATA ~ '__tiny_data__'
GCC_TINY_DATA ~ '__tiny_data'
GCC_TINY_DATA ~ 'tiny_data__'
GCC_TINY_DATA ~ 'tiny_data'
:lexeme ~ <GCC_SIGNAL>           priority => -60
GCC_SIGNAL ~ '__signal__'
GCC_SIGNAL ~ '__signal'
GCC_SIGNAL ~ 'signal__'
GCC_SIGNAL ~ 'signal'
:lexeme ~ <GCC_NAKED>           priority => -60
GCC_NAKED ~ '__naked__'
GCC_NAKED ~ '__naked'
GCC_NAKED ~ 'naked__'
GCC_NAKED ~ 'naked'
:lexeme ~ <GCC_MODEL>           priority => -60
GCC_MODEL ~ '__model__'
GCC_MODEL ~ '__model'
GCC_MODEL ~ 'model__'
GCC_MODEL ~ 'model'
:lexeme ~ <GCC_MODEL_NAME>           priority => -60
GCC_MODEL_NAME ~ '__small__'
GCC_MODEL_NAME ~ '__small'
GCC_MODEL_NAME ~ 'small__'
GCC_MODEL_NAME ~ 'small'
GCC_MODEL_NAME ~ '__medium__'
GCC_MODEL_NAME ~ '__medium'
GCC_MODEL_NAME ~ 'medium__'
GCC_MODEL_NAME ~ 'medium'
GCC_MODEL_NAME ~ '__large__'
GCC_MODEL_NAME ~ '__large'
GCC_MODEL_NAME ~ 'large__'
GCC_MODEL_NAME ~ 'large'
:lexeme ~ <GCC_NOCOMMON>           priority => -60
GCC_NOCOMMON ~ '__nocommon__'
GCC_NOCOMMON ~ '__nocommon'
GCC_NOCOMMON ~ 'nocommon__'
GCC_NOCOMMON ~ 'nocommon'
:lexeme ~ <GCC_SHARED>           priority => -60
GCC_SHARED ~ '__shared__'
GCC_SHARED ~ '__shared'
GCC_SHARED ~ 'shared__'
GCC_SHARED ~ 'shared'
:lexeme ~ <GCC_MALLOC>           priority => -60
GCC_MALLOC ~ '__malloc__'
GCC_MALLOC ~ '__malloc'
GCC_MALLOC ~ 'malloc__'
GCC_MALLOC ~ 'malloc'
:lexeme ~ <GCC_FORMAT_ARCHETYPE>           priority => -60
GCC_FORMAT_ARCHETYPE ~ '__printf__'
GCC_FORMAT_ARCHETYPE ~ '__printf'
GCC_FORMAT_ARCHETYPE ~ 'printf__'
GCC_FORMAT_ARCHETYPE ~ 'printf'
GCC_FORMAT_ARCHETYPE ~ '__scanf__'
GCC_FORMAT_ARCHETYPE ~ '__scanf'
GCC_FORMAT_ARCHETYPE ~ 'scanf__'
GCC_FORMAT_ARCHETYPE ~ 'scanf'
GCC_FORMAT_ARCHETYPE ~ '__strftime__'
GCC_FORMAT_ARCHETYPE ~ '__strftime'
GCC_FORMAT_ARCHETYPE ~ 'strftime__'
GCC_FORMAT_ARCHETYPE ~ 'strftime'
GCC_FORMAT_ARCHETYPE ~ '__strfmon__'
GCC_FORMAT_ARCHETYPE ~ '__strfmon'
GCC_FORMAT_ARCHETYPE ~ 'strfmon__'
GCC_FORMAT_ARCHETYPE ~ 'strfmon'
:lexeme ~ <GCC_NORETURN>           priority => -60
GCC_NORETURN ~ '__noreturn__'
GCC_NORETURN ~ '__noreturn'
GCC_NORETURN ~ 'noreturn__'
GCC_NORETURN ~ 'noreturn'
:lexeme ~ <GCC_PURE>           priority => -60
GCC_PURE ~ '__pure__'
GCC_PURE ~ '__pure'
GCC_PURE ~ 'pure__'
GCC_PURE ~ 'pure'
:lexeme ~ <GCC_TRANSPARENT_UNION>           priority => -60
GCC_TRANSPARENT_UNION ~ '__transparent_union__'
GCC_TRANSPARENT_UNION ~ '__transparent_union'
GCC_TRANSPARENT_UNION ~ 'transparent_union__'
GCC_TRANSPARENT_UNION ~ 'transparent_union'
:lexeme ~ <GCC_UNUSED>           priority => -60
GCC_UNUSED ~ '__unused__'
GCC_UNUSED ~ '__unused'
GCC_UNUSED ~ 'unused__'
GCC_UNUSED ~ 'unused'
:lexeme ~ <GCC_PACKED>           priority => -60
GCC_PACKED ~ '__packed__'
GCC_PACKED ~ '__packed'
GCC_PACKED ~ 'packed__'
GCC_PACKED ~ 'packed'
:lexeme ~ <GCC_NO_INSTRUMENT_FUNCTION>           priority => -60
GCC_NO_INSTRUMENT_FUNCTION ~ '__no_instrument_function__'
GCC_NO_INSTRUMENT_FUNCTION ~ '__no_instrument_function'
GCC_NO_INSTRUMENT_FUNCTION ~ 'no_instrument_function__'
GCC_NO_INSTRUMENT_FUNCTION ~ 'no_instrument_function'
:lexeme ~ <GCC_SECTION>           priority => -60
GCC_SECTION ~ '__section__'
GCC_SECTION ~ '__section'
GCC_SECTION ~ 'section__'
GCC_SECTION ~ 'section'
:lexeme ~ <GCC_CONSTRUCTOR>           priority => -60
GCC_CONSTRUCTOR ~ '__constructor__'
GCC_CONSTRUCTOR ~ '__constructor'
GCC_CONSTRUCTOR ~ 'constructor__'
GCC_CONSTRUCTOR ~ 'constructor'
:lexeme ~ <GCC_DESTRUCTOR>           priority => -60
GCC_DESTRUCTOR ~ '__destructor__'
GCC_DESTRUCTOR ~ '__destructor'
GCC_DESTRUCTOR ~ 'destructor__'
GCC_DESTRUCTOR ~ 'destructor'
:lexeme ~ <GCC_VISIBILITY_TYPE>      priority => -60
GCC_VISIBILITY_TYPE ~ 'default'
GCC_VISIBILITY_TYPE ~ 'hidden'
GCC_VISIBILITY_TYPE ~ 'protected'
GCC_VISIBILITY_TYPE ~ 'internal'

#
# MSVS C LEXEMES
#
:lexeme ~ <MSVS_ASM>                 priority => -60
MSVS_ASM ~ '__asm'
:lexeme ~ <MSVS_FASTCALL>            priority => -60
MSVS_FASTCALL ~ '__fastcall'
:lexeme ~ <MSVS_BASED>               priority => -60
MSVS_BASED ~ '__based'
:lexeme ~ <MSVS_CDECL>               priority => -60
MSVS_CDECL ~ '__cdecl'
:lexeme ~ <MSVS_STDCALL>             priority => -60
MSVS_STDCALL ~ '__stdcall'
:lexeme ~ <MSVS_INT8>                priority => -60
MSVS_INT8 ~ '__int8'
:lexeme ~ <MSVS_INT16>               priority => -60
MSVS_INT16 ~ '__int16'
:lexeme ~ <MSVS_INT32>               priority => -60
MSVS_INT32 ~ '__int32'
:lexeme ~ <MSVS_INT64>               priority => -60
MSVS_INT64 ~ '__int64'
:lexeme ~ <MSVS_DECLSPEC>            priority => -60
MSVS_DECLSPEC ~ '__declspec'
:lexeme ~ <MSVS_ALLOCATE>            priority => -60
MSVS_ALLOCATE ~ 'allocate'
:lexeme ~ <MSVS_DLLIMPORT>           priority => -60
MSVS_DLLIMPORT ~ 'dllimport'
:lexeme ~ <MSVS_DLLEXPORT>           priority => -60
MSVS_DLLEXPORT ~ 'dllexport'
:lexeme ~ <MSVS_NAKED>               priority => -60
MSVS_NAKED ~ 'naked'
:lexeme ~ <MSVS_NORETURN>            priority => -60
MSVS_NORETURN ~ 'noreturn'
:lexeme ~ <MSVS_NOALIAS>             priority => -60
MSVS_NOALIAS ~ 'noalias'
:lexeme ~ <MSVS_DEPRECATED>          priority => -60
MSVS_DEPRECATED ~ 'deprecated'
:lexeme ~ <MSVS_RESTRICT>            priority => -60
MSVS_RESTRICT ~ 'restrict'
:lexeme ~ <MSVS_NOVTABLE>            priority => -60
MSVS_NOVTABLE ~ 'novtable'
:lexeme ~ <MSVS_PROPERTY>            priority => -60
MSVS_PROPERTY ~ 'property'
:lexeme ~ <MSVS_SELECTANY>           priority => -60
MSVS_SELECTANY ~ 'selectany'
:lexeme ~ <MSVS_THREAD>              priority => -60
MSVS_THREAD ~ 'thread'
:lexeme ~ <MSVS_UUID>                priority => -60
MSVS_UUID ~ 'uuid'
:lexeme ~ <MSVS_INLINE>              priority => -60
MSVS_INLINE ~ '__inline'
:lexeme ~ <MSVS_FORCEINLINE>         priority => -60
MSVS_FORCEINLINE ~ '__forceinline'
:lexeme ~ <MSVS_AT>                  priority => -60
MSVS_AT ~ '@'
:lexeme ~ <MSVS_NOTHROW>             priority => -60
MSVS_NOTHROW ~ 'NOTHROW'
:lexeme ~ <MSVS_W64>                 priority => -60
MSVS_W64 ~ '__w64'
:lexeme ~ <MSVS_PTR32>               priority => -60
MSVS_PTR32 ~ '__ptr32'
:lexeme ~ <MSVS_PTR64>               priority => -60
MSVS_PTR64 ~ '__ptr64'

#
# MSVS ASM LEXEMES
# ----------------
:lexeme ~ <MSVS_ASM_REP>                 priority => -60
MSVS_ASM_REP ~ 'REP'
:lexeme ~ <MSVS_ASM_REPE>                priority => -60
MSVS_ASM_REPE ~ 'REPE'
:lexeme ~ <MSVS_ASM_REPZ>                priority => -60
MSVS_ASM_REPZ ~ 'REPZ'
:lexeme ~ <MSVS_ASM_REPNE>               priority => -60
MSVS_ASM_REPNE ~ 'REPNE'
:lexeme ~ <MSVS_ASM_REPNZ>               priority => -60
MSVS_ASM_REPNZ ~ 'REPNZ'
:lexeme ~ <MSVS_ASM_AND>                 priority => -60
MSVS_ASM_AND ~ 'AND'
:lexeme ~ <MSVS_ASM_MOD>                 priority => -60
MSVS_ASM_MOD ~ 'MOD'
:lexeme ~ <MSVS_ASM_NOT>                 priority => -60
MSVS_ASM_NOT ~ 'NOT'
:lexeme ~ <MSVS_ASM_OR>                  priority => -60
MSVS_ASM_OR ~ 'OR'
:lexeme ~ <MSVS_ASM_SEG>                 priority => -60
MSVS_ASM_SEG ~ 'SEG'
:lexeme ~ <MSVS_ASM_SHL>                 priority => -60
MSVS_ASM_SHL ~ 'SHL'
:lexeme ~ <MSVS_ASM_SHR>                 priority => -60
MSVS_ASM_SHR ~ 'SHR'
:lexeme ~ <MSVS_ASM_XOR>                 priority => -60
MSVS_ASM_XOR ~ 'XOR'
#:lexeme ~ <MSVS_ASM_SHORT>               priority => -60
#MSVS_ASM_SHORT ~ 'SHORT'
:lexeme ~ <MSVS_ASM_TYPE>                priority => -60
MSVS_ASM_TYPE ~ '.TYPE'
#:lexeme ~ <MSVS_ASM_OPATTR>              priority => -60
#MSVS_ASM_OPATTR ~ 'OPATTR'
:lexeme ~ <MSVS_ASM_STAR>                priority => -60
MSVS_ASM_STAR ~ '*'
:lexeme ~ <MSVS_ASM_SLASH>               priority => -60
MSVS_ASM_SLASH ~ '/'
:lexeme ~ <MSVS_ASM_AH>                  priority => -60
MSVS_ASM_AH ~ 'AH'
:lexeme ~ <MSVS_ASM_AL>                  priority => -60
MSVS_ASM_AL ~ 'AL'
:lexeme ~ <MSVS_ASM_AX>                  priority => -60
MSVS_ASM_AX ~ 'AX'
:lexeme ~ <MSVS_ASM_BH>                  priority => -60
MSVS_ASM_BH ~ 'BH'
:lexeme ~ <MSVS_ASM_BL>                  priority => -60
MSVS_ASM_BL ~ 'BL'
:lexeme ~ <MSVS_ASM_BP>                  priority => -60
MSVS_ASM_BP ~ 'BP'
:lexeme ~ <MSVS_ASM_BX>                  priority => -60
MSVS_ASM_BX ~ 'BX'
:lexeme ~ <MSVS_ASM_BYTE>                priority => -60
MSVS_ASM_BYTE ~ 'BYTE'
:lexeme ~ <MSVS_ASM_CH>                  priority => -60
MSVS_ASM_CH ~ 'CH'
:lexeme ~ <MSVS_ASM_CL>                  priority => -60
MSVS_ASM_CL ~ 'CL'
:lexeme ~ <MSVS_ASM_COLON>               priority => -60
MSVS_ASM_COLON ~ ':'
:lexeme ~ <MSVS_ASM_CR0>                 priority => -60
MSVS_ASM_CR0 ~ 'CR0'
:lexeme ~ <MSVS_ASM_CR2>                 priority => -60
MSVS_ASM_CR2 ~ 'CR2'
:lexeme ~ <MSVS_ASM_CR3>                 priority => -60
MSVS_ASM_CR3 ~ 'CR3'
:lexeme ~ <MSVS_ASM_CS>                  priority => -60
MSVS_ASM_CS ~ 'CS'
:lexeme ~ <MSVS_ASM_CX>                  priority => -60
MSVS_ASM_CX ~ 'CX'
:lexeme ~ <MSVS_ASM_DH>                  priority => -60
MSVS_ASM_DH ~ 'DH'
:lexeme ~ <MSVS_ASM_DI>                  priority => -60
MSVS_ASM_DI ~ 'DI'
:lexeme ~ <MSVS_ASM_DL>                  priority => -60
MSVS_ASM_DL ~ 'DL'
:lexeme ~ <MSVS_ASM_DOLLAR>              priority => -60
MSVS_ASM_DOLLAR ~ 'DOLLAR'
:lexeme ~ <MSVS_ASM_DOT>                 priority => -60
MSVS_ASM_DOT ~ 'DOT'
:lexeme ~ <MSVS_ASM_DR0>                 priority => -60
MSVS_ASM_DR0 ~ 'DR0'
:lexeme ~ <MSVS_ASM_DR1>                 priority => -60
MSVS_ASM_DR1 ~ 'DR1'
:lexeme ~ <MSVS_ASM_DR2>                 priority => -60
MSVS_ASM_DR2 ~ 'DR2'
:lexeme ~ <MSVS_ASM_DR3>                 priority => -60
MSVS_ASM_DR3 ~ 'DR3'
:lexeme ~ <MSVS_ASM_DR6>                 priority => -60
MSVS_ASM_DR6 ~ 'DR6'
:lexeme ~ <MSVS_ASM_DR7>                 priority => -60
MSVS_ASM_DR7 ~ 'DR7'
:lexeme ~ <MSVS_ASM_DS>                  priority => -60
MSVS_ASM_DS ~ 'DWORD'
:lexeme ~ <MSVS_ASM_DWORD>               priority => -60
MSVS_ASM_DWORD ~ 'DWORD'
:lexeme ~ <MSVS_ASM_DX>                  priority => -60
MSVS_ASM_DX ~ 'DX'
:lexeme ~ <MSVS_ASM_EAX>                 priority => -60
MSVS_ASM_EAX ~ 'EAX'
:lexeme ~ <MSVS_ASM_EBP>                 priority => -60
MSVS_ASM_EBP ~ 'EBP'
:lexeme ~ <MSVS_ASM_EBX>                 priority => -60
MSVS_ASM_EBX ~ 'EBX'
:lexeme ~ <MSVS_ASM_ECX>                 priority => -60
MSVS_ASM_ECX ~ 'ECX'
:lexeme ~ <MSVS_ASM_EDI>                 priority => -60
MSVS_ASM_EDI ~ 'EDI'
:lexeme ~ <MSVS_ASM_EDX>                 priority => -60
MSVS_ASM_EDX ~ 'EDX'
:lexeme ~ <MSVS_ASM_EQ>                  priority => -60
MSVS_ASM_EQ ~ 'EQ'
:lexeme ~ <MSVS_ASM_ES>                  priority => -60
MSVS_ASM_ES ~ 'ES'
:lexeme ~ <MSVS_ASM_ESI>                 priority => -60
MSVS_ASM_ESI ~ 'ESI'
:lexeme ~ <MSVS_ASM_ESP>                 priority => -60
MSVS_ASM_ESP ~ 'ESP'
:lexeme ~ <MSVS_ASM_FAR>                 priority => -60
MSVS_ASM_FAR ~ 'FAR'
:lexeme ~ <MSVS_ASM_FAR16>               priority => -60
MSVS_ASM_FAR16 ~ 'FAR16'
:lexeme ~ <MSVS_ASM_FAR32>               priority => -60
MSVS_ASM_FAR32 ~ 'FAR32'
:lexeme ~ <MSVS_ASM_FS>                  priority => -60
MSVS_ASM_FS ~ 'FS'
:lexeme ~ <MSVS_ASM_FWORD>               priority => -60
MSVS_ASM_FWORD ~ 'FWORD'
:lexeme ~ <MSVS_ASM_GE>                  priority => -60
MSVS_ASM_GE ~ 'GE'
:lexeme ~ <MSVS_ASM_GS>                  priority => -60
MSVS_ASM_GS ~ 'GS'
:lexeme ~ <MSVS_ASM_GT>                  priority => -60
MSVS_ASM_GT ~ 'GT'
:lexeme ~ <MSVS_ASM_HIGH>                priority => -60
MSVS_ASM_HIGH ~ 'HIGH'
:lexeme ~ <MSVS_ASM_HIGHWORD>            priority => -60
MSVS_ASM_HIGHWORD ~ 'HIGHWORD'
:lexeme ~ <MSVS_ASM_LBRACKET>            priority => -60
MSVS_ASM_LBRACKET ~ '['
:lexeme ~ <MSVS_ASM_RBRACKET>            priority => -60
MSVS_ASM_RBRACKET ~ ']'
:lexeme ~ <MSVS_ASM_LE>                  priority => -60
MSVS_ASM_LE ~ 'LE'
:lexeme ~ <MSVS_ASM_LOCK>                priority => -60
MSVS_ASM_LOCK ~ 'LOCK'
:lexeme ~ <MSVS_ASM_LOW>                 priority => -60
MSVS_ASM_LOW ~ 'LOW'
:lexeme ~ <MSVS_ASM_LOWWORD>             priority => -60
MSVS_ASM_LOWWORD ~ 'LOWWORD'
:lexeme ~ <MSVS_ASM_LROFFSET>            priority => -60
MSVS_ASM_LROFFSET ~ 'LROFFSET'
:lexeme ~ <MSVS_ASM_LT>                  priority => -60
MSVS_ASM_LT ~ 'LT'
:lexeme ~ <MSVS_ASM_MINUS>               priority => -60
MSVS_ASM_MINUS ~ 'MINUS'
:lexeme ~ <MSVS_ASM_NE>                  priority => -60
MSVS_ASM_NE ~ 'NE'
:lexeme ~ <MSVS_ASM_NEAR>                priority => -60
MSVS_ASM_NEAR ~ 'NEAR'
:lexeme ~ <MSVS_ASM_NEAR16>              priority => -60
MSVS_ASM_NEAR16 ~ 'NEAR16'
:lexeme ~ <MSVS_ASM_NEAR32>              priority => -60
MSVS_ASM_NEAR32 ~ 'NEAR32'
:lexeme ~ <MSVS_ASM_OFFSET>              priority => -60
MSVS_ASM_OFFSET ~ 'OFFSET'
:lexeme ~ <MSVS_ASM_PLUS>                priority => -60
MSVS_ASM_PLUS ~ 'PLUS'
:lexeme ~ <MSVS_ASM_PTR>                 priority => -60
MSVS_ASM_PTR ~ 'PTR'
:lexeme ~ <MSVS_ASM_QWORD>               priority => -60
MSVS_ASM_QWORD ~ 'QWORD'
:lexeme ~ <MSVS_ASM_REAL10>              priority => -60
MSVS_ASM_REAL10 ~ 'REAL10'
:lexeme ~ <MSVS_ASM_REAL4>               priority => -60
MSVS_ASM_REAL4 ~ 'REAL4'
:lexeme ~ <MSVS_ASM_REAL8>               priority => -60
MSVS_ASM_REAL8 ~ 'REAL8'
:lexeme ~ <MSVS_ASM_WORD>                priority => -60
MSVS_ASM_WORD ~ 'WORD'
:lexeme ~ <MSVS_ASM_TR7>                 priority => -60
MSVS_ASM_TR7 ~ 'TR7'
:lexeme ~ <MSVS_ASM_TR6>                 priority => -60
MSVS_ASM_TR6 ~ 'TR6'
:lexeme ~ <MSVS_ASM_TR5>                 priority => -60
MSVS_ASM_TR5 ~ 'TR5'
:lexeme ~ <MSVS_ASM_TR4>                 priority => -60
MSVS_ASM_TR4 ~ 'TR4'
:lexeme ~ <MSVS_ASM_TR3>                 priority => -60
MSVS_ASM_TR3 ~ 'TR3'
:lexeme ~ <MSVS_ASM_THIS>                priority => -60
MSVS_ASM_THIS ~ 'THIS'
:lexeme ~ <MSVS_ASM_TBYTE>               priority => -60
MSVS_ASM_TBYTE ~ 'TBYTE'
:lexeme ~ <MSVS_ASM_SWORD>               priority => -60
MSVS_ASM_SWORD ~ 'SWORD'
:lexeme ~ <MSVS_ASM_SS>                  priority => -60
MSVS_ASM_SS ~ 'SS'
:lexeme ~ <MSVS_ASM_SP>                  priority => -60
MSVS_ASM_SP ~ 'SP'
:lexeme ~ <MSVS_ASM_SI>                  priority => -60
MSVS_ASM_SI ~ 'SI'
:lexeme ~ <MSVS_ASM_SDWORD>              priority => -60
MSVS_ASM_SDWORD ~ 'SDWORD'
:lexeme ~ <MSVS_ASM_SBYTE>               priority => -60
MSVS_ASM_SBYTE ~ 'SBYTE'

##########################################################
# GCC EXTENSIONS
#
# Copyright 2002-2009 ISP RAS (http://www.ispras.ru), UniTESK Lab (http://www.unitesk.com)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
# @author <A href="mailto:demakov@ispras.ru">Alexey Demakov</A>
# @author <A href="mailto:hed@ispras.ru">Alexey Khoroshilov</A>
# @version $Id$
#
##########################################################
gccBuiltinType ::= gccTypeof
                 | GCC_BUILTIN_VA_LIST

gccDeclarationSpecifier ::= gccAttribute
                          | gccExtensionSpecifier

gccAttribute ::= GCC_ATTRIBUTE LPAREN LPAREN gccAttributeParameterList RPAREN RPAREN

gccAttributeParameterList ::= gccAttributeParameter
                            | gccAttributeParameterList COMMA gccAttributeParameter

gccEmptyRule ::=

gccAttributeParameter ::= gccAnyWord
                        | gccAnyWord LPAREN RPAREN
                        | gccAnyWord LPAREN expression RPAREN
                        | gccEmptyRule
                        | gccAttributeExtension

gccAnyWord ::= IDENTIFIER
             | storageClassSpecifier
             | typeSpecifier
             | typeQualifier
             | functionSpecifier

gccExceptionFunction ::= STRING_LITERAL | IDENTIFIER

gccAttributeExtension ::=
        GCC_ALIAS LPAREN string RPAREN
        GCC_ALIGNED
        GCC_ALIGNED LPAREN gccTaggedAttributeNumber RPAREN
        GCC_ALIGNED LPAREN gccAlignofExpression RPAREN
        GCC_ALWAYS_INLINE
        GCC_CDECL
        GCC_CLEANUP LPAREN gccTaggedAttributeId RPAREN
        CONST
        GCC_CONSTRUCTOR
        GCC_DEPRECATED
        GCC_DESTRUCTOR
        GCC_DLLEXPORT
        GCC_DLLIMPORT
        GCC_EIGHTBIT_DATA
        GCC_EXCEPTION LPAREN gccExceptionFunction gccExceptionArgList RPAREN
        GCC_EXCEPTION LPAREN gccExceptionFunction RPAREN
        GCC_FAR
        GCC_FUNCTION_VECTOR
        GCC_FORMAT LPAREN GCC_FORMAT_ARCHETYPE COMMA gccTaggedAttributeNumber COMMA gccTaggedAttributeNumber RPAREN
        GCC_FORMAT_ARG LPAREN gccTaggedAttributeNumber RPAREN
        GCC_INTERRUPT
        GCC_INTERRUPT LPAREN string RPAREN
        GCC_INTERRUPT_HANDLER
        GCC_INTERRUPT_HANDLER LPAREN string RPAREN
        GCC_LONGCALL
        GCC_LONG_CALL
        GCC_MALLOC
        GCC_MAY_ALIAS
        GCC_MODEL LPAREN gccTaggedAttributeId RPAREN
        GCC_MODEL LPAREN GCC_MODEL_NAME RPAREN
        GCC_NAKED
        GCC_NEAR
        GCC_NO_CHECK_MEMORY_USAGE
        GCC_NO_INSTRUMENT_FUNCTION
        GCC_NOCOMMON
        GCC_NOINLINE
        GCC_NONNULL
        GCC_NONNULL LPAREN gccTaggedAttributeNumberList RPAREN
        GCC_NORETURN
        GCC_LEAF
        GCC_NOTHROW
        GCC_PACKED
        GCC_PURE
        GCC_REGPARM LPAREN gccTaggedAttributeNumber RPAREN
        GCC_SECTION LPAREN string RPAREN
        GCC_SHARED
        GCC_SHORT_CALL
        GCC_SIGNAL LPAREN string RPAREN
        GCC_SP_SWITCH LPAREN string RPAREN
        GCC_STDCALL
        GCC_TINY_DATA
        GCC_TLS_MODEL LPAREN GCC_TLS_MODEL_TYPE RPAREN
        GCC_TRANSPARENT_UNION
        GCC_TRAP_EXIT LPAREN gccTaggedAttributeNumber RPAREN
        GCC_USED
        GCC_UNUSED
        GCC_VECTOR_SIZE LPAREN gccTaggedAttributeNumber RPAREN
        GCC_VISIBILITY LPAREN GCC_VISIBILITY_TYPE RPAREN
        GCC_WEAK

#gccAttributeExtension ::= GCC_NOTHROW
#                        | GCC_LEAF
#                        | GCC_NONNULL LPAREN gccTaggedAttributeNumberList RPAREN

gccExceptionArgUnit ::= STRING_LITERAL
                      | IDENTIFIER

gccExceptionArgList ::= gccExceptionArgUnit
                      | gccExceptionArgList COMMA gccExceptionArgUnit

gccTaggedAttributeNumber ::= I_CONSTANT

gccTaggedAttributeId ::= IDENTIFIER

gccTaggedAttributeNumberList ::= gccTaggedAttributeNumber
                               | gccTaggedAttributeNumberList COMMA gccTaggedAttributeNumber

gccExtensionSpecifier ::= GCC_EXTENSION

gccAsmStatement ::= gccAsmExpression SEMICOLON

typeQualifierMaybe ::= typeQualifier
                     | gccEmptyRule

gccAsmOperandList ::= gccAsmOperand
                       | COMMA gccAsmOperandList

gccAsmClobberList ::= gccAsmClobber
                       | COMMA gccAsmClobber

gccAsmClobberExpressionMaybe ::= gccAsmClobberExpression
                               | gccEmptyRule

gccAsmOperandListMaybe ::= gccAsmOperandList | gccEmptyRule

gccAsmOperandExpression ::= COLON gccAsmOperandListMaybe

gccAsmClobberExpression ::= COLON gccAsmClobberList

gccAsmOperandInnerExpression ::= gccAsmOperandExpression gccAsmClobberExpressionMaybe

gccAsmOperandInnerExpressionMaybe ::= gccAsmOperandInnerExpression
                                    | gccEmptyRule

gccAsmInnerExpressionMaybe ::= gccAsmOperandExpression gccAsmOperandInnerExpressionMaybe
                                 | gccEmptyRule

gccAsmExpression ::= GCC_ASM typeQualifierMaybe LPAREN expression gccAsmInnerExpressionMaybe RPAREN

gccAsmOperandPrefix ::= LBRACKET IDENTIFIER RBRACKET

gccAsmOperandPrefixMaybe ::= gccAsmOperandPrefix
                           | gccEmptyRule

gccAsmOperand ::= gccAsmOperandPrefixMaybe string LPAREN expression RPAREN

gccAsmClobber ::= string

gccStatementExpression ::= LPAREN compoundStatement RPAREN

gccArrayTypeModifier ::= typeQualifier
                       | gccAttribute

# @since 2.6.264
# for error handling: second assignmentExpression is always last parameter name
gccBuiltinVaStart ::= GCC_BUILTIN_VA_START
                      LPAREN
                      assignmentExpression
                      COMMA
                      assignmentExpression
                      RPAREN

# @since 2.6.264
gccBuiltinVaEnd ::= GCC_BUILTIN_VA_END
                    LPAREN
                    assignmentExpression
                    RPAREN

gccBuiltinVaArg ::= GCC_BUILTIN_VA_ARG
                    LPAREN
                    assignmentExpression
                    COMMA
                    typeName
                    RPAREN

gccTypeof ::= GCC_TYPEOF LPAREN typeName RPAREN
            | GCC_TYPEOF LPAREN assignmentExpression RPAREN

gccBuiltinOffsetof ::= GCC_BUILTIN_OFFSETOF LPAREN typeName COMMA offsetofMemberDesignator RPAREN

offsetofMemberDesignator ::=   IDENTIFIER
                               | offsetofMemberDesignator DOT IDENTIFIER
                               | offsetofMemberDesignator LBRACKET expression RBRACKET

#
# Microsoft Extensions
#
msvsAttribute ::= MSVS_ASM | MSVS_FASTCALL | MSVS_BASED | MSVS_CDECL | MSVS_STDCALL

msvsBuiltinType ::=  MSVS_INT8
                  | MSVS_INT16
                  | MSVS_INT32
                  | MSVS_INT64

msvsExtendedDeclModifierList ::= msvsExtendedDeclModifier*

msvsDeclspec ::= MSVS_DECLSPEC LPAREN msvsExtendedDeclModifierList RPAREN

msvsPropertyList ::= IDENTIFIER EQUAL IDENTIFIER
                   | COMMA msvsPropertyList

msvsExtendedDeclModifier ::=   MSVS_ALLOCATE LPAREN string RPAREN
                           | MSVS_DLLIMPORT
                           | MSVS_DLLEXPORT
                           | MSVS_NAKED
                           | MSVS_NORETURN
                           | MSVS_NOALIAS
                           | MSVS_DEPRECATED LPAREN string RPAREN
                           | MSVS_RESTRICT
                           | MSVS_NOTHROW
                           | MSVS_NOVTABLE
                           | MSVS_PROPERTY LPAREN RPAREN
                           | MSVS_PROPERTY LPAREN msvsPropertyList RPAREN
                           | MSVS_SELECTANY
                           | MSVS_THREAD
                           | MSVS_UUID LPAREN string RPAREN

msvsFunctionSpecifier ::= MSVS_INLINE
                        | MSVS_FORCEINLINE

msvsAsmStatementDirectiveList ::= msvsAsmDirective+
msvsAsmStatement ::= MSVS_ASM msvsAsmDirective
                   | MSVS_ASM
                     LCURLY
                     msvsAsmStatementDirectiveList
                     RCURLY

msvsAsmDirective ::= msvsAsmLabelDef msvsAsmSegmentDirective
                   | msvsAsmSegmentDirective
                   | msvsAsmLabelDef

msvsAsmLabelDef ::= IDENTIFIER COLON
                  | IDENTIFIER COLON COLON
                  | MSVS_AT MSVS_AT COLON

#
# msvsAsmSegmentDirective ::=   msvsAsmInstruction
#                                | msvsAsmDataDirective
#                                | msvsAsmControlDirective
#                                | msvsAsmStartupDirective
#                                | msvsAsmExitDirective
#                                | msvsAsmOffsetDirective
#                                | msvsAsmLabelDirective
#                                | msvsAsmProcDirective
#                                  ( msvsAsmLocalDirective )*
#                                  ( msvsAsmDirective )*
#                                  msvsAsmEndpDirective
#                                | msvsAsmInvokeDirective
#                                | msvsAsmGeneralDirective
#
# The full MASM instruction set is not supported.
#
msvsAsmSegmentDirective ::=   msvsAsmInstruction

msvsAsmExprList ::= msvsAsmExpr
                  | COMMA msvsAsmExpr

msvsAsmInstruction ::= msvsAsmInstrPrefix msvsAsmMnemonic msvsAsmExprList
                     | msvsAsmInstrPrefix msvsAsmMnemonic
                     | msvsAsmMnemonic msvsAsmExprList
                     | msvsAsmMnemonic

msvsAsmInstrPrefix ::= MSVS_ASM_REP | MSVS_ASM_REPE | MSVS_ASM_REPZ | MSVS_ASM_REPNE | MSVS_ASM_REPNZ | MSVS_ASM_LOCK

msvsAsmMnemonic ::= IDENTIFIER | MSVS_ASM_AND | MSVS_ASM_MOD | MSVS_ASM_NOT | MSVS_ASM_OR | MSVS_ASM_SEG | MSVS_ASM_SHL | MSVS_ASM_SHR | MSVS_ASM_XOR

#
# msvsAsmExpr ::=   MSVS_ASM_SHORT  msvsAsmExpr05
#                   | MSVS_ASM_TYPE  msvsAsmExpr01
#                   | MSVS_ASM_OPATTR msvsAsmExpr01
#                   | msvsAsmExpr01
#
# The full MASM instruction set is not supported.
#/
msvsAsmExpr ::= msvsAsmExpr01

msvsAsmExpr01 ::=  msvsAsmExpr01 MSVS_ASM_OR  msvsAsmExpr02
                    | msvsAsmExpr01 MSVS_ASM_XOR msvsAsmExpr02
                    | msvsAsmExpr02

msvsAsmExpr02 ::=   msvsAsmExpr02 MSVS_ASM_AND msvsAsmExpr03
                    | msvsAsmExpr03

msvsAsmExpr03 ::=   MSVS_ASM_NOT msvsAsmExpr04
                    | msvsAsmExpr04

msvsAsmExpr04 ::=   msvsAsmExpr04 MSVS_ASM_EQ msvsAsmExpr05
                    | msvsAsmExpr04 MSVS_ASM_NE msvsAsmExpr05
                    | msvsAsmExpr04 MSVS_ASM_LT msvsAsmExpr05
                    | msvsAsmExpr04 MSVS_ASM_LE msvsAsmExpr05
                    | msvsAsmExpr04 MSVS_ASM_GT msvsAsmExpr05
                    | msvsAsmExpr04 MSVS_ASM_GE msvsAsmExpr05
                    | msvsAsmExpr05

msvsAsmExpr05 ::=   msvsAsmExpr05 MSVS_ASM_PLUS msvsAsmExpr06
                    | msvsAsmExpr05 MSVS_ASM_MINUS msvsAsmExpr06
                    | msvsAsmExpr06

msvsAsmExpr06 ::=   msvsAsmExpr06 MSVS_ASM_STAR  msvsAsmExpr07
                    | msvsAsmExpr06 MSVS_ASM_SLASH msvsAsmExpr07
                    | msvsAsmExpr06 MSVS_ASM_MOD   msvsAsmExpr07
                    | msvsAsmExpr06 MSVS_ASM_SHR   msvsAsmExpr07
                    | msvsAsmExpr06 MSVS_ASM_SHL   msvsAsmExpr07
                    | msvsAsmExpr07

msvsAsmExpr07 ::=   MSVS_ASM_PLUS  msvsAsmExpr08
                    | MSVS_ASM_MINUS msvsAsmExpr08
                    | msvsAsmExpr08

msvsAsmExpr08 ::=   MSVS_ASM_HIGH     msvsAsmExpr09
                    | MSVS_ASM_LOW      msvsAsmExpr09
                    | MSVS_ASM_HIGHWORD msvsAsmExpr09
                    | MSVS_ASM_LOWWORD  msvsAsmExpr09
                    | msvsAsmExpr09

msvsAsmExpr09 ::=   MSVS_ASM_OFFSET   msvsAsmExpr10
                    | MSVS_ASM_SEG      msvsAsmExpr10
                    | MSVS_ASM_LROFFSET msvsAsmExpr10
                    | MSVS_ASM_TYPE     msvsAsmExpr10
                    | MSVS_ASM_THIS     msvsAsmExpr10
                    | msvsAsmExpr09 MSVS_ASM_PTR msvsAsmExpr10
                    | msvsAsmExpr09 MSVS_ASM_COLON   msvsAsmExpr10
                    | msvsAsmExpr10

msvsAsmExpr10 ::=   msvsAsmExpr10 MSVS_ASM_DOT msvsAsmExpr11
                    | msvsAsmExpr10 MSVS_ASM_LBRACKET msvsAsmExpr MSVS_ASM_RBRACKET
                    | msvsAsmExpr11

#
# msvsAsmExpr11 ::=   LPAREN msvsAsmExpr RPAREN
#                     | MSVS_ASM_LBRACKET msvsAsmExpr MSVS_ASM_RBRACKET
#                     | MSVS_ASM_WIDTH IDENTIFIER
#                     | MSVS_ASM_MASK  IDENTIFIER
#                     | MSVS_ASM_SIZE    msvsAsmSizeArg
#                     | MSVS_ASM_SIZEOF  msvsAsmSizeArg
#                     | MSVS_ASM_LENGTH   IDENTIFIER
#                     | MSVS_ASM_LENGTHOF IDENTIFIER
#                     | msvsAsmEecordConst
#                     | msvsAsmString
#                     | msvsAsmConstant
#                     | msvsAsmType
#                     | IDENTIFIER
#                     | MSVS_ASM_DOLLAR
#                     | msvsAsmSegmentRegister
#                     | msvsAsmRegister
#                     | MSVS_ASM_ST
#                     | MSVS_ASM_ST LPAREN msvsAsmExpr RPAREN
#
# The full MASM instruction set is not supported.
#
msvsAsmExpr11 ::=   LPAREN msvsAsmExpr RPAREN
                    | MSVS_ASM_LBRACKET msvsAsmExpr RPAREN
                    | msvsAsmConstant
                    | msvsAsmType
                    | IDENTIFIER
                    | MSVS_ASM_DOLLAR
                    | msvsAsmSegmentRegister
                    | msvsAsmRegister

msvsAsmType ::=   IDENTIFIER
                  | msvsAsmDistance
                  | msvsAsmDataType

msvsAsmDistance ::=   msvsAsmNearfar
                      | MSVS_ASM_NEAR16
                      | MSVS_ASM_NEAR32
                      | MSVS_ASM_FAR16
                      | MSVS_ASM_FAR32

msvsAsmNearfar ::= MSVS_ASM_NEAR | MSVS_ASM_FAR

msvsAsmDataType ::=   MSVS_ASM_BYTE
                       | MSVS_ASM_SBYTE
                       | MSVS_ASM_WORD
                       | MSVS_ASM_SWORD
                       | MSVS_ASM_DWORD
                       | MSVS_ASM_SDWORD
                       | MSVS_ASM_FWORD
                       | MSVS_ASM_QWORD
                       | MSVS_ASM_TBYTE
                       | MSVS_ASM_REAL4
                       | MSVS_ASM_REAL8
                       | MSVS_ASM_REAL10

msvsAsmSegmentRegister ::= MSVS_ASM_CS | MSVS_ASM_DS | MSVS_ASM_ES | MSVS_ASM_FS | MSVS_ASM_GS | MSVS_ASM_SS

msvsAsmRegister ::=   msvsAsmSpecialRegister
                      | msvsAsmGpRegister
                      | msvsAsmByteRegister

msvsAsmSpecialRegister ::=   MSVS_ASM_CR0 | MSVS_ASM_CR2 | MSVS_ASM_CR3
                              | MSVS_ASM_DR0 | MSVS_ASM_DR1 | MSVS_ASM_DR2 | MSVS_ASM_DR3 | MSVS_ASM_DR6 | MSVS_ASM_DR7
                              | MSVS_ASM_TR3 | MSVS_ASM_TR4 | MSVS_ASM_TR5 | MSVS_ASM_TR6 | MSVS_ASM_TR7

msvsAsmGpRegister ::=   MSVS_ASM_AX | MSVS_ASM_EAX | MSVS_ASM_BX | MSVS_ASM_EBX | MSVS_ASM_CX | MSVS_ASM_ECX | MSVS_ASM_DX | MSVS_ASM_EDX
                         | MSVS_ASM_BP | MSVS_ASM_EBP | MSVS_ASM_SP | MSVS_ASM_ESP | MSVS_ASM_DI | MSVS_ASM_EDI | MSVS_ASM_SI | MSVS_ASM_ESI

msvsAsmByteRegister ::= MSVS_ASM_AL | MSVS_ASM_AH | MSVS_ASM_BL | MSVS_ASM_BH | MSVS_ASM_CL | MSVS_ASM_CH | MSVS_ASM_DL | MSVS_ASM_DH

msvsAsmConstant ::= I_CONSTANT

#
# Preprocessor ultra minimal grammar: we keep only what is left AFTER preprocessing, indeed
#
PREPROCESSOR_DASH ~ '#'
PREPROCESSOR_LINE ~ 'line'
PREPROCESSOR_PRAGMA ~ 'pragma'
PREPROCESSOR_DIGITS ~ [0-9]+
PREPROCESSOR_DQUOTE ~ '"'
PREPROCESSOR_NOTDQUOTE ~ [^"]*
PREPROCESSOR_WS ~ [ \t]
PREPROCESSOR_WS_any ~ PREPROCESSOR_WS*
PREPROCESSOR_WS_many ~ PREPROCESSOR_WS+
PREPROCESSOR_NOTNEWLINE ~ [^\n]
PREPROCESSOR_NOTNEWLINE_any ~ PREPROCESSOR_NOTNEWLINE*
PREPROCESSOR_FILENAME ~ PREPROCESSOR_DQUOTE PREPROCESSOR_NOTDQUOTE PREPROCESSOR_DQUOTE

:lexeme ~ PREPROCESSOR_LINE_DIRECTIVE
PREPROCESSOR_LINE_DIRECTIVE ~ PREPROCESSOR_DASH PREPROCESSOR_WS_any PREPROCESSOR_LINE PREPROCESSOR_WS_many PREPROCESSOR_DIGITS PREPROCESSOR_WS_many PREPROCESSOR_FILENAME PREPROCESSOR_NOTNEWLINE_any
                            | PREPROCESSOR_DASH PREPROCESSOR_WS_any PREPROCESSOR_DIGITS PREPROCESSOR_WS_many PREPROCESSOR_FILENAME PREPROCESSOR_NOTNEWLINE_any

:lexeme ~ PREPROCESSOR_PRAGMA_DIRECTIVE
PREPROCESSOR_PRAGMA_DIRECTIVE ~ PREPROCESSOR_DASH PREPROCESSOR_WS_any PREPROCESSOR_PRAGMA PREPROCESSOR_NOTNEWLINE_any

preprocessorDirective ::= PREPROCESSOR_LINE_DIRECTIVE
                        | PREPROCESSOR_PRAGMA_DIRECTIVE
