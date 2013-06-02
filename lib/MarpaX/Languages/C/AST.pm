use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST;

# ABSTRACT: Translate a C source to an AST

use Log::Any qw/$log/;
use Carp qw/croak/;
use MarpaX::Languages::C::AST::Grammar;
use MarpaX::Languages::C::AST::Impl qw/DOT_COMPLETION DOT_PREDICTION/;
use MarpaX::Languages::C::AST::Scope;

# VERSION

=head1 DESCRIPTION

This modules translates a C source into an AST tree. The AST consist of blessed objects that map exactly to the C grammar in use. If you want to enable logging, be aware that this module is a Log::Any thingy.

Please note that this module just I<translates> a C source, it does I<not> check for its correctness, i.e. the numerous grammar constraints built on top on the C grammar are not implemented, for example constraint on the number of storage class specifiers, uniqueness of labeled statements within a function, etc.. This is left to a compiler, which is not the goal here. So, to state things clearly, this module is addressing the I<ambiguities> of the grammar itself, i.e. the dangling else, the typedef/enum/identifier. And produces an AST of the parse tree value.

=head1 SYNOPSIS

    use strict;
    use warnings FATAL => 'all';
    use MarpaX::Languages::C::AST;
    use Log::Log4perl qw/:easy/;
    use Log::Any::Adapter;
    use Log::Any qw/$log/;
    use Data::Dumper;
    #
    # Init log
    #
    our $defaultLog4perlConf = '
    log4perl.rootLogger              = WARN, Screen
    log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
    log4perl.appender.Screen.stderr  = 0
    log4perl.appender.Screen.layout  = PatternLayout
    log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
    ';
    Log::Log4perl::init(\$defaultLog4perlConf);
    Log::Any::Adapter->set('Log4perl');
    #
    # Parse C
    #
    my $cSourceCode = '
    typedef struct s1_ {int i1;} x1, y1;
    struct x1 {x1 i2;};
    x1 x;
    ';
    my $cAstObject = MarpaX::Languages::C::AST->new();
    print Dumper($cAstObject->parse(\$cSourceCode));

=head1 SUBROUTINES/METHODS

=head2 new($class, $grammarName)

Instantiate a new object. Takes as parameter an optional base name of a grammar. Default is 'ISO-ANSI-C-2011'.

=cut

sub new {
  my ($class, $grammarName) = @_;

  $grammarName //= 'ISO-ANSI-C-2011';

  my $self  = {};
  $self->{_scope} = MarpaX::Languages::C::AST::Scope->new(),
    $self->{_grammar} = MarpaX::Languages::C::AST::Grammar->new($grammarName);

  my $grammar_option = $self->{_grammar}->grammar_option();
  $grammar_option->{bless_package} = 'C::AST';
  $grammar_option->{source} = \$self->{_grammar}->content();

  my $recce_option = $self->{_grammar}->recce_option();

  $self->{_impl} = MarpaX::Languages::C::AST::Impl->new($grammar_option, $recce_option);
  $self->{_nbTypedef} = 0;               # Number of TYPEDEF
  $self->{_nbParameterTypeList} = 0;     # Number of parameterTypeList
  $self->{_nbStructDeclarationList} = 0; # Number of structDeclarationList
  $self->{_identifier} = '';             # Last lexeme action if it pushed an identifier - could have been done via an event on a new rule like e.g. directDeclaratorIdentifier ::= IDENTIFIER

  bless($self, $class);

  return $self;
}

=head2 parse($self, $referenceToSourceCodep, $optionalArrayOfValuesb)

Do the parsing and return the blessed value. Takes as first parameter the reference to a C source code. Takes as optional second parameter a flag saying if the return value should be an array of all values or not. If this flag is false, the module will croak if there more than one parse tree value.

=cut

sub parse {
  my ($self, $referenceToSourceCodep, $optionalArrayOfValuesb) = @_;

  my $max = length(${$referenceToSourceCodep});
  my $pos = $self->{_impl}->read($referenceToSourceCodep);
  do {
    $self->_doEvent();
    $self->_doLexeme();
  } while (($pos = $self->{_impl}->resume()) < $max);

  return($self->_value($optionalArrayOfValuesb));
}

#
# INTERNAL METHODS
#

######################
# _nbParameterTypeList
######################
sub _nbParameterTypeList {
    my ($self, $context, $inc) = @_;

    if (defined($inc)) {
	$self->{_nbParameterTypeList} += $inc;
    } else {
	$self->{_nbParameterTypeList} = 0;
    }
    $log->debugf('[%s] _nbParameterTypeList is now %d', $context, $self->{_nbParameterTypeList});
}

##########################
# _nbStructDeclarationList
##########################
sub _nbStructDeclarationList {
    my ($self, $context, $inc) = @_;

    if (defined($inc)) {
	$self->{_nbStructDeclarationList} += $inc;
    } else {
	$self->{_nbStructDeclarationList} = 0;
    }
    $log->debugf('[%s] _nbStructDeclarationList is now %d', $context, $self->{_nbStructDeclarationList});
}

############
# _nbTypedef
############
sub _nbTypedef {
    my ($self, $context, $inc) = @_;

    if (defined($inc)) {
	$self->{_nbTypedef} += $inc;
    } else {
	$self->{_nbTypedef} = 0;
    }
    $log->debugf('[%s] _nbTypedef is now %d', $context, $self->{_nbTypedef});
}

#######################
# _show_last_expression
#######################
sub _show_last_expression {
  my ($self) = @_;

  my ($start, $end) = $self->{_impl}->last_completed_range('translationUnit');
  return 'No expression was successfully parsed' if (! defined($start));
  my $lastExpression = $self->{_impl}->range_to_string($start, $end);
  return "Last expression successfully parsed was: $lastExpression";
}

########
# _value
########
sub _value {
  my ($self, $arrayOfValuesb) = @_;

  $arrayOfValuesb ||= 0;

  my @rc = ();
  my $nvalue = 0;
  my $valuep = $self->{_impl}->value() || croak $self->_show_last_expression();
  if (defined($valuep)) {
    push(@rc, $valuep);
  }
  do {
    ++$nvalue;
    $valuep = $self->{_impl}->value();
    if (defined($valuep)) {
      push(@rc, $valuep);
    }
  } while (defined($valuep));
  if ($#rc != 0 && ! $arrayOfValuesb) {
    croak 'Number of parse tree value should be 1';
  }
  if ($arrayOfValuesb) {
    return [ @rc ];
  } else {
    return $rc[0];
  }
}

##########
# _doEvent
##########
sub _doEvent {
  my ($self) = @_;

  #
  ## List of events of interest
  #
  my %event = (
               initDeclaratorList => 0,
               parameterDeclaration => 0,
               declaration => 0,
               functionDefinition => 0,
               enumerationConstant => 0,
               primaryExpression => 0
              );

  my $iEvent = 0;
  my $hasEvent = 0;
  while (defined($_ = $self->{_impl}->event($iEvent++))) {
    ++$hasEvent;
    ++$event{$_->[0]} if (exists($event{$_->[0]}));
  }
  return if (! $hasEvent);

  $log->debugf('<Events %s>', join(',', sort grep {$event{$_}} keys %event));

  # ---------------------------------------
  # Enter/Obscure typedef-name in namespace
  # ---------------------------------------
  if ($event{initDeclaratorList} && $self->{_identifier}) {
    my $event = 'initDeclaratorList';
    if ($self->_canEnterTypedef($event)) {
      if ($self->{_nbTypedef} > 0) {
        $self->{_scope}->parseEnterTypedef($event, $self->{_identifier});
      } else {
        $self->{_scope}->parseObscureTypedef($event, $self->{_identifier});
      }
    }
  }

  # ----------
  # Enter enum
  # ----------
  if ($event{enumerationConstant}) {
      my $event = 'enumerationConstant';
    #
    # Enum is not scope dependend - from now on it obscures any use of its
    # identifier in any scope
    #
    my $enumerationConstant = $self->{_impl}->substring($self->{_impl}->last_completed('enumerationConstant'));
    $self->{_scope}->parseEnterEnum($event, $enumerationConstant);
  }

  # ----------------------------------------------------------------------------------
  # parameterDeclaration, declaration, functionDefinition: reset the number of TYPEDEF
  # ----------------------------------------------------------------------------------
  if ($event{parameterDeclaration} || $event{declaration} || $event{functionDefinition}) {
    my $event = join(',', sort grep {$event{$_}} qw/parameterDeclaration declaration functionDefinition/);
    $self->_nbTypedef($event, undef);
  }

  # -----------------------------------
  # primaryExpression: anything to do ?
  # -----------------------------------
  if ($event{primaryExpression}) {
  }
}

####################
# _expectTypedefName
####################
sub _expectTypedefName {
  my ($self, $context) = @_;
  my $rc = $self->{_impl}->findInProgressShort(DOT_PREDICTION, 'typeSpecifier', [ 'TYPEDEF_NAME' ]);
  $log->debugf('[%s] Expecting TYPEDEF_NAME? %s', $context, $rc ? 'yes' : 'no');
  return $rc;
}

#############
# _expectEnum
#############
sub _expectEnum {
  my ($self, $context) = @_;
  my $rc = $self->{_impl}->findInProgressShort(DOT_PREDICTION, 'constant', [ 'ENUMERATION_CONSTANT' ]);
  $log->debugf('[%s] Expecting ENUMERATION_CONSTANT? %s', $context, $rc ? 'yes' : 'no');
  return $rc;
}

###########
# _doLexeme
###########
sub _doLexeme {
  my ($self) = @_;

  $self->{_identifier} = '';

  my $lexeme = $self->{_impl}->pause_lexeme();
  if (! defined($lexeme)) {
    return;
  }

  $log->debugf('<Lexeme %s>', $lexeme);

  # -------------------------------------------------------------------------
  # Lexemes "before" pause: IDENTIFIER, TYPEDEF_NAME and ENUMERATION_CONSTANT
  # -------------------------------------------------------------------------
  if (grep {$lexeme eq $_} qw/IDENTIFIER TYPEDEF_NAME ENUMERATION_CONSTANT/) {
      #
      # Determine the correct lexeme
      #
      my ($lexeme_start, $lexeme_length) = $self->{_impl}->pause_span();
      my $lexeme_value = $self->{_impl}->literal($lexeme_start, $lexeme_length);
      my $context = sprintf('%s "%s"?', $lexeme, $lexeme_value);
      #
      # All parse symbol activity must be suspended when in the context of a structDeclarator
      #
      my $newlexeme;
      if ($self->_expectTypedefName($context) && $self->_canEnterTypedefName($context) && $self->{_scope}->parseIsTypedef($context, $lexeme_value)) {
	  $newlexeme = 'TYPEDEF_NAME';
      }
      elsif ($self->_expectEnum($context) && $self->_canEnterEnumerationConstant($context) && $self->{_scope}->parseIsEnum($context, $lexeme_value)) {
	  $newlexeme = 'ENUMERATION_CONSTANT';
      }
      else {
	  $newlexeme = 'IDENTIFIER';
	  $self->{_identifier} = $lexeme_value;
      }
      #
      # Push the unambiguated lexeme
      #
      $log->debugf('[%s] Pushing lexeme %s', $context, $newlexeme);
      if (! defined($self->{_impl}->lexeme_read($newlexeme, $lexeme_start, $lexeme_length, $lexeme_value))) {
	  my ($line, $column) = $self->{_impl}->line_column($lexeme_start);
	  my $msg = sprintf('[%s] Error at line %d, column %d: "%s" cannot be associated to lexeme %s', $context, $line, $column, $lexeme_value, $newlexeme);
	  $log->fatalf($msg);
	  croak $msg;
      }
      #
      # A lexeme_read() can generate an event
      #
      $self->_doEvent();
  }
  # -----------------------------------------------------------
  # Lexemes "after" pause: Scope management, Context management
  # -----------------------------------------------------------
  else {
      my $context = $lexeme;
      # ----------------
      # Scope management
      # ----------------
      #
      # Enter/Reenter
      #
      if    ($lexeme eq 'LPAREN_PARAMETER')             {  $self->{_scope}->parseEnterScope($context); }
      elsif ($lexeme eq 'LPAREN_IDENTIFIERLIST')        {  $self->{_scope}->parseEnterScope($context); }
      elsif ($lexeme eq 'LCURLY_COMPOUNDSTATEMENT')     {$self->_canReenterScope() ?
                                                           $self->{_scope}->parseReenterScope($context) :
                                                           $self->{_scope}->parseEnterScope($context); }
      #
      # Exit
      #
      if    ($lexeme eq 'RPAREN_PARAMETER')             { $self->{_scope}->parseExitScope($context); }
      elsif ($lexeme eq 'RPAREN_IDENTIFIERLIST')        { $self->{_scope}->parseExitScope($context); }
      elsif ($lexeme eq 'RCURLY_COMPOUNDSTATEMENT')     { $self->{_scope}->parseExitScope($context); }
      # ------------------
      # Context management
      # ------------------
      if    ($lexeme eq 'LPAREN_PARAMETER')             { $self->_nbParameterTypeList($context, 1);      }
      elsif ($lexeme eq 'RPAREN_PARAMETER')             { $self->_nbParameterTypeList($context, -1);     }
      elsif ($lexeme eq 'LCURLY_STRUCTDECLARATIONLIST') { $self->_nbStructDeclarationList($context, 1);  }
      elsif ($lexeme eq 'RCURLY_STRUCTDECLARATIONLIST') { $self->_nbStructDeclarationList($context, -1); }
      elsif ($lexeme eq 'TYPEDEF')                      { $self->_nbTypedef($context, 1);                }
  }
}

######################
# _canEnterTypedefName
######################
sub _canEnterTypedefName {
  my ($self, $context) = @_;

  my $rc = 1;
  if ($self->{_nbParameterTypeList} > 0) {
    #
    # In parameterDeclaration a typedef-name cannot be entered.
    #
    $log->debugf('[%s] parameterDeclaration context: a typedef-name cannot be entered', $context);
    $rc = 0;
  }

  return($rc);
}

##################
# _canEnterTypedef
##################
sub _canEnterTypedef {
  my ($self, $context) = @_;

  my $rc = 1;
  if ($self->{_nbStructDeclarationList} > 0) {
    #
    # In structDeclaratorparameterDeclaration a typedef-name cannot be entered.
    #
    $log->debugf('[%s] structDeclarationList context: parse symbol activity is suspended', $context);
    $rc = 0;
  }

  return($rc);
}

##############################
# _canEnterEnumerationConstant
##############################
sub _canEnterEnumerationConstant {
  my ($self, $context) = @_;

  my $rc = 1;
  $log->debugf('[%s] enumeration constant can be entered', $context);

  return($rc);
}

##################
# _canReenterScope
##################
sub _canReenterScope {
  my ($self) = @_;

  return ($self->{_impl}->findInProgressShort(-2, 3, 'functionDefinition', [ 'declarationSpecifiers', 'declarator', 'declarationList', 'compoundStatement' ]) ||
          $self->{_impl}->findInProgressShort(-2, 2, 'functionDefinition', [ 'declarationSpecifiers', 'declarator', 'compoundStatement' ]));
}

1;
