package MarpaX::Languages::C::AST;

use strict;
use warnings FATAL => 'all';
use Log::Any qw/$log/;
use Carp qw/croak/;
use MarpaX::Languages::C::AST::Grammar;
use MarpaX::Languages::C::AST::Impl qw/DOT_COMPLETION DOT_PREDICTION/;
use MarpaX::Languages::C::AST::Scope;

=head1 NAME

MarpaX::Languages::C::AST - Translate a C source to an AST

=head1 VERSION

Version 0.02

=cut

our $VERSION = '0.02';


=head1 SYNOPSIS

This modules translates a C source into an AST tree. The AST consist of blessed objects that map exactly to the C grammar in use. If you want to enable logging, be aware that this module is a Log::Any thingy.

Please note that this module just I<translates> a C source, it does I<not> check for its correctness, i.e. the numerous grammar constraints built on top on the C grammar are not implemented, for example constraint on the number of storage class specifiers, uniqueness of labeled statements within a function, etc.. This is left to a compiler, which is not the goal here. So, to state things clearly, this module is adressing the I<ambiguities> of the grammar itself, i.e. the dangling else, the typedef/enum/identifier. And produces an AST of the parse tree value.

Example:

#!env perl
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
our $defaultLog4perlConf = <<DEFAULT_LOG4PERL_CONF;
log4perl.rootLogger              = WARN, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 0
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
DEFAULT_LOG4PERL_CONF
Log::Log4perl::init(\$defaultLog4perlConf);
Log::Any::Adapter->set('Log4perl');
#
# Parse C
#
my $cSourceCode = <<C_SOURCE_CODE;
typedef struct s1_ {int i1;} x1, y1;
struct x1 {x1 i2;};
x1 x;
C_SOURCE_CODE
my $cAstObject = MarpaX::Languages::C::AST->new();
print Dumper($cAstObject->parse(\$cSourceCode));

=head1 SUBROUTINES/METHODS

=head2 new($class, $grammarName)

Instanciate a new object. Takes as parameter an optional base name of a grammar. Default is 'ISO-ANSI-C-2011'.

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

Do the parsing and return the blessed value. Takes as first parameter the reference to a C source code. Takes as optional second parameter a flag saying if the return value should be an array of all values or not. If this flag is false, the module will croak if there more than one parsee tree value.

=cut

sub parse {
  my ($self, $referenceToSourceCodep, $optionalArrayOfValuesb) = @_;

  my $max = length(${$referenceToSourceCodep});
  my $pos = $self->{_impl}->read($referenceToSourceCodep);
  do {
    $self->_doEvent();
    $self->_doLexeme();
  } while (($pos = $self->{_impl}->resume()) < $max);

  $optionalArrayOfValuesb ||= 0;
  return($self->_value($optionalArrayOfValuesb));
}

#
# INTERNAL METHODS
#
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

  $log->debugf('[Events %s]', join(',', sort grep {$event{$_}} keys %event));

  # ---------------------------------------
  # Enter/Obscure typedef-name in namespace
  # ---------------------------------------
  if ($event{initDeclaratorList} && $self->{_identifier}) {
    my $event = 'initDeclaratorList';
    if ($self->_canEnterTypedef($event)) {
      if ($self->{_nbTypedef} > 0) {
        $self->{_scope}->parseEnterTypedef($self->{_identifier});
      } else {
        $self->{_scope}->parseObscureTypedef($self->{_identifier});
      }
    }
  }

  # ----------
  # Enter enum
  # ----------
  if ($event{enumerationConstant}) {
    #
    # Enum is not scope dependend - from now on it obscures any use of its
    # identifier in any scope
    #
    my $enumerationConstant = $self->{_impl}->substring($self->{_impl}->last_completed('enumerationConstant'));
    $self->{_scope}->parseEnterEnum($enumerationConstant);
  }

  # ----------------------------------------------------------------------------------
  # parameterDeclaration, declaration, functionDefinition: reset the number of TYPEDEF
  # ----------------------------------------------------------------------------------
  if ($event{parameterDeclaration} || $event{declaration} || $event{functionDefinition}) {
    my $event = join(',', sort grep {$event{$_}} qw/parameterDeclaration declaration functionDefinition/);
    $self->{_nbTypedef} = 0;
    $log->debugf('[Event %s] _nbTypedef is now %d', $event, $self->{_nbTypedef});
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
  my ($self, $lexeme, $lexeme_value) = @_;
  my $rc = $self->{_impl}->findInProgressShort(DOT_PREDICTION, 'typeSpecifier', [ 'TYPEDEF_NAME' ]);
  $log->debugf('[Lexeme %s "%s"?] Expecting TYPEDEF_NAME? %s', $lexeme, $lexeme_value, $rc ? 'yes' : 'no');
  return $rc;
}

#############
# _expectEnum
#############
sub _expectEnum {
  my ($self, $lexeme, $lexeme_value) = @_;
  my $rc = $self->{_impl}->findInProgressShort(DOT_PREDICTION, 'constant', [ 'ENUMERATION_CONSTANT' ]);
  $log->debugf('[Lexeme %s "%s"?] Expecting ENUMERATION_CONSTANT? %s', $lexeme, $lexeme_value, $rc ? 'yes' : 'no');
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

  # -----------------------------------------------------------------------
  # Ambiguity managenent: IDENTIFIER, TYPEDEF_NAME and ENUMERATION_CONSTANT
  # -----------------------------------------------------------------------
  if (grep {$lexeme eq $_} qw/IDENTIFIER TYPEDEF_NAME ENUMERATION_CONSTANT/) {
    #
    # Determine the correct lexeme
    #
    my ($lexeme_start, $lexeme_length) = $self->{_impl}->pause_span();
    my $lexeme_value = $self->{_impl}->literal($lexeme_start, $lexeme_length);
    #
    # All parse symbol activity must be suspended when in the context of a structDeclarator
    #
    my $newlexeme;
    if ($self->_expectTypedefName($lexeme, $lexeme_value) && $self->_canEnterTypedefName($lexeme, $lexeme_value) && $self->{_scope}->parseIsTypedef($lexeme_value)) {
      $newlexeme = 'TYPEDEF_NAME';
    } elsif ($self->_expectEnum($lexeme, $lexeme_value) && $self->_canEnterEnumerationConstant($lexeme, $lexeme_value) && $self->{_scope}->parseIsEnum($lexeme_value)) {
      $newlexeme = 'ENUMERATION_CONSTANT';
    } else {
      $newlexeme = 'IDENTIFIER';
      $self->{_identifier} = $lexeme_value;
    }
    #
    # Push the unambiguated lexeme
    #
    $log->debugf('[Lexeme %s "%s"?] Pushing lexeme %s', $lexeme, $lexeme_value, $newlexeme);
    if (! defined($self->{_impl}->lexeme_read($newlexeme, $lexeme_start, $lexeme_length, $lexeme_value))) {
      my ($line, $column) = $self->{_impl}->line_column($lexeme_start);
      my $msg = sprintf('[Lexeme %s "%s"?] Error at line %d, column %d: "%s" cannot be associated to lexeme %s', $lexeme, $line, $column, $lexeme_value, $newlexeme);
      $log->fatalf($msg);
      croak $msg;
    }
    #
    # A lexeme_read() can generate an event
    #
    $self->_doEvent();
  }
  # --------------------------------------------------------------------------------------
  # Scope management:   LPAREN_PARAMETER, RPAREN_PARAMETER
  #                     LPAREN_IDENTIFIERLIST, RPAREN_IDENTIFIERLIST
  #                     LCURLY_COMPOUNDSTATEMENT, RCURLY_COMPOUNDSTATEMENT
  # --------------------------------------------------------------------------------------
  elsif ($lexeme eq 'LPAREN_PARAMETER') {
    ++$self->{_nbParameterTypeList};
    $log->debugf('[Lexeme %s] _nbParameterTypeList is now %d', $lexeme, $self->{_nbParameterTypeList});
    $self->{_scope}->parseEnterScope();
  } elsif ($lexeme eq 'RPAREN_PARAMETER') {
    --$self->{_nbParameterTypeList};
    $log->debugf('[Lexeme %s] _nbParameterTypeList is now %d', $lexeme, $self->{_nbParameterTypeList});
    $self->{_scope}->parseExitScope();
  } elsif ($lexeme eq 'LPAREN_IDENTIFIERLIST') {
    $self->{_scope}->parseEnterScope();
  } elsif ($lexeme eq 'RPAREN_IDENTIFIERLIST') {
    $self->{_scope}->parseExitScope();
  } elsif ($lexeme eq 'LCURLY_COMPOUNDSTATEMENT') {
    if ($self->_canReenterScope()) {
      $self->{_scope}->parseReenterScope();
    } else {
      $self->{_scope}->parseEnterScope();
    }
  } elsif ($lexeme eq 'RCURLY_COMPOUNDSTATEMENT') {
    $self->{_scope}->parseExitScope();
  }
  # --------------------------------------------------------------------------------------
  # Context management: LCURLY_STRUCTDECLARATIONLIST, RCURLY_STRUCTDECLARATIONLIST
  #                     TYPEDEF
  # --------------------------------------------------------------------------------------
  elsif ($lexeme eq 'LCURLY_STRUCTDECLARATIONLIST') {
    ++$self->{_nbStructDeclarationList};
    $log->debugf('[Lexeme %s] _nbStructDeclarationList is now %d', $lexeme, $self->{_nbStructDeclarationList});
  } elsif ($lexeme eq 'RCURLY_STRUCTDECLARATIONLIST') {
    --$self->{_nbStructDeclarationList};
    $log->debugf('[Lexeme %s] _nbStructDeclarationList is now %d', $lexeme, $self->{_nbStructDeclarationList});
  } elsif ($lexeme eq 'TYPEDEF') {
    ++$self->{_nbTypedef};
    $log->debugf('[Lexeme %s] _nbTypedef is now %d', $lexeme, $self->{_nbTypedef});
  }

}

######################
# _canEnterTypedefName
######################
sub _canEnterTypedefName {
  my ($self, $lexeme, $lexeme_value) = @_;

  my $rc = 1;
  if ($self->{_nbParameterTypeList} > 0) {
    #
    # In parameterDeclaration a typedef-name cannot be entered.
    #
    $log->debugf('[Lexeme %s "%s"?] parameterDeclaration context: a typedef-name cannot be entered', $lexeme, $lexeme_value);
    $rc = 0;
  }

  return($rc);
}

##################
# _canEnterTypedef
##################
sub _canEnterTypedef {
  my ($self, $event) = @_;

  my $rc = 1;
  if ($self->{_nbStructDeclarationList} > 0) {
    #
    # In structDeclaratorparameterDeclaration a typedef-name cannot be entered.
    #
    $log->debugf('[Event %s] structDeclarationList context: parse symbol activity is suspended', $event);
    $rc = 0;
  }

  return($rc);
}

##############################
# _canEnterEnumerationConstant
##############################
sub _canEnterEnumerationConstant {
  my ($self, $lexeme, $lexeme_value) = @_;

  my $rc = 1;
  $log->debugf('[Lexeme %s "%s"?] enumeration constant can be entered', $lexeme, $lexeme_value);

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

=head1 SEE ALSO

L<Log::Any>, L<MarpaX::Languages::C::AST::Grammar>, L<MarpaX::Languages::C::AST::Impl>, L<MarpaX::Languages::C::AST::Scope>

=head1 AUTHOR

Jean-Damien Durand, C<< <jeandamiendurand at free.fr> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-marpax-language-c-ast at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=MarpaX-Languages-C-AST>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc MarpaX::Languages::C::AST


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=MarpaX-Languages-C-AST>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/MarpaX-Languages-C-AST>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/MarpaX-Languages-C-AST>

=item * Search CPAN

L<http://search.cpan.org/dist/MarpaX-Languages-C-AST/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2013 Jean-Damien Durand.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut

1; # End of MarpaX::Languages::C::AST
