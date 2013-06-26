use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST;

# ABSTRACT: Translate a C source to an AST

use Log::Any qw/$log/;
use Carp qw/croak/;
use MarpaX::Languages::C::AST::Util qw/whoami/;
use MarpaX::Languages::C::AST::Grammar;
use MarpaX::Languages::C::AST::Impl qw/DOT_COMPLETION DOT_PREDICTION/;
use MarpaX::Languages::C::AST::Scope;
use MarpaX::Languages::C::AST::Callback::Events;

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
    $log->infof('%s', $cAstObject->parse(\$cSourceCode));

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
  $self->{_callbackEvents} = MarpaX::Languages::C::AST::Callback::Events->new($self);
  $self->{_referenceToSourceCodep} = undef;
  bless($self, $class);

  return $self;
}

=head2 parse($self, $referenceToSourceCodep, $optionalArrayOfValuesb)

Do the parsing and return the blessed value. Takes as first parameter the reference to a C source code. Takes as optional second parameter a flag saying if the return value should be an array of all values or not. If this flag is false, the module will croak if there more than one parse tree value.

=cut

sub parse {
  my ($self, $referenceToSourceCodep, $optionalArrayOfValuesb) = @_;

  $self->{_referenceToSourceCodep} = $referenceToSourceCodep;
  my $max = length(${$referenceToSourceCodep});
  my $pos = $self->{_impl}->read($referenceToSourceCodep);
  do {
    $self->_doEvents();
    $self->_doLexeme();
  } while (($pos = $self->{_impl}->resume()) < $max);

  return($self->_value($optionalArrayOfValuesb));
}

#
# INTERNAL METHODS
#

#################
# _last_completed
#################
sub _last_completed {
    my ($self, $symbol) = @_;
    return $self->{_impl}->substring($self->{_impl}->last_completed($symbol));
}

####################
# _show_line_and_col
####################
sub _show_line_and_col {
    my ($self, $line_and_colp) = @_;

    my ($line, $col) = @{$line_and_colp};
    my $pointer = ($col > 0 ? '-' x ($col-1) : '') . '^';
    my $content = (split("\n", ${$self->{_referenceToSourceCodep}}))[$line-1];
    $content =~ s/\t/ /g;
    return "$content\n$pointer";
}

##############
# _line_column
##############
sub _line_column {
    my ($self, $g1) = @_;

    $g1 //= $self->{_impl}->current_g1_location();
    my ($start, $length) = $self->{_impl}->g1_location_to_span($g1);
    my ($line, $column) = $self->{_impl}->line_column($start);
    return [ $line, $column ];
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
  my $valuep = $self->{_impl}->value() || $self->_croak($self->_show_last_expression());
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
    $self->_croak('Number of parse tree value should be 1');
  }
  if ($arrayOfValuesb) {
    return [ @rc ];
  } else {
    return $rc[0];
  }
}

###########
# _doEvents
###########
sub _doEvents {
  my $self = shift;

  my %events = ();
  my $iEvent = 0;
  while (defined($_ = $self->{_impl}->event($iEvent++))) {
      next if (! defined($_->[0]));
    ++$events{$_->[0]};
  }

  if (%events) {
    my @events = keys %events;
    $log->debugf('[%s] Events: %s', whoami(__PACKAGE__), \@events);
    $self->{_callbackEvents}->exec(@events);
  }
}

###########
# _doLexeme
###########
#
# We manage ONLY 'before' pause lexemes in here
#
sub _doLexeme {
  my ($self) = @_;

  #
  # Get paused lexeme
  #
  my $lexeme = $self->{_impl}->pause_lexeme();
  return if (! defined($lexeme));

  my @terminals_expected = @{$self->{_impl}->terminals_expected()};

  #
  # Determine the correct lexeme
  #
  my ($start, $length) = $self->{_impl}->pause_span();
  my ($line, $column) = $self->{_impl}->line_column($start);
  my $lexeme_value = $self->{_impl}->literal($start, $length);
  my $newlexeme;
  if ((grep {$_ eq 'TYPEDEF_NAME'} @terminals_expected) && $self->{_scope}->parseIsTypedef($lexeme_value)) {
      $newlexeme = 'TYPEDEF_NAME';
  } elsif ((grep {$_ eq 'ENUMERATION_CONSTANT'} @terminals_expected) && $self->{_scope}->parseIsEnum($lexeme_value)) {
      $newlexeme = 'ENUMERATION_CONSTANT';
  } elsif ((grep {$_ eq 'IDENTIFIER'} @terminals_expected)) {
      $newlexeme = 'IDENTIFIER';
  } else {
      $self->_croak('[%s] Lexeme value "%s" cannot be associated to TYPEDEF_NAME, ENUMERATION_CONSTANT nor IDENTIFIER at position %d:%d', whoami(__PACKAGE__), $lexeme_value, $line, $column);
  }
  #
  # Push the unambiguated lexeme
  #
  $log->debugf('[%s] Pushing lexeme %s "%s"', whoami(__PACKAGE__), $newlexeme, $lexeme_value);
  if (! defined($self->{_impl}->lexeme_read($newlexeme, $start, $length, $lexeme_value))) {
      $self->_croak('[%s] Lexeme value "%s" cannot be associated to lexeme name %s at position %d:%d', whoami(__PACKAGE__), $lexeme_value, $newlexeme, $line, $column);
  }
  #
  # A lexeme_read() can generate an event
  #
  $self->_doEvents();
}

########
# _croak
########
sub _croak {
    my ($self, $fmt, @arg) = @_;

    my $msg = sprintf($fmt, @arg);
    $log->fatalf($msg);
    croak $msg;
}

1;
