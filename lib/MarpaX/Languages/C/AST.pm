use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST;

# ABSTRACT: Translate a C source to an AST

use Log::Any qw/$log/;
use Carp qw/croak/;
use MarpaX::Languages::C::AST::Util qw/:all/;
use MarpaX::Languages::C::AST::Grammar;
use MarpaX::Languages::C::AST::Impl qw/DOT_COMPLETION DOT_PREDICTION/;
use MarpaX::Languages::C::AST::Scope;
use MarpaX::Languages::C::AST::Callback::Events;

# VERSION

=head1 DESCRIPTION

This module translates C source into an AST tree. To assist further processsing of the AST tree, the nodes of the AST are blessed according to the C grammar you have selected. (The default is 'ISO-ANSI-C-2011'.) If you want to enable logging, be aware that this module is a Log::Any thingy.

This module implements the full syntax, as well as those specification constraints which are syntactic in nature: Associativity of nested if-then-else statements is according to the C standards, as is the treatment of names as typedefs, enums, or variable identifiers.

The C standards contain many constraints that are non-syntactic. MarpaX::Languages::C::AST does not implement these, leaving them for AST post-processing. One example of a non-syntactic constraint is the requirement that labeled statements within a function be unique. Another is the requirement that declarations include at most one storage class specifier.

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

=head2 new($class, %options)

Instantiate a new object. Takes as parameter an optional hash of options that can be:

=over

=item grammarName

Name of a grammar. Default is 'ISO-ANSI-C-2011'.

=item logInfo

Issue logging with level 'info' for every found lexeme. Default is 0 (i.e. false). Please note that logging is done through Log::Any.

=item lexemeCallback

Array reference containing a CODE ref and optional arguments. This callback will be trigerred like this: &$CODE(@arguments, $lexemeHashp), where $lexemeHashp is a reference to a hash describing current lexeme:

=over

=item name

Name of the lexeme. You have to refer to the grammar used to get its definition, although this is usually self-explanatory.

=item start

G1 (Marpa term) start location.

=item length

Length of the lexeme

=item line

Line number in the source being parsed.

=item column

Column number in the source being parsed.

=item value

String containing lexeme value.

=back

=back

=cut

# ----------------------------------------------------------------------------------------
sub new {
  my ($class, %opts) = @_;

  my $grammarName = $opts{grammarName} || 'ISO-ANSI-C-2011';

  my $grammar = MarpaX::Languages::C::AST::Grammar->new($grammarName);
  my $grammar_option = $grammar->grammar_option();
  $grammar_option->{bless_package} = 'C::AST';
  $grammar_option->{source} = \$grammar->content();
  my $recce_option = $grammar->recce_option();

  my $lexemeCallback = $opts{lexemeCallback} || '';
  my @lexemeCallbackArgs = ();
  if ($opts{lexemeCallback}) {
      if (ref($opts{lexemeCallback}) ne 'ARRAY') {
	  croak 'lexemeCallback option must be an ARRAY reference';
      }
      if (! @{$opts{lexemeCallback}}) {
	  croak 'lexemeCallback is a reference to an empty array';
      }
      if (ref($opts{lexemeCallback}->[0]) ne 'CODE') {
	  croak 'lexemeCallback must start with a CODE reference';
      }
      @lexemeCallbackArgs = @{$opts{lexemeCallback}};
      $lexemeCallback = shift(@lexemeCallbackArgs);
  }

  my $self  = {
               _scope              => MarpaX::Languages::C::AST::Scope->new(),
               _grammar            => $grammar,
               _impl               => MarpaX::Languages::C::AST::Impl->new($grammar_option, $recce_option),
               _sourcep            => undef,
	       _lexemeCallback     => $lexemeCallback,
	       _lexemeCallbackArgs => \@lexemeCallbackArgs,
	       _logInfo            => $opts{logInfo} || 0
              };

  bless($self, $class);

  return $self;
}

=head2 parse($self, $sourcep, $optionalArrayOfValuesb)

Do the parsing and return the blessed value. Takes as first parameter the reference to a C source code. Takes as optional second parameter a flag saying if the return value should be an array of all values or not. If this flag is false, the module will croak if there more than one parse tree value.

=cut

# ----------------------------------------------------------------------------------------
sub parse {
  my ($self, $sourcep, $optionalArrayOfValuesb) = @_;

  $self->{_sourcep} = $sourcep;
  $self->{_callbackEvents} = MarpaX::Languages::C::AST::Callback::Events->new($self);

  my $max = length(${$sourcep});
  my $pos = $self->{_impl}->read($sourcep);
  do {
    my %lexeme = ();
    $self->_getLexeme(\%lexeme);
    $self->_doScope(\%lexeme);
    $self->_doEvents();
    $self->_doPauseAfterLexeme(\%lexeme);
    $self->_doLogInfo(\%lexeme);
    $self->_doLexemeCallback(\%lexeme);
  } while (($pos = $self->{_impl}->resume()) < $max);

  return($self->_value($optionalArrayOfValuesb));
}

# ----------------------------------------------------------------------------------------
sub _show_last_expression {
  my ($self) = @_;

  my ($start, $end) = $self->{_impl}->last_completed_range('translationUnit');
  return 'No expression was successfully parsed' if (! defined($start));
  my $lastExpression = $self->{_impl}->range_to_string($start, $end);
  return "Last expression successfully parsed was: $lastExpression";
}
# ----------------------------------------------------------------------------------------
sub _value {
  my ($self, $arrayOfValuesb) = @_;

  $arrayOfValuesb ||= 0;

  my @rc = ();
  my $nvalue = 0;
  my $valuep = $self->{_impl}->value() || logCroak('%s', $self->_show_last_expression());
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
    logCroak('Number of parse tree value should be %d', 1);
  }
  if ($arrayOfValuesb) {
    return [ @rc ];
  } else {
    return $rc[0];
  }
}
# ----------------------------------------------------------------------------------------
sub _doEvents {
  my $self = shift;

  my %events = ();
  my $iEvent = 0;
  while (defined($_ = $self->{_impl}->event($iEvent++))) {
    ++$events{$_->[0]};
  }

  if (%events) {
    my @events = keys %events;
    $log->debugf('[%s] Events: %s', whoami(__PACKAGE__), \@events);
    $self->{_callbackEvents}->exec(@events);
  }
}
# ----------------------------------------------------------------------------------------
sub _getLexeme {
  my ($self, $lexemeHashp) = @_;

  #
  # Get paused "before" lexeme
  #
  my $lexeme = $self->{_impl}->pause_lexeme();
  if (defined($lexeme)) {
    $lexemeHashp->{name} = $lexeme;
    ($lexemeHashp->{start}, $lexemeHashp->{length}) = $self->{_impl}->pause_span();
    ($lexemeHashp->{line}, $lexemeHashp->{column}) = $self->{_impl}->line_column($lexemeHashp->{start});
    $lexemeHashp->{value} = $self->{_impl}->literal($lexemeHashp->{start}, $lexemeHashp->{length});
  }
}
# ----------------------------------------------------------------------------------------
sub _doLogInfo {
  my ($self, $lexemeHashp) = @_;

  if ($self->{_logInfo} && exists($lexemeHashp->{name})) {
    $log->infof("[%8d:%3d] %-30s %s", $lexemeHashp->{line}, $lexemeHashp->{column}, $lexemeHashp->{name}, $lexemeHashp->{value});
  }
}
# ----------------------------------------------------------------------------------------
sub _doLexemeCallback {
  my ($self, $lexemeHashp) = @_;

  if ($self->{_lexemeCallback} && exists($lexemeHashp->{name})) {
      my $callback = $self->{_lexemeCallback};
      &$callback(@{$self->{_lexemeCallbackArgs}}, $lexemeHashp);
  }
}
# ----------------------------------------------------------------------------------------
sub _doScope {
  my ($self, $lexemeHashp) = @_;

  #
  # Get paused lexeme
  #
  if (exists($lexemeHashp->{name})) {

    my $lexemeFormatString = "%s \"%s\" at position %d:%d";
    my @lexemeCommonInfo = ($lexemeHashp->{name}, $lexemeHashp->{value}, $lexemeHashp->{line}, $lexemeHashp->{column});

    if (defined($self->{_callbackEvents}->topic_fired_data('fileScopeDeclarator'))) {
      if ($self->{_callbackEvents}->topic_fired_data('fileScopeDeclarator')->[0] == -1) {
        #
        # This will be for next round.
        #
        $log->debugf('[%s] fileScopeDeclarator: flagging lookup required at next round.', whoami(__PACKAGE__));
        $self->{_callbackEvents}->topic_fired_data('fileScopeDeclarator')->[0] = 1;

      } elsif ($self->{_callbackEvents}->topic_fired_data('fileScopeDeclarator')->[0] == 1) {
        #
        # Lookup what follows the file-scope declarator
        #
        if ($lexemeHashp->{name} ne 'COMMA' &&
            $lexemeHashp->{name} ne 'SEMICOLON' &&
            $lexemeHashp->{name} ne 'EQUAL') {
          $log->debugf('[%s] fileScopeDeclarator: next lexeme is %s, flagging reenterScope.', whoami(__PACKAGE__), $lexemeHashp->{name});
          $self->{_callbackEvents}->topic_fired_data('reenterScope')->[0] = 1;
        }
        #
        # Flag lookup done
        #
        $log->debugf('[%s] fileScopeDeclarator: flagging lookup done.', whoami(__PACKAGE__));
        $self->{_callbackEvents}->topic_fired_data('fileScopeDeclarator')->[0] = 0;
      }
    }

    if ($lexemeHashp->{name} eq 'LCURLY_SCOPE' || $lexemeHashp->{name} eq 'LPAREN_SCOPE') {
      $log->debugf('[%s] $lexemeFormatString: entering scope.', whoami(__PACKAGE__), @lexemeCommonInfo);
      $self->{_scope}->parseEnterScope();
    } elsif ($lexemeHashp->{name} eq 'RCURLY_SCOPE' || $lexemeHashp->{name} eq 'RPAREN_SCOPE') {
      if ($self->{_scope}->parseScopeLevel == 1) {
        $log->debugf('[%s] $lexemeFormatString: delay leaving scope.', whoami(__PACKAGE__), @lexemeCommonInfo);
        $self->{_scope}->parseExitScope(0);
      } else {
        $log->debugf('[%s] $lexemeFormatString: immediate leaving scope.', whoami(__PACKAGE__), @lexemeCommonInfo);
        $self->{_scope}->parseExitScope(1);
      }
    } else {
      $log->debugf('[%s] $lexemeFormatString.', whoami(__PACKAGE__), @lexemeCommonInfo);
      if ($self->{_scope}->parseScopeLevel == 1 && $self->{_scope}->parseDelay) {
        if (defined($self->{_callbackEvents}->topic_fired_data('reenterScope')) &&
            $self->{_callbackEvents}->topic_fired_data('reenterScope')->[0]) {
          $log->debugf('[%s] reenterScope flag is on at scope 1.', whoami(__PACKAGE__));
          $self->{_scope}->parseReenterScope();
          $log->debugf('[%s] Unflagging reenterScope.', whoami(__PACKAGE__));
          $self->{_callbackEvents}->topic_fired_data('reenterScope')->[0] = 0;
        } else {
          $log->debugf('[%s] reenterScope flag is off at scope 1.', whoami(__PACKAGE__));
          $self->{_scope}->doExitScope();
        }
      }
    }
  }
}
# ----------------------------------------------------------------------------------------
sub _doPauseAfterLexeme {
  my ($self, $lexemeHashp) = @_;

  #
  # Get paused lexeme
  #
  if (exists($lexemeHashp->{name})) {
      #
      # pause start lexemes
      #
      if ($lexemeHashp->{name} eq 'TYPEDEF_NAME' ||
          $lexemeHashp->{name} eq 'ENUMERATION_CONSTANT' ||
          $lexemeHashp->{name} eq 'IDENTIFIER') {
	  my @terminals_expected = @{$self->{_impl}->terminals_expected()};
	  #
	  # Determine the correct lexeme
	  #
	  my $newlexeme;
	  if ((grep {$_ eq 'TYPEDEF_NAME'} @terminals_expected) && $self->{_scope}->parseIsTypedef($lexemeHashp->{value})) {
	      $newlexeme = 'TYPEDEF_NAME';
	  } elsif ((grep {$_ eq 'ENUMERATION_CONSTANT'} @terminals_expected) && $self->{_scope}->parseIsEnum($lexemeHashp->{value})) {
	      $newlexeme = 'ENUMERATION_CONSTANT';
	  } elsif ((grep {$_ eq 'IDENTIFIER'} @terminals_expected)) {
	      $newlexeme = 'IDENTIFIER';
	  } else {
	      logCroak('[%s] Lexeme value "%s" cannot be associated to TYPEDEF_NAME, ENUMERATION_CONSTANT nor IDENTIFIER at line %d, column %d', whoami(__PACKAGE__), $lexemeHashp->{value}, $lexemeHashp->{line}, $lexemeHashp->{column});
	  }
	  #
	  # Push the unambiguated lexeme
	  #
	  $log->debugf('[%s] Pushing lexeme %s "%s"', whoami(__PACKAGE__), $newlexeme, $lexemeHashp->{value});
	  if (! defined($self->{_impl}->lexeme_read($newlexeme, $lexemeHashp->{start}, $lexemeHashp->{length}, $lexemeHashp->{value}))) {
	      logCroak('[%s] Lexeme value "%s" cannot be associated to lexeme name %s at position %d:%d', whoami(__PACKAGE__), $lexemeHashp->{value}, $newlexeme, $lexemeHashp->{line}, $lexemeHashp->{column});
	  }
          $lexemeHashp->{name} = $newlexeme;
	  #
	  # A lexeme_read() can generate an event
	  #
	  $self->_doEvents();
      }
  }
}

=head1 SEE ALSO

L<Log::Any>, L<Marpa::R2>

=cut

1;
