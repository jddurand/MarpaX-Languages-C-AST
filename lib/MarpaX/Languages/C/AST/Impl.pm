package MarpaX::Languages::C::AST::Impl;

use strict;
use warnings FATAL => 'all';
use MarpaX::Languages::C::AST::Util qw/traceAndUnpack/;
use Marpa::R2 2.056000;
use Carp qw/croak/;
use MarpaX::Languages::C::AST::Impl::Logger;
use Log::Any qw/$log/;
use constant {LATEST_G1_EARLEY_SET_ID => -1};
use constant {DOT_PREDICTION => 0, DOT_COMPLETION => -1};
use Exporter 'import';

our @EXPORT_OK = qw/DOT_PREDICTION DOT_COMPLETION LATEST_G1_EARLEY_SET_ID/;

our $MARPA_TRACE_FILE_HANDLE;
our $MARPA_TRACE_BUFFER;

sub BEGIN {
    #
    ## We do not want Marpa to pollute STDERR
    #
    ## Autovivify a new file handle
    #
    open($MARPA_TRACE_FILE_HANDLE, '>', \$MARPA_TRACE_BUFFER);
    if (! defined($MARPA_TRACE_FILE_HANDLE)) {
      croak "Cannot create temporary file handle to tie Marpa logging, $!\n";
    } else {
      if (! tie ${$MARPA_TRACE_FILE_HANDLE}, 'MarpaX::Languages::C::AST::Impl::Logger') {
        croak "Cannot tie $MARPA_TRACE_FILE_HANDLE, $!\n";
        if (! close($MARPA_TRACE_FILE_HANDLE)) {
          croak "Cannot close temporary file handle, $!\n";
        }
        $MARPA_TRACE_FILE_HANDLE = undef;
      }
    }
}

=head1 NAME

MarpaX::Languages::C::AST::Impl - Implementation of Marpa's interface

=head1 VERSION

Version 0.02

=cut

our $VERSION = '0.02';


=head1 SYNOPSIS

This modules implements all needed Marpa calls using its Scanless interface. Please be aware that logging is done via Log::Any.

Example:

    use MarpaX::Languages::C::AST::Impl;

    my $marpaImpl = MarpaX::Languages::C::AST::Impl->new();

=head1 EXPORTS

The constants DOT_PREDICTION (0), DOT_COMPLETION (-1) and LATEST_EARLEY_SET_ID (-1) are exported on demand.

=head1 SUBROUTINES/METHODS

Please note that except for the new, findInProgress, findInProgressShort, g1Describe and inspectG1 methods, all other methods maps directy to Marpa, passing all arguments as is. Therefore only the eventual arguments to new, findInProgress and inspectG1 are documented. Please see Marpa documentation for the other methods whenever needed.

=head2 new($class, $grammarOptionsHashp, $recceOptionsHashp)

Instanciate a new object. Takes as parameter two references to hashes: the grammar options, the recognizer options. In the recognizer, there is a grammar internal option that will be forced to the grammar object. If the environment variable MARPA_TRACE_TERMINALS is setted to a true value, then internal Marpa trace on terminals is activated. If the environment MARPA_TRACE_VALUES is setted to a true value, then internal Marpa trace on values is actived. If the environment variable MARPA_TRACE is setted to a true value, then both terminals et values internal Marpa traces are activated.

=cut

sub new {

  my ($class, $grammarOptionsHashp, $recceOptionsHashp) = @_;

  my $self  = {
      _cacheRule => {}
  };
  $self->{grammar} = Marpa::R2::Scanless::G->new($grammarOptionsHashp);
  if (defined($recceOptionsHashp)) {
      $recceOptionsHashp->{grammar} = $self->{grammar};
  } else {
      $recceOptionsHashp = {grammar => $self->{grammar}};
  }
  $recceOptionsHashp->{trace_terminals} = $ENV{MARPA_TRACE_TERMINALS} || $ENV{MARPA_TRACE} || 0;
  $recceOptionsHashp->{trace_values} = $ENV{MARPA_TRACE_VALUES} || $ENV{MARPA_TRACE} || 0;
  $recceOptionsHashp->{trace_file_handle} = $MARPA_TRACE_FILE_HANDLE;
  $self->{recce} = Marpa::R2::Scanless::R->new($recceOptionsHashp);
  bless($self, $class);

  return $self;
}

=head2 findInProgress($self, $earleySetId, $wantedRuleId, $wantedDotPosition, $wantedOrigin, $wantedLhs, $wantedRhsp, $fatalMode, $indicesp, $matchesp, $originsp) = @_;

Searches a rule at G1 Earley Set Id $earleySetId. The default Earley Set Id is the current one. $wantedRuleId, if defined, is the rule ID. $wantedDotPosition, if defined, is the dot position, that should be a number between 0 and the number of RHS, or -1. $wantedOrigin, if defined, is the wanted origin. In case $wantedRuleId is undef, the user can use $wantedLhs and/or $wantedRhs. $wantedLhs, if defined, is the LHS name. $wantedRhsp, if defined, is the list of RHS. $fatalMode, if defined and true, will mean the module will croak if there is a match. $indicesp, if defined, must be a reference to an array giving the indices from Marpa's output we are interested in. $matchesp, if defined, has to be a reference to an array, which will be filled reference to hashes like {ruleId=>$ruleId, dotPosition=>$dotPosition, origin=>$origin} that matched. You can use the method g1Describe() to fill it. $originsp, if defined, have to be a reference to a hash that will be filled with Earley Set Id origins as a key if the later is not already in it, the value number will be 0 when the key is created, regardless if there is a match or not. Any origin different than $earleySetId and zero (the root) will putted in it. We remind that for a prediction, origin is always the same as the location of the current report. The shortcuts DOT_PREDICTION (0) and DOT_COMPLETION (-1) can be used if the caller import it. There is a special case: if $dotPrediction is defined, $wantedLhs is defined, and $wantedRhsp is undef then, if $dotPrediction is DOT_PREDICTION we will search any prediction of $wantedLhs, and if $dotPrediction is DOT_COMPLETION we will search any completion of $wantedLhs. This method will return a true value if there is at least one match. If one of $wantedRuleId, $wantedDotPosition, $wantedOrigin, $wantedLhs, or $wantedRhsp is defined and there is match, this method stop at the first match.

=cut

sub findInProgress {
    my $self = shift;

    my $args = traceAndUnpack(['earleySetId', 'wantedRuleId', 'wantedDotPosition', 'wantedOrigin', 'wantedLhs', 'wantedRhsp', 'fatalMode', 'indicesp', 'matchesp', 'originsp'], @_);

    $args->{fatalMode} ||= 0;

    my $rc = 0;

    my $i = 0;
    foreach (@{$self->progress($args->{earleySetId})}) {
	my ($ruleId, $dotPosition, $origin) = @{$_};
	if ($origin != 0 && defined($args->{originsp}) && ! exists($args->{originsp}->{$origin})) {
          $args->{originsp}->{$origin} = 0;
	}
	next if (defined($args->{indicesp}) && ! grep {$_ == $i} @{$args->{indicesp}});
	#
	# There two special cases in findInprogress:
	# if $wantedDotPrediction == DOT_PREDICTION, $args->{wantedLhs} is defined, and $args->{wantedRhsp} is undef
	#   => we are searching any dot position that is predicting $args->{wantedLhs}
	# if $wantedDotPrediction == DOT_COMPLETION, $args->{wantedLhs} is defined, and $args->{wantedRhsp} is undef
	#   => we are searching any dot position that is following lhs
	#
	my $found = 0;
	my ($lhs, @rhs);
	if (defined($args->{wantedDotPosition}) && defined($args->{wantedLhs}) && ! defined($args->{wantedRhsp})) {
	    if ($args->{wantedDotPosition} == DOT_PREDICTION) {
		($lhs, @rhs) = $self->rule($ruleId);
		if ($dotPosition != DOT_COMPLETION && $rhs[$dotPosition] eq $args->{wantedLhs}) {
		    $found = 1;
		}
	    } elsif ($args->{wantedDotPosition} == DOT_COMPLETION) {
		($lhs, @rhs) = $self->rule($ruleId);
		if ($dotPosition != DOT_PREDICTION) {
		    if ($dotPosition != DOT_COMPLETION) {
			if ($rhs[$dotPosition-1] eq $args->{wantedLhs}) {
			    $found = 1;
			} elsif ($rhs[$dotPosition] eq $args->{wantedLhs}) { # indice -1
			    $found = 1;
			}
		    }
		}
	    }
	}
	if (! $found) {
	    next if (defined($args->{wantedRuleId}) && ($ruleId != $args->{wantedRuleId}));
	    next if (defined($args->{wantedDotPosition}) && ($dotPosition != $args->{wantedDotPosition}));
	    next if (defined($args->{wantedOrigin}) && ($origin != $args->{wantedOrigin}));
	    ($lhs, @rhs) = $self->rule($ruleId);
	    next if (defined($args->{wantedLhs}) && ($lhs ne $args->{wantedLhs}));
	    next if (defined($args->{wantedRhsp}) && ! __PACKAGE__->_arrayEq(\@rhs, $args->{wantedRhsp}));
	    $found = 1;
	}
	next if (! $found);
	if ($args->{fatalMode}) {
	    my $msg = sprintf('%s', $self->_sprintfDotPosition($args->{earleySetId}, $i, $dotPosition, $lhs, @rhs));
	    fatalf($msg);
	    croak($msg);
	} else {
	    $log->debugf('Match on [ruleId=%d, dotPosition=%d, origin=%d] : %s', $ruleId, $dotPosition, $origin, $self->_sprintfDotPosition($args->{earleySetId}, $i, $dotPosition, $lhs, @rhs));
            push(@{$args->{matchesp}}, {ruleId => $ruleId, dotPosition => $dotPosition, origin => $origin}) if (defined($args->{matchesp}));
	}
	if (defined($args->{wantedRuleId}) ||
            defined($args->{wantedDotPosition}) ||
	    defined($args->{wantedOrigin}) ||
            defined($args->{wantedLhs}) ||
	    defined($args->{wantedRhsp})) {
	    $rc = 1;
	    last;
	}
	++$i;
    }

    return($rc);
}

=head2 findInProgressShort($self, $earleySetId, $wantedDotPosition, $wantedLhs, $wantedRhsp)

This method is shortcut to findInProgress(), that will force findInProgress()'s parameters $wantedRuleId, $wantedOrigin, $fatalMode, $indicesp and $matchesp to an undef value. This method exist because, in practice, only Earley Set Id, dot position, lhs or rhs are of interest.

=cut

sub findInProgressShort {
  my $self = shift;

  my $args = traceAndUnpack(['earleySetId', 'wantedDotPosition', 'wantedLhs', 'wantedRhsp'], @_);

  return $self->findInProgress($args->{earleySetId}, undef, $args->{wantedDotPosition}, undef, $args->{wantedLhs}, $args->{wantedRhsp}, undef, undef, undef, undef);
}

=head2 g1Describe($self, $earleySetId, $indicesp, $matchesp)

Given a $g1, search for the $ruleId, $dotPosition, $origin that correspond to the lines returned by Marpa's progress() output. If $indicesp is defined it must be a reference to an array of wanted indices. If $matchesp is defined, it must be a reference to an array. For instance $self->g1Describe($earleySetId, undef, $matchesp) returns all lines, while $self->g1Describe($earleySetId, [0], $matchesp) returns the first line. This method is in reality just a shortcut to findInProgress(). Typically the $matchesp can be reinjected into the method inspectG1(). See method findInProgress() for the description of what will be put in $matchesp.

=cut

sub g1Describe {
  my $self = shift;

  my $args = traceAndUnpack(['earleySetId', 'indicesp', 'matchesp'], @_);

  return $self->findInProgress($args->{earleySetId}, undef, undef, undef, undef, undef, undef, $args->{indicesp}, $args->{matchesp}, undef);
}


=head2 inspectG1($self, $lexeme, $g1, $startG1p, $endG1p, $candidateRulep, $matchesInG1p, $endConditionp, $startG1Origins, $endG1Origins)

Inspects G1. Searched for a rule $candidateRulep, corresponding to lexeme $lexeme (send only for tracing purpose), using $matchesInG1p as a match condition from G1 Earley Set Id $g1 and downwards. Downwards means that the origins are scanned until there is a match. The order in which origins are looked at is unspecified. $matchesInG1p is a reference to a hash, where the key is a G1 location, and the value is a reference to an array of references to hash like {ruleId=>$ruleId, dotPosition=>$dotPosition, origin=>$origin}. The match is successful only when $matchesInG1p->{$g1} exist and any the array in it matches as well within the found G1 range. The search will end if $endConditionp is reached. $candidateRulep is a reference to: [dotLocationStart, dotLocationEnd, $lhs, [ @rhs ]]. Content of $candidateRulep is send as is to the method findProgress() using [dotLocation, $lhs, [ @rhs ]] and [dotLocationEnd, $lhs, [@rhs]]. The $endConditionp is reference to: [dotLocation, $lhs, [@rhs]], send as-is to the findProgress() method. If no G1 range is found, this method returns undef. Otherwise it returns the number of matches using $matchesInG1p. $startG1Origins and $endG1Origins are workspaces to avoid deep recursion. They can be setted to undef at the very first call to inspectG1().

=cut

##################################################################
# inspectG1
# rc undef: no candidate rule found
# rc 0    : candidate rule found, no match on $lexeme
# rc 1    : candidate rule found, match on $lexeme
#
# ${$startG1p} would contain the prediction G1 location
# ${$endG1p}   would contain its completion
#
# Take care they can be equal
##################################################################
sub inspectG1 {
    my $self = shift;

    my $args = traceAndUnpack(['lexeme', 'g1', 'startG1p', 'endG1p', 'candidateRulep', 'matchesInG1p', 'endConditionp', 'startG1Originsp', 'endG1Originsp'], @_);

    $args->{g1} = $self->latest_g1_location() if (! defined($args->{g1}));
    $args->{endG1Originsp} = {} if (! defined($args->{endG1Originsp}));
    $args->{startG1Originsp} = {} if (! defined($args->{startG1Originsp}));

    if (! defined($args->{matchesInG1p})) {
	$log->warnf('[G1 %d] Undefined $args->{matchesInG1p}', $args->{g1});
	return(undef);
    }
    if (! defined($args->{candidateRulep})) {
	$log->warnf('[G1 %d] Undefined $args->{candidateRulep}', $args->{g1});
	return(undef);
    }
    if (! defined($args->{endConditionp})) {
	$log->warnf('[G1 %d] Undefined $args->{endConditionp}', $args->{g1});
	return(undef);
    }

    #
    # We flag current G1 to no loop indefinitely in case of recursivity
    #
    $args->{endG1Originsp}->{$args->{g1}} = 1;
    $args->{startG1Originsp}->{$args->{g1}} = 1;

    my ($startG1, $endG1) = (undef, undef);
    my ($startG1Match, $endG1Match) = ([], []);

    my $rc = undef;
    #
    # Search
    #
    my ($dotPredictionStart, $dotPredictionEnd, $lhs, $rhsp) = @{$args->{candidateRulep}};
    $log->debugf('[G1 %d] Searching the end G1', $args->{g1});
    if ($self->findInProgress($args->{g1}, undef, $dotPredictionEnd, undef, $lhs, $rhsp, undef, undef, $endG1Match, $args->{endG1Originsp})) {
      $endG1 = $args->{g1};
      $log->debugf('[G1 %d] End G1 found: %d', $args->{g1}, $endG1);
    } else {
      my @endG1OriginsNotDone = grep {! $args->{endG1Originsp}->{$_}} keys %{$args->{endG1Originsp}};
      my @endG1OriginsDone = grep {$args->{endG1Originsp}->{$_}} keys %{$args->{endG1Originsp}};
      $log->debugf('[G1 %d] endG1Match phase failed', $args->{g1});
      $log->debugf('[G1 %d] Origins already done: %s (endG1Match phase)', $args->{g1}, \@endG1OriginsDone);
      $log->debugf('[G1 %d] Origins not done: %s (endG1Match phase)', $args->{g1}, \@endG1OriginsNotDone);
      my $bestRc = undef;
      foreach (@endG1OriginsNotDone) {
        #
        # The recursive call to inspectG1 could very well have done one of our origin
        #
        if ($args->{endG1Originsp}->{$_}) {
          $log->debugf('[G1 %d] Origin %d skipped (endG1Match phase, already done by a recursive call)', $args->{g1}, $_);
          next;
        }
        if ($self->_endCondition($_, $args->{endConditionp})) {
          $log->debugf('[G1 %d] Origin %d skipped (endG1Match phase, end of condition)', $args->{g1}, $_);
          $args->{endG1Originsp}->{$_} = 1;
          next;
        }
        $log->debugf('[G1 %d] Origin %d try (endG1Match phase)', $args->{g1}, $_);
        my $thisrc = $self->inspectG1($args->{lexeme}, $_, \$startG1, \$endG1, $args->{candidateRulep}, $args->{matchesInG1p}, $args->{endConditionp}, $args->{startG1Originsp}, $args->{endG1Originsp});
        #
        # We give precedence to defined($thisrc)
        #
        if (defined($thisrc)) {
          $rc = $thisrc;
          #
          ## Then to $thisrc
          #
          if ($rc) {
            last;
          }
        }
      }
    }
    if (defined($endG1) && ! defined($startG1)) {
      $log->debugf('[G1 %d] Searching the start G1', $args->{g1});
      if ($self->findInProgress($args->{g1}, undef, $dotPredictionStart, undef, $lhs, $rhsp, undef, undef, $startG1Match, $args->{startG1Originsp})) {
        $startG1 = $args->{g1};
        $log->debugf('[G1 %d] Start G1 found: %d', $args->{g1}, $startG1);
      } else {
        my @startG1OriginsNotDone = grep {! $args->{startG1Originsp}->{$_}} keys %{$args->{startG1Originsp}};
        my @startG1OriginsDone = grep {$args->{startG1Originsp}->{$_}} keys %{$args->{startG1Originsp}};
        $log->debugf('[G1 %d] startG1Match phase failed', $args->{g1});
        $log->debugf('[G1 %d] Origins already done: %s (startG1Match phase)', $args->{g1}, \@startG1OriginsDone);
        $log->debugf('[G1 %d] Origins not done: %s (startG1Match phase)', $args->{g1}, \@startG1OriginsNotDone);
        my $bestRc = undef;
        foreach (@startG1OriginsNotDone) {
          #
          # The recursive call to inspectG1 could very well have done one of our origin
          #
          if ($args->{startG1Originsp}->{$_}) {
            $log->debugf('[G1 %d] Origin %d skipped (startG1Match phase, already done by a recursive call)', $args->{g1}, $_);
            next;
          }
          if ($self->_endCondition($_, $args->{endConditionp})) {
            $log->debugf('[G1 %d] Origin %d skipped (startG1Match phase, end of condition)', $args->{g1}, $_);
            $args->{startG1Originsp}->{$_} = 1;
            next;
          }
          $log->debugf('[G1 %d] Origin %d try (startG1Match phase)', $args->{g1}, $_);
          my $thisrc = $self->inspectG1($args->{lexeme}, $_, \$startG1, undef, $args->{candidateRulep}, $args->{matchesInG1p}, $args->{endConditionp}, $args->{startG1Originsp}, $args->{endG1Originsp});
          #
          # We give precedence to defined($thisrc)
          #
          if (defined($thisrc)) {
            $rc = $thisrc;
            #
            ## Then to $thisrc
            #
            if ($rc) {
              last;
            }
          }
        }
      }
    }
    if (defined($startG1) && defined($endG1) && ! defined($rc)) {
      $rc = 0;
      $log->debugf('[G1 %d] Found G1 range [%d, %d]', $args->{g1}, $startG1, $endG1);
      if ($startG1 > $endG1) {
        my $msg = sprintf('[G1 %d] $startG1 %d > $endG1 %d !?', $args->{g1}, $startG1, $endG1);
        $log->fatalf($msg);
        croak $msg;
      }
      $log->debugf('[G1 %d] Checking matches within range [%d, %d]', $args->{g1}, $startG1, $endG1);
      foreach ($startG1..$endG1) {
        my $g1 = $_;
        if (! exists($args->{matchesInG1p}->{$g1})) {
          $log->debugf('[G1 %d -> range [%d, %d]] No match available at %d', $args->{g1}, $startG1, $endG1, $g1);
          next;
        }
        if (ref($args->{matchesInG1p}->{$g1}) ne 'ARRAY') {
          $log->warnf('[G1 %d -> range [%d, %d]] $args->{matchesInG1p}->{%d} is a reference to \'%s\', should be a reference to \'ARRAY\'', $args->{g1}, $startG1, $endG1, $g1, ref($args->{matchesInG1p}->{$g1}) || '');
          next;
        }
        foreach (@{$args->{matchesInG1p}->{$g1}}) {
          my $description = $_;
          if (ref($description) ne 'HASH') {
            $log->warnf('[G1 %d -> range [%d, %d]] $args->{matchesInG1p}->{%d} contain an item that is a reference to \'%s\', should be a reference to \'HASH\'', $args->{g1}, $startG1, $endG1, $g1, ref($description) || '');
            next;
          }
          if ($self->findInProgress($g1, $description->{ruleId}, $description->{dotPosition}, $description->{origin}, undef, undef, undef, undef, undef, undef)) {
            $log->debugf('[G1 %d -> range [%d, %d]] Successful match %s at level %d', $args->{g1}, $startG1, $endG1, $description, $g1);
            ++$rc;
            last;
          } else {
            $log->debugf('[G1 %d -> range [%d, %d]] Unsuccessful match %s at level %d', $args->{g1}, $startG1, $endG1, $description, $g1);
          }
        }
      }
    }
    if (defined($rc)) {
      if (defined($args->{startG1p})) {
        ${$args->{startG1p}} = $startG1;
      }
      if (defined($args->{endG1p})) {
        ${$args->{endG1p}} = $endG1;
      }
    }

    $log->debugf('[G1 %d] return %s, range [%s, %s]', $args->{g1}, $rc, $startG1, $endG1);
    return($rc);
}

=head2 rule($self, $ruleId)

Returns Marpa's grammar's rule. Because the output of Marpa's rule is a constant for a given $ruleId, any call to this method will use a cached result if it exist, create a cache otherwise.

=cut

sub rule {
    my $self = shift;
    my ($ruleId) = @_;

  #
  # For a given grammar, the conversion ruleid->rule is a constant
  # so it is legal to cache this data
  #
  if (! exists($self->{_cacheRule}->{$ruleId})) {
      my $args = traceAndUnpack(['ruleId'], @_);
      my ($lhs, @rhs) = $self->{grammar}->rule($ruleId);
      $self->{_cacheRule}->{$ruleId} = [ $lhs, @rhs ];
  }
  return @{$self->{_cacheRule}->{$ruleId}};
}

=head2 value($self)

Returns Marpa's recognizer's value.

=cut

sub value {
  my $self = shift;

  my $args = traceAndUnpack([''], @_);

  return $self->{recce}->value();
}

=head2 read($self, $inputp)

Returns Marpa's recognizer's read. Argument is a reference to input.

=cut

sub read {
  my ($self, $inputp) = @_;
  #
  # Well, we know that input is a reference to a string, and do not want its dump in the log...
  #
  my $args = traceAndUnpack(['inputp'], "$inputp");

  return $self->{recce}->read($inputp);
}

=head2 resume($self)

Returns Marpa's recognizer's resume.

=cut

sub resume {
  my $self = shift;

  my $args = traceAndUnpack([''], @_);

  return $self->{recce}->resume();
}

=head2 last_completed($self, $symbol)

Returns Marpa's recognizer's last_completed for symbol $symbol.

=cut

sub last_completed {
  my $self = shift;

  my $args = traceAndUnpack(['symbol'], @_);

  return $self->{recce}->last_completed($args->{symbol});
}

=head2 last_completed_range($self, $symbol)

Returns Marpa's recognizer's last_completed_range for symbol $symbol.

=cut

sub last_completed_range {
  my $self = shift;

  my $args = traceAndUnpack(['symbol'], @_);

  return $self->{recce}->last_completed_range($args->{symbol});
}

=head2 range_to_string($self, $start, $end)

Returns Marpa's recognizer's range_to_string for a start value of $start and an end value of $end.

=cut

sub range_to_string {
  my $self = shift;

  my $args = traceAndUnpack(['start', 'end'], @_);

  return $self->{recce}->range_to_string($args->{start}, $args->{end});
}

=head2 progress($self, $g1)

Returns Marpa's recognizer's progress for a g1 location $g1.

=cut

sub progress {
  my $self = shift;

  my $args = traceAndUnpack(['g1'], @_);

  return $self->{recce}->progress($args->{g1});
}

=head2 event($self, $eventNumber)

Returns Marpa's recognizer's event for event number $eventNumber.

=cut

sub event {
  my $self = shift;

  my $args = traceAndUnpack(['eventNumber'], @_);

  return $self->{recce}->event($args->{eventNumber});
}

=head2 pause_lexeme($self)

Returns Marpa's recognizer's pause_lexeme.

=cut

sub pause_lexeme {
  my $self = shift;

  my $args = traceAndUnpack([''], @_);

  return $self->{recce}->pause_lexeme();
}

=head2 pause_span($self)

Returns Marpa's recognizer's pause_span.

=cut

sub pause_span {
  my $self = shift;

  my $args = traceAndUnpack([''], @_);

  return $self->{recce}->pause_span();
}

=head2 line_column($self, $start)

Returns Marpa's recognizer's line_column at eventual $start location in the input stream. Default location is current location.

=cut

sub line_column {
  my $self = shift;

  my $args = traceAndUnpack(['start'], @_);

  return $self->{recce}->line_column($args->{start});
}

=head2 substring($self, $start, $length)

Returns Marpa's recognizer's substring corresponding to g1 span ($start, $length).

=cut

sub substring {
  my $self = shift;

  my $args = traceAndUnpack(['start', 'length'], @_);

  return $self->{recce}->substring($args->{start}, $args->{length});
}

=head2 lexeme_read($self, $lexeme, $start, $length, $value)

Returns Marpa's recognizer's lexeme_read for lexeme $lexeme, at start position $start, length $length and value $value.

=cut

sub lexeme_read {
  my $self = shift;

  my $args = traceAndUnpack(['lexeme', 'start', 'length', 'value'], @_);

  return $self->{recce}->lexeme_read($args->{lexeme}, $args->{start}, $args->{length}, $args->{value});
}

=head2 latest_g1_location($self)

Returns Marpa's recognizer's latest_g1_location.

=cut

sub latest_g1_location {
  my $self = shift;

  my $args = traceAndUnpack([''], @_);

  return $self->{recce}->latest_g1_location();
}

=head2 g1_location_to_span($self, $g1)

Returns Marpa's recognizer's g1_location_to_span for a g1 location $g1.

=cut

sub g1_location_to_span {
  my $self = shift;

  my $args = traceAndUnpack(['g1'], @_);

  return $self->{recce}->g1_location_to_span($args->{g1});
}

#
# INTERNAL METHODS
#
sub _endCondition {
  my ($self, $g1, $endConditionp) = @_;

  my $rc = 0;
  if (defined($endConditionp)) {
    my ($dotPrediction, $lhs, $rhsp) = @{$endConditionp};
    $rc = $self->findInProgress($g1, undef, $dotPrediction, undef, $lhs, $rhsp, undef, undef, undef, undef);
  }
  return $rc;
}

sub _sprintfDotPosition {
    my ($self, $earleySetId, $i, $dotPosition, $lhs, @rhs) = @_;

    # We insert the 'dot' in the output
    if (defined($dotPosition)) {
	if ($dotPosition >= 0) {
	    splice(@rhs, $dotPosition, 0, '.');
	} else {
	    #
	    # Completion
	    #
	    push(@rhs, '.');
	}
    }
    my $rhs = join(' ', map {if ($_ ne '.') {"<$_>"} else {$_}} @rhs);

    if (defined($earleySetId) && defined($i)) {
	return sprintf('<%s> ::= %s (ordinal %d, indice %d)', $lhs, $rhs, $earleySetId, $i);
    } elsif (defined($earleySetId)) {
	return sprintf('<%s> ::= %s (ordinal %d)', $lhs, $rhs, $earleySetId);
    } elsif (defined($i)) {
	return sprintf('<%s> ::= %s (indice %d)', $lhs, $rhs, $i);
    } else {
	return sprintf('<%s> ::= %s', $lhs, $rhs);
    }
}

sub _arrayEq {
    my ($class, $ap, $bp) = @_;
    my $rc = 1;
    if ($#{$ap} != $#{$bp}) {
	$rc = 0;
    } else {
	foreach (0..$#{$ap}) {
	    if ($ap->[$_] ne $bp->[$_]) {
		$rc = 0;
		last;
	    }
	}
    }
    return($rc);
}

=head1 SEE ALSO

L<Log::Any>, L<Marpa::R2>, L<MarpaX::Languages::C::AST::Impl::Logger>

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

1; # End of MarpaX::Languages::C::AST::Impl
