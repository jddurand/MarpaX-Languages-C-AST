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

Version 0.03

=cut

our $VERSION = '0.03';


=head1 SYNOPSIS

This modules implements all needed Marpa calls using its Scanless interface. Please be aware that logging is done via Log::Any.

Example:

    use MarpaX::Languages::C::AST::Impl;

    my $marpaImpl = MarpaX::Languages::C::AST::Impl->new();

=head1 EXPORTS

The constants DOT_PREDICTION (0), DOT_COMPLETION (-1) and LATEST_EARLEY_SET_ID (-1) are exported on demand.

=head1 SUBROUTINES/METHODS

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
	    $log->tracef('Match on [ruleId=%d, dotPosition=%d, origin=%d] : %s', $ruleId, $dotPosition, $origin, $self->_sprintfDotPosition($args->{earleySetId}, $i, $dotPosition, $lhs, @rhs));
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

=head2 findInProgressShort($self, $wantedDotPosition, $wantedLhs, $wantedRhsp)

This method is shortcut to findInProgress(), that will force findInProgress()'s parameter $earleySetId to -1 (i.e. current G1 location), and $wantedRuleId, $wantedOrigin, $fatalMode, $indicesp, $matchesp to undef. This method exist because, in practice, only dot position, lhs or rhs are of interest within the current G1 location.

=cut

sub findInProgressShort {
  my $self = shift;

  my $args = traceAndUnpack(['wantedDotPosition', 'wantedLhs', 'wantedRhsp'], @_);

  return $self->findInProgress(-1, undef, $args->{wantedDotPosition}, undef, $args->{wantedLhs}, $args->{wantedRhsp}, undef, undef, undef, undef);
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

=head2 literal($self, $start, $length)

Returns Marpa's recognizer's literal.

=cut

sub literal {
  my $self = shift;

  my $args = traceAndUnpack(['start', 'length'], @_);

  return $self->{recce}->literal($args->{start}, $args->{length});
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
