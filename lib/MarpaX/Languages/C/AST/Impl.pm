package MarpaX::Languages::C::AST::Impl;

use 5.006;
use strict;
use warnings FATAL => 'all';
use Marpa::R2 2.055_003;
use Carp qw/croak/;
use MarpaX::Languages::C::AST::Impl::Logger;
use Log::Any qw/$log/;
use constant {LATEST_G1_EARLEY_SET_ID => -1};
use constant {DOT_PREDICTION => 0, DOT_COMPLETION => -1};
use Exporter 'import';

our @EXPORT_OK = qw/DOT_PREDICTION DOT_COMPLETION LATEST_G1_EARLEY_SET_ID/;

=head1 NAME

MarpaX::Languages::C::AST::Impl - Implementation of Marpa's interface

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

This modules implements all needed Marpa calls using its Scanless interface. Please be aware that logging is done via Log::Any.

Example:

    use MarpaX::Languages::C::AST::Impl;

    my $marpaImpl = MarpaX::Languages::C::AST::Impl->new();

=head1 EXPORTS

The constants DOT_PREDICTION (0), DOT_COMPLETION (-1) and LATEST_EARLEY_SET_ID (-1) are exported on demand.

=head1 SUBROUTINES/METHODS

Please note that except for the new, findInProgress, inspectG1 methods, all other methods maps directy to Marpa, passing all arguments as is. Therefore only the eventual arguments to new, findInProgress and inspectG1 are documented. Please see Marpa documentation for the other methods whenever needed.

=head2 new

Instanciate a new object. Takes as parameter two references to hashes: the grammar options, the recognizer options. In the recognizer, there is a grammar internal option that will be forced to the grammar object.

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
  $self->{recce} = Marpa::R2::Scanless::R->new($recceOptionsHashp);
  bless($self, $class);

  return $self;
}

=head2 findInProgress($earleySetId, $wantedDotPosition, $wantedLhs, $wantedRhsp, $fatalMode)

Searches a rule at G1 Earley Set Id $earleySetId. The default Earley Set Id is the current one. $wantedLhs, if defined, is the LHS name. $wantedRhsp, if defined, is the list of RHS. $wantedDotPosition, if defined, is the dot position, that should be a number between 0 and the number of RHS, or -1. The shortcuts DOT_PREDICTION (0) and DOT_COMPLETION (-1) can be used if the caller import it. There is a special case: if $dotPrediction is defined, $wantedLhs is defined, and $wantedRhsp is undef then, if $dotPrediction is DOT_PREDICTION we will search any prediction of $wantedLhs, and if $dotPrediction is DOT_COMPLETION we will search any completion of $wantedLhs. This method will return a true value if there is a match.

=cut

sub findInProgress {
    my ($self, $earleySetId, $wantedDotPosition, $wantedLhs, $wantedRhsp, $fatalMode) = @_;

    $fatalMode ||= 0;

    $log->debugf('findInProgress(%d, %s, "%s", %s, %d)', $earleySetId, $wantedDotPosition, $wantedLhs, $wantedRhsp, $fatalMode);

    my $rc = 0;

    my $i = 0;
    foreach (@{$self->progress($earleySetId)}) {
	my ($rule_id, $dotPosition, $origin) = @{$_};
	#
	# There two special cases in findInprogress:
	# if $wantedDotPrediction == DOT_PREDICTION, $wantedLhs is defined, and $wantedRhsp is undef
	#   => we are searching any dot position that is predicting $wantedLhs
	# if $wantedDotPrediction == DOT_COMPLETION, $wantedLhs is defined, and $wantedRhsp is undef
	#   => we are searching any dot position that is following lhs
	#
	my $found = 0;
	my ($lhs, @rhs);
	if (defined($wantedDotPosition) && defined($wantedLhs) && ! defined($wantedRhsp)) {
	    if ($wantedDotPosition == DOT_PREDICTION) {
		($lhs, @rhs) = $self->rule($rule_id);
		if ($dotPosition != DOT_COMPLETION && $rhs[$dotPosition] eq $wantedLhs) {
		    $found = 1;
		}
	    } elsif ($wantedDotPosition == DOT_COMPLETION) {
		($lhs, @rhs) = $self->rule($rule_id);
		if ($dotPosition != DOT_PREDICTION) {
		    if ($dotPosition != DOT_COMPLETION) {
			if ($rhs[$dotPosition-1] eq $wantedLhs) {
			    $found = 1;
			} elsif ($rhs[$dotPosition] eq $wantedLhs) { # indice -1
			    $found = 1;
			}
		    }
		}
	    }
	}
	if (! $found) {
	    next if (defined($wantedDotPosition) && ($dotPosition != $wantedDotPosition));
	    ($lhs, @rhs) = $self->rule($rule_id);
	    next if (defined($wantedLhs) && ($lhs ne $wantedLhs));
	    next if (defined($wantedRhsp) && ! __PACKAGE__->_arrayEq(\@rhs, $wantedRhsp));
	    $found = 1;
	}
	next if (! $found);
	if ($fatalMode) {
	    my $msg = sprintf('%s', $self->_sprintfDotPosition($earleySetId, $i, $dotPosition, $lhs, @rhs));
	    fatalf($msg);
	    croak($msg);
	} else {
	    $log->tracef('Match on: %s', $self->_sprintfDotPosition($earleySetId, $i, $dotPosition, $lhs, @rhs));
	}
	if (defined($wantedDotPosition) ||
	    defined($wantedLhs) ||
	    defined($wantedRhsp)) {
	    $rc = 1;
	    last;
	}
	++$i;
    }

    return($rc);
}

=head2 inspectG1($lexeme, $g1_location, $start_g1_locationp, $end_g1_locationp, $candidateRulesp, $matchesInG1p, $endConditionp)

Inspects G1. Searched for a lexeme with name $lexeme, using $matchedInG1p as a match condition, from G1 Earley Set Id $g1_location and downwards. $matchesG1p is a reference to hash, where the key is a G1 location, and the value is 0 or 1. The match is successful only when $matchesG1p->{$g1} exist and has a true value. This match appear within $candidateRulesp and the search will end if $endConditionp is reached. $candidateRulesp is a reference to an array of arrays, where each later array has the form: [dotLocationStart, dotLocationEnd, $lhs, [ @rhs ]]. These are send as is to the method findProgress() using [dotLocation, $lhs, [ @rhs ]] and [dotLocationEnd, $lhs, [@rhs]]. The $endConditionp is an array of arrays, where each later array has the form [dotLocation, $lhs, [@rhs]], send as-is to the findProgress() method. If there is no match, this method returns undef. If there a match on $candidateRulesp but no match on $lexeme, it returns a false value. If there is match on both it returns a true value.

=cut

##################################################################
# inspectG1
# rc undef: no candidate rule found
# rc 0    : candidate rule found, no match on $lexeme
# rc 1    : candidate rule found, match on $lexeme
#
# ${ $start_g1_locationp} would contain the prediction G1 location
# ${ $end_g1_locationp}   would contain its completion
#
# Take care they can be equal
##################################################################
sub inspectG1 {
    my ($self, $lexeme, $g1_location, $start_g1_locationp, $end_g1_locationp, $candidateRulesp, $matchesInG1p, $endConditionp) = @_;

    $g1_location ||= $self->latest_g1_location();

    my ($start_g1_location, $end_g1_location) = (undef, undef);
    my $indexInCandidates = 0;
    my $end_condition = 0;
    my $rc = undef;
    while (1) {
	#
	# Search
	#
	if (! defined($end_g1_location) && defined($candidateRulesp)) {
	    my $i = 0;
	    foreach (@{$candidateRulesp}) {
		my ($dotPredictionStart, $dotPredictionEnd, $lhs, $rhsp) = @{$_};
		if ($self->findInProgress($g1_location, $dotPredictionEnd, $lhs, $rhsp, 0)) {
		    $end_g1_location = $g1_location;
		    $indexInCandidates = $i;
		    last;
		}
		++$i;
	    }
	}
	if (defined($end_g1_location) && ! defined($start_g1_location) && defined($candidateRulesp)) {
	    my ($dotPredictionStart, $dotPredictionEnd, $lhs, $rhsp) = @{$candidateRulesp->[$indexInCandidates]};
	    if ($self->findInProgress($g1_location, $dotPredictionStart, $lhs, $rhsp, 0)) {
		$start_g1_location = $g1_location;
	    }
	}
	if (defined($start_g1_location) && defined($end_g1_location)) {
	    $rc = 0;
	    $log->tracef('G1 range [%d, %d]', $start_g1_location, $end_g1_location);
	    if ($start_g1_location > $end_g1_location) {
		my $msg = sprintf('$start_g1_location %d > $end_g1_location %d !?', $start_g1_location, $end_g1_location);
		$log->fatalf($msg);
		croak $msg;
	    }
	    if (grep {exists($matchesInG1p->{$_}) && $matchesInG1p->{$_}} ($start_g1_location..$end_g1_location)) {
		$log->tracef('G1 range [%d, %d] have %s', $start_g1_location, $end_g1_location, $lexeme);
		if (defined($start_g1_locationp)) {
		    ${$start_g1_locationp} = $start_g1_location;
		}
		if (defined($end_g1_locationp)) {
		    ${$end_g1_locationp} = $end_g1_location;
		}
		$rc = 1;
		last;
	    } else {
		$log->tracef('G1 range [%d, %d] do not have %s', $start_g1_location, $end_g1_location, $lexeme);
	    }
	    $start_g1_location = undef;
	    $end_g1_location = undef;
	}
	#
	# End condition
	if (defined($endConditionp)) {
	    foreach (@{$endConditionp}) {
		my ($dotPrediction, $lhs, $rhsp) = @{$_};
		if ($self->findInProgress($g1_location, $dotPrediction, $lhs, $rhsp, 0)) {
		    $end_condition = 1;
		    last;
		}
	    }
	}
	if ($end_condition) {
	    last;
	}
	#
	# Next loop
	#
	if (--$g1_location < 0) {
	    last;
	}
    }

    return($rc);
}

=head2 rule

Returns Marpa's grammar's rule

=cut

sub rule {
  my $self = shift;
  #
  # For a given grammar, the conversion ruleid->rule is a constant
  # so it is legal to cache this data
  #
  my $ruleId = shift;
  if (! exists($self->{_cacheRule}->{$ruleId})) {
      $log->tracef('$grammar->rule(%d)', $ruleId);
      my ($lhs, @rhs) = $self->{grammar}->rule($ruleId);
      $self->{_cacheRule}->{$ruleId} = [ $lhs, @rhs ];
  }
  return @{$self->{_cacheRule}->{$ruleId}};
}

=head2 value

Returns Marpa's recognizer's value

=cut

sub value {
  my $self = shift;
  $log->tracef('$recce->value()');
  return $self->{recce}->value();
}

=head2 read

Returns Marpa's recognizer's read

=cut

sub read {
  my $self = shift;
  #
  # Well, we know that input is a reference to a string, and do not want its dump in the log...
  $log->tracef('$recce->read(%s)', "@_");
  return $self->{recce}->read(@_);
}

=head2 resume

Returns Marpa's recognizer's resume

=cut

sub resume {
  my $self = shift;
  $log->tracef('$recce->resume()');
  return $self->{recce}->resume();
}

=head2 last_completed

Returns Marpa's recognizer's last_completed

=cut

sub last_completed {
  my $self = shift;
  $log->tracef('$recce->last_completed("%s")', @_);
  return $self->{recce}->last_completed(@_);
}

=head2 last_completed_range

Returns Marpa's recognizer's last_completed_range

=cut

sub last_completed_range {
  my $self = shift;
  $log->tracef('$recce->last_completed_range("%s")', @_);
  return $self->{recce}->last_completed_range(@_);
}

=head2 range_to_string

Returns Marpa's recognizer's range_to_string

=cut

sub range_to_string {
  my $self = shift;
  $log->tracef('$recce->range_to_string(%d, %d)', @_);
  return $self->{recce}->range_to_string(@_);
}

=head2 progress

Returns Marpa's recognizer's progress

=cut

sub progress {
  my $self = shift;
  $log->tracef('$recce->progress(%d)', @_);
  return $self->{recce}->progress(@_);
}

=head2 event

Returns Marpa's recognizer's event

=cut

sub event {
  my $self = shift;
  $log->tracef('$recce->event(%d)', @_);
  return $self->{recce}->event(@_);
}

=head2 pause_lexeme

Returns Marpa's recognizer's pause_lexeme

=cut

sub pause_lexeme {
  my $self = shift;
  $log->tracef('$recce->pause_lexeme()');
  return $self->{recce}->pause_lexeme();
}

=head2 pause_span

Returns Marpa's recognizer's pause_span

=cut

sub pause_span {
  my $self = shift;
  $log->tracef('$recce->pause_span()');
  return $self->{recce}->pause_span();
}

=head2 line_column

Returns Marpa's recognizer's line_column

=cut

sub line_column {
  my $self = shift;
  $log->tracef('$recce->line_column(%d)', @_);
  return $self->{recce}->line_column(@_);
}

=head2 substring

Returns Marpa's recognizer's substring

=cut

sub substring {
  my $self = shift;
  $log->tracef('$recce->substring(%d, %d)', @_);
  return $self->{recce}->substring(@_);
}

=head2 lexeme_read

Returns Marpa's recognizer's lexeme_read

=cut

sub lexeme_read {
  my $self = shift;
  $log->tracef('$recce->lexeme_read("%s", %d, %d, "%s")', @_);
  return $self->{recce}->lexeme_read(@_);
}

=head2 latest_g1_location

Returns Marpa's recognizer's latest_g1_location

=cut

sub latest_g1_location {
  my $self = shift;
  $log->tracef('$recce->latest_g1_location()');
  return $self->{recce}->latest_g1_location();
}

=head2 g1_location_to_span

Returns Marpa's recognizer's g1_location_to_span

=cut

sub g1_location_to_span {
  my $self = shift;
  $log->tracef('$recce->g1_location_to_span(%d)', @_);
  return $self->{recce}->g1_location_to_span(@_);
}

#
# INTERNAL METHODS
#
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
