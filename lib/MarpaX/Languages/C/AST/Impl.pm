package MarpaX::Languages::C::AST::Impl;

use 5.006;
use strict;
use warnings FATAL => 'all';
use Marpa::R2 2.054_000;
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

This modules implements all needed Marpa calls using its Scanless interface

Example:

    use MarpaX::Languages::C::AST::Impl;

    my $marpaImpl = MarpaX::Languages::C::AST::Impl->new();

=head1 EXPORTS

The constants DOT_PREDICTION (0), DOT_COMPLETION (-1) and LATEST_EARLEY_SET_ID (-1) are exported on demand.

=head1 SUBROUTINES/METHODS

Please note that except for the new, cacheProgress, findInProgress methods, all other methods maps directy to Marpa, passing all arguments as is. Therefore only the eventual arguments to new, cacheProgress, and findInProgress are documented. Please see Marpa documentation for the other methods whenever needed.

=head2 new

=head3 Instance a new object. Takes as parameter two references to hashes: the grammar options, the recognizer options. In the recognizer, there is a grammar internal option that will be forced to the grammar object.

=cut

sub new {
  my ($class, $grammarOptionsHashp, $recceOptionsHashp) = @_;

  my $self  = {
      _cacheProgress => {}
  };
  $self->{grammar} = Marpa::R2::Scanless::G->new($grammarOptionsHashp);
  $self->{recce} = Marpa::R2::Scanless::R->new({%{$recceOptionsHashp}, grammar => $self->{grammar}});
  bless($self, $class);

  return $self;
}

=head2 cacheProgress($earleySetId[, $progressOutputp])

=head3 Caches or return the result of Marpa's progress() for the Earley Set Id $earleySetId. You should use the $earleySetId value LATEST_G1_EARLEY_SET_ID (or -1) or undef, to always cache/get the latest progress(), which is then converted internally to current G1 Earley Set Id. Take care about caching progress(): as soon as it is cached, the findInProgress() method will use it, it is the responsability of the caller to update the cache or remove it, by setting an undef value for $progressOutputp, and to provide in $progressOutputp the result of $self->progress($earleySetId). If calledd without $progressOutputp argument, this method will return the current cached value.

=cut

sub cacheProgress {
    my $self = shift;
    my $earleySetId = shift;

    if (! defined($earleySetId) || ($earleySetId == LATEST_G1_EARLEY_SET_ID)) {
	$earleySetId = $self->latest_g1_location();
    }
    
    if (@_) {
	my $progressOutputp = shift;
	$self->{_cacheProgress}->{$earleySetId} = $progressOutputp;   
    }

    return $self->{_cacheProgress};
}

=head2 findInProgress($earleySetId, $wantedDotPosition, $wantedLhs, $wantedRhsp, $fatalMode)

=head3 Searches a rule at G1 Earley Set Id $earleySetId. The default Earley Set Id is the current one. $wantedLhs, if defined, is the LHS name. $wantedRhsp, if defined, is the list of RHS. $wantedDotPosition, if defined, is the dot position, that should be a number between 0 and the number of RHS, or -1. The shortcuts DOT_PREDICTION (0) and DOT_COMPLETION (-1) can be used if the caller import it. There is a special case: if $dotPrediction is defined, $wantedLhs is defined, and $wantedRhsp is undef then, if $dotPrediction is DOT_PREDICTION we will search any prediction of $wantedLhs, and if $dotPrediction is DOT_COMPLETION we will search any completion of $wantedLhs. Please note that findInProgress() will use the cached value of progress() for the Earley Set Id if it is defined. It will call progress() if not. It will never try to update progress() cache. Therefore, if you are caching progress() output, you should make sure your cache is either up-to-date before calling findInProgress(), or undef. This method will return a true value if there is a match.

=cut

sub findInProgress {
    my ($self, $earleySetId, $wantedDotPosition, $wantedLhs, $wantedRhsp, $fatalMode) = @_;

    $fatalMode ||= 0;

    my $rc = 0;
    my $progressOutputp = $self->cacheProgress($earleySetId);
    if (! defined($progressOutputp)) {
	$progressOutputp = $self->progress($earleySetId);
    }

    my $i = 0;
    foreach (@{$progressOutputp}) {
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
	    next if (defined($wantedRhsp) && ! arrayEq(\@rhs, $wantedRhsp));
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

=head2 rule

=head3 Returns Marpa's grammar's rule

=cut

sub rule {
  my ($self) = @_;
  $log->tracef('\$grammar->rule(%s)', @_);
  return $self->{grammar}->rule(@_);
}

=head2 value

=head3 Returns Marpa's recognizer's value

=cut

sub value {
  my ($self) = @_;
  $log->tracef('\$recce->value(%s)', @_);
  return $self->{recce}->value(@_);
}

=head2 read

=head3 Returns Marpa's recognizer's read

=cut

sub read {
  my ($self) = @_;
  #
  # Well, we know that input is a reference to a string, and do not want its dump in the log...
  $log->tracef('\$recce->read(%s)', "@_");
  return $self->{recce}->read(@_);
}

=head2 resume

=head3 Returns Marpa's recognizer's resume

=cut

sub resume {
  my ($self) = @_;
  $log->tracef('\$recce->resume(%s)', @_);
  return $self->{recce}->resume(@_);
}

=head2 last_completed

=head3 Returns Marpa's recognizer's last_completed

=cut

sub last_completed {
  my ($self) = @_;
  $log->tracef('\$recce->last_completed(%s)', @_);
  return $self->{recce}->last_completed(@_);
}

=head2 last_completed_range

=head3 Returns Marpa's recognizer's last_completed_range

=cut

sub last_completed_range {
  my ($self) = @_;
  $log->tracef('\$recce->last_completed_range(%s)', @_);
  return $self->{recce}->last_completed_range(@_);
}

=head2 range_to_string

=head3 Returns Marpa's recognizer's range_to_string

=cut

sub range_to_string {
  my ($self) = @_;
  $log->tracef('\$recce->range_to_string(%s)', @_);
  return $self->{recce}->range_to_string(@_);
}

=head2 progress

=head3 Returns Marpa's recognizer's progress

=cut

sub progress {
  my ($self) = @_;
  $log->tracef('\$recce->progress(%s)', @_);
  return $self->{recce}->progress(@_);
}

=head2 event

=head3 Returns Marpa's recognizer's event

=cut

sub event {
  my ($self) = @_;
  $log->tracef('\$recce->event(%s)', @_);
  return $self->{recce}->event(@_);
}

=head2 pause_lexeme

=head3 Returns Marpa's recognizer's pause_lexeme

=cut

sub pause_lexeme {
  my ($self) = @_;
  $log->tracef('\$recce->pause_lexeme(%s)', @_);
  return $self->{recce}->pause_lexeme(@_);
}

=head2 pause_span

=head3 Returns Marpa's recognizer's pause_span

=cut

sub pause_span {
  my ($self) = @_;
  $log->tracef('\$recce->pause_span(%s)', @_);
  return $self->{recce}->pause_span(@_);
}

=head2 line_column

=head3 Returns Marpa's recognizer's line_column

=cut

sub line_column {
  my ($self) = @_;
  $log->tracef('\$recce->line_column(%s)', @_);
  return $self->{recce}->line_column(@_);
}

=head2 substring

=head3 Returns Marpa's recognizer's substring

=cut

sub substring {
  my ($self) = @_;
  $log->tracef('\$recce->substring(%s)', @_);
  return $self->{recce}->substring(@_);
}

=head2 lexeme_read

=head3 Returns Marpa's recognizer's lexeme_read

=cut

sub lexeme_read {
  my ($self) = @_;
  $log->tracef('\$recce->lexeme_read(%s)', @_);
  return $self->{recce}->lexeme_read(@_);
}

=head2 latest_g1_location

=head3 Returns Marpa's recognizer's latest_g1_location

=cut

sub latest_g1_location {
  my ($self) = @_;
  $log->tracef('\$recce->latest_g1_location(%s)', @_);
  return $self->{recce}->latest_g1_location(@_);
}

=head2 g1_location_to_span

=head3 Returns Marpa's recognizer's g1_location_to_span

=cut

sub g1_location_to_span {
  my ($self) = @_;
  $log->tracef('\$recce->g1_location_to_span(%s)', @_);
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
