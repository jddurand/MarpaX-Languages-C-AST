use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Callback;
use Class::Struct
    cb => '@',
    prioritized_cb => '@';

# ABSTRACT: Simple callback generic framework, with priorities and conditions.

use Carp qw/croak/;

# VERSION

=head1 DESCRIPTION

This modules is simple callback framework. The caller register callback functions, with eventual priority, condition.

=cut

sub _sort($$) {
    return $_[1]->{optp}->{priority} <=> $_[0]->{optp}->{priority};
}

sub register {
    my $self = shift;
    my $optp = shift;
    my $callback = shift;

    $optp //= {};
    if (ref($optp) ne 'HASH') {
	croak 'option argument must be undef or a HASH reference';
    }

    $callback //= sub {};
    if (ref($callback) ne 'CODE') {
	croak 'callback must be undef or a CODE reference';
    }

    $optp->{condition} //= sub { 1 };
    if (ref($optp->{condition}) ne 'CODE') {
	croak 'condition option must be undef or a CODE reference';
    }

    $optp->{priority} //= 0;
    if (ref($optp->{priority})) {
	croak 'priority option must be undef or a SCALAR';
    }

    push(@{$self->cb},
	 {
	  optp => $optp,
	  callback  => $callback,
	  arguments => [ @_ ],
	 }
	);

    $self->prioritized_cb([sort _sort @{$self->cb}]);
}

sub exec {
    my $self = shift;

    foreach (@{$self->prioritized_cb}) {
	next if (! $_->{optp}->{condition}(@_));
	$_->{callback}(@{$_->{arguments}}, @_);
    }
}

1;
