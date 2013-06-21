use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Callback::Method;
use MarpaX::Languages::C::AST::Callback::Option;
use Class::Struct
    description   => '$',
    method        => '@',        # [ CODE ref, CODE ref arguments ]
    method_void   => '$',        # Prevent push to topic data
    option        => 'MarpaX::Languages::C::AST::Callback::Option',
    ;

# ABSTRACT: Code reference for the Simple callback generic framework.

use Carp qw/croak/;

# VERSION

=head1 DESCRIPTION

This module is describing the code reference for the Simple Callback framework. The new method supports these items:

=over

=item description

Any string that describes this event

=item method

A reference to an array containing a CODE reference in the first indice, then the arguments. The method will be called as $self->$CODE(@arguments) where $self is a reference to the method object. The arguments to the exec() routine will follow @arguments. Or the user can use $self->SUPER::arguments.

=item method_void

Default is the push any eventual topic data the output of the method. Setting this flag to a true value will disable it.

=item option

A reference to a MarpaX::Languages::C::AST::Callback::Option object.

=back

=cut

1;
