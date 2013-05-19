package MarpaX::Languages::C::AST::Scope;

use 5.006;
use strict;
use warnings FATAL => 'all';
use Clone qw/clone/;
use Log::Any qw/$log/;

=head1 NAME

MarpaX::Languages::C::AST::Scope - Scope management when translating a C source to an AST

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

This modules manages the scopes when translation a C source into an AST tree. This module is an implementation of the article:

I<Resolving Typedefs in a Multipass C Compiler>

from I<Journal of C Languages Translation>, Volume 2, Number 4, writen by W.M. McKeeman. A online version may be accessed at L<http://www.cs.dartmouth.edu/~mckeeman/references/JCLT/ResolvingTypedefsInAMultipassCCompiler.pdf>.

Example:

    use MarpaX::Languages::C::AST::Scope;

    my $cAstScopeObject = MarpaX::Languages::C::AST::Scope->new();
    $cAstScopeObject->parseEnterScope();
    $cAstScopeObject->parseReenterScope();
    $cAstScopeObject->parseEnterTypedef("myTypedef");
    $cAstScopeObject->parseEnterEnum("myEnum");
    $cAstScopeObject->parseObscureTypedef("myVariable");
    foreach (qw/myTypedef myEnum myVariable/) {
      if ($cAstScopeObject->parseIsTypedef($_)) {
        print "\"$_\" is a typedef\n";
      } elsif ($cAstScopeObject->parseIsEnum($_)) {
        print "\"$_\" is an enum\n";
      }
    }
    $cAstScopeObject->parseExitScope();

Please note that this module is logging via Log::Any.

=head1 SUBROUTINES/METHODS

=head2 new

Instance a new object. Takes no parameter.

=cut

sub new {
  my ($class) = @_;

  my $self  = {
               delayedExitScope => 0,
               typedefPerScope => [ {} ],
               enumAnyScope => {}
              };
  bless($self, $class);

  return $self;
}

=head2 parseEnterScope

Say we enter a new scope. Takes no parameter.

=cut

sub parseEnterScope {
  my ($self) = @_;

  $self->_doDelayedExitScope();

  my $scope = $#{$self->{typedefPerScope}};
  push(@{$self->{typedefPerScope}}, clone($self->{typedefPerScope}->[$scope]));

  $log->debugf('Duplicated scope %d to %d', $scope, $scope + 1);
}

=head2 parseExitScope

Say we leave current scope. Takes no parameter.

=cut

sub parseExitScope {
  my ($self) = @_;

  my $scope = $#{$self->{typedefPerScope}};
  $self->{delayedExitScope} = 1;

  $log->debugf('Setting delay flag on scope %d', $scope);
}

=head2 parseReenterScope

Say we re-enter last scope. Takes no parameter.

=cut

sub parseReenterScope {
  my ($self) = @_;

  my $scope = $#{$self->{typedefPerScope}};
  $self->{delayedExitScope} = 0;

  $log->debugf('Resetting delay flag on scope %d', $scope);
}

=head2 parseEnterTypedef($token)

Declare a new typedef, that will be visible until current scope is left. Takes the corresponding token value in input.

=cut

sub parseEnterTypedef {
  my ($self, $token) = @_;

  $self->_doDelayedExitScope();

  my $scope = $#{$self->{typedefPerScope}};
  $self->{typedefPerScope}->[$scope]->{$token} = 1;

  $log->debugf('"%s" at scope %d', $token, $scope);
}

=head2 parseEnterEnum($token)

Declare a new enum, that will be visible at any scope from now on. Takes the corresponding token value in input.

=cut

sub parseEnterEnum {
  my ($self, $token) = @_;

  $self->_doDelayedExitScope();

  $self->{enumAnyScope}->{$token} = 1;

  $log->debugf('"%s"', $token);
}

=head2 parseObscureTypedef($token)

Obscured a typedef. Takes the corresponding token value in input.

=cut

sub parseObscureTypedef {
  my ($self, $token) = @_;

  $self->_doDelayedExitScope();

  my $scope = $#{$self->{typedefPerScope}};
  $self->{typedefPerScope}->[$scope]->{$token} = 0;

  $log->debugf('"%s obscured at scope %d', $token, $scope);
}

=head2 parseIsTypedef($token)

Return a true value if the corresponding token value in input is a typedef. Returns false otherwise.

=cut

sub parseIsTypedef {
  my ($self, $token) = @_;

  $self->_doDelayedExitScope();

  my $scope = $#{$self->{typedefPerScope}};
  my $rc = (exists($self->{typedefPerScope}->[$scope]->{$token}) && $self->{typedefPerScope}->[$scope]->{$token}) ? 1 : 0;

  $log->debugf('"%s" at scope %d ? %s', $token, $scope, $rc ? 'yes' : 'no');

  return($rc);
}

=head2 parseIsEnum($token)

Return a true value if the corresponding token value in input is an enum. Returns false otherwise.

=cut

sub parseIsEnum {
  my ($self, $token) = @_;

  $self->_doDelayedExitScope();

  my $rc = (exists($self->{enumAnyScope}->{$token}) && $self->{enumAnyScope}->{$token}) ? 1 : 0;

  $log->debugf('"%s" ? %s', $token, $rc ? 'yes' : 'no');

  return($rc);
}

#
# INTERNAL METHODS
#
sub _doDelayedExitScope {
  my ($self) = @_;

  if ($self->{delayedExitScope}) {
    my $scope = $#{$self->{typedefPerScope}};
    pop(@{$self->{typedefPerScope}});
    $self->{delayedExitScope} = 0;

    $log->debugf('Removed scope %d and resetted delay flag', $scope);
    }
}



=head1 SEE ALSO

L<Log::Any>

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

1; # End of MarpaX::Languages::C::AST::Scope
