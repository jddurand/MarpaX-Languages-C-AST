package MarpaX::Languages::C::AST::Grammar;

use strict;
use warnings FATAL => 'all';
use MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011;
use Carp qw/croak/;

=head1 NAME

MarpaX::Languages::C::AST::Grammar - C grammar writen in Marpa BNF

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

This modules returns C grammar(s) writen in Marpa BNF.
Current grammars are:
=over
=item *
ISO-ANSI-C-2011. The ISO grammar of ANSI C 2011, as of L<http://www.quut.com/c/ANSI-C-grammar-y-2011.html> and L<http://www.quut.com/c/ANSI-C-grammar-l.html>.
=back

Example:

    use MarpaX::Languages::C::AST::Grammar;

    my $grammar = MarpaX::Languages::C::AST::Grammar->new('ISO-ANSI-C-2011');
    my $grammar_content = $grammar->content();
    my $grammar_option = $grammar->grammar_option();
    my $recce_option = $grammar->recce_option();

=head1 SUBROUTINES/METHODS

=head2 new()

Instance a new object. Takes the name of the grammar as argument.

=cut

sub new {
  my ($class, $grammarName) = @_;

  my $self = {};
  if (! defined($grammarName)) {
    croak 'Usage: new($grammar_Name)';
  } elsif ($grammarName eq 'ISO-ANSI-C-2011') {
    $self->{_grammar} = MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011->new();
  } else {
    croak "Unsupported grammar name $grammarName";
  }
  bless($self, $class);

  return $self;
}

=head2 content()

Returns the content of the grammar.

=cut

sub content {
    my ($self) = @_;
    return $self->{_grammar}->content(@_);
}

=head2 grammar_option()

Returns recommended option for Marpa::R2::Scanless::G->new(), returned as a reference to a hash.

=cut

sub grammar_option {
    my ($self) = @_;
    return $self->{_grammar}->grammar_option(@_);
}

=head2 recce_option()

Returns recommended option for Marpa::R2::Scanless::R->new(), returned as a reference to a hash.

=cut

sub recce_option {
    my ($self) = @_;
    return $self->{_grammar}->recce_option(@_);
}

=head1 SEE ALSO

L<Marpa::R2>, L<MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011>

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

1; # End of MarpaX::Languages::C::AST::Data::Grammar
