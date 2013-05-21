package MarpaX::Languages::C::AST::Grammar;

use strict;
use warnings FATAL => 'all';
use Module::Info qw/new_from_loaded inc_dir/;
use IO::File;
use File::Spec qw/catfile/;
use Log::Any qw/$log/;
use constant {DIRPATH => 'DIRPATH'};
use File::Find qw/find/;
use Carp qw/croak carp/;

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

    my $grammar = MarpaX::Languages::C::AST::Grammar->new();

    my $isoAnsiC2011 = $grammar->read('ISO-ANSI-C-2011.bnf');

    # or
    $grammar->dirpath('My_Directory');
    $myIsoAnsiC2011 = $grammar->read(filename => 'MY-ISO-ANSI-C-2011.bnf');


=head1 SUBROUTINES/METHODS

=head2 new()

Instance a new object. Takes no argument.

=cut

sub new {
  my ($class) = @_;

  my $DIRPATH = sprintf('%s::%s', __PACKAGE__, DIRPATH);
  my $self  = {
      dirpath => defined($ENV{$DIRPATH}) ? $DIRPATH : File::Spec->catdir(Module::Info->new_from_loaded(__PACKAGE__)->inc_dir, split('::', __PACKAGE__), 'inc'),
  };
  bless($self, $class);

  return $self;
}

=head2 read($fileName)

Returns the content of the grammar. Takes the filename of the grammar in parameter, that must be located in the dirpath() directory. Will croak if the file cannot be opened for reading, and carp if there is a warning when closing it. The content of the file is explicitely untainted.

=cut

sub read {
    my ($self, $filename) = @_;
    my $filepath = File::Spec->catfile($self->dirpath(), $filename);
    $log->debugf('Reading %s', $filepath);
    my $fh = IO::File->new($filepath, 'r');
    if (! defined($fh)) {
	croak "$filepath, $!";
    }
    $fh->untaint;
    my $rc = do { local $/; <$fh> };
    if (! $fh->close) {
	carp "$filepath, $!";
    }
    return $rc;
}

=head2 dirpath([$dirPath])

Get/set the directory path where are located the grammars. Default value is, in order of preference, the environment variable MarpaX::Languages::C::AST::Grammar::DIRPATH, the 'inc' directory distributed with this package.

=cut

sub dirpath {
    my $self = shift;
    if (@_) {
	$self->{dirpath} = shift;
    }
    return $self->{dirpath};
}

=head2 list

Returns an array of available grammars. This is in reality just the list of recursive files that are in dirpath().

=cut

sub list {
    my ($self) = @_;

    my @found = ();
    find(
	{
	    wanted => sub {(-f $_) && push(@found, $_);},
	    no_chdir => 1
	},
	$self->dirpath()
	);
    return \@found;
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

1; # End of MarpaX::Languages::C::AST::Data::Grammar
