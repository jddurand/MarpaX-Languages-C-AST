package MarpaX::Languages::C::AST::Impl::Logger;
#
## C.f. http://osdir.com/ml/lang.perl.modules.log4perl.devel/2007-03/msg00030.html
#
use strict;
use diagnostics;
use Carp;
use Log::Any;

=head1 NAME

MarpaX::Languages::C::AST::Impl::Logger - Log::Any implementation on top of Marpa

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


sub BEGIN {
    #
    ## Some Log implementation specificities
    #
    my $have_Log_Log4perl = eval 'use Log::Log4perl; 1;' || 0;
    if ($have_Log_Log4perl != 0) {
	#
	## Here we put know hooks for logger implementations
	#
	Log::Log4perl->wrapper_register(__PACKAGE__);
    }
}

sub TIEHANDLE {
  my($class, %options) = @_;

  my $self = {
              level => exists($options{level}) ? ($options{level} || 'trace') : 'trace',
              category => exists($options{category}) ? ($options{category} || '') : '',
             };

  $self->{logger} = Log::Any->get_logger(category => $self->{category});

  bless $self, $class;
}

sub PRINT {
  my $self = shift;
  my $logger = $self->{logger} || '';
  my $level = $self->{level} || '';
  if ($logger && $level) {
    $logger->trace(@_);
  }
  return 1;
}

sub PRINTF {
  my $self = shift;
  return $self->PRINT(sprintf(@_));
}

sub UNTIE {
  my ($obj, $count) = @_;
  if ($count) {
    croak "untie attempted while $count inner references still exist";
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

1; # End of MarpaX::Languages::C::AST::Impl::Logger
