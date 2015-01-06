use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Impl::Logger;

# ABSTRACT: Log::Any implementation on top of Marpa

use diagnostics;
use Carp;
use Log::Any;

# VERSION

=head1 DESCRIPTION

This module is a Log::Any wrapper on top of Marpa, instantiated with a trace_file_handle tied to this package

=cut
sub BEGIN {
    #
    ## Some Log implementation specificities
    #
    my $log4perl = eval 'use Log::Log4perl; 1;' || 0; ## no critic
    if ($log4perl) {
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
              category => $options{category}, # possible undef is OK
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

L<Log::Any>, L<http://osdir.com/ml/lang.perl.modules.log4perl.devel/2007-03/msg00030.html>

=cut

1;
