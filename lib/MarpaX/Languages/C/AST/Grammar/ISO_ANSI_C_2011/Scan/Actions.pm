use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011::Scan::Actions;
use parent qw/MarpaX::Languages::C::Scan::Actions/;
use SUPER;

# ABSTRACT: ISO ANSI C 2011 grammar actions in Scan mode

# VERSION

=head1 DESCRIPTION

This modules give the actions associated to ISO_ANSI_C_2011 grammar in Scan mode.

=cut

#
# Because Marpa is using $CODE{}
#
sub new {
  super();
}

sub nonTerminalSemantic {
  super();
}

sub getRuleDescription {
  my ($self) = @_;

  my ($lhs, @rhs) = super();

  #
  # Remove known hiden terms that rule_expand do not remove
  #
  my @okRhs = grep {$_ ne 'structContextStart' &&
                    $_ ne 'structContextEnd' &&
                    $_ ne 'WS_MANY'} @rhs;

  return ($lhs, @okRhs);
}

1;
