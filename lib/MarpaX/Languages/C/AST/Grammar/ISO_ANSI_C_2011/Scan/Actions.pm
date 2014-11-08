use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011::Scan::Actions;
use XML::LibXML;

# ABSTRACT: ISO ANSI C 2011 grammar actions in Scan mode

# VERSION

=head1 DESCRIPTION

This modules give the actions associated to ISO_ANSI_C_2011 grammar in Scan mode.

=cut

sub new {
    my $class = shift;
    my $self = {
		dom => XML::LibXML::Document->new()
	       };
    bless($self, $class);
    return $self;
}

sub nonTerminalSemantic {
  my $self = shift;

}

sub terminalSemantic {
  my $self = shift;

}

1;
