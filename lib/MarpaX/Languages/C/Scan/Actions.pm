use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::Scan::Actions;
use XML::LibXML;
use Carp qw/croak/;

# ABSTRACT: ISO ANSI C grammar actions in Scan mode

# VERSION

=head1 DESCRIPTION

This modules give the actions associated to ISO_ANSI_C grammar in Scan mode.

=cut

sub new {
    my $class = shift;
    my $self = {
		dom => XML::LibXML::Document->new(),
	       };
    bless($self, $class);
    return $self;
}

sub nonTerminalSemantic {
  my $self = shift;

  my $rule_id     = $Marpa::R2::Context::rule;
  my $slg         = $Marpa::R2::Context::slg;
  my ($lhs, @rhs) = map { $slg->symbol_display_form($_) } $slg->rule_expand($rule_id);
  my $maxRhs = $#rhs;

  my $node = XML::LibXML::Element->new($lhs);

  foreach (0..$#_) {
    my $child;
    if (ref($_[$_]) eq 'ARRAY') {
      #
      # This is a lexeme
      #
      my $name;
      if ($_ > $maxRhs) {
	if ($maxRhs == 0) {
	  #
	  # Ok only if $maxRhs is 0 : this is (probably) a sequence
	  #
	  $name = $rhs[0];
	} else {
	  croak "Too many arguments on the stack. Rule was: $lhs ::= @rhs\n";
	}
      } else {
	$name = $rhs[$_];
      }
      $child = XML::LibXML::Element->new($name);
      $child->setAttribute('start', $_[$_]->[0]);
      $child->setAttribute('length', $_[$_]->[1]);
      $child->setAttribute('text', $_[$_]->[2]);
    } else {
      $child = $_[$_];
    }
    $node->addChild($child);
  }

  if ($lhs eq 'translationUnit') {
    $self->{dom}->addChild($node);
    return $self->{dom};
  } else {
    return $node;
  }
}

1;
