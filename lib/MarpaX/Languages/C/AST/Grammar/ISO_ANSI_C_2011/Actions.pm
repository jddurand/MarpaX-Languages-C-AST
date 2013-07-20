use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011::Actions;

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    return $self;
}

sub deref {
    my $self = shift;
    return [ @{$_[0]} ];
}

sub deref_and_bless_declaration {
    my $self = shift;
    return bless $self->deref(@_), 'C::AST::declaration';
}

sub deref_and_bless_declarator {
    my $self = shift;
    return bless $self->deref(@_), 'C::AST::declarator';
}

sub deref_and_bless_compoundStatement {
    my $self = shift;
    return bless $self->deref(@_), 'C::AST::compoundStatement';
}

1;
