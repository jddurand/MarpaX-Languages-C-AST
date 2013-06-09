use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Callback::Events;
use parent qw/MarpaX::Languages::C::AST::Callback/;

# ABSTRACT: Events callback when translating a C source to an AST

use Log::Any qw/$log/;
use Carp qw/croak/;
use SUPER;

# VERSION

=head1 DESCRIPTION

This modules implements the Marpa events callback using the very simple framework MarpaX::Languages::C::AST::Callback. it is useful because it shows the FUNCTIONAL things that appear within the events: monitor the TYPEDEFs, introduce/obscure names in name space, apply the few grammar constraints needed at parsing time.

=cut


sub new {
    my ($class, $outerSelf) = @_;
    my $self = $class->SUPER();

    $self->register({priority => 1,
		     condition => sub {grep {$_ eq 'declarationSpecifiers$'} @_}},
		     \&_isTypedef, $outerSelf, 'declarationSpecifiers$', 0);
		    
    $self->register({condition => sub {grep {$_ eq 'storageClassSpecifierTypedef$'} @_}},
		     \&_isTypedef, $outerSelf, 'storageClassSpecifierTypedef$', 1);

    $self->register({condition => sub {(grep {$_ eq 'initDeclaratorList$'} @_) &&
				       (grep {$_ eq 'directDeclaratorIdentifier$'} @_)}},
		     \&_introduceNameInNamespace, $outerSelf, 'directDeclaratorIdentifier$,initDeclaratorList$');

    $self->register({condition => sub {grep {$_ eq '^functionDefinition'} @_}},
		    \&_initSumIsTypedef, $outerSelf, '^functionDefinition');

    $self->register({condition => sub {grep {$_ eq '^declarationList'} @_}},
		    \&_initSumIsTypedef, $outerSelf, '^declarationList');

    $self->register({condition => sub {grep {$_ eq '^parameterDeclaration'} @_}},
		    \&_initSumIsTypedef, $outerSelf, '^parameterDeclaration');

    $self->register({condition => sub {grep {$_ eq 'functionDefinitionCheckpoint[]'} @_}},
		     \&_functionDefinitionCheckpoint, $outerSelf, 'functionDefinitionCheckpoint[]');

    $self->register({condition => sub {grep {$_ eq 'declarationList$'} @_}},
		     \&_declarationList, $outerSelf, 'declarationList$');

    $self->register({condition => sub {grep {$_ eq 'functionDefinition$'} @_}},
		     \&_functionDefinition, $outerSelf, 'functionDefinition$');

    $self->register({condition => sub {grep {$_ eq 'parameterDeclaration$'} @_}},
		     \&_parameterDeclaration, $outerSelf, 'parameterDeclaration$');

    return $self;
}

sub _isTypedef {
    my $self = shift;
    my $event = shift;
    if (@_) {
	$self->{_isTypedef} = shift;
	$self->{_sumIsTypedef} += $self->{_isTypedef};
    }
    $log->debugf('[  %s] _isTypedef: %d', $event, $self->{_isTypedef});
    return $self->{_isTypedef};
}

sub _introduceNameInNamespace {
    my ($self, $event) = @_;

    my $identifier = $self->_last_completed('directDeclaratorIdentifier');
    if ($self->{_isTypedef}) {
	$self->{_scope}->parseEnterTypedef("  $event", $identifier);
    } else {
	$self->{_scope}->parseObscureTypedef("  $event", $identifier);
    }
}

sub _initSumIsTypedef {
    my ($self, $event) = @_;

    $self->{_sumIsTypedef} = 0;
}

sub _functionDefinitionCheckpoint {
    my ($self, $event) = @_;

    $self->{_functionDefinitionCheckpoint} = {
	_sumIsTypedef => $self->{_sumIsTypedef},
	_declarationSpecifiers => $self->_last_completed('declarationSpecifiers')
    };
	
}

sub _declarationList {
    my ($self, $event) = @_;

    $self->{_declarationList} = {
	_sumIsTypedef => $self->{_sumIsTypedef},
	_declaration => $self->_last_completed('declaration')
    };
	
}

sub _parameterDeclaration {
    my ($self, $event) = @_;

    if ($self->{_sumIsTypedef}) {
	$self->_croak(
	    'typedef is not valid in the parameter declaration of a function definition: %s',
	    $self->_last_completed('parameterDeclaration')
	    );
    }
}

sub _functionDefinition {
    my ($self, $event) = @_;

    if ($self->{_functionDefinitionCheckpoint}->{_sumIsTypedef}) {
	$self->_croak(
	    'typedef is not valid in the declaration specifiers of a function definition: %s',
	    $self->{_functionDefinitionCheckpoint}->{_declarationSpecifiers}
	    );
    }
    if ($self->{_declarationList}->{_sumIsTypedef}) {
	$self->_croak(
	    'typedef is not valid in the declaration list of a function definition: %s',
	    $self->{_declarationList}->{_declaration}
	    );
    }
	
}

1;
