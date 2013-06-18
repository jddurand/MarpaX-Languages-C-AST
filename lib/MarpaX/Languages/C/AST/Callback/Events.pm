use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Callback::Events;
use MarpaX::Languages::C::AST::Util qw/whoami/;
use parent qw/MarpaX::Languages::C::AST::Callback/;

# ABSTRACT: Events callback when translating a C source to an AST

use Log::Any qw/$log/;
use Carp qw/croak/;
use Storable qw/dclone/;
use SUPER;

# VERSION

=head1 DESCRIPTION

This modules implements the Marpa events callback using the very simple framework MarpaX::Languages::C::AST::Callback. it is useful because it shows the FUNCTIONAL things that appear within the events: monitor the TYPEDEFs, introduce/obscure names in name space, apply the few grammar constraints needed at parsing time.

=cut

sub new {
    my ($class, $outerSelf) = @_;
    my $self = $class->SUPER();

    # ####################################################################################################
    # Create topics <Gx> based on "genome" rules with a priority of 1, so that they are always triggered first.
    # The topic data will always be in an array reference of [ [$line, $column], $last_completion ]
    # ####################################################################################################
    $self->_register_genome_callbacks($outerSelf, {priority => 998, topic_persistence => 'none' });

    # #######################################################################################################################
    # From now on, the technique is always the same for RIGHT recursive symbols.
    #
    # For a rule that will be isolated for convenience (the grammar uses the action => deref if needed) like:
    # LHS ::= RHS1 RHS2 ... RHSn
    #
    # - We isolate the RHSx that are of interest
    # - We register topics 'LHSRHSx' with persistence_level 'level' that depend on genome topic(x) <Gx> and get <Gx> data
    # - We use internal flags 'LHSRHSxFlag' for each of these new topics
    # - We use <LHS|RHSxOn> and <LHSRHSxOff> to switch on/off the internal flags 'LHSRHSxFlag'
    # - We process the topic data using an <LHS$>, then reset it
    #
    # Please note that we cannot use <^LHS> to initialize or reset the data, because grammar cycles. Only
    # the end of a rule can be trusted.
    #
    # One subtility remains: the topic levels must be in perfect sync with the C source code scopes.
    # But the exitScope is delayed until:
    # - another scope enter
    # - a definition of a new typedef or enum
    # - a obscure/introduce of typedef
    # - a check if an identifier is a typedef or an enum
    # Basically, one have to look carefully to LHS dependencies. IF the end of the rule matches an end of scope
    # lexeme, that one will have to use $self->topic_data(-1) instead of $self->topic_data())
    #
    # In this technique it is assumed that all RHS are different. Otherwise one would have to use
    # a temporary RHS "proxy". But this is not the case in the C grammar.
    #
    # IF one the symbol implies left recursivivity, then the booleans may be putted down into the definition of this
    # symbol. For example: declarationDeclarationSpecifiers depends on declarationSpecifiers that is
    # right recursive. And on initDeclaratorList that is left recursive.
    # #######################################################################################################################

    my @flags = ();          # See below

    # ###############################################################################################
    # A directDeclarator introduces a typedefName only when it eventually participates in the grammar
    # rule:
    # declarationDeclarationSpecifiers ::= declarationSpecifiers initDeclaratorList SEMICOLON
    #
    # No problem with scope at the end of this rule
    # ###############################################################################################
    push(@flags, $self->_register_rule_callbacks($outerSelf,
						 {
						     lhs => 'declarationDeclarationSpecifiers',
						     rhs => { 'declarationSpecifiers' => ['storageClassSpecifierTypedef$'],
							      'initDeclaratorList'    => ['directDeclaratorIdentifier$'  ],
						     },
						     method => \&_declarationDeclarationSpecifiers,
						 }
	 )
	);

    # #############################################################################################
    # Register scope callbacks: they have 'infinite' priorities, because they drive the topic level
    # At every scope, all flags of every managed composite rule are resetted. Just not to pollute
    # topic level data outside of a '^LHS'.
    # #############################################################################################
    $self->_register_scope_callbacks($outerSelf, {priority => 999}, @flags);

    return $self;
}

# ----------------------------------------------------------------------------------------
sub _enterCallback {
    my ($self, $outerSelf, @flags) = @_;

    $log->debugf('[%s] Saving all flags values', whoami(__PACKAGE__));
    my $state = {};
    foreach (@flags) {
	$state->{$_} = $self->hscratchpad($_);
	$self->hscratchpad($_, 0);
	$log->debugf('[%s] \'%s\' was %d, is now %d', whoami(__PACKAGE__), $_, $state->{$_}, $self->hscratchpad($_));
    }
    $self->hscratchpad('allFlags', $state);
    $self->pushTopicLevel();

}
# ----------------------------------------------------------------------------------------
sub _exitCallback {
    my ($self, $outerSelf, @flags) = @_;

    $log->debugf('[%s] Restoring all flags values', whoami(__PACKAGE__));
    my $state = {};
    foreach (@flags) {
	$state->{$_} = $self->hscratchpad($_);
	$self->hscratchpad($_, ($self->hscratchpad('allFlags'))->{$_});
	$log->debugf('[%s] \'%s\' was %d, is now %d', whoami(__PACKAGE__), $_, $state->{$_}, $self->hscratchpad($_));
    }
    $self->popTopicLevel();

}
# ----------------------------------------------------------------------------------------
sub _introduceTypedefName {
    my ($cb, $self, $outerSelf, @execArgs) = @_;
    #
    # Get the topics data we are interested in
    #
    my $storageClassSpecifierTypedef = $self->topic_fired_data('storageClassSpecifierTypedef$');
    my $directDeclaratorIdentifier = $self->topic_fired_data('directDeclaratorIdentifier$');
    #
    # We are not subscribed to storageClassSpecifierTypedef$ so it is not guaranteed there
    # is associated data
    #
    if (! defined($storageClassSpecifierTypedef)) {
	$log->warnf('[%s] No storageClassSpecifierTypedef, identifiers are %s', whoami(__PACKAGE__), $directDeclaratorIdentifier);
	return;
    }

    my $nbTypedef = $#{$storageClassSpecifierTypedef};
    if ($nbTypedef > 0) {
	#
	# Take the second typedef
	#
	my ($line_columnp, $last_completed)  = @{$storageClassSpecifierTypedef->[1]};
	$outerSelf->_croak("[%s] %s cannot appear more than once\n%s", whoami(__PACKAGE__), $last_completed, $outerSelf->_show_line_and_col($line_columnp));
    }
    foreach (@{$directDeclaratorIdentifier}) {
	my ($line_columnp, $last_completed)  = @{$_};
	$log->debugf('[%s] Identifier %s at position %s', whoami(__PACKAGE__), $last_completed, $line_columnp);
	if ($nbTypedef >= 0) {
	    $outerSelf->{_scope}->parseEnterTypedef($last_completed);
	} else {
	    $outerSelf->{_scope}->parseObscureTypedef($last_completed);
	}
    }
    #
    # Reset data
    #
    $self->reset_topic_fired_data('storageClassSpecifierTypedef$');
    $self->reset_topic_fired_data('$directDeclaratorIdentifier');
}
# ----------------------------------------------------------------------------------------
sub _register_scope_callbacks {
    my ($self, $outerSelf, $hashp, @flags) = @_;

    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => 'enterScope[]',
		     method =>  [ sub { $outerSelf->{_scope}->parseEnterScope(); } ],
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [qw/auto/] ],
		      priority => $hashp->{priority}
		     )
		    )
	);
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => 'exitScope[]',
		     method =>  [ sub { $outerSelf->{_scope}->parseExitScope(); } ],
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [qw/auto/] ],
		      priority => $hashp->{priority},
		     )
		    )
	);
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => 'reenterScope[]',
		     method =>  [ sub { $outerSelf->{_scope}->parseReenterScope(); } ],
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [qw/auto/] ],
		      priority => $hashp->{priority},
		     )
		    )
	);

    # --------------------------------------------------------------------
    # We want to have topic levels following the real enter/exit of scopes
    # --------------------------------------------------------------------
    $outerSelf->{_scope}->enterCallback(\&_enterCallback, $self, $outerSelf, @flags);
    $outerSelf->{_scope}->exitCallback(\&_exitCallback, $self, $outerSelf, @flags);
}
# ----------------------------------------------------------------------------------------
sub _register_helper {
    my ($self, $outerSelf, $event, $hashp) = @_;
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => $event,
		     method =>  [ \&_storage_helper, $self, $outerSelf, $event ],
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [qw/auto/] ],
		      topic => {$event => 1},
		      topic_persistence => $hashp->{topic_persistence},
		      priority => $hashp->{priority}
		     )
		    )
	);
}
# ----------------------------------------------------------------------------------------
sub _storage_helper {
    my ($cb, $self, $outerSelf, $event) = @_;
    #
    # The event name, by convention, is "symbol$"
    #
    my $symbol = $event;
    substr($symbol, -1, 1, '');
    my $rc = [ $outerSelf->_line_column(), $outerSelf->_last_completed($symbol) ];
    $log->tracef('[%s] %s', whoami(__PACKAGE__), $rc);
    return $rc;
}
# ----------------------------------------------------------------------------------------
sub _data_flag {
    my ($cb, $self, $outerSelf, $flag, $value, @events) = @_;

    $log->debugf('[%s] Setting \'%s\' flag to %d', whoami(__PACKAGE__), $flag, $value);
    $self->hscratchpad($flag, $value);
}
# ----------------------------------------------------------------------------------------
sub _data_storage {
    my ($cb, $self, $outerSelf, $flag, $genome, @events) = @_;

    my $rc = undef;
    if ($self->hscratchpad($flag)) {
	$rc = _storage_helper($cb, $self, $outerSelf, $genome);
	$log->debugf('[%s] Storing \'%s\' value: %s', whoami(__PACKAGE__), $genome, $rc);
    }
    return $rc;
}
# ----------------------------------------------------------------------------------------
sub _register_genome_callbacks {
    my ($self, $outerSelf, $hashp) = @_;

    foreach (qw/primaryExpressionIdentifier$
              enumerationConstantIdentifier$
              storageClassSpecifierTypedef$
              directDeclaratorIdentifier$/) {
	$self->_register_helper($outerSelf, $_, $hashp);
    }
}
# ----------------------------------------------------------------------------------------
sub _register_rule_callbacks {
    my ($self, $outerSelf, $hashp) = @_;

    #
    # Aggregate all the necessary flags
    #
    my $lhs = $hashp->{lhs};
    my @topics = ();
    foreach (keys %{$hashp->{rhs}}) {
	push(@topics, $lhs . $_);
    }

    #
    # The priorities should be:
    # - "xxxOn"              4
    # - "xxx"                3       because we want to store data before ./..
    # - "xxxOff"             2       ./.. the off flag. The on
    # - 'lhs$'               1       because of eventual grammar cycles ./..

    #
    # Register data initializer, it aims to have a higher priority than
    # the registered topics, although not necessary in our model.
    # But take care, in case of recursivity one can have both
    # '^$lhs' and '$lhs$'. This mean that '$lhs$' must have higher
    # priority than '^$lhs'
    #

    #
    # Register callbacks that are setting the flags
    #
    my @flags = ();
    foreach my $topic (@topics) {
	my $flag = $topic . 'Flag';
	push(@flags, $flag);
	foreach my $switch (qw/On Off/) {
	    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
			    (
			     description => $topic . $switch,
			     method =>  [ \&_data_flag, $self, $outerSelf, $flag, $switch eq 'On' ? 1 : 0 ],
			     option => MarpaX::Languages::C::AST::Callback::Option->new
			     (
			      condition => [ [qw/auto/] ],
			      priority => $switch eq 'On' ? 4 : 2,
			     )
			    )
		);
	}
    }

    #
    # Register topics
    #
    foreach (keys %{$hashp->{rhs}}) {
	my $topic = $lhs . $_;
	my $flag = $topic . 'Flag';
	foreach my $genome (@{$hashp->{rhs}->{$_}}) {
	    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
			    (
			     description => $topic,
			     method =>  [ \&_data_storage, $self, $outerSelf, $topic . 'Flag', $genome ],
			     option => MarpaX::Languages::C::AST::Callback::Option->new
			     (
			      condition => [ [ sub {my $cb = shift;
						    my $self = shift;
						    my $genome = shift;
						    my $flag = shift;
						    return 0 if (! $self->hscratchpad($flag));
						    return grep {$_ eq $genome} @_; }, $self, $genome, $flag ] ],
			      topic => {$topic => 1},
			      topic_persistence => 'level',
			      priority => 3,
			     )
			    )
		);
	}
    }

    #
    # Register check procedure
    #
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => "$lhs\$",
		     method =>  [ $hashp->{method}, $self, $outerSelf, [ @topics ] ],
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [ qw/auto/ ] ],
		      priority => 1
		     )
		    )
	);

    #
    # Return the flags
    #
    return @flags;
}

sub _declarationDeclarationSpecifiers {
    my ($cb, $self, $outerSelf, $topicsp) = @_;

    foreach (qw/declarationDeclarationSpecifiersdeclarationSpecifiers declarationDeclarationSpecifiersinitDeclaratorList/) {
	$log->debugf('[%s] %s = %s', whoami(__PACKAGE__), $_, $self->topic_data($_));
    }

    my $declarationDeclarationSpecifiersdeclarationSpecifiers = $self->topic_data('declarationDeclarationSpecifiersdeclarationSpecifiers');
    my $declarationDeclarationSpecifiersinitDeclaratorList = $self->topic_data('declarationDeclarationSpecifiersinitDeclaratorList');

    #
    # Count the number of typedef - Note that we are NOT here doing a grammar check on the number of storageSpecifier
    #
    my $nbTypedef = scalar(@{$declarationDeclarationSpecifiersdeclarationSpecifiers});
    foreach (@{$declarationDeclarationSpecifiersinitDeclaratorList}) {
	my ($line_columnp, $last_completed)  = @{$_};
	if ($nbTypedef > 0) {
	    $outerSelf->{_scope}->parseEnterTypedef($last_completed);
	} else {
	    $outerSelf->{_scope}->parseObscureTypedef($last_completed);
	}
    }

    foreach (@{$topicsp}) {
	$log->debugf('[%s] Reset \'%s\' topic data', whoami(__PACKAGE__), $_);
	$self->reset_topic_fired_data($_);
    }
}

1;
