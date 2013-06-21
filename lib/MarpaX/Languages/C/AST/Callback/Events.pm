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
    # From now on, the technique is always the same:
    #
    # For a rule that will be isolated for convenience (the grammar uses the action => deref if needed) like:
    # LHS ::= RHS1 RHS2 ... RHSn
    #
    # Suppose we want, at <LHS$> to inspect genome data <Gx,y,...> aggregation associated with rule <RHSn>.
    #
    # - We make sure <LHS$> completion event exist
    # - We replace all RHS of interest by LHSRHS, defined as: LHSRHS ::= RHS action => deref
    #   * This is NOT NECESSARY if RHSn is unique to LHS within all the grammar
    # - We make sure <LHSRHSn$> completion events exist
    # - We register callbacks/topics 'LHSRHSnTmp' with persistence_level 'level' that are subscribed to topics <Gx,y,...$> AND get <Gx,y,...> data
    # - We register callbacks 'LHSRHSn$' with topics 'LHSRHSn' and topic data persistence_level 'level' that depend on condition 'LHSRHS$' and that will:
    #   * push 'LHSRHSnTmp' topic data to 'LHSRHSn' topic data
    #   * reset 'LHSRHSnTmp' topic data
    #   * THE FIRST OF THE 'LHSRHSn$' WILL BE RESPONSIBLE TO DO A RESET DATA IF NOT YET DONE
    # - We register the callback 'LHSRHS$' that depend on condition 'LHSRHS$' and that will do the check
    #   * THIS EVENT WILL FLAG THE NEED FOR A RESET
    #
    # In this technique it is assumed that all RHS are different. Otherwise one would have to use
    # a temporary RHS "proxy". But this is not the case in the C grammar.
    # In addition the RHSs be UNIQUE to the whole grammar. Because we cannot affort recursion with them. There is
    # no problem on the other hand if an RHS depends on something that cycles.
    # #######################################################################################################################

    # ###############################################################################################
    # A directDeclarator introduces a typedefName only when it eventually participates in the grammar
    # rule:
    # declaration ::= declarationSpecifiers initDeclaratorList SEMICOLON
    #
    # Isolated to single rule:
    #
    # declarationDeclarationSpecifiers ::= declarationDeclarationSpecifiersDeclarationSpecifiers initDeclaratorList SEMICOLON  action => deref
    #
    # Take care: initDeclaratorList is left recursive
    # No problem with scope at the end of this rule
    # ###############################################################################################
    $self->_register_rule_callbacks($outerSelf,
				    {
					lhs => 'declarationCheck01',
					rhs => [ [ 'declarationCheck01declarationSpecifiers', [ 'storageClassSpecifierTypedef$' ] ],
						 [ 'declarationCheck01initDeclaratorList',    ['directDeclaratorIdentifier$'  ] ]
                                               ],
					method => \&_declarationCheck01,
					
				    }
	);

    # ###############################################################################################
    # In:
    # functionDefinition ::= declarationSpecifiers declarator declarationList? compoundStatement
    # typedef is syntactically allowed but never valid in either declarationSpecifiers or
    # declarationList.
    #
    # Take care: declarationList is left recursive
    # End of functionDefinition will match exitScope[], which WILL delayed until another call
    # to the Scope package. By definition, current topic level will be one level higher than the
    # level we have to check. We will use $self->topic_level_fired_data($topic, -1) to get that data.
    # ###############################################################################################
    #$self->_register_rule_callbacks($outerSelf,
#				    {
#					lhs => 'functionDefinition',
#					rhs => { 'declarationSpecifiers' => ['storageClassSpecifierTypedef$'],
#						 'declarationList'       => ['storageClassSpecifierTypedef$'  ],
#					},
#					method => \&_functionDefinition,
#				    }
#	);

    # #############################################################################################
    # Register scope callbacks:
    #
    # We want to have scopes happening exactly in time, but have a difficulty with the "reenterScope"
    # at function body definition, that can occur ONLY after the >>>file-scope<<< declarator.
    #
    # This is where it can happen, starting from the very beginning:
    #
    # translationUnit ::= externalDeclaration+
    # externalDeclaration ::= functionDefinition | declaration
    # functionDefinition ::= declarationSpecifiers declarator declarationList compoundStatement
    #                      | declarationSpecifiers declarator                 compoundStatement
    #                                                        ^
    #                                                      HERE
    # declarationList ::= declaration+
    # declaration ::= declarationSpecifiers SEMICOLON
    #               | declarationDeclarationSpecifiers                    action => deref
    #               | staticAssertDeclaration
    #
    # * We isolate file-scope declarator to a new LHS fileScopeDeclarator.
    # * We insert a nulled event <reenterScope[]> after fileScopeDeclarator
    # * We duplicate <enterScope> of a normal compoundStatement to a <maybeEnterScope> in a new compoundStatementWithMaybeEnterScope
    # I.e.:
    #
    # functionDefinition ::= declarationSpecifiers fileScopeDeclarator (<reenterScope>) declarationList compoundStatementWithMaybeEnterScope
    #                      | declarationSpecifiers fileScopeDeclarator (<reenterScope>)                 compoundStatementWithMaybeEnterScope
    #
    # Note that putting (<reenterScope>) on the two lines is redundant.
    # We associate a topic_data with <reenterScope> of persistence level 1.
    # At ^functionDefinition we attach and initialize the topic data to 0.
    # At '<reenterScope[]>' we set the topic to 1.
    #
    # - the following cases then can happen:
    # - fileScopeDeclarator end with a ')' :
    #   the nulled event is triggered:       exitScope[],reenterScope[]
    #
    #   > there is a declarationList:        ................................................. maybeEnterScope[]
    #   > there is no declarationList:       exitScope[],reenterScope[],maybeEnterScope[]
    #
    # - fileScopeDeclarator does not end end with a ')'
    #   the nulled event is triggered:       reenterScope[]
    #
    #   > there is a declarationList:        ................................................. maybeEnterScope[]
    #   > there is no declarationList:       reenterScope[],maybeEnterScope[]
    #
    # The rule is simple:
    # * Execution of <reenterScope[]> has highest priority PRIO and sets a topic data, with persistence 'level' to 1
    # - Take care, if <reenterScope[]> and <exitScope[]> are both matched, then the topic data is at current level - 1
    # * Execution of <exitScope[]> has priority PRIO-1 and is like:
    #   - noop if <reenterScope[]> topic data is 1 AT PREVIOUS topic level
    #   - real exitScope otherwise
    # * Execution of <maybeEnterScope[]> has priority PRIO-2 and is like:
    #   - noop if <reenterScope[]> topic data is 1, reset this data.
    #   - real enterScope otherwise
    #
    # Conclusion: there is NO notion of delayed exit scope anymore.
    # 
    # Implementation:
    # - <reenterScope[]> has priority 999
    # - <exitScope[]> has priority 998
    # - <maybeEnterScope[]> has priority 997
    # - <enterScope[]> has priority 996
    # #############################################################################################
    $self->_register_scope_callbacks($outerSelf);

    return $self;
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
	$log->warnf('[%s[%d]] No storageClassSpecifierTypedef, identifiers are %s', whoami(__PACKAGE__), $self->currentTopicLevel, $directDeclaratorIdentifier);
	return;
    }

    my $nbTypedef = $#{$storageClassSpecifierTypedef};
    if ($nbTypedef > 0) {
	#
	# Take the second typedef
	#
	my ($line_columnp, $last_completed)  = @{$storageClassSpecifierTypedef->[1]};
	$outerSelf->_croak("[%s[%d]] %s cannot appear more than once\n%s", whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $outerSelf->_show_line_and_col($line_columnp));
    }
    foreach (@{$directDeclaratorIdentifier}) {
	my ($line_columnp, $last_completed)  = @{$_};
	$log->debugf('[%s[%d]] Identifier %s at position %s', whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $line_columnp);
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
sub _declarationCheck01 {
    my ($cb, $self, $outerSelf, $cleanerTopic, @execArgs) = @_;
    #
    # Get the topics data we are interested in
    #
    my $declarationCheck01declarationSpecifiers = $self->topic_fired_data('declarationCheck01declarationSpecifiers');
    my $declarationCheck01initDeclaratorList = $self->topic_fired_data('declarationCheck01initDeclaratorList');

    $log->debugf('[%s[%d]] declarationCheck01declarationSpecifiers data is: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $declarationCheck01declarationSpecifiers);
    $log->debugf('[%s[%d]] declarationCheck01initDeclaratorList data is: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $declarationCheck01initDeclaratorList);

    #
    # By definition declarationCheck01declarationSpecifiers contains only typedefs
    # By definition declarationCheck01initDeclaratorList contains only directDeclaratorIdentifier
    #

    my $nbTypedef = $#{$declarationCheck01declarationSpecifiers};
    if ($nbTypedef > 0) {
	#
	# Take the second typedef
	#
	my ($line_columnp, $last_completed)  = @{$declarationCheck01declarationSpecifiers->[1]};
	$outerSelf->_croak("[%s[%d]] %s cannot appear more than once\n%s", whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $outerSelf->_show_line_and_col($line_columnp));
    }
    foreach (@{$declarationCheck01initDeclaratorList}) {
	my ($line_columnp, $last_completed)  = @{$_};
	$log->debugf('[%s[%d]] Identifier %s at position %s', whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $line_columnp);
	if ($nbTypedef >= 0) {
	    $outerSelf->{_scope}->parseEnterTypedef($last_completed);
	} else {
	    $outerSelf->{_scope}->parseObscureTypedef($last_completed);
	}
    }

    my $cleanerTopicData = $self->topic_fired_data($cleanerTopic);
    $log->debugf('[%s[%d]] Resetting \'%s\' topic data', whoami(__PACKAGE__), $self->currentTopicLevel, $cleanerTopic);
    @{$cleanerTopicData} = (0);
}
# ----------------------------------------------------------------------------------------
sub _initReenterScope {
    my ($cb, $self, $outerSelf, @execArgs) = @_;

    my $rc = 0;
    $log->debugf('[%s[%d]] Initializing \'reenterScope\' topic data to %s', whoami(__PACKAGE__), $self->currentTopicLevel, $rc);

    return $rc;
}
sub _reenterScope {
    my ($cb, $self, $outerSelf, @execArgs) = @_;

    if (grep {$_ eq 'exitScope[]'} @execArgs) {
	$self->topic_level_fired_data('reenterScope', -1, [1]);
	$log->debugf('[%s[%d]] Changed reenterScope topic data at level %d to %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->currentTopicLevel - 1, $self->topic_level_fired_data('reenterScope', -1));
    } else {
	$self->topic_level_fired_data('reenterScope', 0, [1]);
	$log->debugf('[%s[%d]] Changed reenterScope topic data to %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->topic_level_fired_data('reenterScope', 0));
    }
}
sub _exitScope {
    my ($cb, $self, $outerSelf, @execArgs) = @_;

    if (defined($self->topic_level_fired_data('reenterScope', -1)) && ($self->topic_level_fired_data('reenterScope', -1))->[0]) {
	$log->debugf('[%s[%d]] reenterScope topic data is %s. Do nothing.', whoami(__PACKAGE__), $self->currentTopicLevel - 1, $self->topic_level_fired_data('reenterScope', -1));
    } else {
	$outerSelf->{_scope}->parseExitScope();
	$self->popTopicLevel();
    }
}
sub _maybeEnterScope {
    my ($cb, $self, $outerSelf, @execArgs) = @_;

    if (($self->topic_level_fired_data('reenterScope', -1))->[0]) {
	$log->debugf('[%s[%d]] reenterScope topic data is %s. Resetted.', whoami(__PACKAGE__), $self->currentTopicLevel - 1, $self->topic_level_fired_data('reenterScope', -1));
	$self->topic_level_fired_data('reenterScope', -1, [0]);
    } else {
	$outerSelf->{_scope}->parseEnterScope();
	$self->pushTopicLevel();
    }
}
sub _enterScope {
    my ($cb, $self, $outerSelf, @execArgs) = @_;

    $outerSelf->{_scope}->parseEnterScope();
    $self->pushTopicLevel();
}
sub _register_scope_callbacks {
    my ($self, $outerSelf, $hashp) = @_;

    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => '^functionDefinition',
		     method =>  [ \&_initReenterScope, $self, $outerSelf ],
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      topic => {'reenterScope'=> 1},
		      topic_persistence => 'level',
		      condition => [ [qw/auto/] ],
		     )
		    )
	);
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => 'reenterScope[]',
		     method =>  [ \&_reenterScope, $self, $outerSelf ],
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [qw/auto/] ],
		      priority => 999
		     )
		    )
	);
    foreach (qw/rparen$ rcurly$/) {
	$self->register(MarpaX::Languages::C::AST::Callback::Method->new
			(
			 description => $_,
			 method =>  [ \&_exitScope, $self, $outerSelf ],
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [qw/auto/] ],
			  priority => 998
			 )
			)
	    );
    }
    foreach (qw/lcurlyMaybeEnterScope$/) {
	$self->register(MarpaX::Languages::C::AST::Callback::Method->new
			(
			 description => $_,
			 method =>  [ \&_maybeEnterScope, $self, $outerSelf ],
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [qw/auto/] ],
			  priority => 997
			 )
			)
	    );
    }
    foreach (qw/lparen$ lcurly$/) {
	$self->register(MarpaX::Languages::C::AST::Callback::Method->new
			(
			 description => $_,
			 method =>  [ \&_enterScope, $self, $outerSelf ],
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [qw/auto/] ],
			  priority => 997
			 )
			)
	    );
    }
}
# ----------------------------------------------------------------------------------------
sub _reset_helper {
    my ($cb, $self, $outerSelf, $cleanerTopic, $topicsp) = @_;

    my $cleanerTopicData = $self->topic_fired_data($cleanerTopic);
    if (! @{$cleanerTopicData} || ! $cleanerTopicData->[0]) {
      foreach (@{$topicsp}) {
        $log->debugf('[%s[%d]] Reset \'%s\' topic data', whoami(__PACKAGE__), $self->currentTopicLevel, $_);
        $self->reset_topic_fired_data($_);
      }
      $log->debugf('[%s[%d]] Setting \'%s\' topic data', whoami(__PACKAGE__), $self->currentTopicLevel, $cleanerTopic);
      @{$cleanerTopicData} = (1);
    } else {
      $log->debugf('[%s[%d]] \'%s\' topic data is %s', whoami(__PACKAGE__), $self->currentTopicLevel, $cleanerTopic, $cleanerTopicData);
    }
}
# ----------------------------------------------------------------------------------------
sub _push_and_reset_helper {
    my ($cb, $self, $outerSelf, $desttopic, $origtopic) = @_;

    $log->debugf('[%s[%d]] Push \'%s\' topic data to \'%s\' topic data', whoami(__PACKAGE__), $self->currentTopicLevel, $origtopic, $desttopic);
    push(@{$self->topic_fired_data($desttopic)}, @{$self->topic_fired_data($origtopic)});
    $log->debugf('[%s[%d]] Reset \'%s\' topic data', whoami(__PACKAGE__), $self->currentTopicLevel, $origtopic);
    $self->reset_topic_fired_data($origtopic);
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
    $log->tracef('[%s[%d]] %s', whoami(__PACKAGE__), $self->currentTopicLevel, $rc);
    return $rc;
}
# ----------------------------------------------------------------------------------------
sub _data_flag {
    my ($cb, $self, $outerSelf, $flag, $value, @events) = @_;

    $log->debugf('[%s[%d]] Setting \'%s\' flag to %d', whoami(__PACKAGE__), $self->currentTopicLevel, $flag, $value);
    $self->hscratchpad($flag, $value);
}
# ----------------------------------------------------------------------------------------
sub _data_storage {
    my ($cb, $self, $outerSelf, $flag, $genome, @events) = @_;

    my $rc = undef;
    if ($self->hscratchpad($flag)) {
	$rc = _storage_helper($cb, $self, $outerSelf, $genome);
	$log->debugf('[%s[%d]] Storing \'%s\' value: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $genome, $rc);
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

    # Rule model:
    # LHS ::= RHS1 RHS2 ... RHSn
    #
    # Events/topics used:
    # <LHS$>       event
    # <Gx$>        event
    # 'LHSRHSnTmp' topic but nothing depend on it. This is juste a storage area.
    # 'LHSRHSn'    topic but nothing depend on it. This is juste a storage area.
    # <LHSRHSn$>   event
    # <LHSRHSn$>   cleaner event

    #
    # The priorities should be:
    # - <Gx$>                3       Because we want to store genome data first
    # - <LHSRHSn$>/cleaner   2       Because we want to make sure data is clean before pushing
    # - <LHSRHSn$>           1       Push data
    # - <LHS$>               0       Check data

    #
    # register callbacks/topics 'LHSRHSnTmp' with persistence_level 'level' that are subscribed to topics <Gx,y,...> AND get <Gx,y,...> data
    #
    my @topics = ();
    my $i = 0;
    my $rhsCleaner = '';
    foreach (@{$hashp->{rhs}}) {
      my ($rhs, $genomep) = @{$_};
      if ($i++ == 0) {
        $rhsCleaner = $rhs;
      }
      my $topic = $rhs;
      push(@topics, $topic);
      my $topicTmp = $topic . 'Tmp';
      foreach my $genome (@{$genomep}) {
        $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                        (
                         description => $genome,
                         method =>  [ \&_storage_helper, $self, $outerSelf, $genome ],
                         option => MarpaX::Languages::C::AST::Callback::Option->new
                         (
                          condition => [ [qw/auto/ ] ],
                          topic => {$topicTmp => 1},
                          topic_persistence => 'level',
                          priority => 3,
                         )
                        )
                       );
      }
      $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                      (
                       description => $rhs . '$',
                       method =>  [ \&_push_and_reset_helper, $self, $outerSelf, $topic, $topicTmp ],
                       method_void => 1,
                       option => MarpaX::Languages::C::AST::Callback::Option->new
                       (
                        topic => {$topic => 1, $topicTmp => 1},
                        topic_persistence => 'level',
                        condition => [ [qw/auto/ ] ],
                        priority => 1,
                       )
                      )
                     );
    }
    #
    # Register reset procedure
    #
    my $cleanerTopic = $rhsCleaner . 'Resetted';
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                    (
                     description => $rhsCleaner . '$',
                     method =>  [ \&_reset_helper, $self, $outerSelf, $cleanerTopic, [ @topics ] ],
                     method_void => 1,
                     option => MarpaX::Languages::C::AST::Callback::Option->new
                     (
                      topic => {$cleanerTopic => 1},
                      topic_persistence => 'level',
                      condition => [ [qw/auto/ ] ],
                      priority => 2,
                     )
                    )
                   );
    #
    # Register check procedure
    #
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => $hashp->{lhs} . '$',
		     method =>  [ $hashp->{method}, $self, $outerSelf, $cleanerTopic, [ @topics ] ],
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [ qw/auto/ ] ],
		      priority => 1
		     )
		    )
                   );

}

sub _declarationDeclarationSpecifiers {
    my ($cb, $self, $outerSelf, $topicsp) = @_;

    foreach (qw/declarationDeclarationSpecifiersdeclarationSpecifiers declarationDeclarationSpecifiersinitDeclaratorList/) {
	$log->debugf('[%s[%d]] %s = %s', whoami(__PACKAGE__), $self->currentTopicLevel, $_, $self->topic_fired_data($_));
    }

    my $declarationDeclarationSpecifiersdeclarationSpecifiers = $self->topic_fired_data('declarationDeclarationSpecifiersdeclarationSpecifiers') || [];
    my $declarationDeclarationSpecifiersinitDeclaratorList = $self->topic_fired_data('declarationDeclarationSpecifiersinitDeclaratorList') || [];

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
	$log->debugf('[%s[%d]] Reset \'%s\' topic data', whoami(__PACKAGE__), $self->currentTopicLevel, $_);
	$self->reset_topic_fired_data($_);
    }
}

sub _functionDefinition {
    my ($cb, $self, $outerSelf, $topicsp) = @_;

    foreach (qw/functionDefinitiondeclarationSpecifiers functionDefinitiondeclarationList/) {
	$log->debugf('[%s[%d]] %s = %s', whoami(__PACKAGE__), $self->currentTopicLevel, $_, $self->topic_level_fired_data($_));
	$log->debugf('[%s[%d]] %s = %s', whoami(__PACKAGE__), $self->currentTopicLevel-1, $_, $self->topic_level_fired_data($_, -1));
    }

    my $functionDefinitiondeclarationSpecifiers = $self->topic_level_fired_data('functionDefinitiondeclarationSpecifiers', -1) || [];
    my $functionDefinitiondeclarationList = $self->topic_level_fired_data('functionDefinitiondeclarationList', -1) || [];

    #
    # Count the number of typedef
    #
    if (scalar(@{$functionDefinitiondeclarationSpecifiers}) > 0) {
	my ($line_columnp, $last_completed)  = @{$functionDefinitiondeclarationSpecifiers->[0]};
	$outerSelf->_croak("[%s] %s is not allowed in functionDefinition\'s declarationSpecifiers\n%s", whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $outerSelf->_show_line_and_col($line_columnp));
    }
    if (scalar(@{$functionDefinitiondeclarationList}) > 0) {
	my ($line_columnp, $last_completed)  = @{$functionDefinitiondeclarationList->[0]};
	$outerSelf->_croak("[%s] %s is not allowed in functionDefinition\'s declarationList\n%s", whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $outerSelf->_show_line_and_col($line_columnp));
    }

    foreach (@{$topicsp}) {
	$log->debugf('[%s[%d]] Reset \'%s\' topic data', whoami(__PACKAGE__), $self->currentTopicLevel, $_);
	$self->reset_topic_fired_data($_, -1);
    }
}

1;
