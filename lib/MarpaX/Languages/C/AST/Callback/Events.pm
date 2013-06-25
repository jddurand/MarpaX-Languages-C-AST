use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Callback::Events;
use MarpaX::Languages::C::AST::Util qw/whoami whowasi/;
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

    # #################################################################################################
    # For the lexeme that are paused 'before' we want to tell what is the context.
    # I.e. we want to know if there is a prediction of TYPEDEF_NAME, ENUMERATION_CONSTANT.
    # Otherwise this will be an identifier.
    # That's why it is very important to process the events BEFORE the paused lexemes.
    # We associate a one-shot topic that has no persistence for every pause-before lexeme.
    # #################################################################################################
    $self->_register_predictions($outerSelf);

    # #######################################################################################################################
    # From now on, the technique is always the same:
    #
    # For a rule that will be isolated for convenience (the grammar uses the action => deref if needed) like:
    # LHS ::= RHS1 RHS2 ... RHSn
    #
    # Suppose we want, at <LHS$> to inspect genome data <Gx,y,...> aggregation associated with rule <RHSn>.
    #
    # - We create a brand new callback object:
    # - We make sure LHS rule is unique, creating a proxy rule with action => deref if needed
    # - We make sure <LHS$> completion event exist
    # - We make sure <LHSRHSn$> completion events exist
    # - We make sure <^LHSRHSn> predictions events exist
    # - We create a dedicated callback that is subscribed to every unique <Gx$> and that collect its data
    #
    # - Every <^LHSRHSn> is resetting the data collections they depend upon
    # - Every <LHSRHSn$> is copying the data collections they depend upon and reset it
    # - The first LHSRHS has a special behaviour: if <LHSRHS$> is hitted while there is a pending <LHS$>,
    #   this mean that we are recursively hitting the rule. This will push one level. Levels are popped off at <LHS$>.
    #
    # - We create callbacks to <Gx$> that are firing the inner callback object.
    #
    # #######################################################################################################################

    # ################################################################################################
    # A directDeclarator introduces a typedef-name only when it eventually participates in the grammar
    # rule:
    # declaration ::= declarationSpecifiers initDeclaratorList SEMICOLON
    #
    # Isolated to single rule:
    #
    # declarationCheck ::= declarationCheckdeclarationSpecifiers declarationCheckinitDeclaratorList
    #                      SEMICOLON action => deref
    # ################################################################################################
    $self->_register_rule_callbacks($outerSelf,
                                    {
                                     lhs => 'declarationCheck',
                                     rhs => [ [ 'declarationCheckdeclarationSpecifiers', [ 'storageClassSpecifierTypedef' ] ],
                                              [ 'declarationCheckinitDeclaratorList',    ['directDeclaratorIdentifier'  ] ]
                                            ],
                                     method => \&_declarationCheck,
                                    }
                                   );


    # ------------------------------------------------------------------------------------------
    # directDeclarator constraint
    # ------------------------------------------------------------------------------------------
    # In:
    # functionDefinition ::= declarationSpecifiers declarator declarationList? compoundStatement
    # typedef is syntactically allowed but never valid in either declarationSpecifiers or
    # declarationList.
    #
    # Isolated to two rules:
    #
    # functionDefinitionCheck1 ::= functionDefinitionCheck1declarationSpecifiers fileScopeDeclarator
    #                              (<reenterScope>)
    #                              functionDefinitionCheck1declarationList
    #                              compoundStatementWithMaybeEnterScope action => deref
    # functionDefinitionCheck2 ::= functionDefinitionCheck2declarationSpecifiers fileScopeDeclarator
    #                              (<reenterScope>)
    #                              compoundStatementWithMaybeEnterScope action => deref
    #
    # Note: We arranged $rcurly to happen at the latest moment.
    #       This mean that functionDefinitionCheckXdeclarationSpecifiers will always belong
    #       to the data of the previous level.
    # ------------------------------------------------------------------------------------------
    $self->_register_rule_callbacks($outerSelf,
				    {
					lhs => 'functionDefinitionCheck1',
					rhs => [ [ 'functionDefinitionCheck1declarationSpecifiers', [ 'storageClassSpecifierTypedef' ] ],
						 [ 'functionDefinitionCheck1declarationList',       [ 'storageClassSpecifierTypedef' ] ]
					    ],
						     method => \&_functionDefinitionCheck1,
						     
				    }
	);
    $self->_register_rule_callbacks($outerSelf,
				    {
					lhs => 'functionDefinitionCheck2',
					rhs => [ [ 'functionDefinitionCheck2declarationSpecifiers', [ 'storageClassSpecifierTypedef' ] ],
					    ],
					method => \&_functionDefinitionCheck2,
					
				    }
	);

    # ------------------------------------------------------------------------------------------
    # directDeclarator constraint
    # ------------------------------------------------------------------------------------------
    # In:
    # structDeclarator ::= declarator COLON constantExpression | declarator
    #
    # ordinary name space names cannot be defined. Therefore all parse symbol activity must be
    # suspended for structDeclarator.
    #
    # We simply create a topic data 'structDeclarator' of persistence 'level', initialized to 0 at
    # ^structDeclarator and
    # setted to 1 at structDeclaratordeclarator, where:
    # structDeclaratordeclarator ::= declarator
    #
    # In _declarationCheck(), we suspend activity if the flag is on.
    #
    # It has quite a high priority so that the flag is setted before _declarationCheck is
    # called.
    # ------------------------------------------------------------------------------------------
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => '^structDeclaratordeclarator',
		     method =>  [ \&_structDeclaratordeclarator, $self, $outerSelf, 0 ],
		     method_mode => 'replace',
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [qw/auto/] ],
		      topic => {'structDeclaratordeclarator'=> 1},
		      topic_persistence => 'level',
		      priority => 500
		     )
		    )
	);
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => 'structDeclaratordeclarator$',
		     method =>  [ \&_structDeclaratordeclarator, $self, $outerSelf, 1 ],
		     method_mode => 'replace',
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [qw/auto/] ],
		      topic => {'structDeclaratordeclarator'=> 1},
		      topic_persistence => 'level',
		      priority => 500
		     )
		    )
	);

    # ################################################################################################
    # An enumerationConstantIdentifier introduces a enum-name. Full point.
    # rule:
    # enumerationConstantIdentifier ::= IDENTIFIER
    # ################################################################################################
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => 'enumerationConstantIdentifier$',
		     method =>  [ \&_enumerationConstantIdentifier, $self, $outerSelf ],
		     option => MarpaX::Languages::C::AST::Callback::Option->new
		     (
		      condition => [ [qw/auto/] ],
		     )
		    )
	);

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
    # - <maybeEnterScope[]> has priority 997
    # - <enterScope[]> has priority 996
    # - <exitScope[]> has priority -999
    #
    # - -999 for <exitScope[]> because this must be a showstopper in the C rules: always at the end
    #   plus it will DESTROY all the topics
    # #############################################################################################
    $self->_register_scope_callbacks($outerSelf);

    return $self;
}
# ----------------------------------------------------------------------------------------
sub _structDeclaratordeclarator {
    my ($cb, $self, $outerSelf, $value, @execArgs) = @_;

    my $topic = 'structDeclaratordeclarator';
    $log->debugf('[%s[%d]] Setting %s to [%d]', whoami(__PACKAGE__), $self->currentTopicLevel, $topic, $value);

    return $value
}
sub _enumerationConstantIdentifier {
    my ($cb, $self, $outerSelf, @execArgs) = @_;

    my $enum = $outerSelf->_last_completed('enumerationConstantIdentifier');
    $log->debugf('[%s[%d]] New enum \'%s\' at position %s', whoami(__PACKAGE__), $self->currentTopicLevel, $enum, $outerSelf->_line_column());
    $outerSelf->{_scope}->parseEnterEnum($enum);
}
# ----------------------------------------------------------------------------------------
sub _functionDefinitionCheck1 {
    my ($cb, $self, $outerSelf, @execArgs) = @_;
    #
    # Get the topics data we are interested in
    #
    my $functionDefinitionCheck1declarationSpecifiers = $self->topic_level_fired_data('functionDefinitionCheck1declarationSpecifiers$', -1);
    my $functionDefinitionCheck1declarationList = $self->topic_fired_data('functionDefinitionCheck1declarationList$');

    $log->debugf('[%s[%d]] functionDefinitionCheck1declarationSpecifiers data is: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $functionDefinitionCheck1declarationSpecifiers);
    $log->debugf('[%s[%d]] functionDefinitionCheck1declarationList data is: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $functionDefinitionCheck1declarationList);

    #
    # By definition functionDefinitionCheck1declarationSpecifiers contains only typedefs
    # By definition functionDefinitionCheck1declarationList contains only typedefs
    #
    my $nbTypedef1 = $#{$functionDefinitionCheck1declarationSpecifiers};
    if ($nbTypedef1 >= 0) {
	my ($line_columnp, $last_completed)  = @{$functionDefinitionCheck1declarationSpecifiers->[0]};
	$outerSelf->_croak("[%s[%d]] %s is not valid in a function declaration specifier\n%s\n", whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $outerSelf->_show_line_and_col($line_columnp));
    }

    my $nbTypedef2 = $#{$functionDefinitionCheck1declarationList};
    if ($nbTypedef2 >= 0) {
	my ($line_columnp, $last_completed)  = @{$functionDefinitionCheck1declarationList->[0]};
	$outerSelf->_croak("[%s[%d]] %s is not valid in a function declaration list\n%s\n", whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $outerSelf->_show_line_and_col($line_columnp));
    }

    #
    # Reset topic data
    #
    foreach (qw/functionDefinitionCheck1declarationSpecifiers$ functionDefinitionCheck1declarationList$/) {
	$log->debugf('[%s[%d]] %s topic data reset', whoami(__PACKAGE__), $self->currentTopicLevel, $_);
	$self->topic_fired_data($_, []);
    }
    
}
sub _functionDefinitionCheck2 {
    my ($cb, $self, $outerSelf, $cleanerTopic, @execArgs) = @_;
    #
    # Get the topics data we are interested in
    #
    my $functionDefinitionCheck2declarationSpecifiers = $self->topic_level_fired_data('functionDefinitionCheck2declarationSpecifiers$', -1);

    $log->debugf('[%s[%d]] functionDefinitionCheck2declarationSpecifiers data is: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $functionDefinitionCheck2declarationSpecifiers);

    #
    # By definition functionDefinitionCheck2declarationSpecifiers contains only typedefs
    #
    my $nbTypedef = $#{$functionDefinitionCheck2declarationSpecifiers};
    if ($nbTypedef >= 0) {
	my ($line_columnp, $last_completed)  = @{$functionDefinitionCheck2declarationSpecifiers->[0]};
	$outerSelf->_croak("[%s[%d]] %s is not valid in a function declaration specifier\n%s\n", whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $outerSelf->_show_line_and_col($line_columnp));
    }

    #
    # Reset topic data
    #
    foreach (qw/functionDefinitionCheck2declarationSpecifiers$/) {
	$log->debugf('[%s[%d]] %s topic data reset', whoami(__PACKAGE__), $self->currentTopicLevel, $_);
	$self->topic_fired_data($_, []);
    }
}
# ----------------------------------------------------------------------------------------
sub _declarationCheck {
    my ($cb, $self, $outerSelf, @execArgs) = @_;

    #
    # Check if we are in _structDeclaratordeclarator context
    #
    my $structDeclaratordeclarator = $self->topic_fired_data('structDeclaratordeclarator') || [0];
    if ($structDeclaratordeclarator->[0]) {
	$log->debugf('[%s[%d]] structDeclaratordeclarator context, doing nothing.', whoami(__PACKAGE__), $self->currentTopicLevel);
	return;
    } else {
	$log->debugf('[%s[%d]] Not in a structDeclaratordeclarator context.', whoami(__PACKAGE__), $self->currentTopicLevel);
    }
    #
    # Get the topics data we are interested in
    #
    my $declarationCheckdeclarationSpecifiers = $self->topic_fired_data('declarationCheckdeclarationSpecifiers$');
    my $declarationCheckinitDeclaratorList = $self->topic_fired_data('declarationCheckinitDeclaratorList$');

    $log->debugf('[%s[%d]] declarationCheckdeclarationSpecifiers data is: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $declarationCheckdeclarationSpecifiers);
    $log->debugf('[%s[%d]] declarationCheckinitDeclaratorList data is: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $declarationCheckinitDeclaratorList);

    #
    # By definition declarationCheckdeclarationSpecifiers contains only typedefs
    # By definition declarationCheckinitDeclaratorList contains only directDeclaratorIdentifier
    #

    my $nbTypedef = $#{$declarationCheckdeclarationSpecifiers};
    if ($nbTypedef > 0) {
	#
	# Take the second typedef
	#
	my ($line_columnp, $last_completed)  = @{$declarationCheckdeclarationSpecifiers->[1]};
	$outerSelf->_croak("[%s[%d]] %s cannot appear more than once\n%s\n", whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $outerSelf->_show_line_and_col($line_columnp));
    }
    foreach (@{$declarationCheckinitDeclaratorList}) {
	my ($line_columnp, $last_completed)  = @{$_};
	$log->debugf('[%s[%d]] Identifier %s at position %s', whoami(__PACKAGE__), $self->currentTopicLevel, $last_completed, $line_columnp);
	if ($nbTypedef >= 0) {
	    $outerSelf->{_scope}->parseEnterTypedef($last_completed);
	} else {
	    $outerSelf->{_scope}->parseObscureTypedef($last_completed);
	}
    }
    #
    # Reset topic data
    #
    foreach (qw/declarationCheckdeclarationSpecifiers$ declarationCheckinitDeclaratorList$/) {
	$log->debugf('[%s[%d]] %s topic data reset', whoami(__PACKAGE__), $self->currentTopicLevel, $_);
	$self->topic_fired_data($_, []);
    }
    
}
# ----------------------------------------------------------------------------------------
sub _initReenterScope {
    my ($cb, $self, $outerSelf, @execArgs) = @_;

    #
    # No need to init 'reenterScope' to an empty array. It can happen only at
    # file-scope level, i.e. at this stage the data is undef. Callback will
    # automatically create it using the return value of this method
    #

    my $rc = 0;
    $log->debugf('[%s[%d]] Setting \'reenterScope\' topic data to [%d]', whoami(__PACKAGE__), $self->currentTopicLevel, $rc);

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
    foreach (qw/rparen$ rcurly$/) {
	$self->register(MarpaX::Languages::C::AST::Callback::Method->new
			(
			 description => $_,
			 method =>  [ \&_exitScope, $self, $outerSelf ],
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [qw/auto/] ],
			  priority => -999
			 )
			)
	    );
    }
}
# ----------------------------------------------------------------------------------------
sub _storage_helper {
    my ($cb, $self, $outerSelf, $event) = @_;
    #
    # The event name, by convention, is 'symbol$' or '^$symbol'
    #
    my $symbol = $event;
    my $rc;
    if (substr($symbol, 0, 1) eq '^') {
	substr($symbol, 0, 1, '');
	$rc = [ $outerSelf->_line_column() ];
    } elsif (substr($symbol, -1, 1) eq '$') {
	substr($symbol, -1, 1, '');
	$rc = [ $outerSelf->_line_column(), $outerSelf->_last_completed($symbol) ];
    }
    $log->debugf('%s[%s[%d]] Callback \'%s\', topic \'%s\', data %s', $self->log_prefix, whoami(__PACKAGE__), $self->currentTopicLevel, $cb->extra_description || $cb->description, $event, $rc);
    return $rc;
}
# ----------------------------------------------------------------------------------------
sub _reset_helper {
    my ($cb, $self, $outerSelf, $genomep) = @_;

    $log->debugf('%s[%s[%d]] Callback \'%s\', resetting data of topics %s', $self->log_prefix, whoami(__PACKAGE__), $self->currentTopicLevel, $cb->extra_description || $cb->description, $genomep);

    return ();
}
# ----------------------------------------------------------------------------------------
sub _push_and_reset_helper {
    my ($cb, $self, $outerSelf, $topicsp) = @_;

    my @rc = ();
    foreach (@{$topicsp}) {
	my $topic = $_;
	$log->debugf('%s[%s[%d]] Callback \'%s\', collecting topic \'%s\' data: %s', $self->log_prefix, whoami(__PACKAGE__), $self->currentTopicLevel, $cb->extra_description || $cb->description, $topic, $self->topic_fired_data($topic));
	push(@rc, @{$self->topic_fired_data($topic)});
	$self->topic_fired_data($topic, []);
    }

    $log->debugf('%s[%s[%d]] Callback \'%s\', collected data: %s', $self->log_prefix, whoami(__PACKAGE__), $self->currentTopicLevel, $cb->extra_description || $cb->description, \@rc);
    return @rc;
}
# ----------------------------------------------------------------------------------------
sub _register_predictions {
    my ($self, $outerSelf, $hashp) = @_;

    foreach (qw/^typedefnameLexeme ^enumerationConstantLexeme/) {
	$self->register(MarpaX::Languages::C::AST::Callback::Method->new
			(
			 description => $_,
                         method =>  [ \&_storage_helper, $self, $outerSelf, $_ ],
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [qw/auto/] ],
			  topic => {$_ => 1},
			  topic_persistence => 'none',
			  priority => 0
			 )
			)
	    );
    }
}
# ----------------------------------------------------------------------------------------
sub _incScratchpad {
  my ($cb, $self, $flag) = @_;

  ++$self->hscratchpad($flag);
  $log->debugf('%s[%s[%d]] Number of expected end of rule: %d', $self->log_prefix, whoami(__PACKAGE__), $self->currentTopicLevel, $self->hscratchpad($flag));
}

sub _subFire {
  my ($cb, $self, $callback, $lhs, $subEventsp, @events) = @_;

  my @subEvents = grep {exists($subEventsp->{$_})} @_;
  if (@subEvents) {
    $log->debugf('%s[%s[%d]] Sub-firing %s callback with %s', $self->log_prefix, whoami(__PACKAGE__), $self->currentTopicLevel, $lhs, \@subEvents);
    $callback->exec(@subEvents);
  }
}

sub _register_rule_callbacks {
  my ($self, $outerSelf, $hashp) = @_;

  #
  # subEvents will be the list of events that we forward to the inner callback object
  #
  my %subEvents = ();

  #
  # Create inner callback object
  #
  my $callback = MarpaX::Languages::C::AST::Callback->new(log_prefix => '  ');

  #
  # Collect the unique list of <Gx$>
  #
  my %genomeEvents = ();
  foreach (@{$hashp->{rhs}}) {
    my ($rhs, $genomep) = @{$_};
    foreach (@{$genomep}) {
	my $event = $_ . '$';
	++$genomeEvents{$event};
	++$subEvents{$event};
    }
  }
  #
  # Create data Gx$ data collectors. The data will be collected in a
  # topic with the same name: Gx
  #
  foreach (keys %genomeEvents) {
	$callback->register(MarpaX::Languages::C::AST::Callback::Method->new
			    (
			     description => $_,
			     method =>  [ \&_storage_helper, $callback, $outerSelf, $_ ],
			     option => MarpaX::Languages::C::AST::Callback::Option->new
			     (
			      topic => {$_ => 1},
			      topic_persistence => 'level',
			      condition => [ [ 'auto' ] ],  # == match on description
			      priority => 999,
			     )
			    )
	    );
  }
  
  my $i = 0;
  foreach (@{$hashp->{rhs}}) {
    my ($rhs, $genomep) = @{$_};

    my %topic = ();
    foreach (@{$genomep}) {
	$topic{$_ . '$'} = 1;
    }
    #
    # ^rhs will reset all Gx$ topics on which it depend
    # We also assign topics explicitely so that they are created if needed
    #
    my $event = '^' . $rhs;
    ++$subEvents{$event};
    $callback->register(MarpaX::Languages::C::AST::Callback::Method->new
			(
			 description => $event,
			 method =>  [ \&_reset_helper, $callback, $outerSelf, [ keys %topic ] ],
			 method_mode => 'replace',
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [ 'auto' ] ],  # == match on description
			  topic => \%topic,
			  topic_persistence => 'level',
			  priority => -1,
			 )
			)
	);

    #
    # rhs$ will collect into $rhs topic all Gx$ topics on which it depend and reset them
    #
    $event = $rhs . '$';
    ++$subEvents{$event};
    $callback->register(MarpaX::Languages::C::AST::Callback::Method->new
			(
			 description => $event,
			 method =>  [ \&_push_and_reset_helper, $callback, $outerSelf, [ keys %topic ] ],
			 method_mode => 'push',
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [ 'auto' ] ],  # == match on description
			  topic => {$rhs  . '$' => 1},
			  topic_persistence => 'level',
			  priority => 1,
			 )
			)
	);

    if ($i++ == 0) {
	$callback->hscratchpad($rhs, 0);
	#
	# The very first rule is special:
	# If we hit its completion and there is a pending LHS$
	# completion, this mean we have recursed into LHS.
	# To know that, we maitain a counter of number of
	# pending LHS$ in the scratchpad
	#
	$callback->register(MarpaX::Languages::C::AST::Callback::Method->new
			    (
			     description => $rhs . '$',
			     method => [ \&_incScratchpad, $callback, $rhs ],
			    )
	    );
    }
  }

  #
  # Final callback
  #
  ++$subEvents{$hashp->{lhs} . '$'};
  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => $hashp->{lhs} . '$',
                   method => [ $hashp->{method}, $callback, $outerSelf ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
		    condition => [ [ 'auto' ] ],  # == match on description
                   )
                  )
                 );

  #
  ## Sub-fire events for this sub-callback object
  #
  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => $hashp->{lhs} . ' sub-events',
                   method => [ \&_subFire, $self, $callback, $hashp->{lhs}, \%subEvents ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
                    condition => [
                                  [ sub { my $cb = shift;
                                          my $subEventsp = shift;
                                          return grep {exists($subEventsp->{$_})} @_;
                                        },
                                    \%subEvents
                                  ]
                                 ]
                   )
                  )
                 );

}

1;
