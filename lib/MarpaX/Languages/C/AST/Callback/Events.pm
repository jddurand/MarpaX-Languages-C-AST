use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Callback::Events;
use MarpaX::Languages::C::AST::Util qw/:all/;
use parent qw/MarpaX::Languages::C::AST::Callback/;

# ABSTRACT: Events callback when translating a C source to an AST

use Log::Any qw/$log/;
use Carp qw/croak/;
use Storable qw/dclone/;
use SUPER;

# VERSION

=head1 DESCRIPTION

This modules implements the Marpa events callback using the very simple framework MarpaX::Languages::C::AST::Callback. it is useful because it shows the FUNCTIONAL things that appear within the events: monitor the TYPEDEFs, introduce/obscure names in name space, apply the few grammar constraints needed at parsing time. And the TECHNICAL things i.e. recursivity of the grammar.

=cut

sub new {
    my ($class, $outerSelf) = @_;

    my $self = $class->SUPER();

    if (! defined($outerSelf) || ref($outerSelf) ne 'MarpaX::Languages::C::AST') {
      croak 'outerSelf must be a reference to MarpaX::Languages::C::AST';
    }

    $self->hscratchpad('_impl', $outerSelf->{_impl});
    $self->hscratchpad('_scope', $outerSelf->{_scope});
    $self->hscratchpad('_sourcep', $outerSelf->{_sourcep});

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
    my @callbacks = ();
    push(@callbacks,
         $self->_register_rule_callbacks({
                                          lhs => 'declarationCheck',
                                          rhs => [ [ 'declarationCheckdeclarationSpecifiers', [ 'storageClassSpecifierTypedef' ] ],
                                                   [ 'declarationCheckinitDeclaratorList',    ['directDeclaratorIdentifier'  ] ]
                                                 ],
                                          method => \&_declarationCheck,
                                          # ---------------------------
                                          # directDeclarator constraint
                                          # ---------------------------
                                          # In:
                                          # structDeclarator ::= declarator COLON constantExpression | declarator
                                          #
                                          # ordinary name space names cannot be defined. Therefore all parse symbol activity must be
                                          # suspended for structDeclarator.
                                          #
                                          # structDeclarator$ will be hitted many time (right recursive), but its container
                                          # structDeclaration will be hitted only once.
                                          # ---------------------------
                                          counters => {
                                                       'structContext' => [ 'structContextStart[]', 'structContextEnd[]' ]
                                                      },
                                         }
                                        )
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
    #                              functionDefinitionCheck1declarationList
    #                              compoundStatementWithMaybeEnterScope action => deref
    # functionDefinitionCheck2 ::= functionDefinitionCheck2declarationSpecifiers fileScopeDeclarator
    #                              compoundStatementWithMaybeEnterScope action => deref
    #
    # Note: We arranged $rcurly to happen at the latest moment.
    #       This mean that functionDefinitionCheckXdeclarationSpecifiers will always belong
    #       to the data of the previous level.
    # ------------------------------------------------------------------------------------------
    push(@callbacks,
         $self->_register_rule_callbacks({
                                          lhs => 'functionDefinitionCheck1',
                                          rhs => [ [ 'functionDefinitionCheck1declarationSpecifiers', [ 'storageClassSpecifierTypedef' ] ],
                                                   [ 'functionDefinitionCheck1declarationList',       [ 'storageClassSpecifierTypedef' ] ]
                                                 ],
                                          method => \&_functionDefinitionCheck1,
                                         }
                                        )
        );
    push(@callbacks,
         $self->_register_rule_callbacks({
                                          lhs => 'functionDefinitionCheck2',
                                          rhs => [ [ 'functionDefinitionCheck2declarationSpecifiers', [ 'storageClassSpecifierTypedef' ] ],
                                                 ],
                                          method => \&_functionDefinitionCheck2,
                                         }
                                        )
        );

    # ------------------------------------------------------------------------------------------
    # directDeclarator constraint
    # ------------------------------------------------------------------------------------------
    # In:
    # parameterDeclaration ::= declarationSpecifiers declarator
    # typedef is syntactically allowed but never valid.
    #
    # Isolated to:
    #
    # parameterDeclarationCheck ::= declarationSpecifiers declarator
    # ------------------------------------------------------------------------------------------
    push(@callbacks,
         $self->_register_rule_callbacks({
                                          lhs => 'parameterDeclarationCheck',
                                          rhs => [ [ 'parameterDeclarationdeclarationSpecifiers', [ 'storageClassSpecifierTypedef' ] ]
                                                 ],
                                          method => \&_parameterDeclarationCheck,
                                         }
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
		     method =>  [ \&_enumerationConstantIdentifier ],
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
    # We associate a topic_data with <reenterScope> of persistence 'level'.
    # At ^functionDefinition we attach and initialize the topic data to 0.
    # At '<reenterScope[]>' we set the topic to 1 if current topic level is 0 (i.e. file scope level)
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
    # * Execution of <reenterScope[]> has highest priority PRIO and sets a topic data, with persistence 'level' to 1 if currentTopicLevel is 0
    # - Take care, if <reenterScope[]> and <exitScope[]> are both matched, then the topic data is at current level - 1
    # * Execution of <exitScope[]> has priority PRIO-1 and is like:
    #   - noop if <reenterScope[]> topic data is 1 AT PREVIOUS topic level, and currentTopicLevel is 1
    #   - real exitScope otherwise
    # * Execution of <maybeEnterScope[]> has priority PRIO-2 and is like:
    #   - noop if <reenterScope[]> topic data is 1 or currentTopicLevel is not 0, reset this data.
    #   - real enterScope otherwise
    # * functionDefinition$ is guaranteed to always close correctly the scopes.
    # * declaration$ will be catched up to force closing of all the scopes.
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
    $self->_register_scope_callbacks(@callbacks);

    return $self;
}
# ----------------------------------------------------------------------------------------
sub _enumerationConstantIdentifier {
    my ($method, $callback, $eventsp) = @_;

    my $enum = lastCompleted($callback->hscratchpad('_impl'), 'enumerationConstantIdentifier');
    $log->debugf('[%s[%d]] New enum \'%s\' at position %s', whoami(__PACKAGE__), $callback->currentTopicLevel, $enum, lineAndCol($callback->hscratchpad('_impl')));
    $callback->hscratchpad('_scope')->parseEnterEnum($enum);
}
# ----------------------------------------------------------------------------------------
sub _parameterDeclarationCheck {
    my ($method, $callback, $eventsp) = @_;
    #
    # Get the topics data we are interested in
    #
    my $parameterDeclarationdeclarationSpecifiers = $callback->topic_level_fired_data('parameterDeclarationdeclarationSpecifiers$');

    $log->debugf('%s[%s[%d]] parameterDeclarationdeclarationSpecifiers data is: %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $parameterDeclarationdeclarationSpecifiers);

    #
    # By definition parameterDeclarationdeclarationSpecifiers contains only typedefs
    #
    my $nbTypedef = $#{$parameterDeclarationdeclarationSpecifiers};
    if ($nbTypedef >= 0) {
	my ($line_columnp, $last_completed)  = @{$parameterDeclarationdeclarationSpecifiers->[0]};
	logCroak("[%s[%d]] %s is not valid in a parameter declaration\n%s\n", whoami(__PACKAGE__), $callback->currentTopicLevel, $last_completed, showLineAndCol(@{$line_columnp}, $callback->hscratchpad('_sourcep')));
    }
}
# ----------------------------------------------------------------------------------------
sub _functionDefinitionCheck1 {
    my ($method, $callback, $eventsp) = @_;
    #
    # Get the topics data we are interested in
    #
    my $functionDefinitionCheck1declarationSpecifiers = $callback->topic_level_fired_data('functionDefinitionCheck1declarationSpecifiers$', -1);
    my $functionDefinitionCheck1declarationList = $callback->topic_fired_data('functionDefinitionCheck1declarationList$');

    $log->debugf('%s[%s[%d]] functionDefinitionCheck1declarationSpecifiers data is: %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $functionDefinitionCheck1declarationSpecifiers);
    $log->debugf('%s[%s[%d]] functionDefinitionCheck1declarationList data is: %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $functionDefinitionCheck1declarationList);

    #
    # By definition functionDefinitionCheck1declarationSpecifiers contains only typedefs
    # By definition functionDefinitionCheck1declarationList contains only typedefs
    #
    my $nbTypedef1 = $#{$functionDefinitionCheck1declarationSpecifiers};
    if ($nbTypedef1 >= 0) {
	my ($line_columnp, $last_completed)  = @{$functionDefinitionCheck1declarationSpecifiers->[0]};
	logCroak("[%s[%d]] %s is not valid in a function declaration specifier\n%s\n", whoami(__PACKAGE__), $callback->currentTopicLevel, $last_completed, showLineAndCol(@{$line_columnp}, $callback->hscratchpad('_sourcep')));
    }

    my $nbTypedef2 = $#{$functionDefinitionCheck1declarationList};
    if ($nbTypedef2 >= 0) {
	my ($line_columnp, $last_completed)  = @{$functionDefinitionCheck1declarationList->[0]};
	logCroak("[%s[%d]] %s is not valid in a function declaration list\n%s\n", whoami(__PACKAGE__), $callback->currentTopicLevel, $last_completed, showLineAndCol(@{$line_columnp}, $callback->hscratchpad('_sourcep')));
    }
}
sub _functionDefinitionCheck2 {
    my ($method, $callback, $eventsp) = @_;
    #
    # Get the topics data we are interested in
    #
    my $functionDefinitionCheck2declarationSpecifiers = $callback->topic_level_fired_data('functionDefinitionCheck2declarationSpecifiers$', -1);

    $log->debugf('%s[%s[%d]] functionDefinitionCheck2declarationSpecifiers data is: %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $functionDefinitionCheck2declarationSpecifiers);

    #
    # By definition functionDefinitionCheck2declarationSpecifiers contains only typedefs
    #
    my $nbTypedef = $#{$functionDefinitionCheck2declarationSpecifiers};
    if ($nbTypedef >= 0) {
	my ($line_columnp, $last_completed)  = @{$functionDefinitionCheck2declarationSpecifiers->[0]};
	logCroak("[%s[%d]] %s is not valid in a function declaration specifier\n%s\n", whoami(__PACKAGE__), $callback->currentTopicLevel, $last_completed, showLineAndCol(@{$line_columnp}, $callback->hscratchpad('_sourcep')));
    }
}
# ----------------------------------------------------------------------------------------
sub _declarationCheck {
    my ($method, $callback, $eventsp) = @_;

    #
    # Check if we are in structContext context
    #
    my $structContext = $callback->topic_fired_data('structContext') || [0];
    if ($structContext->[0]) {
	$log->debugf('%s[%s[%d]] structContext is true, doing nothing.', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel);
	return;
    } else {
	$log->debugf('%s[%s[%d]] structContext is false, continuing.', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel);
    }
    #
    # Get the topics data we are interested in
    #
    my $declarationCheckdeclarationSpecifiers = $callback->topic_fired_data('declarationCheckdeclarationSpecifiers$');
    my $declarationCheckinitDeclaratorList = $callback->topic_fired_data('declarationCheckinitDeclaratorList$');

    $log->debugf('%s[%s[%d]] declarationCheckdeclarationSpecifiers data is: %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $declarationCheckdeclarationSpecifiers);
    $log->debugf('%s[%s[%d]] declarationCheckinitDeclaratorList data is: %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $declarationCheckinitDeclaratorList);

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
	logCroak("[%s[%d]] %s cannot appear more than once\n%s\n", whoami(__PACKAGE__), $callback->currentTopicLevel, $last_completed, showLineAndCol(@{$line_columnp}, $callback->hscratchpad('_sourcep')));
    }
    foreach (@{$declarationCheckinitDeclaratorList}) {
	my ($line_columnp, $last_completed)  = @{$_};
	$log->debugf('%s[%s[%d]] Identifier %s at position %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $last_completed, $line_columnp);
	if ($nbTypedef >= 0) {
	    $callback->hscratchpad('_scope')->parseEnterTypedef($last_completed);
	} else {
	    $callback->hscratchpad('_scope')->parseObscureTypedef($last_completed);
	}
    }
}
# ----------------------------------------------------------------------------------------
sub _initReenterScope {
    my ($method, $callback, $eventsp) = @_;

    #
    # No need to init 'reenterScope' to an empty array. It can happen only at
    # file-scope level, i.e. at this stage the data is undef. Callback will
    # automatically create it using the return value of this method
    #

    my $rc = 0;
    $log->debugf('[%s[%d]] Setting \'reenterScope\' topic data to [%d]', whoami(__PACKAGE__), $callback->currentTopicLevel, $rc);

    return $rc;
}
sub _reenterScope {
    my ($method, $callback, $eventsp) = @_;

    if (grep {$_ eq 'exitScope[]'} @{$eventsp}) {
	$callback->topic_level_fired_data('reenterScope', -1, [1]);
	$log->debugf('[%s[%d]] Changed reenterScope topic data at previous level to %s', whoami(__PACKAGE__), $callback->currentTopicLevel, $callback->topic_level_fired_data('reenterScope', -1));
    } else {
	$callback->topic_level_fired_data('reenterScope', 0, [1]);
	$log->debugf('[%s[%d]] Changed reenterScope topic data to %s', whoami(__PACKAGE__), $callback->currentTopicLevel, $callback->topic_level_fired_data('reenterScope', 0));
    }
}
sub _exitScope {
    my ($method, $callback, $eventsp, @callbacks) = @_;

    if ($callback->currentTopicLevel == 1 &&
        defined($callback->topic_level_fired_data('reenterScope', -1)) && ($callback->topic_level_fired_data('reenterScope', -1))->[0]) {
	$log->debugf('[%s[%d]] reenterScope topic data at previous level is is %s. Do nothing.', whoami(__PACKAGE__), $callback->currentTopicLevel, $callback->topic_level_fired_data('reenterScope', -1));
    } else {
	$callback->hscratchpad('_scope')->parseExitScope();
        foreach ($callback, @callbacks) {
          $_->popTopicLevel();
        }
    }
}
sub _closeScopes {
    my ($method, $callback, $eventsp, @callbacks) = @_;

    while ($callback->currentTopicLevel > 0) {
      $callback->hscratchpad('_scope')->parseExitScope();
      foreach ($callback, @callbacks) {
        $_->popTopicLevel();
      }
    }
}
sub _maybeEnterScope {
    my ($method, $callback, $eventsp, @callbacks) = @_;

    if ($callback->currentTopicLevel == 1 &&
        ($callback->topic_level_fired_data('reenterScope', -1))->[0]) {
	$log->debugf('[%s[%d]] reenterScope topic data at previous level is is %s. Resetted.', whoami(__PACKAGE__), $callback->currentTopicLevel, $callback->topic_level_fired_data('reenterScope', -1));
	$callback->topic_level_fired_data('reenterScope', -1, [0]);
    } else {
	$callback->hscratchpad('_scope')->parseEnterScope();
        foreach ($callback, @callbacks) {
          $_->pushTopicLevel();
        }
    }
}
sub _enterScope {
    my ($method, $callback, $eventsp, @callbacks) = @_;

    $callback->hscratchpad('_scope')->parseEnterScope();
    foreach ($callback, @callbacks) {
      $_->pushTopicLevel();
    }
}
sub _register_scope_callbacks {
    my ($self, @callbacks) = @_;

    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
		    (
		     description => '^functionDefinition',
		     method =>  [ \&_initReenterScope ],
                     method_mode => 'replace',
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
		     method =>  [ \&_reenterScope ],
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
			 method =>  [ \&_maybeEnterScope, @callbacks ],
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
			 method =>  [ \&_enterScope, @callbacks ],
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
			 method =>  [ \&_exitScope, @callbacks ],
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [qw/auto/] ],
			  priority => -999
			 )
			)
	    );
    }
    $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                    (
                     description => 'declaration$',
                     method =>  [ \&_closeScopes, @callbacks ],
                     option => MarpaX::Languages::C::AST::Callback::Option->new
                     (
                      condition => [ [qw/auto/] ],
                      priority => 999
                     )
                    )
                   );
  }
# ----------------------------------------------------------------------------------------
sub _storage_helper {
    my ($method, $callback, $eventsp, $event) = @_;
    #
    # The event name, by convention, is 'symbol$' or '^$symbol'
    #
    my $symbol = $event;
    my $rc;
    if (substr($symbol, 0, 1) eq '^') {
	substr($symbol, 0, 1, '');
	$rc = [ lineAndCol($callback->hscratchpad('_impl')) ];
    } elsif (substr($symbol, -1, 1) eq '$') {
	substr($symbol, -1, 1, '');
	$rc = [ lineAndCol($callback->hscratchpad('_impl')), lastCompleted($callback->hscratchpad('_impl'), $symbol) ];
    }
    $log->debugf('%s[%s[%d]] Callback \'%s\', topic \'%s\', data %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $method->extra_description || $method->description, $event, $rc);
    return $rc;
}
# ----------------------------------------------------------------------------------------
sub _inc_helper {
    my ($method, $callback, $eventsp, $topic, $increment) = @_;

    my $old_value = $callback->topic_fired_data($topic)->[0] || 0;
    my $new_value = $old_value + $increment;
    $log->debugf('%s[%s[%d]] Callback \'%s\', topic \'%s\', counter %d -> %d', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $method->extra_description || $method->description, $topic, $old_value, $new_value);

    return $new_value;
}
# ----------------------------------------------------------------------------------------
sub _reset_helper {
    my ($method, $callback, $eventsp, @topics) = @_;

    my @rc = ();
    $log->debugf('%s[%s[%d]] Callback \'%s\', resetting topics \'%s\' data', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $method->extra_description || $method->description, \@topics);
    return @rc;
}
# ----------------------------------------------------------------------------------------
sub _push_and_reset_helper {
    my ($method, $callback, $eventsp, @topics) = @_;

    my @rc = ();
    foreach (@topics) {
	my $topic = $_;
	$log->debugf('%s[%s[%d]] Callback \'%s\', collecting and resetting topic \'%s\' data: %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $method->extra_description || $method->description, $topic, $callback->topic_fired_data($topic));
	push(@rc, @{$callback->topic_fired_data($topic)});
	$callback->topic_fired_data($topic, []);
    }

    $log->debugf('%s[%s[%d]] Callback \'%s\', collected data: %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $method->extra_description || $method->description, \@rc);
    return @rc;
}
# ----------------------------------------------------------------------------------------
sub _incScratchpad {
  my ($method, $callback, $eventsp, $flag) = @_;

  ++$callback->hscratchpad($flag);
  $log->debugf('%s[%s[%d]] Number of expected end of rule: %d', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $callback->hscratchpad($flag));
}

sub _subFire {
  my ($method, $callback, $eventsp, $lhs, $subCallback) = @_;

  $log->debugf('%s[%s[%d]] Sub-firing %s callbacks with %s', $callback->log_prefix, whoami(__PACKAGE__), $callback->currentTopicLevel, $lhs, $eventsp);
  $subCallback->exec(@{$eventsp});
}

sub _register_rule_callbacks {
  my ($self, $hashp) = @_;

  #
  # Create inner callback object
  #
  my $callback = MarpaX::Languages::C::AST::Callback->new(log_prefix => '  ' . $hashp->{lhs} . ' ');
  $callback->hscratchpad('_impl', $self->hscratchpad('_impl'));
  $callback->hscratchpad('_scope', $self->hscratchpad('_scope'));
  $callback->hscratchpad('_sourcep', $self->hscratchpad('_sourcep'));

  #
  # subEvents will be the list of events that we forward to the inner callback object
  #
  my %subEvents = ();
  #
  # Counters are events associated to a counter: every ^xxx increases a counter.
  # Every xxx$ is decreasing it.
  my $countersHashp = $hashp->{counters} || {};
  foreach (keys %{$countersHashp}) {
    my $counter = $_;
    my ($eventStart, $eventEnd) = @{$countersHashp->{$counter}};
    ++$subEvents{$eventStart};
    $callback->register(MarpaX::Languages::C::AST::Callback::Method->new
                        (
                         description => $eventStart,
                         extra_description => $counter . ' [Start] ',
                         method =>  [ \&_inc_helper, $counter, 1 ],
                         method_mode => 'replace',
                         option => MarpaX::Languages::C::AST::Callback::Option->new
                         (
                          topic => {$counter => 1},
                          topic_persistence => 'any',
                          condition => [ [ 'auto' ] ],  # == match on description
                          priority => 999,
                         )
                        )
                       );
    ++$subEvents{$eventEnd};
    $callback->register(MarpaX::Languages::C::AST::Callback::Method->new
                        (
                         description => $eventEnd,
                         extra_description => $counter . ' [End] ',
                         method =>  [ \&_inc_helper, $counter, -1 ],
                         method_mode => 'replace',
                         option => MarpaX::Languages::C::AST::Callback::Option->new
                         (
                          topic => {$counter => 1},
                          topic_persistence => 'any',
                          condition => [ [ 'auto' ] ],  # == match on description
                          priority => 999,
                         )
                        )
                       );
  }

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
                             extra_description => $_ . ' [storage] ',
			     method =>  [ \&_storage_helper, $_ ],
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
  my %lhsTopicsToUpdate = ();
  foreach (@{$hashp->{rhs}}) {
    my ($rhs, $genomep) = @{$_};

    $lhsTopicsToUpdate{$rhs  . '$'} = 1;

    my %rhsTopicsToUpdate = ();
    my %rhsTopicNotToUpdate = ();
    foreach (@{$genomep}) {
      $rhsTopicsToUpdate{$_ . '$'} = 1;
      $rhsTopicNotToUpdate{$_ . '$'} = -1;
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
                         extra_description => $event . ' [reset] ',
			 method =>  [ \&_reset_helper, keys %rhsTopicsToUpdate ],
			 method_mode => 'replace',
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [ 'auto' ] ],  # == match on description
			  topic => \%rhsTopicsToUpdate,
			  topic_persistence => 'level',
			  priority => -2,
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
                         extra_description => $event . ' [push_and_reset] ',
			 method =>  [ \&_push_and_reset_helper, keys %rhsTopicNotToUpdate ],
			 method_mode => 'push',
			 option => MarpaX::Languages::C::AST::Callback::Option->new
			 (
			  condition => [ [ 'auto' ] ],  # == match on description
			  topic => {$rhs  . '$' => 1,
                                   %rhsTopicNotToUpdate},
			  topic_persistence => 'level',
			  priority => 2,
			 )
			)
	);

    if ($i == 0) {
      #
      # The very first LHSRHS$ is special: we use it to push a scope if this is the first time it is hitted
      #
    }
  }

  #
  # Final callback
  #
  ++$subEvents{$hashp->{lhs} . '$'};
  $callback->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => $hashp->{lhs} . '$',
                   extra_description => $hashp->{lhs} . '$ [processing] ',
                   method => [ $hashp->{method} ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
		    condition => [ [ 'auto' ] ],  # == match on description
                    priority => 1,
                   )
                  )
                 );
  $callback->register(MarpaX::Languages::C::AST::Callback::Method->new
                      (
                       description => $hashp->{lhs} . '$',
                       extra_description => $hashp->{lhs} . '$ [reset] ',
                       method =>  [ \&_reset_helper, keys %lhsTopicsToUpdate ],
                       method_mode => 'replace',
                       option => MarpaX::Languages::C::AST::Callback::Option->new
                       (
                        condition => [ [ 'auto' ] ],  # == match on description
                        topic => \%lhsTopicsToUpdate,
                        topic_persistence => 'level',
                        priority => 0,
                       )
                      )
                     );

  #
  ## Sub-fire events for this sub-callback object
  #
  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => $hashp->{lhs} . ' [sub-events fire]',
                   method => [ \&_subFire, $hashp->{lhs}, $callback ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
                    condition => [
                                  [ sub { my ($method, $callback, $eventsp, $subEventsp) = @_;
                                          return grep {exists($subEventsp->{$_})} @{$eventsp};
                                        },
                                    \%subEvents
                                  ]
                                 ]
                   )
                  )
                 );

  return $callback;
}

1;
