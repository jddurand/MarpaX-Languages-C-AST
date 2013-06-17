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

  # ######
  # Scopes
  # ######
  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => 'enterScope[]',
                   method =>  [ sub { $outerSelf->{_scope}->parseEnterScope(); } ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
                    condition => [qw/auto/],
                   )
                  )
                 );
  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => 'exitScope[]',
                   method =>  [ sub { $outerSelf->{_scope}->parseExitScope(); } ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
                    condition => [qw/auto/],
                   )
                  )
                 );
  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => 'reenterScope[]',
                   method =>  [ sub { $outerSelf->{_scope}->parseReenterScope(); } ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
                    condition => [qw/auto/],
                   )
                  )
                 );

  # --------------------------------------------------------------------
  # We want to have topic levels following the real enter/exit of scopes
  # --------------------------------------------------------------------
  $outerSelf->{_scope}->enterCallback(\&_enterCallback, $self);
  $outerSelf->{_scope}->exitCallback(\&_exitCallback, $self);

  # ################################################################################################
  # Create topics based on "genome" rules with a priority of 1, so that they always triggered first.
  # The topic data will always be in an array reference of [ [$line, $column], $last_completion ]
  # ################################################################################################
  foreach (qw/primaryExpressionIdentifier$
              enumerationConstantIdentifier$
              storageClassSpecifierTypedef$
              directDeclaratorIdentifier$/) {
    $self->_register_helper($outerSelf, $_);
  }

  # ###################################################
  # Create callbacks methods that will use these topics
  # ###################################################

  # ###############################################################################################
  # A directDeclarator introduces a typedefName only when it eventually participates in the grammar
  # rule:
  # declarationDeclarationSpecifiers ::= declarationSpecifiers initDeclaratorList SEMICOLON
  # ###############################################################################################
  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => 'initDeclaratorList$',
                   method =>  [ \&_introduceTypedefName, $self, $outerSelf ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
                    condition => [qw/auto/],
                    subscription => { 'directDeclaratorIdentifier$' => 1 },
                   )
                  )
                 );

  # ###############################################################################################
  # Nevertheless initDeclaratorList leads to
  #
  # initDeclarator ::= declarator EQUAL initializer | declarator
  #
  # that particpates in FOUR other rules:
  # ###############################################################################################
  #
  # functionDefinition ::= declarationSpecifiers <functionDefinitionMark> declarator declarationList compoundStatement
  #                      | declarationSpecifiers <functionDefinitionMark> declarator compoundStatement
  #
  # typedef is syntactically allowed but never valid in either declarationSpecifiers or
  # declarationList. We use:
  #
  # A general callback for functionDefinition that listens on a NEW storageClassSpecifierTypedef$ topic
  # that would have NO persistence.
  #
  # ^functionDefinition to enable the listening of the topic
  # functionDefinitionMark[] to pause
  # ^declarationList to enable again
  # declarationList$ to pause again
  # functionDefinition$ to process
  #
  # We rely on storageClassSpecifierTypedef$ has a priority of 1, so we put a priority of 2.
  # ###############################################################################################
  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => 'functionDefinition|storageClassSpecifierTypedef$',
                   method =>  [ \&_storage_helper, $self, $outerSelf, 'storageClassSpecifierTypedef$' ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
                    condition => [ sub { my $cb = shift; return grep {$_ eq 'storageClassSpecifierTypedef$'} @_; } ],
                    topic => {'functionDefinition|storageClassSpecifierTypedef' => 1},
                   )
                  )
                 );
  my $functionDefinitionTypedefSurvey_cb;
  $self->register($functionDefinitionTypedefSurvey_cb = MarpaX::Languages::C::AST::Callback::Method->new
		  (
		   description => 'functionDefinition typedef survey',
		   method =>  [ \&_storage_functionDefinition_helper, $self, $outerSelf, 'storageClassSpecifierTypedef$' ],
		   option => MarpaX::Languages::C::AST::Callback::Option->new
		   (
		    subscription => { 'functionDefinition|storageClassSpecifierTypedef' => 0 },  # Default subscription is off
		    topic => {'functionDefinitionTypedefSurvey' => 1},
		    topic_persistence => 'level',
		   )
		  )
      );
  {
      my $i = 1;
      foreach (qw/^functionDefinition functionDefinitionMark[] ^declarationList declarationList$/) {
	  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
			  (
			   description => $_,
			   method => [ sub { my ($cb, @execArgs) = @_;
					     my $topicValue = $i % 2;
					     $log->debugf('[%s] Setting %s subscription on topic \'%s\' to %d', $cb->description, 'functionDefinition typedef survey', 'storageClassSpecifierTypedef$', $topicValue);
					     $functionDefinitionTypedefSurvey_cb->option->subscription('functionDefinition|storageClassSpecifierTypedef', $topicValue);
					     #
					     # Redo an inventory of callbacks to fire
					     #
					     $self->inventory();
				       } ],
			   option => MarpaX::Languages::C::AST::Callback::Option->new
			   (
			    condition => [qw/auto/],
			    priority => 2
			   )
			  )
	      );
      }
  }
  $self->register(MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => 'functionDefinition$',
                   method =>  [ \&_functionDefinition, $self, $outerSelf ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
                    condition => [qw/auto/],
                   )
                  )
                 );

    
  return $self;
}

sub _register_helper {
  my ($self, $outerSelf, $event) = @_;
  my $cb;
  $self->register($cb = MarpaX::Languages::C::AST::Callback::Method->new
                  (
                   description => $event,
                   method =>  [ \&_storage_helper, $self, $outerSelf, $event ],
                   option => MarpaX::Languages::C::AST::Callback::Option->new
                   (
                    condition => [qw/auto/],
                    topic => {$event => 1},
                    topic_persistence => 'level',
                    priority => 1
                   )
                  )
                 );
  return $cb;
}

sub _storage_helper {
  my ($cb, $self, $outerSelf, $event) = @_;
  #
  # The event name, by convention, is "symbol$"
  #
  my $symbol = $event;
  substr($symbol, -1, 1, '');
  return [ $outerSelf->_line_column(), $outerSelf->_last_completed($symbol) ]
}

sub _storage_functionDefinition_helper {
  my ($cb, $self, $outerSelf, $event) = @_;

  my $topicValue = 0;
  $log->debugf('[%s] Setting %s subscription on topic \'%s\' to %d', whoami(__PACKAGE__), 'functionDefinition typedef survey', 'storageClassSpecifierTypedef$', $topicValue);
  $cb->option->subscription('storageClassSpecifierTypedef$', $topicValue);

  return [ $outerSelf->_line_column(), $outerSelf->_last_completed('storageClassSpecifierTypedef') ];
}

sub _enterCallback {
  my ($self, $outerSelf, $storageClassSpecifierTypedef_cb, $initDeclaratorList_cb) = @_;

  $self->pushTopicLevel();

}

sub _exitCallback {
  my ($self, $outerSelf, $storageClassSpecifierTypedef_cb, $initDeclaratorList_cb) = @_;

  $self->popTopicLevel();

}

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

sub _functionDefinition {
  my ($cb, $self, $outerSelf, @execArgs) = @_;
  #
  # Get the topics data we are interested in
  #
  my $functionDefinitionTypedefSurvey = $self->topic_fired_data('functionDefinition|storageClassSpecifierTypedef');
  use Data::Dumper;
  print STDERR Dumper($functionDefinitionTypedefSurvey);
  #
  # Reset data
  #
  $self->reset_topic_fired_data('functionDefinition|storageClassSpecifierTypedef');
}

1;
