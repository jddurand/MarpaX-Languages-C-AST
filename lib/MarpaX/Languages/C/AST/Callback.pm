use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Callback;
use MarpaX::Languages::C::AST::Util qw/whoami/;
use MarpaX::Languages::C::AST::Callback::Method;
use Storable qw/dclone/;
use Class::Struct
  #
  # External attributes
  #
  hscratchpad             => '%',   # User working area
  ascratchpad             => '@',   # User working area
  sscratchpad             => '$',   # User working area
  #
  # Internal attributes
  #
  cb                      => '@', # List of methods.
  topic_fired             => '%', # Remember what are the eligible cb's topics.
  topic_fired_data        => '%', # Remember what are the eligible cb's topics data.
  topic_fired_persistence => '%', # Remember what are the eligible cb's topics persistence.
  topic_level             => '@', # Topic levels
  ncb                     => '$', # Number of methods.
  prioritized_cb          => '@', # Prioritized list of methods, for efficiency.
  prioritized_cb_tofire   => '@', # Remember what cb are eligible.
  arguments               => '@', # List of arguments to the exec method.
  ;

# ABSTRACT: Simple but powerful callback generic framework that depend on nothing else but core modules.

use Log::Any qw/$log/;
use Carp qw/croak/;

# VERSION

=head1 DESCRIPTION

    This modules is a simple callback framework.

=cut

sub _sort($$) {
  return $_[1]->option->priority <=> $_[0]->option->priority;
}

sub register {
  my ($self, $cb) = @_;

  if (ref($cb) ne 'MarpaX::Languages::C::AST::Callback::Method') {
    croak 'argument bust be a reference to a MarpaX::Languages::C::AST::Callback::Method object';
  }

  #
  # Sanitize cb
  #
  if (defined($cb->method) && ref($cb->method) ne 'ARRAY') {
    croak 'method must be an ARRAY ref';
  }
  if (defined($cb->method)) {
    if (! @{$cb->method}) {
      croak 'method is a reference to an empty array';
    }
    if (ref(($cb->method)->[0]) ne 'CODE') {
      croak 'method must be an ARRAY ref starting with a CODE reference';
    }
  }
  #
  # Sanitize $cb->option
  #
  if (! defined($cb->option)) {
    $cb->option(MarpaX::Languages::C::AST::Callback::Option->new());
  }
  my $option = $cb->option;
  foreach (@{$option->condition}) {
    if (! defined($_) || (! (ref($_) eq 'ARRAY')) || (! (ref($_->[0]) eq 'CODE' || (! ref($_->[0]) && $_->[0] eq 'auto')))) {
	croak 'A condition is not an ARRAY reference, that must start with a CODE reference or the "auto" keyword"';
    }
  }

  if (! defined($option->conditionMode)) {
    $option->conditionMode('and');
  }
  if (! grep {$option->conditionMode eq $_} qw/and or/) {
    croak 'condition mode must be "and" or "or"';
  }

  if (! defined($option->subscriptionMode)) {
    $option->subscriptionMode('required');
  }
  if (! grep {$option->subscriptionMode eq $_} qw/required optional/) {
    croak 'condition mode must be "and" or "or"';
  }

  if (! defined($option->topic_persistence)) {
    $option->topic_persistence('none');
  }
  if (! grep {$option->topic_persistence eq $_} qw/none any level/) {
    croak 'topic persistence mode must be "none", "any" or "level"';
  }

  if (! defined($option->priority)) {
    $option->priority(0);
  }
  my $priority = $option->priority;
  if (! ("$priority" =~ /^\d+$/)) {
    croak 'priority must be a number';
  }

  $self->ncb(0) if (! defined($self->ncb));
  $self->cb($self->ncb, $cb);
  $self->ncb($self->ncb + 1);

  $log->tracef('[%s] Registering callback with description \'%s\', topic %s, topic persistence \'%s\', subscription %s', whoami(__PACKAGE__), $cb->description, $cb->option->topic, $cb->option->topic_persistence, $cb->option->subscription);
  $self->prioritized_cb([sort _sort @{$self->cb}]);
}

sub exec {
  my $self = shift;
  #
  # Remember our arguments, if the callback need it
  #
  $self->arguments([@_]);
  #
  # Do an inventory of eligible callbacks and topics
  #
  $self->_inventory_fire();
  #
  # Fire everything that is eligible
  #
  $self->_fire();
}

sub _inventory_condition_tofire {
  my $self = shift;
  my $found = 0;
  my $alreadyOk = 0;
  my $alreadyNotOk = 0;
  foreach (my $i = 0; $i < $self->ncb; $i++) {
    my $cb = $self->prioritized_cb($i);
    my $option = $cb->option;

    my @condition = ();
    foreach my $condition (@{$option->condition}) {
	my ($coderef, @arguments) = @{$condition};
	if (ref($coderef) eq 'CODE') {
	    push(@condition, &$coderef($cb, @arguments, @{$self->arguments()}) ? 1 :0);
	} elsif (defined($cb->description)) {
	    push(@condition, (grep {$_ eq $cb->description} @{$self->arguments()}) ? 1 :0);
	}
    }
    #
    ## Apply conditionMethod. If none, then the callback will never be
    ## executed. Only the subscription methods can make it eligible.
    #
    my $condition = 0;
    if (@condition) {
      $condition = shift(@condition);
      if ($condition eq 'and') {
        foreach (@condition) {
          $condition &&= $_;
        }
      } elsif ($condition eq 'or') {
        foreach (@condition) {
          $condition ||= $_;
        }
      }
    }
    if ($condition) {
      $log->tracef('[%s] Condition OK for callback with description \'%s\'', whoami(__PACKAGE__), $cb->description);
      $self->prioritized_cb_tofire($i, 1);
      #
      # Initialize the associated topics if needed
      #
      foreach my $topic (keys %{$option->topic}) {
        next if (! defined($option->topic($topic)));
        next if (! $option->topic($topic));
        if (! defined($self->topic_fired($topic))) {
          $self->topic_fired($topic, 1);
          $self->topic_fired_persistence($topic, $option->topic_persistence);
          $self->topic_fired_data($topic, []);
          $log->tracef('[%s] Created topic \'%s\' with persistence \'%s\' and empty data', whoami(__PACKAGE__), $topic, $self->topic_fired_persistence($topic));
        }
      }
      ++$found;
    } else {
      if (@condition) {
        $log->tracef('[%s] Condition KO for callback with description \'%s\'', whoami(__PACKAGE__), $cb->description);
        $self->prioritized_cb_tofire($i, -1);
      } else {
        # $log->tracef('[%s] Condition NA for callback with description \'%s\'', whoami(__PACKAGE__), $cb->description);
      }
    }
  }
}

sub _fire {
  my $self = shift;
  #
  # Make sure the raised topic data always exist
  #
  foreach (my $i = 0; $i < $self->ncb; $i++) {
    if ($self->prioritized_cb_tofire($i) <= 0) {
      # -1: Condition KO
      # -2: Condition NA and Subscription NA
      # -3: Subscription KO
      next;
    }
    #
    # Fire the callback (if there is a method)
    #
    my $cb = $self->prioritized_cb($i);
    if (defined($cb->method)) {
      my ($method, @arguments) = @{$cb->method};
      $log->tracef('[%s] Calling method for callback No %d with description \'%s\'', whoami(__PACKAGE__), $i, $cb->description);
      my $rc = $cb->$method(@arguments, @{$self->arguments()});
      #
      # Push result to data attached to every topic of this callback
      #
      my $option = $cb->option;
      foreach my $topic (keys %{$option->topic}) {
        next if (! defined($option->topic($topic)));
        next if (! $option->topic($topic));
        my $topic_fired_data = $self->topic_fired_data($topic) || [];
        push(@{$topic_fired_data}, $rc);
        $self->topic_fired_data($topic, $topic_fired_data);
      }
    }
  }
}

sub topic_data {
    my $self = shift;
    my $topic = shift;
    my $level = shift;

    $level //= 0;

    #
    # Level MUST be 0 for current or a negative number
    #
    $level = int($level);
    if ($level > 0) {
	croak 'int(level) must be 0 or a negative number';
    }
    if ($level == 0) {
	if (@_) {
	    return $self->topic_fired_data($topic, shift);
	} else {
	    return $self->topic_fired_data($topic);
	}
    } else {
	my ($old_topic_fired, $old_topic_persistence, $old_topic_data) = @{$self->topic_level($level)};
	if (@_) {
	    return ($old_topic_data->{$topic} = shift);
	} else {
	    return $old_topic_data->{$topic};
	}
    }
}

sub _inventory_initialize_topic {
  my $self = shift;
  #
  # For topics, we want to keep those that have a persistence of 'level' or 'any'
  #
  my $keep_topic_fired = {};
  my $keep_topic_fired_persistence = {};
  my $keep_topic_fired_data = {};
  foreach my $topic (keys %{$self->topic_fired}) {
    my $persistence = $self->topic_fired_persistence($topic);
    if (grep {$_ eq $persistence} qw/any level/) {
      $log->tracef('[%s] Keeping topic \'%s\' with persistence \'%s\'', whoami(__PACKAGE__), $topic, $persistence);
      $keep_topic_fired->{$topic} = $self->topic_fired($topic);
      $keep_topic_fired_persistence->{$topic} = $self->topic_fired_persistence($topic);
      $keep_topic_fired_data->{$topic} = $self->topic_fired_data($topic);
    }
  }
  $self->topic_fired($keep_topic_fired);
  $self->topic_fired_persistence($keep_topic_fired_persistence);
  $self->topic_fired_data($keep_topic_fired_data);
}

sub _inventory_initialize_tofire {
  my $self = shift;
  foreach (my $i = 0; $i < $self->ncb; $i++) {
      $self->prioritized_cb_tofire($i, 0);
  }
}

sub _inventory_fire {
  my $self = shift;

  #
  # Inventory
  #
  $self->_inventory_initialize_topic();
  $self->_inventory();
  #
  # Resume
  #
  $log->tracef('[%s] Topic level %d: Fired topics: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->topic_fired);
  $log->tracef('[%s] Topic level %d: Fired topics persistence: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->topic_fired_persistence);
  $log->tracef('[%s] Topic level %d: Eligible callbacks: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->prioritized_cb_tofire);
}

sub _inventory {
  my $self = shift;
  $self->_inventory_initialize_tofire();
  $self->_inventory_condition_tofire();
  $self->_inventory_subscription_tofire();
}

sub _inventory_subscription_tofire {
  my $self = shift;
  #
  # This is a loop because when a new callback is eligible there might be new topics
  #
  foreach (my $i = 0; $i < $self->ncb; $i++) {
    my $cb = $self->prioritized_cb($i);
    my $option = $cb->option;
    #
    # Here the values can be:
    # -1: condition KO
    #  0: no condition applied
    #  1: condition OK
    next if ($self->prioritized_cb_tofire($i) < 0);

    my %subscribed = ();
    my $nbSubscription = 0;
    foreach my $subscription (keys %{$option->subscription}) {
      next if (! defined($option->subscription($subscription)));
      next if (! $option->subscription($subscription));
      ++$nbSubscription;
      if (ref($subscription) eq 'Regexp') {
        foreach (keys %{$self->topic_fired}) {
          if ($_ =~ $subscription) {
            $subscribed{$_} = 1;
          }
        }
      } else {
        foreach (keys %{$self->topic_fired}) {
          if ("$_" eq "$subscription") {
            $subscribed{$_} = 1;
          }
        }
      }
    }

    if ($self->prioritized_cb_tofire($i) == 0 && ! keys %subscribed) {
      #
      # no condition was setted and no subscription is raised
      #
      # $log->tracef('[%s] Subscription NA for callback with description \'%s\'', whoami(__PACKAGE__), $cb->description);
      $self->prioritized_cb_tofire($i, -2);
      next;
    }

    if ($nbSubscription > 0 && $option->subscriptionMode eq 'required' && $nbSubscription != keys %subscribed) {
      #
      # There are active subscription not raised, and subscriptionMode is 'required'
      #
      # $log->tracef('[%s] Subscription KO for callback with description \'%s\'', whoami(__PACKAGE__), $cb->description);
      $self->prioritized_cb_tofire($i, -3);
      next;
    }

    if ($self->prioritized_cb_tofire($i) == 0) {
      #
      # There must have been topic subscription being raised
      #
      $log->tracef('[%s] Subscription OK for callback with description \'%s\': %s', whoami(__PACKAGE__), $cb->description, \%subscribed);
      $self->prioritized_cb_tofire($i, 1);
    }

    my $nbTopicCreated = 0;
    foreach my $topic (keys %{$option->topic}) {
      next if (! defined($option->topic($topic)));
      next if (! $option->topic($topic));
      if (! defined($self->topic_fired($topic))) {
        $self->topic_fired($topic, 1);
        $self->topic_fired_persistence($topic, $option->topic_persistence);
        $self->topic_fired_data($topic, []);
        $log->tracef('[%s] Created topic \'%s\' with persistence \'%s\' and empty data', whoami(__PACKAGE__), $topic, $self->topic_fired_persistence($topic));
        ++$nbTopicCreated;
      }
    }
    if ($nbTopicCreated) {
      $log->tracef('[%s] %d topic(s) has been created - redo initialisation', whoami(__PACKAGE__), $nbTopicCreated);
      #
      # Take care: this is recursive
      #
      $self->_inventory();
    }
  }
}

sub currentTopicLevel {
  my $self = shift;

  return scalar(@{$self->topic_level});
}

sub pushTopicLevel {
  my $self = shift;

  #
  # We push current topics and their persistence into the topic_level
  #
  $log->tracef('[%s] Topic level %d: Pushing fired topics: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->topic_fired);
  $log->tracef('[%s] Topic level %d: Pushing fired topics persistence: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->topic_fired_persistence);
  $log->tracef('[%s] Topic level %d: Pushing fired topics data', whoami(__PACKAGE__), $self->currentTopicLevel);
  push(@{$self->topic_level}, [ dclone($self->topic_fired), dclone($self->topic_fired_persistence), $self->topic_fired_data ]);
  #
  # We remove from current topics those that do not have the 'any' persistence
  #
  my $new_topic_fired = {};
  my $new_topic_fired_persistence = {};
  my $new_topic_fired_data = {};
  foreach my $topic (keys %{$self->topic_fired}) {
    my $persistence = $self->topic_fired_persistence($topic);
    if (grep {$_ eq $persistence} qw/any/) {
      $log->tracef('[%s] Keeping topic \'%s\' with persistence \'%s\'', whoami(__PACKAGE__), $topic, $persistence);
      $new_topic_fired->{$topic} = $self->topic_fired($topic);
      $new_topic_fired_persistence->{$topic} = $self->topic_fired_persistence($topic);
      $new_topic_fired_data->{$topic} = $self->topic_fired_data($topic);
    }
  }
  $self->topic_fired($new_topic_fired);
  $self->topic_fired_persistence($new_topic_fired_persistence);
  $self->topic_fired_data($new_topic_fired_data);

  $log->tracef('[%s] Topic level %d: Kept topics: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->topic_fired);
  $log->tracef('[%s] Topic level %d: Kept topics persistence: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->topic_fired_persistence);
  $log->tracef('[%s] Topic level %d: Kept topics data', whoami(__PACKAGE__), $self->currentTopicLevel);
}

sub popTopicLevel {
  my $self = shift;

  #
  # We pop current topics and their persistence from the topic_level
  #
  my ($old_topic_fired, $old_topic_persistence, $old_topic_data) = @{$self->topic_level(-1)};
  pop(@{$self->topic_level});
  $self->topic_fired($old_topic_fired);
  $self->topic_fired_persistence($old_topic_persistence);
  $self->topic_fired_data($old_topic_data);

  $log->tracef('[%s] Topic level %d: Restored fired topics: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->topic_fired);
  $log->tracef('[%s] Topic level %d: Restored fired topics persistence: %s', whoami(__PACKAGE__), $self->currentTopicLevel, $self->topic_fired_persistence);
  $log->tracef('[%s] Topic level %d: Restored fired topics data', whoami(__PACKAGE__), $self->currentTopicLevel);
}

sub reset_topic_fired_data {
    my ($self, $topic, $level, $value) = @_;

    $level //= 0;

    #
    # Level MUST be 0 or a negative number
    # It is okay if $value is undef
    #
    $level = int($level);
    if ($level > 0) {
	croak 'int(level) must be 0 or a negative number';
    }
    if ($level == 0) {
	$log->tracef('[%s] Topic \'%s\' data reset at level %d', whoami(__PACKAGE__), $topic, $self->currentTopicLevel);
	$self->topic_fired_data($topic, $value);
    } else {
	my ($old_topic_fired, $old_topic_persistence, $old_topic_data) = @{$self->topic_level($level)};
	$log->tracef('[%s] Topic \'%s\' data reset at previous level %d', whoami(__PACKAGE__), $topic, $self->currentTopicLevel + $level);
	$old_topic_data->{$topic} = $value;
    }

}

1;
