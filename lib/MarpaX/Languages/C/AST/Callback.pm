use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Callback;
use MarpaX::Languages::C::AST::Util qw/whoami/;
use MarpaX::Languages::C::AST::Callback::Method;

use Class::Struct
  #
  # External attributes
  #
  log_prefix              => '$',   # Prepended to every log
  hscratchpad             => '%',   # User working area
  ascratchpad             => '@',   # User working area
  sscratchpad             => '$',   # User working area
  #
  # Internal attributes
  #
  cb                      => '@', # List of methods.
  cb_unregistered         => '@', # List of unregistered methods, post-processed if done during fire()
  topic_fired             => '%', # Remember what are the eligible cb's topics.
  topic_fired_data        => '%', # Remember what are the eligible cb's topics data.
  topic_fired_persistence => '%', # Remember what are the eligible cb's topics persistence.
  topic_level             => '@', # Topic levels
  ncb                     => '$', # Number of methods.
  prioritized_cb          => '@', # Prioritized list of methods, for efficiency.
  prioritized_cb_tofire   => '@', # Remember what cb are eligible.
  prioritized_cb_fired    => '@', # Remember what cb were fired
  arguments               => '@', # List of arguments to the exec method.
  firing                  => '$'
  ;

# ABSTRACT: Simple but powerful callback generic framework that depend on nothing else but core modules.

use Carp qw/croak/;

# VERSION

=head1 DESCRIPTION

    This modules is a simple callback framework.

=cut

sub _sort_by_option_priority_desc {
  return $b->option->priority <=> $a->option->priority;
}

sub _sort_by_numeric_desc {
  return $b <=> $a;
}

sub register {
  my ($self, $cb) = @_;

  if (ref($cb) ne 'MarpaX::Languages::C::AST::Callback::Method') {
    croak 'argument bust be a reference to a MarpaX::Languages::C::AST::Callback::Method object';
  }
  #
  # Sanitize self
  #
  if (! defined($self->log_prefix)) {
      $self->log_prefix('');
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
    if (ref(($cb->method)->[0]) ne 'CODE' && (! ref($cb->method) && $cb->method eq 'auto')) {
      croak 'method must be an ARRAY ref starting with a CODE reference, or the string \'auto\'';
    }
  }
  if (! defined($cb->method_mode)) {
    $cb->method_mode('push');
  }
  if ($cb->method_mode ne 'push' && $cb->method_mode ne 'replace') {
    croak 'method_mode must be \'push\' or \'replace\'';
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
  if (! ("$priority" =~ /^[+-]?\d+$/)) {
    croak 'priority must be a number';
  }

  $self->ncb(0) if (! defined($self->ncb));
  $self->cb($self->ncb, $cb);
  $self->ncb($self->ncb + 1);
  $self->prioritized_cb([sort _sort_by_option_priority_desc  @{$self->cb}]);

  #
  # Invalid cache if any
  #
  $self->hscratchpad('_cache', 0);

  #
  # We return the indice within Callback
  #
  return $self->ncb - 1;
}

sub _unregister {
    my $self = shift;

    foreach (sort _sort_by_numeric_desc @_) {

	my $cb = $self->cb($_);
	croak "Unknown callback indice $_" if (! defined($cb));

	splice(@{$self->cb}, $_, 1);
	$self->ncb($self->ncb - 1);
	$self->prioritized_cb([sort _sort_by_option_priority_desc @{$self->cb}]);

    }

}

sub unregister {
  my $self = shift;

  my $firing = $self->firing() || 0;
  if (! $firing) {
      return $self->_unregister(@_);
  } else {
      push(@{$self->cb_unregistered}, @_);
  }
}

sub exec {
  my $self = shift;
  #
  # Remember our arguments, if the callback need it
  #
  my $argumentsp = \@_;
  $self->arguments($argumentsp);
  #
  # Localize cache mode for faster lookup
  #
  my $cache = $self->hscratchpad('_cache') || 0;
  local $__PACKAGE__::_cacheNcb                      = $cache ? $self->hscratchpad('_cacheNcb')                     : undef;
  local $__PACKAGE__::_cacheArgumentsp               = $cache ? $argumentsp                                         : undef;
  local $__PACKAGE__::_cachePrioritized_cbp          = $cache ? $self->hscratchpad('_cachePrioritized_cb')          : undef;
  local $__PACKAGE__::_cachePrioritized_cb_tofirep   = $cache ? $self->hscratchpad('_cachePrioritized_cb_tofire')   : undef;
  local $__PACKAGE__::_cachePrioritized_cb_firedp    = $cache ? $self->hscratchpad('_cachePrioritized_cb_fired')    : undef;
  local $__PACKAGE__::_cacheOptionp                  = $cache ? $self->hscratchpad('_cacheOption')                  : undef;
  local $__PACKAGE__::_cacheOptionConditionModep     = $cache ? $self->hscratchpad('_cacheOptionConditionMode')     : undef;
  local $__PACKAGE__::_cacheOptionConditionp         = $cache ? $self->hscratchpad('_cacheOptionCondition')         : undef;
  local $__PACKAGE__::_cacheOptionSubscriptionp      = $cache ? $self->hscratchpad('_cacheOptionSubscription')      : undef;
  local $__PACKAGE__::_cacheOptionSubscriptionModep  = $cache ? $self->hscratchpad('_cacheOptionSubscriptionMode')  : undef;
  local $__PACKAGE__::_cacheOptionTopicp             = $cache ? $self->hscratchpad('_cacheOptionTopic')             : undef;
  local $__PACKAGE__::_cacheOptionTopic_persistencep = $cache ? $self->hscratchpad('_cacheOptionTopic_persistence') : undef;
  local $__PACKAGE__::_cacheCbDescriptionp           = $cache ? $self->hscratchpad('_cacheCbDescription')           : undef;
  local $__PACKAGE__::_cacheCbMethodp                = $cache ? $self->hscratchpad('_cacheCbMethod')                : undef;
  local $__PACKAGE__::_cacheCbMethod_voidp           = $cache ? $self->hscratchpad('_cacheCbMethod_void')           : undef;

  #
  # Do an inventory of eligible callbacks and topics
  #
  $self->_inventory_fire();
  #
  # Fire everything that is eligible
  #
  $self->_fire();
  #
  # And post-process eventual unregistrations
  #
  $self->_unregister(@{$self->cb_unregistered});
  $self->cb_unregistered([]);
}

sub _inventory_condition_tofire {
  my $self = shift;
  my $nbNewTopics = 0;
  my $ncb = $__PACKAGE__::_cacheNcb // $self->ncb;
  my $prioritized_cbp = $__PACKAGE__::_cachePrioritized_cbp // $self->prioritized_cb;
  my $prioritized_cb_tofirep = $__PACKAGE__::_cachePrioritized_cb_tofirep // $self->prioritized_cb_tofire;
  my $argumentsp = $__PACKAGE__::_cacheArgumentsp // $self->arguments;
  my $topic_firedp = $self->topic_fired;
  my $topic_fired_datap = $self->topic_fired_data;
  my $topic_fired_persistencep = $self->topic_fired_persistence;

  foreach (my $i = 0; $i < $ncb; $i++) {
    my $cb = $prioritized_cbp->[$i];
    my $option = defined($__PACKAGE__::_cacheOptionp) ? $__PACKAGE__::_cacheOptionp->[$i] : $cb->option;
    my $conditionMode = ((defined($__PACKAGE__::_cacheOptionConditionModep) ? $__PACKAGE__::_cacheOptionConditionModep->[$i] : $option->conditionMode) eq 'and') ? 1 : 0;

    my @condition = ();
    my $description = defined($__PACKAGE__::_cacheCbDescriptionp) ? $__PACKAGE__::_cacheCbDescriptionp->[$i] : $cb->description;
    foreach my $condition (defined($__PACKAGE__::_cacheOptionConditionp) ? @{$__PACKAGE__::_cacheOptionConditionp->[$i]} : @{$option->condition}) {
	my ($coderef, @arguments) = @{$condition};
	if (ref($coderef) eq 'CODE') {
	    push(@condition, &$coderef($cb, $self, $argumentsp, @arguments) ? 1 :0);
	} elsif (defined($description)) {
	    #
	    # Per def condition is the string 'auto'
	    #
	    push(@condition, (grep {$_ eq $description} @{$argumentsp}) ? 1 :0);
	}
    }
    #
    ## Apply conditionMethod. If none, then the callback will never be
    ## executed. Only the subscription methods can make it eligible.
    #
    my $condition = 0;
    if (@condition) {
      $condition = shift(@condition);
      if ($conditionMode) {
	  #
	  # Per def, this is 'and'
	  #
        foreach (@condition) {
          $condition &&= $_;
        }
      } else {
	  #
	  # Per def, this is 'or'
	  #
        foreach (@condition) {
          $condition ||= $_;
        }
      }
    }
    if ($condition) {
      $prioritized_cb_tofirep->[$i] = 1;
      #
      # Initialize the associated topics if needed
      #
      foreach my $topic (keys %{defined($__PACKAGE__::_cacheOptionTopicp) ? $__PACKAGE__::_cacheOptionTopicp->[$i] : $option->topic}) {
        next if (! defined(defined($__PACKAGE__::_cacheOptionTopicp) ? $__PACKAGE__::_cacheOptionTopicp->[$i]->{$topic} : $option->topic($topic)));
        next if (! (defined($__PACKAGE__::_cacheOptionTopicp) ? $__PACKAGE__::_cacheOptionTopicp->[$i]->{$topic} : $option->topic($topic)));
        if (! defined($topic_firedp->{$topic})) {
          $topic_firedp->{$topic} = 1;
          $topic_fired_persistencep->{$topic} = defined($__PACKAGE__::_cacheOptionTopic_persistencep) ? $__PACKAGE__::_cacheOptionTopic_persistencep->[$i] : $option->topic_persistence;
          if (! defined($topic_fired_datap->{$topic})) {
            $topic_fired_datap->{$topic} = [];
	    ++$nbNewTopics;
          }
        }
      }
    } else {
      if (@condition) {
        $prioritized_cb_tofirep->[$i] = -1;
      }
    }
  }

  return $nbNewTopics;
}

#
# Class::Struct is great but introduces overhead
# The most critical accesses, identified using
# Devel::NYTProf are cached here.
#
sub cache {
  my $self = shift;

  my @cacheOption = ();
  my @cacheOptionConditionMode = ();
  my @cacheOptionCondition = ();
  my @cacheOptionSubscription = ();
  my @cacheOptionSubscriptionMode = ();
  my @cacheOptionTopic = ();
  my @cacheOptionTopic_persistence = ();
  my @cacheCbDescription = ();
  my @cacheCbMethod = ();
  my @cacheCbMethod_void = ();
  my $prioritized_cbp = $self->prioritized_cb;
  my $prioritized_cb_tofirep = $self->prioritized_cb_tofire;
  my $prioritized_cb_firedp = $self->prioritized_cb_fired;
  my $ncb = $self->ncb;
  foreach (my $i = 0; $i < $ncb; $i++) {
    my $cb = $prioritized_cbp->[$i];
    my $option = $cb->option;
    push(@cacheOption, $option);
    push(@cacheOptionConditionMode, $option->conditionMode);
    push(@cacheOptionCondition, $option->condition);
    push(@cacheOptionSubscription, $option->subscription);
    push(@cacheOptionSubscriptionMode, $option->subscriptionMode);
    push(@cacheOptionTopic, $option->topic);
    push(@cacheOptionTopic_persistence, $option->topic_persistence);
    push(@cacheCbDescription, $cb->description);
    push(@cacheCbMethod, $cb->method);
    push(@cacheCbMethod_void, $cb->method_void);
  }
  $self->hscratchpad('_cacheNcb', $ncb);
  $self->hscratchpad('_cachePrioritized_cb', $prioritized_cbp);
  $self->hscratchpad('_cachePrioritized_cb_tofire', $prioritized_cb_tofirep);
  $self->hscratchpad('_cachePrioritized_cb_fired', $prioritized_cb_firedp);
  $self->hscratchpad('_cacheOption', \@cacheOption);
  $self->hscratchpad('_cacheOptionConditionMode', \@cacheOptionConditionMode);
  $self->hscratchpad('_cacheOptionCondition', \@cacheOptionCondition);
  $self->hscratchpad('_cacheOptionSubscription', \@cacheOptionSubscription);
  $self->hscratchpad('_cacheOptionSubscriptionMode', \@cacheOptionSubscriptionMode);
  $self->hscratchpad('_cacheOptionTopic', \@cacheOptionTopic);
  $self->hscratchpad('_cacheOptionTopic_persistence', \@cacheOptionTopic_persistence);
  $self->hscratchpad('_cacheCbDescription', \@cacheCbDescription);
  $self->hscratchpad('_cacheCbMethod', \@cacheCbMethod);
  $self->hscratchpad('_cacheCbMethod_void', \@cacheCbMethod_void);

  $self->hscratchpad('_cache', 1);
}

sub _fire {
  my $self = shift;

  $self->firing(1);

  #
  # Make sure the raised topic data always exist.
  # It is very important that this routine is safe v.s. any on-the-fly registration
  # or unregistration. Thus all dependencies are expressed in the beginning.
  # This mean that nay on-the-flu registration/unregistration will happend at NEXT round.
  #
  my $ncb = $__PACKAGE__::_cacheNcb // $self->ncb;
  my $prioritized_cb_tofirep = $__PACKAGE__::_cachePrioritized_cb_tofirep // $self->prioritized_cb_tofire;
  my $prioritized_cb_firedp = $__PACKAGE__::_cachePrioritized_cb_firedp // $self->prioritized_cb_fired;
  my $prioritized_cbp = $__PACKAGE__::_cachePrioritized_cbp // $self->prioritized_cb;
  my $argumentsp = $__PACKAGE__::_cacheArgumentsp // $self->arguments;
  my $topic_fired_datap = $self->topic_fired_data;

  foreach (my $i = 0; $i < $ncb; $i++) {
    if ($prioritized_cb_tofirep->[$i] <= 0) {
      # -1: Condition KO
      # -2: Condition NA and Subscription NA
      # -3: Subscription KO
      next;
    }
    my $cb = $prioritized_cbp->[$i];
    if ($prioritized_cb_firedp->[$i]) {
      # already fired
      next;
    }
    #
    # Fire the callback (if there is a method)
    #
    $prioritized_cb_firedp->[$i] = 1;
    my $method = defined($__PACKAGE__::_cacheCbMethodp) ? $__PACKAGE__::_cacheCbMethodp->[$i] : $cb->method;
    if (defined($method)) {
      my @rc;
      if (ref($method) eq 'ARRAY') {
        my ($method, @arguments) = @{$method};
	if (ref($method) eq 'CODE') {
	    @rc = &$method($cb, $self, $argumentsp, @arguments);
	} else {
	    #
	    # Per def method is the string 'auto'
	    #
	    @rc = $topic_fired_datap->{$cb->description} || [];
	}
      }
      #
      # Push result to data attached to every topic of this callback
      #
      my $option = $cb->option;
      my $method_void = defined($__PACKAGE__::_cacheCbMethod_voidp) ? $__PACKAGE__::_cacheCbMethod_voidp->[$i] : $cb->method_void;
      if (! $method_void) {
        foreach my $topic (keys %{defined($__PACKAGE__::_cacheOptionTopicp) ? $__PACKAGE__::_cacheOptionTopicp->[$i] : $option->topic}) {
          next if (! defined(defined($__PACKAGE__::_cacheOptionTopicp) ? $__PACKAGE__::_cacheOptionTopicp->[$i]->{$topic} : $option->topic($topic)));
          next if ((defined($__PACKAGE__::_cacheOptionTopicp) ? $__PACKAGE__::_cacheOptionTopicp->[$i]->{$topic} : $option->topic($topic)) != 1);
          my $topic_fired_data = $topic_fired_datap->{$topic} || [];
          if (ref($cb->method) eq 'ARRAY') {
            if ($cb->method_mode eq 'push') {
              push(@{$topic_fired_data}, @rc);
            } else {
              @{$topic_fired_data} = @rc;
            }
          } else {
            if ($cb->method_mode eq 'push') {
              push(@{$topic_fired_data}, @rc);
            } else {
              @{$topic_fired_data} = @rc;
            }
          }
          $topic_fired_datap->{$topic} = $topic_fired_data;
        }
      }
    }
  }

  $self->firing(0);
}

sub topic_level_fired_data {
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
	    $self->topic_fired_data($topic, shift);
	}
        return $self->topic_fired_data($topic);
    } else {
	my ($old_topic_firedp, $old_topic_persistencep, $old_topic_datap) = @{$self->topic_level($level)};
	if (@_) {
	    $old_topic_datap->{$topic} = shift;
	}
        return $old_topic_datap->{$topic};
    }
}

sub _inventory_initialize_topic {
  my $self = shift;
  #
  # For topics, we want to keep those that have a persistence of 'level' or 'any'
  #
  my $topic_firedp = $self->topic_fired;
  my $topic_fired_datap = $self->topic_fired_data;
  my $topic_fired_persistencep = $self->topic_fired_persistence;

  my $keep_topic_firedp = {};
  my $keep_topic_fired_persistencep = {};
  my $keep_topic_fired_datap = {};

  foreach my $topic (keys %{$topic_firedp}) {
    my $persistence = $topic_fired_persistencep->{$topic};
    if (grep {$_ eq $persistence} qw/any level/) {
      $keep_topic_firedp->{$topic} = $topic_firedp->{$topic};
      $keep_topic_fired_persistencep->{$topic} = $topic_fired_persistencep->{$topic};
      $keep_topic_fired_datap->{$topic} = $topic_fired_datap->{$topic};
    }
  }
  $self->topic_fired($keep_topic_firedp);
  $self->topic_fired_persistence($keep_topic_fired_persistencep);
  $self->topic_fired_data($keep_topic_fired_datap);
}

sub _inventory_initialize_tofire {
  my ($self) = @_;
  my $ncb = $__PACKAGE__::_cacheNcb // $self->ncb;
  my $prioritized_cb_tofirep = [ (0) x $ncb ];
  $self->prioritized_cb_tofire($prioritized_cb_tofirep);
  if (defined($__PACKAGE__::_cachePrioritized_cb_tofirep)) {
    $__PACKAGE__::_cachePrioritized_cb_tofirep = $prioritized_cb_tofirep;
  }
}

sub _inventory_initialize_fired {
  my ($self) = @_;
  my $ncb = $__PACKAGE__::_cacheNcb // $self->ncb;
  my $prioritized_cb_firedp = [ (0) x $ncb ];
  $self->prioritized_cb_fired($prioritized_cb_firedp);
  if (defined($__PACKAGE__::_cachePrioritized_cb_firedp)) {
    $__PACKAGE__::_cachePrioritized_cb_firedp = $prioritized_cb_firedp;
  }
}

sub _inventory_fire {
  my ($self) = @_;

  #
  # Inventory
  #
  $self->_inventory_initialize_topic();
  $self->_inventory();
}

sub _inventory {
  my ($self) = @_;
  my $nbTopicsCreated = 0;
  do {
      $self->_inventory_initialize_tofire();
      $self->_inventory_initialize_fired();
      $nbTopicsCreated += $self->_inventory_condition_tofire();
      $nbTopicsCreated += $self->_inventory_subscription_tofire();
      if ($nbTopicsCreated > 0) {
	  $self->_fire();
	  $nbTopicsCreated = 0;
      }
  } while ($nbTopicsCreated > 0);
}

sub _inventory_subscription_tofire {
  my $self = shift;
  #
  # This is a loop because when a new callback is eligible there might be new topics
  #
  my $nbNewTopics = 0;
  my $nbSubscriptionOK = 0;
  my $ncb = $__PACKAGE__::_cacheNcb // $self->ncb;
  my $prioritized_cbp = $self->prioritized_cb;
  my $prioritized_cb_tofirep = $self->prioritized_cb_tofire;
  my $topic_firedp = $self->topic_fired;
  my $topic_fired_datap = $self->topic_fired_data;
  my $topic_fired_persistencep = $self->topic_fired_persistence;
  my @keys_topic_fired = keys %{$topic_firedp};

  foreach (my $i = 0; $i < $ncb; $i++) {
    my $cb = $prioritized_cbp->[$i];
    my $option = defined($__PACKAGE__::_cacheOptionp) ? $__PACKAGE__::_cacheOptionp->[$i] : $cb->option;
    #
    # Here the values can be:
    # -1: condition KO
    #  0: no condition applied
    #  1: condition OK
    next if ($prioritized_cb_tofirep->[$i] < 0);

    my %subscribed = ();
    my $nbSubscription = 0;
    foreach my $subscription (keys %{defined($__PACKAGE__::_cacheOptionSubscriptionp) ? $__PACKAGE__::_cacheOptionSubscriptionp->[$i] : $option->subscription}) {
      next if (! defined(defined($__PACKAGE__::_cacheOptionSubscriptionp) ? $__PACKAGE__::_cacheOptionSubscriptionp->[$i]->{$subscription} : $option->subscription($subscription)));
      next if (! (defined($__PACKAGE__::_cacheOptionSubscriptionp) ? $__PACKAGE__::_cacheOptionSubscriptionp->[$i]->{$subscription} : $option->subscription($subscription)));
      ++$nbSubscription;
      if (ref($subscription) eq 'Regexp') {
        foreach (@keys_topic_fired) {
          if ($_ =~ $subscription) {
            $subscribed{$_} = 1;
          }
        }
      } else {
        foreach (@keys_topic_fired) {
          if ("$_" eq "$subscription") {
            $subscribed{$_} = 1;
          }
        }
      }
    }

    if ($prioritized_cb_tofirep->[$i] == 0 && ! %subscribed) {
      #
      # no condition was setted and no subscription is raised
      #
      $prioritized_cb_tofirep->[$i] = -2;
      next;
    }

    if ($nbSubscription > 0 && (defined($__PACKAGE__::_cacheOptionSubscriptionModep) ? $__PACKAGE__::_cacheOptionSubscriptionModep->[$i] : $option->subscriptionMode) eq 'required' && $nbSubscription != keys %subscribed) {
      #
      # There are active subscription not raised, and subscriptionMode is 'required'
      #
      $prioritized_cb_tofirep->[$i] = -3;
      next;
    }

    if ($prioritized_cb_tofirep->[$i] == 0) {
      #
      # There must have been topic subscription being raised
      #
      $prioritized_cb_tofirep->[$i] = 1;
      ++$nbSubscriptionOK;
    }

    foreach my $topic (keys %{defined($__PACKAGE__::_cacheOptionTopicp) ? $__PACKAGE__::_cacheOptionTopicp->[$i] : $option->topic}) {
      next if (! defined(defined($__PACKAGE__::_cacheOptionTopicp) ? $__PACKAGE__::_cacheOptionTopicp->[$i]->{$topic} : $option->topic($topic)));
      next if (! (defined($__PACKAGE__::_cacheOptionTopicp) ? $__PACKAGE__::_cacheOptionTopicp->[$i]->{$topic} : $option->topic($topic)));
      if (! defined($topic_firedp->{$topic})) {
        $topic_firedp->{$topic} = 1;
        $topic_fired_persistencep->{$topic} = $option->topic_persistence;
        $topic_fired_datap->{$topic} = [];
        ++$nbNewTopics;
      }
    }
  }

  return $nbNewTopics;
}

sub currentTopicLevel {
  my $self = shift;

  return scalar(@{$self->topic_level});
}

sub pushTopicLevel {
  my $self = shift;

  my $topic_firedp = $self->topic_fired;
  my $topic_fired_datap = $self->topic_fired_data;
  my $topic_fired_persistencep = $self->topic_fired_persistence;

  #
  # Since we are going to replace the entire hash, keeping a copy of them
  # in @{$self->topic_level} is enough
  #
  push(@{$self->topic_level}, [ $topic_firedp, $topic_fired_persistencep, $topic_fired_datap ]);
  #
  # We remove from current topics those that do not have the 'any' persistence
  #
  my $new_topic_firedp = {};
  my $new_topic_fired_persistencep = {};
  my $new_topic_fired_datap = {};
  foreach my $topic (keys %{$topic_firedp}) {
    my $persistence = $topic_fired_persistencep->{$topic};
    if (grep {$_ eq $persistence} qw/any/) {
      $new_topic_firedp->{$topic} = $topic_firedp->{$topic};
      $new_topic_fired_persistencep->{$topic} = $topic_fired_persistencep->{$topic};
      $new_topic_fired_datap->{$topic} = $topic_fired_datap->{$topic};
    }
  }
  #
  # These lines guarantee that what we have pushed will not be touched using $self->topic_fired() etc... accessors
  # because we replace the entire hash.
  #
  $self->topic_fired($new_topic_firedp);
  $self->topic_fired_persistence($new_topic_fired_persistencep);
  $self->topic_fired_data($new_topic_fired_datap);

}

sub popTopicLevel {
  my $self = shift;

  #
  # We pop current topics and their persistence from the topic_level
  #
  my ($old_topic_firedp, $old_topic_persistencep, $old_topic_datap) = @{$self->topic_level(-1)};
  pop(@{$self->topic_level});
  $self->topic_fired($old_topic_firedp);
  $self->topic_fired_persistence($old_topic_persistencep);
  $self->topic_fired_data($old_topic_datap);

}

sub reset_topic_fired_data {
    my ($self, $topic, $value, $level) = @_;

    $value //= [];
    $level //= 0;

    if (ref($value) ne 'ARRAY') {
      croak 'Topic fired data must be an ARRAY reference';
    }

    #
    # Level MUST be 0 or a negative number
    # It is okay if $value is undef
    #
    $level = int($level);
    if ($level > 0) {
	croak 'int(level) must be 0 or a negative number';
    }
    if ($level == 0) {
	$self->topic_fired_data($topic, $value);
    } else {
	my ($old_topic_fired, $old_topic_persistence, $old_topic_data) = @{$self->topic_level($level)};
	$old_topic_data->{$topic} = $value;
    }

}

1;
