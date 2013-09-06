use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Scope;
use MarpaX::Languages::C::AST::Util qw/whoami/;

# ABSTRACT: Scope management when translating a C source to an AST

use Storable qw/dclone/;
use Log::Any qw/$log/;
use Carp qw/croak/;

# VERSION

=head1 DESCRIPTION

This modules manages the scopes when translation a C source into an AST tree. This module started after reading the article:

I<Resolving Typedefs in a Multipass C Compiler> from I<Journal of C Languages Translation>, Volume 2, Number 4, written by W.M. McKeeman. A online version may be accessed at L<http://www.cs.dartmouth.edu/~mckeeman/references/JCLT/ResolvingTypedefsInAMultipassCCompiler.pdf>.

Please note that this module is logging via Log::Any.

=head1 SYNOPSIS

    use strict;
    use warnings FATAL => 'all';
    use MarpaX::Languages::C::AST::Scope;

    my $cAstScopeObject = MarpaX::Languages::C::AST::Scope->new();
    $cAstScopeObject->parseEnterScope();
    $cAstScopeObject->parseEnterTypedef("myTypedef");
    $cAstScopeObject->parseEnterEnum("myEnum");
    $cAstScopeObject->parseObscureTypedef("myVariable");
    foreach (qw/myTypedef myEnum myVariable/) {
      if ($cAstScopeObject->parseIsTypedef($_)) {
        print "\"$_\" is a typedef\n";
      } elsif ($cAstScopeObject->parseIsEnum($_)) {
        print "\"$_\" is an enum\n";
      }
    }
    $cAstScopeObject->parseExitScope();

=head1 SUBROUTINES/METHODS

=head2 new

Instance a new object. Takes no parameter.

=cut

sub new {
  my ($class) = @_;

  my $self  = {
      _typedefPerScope => [ {} ],
      _enumAnyScope => {},
      _delay => 0,
      _enterScopeCallback => [],
      _exitScopeCallback => [],
  };
  bless($self, $class);

  return $self;
}

=head2 typedefPerScope($self)

Returns the list of known typedefs per scope. At the end of processing a source code, only scope 0 still exist. The output is a reference to an array, file-level scope being at index 0. At each indice, there is a reference to a hash with typedef name as a key, value is useless.

=cut

sub typedefPerScope {
  my ($self) = @_;

  return $self->{_typedefPerScope};
}

=head2 enumAnyScope($self)

Returns the list of known enums. Enums has no scope level: as soon as the parser sees an enum, it available at any level. The output is a reference to a hash with enumeration name as a key, value is useless.

=cut

sub enumAnyScope {
  my ($self) = @_;

  return $self->{_enumAnyScope};
}

=head2 parseEnterScope($self)

Say we enter a scope.

=cut

sub parseEnterScope {
  my ($self) = @_;

  # $self->condExitScope();

  my $scope = $self->parseScopeLevel;
  if ($log->is_debug) {
      $log->debugf('[%s] Duplicating scope %d to %d', whoami(__PACKAGE__), $scope, $scope + 1);
  }
  push(@{$self->{_typedefPerScope}}, dclone($self->{_typedefPerScope}->[$scope]));

  if (@{$self->{_enterScopeCallback}}) {
      my ($ref, @args) = @{$self->{_enterScopeCallback}};
      &$ref(@args);
  }

}

=head2 parseDelay($self, [$value])

Returns/Set current delay flag.

=cut

sub parseDelay {
  my $self = shift;
  if (@_) {
    my $scope = $self->parseScopeLevel;
    my $value = shift;
    if ($log->is_debug) {
	$log->debugf('[%s] Setting delay flag to %d at scope %d', whoami(__PACKAGE__), $value, $scope);
    }
    $self->{_delay} = $value;
  }
  return $self->{_delay};
}

=head2 parseScopeLevel($self)

Returns current scope level, starting at number 0.

=cut

sub parseScopeLevel {
  my ($self) = @_;

  return $#{$self->{_typedefPerScope}};
}

=head2 parseEnterScopeCallback($self, $ref, @args)

Callback method when entering a scope.

=cut

sub parseEnterScopeCallback {
  my ($self, $ref, @args) = @_;

  $self->{_enterScopeCallback} = [ $ref, @args ];
}

=head2 parseExitScopeCallback($self, $ref, @args)

Callback method when leaving a scope (not the delayed operation, the real leave).

=cut

sub parseExitScopeCallback {
  my ($self, $ref, @args) = @_;

  $self->{_exitScopeCallback} = [ $ref, @args ];
}

=head2 parseExitScope($self, [$now])

Say we want to leave current scope. The operation is delayed unless $now flag is true.

=cut

sub parseExitScope {
  my ($self, $now) = @_;
  $now //= 0;

  if ($now) {
    $self->doExitScope();
  } else {
    $self->parseDelay(1);
  }
}

=head2 parseReenterScope($self)

Reenter previous scope.

=cut

sub parseReenterScope {
  my ($self) = @_;

  my $scope = $self->parseScopeLevel;
  if ($log->is_debug) {
      $log->debugf('[%s] Reenter scope at scope %d', whoami(__PACKAGE__), $scope);
  }
  $self->parseDelay(0);

}

=head2 condExitScope($self)

Leave current scope if delay flag is set and not yet done.

=cut

sub condExitScope {
  my ($self) = @_;

  if ($self->parseDelay) {
    $self->doExitScope();
  }
}

=head2 doExitScope($self)

Leave current scope.

=cut

sub doExitScope {
  my ($self) = @_;

  my $scope = $self->parseScopeLevel;
  if ($log->is_debug) {
      $log->debugf('[%s] Removing scope %d', whoami(__PACKAGE__), $scope);
  }
  pop(@{$self->{_typedefPerScope}});

  if (@{$self->{_exitScopeCallback}}) {
      my ($ref, @args) = @{$self->{_exitScopeCallback}};
      &$ref(@args);
  }
  $self->parseDelay(0);
}

=head2 parseEnterTypedef($self, $token)

Declare a new typedef with name $token, that will be visible until current scope is left.

=cut

sub parseEnterTypedef {
  my ($self, $token) = @_;

  my $scope = $self->parseScopeLevel;
  $self->{_typedefPerScope}->[$scope]->{$token} = 1;

  if ($log->is_debug) {
      $log->debugf('[%s] "%s" typedef entered at scope %d', whoami(__PACKAGE__), $token, $scope);
  }
}

=head2 parseEnterEnum($self, $token)

Declare a new enum with name $token, that will be visible at any scope from now on.

=cut

sub parseEnterEnum {
  my ($self, $token) = @_;

  $self->{_enumAnyScope}->{$token} = 1;
  my $scope = $self->parseScopeLevel;
  if ($log->is_debug) {
      $log->debugf('[%s] "%s" enum entered at scope %d', whoami(__PACKAGE__), $token, $scope);
  }
  #
  # Enum wins from now on and forever
  #
  foreach (0..$#{$self->{_typedefPerScope}}) {
      $self->parseObscureTypedef($token, $_);
  }
}

=head2 parseObscureTypedef($self, $token)

Obscures a typedef named $token.

=cut

sub parseObscureTypedef {
  my ($self, $token, $scope) = @_;

  $scope //= $self->parseScopeLevel;
  $self->{_typedefPerScope}->[$scope]->{$token} = 0;

  if ($log->is_debug) {
      $log->debugf('[%s] "%s" eventual typedef obscured at scope %d', whoami(__PACKAGE__), $token, $scope);
  }
}

=head2 parseIsTypedef($self, $token)

Return a true value if $token is a typedef.

=cut

sub parseIsTypedef {
  my ($self, $token) = @_;

  my $scope = $self->parseScopeLevel;
  my $rc = (exists($self->{_typedefPerScope}->[$scope]->{$token}) && $self->{_typedefPerScope}->[$scope]->{$token}) ? 1 : 0;

  if ($log->is_debug) {
      $log->debugf('[%s] "%s" at scope %d is a typedef? %s', whoami(__PACKAGE__), $token, $scope, $rc ? 'yes' : 'no');
  }

  return($rc);
}

=head2 parseIsEnum($self, $token)

Return a true value if $token is an enum.

=cut

sub parseIsEnum {
  my ($self, $token) = @_;

  my $rc = (exists($self->{_enumAnyScope}->{$token}) && $self->{_enumAnyScope}->{$token}) ? 1 : 0;

  my $scope = $self->parseScopeLevel;
  if ($log->is_debug) {
      $log->debugf('[%s] "%s" is an enum at scope %d? %s', whoami(__PACKAGE__), $token, $scope, $rc ? 'yes' : 'no');
  }

  return($rc);
}

1;
