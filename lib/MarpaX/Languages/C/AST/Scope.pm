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

but has been simplified using Marpa events facility and the Callback framework built with C::AST. Basically there is not more notion od delayed exit scope. Using events is enough to exact scope management in time.

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
      _enumAnyScope => {}
  };
  bless($self, $class);

  return $self;
}

=head2 parseEnterScope($self)

Say we enter a scope.

=cut

sub parseEnterScope {
  my ($self) = @_;

  my $scope = $#{$self->{_typedefPerScope}};
  push(@{$self->{_typedefPerScope}}, dclone($self->{_typedefPerScope}->[$scope]));

  $log->debugf('[%s] Duplicated scope %d to %d', whoami(__PACKAGE__), $scope, $scope + 1);
  
}

=head2 parseExitScope($self)

Say we leave current scope.

=cut

sub parseExitScope {
  my ($self) = @_;

  my $scope = $#{$self->{_typedefPerScope}};
  pop(@{$self->{_typedefPerScope}});

  $log->debugf('[%s] Removed scope %d', whoami(__PACKAGE__), $scope);

}

=head2 parseEnterTypedef($self, $token)

Declare a new typedef with name $token, that will be visible until current scope is left.

=cut

sub parseEnterTypedef {
  my ($self, $token) = @_;

  my $scope = $#{$self->{_typedefPerScope}};
  $self->{_typedefPerScope}->[$scope]->{$token} = 1;

  $log->debugf('[%s] "%s" typedef entered at scope %d', whoami(__PACKAGE__), $token, $scope);
}

=head2 parseEnterEnum($self, $token)

Declare a new enum with name $token, that will be visible at any scope from now on.

=cut

sub parseEnterEnum {
  my ($self, $token) = @_;

  $self->{_enumAnyScope}->{$token} = 1;

  $log->debugf('[%s] "%s" enum entered', whoami(__PACKAGE__), $token);

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

  $scope //= $#{$self->{_typedefPerScope}};
  $self->{_typedefPerScope}->[$scope]->{$token} = 0;

  $log->debugf('[%s] "%s" eventual typedef obscured at scope %d', whoami(__PACKAGE__), $token, $scope);
}

=head2 parseIsTypedef($self, $token)

Return a true value if $token is a typedef.

=cut

sub parseIsTypedef {
  my ($self, $token) = @_;

  my $scope = $#{$self->{_typedefPerScope}};
  my $rc = (exists($self->{_typedefPerScope}->[$scope]->{$token}) && $self->{_typedefPerScope}->[$scope]->{$token}) ? 1 : 0;

  $log->debugf('[%s] "%s" at scope %d is a typedef? %s', whoami(__PACKAGE__), $token, $scope, $rc ? 'yes' : 'no');

  return($rc);
}

=head2 parseIsEnum($self, $token)

Return a true value if $token is an enum.

=cut

sub parseIsEnum {
  my ($self, $token) = @_;

  my $rc = (exists($self->{_enumAnyScope}->{$token}) && $self->{_enumAnyScope}->{$token}) ? 1 : 0;

  $log->debugf('[%s] "%s" is an enum? %s', whoami(__PACKAGE__), $token, $rc ? 'yes' : 'no');

  return($rc);
}

1;
