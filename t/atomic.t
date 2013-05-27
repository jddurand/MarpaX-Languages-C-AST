#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 3;

BEGIN {
    push(@INC, 'inc');
    use_ok( 'MarpaX::Languages::C::AST' ) || print "Bail out!\n";
}
use Expected;

my $cSourceCode = do { local $/; <DATA> };
my $cAst = MarpaX::Languages::C::AST->new();
my $valuep = $cAst->parse(\$cSourceCode);
ok(defined($valuep));
is_deeply($valuep, Expected->value($valuep), 'Expected blessed structure');

__DATA__
typedef int AA;
int int;
int AA;
typedef int toto;

int main(void) {
  int _Atomic (toto)(void);
  return toto();
}
