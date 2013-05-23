#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST' ) || print "Bail out!\n";
}

my $cSourceCode = do { local $/; <DATA> };
my $cAst = MarpaX::Languages::C::AST->new();
my $blessed = $cAst->parse(\$cSourceCode);
ok(defined($blessed));

__DATA__
typedef struct s1_ {int i1;} x1, y1;
struct x1 {x1 i2;};
x1 x;
