#!perl -T
use 5.006;
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
if (a > b) then
 if (c > d) then
   print(1)
 else
   print(2)
