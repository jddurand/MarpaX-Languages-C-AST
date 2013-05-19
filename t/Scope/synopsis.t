#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More tests => 7;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST::Scope' ) || print "Bail out!\n";
}

my $cAstScopeObject = MarpaX::Languages::C::AST::Scope->new();
$cAstScopeObject->parseEnterScope();
$cAstScopeObject->parseReenterScope();
$cAstScopeObject->parseEnterTypedef("myTypedef");
$cAstScopeObject->parseEnterEnum("myEnum");
$cAstScopeObject->parseObscureTypedef("myVariable");
foreach (qw/myTypedef myEnum myVariable/) {
    if ($_ eq 'myTypedef') {
        ok($cAstScopeObject->parseIsTypedef($_));
        ok(! $cAstScopeObject->parseIsEnum($_));
    } elsif ($_ eq 'myEnum') {
        ok(! $cAstScopeObject->parseIsTypedef($_));
        ok($cAstScopeObject->parseIsEnum($_));
    } else {
        ok(! $cAstScopeObject->parseIsTypedef($_));
        ok(! $cAstScopeObject->parseIsEnum($_));
    }
}
