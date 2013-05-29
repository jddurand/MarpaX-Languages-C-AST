#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 7;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST::Scope' ) || print "Bail out!\n";
}

    my $context = 'A string';
my $cAstScopeObject = MarpaX::Languages::C::AST::Scope->new();
$cAstScopeObject->parseEnterScope($context);
$cAstScopeObject->parseReenterScope($context);
$cAstScopeObject->parseEnterTypedef($context, "myTypedef");
$cAstScopeObject->parseEnterEnum($context, "myEnum");
$cAstScopeObject->parseObscureTypedef($context, "myVariable");
foreach (qw/myTypedef myEnum myVariable/) {
    if ($_ eq 'myTypedef') {
        ok($cAstScopeObject->parseIsTypedef($context, $_));
        ok(! $cAstScopeObject->parseIsEnum($context, $_));
    } elsif ($_ eq 'myEnum') {
        ok(! $cAstScopeObject->parseIsTypedef($context, $_));
        ok($cAstScopeObject->parseIsEnum($context, $_));
    } else {
        ok(! $cAstScopeObject->parseIsTypedef($context, $_));
        ok(! $cAstScopeObject->parseIsEnum($context, $_));
    }
}
