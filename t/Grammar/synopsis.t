#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST::Grammar' ) || print "Bail out!\n";
}

my $grammar  = MarpaX::Languages::C::AST::Grammar->new();
my $isoAnsiC2011 = $grammar->read('ISO-ANSI-C-2011');
ok(defined($isoAnsiC2011));
