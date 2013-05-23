#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 4;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST::Grammar' ) || print "Bail out!\n";
}

my $grammar  = MarpaX::Languages::C::AST::Grammar->new('ISO-ANSI-C-2011');

my $isoAnsiC2011 = $grammar->content();
ok(defined($isoAnsiC2011));
my $grammar_option = $grammar->grammar_option();
ok(ref($grammar_option) eq 'HASH');
my $recce_option = $grammar->recce_option();
ok(ref($recce_option) eq 'HASH');
