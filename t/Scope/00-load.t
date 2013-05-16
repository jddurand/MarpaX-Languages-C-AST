#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST::Scope' ) || print "Bail out!\n";
}

diag( "Testing MarpaX::Languages::C::AST::Scope $MarpaX::Languages::C::AST::Scope::VERSION, Perl $], $^X" );
