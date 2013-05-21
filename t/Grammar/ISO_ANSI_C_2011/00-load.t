#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011' ) || print "Bail out!\n";
}

diag( "Testing MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011 $MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011::VERSION, Perl $], $^X" );
