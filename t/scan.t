#!perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 3;
use File::Spec;
use Data::Dumper;

BEGIN {
    push(@INC, 'inc');
    use_ok( 'MarpaX::Languages::C::Scan' ) || print "Bail out!\n";
}

my $filename = File::Spec->catfile('inc', 'scan.c');
my $c = MarpaX::Languages::C::Scan->new(filename => $filename);
my $ast = $c->ast();
is_deeply($c->defines_no_args,
          {
              'MACRO_NO_ARGS_01' => '',
              'MACRO_NO_ARGS_02' => 'something'
          },
          'defines_no_args');
is_deeply($c->defines_args,
          {
              'MACRO_NO_ARGS_02' => [
                  [
                   'b',
                   'c'
                  ],
                  "something(b) + else(c) \\\ncontinued"
                  ],
                  'MACRO_NO_ARGS_01' => [
                      [
                       'a'
                      ],
                      ''
                  ]
          },
          'defines_args');
