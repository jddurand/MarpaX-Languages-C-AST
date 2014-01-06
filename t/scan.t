#!perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;
use File::Spec;
use Data::Dumper;

BEGIN {
    push(@INC, 'inc');
    use_ok( 'MarpaX::Languages::C::Scan' ) || print "Bail out!\n";
}

my $filename = File::Spec->catfile('inc', 'scan.c');
my $c = MarpaX::Languages::C::Scan->new(filename => $filename);
my $ast = $c->ast();
ok(defined($ast), 'Output from ast() is ok');
print STDERR Dumper($c->defines_no_args);
