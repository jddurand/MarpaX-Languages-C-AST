#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More tests => 4;
use File::Temp qw/tempfile tempdir/;
use File::Basename;
use File::Spec;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST::Grammar' ) || print "Bail out!\n";
}

my $grammar  = MarpaX::Languages::C::AST::Grammar->new();

my $isoAnsiC2011 = $grammar->read('ISO-ANSI-C-2011.bnf');
ok(defined($isoAnsiC2011));

my $dir = tempdir(CLEANUP => 1);
my ($fh, $filename) = tempfile(DIR => $dir);
$grammar->dirpath($dir);
my $filesp = $grammar->list();
ok($#{$filesp} == 0);
ok($filesp->[0] eq File::Spec->canonpath($filename));
