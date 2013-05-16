#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;
use FindBin qw/$Bin/;
use File::Spec;
use Cwd;

my $test_dir = $Bin;
my $dist_dir = Cwd::realpath(File::Spec->catfile($Bin, File::Spec->updir()));
my $ignore_file = File::Spec->catfile($dist_dir, 'ignore.txt');

unless ( $ENV{RELEASE_TESTING} ) {
    plan( skip_all => "Author tests not required for installation" );
}

my $min_tcm = 0.9;
eval "use Test::CheckManifest $min_tcm";
plan skip_all => "Test::CheckManifest $min_tcm required" if $@;

my @exclude_files = ();
if (-r $ignore_file && -s $ignore_file) {
    open(my $exclude_fh, '<', $ignore_file) || die "couldn't open ignore.txt: $!";
    @exclude_files = map {
        s/\s*$//;
        /\*/ ? glob(File::Spec->catfile($dist_dir, $_ )) : File::Spec->catfile($dist_dir, $_)
    } ( <$exclude_fh> );
}

my @absolute_exclude_files = map {my $rel = File::Spec->abs2rel($_, $dist_dir);
                                  my ($volume, $directories, $file) = File::Spec->splitpath($rel);
                                  my @dirs = grep {defined($_) && $_} File::Spec->splitdir($directories);
                                  '/' . join('/', @dirs, $file);
} @exclude_files;

ok_manifest(
    {
        exclude => [ @absolute_exclude_files ],
        filter => [qr/\.svn/, qr/\.git/, qr/^.*~$/ ],
        bool => 'or'
    }
);

done_testing();
