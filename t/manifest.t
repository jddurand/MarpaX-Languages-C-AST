#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More;
use FindBin qw/$Bin/;
use File::Spec;
use Cwd;

my $test_dir = $Bin;
my $dist_dir = File::Spec->canonpath(Cwd::realpath(File::Spec->catdir($Bin, File::Spec->updir())));
my $ignore_file = File::Spec->canonpath(File::Spec->catfile($dist_dir, 'ignore.txt'));

unless ( $ENV{RELEASE_TESTING} ) {
    plan( skip_all => "Author tests not required for installation" );
}

my $min_tcm = 0.9;
eval "use Test::CheckManifest $min_tcm";
plan skip_all => "Test::CheckManifest $min_tcm required" if $@;

my @exclude_files = ();
if (-r $ignore_file && -s $ignore_file) {
    open(my $exclude_fh, '<', $ignore_file) || die "couldn't open ignore.txt: $!";
    #
    # Only the first character must be '/'.
    # The rest can have native separator, e.g. '\' on Windows
    #
    @exclude_files = map
    {
        $_ = File::Spec->canonpath($_);
        $_ = '/' . File::Spec->abs2rel($_, $dist_dir);
        $_;
    } map
    {
        s/\s*$//;
        /\*/ ?
            glob( File::Spec->catfile( $dist_dir, $_ ) ) :
            File::Spec->catfile( $dist_dir, $_ )
    }
    ( <$exclude_fh> );
}

ok_manifest({ exclude => [ @exclude_files ],
	      filter  => [qr/\.svn/, qr/\.git/, qr/^.*~$/ ],
	      bool    => 'or',
	    });

done_testing();
