#!perl -T
#
# My own version of changes.t since Test::CheckChanges has problems to be cross-platform
#
use strict;
use warnings FATAL => 'all';
use Test::More;
use FindBin qw/$Bin/;
use File::Spec;
use File::Find qw/find/;
use File::Basename qw/fileparse/;
use Cwd;
use version 0.77;

unless ( $ENV{RELEASE_TESTING} ) {
    plan( skip_all => "Author tests not required for installation" );
}

my $test_dir = $Bin;
my $dist_dir = File::Spec->canonpath(Cwd::realpath(File::Spec->catdir($Bin, File::Spec->updir())));
my $lib_dir = File::Spec->catdir($dist_dir, 'lib');

my @pm = ();
sub wantedpm {
    my $path = File::Spec->canonpath($_);
    my $relpath = File::Spec->abs2rel($path, $lib_dir);
    my ($filename, $directories, $suffix) = fileparse($relpath, qr/\.[^.]*/);
    if (lc($suffix) eq '.pm') {
        my @directories = grep {defined($_) && $_} File::Spec->splitdir($directories);
        my $pm = join('::', @directories, $filename);
        push(@pm, $pm);
    }
}
find({ wanted => \&wantedpm, no_chdir => 1 }, $lib_dir);
BAIL_OUT("No module found") if (! @pm);
#
# Check all modules has a VERSION.
#
my @version = ();
foreach (@pm) {
    my $pm = $_;

    #
    ## Untaint the eval
    #
    my $toeval;
    if ("use $pm; 1" =~ /^(.*)$/) {
	$toeval = $1;
    }

    my $useok = eval $toeval;
    fail("$pm, $@") if (! defined($useok) || ! $useok);
    my $version = undef;
    {
        no strict "subs";
	if ("\$${pm}::VERSION" =~ /^(.*)$/) {
	    $toeval = $1;
	}
        $version = eval $toeval;
        if (! defined($version)) {
            fail("$pm, cannot get VERSION");
        } else {
            pass("\$${pm}::VERSIONS => $version");
        }
    }
    push(@version, version->parse($version));
}
#
# Check all modules values of VERSION
#
if ($#version > 0) {
    foreach (1..$#version) {
        #
        # Doing ok($version[$_] != $version[0]) does not work (!?)
        #
        if ($version[$_] != $version[0]) {
            fail("\$$pm[$_]::VERSIONS = " . $version[$_]->stringify() . " is not equal to \$$pm[0]::VERSIONS = " . $version[0]->stringify());
        } else {
            pass("\$$pm[$_]::VERSIONS == \$$pm[0]::VERSIONS == " . $version[0]->stringify());
        }
    }
}
#
# Check the Changes file
#
opendir(DIR, $dist_dir) || BAIL_OUT("$dist_dir, $!");
my $hasChanges = 0;
my @changes_versions = ();
while(defined($_ = readdir(DIR))) {
    my $file = $_;
    next if (! ($file =~ /changes/i));
    ++$hasChanges;
    my $changes = File::Spec->catfile($dist_dir, $file);
    open(CHANGES, '<', $changes) || BAIL_OUT("$changes, $!");
    while (<CHANGES>) {
        my $line = $_;
        #
        ## Well, we do as Test::CheckChanges
        #
        $line =~ s/\s*//;
        $line =~ s/^\s//;
        my $version = undef;
        if ($line =~ /^(?:\*|version\s*:)?\s*([\d\._]+)/) {
            $version = version->parse($1);
        }
        if (defined($version)) {
            pass("Found version $1, understood as " . $version->stringify() . " in $file");
            push(@changes_versions, {file => $file, version => $version});
        }
    }
    close(CHANGES) || warn "Cannot close $changes, $!\n";
}
closedir(DIR) || warn "Cannot closedir $dist_dir, $!\n";
BAIL_OUT("No version found in the $hasChanges Changes-like files") if ($hasChanges && ! @changes_versions);
if (@changes_versions) {
    #
    # Get the highest and compare it to $version[0]
    #
    my @sorted_changes_version = sort {$a->{version} <=> $b->{version}} @changes_versions;
    my $highest_changes_version = $sorted_changes_version[-1];
    if ($highest_changes_version->{version} != $version[0]) {
        fail("$highest_changes_version->{file} highest version = " . $highest_changes_version->{version}->stringify() . " is not equal to \$$pm[0]::VERSIONS = " . $version[0]->stringify());
    } else {
        pass("$highest_changes_version->{file} version == \$$pm[0]::VERSIONS == " . $version[0]->stringify());
    }
}
done_testing();
