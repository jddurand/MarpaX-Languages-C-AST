use 5.10.0;
use version 0.77; # perl 5.10 minimum, which Marpa's requirement as well
use strict;
use warnings FATAL => 'all';
use ExtUtils::MakeMaker;
use File::Spec;
use File::Find;

my @tests = ();
find({wanted => \&wanted}, 't');
sub wanted { /\.t$/i && push(@tests, File::Spec->catfile($File::Find::dir, $_)); }

my %args = (
    NAME             => 'MarpaX::Languages::C::AST',
    AUTHOR           => q{Jean-Damien Durand <jeandamiendurand@free.fr>},
    VERSION_FROM     => 'lib/MarpaX/Languages/C/AST.pm',
    ABSTRACT_FROM    => 'lib/MarpaX/Languages/C/AST.pm',
    LICENSE          => 'Artistic_2_0',
    PL_FILES         => {},
    MIN_PERL_VERSION => 5.006,
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => 0,
    },
    BUILD_REQUIRES => {
        'Test::More' => 0,
    },
    PREREQ_PM => {
        #'ABC'              => 1.6,
        #'Foo::Bar::Module' => 5.0401,
    },
    META_MERGE        => {
	resources => {
	    repository  =>  'https://github.com/jddurand/MarpaX-Languages-C-AST',
	},
    },
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'MarpaX-Languages-C-AST-*' },
    test => { TESTS => "@tests" } # Works on all platforms because there is no special char, no space, etc
);

my $TEST_REQUIRES = {
    'Test::More'                  => 0,
    'Log::Any'                    => 0,
    'Marpa::R2'                   => 2.055002,
    'Module::Info'                => 0,
};

if (version->parse($ExtUtils::MakeMaker::VERSION) >= version->parse('6.63_03')) {
    $args{TEST_REQUIRES} = $TEST_REQUIRES;
} else {
    $args{BUILD_REQUIRES} = $TEST_REQUIRES;
}

WriteMakefile(%args);
