use 5.006;
use strict;
use warnings FATAL => 'all';
use ExtUtils::MakeMaker;
use File::Spec;

my @tests = (File::Spec->catfile('t', '*.t'), File::Spec->catfile('t', 'Scope', '*.t'));

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
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'MarpaX-Languages-C-AST-*' },
    test => { TESTS => "@tests" } # Works on all platforms because there is no special char, no space, etc
);

my $TEST_REQUIRES = {
    'Test::More'                  => 0,
    'Log::Any'                    => 0,
    'Log::Log4perl'               => 0,
    'Log::Any::Adapter::Log4perl' => 0
};

WriteMakefile(%args);
