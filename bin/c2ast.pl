#!env perl
use strict;
use warnings FATAL => 'all';
use diagnostics;
use MarpaX::Languages::C::AST;
use Getopt::Long;
use Config;
use IPC::Run qw/run/;
use Term::ProgressBar;
use POSIX qw/EXIT_FAILURE EXIT_SUCCESS/;
use IO::Handle;

autoflush STDOUT 1;

use Log::Log4perl qw/:easy/;
use Log::Any::Adapter;
use Log::Any qw/$log/;
#
# Init log
#
our $defaultLog4perlConf = <<DEFAULT_LOG4PERL_CONF;
log4perl.rootLogger              = WARN, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 0
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
DEFAULT_LOG4PERL_CONF
Log::Log4perl::init(\$defaultLog4perlConf);
Log::Any::Adapter->set('Log4perl');

# ABSTRACT: C source analysis

# VERSION

# PODNAME: c2ast.pl

my $help = 0;
my @cpp = ();
my @D = ();
my @I = ();
my @U = ();
my $cppfile = '';
my @lexeme = ();

GetOptions ('help!' => \$help,
	    'cpp=s' => \@cpp,
	    'D=s' => \@D,
	    'I=s' => \@I,
	    'U=s' => \@U,
	    'cppfile=s' => \$cppfile,
	    'lexeme=s' => \@lexeme);

@cpp = ('cpp') if (! @cpp);
@lexeme = ('IDENTIFIER') if (! @lexeme);

if ($help || ! @ARGV) {
  usage($help ? EXIT_SUCCESS : EXIT_FAILURE)
}

#
# Run the preprocessor
#
my @cmd = (@cpp, (map {"-D$_"} @D), (map {"-I$_"} @I), (map {"-U$_"} @U), @ARGV);
my $preprocessedOutput;
run(\@cmd, \undef, \$preprocessedOutput);

#
# Number of lines, for Text::ProgressBar
#
my $nbLines = ($preprocessedOutput =~tr/\n/\n/ + ! $preprocessedOutput =~ /\n\z/);
my $progress = Term::ProgressBar->new({name  => $ARGV[-1],
				       count => $nbLines,
				       remove => 1,
				       ETA => 'linear'});
$progress->minor(0);
my $next_update = 0;

#
# Parse C
#
my %lexemeCallbackHash = (file => $cppfile, lexeme => {});
map {++$lexemeCallbackHash{lexeme}->{$_}} @lexeme;
my $cAstObject = MarpaX::Languages::C::AST->new(lexemeCallback => [ \&lexemeCallback, \%lexemeCallbackHash ]);
my $bless = $cAstObject->parse(\$preprocessedOutput);

exit(EXIT_SUCCESS);

# --------------------------------------------------------------------------------------
sub lexemeCallback {
    my ($lexemeCallbackHashp, $lexemeHashp) = @_;

    my $line = $lexemeHashp->{line};
    if ($line >= $next_update) {
	$next_update = $progress->update($line);
    }


    #
    # We wait until the first #line information: this will give the name of current file -;
    #
    if ($lexemeHashp->{name} eq 'PREPROCESSOR_LINE_DIRECTIVE') {
	if ($lexemeHashp->{value} =~ /([\d]+)\s*\"([^\"]+)\"/) {
	    $lexemeCallbackHashp->{curline} = substr($lexemeHashp->{value}, $-[1], $+[1] - $-[1]);
	    $lexemeCallbackHashp->{curline_real} = $lexemeHashp->{line};
	    $lexemeCallbackHashp->{curfile} = substr($lexemeHashp->{value}, $-[2], $+[2] - $-[2]);
	    if (! defined($lexemeCallbackHashp->{file})) {
		$lexemeCallbackHashp->{file} = $lexemeCallbackHashp->{curfile};
	    }
	    if (! defined($lexemeCallbackHashp->{tryToAlignMax})) {
		$lexemeCallbackHashp->{tryToAlignMax} = length(sprintf('%s(%d)', $lexemeCallbackHashp->{file}, 1000000)); # a pretty good max -;
	    }
	}
    }

    if (exists($lexemeCallbackHashp->{lexeme}->{$lexemeHashp->{name}})) {
	if (defined($lexemeCallbackHashp->{file}) &&
	    $lexemeCallbackHashp->{file} eq $lexemeCallbackHashp->{curfile}) {
	    my $line = $lexemeCallbackHashp->{curline} + ($lexemeHashp->{line} - $lexemeCallbackHashp->{curline_real} - 1);
	    my $tryToAlign = sprintf('%s(%d)', $lexemeCallbackHashp->{curfile}, $line);
	    printf "%-*s %-30s %s\n", $lexemeCallbackHashp->{tryToAlignMax}, $tryToAlign, $lexemeHashp->{name}, $lexemeHashp->{value};
	}
    }
}
# --------------------------------------------------------------------------------------
sub usage {
    my $rc = shift;

    print <<USAGE;
Usage: $^X $0 options

where options can be:

--help              This help
--cpp <argument>    cpp executable. Default is 'cpp'.
                    If your setup requires additional option, then you should repeat this option as needed.
                    For example: your cpp setup is "cl -E". Then you say:
                    --cpp cl --cpp -E
-D <argument>       Preprocessor's -D argument, correponding to #define. Can be be repeated if needed.
-I <argument>       Preprocessor's -I argument, corresponding to include path. Can be be repeated if needed.
-U <argument>       Preprocessor's -D argument, corresponding to #undef. Can be be repeated if needed.
-cppfile <filename> In case the C file in input contains a line like # ... "anotherfilename.c".
                    Only one instance of such preprocessor directive is supported.
-lexeme <lexeme>    Lexemes of interest. Look to the grammar to have the exhaustive list.
                    In practice, only IDENTIFIER, TYPEDEF_NAME and ENUMERATION_CONSTANT are useful.
                    This option must be repeated for every lexeme of interest.

Example:

$^X $0                   -D MYDEFINE1 -D MYDEFINE2 -I       /tmp/myIncludeDir            /tmp/myfile.c
$^X $0                   -D MYDEFINE1 -D MYDEFINE2 -I       /tmp/myIncludeDir            /tmp/myfile.c --lexeme IDENTIFIER --lexeme TYPEDEF_NAME
$^X $0 --cpp cl --cpp -E -D MYDEFINE1 -D MYDEFINE2 -I C:/Windows/myIncludeDir C:/Windows/Temp/myfile.c

USAGE

    exit($rc);
}
