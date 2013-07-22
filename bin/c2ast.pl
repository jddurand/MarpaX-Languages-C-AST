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
use MarpaX::Languages::C::AST::Util::Data::Find;
use File::Basename qw/basename dirname/;
use Scalar::Util qw/blessed/;
use Data::Dumper;
use Log::Log4perl qw/:easy/;
use Log::Any::Adapter;
use Log::Any qw/$log/;

autoflush STDOUT 1;

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
my $progress = 0;
my @check = ();
my $dump = 0;
my $loglevel = 'WARN';

if (! GetOptions ('help!' => \$help,
                  'cpp=s' => \@cpp,
                  'D=s' => \@D,
                  'I=s' => \@I,
                  'U=s' => \@U,
                  'cppfile=s' => \$cppfile,
                  'lexeme=s' => \@lexeme,
                  'progress!' => \$progress,
		  'check=s' => \@check,
		  'dump!' => \$dump,
		  'loglevel=s' => \$loglevel,
		  )) {
  usage(EXIT_FAILURE);
}

# ----
# Init 
# ----
my $defaultLog4perlConf = <<DEFAULT_LOG4PERL_CONF;
log4perl.rootLogger              = $loglevel, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 0
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
DEFAULT_LOG4PERL_CONF
Log::Log4perl::init(\$defaultLog4perlConf);
Log::Any::Adapter->set('Log4perl');

@cpp = ('cpp') if (! @cpp);

if ($help || ! @ARGV) {
  usage($help ? EXIT_SUCCESS : EXIT_FAILURE)
}

# --------------------
# Run the preprocessor
# --------------------
my @cmd = (@cpp, (map {"-D$_"} @D), (map {"-I$_"} @I), (map {"-U$_"} @U), @ARGV);
my $preprocessedOutput;
run(\@cmd, \undef, \$preprocessedOutput);

# -----------------
# Callback argument
# -----------------
my %lexemeCallbackHash = (file => $cppfile,
			  lexeme => {},
			  internalLexeme => {},
			  progress => undef,
			  position2line => {},
			  next_progress => 0,
			  allfiles => {});

my %check = ();
map {++$check{$_}} @check;
if (exists($check{reservedNames})) {
    # Force IDENTIFIER internal survey
    $lexemeCallbackHash{internalLexeme}->{IDENTIFIER} = 1;
}

if ($progress) {
  #
  # Number of lines, for Text::ProgressBar
  #
  $lexemeCallbackHash{nbLines} = ($preprocessedOutput =~tr/\n/\n/ + ! $preprocessedOutput =~ /\n\z/);
  $lexemeCallbackHash{progress} = Term::ProgressBar->new({name  => $ARGV[-1],
                                                          count => $lexemeCallbackHash{nbLines},
                                                          remove => 1,
                                                          ETA => 'linear'});
  $lexemeCallbackHash{progress}->minor(0);
}

# -------
# Parse C
# -------
map {++$lexemeCallbackHash{lexeme}->{$_}} @lexeme;
my $cAstObject = MarpaX::Languages::C::AST->new(lexemeCallback => [ \&lexemeCallback, \%lexemeCallbackHash ], logInfo => $log->is_info());
my $bless = $cAstObject->parse(\$preprocessedOutput);
if ($progress) {
    if ($lexemeCallbackHash{nbLines} > $lexemeCallbackHash{next_progress}) {
	$lexemeCallbackHash{progress}->update($lexemeCallbackHash{nbLines});
    }
}

# --------------
# Postprocessing
# --------------
check(\%check, \%lexemeCallbackHash, $bless);

# ----
# Dump
# ----
if ($dump) {
    print Dumper($bless);
}

exit(EXIT_SUCCESS);

# --------------------------------------------------------------------------------------
sub check {
    my ($checkp, $lexemeCallbackHashp, $bless) = @_;

    if (exists($checkp->{reservedNames})) {
	checkreservedNames($lexemeCallbackHashp, $bless);
    }

}
# --------------------------------------------------------------------------------------
sub checkreservedNames {
    my ($lexemeCallbackHashp, $bless) = @_;

    #
    ## Apply GNU rules on every directDeclaratorIdentifier with a position
    ## that matches that ones in the cpp filename
    #

    my %check = (
	qr/^E[\dA-Z]/             => 'Names beginning with a capital \'E\' followed by a digit or uppercase letter may be used for additional error code names',
	qr/^(?:is|to)[a-z]/       => 'Names that begin with either \'is\' or \'to\' followed by a lowercase letter may be used for additional character testing and conversion functions.',
	qr/^LC_[A-Z]/             => 'Names that begin with \'LC_\' followed by an uppercase letter may be used for additional macros specifying locale attributes',
	qr/^(?:sin|cos|tan|sincos|csin|ccos|ctan|asin|acos||atan|atan2|casin|cacos|catan|exp|exp2|exp10|log|log10|log2|logb|ilogb|pow|sqrt|cbrt|hypot|expm1|log1p|cexp|clog|clog10|csqrt|cpow|sinh|cosh|tanh|csinh|ccosh|ctanh|asinh|acosh|atanh|casinh|cacosh|catanh|erf|erfc|lgamma|gamma|tgamma|j0|j1|jn|y0|y1|yn|)[fl]$/                => 'Names of all existing mathematics functions suffixed with \'f\' or \'l\' are reserved for corresponding functions that operate on float and long double arguments, respectively',
	qr/^SIG[A-Z]/             => 'Names that begin with \'SIG\' followed by an uppercase letter are reserved for additional signal names',
	qr/^SIG_[A-Z]/            => 'Names that begin with \'SIG_\' followed by an uppercase letter are reserved for additional signal actions',
	qr/^(?:str|mem|wcs)[a-z]/ => 'Names beginning with \'str\', \'mem\', or \'wcs\' followed by a lowercase letter are reserved for additional string and array functions',
	qr/_t$/                   => 'Names that end with \'_t\' are reserved for additional type names'
    );

    if (grep {basename($_) eq 'dirent.h'} keys %{$lexemeCallbackHashp->{allfiles}}) {
	$check{qr/^d_/}      =  'The header file dirent.h reserves names prefixed with \'d_\'';
    }
    if (grep {basename($_) eq 'fcntl.h'} keys %{$lexemeCallbackHashp->{allfiles}}) {
	$check{qr/^[lFOS]_/} =  'The header file fcntl.h reserves names prefixed with \'l_\', \'F_\', \'O_\', and \'S_\'';
    }
    if (grep {basename($_) eq 'grp.h'} keys %{$lexemeCallbackHashp->{allfiles}}) {
	$check{qr/^gr_/}     =  'The header file grp.h reserves names prefixed with \'gr_\'';
    }
    if (grep {basename($_) eq 'limits.h'} keys %{$lexemeCallbackHashp->{allfiles}}) {
	$check{qr/_MAX$/}    =  'The header file limits.h reserves names suffixed with \'_MAX\'';
    }
    if (grep {basename($_) eq 'pwd.h'} keys %{$lexemeCallbackHashp->{allfiles}}) {
	$check{qr/^pw_/}      =  'The header file pwd.h reserves names prefixed with \'pw_\'';
    }
    if (grep {basename($_) eq 'signal.h'} keys %{$lexemeCallbackHashp->{allfiles}}) {
	$check{qr/^(?:ssa|SA)_/}  =  'The header file signal.h reserves names prefixed with \'sa_\' and \'SA_\'';
    }
    if (grep {basename(dirname($_)) eq 'sys' && basename($_) eq 'stat.h'} keys %{$lexemeCallbackHashp->{allfiles}}) {
	$check{qr/^(?:st|S)_/}      =  'The header file sys/stat.h reserves names prefixed with \'st_\' and \'S_\'';
    }
    if (grep {basename(dirname($_)) eq 'sys' && basename($_) eq 'times.h'} keys %{$lexemeCallbackHashp->{allfiles}}) {
	$check{qr/^tms_/}      =  'The header file sys/times.h reserves names prefixed with \'tms_\'';
    }
    if (grep {basename($_) eq 'termios.h'} keys %{$lexemeCallbackHashp->{allfiles}}) {
	$check{qr/^(?:c_|V|I|O|TC|B\d)/}      =  'The header file termios.h reserves names prefixed with \'c_\', \'V\', \'I\', \'O\', and \'TC\'; and names prefixed with \'B\' followed by a digit';
    }

    MarpaX::Languages::C::AST::Util::Data::Find->new(
	wanted => sub { my $o = shift;
			my $class = blessed($o) || '';
			return ($class eq 'C::AST::directDeclaratorIdentifier');
	},
	callback => sub { my ($lexemeCallbackHashp, $o) = @_;
			  #
			  # By definition, the "value" of directDeclaratorIdentifier is
			  # the IDENTIFIER lexeme value: [start,length,values]
			  #
			  my $position = $o->[0]->[0];
			  if (exists($lexemeCallbackHashp->{position2line}->{$position})) {
			      my $name = $o->[0]->[2];
			      my $line = $lexemeCallbackHashp->{position2line}->{$position};

			      my $tryToAlign = sprintf('%s(%d)', $lexemeCallbackHashp->{curfile}, $line);

			      while (my ($re, $string) = each %check) {
				  if ($name =~ $re) {
				      printf STDERR "%-*s %s: %s\n", $lexemeCallbackHashp->{tryToAlignMax}, $tryToAlign, $name, $string;
				  }
			      }
			  }
	},
	callbackArgs => [ $lexemeCallbackHashp ],
	)->process(${$bless});
}
# --------------------------------------------------------------------------------------
sub lexemeCallback {
    my ($lexemeCallbackHashp, $lexemeHashp) = @_;

    if (defined($lexemeCallbackHashp->{progress})) {
      if ($lexemeHashp->{line} >= $lexemeCallbackHashp->{next_progress}) {
        $lexemeCallbackHashp->{next_progress} = $lexemeCallbackHashp->{progress}->update($lexemeHashp->{line});
      }
    }

    #
    # We wait until the first #line information: this will give the name of current file
    #
    if ($lexemeHashp->{name} eq 'PREPROCESSOR_LINE_DIRECTIVE') {
	if ($lexemeHashp->{value} =~ /([\d]+)\s*\"([^\"]+)\"/) {
	    $lexemeCallbackHashp->{curline} = substr($lexemeHashp->{value}, $-[1], $+[1] - $-[1]);
	    $lexemeCallbackHashp->{curline_real} = $lexemeHashp->{line};
	    $lexemeCallbackHashp->{curfile} = substr($lexemeHashp->{value}, $-[2], $+[2] - $-[2]);
	    $lexemeCallbackHashp->{allfiles}->{$lexemeCallbackHashp->{curfile}}++;
	    if (! $lexemeCallbackHashp->{file}) {
		$lexemeCallbackHashp->{file} = $lexemeCallbackHashp->{curfile};
	    }
	    if (! defined($lexemeCallbackHashp->{tryToAlignMax})) {
		$lexemeCallbackHashp->{tryToAlignMax} = length(sprintf('%s(%d)', $lexemeCallbackHashp->{file}, 1000000)); # a pretty good max -;
	    }
	}
    }

    if (exists($lexemeCallbackHashp->{lexeme}->{$lexemeHashp->{name}}) ||
	exists($lexemeCallbackHashp->{internalLexeme}->{$lexemeHashp->{name}})) {
	if (defined($lexemeCallbackHashp->{file}) &&
	    defined($lexemeCallbackHashp->{curfile}) &&
	    $lexemeCallbackHashp->{file} eq $lexemeCallbackHashp->{curfile}) {
	    my $line = $lexemeCallbackHashp->{curline} + ($lexemeHashp->{line} - $lexemeCallbackHashp->{curline_real} - 1);
	    $lexemeCallbackHashp->{position2line}->{$lexemeHashp->{start}} = $line;
	    if (exists($lexemeCallbackHashp->{lexeme}->{$lexemeHashp->{name}})) {
		my $tryToAlign = sprintf('%s(%d)', $lexemeCallbackHashp->{curfile}, $line);
		printf "%-*s %-30s %s\n", $lexemeCallbackHashp->{tryToAlignMax}, $tryToAlign, $lexemeHashp->{name}, $lexemeHashp->{value};
	    }
	}
    }

}
# --------------------------------------------------------------------------------------
sub usage {
    my $rc = shift;

    print <<USAGE;
Usage: $^X $0 options

where options can be:

--help               This help
--cpp <argument>     cpp executable. Default is 'cpp'.
                     If your setup requires additional option, then you should repeat this option as needed.
                     For example: your cpp setup is "cl -E". Then you say:
                     --cpp cl --cpp -E
-D <argument>        Preprocessor's -D argument, correponding to #define. Can be be repeated if needed.
-I <argument>        Preprocessor's -I argument, corresponding to include path. Can be be repeated if needed.
-U <argument>        Preprocessor's -D argument, corresponding to #undef. Can be be repeated if needed.
--cppfile <filename> In case the C file in input contains a line like # ... "anotherfilename.c", then
                     it is "anotherfilename.c" that will be taken as the file name to trace/check.
                     Only one instance of such preprocessor directive is supported.
                     This option is only needed when the real original filename is not the file passed
                     as argument to c2ast. Typically, this happens when the file passed as argument is
                     the result of another preprocessing phase.
                     Exemple: the file marpa.c in the build phase of libmarpa: this is the result of
                     a preprocessing on the file marpa.w.
--lexeme <lexeme>    Lexemes of interest. Look to the grammar to have the exhaustive list.
                     In practice, only IDENTIFIER, TYPEDEF_NAME and ENUMERATION_CONSTANT are useful.
                     This option must be repeated for every lexeme of interest.
                     The output will do to STDOUT.
--progress           Progress bar with ETA information.
--check <checkName>  Perform some hardcoded checks on the code. Supported values for checkName are:
  reservedNames      Check IDENTIFIER lexemes v.s. Gnu recommended list of Reserved Names [1].
                     Any check that is not ok will print on STDERR.
--dump               Dump parse tree value. Always happen eventually as the last post-processing.
                     Will print on STDOUT.
--loglevel <level>   Log::log4perl MarpaX::Languages::C::AST level. <level> has to be something meaningful for Log::Log4perl, typically WARN, INFO, ERROR, etc. Please note that to trace Marpa library itself, ony the environment variable MARPA_TRACE can be used. So, for example, to have the maximum logging possible, you set MARPA_TRACE environment variable to a true value and --loglevel TRACE. Default value is WARN.
Examples:

Typical usages

$^X $0                   -D MYDEFINE1 -D MYDEFINE2 -I       /tmp/myIncludeDir            /tmp/myfile.c
$^X $0                   -D MYDEFINE1 -D MYDEFINE2 -I       /tmp/myIncludeDir            /tmp/myfile.c --lexeme IDENTIFIER --lexeme TYPEDEF_NAME
$^X $0 --cpp cl --cpp -E -D MYDEFINE1 -D MYDEFINE2 -I C:/Windows/myIncludeDir C:/Windows/Temp/myfile.c

Less typical usage:

$^X $0 -I libmarpa_build libmarpa_build/marpa.c --cppfile ./marpa.w  --progress --check reservedNames

[1] http://www.gnu.org/software/libc/manual/html_node/Reserved-Names.html
USAGE

    exit($rc);
}
