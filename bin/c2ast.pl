#!env perl
use strict;
use warnings FATAL => 'all';
use diagnostics;
use MarpaX::Languages::C::AST;
use Getopt::Long;
use Config;
use IPC::Run qw/run/;
use Term::ProgressBar;

my $help = 0;
my $cpp = 'cpp';
my @D = ();
my @I = ();
my @U = ();

GetOptions ("help!" => \$help,
	    "cpp=s" => \$cpp,
	    "D=s" => \@D,
	    "I=s" => \@I,
	    "U=s" => \@U);

#
# Run the preprocessor
#
my @cmd = ($cpp, (map {"-D$_"} @D), (map {"-I$_"} @I), (map {"-U$_"} @U), @ARGV);
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
my $cAstObject = MarpaX::Languages::C::AST->new(lexemeCallback => [ \&lexemeCallback ]);
my $bless = $cAstObject->parse(\$preprocessedOutput);

sub lexemeCallback {
    my $lexemeHashp = shift;

    my $line = $lexemeHashp->{line};
    if ($line >= $next_update) {
	$next_update = $progress->update($line);
    }
}
