#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 3;

BEGIN {
    push(@INC, 'inc');
    use_ok( 'MarpaX::Languages::C::AST' ) || print "Bail out!\n";
}
use Expected;

my $cSourceCode = do { local $/; <DATA> };
my $cAst = MarpaX::Languages::C::AST->new();
my $valuep = $cAst->parse(\$cSourceCode);
ok(defined($valuep));
is_deeply($valuep, Expected->value($valuep), 'Expected blessed structure');

__DATA__
void function() {
	if (1)
		if (2)
			if (3)
				something3();
			else
				somethingElse3();
		else
			somethingElse2();
	else
		somethingElse1();
}
