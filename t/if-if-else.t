#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST' ) || print "Bail out!\n";
}

my $cSourceCode = do { local $/; <DATA> };
my $cAst = MarpaX::Languages::C::AST->new();
my $blessed = $cAst->parse(\$cSourceCode);
ok(defined($blessed));
use Data::Dumper;
print Dumper($blessed);
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
