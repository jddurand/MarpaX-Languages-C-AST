#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;

BEGIN {
    push(@INC, 'inc');
    use_ok( 'MarpaX::Languages::C::AST' ) || print "Bail out!\n";
}
use MarpaX::Languages::C::AST::Expected;

my $cSourceCode = do { local $/; <DATA> };
my $cAst = MarpaX::Languages::C::AST->new();
my $valuep = $cAst->parse(\$cSourceCode);
ok(defined($valuep), 'Output from parse() is ok');

__DATA__
__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;
__extension__ struct G {
  struct { char z; };
  char g;
};
static __inline __attribute__((__always_inline__))
_mm_set_sd (double __F)
{
  return __extension__ 0;
}
