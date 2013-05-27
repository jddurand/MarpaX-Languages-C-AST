#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 8;

BEGIN {
    use_ok( 'MarpaX::Languages::C::AST::Util', qw/whoami whowasi traceAndUnpack/ ) || print "Bail out!\n";
}

callThem();

sub callThem {
    my $whoami = whoami();
    ok($whoami eq __PACKAGE__ . '::callThem');
    my $this = 5;
    callIt(0, '1', [2], {3 => 4}, \$this);
}

sub callIt {
    my $whowasi = whowasi();
    ok($whowasi eq __PACKAGE__ . '::callThem');
    my $hashp = traceAndUnpack(['var1', 'var2', 'arrayp', 'hashp', 'scalarp'], @_);
    ok($hashp->{var1} == 0);
    ok($hashp->{var2} eq '1');
    is_deeply($hashp->{arrayp}, [2], 'array reference');
    is_deeply($hashp->{hashp}, {3 => 4}, 'hash reference');
    is_deeply($hashp->{scalarp}, \5, 'scalar reference');
}
