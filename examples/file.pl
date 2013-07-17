#!env perl
use strict;
use warnings FATAL => 'all';
use MarpaX::Languages::C::AST;

#
# Parse C
#
my $file = shift || die "Usage: $^X $0 file\n";
open(FILE, '<', $file) || die "Cannot open $file, $!\n";
my $cSourceCode = do { local $/; <FILE> };
close(FILE) || warn "Cannot close $file, $!\n";

my $cAstObject = MarpaX::Languages::C::AST->new();
print Dumper($cAstObject->parse(\$cSourceCode));
