#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;
use Log::Log4perl qw/:easy/;
use Log::Any::Adapter;
use Log::Any qw/$log/;
#
# Init log
#
$ENV{MARPA_TRACE} = 1;
our $defaultLog4perlConf = <<DEFAULT_LOG4PERL_CONF;
log4perl.rootLogger              = TRACE, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 0
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
DEFAULT_LOG4PERL_CONF
Log::Log4perl::init(\$defaultLog4perlConf);
Log::Any::Adapter->set('Log4perl');

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
// ----------
// alloc_text
// ----------
__pragma( alloc_text( "textsection" ))
__pragma( alloc_text( "textsection", function1 ))
__pragma( alloc_text( "textsection", function1 , function2 ))

// -----------
// auto_inline
// -----------
__pragma( auto_inline())
__pragma( auto_inline(on))
__pragma( auto_inline(off))

// -------
// warning
// -------
__pragma( warning( disable : 4507 34; once : 4385; error : 164 ))
// Disable warning messages 4507 and 4034.
__pragma( warning( disable : 4507 34 ))

// Issue warning 4385 only once.
__pragma( warning( once : 4385 ))

// Report warning 4164 as an error.
__pragma( warning( error : 164 ))
// pragma_warning.cpp
// compile with: /W1
__pragma( warning(disable:4700))
void Test() {
   int x;
   int y = x;   // no C4700 here
   __pragma( warning(default:4700))   // C4700 enabled after Test ends
}

int main() {
   int x;
   int y = x;   // C4700
}
__pragma( warning( push ))
__pragma( warning( disable : 4705 ))
__pragma( warning( disable : 4706 ))
__pragma( warning( disable : 4707 ))
// Some code
__pragma( warning( pop ) )
__pragma( warning( push, 3 ))
// Declarations/definitions
__pragma( warning( pop ) )

// -------
// bss_seg
// -------
// pragma_directive_bss_seg.cpp
int i;                     // stored in .bss
#pragma bss_seg(".my_data1")
int j;                     // stored in "my_data1"

#pragma bss_seg(push, stack1, ".my_data2")   
int l;                     // stored in "my_data2"

#pragma bss_seg(pop, stack1)   // pop stack1 from stack
int m;                     // stored in "stack_data1"

int main() {
}
