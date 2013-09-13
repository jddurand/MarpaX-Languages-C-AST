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
__pragma( warning( disable : 4507 34 ))
__pragma( warning( once : 4385 ))
__pragma( warning( error : 164 ))
__pragma( warning(disable:4700))
__pragma( warning( push ))
__pragma( warning( disable : 4705 ))
__pragma( warning( disable : 4706 ))
__pragma( warning( disable : 4707 ))
__pragma( warning( pop ) )
__pragma( warning( push, 3 ))
__pragma( warning( pop ) )

// -------
// bss_seg
// -------
__pragma( bss_seg(".my_data1") )
__pragma( bss_seg(push, stack1, ".my_data2")   )
__pragma( bss_seg(pop, stack1) )

// -------
// code_seg
// -------
__pragma( code_seg(".my_data1") )
__pragma( code_seg(push, stack1, ".my_data2")   )
__pragma( code_seg(pop, stack1) )

// ---------
// const_seg
// ---------
__pragma( const_seg(".my_data1") )
__pragma( const_seg(push, stack1, ".my_data2")   )
__pragma( const_seg(pop, stack1) )

// --------
// data_seg
// --------
__pragma( data_seg(".my_data1") )
__pragma( data_seg(push, stack1, ".my_data2")   )
__pragma( data_seg(pop, stack1) )

// -----------
// check_stack
// -----------
__pragma( check_stack( ))
__pragma( check_stack (on))
__pragma( check_stack (off))
__pragma( check_stack (+))
__pragma( check_stack (-))

// -------
// comment
// -------
__pragma( comment(linker, "/include:__mySymbol"))
__pragma( comment( lib, "emapi" ) )
__pragma( comment( compiler ))
__pragma( comment( user, "Compiled on " "Some date" " at " "Some hour" ) )

// ---------
// component
// ---------
__pragma( component(browser, off))
__pragma( component(browser, off, references))
__pragma( component(browser, off, references, DWORD))
__pragma( component(browser, on, references, DWORD))
__pragma( component(browser, off, references, "NULL"))
__pragma( component( minrebuild, on ) )
__pragma( component( minrebuild, off ) )
__pragma( component( mintypeinfo, on ) )
__pragma( component( mintypeinfo, off ) )

// -------
// conform
// -------
__pragma( conform(forScope, show))
__pragma( conform(forScope, push, x, on))
__pragma( conform(forScope, push, x1, off))
__pragma( conform(forScope, push, x2, off))
__pragma( conform(forScope, push, x3, off))
__pragma( conform(forScope, show))
__pragma( conform(forScope, pop, x1))
__pragma( conform(forScope, show))

// ----------
// deprecated
// ----------
__pragma( deprecated(func1))
__pragma( deprecated(func1, func2))

// ---------------
// detect_mismatch
// ---------------
__pragma( detect_mismatch("myLib_version", "9"))

// -----------
// fenv_access
// -----------
__pragma( fenv_access)
__pragma( fenv_access(  ))
__pragma( fenv_access(on))
__pragma( fenv_access(ON))
__pragma( fenv_access(off))
__pragma( fenv_access(OFF))

// -------------
// float_control
// -------------
__pragma( float_control(except, off) )
__pragma( float_control(except, off push) )
__pragma( float_control(precise, off) )
__pragma( float_control(precise, off push) )
__pragma( float_control(precise, on) )
__pragma( float_control(precise, on push) )
__pragma( float_control(except, on) )
__pragma( float_control(except, on push) )
__pragma( float_control(push) )
__pragma( float_control(pop) )
