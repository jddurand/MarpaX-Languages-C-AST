#!env perl
use strict;
use warnings FATAL => 'all';
use MarpaX::Languages::C::AST;
use Log::Log4perl qw/:easy/;
use Log::Any::Adapter;
use Log::Any qw/$log/;
use Data::Dumper;
#
# Init log
#
our $defaultLog4perlConf = <<DEFAULT_LOG4PERL_CONF;
log4perl.rootLogger              = DEBUG, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 1
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
DEFAULT_LOG4PERL_CONF
Log::Log4perl::init(\$defaultLog4perlConf);
Log::Any::Adapter->set('Log4perl');
#
# Parse C
#
my $cSourceCode = do { local $/; <DATA> };
$log->infof("Source code:\n%s", $cSourceCode);
MarpaX::Languages::C::AST->new()->parse(\$cSourceCode);
__DATA__
void func(x ,y) int x; typedef y; {}
