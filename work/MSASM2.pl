#!perl
use strict;
use diagnostics;
use Marpa::R2;
use File::Slurp qw/read_file/;
use IO::Handle;

autoflush STDOUT 1;

my $grammar_source = do { local $/; <DATA> };
my $grammar = Marpa::R2::Scanless::G->new({ source => \$grammar_source});
my $input = read_file(shift);
my $re = Marpa::R2::Scanless::R->new( { grammar => $grammar, trace_terminals => 1 } );
my $length = length $input;
eval {
    for ( my $pos = $re->read(\$input); $pos < $length; $pos = $re->resume()) {
	foreach (@{$re->events()}) {
	    my ($name) = @{$_};
	    if ($name eq '^COMMENT_INTERIOR') {
		#
		# Get delimiter character
		#
		my $origPos = $pos;
		my $delimiter = $re->literal($origPos, 1);
		my $text = $delimiter;
		++$pos;
		while ($pos < $length) {
		    my $c = substr($input, $pos++, 1);
		    $text .= $c;
		    if ($c eq $delimiter) {
			last;
		    }
		}
		#
		# Eat the remaining up to newline
		#
		while ($pos < $length) {
		    my $c = substr($input, $pos, 1);
		    if (ord($c) eq oct(12)) {
			last;
		    }
		    ++$pos;
		    $text .= $c;
		}
		#
		# Inject COMMENT_INTERIOR lexeme
		#
		my $l = length($text);
		print STDERR "==> lexeme_read('COMMENT_INTERIOR', $origPos, $l, \"$text\")\n";
		$re->lexeme_read('COMMENT_INTERIOR', $origPos, $l, $text);
		print STDERR "==> Resume at $pos: <" . (substr($input, $pos, 10) . '...') . ">\n";
	    }
	}
    }
};
if ($@) {
    print $@;
    my $progress_report = $re->show_progress();
    print $progress_report;
    my $terminals_expected = $re->terminals_expected();
    print "Terminals expected:\n" . join(', ', @{$terminals_expected}) . "\n";
    exit;
    print "G0 Symbols:\n" . $grammar->show_symbols(3, 'G0');
    print "G1 Symbols:\n" . $grammar->show_symbols(3, 'G1');
    print "G0 Rules:\n" . $grammar->show_rules(3, 'G0');
    print "G1 Rules:\n" . $grammar->show_rules(3, 'G1');
}

__DATA__
#
# References:
#
# Microsoft Macro Assembler Reference
# -----------------------------------
# http://msdn.microsoft.com/fr-fr/library/afzk3475.aspx

# The Art of ASSEMBLY LANGUAGE PROGRAMMING
# ----------------------------------------
# http://www.phatcode.net/res/223/files/html/toc.html

#
# Notes & comments in the followings are sometimes direct cut/paste
# from these two sites
#

:start        ::= directives

directives    ::= directive+

directive     ::= equalDirective        (EOD)
              |   dot386Directive       (EOD)
              |   dot386PDirective      (EOD)
              |   dot387Directive       (EOD)
              |   dot486Directive       (EOD)
              |   dot486PDirective      (EOD)
              |   dot586Directive       (EOD)
              |   dot586PDirective      (EOD)
              |   dot686Directive       (EOD)
              |   dot686PDirective      (EOD)
              |   aliasDirective        (EOD)
              |   alignDirective        (EOD)
              |   allocstackDirective   (EOD)
              |   alphaDirective        (EOD)
              |   assumeDirective       (EOD)
              |   breakDirective        (EOD)
              |   commentDirective      (EOD)
              |                         (EOD)   # Empty directive

#instruction   ::= label COLON mnenomic operands 
#              |   label COLON mnenomic
#              |               mnenomic operands 
#              |               mnenomic

##############
# DIRECTIVES #
##############
equalDirective            ::= name EQUAL expression
dot386Directive           ::= DOT386
dot386PDirective          ::= DOT386P
dot387Directive           ::= DOT387
dot486Directive           ::= DOT486
dot486PDirective          ::= DOT486P
dot586Directive           ::= DOT586
dot586PDirective          ::= DOT586P
dot686Directive           ::= DOT686
dot686PDirective          ::= DOT686P
aliasDirective            ::= ALIAS LANGLE alias RANGLE EQUAL LANGLE actualName RANGLE
alias                     ::= ID
actualName                ::= ID
alignDirective            ::= ALIGN
                          |   ALIGN number       # Post-AST role: number is a power of 2
number                    ::= INTEGERCONSTANT
allocstackDirective       ::= DOTALLOCSTACK size
size                      ::= INTEGERCONSTANT     # Post-AST role: size is a power of 8
alphaDirective            ::= ALPHA
assumeDirective           ::= ASSUME assumeSegregisterList
                          |   ASSUME assumeDataregisterList
                          |   ASSUME assumeRegisterErrorList
                          |   ASSUME assumeRegisterNothingList
assumeSegregisterList     ::= segregister COLON name
                          |   assumeSegregisterList COMMA segregister COLON name
assumeDataregisterList    ::= dataregister COLON type
                          |   assumeDataregisterList COMMA dataregister COLON type
assumeRegisterErrorList   ::= register COLON ERROR
                          |   assumeRegisterErrorList COMMA register COLON ERROR
assumeRegisterNothingList ::= register COLON NOTHING
                          |                  NOTHING
                          |   registerNothingList COMMA register COLON NOTHING
segregister               ::= SEGREGISTER
register                  ::= REGISTER
commentDirective          ::= COMMENT COMMENT_INTERIOR # Special rule handled in user space

####################
# INTERNAL LEXEMES #
####################
_ID                ~ [A-Za-z_@?$]
                   | _ID [A-Za-z_@?$0-9]
_DECDIGIT          ~ [0-9]
_HEXDIGIT          ~ [0-9a-fA-F]
_BINDIGIT          ~ [0-1]
#_DECDIGIT_ANY      ~ _DECDIGIT*
_DECDIGIT_MANY     ~ _DECDIGIT+
_HEXDIGIT_ANY      ~ _HEXDIGIT*
_BINDIGIT_ANY      ~ _BINDIGIT*
_HEXNUMBER         ~ _DECDIGIT _HEXDIGIT_ANY [hH]
_ENCODEDREALNUMBER ~ _DECDIGIT _HEXDIGIT_ANY [rR]
_BINNUMBER         ~ _DECDIGIT _BINDIGIT_ANY [bB]
_DECNUMBER         ~ _DECDIGIT_MANY |  _DECNUMBER [tdTD]
#_SIGN              ~ [-+]

_INTEGERCONSTANT   ~ _HEXNUMBER
                   | _ENCODEDREALNUMBER
                   | _BINNUMBER
                   | _DECNUMBER

#_EXPONENT          ~  'E':i _DECDIGIT_MANY
#                   |  'E':i _SIGN _DECDIGIT

#_REALNUMBERLCONSTANT ~ _SIGN _DECDIGIT_MANY '.'
#                    | _SIGN _DECDIGIT_MANY '.' _DECDIGIT_ANY _EXPONENT
#                    | _SIGN _DECDIGIT_MANY '.' _DECDIGIT_ANY
#                    | _SIGN _DECDIGIT_MANY '.' _EXPONENT

#_SQUOTE             ~ [']
#_DQUOTE             ~ '"'

#_CHARACTERCONSTANT_SQUOTE_INTERIOR     ~ [^']
#_CHARACTERCONSTANT_SQUOTE_INTERIOR_ANY ~ _CHARACTERCONSTANT_SQUOTE_INTERIOR*
#_CHARACTERCONSTANT_DQUOTE_INTERIOR     ~ [^"]
#_CHARACTERCONSTANT_DQUOTE_INTERIOR_ANY ~ _CHARACTERCONSTANT_DQUOTE_INTERIOR*

#_CHARACTERCONSTANT ~ _SQUOTE _CHARACTERCONSTANT_SQUOTE_INTERIOR _SQUOTE
#                   | _DQUOTE _CHARACTERCONSTANT_DQUOTE_INTERIOR _DQUOTE

#_STRINGCONSTANT ~ _SQUOTE _CHARACTERCONSTANT_SQUOTE_INTERIOR_ANY _SQUOTE
#                | _DQUOTE _CHARACTERCONSTANT_DQUOTE_INTERIOR_ANY _DQUOTE

_SEGREGISTER           ~ 'CS':i
                       | 'DS':i
                       | 'ES':i
                       | 'FS':i
                       | 'GS':i
                       | 'SS':i
_SPECIALREGISTER       ~ 'CR0':i
                       | 'CR2':i
                       | 'CR3':i
                       | 'DR0':i
                       | 'DR1':i
                       | 'DR2':i
                       | 'DR3':i
                       | 'DR6':i
                       | 'DR7':i
                       | 'TR3':i
                       | 'TR4':i
                       | 'TR5':i
                       | 'TR6':i
                       | 'TR7':i
_GPREGISTER            ~ 'AX':i
                       | 'EAX':i
                       | 'BX':i
                       | 'EBX':i
                       | 'CX':i
                       | 'ECX':i
                       | 'DX':i
                       | 'EDX':i
                       | 'BP':i
                       | 'EBP':i
                       | 'SP':i
                       | 'ESP':i
                       | 'DI':i
                       | 'EDI':i
                       | 'SI':i
                       | 'ESI':i
_BYTEREGISTER          ~ 'AL':i
                       | 'AH':i
                       | 'BL':i
                       | 'BH':i
                       | 'CL':i
                       | 'CH':i
                       | 'DL':i
                       | 'DH':i
_EOL                   ~ [\012]
_COMMENT_START         ~ ';'
_COMMENT_INTERIOR      ~ [^\012]*
_COMMENT               ~ _COMMENT_START _COMMENT_INTERIOR
_WS                    ~ [\010\011\013-\015\032\040]
_WS_MANY               ~ _WS*
_DELIMITER             ~ [^\010-\013-\015\032\040]

####################
# Exported lexemes #
####################
EQUAL                 ~ '='
DOT386                ~ '.386'
DOT386P               ~ '.386P':i
DOT387                ~ '.387'
DOT486                ~ '.486'
DOT486P               ~ '.486P':i
DOT586                ~ '.586'
DOT586P               ~ '.586P':i
DOT686                ~ '.686'
DOT686P               ~ '.686P':i
ALIAS                 ~ 'ALIAS':i
LANGLE                ~ '<'
RANGLE                ~ '>'
ID                    ~ _ID
ALIGN                 ~ 'ALIGN':i
ALPHA                 ~ 'ALPHA':i
ASSUME                ~ 'ASSUME':i
COLON                 ~ ':'
ERROR                 ~ 'ERROR':i
NOTHING               ~ 'NOTHING':i
COMMENT               ~ 'COMMENT':i
:lexeme ~ COMMENT_INTERIOR pause => before event => '^COMMENT_INTERIOR'   # Handled in user's space
COMMENT_INTERIOR      ~ _DELIMITER

INTEGERCONSTANT       ~ _INTEGERCONSTANT
#SIGNEDINTEGERCONSTANT ~ _SIGN _INTEGERCONSTANT
#REALNUMBERCONSTANT    ~ _REALNUMBERCONSTANT
#CHARACTERCONSTANT     ~ _CHARACTERCONSTANT
#STRINGCONSTANT        ~ _STRINGCONSTANT
SEGREGISTER           ~ _SEGREGISTER
REGISTER              ~ _SPECIALREGISTER
                      | _GPREGISTER
                      | _BYTEREGISTER

# End of directive: this is EOL or a comment
EOD                   ~ _EOL
                      | _COMMENT

#########################
# Discarded: whitespace #
#########################
:discard              ~ _WS_MANY
