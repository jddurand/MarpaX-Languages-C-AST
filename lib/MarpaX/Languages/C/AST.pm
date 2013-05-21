package MarpaX::Languages::C::AST;

use 5.006;
use strict;
use warnings FATAL => 'all';
use Log::Any qw/$log/;
use Carp qw/croak/;
use MarpaX::Languages::C::AST::Grammar;
use MarpaX::Languages::C::AST::Impl qw/DOT_COMPLETION DOT_PREDICTION/;
use MarpaX::Languages::C::AST::Scope;

=head1 NAME

MarpaX::Languages::C::AST - Translate a C source to an AST

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

This modules translates a C source into an AST tree. The AST consist of blessed objects that map exactly to the C grammar in use. If you want to enable logging, be aware that this module is a Log::Any thingy.

Please note that this module just I<translates> a C source, it does I<not> check for its correctness, i.e. the numerous grammar constraints built on top on the C grammar are not implemented, for example constraint on the number of storage class specifiers, uniqueness of labeled statements within a function, etc.. This is left to a compiler, which is not the goal here. So, to state things clearly, this module is adressing the I<ambiguities> of the grammar itself, i.e. the dangling else, the typedef/enum/identifier. And produces an AST of the parse tree value.

Example:

    use MarpaX::Languages::C::AST;
    use Log::Log4perl qw/:easy/;
    use Log::Any::Adapter;
    use Log::Any qw/$log/;
    use Data::Dumper;

    #
    # Init log
    #
    our $defaultLog4perlConf = <<DEFAULT_LOG4PERL_CONF;
log4perl.rootLogger              = WARN, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 0
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
DEFAULT_LOG4PERL_CONF
    Log::Log4perl::init(\$defaultLog4perlConf);
    Log::Any::Adapter->set('Log4perl');

    #
    # Parse C
    #
    my $cSourceCode = <<C_SOURCE_CODE;
typedef struct s1_ {int i1;} x1, y1;
struct x1 {x1 i2;};
x1 x;
C_SOURCE_CODE
    my $cAstObject = MarpaX::Languages::C::AST->new(\$cSourceCode);
    print Dumper($cAstObject->parse(\$input));

=head1 SUBROUTINES/METHODS

=head2 new

Instanciate a new object. Takes as parameter an optional base name of a grammar. Default is 'ISO-ANSI-C-2011'.

=cut

sub new {
  my ($class, $grammarName) = @_;

  $grammarName //= 'ISO-ANSI-C-2011';

  my $self  = {};
  $self->{_scope} = MarpaX::Languages::C::AST::Scope->new(),
  $self->{_grammar} = MarpaX::Languages::C::AST::Grammar->new($grammarName);
  my $grammar_option = $self->{_grammar}->grammar_option();
  my $recce_option = $self->{_grammar}->recce_option();
  $grammar_option->{bless_package} = 'C::AST';
  $grammar_option->{source} = \$self->{_grammar}->content();
  $self->{_impl} = MarpaX::Languages::C::AST::Impl->new($grammar_option, $recce_option);
  $self->{_G1LocationToTypedef} = {};
  $self->{_G1LocationToTypedefName} = {};

  bless($self, $class);

  return $self;
}

=head2 parse($referenceToSourceCodep[, $arrayOfValuesb])

Do the parsing and return the blessed value. Takes as first parameter the reference to a C source code. Takes as optional second parameter a flag saying if the return value should be an array of all values or not. If this flag is false, the module will croak if there more than one parsee tree value.

=cut

sub parse {
    my ($self, $referenceToSourceCodep, $arrayOfValuesb) = @_;

    my $max = length(${$referenceToSourceCodep});
    my $pos = $self->{_impl}->read($referenceToSourceCodep);
    do {
	$self->_doEvent($referenceToSourceCodep);
	$self->_doLexeme($referenceToSourceCodep);
    } while (($pos = $self->{_impl}->resume()) < $max);
    
    $arrayOfValuesb ||= 0;
    return($self->_value($arrayOfValuesb));
}

#
# INTERNAL METHODS
#
#######################
# _show_last_expression
#######################
sub _show_last_expression {
    my ($self) = @_;

    my ($start, $end) = $self->{_impl}->last_completed_range('translationUnit');
    return 'No expression was successfully parsed' if (! defined($start));
    my $lastExpression = $self->{_impl}->range_to_string($start, $end);
    return "Last expression successfully parsed was: $lastExpression";
}

########
# _value
########
sub _value {
    my ($self, $arrayOfValuesb) = @_;

    my @rc = ();
    my $nvalue = 0;
    my $valuep = $self->{_impl}->value() || croak $self->_show_last_expression();
    if (defined($valuep)) {
	push(@rc, $valuep);
    }
    do {
	++$nvalue;
	$valuep = $self->{_impl}->value();
	if (defined($valuep)) {
	    push(@rc, $valuep);
	}
    } while (defined($valuep));
    if ($#rc != 0 && ! $arrayOfValuesb) {
	croak 'Number of parse tree value should be 1';
    }
    if ($arrayOfValuesb) {
	return [ @rc ];
    } else {
	return $rc[0];
    }
}

##########
# _doEvent
##########
sub _doEvent {
    my ($self, $referenceToSourceCodep) = @_;

    my $iEvent = 0;
    my $g1 = undef;

    while (defined($_ = $self->{_impl}->event($iEvent++))) {
	foreach my $event (@{$_}) {
	    $log->debugf('> Event %s', $event);
	    if ($event eq 'parameterDeclaration') {
		#
		# In parameterDeclaration typedef is syntactically allowed but never valid.
		# [ANSI C 3.5.4.3, 3.7.1]
		#
		$g1 ||= $self->{_impl}->latest_g1_location;
		$self->_doGrammarConstraint($g1,
					    'TYPEDEF',
					    $referenceToSourceCodep,
					    [DOT_PREDICTION, DOT_COMPLETION, 'storageClassSpecifier' , undef ],
					    $self->{_G1LocationToTypedef},
					    [DOT_PREDICTION, $event, undef ],
					    1);
		#
		# A typedef-name cannot be entered neither, so it would be too late in the event
		# mechanism: this is enforced in the lexeme pause 'before'.
		#
		#$self->_doGrammarConstraint($g1,
		#                            'TYPEDEF_NAME',
		#				 $referenceToSourceCodep,
		#				 [DOT_PREDICTION, DOT_COMPLETION, 'typeSpecifier' , undef ],
		#				 $self->{_G1LocationToTypedefName},
		#				 [DOT_PREDICTION, $event, undef ],
		#				 1);
	    }
	    elsif ($event eq 'declarationList') {
		#
		# In functionDefinition only:
		# In declarationList typedef is syntactically allowed but never valid    
		# [ANSI C 3.7.1]
		#
		# No need to check the context because declarationList exist only in on place in all the grammar:
		# functionDefinition ::= declarationSpecifiers declarator declarationList . compoundStatement
		#
		$g1 ||= $self->{_impl}->latest_g1_location;
		$self->_doGrammarConstraint($g1,
					    'TYPEDEF',
					    $referenceToSourceCodep,
					    [DOT_PREDICTION, DOT_COMPLETION, 'storageClassSpecifier' , undef ],
					    $self->{_G1LocationToTypedef},
					    [DOT_PREDICTION, $event, undef],
					    1);
	    }
	    elsif ($event eq 'directDeclarator') {
		#
		# directDeclarator ::= IDENTIFIER .
		# can introduce names into ordinaty name space when it eventually participates
		# in the grammar rule:
		# declaration: declarationSpecifiers initDeclaratorList . SEMICOLON
		#
		$g1 ||= $self->{_impl}->latest_g1_location;
		if ($self->{_impl}->findInProgress($g1, DOT_COMPLETION, 'directDeclarator', ['IDENTIFIER']) &&
		    $self->{_impl}->findInProgress($g1,              2, 'declaration',      ['declarationSpecifiers', 'initDeclaratorList', 'SEMICOLON'])) {
		    #
		    ## In structDeclarator ordinaty name space names cannot be defined
		    #
		    my $directDeclarator = $self->{_impl}->substring($self->{_impl}->last_completed('directDeclarator'));
		    if ($self->{_impl}->findInProgress($g1, 1, 'structDeclarator', [ 'declarator', 'COLON', 'constantExpression' ]) ||
			$self->{_impl}->findInProgress($g1, 1, 'structDeclarator', [ 'declarator' ])) {
			$log->debugf('> Declaration of IDENTIFIER "%s" in structDeclarator context: parse symbol inactive', $directDeclarator);
		    } else {
			my $directDeclarator = $self->{_impl}->substring($self->{_impl}->last_completed('directDeclarator'));
			$log->debugf('> Declaration of IDENTIFIER "%s" that can introduce name in name-space', $directDeclarator);
			if ($self->_doGrammarConstraint($g1,
							'TYPEDEF',
							$referenceToSourceCodep,
							[DOT_PREDICTION, DOT_COMPLETION, 'storageClassSpecifier' , undef ],
							$self->{_G1LocationToTypedef},
							[DOT_PREDICTION, 'functionDefinition', undef ],
							0)) {
			    $self->{_scope}->parseEnterTypedef($directDeclarator);
			} else {
			    $self->{_scope}->parseObscureTypedef($directDeclarator);
			}
		    }
		}
	    } elsif ($event eq 'enumerationConstant') {
		#
		# Enum is not scope dependend - from now on it obscures any use of its
		# identifier in any scope
		#
		my $enumerationConstant = $self->{_impl}->substring($self->{_impl}->last_completed('enumerationConstant'));
		$self->{_scope}->parseEnterEnum($enumerationConstant);
	    } elsif ($event eq 'primaryExpression') {
		#
		## Anything special to do ?
		#
	    }
	}
    }
}

###########
# _doLexeme
###########
sub _doLexeme {
    my ($self, $referenceToSourceCodep) = @_;

    my $lexeme = $self->{_impl}->pause_lexeme();

    if (! defined($lexeme)) {
	return;
    }

    my $g1 = undef;

    $log->debugf('> Lexeme %s', $lexeme);

    #
    # Ambiguity managenent: 'before' paused lexemes
    #
    if (grep {$lexeme eq $_} qw/IDENTIFIER TYPEDEF_NAME ENUMERATION_CONSTANT/) {
	my ($lexeme_start, $lexeme_length) = $self->{_impl}->pause_span();
	my $lexeme_value = substr(${$referenceToSourceCodep}, $lexeme_start, $lexeme_length);
	$g1 ||= $self->{_impl}->latest_g1_location;
	if ($self->{_impl}->findInProgress($g1, DOT_PREDICTION, 'typeSpecifier', [ 'TYPEDEF_NAME' ]) && $self->{_scope}->parseIsTypedef($lexeme_value) && $self->_canEnterTypedefName($g1)) {
	    $lexeme = 'TYPEDEF_NAME';
	} elsif ($self->{_impl}->findInProgress($g1, DOT_PREDICTION, 'constant', [ 'ENUMERATION_CONSTANT' ]) && $self->{_scope}->parseIsEnum($lexeme_value) && $self->_canEnterEnumerationConstant($g1)) {
	    $lexeme = 'ENUMERATION_CONSTANT';
	} else {
	    $lexeme = 'IDENTIFIER';
	}
	#
	# Push the unambiguated lexeme
	#
	if (! defined($self->{_impl}->lexeme_read($lexeme, $lexeme_start, $lexeme_length, $lexeme_value))) {
	    my ($line, $column) = $self->{_impl}->line_column($lexeme_start);
	    my $msg = sprintf('Error at line %d, column %d: "%s" cannot be associated to lexeme %s', $line, $column, $lexeme_value, $lexeme);
	    $log->fatalf($msg);
	    croak $msg;
	}
	if ($lexeme eq 'TYPEDEF_NAME') {
	    $log->infof('%s detected at G1 location %d', $lexeme, $g1);
	    $self->{_G1LocationToTypedefName}->{$g1} = 1;
	}
	#
	# A lexeme_read() can generate an event
	#
	$self->_doEvent($referenceToSourceCodep);
    }
    #
    # Scope management: Associated with file-scope, function body, compound statement, or prototype
    # - function body matches compound statement
    # - file-scope is implicit here, we treat one file at a time
    #
    elsif ($lexeme eq 'LPAREN_SCOPE') {
	$self->{_scope}->parseEnterScope();
    } elsif ($lexeme eq 'LCURLY_SCOPE') {
	$g1 ||= $self->{_impl}->latest_g1_location;
	if ($self->_canReenterScope($g1)) {
	    #
	    # We know now that we are in the functionDefinition beginning of body
	    # so this is a place where we can put this check unambiguously:
	    #
	    # In functionDefinion typedef is syntactically allowed but never valid in declarationSpecifiers
	    # [ANSI C 3.7.1]
	    #
	    $self->_doGrammarConstraint($g1,
					'TYPEDEF',
					$referenceToSourceCodep,
					[DOT_PREDICTION, DOT_COMPLETION, 'initDeclarator' , undef ],
					$self->{_G1LocationToTypedef},
					[DOT_PREDICTION, 'declarationSpecifiers', undef ],
					1);
	    $self->{_scope}->parseReenterScope();
	} else {
	    $self->{_scope}->parseEnterScope();
	}
    } elsif ($lexeme eq 'RPAREN_SCOPE') {
	$self->{_scope}->parseExitScope();
    } elsif ($lexeme eq 'RCURLY_SCOPE') {
	$self->{_scope}->parseExitScope();
    }
    #
    # Track of TYPEDEF lexeme per G1 location
    #
    elsif ($lexeme eq 'TYPEDEF') {
	$g1 ||= $self->{_impl}->latest_g1_location;
	$log->infof('%s detected at G1 location %d', $lexeme, $g1);
	$self->{_G1LocationToTypedef}->{$g1} = 1;
    }
}

######################
# _doGrammarConstraint
######################
sub _doGrammarConstraint {
    my ($self, $g1, $what, $referenceToSourceCodep, $candidateRulep, $matchesInG1p, $endConditionp, $fatal_mode) = @_;

    $fatal_mode ||= 0;
    my ($start_g1_location, $end_g1_location);
    my $rc = $self->{_impl}->inspectG1($what, $g1, \$start_g1_location, \$end_g1_location, [ $candidateRulep ], $matchesInG1p, [ $endConditionp ]);
    if (defined($rc) && $rc) {
	#
	# Match
	#
	my @args = ( $what );
	my ($start_g1_location_g0_start, $start_g1_location_g1_g0_length) = $self->{_impl}->g1_location_to_span($start_g1_location);
	if ($start_g1_location < $end_g1_location) {
	    my ($end_g1_location_g0_start, $end_g1_location_g1_g0_length) = $self->{_impl}->g1_location_to_span($end_g1_location);
	    push(@args, substr(${$referenceToSourceCodep}, $start_g1_location_g0_start, ($end_g1_location_g0_start - $start_g1_location_g0_start) + $end_g1_location_g1_g0_length));
	} else {
	    push(@args, substr(${$referenceToSourceCodep}, $start_g1_location_g0_start, $start_g1_location_g1_g0_length));
	}
	if ($fatal_mode) {
	    my $msg = sprintf('%s is not allowed in "%s"', @args);
	    $log->fatalf($msg);
	    croak $msg;
	} else {
	    $log->debugf('%s found in "%s"', @args);
	}
	$rc = 1;
    } else {
	$rc = 0
    }
    return($rc);
}

######################
# _canEnterTypedefName
######################
sub _canEnterTypedefName {
    my ($self, $g1) = @_;

    my $rc = 1;
    if ($self->{_impl}->findInProgress($g1, 1, 'parameterDeclaration', ['declarationSpecifiers', 'declarator'])) {
	#
	# In parameterDeclaration a typedef-name cannot be entered.
	#
	$log->debugf('A parameterDeclaration cannot enter a TYPEDEF_NAME');
	$rc = 0;
    }
    return($rc);
}

##############################
# _canEnterEnumerationConstant
##############################
sub _canEnterEnumerationConstant {
    my ($self, $g1) = @_;

    my $rc = 1;
    return($rc);
}

##################
# _canReenterScope
##################
sub _canReenterScope {
    my ($self, $g1) = @_;

    return ($self->{_impl}->findInProgress($g1 - 1, 3, 'functionDefinition', [ 'declarationSpecifiers', 'declarator', 'declarationList', 'compoundStatement' ]) ||
	    $self->{_impl}->findInProgress($g1 - 1, 2, 'functionDefinition', [ 'declarationSpecifiers', 'declarator', 'compoundStatement' ]));
}

=head1 SEE ALSO

L<Log::Any>, L<MarpaX::Languages::C::AST::Grammar>, L<MarpaX::Languages::C::AST::Impl>, L<MarpaX::Languages::C::AST::Scope>

=head1 AUTHOR

Jean-Damien Durand, C<< <jeandamiendurand at free.fr> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-marpax-language-c-ast at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=MarpaX-Languages-C-AST>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc MarpaX::Languages::C::AST


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=MarpaX-Languages-C-AST>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/MarpaX-Languages-C-AST>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/MarpaX-Languages-C-AST>

=item * Search CPAN

L<http://search.cpan.org/dist/MarpaX-Languages-C-AST/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2013 Jean-Damien Durand.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut

1; # End of MarpaX::Languages::C::AST
