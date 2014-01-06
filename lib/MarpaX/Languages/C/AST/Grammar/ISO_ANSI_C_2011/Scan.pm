use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011::Scan;

# ABSTRACT: Scan C source

use MarpaX::Languages::C::AST;
use MarpaX::Languages::C::AST::Util::Data::Find;
use Config;
use Carp qw/croak/;
use IPC::Cmd qw/run/;
use File::Temp qw/tempfile/;
use IO::File;
use Scalar::Util qw/blessed reftype/;
use Regexp::Common;
use constant {
    LEXEME_POSITION_INDEX => 0,
    LEXEME_LENGTH_INDEX => 1,
    LEXEME_VALUE_INDEX => 2
};
    
our $RESAMELINE = qr/(?:[ \t\v\f])*/;                        # i.e. WS* without \n
our $REDEFINE = qr/^${RESAMELINE}#${RESAMELINE}define${RESAMELINE}(\w+(?>[^\n\\]*)(?>\\.[^\n\\]*)*)/ms; # dot-matches-all mode, keeping ^ meaningful
our $BALANCEDPARENS = qr/$RE{balanced}{-parens=>'()'}{-keep}/;

#
# Mapping hash keys <=> array indices
#
our %KEY2ID = (
    rt             =>  0,
    nm             =>  1,
    args           =>  2,
    ft             =>  3,
    mod            =>  4,
    ty             =>  5,
    extern         =>  6,
    static         =>  7,
    typedef        =>  8,
    init           =>  9,
    func           => 10,
    struct         => 12,
    union          => 13,
    structOrUnion  => 14,
    type           => 15,
    var            => 16,
    _MAX           => 17,           # Internal usage only
    _startPosition => 90,           # Internal usage only
);

our @PURGE_KEYS = sort {$KEY2ID{$a} <=> $KEY2ID{$b}} grep {$KEY2ID{$_} >= $KEY2ID{_MAX}} keys %KEY2ID;
our $PURGE_IDX  = $KEY2ID{$PURGE_KEYS[0]};

# VERSION

=head1 DESCRIPTION

This module scans a C source and exposes methods compatible with C::Scan module.

=head1 SYNOPSIS

    use strict;
    use warnings FATAL => 'all';
    use MarpaX::Languages::C::Scan;
    use Config;
    use Data::Dumper;
    #
    # Parse C
    #
    my $filename = 'mysource.c';
    my %config = (cpprun => $Config{cpprun},
                  cppflags => $Config{cppflags});
    my $c = MarpaX::Languages::C::Scan->new(filename => $filename, %config);
    print Dumper($c->get('parsed_fdecls'));
    print Dumper($c->parsed_fdecls);

=head1 SUBROUTINES/METHODS

=head2 new($class, %options)

Instantiate a new object. Parameters are are in a hash with the following keys:

=over

=item filename

File name to parse

=item content

Content to parse

=item filename_filter

Filter on filename from cpp output

=item asHash

Use hash references for parsed information instead of array references

=item cpprun

Preprocessor command

=item cppflags

Preprocessor flags

=item enumType

Default enum type. This is compiler dependant but is usually the default value: 'int'.

=back

Please refer to the Config perl documentation for the meaning of these flags. Is not specified, all these keys will have the default perl's values. This module will execute "$cpprun $cppflags $filename", using a temporary filename if $content was given.

A working precompiler is required.

$filename and $content are mutually exclusive. If $content is used a temporary file will be created using File::Temp (which may fail under taint mode -;). The $filename_filter value limits the output to file names equal to $filename_filter (if this is a SCALAR) or matching $filename_filter (if this is a Regexp): since we use the precompiler, any #include statements is "polluting" the original source, i.e. much more files that just $filename (or $content) are used. Default value is $filename or the generated temporary filename when using $content mode.

If $asHash is a true value, then parsed declarations will be an array reference of hashes, having the key/value information listed below. If $asHash is a false value, the items below are ordered by indice. Default $asHash is a false value.

=over

=item rt

A String: return type of a function.

=item nm

A String: name (i.e. identifier)

=item args

Array reference of undef of arguments parsed declarations.

=item ft

A string: full text used to get the information of current hash. Please note that this may include more text than needed (for example with a declaration of multiple variables separated by commas: the full text will include any previous declared variable). Modulo eventual discarded lexemes from MarpaX::Languages::C::AST (for example: gcc __attribute__'s).

=item mod

A string: array modifiers if any (for example: char x[2] will make mod to be: '[2]').

=item ty

A string: type of a declarator. In case of a function, the type will contain only eventual stars '*'.

=item extern

A flag: true value means this is an 'extern' declaration.

=item typedef

A flag: true value means this is an 'typedef' declaration.

=item init

A string: declarator initialization, if any. For example, with char *x = "value" init will be the string "value".

=item func

A flag: true value means this is an function declaration.

=item struct

A flag: true value means this is a struct declaration.

=item union

A flag: true value means this is a union declaration.

=item structOrUnion

A flag: true value means this is a struct or union declaration. If true, it is guaranteed that one of 'struct' or 'union' flag is true.

=item type

A flag: true value means this is a type declaration. If true, it is guaranteed that one of 'typedef' or 'structOrUnion' flag is true, and that the 'var' flag (see below) is false.

=item var

A flag: true value means this is a variable declaration. If true, it is guaranteed that the 'type' flag is false.

=back

The methods defines_args() and defines_no_args() are not subject to the filename_filter parameter, they always apply on the content or filename given /before/ the preprocessing. They are based on heuristic parsing, i.e. their result should not be blindly trusted. A typical example of false positive is a macro inside string or a comment.

Some method have an optional argument:

=over

=item $level

Default C::Scan behaviour is to not give information for inner scopes more than once, corresponding to the default value $level of 1. This correspond to number of inner 'args' array references (c.f. args element described below): if the number of inner 'args' is bigger than $level, then 'args' is forced to undef. For any method that claim to be C::Scan compatible, default $level parameter is 1. You must give a negative value (e.g. -1) for an output with no limit on the number of scopes.

=back

Finally, This module will croak on any error.

=cut

# ----------------------------------------------------------------------------------------

sub new {
  my ($class, %opts) = @_;

  if (exists($opts{filename}) && exists($opts{content})) {
    croak 'filename and content are mutually exclusive';
  }
  if (! exists($opts{filename}) && ! exists($opts{content})) {
    croak 'filename or content is required';
  }

  my $self = {
              _cpprun          => exists($opts{cpprun})            ? $opts{cpprun}              : $Config{cpprun},
              _cppflags        => exists($opts{cppflags})          ? $opts{cppflags}            : $Config{cppflags},
              _filename_filter => exists($opts{filename_filter}  ) ? $opts{filename_filter}     : undef,
              _asHash          => exists($opts{asHash}           ) ? $opts{asHash}              : 0,
              _enumType        => exists($opts{enumType}         ) ? $opts{enumType}            : 'int',
             };

  #
  # For anonymous enums or structs, so that their names do not clash
  #
  $self->{_anonCount} = 0;

  my $local_filename = '';
  my $local_fh = undef;
  if (exists($opts{content})) {
    if (! defined($opts{content})) {
      croak 'Undefined content';
    }
    $self->{_content2fh} = File::Temp->new(UNLINK => 1, SUFFIX => '.c');
    $self->{_filename} = File::Spec->canonpath($self->{_content2fh}->filename);
    #
    # We open twice the temporary file to make sure it is not deleted
    # physically on disk and still visible for our process
    #
    $self->{_tmpfh} = IO::File->new($self->{_filename}, 'r') || croak "Cannot open $self->{_filename}, $!";
    print($self->{_content2fh}, $opts{content});
    close($self->{_content2fh}) || warn "Cannot close $self->{_content2fh}, $!";
    $self->{_content} = $opts{content};
  } else {
    if (! defined($opts{filename})) {
      if ($local_filename) {
        unlink($local_filename);
      }
      croak 'Undefined filename';
    }
    $self->{_tmpfh} = IO::File->new($opts{filename}, 'r') || croak "Cannot open $opts{filename}, $!";
    $self->{_filename} = File::Spec->canonpath($opts{filename});
  }

  if (defined($self->{_filename_filter})) {
      my $ref = reftype($self->{_filename_filter}) || '';
      if ($ref) {
	  if ($ref ne 'REGEXP') {
	      croak 'filename_filter must be a scalar or a regular expression';
	  } else {
	      #
	      # For efficiency, instead of doing ref() or reftype() all the time, we will do exists()
	      #
	      $self->{_filename_filter_re} = $self->{_filename_filter};
	  }
      } else {
	  $self->{_filename_filter} = File::Spec->canonpath($self->{_filename_filter});
      }
  } else {
      $self->{_filename_filter} = $self->{_filename};
  }

  bless($self, $class);

  $self->_init();

  #
  # This will unlink temporary file
  #
  delete($self->{_tmpfh});
  delete($self->{_content2fh});
  #
  # And eventual reference counts
  #
  delete($self->{_content});

  return $self;
}

# ----------------------------------------------------------------------------------------

=head2 ast($self, $ast)

Getter/setter of the AST of the preprocessed output. if $ast is in the parameter this will set the value and returns it. This must be a parsed tree value as returned by Marpa::C::Languages::AST->value.

=cut

sub ast {
  my $self = shift;

  if (@_) {
      $self->{_ast} = shift;
  }

  return $self->{_ast};
}

# ----------------------------------------------------------------------------------------

=head2 get($self, $attribute, $level)

C::Scan like method, that is a proxy to $self->$attribute. All methods described after can be used as attribute, for example: $self->get('strings'), or $self->get('includes'). The level optional argument has a meaning only on methods that does support this notion. C.f. the L<SYNOPSIS> section.

=cut

sub get {
  my ($self, $attribute, $level) = @_;

  return $self->$attribute($level);
}

# ----------------------------------------------------------------------------------------

=head2 includes($self)

Returns a reference to a list of included files, sorted alphabetically. This is available JUST because preprocessors give the file that has been preprocessed in their output using a #line directive, and there is a special procedure in MarpaX::Languages::C::AST for that, on top of the ISO C grammar.

=cut

sub includes {
  my ($self) = @_;

  return $self->{_includes};
}

# ----------------------------------------------------------------------------------------

=head2 strings($self)

Returns a reference to a list of strings after preprocessing, regardless of scope level.

=cut

sub strings {
  my ($self) = @_;

  return $self->{_strings};
}

# ----------------------------------------------------------------------------------------

=head2 macros($self)

Returns a reference to a list of macros before preprocessing.

=cut

sub macros {
  my ($self) = @_;

  return $self->{_macros};
}

# ----------------------------------------------------------------------------------------

=head2 defines_args($self)

Returns a reference to a hash of macros with arguments. This is a post-processing of $self->macros.

=cut

sub defines_args {
  my ($self) = @_;

  return $self->{_defines_args};
}

# ----------------------------------------------------------------------------------------

=head2 defines_no_args($self)

Returns a reference to a hash of macros with no argument. This is also a post-processing of $self->macros.

=cut

sub defines_no_args {
  my ($self) = @_;

  return $self->{_defines_no_args};
}

# ----------------------------------------------------------------------------------------

=head2 decls($self)

Returns a reference to a list of parsed declarations.

=cut

sub decls {
  my ($self) = @_;

  return $self->{_decls};
}

# ----------------------------------------------------------------------------------------

=head2 defs($self)

Returns a reference to a list of function definitions.

=cut

sub defs {
  my ($self) = @_;

  return $self->{_defs};
}

# ----------------------------------------------------------------------------------------

=head2 parsed_fdecls($self)

C::Scan compatible reference to list of parsed declarations of functions. Please note that the arguments, as per C::Scan documents, are an array reference of: (ty, nm, args, ft, mod). In our terminology, if the argument is a function, then the type 'ty' is the return type 'rt'.

For example, in:

int func1(int x1, double *x2, float *(*f1)(int x11, double x12));

the type 'ty' of f1 is '*', its return type 'rt' is 'float *'. And what C::Scan calls 'ty' is in fact the return type of the function.

=cut

sub parsed_fdecls {
  my ($self) = @_;

  my $level = 1;
  my @list = ();

  foreach (@{$self->decls}) {
      if (! $self->_getRcp($_, 'func')) {
	  next;
      }
      my $argsp = [];
      push(@list,
	   [
	    $self->_getRcp($_, 'rt') || '',
	    $self->_getRcp($_, 'nm') || '',
	    $argsp,
	    $self->_getRcp($_, 'ft') || '',
	    undef
	   ]);
      if ($self->_definedRcp($_, 'args')) {
	  foreach (@{$self->_getRcp($_, 'args')}) {
	      push(@{$argsp},
		   [
		    ($self->_getRcp($_, 'func') ? $self->_getRcp($_, 'rt') : $self->_getRcp($_, 'ty')) || '',
		    $self->_getRcp($_, 'nm') || '',
		    undef,
		    $self->_getRcp($_, 'ft') || '',
		    $self->_getRcp($_, 'mod') || '',
		   ]);
	  }
      }

  }

  return \@list;
}

# ----------------------------------------------------------------------------------------

=head2 fdecls($self)

C::Scan compatible reference to a list of parsed declarations of functions.

=cut

sub fdecls {
  my ($self) = @_;

  return [ map { $_->[1] } @{$self->parsed_fdecls} ];
}

# ----------------------------------------------------------------------------------------

=head2 inlines($self)

C::Scan compatible reference to a list of definitions of functions.

=cut

sub inlines {
  my ($self) = @_;

  return [ map {$self->_getRcp($_, 'nm')} @{$self->defs} ];
}

# ----------------------------------------------------------------------------------------

=head2 typedef_hash($self)

Hopefully C::Scan compatible reference to a hash which contains known typedefs as keys. The values of the hash may not be compatible with C::Scan output. In our case these are array references of length 2, with at index 0 the full text used to parsed this typedef (maybe inclusing more than needed, but always what is necessary), and at index 1 an empty string.

=cut

sub typedef_hash {
  my ($self) = @_;

  my %hash = ();

  foreach (@{$self->decls}) {
      if ($self->_existsRcp($_, 'typedef') && $self->_getRcp($_, 'typedef')) {
	  my $nm = $self->_getRcp($_, 'nm');
	  my $ft = $self->_getRcp($_, 'ft');
	  if ($ft =~ /^\s*typedef\s*/) {
	      #
	      # typedef is at the beginning
	      #
	      $ft =~ s/^\s*typedef\s*//;
	  } elsif ($ft =~ /\s*typedef\s*$/) {
	      #
	      # typedef is at the end (huh, impossible in fact)
	      #
	      $ft =~ s/\s*typedef\s*$//;
	  } else {
	      #
	      # Somewhere else
	      #
	      $ft =~ s/\s*typedef\s*/ /;
	  }
	  $hash{$nm} = [ $ft, '' ];
      }
  }

  return \%hash;
}

# ----------------------------------------------------------------------------------------

=head2 typedef_texts($self)

Returns a reference to a list which contains known expansions of typedefs. This is just the first indice from "value" part of typedef_hash.

=cut

sub typedef_texts {
  my ($self) = @_;

  return [ sort map {$_->[0]} values %{$self->typedef_hash} ];

}

# ----------------------------------------------------------------------------------------

=head2 typedefs_maybe($self)

Returns a reference to a list of typedefed names. This is just the "key" part of typedef_hash. The name "maybe" is kept for compatibility with C::Scan.

=cut

sub typedefs_maybe {
  my ($self) = @_;

  return [ sort keys %{$self->typedef_hash} ];

}

# ----------------------------------------------------------------------------------------

=head2 vdecls($self)

Returns a reference to a list of extern variable declarations.

=cut

sub vdecls {
  my ($self) = @_;

  return [ sort map { $self->_getRcp($_, 'nm') } grep { $self->_getRcp($_, 'extern') } @{$self->decls} ];
}

# ----------------------------------------------------------------------------------------

=head2 vdecl_hash($self)

Hopefully C::Scan compatible reference to a hash of parsed extern variable declarations, containing the variable names as keys.  The values of the hash may not be compatible with C::Scan output. In our case these are array references of length 2, with at index 0 the full text used to parsed this typedef (maybe inclusing more than needed, but always what is necessary), and at index 1 an empty string.

=cut

sub vdecl_hash {
  my ($self) = @_;

  my %hash = ();

  foreach (@{$self->decls}) {
      if ($self->_existsRcp($_, 'extern') && $self->_getRcp($_, 'extern')) {
	  my $nm = $self->_getRcp($_, 'nm');
	  my $ft = $self->_getRcp($_, 'ft');
	  if ($ft =~ /^\s*extern\s*/) {
	      #
	      # extern is at the beginning
	      #
	      $ft =~ s/^\s*extern\s*//;
	  } elsif ($ft =~ /\s*extern\s*$/) {
	      #
	      # extern is at the end (huh, impossible in fact)
	      #
	      $ft =~ s/\s*extern\s*$//;
	  } else {
	      #
	      # Somewhere else
	      #
	      $ft =~ s/\s*extern\s*/ /;
	  }
	  $hash{$nm} = [ $ft, '' ];
      }
  }

  return \%hash;
}

# ----------------------------------------------------------------------------------------

=head2 typedef_structs($self)

Hopefully C::Scan compatible reference to a hash which contains known typedefs as keys. The values of the hash may not be compatible with C::Scan output. In our case these are array references of length 2, with at index 0 the full text used to parsed this typedef (maybe inclusing more than needed, but always what is necessary), and at index 1 an empty string.

=cut

sub typedef_structs {
  my ($self) = @_;

  my %hash = ();

  foreach (@{$self->decls}) {
      if ($self->_existsRcp($_, 'typedef') && $self->_getRcp($_, 'typedef')) {
	  my $nm = $self->_getRcp($_, 'nm');
	  my $ft = $self->_getRcp($_, 'ft');
	  if ($ft =~ /^\s*typedef\s*/) {
	      #
	      # typedef is at the beginning
	      #
	      $ft =~ s/^\s*typedef\s*//;
	  } elsif ($ft =~ /\s*typedef\s*$/) {
	      #
	      # typedef is at the end (huh, impossible in fact)
	      #
	      $ft =~ s/\s*typedef\s*$//;
	  } else {
	      #
	      # Somewhere else
	      #
	      $ft =~ s/\s*typedef\s*/ /;
	  }
	  $hash{$nm} = [ $ft, '' ];
      }
  }

  return \%hash;
}

# ----------------------------------------------------------------------------------------

sub _init {
    my ($self) = @_;

    my $cmd = "$self->{_cpprun} $self->{_cppflags} $self->{_filename}";

    my ($success, $error_code, undef, $stdout_bufp, $stderr_bufp) = run(command => $cmd);

    if (! $success) {
      croak join('', @{$stderr_bufp});
    }

    my $stdout_buf = join('',@{$stdout_bufp});

    $self->_analyse_with_grammar($stdout_buf);
    $self->_analyse_with_heuristics($stdout_buf);
    $self->_posprocess_heuristics();

}

# ----------------------------------------------------------------------------------------

sub _getAst {
  my ($self, $stdout_buf) = @_;

  #
  # Temporary stuff
  #
  my %tmpHash = (_currentFile => undef, _includes => {});
  #
  # Get the AST, the lexeme callback will flag position2file to things of interest
  #
  $self->{_includes} = {};
  $self->{_strings} = [];
  $self->{_position2File} = {};
  #
  # Plus from our module: strings detection
  #
  my $value = MarpaX::Languages::C::AST->new
      (
       logInfo => ['STRING_LITERAL_UNIT'],
       lexemeCallback => [ \&_lexemeCallback,
			   {self => $self,
			    tmpHashp => \%tmpHash,
			   }
       ]
      )->parse(\$stdout_buf)->value;
  $self->{_ast} = ${$value};

  #
  # Includes was a hash in %tmpHash
  #
  $self->{_includes} = [ sort keys %{$tmpHash{_includes}} ];
  #
  # Precompute all position2File keys in sorted order for $self->_positionOk() efficiency
  #
  $self->{_sortedPosition2File} = [ sort {$a <=> $b} keys %{$self->{_position2File}} ];
}

# ----------------------------------------------------------------------------------------

sub _analyse_with_grammar {
  my ($self, $stdout_buf) = @_;

  $self->_getAst($stdout_buf);
  #
  # The ISO ANSI C 2011, like previous ISO, falls into two big categories:
  # - declarations
  # - definitions
  #
  # i.e.:
  #
  # translationUnit     ::= externalDeclaration+
  #
  # C::Scan is only interested by declaration at the top level, i.e:
  #
  # externalDeclaration ::= declaration
  #
  # externalDeclaration is not recursive. declaration is. So we check this
  # is a top-level declaration explicitely.
  #
  $self->{_decls} = [];
  $self->{_defs} = [];
  my $nbDeclarationOk = 0;
  my $nbFunctionDefinitionOk = 0;
  my $nbExternalDeclarationSkipped = 0;
  foreach (@{$self->ast}) {
      my $externalDeclaration = $_;
      #
      # An externalDeclaration is:
      #
      # externalDeclaration ::= functionDefinition
      #                       | declaration
      my $blessed = blessed($externalDeclaration->[0]) || '';
      if ($blessed eq 'C::AST::declaration') {
	  my $declaration = $externalDeclaration->[0];
	  if ($self->_analyseDeclaration($stdout_buf, $declaration, $self->{_decls})) {
	      ++$nbDeclarationOk;
	  }
      } elsif ($blessed eq 'C::AST::functionDefinition') {
	  my $functionDefinition = $externalDeclaration->[0];
	  if ($self->_analyseFunctionDefinition($stdout_buf, $functionDefinition, $self->{_defs})) {
	      ++$nbFunctionDefinitionOk;
	  }
      } else {
	  ++$nbExternalDeclarationSkipped;
      }
  }

}

# ----------------------------------------------------------------------------------------

sub _analyseDeclaration {
  my ($self, $stdout_buf, $declaration, $listp) = @_;
  #
  # A declaration is:
  #
  # declaration ::= declarationSpecifiers SEMICOLON
  #               | declarationCheck
  #               | staticAssertDeclaration
  #
  # Note: staticAssertDeclaration is not a real declaration, but a hack to
  # insert executable code and is here because this is the best place for
  # it in the grammar...
  #
  # Lexeme available: check if position is ok
  #
  if ($#{$declaration} == 1 && ! $self->_positionOk($declaration->[1]->[0])) {
      return 0;
  }

  if (blessed($declaration->[0]) eq 'C::AST::declarationSpecifiers') {
      #
      # Unnamed declaration
      #
      my $declarationSpecifiers = $declaration->[0];
      my $context = $self->_newRcp();
      if (! $self->_buildContext($stdout_buf, $declarationSpecifiers, $context, $listp)) {
	  return 0;
      }
      #
      # Note that we pushing an rcp without context, in fact rcp is a context
      # by itself -;
      #
      $self->_pushRcp($stdout_buf, $declaration, $context, $listp);
  } elsif (blessed($declaration->[0]) eq 'C::AST::declarationCheck') {
      #
      # Named declaration
      #
      my $declarationCheck = $declaration->[0];
      if (! $self->_analyseDeclarationCheck($stdout_buf, $declarationCheck, $listp)) {
	  return 0
      }

  } else {
      #
      # not a real declaration
      #
      return 0;
  }

  return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseFunctionDefinition {
  my ($self, $stdout_buf, $functionDefinition, $listp) = @_;
  #
  # A functionDefinition is:
  #
  # functionDefinition ::= functionDefinitionCheck1
  #                      | functionDefinitionCheck2
  #

  if (blessed($functionDefinition->[0]) eq 'C::AST::functionDefinitionCheck1') {
      my $functionDefinitionCheck1 = $functionDefinition->[0];
      if (! $self->_analyseFunctionDefinitionCheck1($stdout_buf, $functionDefinitionCheck1, $listp)) {
	  return 0
      }
  } elsif (blessed($functionDefinition->[0]) eq 'C::AST::functionDefinitionCheck2') {
      my $functionDefinitionCheck2 = $functionDefinition->[0];
      if (! $self->_analyseFunctionDefinitionCheck2($stdout_buf, $functionDefinitionCheck2, $listp)) {
	  return 0
      }
  }

  return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseFunctionDefinitionCheck1 {
    my ($self, $stdout_buf, $functionDefinitionCheck1, $listp) = @_;
    #
    # A functionDefinitionCheck1 is:
    #
    # functionDefinitionCheck1 ::= functionDefinitionCheck1declarationSpecifiers
    #                              fileScopeDeclarator
    #                              functionDefinitionCheck1declarationList
    #                              compoundStatementReenterScope
    #
    # where
    #
    # functionDefinitionCheck1declarationSpecifiers ::= declarationSpecifiers
    # fileScopeDeclarator ::= declarator
    # functionDefinitionCheck1declarationList ::= declarationList
    # compoundStatementReenterScope ::= LCURLY RCURLY_SCOPE | LCURLY blockItemList RCURLY_SCOPE
    #
    #
    # Lexeme available: check if position is ok
    #
    my $compoundStatementReenterScope = $functionDefinitionCheck1->[3];
    if (! $self->_positionOk($compoundStatementReenterScope->[0]->[0])) {
	return 0;
    }

    my $declarationSpecifiers = $functionDefinitionCheck1->[0]->[0];
    my $contextp = $self->_newRcp();
    if (! $self->_buildContext($stdout_buf, $declarationSpecifiers, $contextp, $listp)) {
	return 0;
    }

    my $newRcp = $self->_newRcp();
    my $declarator = $functionDefinitionCheck1->[1]->[0];
    if (! $self->_analyseDeclarator($stdout_buf, $declarator, $newRcp)) {
	return 0;
    }

    my $declarationList = $functionDefinitionCheck1->[2]->[0];
    $self->_setRcp($newRcp, 'args', []);
    if (! $self->_analyseDeclarationList($stdout_buf, $declarationList, $self->_getRcp($newRcp, 'args'))) {
	return 0;
    }

    $self->_pushRcp($stdout_buf, $functionDefinitionCheck1, $newRcp, $listp, $contextp);

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseFunctionDefinitionCheck2 {
    my ($self, $stdout_buf, $functionDefinitionCheck2, $listp) = @_;
    #
    # A functionDefinitionCheck2 is:
    #
    # functionDefinitionCheck2 ::= functionDefinitionCheck2declarationSpecifiers
    #                              fileScopeDeclarator
    #                              compoundStatementReenterScope
    #
    # where
    #
    # functionDefinitionCheck2declarationSpecifiers ::= declarationSpecifiers
    # fileScopeDeclarator ::= declarator
    # compoundStatementReenterScope ::= LCURLY RCURLY_SCOPE | LCURLY blockItemList RCURLY_SCOPE
    #
    # Lexeme available: check if position is ok
    #
    my $compoundStatementReenterScope = $functionDefinitionCheck2->[2];
    if (! $self->_positionOk($compoundStatementReenterScope->[0]->[0])) {
	return 0;
    }

    my $declarationSpecifiers = $functionDefinitionCheck2->[0]->[0];
    my $contextp = $self->_newRcp();
    if (! $self->_buildContext($stdout_buf, $declarationSpecifiers, $contextp, $listp)) {
	return 0;
    }

    my $newRcp = $self->_newRcp();
    my $declarator = $functionDefinitionCheck2->[1]->[0];
    if (! $self->_analyseDeclarator($stdout_buf, $declarator, $newRcp)) {
	return 0;
    }

    $self->_pushRcp($stdout_buf, $functionDefinitionCheck2, $newRcp, $listp, $contextp);

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseDeclarationList {
    my ($self, $stdout_buf, $declarationList, $listp) = @_;
    #
    # declarationList is:
    #
    # declarationList ::= declaration+
    #
    foreach (@{$declarationList}) {
	if (! $self->_analyseDeclaration($stdout_buf, $_, $listp)) {
	    return 0;
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseDeclarationCheck {
    my ($self, $stdout_buf, $declarationCheck, $listp) = @_;

    #
    # A declarationCheck is:
    #
    # declarationCheck ::= declarationCheckdeclarationSpecifiers (1)
    #                      declarationCheckinitDeclaratorList    (2)
    #                      SEMICOLON                             (3)
    #
    # (1) defines entirely the type specifiers and qualifiers of this declaration
    # (2) is where are the declarators
    # (3) says this is the end. Since this is a lexeme we can use it to
    #     do a source filter.
    #
    # A lexeme is always, in our AST: [start, length, value]
    #
    if (! $self->_positionOk($declarationCheck->[2]->[0])) {
	return 0;
    }

    #
    # A declarationCheckdeclarationSpecifiers is:
    #
    # declarationCheckdeclarationSpecifiers ::= declarationSpecifiers
    #
    my $declarationSpecifiers = $declarationCheck->[0]->[0];
    my $contextp = $self->_newRcp();
    if (! $self->_buildContext($stdout_buf, $declarationSpecifiers, $contextp, $listp)) {
	return 0;
    }
    #
    # A declarationCheckinitDeclaratorList is:
    #
    # declarationCheckinitDeclaratorList ::= initDeclaratorList
    #
    my $initDeclaratorList = $declarationCheck->[1]->[0];
    if (! $self->_analyseInitDeclaratorList(
	      $stdout_buf,
	      $initDeclaratorList,
	      $listp,
	      $contextp)) {
	return 0;
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _buildContext {
    my ($self, $stdout_buf, $specifiersList, $contextp, $listp) = @_;

    if (blessed($specifiersList) eq 'C::AST::declarationSpecifiers') {
	if (! $self->_analyseDeclarationSpecifiers($stdout_buf, $specifiersList, $contextp)) {
	    return 0;
	}
    } elsif (blessed($specifiersList) eq 'C::AST::specifierQualifierList') {
	if (! $self->_analyseSpecifierQualifierList($stdout_buf, $specifiersList, $contextp)) {
	    return 0;
	}
    }
    #
    # The context can not only give current type, but also be a new storage type specifier:
    # - Enum cases, simpler but also orthogonal, are treated explicitely in _analyseEnumSpecifier.
    # - Struct cases are treated here.
    #
    if ($self->_existsRcp($contextp, 'struct') && $self->_getRcp($contextp, 'struct')) {
	#
	# Look to _analyseStructOrUnionSpecifier and you will see it can set only
	# the following fields:
	# 'nm'
	# 'struct'
	# 'args'
	# 'ty'
	# 'structOrUnion'
	#
	my $newRcp = $self->_newRcp();
	foreach (qw/nm struct args ty structOrUnion/) {
	    $self->_setRcp($newRcp, $_, $self->_getRcp($contextp, $_));
	}
	#
	# Force 'type' flag
	#
	$self->_setRcp($newRcp, 'type', 1);
	#
	# Push to same-level context
	#
	push(@{$listp}, $newRcp);
	#
	# List of fields we delete: all but 'ty'
	#
	foreach (qw/nm struct args structOrUnion type/) {
	    $self->_deleteRcp($contextp, $_);
	}
    }

    #
    # Add startPosition - used to get full text
    #
    $self->_setRcp($contextp, '_startPosition', $self->_startPosition($specifiersList));

    return 1;
}

# ----------------------------------------------------------------------------------------
#
# Important note: Everywhere there is a _pushRcp() in the code, there is a corresponding
# _newRcp() in the SAME scope.
#
sub _pushRcp {
    my ($self, $stdout_buf, $o, $rcp, $listp, $contextp) = @_;

    $contextp //= $self->_newRcp();

    #
    # The push always takes care of:
    # - Unnamed $rcp. In practice this happen only for unnamed typedefs.
    #
    if (! $self->_definedRcp($rcp, 'nm')) {
	my $nm = sprintf('ANON%d', $self->{_anonCount}++);
	$self->_setRcp($rcp, 'nm', $nm);
    }
    #
    # - Full text
    #
    my $ft = $self->_text($stdout_buf, $o, $self->_getRcp($contextp, '_startPosition'));
    $self->_setRcp($rcp, 'ft', $ft);
    #
    # - Final type: rt for a function, ty otherwise, EXCEPT at the
    #   top level of functionDefinition, where there is no type
    #   attached to a function, neither it has a 'type' or a 'var' flag
    #
    if ($self->_definedRcp($rcp, 'func')) {
	if (defined($contextp) && $self->_definedRcp($contextp, 'ty')) {
	    $self->_prependRcp($rcp, 'rt', $self->_getRcp($contextp, 'ty'));
	} elsif (! $self->_definedRcp($rcp, 'rt')) {
	    #
	    # Default return type is int
	    #
	    $self->_setRcp($rcp, 'rt', 'int');
	}
    } else {
	if (defined($contextp) && $self->_definedRcp($contextp, 'ty')) {
	    $self->_prependRcp($rcp, 'ty', $self->_getRcp($contextp, 'ty'));
	}
    }
    #
    # Inheritance from context
    #
    foreach (keys %KEY2ID) {
	if (! $self->_definedRcp($rcp, $_) && $self->_definedRcp($contextp, $_)) {
	    $self->_setRcp($rcp, $_, $self->_getRcp($contextp, $_));
	}
    }
    #
    # type or var flag
    #
    if ($self->_definedRcp($rcp, 'structOrUnion') || $self->_definedRcp($rcp, 'typedef')) {
	$self->_setRcp($rcp, 'type', 1);
    } else {
	$self->_setRcp($rcp, 'var', 1);
    }
    if ($listp == $self->defs) {
	$self->_deleteRcp($rcp, 'ty');
	$self->_deleteRcp($rcp, 'type');
	$self->_deleteRcp($rcp, 'var');
    }
    #
    #
    # Remove any internal meanings
    #
    $self->_purgeRcp($rcp);

    push(@{$listp}, $rcp);
}

# ----------------------------------------------------------------------------------------

sub _analyseInitDeclaratorList {
    my ($self, $stdout_buf, $initDeclaratorList, $listp, $contextp) = @_;
    
    #
    # A initDeclaratorList is:
    #
    # initDeclaratorList ::= initDeclarator
    #                      | initDeclaratorList COMMA initDeclarator
    #
    foreach (@{$initDeclaratorList}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::initDeclarator') {
	    #
	    # Every declarator is a new entry and inherits declarationSpecifiers
	    # from the top level.
	    #
	    my $initDeclarator = $_;
	    my $newRcp = $self->_newRcp();
	    if (! $self->_analyseInitDeclarator($stdout_buf, $initDeclarator, $newRcp)) {
		return 0;
	    }
	    #
	    # Push initDeclarator
	    #
	    $self->_pushRcp($stdout_buf, $initDeclarator, $newRcp, $listp, $contextp);
	} elsif ($blessed eq 'C::AST::initDeclaratorList') {
	    #
	    # Will croak by default if more than 100... Is that going to happen in a real C
	    # source-code ? If yes, it better be rewriten -;
	    #
	    no warnings 'recursion';
	    if (! $self->_analyseInitDeclaratorList($stdout_buf, $_, $listp, $contextp)) {
		return 0;
	    }
	} else {
	    #
	    # Lexeme available: check if position is ok
	    #
	    if (! $self->_positionOk($_->[0])) {
		return 0;
	    }
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _setRcp {
    my ($self, $rcp, %what) = @_;

    while (my ($key,$value) = each %what) {
	if ($self->{_asHash}) {
	    $rcp->{$key} = $value;
	} else {
	    $rcp->[$KEY2ID{$key}] = $value;
	}
    }
}

# ----------------------------------------------------------------------------------------

sub _appendRcp {
    my ($self, $rcp, $key, $value) = @_;

    if (defined($value)) {

      if ($self->{_asHash}) {
	if (defined($rcp->{$key}) && length($rcp->{$key}) > 0) {
          $rcp->{$key} .= " $value";
	} else {
          $self->_setRcp($rcp, $key, $value);
	}
      } else {
	if (defined($rcp->[$KEY2ID{$key}]) && length($rcp->[$KEY2ID{$key}]) > 0) {
          $rcp->[$KEY2ID{$key}] .= " $value";
	} else {
          $self->_setRcp($rcp, $key, $value);
	}
      }
    }
}

# ----------------------------------------------------------------------------------------

sub _prependRcp {
    my ($self, $rcp, $key, $value) = @_;

    if (defined($value)) {

      if ($self->{_asHash}) {
	if (defined($rcp->{$key}) && length($rcp->{$key}) > 0) {
          $rcp->{$key} = "$value $rcp->{$key}";
	} else {
          $self->_setRcp($rcp, $key, $value);
	}
      } else {
	if (defined($rcp->[$KEY2ID{$key}]) && length($rcp->[$KEY2ID{$key}]) > 0) {
          $rcp->[$KEY2ID{$key}] = "$value $rcp->[$KEY2ID{$key}]";
	} else {
          $self->_setRcp($rcp, $key, $value);
	}
      }
    }
}

# ----------------------------------------------------------------------------------------

sub _definedRcp {
    my ($self, $rcp, $key) = @_;

    if ($self->{_asHash} && ! exists($rcp->{$key})) {
	#
	# This is preventing undef to be inserted in the hash, in hash mode if any
	#
	return 0;
    }

    if ($self->{_asHash}) {
	return defined($rcp->{$key});
    } else {
	return defined($rcp->[$KEY2ID{$key}]);
    }
}

# ----------------------------------------------------------------------------------------

sub _existsRcp {
    my ($self, $rcp, $key) = @_;

    if ($self->{_asHash}) {
	return exists($rcp->{$key});
    } else {
	#
	# No notion of exists for an array. Using defined will put the  value to undef
	#
	return $self->_definedRcp($rcp, $key);
    }
}

# ----------------------------------------------------------------------------------------

sub _deleteRcp {
    my ($self, $rcp, $key) = @_;

    if ($self->{_asHash}) {
	delete($rcp->{$key});
    } else {
	$rcp->[$KEY2ID{$key}] = undef;
    }
}

# ----------------------------------------------------------------------------------------

sub _purgeRcp {
    my ($self, $rcp) = @_;

    if ($self->{_asHash}) {
      foreach (@PURGE_KEYS) {
	delete($rcp->{$_});
      }
    } else {
	splice(@{$rcp}, $PURGE_IDX);
    }
}

# ----------------------------------------------------------------------------------------

sub _getRcp {
    my ($self, $rcp, $key) = @_;

    if ($self->{_asHash}) {
	return $rcp->{$key};
    } else {
	return $rcp->[$KEY2ID{$key}];
    }
}

# ----------------------------------------------------------------------------------------

sub _analyseInitDeclarator {
  my ($self, $stdout_buf, $initDeclarator, $rcp) = @_;
  #
  # A initDeclarator is:
  #
  # initDeclarator ::= declarator EQUAL initializer
  #                  | declarator
  #
  # Lexeme available: check if position is ok
  #
  if ($#{$initDeclarator} >= 1 && ! $self->_positionOk($initDeclarator->[1]->[0])) {
      return 0;
  }

  my $declarator = $initDeclarator->[0];
  if (! $self->_analyseDeclarator($stdout_buf, $declarator, $rcp)) {
      return 0;
  }

  my $initializer = undef;
  if ($#{$initDeclarator} == 2) {
      $self->_setRcp($rcp, 'init', $self->_text($stdout_buf, $initDeclarator->[2]));
  }

  return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseDeclarator {
  my ($self, $stdout_buf, $declarator, $rcp) = @_;

  #
  # A declarator is:
  #
  # declarator ::= pointer msvsAttributeAny directDeclarator                   (1)
  #              | pointer msvsAttributeAny directDeclarator gccAsmExpression  (2)
  #              | msvsAttributeAny directDeclarator                           (3)
  #              | msvsAttributeAny directDeclarator gccAsmExpression          (4)
  #              | MSVS___C_ASSERT__ LBRACKET expression RBRACKET              (5)
  #
  # (1) to (4) are of interest. (5) is not. We make sure we fall into (1) to (4)
  # by search an immediate directDeclarator.
  #
  # Lexeme available: check if position is ok
  #
  if (! blessed($declarator->[0]) && ! $self->_positionOk($declarator->[0]->[0])) {
      return 0;
  }

  my $firstElementBlessed = blessed($declarator->[0]) || '';
  if ($firstElementBlessed eq 'C::AST::pointer') {
      my $pointers = $self->_concatPointers($declarator->[0]);
      $self->_appendRcp($rcp, 'ty', $pointers);
  }

  my $directDeclarator = undef;
  foreach (@{$declarator}) {
      my $blessed = blessed($_) || '';
      if ($blessed eq 'C::AST::directDeclarator') {
	  $directDeclarator = $_;
      }
  }
  if (defined($directDeclarator)) {
      if (! $self->_analyseDirectDeclarator($stdout_buf, $directDeclarator, $rcp)) {
	  return 0
      }
  }

  return 1;
}

# ----------------------------------------------------------------------------------------

sub _startPosition {
    my ($self, $o) = @_;

    my $startPosition = undef;
    MarpaX::Languages::C::AST::Util::Data::Find->new
	(
	 wanted => sub {
	     my $o = shift;
	     my $blessed = blessed($o) || '';
	     my $reftype = reftype($o) || '';
	     return (! $blessed && $reftype eq 'ARRAY');
	 },
	 callback => sub {
	     my ($self, $o) = @_;
	     if (! defined($startPosition) || $o->[0] < $startPosition) {
		 $startPosition = $o->[0];
	     }
	 },
	 callbackArgs => [ $self ],
	)->process($o);

    return $startPosition;
}

# ----------------------------------------------------------------------------------------

sub _text {
    my ($self, $stdout_buf, $o, $startPosition, $endPosition) = @_;

    if (! defined($startPosition) || ! defined($endPosition)) {

	MarpaX::Languages::C::AST::Util::Data::Find->new
	    (
	     callback => sub {
		 my ($o) = @_;
		 my $blessed = blessed($o) || '';
		 my $reftype = reftype($o) || '';
		 if (! $blessed && $reftype eq 'ARRAY') {
		     my $start = $o->[0];
		     my $end = $start + $o->[1];
		     if (! defined($startPosition) || $start < $startPosition) {
			 $startPosition = $start;
		     }
		     if (! defined($endPosition) || $end > $endPosition) {
			 $endPosition = $end;
		     }
		 }
	     }
	    )->process($o);
    }

    my $text = substr($stdout_buf, $startPosition, $endPosition - $startPosition);
    #
    # We sanitize the text
    #
    #$text =~ s/^\s*//;
    #$text =~ s/\s$//;
    #$text =~ s/\s+/ /g;

    return $text;
}

# ----------------------------------------------------------------------------------------

sub _endPosition {
    my ($self, $o) = @_;

    my $endPosition = undef;
    MarpaX::Languages::C::AST::Util::Data::Find->new
	(
	 wanted => sub {
	     my $o = shift;
	     my $blessed = blessed($o) || '';
	     my $reftype = reftype($o) || '';
	     return (! $blessed && $reftype eq 'ARRAY');
	 },
	 callback => sub {
	     my ($self, $o) = @_;
	     if (! defined($endPosition) || $o->[0] > $endPosition) {
		 $endPosition = $o->[0];
	     }
	 },
	 callbackArgs => [ $self ],
	)->process($o);

    return $endPosition;
}

# ----------------------------------------------------------------------------------------

sub _analyseEnumerationConstant {
    my ($self, $stdout_buf, $enumerationConstant, $rcp) = @_;
    #
    # enumerationConstant is:
    #
    # enumerationConstant ::= enumerationConstantIdentifier
    #
    if (! $self->_positionOk($enumerationConstant->[0]->[0]->[0])) {
	return 0;
    }

    $self->_setRcp($rcp, 'nm', $enumerationConstant->[0]->[0]->[2]);
    #
    # The type of an enum is compiler dependant. Usually int, though.
    #
    $self->_setRcp($rcp, 'ty', $self->{_enumType});

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseEnumerator {
    my ($self, $stdout_buf, $enumerator, $rcp) = @_;
    #
    # enumerator is:
    #
    # enumerator ::= enumerationConstant EQUAL constantExpression
    #              | enumerationConstant
    #
    #
    # Lexeme available: check if position is ok
    #
    if ($#{$enumerator} > 0 && ! $self->_positionOk($enumerator->[1]->[0])) {
	return 0;
    }

    my $blessed = blessed($enumerator->[-1]) || '';
    if ($blessed eq 'C::AST::constantExpression') {
	$self->_setRcp($rcp, 'init', $self->_text($stdout_buf, $enumerator->[-1]));
    }

    my $enumerationConstant = $enumerator->[0];
    if (! $self->_analyseEnumerationConstant($stdout_buf, $enumerationConstant, $rcp)) {
	return 0;
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseEnumeratorList {
    my ($self, $stdout_buf, $enumeratorList, $listp) = @_;
    #
    # enumeratorList is:
    #
    # enumeratorList ::= enumerator
    #                  | enumeratorList COMMA enumerator
    #
    # Lexeme available: check if position is ok
    #
    if ($#{$enumeratorList} > 0 && ! $self->_positionOk($enumeratorList->[1]->[0])) {
	return 0;
    }

    my $blessed = blessed($enumeratorList->[0]) || '';
    if ($blessed eq 'C::AST::enumeratorList') {
	if (! $self->_analyseEnumeratorList($stdout_buf, $enumeratorList->[0], $listp)) {
	    return 0;
	}
    } else {
	#
	# Note: no toplevel information.
	#
	my $enumerator = $enumeratorList->[-1];
	my $newRcp = $self->_newRcp();
	if (! $self->_analyseEnumerator($stdout_buf, $enumerator, $newRcp)) {
	    return 0;
	}
	$self->_pushRcp($stdout_buf, $enumerator, $newRcp, $listp);
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseEnumSpecifier {
    my ($self, $stdout_buf, $enumSpecifier, $rcp) = @_;
    #
    # enumSpecifier is:
    #
    # enumSpecifier ::= ENUM LCURLY enumeratorList RCURLY
    #                 | ENUM LCURLY enumeratorList COMMA RCURLY
    #                 | ENUM IDENTIFIER_UNAMBIGUOUS LCURLY enumeratorList RCURLY
    #                 | ENUM IDENTIFIER_UNAMBIGUOUS LCURLY enumeratorList COMMA RCURLY
    #                 | ENUM IDENTIFIER_UNAMBIGUOUS

    #
    # Lexeme available: check if position is ok
    #
    if (! $self->_positionOk($enumSpecifier->[0]->[0])) {
	return 0;
    }

    #
    # enum are special beasts: they always introduce a new type
    # in the global namespace, wherever and whenever.
    #
    my $newRcp = $self->_newRcp();

    $self->_setRcp($newRcp, 'enum', 1);
    if ($enumSpecifier->[1]->[2] ne '{') {
	$self->_setRcp($newRcp, 'nm', $enumSpecifier->[1]->[2]);
    } else {
	my $nm = sprintf('ANON%d', $self->{_anonCount}++);
	$self->_setRcp($newRcp, 'nm', $nm);
    }
    #
    # Enum introduce a new type by its name
    #
    $self->_setRcp($newRcp, 'type', 1);
    $self->_setRcp($newRcp, 'ty', $self->_getRcp($newRcp, 'nm'));
    #
    # Look for an eventual enumeratorList
    #
    my $enumeratorList = undef;
    foreach (@{$enumSpecifier}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::enumeratorList') {
	    $enumeratorList = $_;
	    last;
	}
    }
    if (defined($enumeratorList)) {
	$self->_setRcp($newRcp, 'args', []);
	if (! $self->_analyseEnumeratorList($stdout_buf, $enumeratorList, $self->_getRcp($newRcp, 'args'))) {
	    return 0;
	}
    }

    push($self->{_decls}, $newRcp);

    $self->_appendRcp($rcp, 'ty', $self->_getRcp($newRcp, 'nm'));

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseTypeSpecifier1 {
    my ($self, $stdout_buf, $typeSpecifier1, $rcp) = @_;
    #
    # typeSpecifier1 is:
    #
    # typeSpecifier1 ::= VOID
    #                  | FLOAT
    #                  | structOrUnionSpecifier
    #                  | enumSpecifier
    #                  | TYPEDEF_NAME
    my $blessed = blessed($typeSpecifier1->[0]) || '';

    #
    # Lexeme available: check if position is ok
    #
    if (! $blessed && ! $self->_positionOk($typeSpecifier1->[0]->[0])) {
	return 0;
    }

    if ($blessed eq 'C::AST::structOrUnionSpecifier') {
	if (! $self->_analyseStructOrUnionSpecifier($stdout_buf, $typeSpecifier1->[0], $rcp)) {
	    return 0;
	}
    } elsif ($blessed eq 'C::AST::enumSpecifier') {
	if (! $self->_analyseEnumSpecifier($stdout_buf, $typeSpecifier1->[0], $rcp)) {
	    return 0;
	}
    } else {
	$self->_appendRcp($rcp, 'ty', $typeSpecifier1->[0]->[2]);
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseStructOrUnion {
    my ($self, $stdout_buf, $structOrUnion, $rcp) = @_;
    #
    # structOrUnion is:
    #
    # structOrUnion ::= STRUCT | UNION
    #
    #
    # Lexeme available: check if position is ok
    #
    if (! $self->_positionOk($structOrUnion->[0]->[0])) {
	return 0;
    }

    if ($structOrUnion->[0]->[2] eq 'struct') {
	$self->_setRcp($rcp, 'struct', 1);
    } else {
	$self->_setRcp($rcp, 'union', 1);
    }
    $self->_setRcp($rcp, 'structOrUnion', 1);

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseSpecifierQualifierList0 {
    my ($self, $stdout_buf, $specifierQualifierList0, $rcp) = @_;
    #
    # specifierQualifierList0 is:
    #
    # specifierQualifierList0 ::= typeQualifier
    #                           | specifierQualifierList0 typeQualifier
    #                           | (gccExtension)
    #                           | specifierQualifierList0 (gccExtension)
    #
    foreach (@{$specifierQualifierList0}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::typeQualifier') {
	    if (! $self->_analyseTypeQualifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::specifierQualifierList0') {
	    if (! $self->_analyseSpecifierQualifierList0($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseSpecifierQualifierList1 {
    my ($self, $stdout_buf, $specifierQualifierList1, $rcp) = @_;
    #
    # specifierQualifierList1 is:
    #
    # specifierQualifierList1 ::= typeSpecifier1
    #                           | specifierQualifierList0 typeSpecifier1
    #                           | specifierQualifierList1 typeQualifier
    #                           | specifierQualifierList1 (gccExtension)
    #
    foreach (@{$specifierQualifierList1}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::typeSpecifier1') {
	    if (! $self->_analyseTypeSpecifier1($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::typeQualifier') {
	    if (! $self->_analyseTypeQualifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::specifierQualifierList0') {
	    if (! $self->_analyseSpecifierQualifierList0($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::specifierQualifierList1') {
	    if (! $self->_analyseSpecifierQualifierList1($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseSpecifierQualifierList2 {
    my ($self, $stdout_buf, $specifierQualifierList2, $rcp) = @_;
    #
    # specifierQualifierList2 is:
    #
    # specifierQualifierList2 ::= typeSpecifier2
    #                           | specifierQualifierList0 typeSpecifier2
    #                           | specifierQualifierList2 typeSpecifier2
    #                           | specifierQualifierList2 typeQualifier
    #                           | specifierQualifierList2 (gccExtension)
    #
    foreach (@{$specifierQualifierList2}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::typeSpecifier2') {
	    if (! $self->_analyseTypeSpecifier2($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::typeQualifier') {
	    if (! $self->_analyseTypeQualifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::specifierQualifierList0') {
	    if (! $self->_analyseSpecifierQualifierList0($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::specifierQualifierList2') {
	    if (! $self->_analyseSpecifierQualifierList2($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseStructDeclarator {
    my ($self, $stdout_buf, $structDeclarator, $rcp) = @_;
    #
    # structDeclarator is:
    #
    # structDeclarator ::= COLON constantExpression
    #                    | declarator COLON constantExpression
    #                    | declarator
    #
    my $blessed = blessed($structDeclarator->[0]) || '';
    #
    # Lexeme available: check if position is ok
    #
    if (! $blessed && ! $self->_positionOk($structDeclarator->[0]->[0])) {
	return 0;
    }
    if ($#{$structDeclarator} > 0 && ! blessed($structDeclarator->[1]) && ! $self->_positionOk($structDeclarator->[1]->[0])) {
	return 0;
    }

    if ($blessed eq 'C::AST::declarator') {
	my $declarator = $structDeclarator->[0];
	if (! $self->_analyseDeclarator($stdout_buf, $declarator, $rcp)) {
	    return 0;
	}
    } else {
	#
	# Unnamed field, used only for padding
	#
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseStructDeclaratorList {
    my ($self, $stdout_buf, $structDeclaratorList, $listp, $contextp) = @_;
    #
    # structDeclaratorList is:
    #
    # structDeclaratorList ::= structDeclarator
    #                        | structDeclaratorList COMMA structDeclarator
    #
    #
    # Lexeme available: check if position is ok
    #
    if ($#{$structDeclaratorList} > 0 && ! $self->_positionOk($structDeclaratorList->[1]->[0])) {
	return 0;
    }

    if ($#{$structDeclaratorList} > 0) {
	if (! $self->_analyseStructDeclaratorList($stdout_buf, $structDeclaratorList->[0], $listp, $contextp)) {
	    return 0;
	}
    }

    my $newRcp = $self->_newRcp();
    my $structDeclarator = $structDeclaratorList->[-1];
    if (! $self->_analyseStructDeclarator($stdout_buf, $structDeclarator, $newRcp)) {
	return 0;
    }
    $self->_pushRcp($stdout_buf, $structDeclarator, $newRcp, $listp, $contextp);

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseSpecifierQualifierList {
    my ($self, $stdout_buf, $specifierQualifierList, $rcp) = @_;
    #
    # specifierQualifierList is:
    #
    # specifierQualifierList ::= specifierQualifierList0
    #                          | specifierQualifierList1
    #                          | specifierQualifierList2
    my $blessed = blessed($specifierQualifierList->[0]) || '';
    if ($blessed eq 'C::AST::specifierQualifierList0') {
	if (! $self->_analyseSpecifierQualifierList0($stdout_buf, $specifierQualifierList->[0], $rcp)) {
	    return 0;
	}
    } elsif ($blessed eq 'C::AST::specifierQualifierList1') {
	if (! $self->_analyseSpecifierQualifierList1($stdout_buf, $specifierQualifierList->[0], $rcp)) {
	    return 0;
	}
    } else {
	if (! $self->_analyseSpecifierQualifierList2($stdout_buf, $specifierQualifierList->[0], $rcp)) {
	    return 0;
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseStructDeclaration {
    my ($self, $stdout_buf, $structDeclaration, $listp) = @_;
    #
    # structDeclaration is:
    #
    # structDeclaration ::= specifierQualifierList SEMICOLON
    #                     | specifierQualifierList structDeclaratorList SEMICOLON
    #                     | SEMICOLON

    #
    # Lexeme available: check if position is ok
    #
    if (! $self->_positionOk($structDeclaration->[-1]->[0])) {
	return 0;
    }

    my $blessed = blessed($structDeclaration->[0]) || '';
    if ($blessed eq 'C::AST::specifierQualifierList') {
	my $specifierQualifierList = $structDeclaration->[0];
	#
	# specifierQualifierList is nothing else but another declarationSpecifier
	#
	my $contextp = $self->_newRcp();
	if (! $self->_buildContext($stdout_buf, $specifierQualifierList, $contextp, $listp)) {
	    return 0;
	}
	$blessed = blessed($structDeclaration->[1]) || '';
	if ($blessed eq 'C::AST::structDeclaratorList') {
	    my $structDeclaratorList = $structDeclaration->[1];
	    if (! $self->_analyseStructDeclaratorList($stdout_buf, $structDeclaratorList, $listp, $contextp)) {
		return 0;
	    }
	} else {
	    #
	    # No declarator
	    #
	    $self->_pushRcp($stdout_buf, $specifierQualifierList, $contextp, $listp);
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseStructDeclarationList {
    my ($self, $stdout_buf, $structDeclarationList, $listp) = @_;
    #
    # structDeclarationList is:
    #
    # structDeclarationList ::= structDeclaration+
    #
    foreach (@{$structDeclarationList}) {
	if (! $self->_analyseStructDeclaration($stdout_buf, $_, $listp)) {
	    return 0;
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseStructOrUnionSpecifier {
    my ($self, $stdout_buf, $structOrUnionSpecifier, $rcp) = @_;
    #
    # structOrUnionSpecifier is:
    #
    # structOrUnionSpecifier ::= structOrUnion LCURLY <structContextStart> structDeclarationList RCURLY <structContextEnd>
    #                          | structOrUnion IDENTIFIER_UNAMBIGUOUS LCURLY <structContextStart> structDeclarationList RCURLY <structContextEnd>
    #                          | structOrUnion IDENTIFIER_UNAMBIGUOUS
    #

    #
    # Lexeme available: check if position is ok
    #
    if (! $self->_positionOk($structOrUnionSpecifier->[1]->[0])) {
	return 0;
    }

    if ($structOrUnionSpecifier->[1]->[2] ne '{') {
	$self->_setRcp($rcp, 'nm', $structOrUnionSpecifier->[1]->[2]);
    } else {
	my $nm = sprintf('ANON%d', $self->{_anonCount}++);
	$self->_setRcp($rcp, 'nm', $nm);
    }

    my $structOrUnion = $structOrUnionSpecifier->[0];
    if (! $self->_analyseStructOrUnion($stdout_buf, $structOrUnion, $rcp)) {
	return 0;
    }

    if ($#{$structOrUnionSpecifier} >= 2) {
	my $structDeclarationList = $structOrUnionSpecifier->[-3];
	$self->_setRcp($rcp, 'args', []);
	if (! $self->_analyseStructDeclarationList($stdout_buf, $structDeclarationList, $self->_getRcp($rcp, 'args'))) {
	    return 0;
	}
    }
    #
    # For structs, type is: 'struct nameOfStruct'.
    # For enums, type is: 'nameOfEnum'.
    #
    if ($self->_definedRcp($rcp, 'struct')) {
	$self->_setRcp($rcp, 'ty', sprintf('struct %s', $self->_getRcp($rcp, 'nm')));
    } else {
	$self->_setRcp($rcp, 'ty', $self->_getRcp($rcp, 'nm'));
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseTypeSpecifier2 {
    my ($self, $stdout_buf, $typeSpecifier2, $rcp) = @_;
    #
    # typeSpecifier2 is:
    #
    # typeSpecifier2 ::= CHAR
    #                  | SHORT
    #                  | INT
    #                  | LONG
    #                  | DOUBLE
    #                  | SIGNED
    #                  | UNSIGNED
    #                  | BOOL
    #                  | LABEL
    #                  | COMPLEX
    #                  | IMAGINARY
    #                  | atomicTypeSpecifier
    #                  | msvsBuiltinType
    #                  | gccBuiltinType

    #
    # Lexeme available: check if position is ok
    #
    if (! blessed($typeSpecifier2->[0]) && ! $self->_positionOk($typeSpecifier2->[0]->[0])) {
	return 0;
    }

    $self->_appendRcp($rcp, 'ty', $self->_text($stdout_buf, $typeSpecifier2));

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseTypeQualifier {
    my ($self, $stdout_buf, $typeQualifier, $rcp) = @_;
    #
    # No functionnality for us
    #

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseFunctionSpecifier {
    my ($self, $stdout_buf, $functionSpecifier, $rcp) = @_;
    #
    # No functionnality for us
    #

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseAlignmentSpecifier {
    my ($self, $stdout_buf, $functionSpecifier, $rcp) = @_;
    #
    # No functionnality for us
    #

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseStorageClassSpecifierTypedef {
    my ($self, $stdout_buf, $storageClassSpecifierTypedef, $rcp) = @_;
    #
    # storageClassSpecifierTypedef is:
    #
    # storageClassSpecifierTypedef ::= TYPEDEF
    #
    #
    # Lexeme available: check if position is ok
    #
    if (! $self->_positionOk($storageClassSpecifierTypedef->[0]->[0])) {
	return 0;
    }

    $self->_setRcp($rcp, 'typedef', 1);

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseStorageClassSpecifier {
    my ($self, $stdout_buf, $storageClassSpecifier, $rcp) = @_;
    #
    # storageClassSpecifiers is:
    #
    # storageClassSpecifier ::= storageClassSpecifierTypedef
    #                         | EXTERN
    #                         | STATIC
    #                         | THREAD_LOCAL
    #                         | AUTO
    #                         | REGISTER
    #
    # We just position the extern and typedef flags, if any
    #
    my $blessed = blessed($storageClassSpecifier->[0]) || '';

    #
    # Lexeme available: check if position is ok
    #
    if (! $blessed && ! $self->_positionOk($storageClassSpecifier->[0]->[0])) {
	return 0;
    }

    if ($blessed eq 'C::AST::storageClassSpecifierTypedef') {
	if (! $self->_analyseStorageClassSpecifierTypedef($stdout_buf, $storageClassSpecifier->[0], $rcp)) {
	    return 0;
	}
    } else {
	if ($storageClassSpecifier->[0]->[2] eq 'extern') {
	    $self->_setRcp($rcp, 'extern', 1);
	} elsif ($storageClassSpecifier->[0]->[2] eq 'static') {
	    $self->_setRcp($rcp, 'static', 1);
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseDeclarationSpecifiers0 {
    my ($self, $stdout_buf, $declarationSpecifiers0, $rcp) = @_;
    #
    # declarationSpecifiers0 is:
    #
    # declarationSpecifiers0 ::= storageClassSpecifier
    #                          | declarationSpecifiers0 storageClassSpecifier
    #                          | typeQualifier
    #                          | declarationSpecifiers0 typeQualifier
    #                          | functionSpecifier
    #                          | declarationSpecifiers0 functionSpecifier
    #                          | alignmentSpecifier
    #                          | declarationSpecifiers0 alignmentSpecifier
    #                          | (gccExtension)
    #                          | declarationSpecifiers0 (gccExtension)
    foreach (@{$declarationSpecifiers0}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::storageClassSpecifier') {
	    if (! $self->_analyseStorageClassSpecifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::declarationSpecifiers0') {
	    if (! $self->_analyseDeclarationSpecifiers0($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::typeQualifier') {
	    if (! $self->_analyseTypeQualifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::functionSpecifier') {
	    if (! $self->_analyseFunctionSpecifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::alignmentSpecifier') {
	    if (! $self->_analyseAlignmentSpecifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseDeclarationSpecifiers1 {
    my ($self, $stdout_buf, $declarationSpecifiers1, $rcp) = @_;
    #
    # declarationSpecifiers1 is:
    #
    # declarationSpecifiers1 ::= typeSpecifier1
    #                          | declarationSpecifiers0 typeSpecifier1
    #                          | declarationSpecifiers1 storageClassSpecifier
    #                          | declarationSpecifiers1 typeQualifier
    #                          | declarationSpecifiers1 functionSpecifier
    #                          | declarationSpecifiers1 alignmentSpecifier
    #                          | declarationSpecifiers1 (gccExtension)
    #
    foreach (@{$declarationSpecifiers1}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::typeSpecifier1') {
	    if (! $self->_analyseTypeSpecifier1($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::declarationSpecifiers0') {
	    if (! $self->_analyseDeclarationSpecifiers0($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::declarationSpecifiers1') {
	    if (! $self->_analyseDeclarationSpecifiers1($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::storageClassSpecifier') {
	    if (! $self->_analyseStorageClassSpecifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::typeQualifier') {
	    if (! $self->_analyseTypeQualifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::functionSpecifier') {
	    if (! $self->_analyseFunctionSpecifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::alignmentSpecifier') {
	    if (! $self->_analyseAlignmentSpecifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseDeclarationSpecifiers2 {
    my ($self, $stdout_buf, $declarationSpecifiers2, $rcp) = @_;
    #
    # declarationSpecifiers2 is:
    #
    # declarationSpecifiers2 ::= typeSpecifier2
    #                          | declarationSpecifiers0 typeSpecifier2
    #                          | declarationSpecifiers2 typeSpecifier2
    #                          | declarationSpecifiers2 storageClassSpecifier
    #                          | declarationSpecifiers2 typeQualifier
    #                          | declarationSpecifiers2 functionSpecifier
    #                          | declarationSpecifiers2 alignmentSpecifier
    #                          | declarationSpecifiers2 (gccExtension)
    #
    foreach (@{$declarationSpecifiers2}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::typeSpecifier2') {
	    if (! $self->_analyseTypeSpecifier2($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::declarationSpecifiers0') {
	    if (! $self->_analyseDeclarationSpecifiers0($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::declarationSpecifiers2') {
	    if (! $self->_analyseDeclarationSpecifiers2($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::storageClassSpecifier') {
	    if (! $self->_analyseStorageClassSpecifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::typeQualifier') {
	    if (! $self->_analyseTypeQualifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::functionSpecifier') {
	    if (! $self->_analyseFunctionSpecifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::alignmentSpecifier') {
	    if (! $self->_analyseAlignmentSpecifier($stdout_buf, $_, $rcp)) {
		return 0;
	    }
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseDeclarationSpecifiers {
    my ($self, $stdout_buf, $declarationSpecifiers, $rcp) = @_;
    #
    # declarationSpecifiers is:
    #
    # declarationSpecifiers ::= declarationSpecifiers0
    #                         | declarationSpecifiers1
    #                         | declarationSpecifiers2
    #
    my $blessed = blessed($declarationSpecifiers->[0]) || '';
    if ($blessed eq 'C::AST::declarationSpecifiers0') {
	if (! $self->_analyseDeclarationSpecifiers0($stdout_buf, $declarationSpecifiers->[0], $rcp)) {
	    return 0;
	}
    } elsif ($blessed eq 'C::AST::declarationSpecifiers1') {
	if (! $self->_analyseDeclarationSpecifiers1($stdout_buf, $declarationSpecifiers->[0], $rcp)) {
	    return 0
	}
    } else {
	if (! $self->_analyseDeclarationSpecifiers2($stdout_buf, $declarationSpecifiers->[0], $rcp)) {
	    return 0;
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseDirectDeclarator {
    my ($self, $stdout_buf, $directDeclarator, $rcp) = @_;

    #
    # A directDeclarator is:
    #
    # directDeclarator ::= directDeclaratorIdentifier                                                               ( 1)
    #                    | LPAREN declarator RPAREN                                                                 ( 2)
    #                    | directDeclarator LBRACKET RBRACKET                                                       ( 3)
    #                    | directDeclarator LBRACKET STAR RBRACKET                                                  ( 4)
    #                    | directDeclarator LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET  ( 5)
    #                    | directDeclarator LBRACKET STATIC assignmentExpression RBRACKET                           ( 6)
    #                    | directDeclarator LBRACKET gccArrayTypeModifierList STAR RBRACKET                         ( 7)
    #                    | directDeclarator LBRACKET gccArrayTypeModifierList STATIC assignmentExpression RBRACKET  ( 8)
    #                    | directDeclarator LBRACKET gccArrayTypeModifierList assignmentExpression RBRACKET         ( 9)
    #                    | directDeclarator LBRACKET gccArrayTypeModifierList RBRACKET                              (10)
    #                    | directDeclarator LBRACKET assignmentExpression RBRACKET                                  (11)
    #                    | directDeclarator LPAREN_SCOPE parameterTypeList RPAREN_SCOPE                             (12)
    #                    | directDeclarator LPAREN_SCOPE RPAREN_SCOPE                                               (13)
    #                    | directDeclarator LPAREN_SCOPE identifierList RPAREN_SCOPE                                (14)
    #
    # ( 1)         is just a proxy rule to IDENTIFIER, and will give the name
    # ( 2)         justifies a recursive call
    # ( 3) to (11) as well, plus give the (array) modifiers
    # (12) to (13) also, plus give the arguments
    #
    my $firstElement = $directDeclarator->[0];
    my $firstElementBlessed = blessed($firstElement) || '';
    my $firstElementReftype = reftype($firstElement) || '';

    #
    # Lexeme available: check if position is ok
    #
    if (! $firstElementBlessed && ! $self->_positionOk($firstElement->[0])) {
	return 0;
    }
    if ($#{$directDeclarator} > 0 && ! blessed($directDeclarator->[1]) && ! $self->_positionOk($directDeclarator->[1]->[0])) {
	return 0;
    }

    if ($firstElementBlessed eq 'C::AST::directDeclaratorIdentifier') {
	#
	# directDeclarator ::= directDeclaratorIdentifier                                                               ( 1)
	#
	# where a lexeme value is always [start, length, string]
	#
	if (! $self->_definedRcp($rcp, 'nm')) {
	    #
	    # This should not be already defined in theory since we do not allow more than one recursion
	    #
	    $self->_setRcp($rcp, 'nm', $firstElement->[0]->[2]);
	}
    }
    elsif (! $firstElementBlessed && $firstElementReftype eq 'ARRAY' && $firstElement->[2] eq '(') {
	#
	#                    | LPAREN declarator RPAREN                                                                 ( 2)
	#
	my $declarator = $directDeclarator->[1];
	if (! $self->_analyseDeclarator($stdout_buf, $declarator, $rcp)) {
	    return 0;
	}
    }
    elsif ($directDeclarator->[1]->[2] eq '(') {
	#
	#                    | directDeclarator LPAREN_SCOPE parameterTypeList RPAREN_SCOPE                             (12)
	#                    | directDeclarator LPAREN_SCOPE RPAREN_SCOPE                                               (13)
	#                    | directDeclarator LPAREN_SCOPE identifierList RPAREN_SCOPE                                (14)
	#
	$self->_setRcp($rcp, 'func', 1);
	#
	# When an rcp becomes a func, everything that was a type before becomes a return type
	#
	$self->_appendRcp($rcp, 'rt', $self->_getRcp($rcp, 'ty'));
	$self->_deleteRcp($rcp, 'ty');
	if (! $self->_analyseDirectDeclarator($stdout_buf, $directDeclarator->[0], $rcp)) {
	    return 0;
	}
	if ($#{$directDeclarator} == 3) {
	    #
	    # This is a new scope: we intentionnaly do not propagate $topLevelInfop
	    #
	    $self->_setRcp($rcp, 'args', []);
	    if (blessed($directDeclarator->[2]) eq 'C::AST::parameterTypeList') {
		if (! $self->_analyseParameterTypeList($stdout_buf, $directDeclarator->[2], $self->_getRcp($rcp, 'args'))) {
		    return 0;
		}
	    } else {
		if (! $self->_analyseIdentifierList($stdout_buf, $directDeclarator->[2], $self->_getRcp($rcp, 'args'))) {
		    return 0;
		}
	    }
	}
    }
    else {
	#
	#                    | directDeclarator LBRACKET RBRACKET                                                       ( 3)
	#                    | directDeclarator LBRACKET STAR RBRACKET                                                  ( 4)
	#                    | directDeclarator LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET  ( 5)
	#                    | directDeclarator LBRACKET STATIC assignmentExpression RBRACKET                           ( 6)
	#                    | directDeclarator LBRACKET gccArrayTypeModifierList STAR RBRACKET                         ( 7)
	#                    | directDeclarator LBRACKET gccArrayTypeModifierList STATIC assignmentExpression RBRACKET  ( 8)
	#                    | directDeclarator LBRACKET gccArrayTypeModifierList assignmentExpression RBRACKET         ( 9)
	#                    | directDeclarator LBRACKET gccArrayTypeModifierList RBRACKET                              (10)
	#                    | directDeclarator LBRACKET assignmentExpression RBRACKET                                  (11)
	#
	if (! $self->_analyseDirectDeclarator($stdout_buf, $directDeclarator->[0], $rcp)) {
	    return 0;
	}
	if (! $self->_definedRcp($rcp, 'mod')) {
	    #
	    # This should not be already defined in theory since we do not allow more than one recursion
	    #
	    my $startPosition = $directDeclarator->[1]->[0];
	    my $endPosition = $self->_endPosition($directDeclarator);
	    $self->_setRcp($rcp, 'mod', substr($stdout_buf, $startPosition, $endPosition - $startPosition + 1));
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseDirectAbstractDeclarator {
    my ($self, $stdout_buf, $directAbstractDeclarator, $rcp) = @_;
    #
    # A directAbstractDeclarator is:
    #
    # directAbstractDeclarator ::= LPAREN abstractDeclarator RPAREN                                                                 ( 1)
    #                            | LBRACKET RBRACKET                                                                                ( 2)
    #                            | LBRACKET STAR RBRACKET                                                                           ( 3)
    #                            | LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET                           ( 4)
    #                            | LBRACKET STATIC assignmentExpression RBRACKET                                                    ( 5)
    #                            | LBRACKET gccArrayTypeModifierList STATIC assignmentExpression RBRACKET                           ( 6)
    #                            | LBRACKET gccArrayTypeModifierList assignmentExpression RBRACKET                                  ( 7)
    #                            | LBRACKET gccArrayTypeModifierList RBRACKET                                                       ( 8)
    #                            | LBRACKET assignmentExpression RBRACKET                                                           ( 9)
    #                            | directAbstractDeclarator LBRACKET RBRACKET                                                       (10)
    #                            | directAbstractDeclarator LBRACKET STAR RBRACKET                                                  (11)
    #                            | directAbstractDeclarator LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET  (12)
    #                            | directAbstractDeclarator LBRACKET STATIC assignmentExpression RBRACKET                           (13)
    #                            | directAbstractDeclarator LBRACKET gccArrayTypeModifierList assignmentExpression RBRACKET         (14)
    #                            | directAbstractDeclarator LBRACKET gccArrayTypeModifierList STATIC assignmentExpression RBRACKET  (15)
    #                            | directAbstractDeclarator LBRACKET gccArrayTypeModifierList RBRACKET                              (16)
    #                            | directAbstractDeclarator LBRACKET assignmentExpression RBRACKET                                  (17)
    #                            | LPAREN_SCOPE RPAREN_SCOPE                                                                        (18)
    #                            | LPAREN_SCOPE parameterTypeList RPAREN_SCOPE                                                      (19)
    #                            | directAbstractDeclarator LPAREN_SCOPE RPAREN_SCOPE                                               (20)
    #                            | directAbstractDeclarator LPAREN_SCOPE parameterTypeList RPAREN_SCOPE                             (21)
    #
    # ( 1)         justifies a recursive call
    # ( 2) to ( 9) give the (array) modifiers
    # (10) to (17) justifies a recursive call, plus give the (array) modifiers
    # (18) to (19) give the arguments
    # (20) to (21) justifies a recursive call and give the arguments
    #
    my $firstElement = $directAbstractDeclarator->[0];
    my $firstElementBlessed = blessed($firstElement) || '';
    my $firstElementReftype = reftype($firstElement) || '';

    my $secondElement = $directAbstractDeclarator->[0];
    my $secondElementBlessed = blessed($secondElement) || '';
    my $secondElementReftype = reftype($secondElement) || '';

    my $lastButOneElement = $directAbstractDeclarator->[-2];
    my $lastButOneElementBlessed = blessed($lastButOneElement) || '';
    my $lastButOneElementReftype = reftype($lastButOneElement) || '';

    #
    # Lexeme available: check if position is ok
    #
    if (! $firstElementBlessed && ! $self->_positionOk($firstElement->[0])) {
	return 0;
    }
    if (! $secondElementBlessed && ! $self->_positionOk($secondElement->[0])) {
	return 0;
    }

    if (! $firstElementBlessed && $firstElementReftype eq 'ARRAY' && $firstElement->[2] eq '(' &&
	$secondElementBlessed eq 'C::AST:abstractDeclarator') {
	#
	# directAbstractDeclarator ::= LPAREN abstractDeclarator RPAREN                                                                 ( 1)
	#
	if (! $self->_analyseAbstractDeclarator($stdout_buf, $secondElement, $rcp)) {
	    return 0;
	}
    }
    elsif (! $firstElementBlessed && $firstElementReftype eq 'ARRAY' && $firstElement->[2] eq '[') {
	#
	#                            | LBRACKET RBRACKET                                                                                ( 2)
	#                            | LBRACKET STAR RBRACKET                                                                           ( 3)
	#                            | LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET                           ( 4)
	#                            | LBRACKET STATIC assignmentExpression RBRACKET                                                    ( 5)
	#                            | LBRACKET gccArrayTypeModifierList STATIC assignmentExpression RBRACKET                           ( 6)
	#                            | LBRACKET gccArrayTypeModifierList assignmentExpression RBRACKET                                  ( 7)
	#                            | LBRACKET gccArrayTypeModifierList RBRACKET                                                       ( 8)
	#                            | LBRACKET assignmentExpression RBRACKET                                                           ( 9)
	#
	if (! $self->_definedRcp($rcp, 'mod')) {
	    #
	    # This should not be already defined in theory since we do not allow more than one recursion
	    #
	    my $startPosition = $directAbstractDeclarator->[0]->[0];
	    my $endPosition = $self->_endPosition($directAbstractDeclarator);
	    $self->_setRcp($rcp, 'mod', substr($stdout_buf, $startPosition, $endPosition - $startPosition + 1));
	}
    }
    elsif ($firstElementBlessed eq 'C::AST::directAbstractDeclarator' &&
	   ! $secondElementBlessed && $secondElementReftype eq 'ARRAY' && $secondElement->[2] eq '[') {
	#
	#                            | directAbstractDeclarator LBRACKET RBRACKET                                                       (10)
	#                            | directAbstractDeclarator LBRACKET STAR RBRACKET                                                  (11)
	#                            | directAbstractDeclarator LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET  (12)
	#                            | directAbstractDeclarator LBRACKET STATIC assignmentExpression RBRACKET                           (13)
	#                            | directAbstractDeclarator LBRACKET gccArrayTypeModifierList assignmentExpression RBRACKET         (14)
	#                            | directAbstractDeclarator LBRACKET gccArrayTypeModifierList STATIC assignmentExpression RBRACKET  (15)
	#                            | directAbstractDeclarator LBRACKET gccArrayTypeModifierList RBRACKET                              (16)
	#                            | directAbstractDeclarator LBRACKET assignmentExpression RBRACKET                                  (17)
	#
	if (! $self->_analyseDirectAbstractDeclarator($stdout_buf, $firstElement, $rcp)) {
	    return 0;
	}
	if (! $self->_definedRcp($rcp, 'mod')) {
	    #
	    # This should not be already defined in theory since we do not allow more than one recursion
	    #
	    my $startPosition = $directAbstractDeclarator->[1]->[0];
	    my $endPosition = $self->_endPosition($directAbstractDeclarator);
	    $self->_setRcp($rcp, 'mod', substr($stdout_buf, $startPosition, $endPosition - $startPosition + 1));
	}
    } else {
	#                            | LPAREN_SCOPE RPAREN_SCOPE                                                                        (18)
	#                            | LPAREN_SCOPE parameterTypeList RPAREN_SCOPE                                                      (19)
	#                            | directAbstractDeclarator LPAREN_SCOPE RPAREN_SCOPE                                               (20)
	#                            | directAbstractDeclarator LPAREN_SCOPE parameterTypeList RPAREN_SCOPE                             (21)
	$self->_setRcp($rcp, 'func', 1);
	#
	# When an rcp becomes a func, everything that was a type before becomes a return type
	#
	$self->_appendRcp($rcp, 'rt', $self->_getRcp($rcp, 'ty'));
	$self->_deleteRcp($rcp, 'ty');
	if ($firstElementBlessed eq 'C::AST::directAbstractDeclarator') {
	    if (! $self->_analyseDirectAbstractDeclarator($stdout_buf, $firstElement, $rcp)) {
		return 0;
	    }
	}
	if ($lastButOneElementBlessed eq 'C::AST::parameterTypeList') {
	    #
	    # This is a new scope: we intentionnaly do not propagate $topLevelInfop
	    #
	    $self->_setRcp($rcp, 'args', []);
	    if (! $self->_analyseParameterTypeList($stdout_buf, $lastButOneElement, $self->_getRcp($rcp, 'args'))) {
		return 0;
	    }
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseParameterTypeList {
    my ($self, $stdout_buf, $parameterTypeList, $listp) = @_;
    #
    # parameterTypeList is:
    #
    # parameterTypeList ::= parameterList COMMA ELLIPSIS
    #                     | parameterList
    #
    #
    # Lexeme available: check if position is ok
    #
    if ($#{$parameterTypeList} > 0 && ! $self->_positionOk($parameterTypeList->[1]->[0])) {
	return 0;
    }

    my $parameterList = $parameterTypeList->[0];
    if (! $self->_analyseParameterList($stdout_buf, $parameterList, $listp)) {
	return 0;
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseIdentifierList {
    my ($self, $stdout_buf, $identifierList, $listp) = @_;
    #
    # identifierList is:
    #
    # identifierList ::= IDENTIFIER
    #                  | identifierList COMMA IDENTIFIER
    #
    #
    # Lexeme available: check if position is ok
    #
    if (! $self->_positionOk($identifierList->[-1]->[0])) {
	return 0;
    }

    if ($#{$identifierList} > 0) {
	if (! $self->_analyseIdentifierList($stdout_buf, $identifierList->[0], $listp)) {
	    return 0;
	}
    }
    my $newRcp = $self->_newRcp();
    my $identifier = $identifierList->[-1]->[2];
    $self->_setRcp($newRcp, 'nm', $identifier);
    #
    # Push identifier
    #
    $self->_pushRcp($stdout_buf, $identifierList, $newRcp, $listp);

    return 1;
}

# ----------------------------------------------------------------------------------------
sub _newRcp {
    my ($self) = @_;

    return $self->{_asHash} ? {} : [];

}

# ----------------------------------------------------------------------------------------

sub _analyseParameterList {
    my ($self, $stdout_buf, $parameterList, $listp) = @_;
    #
    # parameterList is:
    #
    # parameterList ::= parameterDeclaration
    #                 | parameterList COMMA parameterDeclaration

    #
    # Lexeme available: check if position is ok
    #
    if ($#{$parameterList} > 0 && ! $self->_positionOk($parameterList->[1]->[0])) {
	return 0;
    }


    foreach (@{$parameterList}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::parameterDeclaration') {
	    if (! $self->_analyseParameterDeclaration($stdout_buf, $_, $listp)) {
		return 0;
	    }
	} elsif ($blessed eq 'C::AST::parameterList') {
	    if (! $self->_analyseParameterList($stdout_buf, $_, $listp)) {
		return 0;
	    }
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseParameterDeclaration {
    my ($self, $stdout_buf, $parameterDeclaration, $listp) = @_;
    #
    # parameterDeclaration is:
    #
    # parameterDeclaration ::= parameterDeclarationCheck
    #                        | declarationSpecifiers abstractDeclarator
    #                        | declarationSpecifiers
    #
    # Note: abstractDeclarator is a declarator without an identifier
    #
    if (blessed($parameterDeclaration->[0]) eq 'C::AST::parameterDeclarationCheck') {
	my $parameterDeclarationCheck = $parameterDeclaration->[0];
	if (! $self->_analyseParameterDeclarationCheck($stdout_buf, $parameterDeclarationCheck, $listp)) {
	    return 0;
	}
    } else {
	my $declarationSpecifiers = $parameterDeclaration->[0];
	my $contextp = $self->_newRcp();
	if (! $self->_buildContext($stdout_buf, $declarationSpecifiers, $contextp, $listp)) {
	    return 0;
	}
	my $newRcp = $self->_newRcp();
	if ($#{$parameterDeclaration} > 0) {
	    my $abstractDeclarator = $parameterDeclaration->[1];
	    if (! $self->_analyseAbstractDeclarator($stdout_buf, $abstractDeclarator, $newRcp)) {
		return 0;
	    }
	}
	#
	# Per-def there is no name attached. This is a parameter: we want arg%d instead of ANON%d.
	#
	if (! $self->_definedRcp($newRcp, 'nm')) {
	    $self->_setRcp($newRcp, 'nm', sprintf('arg%d', scalar(@{$listp})));
	}
	#
	# Push parameterDeclaration
	#
	$self->_pushRcp($stdout_buf, $parameterDeclaration, $newRcp, $listp, $contextp);
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseParameterDeclarationCheck {
    my ($self, $stdout_buf, $parameterDeclarationCheck, $listp) = @_;
    #
    # parameterDeclarationCheck is:
    #
    # parameterDeclarationCheck ::= parameterDeclarationdeclarationSpecifiers declarator
    # parameterDeclarationdeclarationSpecifiers ::= declarationSpecifiers
    #
    # i.e. we are back to a routine very similar to _analyseInitDeclarator...
    #
    my $declarationSpecifiers = $parameterDeclarationCheck->[0]->[0];
    my $contextp = $self->_newRcp();
    if (! $self->_buildContext($stdout_buf, $declarationSpecifiers, $contextp, $listp)) {
	return 0;
    }
    my $newRcp = $self->_newRcp();
    my $declarator  = $parameterDeclarationCheck->[1];
    if (!$self->_analyseDeclarator($stdout_buf, $declarator, $newRcp)) {
	return 0;
    }
    #
    # Push parameterDeclarationCheck
    #
    $self->_pushRcp($stdout_buf, $parameterDeclarationCheck, $newRcp, $listp, $contextp);

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _analyseAbstractDeclarator {
    my ($self, $stdout_buf, $abstractDeclarator, $rcp) = @_;
    #
    # abstractDeclarator is:
    #
    # abstractDeclarator ::= pointer msvsAttributeAny directAbstractDeclarator                  (1)
    #                      | pointer msvsAttributeAny directAbstractDeclarator gccAsmExpression (2)
    #                      | pointer msvsAttributeAny                                           (3)
    #                      | directAbstractDeclarator                                           (4)
    #                      | directAbstractDeclarator gccAsmExpression                          (5)
    #
    my $firstElementBlessed = blessed($abstractDeclarator->[0]) || '';
    if ($firstElementBlessed eq 'C::AST::pointer') {
	my $pointers = $self->_concatPointers($abstractDeclarator->[0]);
	$self->_appendRcp($rcp, 'ty', $pointers);
    }

    my $directAbstractDeclarator = undef;
    foreach (@{$abstractDeclarator}) {
	my $blessed = blessed($_) || '';
	if ($blessed eq 'C::AST::directAbstractDeclarator') {
	    $directAbstractDeclarator = $_;
	}
    }
    if (defined($directAbstractDeclarator)) {
	if (! $self->_analyseDirectAbstractDeclarator($stdout_buf, $directAbstractDeclarator, $rcp)) {
	    return 0;
	}
    }

    return 1;
}

# ----------------------------------------------------------------------------------------

sub _lexemes {
    my ($self, $o) = @_;

    my @lexemes = ();
    MarpaX::Languages::C::AST::Util::Data::Find->new
	(
	 wanted => sub {
	     my $o = shift;
	     my $reftype = reftype($o) || '';
	     return (! defined(blessed($o)) && $reftype eq 'ARRAY');
	 },
	 callback => sub {
	     my ($self, $o) = @_;
	     push(@lexemes, $o->[2]);
	 },
	 callbackArgs => [ $self ],
	)->process($o);

    return @lexemes;
}

# ----------------------------------------------------------------------------------------

sub _concatPointers {
    my ($self, $pointer) = @_;
    #
    # pointer is:
    #
    # pointer ::= msvsAttributeAny STAR pointerQualifierList pointer
    #           | msvsAttributeAny STAR pointerQualifierList
    #           | msvsAttributeAny STAR pointer
    #           | msvsAttributeAny STAR
    #

    my $stars = '';
    MarpaX::Languages::C::AST::Util::Data::Find->new
	(
	 wanted => sub {
	     my $o = shift;
	     my $blessed = blessed($o) || '';
	     my $reftype = reftype($o) || '';
	     return (! $blessed && $reftype eq 'ARRAY');
	 },
	 callback => sub {
	     my ($self, $o) = @_;
	     my $lexemeValue = $o->[2];
	     if ($lexemeValue eq '*') {
		 $stars .= $lexemeValue;
	     }
	 },
	 callbackArgs => [ $self ],
	)->process($pointer);

    return $stars;
}

# ----------------------------------------------------------------------------------------

sub _lexemeCallback {
  my ($lexemeCallbackHashp, $lexemeHashp) = @_;

  my $self = $lexemeCallbackHashp->{self};
  my $tmpHashp = $lexemeCallbackHashp->{tmpHashp};

  #
  # We wait until the first #line information: this will give the name of current file
  #
  if ($lexemeHashp->{name} eq 'PREPROCESSOR_LINE_DIRECTIVE') {
    if ($lexemeHashp->{value} =~ /[\d]+\s*\"([^\"]+)\"/) {
	my $currentFile = File::Spec->canonpath(substr($lexemeHashp->{value}, $-[1], $+[1] - $-[1]));
	#
	# It can very well be that current file from cpp point of view is an internal thing.
	# Not a real file. For example: '<command-line>', says GCC.
	#
	$tmpHashp->{_currentFile} = $currentFile;
	$self->{_position2File}->{$lexemeHashp->{start}} = $tmpHashp->{_currentFile};
	$tmpHashp->{_includes}->{$tmpHashp->{_currentFile}}++;
    }
    #
    # This is an internal lexeme, no problem to change a bit the value. For instance, remove
    # \s if any.
    #
    $lexemeHashp->{value} =~ s/^\s*//g;
    $lexemeHashp->{value} =~ s/\s*$//g;
    $lexemeHashp->{value} =~ s/\n/\\n/g;
  }

  #
  # We cannot use $self->_positionOk() here because it is being constructed
  # We could have used a traversal of the final AST to find strings.
  # But I do it here because strings are exactly lexemes, so this can be done
  # at lexing step.
  #
  if (defined($tmpHashp->{_currentFile})) {
      if ((exists($self->{_filename_filter_re}) && $tmpHashp->{_currentFile} =~ $self->{_filename_filter_re}) ||
	  $tmpHashp->{_currentFile} eq $self->{_filename_filter}) {
	  if ($lexemeHashp->{name} eq 'STRING_LITERAL_UNIT') {
	      #
	      # ISO C permits WS at the end of a string literal, we remove it
	      #
	      my $string = $lexemeHashp->{value};
	      $string =~ s/[ \t\v\n\f]*$//;
	      push(@{$self->{_strings}}, $string);
	  }
      }
  }
}

# ----------------------------------------------------------------------------------------

sub _positionOk {
    my ($self, $position) = @_;

    #
    # A position is OK if:
    # position is known and passes filename_filter, or
    # previous known position passes filename_filter
    #
    if (exists($self->{_position2File}->{$position})) {
	if (exists($self->{_filename_filter_re})) {
	    return $self->{_position2File}->{$position} =~ $self->{_filename_filter_re};
	} else {
	    return $self->{_position2File}->{$position} eq $self->{_filename_filter};
	}
    }
    my $previousPosition = undef;
    foreach (@{$self->{_sortedPosition2File}}) {
	if ($_ <= $position) {
	    if (! defined($previousPosition) || $_ > $previousPosition) {
		$previousPosition = $_;
	    }
	} else {
	    last;
	}
    }
    if (! defined($previousPosition)) {
	return 0;
    }
    return $self->_positionOk($previousPosition);
}

# ----------------------------------------------------------------------------------------

sub _analyse_with_heuristics {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_content})) {
      #
      # Case where it was a filename given.
      # Per-def $self->{_tmpfh} is at the beginning of file at this time
      #
      $self->{_content} = do {my $fh = $self->{_tmpfh}; local $/; <$fh>;};
  }

  $self->{_macros} = [];
  pos($self->{_content}) = undef;
  while ($self->{_content} =~ m/$REDEFINE/g) {
      my $start = $-[1];
      my $end = $+[1];
      push(@{$self->{_macros}}, substr($self->{_content}, $start, $end - $start));
  }
}

# ----------------------------------------------------------------------------------------

sub _posprocess_heuristics {
    my ($self) = @_;

    #
    # We want to have defines_args and defines_no_args
    #
    $self->{_defines_args} = {};
    $self->{_defines_no_args} = {};
    foreach (@{$self->macros}) {
	if (/^(\w+)\s*$BALANCEDPARENS\s*(.*)/s) {
	    my $name  = substr($_, $-[1], $+[1] - $-[1]);
	    my $args  = substr($_, $-[2], $+[2] - $-[2]);
	    my $value = substr($_, $-[3], $+[3] - $-[3]);
	    substr($args,  0, 1, '');  # '('
	    substr($args, -1, 1, '');  # ')'
	    my @args = map {s/\s//g; $_;} split(/,/, $args);
	    $self->{_defines_args}->{$name} = [ [ @args ], $value ];
	} else {
	    /(\w+)\s*(.*)/s;
	    my $name  = substr($_, $-[1], $+[1] - $-[1]);
	    my $value = substr($_, $-[2], $+[2] - $-[2]);
	    $self->{_defines_no_args}->{$name} = $value;
	}
    }
}

=head1 NOTES

There is no default for function return type without any type specifier: the C standard says this is (also, bw the way) 'int'.

=head1 SEE ALSO

L<Config>

L<MarpaX::Languages::C::AST>

L<C::Scan>

L<File:Temp>

L<C::Tokenize>

L<ModPerl::CScan>

=cut

1;
