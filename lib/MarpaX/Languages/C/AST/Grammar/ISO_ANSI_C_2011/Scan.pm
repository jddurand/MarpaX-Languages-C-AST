use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011::Scan;

# ABSTRACT: Scan C source

use MarpaX::Languages::C::AST;
use Config;
use Carp qw/croak/;
use IPC::Cmd qw/run/;
use File::Temp qw/tempfile/;
use IO::File;
use Scalar::Util qw/blessed reftype/;
use Regexp::Common;
use String::ShellQuote qw/shell_quote_best_effort/;  # Not for Win32, but passes everywhere, so ok to use it like that
use Log::Any qw/$log/;
use constant {
    LEXEME_POSITION_INDEX => 0,
    LEXEME_LENGTH_INDEX => 1,
    LEXEME_VALUE_INDEX => 2
};
use MarpaX::Languages::C::AST::Grammar::ISO_ANSI_C_2011::Scan::Actions;
use File::ShareDir::ProjectDistDir 1.0 ':all', strict => 1;
use File::Find qw/find/;
use File::Spec;
use File::Basename qw/basename/;
use Unicode::CaseFold;
use XML::LibXML;
use XML::LibXSLT;
use constant { TYPE => 0, QUALIFIER => 1, IDENTIFIER => 2, OTHER => 3, SKIPPED => 4 };

our $HAVE_SYS__INFO = eval 'use Sys::Info; 1' || 0;
our $HAVE_Win32__ShellQuote = _is_windows() ? (eval 'use Win32::ShellQuote qw/quote_native/; 1' || 0) : 0;
our $RESAMELINE = qr/(?:[ \t\v\f])*/;                        # i.e. WS* without \n
our $REDEFINE = qr/^${RESAMELINE}#${RESAMELINE}define${RESAMELINE}((\w+)(?>[^\n\\]*)(?>\\.[^\n\\]*)*)/ms; # dot-matches-all mode, keeping ^ meaningful
our $BALANCEDPARENS = qr/$RE{balanced}{-parens=>'()'}{-keep}/;

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

=head1 SUBROUTINES

=head2 new($class, %options)

Instantiate a new object. Parameters are in a hash that can contain the following keys:

=over

=item filename

File name to parse.

=item content

Content to parse.

=item filename_filter

Filter on filename from pre-processor output.

=item asDOM

Say that all C::Scan-like methods should return an xml document.

=item xpathDirectories

A reference to an array giving xpath directories that will have precedence over the shared directory installed with this module.

=item xsltDirectories

A reference to an array giving xslt directories that will have precedence over the shared directory installed with this module.

=item cpprun

Preprocessor command, default is $ENV{MARPAX_LANGUAGES_C_SCAN_CPPRUN}, or $Config{cpprun}. It is assume that cpprun is already correctly quoted for your system shell.

=item cppflags

Preprocessor flags, default is $ENV{MARPAX_LANGUAGES_C_SCAN_CPPFLAGS}, $Config{cppflags}. It is assume that cppflags is already correctly quoted for your system shell.

=back

Please refer to the Config perl documentation for the meaning of $Config{cpprun} or $Config{cppflags}.

This module will execute "$cpprun $cppflags $filename", using a temporary filename if $content was given. Thus a working precompiler is required.

$filename and $content are mutually exclusive. If $content is used a temporary file will be created using File::Temp (which may fail under taint mode -;).

The $filename_filter value limits the output to file names equal to $filename_filter (if this is a SCALAR) or matching $filename_filter (if this is a Regexp): since we use the precompiler, any #include statements is "polluting" the original source, i.e. much more files that just $filename (or $content) are used. Default value is $filename or the generated temporary filename when using $content mode.

The methods defines_args() and defines_no_args() are not subject to the filename_filter parameter: they always apply on the content or filename given /before/ the preprocessing. They are based on heuristic parsing, so their result should not be blindly trusted. A typical example of false positive is a macro inside string or a comment.

This module will croak on any error.

=back

=head1 METHODS

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
              _asDOM            => exists($opts{asDOM})             ? $opts{asDOM}               : undef,
              _xpathDirectories => exists($opts{xpathDirectories})  ? $opts{xpathDirectories}    : [],
              _xsltDirectories  => exists($opts{xsltDirectories})   ? $opts{xsltDirectories}     : [],
              _filename_filter  => exists($opts{filename_filter}  ) ? $opts{filename_filter}     : undef,
              _cpprun           => exists($opts{cpprun})            ? $opts{cpprun}              : ($ENV{MARPAX_LANGUAGES_C_SCAN_CPPRUN} || $Config{cpprun}),
              _cppflags         => exists($opts{cppflags})          ? $opts{cppflags}            : ($ENV{MARPAX_LANGUAGES_C_SCAN_CPPFLAGS} || $Config{cppflags}),
             };

  #
  # For anonymous enums or structs, so that their names do not clash
  #
  $self->{_anonCount} = 0;

  if (exists($opts{content})) {
    if (! defined($opts{content})) {
      croak 'Undefined content';
    }
    $self->{_content2fh} = File::Temp->new(UNLINK => 1, SUFFIX => '.c');
    my $filename = $self->{_orig_filename} = $self->{_content2fh}->filename;
    #
    # We open twice the temporary file to make sure it is not deleted
    # physically on disk and still visible for our process
    #
    $self->{_tmpfh} = IO::File->new($filename, 'r') || croak "Cannot open $filename, $!";
    print($self->{_content2fh}, $opts{content});
    close($self->{_content2fh}) || warn "Cannot close $self->{_content2fh}, $!";
    $self->{_content} = $opts{content};
  } else {
    if (! exists($opts{filename}) || ! defined($opts{filename})) {
      croak 'Undefined filename';
    }
    my $filename = $self->{_orig_filename} = $opts{filename};
    $self->{_tmpfh} = IO::File->new($filename, 'r') || croak "Cannot open $filename, $!";
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
      }
  }

  bless($self, $class);

  $self->_init();

  #
  # This will unlink temporary file
  #
  delete($self->{_tmpfh});
  delete($self->{_content2fh});
  #
  # Delete what is left
  #
  delete($self->{_content});
  delete($self->{_anonCount});

  return $self;
}

# ----------------------------------------------------------------------------------------

=head2 ast($self)

AST of the preprocessed output. This is an XML::LibXML document.

=cut

sub ast {
  my $self = shift;

  return $self->{_ast};
}

# ----------------------------------------------------------------------------------------

=head2 astToString($self)

Stringified AST of the preprocessed output. This is an XML::LibXML document passed through its toString(1) method.

=cut

sub astToString {
  my $self = shift;

  return $self->ast()->toString(1);
}

# ----------------------------------------------------------------------------------------

=head2 get($self, $attribute)

C::Scan like method, that is a proxy to $self->$attribute. All methods described after can be used as attribute, for example: $self->get('strings'), or $self->get('includes').

=cut

sub get {
  my ($self, $attribute) = @_;

  if ($attribute eq 'get' ||
      $attribute eq 'new') {
    croak "$attribute attribute is not supported";
  }

  return $self->$attribute;
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

=head2 defines_args($self)

Returns a reference to hash of macros with arguments. The values are references to an array of length 2, the first element is a reference to the list of arguments, the second one being the expansion.

=cut

sub defines_args {
  my ($self) = @_;

  return $self->{_defines_args};
}

# ----------------------------------------------------------------------------------------

=head2 defines_no_args($self)

Returns a reference to hash of macros without arguments.

=cut

sub defines_no_args {
  my ($self) = @_;

  return $self->{_defines_no_args};
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

=head2 fdecls($self)

C::Scan compatible reference to a list of parsed declarations of functions.

=cut

sub fdecls {
  my ($self) = @_;

  return $self->{_fdecls};
}

# ----------------------------------------------------------------------------------------

=head2 inlines($self)

C::Scan compatible reference to a list of definitions of functions.

=cut

sub inlines {
  my ($self) = @_;

  return $self->{_inlines};
}

# ----------------------------------------------------------------------------------------

=head2 parsed_fdecls($self)

C::Scan NOT-FULLY compatible reference to list of parsed declarations of functions: the type of arguments consist only of type specifiers as per the grammar. For instance pointers are not in argument types: strictly speaking pointers are part of a declarator.

=cut

sub parsed_fdecls {
  my ($self) = @_;

  return $self->{_parsed_fdecls};
}

# ----------------------------------------------------------------------------------------

=head2 typedef_hash($self)

Reference to a hash which contains known typedefs as keys. Values of the hash are array references of length 2, with what should be put before/after the type for a standalone typedef declaration (but without the typedef substring). Note that it is the minimal full text of the C source that is used to obtain the before/after strings, so this /can/ contain definition of other variables.

=cut

sub typedef_hash {
  my ($self) = @_;

  return $self->{_typedef_hash};
}

# ----------------------------------------------------------------------------------------

=head2 typedef_texts($self)

Returns a reference to a list which contains known expansions of typedefs. This is just the first indice from "value" part of typedef_hash.

=cut

sub typedef_texts {
  my ($self) = @_;

  return $self->{_typedef_texts};
}

# ----------------------------------------------------------------------------------------

=head2 typedefs_maybe($self)

Returns a reference to a list of typedefed names. This is just the "key" part of typedef_hash. The name "maybe" is kept for compatibility with C::Scan.

=cut

sub typedefs_maybe {
  my ($self) = @_;

  return $self->{_typedefs_maybe};
}

# ----------------------------------------------------------------------------------------

=head2 vdecls($self)

Returns a reference to a list of extern variable declarations.

=cut

sub vdecls {
  my ($self) = @_;

  return $self->{_vdecls};
}

# ----------------------------------------------------------------------------------------

=head2 vdecl_hash($self)

Reference to a hash of parsed extern variable declarations, containing the variable names as keys. Values of the hash are array references of length 2, with what should be put before/after the name for a standalone extern variable declaration (but without the extern substring). Note that it is the minimal full text of the C source that is used to obtain the before/after strings, so this /can/ contain definition of other variables.

=cut

sub vdecl_hash {
  my ($self) = @_;

  return $self->{_vdecl_hash};
}

# ----------------------------------------------------------------------------------------

=head2 typedef_structs($self)

Hopefully C::Scan compatible reference to a hash which contains known typedefs as keys. The values of the hash may not be compatible with C::Scan output. In our case these are array references of length 2, with at index 0 the full text used to parsed this typedef (maybe inclusing more than needed, but always what is necessary), and at index 1 an empty string.

=cut

sub typedef_structs {
  my ($self) = @_;

  return $self->{_typedef_structs};
}

# ----------------------------------------------------------------------------------------

=head2 topDeclarations($self)

All top-level declarations. This is a XML::LibXML::Document containing a list of declaration children. Available only when asDOM option is a true value.

=cut

sub topDeclarations {
  my ($self) = @_;

  if ($self->{_asDOM}) {
    return $self->{_topDeclarations};
  }
  $log->warnf('topDeclarations is available only when asDOM option is a true value');
  return undef;
}

# ----------------------------------------------------------------------------------------
# Brutal copy of String::ShellQuote::quote_literal

sub _quote_literal {
    my ($text, $force) = @_;

    # basic argument quoting.  uses backslashes and quotes to escape
    # everything.
    if (!$force && $text ne '' && $text !~ /[ \t\n\x0b"]/) {
        # no quoting needed
    }
    else {
        $text =~ s{(\\*)(?="|\z)}{$1$1}g;
        $text =~ s{"}{\\"}g;
        $text = qq{"$text"};
    }

    return $text;
}

# ----------------------------------------------------------------------------------------

sub _is_windows {
  my $rc;

  if ($HAVE_SYS__INFO) {
    my $info = Sys::Info->new;
    my $os   = $info->os();
    $rc = $os->is_windows;
  } else {
    if ($^O =~ /win32/i) {
      $rc = 1;
    } else {
      $rc = 0;
    }
  }

  return $rc;
}

# ----------------------------------------------------------------------------------------

sub _init {
    my ($self) = @_;

    #
    # Note that, because we do not know if cpprun or cppflags contain multiple things
    # we cannot use the array version of run(). So ye have to stringify ourself.
    # It is assumed (and is the case with %Config value), that cpprun and cppflags
    # will be already properly escaped.
    # Remains the filename that we do ourself.
    # Two big categories: Win32, others
    #
    my $quotedFilename;
    my $cmd = "$self->{_cpprun} $self->{_cppflags} ";
    if (_is_windows()) {
      if ($HAVE_Win32__ShellQuote) {
        $quotedFilename = quote_native($self->{_orig_filename});
      } else {
        $quotedFilename = _quote_literal($self->{_orig_filename}, 1);
      }
    } else {
      $quotedFilename = shell_quote_best_effort($self->{_orig_filename});
    }
    $cmd .= $quotedFilename;

    my ($success, $error_code, undef, $stdout_bufp, $stderr_bufp) = run(command => $cmd);

    if (! $success) {
      croak join('', @{$stderr_bufp});
    }

    my $stdout_buf = join('',@{$stdout_bufp});

    $self->_initInternals();
    $self->_analyse_with_grammar($stdout_buf);
    $self->_analyse_with_heuristics($stdout_buf);
    $self->_posprocess_heuristics();
    $self->_cleanInternals();

}

# ----------------------------------------------------------------------------------------

sub _initInternals {
    my ($self) = @_;

    $self->{_position2File} = {};
    $self->{_sortedPosition2File} = [];

}

# ----------------------------------------------------------------------------------------

sub _cleanInternals {
    my ($self) = @_;

    delete($self->{_position2File});
    delete($self->{_sortedPosition2File});

}

# ----------------------------------------------------------------------------------------

sub _getAst {
  my ($self, $stdout_buf) = @_;

  #
  # Temporary stuff
  #
  my %tmpHash = (_currentFile => undef, _includes => {});
  #
  # Get the AST, the lexeme callback will flag position2File to things of interest
  #
  $self->{_includes} = $self->{_asDOM} ? XML::LibXML::Document->new() : {};
  $self->{_strings} = $self->{_asDOM} ? XML::LibXML::Document->new() : [];
  #
  # Plus from our module: strings detection
  #
  my $value = MarpaX::Languages::C::AST->new
      (
       lexemeCallback => [ \&_lexemeCallback,
			   {self => $self,
			    tmpHashp => \%tmpHash,
			   }
       ],
       actionObject => sprintf('%s::%s', __PACKAGE__, 'Actions'),
       nonTerminalSemantic => ':default ::= action => nonTerminalSemantic',
      )->parse(\$stdout_buf)->value;
  $self->{_ast} = ${$value};

  #
  # For lookup, do a sorted version of position2File
  #
  $self->{_sortedPosition2File}  = [ map { [ $_, $self->{_position2File}->{$_} ] } sort { $a <=> $b } keys %{$self->{_position2File}} ];
  #
  # Includes was a hash in %tmpHash
  #
  if ($self->{_asDOM}) {
    foreach (sort keys %{$tmpHash{_includes}}) {
      my $child = XML::LibXML::Element->new('include');
      $self->{_includes}->addChild(XML::LibXML::Element->new('include'))->setAttribute('text', $_);
    }
  } else {
    $self->{_includes} = [ sort keys %{$tmpHash{_includes}} ];
  }
}

# ----------------------------------------------------------------------------------------

sub _position2File {
  my ($self, $position) = @_;

  my $file = '';
  if (! exists($ENV{MARPAX_LANGUAGES_C_AST_T_SCAN})) {
    #
    # In the test suite, we cannot rely on filename that is compiler+OS dependant
    #
    foreach (@{$self->{_sortedPosition2File}}) {
      if ($_->[0] > $position) {
        last;
      }
      $file = $_->[1];
    }
  }

  return $file;
}

# ----------------------------------------------------------------------------------------

sub _analyse_with_grammar {
  my ($self, $stdout_buf) = @_;

  #
  # We get the AST and call other methods
  #
  $self->_getAst($stdout_buf);

  #
  # We want to systematically provide a "text" attribute on all nodes
  #
  foreach ($self->ast()->findnodes($self->_xpath('allNodes.xpath'))) {
    #
    # In order to distringuish between a lexeme or not in the future, we remember
    # if there was originally a lexeme -;
    #
    my $text = $_->getAttribute('text');
    my $isLexeme = defined($text) ? 'true' : 'false';
    $_->setAttribute('isLexeme', $isLexeme);
    #
    # And file information, which is acting as a filter
    #
    $self->_pushNodeFile($stdout_buf, undef, $_, 1);
    $self->_pushNodeString($stdout_buf, undef, $_, 1);
  }


  # -------------------------------------------------------------------------------------------
  # Producing a C::Scan equivalent is just a matter of revisiting the XML, i.e. the AST's value
  #
  # C::Scan outputs can be:
  #
  # includes                   Handled as a callback during lexing
  # defines_args               Handled in _posprocess_heuristics()
  # defines_no_args            Handled in _posprocess_heuristics()
  # fdecls                     functions declarations. Handled here.
  # inlines                    functions definitions. Handled here.
  # parsed_fdecls              parsed functions declarations. Handled here.
  # typedef_hash               typedefs. Handled here.
  # typedef_texts              typedefs expansions. Handled here.
  # typedefs_maybe             Empty here.
  # vdecls                     List of extern variables declarations. Handled here.
  # vdecl_hash                 Parsed extern variables declarations. Handled here.
  # typedef_structs            Parsed struct declarations. Handled here.
  # -------------------------------------------------------------------------------------------

  $self->_ast2fdecls($stdout_buf);
  $self->_ast2inlines($stdout_buf);
  $self->_ast2parsed_fdecls($stdout_buf);
  $self->_ast2typedef_hash($stdout_buf);
  $self->_ast2typedef_texts($stdout_buf);
  $self->_ast2typedefs_maybe($stdout_buf);
  $self->_ast2vdecls($stdout_buf);
  $self->_ast2vdecl_hash($stdout_buf);
  $self->_ast2typedef_structs($stdout_buf);
  $self->_ast2topDeclarations($stdout_buf);
  $self->_ast2cdecl($stdout_buf);
}

# ----------------------------------------------------------------------------------------

sub _xpath {
  my ($self, $wantedFilename) = @_;

  if (! defined($self->{_xpath}->{$wantedFilename})) {
    my $found = 0;
    my @searchPath = (@{$self->{_xpathDirectories}}, File::Spec->catdir(dist_dir('MarpaX-Languages-C-AST'), 'xpath'));
    foreach (@searchPath) {
      #
      # The fact that filesystem could be case tolerant is not an issue here
      #
      my $filename = File::Spec->canonpath(File::Spec->catfile($_, $wantedFilename));
      $log->tracef('%s: trying with %s', $wantedFilename, $filename);
      {
        use filetest 'access';
        if (-r $filename) {
          if (! open(XPATH, '<', $filename)) {
            #
            # This should not happen
            #
            $log->warnf('Cannot open %s, %s', $filename, $!);
          } else {
            my $xpath = do {local $/; <XPATH>};
            if (! close(XPATH)) {
              $log->warnf('Cannot close %s, %s', $filename, $!);
            }
            #
            # Remove any blank outside of the xpath expression
            #
            $xpath =~ s/^\s*//;
            $xpath =~ s/\s*$//;
            $self->{_xpath}->{$wantedFilename} = eval {XML::LibXML::XPathExpression->new($xpath)};
            if ($@) {
              $log->warnf('Cannot evaluate xpath in %s, %s', $filename, $@);
              #
              # Make sure it is really undefined
              #
              $self->{_xpath}->{$wantedFilename} = undef;
            } else {
              #
              # Done
              #
              $log->infof('%s evaluated using %s', $wantedFilename, $filename);
	      $found = 1;
              last;
            }
          }
        }
      }
    }
    if (! $found) {
      croak "Cannot find or evaluate \"$wantedFilename\". Search path was: [" . join(', ', map {"\"$_\""} (@searchPath)) . ']';
    }
  }
  return $self->{_xpath}->{$wantedFilename};
}

# ----------------------------------------------------------------------------------------

sub _xslt {
  my ($self, $wantedFilename) = @_;

  if (! defined($self->{_xslt}->{$wantedFilename})) {
    my $found = 0;
    my @searchPath = (@{$self->{_xsltDirectories}}, File::Spec->catdir(dist_dir('MarpaX-Languages-C-AST'), 'xslt'));
    foreach (@searchPath) {
      #
      # The fact that filesystem could be case tolerant is not an issue here
      #
      my $filename = File::Spec->canonpath(File::Spec->catfile($_, $wantedFilename));
      $log->tracef('%s: trying with %s', $wantedFilename, $filename);
      {
        use filetest 'access';
        if (-r $filename) {
          $self->{_xslt}->{$wantedFilename} = eval {XML::LibXSLT->new()->parse_stylesheet_file($filename)};
          if ($@) {
            $log->warnf('Cannot evaluate xslt in %s, %s', $filename, $@);
            #
            # Make sure it is really undefined
            #
            $self->{_xslt}->{$wantedFilename} = undef;
          } else {
            #
            # Done
            #
            $log->infof('%s evaluated using %s', $wantedFilename, $filename);
            $found = 1;
            last;
          }
        }
      }
    }
    if (! $found) {
      croak "Cannot find or evaluate \"$wantedFilename\". Search path was: [" . join(', ', map {"\"$_\""} (@searchPath)) . ']';
    }
  }
  return $self->{_xslt}->{$wantedFilename};
}

# ----------------------------------------------------------------------------------------

sub _pushNodeString {
  my ($self, $stdout_buf, $outputp, $node, $setAttributes) = @_;

  $setAttributes //= 0;

  #
  # Unless the node is already a lexeme, we have to search surrounding lexemes
  # This routine assumes that $outputp is always a reference to either an array or a scalar
  #
  my $text = $node->getAttribute('text');
  if (defined($text)) {
    #
    # Per def text, start and length attributes already exist
    #
    if (defined($outputp)) {
      if (ref($outputp) eq 'ARRAY') {
	push(@{$outputp}, $text);
      } elsif (ref($outputp) eq 'SCALAR') {
	${$outputp} = $text;
      } else {
	croak "Expecting a reference to an array or a scalar, not a reference to " . (ref($outputp) || 'nothing');
      }
    }
    return $text;
  } else {
    #
    ## Get first and last lexemes positions
    #
    my $firstLexemeXpath = $self->_xpath('firstLexeme.xpath');
    my $lastLexemeXpath = $self->_xpath('lastLexeme.xpath');

    my $firstLexeme = $node->findnodes($firstLexemeXpath);
    my $lastLexeme = $node->findnodes($lastLexemeXpath);

    if ($firstLexeme && $lastLexeme) {
      my $startPosition = $firstLexeme->[0]->findvalue('./@start');
      my $endPosition = $lastLexeme->[0]->findvalue('./@start') + $lastLexeme->[0]->findvalue('./@length');
      my $length = $endPosition - $startPosition;
      my $text = substr($stdout_buf, $startPosition, $length);
      if (defined($outputp)) {
	if (ref($outputp) eq 'ARRAY') {
	  push(@{$outputp}, $text);
	} elsif (ref($outputp) eq 'SCALAR') {
	  ${$outputp} = $text;
	} else {
	  croak "Expecting a reference to an array or a scalar, not a reference to " . (ref($outputp) || 'nothing');
	}
      }
      if ($setAttributes) {
        $node->setAttribute('start', $startPosition);
        $node->setAttribute('length', $length);
        $node->setAttribute('text', $text);
      }
      return $text;
    } else {
      return undef;
    }
  }
}

# ----------------------------------------------------------------------------------------

sub _fileOk {
  my ($self, $file) = @_;

  my ($volume, $directories, $filename) = File::Spec->splitpath($file);

  my $rc = 0;

  if (exists($self->{_filename_filter_re})) {
    if (File::Spec->case_tolerant($volume)) {
      $rc = ($file =~ /$self->{_filename_filter_re}/i) ? 1 : 0;
    } else {
      $rc = ($file =~ $self->{_filename_filter_re}) ? 1 : 0;
    }
  } elsif (defined($self->{_filename_filter})) {
    $rc = (fc($file) eq fc($self->{_filename_filter})) ? 1 : 0;
  } else {
    $rc = 1;
  }

  return $rc;
}

# ----------------------------------------------------------------------------------------

sub _pushNodeFile {
  my ($self, $stdout_buf, $outputp, $node, $setAttribute) = @_;

  $setAttribute //= 0;

  #
  # Unless the node is already a lexeme, we have to search surrounding lexemes
  # This routine assumes that $outputp is always a reference to either an array or a scalar
  #
  # Get first lexeme position and return a false value only if filename filter is on and output does not match the filter
  #
  my $firstLexeme;
  if ($node->getAttribute('text')) {
    $firstLexeme = [$node];
  } else {
    my $firstLexemeXpath = $self->_xpath('firstLexeme.xpath');
    $firstLexeme = $node->findnodes($firstLexemeXpath);
  }
  my $file = '';

  if ($firstLexeme) {
    my $startPosition = $firstLexeme->[0]->findvalue('./@start');
    $file = $self->_position2File($startPosition);
  }

  if (defined($outputp)) {
    if (ref($outputp) eq 'ARRAY') {
      push(@{$outputp}, $file);
    } elsif (ref($outputp) eq 'SCALAR') {
      ${$outputp} = $file;
    } else {
      croak "Expecting a reference to an array or a scalar, not a reference to " . (ref($outputp) || 'nothing');
    }
  }

  if ($setAttribute) {
    $node->setAttribute('file', $file);
  }

  return $self->_fileOk($file);
}

# ----------------------------------------------------------------------------------------

sub _ast2fdecls {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_fdecls})) {
    #
    # We rely on parsed_fdecls
    #
    $self->_ast2parsed_fdecls($stdout_buf);
  }

  return $self->{_fdecls};
}

# ----------------------------------------------------------------------------------------

sub _ast2typedef_texts {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_typedef_texts})) {
    #
    # We rely on typedef_hash
    #
    $self->_ast2typedef_hash($stdout_buf);
  }

  return $self->{_typedef_texts};
}

# ----------------------------------------------------------------------------------------

sub _ast2typedefs_maybe {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_typedefs_maybe})) {
    #
    # We rely on typedef_hash
    #
    $self->_ast2typedef_hash($stdout_buf);
  }

  return $self->{_typedefs_maybe};
}

# ----------------------------------------------------------------------------------------

sub _ast2typedef_structs {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_typedef_structs})) {
    #
    # We rely on typedef_hash
    #
    $self->_ast2typedef_hash($stdout_buf);
  }

  return $self->{_typedef_structs};
}

# ----------------------------------------------------------------------------------------

sub _ast2vdecls {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_vdecls})) {
    #
    # We rely on vdecl_hash
    #
    $self->_ast2vdecl_hash($stdout_buf);
  }

  return $self->{_vdecls};
}

# ----------------------------------------------------------------------------------------

sub _removeWord {
  my ($self, $outputp, $toRemove) = @_;

  my $quotemeta = quotemeta($toRemove);
  ${$outputp} =~ s/^\s*$quotemeta\b\s*//;
  ${$outputp} =~ s/\s*\b$quotemeta\s*$//;
  ${$outputp} =~ s/\s*\b$quotemeta\b\s*/ /;
}

# ----------------------------------------------------------------------------------------

sub _ast2vdecl_hash {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_vdecl_hash})) {
    $self->{_vdecl_hash} = $self->{_asDOM} ? XML::LibXML::Document->new() : {};
    $self->{_vdecls} = $self->{_asDOM} ? XML::LibXML::Document->new() : [];
    #
    # a vdecl is a "declaration" node
    #
    foreach my $declaration ($self->ast()->findnodes($self->_xpath('vdecl.xpath'))) {
      my $file = '';
      if (! $self->_pushNodeFile($stdout_buf, \$file, $declaration)) {
        next;
      }
      #
      # Get first declarationSpecifiers
      #
      my @declarationSpecifiers = $declaration->findnodes($self->_xpath('firstDeclarationSpecifiers.xpath'));
      if (! @declarationSpecifiers) {
	#
	# Could be a static assert declaration
	#
	next;
      }
      my $vdecl = '';
      $self->_pushNodeString($stdout_buf, \$vdecl, $declaration);
      #
      # vdecl_hash does not have the extern keyword.
      #
      my $textForHash;
      $self->_pushNodeString($stdout_buf, \$textForHash, $declarationSpecifiers[0]);
      $self->_removeWord(\$textForHash, 'extern');

      if ($self->{_asDOM}) {
	my $child = XML::LibXML::Element->new('vdecl');
	$child->setAttribute('text', $vdecl);
	$child->setAttribute('file', $file);
        $self->{_vdecls}->addChild($child);
      } else {
        push(@{$self->{_vdecls}}, $vdecl);
      }
      #
      # variable name
      #
      my @declarator = $declaration->findnodes($self->_xpath('declaration2Declarator.xpath'));
      my @keys = ();
      my @before = ();
      my @after = ();
      foreach (@declarator) {
	my $declarator;
	$self->_pushNodeString($stdout_buf, \$declarator, $_);

	my @IDENTIFIER = $_->findnodes($self->_xpath('declarator2IDENTIFIER.xpath'));
	if (@IDENTIFIER) {
	  $self->_pushNodeString($stdout_buf, \@keys, $IDENTIFIER[0]);
	} else {
	  my $anon = sprintf('ANON%d', $self->{_anonCount}++);
	  push(@keys, $anon);
	}
	$declarator =~ /(.*)$keys[-1](.*)/;
        my $before = defined($-[1]) ? substr($declarator, $-[1], $+[1]-$-[1]) : '';
        my $after = defined($-[2]) ? substr($declarator, $-[2], $+[2]-$-[2]) : '';
	push(@before, ($before =~ /[^\s]/) ? ' ' . $before : '');
	push(@after, ($after =~ /[^\s]/) ? ' ' . $after : '');
      }
      if (! @keys) {
	push(@keys, sprintf('ANON%d', $self->{_anonCount}++));
	push(@before, '');
	push(@after, '');
      }
      foreach (0..$#keys) {
        if ($self->{_asDOM}) {
          my $child = XML::LibXML::Element->new('vdecl');
          $child->setAttribute('before', $textForHash . $before[$_]);
          $child->setAttribute('after', $after[$_]);
          $child->setAttribute('id', $keys[$_]);
          $child->setAttribute('file', $file);
          $self->{_vdecl_hash}->addChild($child);
        } else {
          $self->{_vdecl_hash}->{$keys[$_]} = [ $textForHash . $before[$_], $after[$_] ];
        }
      }
    }
  }

  return $self->{_vdecl_hash};
}

# ----------------------------------------------------------------------------------------

sub _ast2topDeclarations {
  my ($self, $stdout_buf) = @_;

  if ($self->{_asDOM} && ! defined($self->{_topDeclarations})) {
    $self->{_topDeclarations} = XML::LibXML::Document->new();
    my $declarationList = XML::LibXML::Element->new('declarationList');
    $self->{_topDeclarations}->addChild($declarationList);

    foreach ($self->ast()->findnodes($self->_xpath('topDeclarations.xpath'))) {
      my $declaration = $_;
      my $file;
      if (! $self->_pushNodeFile($stdout_buf, \$file, $_)) {
	next;
      }
      $declarationList->addChild($declaration->cloneNode(1));
    }
  }
}

# ----------------------------------------------------------------------------------------

=head2 cdecl($self)

Cdecl-like string of top-level declarations. Available only when asDOM option is atrue value. Returns a reference to an array.

=cut

sub cdecl {
  my ($self) = @_;

  if ($self->{_asDOM}) {
    return $self->{_cdecl};
  } else {
    $log->warnf('cdecl is available only when asDOM option is a true value');
    return undef;
  }
}

# ----------------------------------------------------------------------------------------

sub _ast2cdecl {
  my ($self, $stdout_buf) = @_;

  if ($self->{_asDOM} && ! defined($self->{_cdecl})) {
    $self->{_cdeclAnonNb} = 0;
    $self->{_cdecl} = [];
    #
    # We will analyse topDeclarations
    #
    foreach ($self->topDeclarations()->firstChild()->childNodes()) {
      push(@{$self->{_cdecl}}, $self->_topDeclaration2Cdecl($_, $stdout_buf));
    }
    delete($self->{_cdeclAnonNb});
  }
}

# ----------------------------------------------------------------------------------------

sub _topDeclaration2Cdecl {
  my ($self, $declaration, $stdout_buf) = @_;

  $self->_logCdecl('[>]_topDeclaration2Cdecl');

  #
  # For every declaration we scan all the lexemes, aka tokens.
  #
  my $allNodesXpath = $self->_xpath('allNodes.xpath');
  my @nodes = $declaration->findnodes($allNodesXpath);
  my $cdecl = '';
  my @stack = ();
  my $last = $self->_readToId($stdout_buf, \@nodes, \@stack, \$cdecl);
  $self->_parseDeclarator($stdout_buf, \@nodes, \@stack, \$cdecl, $last);

  $self->_logCdecl('[<]_topDeclaration2Cdecl', cdecl => $cdecl);

  return $cdecl;
}

sub _logCdecl {
  my ($self, $function, %h) = @_;

  #
  # Rework case of stack
  #
  if (exists($h{stack})) {
    $h{stack} = [ map { $_->{string} } @{$h{stack}} ];
  }
  $log->debugf('%s: %s', $function, \%h);
}

sub _checkPtr {
  my ($self, $tokensp, $stackp, $cdeclp) = @_;

  $self->_logCdecl('[>]_checkPtr', stack => $stackp, cdecl => $cdeclp);

  if (! @{$stackp}) {
    $self->_logCdecl('[<]_checkPtr', stack => $stackp, cdecl => $cdeclp);
    return;
  }

  my $t;
  for ($t = pop(@{$stackp});

       $t->{token}->localname() eq 'STAR';

       $t = pop(@{$stackp})) {
    ${$cdeclp} .= 'pointer to ';
  }
  push(@{$stackp}, $t);

  $self->_logCdecl('[<]_checkPtr', stack => $stackp, cdecl => $cdeclp);

}

sub _parseDeclarator {
  my ($self, $stdout_buf, $tokensp, $stackp, $cdeclp, $last) = @_;

  $self->_logCdecl('[>]_parseDeclarator', stack => $stackp, cdecl => $cdeclp);

  if ($last->{token}->localname() eq 'LBRACKET') {
    while ($last->{token}->localname() eq 'LBRACKET') {
      $last = $self->_readArraySize($stdout_buf, $tokensp, $cdeclp);
    }
  } elsif ($last->{token}->localname() eq 'LPAREN_SCOPE') {
    $last = $self->_readFunctionArgs($stdout_buf, $tokensp, $cdeclp);
  }
  $self->_checkPtr($tokensp, $stackp, $cdeclp);

  while (@{$stackp}) {
    my $t = pop(@{$stackp});
    if ($t->{token}->localname() eq 'LPAREN') {
      $last = $self->_getToken($stdout_buf, $tokensp);
      $self->_parseDeclarator($stdout_buf, $tokensp, $stackp, $cdeclp, $last); # Recursively parse this ( dcl )
    } else {
      ${$cdeclp} .= sprintf('%s ', $t->{string});
    }
  }

  $self->_logCdecl('[<]_parseDeclarator', stack => $stackp, cdecl => $cdeclp);

}

sub _readFunctionArgs {
  my ($self, $stdout_buf, $tokensp, $cdeclp) = @_;

  $self->_logCdecl('[>]_readFunctionArgs', cdecl => $cdeclp);

  my $last = $self->_getToken($stdout_buf, $tokensp);

  if ($last->{token}->localname() eq 'RPAREN_SCOPE') {
    ${$cdeclp} .= 'function returning ';
    $self->_logCdecl('[<]_readFunctionArgs', cdecl => $cdeclp);
    return;
  }

  #
  # Push back the token
  #
  unshift(@{$tokensp}, $last->{token});

  ${$cdeclp} .= 'function receiving (';

  do {
    my @stack = ();

    my $nextToken = $last->{token}->nextSibling();
    if (defined($nextToken) && $nextToken->localname() eq 'ELLIPSIS') {
      #
      # "..." is a special case in function arguments. Eat it.
      #
      ${$cdeclp} .= '... ';
      $last = $self->_getToken($stdout_buf, $tokensp);
    } else {

      $last = $self->_readToId($stdout_buf, $tokensp, \@stack, $cdeclp);
      $self->_parseDeclarator($stdout_buf, $tokensp, \@stack, $cdeclp, $last);

      if ($last->{token}->localname() eq 'COMMA') {
	${$cdeclp} .= 'and ';
      }
    }

  } while ($last->{token}->localname() eq 'COMMA');

  ${$cdeclp} .= ') and returning ';

  $self->_logCdecl('[<]_readFunctionArgs', cdecl => $cdeclp);
  return $self->_getToken($stdout_buf, $tokensp);
}

sub _readArraySize {
  my ($self, $stdout_buf, $tokensp, $cdeclp) = @_;

  $self->_logCdecl('[>]_readArraySize', cdecl => $cdeclp);

  #
  # Per def next token in the list is the one just after LBRACKET
  #
  my $last = $self->_getToken($stdout_buf, $tokensp);
  my $start = $last->{token}->getAttribute('start');
  my $end = 0;

  $self->_logCdecl('_parseDeclarator', last => $last);

  while ($last->{token}->localname() ne 'RBRACKET') {
    $end = $last->{token}->getAttribute('start') + $last->{token}->getAttribute('length');
    $last = $self->_getToken($stdout_buf, $tokensp);
  }
  my $size = '';
  if ($end > 0) {
    ${$cdeclp} .= sprintf('array[%s] of ', substr($stdout_buf, $start, $end - $start));
  } else {
    ${$cdeclp} .= sprintf('array[] of ');
  }

  $self->_logCdecl('[<]_readArraySize', cdecl => $cdeclp);

  return $self->_getToken($stdout_buf, $tokensp);
}

sub _readToId {
  my ($self, $stdout_buf, $tokensp, $stackp, $cdeclp) = @_;

  $self->_logCdecl('[>]_readToId', stack => $stackp, cdecl => $cdeclp);

  my $last;

  for ($last = $self->_getToken($stdout_buf, $tokensp);
       $last->{type} != IDENTIFIER;
       push(@{$stackp}, $last), $last = $self->_getToken($stdout_buf, $tokensp)) {}

  ${$cdeclp} .= sprintf('%s: ', $last->{string});

  $self->_logCdecl('[<]_readToId', stack => $stackp, cdecl => $cdeclp);

  return $self->_getToken($stdout_buf, $tokensp);
}

sub _classifyNode {
  my ($self, $stdout_buf, $token) = @_;

  my $last = {token => $token,
	      string => undef,
	      type => undef};

  $self->_logCdecl('[<]_classifyNode', name => $token->localname(), isLexeme => $token->getAttribute('isLexeme'), text => $token->getAttribute('text'));

  my $name = $token->localname();
  my $previous = $token->previousSibling();
  my $next = $token->nextSibling();
  my $isLexeme = $last->{token}->getAttribute('isLexeme') || 'false';


  if ($name eq 'CONST') { # We call const "read-only" to clarify
    $last->{string} = 'read-only';
  } else {
    $last->{string} = $last->{token}->getAttribute('text');
  }

  my $parent = $last->{token}->parentNode();
  my $parentName = $parent->localname();

  if ($name eq 'IDENTIFIER' || $name eq 'IDENTIFIER_UNAMBIGUOUS' || $name eq 'ELLIPSIS') {
    $last->{type} = IDENTIFIER;
  }
  #
  # Case of anonymous thingies
  #
  elsif ($name eq 'LCURLY' && defined($previous) && $previous->localname() eq 'structOrUnion') {
    $last->{string} = sprintf('__ANON%d', ++$self->{_cdeclAnonNb});
    $last->{type} = IDENTIFIER;
  } elsif ($name eq 'LCURLY' && defined($previous) && $previous->localname() eq 'ENUM') {
    $last->{string} = sprintf('__ANON%d', ++$self->{_cdeclAnonNb});
    $last->{type} = IDENTIFIER;
  } elsif ($name eq 'declarationSpecifiers' && $parentName eq 'parameterDeclaration' && ! defined($next)) {
    $last->{string} = sprintf('__ANON%d', ++$self->{_cdeclAnonNb});
    $last->{type} = IDENTIFIER;
  } elsif ($name eq 'msvsAttributeAny' && $parentName eq 'abstractDeclarator' && ! defined($next)) {
    $last->{string} = sprintf('__ANON%d', ++$self->{_cdeclAnonNb});
    $last->{type} = IDENTIFIER;
  } elsif ($name eq 'directAbstractDeclarator' && $parentName ne 'directAbstractDeclarator') {
    $last->{string} = sprintf('__ANON%d', ++$self->{_cdeclAnonNb});
    $last->{type} = IDENTIFIER;
  }
  #
  # Types stuff
  #
  elsif ($parentName eq 'typeQualifier') {
    $last->{type} = QUALIFIER;
  }
  elsif ($parentName eq 'structOrUnion' ||
      $parentName eq 'enumSpecifier' ||
      $parentName eq 'typeSpecifier1' ||
      $parentName eq 'typeSpecifier2' ||
      $parentName eq 'atomicTypeSpecifier' ||
      $parentName eq 'msvsBuiltinType' ||
      $parentName eq 'gccBuiltinType' ||
      $parentName eq 'gccTypeof') {
    $last->{type} = TYPE;
  }
  elsif ($isLexeme eq 'true') {
    $last->{type} = OTHER;
    if ($name eq 'STAR') {
      # Make string contain "pointer to", otherwise, qualified pointers would be printed as '*'
      $last->{string} = 'pointer to';
    }
  } else {
    $last->{type} = SKIPPED;
  }

  $self->_logCdecl('[>]_classifyNode', name => $token->localname(), isLexeme => $token->getAttribute('isLexeme'), text => $token->getAttribute('text'));

  return $last;
}

sub _getToken {
  my ($self, $stdout_buf, $tokensp) = @_;

  $self->_logCdecl('[>]_getToken');

  my $token;
  my $last;
  do {
    $token = shift(@{$tokensp});
    if (! defined($token)) {
      return undef;
    }
    $last = $self->_classifyNode($stdout_buf, $token);
  } while ($last->{type} == SKIPPED);

  $self->_logCdecl('[<]_getToken', name => $last->{token}->localname(), isLexeme => $last->{token}->getAttribute('isLexeme'), text => $last->{token}->getAttribute('text'));

  return $last;
}

# ----------------------------------------------------------------------------------------

sub _ast2typedef_hash {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_typedef_hash})) {
    $self->{_typedef_hash} = $self->{_asDOM} ? XML::LibXML::Document->new() : {};
    $self->{_typedef_texts} = $self->{_asDOM} ? XML::LibXML::Document->new() : [];
    $self->{_typedefs_maybe} = $self->{_asDOM} ? XML::LibXML::Document->new() : [];
    $self->{_typedef_structs} = $self->{_asDOM} ? XML::LibXML::Document->new() : {};
    #
    # typedef is a "declaration" node
    #
    foreach my $declaration ($self->ast()->findnodes($self->_xpath('typedef.xpath'))) {
      my $file;
      if (! $self->_pushNodeFile($stdout_buf, \$file, $declaration)) {
        next;
      }

      my @declarationSpecifiers = $declaration->findnodes($self->_xpath('firstDeclarationSpecifiers.xpath'));
      if (! @declarationSpecifiers) {
	#
	# Could be a static assert declaration
	#
	next;
      }
      my $text;
      my $declarationSpecifiers;
      $self->_pushNodeString($stdout_buf, \$text, $declaration);
      $self->_pushNodeString($stdout_buf, \$declarationSpecifiers, $declarationSpecifiers[0]);
      #
      # typedef_texts does not have the typedef keyword, neither what will contain typedef_hash
      #
      $self->_removeWord(\$text, 'typedef');
      $self->_removeWord(\$declarationSpecifiers, 'typedef');
      if ($self->{_asDOM}) {
	my $child = XML::LibXML::Element->new('typedef');
	$child->setAttribute('text', $text);
	$child->setAttribute('file', $file);
        $self->{_typedef_texts}->addChild($child);
      } else {
	push(@{$self->{_typedef_texts}}, $text);
      }
      #
      # typedef name
      #
      my @declarator = $declaration->findnodes($self->_xpath('declaration2Declarator.xpath'));
      my @keys = ();
      my @before = ();
      my @after = ();
      foreach (@declarator) {
	my $declarator;
	$self->_pushNodeString($stdout_buf, \$declarator, $_);

	my @IDENTIFIER = $_->findnodes($self->_xpath('declarator2IDENTIFIER.xpath'));
	$self->_pushNodeString($stdout_buf, \@keys, $IDENTIFIER[0]);
	$declarator =~ /(.*)$keys[-1](.*)/;
        my $before = defined($-[1]) ? substr($declarator, $-[1], $+[1]-$-[1]) : '';
        my $after = defined($-[2]) ? substr($declarator, $-[2], $+[2]-$-[2]) : '';
	push(@before, ($before =~ /[^\s]/) ? ' ' . $before : '');
	push(@after, ($after =~ /[^\s]/) ? ' ' . $after : '');
      }
      if (! @keys) {
	push(@keys, sprintf('ANON%d', $self->{_anonCount}++));
	push(@before, '');
	push(@after, '');
      }
      if ($self->{_asDOM}) {
	foreach (@keys) {
	  my $child = XML::LibXML::Element->new('typedef');
	  $child->setAttribute('id', $_);
	  $child->setAttribute('file', $file);
	  $self->{_typedefs_maybe}->addChild($child);
	}
      } else {
	push(@{$self->{_typedefs_maybe}}, @keys);
      }
      foreach (0..$#keys) {
	#
	# typedef before/after
	#
        if ($self->{_asDOM}) {
          my $child = XML::LibXML::Element->new('typedef');
          $child->setAttribute('id', $keys[$_]);
          $child->setAttribute('before', $declarationSpecifiers . $before[$_]);
          $child->setAttribute('after', $after[$_]);
          $child->setAttribute('file', $file);
          $self->{_typedef_hash}->addChild($child);
        } else {
	  $self->{_typedef_hash}->{$keys[$_]} = [ $declarationSpecifiers . $before[$_], $after[$_] ];
	}
      }
      #
      # Is a struct or union declaration ?
      #
      my @structOrUnionSpecifier = $declarationSpecifiers[0]->findnodes($self->_xpath('declarationSpecifiers2structOrUnionSpecifier.xpath'));
      if (@structOrUnionSpecifier) {
	my $struct = $self->{_asDOM} ? XML::LibXML::Document->new() : [];
	my $declsDOM = undef;

        my @structDeclaration = $structOrUnionSpecifier[0]->findnodes($self->_xpath('structOrUnionSpecifier2structDeclaration.xpath'));
        foreach (@structDeclaration) {

          my @specifierQualifierList = $_->findnodes($self->_xpath('structDeclaration2specifierQualifierList.xpath'));
	  if (! @specifierQualifierList) {
	    # Gcc extension
	    next;
	  }
          my $specifierQualifierList;
          $self->_pushNodeString($stdout_buf, \$specifierQualifierList, $specifierQualifierList[0]);

          my @structDeclarator = $_->findnodes($self->_xpath('structDeclaration2structDeclarator.xpath'));
          my @keys = ();
          my @before = ();
          my @after = ();
          foreach (@structDeclarator) {
            my $structDeclarator;
            $self->_pushNodeString($stdout_buf, \$structDeclarator, $_);

            my @IDENTIFIER = $_->findnodes($self->_xpath('structDeclarator2IDENTIFIER.xpath'));
	    if (@IDENTIFIER) {
	      $self->_pushNodeString($stdout_buf, \@keys, $IDENTIFIER[0]);
	    } else {
	      # COLON constantExpression
	      push(@keys, sprintf('ANON%d', $self->{_anonCount}++));
	    }
            $structDeclarator =~ /(.*)$keys[-1](.*)/;

            my $before = defined($-[1]) ? substr($structDeclarator, $-[1], $+[1]-$-[1]) : '';
            my $after = defined($-[2]) ? substr($structDeclarator, $-[2], $+[2]-$-[2]) : '';
            push(@before, $specifierQualifierList . (($before =~ /[^\s]/) ? ' ' . $before : ''));
            push(@after, $after);
          }
          if (! @keys) {
            push(@keys, sprintf('ANON%d', $self->{_anonCount}++));
            push(@before, '');
            push(@after, '');
          }
          foreach (0..$#keys) {
            #
            # structDeclarator before/after
            #
	    if ($self->{_asDOM}) {
              my $child = XML::LibXML::Element->new('decl');
              $child->setAttribute('id', $keys[$_]);
              $child->setAttribute('before', $before[$_]);
              $child->setAttribute('after', $after[$_]);
              $child->setAttribute('file', $file);
	      if (! defined($declsDOM)) {
		$declsDOM = XML::LibXML::Element->new('decls');
		$struct->addChild($declsDOM);
	      }
              $declsDOM->addChild($child);
	    } else {
	      push(@{$struct}, [ $before[$_], $after[$_], $keys[$_] ]);
	    }
          }
        }
        foreach (0..$#keys) {
          #
          # typedef before/after
          #
          if ($self->{_asDOM}) {
	    my $child = XML::LibXML::Element->new('struct');
	    $child->setAttribute('id', $keys[$_]);
	    $child->setAttribute('file', $file);
	    $child->setAttribute('structOrUnion', 'true');
            $self->{_typedef_structs}->addChild($child);
            foreach ($struct->childNodes()) {
              my $newnode = $_->cloneNode(1);
              $child->addChild($newnode);
            }
          } else {
            $self->{_typedef_structs}->{$keys[$_]} = $struct;
          }
        }
      } else {
        foreach (0..$#keys) {
          #
          # Not a struct nor union
          #
          if ($self->{_asDOM}) {
            my $child = XML::LibXML::Element->new('struct');
            $child->setAttribute('id', $keys[$_]);
	    $child->setAttribute('file', $file);
	    $child->setAttribute('structOrUnion', 'false');
            $self->{_typedef_structs}->addChild($child);
          } else {
            $self->{_typedef_structs}->{$keys[$_]} = undef;
          }
        }
      }
    }
  }

  return $self->{_typedef_hash};
}

# ----------------------------------------------------------------------------------------

sub _ast2parsed_fdecls {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_parsed_fdecls})) {
    $self->{_parsed_fdecls} = $self->{_asDOM} ? XML::LibXML::Document->new() : [];
    $self->{_fdecls} = $self->{_asDOM} ? XML::LibXML::Element->new('fdecls') : [];

    foreach my $node ($self->ast()->findnodes($self->_xpath('fdecls.xpath'))) {
      my $file = '';
      if (! $self->_pushNodeFile($stdout_buf, \$file, $node)) {
        next;
      }

      my $fdecl = [];
      #
      # rt
      #
      my @declarationSpecifiers = $node->findnodes($self->_xpath('firstDeclarationSpecifiers.xpath'));
      if (! @declarationSpecifiers) {
	#
	# Could be a static assert declaration
	#
	next;
      }
      $self->_pushNodeString($stdout_buf, $fdecl, $declarationSpecifiers[0]);
      #
      # Remove eventual typedef
      #
      $self->_removeWord(\$fdecl->[-1], 'typedef');
      #
      # nm. In case of a function declaration, there can be only a single declarator
      # in the declaration
      #
      my @declarator = $node->findnodes($self->_xpath('declaration2Declarator.xpath'));

      my @IDENTIFIER = $declarator[0]->findnodes($self->_xpath('declarator2IDENTIFIER.xpath'));
      if (@IDENTIFIER) {
	$self->_pushNodeString($stdout_buf, $fdecl, $IDENTIFIER[0]);
      } else {
	my $anon = sprintf('ANON%d', $self->{_anonCount}++);
	push(@{$fdecl}, $anon);
      }
      #
      # args
      #
      my $args = $self->{_asDOM} ? XML::LibXML::Element->new('args') : [];
      my @args = $node->findnodes($self->_xpath('fdecl2args.xpath'));
      foreach (@args) {
	#
	# arg is a parameterDeclaration
	#
	my $arg = [];
	#
	# arg.rt
	#
	my @declarationSpecifiers = $_->findnodes($self->_xpath('firstDeclarationSpecifiers.xpath'));
	$self->_pushNodeString($stdout_buf, $arg, $declarationSpecifiers[0]);
	#
	# arg.nm or ANON
	#
        my $anon = undef;
	my @nm = $_->findnodes($self->_xpath('arg2nm.xpath'));
	if (@nm) {
	  $self->_pushNodeString($stdout_buf, $arg, $nm[0]);
	} else {
          my $anon = sprintf('ANON%d', $self->{_anonCount}++);
	  push(@{$arg}, $anon);
	}
	#
	# arg.arg is always undef
	#
	push(@{$arg}, undef);
	#
	# arg.ft
	#
	$self->_pushNodeString($stdout_buf, $arg, $_);
        if ($anon) {
          #
          # We faked an anonymous identifier
          #
          $arg->[-1] .= ' ' . $anon;
        }
	#
	# arg.mod
	#
        my @mod = $_->findnodes($self->_xpath('arg2mod.xpath'));
        if (@mod) {
	  #
	  # Per def $mod[0] is a directDeclarator that can be:
	  #
	  # directDeclarator LBRACKET RBRACKET
	  # directDeclarator LBRACKET STAR RBRACKET
	  # directDeclarator LBRACKET STATIC gccArrayTypeModifierList assignmentExpression RBRACKET
	  # etc...
	  #
	  # We clone the node, remove the first child. What remains will be the array modifiers.
	  #
	  my $newnode = $mod[0]->cloneNode(1);
	  my $childnode = $newnode->firstChild;
	  $newnode->removeChild($childnode );
          $self->_pushNodeString($stdout_buf, $arg, $newnode);
        } else {
          push(@{$arg}, '');
        }
	if ($self->{_asDOM}) {
          my $child = XML::LibXML::Element->new('arg');
          $child->setAttribute('type', $arg->[0]);
          $child->setAttribute('id', $arg->[1]);
          #
          # Undef per construction, i.e. we do not put this attribute
          #
          # $child->setAttribute('args', $arg->[2]);
          $child->setAttribute('text', $arg->[3]);
          $child->setAttribute('mod', $arg->[4]);
          $args->addChild($child);
	} else {
	  push(@{$args}, $arg);
	}
      }
      push(@{$fdecl}, $args);
      #
      # ft, without remaining semicolon
      #
      $self->_pushNodeString($stdout_buf, $fdecl, $node);
      $fdecl->[-1] =~ s/\s*;$//;
      #
      # mod is always undef
      #
      push(@{$fdecl}, undef);

      if ($self->{_asDOM}) {
        my $child = XML::LibXML::Element->new('fdecl');
        $child->setAttribute('type', $fdecl->[0]);
        $child->setAttribute('id', $fdecl->[1]);
        $child->addChild($fdecl->[2]);
        $child->setAttribute('text', $fdecl->[3]);
        $child->setAttribute('file', $file);
        #
        # Undef per construction: we do not include this attribute
        #
        # $child->setAttribute('mod', $fdecl->[4]);
        $self->{_parsed_fdecls}->addChild($child);
      } else {
	push(@{$self->{_parsed_fdecls}}, $fdecl);
      }

      if ($self->{_asDOM}) {
        my $child = XML::LibXML::Element->new('fdecl');
        $child->setAttribute('id', $fdecl->[1]);
        $child->setAttribute('text', $fdecl->[3]);
        $child->setAttribute('file', $file);
        $self->{_fdecls}->addChild($child);
      } else {
	push(@{$self->{_fdecls}}, $fdecl->[3]);
      }
    }
  }

  return $self->{_parsed_fdecls};
}

# ----------------------------------------------------------------------------------------

sub _ast2inlines {
  my ($self, $stdout_buf) = @_;

  if (! defined($self->{_inlines})) {
    $self->{_inlines} = $self->{_asDOM} ? XML::LibXML::Document->new() : [];
    #
    # Simply, any path matching functionDefinition
    #
    foreach ($self->ast()->findnodes($self->_xpath('inlines.xpath'))) {
      my $file = '';
      if (! $self->_pushNodeFile($stdout_buf, \$file, $_)) {
        next;
      }
      my $text = '';
      $self->_pushNodeString($stdout_buf, \$text, $_);
      if ($self->{_asDOM}) {
        my $child = XML::LibXML::Element->new('inline');
        $child->setAttribute('text', $text);
        $child->setAttribute('file', $file);
        $self->{_inlines}->addChild($child);
      } else {
        push(@{$self->{_inlines}}, $text);
      }
    }
  }

  return $self->{_inlines};
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
    if ($lexemeHashp->{value} =~ /([\d]+)\s*\"([^\"]+)\"/) {
	my $currentFile = File::Spec->canonpath(substr($lexemeHashp->{value}, $-[2], $+[2] - $-[2]));
        if (! defined($self->{_filename})) {
          #
          # The very first filename is always the original source.
          #
          $self->{_filename} = $currentFile;
        }

	$tmpHashp->{_currentFile} = $currentFile;
	$tmpHashp->{_includes}->{$currentFile} = 1;

	$self->{_position2File}->{$lexemeHashp->{start}} = $currentFile;

    }
    #
    # This is an internal lexeme, no problem to change a bit the value. For instance, remove
    # \s if any.
    #
    $lexemeHashp->{value} =~ s/^\s*//g;
    $lexemeHashp->{value} =~ s/\s*$//g;
    $lexemeHashp->{value} =~ s/\n/\\n/g;
  }

  if (defined($tmpHashp->{_currentFile}) && $self->_fileOk($tmpHashp->{_currentFile})) {
    if ($lexemeHashp->{name} eq 'STRING_LITERAL_UNIT') {
      #
      # ISO C permits WS at the end of a string literal, we remove it
      #
      my $string = $lexemeHashp->{value};
      $string =~ s/[ \t\v\n\f]*$//;
      if ($self->{_asDOM}) {
        my $child = XML::LibXML::Element->new('string');
        $child->setAttribute('text', $string);
        $child->setAttribute('file', $tmpHashp->{_currentFile});
        $self->{_strings}->addChild($child)
      } else {
        push(@{$self->{_strings}}, $string);
      }
    }
  }
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

  $self->{_macros} = $self->{_asDOM} ? XML::LibXML::Document->new() : [];
  pos($self->{_content}) = undef;
  while ($self->{_content} =~ m/$REDEFINE/g) {
    my $text = substr($self->{_content}, $-[1], $+[1] - $-[1]);
    my $id = substr($self->{_content}, $-[2], $+[2] - $-[2]);
    my $file = $self->_position2File($-[0]);
    if ($self->{_asDOM}) {
      my $child = XML::LibXML::Element->new('macro');
      $child->setAttribute('text', $text);
      $child->setAttribute('id', $id);
      $child->setAttribute('file', $file);
      $self->{_macros}->addChild($child);
    } else {
      push(@{$self->{_macros}}, [ $text, $id, $file ]);
    }
  }
}

# ----------------------------------------------------------------------------------------

sub _posprocess_heuristics {
  my ($self) = @_;

  #
  # We want to have defines_args and defines_no_args
  #
  $self->{_defines_args} = $self->{_asDOM} ? XML::LibXML::Document->new() : {};
  $self->{_defines_no_args} = $self->{_asDOM} ? XML::LibXML::Document->new() : {};
  foreach ($self->{_asDOM} ? $self->macros->childNodes() : @{$self->macros}) {
    my $text  = $self->{_asDOM} ? $_->getAttribute('text') : $_->[0];
    my $id    = $self->{_asDOM} ? $_->getAttribute('id')   : $_->[1];
    my $file  = $self->{_asDOM} ? $_->getAttribute('file') : $_->[2];
    if ($text =~ /^(\w+)\s*$BALANCEDPARENS\s*(.*)/s) {
      my $args  = substr($text, $-[2], $+[2] - $-[2]);
      my $value = substr($text, $-[3], $+[3] - $-[3]);
      substr($args,  0, 1, '');  # '('
      substr($args, -1, 1, '');  # ')'
      my @args = map {my $element = $_; $element =~ s/\s//g; $element;} split(/,/, $args);
      if ($self->{_asDOM}) {
	my $child = XML::LibXML::Element->new('define');
	$child->setAttribute('text', $text);
	$child->setAttribute('id', $id);
	$child->setAttribute('file', $file);
	$child->setAttribute('value', $value);

	my $subchild = XML::LibXML::Element->new('args');
	foreach (@args) {
	  $subchild->addChild(XML::LibXML::Element->new('arg'))->setAttribute('id', $_);
	}
	$child->addChild($subchild);

	$self->{_defines_args}->addChild($child);
      } else {
	$self->{_defines_args}->{$id} = [ $text, [ @args ], $value, $file ];
      }
    } elsif ($text =~ /(\w+)\s*(.*)/s) {
      my $value = substr($text, $-[2], $+[2] - $-[2]);
      if ($self->{_asDOM}) {
	my $child = XML::LibXML::Element->new('define');
	$child->setAttribute('text', $text);
	$child->setAttribute('id', $id);
	$child->setAttribute('file', $file);
	$child->setAttribute('value', $value);
	$self->{_defines_no_args}->addChild($child);
      } else {
	$self->{_defines_no_args}->{$id} = [ $text, $value, $file ];
      }
    }
  }
}

# ----------------------------------------------------------------------------------------

=head2 c2cifce($self, $lang, %params)

Applies the transformation $lang, with parameters %params, and returns an array composed of the XML::LibXSLT instance and the transformed new XML::LibXML::Document.

=cut

sub c2cifce {
  my ($self, $lang, %params) = @_;

  $log->tracef('Calling transformation with parameters %s', \%params);

  my $ast = $self->ast();
  my $langXslt = $self->_xslt($lang);
  my $transform = $langXslt->transform($ast, %params);

  return ($langXslt, $transform);
}

# ----------------------------------------------------------------------------------------

=head1 NOTES

The default return type for functions without type specifier is fixed to 'int', as per the C standard.

=head1 SEE ALSO

L<Config>

L<MarpaX::Languages::C::AST>

L<C::Scan>

L<File:Temp>

L<C::Tokenize>

L<ModPerl::CScan>

=cut

1;
