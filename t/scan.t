#!perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 10;
use Test::Differences;
use File::Spec;
use Data::Dumper;
#use C::Scan;

BEGIN {
    push(@INC, 'inc');
    use_ok( 'MarpaX::Languages::C::Scan' ) || print "Bail out!\n";
}

#
# I do not want to bother with file/line - too much OS/compiler specific
#
$ENV{MARPAX_LANGUAGES_C_AST_SCAN_TEST} = 1;

my $filename = File::Spec->catfile('inc', 'scan.c');
my $c = MarpaX::Languages::C::Scan->new(filename => $filename);
#my $cscan = C::Scan->new(filename => $filename, filename_filter => $filename);

eq_or_diff($c->defines_no_args,
          {
              'MACRO_NO_ARGS_01' => '',
              'MACRO_NO_ARGS_02' => 'something'
          },
          'defines_no_args');
eq_or_diff($c->defines_args,
          {
              'MACRO_NO_ARGS_04' =>
                  [
                   [
                    'b',
                    'c'
                   ],
                   "something(b) + else(c) \\\ncontinued"
                  ],
                   'MACRO_NO_ARGS_03' =>
                   [
                    [
                     'a'
                    ],
                    ''
                   ]
          },
          'defines_args');
ok(defined($c->includes), 'includes');
eq_or_diff($c->parsed_fdecls,
          [
           [
            'int',
            'func1',
            [
             [
              'int',
              'x1',
              undef,
              '',
              ''
             ],
             [
              'double *',
              'x2',
              undef,
              '',
              ''
             ],
             [
              'float *',
              'f1',
              undef,
              '',
              ''
             ]
            ],
            '',
            undef
           ],
           [
            'int',
            'func2',
            [
             [
              'int',
              'x1',
              undef,
              '',
              ''
             ],
             [
              'double *',
              'x2',
              undef,
              '',
              ''
             ],
             [
              'float *',
              'f1',
              undef,
              '',
              ''
             ]
            ],
            '',
            undef
           ],
           [
            'int',
            'func3',
            [
             [
              'int',
              'arg0',
              undef,
              '',
              ''
             ],
             [
              'double *',
              'arg1',
              undef,
              '',
              ''
             ],
             [
              'float *',
              'arg2',
              undef,
              '',
              ''
             ]
            ],
            '',
            undef
           ],
           [
            'int',
            'func4',
            [
             [
              'int',
              'arg0',
              undef,
              '',
              ''
             ],
             [
              'double *',
              'arg1',
              undef,
              '',
              ''
             ],
             [
              'float *',
              'arg2',
              undef,
              '',
              ''
             ]
            ],
            '',
            undef
           ]
          ],
          'parsed_fdecls');
eq_or_diff($c->typedef_hash,
{
          'myStructType1p_t' => [
                                  '',
                                  ''
                                ],
          'myEnumType2p_t' => [
                                '',
                                ''
                              ],
          'myStructType1_t' => [
                                 '',
                                 ''
                               ],
          'myEnumType2_t' => [
                               '',
                               ''
                             ],
          'myStructType2p_t' => [
                                  '',
                                  ''
                                ],
          'myStructType2_t' => [
                                 '',
                                 ''
                               ],
          'myInt_type' => [
                            '',
                            ''
                          ],
          'myEnumType1p_t' => [
                                '',
                                ''
                              ],
          'myEnumType1_t' => [
                               '',
                               ''
                             ]
},
          'typedef_hash');
eq_or_diff($c->typedefs_maybe,
    [
     'myEnumType1_t',
     'myEnumType1p_t',
     'myEnumType2_t',
     'myEnumType2p_t',
     'myInt_type',
     'myStructType1_t',
     'myStructType1p_t',
     'myStructType2_t',
     'myStructType2p_t'
    ],
    'typedefs_maybe');
eq_or_diff($c->vdecls,
    [
     'vdouble2p',
     'vint1'
    ],
    'vdecls');
eq_or_diff($c->vdecl_hash,
{
    'vdouble2p' => [
        '',
        ''
        ],
        'vint1' => [
            '',
            ''
        ]
},
    'vdecl_hash');
eq_or_diff($c->typedef_structs,
{
    'myStructType2_t' => [
        [
         '',
         '',
         'x'
        ]
        ],
        'myStructType1_t' => [
            [
             '',
             '',
             'x'
            ]
        ],
            'myEnumType1p_t' => undef,
            'myStructType2p_t' => undef,
            'myEnumType1_t' => undef,
            'myInt_type' => undef,
            'myEnumType2p_t' => undef,
            'myEnumType2_t' => undef,
            'myStructType1p_t' => undef
}, 'typedef_structs');

#TODO: {
#    local $TODO = 'C::Scan cmp MarpaX::Languages::C::Scan';
#    print STDERR <<BIGWARN;
#
#=====================================================
#The following tests are likely to fail - do not worry
#=====================================================
#
#BIGWARN
#    foreach (qw/defines_no_args
#                defines_args
#                includes
#                parsed_fdecls
#                typedef_hash
#                typedef_texts
#                typedefs_maybe
#                vdecls
#                vdecl_hash
#                typedef_structs/) {
#        eval {eq_or_diff($cscan->get($_), $c->get($_), , $_)};
#    }
#}
