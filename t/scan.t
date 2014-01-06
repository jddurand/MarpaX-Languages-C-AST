#!perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 6;
use File::Spec;
use Data::Dumper;

BEGIN {
    push(@INC, 'inc');
    use_ok( 'MarpaX::Languages::C::Scan' ) || print "Bail out!\n";
}

my $filename = File::Spec->catfile('inc', 'scan.c');
my $c = MarpaX::Languages::C::Scan->new(filename => $filename, asHash => 1);
my $ast = $c->ast();
is_deeply($c->defines_no_args,
          {
              'MACRO_NO_ARGS_01' => '',
              'MACRO_NO_ARGS_02' => 'something'
          },
          'defines_no_args');
is_deeply($c->defines_args,
          {
              'MACRO_NO_ARGS_02' =>
                  [
                   [
                    'b',
                    'c'
                   ],
                   "something(b) + else(c) \\\ncontinued"
                  ],
                   'MACRO_NO_ARGS_01' =>
                   [
                    [
                     'a'
                    ],
                    ''
                   ]
          },
          'defines_args');
ok(defined($c->includes), 'includes');
is_deeply($c->parsed_fdecls,
          [
           [
            'int',
            'func1',
            [
             [
              'int',
              'x1',
              undef,
              'int x1',
              ''
             ],
             [
              'double *',
              'x2',
              undef,
              'double *x2',
              ''
             ],
             [
              'float *',
              'f1',
              undef,
              'float *( f1)(int x11, double x12)',
              ''
             ]
            ],
            'int func1(int x1, double *x2, float *( f1)(int x11, double x12))',
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
              'int x1',
              ''
             ],
             [
              'double *',
              'x2',
              undef,
              'double *x2',
              ''
             ],
             [
              'float *',
              'f1',
              undef,
              'float *(*f1)(int x11, double x12)',
              ''
             ]
            ],
            'int func2(int x1, double *x2, float *(*f1)(int x11, double x12))',
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
              'int',
              ''
             ],
             [
              'double *',
              'arg1',
              undef,
              'double *',
              ''
             ],
             [
              'float *',
              'arg2',
              undef,
              'float *(* )(int , double )',
              ''
             ]
            ],
            'int func3(int , double * , float *(* )(int , double ))',
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
              'int',
              ''
             ],
             [
              'double *',
              'arg1',
              undef,
              'double *',
              ''
             ],
             [
              'float *',
              'arg2',
              undef,
              'float *(* )(int , double )',
              ''
             ]
            ],
            'int func4(int , double * , float *(* )(int , double ))',
            undef
           ]
          ],
          'parsed_fdecls');
is_deeply($c->typedef_hash,
{
          'myStructType2_t' => [
                                 'struct {int x;} myStructType2_t',
                                 ''
                               ],
          'myEnumType1_t' => [
                               'enum myEnum1_e {X11 = 0, X12} myEnumType1_t',
                               ''
                             ],
          'myStructType1p_t' => [
                                  'struct myStruct1 {int x;} myStructType1_t, *myStructType1p_t',
                                  ''
                                ],
          'myEnumType2p_t' => [
                                'enum {X21 = 0, X22} myEnumType2_t, *myEnumType2p_t',
                                ''
                              ],
          'myStructType1_t' => [
                                 'struct myStruct1 {int x;} myStructType1_t',
                                 ''
                               ],
          'myStructType2p_t' => [
                                  'struct {int x;} myStructType2_t, *myStructType2p_t',
                                  ''
                                ],
          'myEnumType2_t' => [
                               'enum {X21 = 0, X22} myEnumType2_t',
                               ''
                             ],
          'myEnumType1p_t' => [
                                'enum myEnum1_e {X11 = 0, X12} myEnumType1_t, *myEnumType1p_t',
                                ''
                              ],
          'myInt_type' => [
                            'int myInt_type',
                            ''
                          ]
},
          'typedef_hash');

