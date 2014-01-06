/*
  defines_no_args test
*/
#define MACRO_NO_ARGS_01
#define MACRO_NO_ARGS_02 something

/*
  defines_args test
*/
#define MACRO_NO_ARGS_01(a)
#define MACRO_NO_ARGS_02 (b,    c) something(b) + else(c) \
continued
/*
  parsed_fdecls test
*/
int func1(int x1, double *x2, float *( f1)(int x11, double x12));
int func2(int x1, double *x2, float *(*f1)(int x11, double x12));
int func3(int   , double *  , float *(*  )(int    , double    ));
int func4(int   , double *  , float *(*  )(int    , double    ));
