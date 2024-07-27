
 MODULE ModLib_FMZM_1
      USE ModLib_FMVALS, ONLY : MULTI, FM, IM, ZM


!  FMZM 1.4                        David M. Smith

!  This module extends the definition of the basic Fortran arithmetic and function operations so
!  they also apply to multiple precision numbers, using version 1.3 of FM.
!  There are three multiple precision data types:
!     FM  (multiple precision real)
!     IM  (multiple precision integer)
!     ZM  (multiple precision complex)

!  For some examples and general advice about using these multiple-precision data types, see the
!  program SampleFM.f95.

!  Most of the functions defined in this module are multiple precision versions of standard Fortran
!  functions.  In addition, there are functions for direct conversion, formatting, and some
!  mathematical special functions.

!  TO_FM is a function for converting other types of numbers to type FM.  Note that TO_FM(3.12)
!  converts the REAL constant to FM, but it is accurate only to single precision, since the number
!  3.12 cannot be represented exactly in binary and has already been rounded to single precision.
!  Similarly, TO_FM(3.12D0) agrees with 3.12 to double precision accuracy, and TO_FM('3.12') or
!  TO_FM(312)/TO_FM(100) agrees to full FM accuracy.

!  TO_IM converts to type IM, and TO_ZM converts to type ZM.

!  Functions are also supplied for converting the three multiple precision types to the other
!  numeric data types:
!     TO_INT   converts to machine precision integer
!     TO_SP    converts to single precision
!     TO_DP    converts to double precision
!     TO_SPZ   converts to single precision complex
!     TO_DPZ   converts to double precision complex

!  WARNING:   When multiple precision type declarations are inserted in an existing program, take
!             care in converting functions like DBLE(X), where X has been declared as a multiple
!             precision type.  If X was single precision in the original program, then replacing
!             the DBLE(X) by TO_DP(X) in the new version could lose accuracy. For this reason, the
!             Fortran type-conversion functions defined in this module assume that results should
!             be multiple precision whenever inputs are.  Examples:
!             DBLE(TO_FM('1.23E+123456'))         is type FM
!             REAL(TO_FM('1.23E+123456'))         is type FM
!             REAL(TO_ZM('3.12+4.56i'))           is type FM   = TO_FM('3.12')
!             INT(TO_FM('1.23'))                  is type IM   = TO_IM(1)
!             INT(TO_IM('1E+23'))                 is type IM
!             CMPLX(TO_FM('1.23'),TO_FM('4.56'))  is type ZM

!  IS_OVERFLOW, IS_UNDERFLOW, and IS_UNKNOWN are logical functions for checking whether a multiple
!  precision number is in one of the exception categories.  Testing to see if a type FM number is
!  in the +overflow category by directly using an IF can be tricky.  When MAFM is +overflow, the
!  statement
!             IF (MAFM == TO_FM(' +OVERFLOW ')) THEN
!  will return false, since the comparison routine cannot be sure that two different overflowed
!  results would have been equal if the overflow threshold had been higher.  Instead, use
!             IF (IS_OVERFLOW(MAFM)) THEN
!  which will be true if MAFM is + or - overflow.

!  Programs using this module may sometimes need to call FM, IM, or ZM routines directly.  This
!  is normally the case when routines are needed that are not Fortran intrinsics, such as the
!  formatting subroutine FM_FORM.  In a program using this module, suppose MAFM has been declared
!  with TYPE (FM) :: MAFM.  To convert the number to a character string with F65.60 format, use
!     CALL FM_FORM('F65.60',MAFM,ST1)

!  WARNING:   To be safe, all multiple precision variables in a user's program should be declared
!             as type (FM), (IM), or (ZM), and any direct calls to subroutines should be the kind
!             with the underscore.  Subroutines that define one or more multiple precision output
!             values, such as computing pi using
!                 CALL FM_PI(PI)
!             automatically cause PI to be put into the FM saved variable area of storage.  Calling
!             the low-level routine ( CALL FMPI(PI%MFM) ) would cause PI to be treated as an FM
!             temporary variable if PI had not been previously defined in the program.  Then the
!             value of PI could be discarded before the program is finished using it.

!  In subroutine or function subprograms all multiple precision variables that are local to that
!  routine should be declared with the SAVE attribute.  It is not an error to omit SAVE, but if
!  the compiler creates new copies of the variables for each call to the routine, then the program
!  will leak memory.

!  Type (FM), (IM), or (ZM) variables cannot have their multiple precision values initialized in
!  the declaration statement, as can ordinary variables.  If the original program had
!      DOUBLE PRECISION :: X = 2.3D0
!  then the corresponding FM version would have
!      TYPE (FM), SAVE :: X
!      ... (other declarations) ...
!      X = TO_FM( '2.3' )
!
!  An attempt to use a multiple precision variable that has not been defined will be detected by
!  the routines in this module and an error message printed.
!
!  For each of the operations =,  == ,  /= ,  < ,  <= ,  > ,  >= , +, -, *, /, and **, the interface
!  module defines all mixed mode variations involving one of the three multiple precision derived
!  types and another argument having one of the types: { integer, real, double, complex, complex
!  double, FM, IM, ZM }.  So mixed mode expressions such as
!        MAFM = 12
!        MAFM = MAFM + 1
!        IF (ABS(MAFM) > 1.0D-23) THEN
!  are handled correctly.

!  Not all the named functions are defined for all three multiple precision derived types, so the
!  list below shows which can be used.  The labels "real", "integer", and "complex" refer to types
!  FM, IM, and ZM respectively, "string" means the function accepts character strings (e.g.,
!  TO_FM('3.45')), and "other" means the function can accept any of the machine precision data
!  types integer, real, double, complex, or complex double.  For functions that accept two or more
!  arguments, like ATAN2 or MAX, all the arguments must be of the same type.

!  Note that TO_ZM also has a 2-argument form:  TO_ZM(2,3) for getting 2 + 3*i.
!  CMPLX can be used for that, as in CMPLX( TO_FM(2) , TO_FM(3) ), but the 2-argument form is
!  more concise.  The 2-argument form is available for machine precision integer, single and
!  double precision real pairs.  For others, such as X and Y being type(fm), just use CMPLX(X,Y).

!  Fortran's 2-argument version of atan(x,y) is also provided.  It is the same as the older atan2.
!  Functions in this list that are not provided by standard Fortran, such as special functions,
!  have more information about their arguments farther down.


!  AVAILABLE FUNCTIONS:

!     =
!     +
!     -
!     *
!     /
!     **
!     ==
!     /=
!     <
!     <=
!     >
!     >=
!     ABS                  real    integer    complex
!     ACOS                 real               complex
!     ACOSH                real               complex
!     AIMAG                                   complex
!     AINT                 real               complex
!     ANINT                real               complex
!     ASIN                 real               complex
!     ASINH                real               complex
!     ATAN                 real               complex
!     ATAN2                real
!     ATANH                real               complex
!     BERNOULLI            real
!     BESSEL_J             real
!     BESSEL_Y             real
!     BETA                 real
!     BINOMIAL             real    integer
!     BTEST                        integer
!     CEILING              real    integer    complex
!     CMPLX                real    integer
!     CONJG                                   complex
!     COS                  real               complex
!     COSH                 real               complex
!     COS_INTEGRAL         real
!     COSH_INTEGRAL        real
!     DBLE                 real    integer    complex
!     DIGITS               real    integer    complex
!     DIM                  real    integer
!     DINT                 real               complex
!     EPSILON              real
!     ERF                  real               complex
!     ERFC                 real               complex
!     ERFC_SCALED          real               complex
!     EXP                  real               complex
!     EXPONENT             real
!     EXP_INTEGRAL_EI      real
!     EXP_INTEGRAL_EN      real
!     FACTORIAL            real    integer    complex
!     FLOOR                real    integer    complex
!     FRACTION             real               complex
!     FRESNEL_C            real
!     FRESNEL_S            real
!     GAMMA                real               complex
!     GCD                          integer
!     HUGE                 real    integer    complex
!     HYPOT                real
!     INCOMPLETE_BETA      real
!     INCOMPLETE_GAMMA1    real
!     INCOMPLETE_GAMMA2    real
!     INT                  real    integer    complex
!     LOG                  real               complex
!     LOG10                real               complex
!     LOG_ERFC             real
!     LOG_GAMMA            real               complex
!     LOG_INTEGRAL         real
!     MAX                  real    integer
!     MAXEXPONENT          real
!     MIN                  real    integer
!     MINEXPONENT          real
!     MOD                  real    integer
!     MODULO               real    integer
!     MULTIPLY_MOD                 integer
!     NEAREST              real
!     NINT                 real    integer    complex
!     NORM2                real
!     POCHHAMMER           real
!     POLYGAMMA            real
!     POWER_MOD                    integer
!     PRECISION            real               complex
!     PSI                  real
!     RADIX                real    integer    complex
!     RANGE                real    integer    complex
!     REAL                 real    integer    complex
!     RRSPACING            real
!     SCALE                real               complex
!     SETEXPONENT          real
!     SIGN                 real    integer
!     SIN                  real               complex
!     SINH                 real               complex
!     SIN_INTEGRAL         real
!     SINH_INTEGRAL        real
!     SPACING              real
!     SQRT                 real               complex
!     TAN                  real               complex
!     TANH                 real               complex
!     TINY                 real    integer    complex
!     TO_FM                real    integer    complex    string    other
!     TO_IM                real    integer    complex    string    other
!     TO_ZM                real    integer    complex    string    other
!     TO_INT               real    integer    complex
!     TO_SP                real    integer    complex
!     TO_DP                real    integer    complex
!     TO_SPZ               real    integer    complex
!     TO_DPZ               real    integer    complex
!     IS_OVERFLOW          real    integer    complex
!     IS_UNDERFLOW         real    integer    complex
!     IS_UNKNOWN           real    integer    complex


!  SUBROUTINES THAT DO NOT CORRESPOND TO ANY FUNCTION ABOVE:

!  1. Type (FM).  MA, MB, MC refer to type (FM) numbers.

!     FM_COSH_SINH(MA,MB,MC)     MB = COSH(MA),  MC = SINH(MA)
!                                Faster than making two separate calls.

!     FM_COS_SIN(MA,MB,MC)       MB = COS(MA),  MC = SIN(MA)
!                                Faster than making two separate calls.

!     FM_EULER(MA)               MA = Euler's constant ( 0.5772156649... )

!     FM_FLAG(K)                 K = KFLAG  get the value of the FM condition flag -- stored in
!                                           the internal FM variable KFLAG in module FMVALS.

!     FM_FORM(FORM,MA,STRING)    MA is converted to a character string using format FORM and
!                                   returned in STRING.  FORM can represent I, F, E, or ES formats.
!                                   Example:
!                                   CALL FMFORM('F60.40',MA,STRING)

!     FM_FPRINT(FORM,MA)         Print MA on unit KW using FORM format.

!     FM_PI(MA)                  MA = pi

!     FM_PRINT(MA)               Print MA on unit KW using current format.

!     FM_RANDOM_NUMBER(X)        X is returned as a double precision random number, uniformly
!                                distributed on the open interval (0,1).  It is a high-quality,
!                                long-period generator based on 49-digit prime numbers.
!                                Note that X is double precision, unlike the similar Fortran
!                                intrinsic random number routine, which can return a single
!                                or double precision result.
!                                A default initial seed is used if FM_RANDOM_NUMBER is called
!                                without calling FM_RANDOM_SEED_PUT first.

!     FM_RANDOM_SEED_GET(SEED)   returns the seven integers SEED(1) through SEED(7) as the current
!                                seed for the FM_RANDOM_NUMBER generator.

!     FM_RANDOM_SEED_PUT(SEED)   initializes the FM_RANDOM_NUMBER generator using the seven integers
!                                SEED(1) through SEED(7). These get and put functions are slower
!                                than FM_RANDOM_NUMBER, so FM_RANDOM_NUMBER should be called many
!                                times between FM_RANDOM_SEED_PUT calls.  Also, some generators that
!                                used a 9-digit modulus have failed randomness tests when used with
!                                only a few numbers being generated between calls to re-start with
!                                a new seed.

!     FM_RANDOM_SEED_SIZE(SIZE)  returns integer SIZE as the size of the SEED array used by the
!                                FM_RANDOM_NUMBER generator.  Currently, SIZE = 7.

!     FM_RATIONAL_POWER(MA,K,J,MB)
!                                MB = MA**(K/J)  Rational power.
!                                Faster than MB = MA**(TO_FM(K)/J) for functions like the cube root.

!     FM_READ(KREAD,MA)          MA is returned after reading one (possibly multi-line) FM number
!                                   on unit KREAD.  This routine reads numbers written by FM_WRITE.

!     FM_SET(NPREC)              Set the internal FM variables so that the precision is at least
!                                NPREC base 10 digits plus three base 10 guard digits.

!     FM_SETVAR(STRING)          Define a new value for one of the internal FM variables in module
!                                FMVALS that controls one of the FM options.  STRING has the form
!                                      variable = value.
!                                Example:  To change the screen width for FM output:
!                                      CALL FM_SETVAR(' KSWIDE = 120 ')
!                                The variables that can be changed and the options they control are
!                                listed in sections 2 through 6 of the comments at the top of the
!                                FM.f95 file.  Only one variable can be set per call.  The variable
!                                name in STRING must have no embedded blanks.  The value part of
!                                STRING can be in any numerical format, except in the case of
!                                variable CMCHAR, which is character type.  To set CMCHAR to 'E',
!                                don't use any quotes in STRING:
!                                      CALL FM_SETVAR(' CMCHAR = E ')

!     FM_ULP(MA,MB)              MB = One Unit in the Last Place of MA.  For positive MA this is the
!                                     same as the Fortran function SPACING, but MB < 0 if MA < 0.
!                                     Examples:  If MBASE = 10 and NDIG = 30, then ulp(1.0) =
!                                                1.0E-29,  ulp(-4.5E+67) = -1.0E+38.
!

!     FM_VARS                    Write the current values of the internal FM variables on unit KW.

!     FM_WRITE(KWRITE,MA)        Write MA on unit KWRITE.
!                                Multi-line numbers will have '&' as the last nonblank character
!                                on all but the last line.  These numbers can then be read easily
!                                using FM_READ.


!  2. Type (IM).    MA, MB, MC refer to type (IM) numbers.

!     IM_DIVR(MA,MB,MC,MD)       MC = int(MA/MB),   MD = MA mod MB
!                                     When both the quotient and remainder are needed, this routine
!                                     is twice as fast as doing MC = MA/MB and MD = MOD(MA,MB)
!                                     separately.

!     IM_DVIR(MA,IVAL,MB,IREM)   MB = int(MA/IVAL),   IREM = MA mod IVAL
!                                IVAL and IREM are one word integers.  Faster than doing separately.

!     IM_FORM(FORM,MA,STRING)    MA is converted to a character string using format FORM and
!                                   returned in STRING.  FORM can represent I, F, E, or ES formats.
!                                   Example: CALL IMFORM('I70',MA,STRING)

!     IM_FPRINT(FORM,MA)         Print MA on unit KW using FORM format.

!     IM_PRINT(MA)               Print MA on unit KW.

!     IM_READ(KREAD,MA)          MA is returned after reading one (possibly multi-line) IM number
!                                   on unit KREAD.  This routine reads numbers written by IM_WRITE.

!     IM_WRITE(KWRITE,MA)        Write MA on unit KWRITE.  Multi-line numbers will have '&' as the
!                                last nonblank character on all but the last line.
!                                These numbers can then be read easily using IM_READ.


!  3. Type (ZM).    MA, MB, MC refer to type (ZM) numbers.  MBFM is type (FM).

!     ZM_ARG(MA,MBFM)              MBFM = complex argument of MA.  MBFM is the (real) angle in the
!                                         interval ( -pi , pi ] from the positive real axis to the
!                                         point (x,y) when MA = x + y*i.

!     ZM_COSH_SINH(MA,MB,MC)     MB = COSH(MA),  MC = SINH(MA).
!                                     Faster than 2 calls.

!     ZM_COS_SIN(MA,MB,MC)       MB = COS(MA),  MC = SIN(MA).
!                                     Faster than 2 calls.

!     ZM_FORM(FORM1,FORM2,MA,STRING)
!                                STRING = MA
!                                MA is converted to a character string using format FORM1 for the
!                                real part and FORM2 for the imaginary part.  The result is returned
!                                in STRING.  FORM1 and FORM2 can represent I, F, E, or ES formats.
!                                Example:
!                                      CALL ZMFORM('F20.10','F15.10',MA,STRING)

!     ZM_FPRINT(FORM1,FORM2,MA)  Print MA on unit KW using formats FORM1 and FORM2.

!     ZM_PRINT(MA)               Print MA on unit KW using current format.

!     ZM_READ(KREAD,MA)          MA is returned after reading one (possibly multi-line) ZM number
!                                   on unit KREAD.  This routine reads numbers written by ZMWRITE.

!     ZM_RATIONAL_POWER(MA,IVAL,JVAL,MB)
!                                MB = MA ** (IVAL/JVAL)
!                                Faster than MB = MA**(TO_FM(K)/J) for functions like the cube root.

!     ZM_WRITE(KWRITE,MA)        Write MA on unit KWRITE.  Multi-line numbers are formatted for
!                                automatic reading with ZMREAD.


!  Some other functions are defined that do not correspond to machine precision intrinsic
!  functions. These include formatting functions, integer modular functions and GCD, and some
!  mathematical special functions.
!  N, K below are machine precision integers, J1, J2, J3 are TYPE (IM), FMT, FMTR, FMTI are
!  character strings, A, B, X are TYPE (FM), and Z is TYPE (ZM).
!  The three formatting functions return a character string containing the formatted number, the
!  three TYPE (IM) functions return a TYPE (IM) result, and the 12 special functions return
!  TYPE (FM) results.

!  Formatting functions:

!     FM_FORMAT(FMT,A)        Put A into FMT (real) format
!     IM_FORMAT(FMT,J1)       Put J1 into FMT (integer) format
!     ZM_FORMAT(FMTR,FMTI,Z)  Put Z into (complex) format, FMTR for the real
!                             part and FMTI for the imaginary part

!     Examples:
!        ST = FM_FORMAT('F65.60',A)
!        WRITE (*,*) ' A = ',TRIM(ST)
!        ST = FM_FORMAT('E75.60',B)
!        WRITE (*,*) ' B = ',ST(1:75)
!        ST = IM_FORMAT('I50',J1)
!        WRITE (*,*) ' J1 = ',ST(1:50)
!        ST = ZM_FORMAT('F35.30','F30.25',Z)
!        WRITE (*,*) ' Z = ',ST(1:70)

!     These functions are used for one-line output.  The returned character strings are of
!     length 200.

!     For higher precision numbers, the output can be broken onto multiple lines automatically by
!     calling subroutines FM_PRINT, IM_PRINT, ZM_PRINT, or the line breaks can be done by hand after
!     calling one of the subroutines FM_FORM, IM_FORM, ZM_FORM.

!     For ZM_FORMAT the length of the output is 5 more than the sum of the two field widths.

!  Integer functions:

!     BINOMIAL(N,K)           Binomial coefficient N choose K.  Returns the exact result as a
!                                  type IM value.
!     BINOMIAL(J1,J2)         Binomial coefficient J1 choose J2.  Like factorial below, the result
!                                  might be too large unless min(J2,J1-J2) is fairly small,
!     FACTORIAL(N)            N!   Returns the exact result as a type IM value.
!     FACTORIAL(J1)           J1!  Note that the factorial function grows so rapidly that if type IM
!                                  variable J1 is larger than the largest machine precision integer,
!                                  then J1! has over 10 billion digits and the calculation would
!                                  likely fail due to memory or time constraints.  This version is
!                                  provided for convenience, and will return UNKNOWN if J1 cannot
!                                  be represented as a machine precision integer.
!     GCD(J1,J2)              Greatest Common Divisor of J1 and J2.
!     MULTIPLY_MOD(J1,J2,J3)  J1 * J2 mod J3
!     POWER_MOD(J1,J2,J3)     J1 ** J2 mod J3

!  Special functions:

!     BERNOULLI(N)            Nth Bernoulli number
!     BESSEL_J(N,X)           Bessel function of the first kind J_n(x)
!     BESSEL_J0(X)            Fortran-08 name for J_0(x)
!     BESSEL_J1(X)            Fortran-08 name for J_1(x)
!     BESSEL_JN(N,X)          Fortran-08 name for J_n(x)
!     BESSEL_JN(N1,N2,X)      Returns array (/ J_n1(x) , ... , J_n2(x) /)
!     BESSEL_Y(N,X)           Bessel function of the second kind Y_n(x)
!     BESSEL_Y0(X)            Fortran-08 name for Y_0(x)
!     BESSEL_Y1(X)            Fortran-08 name for Y_1(x)
!     BESSEL_YN(N,X)          Fortran-08 name for Y_n(x)
!     BESSEL_YN(N1,N2,X)      Returns array (/ Y_n1(x) , ... , Y_n2(x) /)
!     BETA(A,B)               Integral (0 to 1)  t**(a-1) * (1-t)**(b-1)  dt
!     BINOMIAL(A,B)           Binomial Coefficient  a! / ( b! (a-b)! )
!     COS_INTEGRAL(X)         Cosine Integral Ci(x)
!     COSH_INTEGRAL(X)        Hyperbolic Cosine Integral Chi(x)
!     ERF(X)                  Error function Erf(x)
!     ERFC(X)                 Complimentary error function Erfc(x)
!     ERFC_SCALED(X)          Exp(x^2) * Erfc(x)
!     EXP_INTEGRAL_EI(X)      Exponential Integral Ei(x)
!     EXP_INTEGRAL_EN(N,X)    Exponential Integral E_n(x)
!     FACTORIAL(X)            x!   = Gamma(x+1)
!     FRESNEL_C(X)            Fresnel Cosine Integral C(x)
!     FRESNEL_S(X)            Fresnel Sine Integral S(x)
!     GAMMA(X)                Integral (0 to infinity)  t**(x-1) * exp(-t)  dt
!     INCOMPLETE_BETA(X,A,B)  Integral (0 to x)  t**(a-1) * (1-t)**(b-1)  dt
!     INCOMPLETE_GAMMA1(A,X)  Integral (0 to x)  t**(a-1) * exp(-t)  dt
!     INCOMPLETE_GAMMA2(A,X)  Integral (x to infinity)  t**(a-1) * exp(-t)  dt
!     LOG_ERFC(X)             Ln( Erfc(x) )
!     LOG_GAMMA(X)            Analytic continuation of real Ln( Gamma(x) ).  May differ from complex
!                             Ln( Gamma(x) ) by an integer multiple of 2*pi*i.
!     LOG_INTEGRAL(X)         Logarithmic Integral Li(x)
!     POCHHAMMER(X,N)         x*(x+1)*(x+2)*...*(x+n-1)
!     POLYGAMMA(N,X)          Nth derivative of Psi(x)
!     PSI(X)                  Derivative of Ln(Gamma(x))
!     SIN_INTEGRAL(X)         Sine Integral Si(x)
!     SINH_INTEGRAL(X)        Hyperbolic Sine Integral Shi(x)


!  Array operations:

!  Arithmetic operations and functions on arrays of dimension (rank) one or two are supported for
!  each of the three multiple-precision types.  Binary operations (+-*/) require both arguments to
!  have the same rank and shape.

!     Examples:
!        TYPE (FM), SAVE, DIMENSION(10)  :: A, B
!        TYPE (FM), SAVE, DIMENSION(3,3) :: C
!        TYPE (IM), SAVE, DIMENSION(10)  :: J, K
!        TYPE (IM), SAVE, DIMENSION(3,3) :: L
!        ...
!        A = 0                           ! Set the whole array to zero
!        J = J * K                       ! Set J(i) = J(i) * K(i) for i = 1, ..., 10
!        B = A - K                       ! Mixed-mode operations are ok
!        C = 7.3D0 * C - ( C + 2*L )/3

!     Array functions:

!        DOT_PRODUCT(X,Y)     Dot product of rank 1 vectors of the same type.
!                             Note that when X and Y are complex, the result is not just the sum
!                             of the products of the corresponding array elements, as it is for
!                             types FM and IM.  For ZM the formula is the sum of
!                             conjg(X(j)) * Y(j).
!        IS_OVERFLOW(X)       Returns true if any element is + or - overflow.
!        IS_UNDERFLOW(X)      Returns true if any element is + or - underflow.
!        IS_UNKNOWN(X)        Returns true if any element is unknown.
!        MATMUL(X,Y)          Matrix multiplication of arrays of the same type
!                             Cases for valid argument shapes:
!                             (1)  (n,m) * (m,k) --> (n,k)
!                             (2)    (m) * (m,k) --> (k)
!                             (3)  (n,m) * (m)   --> (n)
!        MAXLOC(X)            Location of the maximum value in the array
!        MAXVAL(X)            Maximum value in the array
!        MINLOC(X)            Location of the minimum value in the array
!        MINVAL(X)            Minimum value in the array
!        PRODUCT(X)           Product of all values in the array
!        SUM(X)               Sum of all values in the array
!        TRANSPOSE(X)         Matrix transposition.  If X is a rank 2 array with shape (n,m), then
!                             Y = TRANSPOSE(X) has shape (m,n) with Y(i,j) = X(j,i).
!        TO_FM(X)             Rank 1 or 2 arrays are converted to similar type (fm) arrays.
!        TO_IM(X)             Rank 1 or 2 arrays are converted to similar type (im) arrays.
!        TO_ZM(X)             Rank 1 or 2 arrays are converted to similar type (zm) arrays.
!        TO_INT(X)            Rank 1 or 2 arrays are converted to similar integer arrays.
!        TO_SP(X)             Rank 1 or 2 arrays are converted to similar single precision arrays.
!        TO_DP(X)             Rank 1 or 2 arrays are converted to similar double precision arrays.
!        TO_SPZ(X)            Rank 1 or 2 arrays are converted to similar single complex arrays.
!        TO_DPZ(X)            Rank 1 or 2 arrays are converted to similar double complex arrays.

!     The arithmetic array functions DOT_PRODUCT, MATMUL, PRODUCT, and SUM work like the other
!     functions in the FM package in that they raise precision and compute the sums and/or products
!     at the higher precision, then round the final result back to the user's precision to provide
!     a more accurate result.

!     Fortran's optional [,mask] argument for these functions is not provided.

!     Many of the 1-argument functions can be used with array arguments, with the result being an
!     array of the same size and shape where the function has been applied to each element.

!     Examples:
!        TYPE (FM), SAVE, DIMENSION(10) :: A, B, C
!        ...
!        A = ABS(B)                ! Set A(i) = ABS(B(i)) for i = 1, ..., 10
!        C = SQRT(A+4+B*B)         ! Set C(i) = SQRT(A(i)+4+B(i)*B(i)) for i = 1, ..., 10

!     Functions that can have array arguments.  As above, "real", "integer", and "complex" refer
!     to types FM, IM, and ZM respectively.

!     ABS              real    integer    complex
!     ACOS             real               complex
!     ACOSH            real               complex
!     AIMAG                               complex
!     AINT             real               complex
!     ANINT            real               complex
!     ASIN             real               complex
!     ASINH            real               complex
!     ATAN             real               complex
!     ATANH            real               complex
!     CEILING          real    integer    complex
!     CONJG                               complex
!     COS              real               complex
!     COSH             real               complex
!     EXP              real               complex
!     FLOOR            real    integer    complex
!     FRACTION         real               complex
!     INT              real    integer    complex
!     LOG              real               complex
!     LOG10            real               complex
!     NINT             real    integer    complex
!     SIN              real               complex
!     SINH             real               complex
!     SQRT             real               complex
!     TAN              real               complex
!     TANH             real               complex
!     COS_INTEGRAL     real
!     COSH_INTEGRAL    real
!     ERF              real               complex
!     ERFC             real               complex
!     ERFC_SCALED      real               complex
!     EXP_INTEGRAL_EI  real
!     FACTORIAL        real    integer    complex    machine-precision integer
!     FRESNEL_C        real
!     FRESNEL_S        real
!     GAMMA            real               complex
!     LOG_ERFC         real
!     LOG_GAMMA        real               complex
!     LOG_INTEGRAL     real
!     PSI              real
!     SIN_INTEGRAL     real
!     SINH_INTEGRAL    real


   INTERFACE TO_FM
      MODULE PROCEDURE FM_I
      MODULE PROCEDURE FM_R
      MODULE PROCEDURE FM_D
      MODULE PROCEDURE FM_Z
      MODULE PROCEDURE FM_ZD
      MODULE PROCEDURE FM_FM
      MODULE PROCEDURE FM_IM
      MODULE PROCEDURE FM_ZM
      MODULE PROCEDURE FM_ST
      MODULE PROCEDURE FM_I1
      MODULE PROCEDURE FM_R1
      MODULE PROCEDURE FM_D1
      MODULE PROCEDURE FM_Z1
      MODULE PROCEDURE FM_ZD1
      MODULE PROCEDURE FM_FM1
      MODULE PROCEDURE FM_IM1
      MODULE PROCEDURE FM_ZM1
      MODULE PROCEDURE FM_ST1
      MODULE PROCEDURE FM_I2
      MODULE PROCEDURE FM_R2
      MODULE PROCEDURE FM_D2
      MODULE PROCEDURE FM_Z2
      MODULE PROCEDURE FM_ZD2
      MODULE PROCEDURE FM_FM2
      MODULE PROCEDURE FM_IM2
      MODULE PROCEDURE FM_ZM2
      MODULE PROCEDURE FM_ST2
   END INTERFACE

   INTERFACE TO_IM
      MODULE PROCEDURE IM_I
      MODULE PROCEDURE IM_R
      MODULE PROCEDURE IM_D
      MODULE PROCEDURE IM_Z
      MODULE PROCEDURE IM_C
      MODULE PROCEDURE IM_FM
      MODULE PROCEDURE IM_IM
      MODULE PROCEDURE IM_ZM
      MODULE PROCEDURE IM_ST
      MODULE PROCEDURE IM_I1
      MODULE PROCEDURE IM_R1
      MODULE PROCEDURE IM_D1
      MODULE PROCEDURE IM_Z1
      MODULE PROCEDURE IM_C1
      MODULE PROCEDURE IM_FM1
      MODULE PROCEDURE IM_IM1
      MODULE PROCEDURE IM_ZM1
      MODULE PROCEDURE IM_ST1
      MODULE PROCEDURE IM_I2
      MODULE PROCEDURE IM_R2
      MODULE PROCEDURE IM_D2
      MODULE PROCEDURE IM_Z2
      MODULE PROCEDURE IM_C2
      MODULE PROCEDURE IM_FM2
      MODULE PROCEDURE IM_IM2
      MODULE PROCEDURE IM_ZM2
      MODULE PROCEDURE IM_ST2
   END INTERFACE

   INTERFACE TO_ZM
      MODULE PROCEDURE ZM_I
      MODULE PROCEDURE ZM2_I
      MODULE PROCEDURE ZM_R
      MODULE PROCEDURE ZM2_R
      MODULE PROCEDURE ZM_D
      MODULE PROCEDURE ZM2_D
      MODULE PROCEDURE ZM_Z
      MODULE PROCEDURE ZM_C
      MODULE PROCEDURE ZM_FM
      MODULE PROCEDURE ZM_IM
      MODULE PROCEDURE ZM_ZM
      MODULE PROCEDURE ZM_ST
      MODULE PROCEDURE ZM_I1
      MODULE PROCEDURE ZM_R1
      MODULE PROCEDURE ZM_D1
      MODULE PROCEDURE ZM_Z1
      MODULE PROCEDURE ZM_C1
      MODULE PROCEDURE ZM_FM1
      MODULE PROCEDURE ZM_IM1
      MODULE PROCEDURE ZM_ZM1
      MODULE PROCEDURE ZM_ST1
      MODULE PROCEDURE ZM_I2
      MODULE PROCEDURE ZM_R2
      MODULE PROCEDURE ZM_D2
      MODULE PROCEDURE ZM_Z2
      MODULE PROCEDURE ZM_C2
      MODULE PROCEDURE ZM_FM2
      MODULE PROCEDURE ZM_IM2
      MODULE PROCEDURE ZM_ZM2
      MODULE PROCEDURE ZM_ST2
   END INTERFACE

   INTERFACE TO_INT
      MODULE PROCEDURE FM_2INT
      MODULE PROCEDURE IM_2INT
      MODULE PROCEDURE ZM_2INT
      MODULE PROCEDURE FM_2INT1
      MODULE PROCEDURE IM_2INT1
      MODULE PROCEDURE ZM_2INT1
      MODULE PROCEDURE FM_2INT2
      MODULE PROCEDURE IM_2INT2
      MODULE PROCEDURE ZM_2INT2
   END INTERFACE

   INTERFACE TO_SP
      MODULE PROCEDURE FM_2SP
      MODULE PROCEDURE IM_2SP
      MODULE PROCEDURE ZM_2SP
      MODULE PROCEDURE FM_2SP1
      MODULE PROCEDURE IM_2SP1
      MODULE PROCEDURE ZM_2SP1
      MODULE PROCEDURE FM_2SP2
      MODULE PROCEDURE IM_2SP2
      MODULE PROCEDURE ZM_2SP2
   END INTERFACE

   INTERFACE TO_DP
      MODULE PROCEDURE FM_2DP
      MODULE PROCEDURE IM_2DP
      MODULE PROCEDURE ZM_2DP
      MODULE PROCEDURE FM_2DP1
      MODULE PROCEDURE IM_2DP1
      MODULE PROCEDURE ZM_2DP1
      MODULE PROCEDURE FM_2DP2
      MODULE PROCEDURE IM_2DP2
      MODULE PROCEDURE ZM_2DP2
   END INTERFACE

   INTERFACE TO_SPZ
      MODULE PROCEDURE FM_2SPZ
      MODULE PROCEDURE IM_2SPZ
      MODULE PROCEDURE ZM_2SPZ
      MODULE PROCEDURE FM_2SPZ1
      MODULE PROCEDURE IM_2SPZ1
      MODULE PROCEDURE ZM_2SPZ1
      MODULE PROCEDURE FM_2SPZ2
      MODULE PROCEDURE IM_2SPZ2
      MODULE PROCEDURE ZM_2SPZ2
   END INTERFACE

   INTERFACE TO_DPZ
      MODULE PROCEDURE FM_2DPZ
      MODULE PROCEDURE IM_2DPZ
      MODULE PROCEDURE ZM_2DPZ
      MODULE PROCEDURE FM_2DPZ1
      MODULE PROCEDURE IM_2DPZ1
      MODULE PROCEDURE ZM_2DPZ1
      MODULE PROCEDURE FM_2DPZ2
      MODULE PROCEDURE IM_2DPZ2
      MODULE PROCEDURE ZM_2DPZ2
   END INTERFACE

   INTERFACE IS_OVERFLOW
      MODULE PROCEDURE FM_IS_OVERFLOW
      MODULE PROCEDURE IM_IS_OVERFLOW
      MODULE PROCEDURE ZM_IS_OVERFLOW
      MODULE PROCEDURE FM_IS_OVERFLOW1
      MODULE PROCEDURE IM_IS_OVERFLOW1
      MODULE PROCEDURE ZM_IS_OVERFLOW1
      MODULE PROCEDURE FM_IS_OVERFLOW2
      MODULE PROCEDURE IM_IS_OVERFLOW2
      MODULE PROCEDURE ZM_IS_OVERFLOW2
   END INTERFACE

   INTERFACE IS_UNDERFLOW
      MODULE PROCEDURE FM_IS_UNDERFLOW
      MODULE PROCEDURE IM_IS_UNDERFLOW
      MODULE PROCEDURE ZM_IS_UNDERFLOW
      MODULE PROCEDURE FM_IS_UNDERFLOW1
      MODULE PROCEDURE IM_IS_UNDERFLOW1
      MODULE PROCEDURE ZM_IS_UNDERFLOW1
      MODULE PROCEDURE FM_IS_UNDERFLOW2
      MODULE PROCEDURE IM_IS_UNDERFLOW2
      MODULE PROCEDURE ZM_IS_UNDERFLOW2
   END INTERFACE

   INTERFACE IS_UNKNOWN
      MODULE PROCEDURE FM_IS_UNKNOWN
      MODULE PROCEDURE IM_IS_UNKNOWN
      MODULE PROCEDURE ZM_IS_UNKNOWN
      MODULE PROCEDURE FM_IS_UNKNOWN1
      MODULE PROCEDURE IM_IS_UNKNOWN1
      MODULE PROCEDURE ZM_IS_UNKNOWN1
      MODULE PROCEDURE FM_IS_UNKNOWN2
      MODULE PROCEDURE IM_IS_UNKNOWN2
      MODULE PROCEDURE ZM_IS_UNKNOWN2
   END INTERFACE

   INTERFACE FM_UNDEF_INP
      MODULE PROCEDURE FM_UNDEF_INP_FM0
      MODULE PROCEDURE FM_UNDEF_INP_IM0
      MODULE PROCEDURE FM_UNDEF_INP_ZM0
      MODULE PROCEDURE FM_UNDEF_INP_FM1
      MODULE PROCEDURE FM_UNDEF_INP_IM1
      MODULE PROCEDURE FM_UNDEF_INP_ZM1
      MODULE PROCEDURE FM_UNDEF_INP_FM2
      MODULE PROCEDURE FM_UNDEF_INP_IM2
      MODULE PROCEDURE FM_UNDEF_INP_ZM2
   END INTERFACE

!  The next function is no longer needed in version 1.4.
!  Dummy versions of the individual procedures are included for compatibility with version 1.3.

   INTERFACE FM_DEALLOCATE
      MODULE PROCEDURE FM_DEALLOCATE_FM1
      MODULE PROCEDURE FM_DEALLOCATE_IM1
      MODULE PROCEDURE FM_DEALLOCATE_ZM1
      MODULE PROCEDURE FM_DEALLOCATE_FM2
      MODULE PROCEDURE FM_DEALLOCATE_IM2
      MODULE PROCEDURE FM_DEALLOCATE_ZM2
   END INTERFACE


 CONTAINS

!                                                               TO_FM

   FUNCTION FM_I(IVAL)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: RETURN_VALUE
      INTEGER :: IVAL
      INTENT (IN) :: IVAL
      CALL FMI2M(IVAL,RETURN_VALUE%MFM)
   END FUNCTION FM_I

   FUNCTION FM_R(R)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: RETURN_VALUE
      REAL :: R
      INTENT (IN) :: R
      CALL FMSP2M(R,RETURN_VALUE%MFM)
   END FUNCTION FM_R

   FUNCTION FM_D(D)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: RETURN_VALUE
      DOUBLE PRECISION :: D
      INTENT (IN) :: D
      CALL FMDP2M(D,RETURN_VALUE%MFM)
   END FUNCTION FM_D

   FUNCTION FM_Z(Z)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: RETURN_VALUE
      COMPLEX :: Z
      INTENT (IN) :: Z
      CALL FMSP2M(REAL(Z),RETURN_VALUE%MFM)
   END FUNCTION FM_Z

   FUNCTION FM_ZD(C)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: RETURN_VALUE
      COMPLEX (KIND(0.0D0)) :: C
      INTENT (IN) :: C
      CALL FMDP2M(REAL(C,KIND(0.0D0)),RETURN_VALUE%MFM)
   END FUNCTION FM_ZD

   FUNCTION FM_FM(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: RETURN_VALUE,MA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FMEQ(MA%MFM,RETURN_VALUE%MFM)
   END FUNCTION FM_FM

   FUNCTION FM_IM(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: RETURN_VALUE
      TYPE (IM) :: MA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMI2FM(MA%MIM,RETURN_VALUE%MFM)
   END FUNCTION FM_IM

   FUNCTION FM_ZM(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: RETURN_VALUE
      TYPE (ZM) :: MA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL ZMREAL(MA%MZM,RETURN_VALUE%MFM)
   END FUNCTION FM_ZM

   FUNCTION FM_ST(ST)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: RETURN_VALUE
      CHARACTER(*) :: ST
      INTENT (IN) :: ST
      CALL FMST2M(ST,RETURN_VALUE%MFM)
   END FUNCTION FM_ST

   FUNCTION FM_I1(IVAL)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: IVAL
      TYPE (FM), DIMENSION(SIZE(IVAL)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: IVAL
      N = SIZE(IVAL)
      DO J = 1, N
         CALL FMI2M(IVAL(J),RETURN_VALUE(J)%MFM)
      ENDDO
   END FUNCTION FM_I1

   FUNCTION FM_R1(R)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      REAL, DIMENSION(:) :: R
      TYPE (FM), DIMENSION(SIZE(R)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: R
      N = SIZE(R)
      DO J = 1, N
         CALL FMSP2M(R(J),RETURN_VALUE(J)%MFM)
      ENDDO
   END FUNCTION FM_R1

   FUNCTION FM_D1(D)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION, DIMENSION(:) :: D
      TYPE (FM), DIMENSION(SIZE(D)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: D
      N = SIZE(D)
      DO J = 1, N
         CALL FMDP2M(D(J),RETURN_VALUE(J)%MFM)
      ENDDO
   END FUNCTION FM_D1

   FUNCTION FM_Z1(Z)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX, DIMENSION(:) :: Z
      TYPE (FM), DIMENSION(SIZE(Z)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: Z
      N = SIZE(Z)
      DO J = 1, N
         CALL FMSP2M(REAL(Z(J)),RETURN_VALUE(J)%MFM)
      ENDDO
   END FUNCTION FM_Z1

   FUNCTION FM_ZD1(C)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX (KIND(0.0D0)), DIMENSION(:) :: C
      TYPE (FM), DIMENSION(SIZE(C)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: C
      N = SIZE(C)
      DO J = 1, N
         CALL FMDP2M(REAL(C(J),KIND(0.0D0)),RETURN_VALUE(J)%MFM)
      ENDDO
   END FUNCTION FM_ZD1

   FUNCTION FM_FM1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      TYPE (FM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL FMEQ(MA(J)%MFM,RETURN_VALUE(J)%MFM)
      ENDDO
   END FUNCTION FM_FM1

   FUNCTION FM_IM1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (FM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL IMI2FM(MA(J)%MIM,RETURN_VALUE(J)%MFM)
      ENDDO
   END FUNCTION FM_IM1

   FUNCTION FM_ZM1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      TYPE (FM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL ZMREAL(MA(J)%MZM,RETURN_VALUE(J)%MFM)
      ENDDO
   END FUNCTION FM_ZM1

   FUNCTION FM_ST1(ST)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*), DIMENSION(:) :: ST
      TYPE (FM), DIMENSION(SIZE(ST)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: ST
      N = SIZE(ST)
      DO J = 1, N
         CALL FMST2M(ST(J),RETURN_VALUE(J)%MFM)
      ENDDO
   END FUNCTION FM_ST1

   FUNCTION FM_I2(IVAL)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: IVAL
      TYPE (FM), DIMENSION(SIZE(IVAL,DIM=1),SIZE(IVAL,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: IVAL
      DO J = 1, SIZE(IVAL,DIM=1)
         DO K = 1, SIZE(IVAL,DIM=2)
            CALL FMI2M(IVAL(J,K),RETURN_VALUE(J,K)%MFM)
         ENDDO
      ENDDO
   END FUNCTION FM_I2

   FUNCTION FM_R2(R)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      REAL, DIMENSION(:,:) :: R
      TYPE (FM), DIMENSION(SIZE(R,DIM=1),SIZE(R,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: R
      DO J = 1, SIZE(R,DIM=1)
         DO K = 1, SIZE(R,DIM=2)
            CALL FMSP2M(R(J,K),RETURN_VALUE(J,K)%MFM)
         ENDDO
      ENDDO
   END FUNCTION FM_R2

   FUNCTION FM_D2(D)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION, DIMENSION(:,:) :: D
      TYPE (FM), DIMENSION(SIZE(D,DIM=1),SIZE(D,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: D
      DO J = 1, SIZE(D,DIM=1)
         DO K = 1, SIZE(D,DIM=2)
            CALL FMDP2M(D(J,K),RETURN_VALUE(J,K)%MFM)
         ENDDO
      ENDDO
   END FUNCTION FM_D2

   FUNCTION FM_Z2(Z)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX, DIMENSION(:,:) :: Z
      TYPE (FM), DIMENSION(SIZE(Z,DIM=1),SIZE(Z,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: Z
      DO J = 1, SIZE(Z,DIM=1)
         DO K = 1, SIZE(Z,DIM=2)
            CALL FMSP2M(REAL(Z(J,K)),RETURN_VALUE(J,K)%MFM)
         ENDDO
      ENDDO
   END FUNCTION FM_Z2

   FUNCTION FM_ZD2(C)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX (KIND(0.0D0)), DIMENSION(:,:) :: C
      TYPE (FM), DIMENSION(SIZE(C,DIM=1),SIZE(C,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: C
      DO J = 1, SIZE(C,DIM=1)
         DO K = 1, SIZE(C,DIM=2)
            CALL FMDP2M(REAL(C(J,K),KIND(0.0D0)),RETURN_VALUE(J,K)%MFM)
         ENDDO
      ENDDO
   END FUNCTION FM_ZD2

   FUNCTION FM_FM2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      TYPE (FM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMEQ(MA(J,K)%MFM,RETURN_VALUE(J,K)%MFM)
         ENDDO
      ENDDO
   END FUNCTION FM_FM2

   FUNCTION FM_IM2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (FM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMI2FM(MA(J,K)%MIM,RETURN_VALUE(J,K)%MFM)
         ENDDO
      ENDDO
   END FUNCTION FM_IM2

   FUNCTION FM_ZM2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      TYPE (FM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL ZMREAL(MA(J,K)%MZM,RETURN_VALUE(J,K)%MFM)
         ENDDO
      ENDDO
   END FUNCTION FM_ZM2

   FUNCTION FM_ST2(ST)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*), DIMENSION(:,:) :: ST
      TYPE (FM), DIMENSION(SIZE(ST,DIM=1),SIZE(ST,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: ST
      DO J = 1, SIZE(ST,DIM=1)
         DO K = 1, SIZE(ST,DIM=2)
            CALL FMST2M(ST(J,K),RETURN_VALUE(J,K)%MFM)
         ENDDO
      ENDDO
   END FUNCTION FM_ST2

!                                                               TO_IM

   FUNCTION IM_I(IVAL)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: RETURN_VALUE
      INTEGER :: IVAL
      INTENT (IN) :: IVAL
      CALL IMI2M(IVAL,RETURN_VALUE%MIM)
   END FUNCTION IM_I

   FUNCTION IM_R(R)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: RETURN_VALUE
      REAL :: R
      CHARACTER(25) :: ST
      INTEGER :: IVAL
      INTENT (IN) :: R
      IF (ABS(R) < HUGE(1)) THEN
          IVAL = INT(R)
          CALL IMI2M(IVAL,RETURN_VALUE%MIM)
      ELSE
          WRITE (ST,'(E25.16)') R
          CALL IMST2M(ST,RETURN_VALUE%MIM)
      ENDIF
   END FUNCTION IM_R

   FUNCTION IM_D(D)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: RETURN_VALUE
      DOUBLE PRECISION :: D
      CHARACTER(25) :: ST
      INTEGER :: IVAL
      INTENT (IN) :: D
      IF (ABS(D) < HUGE(1)) THEN
          IVAL = INT(D)
          CALL IMI2M(IVAL,RETURN_VALUE%MIM)
      ELSE
          WRITE (ST,'(E25.16)') D
          CALL IMST2M(ST,RETURN_VALUE%MIM)
      ENDIF
   END FUNCTION IM_D

   FUNCTION IM_Z(Z)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: RETURN_VALUE
      COMPLEX :: Z
      REAL :: R
      CHARACTER(25) :: ST
      INTEGER :: IVAL
      INTENT (IN) :: Z
      R = REAL(Z)
      IF (ABS(R) < HUGE(1)) THEN
          IVAL = INT(R)
          CALL IMI2M(IVAL,RETURN_VALUE%MIM)
      ELSE
          WRITE (ST,'(E25.16)') R
          CALL IMST2M(ST,RETURN_VALUE%MIM)
      ENDIF
   END FUNCTION IM_Z

   FUNCTION IM_C(C)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: RETURN_VALUE
      COMPLEX (KIND(0.0D0)) :: C
      DOUBLE PRECISION :: D
      CHARACTER(25) :: ST
      INTEGER :: IVAL
      INTENT (IN) :: C
      D = REAL(C,KIND(0.0D0))
      IF (ABS(D) < HUGE(1)) THEN
          IVAL = INT(D)
          CALL IMI2M(IVAL,RETURN_VALUE%MIM)
      ELSE
          WRITE (ST,'(E25.16)') D
          CALL IMST2M(ST,RETURN_VALUE%MIM)
      ENDIF
   END FUNCTION IM_C

   FUNCTION IM_FM(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: RETURN_VALUE
      TYPE (FM) :: MA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMFM2I(MA%MFM,RETURN_VALUE%MIM)
   END FUNCTION IM_FM

   FUNCTION IM_IM(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: RETURN_VALUE,MA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%MIM,RETURN_VALUE%MIM)
   END FUNCTION IM_IM

   FUNCTION IM_ZM(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: RETURN_VALUE
      TYPE (ZM) :: MA
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      CALL ZMREAL(MA%MZM,MTLVFM)
      CALL IMFM2I(MTLVFM,RETURN_VALUE%MIM)
   END FUNCTION IM_ZM

   FUNCTION IM_ST(ST)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: RETURN_VALUE
      CHARACTER(*) :: ST
      INTENT (IN) :: ST
      CALL IMST2M(ST,RETURN_VALUE%MIM)
   END FUNCTION IM_ST

   FUNCTION IM_I1(IVAL)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: IVAL
      TYPE (IM), DIMENSION(SIZE(IVAL)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: IVAL
      N = SIZE(IVAL)
      DO J = 1, N
         CALL IMI2M(IVAL(J),RETURN_VALUE(J)%MIM)
      ENDDO
   END FUNCTION IM_I1

   FUNCTION IM_R1(R)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      REAL, DIMENSION(:) :: R
      TYPE (IM), DIMENSION(SIZE(R)) :: RETURN_VALUE
      CHARACTER(25) :: ST
      INTEGER :: IVAL,J,N
      INTENT (IN) :: R
      N = SIZE(R)
      DO J = 1, N
         IF (ABS(R(J)) < HUGE(1)) THEN
             IVAL = INT(R(J))
             CALL IMI2M(IVAL,RETURN_VALUE(J)%MIM)
         ELSE
             WRITE (ST,'(E25.16)') R(J)
             CALL IMST2M(ST,RETURN_VALUE(J)%MIM)
         ENDIF
      ENDDO
   END FUNCTION IM_R1

   FUNCTION IM_D1(D)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION, DIMENSION(:) :: D
      TYPE (IM), DIMENSION(SIZE(D)) :: RETURN_VALUE
      CHARACTER(25) :: ST
      INTEGER :: IVAL,J,N
      INTENT (IN) :: D
      N = SIZE(D)
      DO J = 1, N
         IF (ABS(D(J)) < HUGE(1)) THEN
             IVAL = INT(D(J))
             CALL IMI2M(IVAL,RETURN_VALUE(J)%MIM)
         ELSE
             WRITE (ST,'(E25.16)') D(J)
             CALL IMST2M(ST,RETURN_VALUE(J)%MIM)
         ENDIF
      ENDDO
   END FUNCTION IM_D1

   FUNCTION IM_Z1(Z)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX, DIMENSION(:) :: Z
      TYPE (IM), DIMENSION(SIZE(Z)) :: RETURN_VALUE
      REAL :: R
      CHARACTER(25) :: ST
      INTEGER :: IVAL,J,N
      INTENT (IN) :: Z
      N = SIZE(Z)
      DO J = 1, N
         R = REAL(Z(J))
         IF (ABS(R) < HUGE(1)) THEN
             IVAL = INT(R)
             CALL IMI2M(IVAL,RETURN_VALUE(J)%MIM)
         ELSE
             WRITE (ST,'(E25.16)') R
             CALL IMST2M(ST,RETURN_VALUE(J)%MIM)
         ENDIF
      ENDDO
   END FUNCTION IM_Z1

   FUNCTION IM_C1(C)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX (KIND(0.0D0)), DIMENSION(:) :: C
      TYPE (IM), DIMENSION(SIZE(C)) :: RETURN_VALUE
      DOUBLE PRECISION :: D
      CHARACTER(25) :: ST
      INTEGER :: IVAL,J,N
      INTENT (IN) :: C
      N = SIZE(C)
      DO J = 1, N
         D = REAL(C(J),KIND(0.0D0))
         IF (ABS(D) < HUGE(1)) THEN
             IVAL = INT(D)
             CALL IMI2M(IVAL,RETURN_VALUE(J)%MIM)
         ELSE
             WRITE (ST,'(E25.16)') D
             CALL IMST2M(ST,RETURN_VALUE(J)%MIM)
         ENDIF
      ENDDO
   END FUNCTION IM_C1

   FUNCTION IM_FM1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      TYPE (IM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL IMFM2I(MA(J)%MFM,RETURN_VALUE(J)%MIM)
      ENDDO
   END FUNCTION IM_FM1

   FUNCTION IM_IM1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (IM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL IMEQ(MA(J)%MIM,RETURN_VALUE(J)%MIM)
      ENDDO
   END FUNCTION IM_IM1

   FUNCTION IM_ZM1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      TYPE (IM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL ZMREAL(MA(J)%MZM,MTLVFM)
         CALL IMFM2I(MTLVFM,RETURN_VALUE(J)%MIM)
      ENDDO
   END FUNCTION IM_ZM1

   FUNCTION IM_ST1(ST)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*), DIMENSION(:) :: ST
      TYPE (IM), DIMENSION(SIZE(ST)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: ST
      N = SIZE(ST)
      DO J = 1, N
         CALL IMST2M(ST(J),RETURN_VALUE(J)%MIM)
      ENDDO
   END FUNCTION IM_ST1

   FUNCTION IM_I2(IVAL)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: IVAL
      TYPE (IM), DIMENSION(SIZE(IVAL,DIM=1),SIZE(IVAL,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: IVAL
      DO J = 1, SIZE(IVAL,DIM=1)
         DO K = 1, SIZE(IVAL,DIM=2)
            CALL IMI2M(IVAL(J,K),RETURN_VALUE(J,K)%MIM)
         ENDDO
      ENDDO
   END FUNCTION IM_I2

   FUNCTION IM_R2(R)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      REAL, DIMENSION(:,:) :: R
      TYPE (IM), DIMENSION(SIZE(R,DIM=1),SIZE(R,DIM=2)) :: RETURN_VALUE
      CHARACTER(25) :: ST
      INTEGER :: IVAL,J,K
      INTENT (IN) :: R
      DO J = 1, SIZE(R,DIM=1)
         DO K = 1, SIZE(R,DIM=2)
            IF (ABS(R(J,K)) < HUGE(1)) THEN
                IVAL = INT(R(J,K))
                CALL IMI2M(IVAL,RETURN_VALUE(J,K)%MIM)
            ELSE
                WRITE (ST,'(E25.16)') R(J,K)
                CALL IMST2M(ST,RETURN_VALUE(J,K)%MIM)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION IM_R2

   FUNCTION IM_D2(D)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION, DIMENSION(:,:) :: D
      TYPE (IM), DIMENSION(SIZE(D,DIM=1),SIZE(D,DIM=2)) :: RETURN_VALUE
      CHARACTER(25) :: ST
      INTEGER :: IVAL,J,K
      INTENT (IN) :: D
      DO J = 1, SIZE(D,DIM=1)
         DO K = 1, SIZE(D,DIM=2)
            IF (ABS(D(J,K)) < HUGE(1)) THEN
                IVAL = INT(D(J,K))
                CALL IMI2M(IVAL,RETURN_VALUE(J,K)%MIM)
            ELSE
                WRITE (ST,'(E25.16)') D(J,K)
                CALL IMST2M(ST,RETURN_VALUE(J,K)%MIM)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION IM_D2

   FUNCTION IM_Z2(Z)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX, DIMENSION(:,:) :: Z
      TYPE (IM), DIMENSION(SIZE(Z,DIM=1),SIZE(Z,DIM=2)) :: RETURN_VALUE
      REAL :: R
      CHARACTER(25) :: ST
      INTEGER :: IVAL,J,K
      INTENT (IN) :: Z
      DO J = 1, SIZE(Z,DIM=1)
         DO K = 1, SIZE(Z,DIM=2)
            R = REAL(Z(J,K))
            IF (ABS(R) < HUGE(1)) THEN
                IVAL = INT(R)
                CALL IMI2M(IVAL,RETURN_VALUE(J,K)%MIM)
            ELSE
                WRITE (ST,'(E25.16)') R
                CALL IMST2M(ST,RETURN_VALUE(J,K)%MIM)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION IM_Z2

   FUNCTION IM_C2(C)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX (KIND(0.0D0)), DIMENSION(:,:) :: C
      TYPE (IM), DIMENSION(SIZE(C,DIM=1),SIZE(C,DIM=2)) :: RETURN_VALUE
      DOUBLE PRECISION :: D
      CHARACTER(25) :: ST
      INTEGER :: IVAL,J,K
      INTENT (IN) :: C
      DO J = 1, SIZE(C,DIM=1)
         DO K = 1, SIZE(C,DIM=2)
            D = REAL(C(J,K),KIND(0.0D0))
            IF (ABS(D) < HUGE(1)) THEN
                IVAL = INT(D)
                CALL IMI2M(IVAL,RETURN_VALUE(J,K)%MIM)
            ELSE
                WRITE (ST,'(E25.16)') D
                CALL IMST2M(ST,RETURN_VALUE(J,K)%MIM)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION IM_C2

   FUNCTION IM_FM2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      TYPE (IM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMFM2I(MA(J,K)%MFM,RETURN_VALUE(J,K)%MIM)
         ENDDO
      ENDDO
   END FUNCTION IM_FM2

   FUNCTION IM_IM2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (IM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMEQ(MA(J,K)%MIM,RETURN_VALUE(J,K)%MIM)
         ENDDO
      ENDDO
   END FUNCTION IM_IM2

   FUNCTION IM_ZM2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      TYPE (IM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL ZMREAL(MA(J,K)%MZM,MTLVFM)
            CALL IMFM2I(MTLVFM,RETURN_VALUE(J,K)%MIM)
         ENDDO
      ENDDO
   END FUNCTION IM_ZM2

   FUNCTION IM_ST2(ST)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*), DIMENSION(:,:) :: ST
      TYPE (IM), DIMENSION(SIZE(ST,DIM=1),SIZE(ST,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: ST
      DO J = 1, SIZE(ST,DIM=1)
         DO K = 1, SIZE(ST,DIM=2)
            CALL IMST2M(ST(J,K),RETURN_VALUE(J,K)%MIM)
         ENDDO
      ENDDO
   END FUNCTION IM_ST2

!                                                               TO_ZM

   FUNCTION ZM_I(IVAL)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      INTEGER :: IVAL
      INTENT (IN) :: IVAL
      CALL ZMI2M(IVAL,RETURN_VALUE%MZM)
   END FUNCTION ZM_I

   FUNCTION ZM2_I(I1,I2)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      INTEGER :: I1,I2
      INTENT (IN) :: I1,I2
      CALL ZM2I2M(I1,I2,RETURN_VALUE%MZM)
   END FUNCTION ZM2_I

   FUNCTION ZM_R(R)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      REAL :: R
      INTENT (IN) :: R
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FMSP2M(R,MTLVFM)
      CALL FMI2M(0,MULVFM)
      CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE%MZM)
   END FUNCTION ZM_R

   FUNCTION ZM2_R(R1,R2)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      REAL :: R1,R2
      INTENT (IN) :: R1,R2
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FMSP2M(R1,MTLVFM)
      CALL FMSP2M(R2,MULVFM)
      CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE%MZM)
   END FUNCTION ZM2_R

   FUNCTION ZM_D(D)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      DOUBLE PRECISION :: D
      INTENT (IN) :: D
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FMDP2M(D,MTLVFM)
      CALL FMI2M(0,MULVFM)
      CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE%MZM)
   END FUNCTION ZM_D

   FUNCTION ZM2_D(D1,D2)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      DOUBLE PRECISION :: D1,D2
      INTENT (IN) :: D1,D2
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FMDP2M(D1,MTLVFM)
      CALL FMDP2M(D2,MULVFM)
      CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE%MZM)
   END FUNCTION ZM2_D

   FUNCTION ZM_Z(Z)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      COMPLEX :: Z
      INTENT (IN) :: Z
      CALL ZMZ2M(Z,RETURN_VALUE%MZM)
   END FUNCTION ZM_Z

   FUNCTION ZM_C(C)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      COMPLEX (KIND(0.0D0)) :: C
      INTENT (IN) :: C
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FMDP2M(REAL(C,KIND(0.0D0)),MTLVFM)
      CALL FMDP2M(AIMAG(C),MULVFM)
      CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE%MZM)
   END FUNCTION ZM_C

   FUNCTION ZM_FM(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      TYPE (FM) :: MA
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MULVFM
      CALL FM_UNDEF_INP(MA)
      CALL FMI2M(0,MULVFM)
      CALL ZMCMPX(MA%MFM,MULVFM,RETURN_VALUE%MZM)
   END FUNCTION ZM_FM

   FUNCTION ZM_IM(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      TYPE (IM) :: MA
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FM_UNDEF_INP(MA)
      CALL IMI2FM(MA%MIM,MTLVFM)
      CALL FMI2M(0,MULVFM)
      CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE%MZM)
   END FUNCTION ZM_IM

   FUNCTION ZM_ZM(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE,MA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL ZMEQ(MA%MZM,RETURN_VALUE%MZM)
   END FUNCTION ZM_ZM

   FUNCTION ZM_ST(ST)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: RETURN_VALUE
      CHARACTER(*) :: ST
      INTENT (IN) :: ST
      CALL ZMST2M(ST,RETURN_VALUE%MZM)
   END FUNCTION ZM_ST

   FUNCTION ZM_I1(IVAL)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: IVAL
      TYPE (ZM), DIMENSION(SIZE(IVAL)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: IVAL
      N = SIZE(IVAL)
      DO J = 1, N
         CALL ZMI2M(IVAL(J),RETURN_VALUE(J)%MZM)
      ENDDO
   END FUNCTION ZM_I1

   FUNCTION ZM_R1(R)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      REAL, DIMENSION(:) :: R
      TYPE (ZM), DIMENSION(SIZE(R)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: R
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      N = SIZE(R)
      CALL FMI2M(0,MULVFM)
      DO J = 1, N
         CALL FMSP2M(R(J),MTLVFM)
         CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE(J)%MZM)
      ENDDO
   END FUNCTION ZM_R1

   FUNCTION ZM_D1(D)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION, DIMENSION(:) :: D
      TYPE (ZM), DIMENSION(SIZE(D)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: D
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      N = SIZE(D)
      CALL FMI2M(0,MULVFM)
      DO J = 1, N
         CALL FMDP2M(D(J),MTLVFM)
         CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE(J)%MZM)
      ENDDO
   END FUNCTION ZM_D1

   FUNCTION ZM_Z1(Z)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX, DIMENSION(:) :: Z
      TYPE (ZM), DIMENSION(SIZE(Z)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: Z
      N = SIZE(Z)
      DO J = 1, N
         CALL ZMZ2M(Z(J),RETURN_VALUE(J)%MZM)
      ENDDO
   END FUNCTION ZM_Z1

   FUNCTION ZM_C1(C)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX (KIND(0.0D0)), DIMENSION(:) :: C
      TYPE (ZM), DIMENSION(SIZE(C)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: C
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      N = SIZE(C)
      DO J = 1, N
         CALL FMDP2M(REAL(C(J),KIND(0.0D0)),MTLVFM)
         CALL FMDP2M(AIMAG(C(J)),MULVFM)
         CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE(J)%MZM)
      ENDDO
   END FUNCTION ZM_C1

   FUNCTION ZM_FM1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      TYPE (ZM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MULVFM
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      CALL FMI2M(0,MULVFM)
      DO J = 1, N
         CALL ZMCMPX(MA(J)%MFM,MULVFM,RETURN_VALUE(J)%MZM)
      ENDDO
   END FUNCTION ZM_FM1

   FUNCTION ZM_IM1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (ZM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      CALL FMI2M(0,MULVFM)
      DO J = 1, N
         CALL IMI2FM(MA(J)%MIM,MTLVFM)
         CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE(J)%MZM)
      ENDDO
   END FUNCTION ZM_IM1

   FUNCTION ZM_ZM1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      TYPE (ZM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL ZMEQ(MA(J)%MZM,RETURN_VALUE(J)%MZM)
      ENDDO
   END FUNCTION ZM_ZM1

   FUNCTION ZM_ST1(ST)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*), DIMENSION(:) :: ST
      TYPE (ZM), DIMENSION(SIZE(ST)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: ST
      N = SIZE(ST)
      DO J = 1, N
         CALL ZMST2M(ST(J),RETURN_VALUE(J)%MZM)
      ENDDO
   END FUNCTION ZM_ST1

   FUNCTION ZM_I2(IVAL)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: IVAL
      TYPE (ZM), DIMENSION(SIZE(IVAL,DIM=1),SIZE(IVAL,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: IVAL
      DO J = 1, SIZE(IVAL,DIM=1)
         DO K = 1, SIZE(IVAL,DIM=2)
            CALL ZMI2M(IVAL(J,K),RETURN_VALUE(J,K)%MZM)
         ENDDO
      ENDDO
   END FUNCTION ZM_I2

   FUNCTION ZM_R2(R)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      REAL, DIMENSION(:,:) :: R
      TYPE (ZM), DIMENSION(SIZE(R,DIM=1),SIZE(R,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: R
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FMI2M(0,MULVFM)
      DO J = 1, SIZE(R,DIM=1)
         DO K = 1, SIZE(R,DIM=2)
            CALL FMSP2M(R(J,K),MTLVFM)
            CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE(J,K)%MZM)
         ENDDO
      ENDDO
   END FUNCTION ZM_R2

   FUNCTION ZM_D2(D)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION, DIMENSION(:,:) :: D
      TYPE (ZM), DIMENSION(SIZE(D,DIM=1),SIZE(D,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: D
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FMI2M(0,MULVFM)
      DO J = 1, SIZE(D,DIM=1)
         DO K = 1, SIZE(D,DIM=2)
            CALL FMDP2M(D(J,K),MTLVFM)
            CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE(J,K)%MZM)
         ENDDO
      ENDDO
   END FUNCTION ZM_D2

   FUNCTION ZM_Z2(Z)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX, DIMENSION(:,:) :: Z
      TYPE (ZM), DIMENSION(SIZE(Z,DIM=1),SIZE(Z,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: Z
      DO J = 1, SIZE(Z,DIM=1)
         DO K = 1, SIZE(Z,DIM=2)
            CALL ZMZ2M(Z(J,K),RETURN_VALUE(J,K)%MZM)
         ENDDO
      ENDDO
   END FUNCTION ZM_Z2

   FUNCTION ZM_C2(C)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX (KIND(0.0D0)), DIMENSION(:,:) :: C
      TYPE (ZM), DIMENSION(SIZE(C,DIM=1),SIZE(C,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: C
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      DO J = 1, SIZE(C,DIM=1)
         DO K = 1, SIZE(C,DIM=2)
            CALL FMDP2M(REAL(C(J,K),KIND(0.0D0)),MTLVFM)
            CALL FMDP2M(AIMAG(C(J,K)),MULVFM)
            CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE(J,K)%MZM)
         ENDDO
      ENDDO
   END FUNCTION ZM_C2

   FUNCTION ZM_FM2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      TYPE (ZM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MULVFM
      CALL FM_UNDEF_INP(MA)
      CALL FMI2M(0,MULVFM)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL ZMCMPX(MA(J,K)%MFM,MULVFM,RETURN_VALUE(J,K)%MZM)
         ENDDO
      ENDDO
   END FUNCTION ZM_FM2

   FUNCTION ZM_IM2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (ZM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM,MULVFM
      CALL FM_UNDEF_INP(MA)
      CALL FMI2M(0,MULVFM)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMI2FM(MA(J,K)%MIM,MTLVFM)
            CALL ZMCMPX(MTLVFM,MULVFM,RETURN_VALUE(J,K)%MZM)
         ENDDO
      ENDDO
   END FUNCTION ZM_IM2

   FUNCTION ZM_ZM2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      TYPE (ZM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL ZMEQ(MA(J,K)%MZM,RETURN_VALUE(J,K)%MZM)
         ENDDO
      ENDDO
   END FUNCTION ZM_ZM2

   FUNCTION ZM_ST2(ST)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*), DIMENSION(:,:) :: ST
      TYPE (ZM), DIMENSION(SIZE(ST,DIM=1),SIZE(ST,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: ST
      DO J = 1, SIZE(ST,DIM=1)
         DO K = 1, SIZE(ST,DIM=2)
            CALL ZMST2M(ST(J,K),RETURN_VALUE(J,K)%MZM)
         ENDDO
      ENDDO
   END FUNCTION ZM_ST2

!                                                              TO_INT

   FUNCTION FM_2INT(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      INTEGER :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FMM2I(MA%MFM,RETURN_VALUE)
   END FUNCTION FM_2INT

   FUNCTION IM_2INT(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      INTEGER :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMM2I(MA%MIM,RETURN_VALUE)
   END FUNCTION IM_2INT

   FUNCTION ZM_2INT(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA
      INTEGER :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL ZMM2I(MA%MZM,RETURN_VALUE)
   END FUNCTION ZM_2INT

   FUNCTION FM_2INT1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      INTEGER, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL FMM2I(MA(J)%MFM,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FM_2INT1

   FUNCTION IM_2INT1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      INTEGER, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL IMM2I(MA(J)%MIM,RETURN_VALUE(J))
      ENDDO
   END FUNCTION IM_2INT1

   FUNCTION ZM_2INT1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      INTEGER, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL FMM2I(MA(J)%MZM(1),RETURN_VALUE(J))
      ENDDO
   END FUNCTION ZM_2INT1

   FUNCTION FM_2INT2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      INTEGER, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMM2I(MA(J,K)%MFM,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FM_2INT2

   FUNCTION IM_2INT2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      INTEGER, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMM2I(MA(J,K)%MIM,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION IM_2INT2

   FUNCTION ZM_2INT2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      INTEGER, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMM2I(MA(J,K)%MZM(1),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION ZM_2INT2

!                                                               TO_SP

   FUNCTION FM_2SP(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      REAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FMM2SP(MA%MFM,RETURN_VALUE)
   END FUNCTION FM_2SP

   FUNCTION IM_2SP(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      REAL :: RETURN_VALUE
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      CALL IMI2FM(MA%MIM,MTLVFM)
      CALL FMM2SP(MTLVFM,RETURN_VALUE)
   END FUNCTION IM_2SP

   FUNCTION ZM_2SP(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA
      REAL :: RETURN_VALUE
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      CALL ZMREAL(MA%MZM,MTLVFM)
      CALL FMM2SP(MTLVFM,RETURN_VALUE)
   END FUNCTION ZM_2SP

   FUNCTION FM_2SP1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      REAL, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL FMM2SP(MA(J)%MFM,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FM_2SP1

   FUNCTION IM_2SP1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      REAL, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL IMM2SP(MA(J)%MIM,RETURN_VALUE(J))
      ENDDO
   END FUNCTION IM_2SP1

   FUNCTION ZM_2SP1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      REAL, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL FMM2SP(MA(J)%MZM(1),RETURN_VALUE(J))
      ENDDO
   END FUNCTION ZM_2SP1

   FUNCTION FM_2SP2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      REAL, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMM2SP(MA(J,K)%MFM,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FM_2SP2

   FUNCTION IM_2SP2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      REAL, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMM2SP(MA(J,K)%MIM,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION IM_2SP2

   FUNCTION ZM_2SP2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      REAL, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMM2SP(MA(J,K)%MZM(1),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION ZM_2SP2

!                                                               TO_DP

   FUNCTION FM_2DP(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      DOUBLE PRECISION :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FMM2DP(MA%MFM,RETURN_VALUE)
   END FUNCTION FM_2DP

   FUNCTION IM_2DP(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      DOUBLE PRECISION :: RETURN_VALUE
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      CALL IMI2FM(MA%MIM,MTLVFM)
      CALL FMM2DP(MTLVFM,RETURN_VALUE)
   END FUNCTION IM_2DP

   FUNCTION ZM_2DP(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA
      DOUBLE PRECISION :: RETURN_VALUE
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      CALL ZMREAL(MA%MZM,MTLVFM)
      CALL FMM2DP(MTLVFM,RETURN_VALUE)
   END FUNCTION ZM_2DP

   FUNCTION FM_2DP1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      DOUBLE PRECISION, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL FMM2DP(MA(J)%MFM,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FM_2DP1

   FUNCTION IM_2DP1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      DOUBLE PRECISION, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL IMM2DP(MA(J)%MIM,RETURN_VALUE(J))
      ENDDO
   END FUNCTION IM_2DP1

   FUNCTION ZM_2DP1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      DOUBLE PRECISION, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL FMM2DP(MA(J)%MZM(1),RETURN_VALUE(J))
      ENDDO
   END FUNCTION ZM_2DP1

   FUNCTION FM_2DP2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      DOUBLE PRECISION, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMM2DP(MA(J,K)%MFM,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FM_2DP2

   FUNCTION IM_2DP2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      DOUBLE PRECISION, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMM2DP(MA(J,K)%MIM,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION IM_2DP2

   FUNCTION ZM_2DP2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      DOUBLE PRECISION, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMM2DP(MA(J,K)%MZM(1),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION ZM_2DP2

!                                                              TO_SPZ

   FUNCTION FM_2SPZ(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      COMPLEX :: RETURN_VALUE
      REAL :: R
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FMM2SP(MA%MFM,R)
      RETURN_VALUE = CMPLX( R , 0.0 )
   END FUNCTION FM_2SPZ

   FUNCTION IM_2SPZ(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      COMPLEX :: RETURN_VALUE
      REAL :: R
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      CALL IMI2FM(MA%MIM,MTLVFM)
      CALL FMM2SP(MTLVFM,R)
      RETURN_VALUE = CMPLX( R , 0.0 )
   END FUNCTION IM_2SPZ

   FUNCTION ZM_2SPZ(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA
      COMPLEX :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL ZMM2Z(MA%MZM,RETURN_VALUE)
   END FUNCTION ZM_2SPZ

   FUNCTION FM_2SPZ1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      COMPLEX, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      REAL :: R
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL FMM2SP(MA(J)%MFM,R)
         RETURN_VALUE(J) = CMPLX( R , 0.0 )
      ENDDO
   END FUNCTION FM_2SPZ1

   FUNCTION IM_2SPZ1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      COMPLEX, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      REAL :: R
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL IMI2FM(MA(J)%MIM,MTLVFM)
         CALL FMM2SP(MTLVFM,R)
         RETURN_VALUE(J) = CMPLX( R , 0.0 )
      ENDDO
   END FUNCTION IM_2SPZ1

   FUNCTION ZM_2SPZ1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      COMPLEX, DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL ZMM2Z(MA(J)%MZM,RETURN_VALUE(J))
      ENDDO
   END FUNCTION ZM_2SPZ1

   FUNCTION FM_2SPZ2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      COMPLEX, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      REAL :: R
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMM2SP(MA(J,K)%MFM,R)
            RETURN_VALUE(J,K) = CMPLX( R , 0.0 )
         ENDDO
      ENDDO
   END FUNCTION FM_2SPZ2

   FUNCTION IM_2SPZ2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      COMPLEX, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      REAL :: R
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMI2FM(MA(J,K)%MIM,MTLVFM)
            CALL FMM2SP(MTLVFM,R)
            RETURN_VALUE(J,K) = CMPLX( R , 0.0 )
         ENDDO
      ENDDO
   END FUNCTION IM_2SPZ2

   FUNCTION ZM_2SPZ2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      COMPLEX, DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL ZMM2Z(MA(J,K)%MZM,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION ZM_2SPZ2

!                                                              TO_DPZ

   FUNCTION FM_2DPZ(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      COMPLEX (KIND(0.0D0)) :: RETURN_VALUE
      DOUBLE PRECISION :: D
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FMM2DP(MA%MFM,D)
      RETURN_VALUE = CMPLX( D , 0.0D0 , KIND(0.0D0) )
   END FUNCTION FM_2DPZ

   FUNCTION IM_2DPZ(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      COMPLEX (KIND(0.0D0)) :: RETURN_VALUE
      DOUBLE PRECISION :: D
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMM2DP(MA%MIM,D)
      RETURN_VALUE = CMPLX( D , 0.0D0 , KIND(0.0D0) )
   END FUNCTION IM_2DPZ

   FUNCTION ZM_2DPZ(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA
      COMPLEX (KIND(0.0D0)) :: RETURN_VALUE
      DOUBLE PRECISION :: D1,D2
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      CALL ZMREAL(MA%MZM,MTLVFM)
      CALL FMM2DP(MTLVFM,D1)
      CALL ZMIMAG(MA%MZM,MTLVFM)
      CALL FMM2DP(MTLVFM,D2)
      RETURN_VALUE = CMPLX( D1 , D2 , KIND(0.0D0) )
   END FUNCTION ZM_2DPZ

   FUNCTION FM_2DPZ1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      COMPLEX (KIND(0.0D0)), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      DOUBLE PRECISION :: D
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL FMM2DP(MA(J)%MFM,D)
         RETURN_VALUE(J) = CMPLX( D , 0.0D0 , KIND(0.0D0) )
      ENDDO
   END FUNCTION FM_2DPZ1

   FUNCTION IM_2DPZ1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      COMPLEX (KIND(0.0D0)), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      DOUBLE PRECISION :: D
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL IMI2FM(MA(J)%MIM,MTLVFM)
         CALL FMM2DP(MTLVFM,D)
         RETURN_VALUE(J) = CMPLX( D , 0.0D0 , KIND(0.0D0) )
      ENDDO
   END FUNCTION IM_2DPZ1

   FUNCTION ZM_2DPZ1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      COMPLEX (KIND(0.0D0)), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J,N
      DOUBLE PRECISION :: D1,D2
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      DO J = 1, N
         CALL ZMREAL(MA(J)%MZM,MTLVFM)
         CALL FMM2DP(MTLVFM,D1)
         CALL ZMIMAG(MA(J)%MZM,MTLVFM)
         CALL FMM2DP(MTLVFM,D2)
         RETURN_VALUE(J) = CMPLX( D1 , D2 , KIND(0.0D0) )
      ENDDO
   END FUNCTION ZM_2DPZ1

   FUNCTION FM_2DPZ2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      COMPLEX (KIND(0.0D0)), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      DOUBLE PRECISION :: D
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMM2DP(MA(J,K)%MFM,D)
            RETURN_VALUE(J,K) = CMPLX( D , 0.0D0 , KIND(0.0D0) )
         ENDDO
      ENDDO
   END FUNCTION FM_2DPZ2

   FUNCTION IM_2DPZ2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      COMPLEX (KIND(0.0D0)), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      DOUBLE PRECISION :: D
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMI2FM(MA(J,K)%MIM,MTLVFM)
            CALL FMM2DP(MTLVFM,D)
            RETURN_VALUE(J,K) = CMPLX( D , 0.0D0 , KIND(0.0D0) )
         ENDDO
      ENDDO
   END FUNCTION IM_2DPZ2

   FUNCTION ZM_2DPZ2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      COMPLEX (KIND(0.0D0)), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      DOUBLE PRECISION :: D1,D2
      INTENT (IN) :: MA
      TYPE(MULTI), SAVE :: MTLVFM
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL ZMREAL(MA(J,K)%MZM,MTLVFM)
            CALL FMM2DP(MTLVFM,D1)
            CALL ZMIMAG(MA(J,K)%MZM,MTLVFM)
            CALL FMM2DP(MTLVFM,D2)
            RETURN_VALUE(J,K) = CMPLX( D1 , D2 , KIND(0.0D0) )
         ENDDO
      ENDDO
   END FUNCTION ZM_2DPZ2

   SUBROUTINE FM_EQ(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FM_UNDEF_INP(MA)
      CALL FMEQ(MA%MFM,MB%MFM)
   END SUBROUTINE FM_EQ

   SUBROUTINE IM_EQ(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%MIM,MB%MIM)
   END SUBROUTINE IM_EQ

   SUBROUTINE ZM_EQ(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FM_UNDEF_INP(MA)
      CALL ZMEQ(MA%MZM,MB%MZM)
   END SUBROUTINE ZM_EQ

!                                                         IS_OVERFLOW

   FUNCTION FM_IS_OVERFLOW(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%MFM%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
   END FUNCTION FM_IS_OVERFLOW

   FUNCTION IM_IS_OVERFLOW(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%MIM%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
   END FUNCTION IM_IS_OVERFLOW

   FUNCTION ZM_IS_OVERFLOW(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%MZM(1)%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
      IF (MA%MZM(2)%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
   END FUNCTION ZM_IS_OVERFLOW

   FUNCTION FM_IS_OVERFLOW1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%MFM%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION FM_IS_OVERFLOW1

   FUNCTION IM_IS_OVERFLOW1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%MIM%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION IM_IS_OVERFLOW1

   FUNCTION ZM_IS_OVERFLOW1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%MZM(1)%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
         IF (MA(J)%MZM(2)%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION ZM_IS_OVERFLOW1

   FUNCTION FM_IS_OVERFLOW2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%MFM%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION FM_IS_OVERFLOW2

   FUNCTION IM_IS_OVERFLOW2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%MIM%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION IM_IS_OVERFLOW2

   FUNCTION ZM_IS_OVERFLOW2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%MZM(1)%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
            IF (MA(J,K)%MZM(2)%MP(2) == MEXPOV) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION ZM_IS_OVERFLOW2

!                                                        IS_UNDERFLOW

   FUNCTION FM_IS_UNDERFLOW(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%MFM%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
   END FUNCTION FM_IS_UNDERFLOW

!  The integer versions are included for completeness, but type (im) numbers can't underflow.

   FUNCTION IM_IS_UNDERFLOW(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%MIM%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
   END FUNCTION IM_IS_UNDERFLOW

   FUNCTION ZM_IS_UNDERFLOW(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%MZM(1)%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
      IF (MA%MZM(2)%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
   END FUNCTION ZM_IS_UNDERFLOW

   FUNCTION FM_IS_UNDERFLOW1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%MFM%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION FM_IS_UNDERFLOW1

   FUNCTION IM_IS_UNDERFLOW1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%MIM%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION IM_IS_UNDERFLOW1

   FUNCTION ZM_IS_UNDERFLOW1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%MZM(1)%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
         IF (MA(J)%MZM(2)%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION ZM_IS_UNDERFLOW1

   FUNCTION FM_IS_UNDERFLOW2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%MFM%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION FM_IS_UNDERFLOW2

   FUNCTION IM_IS_UNDERFLOW2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%MIM%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION IM_IS_UNDERFLOW2

   FUNCTION ZM_IS_UNDERFLOW2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%MZM(1)%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
            IF (MA(J,K)%MZM(2)%MP(2) == MEXPUN) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION ZM_IS_UNDERFLOW2

!                                                          IS_UNKNOWN

   FUNCTION FM_IS_UNKNOWN(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%MFM%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
   END FUNCTION FM_IS_UNKNOWN

   FUNCTION IM_IS_UNKNOWN(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%MIM%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
   END FUNCTION IM_IS_UNKNOWN

   FUNCTION ZM_IS_UNKNOWN(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%MZM(1)%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
      IF (MA%MZM(2)%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
   END FUNCTION ZM_IS_UNKNOWN

   FUNCTION FM_IS_UNKNOWN1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%MFM%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION FM_IS_UNKNOWN1

   FUNCTION IM_IS_UNKNOWN1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%MIM%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION IM_IS_UNKNOWN1

   FUNCTION ZM_IS_UNKNOWN1(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%MZM(1)%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
         IF (MA(J)%MZM(2)%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION ZM_IS_UNKNOWN1

   FUNCTION FM_IS_UNKNOWN2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%MFM%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION FM_IS_UNKNOWN2

   FUNCTION IM_IS_UNKNOWN2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%MIM%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION IM_IS_UNKNOWN2

   FUNCTION ZM_IS_UNKNOWN2(MA)     RESULT (RETURN_VALUE)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%MZM(1)%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
            IF (MA(J,K)%MZM(2)%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION ZM_IS_UNKNOWN2

   SUBROUTINE FM_UNDEF_INP_FM0(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      INTENT (IN) :: MA
      IF (.NOT. ALLOCATED(MA%MFM%MP)) CALL FM_INPUT_ERROR
   END SUBROUTINE FM_UNDEF_INP_FM0

   SUBROUTINE FM_UNDEF_INP_IM0(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      INTENT (IN) :: MA
      IF (.NOT. ALLOCATED(MA%MIM%MP)) CALL FM_INPUT_ERROR
   END SUBROUTINE FM_UNDEF_INP_IM0

   SUBROUTINE FM_UNDEF_INP_ZM0(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM) :: MA
      INTENT (IN) :: MA
      IF (.NOT. ALLOCATED(MA%MZM(1)%MP)) CALL FM_INPUT_ERROR
      IF (.NOT. ALLOCATED(MA%MZM(2)%MP)) CALL FM_INPUT_ERROR
   END SUBROUTINE FM_UNDEF_INP_ZM0

   SUBROUTINE FM_UNDEF_INP_FM1(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      INTEGER :: J
      INTENT (IN) :: MA
      DO J = 1, SIZE(MA)
         IF (.NOT. ALLOCATED(MA(J)%MFM%MP)) CALL FM_INPUT_ERROR1(J)
      ENDDO
   END SUBROUTINE FM_UNDEF_INP_FM1

   SUBROUTINE FM_UNDEF_INP_IM1(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      INTEGER :: J
      INTENT (IN) :: MA
      DO J = 1, SIZE(MA)
         IF (.NOT. ALLOCATED(MA(J)%MIM%MP)) CALL FM_INPUT_ERROR1(J)
      ENDDO
   END SUBROUTINE FM_UNDEF_INP_IM1

   SUBROUTINE FM_UNDEF_INP_ZM1(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      INTEGER :: J
      INTENT (IN) :: MA
      DO J = 1, SIZE(MA)
         IF (.NOT. ALLOCATED(MA(J)%MZM(1)%MP)) CALL FM_INPUT_ERROR1(J)
         IF (.NOT. ALLOCATED(MA(J)%MZM(2)%MP)) CALL FM_INPUT_ERROR1(J)
      ENDDO
   END SUBROUTINE FM_UNDEF_INP_ZM1

   SUBROUTINE FM_UNDEF_INP_FM2(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      INTEGER :: J,K
      INTENT (IN) :: MA
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (.NOT. ALLOCATED(MA(J,K)%MFM%MP)) CALL FM_INPUT_ERROR2(J,K)
         ENDDO
      ENDDO
   END SUBROUTINE FM_UNDEF_INP_FM2

   SUBROUTINE FM_UNDEF_INP_IM2(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      INTEGER :: J,K
      INTENT (IN) :: MA
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (.NOT. ALLOCATED(MA(J,K)%MIM%MP)) CALL FM_INPUT_ERROR2(J,K)
         ENDDO
      ENDDO
   END SUBROUTINE FM_UNDEF_INP_IM2

   SUBROUTINE FM_UNDEF_INP_ZM2(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      INTEGER :: J,K
      INTENT (IN) :: MA
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (.NOT. ALLOCATED(MA(J,K)%MZM(1)%MP)) CALL FM_INPUT_ERROR2(J,K)
            IF (.NOT. ALLOCATED(MA(J,K)%MZM(2)%MP)) CALL FM_INPUT_ERROR2(J,K)
         ENDDO
      ENDDO
   END SUBROUTINE FM_UNDEF_INP_ZM2

   SUBROUTINE FM_DEALLOCATE_FM1(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:) :: MA
      INTENT (INOUT) :: MA
      IF (SIZE(MA) < 0 .AND. NDIG < 0) WRITE (*,*) ' Invalid NDIG = ', NDIG
   END SUBROUTINE FM_DEALLOCATE_FM1

   SUBROUTINE FM_DEALLOCATE_IM1(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      INTENT (INOUT) :: MA
      IF (SIZE(MA) < 0 .AND. NDIG < 0) WRITE (*,*) ' Invalid NDIG = ', NDIG
   END SUBROUTINE FM_DEALLOCATE_IM1

   SUBROUTINE FM_DEALLOCATE_ZM1(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:) :: MA
      INTENT (INOUT) :: MA
      IF (SIZE(MA) < 0 .AND. NDIG < 0) WRITE (*,*) ' Invalid NDIG = ', NDIG
   END SUBROUTINE FM_DEALLOCATE_ZM1

   SUBROUTINE FM_DEALLOCATE_FM2(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (FM), DIMENSION(:,:) :: MA
      INTENT (INOUT) :: MA
      IF (SIZE(MA) < 0 .AND. NDIG < 0) WRITE (*,*) ' Invalid NDIG = ', NDIG
   END SUBROUTINE FM_DEALLOCATE_FM2

   SUBROUTINE FM_DEALLOCATE_IM2(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      INTENT (INOUT) :: MA
      IF (SIZE(MA) < 0 .AND. NDIG < 0) WRITE (*,*) ' Invalid NDIG = ', NDIG
   END SUBROUTINE FM_DEALLOCATE_IM2

   SUBROUTINE FM_DEALLOCATE_ZM2(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE (ZM), DIMENSION(:,:) :: MA
      INTENT (INOUT) :: MA
      IF (SIZE(MA) < 0 .AND. NDIG < 0) WRITE (*,*) ' Invalid NDIG = ', NDIG
   END SUBROUTINE FM_DEALLOCATE_ZM2

   SUBROUTINE FM_INPUT_ERROR
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: ERR(2) = (/ 22, 23 /)
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) ' ***  Error in a program using the FM package  ***'
      WRITE (*,*) ' '
      WRITE (*,*) ' A multiple precision number is undefined in an expression or as an input'
      WRITE (*,*) ' argument to a subprogram.'
      WRITE (*,*) ' '
      WRITE (*,*) ' The most common cause of this error is:'
      WRITE (*,*) "     A multiple-precision variable (type(fm), (im), (zm), etc.) appears in an"
      WRITE (*,*) "     expression in the user's program before it has been given a value."
      WRITE (*,*) ' '
      WRITE (*,*) ' To help isolate the code that caused this error, this error message is followed'
      WRITE (*,*) ' by an illegal out-of-range array reference.  Some compilers have an option for'
      WRITE (*,*) ' checking array bounds and will give a traceback with the line number in the'
      WRITE (*,*) ' calling program where the error originated.'
      WRITE (*,*) ' '
      WRITE (*,*) ' The program has been stopped.'
      WRITE (*,*) ' '
      IF (ERR(-NDIG) > 22) WRITE (*,*) ' Negative array subscript.'
      STOP
   END SUBROUTINE FM_INPUT_ERROR

   SUBROUTINE FM_INPUT_ERROR1(J)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: J
      INTEGER :: ERR(2) = (/ 22, 23 /)
      INTENT (IN) :: J
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) ' ***  Error in a program using the FM package  ***'
      WRITE (*,*) ' '
      WRITE (*,*) ' Element (',J,') of a multiple precision one-dimensional array'
      WRITE (*,*) ' is undefined in an expression.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Note that if an array section is being used, like A(6:10), then if A(7) is'
      WRITE (*,*) ' undefined it will be listed as element (2) here.'
      WRITE (*,*) ' '
      WRITE (*,*) ' The most common cause of this error is:'
      WRITE (*,*) " 1.  A multiple-precision variable (type(fm), (im), (zm), etc.) appears in an"
      WRITE (*,*) "     expression in the user's program before it has been given a value."
      WRITE (*,*) ' '
      WRITE (*,*) ' To help isolate the code that caused this error, this error message is followed'
      WRITE (*,*) ' by an illegal out-of-range array reference.  Some compilers have an option for'
      WRITE (*,*) ' checking array bounds and will give a traceback with the line number in the'
      WRITE (*,*) ' calling program where the error originated.'
      WRITE (*,*) ' '
      WRITE (*,*) ' The program has been stopped.'
      WRITE (*,*) ' '
      IF (ERR(-NDIG) > 22) WRITE (*,*) ' Negative array subscript.'
      STOP
   END SUBROUTINE FM_INPUT_ERROR1

   SUBROUTINE FM_INPUT_ERROR2(J,K)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: J,K
      INTEGER :: ERR(2) = (/ 22, 23 /)
      INTENT (IN) :: J,K
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) ' ***  Error in a program using the FM package  ***'
      WRITE (*,*) ' '
      WRITE (*,*) ' Element (',J,',',K,') of a multiple precision two-dimensional array'
      WRITE (*,*) ' is undefined in an expression.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Note that if an array section is being used, like A(6:10), then if A(7) is'
      WRITE (*,*) ' undefined it will be listed as element (2) here.'
      WRITE (*,*) ' '
      WRITE (*,*) ' The most common cause of this error is:'
      WRITE (*,*) "     A multiple-precision variable (type(fm), (im), (zm), etc.) appears in an"
      WRITE (*,*) "     expression in the user's program before it has been given a value."
      WRITE (*,*) ' '
      WRITE (*,*) ' To help isolate the code that caused this error, this error message is followed'
      WRITE (*,*) ' by an illegal out-of-range array reference.  Some compilers have an option for'
      WRITE (*,*) ' checking array bounds and will give a traceback with the line number in the'
      WRITE (*,*) ' calling program where the error originated.'
      WRITE (*,*) ' '
      WRITE (*,*) ' The program has been stopped.'
      WRITE (*,*) ' '
      IF (ERR(-NDIG) > 22) WRITE (*,*) ' Negative array subscript.'
      STOP
   END SUBROUTINE FM_INPUT_ERROR2

 END MODULE ModLib_FMZM_1
