    SUBROUTINE DOCUMENTATION

!     FM 1.4                              David M. Smith                           9-01-2021


!  The routines in this package perform multiple precision arithmetic and functions
!  on three kinds of numbers.
!  FM routines handle floating-point real multiple precision numbers,
!  IM routines handle integer multiple precision numbers, and
!  ZM routines handle floating-point complex multiple precision numbers.
!  References to FM numbers below mean the low-level array form of the number used by the routines
!  in FM.f95, and not the derived type (fm) numbers handled by the FMZM module.  Logically, both may
!  refer to the same multiple precision number, but the syntax for dealing with the two types of
!  objects is different.  The same is true of references to IM numbers and ZM numbers below.

!  These are the basic routines for the FM package, and the expectation is that the user will not
!  call these routines directly.  The typical usage is for a program to declare multiple precision
!  variables with the three derived types defined in module FMZM in file FMZM90.f95.  Then that
!  module provides the interface between the user's program and the routines in this file.  See the
!  documentation in the FM_User_Manual.txt file for advice on using the FMZM module.
!  The information below is intended as a technical reference on the inner workings of FM, and most
!  FM users should not need to study it.


!  1. INITIALIZING THE PACKAGE

!  The variables that contain values to be shared by the different routines are located in module
!  FMVALS in file FMSAVE.f95.  Variables that are described below for controlling various features
!  of the FM package are found in this module.  They are initialized to default values assuming
!  32-bit integers and 64-bit double precision representation of the arrays holding multiple
!  precision numbers.  The base and number of digits to be used are initialized to give slightly
!  more than 50 decimal digits.  Subroutine FMVARS can be used to get a list of these variables
!  and their values.

!  The intent of module FMVALS is to hide the FM internal variables from the user's program, so that
!  no name conflicts can occur.  Subroutine FMSETVAR can be used to change the variables listed
!  below to new values.  It is not always safe to try to change these variables directly by putting
!  USE ModLib_FMVALS into the calling program and then changing them by hand.  Some of the saved constants
!  depend upon others, so that changing one variable may cause errors if others depending on that
!  one are not also changed.  FMSETVAR automatically updates any others that depend upon the one
!  being changed.

!  Subroutine FMSET also initializes these variables.  It tries to compute the best value for each,
!  and it checks several of the default values set in FMVALS to see that they are reasonable for a
!  given machine.  FMSET can also be called to set or change the current precision level for the
!  multiple precision numbers.

!  Calling FMSET is optional starting in version 1.2 of the FM package.  In previous versions one
!  call was required before any other routine in the package could be used.

!  The routine ZMSET from version 1.1 is no longer needed, and the complex operations are
!  automatically initialized in FMVALS.  It has been left in the package for compatibility with
!  version 1.1.


!  2.  REPRESENTATION OF FM NUMBERS

!  MBASE is the base in which the arithmetic is done.  MBASE must be bigger than one, and less than
!        or equal to the square root of the largest representable integer.  For best efficiency
!        MBASE should be large, but no more than about 1/4 of the square root of the largest
!        representable integer.  Input and output conversions are much faster when MBASE is a
!        power of ten.

!  NDIG  is the number of base MBASE digits that are carried in the multiple precision numbers.
!        NDIG must be at least two.  The upper limit for NDIG is restricted only by the amount
!        of memory available.

!  Sometimes it is useful to dynamically vary NDIG during the program.  Routine FMEQU should be used
!  to round numbers to lower precision or zero-pad them to higher precision when changing NDIG.

!  The default value of MBASE is a large power of ten.  FMSET also sets MBASE to a large power of
!  ten.  For an application where another base is used, such as simulating a given machine's base
!  two arithmetic, use subroutine FMSETVAR to change MBASE, so that the other internal values
!  depending on MBASE will be changed accordingly.

!  There are two representations for a floating point multiple precision number.  The unpacked
!  representation used by the routines while doing the computations is base MBASE and is stored
!  in NDIG+3 words.  A packed representation is available to store the numbers in compressed form.
!  In this format, the NDIG (base MBASE) digits of the mantissa are packed two per word to conserve
!  storage.  Thus the external, packed form of a number requires (NDIG+1)/2+3 words.

!  The unpacked format of a floating multiple precision number is as follows.  A number MA refers
!  to elements of an array with the multiple precision number stored as follows:
!       1  Sign of the number
!       2  Exponent of the number
!       3  First digit of the number
!     ...
!  NDIG+2  Last digit of the number.

!  The exponent is a power of MBASE and the implied radix point is immediately before the first
!  digit of the mantissa.  The exponent is a signed integer.  The overflow threshold is
!  MBASE**(MXEXP+1), and the underflow threshold is MBASE**(-MXEXP-1).  This means the valid
!  exponents for an FM number can range from -MXEXP to MXEXP+1 (inclusive).
!  Every nonzero number is normalized so that the first digit of the mantissa is nonzero.

!  For MBASE = 10,000 and NDIG = 4, if MA is the number -pi, it would have these representations:

!                   Word 1         2         3         4         5         6

!         Unpacked:     -1         1         3      1415      9265      3590
!         Packed:       -1         1     31415  92653590

!  The number represented is (-1)*(10000**1)*(.0003141592653590).

!  Because of the normalization of the digits with a large base, the equivalent number of base 10
!  significant digits for an FM number may be as small as LOG10(MBASE)*(NDIG-1) + 1.  In the -pi
!  example above, this is 4*3 + 1 = 13.

!  The integer routines use the FM format to represent numbers, without the number of digits (NDIG)
!  being fixed.  Integers in IM format are essentially variable precision, using the minimum number
!  of words to represent each value.

!  The unpacked format is the default.  As machines' memories have gotten bigger, few applications
!  need the packed format.  A program that uses packed format numbers should not use the FMZM module
!  or the multiple precision derived types defined in FMZM.

!  For programs using both FM and IM numbers, FM routines should not be called with IM numbers, and
!  IM routines should not be called with FM numbers, since the implied value of NDIG used for an IM
!  number may not match the explicit NDIG expected by an FM routine.  Use the conversion routines
!  IMFM2I and IMI2FM to change between the FM and IM formats.

!  The format for complex FM numbers (called ZM numbers below) is very similar to that for real FM
!  numbers.  Each ZM number consists of two FM numbers representing the real and imaginary parts of
!  a complex number.


!  3. INPUT/OUTPUT ROUTINES

!  All versions of the input routines perform free-format conversion from characters to FM numbers.

!  a. Conversion to or from a character array

!     FMINP converts from a character(1) array to an FM number.

!     FMOUT converts an FM number to base 10 and formats it for output as an array of type
!           character(1).  The output is left justified in the array, and the format is defined
!           by two variables in module FMVALS, so that a separate format definition does not have
!           to be provided for each output call.

!     JFORM1 and JFORM2 define a default output format.

!     JFORM1 = 0     E   format       ( .314159M+6 )
!            = 1     ES  format       ( 3.14159M+5 )
!            = 2     F   format       ( 314159.000 )

!     JFORM2 is the number of significant digits to display (if JFORM1 = 0 or 1).
!            If JFORM2 = 0 then a default number of digits is chosen.  The default is roughly
!            the full precision of the number.
!     JFORM2 is the number of digits after the decimal point (if JFORM1 = 2).
!            See the FMOUT documentation for more details.

!  b. Conversion to or from a character string

!     FMST2M converts from a character string to an FM number.

!     FMFORM converts an FM number to a character string according to a format provided in each
!            call.  The format description is more like that of a Fortran FORMAT statement, and
!            integer or fixed-point output is right justified.

!  c. Direct read or write

!     FMPRINT uses FMOUT to print one FM number.

!     FMFPRINT uses FMFORM to print one FM number.

!     FMWRITE writes FM numbers for later input using FMREAD.

!     FMREAD reads FM numbers written by FMWRITE.

!  The values given to JFORM1 and JFORM2 can be used to define a default output format when FMOUT
!  or FMPRINT are called.  The explicit format used in a call to FMFORM or FMFPRINT overrides the
!  settings of JFORM1 and JFORM2.

!  KW is the unit number to be used for standard output from the package, including error and
!  warning messages, and trace output.

!  For multiple precision integers, the corresponding routines IMINP, IMOUT, IMST2M, IMFORM,
!  IMPRINT, IMFPRINT, IMWRITE, and IMREAD provide similar input and output conversions.  For output
!  of IM numbers, JFORM1 and JFORM2 are ignored and integer format (JFORM1=2, JFORM2=0) is used.

!  For ZM numbers, the corresponding routines ZMINP, ZMOUT, ZMST2M, ZMFORM, ZMPRINT, ZMFPRINT,
!  ZMWRITE, and ZMREAD provide similar input and output conversions.

!  For the output format of ZM numbers, JFORM1 and JFORM2 determine the default format for the
!  individual parts of a complex number as with FM numbers.

!     JFORMZ determines the combined output format of the real and imaginary parts.

!     JFORMZ = 1  normal setting    :    1.23 - 4.56 i
!            = 2  use capital I     :    1.23 - 4.56 I
!            = 3  parenthesis format:  ( 1.23 , -4.56 )

!     JPRNTZ controls whether to print real and imaginary parts on one line whenever possible.

!     JPRNTZ = 1  print both parts as a single string :
!                     1.23456789M+321 - 9.87654321M-123 i
!            = 2  print on separate lines without the 'i' :
!                     1.23456789M+321
!                    -9.87654321M-123

!  For further description of these routines, see section 8 below.


!  4. ARITHMETIC TRACING

!  NTRACE and LVLTRC control trace printout from the package.

!  NTRACE =  0   No output except warnings and errors.  (Default)
!         =  1   The result of each call to one of the routines is printed in base 10, using FMOUT.
!         = -1   The result of each call to one of the routines is printed in internal base MBASE
!                     format.
!         =  2   The input arguments and result of each call to one of the routines is printed in
!                     base 10, using FMOUT.
!         = -2   The input arguments and result of each call to one of the routines is printed in
!                     base MBASE format.

!  LVLTRC defines the call level to which the trace is done.  LVLTRC = 1 means only FM routines
!         called directly by the user are traced, LVLTRC = 2 also prints traces for FM routines
!         called by other FM routines called directly by the user, etc.  Default is 1.

!  In the above description, internal MBASE format means the number is printed as it appears in the
!  array --- the sign, exponent, then the NDIG base MBASE digits.


!  5. ERROR CONDITIONS

!  KFLAG is a condition value returned by the package after each call to one of the routines.
!        Negative values indicate conditions for which a warning message will be printed unless
!        KWARN = 0.
!        Positive values indicate conditions that may be of interest but are not errors.  No warning
!        message is printed if KFLAG is nonnegative.

!  Subroutine FMFLAG is provided to give the user access to the current condition code.  For
!  example, to set the user's local variable LFLAG to FM's internal KFLAG value:
!        CALL FMFLAG(LFLAG)

!    KFLAG =  0     Normal operation.

!          =  1     One of the operands in FMADD or FMSUB was insignificant with respect to the
!                       other.  This means that in the default (symmetric) rounding mode the result
!                       is equal to the argument of larger magnitude.  KFLAG = 1 is still returned
!                       with the other three rounding modes (see KROUND below), but the result may
!                       not be equal to either input argument.
!          =  2     In converting an FM number to a one word integer in FMM2I, the FM number was
!                       not exactly an integer.  The next integer toward zero was returned.

!          = -1     NDIG was less than 2.
!          = -2     MBASE was less than 2 or more than MXBASE.
!          = -3     An exponent was out of range.
!          = -4     Invalid input argument(s) to an FM routine.  UNKNOWN was returned.
!          = -5     + or - OVERFLOW was generated as a result from an FM routine.
!          = -6     + or - UNDERFLOW was generated as a result from an FM routine.
!          = -7     The input string (array) to FMINP was not legal.
!          = -8     The character array was not large enough in an input or output routine.
!          = -9     Precision could not be raised enough to provide all requested guard digits.
!                        This means the program has run out of memory.
!                        UNKNOWN was returned.
!          = -10    An FM input argument was too small in magnitude to convert to the machine's
!                        single or double precision in FMM2SP or FMM2DP.  Check that the definitions
!                        of SPMAX and DPMAX in file FMSAVE.f95 are correct for the current machine.
!                        Zero was returned.
!          = -11    Array MBERN is not dimensioned large enough for the requested number of
!                        Bernoulli numbers.
!          = -12    Array MJSUMS is not dimensioned large enough for the number of coefficients
!                        needed in the reflection formula in FMPGAM.

!  When a negative KFLAG condition is encountered, the value of KWARN determines the action to
!  be taken.

!  KWARN = 0     Execution continues and no message is printed.
!        = 1     A warning message is printed and execution continues.
!        = 2     A warning message is printed and execution stops.

!  The default setting is KWARN = 1.

!  When an overflow or underflow is generated for an operation in which an input argument was
!  already an overflow or underflow, no additional message is printed.  When an unknown result
!  is generated and an input argument was already unknown, no additional message is printed.
!  In these cases the negative KFLAG value is still returned.

!  IM routines handle exceptions like OVERFLOW or UNKNOWN in the same way as FM routines, but there
!  are some differences because the number of digits carried for IM numbers is not fixed.  For
!  example, in computing the product of two large integers FM will try to allocate more space rather
!  than returning +OVERFLOW.  If this allocation fails, FM will write an error message indicating it
!  could not get more memory, and the program will stop.  The routines IMMPY_MOD and IMPOWER_MOD can
!  be used to obtain modular products and powers without as much chance of running out of memory.


!  6. OTHER OPTIONS

!  KRAD = 0     All angles in the real trigonometric functions and inverse functions are measured
!                   in degrees.
!       = 1     All angles are measured in radians.  (Default)

!  KROUND = -1  All results are rounded toward minus infinity.
!         =  0  All results are rounded toward zero (chopped).
!         =  1  All results are rounded to the nearest FM number, or to the value with an even last
!                   digit if the result is exactly halfway between two FM numbers.  (Default)
!         =  2  All results are rounded toward plus infinity.

!  KSWIDE defines the maximum screen width to be used for all unit KW output.  Default is 80.

!  KESWCH controls the action taken in FMINP and other input routines for strings like 'E7' that
!         have no digits before the exponent field.  This is sometimes a convenient abbreviation
!         when doing interactive keyboard input.
!         KESWCH = 1 causes 'E7' to translate like '1.0E+7'.  (Default)
!         KESWCH = 0 causes 'E7' to translate like '0.0E+7' and give 0.

!  CMCHAR defines the exponent letter to be used for FM variable output.
!         Default is 'M', as in 1.2345M+678.
!         Change it to 'E' for output to be read by a non-FM program.

!  KDEBUG = 0   No error checking is done to see if input arguments are valid and parameters like
!                  NDIG and MBASE are correct upon entry to each routine.  (Default)
!         = 1   Some error checking is done.  (Slower speed)

!  See module FMVALS in file FMSAVE.f95 for additional description of these and other variables
!  defining various FM conditions.


!  7. PORTABILITY

!  In FMSET several variables are set to machine-dependent values, and many of the variables
!  initialized in module FMVALS in file FMSAVE.f95 are checked to see that they have reasonable
!  values.  FMSET will print warning messages on unit KW for any of the FMVALS variables that
!  seem to be poorly initialized.

!  If an FM run fails, call FMVARS to get a list of all the FMVALS variables printed on unit KW.
!  Setting KDEBUG = 1 at the start may also identify some errors.

!  In the routines for special functions, several constants are used that require the machine's
!  integer word size to be at least 32 bits.


!  8.  LIST OF ROUTINES

!  First are the routines that deal with multiple precision real numbers.  All of these are
!  subroutines except logical function FMCOMPARE.

!  MA, MB, MC refer to FM format numbers (i.e., low-level type(multi) as opposed to the type(fm),
!  (im), or (zm) that are defined in file FMZM90.f95)

!  In Fortran-90 and later versions of the Fortran standard, it is potentially unsafe to use the
!  same variable both as input and output arguments in the calling sequence.
!  The operation MA = MA + MB should not be written as
!        CALL FMADD(MA,MB,MA)
!  since the code for the subroutine will not know that the first and third arguments are the same,
!  and some code optimizations under the assumption that all three arguments are different could
!  cause errors.

!  One solution is to use a third array and then put the result back in MA:
!        CALL FMADD(MA,MB,MC)
!        CALL FMEQ(MC,MA)

!  When the first call is doing one of the "fast" operations like addition, the extra call to move
!  the result back to MA can cause a noticeable loss in efficiency.  To avoid this, separate
!  routines are provided for the basic arithmetic operations when the result is to be returned in
!  the same array as one of the inputs.

!  A routine name with a suffix of  "_R1" returns the result in the first input array, and a suffix
!  of "_R2" returns the result in the second input array.  The example above would then be:
!        CALL FMADD_R1(MA,MB)

!  These routines each have one less argument than the original version, since the output is
!  re-directed to one of the inputs.  The result array should not be the same as any input array
!  when the original version of the routine is used.

!  The routines that can be used this way are listed below.  For others, like
!        CALL FMEXP(MA,MA)
!  the relative cost of doing an extra copy is small.  This one should become
!        CALL FMEXP(MA,MB)
!        CALL FMEQ(MB,MA)

!  When the derived-type interface from FMZM is used, as in
!        TYPE (FM), SAVE :: A, B
!        ...
!        A = A + B
!  there is no problem putting the result back into A, since the interface routine creates a
!  temporary scratch array for the result of A + B.

!  For each of these routines there is also a version available for which the argument list is
!  the same but all FM numbers are in packed format.  The routines using packed numbers have the
!  same names except 'FM' is replaced by 'FP' at the start of each name.

!  Some of the routine names were restricted to 6 characters in earlier versions of FM.  The old
!  names have been retained for compatibility, but new names that are longer and more readable
!  have been added.  For example, the old routine FMCSSN can now also be called as FMCOS_SIN.
!  Both old and new names are listed below.


!  FMABS(MA,MB)              MB = ABS(MA)

!  FMACOS(MA,MB)             MB = ACOS(MA)

!  FMACOSH(MA,MB)            MB = ACOSH(MA)

!  FMADD(MA,MB,MC)           MC = MA + MB

!  FMADD_R1(MA,MB)           MA = MA + MB

!  FMADD_R2(MA,MB)           MB = MA + MB

!  FMADDI(MA,IVAL)           MA = MA + IVAL   Increment an FM number by a one word integer.
!                                             Note this call does not have an "MB" result
!                                             like FMDIVI and FMMPYI.

!  FMASIN(MA,MB)             MB = ASIN(MA)

!  FMASINH(MA,MB)            MB = ASINH(MA)

!  FMATAN(MA,MB)             MB = ATAN(MA)

!  FMATANH(MA,MB)            MB = ATANH(MA)

!  FMATAN2(MA,MB,MC)         MC = ATAN2(MA,MB)     < old name: FMATN2 >

!  FMBIG(MA)                 MA = Biggest FM number less than overflow.

!  FMCHANGEBASE(MA,MB,NEW_MBASE,NEW_NDIG)
!                            MB is returned with the base NEW_MBASE and precision NEW_NDIG
!                               representation of MA, where MA is given in the current base (MBASE)
!                               and precision (NDIG).  This routine is primarily meant to be used
!                               for input and output conversion when a base is being used that is
!                               not a power of ten.

!  FMCOMPARE(MA,LREL,MB)     Logical comparison of MA and MB.     < old name: FMCOMP >
!                            LREL is a character(2) value identifying which of the six comparisons
!                                 is to be made.
!                            Example:  IF (FMCOMPARE(MA,'>=',MB)) ...
!                            Also can be:  IF (FMCOMPARE(MA,'GE',MB)) ...
!                            character(1) is ok:  IF (FMCOMPARE(MA,'>',MB)) ...

!  FMCONS                    Set several saved constants that depend on MBASE, the base being used.
!                            FMCONS should be called immediately after changing MBASE.

!  FMCOS(MA,MB)              MB = COS(MA)

!  FMCOS_SIN(MA,MB,MC)       MB = COS(MA),  MC = SIN(MA).     < old name: FMCSSN >
!                                 Faster than making two separate calls.

!  FMCOSH(MA,MB)             MB = COSH(MA)

!  FMCOSH_SINH(MA,MB,MC)     MB = COSH(MA),  MC = SINH(MA).     < old name: FMCHSH >
!                                 Faster than making two separate calls.

!  FMDIG(NSTACK,KST)         Find a set of precisions to use during Newton iteration for finding a
!                            simple root starting with about double precision accuracy.

!  FMDIM(MA,MB,MC)           MC = DIM(MA,MB)

!  FMDIV(MA,MB,MC)           MC = MA / MB

!  FMDIV_R1(MA,MB)           MA = MA / MB

!  FMDIV_R2(MA,MB)           MB = MA / MB

!  FMDIVI(MA,IVAL,MB)        MB = MA/IVAL   IVAL is a one word integer.

!  FMDIVI_R1(MA,IVAL)        MA = MA/IVAL

!  FMDP2M(X,MA)              MA = X    Convert from double precision to FM.

!  FMDPM(X,MA)               MA = X    Convert from double precision to FM.
!                                      Faster than FMDP2M, but MA agrees with X only to D.P.
!                                      accuracy.  See the comments in the two routines.

!  FMEQ(MA,MB)               MB = MA   Both have precision NDIG.
!                                      This is the version to use for standard  B = A  statements.

!  FMEQU(MA,MB,NA,NB)        MB = MA   Version for changing precision.
!                                      MA has NA digits (i.e., MA was computed using NDIG = NA), and
!                                      MB will be defined having NB digits.
!                                      MB is rounded if NB < NA
!                                      MB is zero-padded if NB > NA

!  FMEXP(MA,MB)              MB = EXP(MA)

!  FMFLAG(K)                 K = KFLAG  get the value of the FM condition flag -- stored in the
!                                       internal FM variable KFLAG in module FMVALS.

!  FMFORM(FORM,MA,STRING)    MA is converted to a character string using format FORM and returned in
!                               STRING.  FORM can represent I, F, E, or ES formats.  Example:
!                               CALL FMFORM('F60.40',MA,STRING)

!  FMFPRINT(FORM,MA)         Print MA on unit KW using FORM format.     < old name: FMFPRT >

!  FMHYPOT(MA,MB,MC)         MA = SQRT(MA**2 + MB**2)

!  FMI2M(IVAL,MA)            MA = IVAL   Convert from one word integer to FM.

!  FMINP(LINE,MA,LA,LB)      MA = LINE   Input conversion.
!                                        Convert LINE(LA) through LINE(LB) from characters to FM.

!  FMINT(MA,MB)              MB = INT(MA)    Integer part of MA.

!  FMIPOWER(MA,IVAL,MB)      MB = MA**IVAL   Raise an FM number to a one word integer power.
!                                            < old name: FMIPWR >

!  FMLOG10(MA,MB)            MB = LOG10(MA)     < old name: FMLG10 >

!  FMLN(MA,MB)               MB = LOG(MA)

!  FMLNI(IVAL,MA)            MA = LOG(IVAL)   Natural log of a one word integer.

!  FMM2DP(MA,X)              X  = MA     Convert from FM to double precision.

!  FMM2I(MA,IVAL)            IVAL = MA   Convert from FM to integer.

!  FMM2SP(MA,X)              X  = MA     Convert from FM to single precision.

!  FMMAX(MA,MB,MC)           MC = MAX(MA,MB)

!  FMMIN(MA,MB,MC)           MC = MIN(MA,MB)

!  FMMOD(MA,MB,MC)           MC = MA mod MB

!  FMMPY(MA,MB,MC)           MC = MA * MB

!  FMMPY_R1(MA,MB)           MA = MA * MB

!  FMMPY_R2(MA,MB)           MB = MA * MB

!  FMMPYI(MA,IVAL,MB)        MB = MA*IVAL    Multiply by a one word integer.

!  FMMPYI_R1(MA,IVAL)        MA = MA*IVAL

!  FMNINT(MA,MB)             MB = NINT(MA)   Nearest FM integer.

!  FMNORM2(MA,N,MB)          MB = SQRT( MA(1)**2 + MA(2)**2 + ... + MA(N)**2 )

!  FMOUT(MA,LINE,LB)         LINE = MA   Convert from FM to character.
!                                        LINE is a character array of length LB.

!  FMPI(MA)                  MA = pi

!  FMPRINT(MA)               Print MA on unit KW using current format.     < old name: FMPRNT >

!  FMPOWER(MA,MB,MC)         MC = MA**MB     < old name: FMPWR >

!  FM_RANDOM_NUMBER(X)       X is returned as a double precision random number, uniformly
!                            distributed on the open interval (0,1).  It is a high-quality,
!                            long-period generator based on 49-digit prime numbers.
!                            A default initial seed is used if FM_RANDOM_NUMBER is called without
!                            calling FM_RANDOM_SEED_PUT first.

!  FM_RANDOM_SEED_GET(SEED)  returns the seven integers SEED(1) through SEED(7) as the current seed
!                            for the FM_RANDOM_NUMBER generator.

!  FM_RANDOM_SEED_PUT(SEED)  initializes the FM_RANDOM_NUMBER generator using the seven integers
!                            SEED(1) through SEED(7). These get and put functions are slower than
!                            FM_RANDOM_NUMBER, so FM_RANDOM_NUMBER should be called many times
!                            between FM_RANDOM_SEED_PUT calls.  Also, some generators that used a
!                            9-digit modulus have failed randomness tests when used with only a few
!                            numbers being generated between calls to re-start with a new seed.

!  FM_RANDOM_SEED_SIZE(SIZE) returns integer SIZE as the size of the SEED array used by the
!                            FM_RANDOM_NUMBER generator.  Currently, SIZE = 7.

!  FMRATIONAL_POWER(MA,K,J,MB)
!                            MB = MA**(K/J)  Rational power.     < old name: FMRPWR >
!                            Faster than FMPOWER for functions like the cube root.

!  FMREAD(KREAD,MA)          MA   is returned after reading one (possibly multi-line) FM number
!                                 on unit KREAD.  This routine reads numbers written by FMWRITE.

!  FMSET(NPREC)              Set the internal FM variables so that the precision is at least NPREC
!                            base 10 digits plus three base 10 guard digits.

!  FMSETVAR(STRING)          Define a new value for one of the internal FM variables in module
!                            FMVALS that controls one of the FM options.  STRING has the form
!                                  variable = value.
!                            Example:  To change the screen width for FM output:
!                                  CALL FMSETVAR(' KSWIDE = 120 ')
!                            The variables that can be changed and the options they control are
!                            listed in sections 2 through 6 above.  Only one variable can be set
!                            per call.  The variable name in STRING must have no embedded blanks.
!                            The value part of STRING can be in any numerical format, except in
!                            the case of variable CMCHAR, which is character type.  To set CMCHAR
!                            to 'E', don't use any quotes in STRING:
!                                  CALL FMSETVAR(' CMCHAR = E ')

!  FMSIGN(MA,MB,MC)          MC = SIGN(MA,MB)   Returns the absolute value of MA times the sign
!                                               of MB.

!  FMSIN(MA,MB)              MB = SIN(MA)

!  FMSINH(MA,MB)             MB = SINH(MA)

!  FMSP2M(X,MA)              MA = X   Convert from single precision to FM.

!  FMSQR(MA,MB)              MB = MA * MA   Faster than FMMPY.

!  FMSQR_R1(MA)              MA = MA * MA

!  FMSQRT(MA,MB)             MB = SQRT(MA)

!  FMSQRT_R1(MA)             MA = SQRT(MA)

!  FMST2M(STRING,MA)         MA = STRING
!                                 Convert from character string to FM.  STRING may be in any
!                                 numerical format.  FMST2M is often more convenient than FMINP,
!                                 which converts an array of character(1) values.  Example:
!                                       CALL FMST2M('123.4',MA)

!  FMSUB(MA,MB,MC)           MC = MA - MB

!  FMSUB_R1(MA,MB)           MA = MA - MB

!  FMSUB_R2(MA,MB)           MB = MA - MB

!  FMTAN(MA,MB)              MB = TAN(MA)

!  FMTANH(MA,MB)             MB = TANH(MA)

!  FMTINY(MA)                MA = Smallest positive FM number greater than underflow.

!  FMULP(MA,MB)              MB = One Unit in the Last Place of MA.  For positive MA this is the
!                                 same as the Fortran function SPACING, but MB < 0 if MA < 0.
!                                 Examples:  If MBASE = 10 and NDIG = 30, then ulp(1.0) = 1.0E-29,
!                                            ulp(-4.5E+67) = -1.0E+38.

!  FMVARS                    Write the current values of the internal FM variables on unit KW.

!  FMWRITE(KWRITE,MA)        Write MA on unit KWRITE.     < old name: FMWRIT >
!                            Multi-line numbers will have '&' as the last nonblank character on all
!                            but the last line.  These numbers can then be read easily using FMREAD.


!  These are the available mathematical special functions.

!  FMBERNOULLI(N,MA)         MA = B(N)      Nth Bernoulli number

!  FMBESJ(N,MA,MB)           MB = J(N,MA)   Bessel function of the first kind

!  FMBESJ2(N1,N2,MA,MB)      MB = (/  J(N1,MA) , ..., J(N2,MA)  /)  returns an array

!  FMBESY(N,MA,MB)           MB = Y(N,MA)   Bessel function of the second kind

!  FMBESY2(N1,N2,MA,MB)      MB = (/  Y(N1,MA) , ..., Y(N2,MA)  /)  returns an array

!  FMBETA(MA,MB,MC)          MC = Beta(MA,MB)

!  FMC(MA,MB)                MB = C(MA)     Fresnel Cosine Integral

!  FMCHI(MA,MB)              MB = Chi(MA)   Hyperbolic Cosine Integral

!  FMCI(MA,MB)               MB = Ci(MA)    Cosine Integral

!  FMCOMB(MA,MB,MC)          MC = Combination MA choose MB  (Binomial coefficient)

!  FMEI(MA,MB)               MB = Ei(MA)    Exponential Integral

!  FMEN(N,MA,MB)             MB = E(N,MA)   Exponential Integral E_n

!  FMERF(MA,MB)              MB = Erf(MA)   Error function

!  FMERFC(MA,MB)             MB = Erfc(MA)  Complimentary Error function

!  FMERFCS(MA,MB)            MB = Erfc_Scaled(MA)  Scaled Complimentary Error function.

!  FMEULER(MA)               MA = Euler's constant ( 0.5772156649... )     < old name: FMEULR >

!  FMFACT(MA,MB)             MB = MA Factorial  (Gamma(MA+1))

!  FMGAM(MA,MB)              MB = Gamma(MA)

!  FMIBTA(MX,MA,MB,MC)       MC = Incomplete Beta(MX,MA,MB)

!  FMIGM1(MA,MB,MC)          MC = Incomplete Gamma(MA,MB).  Lower case Gamma(a,x)

!  FMIGM2(MA,MB,MC)          MC = Incomplete Gamma(MA,MB).  Upper case Gamma(a,x)

!  FMLERC(MA,MB)             MB = Ln(Erfc(MA))  Log Erfc

!  FMLI(MA,MB)               MB = Li(MA)    Logarithmic Integral

!  FMLNGM(MA,MB)             MB = Ln(Gamma(MA))

!  FMPGAM(N,MA,MB)           MB = Polygamma(N,MA)  (Nth derivative of Psi)

!  FMPOCH(MA,N,MB)           MB = MA*(MA+1)*(MA+2)*...*(MA+N-1)  (Pochhammer)

!  FMPSI(MA,MB)              MB = Psi(MA)   (Derivative of Ln(Gamma(MA))

!  FMS(MA,MB)                MB = S(MA)     Fresnel Sine Integral

!  FMSHI(MA,MB)              MB = Shi(MA)   Hyperbolic Sine Integral

!  FMSI(MA,MB)               MB = Si(MA)    Sine Integral


!  These are the routines that deal with multiple precision integer numbers.
!  All are subroutines except logical function IMCOMPARE.  MA, MB, MC refer to IM format numbers.
!  In each case the version of the routine to handle packed IM numbers has the same name, with
!  'IM' replaced by 'IP'.

!  IMABS(MA,MB)              MB = ABS(MA)

!  IMADD(MA,MB,MC)           MC = MA + MB

!  IMBIG(MA)                 MA = 10**(10**6).
!                                 Larger IM numbers can be obtained, but setting MA to the largest
!                                 possible value would leave no room for any other numbers.

!  IMCOMPARE(MA,LREL,MB)     Logical comparison of MA and MB.     < old name: IMCOMP >
!                            LREL is a character(2) value identifying which of the six comparisons
!                                 is to be made.
!                            Example:  IF (IMCOMPARE(MA,'GE',MB)) ...
!                            Also can be:  IF (IMCOMPARE(MA,'>=',MB))
!                            character(1) is ok:  IF (IMCOMPARE(MA,'>',MB)) ...

!  IMDIM(MA,MB,MC)           MC = DIM(MA,MB)

!  IMDIV(MA,MB,MC)           MC = int(MA/MB)
!                                 Use IMDIVR if the remainder is also needed.

!  IMDIVI(MA,IVAL,MB)        MB = int(MA/IVAL)
!                                 IVAL is a one word integer.  Use IMDVIR to get the remainder also.

!  IMDIVR(MA,MB,MC,MD)       MC = int(MA/MB),   MD = MA mod MB
!                                 When both the quotient and remainder are needed, this routine is
!                                 twice as fast as calling both IMDIV and IMMOD.

!  IMDVIR(MA,IVAL,MB,IREM)   MB = int(MA/IVAL),   IREM = MA mod IVAL
!                            IVAL and IREM are one word integers.

!  IMEQ(MA,MB)               MB = MA

!  IMFM2I(MAFM,MB)           MB = MAFM  Convert from real (FM) format to integer (IM) format.

!  IMFORM(FORM,MA,STRING)    MA is converted to a character string using format FORM and
!                               returned in STRING.  FORM can represent I, F, E, or ES formats.
!                               Example: CALL IMFORM('I70',MA,STRING)

!  IMFPRINT(FORM,MA)         Print MA on unit KW using FORM format.     < old name: IMFPRT >

!  IMGCD(MA,MB,MC)           MC = greatest common divisor of MA and MB.

!  IMI2FM(MA,MBFM)           MBFM = MA  Convert from integer (IM) format to real (FM) format.

!  IMI2M(IVAL,MA)            MA = IVAL   Convert from one word integer to IM.

!  IMINP(LINE,MA,LA,LB)      MA = LINE   Input conversion.
!                                        Convert LINE(LA) through LINE(LB) from characters to IM.

!  IMM2DP(MA,X)              X  = MA     Convert from IM to double precision.

!  IMM2I(MA,IVAL)            IVAL = MA   Convert from IM to one word integer.

!  IMM2SP(MA,X)              X  = MA     Convert from IM to single precision.

!  IMMAX(MA,MB,MC)           MC = MAX(MA,MB)

!  IMMIN(MA,MB,MC)           MC = MIN(MA,MB)

!  IMMOD(MA,MB,MC)           MC = MA mod MB

!  IMMPY(MA,MB,MC)           MC = MA*MB

!  IMMPYI(MA,IVAL,MB)        MB = MA*IVAL    Multiply by a one word integer.

!  IMMPY_MOD(MA,MB,MC,MD)    MD = MA*MB mod MC     < old name: IMMPYM >
!                                 Slightly faster than calling IMMPY and IMMOD separately.

!  IMOUT(MA,LINE,LB)         LINE = MA   Convert from IM to character.
!                                        LINE is a character array of length LB.

!  IMPOWER(MA,MB,MC)         MC = MA**MB     < old name: IMPWR >

!  IMPOWER_MOD(MA,MB,MC,MD)  MD = MA**MB mod MC     < old name: IMPMOD >

!  IMPRINT(MA)               Print MA on unit KW.     < old name: IMPRNT >

!  IMREAD(KREAD,MA)          MA   is returned after reading one (possibly multi-line)
!                                 IM number on unit KREAD.
!                                 This routine reads numbers written by IMWRITE.

!  IMSIGN(MA,MB,MC)          MC = SIGN(MA,MB)   Returns the absolute value of MA times the
!                                               sign of MB.

!  IMSQR(MA,MB)              MB = MA*MA   Faster than IMMPY.

!  IMST2M(STRING,MA)         MA = STRING
!                                 Convert from character string to IM.
!                                 IMST2M is often more convenient than IMINP, which converts
!                                 an array of character(1) values.  Example:
!                                      CALL IMST2M('12345678901',MA)

!  IMSUB(MA,MB,MC)           MC = MA - MB

!  IMWRITE(KWRITE,MA)        Write MA on unit KWRITE.
!                            Multi-line numbers will have '&' as the last nonblank character on all
!                            but the last line.  These numbers can then be read easily using IMREAD.


!  These are the routines that deal with multiple precision complex numbers.
!  All are subroutines, and in each case the version of the routine to handle packed ZM numbers has
!  the same name, with 'ZM' replaced by 'ZP'.

!  MA, MB, MC refer to ZM format complex numbers.
!  MAFM, MBFM, MCFM refer to FM format real numbers.
!  INTEG is a Fortran INTEGER variable.
!  ZVAL is a Fortran COMPLEX variable.

!  ZMABS(MA,MBFM)            MBFM = ABS(MA)    Result is real.

!  ZMACOS(MA,MB)             MB = ACOS(MA)

!  ZMACOSH(MA,MB)            MB = ACOSH(MA)

!  ZMADD(MA,MB,MC)           MC = MA + MB

!  ZMADDI(MA,INTEG)          MA = MA + INTEG  Increment an ZM number by a one word integer.
!                                             Note this call does not have an "MB" result
!                                             like ZMDIVI and ZMMPYI.

!  ZMARG(MA,MBFM)            MBFM = Argument(MA)    Result is real.

!  ZMASIN(MA,MB)             MB = ASIN(MA)

!  ZMASINH(MA,MB)            MB = ASINH(MA)

!  ZMATAN(MA,MB)             MB = ATAN(MA)

!  ZMATANH(MA,MB)            MB = ATANH(MA)

!  ZMCOMPLEX(MAFM,MBFM,MC)   MC = CMPLX(MAFM,MBFM)     < old name: ZMCMPX >

!  ZMCONJUGATE(MA,MB)        MB = CONJG(MA)     < old name: ZMCONJ >

!  ZMCOS(MA,MB)              MB = COS(MA)

!  ZMCOS_SIN(MA,MB,MC)       MB = COS(MA),  MC = SIN(MA).     < old name: ZMCSSN >
!                                 Faster than 2 calls.

!  ZMCOSH(MA,MB)             MB = COSH(MA)

!  ZMCOSH_SINH(MA,MB,MC)     MB = COSH(MA),  MC = SINH(MA).     < old name: ZMCHSH >
!                                 Faster than 2 calls.

!  ZMDIV(MA,MB,MC)           MC = MA / MB

!  ZMDIVI(MA,INTEG,MB)       MB = MA / INTEG

!  ZMEQ(MA,MB)               MB = MA

!  ZMEQU(MA,MB,NDA,NDB)      MB = MA    Version for changing precision.
!                                       (NDA and NDB are as in FMEQU)

!  ZMERF(MA,MB)              MB = ERF(MA)    Error function

!  ZMERFC(MA,MB)             MB = ERFC(MA)   Complimentary error function

!  ZMERFC_SC(MA,MB)          MB = ERFC_SCALED(MA)   Scaled complimentary error function

!  ZMEXP(MA,MB)              MB = EXP(MA)

!  ZMFACT(MA,MB)             MB = MA!              Factorial function

!  ZMFORM(FORM1,FORM2,MA,STRING)
!                            STRING = MA
!                            MA is converted to a character string using format FORM1 for the real
!                            part and FORM2 for the imaginary part.  The result is returned in
!                            STRING.  FORM1 and FORM2 can represent I, F, E, or ES formats.
!                            Example:
!                                  CALL ZMFORM('F20.10','F15.10',MA,STRING)

!  ZMFPRINT(FORM1,FORM2,MA)  Print MA on unit KW using formats FORM1 and FORM2.
!                            < old name: ZMFPRT >

!  ZMGAM(MA,MB)              MB = Gamma(MA)        Gamma function

!  ZMI2M(INTEG,MA)           MA = CMPLX(INTEG,0)

!  ZM2I2M(INTEG1,INTEG2,MA)  MA = CMPLX(INTEG1,INTEG2)

!  ZMIMAG(MA,MBFM)           MBFM = IMAG(MA)    Imaginary part.

!  ZMINP(LINE,MA,LA,LB)      MA = LINE   Input conversion.
!                                 Convert LINE(LA) through LINE(LB) from characters to ZM.
!                                 LINE is a character array of length at least LB.

!  ZMINT(MA,MB)              MB = INT(MA)       Integer part of both Real and Imaginary parts of MA.

!  ZMIPOWER(MA,INTEG,MB)     MB = MA ** INTEG   Integer power function.     < old name: ZMIPWR >

!  ZMLOG10(MA,MB)            MB = LOG10(MA)     < old name: ZMLG10 >

!  ZMLN(MA,MB)               MB = LOG(MA)

!  ZMLNGM(MA,MB)             MB = Log_Gamma(MA)

!  ZMM2I(MA,INTEG)           INTEG = INT(REAL(MA))

!  ZMM2Z(MA,ZVAL)            ZVAL = MA

!  ZMMPY(MA,MB,MC)           MC = MA * MB

!  ZMMPYI(MA,INTEG,MB)       MB = MA * INTEG

!  ZMNINT(MA,MB)             MB = NINT(MA)   Nearest integer of both Real and Imaginary.

!  ZMOUT(MA,LINE,LB,LAST1,LAST2)
!                            LINE = MA
!                            Convert from FM to character.
!                            LINE  is the returned character(1) array.
!                            LB    is the dimensioned size of LINE.
!                            LAST1 is returned as the position in LINE of the last character
!                                  of REAL(MA)
!                            LAST2 is returned as the position in LINE of the last character
!                                  of AIMAG(MA)

!  ZMPOWER(MA,MB,MC)         MC = MA ** MB     < old name: ZMPWR >

!  ZMPRINT(MA)               Print MA on unit KW using current format.     < old name: ZMPRNT >

!  ZMRATIONAL_POWER(MA,IVAL,JVAL,MB)
!                            MB = MA ** (IVAL/JVAL)     < old name: ZMRPWR >

!  ZMREAD(KREAD,MA)          MA   is returned after reading one (possibly multi-line) ZM number on
!                                 unit KREAD.  This routine reads numbers written by ZMWRITE.

!  ZMREAL(MA,MBFM)           MBFM = REAL(MA)    Real part.

!  ZMSET(NPREC)              Set precision to the equivalent of a few more than NPREC base 10
!                            digits.  This is now the same as FMSET, but is retained for
!                            compatibility with earlier versions of the package.

!  ZMSIN(MA,MB)              MB = SIN(MA)

!  ZMSINH(MA,MB)             MB = SINH(MA)

!  ZMSQR(MA,MB)              MB = MA*MA    Faster than ZMMPY.

!  ZMSQRT(MA,MB)             MB = SQRT(MA)

!  ZMST2M(STRING,MA)         MA = STRING
!                                 Convert from character string to ZM.  ZMST2M is often more
!                                 convenient than ZMINP, which converts an array of character(1)
!                                 values.  Example:
!                                       CALL ZMST2M('123.4+5.67i',MA).

!  ZMSUB(MA,MB,MC)           MC = MA - MB

!  ZMTAN(MA,MB)              MB = TAN(MA)

!  ZMTANH(MA,MB)             MB = TANH(MA)

!  ZMWRITE(KWRITE,MA)        Write MA on unit KWRITE.  Multi-line numbers are formatted for
!                            automatic reading with ZMREAD.     < old name: ZMWRIT >

!  ZMZ2M(ZVAL,MA)            MA = ZVAL



!  9. NEW FOR VERSION 1.3

!  The first edition of version 1.3 appeared in ACM Transactions on Mathematical Software (2-2011).
!  Since then several additions have been made.
!  (a) New Fortran-08 functions are available in FMZM
!      ACOSH(X), ASINH(X), ATANH(X) for real and complex X
!      ATAN(X,Y) can be used in place of ATAN2(X,Y)
!      BESSEL_J0(X), BESSEL_J1(X), BESSEL_JN(N,X), BESSEL_JN(N1,N2,X)
!      BESSEL_Y0(X), BESSEL_Y1(X), BESSEL_YN(N,X), BESSEL_YN(N1,N2,X)
!         The older FM names, BESSEL_J(N,X) and BESSEL_Y(N,X) are still available.
!      ERFC_SCALED(X) for exp(x**2) * erfc(x)
!         The older FM function LOG_ERFC(X) is also still available for avoiding underflow in erfc.
!      HYPOT(X,Y) for sqrt(x**2 + y**2)
!      NORM2(A) for sqrt( a(1)**2 + a(2)**2 + ... + a(n)**2 )
!         This could previously have been done with array operations as SQRT(DOT_PRODUCT(A,A)).
!  (b) Many of the elementary and special functions are now faster, after some code-tuning was
!      done and a few new methods were added.

!  The routines for the exponential integral function and related mathematical special functions
!  are new in version 1.3.  These routines are:
!  FMBESJ, FMBESY, FMC, FMCHI, FMCI, FMEI, FMEN, FMERF, FMERFC, FMLERC, FMLI, FMS, FMSHI, FMSI.

!  Some of the routines were moved between files FM.f95 and FMZM90.f95 so that now all routines
!  using the module FMZM (in file FMZM90.f95) for multiple precision derived types and operator
!  overloading are located in FMZM90.f95.  This means that programs not using derived types can
!  skip compiling and/or linking FMZM90.f95.

!  The array function DOTPRODUCT in FMZM has been re-named DOT_PRODUCT to agree with the Fortran
!  standard.  For type ZM complex arguments its definition has been changed to agree with the
!  Fortran intrinsic function.  When X and Y are complex, DOT_PRODUCT(X,Y) is not just the sum of
!  the products of the corresponding array elements, as it is for types FM and IM.  For type ZM,
!  the formula is the sum of conjg(X(j)) * Y(j).  This definition is used so that the complex dot
!  product will be an inner product in the mathematical sense.

!  New routines have been added to module FMZM to provide array syntax for the three multiple
!  precision derived types.  This means statements like V = 1 and A = B + C now work when these
!  variables are vectors or matrices of multiple precision numbers.

!  One routine from FM 1.2 has been split into three routines in version 1.3.  The routine
!  FM_RANDOM_SEED from FM 1.2 has become three subroutines, so that the optional arguments and
!  the need for an explicit interface can be avoided.  See the three routines starting with
!  FM_RANDOM_SEED in the list above.  The same multiplicative congruential generator as before
!  is used, but the shuffling of those values has been removed, so that saving seeds and
!  re-starting the generator now works more like the standard Fortran random function.

!  Multiple precision variables were separate fixed-size arrays in previous versions.  Now they are
!  single integers that serve as index values to a single large array (MWK, defined in file
!  FMSAVE.f95) where the actual values are stored.  This often improves both efficiency and memory
!  utilization, since many compilers implemented the derived type operations using copy in and copy
!  out of the arguments for a given operation.  Copying entire arrays was slower, and there were
!  often memory leaks when the compiler automatically created temporary derived type objects while
!  evaluating derived type expressions.  The static arrays in previous versions also meant that
!  memory was wasted when only a few kinds of operations were used at high precision.  Now the
!  space needed by any unused operations never gets allocated.

!  Some new error checking is now done for the derived type multiple precision variables. Attempting
!  to use an undefined variable will cause an error message to be printed.

!  Much higher precision can be attained in version 1.3, since machines are faster and have more
!  memory.  To support higher precision, a routine for FFT-based multiplication has been included,
!  and when precision gets high enough, the algorithms for multiplication, division, squares, square
!  roots, etc., will switch to the FFT routine.

!  Binary splitting algorithms are used for the mathematical constants at high precision.  At the
!  time version 1.3 was released, computing a million digits of e, pi, or the logarithm of a small
!  integer took a few seconds, while a million digits of Euler's constant took a few minutes.

!  Perfect rounding is now done all the time.  In version 1.2 perfect rounding was an option, but
!  the default rounding could round the wrong direction once every few million operations, when the
!  exact result was very close to halfway between two adjacent representable numbers.

!  10. NEW FOR VERSION 1.4

!  The changes in version 1.4 were made to enable a thread-safe special version of FM to be created.
!  See file FM_parallel.f95 for the thread-safe version.

!  The memory model for multi-precision variables has been changed from having one global database
!  kept in module FMVALS to hold all the numbers to making the multi-precision variables local to
!  the routines using them.

!  The way in which the user declares and uses type(fm), etc., variables is the same in this
!  version as before.

!  Improvements from the user's point of view are:
!     a.  No longer needing to insert calls into the user's routines to FM_ENTER_FUNCTION, etc.
!     b.  No need to call FM_DEALLOCATE before deallocating a multi-precision variable.

! --------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------

    END SUBROUTINE

!  The FM routines perform floating point multiple-precision arithmetic.

      SUBROUTINE FMSET(NPREC)

!  Initialize the global FM variables that must be set before calling other FM routines.
!  These variables are initialized to fairly standard values in the FMSAVE.f95 file (MODULE ModLib_FMVALS),
!  so calling FMSET at the beginning of a program is now optional.  FMSET is a convenient way to set
!  or change the precision being used, and it also checks to see that the generic values chosen for
!  several machine-dependent variables are valid.

!  Base and precision will be set to give at least NPREC+3 decimal digits of precision (giving the
!  user at least three base ten guard digits).  When the base is large, each extra word contains
!  several extra digits when viewed in base ten.  This means that some choices of NPREC will give
!  a few more than three base ten guard digits.

!  MBASE (base for FM arithmetic) is set to a large power of ten.
!  JFORM1 and JFORM2 (default output format controls) are set to ES format displaying NPREC
!  significant digits.

!  Several FM options were set here in previous versions of the package, and are now initialized to
!  their default values in module FMVALS.
!  Here are the initial settings:

!  The trace option is set off.
!  The mode for angles in trig functions is set to radians.
!  The rounding mode is set to symmetric rounding.
!  Warning error message level is set to 1.
!  Cancellation error monitor is set off.
!  Screen width for output is set to 80 columns.
!  The exponent character for FM output is set to 'M'.
!  Debug error checking is set off.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NPREC

      REAL (KIND(1.0D0)) :: MAXINT_CHK,MXEXP2_CHK,MEXPOV_CHK,MEXPUN_CHK,MUNKNO_CHK
      DOUBLE PRECISION :: DPEPS_CHK,DPMAX_CHK,SPMAX_CHK,TEMP
      INTEGER :: INTMAX_CHK,K,NPSAVE
      INTENT (IN) :: NPREC

!             MAXINT should be set to a very large integer, possibly the largest representable
!                    integer for the current machine.  For most 32-bit machines, MAXINT is set
!                    to  2**53 - 1 = 9.007D+15  when double precision arithmetic is used for
!                    M-variables.  Using integer M-variables usually gives
!                    MAXINT = 2**31 - 1 = 2147483647.

!                    Setting MAXINT to a smaller number is ok, but this unnecessarily restricts
!                    the permissible range of MBASE and MXEXP.

      MAXINT_CHK = MAX_REPRESENTABLE_M_VAR
      IF (MAXINT > MAXINT_CHK) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MAXINT was set to ',MAXINT,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no more than ',MAXINT_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MAXINT has been changed to ',MAXINT_CHK
          WRITE (KW,*) ' '
          MAXINT = MAXINT_CHK
      ELSE IF (MAXINT < MAXINT_CHK/2) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MAXINT was set to ',MAXINT,' in file FMSAVE.f95'
          WRITE (KW,*) ' For better performance set it to ',MAXINT_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MAXINT has been changed to ',MAXINT_CHK
          WRITE (KW,*) ' '
          MAXINT = MAXINT_CHK
      ENDIF

!             INTMAX is a large value close to the overflow threshold for integer variables.
!                    It is usually 2**31 - 1 for machines with 32-bit integer arithmetic.

!                    The following code sets INTMAX_CHK to the largest representable integer.
!                    Then INTMAX is checked against this value.

      INTMAX_CHK = HUGE(I_TWO)
      IF (INTMAX > INTMAX_CHK) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' INTMAX was set to ',INTMAX,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no more than ',INTMAX_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, INTMAX has been changed to ',INTMAX_CHK
          WRITE (KW,*) ' '
          INTMAX = INTMAX_CHK
      ELSE IF (INTMAX < INTMAX_CHK/2) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' INTMAX was set to ',INTMAX,' in file FMSAVE.f95'
          WRITE (KW,*) ' For better performance set it to ',INTMAX_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, INTMAX has been changed to ',INTMAX_CHK
          WRITE (KW,*) ' '
          INTMAX = INTMAX_CHK
      ENDIF

!             DPMAX should be set to a value near the machine's double precision overflow threshold,
!                   so that DPMAX and 1.0D0/DPMAX are both representable in double precision.

      DPMAX_CHK = HUGE(DP_TWO)/5
      IF (DPMAX > DPMAX_CHK) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' DPMAX was set to ',DPMAX,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no more than ',DPMAX_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, DPMAX has been changed to ',DPMAX_CHK
          WRITE (KW,*) ' '
          DPMAX = DPMAX_CHK
      ELSE IF (DPMAX < DPMAX_CHK/1.0D2) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' DPMAX was set to ',DPMAX,' in file FMSAVE.f95'
          WRITE (KW,*) ' For better performance set it to ',DPMAX_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, DPMAX has been changed to ',DPMAX_CHK
          WRITE (KW,*) ' '
          DPMAX = DPMAX_CHK
      ENDIF

!             SPMAX should be set to a value near the machine's single precision overflow threshold,
!                   so that 1.01*SPMAX and 1.0/SPMAX are both representable in single precision.

      SPMAX_CHK = HUGE(R_TWO)/5
      IF (SPMAX > SPMAX_CHK) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' SPMAX was set to ',SPMAX,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no more than ',SPMAX_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, SPMAX has been changed to ',SPMAX_CHK
          WRITE (KW,*) ' '
          SPMAX = SPMAX_CHK
      ELSE IF (SPMAX < SPMAX_CHK/1.0D2) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' SPMAX was set to ',SPMAX,' in file FMSAVE.f95'
          WRITE (KW,*) ' For better performance set it to ',SPMAX_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, SPMAX has been changed to ',SPMAX_CHK
          WRITE (KW,*) ' '
          SPMAX = SPMAX_CHK
      ENDIF

!             MXBASE is the maximum value for MBASE.

      TEMP = MAXINT
      TEMP = INT(MIN(DBLE(INTMAX),SQRT(TEMP)))
      IF (MXBASE > TEMP) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MXBASE was set to ',MXBASE,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no more than ',TEMP
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MXBASE has been changed to ',TEMP
          WRITE (KW,*) ' '
          MXBASE = TEMP
      ELSE IF (MXBASE < TEMP/2) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MXBASE was set to ',MXBASE,' in file FMSAVE.f95'
          WRITE (KW,*) ' For better performance set it to ',TEMP
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MXBASE has been changed to ',TEMP
          WRITE (KW,*) ' '
          MXBASE = TEMP
      ENDIF

!             KROUND controls the rounding mode.  Set it to symmetric rounding.

      KROUND = 1

!             MBASE is the currently used base for arithmetic.

      K = INT(LOG10(DBLE(MXBASE)/4))
      MBASE = 10**K

!             NDIG is the number of digits currently being carried.

      NPSAVE = NPREC
      NDIG = 2 + (NPREC+2)/K
      IF (NDIG < 2) THEN
          NDIG = MAX(2,NDIG)
          WRITE (KW,                                                      &
                 "(//' Precision out of range when calling FMSET.',"  //  &
                 "'  NPREC =',I20/' The nearest valid NDIG will be'," //  &
                 "' used instead:   NDIG =',I20//)"                       &
                ) NPREC,NDIG
          NPSAVE = 0
      ENDIF

!             NCALL is the call stack pointer.

      NCALL = 0

!             MXEXP  is the current maximum exponent.
!             MXEXP2 is the internal maximum exponent.  This is used to define the overflow and
!                    underflow thresholds.

!             These values are chosen so that FM routines can raise the overflow/underflow limit
!             temporarily while computing intermediate results, and so that EXP(INTMAX) is greater
!             than MXBASE**(MXEXP2+1).

!             The overflow threshold is MBASE**(MXEXP+1), and the underflow threshold is
!             MBASE**(-MXEXP-1).  This means the valid exponents in the first word of an FM number
!             can range from -MXEXP to MXEXP+1 (inclusive).

      MXEXP = AINT( MAX_EXPONENT / 2.01D0 + 0.5D0 )
      MXEXP2_CHK = MAX_EXPONENT
      IF (MXEXP2 > MXEXP2_CHK*1.01) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MXEXP2 was set to ',MXEXP2,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no more than ',MXEXP2_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MXEXP2 has been changed to ',MXEXP2_CHK
          WRITE (KW,*) ' '
          MXEXP2 = MXEXP2_CHK
      ELSE IF (MXEXP2 < MXEXP2_CHK*0.99) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MXEXP2 was set to ',MXEXP2,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no less than ',MXEXP2_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MXEXP2 has been changed to ',MXEXP2_CHK
          WRITE (KW,*) ' '
          MXEXP2 = MXEXP2_CHK
      ENDIF

!             MEXPUN is the exponent used as a special symbol for underflowed results.

      MEXPUN_CHK = AINT( -MAX_EXPONENT * 1.01D0 )
      IF (MEXPUN < MEXPUN_CHK*1.01) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MEXPUN was set to ',MEXPUN,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no less than ',MEXPUN_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MEXPUN has been changed to ',MEXPUN_CHK
          WRITE (KW,*) ' '
          MEXPUN = MEXPUN_CHK
      ELSE IF (MEXPUN > MEXPUN_CHK) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MEXPUN was set to ',MEXPUN,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no more than ',MEXPUN_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MEXPUN has been changed to ',MEXPUN_CHK
          WRITE (KW,*) ' '
          MEXPUN = MEXPUN_CHK
      ENDIF

!             MEXPOV is the exponent used as a special symbol for overflowed results.

      MEXPOV_CHK = -MEXPUN
      IF (MEXPOV /= MEXPOV_CHK) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MEXPOV was set to ',MEXPOV,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be ',MEXPOV_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MEXPOV has been changed to ',MEXPOV_CHK
          WRITE (KW,*) ' '
          MEXPOV = MEXPOV_CHK
      ENDIF

!             MUNKNO is the exponent used as a special symbol for unknown FM results
!                    (1/0, SQRT(-3.0), ...).

      MUNKNO_CHK = AINT( MAX_EXPONENT * 1.0201D0 )
      IF (MUNKNO > MUNKNO_CHK*1.01) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MUNKNO was set to ',MUNKNO,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no more than ',MUNKNO_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MUNKNO has been changed to ',MUNKNO_CHK
          WRITE (KW,*) ' '
          MUNKNO = MUNKNO_CHK
      ELSE IF (MUNKNO < MUNKNO_CHK) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' MUNKNO was set to ',MUNKNO,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no less than ',MUNKNO_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, MUNKNO has been changed to ',MUNKNO_CHK
          WRITE (KW,*) ' '
          MUNKNO = MUNKNO_CHK
      ENDIF

!             RUNKNO is returned from FM to real or double conversion routines when no valid result
!                    can be expressed in real or double precision.  On systems that provide a value
!                    for undefined results (e.g., Not A Number) setting RUNKNO to that value is
!                    reasonable.  On other systems set it to a value that is likely to make any
!                    subsequent results obviously wrong that use it.  In either case a KFLAG = -4
!                    condition is also returned.

      RUNKNO = -1.01*SPMAX

!             IUNKNO is returned from FM to integer conversion routines when no valid result can be
!                    expressed as a one word integer.  KFLAG = -4 is also set.

      IUNKNO = -INT(MXEXP2)

!             DPEPS is the approximate machine precision.

      DPEPS_CHK = EPSILON(DP_TWO)
      IF (DPEPS > DPEPS_CHK*1.01) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' DPEPS was set to ',DPEPS,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no more than ',DPEPS_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, DPEPS has been changed to ',DPEPS_CHK
          WRITE (KW,*) ' '
          DPEPS = DPEPS_CHK
      ELSE IF (DPEPS < DPEPS_CHK*0.99) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' In routine FMSET it appears that FM internal variable'
          WRITE (KW,*) ' DPEPS was set to ',DPEPS,' in file FMSAVE.f95'
          WRITE (KW,*) ' For this machine it should be no less than ',DPEPS_CHK
          WRITE (KW,*) ' Change the initialization in FMSAVE.f95 to this value.'
          WRITE (KW,*) ' For this run, DPEPS has been changed to ',DPEPS_CHK
          WRITE (KW,*) ' '
          DPEPS = DPEPS_CHK
      ENDIF

!             JFORM1 indicates the format used by FMOUT.

      JFORM1 = 1

!             JFORM2 indicates the number of digits used in FMOUT.

      JFORM2 = NPSAVE

!             Set JFORMZ to ' 1.23 + 4.56 i ' format.

      JFORMZ = 1

!             Set JPRNTZ to print real and imaginary parts on one line whenever possible.

      JPRNTZ = 1

!             Initialize two hash tables that are used for character look-up during
!             input conversion.

      CALL FMHTBL

!             FMCONS sets several real and double precision constants.

      CALL FMCONS

      RETURN
      END SUBROUTINE FMSET

      SUBROUTINE FMABS(MA,MB)

!  MB = ABS(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      INTEGER :: KWRNSV

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMABS'
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)

      KFLAG = 0
      KWRNSV = KWARN
      KWARN = 0
      CALL FMEQ(MA,MB)
      MB%MP(1) = 1
      KWARN = KWRNSV
      IF (NTRACE /= 0) CALL FMNTR(1,MB,MB,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMABS

      SUBROUTINE FMACOS(MA,MB)

!  MB = ACOS(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAS,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(6)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG .AND. KRAD == 0) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMACOS'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMI2M(180,MXY(1))
          CALL FMPI(MXY(2))
          CALL FMDIV(MXY(1),MXY(2),MXY(3))
          IF (MA%MP(2) > MEXPUN) THEN
              CALL FMMPY(MXY(3),MA,MXY(2))
          ELSE
              CALL FMEQ(MA,MXY(2))
          ENDIF
          CALL FMI2M(90,MXY(1))
          CALL FMSUB(MXY(1),MXY(2),MB)
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (MB%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMACOS'
              KFLAG = -4
              CALL FMWARN
              NCALL = NCALL - 1
          ELSE IF (ABS(MB%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMACOS'
              IF (MB%MP(2) == MEXPOV) KFLAG = -5
              IF (MB%MP(2) == MEXPUN) KFLAG = -6
              CALL FMWARN
              NCALL = NCALL - 1
          ENDIF
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMACOS'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(2) > 0 .OR. MA%MP(3) == 0) THEN
          CALL FMENTR('FMACOS   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMACOS'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      MAS = MA%MP(1)
      CALL FMEQU(MA,MXY(6),NDSAVE,NDIG)
      IF (KROUND /= 1 .AND. KRAD /= 1) THEN
          CALL FMST2M('0.5',MXY(1))
          CALL FMSUB(MXY(6),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0) THEN
              CALL FMST2M('60',MXY(6))
              GO TO 120
          ENDIF
          CALL FMADD(MXY(6),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0) THEN
              CALL FMST2M('120',MXY(6))
              GO TO 120
          ENDIF
      ENDIF

!             Use ACOS(X) = ATAN(SQRT(1-X*X)/X)

      MXY(6)%MP(1) = 1
      CALL FMI2M(1,MXY(4))
      CALL FMSUB(MXY(4),MXY(6),MXY(2))
      CALL FMADD(MXY(4),MXY(6),MXY(3))
      CALL FMMPY_R2(MXY(2),MXY(3))
      CALL FMSQRT_R1(MXY(3))
      CALL FMDIV(MXY(3),MXY(6),MXY(5))
      CALL FMATAN(MXY(5),MXY(6))

      IF (MAS < 0) THEN
          IF (KRAD == 1) THEN
              CALL FMPI(MXY(4))
          ELSE
              CALL FMI2M(180,MXY(4))
          ENDIF
          CALL FMSUB_R2(MXY(4),MXY(6))
      ENDIF

!             Round the result and return.

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(6)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(6),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMACOS

      SUBROUTINE FMACOSH(MA,MB)

!  MB = ARCCOSH(MA)      Inverse hyperbolic cosine.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENTR('FMACOSH  ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

      IF (MXY(1)%MP(2) == MEXPOV .OR. MXY(1)%MP(2) <= 0 .OR.  &
          MXY(1)%MP(1) < 0) THEN
          KFLAG = -4
          CALL FMST2M('UNKNOWN',MXY(2))
      ELSE IF (4.0*(MXY(1)%MP(2)-1) > NDIG) THEN
          CALL FMMPYI(MXY(1),2,MXY(2))
          IF (MXY(2)%MP(2) == MEXPOV) THEN
              CALL FMLN(MXY(1),MXY(2))
              CALL FMLNI(2,MXY(3))
              CALL FMADD_R1(MXY(2),MXY(3))
          ELSE
              CALL FMI2M(1,MXY(3))
              CALL FMSQR(MXY(2),MXY(4))
              CALL FMDIV_R2(MXY(3),MXY(4))
              CALL FMLN(MXY(2),MXY(3))
              CALL FMSUB(MXY(3),MXY(4),MXY(2))
          ENDIF
      ELSE
          IEXTRA = MXY(1)%MP(2)
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
              NDIG = NDIG + IEXTRA
          ENDIF
          CALL FMI2M(1,MXY(2))
          CALL FMSUB(MXY(1),MXY(2),MXY(3))
          CALL FMADD(MXY(1),MXY(2),MXY(4))
          CALL FMMPY_R1(MXY(3),MXY(4))
          CALL FMSQRT_R1(MXY(3))
          CALL FMDIV_R1(MXY(3),MXY(1))
          CALL FMATANH(MXY(3),MXY(2))
      ENDIF
      NAMEST(NCALL) = 'FMACOSH'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

      CALL FMEXIT(MXY(2),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMACOSH

      SUBROUTINE FMADD(MA,MB,MC)

!  MC = MA + MB

!  This routine performs the trace printing for addition.  FMADD2 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMADD'
          CALL FMNTR(2,MA,MB,2,1)

          CALL FMADD2(MA,MB,MC)

          CALL FMNTR(1,MC,MC,1,1)
      ELSE
          CALL FMADD2(MA,MB,MC)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMADD

      SUBROUTINE FMADD2(MA,MB,MC)

!  Internal addition routine.  MC = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      REAL (KIND(1.0D0)) :: MAS,MBS
      INTEGER :: J,JCOMP,JRSSAV,JSIGN,KRESLT,N1,NGUARD,NMWA
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR. KDEBUG == 1) THEN
          IF (KSUB == 1) THEN
              CALL FMARGS('FMSUB    ',2,MA,MB,KRESLT)
          ELSE
              CALL FMARGS('FMADD    ',2,MA,MB,KRESLT)
          ENDIF
          IF (KRESLT /= 0) THEN
              IF ((KRESLT /= 1 .AND. KRESLT /= 2) .OR. MA%MP(3) == 0 .OR.  &
                  MB%MP(3) == 0) THEN
                  NCALL = NCALL + 1
                  IF (KSUB == 1) THEN
                      NAMEST(NCALL) = 'FMSUB'
                  ELSE
                      NAMEST(NCALL) = 'FMADD'
                  ENDIF
                  CALL FMRSLT(MA,MB,MC,KRESLT)
                  JRSIGN = JRSSAV
                  NCALL = NCALL - 1
                  RETURN
              ENDIF
          ENDIF
      ELSE
          IF (MA%MP(3) == 0) THEN
              CALL FMEQ(MB,MC)
              KFLAG = 1
              IF (KSUB == 1) THEN
                  IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0)  &
                      MC%MP(1) = -MC%MP(1)
                  KFLAG = 0
              ENDIF
              JRSIGN = JRSSAV
              RETURN
          ENDIF
          IF (MB%MP(3) == 0) THEN
              CALL FMEQ(MA,MC)
              KFLAG = 1
              JRSIGN = JRSSAV
              RETURN
          ENDIF
      ENDIF
      KFLAG = 0
      N1 = NDIG + 1

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD21
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (KROUND_RETRY >= 1) THEN
              NGUARD = NDIG
          ENDIF
      ENDIF
      NMWA = N1 + NGUARD

!             Save the signs of MA and MB and then work with positive numbers.
!             JSIGN is the sign of the result of MA + MB.

      JSIGN = 1
      MAS = MA%MP(1)
      MBS = MB%MP(1)
      IF (KSUB == 1) MBS = -MBS

!             See which one is larger in absolute value.

      JCOMP = 2
      IF (MA%MP(2) > MB%MP(2)) THEN
          JCOMP = 1
      ELSE IF (MB%MP(2) > MA%MP(2)) THEN
          JCOMP = 3
      ELSE
          DO J = 2, N1
             IF (MA%MP(J+1) > MB%MP(J+1)) THEN
                 JCOMP = 1
                 EXIT
             ENDIF
             IF (MB%MP(J+1) > MA%MP(J+1)) THEN
                 JCOMP = 3
                 EXIT
             ENDIF
          ENDDO
      ENDIF

      IF (JCOMP < 3) THEN
          IF (MAS < 0) JSIGN = -1
          JRSIGN = JSIGN
          IF (MAS*MBS > 0) THEN
              CALL FMADDP(MA,MB,NGUARD,NMWA)
          ELSE
              CALL FMADDN(MA,MB,NGUARD,NMWA)
          ENDIF
      ELSE
          IF (MBS < 0) JSIGN = -1
          JRSIGN = JSIGN
          IF (MAS*MBS > 0) THEN
              CALL FMADDP(MB,MA,NGUARD,NMWA)
          ELSE
              CALL FMADDN(MB,MA,NGUARD,NMWA)
          ENDIF
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (KROUND_RETRY == 1 .AND. NGUARD < NDIG) THEN
          KROUND_RETRY = 2
          GO TO 110
      ENDIF

!             Transfer to MC and fix the sign of the result.

      CALL FMMOVE(MWA,MC)
      MC%MP(1) = 1
      IF (JSIGN < 0 .AND. MC%MP(3) /= 0) MC%MP(1) = -1

      IF (KFLAG < 0) THEN
          IF (KSUB == 1) THEN
              NAMEST(NCALL) = 'FMSUB'
          ELSE
              NAMEST(NCALL) = 'FMADD'
          ENDIF
          CALL FMWARN
      ENDIF

      JRSIGN = JRSSAV
      KROUND_RETRY = 0
      RETURN
      END SUBROUTINE FMADD2

      SUBROUTINE FMADD_R1(MA,MB)

!  MA = MA + MB

!  This routine performs the trace printing for addition.  FMADD2_R1 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMADD_R1'
          CALL FMNTR(2,MA,MB,2,1)

          CALL FMADD2_R1(MA,MB)

          NAMEST(NCALL) = 'FMADD_R1'
          CALL FMNTR(1,MA,MA,1,1)
      ELSE
          CALL FMADD2_R1(MA,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMADD_R1

      SUBROUTINE FMADD2_R1(MA,MB)

!  Internal addition routine.  MA = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAS,MBS
      INTEGER :: J,JCOMP,JRSSAV,JSIGN,KRESLT,N1,NGUARD,NMWA
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR. KDEBUG == 1) THEN
          IF (KSUB == 1) THEN
              CALL FMARGS('FMSUB    ',2,MA,MB,KRESLT)
          ELSE
              CALL FMARGS('FMADD    ',2,MA,MB,KRESLT)
          ENDIF
          IF (KRESLT /= 0) THEN
              IF ((KRESLT /= 1 .AND. KRESLT /= 2) .OR. MA%MP(3) == 0 .OR.  &
                  MB%MP(3) == 0) THEN
                  NCALL = NCALL + 1
                  IF (KSUB == 1) THEN
                      NAMEST(NCALL) = 'FMSUB_R1'
                  ELSE
                      NAMEST(NCALL) = 'FMADD_R1'
                  ENDIF
                  CALL FMRSLT(MA,MB,MXY(1),KRESLT)
                  CALL FMEQ(MXY(1),MA)
                  JRSIGN = JRSSAV
                  NCALL = NCALL - 1
                  RETURN
              ENDIF
          ENDIF
      ELSE
          IF (MA%MP(3) == 0) THEN
              CALL FMEQ(MB,MA)
              KFLAG = 1
              IF (KSUB == 1) THEN
                  IF (MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0)  &
                      MA%MP(1) = -MA%MP(1)
                  KFLAG = 0
              ENDIF
              JRSIGN = JRSSAV
              RETURN
          ENDIF
          IF (MB%MP(3) == 0) THEN
              KFLAG = 1
              JRSIGN = JRSSAV
              RETURN
          ENDIF
      ENDIF
      KFLAG = 0
      N1 = NDIG + 1

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD21
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (KROUND_RETRY >= 1) THEN
              NGUARD = NDIG
          ENDIF
      ENDIF
      NMWA = N1 + NGUARD

!             Save the signs of MA and MB and then work with positive numbers.
!             JSIGN is the sign of the result of MA + MB.

      JSIGN = 1
      MAS = MA%MP(1)
      MBS = MB%MP(1)
      IF (KSUB == 1) MBS = -MBS

!             See which one is larger in absolute value.

      JCOMP = 2
      IF (MA%MP(2) > MB%MP(2)) THEN
          JCOMP = 1
      ELSE IF (MB%MP(2) > MA%MP(2)) THEN
          JCOMP = 3
      ELSE
          DO J = 2, N1
             IF (MA%MP(J+1) > MB%MP(J+1)) THEN
                 JCOMP = 1
                 EXIT
             ENDIF
             IF (MB%MP(J+1) > MA%MP(J+1)) THEN
                 JCOMP = 3
                 EXIT
             ENDIF
          ENDDO
      ENDIF

      IF (JCOMP < 3) THEN
          IF (MAS < 0) JSIGN = -1
          JRSIGN = JSIGN
          IF (MAS*MBS > 0) THEN
              CALL FMADDP(MA,MB,NGUARD,NMWA)
          ELSE
              CALL FMADDN(MA,MB,NGUARD,NMWA)
          ENDIF
      ELSE
          IF (MBS < 0) JSIGN = -1
          JRSIGN = JSIGN
          IF (MAS*MBS > 0) THEN
              CALL FMADDP(MB,MA,NGUARD,NMWA)
          ELSE
              CALL FMADDN(MB,MA,NGUARD,NMWA)
          ENDIF
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (KROUND_RETRY == 1 .AND. NGUARD < NDIG) THEN
          KROUND_RETRY = 2
          GO TO 110
      ENDIF

!             Transfer to MA and fix the sign of the result.

      CALL FMMOVE(MWA,MA)
      MA%MP(1) = 1
      IF (JSIGN < 0 .AND. MA%MP(3) /= 0) MA%MP(1) = -1

      IF (KFLAG < 0) THEN
          IF (KSUB == 1) THEN
              NAMEST(NCALL) = 'FMSUB_R1'
          ELSE
              NAMEST(NCALL) = 'FMADD_R1'
          ENDIF
          CALL FMWARN
      ENDIF

      JRSIGN = JRSSAV
      KROUND_RETRY = 0
      RETURN
      END SUBROUTINE FMADD2_R1

      SUBROUTINE FMADD_R2(MA,MB)

!  MB = MA + MB

!  This routine performs the trace printing for addition.  FMADD2_R2 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMADD_R2'
          CALL FMNTR(2,MA,MB,2,1)

          CALL FMADD2_R2(MA,MB)

          NAMEST(NCALL) = 'FMADD_R2'
          CALL FMNTR(1,MB,MB,1,1)
      ELSE
          CALL FMADD2_R2(MA,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMADD_R2

      SUBROUTINE FMADD2_R2(MA,MB)

!  Internal addition routine.  MB = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAS,MBS
      INTEGER :: J,JCOMP,JRSSAV,JSIGN,KRESLT,N1,NGUARD,NMWA
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR. KDEBUG == 1) THEN
          IF (KSUB == 1) THEN
              CALL FMARGS('FMSUB    ',2,MA,MB,KRESLT)
          ELSE
              CALL FMARGS('FMADD    ',2,MA,MB,KRESLT)
          ENDIF
          IF (KRESLT /= 0) THEN
              IF ((KRESLT /= 1 .AND. KRESLT /= 2) .OR. MA%MP(3) == 0 .OR.  &
                  MB%MP(3) == 0) THEN
                  NCALL = NCALL + 1
                  IF (KSUB == 1) THEN
                      NAMEST(NCALL) = 'FMSUB_R2'
                  ELSE
                      NAMEST(NCALL) = 'FMADD_R2'
                  ENDIF
                  CALL FMRSLT(MA,MB,MXY(1),KRESLT)
                  CALL FMEQ(MXY(1),MB)
                  JRSIGN = JRSSAV
                  NCALL = NCALL - 1
                  RETURN
              ENDIF
          ENDIF
      ELSE
          IF (MA%MP(3) == 0) THEN
              KFLAG = 1
              IF (KSUB == 1) THEN
                  IF (MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0)  &
                      MB%MP(1) = -MB%MP(1)
                  KFLAG = 0
              ENDIF
              JRSIGN = JRSSAV
              RETURN
          ENDIF
          IF (MB%MP(3) == 0) THEN
              CALL FMEQ(MA,MB)
              KFLAG = 1
              JRSIGN = JRSSAV
              RETURN
          ENDIF
      ENDIF
      KFLAG = 0
      N1 = NDIG + 1

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD21
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (KROUND_RETRY >= 1) THEN
              NGUARD = NDIG
          ENDIF
      ENDIF
      NMWA = N1 + NGUARD

!             Save the signs of MA and MB and then work with positive numbers.
!             JSIGN is the sign of the result of MA + MB.

      JSIGN = 1
      MAS = MA%MP(1)
      MBS = MB%MP(1)
      IF (KSUB == 1) MBS = -MBS

!             See which one is larger in absolute value.

      JCOMP = 2
      IF (MA%MP(2) > MB%MP(2)) THEN
          JCOMP = 1
      ELSE IF (MB%MP(2) > MA%MP(2)) THEN
          JCOMP = 3
      ELSE
          DO J = 2, N1
             IF (MA%MP(J+1) > MB%MP(J+1)) THEN
                 JCOMP = 1
                 EXIT
             ENDIF
             IF (MB%MP(J+1) > MA%MP(J+1)) THEN
                 JCOMP = 3
                 EXIT
             ENDIF
          ENDDO
      ENDIF

      IF (JCOMP < 3) THEN
          IF (MAS < 0) JSIGN = -1
          JRSIGN = JSIGN
          IF (MAS*MBS > 0) THEN
              CALL FMADDP(MA,MB,NGUARD,NMWA)
          ELSE
              CALL FMADDN(MA,MB,NGUARD,NMWA)
          ENDIF
      ELSE
          IF (MBS < 0) JSIGN = -1
          JRSIGN = JSIGN
          IF (MAS*MBS > 0) THEN
              CALL FMADDP(MB,MA,NGUARD,NMWA)
          ELSE
              CALL FMADDN(MB,MA,NGUARD,NMWA)
          ENDIF
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (KROUND_RETRY == 1 .AND. NGUARD < NDIG) THEN
          KROUND_RETRY = 2
          GO TO 110
      ENDIF

!             Transfer to MB and fix the sign of the result.

      CALL FMMOVE(MWA,MB)
      MB%MP(1) = 1
      IF (JSIGN < 0 .AND. MB%MP(3) /= 0) MB%MP(1) = -1

      IF (KFLAG < 0) THEN
          IF (KSUB == 1) THEN
              NAMEST(NCALL) = 'FMSUB_R2'
          ELSE
              NAMEST(NCALL) = 'FMADD_R2'
          ENDIF
          CALL FMWARN
      ENDIF

      JRSIGN = JRSSAV
      KROUND_RETRY = 0
      RETURN
      END SUBROUTINE FMADD2_R2

      SUBROUTINE FMADDI(MA,IVAL)

!  MA = MA + IVAL

!  Increment MA by one word integer IVAL.

!  This routine is faster than FMADD when IVAL is small enough so that it can be added to a single
!  word of MA without often causing a carry.  Otherwise FMI2M and FMADD are used.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MAEXP,MKSUM
      INTEGER :: KPTMA
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      TYPE(MULTI) :: MXY(2)


      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMADDI'
          CALL FMNTR(2,MA,MA,1,1)
          CALL FMNTRI(2,IVAL,0)
      ENDIF
      KFLAG = 0

      MAEXP = MA%MP(2)
      IF (MAEXP <= 0 .OR. MAEXP > NDIG) GO TO 110
      KPTMA = INT(MAEXP) + 1
      IF (MA%MP(1) < 0) THEN
          MKSUM = MA%MP(KPTMA+1) - IVAL
      ELSE
          MKSUM = MA%MP(KPTMA+1) + IVAL
      ENDIF

      IF (MKSUM >= MBASE .OR. MKSUM < 0) GO TO 110
      IF (KPTMA == 2 .AND. MKSUM == 0) GO TO 110
      MA%MP(KPTMA+1) = MKSUM
      GO TO 120

  110 CALL FMI2M(IVAL,MXY(1))
      CALL FMADD2_R1(MA,MXY(1))

  120 IF (NTRACE /= 0) THEN
          CALL FMNTR(1,MA,MA,1,1)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMADDI

      SUBROUTINE FMADDN(MA,MB,NGUARD,NMWA)

!  Internal addition routine.  MWA = MA - MB
!  The arguments are such that MA >= MB >= 0.

!  NGUARD is the number of guard digits being carried.
!  NMWA is the number of words in MWA that will be used.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: NGUARD,NMWA
      REAL (KIND(1.0D0)) :: MK,MR
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KP1,KP2,KPT,KSH,N1,N2,NK,NK1
      INTENT (IN) :: MA,MB

      N1 = NDIG + 1

!             Check for an insignificant operand.

      MK = MA%MP(2) - MB%MP(2)
      IF (MK >= NDIG+2) THEN
          DO J = 1, N1
             MWA%MP(J+1) = MA%MP(J+1)
          ENDDO
          MWA%MP(N1+2) = 0
          IF (KROUND == 0 .OR. (KROUND == 2 .AND. JRSIGN == -1) .OR.  &
             (KROUND == -1 .AND. JRSIGN == 1)) THEN
              MWA%MP(N1+1) = MWA%MP(N1+1) - 1
              GO TO 120
          ENDIF
          KFLAG = 1
          RETURN
      ENDIF
      K = INT(MK)
      IF (NGUARD <= 1) NMWA = N1 + 2

!             Subtract MB from MA.

      KP1 = MIN(N1,K+1)
      MWA%MP(K+2) = 0
      DO J = 1, KP1
         MWA%MP(J+1) = MA%MP(J+1)
      ENDDO
      KP2 = K + 2

!             (Inner Loop)

      DO J = KP2+1, N1+1
         MWA%MP(J) = MA%MP(J) - MB%MP(J-K)
      ENDDO

      N2 = NDIG + 2
      IF (N2-K <= 1) N2 = 2 + K
      NK = MIN(NMWA,N1+K)
      DO J = N2, NK
         MWA%MP(J+1) = -MB%MP(J-K+1)
      ENDDO
      NK1 = NK + 1
      DO J = NK1, NMWA
         MWA%MP(J+1) = 0
      ENDDO

!             Normalize.  Fix the sign of any negative digit.

      IF (K > 0) THEN
          DO J = NMWA, KP2, -1
             IF (MWA%MP(J+1) < 0) THEN
                 MWA%MP(J+1) = MWA%MP(J+1) + MBASE
                 MWA%MP(J) = MWA%MP(J) - 1
             ENDIF
          ENDDO

          KPT = KP2 - 1
  110     IF (MWA%MP(KPT+1) < 0 .AND. KPT >= 3) THEN
              MWA%MP(KPT+1) = MWA%MP(KPT+1) + MBASE
              MWA%MP(KPT) = MWA%MP(KPT) - 1
              KPT = KPT - 1
              GO TO 110
          ENDIF
          GO TO 130
      ENDIF

  120 DO J = N1, 3, -1
         IF (MWA%MP(J+1) < 0) THEN
             MWA%MP(J+1) = MWA%MP(J+1) + MBASE
             MWA%MP(J) = MWA%MP(J) - 1
         ENDIF
      ENDDO

!             Shift left if there are any leading zeros in the mantissa.

  130 DO J = 2, NMWA
         IF (MWA%MP(J+1) > 0) THEN
             KSH = J - 2
             GO TO 140
         ENDIF
      ENDDO
      MWA%MP(2) = 0
      RETURN

  140 IF (KSH > 0) THEN
          KL = NMWA - KSH
          DO J = 2, KL
             MWA%MP(J+1) = MWA%MP(J+KSH+1)
          ENDDO
          DO J = KL+1, NMWA
             MWA%MP(J+1) = 0
          ENDDO
          MWA%MP(2) = MWA%MP(2) - KSH
          IF (MK >= NDIG+2) THEN
              MWA%MP(N1+1) = MBASE - 1
          ENDIF
      ENDIF

!             Round the result.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KROUND_RETRY = KROUND_RETRY + 1
      ENDIF
      MR = 2*MWA%MP(NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,0)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                  MWA%MP(N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,0)
          ENDIF
      ENDIF

!             See if the result is equal to one of the input arguments.

      IF (ABS(MA%MP(2)-MB%MP(2)) < NDIG) GO TO 150
      IF (ABS(MA%MP(2)-MB%MP(2)) > NDIG+1) THEN
          KFLAG = 1
          GO TO 150
      ENDIF

      N2 = NDIG + 4
      DO J = 3, N1
         IF (MWA%MP(N2-J+1) /= MA%MP(N2-J+1)) GO TO 150
      ENDDO
      IF (MWA%MP(2) /= MA%MP(2)) GO TO 150
      IF (MWA%MP(3) /= ABS(MA%MP(3))) GO TO 150
      KFLAG = 1

  150 RETURN
      END SUBROUTINE FMADDN

      SUBROUTINE FMADDP(MA,MB,NGUARD,NMWA)

!  Internal addition routine.  MWA = MA + MB
!  The arguments are such that MA >= MB >= 0.

!  NMWA is the number of words in MWA that will be used.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: NGUARD,NMWA
      REAL (KIND(1.0D0)) :: MK,MKT,MR
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KP,KP2,KPT,KSHIFT,N1,N2,NK
      INTENT (IN) :: MA,MB

      N1 = NDIG + 1

!             Check for an insignificant operand.

      MK = MA%MP(2) - MB%MP(2)
      IF (MK >= NDIG+1) THEN
          MWA%MP(2) = MA%MP(2) + 1
          MWA%MP(3) = 0
          DO J = 2, N1
             MWA%MP(J+2) = MA%MP(J+1)
          ENDDO
          MWA%MP(N1+3) = 0
          IF ((KROUND ==  2 .AND. JRSIGN ==  1) .OR.  &
              (KROUND == -1 .AND. JRSIGN == -1)) THEN
              MWA%MP(N1+3) = 1
              GO TO 120
          ENDIF
          KFLAG = 1
          RETURN
      ENDIF
      K = INT(MK)

!             Add MA and MB.

      MWA%MP(2) = MA%MP(2) + 1
      MWA%MP(3) = 0
      DO J = 2, K+1
         MWA%MP(J+2) = MA%MP(J+1)
      ENDDO
      KP2 = K + 2

!             (Inner Loop)

      DO J = KP2+1, N1+1
         MWA%MP(J+1) = MA%MP(J) + MB%MP(J-K)
      ENDDO
      N2 = NDIG + 2
      NK = MIN(NMWA,N1+K)
      DO J = N2, NK
         MWA%MP(J+2) = MB%MP(J-K+1)
      ENDDO
      DO J = NK+1, NMWA
         MWA%MP(J+2) = 0
      ENDDO

!             Normalize.  Fix any digit not less than MBASE.

      IF (K == NDIG) GO TO 140

      IF (K > 0) THEN
          DO J = N1+1, KP2, -1
             IF (MWA%MP(J+1) >= MBASE) THEN
                 MWA%MP(J+1) = MWA%MP(J+1) - MBASE
                 MWA%MP(J) = MWA%MP(J) + 1
             ENDIF
          ENDDO

          KPT = KP2 - 1
  110     IF (MWA%MP(KPT+1) >= MBASE .AND. KPT >= 3) THEN
              MWA%MP(KPT+1) = MWA%MP(KPT+1) - MBASE
              MWA%MP(KPT) = MWA%MP(KPT) + 1
              KPT = KPT - 1
              GO TO 110
          ENDIF
          GO TO 120
      ENDIF

      DO J = N1+1, 3, -1
         IF (MWA%MP(J+1) >= MBASE) THEN
             MWA%MP(J+1) = MWA%MP(J+1) - MBASE
             MWA%MP(J) = MWA%MP(J) + 1
         ENDIF
      ENDDO

!             Shift right if the leading digit is not less than MBASE.

  120 IF (MWA%MP(3) >= MBASE) THEN
  130     KP = NMWA + 4
          DO J = 4, NMWA
             MWA%MP(KP-J+1) = MWA%MP(KP-J)
          ENDDO
          MKT = AINT (MWA%MP(3)/MBASE)
          MWA%MP(4) = MWA%MP(3) - MKT*MBASE
          MWA%MP(3) = MKT
          MWA%MP(2) = MWA%MP(2) + 1
          IF (MWA%MP(3) >= MBASE) GO TO 130
      ENDIF

!             Round the result.

  140 KSHIFT = 0
      IF (MWA%MP(3) == 0) KSHIFT = 1
      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KROUND_RETRY = KROUND_RETRY + 1
      ENDIF
      MR = 2*MWA%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWA%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(KSHIFT+N1+1) = MWA%MP(KSHIFT+N1+1) + 1
                  MWA%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF

!             See if the result is equal to one of the input arguments.

      IF (ABS(MA%MP(2)-MB%MP(2)) < NDIG) GO TO 150
      IF (KSHIFT == 0) GO TO 150
      IF (ABS(MA%MP(2)-MB%MP(2)) > NDIG+1) THEN
          KFLAG = 1
          GO TO 150
      ENDIF

      N2 = NDIG + 4
      DO J = 3, N1
         IF (MWA%MP(N2-J+2) /= MA%MP(N2-J+1)) GO TO 150
      ENDDO
      IF (MWA%MP(2) /= MA%MP(2)+1) GO TO 150
      IF (MWA%MP(4) /= ABS(MA%MP(3))) GO TO 150
      KFLAG = 1

  150 RETURN
      END SUBROUTINE FMADDP

      SUBROUTINE FMARGS(KROUTN,NARGS,MA,MB,KRESLT)

!  Check the input arguments to a routine for special cases.

!  KROUTN - Name of the subroutine that was called
!  NARGS  - The number of input arguments (1 or 2)
!  MA     - First input argument
!  MB     - Second input argument (if NARGS is 2)
!  KRESLT - Result code returned to the calling routine.

!  Result codes:

!   0 - Perform the normal operation
!   1 - The result is the first input argument
!   2 - The result is the second input argument
!   3 - The result is -OVERFLOW
!   4 - The result is +OVERFLOW
!   5 - The result is -UNDERFLOW
!   6 - The result is +UNDERFLOW
!   7 - The result is -1.0
!   8 - The result is +1.0
!   9 - The result is -pi/2
!  10 - The result is +pi/2
!  11 - The result is 0.0
!  12 - The result is UNKNOWN
!  13 - The result is +pi
!  14 - The result is -pi/4
!  15 - The result is +pi/4

      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(9) :: KROUTN
      TYPE(MULTI) :: MA,MB
      INTEGER :: NARGS,KRESLT

      REAL (KIND(1.0D0)) :: MBS
      INTEGER :: J,KWRNSV,NCATMA,NCATMB,NDS

!             These tables define the result codes to be returned for given values of the input
!             argument(s).

!             For example, row 7 column 2 of this array initialization KADD(2,7) = 2 means that if
!             the first argument in a call to FMADD is in category 7 ( -UNDERFLOW ) and the second
!             argument is in category 2 ( near -OVERFLOW but representable ) then the result code
!             is 2 ( the value of the sum is equal to the second input argument ).
!             See routine FMCAT for descriptions of the categories.

      INTEGER :: KADD(15,15) = RESHAPE(  (/                        &
                 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,12,12,      &
                 3, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0,12,      &
                 3, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 4,      &
                 3, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 4,      &
                 3, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 4,      &
                 3, 0, 0, 0, 0, 0,12, 1,12, 0, 0, 0, 0, 0, 4,      &
                 3, 2, 2, 2, 2,12,12, 5,12,12, 2, 2, 2, 2, 4,      &
                 3, 2, 2, 2, 2, 2, 5, 2, 6, 2, 2, 2, 2, 2, 4,      &
                 3, 2, 2, 2, 2,12,12, 6,12,12, 2, 2, 2, 2, 4,      &
                 3, 0, 0, 0, 0, 0,12, 1,12, 0, 0, 0, 0, 0, 4,      &
                 3, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 4,      &
                 3, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 4,      &
                 3, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 4,      &
                12, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 4,      &
                12,12, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4   /)  &
        , (/ 15,15 /) )

      INTEGER :: KMPY(15,15) = RESHAPE(  (/                        &
                 4, 4, 4, 4,12,12,12,11,12,12,12, 3, 3, 3, 3,      &
                 4, 0, 0, 0, 0, 0,12,11,12, 0, 0, 1, 0, 0, 3,      &
                 4, 0, 0, 0, 0, 0,12,11,12, 0, 0, 1, 0, 0, 3,      &
                 4, 0, 0, 0, 0, 0, 6,11, 5, 0, 0, 1, 0, 0, 3,      &
                12, 0, 0, 0, 0, 0, 6,11, 5, 0, 0, 1, 0, 0,12,      &
                12, 0, 0, 0, 0, 0, 6,11, 5, 0, 0, 1, 0, 0,12,      &
                12,12,12, 6, 6, 6, 6,11, 5, 5, 5, 5,12,12,12,      &
                11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,      &
                12,12,12, 5, 5, 5, 5,11, 6, 6, 6, 6,12,12,12,      &
                12, 0, 0, 0, 0, 0, 5,11, 6, 0, 0, 1, 0, 0,12,      &
                12, 0, 0, 0, 0, 0, 5,11, 6, 0, 0, 1, 0, 0,12,      &
                 3, 2, 2, 2, 2, 2, 5,11, 6, 2, 2, 2, 2, 2, 4,      &
                 3, 0, 0, 0, 0, 0,12,11,12, 0, 0, 1, 0, 0, 4,      &
                 3, 0, 0, 0, 0, 0,12,11,12, 0, 0, 1, 0, 0, 4,      &
                 3, 3, 3, 3,12,12,12,11,12,12,12, 4, 4, 4, 4   /)  &
        , (/ 15,15 /) )

      INTEGER :: KDIV(15,15) = RESHAPE(  (/                        &
                12,12,12, 4, 4, 4, 4,12, 3, 3, 3, 3,12,12,12,      &
                12, 0, 0, 0, 0, 0, 4,12, 3, 0, 0, 1, 0, 0,12,      &
                12, 0, 0, 0, 0, 0, 4,12, 3, 0, 0, 1, 0, 0,12,      &
                 6, 0, 0, 0, 0, 0, 4,12, 3, 0, 0, 1, 0, 0, 5,      &
                 6, 0, 0, 0, 0, 0,12,12,12, 0, 0, 1, 0, 0, 5,      &
                 6, 0, 0, 0, 0, 0,12,12,12, 0, 0, 1, 0, 0, 5,      &
                 6, 6, 6, 6,12,12,12,12,12,12,12, 5, 5, 5, 5,      &
                11,11,11,11,11,11,11,12,11,11,11,11,11,11,11,      &
                 5, 5, 5, 5,12,12,12,12,12,12,12, 6, 6, 6, 6,      &
                 5, 0, 0, 0, 0, 0,12,12,12, 0, 0, 1, 0, 0, 6,      &
                 5, 0, 0, 0, 0, 0,12,12,12, 0, 0, 1, 0, 0, 6,      &
                 5, 0, 0, 0, 0, 0, 3,12, 4, 0, 0, 1, 0, 0, 6,      &
                12, 0, 0, 0, 0, 0, 3,12, 4, 0, 0, 1, 0, 0,12,      &
                12, 0, 0, 0, 0, 0, 3,12, 4, 0, 0, 1, 0, 0,12,      &
                12,12,12, 3, 3, 3, 3,12, 4, 4, 4, 4,12,12,12   /)  &
        , (/ 15,15 /) )

      INTEGER :: KPWR(15,15) = RESHAPE(  (/                        &
                12,12, 0, 5,12,12,12, 8,12,12,12, 3, 0,12,12,      &
                12, 0, 0, 0,12,12,12, 8,12,12,12, 1, 0, 0,12,      &
                12, 0, 0, 0,12,12,12, 8,12,12,12, 1, 0, 0,12,      &
                12, 0, 0, 0,12,12,12, 8,12,12,12, 1, 0, 0,12,      &
                12, 0, 0, 0,12,12,12, 8,12,12,12, 1, 0, 0,12,      &
                12, 0, 0, 0,12,12,12, 8,12,12,12, 1, 0, 0,12,      &
                12,12, 0, 3,12,12,12, 8,12,12,12, 5, 0,12,12,      &
                12,12,12,12,12,12,12,12,11,11,11,11,11,11,11,      &
                 4, 4, 4, 4,12,12,12, 8,12,12,12, 6, 6, 6, 6,      &
                 4, 4, 0, 0, 0, 8, 8, 8, 8, 0, 0, 1, 0, 6, 6,      &
                 4, 4, 0, 0, 0, 8, 8, 8, 8, 0, 0, 1, 0, 6, 6,      &
                 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,      &
                 6, 6, 0, 0, 0, 8, 8, 8, 8, 8, 0, 1, 0, 4, 4,      &
                 6, 6, 0, 0, 0, 8, 8, 8, 8, 8, 0, 1, 0, 4, 4,      &
                 6, 6, 6, 6,12,12,12, 8,12,12,12, 4, 4, 4, 4   /)  &
        , (/ 15,15 /) )

      INTEGER :: KSQRT(15) = (/ 12,12,12,12,12,12,12,11,12, 0, 0, 8, 0, 0,12 /)
      INTEGER :: KEXP(15)  = (/  6, 6, 0, 0, 0, 8, 8, 8, 8, 8, 0, 0, 0, 4, 4 /)
      INTEGER :: KLN(15)   = (/ 12,12,12,12,12,12,12,12,12, 0, 0,11, 0, 0,12 /)
      INTEGER :: KSIN(15)  = (/ 12,12, 0, 0, 0, 0, 5,11, 6, 0, 0, 0, 0,12,12 /)
      INTEGER :: KCOS(15)  = (/ 12,12, 0, 0, 0, 8, 8, 8, 8, 8, 0, 0, 0,12,12 /)
      INTEGER :: KTAN(15)  = (/ 12,12, 0, 0, 0, 0, 5,11, 6, 0, 0, 0, 0,12,12 /)
      INTEGER :: KASIN(15) = (/ 12,12,12, 9, 0, 0, 5,11, 6, 0, 0,10,12,12,12 /)
      INTEGER :: KACOS(15) = (/ 12,12,12,13, 0,10,10,10,10,10, 0,11,12,12,12 /)
      INTEGER :: KATAN(15) = (/  9, 9, 0,14, 0, 0, 5,11, 6, 0, 0,15, 0,10,10 /)
      INTEGER :: KSINH(15) = (/  3, 3, 0, 0, 0, 1, 5,11, 6, 1, 0, 0, 0, 4, 4 /)
      INTEGER :: KCOSH(15) = (/  4, 4, 0, 0, 0, 8, 8, 8, 8, 8, 0, 0, 0, 4, 4 /)
      INTEGER :: KTANH(15) = (/  7, 7, 0, 0, 0, 1, 5,11, 6, 1, 0, 0, 0, 8, 8 /)
      INTEGER :: KLG10(15) = (/ 12,12,12,12,12,12,12,12,12, 0, 0,11, 0, 0,12 /)
      INTENT (IN) :: MA,MB

      KRESLT = 12
      KFLAG = -4
      IF (MA%MP(2) == MUNKNO) RETURN
      IF (NARGS == 2) THEN
          IF (MB%MP(2) == MUNKNO) RETURN
      ENDIF
      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      NAMEST(NCALL) = KROUTN

!             Check the validity of parameters if this is a user call.

      IF (NCALL > 1 .AND. KDEBUG == 0) GO TO 130

!             Check NDIG.

      IF (NDIG < 2) THEN
          KFLAG = -1
          CALL FMWARN
          NDS = NDIG
          IF (NDIG < 2) NDIG = 2
          WRITE (KW,                                                      &
                 "(' NDIG was',I10,'.  It has been changed to',I10,'.')"  &
                ) NDS,NDIG
          RETURN
      ENDIF

!             Check MBASE.

      IF (MBASE < 2 .OR. MBASE > MXBASE) THEN
          KFLAG = -2
          CALL FMWARN
          MBS = MBASE
          IF (MBASE < 2) MBASE = 2
          IF (MBASE > MXBASE) MBASE = MXBASE
          WRITE (KW,                                                       &
                 "(' MBASE was',I10,'.  It has been changed to',I10,'.')"  &
                ) INT(MBS),INT(MBASE)
          CALL FMCONS
          RETURN
      ENDIF

!             Check exponent range.

      IF (MA%MP(2) > MXEXP+1 .OR. MA%MP(2) < -MXEXP) THEN
          IF (ABS(MA%MP(2)) /= MEXPOV .OR. ABS(MA%MP(3)) /= 1) THEN
              KFLAG = -3
              CALL FMWARN
              RETURN
          ENDIF
      ENDIF
      IF (NARGS == 2) THEN
          IF (MB%MP(2) > MXEXP+1 .OR. MB%MP(2) < -MXEXP) THEN
              IF (ABS(MB%MP(2)) /= MEXPOV .OR. ABS(MB%MP(3)) /= 1) THEN
                  KFLAG = -3
                  CALL FMWARN
                  RETURN
              ENDIF
          ENDIF
      ENDIF

!             Check for properly normalized digits in the input arguments.

      IF (ABS(MA%MP(2)-INT(MA%MP(2))) /= 0) KFLAG = 1
      IF (MA%MP(3) <= (-1) .OR. MA%MP(3) >= MBASE .OR.  &
          ABS(MA%MP(3)-INT(MA%MP(3))) /= 0) KFLAG = 2
      IF (KDEBUG == 0) GO TO 110
      DO J = 3, NDIG+1
         IF (MA%MP(J+1) < 0 .OR. MA%MP(J+1) >= MBASE .OR.  &
             ABS(MA%MP(J+1)-INT(MA%MP(J+1))) /= 0) THEN
             KFLAG = J
             GO TO 110
         ENDIF
      ENDDO
  110 IF (KFLAG /= 0) THEN
          J = KFLAG
          KFLAG = -4
          KWRNSV = KWARN
          IF (KWARN >= 2) KWARN = 1
          CALL FMWARN
          KWARN = KWRNSV
          IF (KWARN >= 1) THEN
              WRITE (KW,*) ' First invalid array element:  MA(', J,') = ',MA%MP(J+1)
          ENDIF
          KFLAG = -4
          IF (KWARN >= 2) THEN
              STOP
          ENDIF
          RETURN
      ENDIF
      IF (NARGS == 2) THEN
          IF (ABS(MB%MP(2)-INT(MB%MP(2))) /= 0) KFLAG = 1
          IF (MB%MP(3) <= (-1) .OR. MB%MP(3) >= MBASE .OR.  &
              ABS(MB%MP(3)-INT(MB%MP(3))) /= 0) KFLAG = 2
          IF (KDEBUG == 0) GO TO 120
          DO J = 3, NDIG+1
             IF (MB%MP(J+1) < 0 .OR. MB%MP(J+1) >= MBASE .OR.  &
                 ABS(MB%MP(J+1)-INT(MB%MP(J+1))) /= 0) THEN
                 KFLAG = J
                 GO TO 120
             ENDIF
          ENDDO
  120     IF (KFLAG /= 0) THEN
              J = KFLAG
              KFLAG = -4
              KWRNSV = KWARN
              IF (KWARN >= 2) KWARN = 1
              CALL FMWARN
              KWARN = KWRNSV
              IF (KWARN >= 1) THEN
                  WRITE (KW,*) ' First invalid array element:  MB(', J,') = ',MB%MP(J+1)
              ENDIF
              KFLAG = -4
              IF (KWARN >= 2) THEN
                  STOP
              ENDIF
              RETURN
          ENDIF
      ENDIF

!             Check for special cases.

  130 CALL FMCAT(MA,NCATMA)
      IF (NCATMA > 15) THEN
          KRESLT = 12
          RETURN
      ENDIF
      NCATMB = 0
      IF (NARGS == 2) THEN
          CALL FMCAT(MB,NCATMB)
          IF (NCATMB > 15) THEN
              KRESLT = 12
              RETURN
          ENDIF
      ENDIF

      IF (KROUTN == 'FMADD') THEN
          KRESLT = KADD(NCATMB,NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMSUB') THEN
          IF (NCATMB < 16) NCATMB = 16 - NCATMB
          KRESLT = KADD(NCATMB,NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMMPY') THEN
          KRESLT = KMPY(NCATMB,NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMDIV') THEN
          KRESLT = KDIV(NCATMB,NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMPWR') THEN
          KRESLT = KPWR(NCATMB,NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMSQRT' .OR. KROUTN == 'FMSQRT_R1') THEN
          KRESLT = KSQRT(NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMEXP') THEN
          KRESLT = KEXP(NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMLN') THEN
          KRESLT = KLN(NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMSIN') THEN
          KRESLT = KSIN(NCATMA)
          IF (KRAD == 0 .AND. (NCATMA == 2 .OR. NCATMA == 14)) KRESLT = 0
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMCOS') THEN
          KRESLT = KCOS(NCATMA)
          IF (KRAD == 0 .AND. (NCATMA == 2 .OR. NCATMA == 14)) KRESLT = 0
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMTAN') THEN
          KRESLT = KTAN(NCATMA)
          IF (KRAD == 0 .AND. (NCATMA == 2 .OR. NCATMA == 14)) KRESLT = 0
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMASIN') THEN
          KRESLT = KASIN(NCATMA)
          IF ((NCATMA == 7.OR.NCATMA == 9) .AND. KRAD == 0) KRESLT = 12
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMACOS') THEN
          KRESLT = KACOS(NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMATAN') THEN
          KRESLT = KATAN(NCATMA)
          IF ((NCATMA == 7.OR.NCATMA == 9) .AND. KRAD == 0) KRESLT = 12
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMSINH') THEN
          KRESLT = KSINH(NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMCOSH') THEN
          KRESLT = KCOSH(NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMTANH') THEN
          KRESLT = KTANH(NCATMA)
          GO TO 140
      ENDIF

      IF (KROUTN == 'FMLG10') THEN
          KRESLT = KLG10(NCATMA)
          GO TO 140
      ENDIF

      KRESLT = 0
      RETURN

  140 IF (KRESLT == 12) THEN
          KFLAG = -4
          CALL FMWARN
      ENDIF
      IF (KRESLT == 3 .OR. KRESLT == 4) THEN
          IF (NCATMA == 1 .OR. NCATMA == 7 .OR. NCATMA == 9 .OR. NCATMA == 15 .OR.  &
              NCATMB == 1 .OR. NCATMB == 7 .OR. NCATMB == 9 .OR. NCATMB == 15) THEN
              KFLAG = -5
          ELSE
              KFLAG = -5
              CALL FMWARN
          ENDIF
      ENDIF
      IF (KRESLT == 5 .OR. KRESLT == 6) THEN
          IF (NCATMA == 1 .OR. NCATMA == 7 .OR. NCATMA == 9 .OR. NCATMA == 15 .OR.  &
              NCATMB == 1 .OR. NCATMB == 7 .OR. NCATMB == 9 .OR. NCATMB == 15) THEN
              KFLAG = -6
          ELSE
              KFLAG = -6
              CALL FMWARN
          ENDIF
      ENDIF
      RETURN
      END SUBROUTINE FMARGS

      SUBROUTINE FMASIN(MA,MB)

!  MB = ARCSIN(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG .AND. KRAD == 1) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMASIN'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMMPY(MXY(1),MA,MXY(2))
          CALL FMDIVI(MXY(2),6,MXY(3))
          IF (MXY(3)%MP(2) > MEXPUN) THEN
              CALL FMADD(MA,MXY(3),MB)
          ELSE IF (MA%MP(2) == MEXPUN) THEN
              CALL FMEQ(MA,MB)
          ELSE IF (MA%MP(1) < 0 .AND. KROUND == -1) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMADD(MXY(1),MXY(2),MB)
              MB%MP(2) = MA%MP(2) + MB%MP(2)
          ELSE IF (MA%MP(1) >= 0 .AND. KROUND == 2) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMADD(MXY(1),MXY(2),MB)
              MB%MP(2) = MA%MP(2) + MB%MP(2)
          ELSE
              CALL FMEQ(MA,MB)
          ENDIF
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (MB%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMASIN'
              KFLAG = -4
              CALL FMWARN
              NCALL = NCALL - 1
          ELSE IF (ABS(MB%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMASIN'
              IF (MB%MP(2) == MEXPOV) KFLAG = -5
              IF (MB%MP(2) == MEXPUN) KFLAG = -6
              CALL FMWARN
              NCALL = NCALL - 1
          ENDIF
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMASIN'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(2) > 0 .OR. MA%MP(3) == 0) THEN
          CALL FMENTR('FMASIN   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMASIN'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(5),NDSAVE,NDIG)
      IF (KROUND /= 1 .AND. KRAD /= 1) THEN
          CALL FMST2M('0.5',MXY(1))
          CALL FMSUB(MXY(5),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0) THEN
              CALL FMST2M('30',MXY(5))
              GO TO 120
          ENDIF
          CALL FMADD(MXY(5),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0) THEN
              CALL FMST2M('-30',MXY(5))
              GO TO 120
          ENDIF
      ENDIF

!             Use ASIN(X) = ATAN(X/SQRT(1-X*X))

      CALL FMI2M(1,MXY(3))
      CALL FMSUB(MXY(3),MXY(5),MXY(1))
      CALL FMADD(MXY(3),MXY(5),MXY(2))
      CALL FMMPY_R2(MXY(1),MXY(2))
      CALL FMSQRT_R1(MXY(2))
      CALL FMDIV(MXY(5),MXY(2),MXY(4))
      CALL FMATAN(MXY(4),MXY(5))

!             Round the result and return.

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(5)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(5),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMASIN

      SUBROUTINE FMASINH(MA,MB)

!  MB = ARCSINH(MA)      Inverse hyperbolic sine.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENTR('FMASINH  ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

      IF (MXY(1)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(2))
      ELSE IF (MXY(1)%MP(2) == MEXPOV) THEN
          KFLAG = -4
          CALL FMST2M('UNKNOWN',MXY(2))
      ELSE IF (MXY(1)%MP(2) <= -MEXPAB) THEN
          CALL FMSIN(MXY(1),MXY(2))
          IF (KROUND /= 1) KR_RETRY = 2
      ELSE IF (MXY(1)%MP(2) < -NDIG) THEN
          CALL FMSQR(MXY(1),MXY(2))
          CALL FMMPY_R2(MXY(1),MXY(2))
          CALL FMDIVI_R1(MXY(2),6)
          CALL FMSUB_R2(MXY(1),MXY(2))
          IF (KROUND /= 1) KR_RETRY = 2
      ELSE IF (4.0*(MXY(1)%MP(2)-1) > NDIG) THEN
          CALL FMMPYI(MXY(1),2,MXY(2))
          IF (MXY(2)%MP(2) == MEXPOV) THEN
              IF (MA%MP(1) < 0) THEN
                  CALL FMABS(MXY(1),MXY(3))
                  CALL FMLN(MXY(3),MXY(2))
                  CALL FMLNI(2,MXY(3))
                  CALL FMADD_R1(MXY(2),MXY(3))
                  IF (MXY(2)%MP(2) /= MUNKNO .AND. MXY(2)%MP(3) /= 0)  &
                      MXY(2)%MP(1) = -MXY(2)%MP(1)
              ELSE
                  CALL FMLN(MXY(1),MXY(2))
                  CALL FMLNI(2,MXY(3))
                  CALL FMADD_R1(MXY(2),MXY(3))
              ENDIF
          ELSE
              IF (MA%MP(1) < 0) THEN
                  CALL FMI2M(1,MXY(3))
                  CALL FMSQR(MXY(2),MXY(4))
                  CALL FMDIV_R2(MXY(3),MXY(4))
                  CALL FMABS(MXY(2),MXY(3))
                  CALL FMLN(MXY(3),MXY(2))
                  CALL FMADD_R1(MXY(2),MXY(4))
                  IF (MXY(2)%MP(2) /= MUNKNO .AND. MXY(2)%MP(3) /= 0)  &
                      MXY(2)%MP(1) = -MXY(2)%MP(1)
              ELSE
                  CALL FMI2M(1,MXY(3))
                  CALL FMSQR(MXY(2),MXY(4))
                  CALL FMDIV_R2(MXY(3),MXY(4))
                  CALL FMLN(MXY(2),MXY(3))
                  CALL FMADD(MXY(3),MXY(4),MXY(2))
              ENDIF
          ENDIF
      ELSE IF (MXY(1)%MP(2) > 0) THEN
          IF (MA%MP(1) < 0) THEN
              CALL FMSQR(MXY(1),MXY(3))
              CALL FMI2M(1,MXY(2))
              CALL FMADD_R1(MXY(3),MXY(2))
              CALL FMSQRT_R1(MXY(3))
              CALL FMSUB_R1(MXY(3),MXY(1))
              CALL FMLN(MXY(3),MXY(2))
              IF (MXY(2)%MP(2) /= MUNKNO .AND. MXY(2)%MP(3) /= 0)  &
                  MXY(2)%MP(1) = -MXY(2)%MP(1)
          ELSE
              CALL FMSQR(MXY(1),MXY(3))
              CALL FMI2M(1,MXY(2))
              CALL FMADD_R1(MXY(3),MXY(2))
              CALL FMSQRT_R1(MXY(3))
              CALL FMADD_R2(MXY(1),MXY(3))
              CALL FMLN(MXY(3),MXY(2))
          ENDIF
      ELSE
          CALL FMSQR(MXY(1),MXY(3))
          CALL FMI2M(1,MXY(2))
          CALL FMADD_R1(MXY(3),MXY(2))
          CALL FMSQRT_R1(MXY(3))
          CALL FMDIV_R2(MXY(1),MXY(3))
          CALL FMATANH(MXY(3),MXY(2))
      ENDIF

      NAMEST(NCALL) = 'FMASINH'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

      CALL FMEXIT(MXY(2),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMASINH

      SUBROUTINE FMATAN(MA,MB)

!  MB = ARCTAN(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      DOUBLE PRECISION :: X
      TYPE(MULTI) :: MA,MB
      INTEGER :: NSTACK(49)
      REAL (KIND(1.0D0)) :: MA1,MAS,MAXV,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,J2,K,K2,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,KST,KTWO,KWRNSV,  &
                 NDSAV1,NDSAVE,NDSV,NMETHD,NTERM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KWRNSV = KWARN

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG .AND. KRAD == 1) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMATAN'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMMPY(MXY(1),MA,MXY(2))
          CALL FMDIVI(MXY(2),-3,MXY(3))
          IF (MXY(3)%MP(2) > MEXPUN) THEN
              CALL FMADD(MA,MXY(3),MB)
          ELSE IF (MA%MP(2) == MEXPUN) THEN
              CALL FMEQ(MA,MB)
          ELSE IF (MA%MP(1) < 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMSUB(MXY(1),MXY(2),MXY(3))
              MXY(3)%MP(2) = MA%MP(2) + MXY(3)%MP(2)
              CALL FMEQ(MXY(3),MB)
          ELSE IF (MA%MP(1) >= 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMSUB(MXY(1),MXY(2),MXY(3))
              MXY(3)%MP(2) = MA%MP(2) + MXY(3)%MP(2)
              CALL FMEQ(MXY(3),MB)
          ELSE
              CALL FMEQ(MA,MB)
          ENDIF
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = KWRNSV
          IF (MB%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMATAN'
              KFLAG = -4
              CALL FMWARN
              NCALL = NCALL - 1
          ELSE IF (ABS(MB%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMATAN'
              IF (MB%MP(2) == MEXPOV) KFLAG = -5
              IF (MB%MP(2) == MEXPUN) KFLAG = -6
              CALL FMWARN
              NCALL = NCALL - 1
          ENDIF
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMATAN'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (KROUND /= 1 .AND. MA%MP(2) > NDIG .AND. MA%MP(2) /= MUNKNO .AND.  &
          KRAD /= 1) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMATAN'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          KWARN = 0
          IF (MA%MP(1) < 0) THEN
              IF (MA%MP(2) /= MEXPOV) THEN
                  CALL FMI2M(180,MXY(1))
                  CALL FMPI(MXY(2))
                  CALL FMDIV(MXY(1),MXY(2),MXY(3))
                  CALL FMDIV(MXY(3),MA,MXY(2))
                  CALL FMI2M(-90,MXY(1))
                  CALL FMSUB(MXY(1),MXY(2),MXY(3))
              ELSE
                  CALL FMI2M(-90,MXY(1))
                  CALL FMTINY(MXY(2))
                  CALL FMADD(MXY(1),MXY(2),MXY(3))
              ENDIF
          ELSE
              IF (MA%MP(2) /= MEXPOV) THEN
                  CALL FMI2M(180,MXY(1))
                  CALL FMPI(MXY(2))
                  CALL FMDIV(MXY(1),MXY(2),MXY(3))
                  CALL FMDIV(MXY(3),MA,MXY(2))
                  CALL FMI2M(90,MXY(1))
                  CALL FMSUB(MXY(1),MXY(2),MXY(3))
              ELSE
                  CALL FMI2M(90,MXY(1))
                  CALL FMTINY(MXY(2))
                  CALL FMSUB(MXY(1),MXY(2),MXY(3))
              ENDIF
          ENDIF
          CALL FMEQ(MXY(3),MB)
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = KWRNSV
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMATAN'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0) THEN
          CALL FMENTR('FMATAN   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMATAN'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      NDIG = NDIG + NDIG/100
      CALL FMEQU(MA,MXY(3),NDSAVE,NDIG)
      IF (KROUND /= 1 .AND. KRAD /= 1) THEN
          CALL FMI2M(1,MXY(1))
          CALL FMSUB(MXY(3),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0) THEN
              CALL FMST2M('45',MXY(5))
              GO TO 160
          ENDIF
          CALL FMADD(MXY(3),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0) THEN
              CALL FMST2M('-45',MXY(5))
              GO TO 160
          ENDIF
      ENDIF

!             If MA >= 1 work with 1/MA.

      MA1 = MA%MP(2)
      MAS = MA%MP(1)
      MXY(3)%MP(1) = 1
      IF (MA1 >= 1) THEN
          CALL FMI2M(1,MXY(5))
          CALL FMDIV_R2(MXY(5),MXY(3))
      ENDIF

      KRSAVE = KRAD
      KRAD = 1
      X = MXY(3)%MP(2)

!             In case pi has not been computed at the current precision and will be needed here,
!             get it to full precision first to avoid repeated calls at increasing precision during
!             Newton iteration.

      IF (MA1 >= 1 .OR. KRSAVE == 0) THEN
          IF (MBSPI /= MBASE .OR. NDIGPI < NDIG)  THEN
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMPI(MXY(5))
              NDIG = NDSV
          ENDIF
      ENDIF

      NMETHD = 1
      IF (NDIG*ALOGMT > 2000) NMETHD = 2
      IF (MXY(3)%MP(2) < -NDIG) NMETHD = 1
      IF (NMETHD == 2) GO TO 140

!             Method 1.  Reduce the argument and use the Taylor series.
!                        Atan(x) = x - x^3 / 3 + x^5 / 5 - ...

      K2 = MAX(2,INT(0.67*(NDIG*ALOGMT)**0.3333 + 0.4))
      K2 = MAX(K2,3)
      IF (MXY(3)%MP(2) <= -NDIG/3) THEN
          K2 = 0
      ELSE
          IF (MXY(3)%MP(2)*DLOGMB < LOG(2.0D0**(-K2))) THEN
              K2 = 0
          ELSE
              CALL FMM2DP(MXY(3),X)
              K = K2 + 1
              DO J = 0, K
                 IF (X < 0.375D0/2.0D0**(J)) THEN
                     K2 = K2 - 1
                     IF (K2 <= 0) EXIT
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      CALL FMEQ(MXY(3),MXY(1))
      CALL FMI2M(1,MXY(4))
      DO J = 1, K2
         CALL FMSQR(MXY(1),MXY(2))
         CALL FMADD_R2(MXY(4),MXY(2))
         CALL FMSQRT_R1(MXY(2))
         CALL FMSUB_R1(MXY(2),MXY(4))
         CALL FMDIV_R2(MXY(2),MXY(1))
      ENDDO

      J2 = INT(0.96*(NDIG*ALOGMT)**0.3333 - 1.7)
      J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))

!             Split into J2 concurrent sums and reduce NDIG while computing each term in the sum
!             as the terms get smaller.

      NTERM = 1
      DO J = 1, J2
         IF (NTERM > 1) THEN
             CALL FMCSDIVI(MXY(1),NTERM,MJSUMS(J))
         ELSE
             CALL FMEQ(MXY(1),MJSUMS(J))
         ENDIF
         NTERM = NTERM + 2
      ENDDO
      NDSAV1 = NDIG
      CALL FMEQ(MXY(1),MXY(4))
      CALL FMSQR_R1(MXY(1))
      IF (MXY(1)%MP(2) < -NDIG) GO TO 130
      CALL FMIPWR(MXY(1),J2,MXY(2))

  120 CALL FMCSMPY_R1(MXY(4),MXY(2))
      DO J = 1, J2
         CALL FMCSDIVI(MXY(4),NTERM,MXY(5))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(5))
         IF (KFLAG /= 0) GO TO 130
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(5)%MP(2))
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 2
      ENDDO
      GO TO 120

!             Put the J2 separate sums back together.

  130 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(5))
      IF (MXY(1)%MP(2) /= MUNKNO .AND. MXY(1)%MP(3) /= 0)  &
          MXY(1)%MP(1) = -MXY(1)%MP(1)
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(5),MXY(1))
         CALL FMADD_R1(MXY(5),MJSUMS(J2-J+1))
      ENDDO

!             Reverse the argument reduction.

      KTWO = 1
      MAXV = MXBASE/2
      DO J = 1, K2
         KTWO = 2*KTWO
         IF (KTWO > MAXV) THEN
             CALL FMCSMPYI_R1(MXY(5),KTWO)
             KTWO = 1
         ENDIF
      ENDDO
      IF (KTWO > 1) CALL FMCSMPYI_R1(MXY(5),KTWO)

      GO TO 150

!             Method 2.  Newton iteration.

  140 CALL FMI2M(0,MXY(1))
      CALL FMI2M(0,MXY(2))
      CALL FMI2M(0,MXY(4))

      IF (MXY(3)%MP(2)*DLOGMB < -46) THEN
          CALL FMEQ(MXY(3),MXY(5))
      ELSE
          CALL FMM2DP(MXY(3),X)
          X = ATAN(X)
          CALL FMDPM(X,MXY(5))
      ENDIF
      CALL FMDIG(NSTACK,KST)

!             Newton iteration.

      DO J = 1, KST
         NDIG = NSTACK(J)
         CALL FMSIN(MXY(5),MXY(4))
         IF (2*MXY(5)%MP(2) <= -NDIG) THEN
             CALL FMI2M(1,MXY(2))
             CALL FMSUB(MXY(2),MXY(4),MXY(1))
             CALL FMADD_R1(MXY(2),MXY(4))
             CALL FMMPY_R1(MXY(1),MXY(2))
             CALL FMSQRT(MXY(1),MXY(2))
             CALL FMDIV_R2(MXY(4),MXY(2))
             CALL FMSUB_R1(MXY(2),MXY(3))
             CALL FMMPY_R2(MXY(1),MXY(2))
             CALL FMSUB_R1(MXY(5),MXY(2))
         ELSE
             CALL FMSQR(MXY(4),MXY(1))
             CALL FMI2M(1,MXY(2))
             CALL FMSUB_R2(MXY(2),MXY(1))
             CALL FMSQRT(MXY(1),MXY(2))
             CALL FMDIV_R2(MXY(4),MXY(2))
             CALL FMSUB_R1(MXY(2),MXY(3))
             CALL FMMPY_R2(MXY(1),MXY(2))
             CALL FMSUB_R1(MXY(5),MXY(2))
         ENDIF
      ENDDO

!             If MA >= 1 use pi/2 - ATAN(1/MA)

  150 IF (MA1 >= 1) THEN
          CALL FMDIVI(MPISAV,2,MXY(4))
          CALL FMSUB_R2(MXY(4),MXY(5))
      ENDIF

!             Convert to degrees if necessary, round and return.

      KRAD = KRSAVE
      IF (KRAD == 0) THEN
          CALL FMMPYI_R1(MXY(5),180)
          CALL FMDIV_R1(MXY(5),MPISAV)
      ENDIF
      IF (MXY(5)%MP(2) /= MUNKNO .AND. MXY(5)%MP(3) /= 0 .AND. MAS < 0)  &
          MXY(5)%MP(1) = -MXY(5)%MP(1)

  160 IF (KFLAG == 1) KFLAG = 0
      KWARN = KWRNSV

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(5)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(5),MB,NDSAVE,MXSAVE,KOVUN)
      IF (KFLAG == 1) KFLAG = 0
      RETURN
      END SUBROUTINE FMATAN

      SUBROUTINE FMATANH(MA,MB)

!  MB = ARCTANH(MA)      Inverse hyperbolic tangent.

      USE ModLib_FMVALS
      IMPLICIT NONE

      DOUBLE PRECISION :: X
      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAS,MAXV,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,J2,K,K2,KL,KOVUN,KR_RETRY,KRESLT,K_RETURN_CODE,KRSAVE,KTWO,  &
                 KWRNSV,NDSAV1,NDSAVE,NMETHD,NTERM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(10),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KWRNSV = KWARN

      IF (MBLOGS /= MBASE) CALL FMCONS
      MAS = MA%MP(1)
      K = 0
      K_RETURN_CODE = 0
      NCALL = NCALL + 1

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < 1) THEN
          J = NTRACE
          NTRACE = 0
          KL = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMDIVI(MXY(1),3,MXY(2))
          IF (MXY(2)%MP(2) < -NDIG) K = 1
          NTRACE = J
          KWARN = KL
      ENDIF
      IF (KROUND /= 1 .AND. K == 1) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMSQR(MXY(1),MXY(2))
          CALL FMMPY(MXY(1),MXY(2),MXY(3))
          CALL FMDIVI(MXY(3),3,MXY(5))
          CALL FMEQ(MXY(1),MXY(4))
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG .AND.  &
              MXY(4)%MP(2) > MEXPUN) THEN
              CALL FMEQU(MXY(4),MXY(10),NDIG,NDSAVE)
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(6),MXY(8),NDSAVE,NDIG)
              CALL FMSUB(MXY(4),MXY(8),MXY(6))
              IF (MXY(6)%MP(3) == 0) THEN
                  CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
                  NDIG = NDSAVE
                  KROUND = KRSAVE
                  CALL FMADD(MXY(6),MXY(7),MXY(9))
                  IF (MXY(9)%MP(2) >= MEXPOV) THEN
                      IF (MXY(6)%MP(1) > 0) THEN
                          IF (MXY(7)%MP(1) < 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MXY(9))
                          ELSE IF (MXY(7)%MP(1) > 0 .AND. KROUND == 2) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MXY(9))
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MXY(9))
                          ENDIF
                      ELSE
                          IF (MXY(7)%MP(1) < 0 .AND. KROUND == -1) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MXY(9))
                          ELSE IF (MXY(7)%MP(1) > 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MXY(9))
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MXY(9))
                          ENDIF
                      ENDIF
                  ENDIF
                  CALL FMEQU(MXY(9),MB,NDIG,NDSAVE)
              ELSE
                  KROUND = KRSAVE
                  CALL FMEQU(MXY(4),MB,NDIG,NDSAVE)
              ENDIF
              K_RETURN_CODE = 1
          ELSE IF (MXY(4)%MP(2) == MEXPUN) THEN
              IF (KRSAVE == 2 .AND. MA%MP(1) == 1) THEN
                  CALL FMTINY(MXY(9))
                  CALL FMEQU(MXY(9),MB,NDIG,NDSAVE)
                  K_RETURN_CODE = 1
              ENDIF
              IF (KRSAVE == -1 .AND. MA%MP(1) == -1) THEN
                  CALL FMTINY(MXY(9))
                  MXY(9)%MP(1) = -1
                  CALL FMEQU(MXY(9),MB,NDIG,NDSAVE)
                  K_RETURN_CODE = 1
              ENDIF
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NAMEST(NCALL) = 'FMATANH'
                  CALL FMNTR(2,MA,MA,1,1)
                  CALL FMNTR(1,MB,MB,1,1)
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      IF (K_RETURN_CODE == 1) RETURN

      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMENTR('FMATANH  ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMATANH'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      NDIG = NDIG + NDIG/100
      CALL FMEQU(MA,MXY(3),NDSAVE,NDIG)
      MXY(3)%MP(1) = 1

!             Check for special cases.

      IF (MXY(3)%MP(2) >= 1) THEN
          CALL FMST2M('UNKNOWN',MXY(5))
          KFLAG = -4
          GO TO 160
      ELSE IF (MXY(3)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(5))
          GO TO 160
      ELSE IF (MXY(3)%MP(2) == MEXPUN) THEN
          CALL FMEQ(MXY(3),MXY(5))
          IF (MA%MP(1) < 0) MXY(5)%MP(1) = -1
          KFLAG = -6
          GO TO 160
      ENDIF

      NMETHD = 1
      CALL FMM2DP(MXY(3),X)
      IF (MXY(3)%MP(2) >= -NDIG) THEN
          IF (ABS(X) >= 1234.0/(NDIG*ALOGMT)**2) NMETHD = 2
      ENDIF

      IF (NMETHD == 2) GO TO 140

!             Method 1.  Reduce the argument and use the Taylor series.
!                        Atanh(x) = x + x^3 / 3 + x^5 / 5 + ...

      K2 = MAX(2,INT(0.67*(NDIG*ALOGMT)**0.3333 + 0.4))
      K2 = MAX(K2,3)
      IF (MXY(3)%MP(2) <= -NDIG/3) THEN
          K2 = 0
      ELSE
          IF (MXY(3)%MP(2)*DLOGMB < LOG(2.0D0**(-K2))) THEN
              K2 = 0
          ELSE
              K = K2 + 1
              DO J = 0, K
                 IF (X < 0.375D0/2.0D0**(J)) THEN
                     K2 = K2 - 1
                     IF (K2 <= 0) EXIT
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      CALL FMEQ(MXY(3),MXY(1))
      CALL FMI2M(1,MXY(4))
      DO J = 1, K2
         CALL FMSQR(MXY(1),MXY(2))
         CALL FMSUB_R2(MXY(4),MXY(2))
         CALL FMSQRT_R1(MXY(2))
         CALL FMSUB_R2(MXY(4),MXY(2))
         CALL FMDIV_R2(MXY(2),MXY(1))
      ENDDO

      J2 = INT(0.96*(NDIG*ALOGMT)**0.3333 - 1.7)
      J2 = MAX(1,MIN(J2,LJSUMS))

!             Split into J2 concurrent sums and reduce NDIG while computing each term in the sum
!             as the terms get smaller.

      NTERM = 1
      DO J = 1, J2
         IF (NTERM > 1) THEN
             CALL FMCSDIVI(MXY(1),NTERM,MJSUMS(J))
         ELSE
             CALL FMEQ(MXY(1),MJSUMS(J))
         ENDIF
         NTERM = NTERM + 2
      ENDDO
      NDSAV1 = NDIG
      CALL FMEQ(MXY(1),MXY(4))
      CALL FMSQR_R1(MXY(1))
      IF (MXY(1)%MP(2) < -NDIG) GO TO 130
      CALL FMIPWR(MXY(1),J2,MXY(2))

  120 CALL FMCSMPY_R1(MXY(4),MXY(2))
      DO J = 1, J2
         CALL FMCSDIVI(MXY(4),NTERM,MXY(5))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(5))
         IF (KFLAG /= 0) GO TO 130
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(5)%MP(2))
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 2
      ENDDO
      GO TO 120

!             Put the J2 separate sums back together.

  130 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(5))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(5),MXY(1))
         CALL FMADD_R1(MXY(5),MJSUMS(J2-J+1))
      ENDDO

!             Reverse the argument reduction.

      KTWO = 1
      MAXV = MXBASE/2
      DO J = 1, K2
         KTWO = 2*KTWO
         IF (KTWO > MAXV) THEN
             CALL FMCSMPYI_R1(MXY(5),KTWO)
             KTWO = 1
         ENDIF
      ENDDO
      IF (KTWO > 1) CALL FMCSMPYI_R1(MXY(5),KTWO)

      GO TO 150

!             Method 2.  Atanh(x) =  ln( (1+x) / (1-x) ) / 2

  140 IEXTRA = -MXY(3)%MP(2)
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(3),NDIG,NDIG+IEXTRA)
          NDIG = NDIG + IEXTRA
      ENDIF
      CALL FMI2M(1,MXY(1))
      CALL FMADD(MXY(1),MXY(3),MXY(4))
      CALL FMSUB(MXY(1),MXY(3),MXY(5))
      CALL FMDIV(MXY(4),MXY(5),MXY(2))
      CALL FMLN(MXY(2),MXY(5))
      CALL FMDIVI_R1(MXY(5),2)

  150 IF (MXY(5)%MP(2) /= MUNKNO .AND. MXY(5)%MP(3) /= 0 .AND. MAS < 0)  &
          MXY(5)%MP(1) = -MXY(5)%MP(1)

      IF (KFLAG == 1) KFLAG = 0
      KWARN = KWRNSV

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(5)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
  160 CALL FMEXIT(MXY(5),MB,NDSAVE,MXSAVE,KOVUN)
      IF (KFLAG == 1) KFLAG = 0
      RETURN
      END SUBROUTINE FMATANH

      SUBROUTINE FMATN2(MA,MB,MC)

!  MC = ATAN2(MA,MB)

!  MC is returned as the angle between -pi and pi (or -180 and 180 if degree mode is selected) for
!  which TAN(MC) = MA/MB.  MC is an angle for the point (MB,MA) in polar coordinates.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MXEXP1,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JQUAD,K,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NDSAVE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(5)

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MB%MP(1) >= 0 .AND. KRAD == 1) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KL = KROUND
          KROUND = 1
          NCALL = NCALL + 1
          CALL FMDIV(MA,MB,MXY(1))
          NCALL = NCALL - 1
          KROUND = KL
          IF (MXY(1)%MP(2) < -NDIG) THEN
              NTRACE = J
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMATN2'
                  CALL FMNTR(2,MA,MB,2,1)
                  NCALL = NCALL - 1
              ENDIF
              NTRACE = 0
              IF (MXY(1)%MP(2) == MEXPUN) THEN
                  CALL FMEQ(MXY(1),MXY(3))
              ELSE
                  CALL FMSQR(MXY(1),MXY(2))
                  CALL FMMPY_R1(MXY(2),MXY(1))
                  CALL FMDIVI_R1(MXY(2),3)
                  IF (MXY(2)%MP(2) /= MEXPUN) THEN
                      CALL FMSUB(MXY(1),MXY(2),MXY(3))
                  ELSE IF (MXY(1)%MP(1) < 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
                      KL = MXY(1)%MP(2)
                      MXY(1)%MP(2) = 0
                      CALL FMULP(MXY(1),MXY(2))
                      CALL FMSUB(MXY(1),MXY(2),MXY(4))
                      MXY(4)%MP(2) = KL + MXY(4)%MP(2)
                      CALL FMEQ(MXY(4),MXY(3))
                  ELSE IF (MXY(1)%MP(1) >= 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
                      KL = MXY(1)%MP(2)
                      MXY(1)%MP(2) = 0
                      CALL FMULP(MXY(1),MXY(2))
                      CALL FMSUB(MXY(1),MXY(2),MXY(4))
                      MXY(4)%MP(2) = KL + MXY(4)%MP(2)
                      CALL FMEQ(MXY(4),MXY(3))
                  ELSE
                      CALL FMEQ(MXY(1),MXY(3))
                  ENDIF
              ENDIF
              KFLAG = 0
              NTRACE = J
              KWARN = K
              CALL FMEQ(MXY(3),MC)
              IF (MC%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO .AND.  &
                  MB%MP(2) /= MUNKNO) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMATN2'
                  KFLAG = -4
                  CALL FMWARN
                  NCALL = NCALL - 1
              ELSE IF (ABS(MC%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV .AND.  &
                       ABS(MB%MP(2))  < MEXPOV) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMATN2'
                  IF (MC%MP(2) == MEXPOV) KFLAG = -5
                  IF (MC%MP(2) == MEXPUN) KFLAG = -6
                  CALL FMWARN
                  NCALL = NCALL - 1
              ENDIF
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMATN2'
                  CALL FMNTR(1,MC,MC,1,1)
                  NCALL = NCALL - 1
              ENDIF
              RETURN
          ENDIF
          NTRACE = J
          KWARN = K
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB) THEN
          CALL FMENTR('FMATN2   ',MA,MB,2,1,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMATN2'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

      KWRNSV = KWARN
      KWARN = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)

!             Check for special cases.

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO .OR.  &
         (MA%MP(3) == 0 .AND. MB%MP(3) == 0)) THEN
          CALL FMST2M('UNKNOWN',MXY(5))
          KFLAG = -4
          GO TO 120
      ENDIF
      IF (KROUND /= 1 .AND. KRAD /= 1) THEN
          IF (MA%MP(2) == MEXPUN) THEN
              CALL FMTINY(MXY(3))
          ELSE IF (MA%MP(2) == MEXPOV) THEN
              CALL FMBIG(MXY(3))
          ELSE
              CALL FMEQ(MXY(1),MXY(3))
          ENDIF
          MXY(3)%MP(1) = MA%MP(1)
          IF (MB%MP(2) == MEXPUN) THEN
              CALL FMTINY(MXY(4))
          ELSE IF (MB%MP(2) == MEXPOV) THEN
              CALL FMBIG(MXY(4))
          ELSE
              CALL FMEQ(MXY(2),MXY(4))
          ENDIF
          MXY(4)%MP(1) = MB%MP(1)
          IF (MXY(3)%MP(2) > MXY(4)%MP(2)+NDIG) THEN
              IF (MA%MP(1) > 0) THEN
                  CALL FMDIV(MXY(4),MXY(3),MXY(5))
                  CALL FMMPYI_R1(MXY(5),180)
                  CALL FMPI(MXY(3))
                  CALL FMDIV(MXY(5),MXY(3),MXY(4))
                  CALL FMI2M(90,MXY(3))
                  CALL FMSUB(MXY(3),MXY(4),MXY(5))
                  GO TO 120
              ENDIF
              IF (MA%MP(1) < 0) THEN
                  CALL FMDIV(MXY(4),MXY(3),MXY(5))
                  CALL FMMPYI_R1(MXY(5),180)
                  CALL FMPI(MXY(3))
                  CALL FMDIV(MXY(5),MXY(3),MXY(4))
                  CALL FMI2M(-90,MXY(3))
                  CALL FMSUB(MXY(3),MXY(4),MXY(5))
                  GO TO 120
              ENDIF
          ENDIF
          IF (MXY(4)%MP(2) > MXY(3)%MP(2)+NDIG .AND. MB%MP(1) < 0) THEN
              IF (MA%MP(1) > 0) THEN
                  CALL FMDIV(MXY(3),MXY(4),MXY(5))
                  CALL FMMPYI_R1(MXY(5),180)
                  CALL FMPI(MXY(3))
                  CALL FMDIV(MXY(5),MXY(3),MXY(4))
                  CALL FMI2M(180,MXY(3))
                  CALL FMADD(MXY(3),MXY(4),MXY(5))
                  GO TO 120
              ENDIF
              IF (MA%MP(1) < 0) THEN
                  CALL FMDIV(MXY(3),MXY(4),MXY(5))
                  CALL FMMPYI_R1(MXY(5),180)
                  CALL FMPI(MXY(3))
                  CALL FMDIV(MXY(5),MXY(3),MXY(4))
                  CALL FMI2M(-180,MXY(3))
                  CALL FMADD(MXY(3),MXY(4),MXY(5))
                  GO TO 120
              ENDIF
          ENDIF
      ENDIF

      IF (MB%MP(3) == 0 .AND. MA%MP(1) > 0) THEN
          IF (KRAD == 0) THEN
              CALL FMI2M(90,MXY(5))
          ELSE
              CALL FMPI(MXY(5))
              CALL FMDIVI_R1(MXY(5),2)
          ENDIF
          GO TO 120
      ENDIF

      IF (MB%MP(3) == 0 .AND. MA%MP(1) < 0) THEN
          IF (KRAD == 0) THEN
              CALL FMI2M(-90,MXY(5))
          ELSE
              CALL FMPI(MXY(5))
              CALL FMDIVI_R1(MXY(5),-2)
          ENDIF
          GO TO 120
      ENDIF

      MXEXP1 = INT(MXEXP2/2.01D0)
      IF (MA%MP(2) == MEXPOV .AND. MB%MP(2) < MXEXP1-NDIG-2) THEN
          IF (KRAD == 0) THEN
              CALL FMI2M(90,MXY(5))
          ELSE
              CALL FMPI(MXY(5))
              CALL FMDIVI_R1(MXY(5),2)
          ENDIF
          IF (MXY(1)%MP(1) < 0) MXY(5)%MP(1) = -1
          GO TO 120
      ENDIF

      IF (MA%MP(2) == MEXPUN .AND. (-MB%MP(2)) < MXEXP1-NDIG-2 .AND.  &
                                 MB%MP(1) < 0) THEN
          IF (KRAD == 0) THEN
              CALL FMI2M(180,MXY(5))
          ELSE
              CALL FMPI(MXY(5))
          ENDIF
          IF (MXY(1)%MP(1) < 0) MXY(5)%MP(1) = -1
          GO TO 120
      ENDIF

      IF (MB%MP(2) == MEXPOV .AND. MA%MP(2) < MXEXP1-NDIG-2 .AND.  &
                                MB%MP(1) < 0) THEN
          IF (KRAD == 0) THEN
              CALL FMI2M(180,MXY(5))
          ELSE
              CALL FMPI(MXY(5))
          ENDIF
          IF (MXY(1)%MP(1) < 0) MXY(5)%MP(1) = -1
          GO TO 120
      ENDIF

      IF (MB%MP(2) == MEXPUN .AND. MA%MP(3) == 0) THEN
          IF (MB%MP(1) < 0) THEN
              IF (KRAD == 0) THEN
                  CALL FMI2M(180,MXY(5))
              ELSE
                  CALL FMPI(MXY(5))
              ENDIF
          ELSE
              CALL FMI2M(0,MXY(5))
          ENDIF
          GO TO 120
      ENDIF

      IF (MB%MP(2) == MEXPUN .AND. (-MA%MP(2)) < MXEXP1-NDIG-2) THEN
          IF (KRAD == 0) THEN
              CALL FMI2M(90,MXY(5))
          ELSE
              CALL FMPI(MXY(5))
              CALL FMDIVI_R1(MXY(5),2)
          ENDIF
          IF (MXY(1)%MP(1) < 0) MXY(5)%MP(1) = -1
          GO TO 120
      ENDIF

!             Determine the quadrant for the result, then use FMATAN.

      IF (MA%MP(1) >= 0 .AND. MB%MP(1) > 0) JQUAD = 1
      IF (MA%MP(1) >= 0 .AND. MB%MP(1) < 0) JQUAD = 2
      IF (MA%MP(1)  < 0 .AND. MB%MP(1) < 0) JQUAD = 3
      IF (MA%MP(1)  < 0 .AND. MB%MP(1) > 0) JQUAD = 4

      CALL FMDIV(MXY(1),MXY(2),MXY(4))
      MXY(4)%MP(1) = 1
      CALL FMATAN(MXY(4),MXY(5))

      IF (JQUAD == 2 .OR. JQUAD == 3) THEN
          IF (KRAD == 0) THEN
              CALL FMI2M(180,MXY(3))
              CALL FMSUB_R2(MXY(3),MXY(5))
          ELSE
              CALL FMPI(MXY(3))
              CALL FMSUB_R2(MXY(3),MXY(5))
          ENDIF
      ENDIF

      IF ((JQUAD == 3 .OR. JQUAD == 4) .AND. MXY(5)%MP(2) /= MUNKNO .AND.  &
          MXY(5)%MP(3) /= 0) MXY(5)%MP(1) = -MXY(5)%MP(1)

!             Round the result and return.

  120 IF (KFLAG == 1) KFLAG = 0
      KWARN = KWRNSV

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(5)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(5),MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMATN2

      SUBROUTINE FMBIG(MA)

!     MA = The biggest representable FM number using the current base and precision.
!          The smallest positive number is then 1.0/MA.
!          In some rounding modes, 1.0/(1.0/MA) may then overflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      INTEGER :: J,N1

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMBIG'

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      N1 = NDIG + 1
      DO J = 2, N1
         MA%MP(J+1) = MBASE - 1
      ENDDO
      MA%MP(2) = MXEXP + 1
      MA%MP(1) = 1

      IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMBIG

      SUBROUTINE FMCAT(MA,NCAT)

!  NCAT is returned as the category of MA.  This is used by the various arithmetic routines to
!  handle special cases such as: 'number greater than 1' + 'underflowed result' is the first
!  argument, 'overflowed result' / 'overflowed result' is 'unknown'.

!  NCAT       range

!   1.         -OV                OV stands for overflowed results.
!   2.   (-OV   , -OVTH)             ( MA%MP(2) >= MAXEXP+2 )
!   3.   (-OVTH ,    -1)
!   4.         -1                 OVTH stands for a representable
!   5.   (-1    , -UNTH)               number near the overflow
!   6.   (-UNTH ,   -UN)               threshold.
!   7.         -UN                     ( MA%MP(2) >= MAXEXP-NDIG+1 )
!   8.          0
!   9.         +UN                UN stands for underflowed results.
!  10.   (+UN   , +UNTH)             ( MA%MP(2) <= -MAXEXP-1 )
!  11.   (+UNTH ,    +1)
!  12.         +1                 UNTH stands for a representable
!  13.   (+1    , +OVTH)               number near the underflow
!  14.   (+OVTH ,   +OV)               threshold.
!  15.         +OV                     ( MA%MP(2) <= -MAXEXP+NDIG-1 )
!  16.       UNKNOWN

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: NCAT

      REAL (KIND(1.0D0)) :: MA2,MXEXP1
      INTEGER :: J,NLAST
      INTENT (IN) :: MA
      INTENT (INOUT) :: NCAT

!             Check for special symbols.

      NCAT = 16
      IF (MA%MP(2) == MUNKNO) RETURN

      IF (MA%MP(2) == MEXPOV) THEN
          NCAT = 15
          IF (MA%MP(1) < 0) NCAT = 1
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPUN) THEN
          NCAT = 9
          IF (MA%MP(1) < 0) NCAT = 7
          RETURN
      ENDIF

      IF (MA%MP(3) == 0) THEN
          NCAT = 8
          RETURN
      ENDIF

!             Check for +1 or -1.

      MA2 = ABS(MA%MP(3))
      IF (MA%MP(2) == 1 .AND. MA2 == 1) THEN
          NLAST = NDIG + 1
          IF (NLAST >= 3) THEN
              DO J = 3, NLAST
                 IF (MA%MP(J+1) /= 0) GO TO 110
              ENDDO
          ENDIF
          NCAT = 12
          IF (MA%MP(1) < 0) NCAT = 4
          RETURN
      ENDIF

  110 MXEXP1 = INT(MXEXP)
      IF (MA%MP(2) >= MXEXP1-NDIG+2) THEN
          NCAT = 14
          IF (MA%MP(1) < 0) NCAT = 2
          RETURN
      ENDIF

      IF (MA%MP(2) >= 1) THEN
          NCAT = 13
          IF (MA%MP(1) < 0) NCAT = 3
          RETURN
      ENDIF

      IF (MA%MP(2) >= -MXEXP1+NDIG) THEN
          NCAT = 11
          IF (MA%MP(1) < 0) NCAT = 5
          RETURN
      ENDIF

      IF (MA%MP(2) >= -MXEXP1) THEN
          NCAT = 10
          IF (MA%MP(1) < 0) NCAT = 6
          RETURN
      ENDIF

      RETURN
      END SUBROUTINE FMCAT

      SUBROUTINE FMCHANGEBASE(MA,MB,NEW_MBASE,NEW_NDIG)

!  Change the internal representation of a number from one base to another.
!  MA is given with NDIG digits in base MBASE (the current precision and base).
!  MB is returned as the same number, approximated with NEW_NDIG digits in base NEW_MBASE.

!  Note NDIG and MBASE are unchanged after calling FMCHANGEBASE, but if MB is to be used
!  in further operations, NDIG and MBASE should be changed to the new values in the calling program.
!
!  This routine is primarily meant to be used by the input and output conversion routines when the
!  base being used is not a power of ten.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: NEW_MBASE,NEW_NDIG
      INTENT (IN) :: MA,NEW_MBASE,NEW_NDIG
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4),MBPOWERS(9)
      INTEGER :: IEXTRA,J,K,KL,KRSAVE,KR_RETRY,NDSAVE
      REAL (KIND(1.0D0)) :: MBSAVE
      DOUBLE PRECISION :: ERR

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMCHANGEB'

!             The change of base is done by summing this series in the new base:
!                 d(1)/b + d(2)/b**2 + ... + d(k)/b**k
!             where d(i) is the i-th digit in the old base, b.

      NDSAVE = NDIG
      MBSAVE = MBASE
      MBASE = NEW_MBASE
      CALL FMCONS
      NDIG = NEW_NDIG + NGRD52

!             If the exponent is large, raise the precision.

      IEXTRA = MAX(0,ABS(INT(LOG(MAX(1.0D0,DBLE(ABS(MA%MP(2)))))/LOG(DBLE(MBSAVE))))+1)
      NDIG = NDIG + IEXTRA

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KRSAVE = KROUND
      KR_RETRY = 0
  110 IF (KR_RETRY >= 1) THEN
          NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF

!             Initialize the array of powers of the base.

      CALL IMI2M2(1,MXY(3))
      DO K = 1, 9
         CALL IMMPYI2(MXY(3),INT(MBSAVE),MBPOWERS(K))
         CALL IMEQ(MBPOWERS(K),MXY(3))
      ENDDO

      CALL FMCHANGEBASE_TQ(MA,MBSAVE,0,NDSAVE-1,MXY(1),MXY(2),MBPOWERS)

      KROUND = 1
      CALL IMI2FM(MXY(1),MXY(3))
      CALL IMI2FM(MXY(2),MXY(4))
      CALL FMDIV2(MXY(3),MXY(4),MXY(2))

!             Put the exponent and sign on MB.

      K = MBSAVE
      CALL FMIM(K,MXY(3))
      K = MA%MP(2)
      IF (K /= 0) THEN
          CALL FMIPWR2(MXY(3),K,MXY(4))
          CALL FMMPY2(MXY(2),MXY(4),MXY(1))
          CALL FMEQ(MXY(1),MXY(2))
      ENDIF
      MXY(2)%MP(1) = MA%MP(1)
      IF (KRSAVE /= 1) THEN
          K = (NDIG + NEW_NDIG) / 2
          KROUND = 1
          CALL FMEQU(MXY(2),MB,NDIG,K)
          KROUND = KRSAVE
          CALL FMEQU_R1(MB,K,NEW_NDIG)
      ELSE
          KROUND = KRSAVE
          CALL FMEQU(MXY(2),MB,NDIG,NEW_NDIG)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NEW_NDIG,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(2)%MP(J+NEW_NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NEW_NDIG+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

      MBASE = MBSAVE
      CALL FMCONS
      NDIG = NDSAVE

      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMCHANGEBASE

      RECURSIVE SUBROUTINE FMCHANGEBASE_TQ(MA,MBSAVE,A,B,MT,MQ,MBPOWERS)

!  This routine does the binary splitting for computing a change of base.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MT,MQ
      INTEGER :: A,B
      REAL (KIND(1.0D0)) :: MBSAVE
      INTENT (IN) :: MA,A,B,MBSAVE
      INTENT (INOUT) :: MT,MQ
      TYPE(MULTI) :: MXY(4),MBPOWERS(9)
      INTEGER :: DIGIT,J,K,M,OLD_BASE,RESULT_SIZE
      REAL (KIND(0.0D0)) :: DA,DB,DM

      DA = A
      DB = B
      DM = MBSAVE
      OLD_BASE = MBSAVE
      RESULT_SIZE = ( (DB-DA+1)*LOG(DM) + 5 ) / DLOGMB + 8
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MT%MP)) THEN
          ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MT%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MT%MP)
          ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MQ%MP)) THEN
          ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MQ%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MQ%MP)
          ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (B-A < 9) THEN
          IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(1)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(1)%MP)
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(2)%MP)) THEN
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(2)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(2)%MP)
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          DIGIT = MA%MP(A+3)
          CALL IMI2M2(DIGIT,MT)
          DO J = A+1, B
             CALL IMMPYI2(MT,OLD_BASE,MXY(2))
             DIGIT = MA%MP(J+3)
             CALL IMI2M2(DIGIT,MXY(1))
             CALL IMADD2(MXY(2),MXY(1),MT)
          ENDDO

!             There may be thousands of calls, all with K = 5,6,7,8,9.
!             These powers are saved instead of re-computing them each time.

          K = B - A + 1
          CALL IMEQ(MBPOWERS(K),MQ)
          RETURN
      ENDIF

      M = A/2 + B/2 + MOD(A,2)*MOD(B,2)
      CALL FMCHANGEBASE_TQ(MA,MBSAVE,A,M-1,MXY(1),MXY(2),MBPOWERS)
      CALL FMCHANGEBASE_TQ(MA,MBSAVE,M,B,MXY(3),MXY(4),MBPOWERS)
      CALL IMMPY2(MXY(1),MXY(4),MQ)
      CALL IMADD2(MXY(3),MQ,MT)

      CALL IMMPY2(MXY(2),MXY(4),MQ)

      RETURN
      END SUBROUTINE FMCHANGEBASE_TQ

      SUBROUTINE FMCHSH(MA,MB,MC)

!  MB = COSH(MA),    MC = SINH(MA)

!  If both the hyperbolic sine and cosine are needed, this routine is faster than calling both
!  FMCOSH and FMSINH.

!  MB and MC must be distinct arrays.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MAS,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NCSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      TYPE(MULTI) :: MXY(5)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      MAS = MA%MP(1)
      KR_RETRY = 0

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCHSH'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMCOSH(MA,MB)
          CALL FMSINH(MA,MC)
          KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCHSH'
              CALL FMNTR(1,MB,MB,1,1)
              IF (ABS(NTRACE) >= 1 .AND. NCALL <= LVLTRC) THEN
                  IF (NTRACE < 0) THEN
                      CALL FMNTRJ(MC,NDIG)
                  ELSE
                      CALL FMPRNT(MC)
                  ENDIF
              ENDIF
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          NCSAVE = NCALL
          CALL FMENTR('FMCHSH   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (MA%MP(2) == MUNKNO) KOVUN = 2
          NCALL = NCSAVE + 1
          CALL FMEQU(MA,MXY(3),NDSAVE,NDIG)
          MXY(3)%MP(1) = 1
          CALL FMCOSH(MXY(3),MXY(4))
          CALL FMSINH(MXY(3),MXY(5))
          GO TO 120
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMCHSH'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(3),NDSAVE,NDIG)
      MXY(3)%MP(1) = 1

      K = 1
      IF (MXY(3)%MP(2) == 0 .AND. MXY(3)%MP(3) /= 0) THEN
          IF (MBASE/MXY(3)%MP(3) >= 100) K = 2
      ENDIF
      IF (MXY(3)%MP(2) >= 0 .AND. MXY(3)%MP(3) /= 0 .AND. K == 1) THEN
          CALL FMCOSH(MXY(3),MXY(4))
          IF (MXY(4)%MP(2) > NDIG) THEN
              CALL FMEQ(MXY(4),MXY(5))
              GO TO 120
          ENDIF
          CALL FMSQR(MXY(4),MXY(2))
          CALL FMI2M(-1,MXY(1))
          CALL FMADD_R1(MXY(2),MXY(1))
          CALL FMSQRT(MXY(2),MXY(5))
      ELSE
          CALL FMSINH(MXY(3),MXY(5))
          CALL FMSQR(MXY(5),MXY(2))
          CALL FMI2M(1,MXY(1))
          CALL FMADD_R1(MXY(2),MXY(1))
          CALL FMSQRT(MXY(2),MXY(4))
      ENDIF

!             Round and return.

  120 IF (MAS < 0 .AND. MXY(5)%MP(2) /= MUNKNO .AND. MXY(5)%MP(3) /= 0)  &
          MXY(5)%MP(1) = -MXY(5)%MP(1)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(4)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(5)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEQU(MXY(5),MC,NDIG,NDSAVE)
      IF (KOVUN == 2) THEN
          KWRNSV = KWARN
          KWARN = 0
      ENDIF
      CALL FMEXIT(MXY(4),MB,NDSAVE,MXSAVE,KOVUN)
      IF (KOVUN == 2) THEN
          KWARN = KWRNSV
      ENDIF
      IF (NTRACE /= 0) THEN
          IF (ABS(NTRACE) >= 1 .AND. NCALL+1 <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL FMNTRJ(MC,NDIG)
              ELSE
                  CALL FMPRNT(MC)
              ENDIF
          ENDIF
      ENDIF
      RETURN
      END SUBROUTINE FMCHSH

      FUNCTION FMCOMP(MA,LREL,MB)

!  Logical comparison of FM numbers MA and MB.

!  LREL is a CHARACTER description of the comparison to be done:
!  LREL = 'EQ' returns FMCOMP = .TRUE. if MA == MB
!       = 'NE', 'GE', 'GT', 'LE', 'LT' also work like a logical IF.
!       = '==', '/=', '<', '<=', '>', '>=' may be used.

!  For comparisons involving 'UNKNOWN' or two identical special symbols such as
!  +OVERFLOW,'EQ',+OVERFLOW, FMCOMP is returned FALSE and a KFLAG = -4 error condition is returned.

      USE ModLib_FMVALS
      IMPLICIT NONE

      LOGICAL :: FMCOMP
      CHARACTER(*) :: LREL
      CHARACTER(2) :: JREL
      TYPE(MULTI) :: MA,MB

      INTEGER :: J,JCOMP,NLAST
      INTENT (IN) :: MA,LREL,MB

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMCOMP'

      IF (NCALL <= LVLTRC .AND. ABS(NTRACE) >= 2) THEN
          WRITE (KW,"(' Input to FMCOMP')")

          IF (NTRACE > 0) THEN
              CALL FMPRNT(MA)
              IF (INDEX('=/<>',LREL(1:1)) > 0) THEN
                  WRITE (KW,"(8X,A)") LREL
              ELSE
                  WRITE (KW,"(7X,'.',A,'.')") LREL
              ENDIF
              CALL FMPRNT(MB)
          ELSE
              CALL FMNTRJ(MA,NDIG)
              IF (INDEX('=/<>',LREL(1:1)) > 0) THEN
                  WRITE (KW,"(8X,A)") LREL
              ELSE
                  WRITE (KW,"(7X,'.',A,'.')") LREL
              ENDIF
              CALL FMNTRJ(MB,NDIG)
          ENDIF
      ENDIF

!             JCOMP will be 1 if MA > MB
!                           2 if MA == MB
!                           3 if MA < MB

!             Check for special cases.

      JREL = LREL
      IF (LREL /= 'EQ' .AND. LREL /= 'NE' .AND. LREL /= 'LT' .AND.  &
          LREL /= 'GT' .AND. LREL /= 'LE' .AND. LREL /= 'GE') THEN
          IF (LREL == 'eq' .OR. LREL == '==') THEN
              JREL = 'EQ'
          ELSE IF (LREL == 'ne' .OR. LREL == '/=') THEN
              JREL = 'NE'
          ELSE IF (LREL == 'lt' .OR. LREL == '<') THEN
              JREL = 'LT'
          ELSE IF (LREL == 'gt' .OR. LREL == '>') THEN
              JREL = 'GT'
          ELSE IF (LREL == 'le' .OR. LREL == '<=') THEN
              JREL = 'LE'
          ELSE IF (LREL == 'ge' .OR. LREL == '>=') THEN
              JREL = 'GE'
          ELSE
              FMCOMP = .FALSE.
              KFLAG = -4
              IF (NCALL /= 1 .OR. KWARN <= 0) GO TO 120
              IF (KWARN <= 0) GO TO 120
              WRITE (KW,                                                     &
                     "(/' Error of type KFLAG = -4 in FM package in',"   //  &
                     "' routine FMCOMP'//1X,A,' is not one of the six'," //  &
                     "' recognized comparisons.'//' .FALSE. has been',"  //  &
                     "' returned.'/)"                                        &
                    ) LREL
              IF (KWARN >= 2) THEN
                  STOP
              ENDIF
              GO TO 120
          ENDIF
      ENDIF

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          FMCOMP = .FALSE.
          KFLAG = -4
          GO TO 120
      ENDIF

      IF (ABS(MA%MP(2)) == MEXPOV .AND. MA%MP(2) == MB%MP(2) .AND.  &
          MA%MP(3) == MB%MP(3) .AND. MA%MP(1) == MB%MP(1)) THEN
          FMCOMP = .FALSE.
          KFLAG = -4
          IF (NCALL /= 1 .OR. KWARN <= 0) GO TO 120
          IF (KWARN <= 0) GO TO 120
          WRITE (KW,                                                           &
                 "(/' Error of type KFLAG = -4 in FM package in routine'," //  &
                 "' FMCOMP'//' Two numbers in the same overflow or',"      //  &
                 "' underflow category cannot be compared.'//"             //  &
                 "' .FALSE. has been returned.'/)"                             &
                 )
          IF (KWARN >= 2) THEN
              STOP
          ENDIF
          GO TO 120
      ENDIF

!             Check for zero.

      KFLAG = 0
      IF (MA%MP(3) == 0) THEN
          JCOMP = 2
          IF (MB%MP(3) == 0) GO TO 110
          IF (MB%MP(1) < 0) JCOMP = 1
          IF (MB%MP(1) > 0) JCOMP = 3
          GO TO 110
      ENDIF
      IF (MB%MP(3) == 0) THEN
          JCOMP = 1
          IF (MA%MP(1) < 0) JCOMP = 3
          GO TO 110
      ENDIF

!             Check for opposite signs.

      IF (MA%MP(1) > 0 .AND. MB%MP(1) < 0) THEN
          JCOMP = 1
          GO TO 110
      ENDIF
      IF (MB%MP(1) > 0 .AND. MA%MP(1) < 0) THEN
          JCOMP = 3
          GO TO 110
      ENDIF

!             See which one is larger in absolute value.

      IF (MA%MP(2) > MB%MP(2)) THEN
          JCOMP = 1
          GO TO 110
      ENDIF
      IF (MB%MP(2) > MA%MP(2)) THEN
          JCOMP = 3
          GO TO 110
      ENDIF
      NLAST = NDIG + 1

      DO J = 2, NLAST
         IF (ABS(MA%MP(J+1)) > ABS(MB%MP(J+1))) THEN
             JCOMP = 1
             GO TO 110
         ENDIF
         IF (ABS(MB%MP(J+1)) > ABS(MA%MP(J+1))) THEN
             JCOMP = 3
             GO TO 110
         ENDIF
      ENDDO

      JCOMP = 2

!             Now match the JCOMP value to the requested comparison.

  110 IF (JCOMP == 1 .AND. MA%MP(1) < 0) THEN
          JCOMP = 3
      ELSE IF (JCOMP == 3 .AND. MB%MP(1) < 0) THEN
          JCOMP = 1
      ENDIF

      FMCOMP = .FALSE.
      IF (JCOMP == 1 .AND. (JREL == 'GT' .OR. JREL == 'GE' .OR. JREL == 'NE')) FMCOMP = .TRUE.
      IF (JCOMP == 2 .AND. (JREL == 'EQ' .OR. JREL == 'GE' .OR. JREL == 'LE')) FMCOMP = .TRUE.
      IF (JCOMP == 3 .AND. (JREL == 'NE' .OR. JREL == 'LT' .OR. JREL == 'LE')) FMCOMP = .TRUE.

  120 CONTINUE
      IF (NTRACE /= 0) THEN
          IF (NCALL <= LVLTRC .AND. ABS(NTRACE) >= 1) THEN
              IF (KFLAG == 0) THEN
                  WRITE (KW,                                                   &
                         "(' FMCOMP',15X,'Call level =',I2,5X,'MBASE =',"  //  &
                         "I10,5X,'NDIG =',I10)"                                &
                        ) NCALL,INT(MBASE),NDIG
              ELSE
                  WRITE (KW,                                                  &
                         "(' FMCOMP',6X,'Call level =',I2,4X,'MBASE =',"  //  &
                         "I10,4X,'NDIG =',I10,4X,'KFLAG =',I3)"               &
                        ) NCALL,INT(MBASE),NDIG,KFLAG
              ENDIF
              IF (FMCOMP) THEN
                  WRITE (KW,"(7X,'.TRUE.')")
              ELSE
                  WRITE (KW,"(7X,'.FALSE.')")
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END FUNCTION FMCOMP

      SUBROUTINE FMCONS

!  Set several saved machine precision constants.

      USE ModLib_FMVALS
      IMPLICIT NONE

      MBLOGS = MBASE
      ALOGMB = LOG(REAL(MBASE))
      ALOGM2 = ALOGMB/LOG(2.0)
      ALOGMX = LOG(REAL(MAXINT))
      ALOGMT = ALOGMB/LOG(10.0)
      NGRD21 = INT(2.0/ALOGMT + 1.0)
      NGRD52 = INT(5.0/ALOGMT + 2.0)
      NGRD22 = INT(2.0/ALOGMT + 2.0)
      IF (MBASE < 1000) THEN
          NGRD21 = 2*NGRD21
          NGRD52 = 4*NGRD52
          NGRD22 = 2*NGRD22
      ELSE
          NGRD21 = NGRD21 + 1
          NGRD52 = NGRD52 + 1
          NGRD22 = NGRD22 + 1
      ENDIF
      MEXPAB = AINT (MXEXP2/5)
      DLOGMB = LOG(DBLE(MBASE))
      DLOGTN = LOG(10.0D0)
      DLOGTW = LOG(2.0D0)
      DPPI = 4.0D0*ATAN(1.0D0)
      DLOGTP = LOG(2.0D0*DPPI)
      DLOGPI = LOG(DPPI)
      DLOGEB = -LOG(DPEPS)/DLOGMB

      RETURN
      END SUBROUTINE FMCONS

      SUBROUTINE FMCOS(MA,MB)

!  MB = COS(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JCOS,JSIN,JSWAP,K,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NDSAVE,NDSV
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(6)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCOS'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          IF (KRAD == 0) THEN
              CALL FMPI(MXY(2))
              CALL FMSQR(MXY(2),MXY(3))
              CALL FMDIVI_R1(MXY(3),32400)
              CALL FMMPY_R1(MXY(1),MXY(3))
          ENDIF
          CALL FMDIVI(MXY(1),-2,MXY(3))
          IF (MXY(3)%MP(2) > MEXPUN) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMADD(MXY(1),MXY(3),MB)
          ELSE IF (MA%MP(2) == MEXPUN) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMADD(MXY(1),MXY(3),MB)
          ELSE IF (KROUND == -1 .OR. KROUND == 0) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMDP2M(0.9D0,MXY(2))
              CALL FMULP(MXY(2),MXY(3))
              CALL FMSUB(MXY(1),MXY(3),MB)
          ELSE
              CALL FMI2M(1,MB)
          ENDIF
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCOS'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0) THEN
          CALL FMENTR('FMCOS    ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMCOS'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(6),NDSAVE,NDIG)
      MXY(6)%MP(1) = 1
      IF (MA%MP(2) > 3*10**5 .AND. KRAD == 1) THEN
          KFLAG = -4
          CALL FMST2M('UNKNOWN',MXY(6))
          GO TO 120
      ENDIF
      CALL FMEQ(MXY(6),MXY(5))
      KWRNSV = KWARN
      KWARN = 0

!             Reduce the argument, convert to radians if the input is in degrees, and evaluate
!             the function.

      CALL FMRDC(MXY(6),JSIN,JCOS,JSWAP)
      IF (KROUND /= 1 .AND. KRAD /= 1) THEN
          CALL FMI2M(30,MXY(1))
          CALL FMSUB(MXY(6),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0 .AND. JSWAP == 1) THEN
              CALL FMST2M('0.5',MXY(1))
              CALL FMMPYI(MXY(1),JCOS,MXY(6))
              GO TO 120
          ENDIF
      ENDIF
      KWARN = KWRNSV
      IF (MXY(6)%MP(2) == MUNKNO) THEN
          IF (KRAD /= 1 .OR. JSWAP == 1) THEN
              CALL FMEQ(MXY(5),MXY(6))
              CALL FMRDC(MXY(6),JSIN,JCOS,JSWAP)
              GO TO 120
          ENDIF
          IF (MBSPI /= MBASE .OR. NDIGPI < NDIG)  THEN
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMPI(MXY(3))
              NDIG = NDSV
          ENDIF
          CALL FMDIV(MXY(5),MPISAV,MXY(3))
          CALL FMNINT(MXY(3),MXY(2))
          CALL FMMPY(MXY(2),MPISAV,MXY(1))
          CALL FMSUB_R2(MXY(5),MXY(1))
          IF (MXY(1)%MP(3) == 0) CALL FMULP(MXY(5),MXY(1))
          CALL FMI2M(1,MXY(3))
          CALL FMSQR_R1(MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMSUB_R2(MXY(3),MXY(1))
          CALL FMSUB_R1(MXY(1),MXY(3))
          IF (MXY(1)%MP(3) == 0) THEN
              CALL FMI2M(JCOS,MXY(6))
          ELSE
              CALL FMEQ(MXY(5),MXY(6))
              CALL FMRDC(MXY(6),JSIN,JCOS,JSWAP)
          ENDIF
          GO TO 120
      ENDIF
      IF (KRAD == 0) THEN
          IF (MBSPI /= MBASE .OR. NDIGPI < NDIG)  THEN
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMPI(MXY(4))
              NDIG = NDSV
          ENDIF
          CALL FMMPY_R1(MXY(6),MPISAV)
          CALL FMDIVI_R1(MXY(6),180)
      ENDIF
      IF (MXY(6)%MP(2) /= MUNKNO) THEN
          IF (JSWAP == 0) THEN
              CALL FMCOS2(MXY(6),MXY(4))
              CALL FMEQ(MXY(4),MXY(6))
          ELSE
              IF (MXY(6)%MP(2) < 0 .OR. NDIG <= 50) THEN
                  CALL FMSIN2(MXY(6),MXY(4))
                  CALL FMEQ(MXY(4),MXY(6))
              ELSE
                  CALL FMCOS2(MXY(6),MXY(4))
                  CALL FMI2M(1,MXY(2))
                  CALL FMSQR(MXY(4),MXY(6))
                  CALL FMSUB_R2(MXY(2),MXY(6))
                  CALL FMSQRT_R1(MXY(6))
              ENDIF
          ENDIF
      ENDIF

!             Append the sign, round, and return.

      IF (MXY(6)%MP(2) /= MUNKNO .AND. MXY(6)%MP(3) /= 0 .AND. JCOS == -1)  &
          MXY(6)%MP(1) = -MXY(6)%MP(1)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(6)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(6),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMCOS

      SUBROUTINE FMCOS2(MA,MB)

!  Internal subroutine for MB = COS(MA) where 0 <= MA <= 1.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAXV
      INTEGER :: J,J2,K2,KTWO,KWRNSV,L,L2,LARGE,N2,NBOT,NDSAV1,NDSAVE,NTERM
      REAL :: ALOG2,ALOGT,B,T,TJ
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (MA%MP(3) == 0) THEN
          CALL FMI2M(1,MB)
          RETURN
      ENDIF
      NDSAVE = NDIG
      KWRNSV = KWARN
      KWARN = 0

!             Use the direct series:  COS(X) = 1 - X**2/2! + X**4/4! - ...

!             The argument will be divided by 2**K2 before the series is summed.  The series will be
!             added as J2 concurrent series.

!             Since X is small when the series is summed, COS(X) - 1 is computed.  Then a version of
!             the recovery formula can be used that does not suffer from severe cancellation.

      B = REAL(MBASE)
      T = MAX(NDIG-NGRD52,2)
      ALOG2 = LOG(2.0)
      ALOGT = LOG(T)
      TJ = 0.69*(NDIG*ALOGMT)**0.3333
      J2 = INT(TJ)
      J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      IF (NDIG > 2000) THEN
          J2 = -4.0 + 3.0*ALOGT
      ENDIF
      K2 = MAX(2,INT(1.25*(NDIG*ALOGMT)**0.3333 - 3.4))


      TJ = -(REAL(MA%MP(2))*ALOGMB+LOG(REAL(MA%MP(3))/B +  &
             REAL(MA%MP(4))/(B*B)))/ALOG2 - 0.3
      IF (TJ >= K2) THEN
          L = K2
      ELSE IF (TJ > 0) THEN
          L = INT(TJ)
      ELSE
          L = 0
      ENDIF
      K2 = K2 - L
      IF (NDIG > 2000) THEN
          K2 = 5.8*ALOGMB - 263 + (35 - 0.58*ALOGMB)*ALOGT
      ENDIF
      IF (K2 <= 0) THEN
          K2 = 0
          J2 = INT(.43*SQRT(T*ALOGMB/(ALOGT+REAL(L)*ALOG2)) + .33)
          J2 = MAX(1,MIN(J2,LJSUMS))
      ENDIF

      N2 = INT(T*ALOGMB/(ALOGT+REAL(L)*ALOG2))
      L2 = INT(LOG(REAL(N2)+2.0D0**K2)/ALOGMB)
      NDIG = NDIG + MAX(L2,0)
      NDSAV1 = NDIG
      J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))

!             Divide the argument by 2**K2.

      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      KTWO = 1
      MAXV = MXBASE/2
      IF (K2 > 0) THEN
          DO J = 1, K2
             KTWO = 2*KTWO
             IF (KTWO > MAXV) THEN
                 CALL FMCSDIVI_R1(MXY(1),KTWO)
                 KTWO = 1
             ENDIF
          ENDDO
          IF (KTWO > 1) CALL FMCSDIVI_R1(MXY(1),KTWO)
      ENDIF

!             Split into J2 concurrent sums and reduce NDIG while computing each term in the sum as
!             the terms get smaller.

      NTERM = 2
      CALL FMI2M(1,MXY(2))
      DO J = 1, J2
         NBOT = NTERM*(NTERM-1)
         CALL FMCSDIVI_R1(MXY(2),NBOT)
         CALL FMEQ(MXY(2),MJSUMS(J))
         NTERM = NTERM + 2
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 120
      CALL FMSQR_R1(MXY(1))
      CALL FMIPWR(MXY(1),J2,MXY(3))
      IF (MXY(3)%MP(2) < -NDIG) GO TO 120

  110 CALL FMCSMPY_R1(MXY(2),MXY(3))
      DO J = 1, J2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(2),NTERM)
             NBOT = NTERM - 1
             CALL FMCSDIVI_R1(MXY(2),NBOT)
         ELSE
             NBOT = NTERM*(NTERM-1)
             CALL FMCSDIVI_R1(MXY(2),NBOT)
         ENDIF
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(2))
         IF (KFLAG /= 0) GO TO 120
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(2)%MP(2))
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 2
      ENDDO
      GO TO 110

!             Put the J2 separate sums back together.

  120 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(3))
      IF (MXY(1)%MP(2) /= MUNKNO .AND. MXY(1)%MP(3) /= 0)  &
          MXY(1)%MP(1) = -MXY(1)%MP(1)
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(3),MXY(1))
         CALL FMADD_R1(MXY(3),MJSUMS(J2-J+1))
      ENDDO
      CALL FMCSMPY_R1(MXY(3),MXY(1))

!             Reverse the effect of reducing the argument to compute COS(MA).

      NDIG = NDSAV1
      IF (K2 > 0) THEN
          IF (NDSAVE <= 20) THEN
              CALL FMI2M(2,MXY(1))
              DO J = 1, K2
                 CALL FMADD(MXY(3),MXY(1),MXY(2))
                 CALL FMCSMPY_R1(MXY(2),MXY(3))
                 CALL FMADD(MXY(2),MXY(2),MXY(3))
              ENDDO
          ELSE
              DO J = 1, K2
                 CALL FMSQR(MXY(3),MXY(2))
                 CALL FMADD(MXY(3),MXY(3),MXY(1))
                 CALL FMADD_R1(MXY(2),MXY(1))
                 CALL FMADD(MXY(2),MXY(2),MXY(3))
              ENDDO
          ENDIF
      ENDIF
      CALL FMI2M(1,MXY(2))
      CALL FMADD_R2(MXY(2),MXY(3))

      CALL FMEQU(MXY(3),MB,NDSAV1,NDSAVE)
      NDIG = NDSAVE
      KWARN = KWRNSV

      RETURN
      END SUBROUTINE FMCOS2

      SUBROUTINE FMCOSH(MA,MB)

!  MB = COSH(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE,NMETHD
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCOSH'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMDIVI(MXY(1),2,MXY(2))
          IF (MXY(2)%MP(2) > MEXPUN) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMADD(MXY(1),MXY(2),MB)
          ELSE IF (MA%MP(2) == MEXPUN) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMADD(MXY(1),MXY(2),MB)
          ELSE IF (KROUND == 2) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMULP(MXY(1),MXY(2))
              CALL FMADD(MXY(1),MXY(2),MB)
          ELSE
              CALL FMI2M(1,MB)
          ENDIF
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCOSH'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMENTR('FMCOSH   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMCOSH'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
      MXY(2)%MP(1) = 1
      IF (MA%MP(3) == 0) THEN
          CALL FMI2M(1,MXY(2))
          GO TO 120
      ENDIF

!             Use a series for small arguments, FMEXP for large ones.

      IF (MXY(2)%MP(2) == MUNKNO) GO TO 120
      IF (MBASE > 99) THEN
          IF (MXY(2)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE IF (MXY(2)%MP(2) >= 2) THEN
              NMETHD = 2
          ELSE IF (ABS(MXY(2)%MP(3)) < 10) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          IF (MXY(2)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ENDIF

      IF (NMETHD == 1) THEN
          CALL FMCSH2(MXY(2),MXY(1))
          CALL FMEQ(MXY(1),MXY(2))
      ELSE
          CALL FMEXP(MXY(2),MXY(1))
          CALL FMEQ(MXY(1),MXY(2))
          IF (MXY(2)%MP(2) == MEXPOV) THEN
              GO TO 120
          ENDIF
          IF (INT(MXY(2)%MP(2)) <= (NDIG+1)/2) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMDIV_R1(MXY(1),MXY(2))
              CALL FMADD_R1(MXY(2),MXY(1))
          ENDIF
          CALL FMDIVI_R1(MXY(2),2)
      ENDIF

!             Round and return.

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(2),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMCOSH

      SUBROUTINE FMCSADD_R1(MA,MB)

!  Internal addition routine.  MA = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: J,K,KP2,N1
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      IF (NTRACE /= 0 .OR. ABS(MA%MP(2)) >= MEXPAB .OR.           &
          ABS(MB%MP(2)) >= MEXPAB .OR. MA%MP(2) < MB%MP(2) .OR.   &
          MA%MP(1) < 0 .OR. MB%MP(1) < 0 .OR. MA%MP(3) == 0 .OR.  &
          MB%MP(3) == 0) THEN
          CALL FMADD_R1(MA,MB)
          RETURN
      ENDIF
      KFLAG = 0
      N1 = NDIG + 1
      K = MA%MP(2) - MB%MP(2)

!             Add MA and MB.

      KP2 = K + 2
      DO J = KP2+1, N1+1
         MA%MP(J) = MA%MP(J) + MB%MP(J-K)
      ENDDO

!             Normalize.  Fix any digit not less than MBASE.

      IF (MA%MP(3) >= MBASE) THEN
          MA%MP(2) = MA%MP(2) + 1
          IF (MA%MP(N1+1) >= MBASE) MA%MP(NDIG+1) = MA%MP(NDIG+1) + 1
          DO J = NDIG+1, 4, -1
             IF (MA%MP(J) >= MBASE) THEN
                 MA%MP(J+1) = MA%MP(J) - MBASE
                 MA%MP(J-1) = MA%MP(J-1) + 1
             ELSE
                 MA%MP(J+1) = MA%MP(J)
             ENDIF
          ENDDO
          MA%MP(4) = MA%MP(3) - MBASE
          MA%MP(3) = 1
      ELSE
          DO J = N1+1, 4, -1
             IF (MA%MP(J) >= MBASE) THEN
                 MA%MP(J) = MA%MP(J) - MBASE
                 MA%MP(J-1) = MA%MP(J-1) + 1
             ENDIF
          ENDDO
          IF (MA%MP(3) >= MBASE) THEN
              DO J = N1+1, 5, -1
                 MA%MP(J) = MA%MP(J-1)
              ENDDO
              MA%MP(4) = MA%MP(3) - MBASE
              MA%MP(3) = 1
              MA%MP(2) = MA%MP(2) + 1
          ENDIF
      ENDIF

      IF (ABS(MA%MP(2)-MB%MP(2)) >= NDIG) KFLAG = 1

      RETURN
      END SUBROUTINE FMCSADD_R1

      SUBROUTINE FMCSADDNN_R1(MA,MB)

!  Internal addition routine.  MA = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: J,K,KP2,N1
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      IF (NTRACE /= 0 .OR. ABS(MA%MP(2)) >= MEXPAB .OR. ABS(MB%MP(2)) >= MEXPAB  &
          .OR. MA%MP(2) < MB%MP(2) .OR. MA%MP(3) == 0                            &
          .OR. MB%MP(3) == 0) THEN
          CALL FMCSNORM(MA)
          CALL FMADD_R1(MA,MB)
          RETURN
      ENDIF
      KFLAG = 0
      N1 = NDIG + 1
      K = MA%MP(2) - MB%MP(2)

!             Add MA and MB.

      KP2 = K + 2
      DO J = KP2+1, N1+1
         MA%MP(J) = MA%MP(J) + MB%MP(J-K)
      ENDDO

!             See if the result is equal to one of the input arguments.

      IF (ABS(MA%MP(2)-MB%MP(2)) >= NDIG) KFLAG = 1

      RETURN
      END SUBROUTINE FMCSADDNN_R1

      SUBROUTINE FMCSDIV(MA,MB,MC)

!  Internal division routine.  MC = MA/MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      DOUBLE PRECISION :: XB,XBR,XBASE,XMWA
      REAL (KIND(1.0D0)) :: MAXMWA,MBP1,MCARRY,MKT,MLMAX,MQD
      REAL :: C
      INTEGER :: J,JB,JL,KA,KB,KL,KPTMWA,N1,NG,NL,NMBWDS,NZDMB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      C = 3100
      IF (NTRACE /= 0 .OR. ABS(MA%MP(2)) >= MEXPAB  .OR. ABS(MB%MP(2)) >= MEXPAB  &
          .OR. MA%MP(3) == 0 .OR. MB%MP(3) == 0 .OR. MBASE < 1000                 &
          .OR. NDIG >= C) THEN
          CALL FMDIV(MA,MB,MC)
          RETURN
      ENDIF
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KFLAG = 0
      N1 = NDIG + 1
      NG = NDIG + NGRD21
      NL = NG + 4
      MWA%MP(2) = MA%MP(2) - MB%MP(2) + 1
      MWA%MP(3) = 0
      DO J = 2, N1
         MWA%MP(J+2) = MA%MP(J+1)
      ENDDO
      NL = N1 + NGRD21 + 3
      DO J = NDIG+3, NL
         MWA%MP(J+1) = 0
      ENDDO

!             NMBWDS is the number of words of MB used to compute the estimated quotient digit MQD.

      NMBWDS = 4
      IF (MBASE < 100) NMBWDS = 7

!             XB is an approximation of MB used in estimating the quotient digits.

      XBASE = DBLE(MBASE)
      XB = 0
      JL = NMBWDS
      IF (JL <= N1) THEN
          DO J = 2, JL
             XB = XB*XBASE + DBLE(MB%MP(J+1))
          ENDDO
      ELSE
          DO J = 2, JL
             IF (J <= N1) THEN
                 XB = XB*XBASE + DBLE(MB%MP(J+1))
             ELSE
                 XB = XB*XBASE
             ENDIF
          ENDDO
      ENDIF
      IF (JL+1 <= N1) THEN
          XB = XB + DBLE(MB%MP(JL+2))/XBASE
      ENDIF
      XBR = 1.0D0/XB

!             MLMAX determines when to normalize all of MWA.

      MBP1 = MBASE + 1
      MLMAX = MAXINT/MBP1
      MKT = INTMAX - MBASE
      MLMAX = MIN(MLMAX,MKT)

!             Count the trailing zero digits of MB.

      DO J = N1, 2, -1
         IF (MB%MP(J+1) /= 0) THEN
             NZDMB = N1 - J
             GO TO 110
         ENDIF
      ENDDO

!             MAXMWA is an upper bound on the size of values in MWA divided by MBASE-1.  It is used
!             to determine whether normalization can be postponed.

  110 MAXMWA = 0

!             KPTMWA points to the next digit in the quotient.

      KPTMWA = 2

!             This is the start of the division loop.

!             XMWA is an approximation of the active part of MWA used in estimating quotient digits.

  120 KL = KPTMWA + NMBWDS - 1
      IF (KL <= NL) THEN
          XMWA = ((DBLE(MWA%MP(KPTMWA+1))*XBASE + DBLE(MWA%MP(KPTMWA+2)))*XBASE  &
                 + DBLE(MWA%MP(KPTMWA+3)))*XBASE + DBLE(MWA%MP(KPTMWA+4))
          DO J = KPTMWA+4, KL
             XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
          ENDDO
      ELSE
          XMWA = DBLE(MWA%MP(KPTMWA+1))
          DO J = KPTMWA+1, KL
             IF (J <= NL) THEN
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
             ELSE
                 XMWA = XMWA*XBASE
             ENDIF
          ENDDO
      ENDIF

!             MQD is the estimated quotient digit.

      MQD = AINT(XMWA*XBR)
      IF (MQD < 0) MQD = MQD - 1

      IF (MQD > 0) THEN
          MAXMWA = MAXMWA + MQD
      ELSE
          MAXMWA = MAXMWA - MQD
      ENDIF

!             See if MWA must be normalized.

      KA = KPTMWA + 1
      KB = MIN(KA+NDIG-1-NZDMB,NL)
      IF (MAXMWA >= MLMAX) THEN
          DO J = KB, KA, -1
             IF (MWA%MP(J+1) < 0) THEN
                 MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ELSE IF (MWA%MP(J+1) >= MBASE) THEN
                 MCARRY = -INT(MWA%MP(J+1)/MBASE)
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ENDIF
          ENDDO
          XMWA = 0
          IF (KL <= NL) THEN
              DO J = KPTMWA, KL
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
              ENDDO
          ELSE
              DO J = KPTMWA, KL
                 IF (J <= NL) THEN
                     XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
                 ELSE
                     XMWA = XMWA*XBASE
                 ENDIF
              ENDDO
          ENDIF
          MQD = AINT(XMWA*XBR)
          IF (MQD < 0) MQD = MQD - 1
          IF (MQD > 0) THEN
              MAXMWA = MQD
          ELSE
              MAXMWA = -MQD
          ENDIF
      ENDIF

!             Subtract MQD*MB from MWA.

      JB = KA - 2
      IF (MQD /= 0) THEN

!             Major (Inner Loop)

          DO J = KA+1, KB+1
             MWA%MP(J) = MWA%MP(J) - MQD*MB%MP(J-JB)
          ENDDO
      ENDIF

      MWA%MP(KA+1) = MWA%MP(KA+1) + MWA%MP(KA)*MBASE
      MWA%MP(KPTMWA+1) = MQD

      KPTMWA = KPTMWA + 1
      IF (KPTMWA <= NG) GO TO 120
      IF (MWA%MP(3) == 0 .AND. KPTMWA <= NG+1) GO TO 120

      KL = KPTMWA + NMBWDS - 1
      IF (KL <= NL) THEN
          XMWA = ((DBLE(MWA%MP(KPTMWA+1))*XBASE + DBLE(MWA%MP(KPTMWA+2)))*XBASE  &
                 + DBLE(MWA%MP(KPTMWA+3)))*XBASE + DBLE(MWA%MP(KPTMWA+4))
          DO J = KPTMWA+4, KL
             XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
          ENDDO
      ELSE
          XMWA = DBLE(MWA%MP(KPTMWA+1))
          DO J = KPTMWA+1, KL
             IF (J <= NL) THEN
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
             ELSE
                 XMWA = XMWA*XBASE
             ENDIF
          ENDDO
      ENDIF
      MQD = AINT(XMWA*XBR)
      IF (MQD < 0) MQD = MQD - 1
      MWA%MP(KPTMWA+1) = MQD
      MWA%MP(KPTMWA+2) = 0
      MWA%MP(KPTMWA+3) = 0

!             Final normalization.

      IF (KPTMWA > 2*NDIG) THEN
          DO J = 2*NDIG+1, KPTMWA
             IF (MWA%MP(J+1) /= MBASE-1) EXIT
             IF (J == KPTMWA) MWA%MP(J+1) = MBASE
          ENDDO
      ENDIF
      DO J = KPTMWA, 3, -1
         IF (MWA%MP(J+1) < 0) THEN
             MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ELSE IF (MWA%MP(J+1) >= MBASE) THEN
             MCARRY = -INT(MWA%MP(J+1)/MBASE)
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ENDIF
      ENDDO
      CALL FMMOVE(MWA,MC)
      MC%MP(1) = MA%MP(1) * MB%MP(1)

      RETURN
      END SUBROUTINE FMCSDIV

      SUBROUTINE FMCSDIVI(MA,IVAL,MB)

!  Internal divide by integer routine.  MA = MA / IVAL

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MKT,MODINT,MVALP
      INTEGER :: J,KA,KB,KL,KPT,KPTWA,N1,NMVAL,NV2
      INTENT (INOUT) :: MB
      INTENT (IN) :: MA,IVAL

      MVALP = ABS(IVAL)
      NMVAL = INT(MVALP)
      NV2 = NMVAL - 1
      IF (NTRACE /= 0 .OR. ABS(MA%MP(2)) >= MEXPAB .OR. MA%MP(3) == 0 .OR.  &
          IVAL == 0 .OR. ABS(IVAL) > MXBASE .OR. NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL FMDIVI(MA,IVAL,MB)
          RETURN
      ENDIF
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (ABS(IVAL) == 1) THEN
          CALL FMEQ(MA,MB)
          MB%MP(1) = MA%MP(1)*IVAL
          RETURN
      ENDIF

!             Find the first significant digit of the quotient.

      KFLAG = 0
      N1 = NDIG + 1
      MVALP = ABS(IVAL)
      MKT = MA%MP(3)
      IF (MKT >= MVALP) THEN
          KPT = 2
          GO TO 120
      ENDIF
      DO J = 3, N1
         MKT = MKT*MBASE + MA%MP(J+1)
         IF (MKT >= MVALP) THEN
             KPT = J
             GO TO 120
         ENDIF
      ENDDO
      KPT = N1

  110 KPT = KPT + 1
      MKT = MKT*MBASE
      IF (MKT < MVALP) GO TO 110

!             Do the rest of the division.

  120 KA = KPT + 1
      MB%MP(1) = MA%MP(1)*IVAL/ABS(IVAL)
      MB%MP(2) = MA%MP(2) + 2 - KPT
      MB%MP(3) = INT (MKT/MVALP)
      MODINT = MKT - MB%MP(3)*MVALP
      KPTWA = 2
      IF (KA <= N1) THEN
          KL = 3 - KA

!             (Inner Loop)

          DO J = KA+1, N1+1
             MKT = MODINT*MBASE + MA%MP(J)
             MB%MP(J+KL) = INT (MKT/MVALP)
             MODINT = MKT - MB%MP(J+KL)*MVALP
          ENDDO
          KPTWA = KL + N1
      ENDIF

      KA = KPTWA + 1
      KB = N1
      DO J = KA, KB
         MKT = MODINT*MBASE
         MB%MP(J+1) = INT (MKT/MVALP)
         MODINT = MKT - MB%MP(J+1)*MVALP
      ENDDO

      RETURN
      END SUBROUTINE FMCSDIVI

      SUBROUTINE FMCSDIVI_R1(MA,IVAL)

!  Internal divide by integer routine.  MA = MA / IVAL

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MKT,MODINT,MVALP
      INTEGER :: J,KA,KB,KL,KPT,KPTWA,N1,NMVAL,NV2
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL

      MVALP = ABS(IVAL)
      NMVAL = INT(MVALP)
      NV2 = NMVAL - 1
      IF (NTRACE /= 0 .OR. ABS(MA%MP(2)) >= MEXPAB .OR. MA%MP(3) == 0 .OR.  &
          IVAL == 0 .OR. ABS(IVAL) > MXBASE .OR. NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL FMDIVI_R1(MA,IVAL)
          RETURN
      ENDIF
      IF (ABS(IVAL) == 1) THEN
          MA%MP(1) = MA%MP(1)*IVAL
          RETURN
      ENDIF

!             Find the first significant digit of the quotient.

      KFLAG = 0
      N1 = NDIG + 1
      MVALP = ABS(IVAL)
      MKT = MA%MP(3)
      IF (MKT >= MVALP) THEN
          KPT = 2
          GO TO 120
      ENDIF
      DO J = 3, N1
         MKT = MKT*MBASE + MA%MP(J+1)
         IF (MKT >= MVALP) THEN
             KPT = J
             GO TO 120
         ENDIF
      ENDDO
      KPT = N1

  110 KPT = KPT + 1
      MKT = MKT*MBASE
      IF (MKT < MVALP) GO TO 110

!             Do the rest of the division.

  120 KA = KPT + 1
      MA%MP(2) = MA%MP(2) + 2 - KPT
      MA%MP(3) = INT (MKT/MVALP)
      MODINT = MKT - MA%MP(3)*MVALP
      KPTWA = 2
      IF (KA <= N1) THEN
          KL = 3 - KA

!             (Inner Loop)

          DO J = KA+1, N1+1
             MKT = MODINT*MBASE + MA%MP(J)
             MA%MP(J+KL) = INT (MKT/MVALP)
             MODINT = MKT - MA%MP(J+KL)*MVALP
          ENDDO
          KPTWA = KL + N1
      ENDIF

      KA = KPTWA + 1
      KB = N1
      DO J = KA, KB
         MKT = MODINT*MBASE
         MA%MP(J+1) = INT (MKT/MVALP)
         MODINT = MKT - MA%MP(J+1)*MVALP
      ENDDO
      IF (IVAL < 0) MA%MP(1) = -MA%MP(1)

      RETURN
      END SUBROUTINE FMCSDIVI_R1

      SUBROUTINE FMCSH2(MA,MB)

!  Internal subroutine for MB = COSH(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAXV
      INTEGER :: J,J2,K,K2,KEXP,KTWO,KWRNSV,L,L2,LARGE,N2,NBOT,NDSAV1,NDSAVE,NTERM
      REAL :: ALOG2,ALOGT,B,T,TJ
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (MA%MP(3) == 0) THEN
          CALL FMI2M(1,MB)
          RETURN
      ENDIF
      NDSAVE = NDIG
      KWRNSV = KWARN
      KWARN = 0

!             Use the direct series:  COSH(X) = 1 + X**2/2! + X**4/4! - ...

!             The argument will be divided by 2**K2 before the series is summed.  The series will be
!             added as J2 concurrent series.

!             Since X is small when the series is summed, COSH(X) - 1 is computed.  Then a version
!             of the recovery formula can be used that does not suffer from severe cancellation.

      B = REAL(MBASE)
      K = NGRD52
      T = MAX(NDIG-K,2)
      ALOG2 = LOG(2.0)
      ALOGT = LOG(T)
      TJ = 0.65*(NDIG*ALOGMT)**0.3333 - 2.0
      J2 = INT(TJ)
      J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      K2 = INT(1.38*(NDIG*ALOGMT)**0.3333 - 1.8)

      TJ = -(REAL(MA%MP(2))*ALOGMB+LOG(REAL(MA%MP(3))/B +  &
             REAL(MA%MP(4))/(B*B)))/ALOG2 - 0.3
      IF (TJ >= K2) THEN
          L = K2
      ELSE IF (TJ > 0) THEN
          L = INT(TJ)
      ELSE
          L = 0
      ENDIF
      K2 = K2 - L
      IF (K2 <= 0) THEN
          K2 = 0
          J2 = INT(.43*SQRT(T*ALOGMB/(ALOGT+REAL(L)*ALOG2)) + .33)
          J2 = MAX(1,MIN(J2,LJSUMS))
      ENDIF
      IF (J2 <= 1) J2 = 1

      N2 = INT(T*ALOGMB/(ALOGT+REAL(L)*ALOG2))
      L2 = INT(LOG(REAL(N2)+2.0D0**K2)/ALOGMB)
      NDIG = NDIG + L2
      NDSAV1 = NDIG
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

!             Divide the argument by 2**K2.

      KTWO = 1
      MAXV = MXBASE/2
      IF (K2 > 0) THEN
          DO J = 1, K2
             KTWO = 2*KTWO
             IF (KTWO > MAXV) THEN
                 CALL FMCSDIVI_R1(MXY(1),KTWO)
                 KTWO = 1
             ENDIF
          ENDDO
          IF (KTWO > 1) CALL FMCSDIVI_R1(MXY(1),KTWO)
      ENDIF

!             Split into J2 concurrent sums and reduce NDIG while computing each term in the sum as
!             the terms get smaller.

      CALL FMSQR_R1(MXY(1))
      CALL FMEQ(MXY(1),MXY(2))
      NTERM = 2
      DO J = 1, J2
         NBOT = NTERM*(NTERM-1)
         CALL FMCSDIVI_R1(MXY(2),NBOT)
         NTERM = NTERM + 2
         CALL FMEQ(MXY(2),MJSUMS(J))
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 120
      CALL FMIPWR(MXY(1),J2,MXY(3))

  110 CALL FMCSMPY_R1(MXY(2),MXY(3))
      DO J = 1, J2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(2),NTERM)
             NBOT = NTERM - 1
             CALL FMCSDIVI_R1(MXY(2),NBOT)
         ELSE
             NBOT = NTERM*(NTERM-1)
             CALL FMCSDIVI_R1(MXY(2),NBOT)
         ENDIF
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(2))
         IF (KFLAG /= 0) GO TO 120
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(2)%MP(2))
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 2
      ENDDO
      GO TO 110

!             Put the J2 separate sums back together.

  120 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(3))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(3),MXY(1))
         CALL FMADD_R1(MXY(3),MJSUMS(J2-J+1))
      ENDDO

!             Reverse the effect of reducing the argument to compute COSH(MA).

      NDIG = NDSAV1
      IF (K2 > 0) THEN
          IF (NDSAVE <= 20) THEN
              CALL FMI2M(2,MXY(1))
              DO J = 1, K2
                 KEXP = MXY(3)%MP(2)
                 IF (MBASE == 2 .OR. KEXP > 0) THEN
                     CALL FMADD(MXY(3),MXY(1),MXY(2))
                 ELSE
                     DO K = 1, 3-KEXP
                        MXY(2)%MP(K) = MXY(1)%MP(K)
                     ENDDO
                     DO K = 4-KEXP, NDIG+2
                        MXY(2)%MP(K) = MXY(3)%MP(K-1+KEXP)
                     ENDDO
                 ENDIF
                 CALL FMCSMPY_R1(MXY(2),MXY(3))
                 CALL FMCSMPYI(MXY(2),2,MXY(3))
              ENDDO
          ELSE
              DO J = 1, K2
                 CALL FMSQR(MXY(3),MXY(2))
                 CALL FMADD(MXY(3),MXY(3),MXY(1))
                 CALL FMADD_R1(MXY(2),MXY(1))
                 CALL FMADD(MXY(2),MXY(2),MXY(3))
              ENDDO
          ENDIF
      ENDIF
      CALL FMI2M(1,MXY(2))
      CALL FMADD_R2(MXY(2),MXY(3))

      CALL FMEQU(MXY(3),MB,NDSAV1,NDSAVE)
      NDIG = NDSAVE
      KWARN = KWRNSV

      RETURN
      END SUBROUTINE FMCSH2

      SUBROUTINE FMCSMPY_R1(MA,MB)

!  Internal multiplication of MA*MB.  The result is returned in MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAXMWA,MBJ,MBKJ,MBNORM,MBP1,MK,MKT,MMAX,MT
      REAL :: C
      INTEGER :: J,JM1,K,KB,KI,KJ,KL,KNZ,KWA,L,N1,NGUARD
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      C = 900
      IF (NTRACE /= 0 .OR. ABS(MA%MP(2)) >= MEXPAB  .OR. ABS(MB%MP(2)) >= MEXPAB  &
          .OR. MA%MP(3) == 0 .OR. MB%MP(3) == 0 .OR. MBASE < 1000                 &
          .OR. NDIG >= C) THEN
          CALL FMMPY_R1(MA,MB)
          RETURN
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KFLAG = 0
      N1 = NDIG + 1
      NGUARD = NGRD22
      MWA%MP(2) = MA%MP(2) + MB%MP(2)
      L = N1 + NGUARD
      MWA%MP(L+2) = 0

!             The multiplication loop begins here.

!             MBNORM is the minimum number of digits that can be multiplied before normalization
!                    is required.
!             MAXMWA is an upper bound on the size of values in MWA divided by (MBASE-1).  It is
!                    used to determine whether to normalize before the next digit is multiplied.

      MBP1 = MBASE + 1
      MBNORM = (MAXINT/(MBP1*MBP1))
      MMAX = INTMAX - MBASE
      MMAX = MIN((MAXINT/MBP1 - MBP1),MMAX)
      IF (MBNORM >= 2) THEN
          MBJ = MB%MP(3)

!             Count the trailing zeros in MA.

          IF (MA%MP(N1+1) /= 0) THEN
              KNZ = N1
          ELSE
              DO J = NDIG, 2, -1
                 IF (MA%MP(J+1) /= 0) THEN
                     KNZ = J
                     GO TO 110
                 ENDIF
              ENDDO
          ENDIF

  110     MWA%MP(3) = 0
          DO K = NDIG+2, L
             MWA%MP(K+1) = 0
          ENDDO

!             (Inner Loop)

          DO K = 3, N1+1
             MWA%MP(K+1) = MA%MP(K)*MBJ
          ENDDO
          MAXMWA = MBJ
          DO J = 3, N1
             MBJ = MB%MP(J+1)
             IF (MBJ /= 0) THEN
                 MAXMWA = MAXMWA + MBJ
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)
                 DO K = J+2, J+KL
                    MWA%MP(K) = MWA%MP(K) + MA%MP(K-JM1)*MBJ
                 ENDDO
             ENDIF

             IF (MAXMWA > MMAX) THEN
                 MAXMWA = 0
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)

!                       Here normalization is only required for the range of digits currently
!                       changing in MWA.

                 DO KB = JM1+KL, JM1+2, -1
                    MKT = INT (MWA%MP(KB+1)/MBASE)
                    MWA%MP(KB) = MWA%MP(KB) + MKT
                    MWA%MP(KB+1) = MWA%MP(KB+1) - MKT*MBASE
                 ENDDO
             ENDIF
          ENDDO

!             Perform the final normalization.  (Inner Loop)

          DO KB = L+1, 4, -1
             MKT = INT (MWA%MP(KB)/MBASE)
             MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
             MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
          ENDDO

      ELSE

!             If normalization must be done for each digit, combine the two loops and normalize as
!             the digits are multiplied.

          DO J = 2, L
             MWA%MP(J+1) = 0
          ENDDO
          KJ = NDIG + 2
          DO J = 2, N1
             KJ = KJ - 1
             MBKJ = MB%MP(KJ+1)
             IF (MBKJ == 0) CYCLE
             KL = L - KJ + 1
             IF (KL > N1) KL = N1
             KI = KL + 2
             KWA = KL+ KJ + 1
             MK = 0
             DO K = 2, KL
                MT = MA%MP(KI-K+1)*MBKJ + MWA%MP(KWA-K+1) + MK
                MK = INT (MT/MBASE)
                MWA%MP(KWA-K+1) = MT - MBASE*MK
             ENDDO
             MWA%MP(KWA-KL) = MK
          ENDDO
      ENDIF

      IF (MA%MP(1)*MB%MP(1) < 0) THEN
          MA%MP(1) = -1
      ELSE
          MA%MP(1) = 1
      ENDIF
      MA%MP(2) = MWA%MP(2)
      IF (MWA%MP(3) == 0) THEN
          MA%MP(2) = MA%MP(2) - 1
          DO J = 3, N1+1
             MA%MP(J) = MWA%MP(J+1)
          ENDDO
      ELSE
          DO J = 3, N1+1
             MA%MP(J) = MWA%MP(J)
          ENDDO
      ENDIF

      RETURN
      END SUBROUTINE FMCSMPY_R1

      SUBROUTINE FMCSMPYI(MA,IVAL,MB)

!  Internal multiply by integer routine.  MB = MA * IVAL

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MAS,MCARRY,MKT,MVAL
      INTEGER :: J,KSHIFT,NMVAL,NV2
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB

      IF (NTRACE /= 0 .OR. ABS(MA%MP(2)) >= MEXPAB .OR. MA%MP(3) == 0 .OR.  &
          ABS(IVAL) <= 1 .OR. ABS(IVAL) > MXBASE) THEN
          CALL FMMPYI(MA,IVAL,MB)
          RETURN
      ENDIF
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

!             Work with positive numbers.

      KFLAG = 0
      MAS = MA%MP(1)
      MVAL = ABS(IVAL)
      NMVAL = INT(MVAL)
      NV2 = NMVAL - 1

!             To leave room for the normalization, shift the product to the right KSHIFT
!             places in MB.

      KSHIFT = 0
      MKT = MA%MP(3) * MVAL
      DO J = 1, 100
         IF (MKT < MBASE) EXIT
         KSHIFT = KSHIFT + 1
         MKT = INT(MKT/MBASE)
      ENDDO

      IF (KSHIFT > NDIG .OR. NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL FMMPYI(MA,IVAL,MB)
          RETURN
      ENDIF
      MB%MP(2) = MA%MP(2) + KSHIFT

      MCARRY = 0
      DO J = NDIG, NDIG-KSHIFT+1, -1
         MKT = MA%MP(J+2)*MVAL + MCARRY
         MCARRY = INT(MKT/MBASE)
      ENDDO
      DO J = NDIG-KSHIFT, 1, -1
         MKT = MA%MP(J+2)*MVAL + MCARRY
         MCARRY = INT(MKT/MBASE)
         MB%MP(J+2+KSHIFT) = MKT - MCARRY*MBASE
      ENDDO
      DO J = KSHIFT, 1, -1
         MKT = MCARRY
         MCARRY = INT(MKT/MBASE)
         MB%MP(J+2) = MKT - MCARRY*MBASE
      ENDDO
      IF (MCARRY > 0) THEN
          MB%MP(2) = MB%MP(2) + 1
          DO J = NDIG, 2, -1
             MB%MP(J+2) = MB%MP(J+1)
          ENDDO
          MB%MP(3) = MCARRY
      ENDIF

      IF ((MAS > 0 .AND. IVAL > 0) .OR. (MAS < 0 .AND. IVAL < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      MB%MP(1) = JRSIGN

      RETURN
      END SUBROUTINE FMCSMPYI

      SUBROUTINE FMCSMPYI_R1(MA,IVAL)

!  Internal multiply by integer routine.  MA = MA * IVAL

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MAS,MCARRY,MKT,MVAL
      INTEGER :: J,KSHIFT,NMVAL,NV2
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA

      IF (NTRACE /= 0 .OR. ABS(MA%MP(2)) >= MEXPAB .OR. MA%MP(3) == 0 .OR.  &
          ABS(IVAL) <= 1 .OR. ABS(IVAL) > MXBASE) THEN
          CALL FMMPYI_R1(MA,IVAL)
          RETURN
      ENDIF

!             Work with positive numbers.

      KFLAG = 0
      MAS = MA%MP(1)
      MVAL = ABS(IVAL)
      NMVAL = INT(MVAL)
      NV2 = NMVAL - 1

!             To leave room for the normalization, shift the product to the right KSHIFT
!             places.

      KSHIFT = 0
      MKT = MA%MP(3) * MVAL
      DO J = 1, 100
         IF (MKT < MBASE) EXIT
         KSHIFT = KSHIFT + 1
         MKT = INT(MKT/MBASE)
      ENDDO

      IF (KSHIFT > NDIG .OR. NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL FMMPYI_R1(MA,IVAL)
          RETURN
      ENDIF
      MA%MP(2) = MA%MP(2) + KSHIFT

      MCARRY = 0
      DO J = NDIG, NDIG-KSHIFT+1, -1
         MKT = MA%MP(J+2)*MVAL + MCARRY
         MCARRY = INT(MKT/MBASE)
      ENDDO
      DO J = NDIG-KSHIFT, 1, -1
         MKT = MA%MP(J+2)*MVAL + MCARRY
         MCARRY = INT(MKT/MBASE)
         MA%MP(J+2+KSHIFT) = MKT - MCARRY*MBASE
      ENDDO
      DO J = KSHIFT, 1, -1
         MKT = MCARRY
         MCARRY = INT(MKT/MBASE)
         MA%MP(J+2) = MKT - MCARRY*MBASE
      ENDDO
      IF (MCARRY > 0) THEN
          MA%MP(2) = MA%MP(2) + 1
          DO J = NDIG, 2, -1
             MA%MP(J+2) = MA%MP(J+1)
          ENDDO
          MA%MP(3) = MCARRY
      ENDIF

      IF ((MAS > 0 .AND. IVAL > 0) .OR. (MAS < 0 .AND. IVAL < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      MA%MP(1) = JRSIGN

      RETURN
      END SUBROUTINE FMCSMPYI_R1

      SUBROUTINE FMCSMPYIN_R1(MA,JB,JE)

!  Internal routine for  MA = MA * JB**JE

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: JB,JE,J,KJ
      REAL (KIND(1.0D0)) :: MAXV
      INTENT (IN) :: JB,JE
      INTENT (INOUT) :: MA

      KJ = 1
      MAXV = MXBASE/JB
      IF (JE > 0) THEN
          DO J = 1, JE
             KJ = JB*KJ
             IF (KJ > MAXV) THEN
                 CALL FMCSMPYI_R1(MA,KJ)
                 KJ = 1
             ENDIF
          ENDDO
          IF (KJ > 1) CALL FMCSMPYI_R1(MA,KJ)
      ENDIF

      END SUBROUTINE FMCSMPYIN_R1

      SUBROUTINE FMCSNORM(MA)

!  Internal routine to normalize the digits of ma.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: K,KSHIFT
      REAL (KIND(1.0D0)) :: MKT
      INTENT (INOUT) :: MA

      DO K = NDIG+2, 4, -1
         MKT = INT (MA%MP(K)/MBASE)
         MA%MP(K-1) = MA%MP(K-1) + MKT
         MA%MP(K) = MA%MP(K) - MKT*MBASE
      ENDDO
      IF (MA%MP(3) >= MBASE) THEN
          MKT = MA%MP(3)
          KSHIFT = 1
          DO
             MKT = INT (MKT/MBASE)
             IF (MKT < MBASE) EXIT
             KSHIFT = KSHIFT + 1
          ENDDO
          DO K = NDIG+2, 3+KSHIFT, -1
             MA%MP(K) = MA%MP(K-KSHIFT)
          ENDDO
          DO K = 3+KSHIFT, 4, -1
             MKT = INT (MA%MP(K)/MBASE)
             MA%MP(K-1) = MKT
             MA%MP(K) = MA%MP(K) - MKT*MBASE
          ENDDO
          MA%MP(2) = MA%MP(2) + KSHIFT
      ENDIF

      END SUBROUTINE FMCSNORM

      SUBROUTINE FMCSNSUMS(J2,MJSUMS)

!  Internal routine to normalize mjsums.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: J2
      TYPE(MULTI) :: MJSUMS(J2)
      INTEGER :: J
      INTENT (IN) :: J2
      INTENT (INOUT) :: MJSUMS

      DO J = 1, J2
         CALL FMCSNORM(MJSUMS(J))
      ENDDO

      RETURN
      END SUBROUTINE FMCSNSUMS

      SUBROUTINE FMCSSN(MA,MB,MC)

!  MB = COS(MA),    MC = SIN(MA)

!  If both the sine and cosine are needed, this routine is faster than calling both FMCOS and FMSIN.

!  MB and MC must be distinct arrays.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MAS,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JCOS,JSIN,JSWAP,K,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NCSAVE,NDSAVE,NDSV
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      TYPE(MULTI) :: MXY(7)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      MAS = MA%MP(1)
      KR_RETRY = 0

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCSSN'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMCOS(MA,MB)
          CALL FMSIN(MA,MC)
          KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCSSN'
              CALL FMNTR(1,MB,MB,1,1)
              IF (ABS(NTRACE) >= 1 .AND. NCALL <= LVLTRC) THEN
                  IF (NTRACE < 0) THEN
                      CALL FMNTRJ(MC,NDIG)
                  ELSE
                      CALL FMPRNT(MC)
                  ENDIF
              ENDIF
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0) THEN
          NCSAVE = NCALL
          CALL FMENTR('FMCSSN   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (MA%MP(2) == MUNKNO) KOVUN = 2
          NCALL = NCSAVE + 1
          CALL FMEQU(MA,MXY(3),NDSAVE,NDIG)
          MXY(3)%MP(1) = 1
          IF (MA%MP(2) == MEXPOV) THEN
              KFLAG = -4
              CALL FMST2M('UNKNOWN',MXY(5))
              CALL FMST2M('UNKNOWN',MXY(6))
          ELSE
              CALL FMCOS(MXY(3),MXY(5))
              CALL FMSIN(MXY(3),MXY(6))
          ENDIF
          GO TO 120
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMCSSN'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      IF (MA%MP(2) > 3*10**5 .AND. KRAD == 1) THEN
          KFLAG = -4
          CALL FMWARN
          CALL FMST2M('UNKNOWN',MXY(5))
          CALL FMST2M('UNKNOWN',MXY(6))
          GO TO 120
      ENDIF

      CALL FMEQU(MA,MXY(5),NDSAVE,NDIG)
      MXY(5)%MP(1) = 1
      CALL FMEQ(MXY(5),MXY(7))

!             Reduce the argument, convert to radians if the input is in degrees, and evaluate
!             the functions.

      CALL FMRDC(MXY(5),JSIN,JCOS,JSWAP)
      IF (KROUND /= 1 .AND. KRAD /= 1) THEN
          CALL FMI2M(30,MXY(1))
          CALL FMSUB(MXY(5),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0) THEN
              CALL FMCOS(MXY(7),MXY(5))
              CALL FMSIN(MXY(7),MXY(6))
              GO TO 120
          ENDIF
      ENDIF
      IF (MXY(5)%MP(2) == MUNKNO) THEN
          CALL FMCOS(MXY(7),MXY(5))
          CALL FMSIN(MXY(7),MXY(6))
          GO TO 120
      ENDIF
      IF (KRAD == 0) THEN
          IF (MBSPI /= MBASE .OR. NDIGPI < NDIG)  THEN
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMPI(MXY(6))
              NDIG = NDSV
          ENDIF
          CALL FMMPY_R1(MXY(5),MPISAV)
          CALL FMDIVI_R1(MXY(5),180)
      ENDIF
      IF (MXY(5)%MP(2) /= MUNKNO) THEN
          IF (JSWAP == 0) THEN
              IF (MXY(5)%MP(2) < 0) THEN
                  CALL FMSIN2(MXY(5),MXY(6))
                  MXY(6)%MP(1) = JSIN*MXY(6)%MP(1)
                  CALL FMSQR(MXY(6),MXY(2))
                  CALL FMI2M(1,MXY(1))
                  CALL FMSUB_R2(MXY(1),MXY(2))
                  CALL FMSQRT(MXY(2),MXY(5))
                  MXY(5)%MP(1) = JCOS*MXY(5)%MP(1)
              ELSE
                  CALL FMCOS2(MXY(5),MXY(4))
                  CALL FMEQ(MXY(4),MXY(5))
                  MXY(5)%MP(1) = JCOS*MXY(5)%MP(1)
                  CALL FMSQR(MXY(5),MXY(2))
                  CALL FMI2M(1,MXY(1))
                  CALL FMSUB_R2(MXY(1),MXY(2))
                  CALL FMSQRT(MXY(2),MXY(6))
                  MXY(6)%MP(1) = JSIN*MXY(6)%MP(1)
              ENDIF
          ELSE
              IF (MXY(5)%MP(2) < 0) THEN
                  CALL FMSIN2(MXY(5),MXY(4))
                  CALL FMEQ(MXY(4),MXY(5))
                  MXY(5)%MP(1) = JCOS*MXY(5)%MP(1)
                  CALL FMSQR(MXY(5),MXY(2))
                  CALL FMI2M(1,MXY(1))
                  CALL FMSUB_R2(MXY(1),MXY(2))
                  CALL FMSQRT(MXY(2),MXY(6))
                  MXY(6)%MP(1) = JSIN*MXY(6)%MP(1)
              ELSE
                  CALL FMCOS2(MXY(5),MXY(6))
                  MXY(6)%MP(1) = JSIN*MXY(6)%MP(1)
                  CALL FMSQR(MXY(6),MXY(2))
                  CALL FMI2M(1,MXY(1))
                  CALL FMSUB_R2(MXY(1),MXY(2))
                  CALL FMSQRT(MXY(2),MXY(5))
                  MXY(5)%MP(1) = JCOS*MXY(5)%MP(1)
              ENDIF
          ENDIF
      ELSE
          CALL FMEQ(MXY(5),MXY(6))
      ENDIF

!             Round and return.

  120 IF (MAS < 0 .AND. MXY(6)%MP(2) /= MUNKNO .AND. MXY(6)%MP(3) /= 0)  &
          MXY(6)%MP(1) = -MXY(6)%MP(1)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1 .AND. MA%MP(2) /= MEXPOV .AND. MXY(5)%MP(2) /= MUNKNO .AND.  &
          MXY(6)%MP(2) /= MUNKNO) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(6)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1 .AND. MA%MP(2) /= MEXPOV .AND. MXY(5)%MP(2) /= MUNKNO .AND.  &
          MXY(6)%MP(2) /= MUNKNO) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(5)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (KOVUN == 2) THEN
          KWRNSV = KWARN
          KWARN = 0
      ENDIF
      NDSV = NDIG
      CALL FMEXIT(MXY(5),MB,NDSAVE,MXSAVE,KOVUN)
      CALL FMEQU(MXY(6),MC,NDSV,NDSAVE)
      IF (KOVUN == 2) THEN
          KWARN = KWRNSV
      ENDIF
      IF (NTRACE /= 0) THEN
          IF (ABS(NTRACE) >= 1 .AND. NCALL+1 <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL FMNTRJ(MC,NDIG)
              ELSE
                  CALL FMPRNT(MC)
              ENDIF
          ENDIF
      ENDIF
      RETURN
      END SUBROUTINE FMCSSN

      SUBROUTINE FMDBL(A,B,C)

!  C = A + B.  All are double precision.  This routine tries to force the compiler to round C to
!  double precision accuracy. Some compilers allow double precision loops like the one in FMDM to
!  be done in extended precision, which defeats the routine's attempt to determine double precision
!  accuracy.  This can lead to doing too few Newton steps and failing to get sufficient accuracy in
!  several FM routines.

      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION :: A,B,C
      INTENT (IN) :: A,B
      INTENT (INOUT) :: C
      C = A + B
      RETURN
      END SUBROUTINE FMDBL

      SUBROUTINE FMDEFINE_ERROR

      USE ModLib_FMVALS
      IMPLICIT NONE

      WRITE (KW,*) ' '
      WRITE (KW,*) ' Error in FM.  Out of memory for multiple precision numbers'
      WRITE (KW,*) '               or character strings to format FM output.'
      WRITE (KW,*) '               Allocation of more memory failed.'
      WRITE (KW,*) ' '

      STOP
      END SUBROUTINE FMDEFINE_ERROR

      SUBROUTINE FMDIG(NSTACK,KST)

!  Compute the number of intermediate digits to be used in Newton iteration.  This assumes that a
!  starting approximation that is accurate to double precision is used, and the root is simple.

!  KST is the number of iterations needed for final accuracy NDIG.
!  NSTACK(J) holds the value of NDIG to be used for the Jth iteration.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NSTACK(49),KST
      INTENT (INOUT) :: NSTACK,KST

      DOUBLE PRECISION :: Y
      INTEGER :: J,JT,L,ND,NDT,NE

      IF (MBLOGS /= MBASE) CALL FMCONS

!             NE is the maximum number of base MBASE digits that can be used in the first
!                Newton iteration.

      NE = INT(1.9D0*DLOGEB)

!             Fill the intermediate digit stack (backwards).

      KST = 1
      ND = NDIG
      NSTACK(1) = ND
      IF (ND < NE .OR. ND <= 2) RETURN

  110 Y = ND

!             The 1.9 accounts for the fact that the number of correct digits approximately doubles
!             at each iteration.

      NDT = INT(Y/1.9D0)
      IF (2*NDT <= ND) NDT = NDT + 1
      ND = NDT
      KST = KST + 1
      NSTACK(KST) = ND
      IF (ND > NE .AND. ND > 2) GO TO 110

!             Reverse the stack.

      L = KST/2
      DO J = 1, L
         JT = NSTACK(J)
         NSTACK(J) = NSTACK(KST+1-J)
         NSTACK(KST+1-J) = JT
      ENDDO

      RETURN
      END SUBROUTINE FMDIG

      SUBROUTINE FMDIM(MA,MB,MC)

!  MC = DIM(MA,MB)

!  Positive difference.  MC = MA - MB  if MA >= MB,
!                           = 0        otherwise.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: KOVUN,KRESLT,KWRNSV,NDSAVE
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(3)

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB) THEN
          CALL FMENTR('FMDIM    ',MA,MB,2,1,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
          NDIG = NDSAVE
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMDIM'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          IF (MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KWRNSV = KWARN
      KWARN = 0
      MXEXP = MXSAVE

      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)

      IF (FMCOMP(MXY(1),'<',MXY(2))) THEN
          CALL FMI2M(0,MXY(3))
      ELSE
          NCALL = NCALL - 1
          CALL FMSUB(MXY(1),MXY(2),MXY(3))
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMDIM'
      ENDIF

      IF (KFLAG == 1) KFLAG = 0
      KWARN = KWRNSV
      CALL FMEXIT(MXY(3),MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMDIM

      SUBROUTINE FMDIV(MA,MB,MC)

!  MC = MA / MB

!  This routine performs the trace printing for division.  FMDIV2 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMDIV'
          CALL FMNTR(2,MA,MB,2,1)

          CALL FMDIV2(MA,MB,MC)

          CALL FMNTR(1,MC,MC,1,1)
      ELSE
          CALL FMDIV2(MA,MB,MC)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMDIV

      SUBROUTINE FMDIV2(MA,MB,MC)

!  Internal division routine.  MC = MA / MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      REAL (KIND(1.0D0)) :: MAS,MBS,MLR,MR,MS,MT1,MT2
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JRSSAV,K,KL,KR_RETRY,KRESLT,KT,KT1,KT2,KT3,L,N1,NG,NGUARD,NL
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      KR_RETRY = 0
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR. KDEBUG == 1) THEN
          CALL FMARGS('FMDIV    ',2,MA,MB,KRESLT)
          IF (KRESLT /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMDIV'
              CALL FMRSLT(MA,MB,MC,KRESLT)
              JRSIGN = JRSSAV
              NCALL = NCALL - 1
              RETURN
          ENDIF
      ELSE
          IF (MB%MP(3) == 0) THEN
              NAMEST(NCALL) = 'FMDIV'
              CALL FMIM(0,MC)
              KFLAG = -4
              CALL FMWARN
              MC%MP(2) = MUNKNO
              MC%MP(3) = 1
              JRSIGN = JRSSAV
              RETURN
          ENDIF
          IF (MA%MP(3) == 0) THEN
              CALL FMIM(0,MC)
              JRSIGN = JRSSAV
              RETURN
          ENDIF
      ENDIF
      KFLAG = 0

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD21
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52 - 1
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 10
          ENDIF
      ENDIF
      IF (NGUARD > NDIG+10) NGUARD = NDIG + 10
      N1 = NDIG + 1
      NG = NDIG + NGUARD

!             Save the sign of MA and MB and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)
      IF ((MAS > 0 .AND. MBS > 0) .OR. (MAS < 0 .AND. MBS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      MWA%MP(2) = MA%MP(2) - MB%MP(2) + 1

      IF (MBASE*MBASE <= MXBASE/(4*MBASE)) THEN

!             If a small base is being used (for example, using base 2 to check some machine
!             precision results), it is faster to switch to a larger base (like 2^24) while
!             doing the operation.

          IF (NDIGL /= NDIG .OR. MBASEL /= MBASE .OR. NGUARL /= NGUARD) THEN
              MBASEL = MBASE
              NDIGL = NDIG
              NGUARL = NGUARD
              DO J = 2, 1000
                 MR = MBASE*MBASEL
                 IF (4*MR > MXBASE) THEN
                     N21 = J - 1
                     NDIG = (NDIGL-1)/N21 + 1
                     IF (NDIG < 2) NDIG = 2
                     NGRDN = (NDIGL+NGUARD-1)/N21 + 2 - NDIG
                     IF (NGRDN < 1) NGRDN = 1
                     EXIT
                 ENDIF
                 MBASE = MR
              ENDDO
              MBASEN = MBASE
              NDIGN = NDIG
          ELSE
              MBASE = MBASEN
              NDIG = NDIGN
          ENDIF
          IF (MBLOGS /= MBASE) CALL FMCONS
          J = NDIG
          NDIG = NDIG + NGRDN + 5
          IF (.NOT. ALLOCATED(MPMA%MP)) THEN
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMA%MP) < NDIG+2) THEN
              DEALLOCATE(MPMA%MP)
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MPMB%MP)) THEN
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMB%MP) < NDIG+2) THEN
              DEALLOCATE(MPMB%MP)
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          NDIG = J
          MPMA%MP(1) = MA%MP(1)
          MPMB%MP(1) = MB%MP(1)
          MPMA%MP(2) = 0
          MPMB%MP(2) = 0
          L = 2 - N21
          DO J = 2, NDIGL+2-N21, N21
             MT1 = MA%MP(J+1)
             MT2 = MB%MP(J+1)
             DO K = J+1, J+N21-1
                MT1 = MT1*MBASEL + MA%MP(K+1)
                MT2 = MT2*MBASEL + MB%MP(K+1)
             ENDDO
             MPMA%MP(3+J/N21) = MT1
             MPMB%MP(3+J/N21) = MT2
             L = J
          ENDDO
          DO J = 3+L/N21, NDIG+NGRDN+2
             MPMA%MP(J+1) = 0
             MPMB%MP(J+1) = 0
          ENDDO
          IF (L+N21 <= NDIGL+1) THEN
              MT1 = 0
              MT2 = 0
              DO J = L+N21, L+2*N21-1
                 IF (J <= NDIGL+1) THEN
                     MT1 = MT1*MBASEL + MA%MP(J+1)
                     MT2 = MT2*MBASEL + MB%MP(J+1)
                 ELSE
                     MT1 = MT1*MBASEL
                     MT2 = MT2*MBASEL
                 ENDIF
              ENDDO
              MPMA%MP(3+(L+N21)/N21) = MT1
              MPMB%MP(3+(L+N21)/N21) = MT2
          ENDIF
          NG = NDIG + NGRDN + 1
          IF (MPMA%MP(3) >= MPMB%MP(3)) NG = NG + 1

!             Copy MA into the working array.

          DO J = 2, NDIG+1
             MWA%MP(J+2) = MPMA%MP(J+1)
          ENDDO
          MWA%MP(3) = 0
          DO J = NDIG+3, NG+4
             MWA%MP(J+1) = 0
          ENDDO
          CALL FMDIV3(MPMB,NG)
          IF (MWA%MP(3) >= MBASEL) THEN
              DO J = 2+NDIG+NGRDN, 3, -1
                 MWA%MP(J+1) = MWA%MP(J)
              ENDDO
              MWA%MP(3) = 0
              MWA%MP(2) = MWA%MP(2) + 1
          ENDIF
          KT3 = N21 - 1
          IF (MBASEL == 2 .AND. MBASE < INTMAX) THEN
              DO J = 2+NDIG+NGRDN, 3, -1
                 KT1 = MWA%MP(J+1)
                 KT = 2 + (J-2)*N21
                 KT2 = N21 + KT - 1
                 DO K = KT, MIN(1+(J-1)*N21,NDIGL+NGUARD+2+KT3)
                    MWA%MP(K-KT3+1) = IBITS(KT1,KT2-K,1)
                 ENDDO
              ENDDO
          ELSE
              MS = MBASEL**(N21-1)
              DO J = 2+NDIG+NGRDN, 3, -1
                 MR = MS
                 MT1 = MWA%MP(J+1)
                 DO K = 2+(J-2)*N21, MIN(1+(J-1)*N21,NDIGL+NGUARD+2+KT3)
                    MWA%MP(K-KT3+1) = AINT (MT1/MR)
                    MT1 = MT1 - MWA%MP(K-KT3+1)*MR
                    MR = AINT (MR/MBASEL)
                 ENDDO
              ENDDO
          ENDIF
          NDIG = NDIGL
          MBASE = MBASEL
          IF (MBLOGS /= MBASE) CALL FMCONS
      ELSE

!             This is the normal case, where the base is not small.

!             Copy MA into the working array.

          DO J = 2, N1
             MWA%MP(J+2) = MA%MP(J+1)
          ENDDO
          MWA%MP(3) = 0
          NL = N1 + NGUARD + 3
          DO J = NDIG+3, NL
             MWA%MP(J+1) = 0
          ENDDO
          CALL FMDIV3(MB,NG)
      ENDIF

!             Round, affix the sign, and return.

      IF (MWA%MP(3) == 0) THEN

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

          IF (NCALL >= 1 .AND. NGUARD < NDIG+10) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWA%MP(J+NDIG+3)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) )    &
                  KR_RETRY = KR_RETRY + 1
          ENDIF
          IF (KR_RETRY == 1 .AND. NGUARD < NDIG+10) THEN
              KR_RETRY = 2
              GO TO 110
          ENDIF
          MLR = 2*MWA%MP(NDIG+4) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,NGUARD,1)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+2) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+2) = MWA%MP(N1+2) + 1
                      MWA%MP(N1+3) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,NGUARD,1)
              ENDIF
          ENDIF
      ELSE
          IF (NCALL >= 1 .AND. NGUARD < NDIG+10) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWA%MP(J+NDIG+2)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) )    &
                  KR_RETRY = KR_RETRY + 1
          ENDIF
          IF (KR_RETRY == 1 .AND. NGUARD < NDIG+10) THEN
              KR_RETRY = 2
              GO TO 110
          ENDIF
          MLR = 2*MWA%MP(NDIG+3) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,NGUARD,0)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                      MWA%MP(N1+2) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,NGUARD,0)
              ENDIF
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MC)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMDIV'
          CALL FMWARN
      ENDIF

      MC%MP(1) = 1
      IF (MAS*MBS < 0 .AND. MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0)  &
          MC%MP(1) = -1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMDIV2

      SUBROUTINE FMDIV_R1(MA,MB)

!  MA = MA / MB

!  This routine performs the trace printing for division.  FMDIV2_R1 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMDIV_R1'
          CALL FMNTR(2,MA,MB,2,1)

          CALL FMDIV2_R1(MA,MB)

          NAMEST(NCALL) = 'FMDIV_R1'
          CALL FMNTR(1,MA,MA,1,1)
      ELSE
          CALL FMDIV2_R1(MA,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMDIV_R1

      SUBROUTINE FMDIV2_R1(MA,MB)

!  Internal division routine.  MA = MA / MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAS,MBS,MLR,MR,MS,MT1,MT2
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JRSSAV,K,KL,KR_RETRY,KRESLT,KT,KT1,KT2,KT3,L,N1,NG,NGUARD,NL
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      KR_RETRY = 0
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR. KDEBUG == 1) THEN
          CALL FMARGS('FMDIV    ',2,MA,MB,KRESLT)
          IF (KRESLT /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMDIV_R1'
              CALL FMRSLT(MA,MB,MXY(1),KRESLT)
              CALL FMEQ(MXY(1),MA)
              JRSIGN = JRSSAV
              NCALL = NCALL - 1
              RETURN
          ENDIF
      ELSE
          IF (MB%MP(3) == 0) THEN
              NAMEST(NCALL) = 'FMDIV_R1'
              CALL FMIM(0,MA)
              KFLAG = -4
              CALL FMWARN
              MA%MP(2) = MUNKNO
              MA%MP(3) = 1
              JRSIGN = JRSSAV
              RETURN
          ENDIF
          IF (MA%MP(3) == 0) THEN
              CALL FMIM(0,MA)
              JRSIGN = JRSSAV
              RETURN
          ENDIF
      ENDIF
      KFLAG = 0

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD21
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52 - 1
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 10
          ENDIF
      ENDIF
      IF (NGUARD > NDIG+10) NGUARD = NDIG + 10
      N1 = NDIG + 1
      NG = NDIG + NGUARD

!             Save the sign of MA and MB and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)
      IF ((MAS > 0 .AND. MBS > 0) .OR. (MAS < 0 .AND. MBS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      MWA%MP(2) = MA%MP(2) - MB%MP(2) + 1

      IF (MBASE*MBASE <= MXBASE/(4*MBASE)) THEN

!             If a small base is being used (for example, using base 2 to check some machine
!             precision results), it is faster to switch to a larger base (like 2^24) while
!             doing the operation.

          IF (NDIGL /= NDIG .OR. MBASEL /= MBASE .OR. NGUARL /= NGUARD) THEN
              MBASEL = MBASE
              NDIGL = NDIG
              NGUARL = NGUARD
              DO J = 2, 1000
                 MR = MBASE*MBASEL
                 IF (4*MR > MXBASE) THEN
                     N21 = J - 1
                     NDIG = (NDIGL-1)/N21 + 1
                     IF (NDIG < 2) NDIG = 2
                     NGRDN = (NDIGL+NGUARD-1)/N21 + 2 - NDIG
                     IF (NGRDN < 1) NGRDN = 1
                     EXIT
                 ENDIF
                 MBASE = MR
              ENDDO
              MBASEN = MBASE
              NDIGN = NDIG
          ELSE
              MBASE = MBASEN
              NDIG = NDIGN
          ENDIF
          IF (MBLOGS /= MBASE) CALL FMCONS
          J = NDIG
          NDIG = NDIG + NGRDN + 5
          IF (.NOT. ALLOCATED(MPMA%MP)) THEN
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMA%MP) < NDIG+2) THEN
              DEALLOCATE(MPMA%MP)
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MPMB%MP)) THEN
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMB%MP) < NDIG+2) THEN
              DEALLOCATE(MPMB%MP)
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          NDIG = J
          MPMA%MP(1) = MA%MP(1)
          MPMB%MP(1) = MB%MP(1)
          MPMA%MP(2) = 0
          MPMB%MP(2) = 0
          L = 2 - N21
          DO J = 2, NDIGL+2-N21, N21
             MT1 = MA%MP(J+1)
             MT2 = MB%MP(J+1)
             DO K = J+1, J+N21-1
                MT1 = MT1*MBASEL + MA%MP(K+1)
                MT2 = MT2*MBASEL + MB%MP(K+1)
             ENDDO
             MPMA%MP(3+J/N21) = MT1
             MPMB%MP(3+J/N21) = MT2
             L = J
          ENDDO
          DO J = 3+L/N21, NDIG+NGRDN+2
             MPMA%MP(J+1) = 0
             MPMB%MP(J+1) = 0
          ENDDO
          IF (L+N21 <= NDIGL+1) THEN
              MT1 = 0
              MT2 = 0
              DO J = L+N21, L+2*N21-1
                 IF (J <= NDIGL+1) THEN
                     MT1 = MT1*MBASEL + MA%MP(J+1)
                     MT2 = MT2*MBASEL + MB%MP(J+1)
                 ELSE
                     MT1 = MT1*MBASEL
                     MT2 = MT2*MBASEL
                 ENDIF
              ENDDO
              MPMA%MP(3+(L+N21)/N21) = MT1
              MPMB%MP(3+(L+N21)/N21) = MT2
          ENDIF
          NG = NDIG + NGRDN + 1
          IF (MPMA%MP(3) >= MPMB%MP(3)) NG = NG + 1

!             Copy MA into the working array.

          DO J = 2, NDIG+1
             MWA%MP(J+2) = MPMA%MP(J+1)
          ENDDO
          MWA%MP(3) = 0
          DO J = NDIG+3, NG+4
             MWA%MP(J+1) = 0
          ENDDO
          CALL FMDIV3(MPMB,NG)
          IF (MWA%MP(3) >= MBASEL) THEN
              DO J = 2+NDIG+NGRDN, 3, -1
                 MWA%MP(J+1) = MWA%MP(J)
              ENDDO
              MWA%MP(3) = 0
              MWA%MP(2) = MWA%MP(2) + 1
          ENDIF
          KT3 = N21 - 1
          IF (MBASEL == 2 .AND. MBASE < INTMAX) THEN
              DO J = 2+NDIG+NGRDN, 3, -1
                 KT1 = MWA%MP(J+1)
                 KT = 2 + (J-2)*N21
                 KT2 = N21 + KT - 1
                 DO K = KT, MIN(1+(J-1)*N21,NDIGL+NGUARD+2+KT3)
                    MWA%MP(K-KT3+1) = IBITS(KT1,KT2-K,1)
                 ENDDO
              ENDDO
          ELSE
              MS = MBASEL**(N21-1)
              DO J = 2+NDIG+NGRDN, 3, -1
                 MR = MS
                 MT1 = MWA%MP(J+1)
                 DO K = 2+(J-2)*N21, MIN(1+(J-1)*N21,NDIGL+NGUARD+2+KT3)
                    MWA%MP(K-KT3+1) = AINT (MT1/MR)
                    MT1 = MT1 - MWA%MP(K-KT3+1)*MR
                    MR = AINT (MR/MBASEL)
                 ENDDO
              ENDDO
          ENDIF
          NDIG = NDIGL
          MBASE = MBASEL
          IF (MBLOGS /= MBASE) CALL FMCONS
      ELSE

!             This is the normal case, where the base is not small.

!             Copy MA into the working array.

          DO J = 2, N1
             MWA%MP(J+2) = MA%MP(J+1)
          ENDDO
          MWA%MP(3) = 0
          NL = N1 + NGUARD + 3
          DO J = NDIG+3, NL
             MWA%MP(J+1) = 0
          ENDDO
          CALL FMDIV3(MB,NG)
      ENDIF

!             Round, affix the sign, and return.

      IF (MWA%MP(3) == 0) THEN

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

          IF (NCALL >= 1 .AND. NGUARD < NDIG+10) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWA%MP(J+NDIG+3)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) )    &
                  KR_RETRY = KR_RETRY + 1
          ENDIF
          IF (KR_RETRY == 1 .AND. NGUARD < NDIG+10) THEN
              KR_RETRY = 2
              GO TO 110
          ENDIF
          MLR = 2*MWA%MP(NDIG+4) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,NGUARD,1)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+2) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+2) = MWA%MP(N1+2) + 1
                      MWA%MP(N1+3) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,NGUARD,1)
              ENDIF
          ENDIF
      ELSE
          IF (NCALL >= 1 .AND. NGUARD < NDIG+10) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWA%MP(J+NDIG+2)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) )    &
                  KR_RETRY = KR_RETRY + 1
          ENDIF
          IF (KR_RETRY == 1 .AND. NGUARD < NDIG+10) THEN
              KR_RETRY = 2
              GO TO 110
          ENDIF
          MLR = 2*MWA%MP(NDIG+3) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,NGUARD,0)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                      MWA%MP(N1+2) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,NGUARD,0)
              ENDIF
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MA)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMDIV_R1'
          CALL FMWARN
      ENDIF

      MA%MP(1) = 1
      IF (MAS*MBS < 0 .AND. MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0)  &
          MA%MP(1) = -1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMDIV2_R1

      SUBROUTINE FMDIV_R2(MA,MB)

!  MB = MA / MB

!  This routine performs the trace printing for division.  FMDIV2_R2 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMDIV_R2'
          CALL FMNTR(2,MA,MB,2,1)

          CALL FMDIV2_R2(MA,MB)

          NAMEST(NCALL) = 'FMDIV_R2'
          CALL FMNTR(1,MB,MB,1,1)
      ELSE
          CALL FMDIV2_R2(MA,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMDIV_R2

      SUBROUTINE FMDIV2_R2(MA,MB)

!  Internal division routine.  MB = MA / MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAS,MBS,MLR,MR,MS,MT1,MT2
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JRSSAV,K,KL,KR_RETRY,KRESLT,KT,KT1,KT2,KT3,L,N1,NG,NGUARD,NL
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      KR_RETRY = 0
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR. KDEBUG == 1) THEN
          CALL FMARGS('FMDIV    ',2,MA,MB,KRESLT)
          IF (KRESLT /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMDIV_R2'
              CALL FMRSLT(MA,MB,MXY(1),KRESLT)
              CALL FMEQ(MXY(1),MB)
              JRSIGN = JRSSAV
              NCALL = NCALL - 1
              RETURN
          ENDIF
      ELSE
          IF (MB%MP(3) == 0) THEN
              NAMEST(NCALL) = 'FMDIV_R2'
              CALL FMIM(0,MB)
              KFLAG = -4
              CALL FMWARN
              MB%MP(2) = MUNKNO
              MB%MP(3) = 1
              JRSIGN = JRSSAV
              RETURN
          ENDIF
          IF (MA%MP(3) == 0) THEN
              CALL FMIM(0,MB)
              JRSIGN = JRSSAV
              RETURN
          ENDIF
      ENDIF
      KFLAG = 0

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD21
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52 - 1
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 10
          ENDIF
      ENDIF
      IF (NGUARD > NDIG+10) NGUARD = NDIG + 10
      N1 = NDIG + 1
      NG = NDIG + NGUARD

!             Save the sign of MA and MB and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)
      IF ((MAS > 0 .AND. MBS > 0) .OR. (MAS < 0 .AND. MBS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      MWA%MP(2) = MA%MP(2) - MB%MP(2) + 1

      IF (MBASE*MBASE <= MXBASE/(4*MBASE)) THEN

!             If a small base is being used (for example, using base 2 to check some machine
!             precision results), it is faster to switch to a larger base (like 2^24) while
!             doing the operation.

          IF (NDIGL /= NDIG .OR. MBASEL /= MBASE .OR. NGUARL /= NGUARD) THEN
              MBASEL = MBASE
              NDIGL = NDIG
              NGUARL = NGUARD
              DO J = 2, 1000
                 MR = MBASE*MBASEL
                 IF (4*MR > MXBASE) THEN
                     N21 = J - 1
                     NDIG = (NDIGL-1)/N21 + 1
                     IF (NDIG < 2) NDIG = 2
                     NGRDN = (NDIGL+NGUARD-1)/N21 + 2 - NDIG
                     IF (NGRDN < 1) NGRDN = 1
                     EXIT
                 ENDIF
                 MBASE = MR
              ENDDO
              MBASEN = MBASE
              NDIGN = NDIG
          ELSE
              MBASE = MBASEN
              NDIG = NDIGN
          ENDIF
          IF (MBLOGS /= MBASE) CALL FMCONS
          J = NDIG
          NDIG = NDIG + NGRDN + 5
          IF (.NOT. ALLOCATED(MPMA%MP)) THEN
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMA%MP) < NDIG+2) THEN
              DEALLOCATE(MPMA%MP)
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MPMB%MP)) THEN
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMB%MP) < NDIG+2) THEN
              DEALLOCATE(MPMB%MP)
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          NDIG = J
          MPMA%MP(1) = MA%MP(1)
          MPMB%MP(1) = MB%MP(1)
          MPMA%MP(2) = 0
          MPMB%MP(2) = 0
          L = 2 - N21
          DO J = 2, NDIGL+2-N21, N21
             MT1 = MA%MP(J+1)
             MT2 = MB%MP(J+1)
             DO K = J+1, J+N21-1
                MT1 = MT1*MBASEL + MA%MP(K+1)
                MT2 = MT2*MBASEL + MB%MP(K+1)
             ENDDO
             MPMA%MP(3+J/N21) = MT1
             MPMB%MP(3+J/N21) = MT2
             L = J
          ENDDO
          DO J = 3+L/N21, NDIG+NGRDN+2
             MPMA%MP(J+1) = 0
             MPMB%MP(J+1) = 0
          ENDDO
          IF (L+N21 <= NDIGL+1) THEN
              MT1 = 0
              MT2 = 0
              DO J = L+N21, L+2*N21-1
                 IF (J <= NDIGL+1) THEN
                     MT1 = MT1*MBASEL + MA%MP(J+1)
                     MT2 = MT2*MBASEL + MB%MP(J+1)
                 ELSE
                     MT1 = MT1*MBASEL
                     MT2 = MT2*MBASEL
                 ENDIF
              ENDDO
              MPMA%MP(3+(L+N21)/N21) = MT1
              MPMB%MP(3+(L+N21)/N21) = MT2
          ENDIF
          NG = NDIG + NGRDN + 1
          IF (MPMA%MP(3) >= MPMB%MP(3)) NG = NG + 1

!             Copy MA into the working array.

          DO J = 2, NDIG+1
             MWA%MP(J+2) = MPMA%MP(J+1)
          ENDDO
          MWA%MP(3) = 0
          DO J = NDIG+3, NG+4
             MWA%MP(J+1) = 0
          ENDDO
          CALL FMDIV3(MPMB,NG)
          IF (MWA%MP(3) >= MBASEL) THEN
              DO J = 2+NDIG+NGRDN, 3, -1
                 MWA%MP(J+1) = MWA%MP(J)
              ENDDO
              MWA%MP(3) = 0
              MWA%MP(2) = MWA%MP(2) + 1
          ENDIF
          KT3 = N21 - 1
          IF (MBASEL == 2 .AND. MBASE < INTMAX) THEN
              DO J = 2+NDIG+NGRDN, 3, -1
                 KT1 = MWA%MP(J+1)
                 KT = 2 + (J-2)*N21
                 KT2 = N21 + KT - 1
                 DO K = KT, MIN(1+(J-1)*N21,NDIGL+NGUARD+2+KT3)
                    MWA%MP(K-KT3+1) = IBITS(KT1,KT2-K,1)
                 ENDDO
              ENDDO
          ELSE
              MS = MBASEL**(N21-1)
              DO J = 2+NDIG+NGRDN, 3, -1
                 MR = MS
                 MT1 = MWA%MP(J+1)
                 DO K = 2+(J-2)*N21, MIN(1+(J-1)*N21,NDIGL+NGUARD+2+KT3)
                    MWA%MP(K-KT3+1) = AINT (MT1/MR)
                    MT1 = MT1 - MWA%MP(K-KT3+1)*MR
                    MR = AINT (MR/MBASEL)
                 ENDDO
              ENDDO
          ENDIF
          NDIG = NDIGL
          MBASE = MBASEL
          IF (MBLOGS /= MBASE) CALL FMCONS
      ELSE

!             This is the normal case, where the base is not small.

!             Copy MA into the working array.

          DO J = 2, N1
             MWA%MP(J+2) = MA%MP(J+1)
          ENDDO
          MWA%MP(3) = 0
          NL = N1 + NGUARD + 3
          DO J = NDIG+3, NL
             MWA%MP(J+1) = 0
          ENDDO
          CALL FMDIV3(MB,NG)
      ENDIF

!             Round, affix the sign, and return.

      IF (MWA%MP(3) == 0) THEN

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

          IF (NCALL >= 1 .AND. NGUARD < NDIG+10) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWA%MP(J+NDIG+3)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) )    &
                  KR_RETRY = KR_RETRY + 1
          ENDIF
          IF (KR_RETRY == 1 .AND. NGUARD < NDIG+10) THEN
              KR_RETRY = 2
              GO TO 110
          ENDIF
          MLR = 2*MWA%MP(NDIG+4) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,NGUARD,1)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+2) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+2) = MWA%MP(N1+2) + 1
                      MWA%MP(N1+3) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,NGUARD,1)
              ENDIF
          ENDIF
      ELSE
          IF (NCALL >= 1 .AND. NGUARD < NDIG+10) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWA%MP(J+NDIG+2)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) )    &
                  KR_RETRY = KR_RETRY + 1
          ENDIF
          IF (KR_RETRY == 1 .AND. NGUARD < NDIG+10) THEN
              KR_RETRY = 2
              GO TO 110
          ENDIF
          MLR = 2*MWA%MP(NDIG+3) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,NGUARD,0)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                      MWA%MP(N1+2) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,NGUARD,0)
              ENDIF
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MB)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMDIV_R2'
          CALL FMWARN
      ENDIF

      MB%MP(1) = 1
      IF (MAS*MBS < 0 .AND. MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0)  &
          MB%MP(1) = -1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMDIV2_R2

      SUBROUTINE FMDIV3(MB,NG)

!  Internal division routine.  Divide MA/MB and return the quotient in MWA.
!  MA has already been copied into MWA.
!  NG is the number of guard digits used.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MB

      DOUBLE PRECISION :: XB,XBR,XBASE,XMWA
      REAL (KIND(1.0D0)) :: MAXMWA,MBP1,MCARRY,MKT,MLMAX,MQD
      INTEGER :: J,JB,JL,KA,KB,KL,KPTMWA,N1,NG,NL,NMBWDS,NMETHD,NZDMB
      REAL :: C
      INTENT (IN) :: MB

      N1 = NDIG + 1
      NL = NG + 4

!             Check for using an FFT-based method if precision is very high.

      C = 3100
      IF (NDIG >= C) THEN
          NZDMB = 0
          DO J = 2, NDIG
             IF (MB%MP(J+2) == 0) NZDMB = NZDMB + 1
          ENDDO
          IF (NDIG-NZDMB < 50 .OR. REAL(NZDMB)/NDIG > 0.8 ) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          NMETHD = 1
      ENDIF
      IF (NMETHD == 2) THEN
          CALL FMDIVFFT(MB)
          RETURN
      ENDIF

!             NMBWDS is the number of words of MB used to compute the estimated quotient digit MQD.

      NMBWDS = 4
      IF (MBASE < 100) NMBWDS = 7

!             XB is an approximation of MB used in estimating the quotient digits.

      XBASE = DBLE(MBASE)
      XB = 0
      JL = NMBWDS
      IF (JL <= N1) THEN
          DO J = 2, JL
             XB = XB*XBASE + DBLE(MB%MP(J+1))
          ENDDO
      ELSE
          DO J = 2, JL
             IF (J <= N1) THEN
                 XB = XB*XBASE + DBLE(MB%MP(J+1))
             ELSE
                 XB = XB*XBASE
             ENDIF
          ENDDO
      ENDIF
      IF (JL+1 <= N1) THEN
          XB = XB + DBLE(MB%MP(JL+2))/XBASE
      ENDIF
      XBR = 1.0D0/XB

!             MLMAX determines when to normalize all of MWA.

      MBP1 = MBASE + 1
      MLMAX = MAXINT/MBP1
      MKT = INTMAX - MBASE
      MLMAX = MIN(MLMAX,MKT)

!             Count the trailing zero digits of MB.

      DO J = N1, 2, -1
         IF (MB%MP(J+1) /= 0) THEN
             NZDMB = N1 - J
             GO TO 110
         ENDIF
      ENDDO

!             MAXMWA is an upper bound on the size of values in MWA divided by MBASE-1.  It is used
!             to determine whether normalization can be postponed.

  110 MAXMWA = 0

!             KPTMWA points to the next digit in the quotient.

      KPTMWA = 2

!             This is the start of the division loop.

!             XMWA is an approximation of the active part of MWA used in estimating quotient digits.

  120 KL = KPTMWA + NMBWDS - 1
      IF (KL <= NL) THEN
          XMWA = ((DBLE(MWA%MP(KPTMWA+1))*XBASE + DBLE(MWA%MP(KPTMWA+2)))*XBASE  &
                 + DBLE(MWA%MP(KPTMWA+3)))*XBASE + DBLE(MWA%MP(KPTMWA+4))
          DO J = KPTMWA+4, KL
             XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
          ENDDO
      ELSE
          XMWA = DBLE(MWA%MP(KPTMWA+1))
          DO J = KPTMWA+1, KL
             IF (J <= NL) THEN
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
             ELSE
                 XMWA = XMWA*XBASE
             ENDIF
          ENDDO
      ENDIF

!             MQD is the estimated quotient digit.

      MQD = AINT(XMWA*XBR)
      IF (MQD < 0) MQD = MQD - 1

      IF (MQD > 0) THEN
          MAXMWA = MAXMWA + MQD
      ELSE
          MAXMWA = MAXMWA - MQD
      ENDIF

!             See if MWA must be normalized.

      KA = KPTMWA + 1
      KB = MIN(KA+NDIG-1-NZDMB,NL)
      IF (MAXMWA >= MLMAX) THEN
          DO J = KB, KA, -1
             IF (MWA%MP(J+1) < 0) THEN
                 MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ELSE IF (MWA%MP(J+1) >= MBASE) THEN
                 MCARRY = -INT(MWA%MP(J+1)/MBASE)
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ENDIF
          ENDDO
          XMWA = 0
          IF (KL <= NL) THEN
              DO J = KPTMWA, KL
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
              ENDDO
          ELSE
              DO J = KPTMWA, KL
                 IF (J <= NL) THEN
                     XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
                 ELSE
                     XMWA = XMWA*XBASE
                 ENDIF
              ENDDO
          ENDIF
          MQD = AINT(XMWA*XBR)
          IF (MQD < 0) MQD = MQD - 1
          IF (MQD > 0) THEN
              MAXMWA = MQD
          ELSE
              MAXMWA = -MQD
          ENDIF
      ENDIF

!             Subtract MQD*MB from MWA.

      JB = KA - 2
      IF (MQD /= 0) THEN

!             Major (Inner Loop)

          DO J = KA+1, KB+1
             MWA%MP(J) = MWA%MP(J) - MQD*MB%MP(J-JB)
          ENDDO
      ENDIF

      MWA%MP(KA+1) = MWA%MP(KA+1) + MWA%MP(KA)*MBASE
      MWA%MP(KPTMWA+1) = MQD

      KPTMWA = KPTMWA + 1
      IF (KPTMWA <= NG) GO TO 120
      IF (MWA%MP(3) == 0 .AND. KPTMWA <= NG+1) GO TO 120

      KL = KPTMWA + NMBWDS - 1
      IF (KL <= NL) THEN
          XMWA = ((DBLE(MWA%MP(KPTMWA+1))*XBASE + DBLE(MWA%MP(KPTMWA+2)))*XBASE  &
                 + DBLE(MWA%MP(KPTMWA+3)))*XBASE + DBLE(MWA%MP(KPTMWA+4))
          DO J = KPTMWA+4, KL
             XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
          ENDDO
      ELSE
          XMWA = DBLE(MWA%MP(KPTMWA+1))
          DO J = KPTMWA+1, KL
             IF (J <= NL) THEN
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
             ELSE
                 XMWA = XMWA*XBASE
             ENDIF
          ENDDO
      ENDIF
      MQD = AINT(XMWA*XBR)
      IF (MQD < 0) MQD = MQD - 1
      MWA%MP(KPTMWA+1) = MQD
      MWA%MP(KPTMWA+2) = 0
      MWA%MP(KPTMWA+3) = 0

!             Final normalization.

      IF (KPTMWA > 2*NDIG) THEN
          DO J = 2*NDIG+1, KPTMWA
             IF (MWA%MP(J+1) /= MBASE-1) EXIT
             IF (J == KPTMWA) MWA%MP(J+1) = MBASE
          ENDDO
      ENDIF
      DO J = KPTMWA, 3, -1
         IF (MWA%MP(J+1) < 0) THEN
             MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ELSE IF (MWA%MP(J+1) >= MBASE) THEN
             MCARRY = -INT(MWA%MP(J+1)/MBASE)
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE FMDIV3

      SUBROUTINE FMDIVD(MA,MB,MC,MD,ME)

!  Double division routine.  MD = MA / MC,   ME = MB / MC

!  It is usually slightly faster to do two divisions that have a common denominator with one call.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC,MD,ME

      REAL (KIND(1.0D0)) :: MA2P,MAS,MAXMWA,MB2P,MBP1,MBS,MC2P,MCARRY,MCS,  &
                            MKT,MLMAX,MLR,MQDMWA,MQDMWD,MTEMP
      DOUBLE PRECISION :: ERR,XB,XBR,XBASE,XMWA,XMWD
      REAL :: C
      INTEGER :: J,JB,JL,JRSSAV,KA,KB,KL,KOVUN,KPTMW,N1,NG,NGUARD,NL,NMBWDS,NZDMB
      INTENT (IN) :: MA,MB,MC
      INTENT (INOUT) :: MD,ME

      IF (.NOT. ALLOCATED(MD%MP)) THEN
          ALLOCATE(MD%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MD%MP) < NDIG+2) THEN
          DEALLOCATE(MD%MP)
          ALLOCATE(MD%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(ME%MP)) THEN
          ALLOCATE(ME%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(ME%MP) < NDIG+2) THEN
          DEALLOCATE(ME%MP)
          ALLOCATE(ME%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWD%MP)) THEN
          ALLOCATE(MWD%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWD%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWD%MP)
          ALLOCATE(MWD%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      JRSSAV = JRSIGN
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMDIVD'
          CALL FMNTR(2,MA,MB,2,1)
          IF (ABS(NTRACE) >= 2 .AND. NCALL <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL FMNTRJ(MC,NDIG)
              ELSE
                  CALL FMPRNT(MC)
              ENDIF
          ENDIF
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      C = 3100
      IF (NDIG >= C .OR.                                           &
          ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR.  &
          ABS(MC%MP(2)) > MEXPAB .OR. MBASE*MBASE <= MXBASE/(4*MBASE)) THEN
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN .OR.  &
              MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN .OR.  &
              MC%MP(2) == MEXPOV .OR. MC%MP(2) == MEXPUN) KOVUN = 1
          IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO .OR.  &
              MC%MP(2) == MUNKNO) KOVUN = 2
          NCALL = NCALL + 1
          CALL FMDIV2(MA,MC,MWD)
          KB = KFLAG
          CALL FMDIV2(MB,MC,ME)
          NCALL = NCALL - 1
          IF (((KFLAG < 0 .OR. KB < 0) .AND. KOVUN == 0) .OR.  &
              ((KFLAG == -4 .OR. KB == -4) .AND. KOVUN == 1)) THEN
              IF (KFLAG == -4 .OR. KB == -4) THEN
                  KFLAG = -4
              ELSE IF (KFLAG == -5 .OR. KB == -5) THEN
                  KFLAG = -5
              ELSE
                  KFLAG = MIN(KFLAG,KB)
              ENDIF
              NAMEST(NCALL) = 'FMDIVD'
              CALL FMWARN
          ENDIF
          CALL FMEQ(MWD,MD)
          GO TO 150
      ENDIF
      IF (MC%MP(3) == 0) THEN
          NAMEST(NCALL) = 'FMDIVD'
          KFLAG = -4
          CALL FMWARN
          CALL FMST2M('UNKNOWN',MD)
          CALL FMST2M('UNKNOWN',ME)
          GO TO 150
      ENDIF
      IF (MA%MP(3) == 0 .OR. MB%MP(3) == 0) THEN
          CALL FMDIV2(MA,MC,MWD)
          CALL FMDIV2(MB,MC,ME)
          CALL FMEQ(MWD,MD)
          GO TO 150
      ENDIF
      KFLAG = 0

!             NGUARD is the number of guard digits used.

      IF (NCALL > 1) THEN
          NGUARD = NGRD21
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52 - 1
      ENDIF
      IF (NGUARD > NDIG+10) NGUARD = NDIG + 10
      MA2P = ABS(MA%MP(3))
      MB2P = ABS(MB%MP(3))
      MC2P = ABS(MC%MP(3))
      IF ((MC2P >= MA2P .OR. MC2P >= MB2P) .AND. NGUARD < 2) NGUARD = 2
      N1 = NDIG + 1
      NG = NDIG + NGUARD

!             Copy MA and MB into the working arrays.

      DO J = 3, N1
         MWA%MP(J+2) = MA%MP(J+1)
         MWD%MP(J+2) = MB%MP(J+1)
      ENDDO
      MWA%MP(2) = MA%MP(2) - MC%MP(2) + 1
      MWD%MP(2) = MB%MP(2) - MC%MP(2) + 1
      MWA%MP(3) = 0
      MWD%MP(3) = 0
      NL = N1 + NGUARD + 3
      DO J = NDIG+3, NL
         MWA%MP(J+1) = 0
         MWD%MP(J+1) = 0
      ENDDO

!             Save the signs and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)
      MCS = MC%MP(1)
      MWA%MP(4) = MA%MP(3)
      MWD%MP(4) = MB%MP(3)

!             NMBWDS is the number of words used to compute the estimated quotient digits.

      NMBWDS = 4
      IF (MBASE < 100) NMBWDS = 7

!             XB is an approximation of MC used in selecting estimated quotients.

      XBASE = DBLE(MBASE)
      XB = 0
      JL = NMBWDS
      IF (JL <= N1) THEN
          DO J = 2, JL
             XB = XB*XBASE + DBLE(MC%MP(J+1))
          ENDDO
      ELSE
          DO J = 2, JL
             IF (J <= N1) THEN
                 XB = XB*XBASE + DBLE(MC%MP(J+1))
             ELSE
                 XB = XB*XBASE
             ENDIF
          ENDDO
      ENDIF
      IF (JL+1 <= N1) XB = XB + DBLE(MC%MP(JL+2))/XBASE
      XBR = 1.0D0/XB

!             MLMAX determines when to normalize all of MWA.

      MBP1 = MBASE + 1
      MLMAX = MAXINT/MBP1
      MKT = INTMAX - MBASE
      MLMAX = MIN(MLMAX,MKT)

!             Count the trailing zero digits of MC.

      DO J = N1, 2, -1
         IF (MC%MP(J+1) /= 0) THEN
             NZDMB = N1 - J
             GO TO 110
         ENDIF
      ENDDO

!             MAXMWA is an upper bound on the size of values in MWA divided by MBASE-1.  It is used
!             to determine whether normalization can be postponed.

  110 MAXMWA = 0

!             KPTMW points to the next digit in the quotient.

      KPTMW = 2

!             This is the start of the division loop.

!             XMWA is an approximation of the active part of MWA used in selecting estimated
!             quotients.

  120 KL = KPTMW + NMBWDS - 1
      IF (KL <= NL) THEN
          XMWA = ((DBLE(MWA%MP(KPTMW+1))*XBASE + DBLE(MWA%MP(KPTMW+2)))*XBASE  &
                 + DBLE(MWA%MP(KPTMW+3)))*XBASE + DBLE(MWA%MP(KPTMW+4))
          XMWD = ((DBLE(MWD%MP(KPTMW+1))*XBASE + DBLE(MWD%MP(KPTMW+2)))*XBASE  &
                 + DBLE(MWD%MP(KPTMW+3)))*XBASE + DBLE(MWD%MP(KPTMW+4))
          DO J = KPTMW+4, KL
             XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
             XMWD = XMWD*XBASE + DBLE(MWD%MP(J+1))
          ENDDO
      ELSE
          XMWA = DBLE(MWA%MP(KPTMW+1))
          XMWD = DBLE(MWD%MP(KPTMW+1))
          DO J = KPTMW+1, KL
             IF (J <= NL) THEN
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
                 XMWD = XMWD*XBASE + DBLE(MWD%MP(J+1))
             ELSE
                 XMWA = XMWA*XBASE
                 XMWD = XMWD*XBASE
             ENDIF
          ENDDO
      ENDIF

!             MQDMWA and MQDMWD are the estimated quotient digits.

      MQDMWA = AINT(XMWA*XBR)
      IF (MQDMWA < 0) MQDMWA = MQDMWA - 1
      MQDMWD = AINT(XMWD*XBR)
      IF (MQDMWD < 0) MQDMWD = MQDMWD - 1

      MAXMWA = MAXMWA + MAX(ABS(MQDMWA),ABS(MQDMWD))

!             See if MWA and MWD must be normalized.

      KA = KPTMW + 1
      KB = MIN(KA+NDIG-1-NZDMB,NL)
      IF (MAXMWA >= MLMAX) THEN
          DO J = KB, KA, -1
             IF (MWA%MP(J+1) < 0) THEN
                 MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ELSE IF (MWA%MP(J+1) >= MBASE) THEN
                 MCARRY = -INT(MWA%MP(J+1)/MBASE)
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ENDIF
             IF (MWD%MP(J+1) < 0) THEN
                 MCARRY = INT((-MWD%MP(J+1)-1)/MBASE) + 1
                 MWD%MP(J+1) = MWD%MP(J+1) + MCARRY*MBASE
                 MWD%MP(J) = MWD%MP(J) - MCARRY
             ELSE IF (MWD%MP(J+1) >= MBASE) THEN
                 MCARRY = -INT(MWD%MP(J+1)/MBASE)
                 MWD%MP(J+1) = MWD%MP(J+1) + MCARRY*MBASE
                 MWD%MP(J) = MWD%MP(J) - MCARRY
             ENDIF
          ENDDO
          XMWA = 0
          XMWD = 0
          IF (KL <= NL) THEN
              DO J = KPTMW, KL
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
                 XMWD = XMWD*XBASE + DBLE(MWD%MP(J+1))
              ENDDO
          ELSE
              DO J = KPTMW, KL
                 IF (J <= NL) THEN
                     XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
                     XMWD = XMWD*XBASE + DBLE(MWD%MP(J+1))
                 ELSE
                     XMWA = XMWA*XBASE
                     XMWD = XMWD*XBASE
                 ENDIF
              ENDDO
          ENDIF
          MQDMWA = AINT(XMWA*XBR)
          IF (MQDMWA < 0) MQDMWA = MQDMWA - 1
          MQDMWD = AINT(XMWD*XBR)
          IF (MQDMWD < 0) MQDMWD = MQDMWD - 1
          MAXMWA = MAX(ABS(MQDMWA),ABS(MQDMWD))
      ENDIF

!             Subtract MQDMWA*MC from MWA and MQDMWD*MC from MWD.

      JB = KA - 2

!             Major (Inner Loop)

      DO J = KA+1, KB+1
         MTEMP = MC%MP(J-JB)
         MWA%MP(J) = MWA%MP(J) - MQDMWA*MTEMP
         MWD%MP(J) = MWD%MP(J) - MQDMWD*MTEMP
      ENDDO

      MWA%MP(KA+1) = MWA%MP(KA+1) + MWA%MP(KA)*MBASE
      MWD%MP(KA+1) = MWD%MP(KA+1) + MWD%MP(KA)*MBASE
      MWA%MP(KPTMW+1) = MQDMWA
      MWD%MP(KPTMW+1) = MQDMWD

      KPTMW = KPTMW + 1
      IF (KPTMW <= NG) GO TO 120

      KL = KPTMW + NMBWDS - 1
      IF (KL <= NL) THEN
          XMWA = ((DBLE(MWA%MP(KPTMW+1))*XBASE + DBLE(MWA%MP(KPTMW+2)))*XBASE  &
                 + DBLE(MWA%MP(KPTMW+3)))*XBASE + DBLE(MWA%MP(KPTMW+4))
          XMWD = ((DBLE(MWD%MP(KPTMW+1))*XBASE + DBLE(MWD%MP(KPTMW+2)))*XBASE  &
                 + DBLE(MWD%MP(KPTMW+3)))*XBASE + DBLE(MWD%MP(KPTMW+4))
          DO J = KPTMW+4, KL
             XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
             XMWD = XMWD*XBASE + DBLE(MWD%MP(J+1))
          ENDDO
      ELSE
          XMWA = DBLE(MWA%MP(KPTMW+1))
          XMWD = DBLE(MWD%MP(KPTMW+1))
          DO J = KPTMW+1, KL
             IF (J <= NL) THEN
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
                 XMWD = XMWD*XBASE + DBLE(MWD%MP(J+1))
             ELSE
                 XMWA = XMWA*XBASE
                 XMWD = XMWD*XBASE
             ENDIF
          ENDDO
      ENDIF
      MQDMWA = AINT(XMWA*XBR)
      IF (MQDMWA < 0) MQDMWA = MQDMWA - 1
      MQDMWD = AINT(XMWD*XBR)
      IF (MQDMWD < 0) MQDMWD = MQDMWD - 1
      MWA%MP(KPTMW+1) = MQDMWA
      MWA%MP(KPTMW+2) = 0
      MWA%MP(KPTMW+3) = 0
      MWD%MP(KPTMW+1) = MQDMWD
      MWD%MP(KPTMW+2) = 0
      MWD%MP(KPTMW+3) = 0

!             Final normalization.

      DO J = KPTMW-1, 3, -1
         IF (MWA%MP(J+1) < 0) THEN
             MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ELSE IF (MWA%MP(J+1) >= MBASE) THEN
             MCARRY = -INT(MWA%MP(J+1)/MBASE)
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ENDIF
         IF (MWD%MP(J+1) < 0) THEN
             MCARRY = INT((-MWD%MP(J+1)-1)/MBASE) + 1
             MWD%MP(J+1) = MWD%MP(J+1) + MCARRY*MBASE
             MWD%MP(J) = MWD%MP(J) - MCARRY
         ELSE IF (MWD%MP(J+1) >= MBASE) THEN
             MCARRY = -INT(MWD%MP(J+1)/MBASE)
             MWD%MP(J+1) = MWD%MP(J+1) + MCARRY*MBASE
             MWD%MP(J) = MWD%MP(J) - MCARRY
         ENDIF
      ENDDO

!             Round, affix the sign, and return.

      IF ((MAS > 0 .AND. MCS > 0) .OR. (MAS < 0 .AND. MCS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (MWA%MP(3) == 0) THEN
          IF (NCALL >= 1) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWA%MP(J+NDIG+3)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) THEN
                  CALL FMDIV2(MA,MC,MD)
                  GO TO 130
              ENDIF
          ENDIF
          MLR = 2*MWA%MP(NDIG+4) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,NGUARD,1)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+2) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+2) = MWA%MP(N1+2) + 1
                      MWA%MP(N1+3) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,NGUARD,1)
              ENDIF
          ENDIF
      ELSE
          IF (NCALL >= 1) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWA%MP(J+NDIG+2)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) THEN
                  CALL FMDIV2(MA,MC,MD)
                  GO TO 130
              ENDIF
          ENDIF
          MLR = 2*MWA%MP(NDIG+3) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,NGUARD,0)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                      MWA%MP(N1+2) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,NGUARD,0)
              ENDIF
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MD)

  130 IF ((MBS > 0 .AND. MCS > 0) .OR. (MBS < 0 .AND. MCS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (MWD%MP(3) == 0) THEN
          IF (NCALL >= 1) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWD%MP(J+NDIG+3)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) THEN
                  CALL FMDIV2(MB,MC,ME)
                  GO TO 140
              ENDIF
          ENDIF
          MLR = 2*MWD%MP(NDIG+4) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWD,NDIG,NGUARD,1)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWD%MP(N1+2) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWD%MP(N1+2) = MWD%MP(N1+2) + 1
                      MWD%MP(N1+3) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWD,NDIG,NGUARD,1)
              ENDIF
          ENDIF
      ELSE
          IF (NCALL >= 1) THEN
              KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
              ERR = 0
              DO J = KL, 1, -1
                 ERR = (ERR + MWD%MP(J+NDIG+2)) / MBASE
              ENDDO
              IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
                   (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) THEN
                  CALL FMDIV2(MB,MC,ME)
                  GO TO 140
              ENDIF
          ENDIF
          MLR = 2*MWD%MP(NDIG+3) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWD,NDIG,NGUARD,0)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWD%MP(N1+1) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWD%MP(N1+1) = MWD%MP(N1+1) + 1
                      MWD%MP(N1+2) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWD,NDIG,NGUARD,0)
              ENDIF
          ENDIF
      ENDIF
      CALL FMMOVE(MWD,ME)

  140 IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMDIVD'
          CALL FMWARN
      ENDIF

      MD%MP(1) = 1
      IF (MAS*MCS < 0 .AND. MD%MP(2) /= MUNKNO .AND. MD%MP(3) /= 0)  &
          MD%MP(1) = -1
      ME%MP(1) = 1
      IF (MBS*MCS < 0 .AND. ME%MP(2) /= MUNKNO .AND. ME%MP(3) /= 0)  &
          ME%MP(1) = -1

  150 IF (NTRACE /= 0) THEN
          CALL FMNTR(1,MD,MD,1,1)
          IF (ABS(NTRACE) >= 1 .AND. NCALL <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL FMNTRJ(ME,NDIG)
              ELSE
                  CALL FMPRNT(ME)
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMDIVD

      SUBROUTINE FMDIVFFT(MB)
      USE ModLib_FMVALS
      IMPLICIT NONE

!  Internal division routine MA/MB for very high precision.
!  MA has already been moved to MWA before this routine is called.
!  Fast Fourier transforms are used, and the number of digits carried is usually
!  raised slightly, because the FFT is faster when N has only small prime factors.

      TYPE(MULTI) :: MB
      INTENT (IN) :: MB
      DOUBLE PRECISION :: XB
      REAL (KIND(1.0D0)) :: MWA1
      INTEGER :: J,K,KST,ND2,NDSAVE,NSTACK(49)
      TYPE(MULTI) :: MXY(10)

      NDSAVE = NDIG

!             Save low precision copies of the numerator and denominator.

      NDIG = 20
      CALL FMIM(1,MXY(7))
      DO J = 1, SIZE(MXY(7)%MP)-3
         MXY(7)%MP(J+2) = MWA%MP(J+3)
      ENDDO
      MXY(7)%MP(2) = MWA%MP(2) + MB%MP(2) - 1
      CALL FMEQU(MB,MXY(8),NDSAVE,20)
      NDIG = NDSAVE

!             Use Newton iteration and the routine FMMPYFFT, with the formula
!                 x = x + x*(1 - b*x)
!             to converge to 1/b.

      K = MAX(NGRD52-1,2)
      NDIG = MAX(NDIG+K,2)

!             Generate the first approximation.

      CALL FMIM(1,MXY(1))
      DO J = 1, NDSAVE
         MXY(1)%MP(J+2) = MWA%MP(J+3)
      ENDDO
      CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
      MXY(1)%MP(2) = 0
      MXY(2)%MP(1) = 1
      MXY(2)%MP(2) = 0
      KST = MWA%MP(2)

!             FMADD2 will negate MXY(2) and add.

      KSUB = 1
      CALL FMADD2(MXY(1),MXY(2),MXY(3))
      KSUB = 0
      IF (MXY(3)%MP(1) >= 0) THEN
          MWA1 = KST
      ELSE
          MWA1 = KST - 1
      ENDIF
      CALL FMMD(MXY(2),XB)
      XB = 1.0D0/XB
      CALL FMDM(XB,MXY(3))

!             Initialize.

      CALL FMIM(0,MXY(4))
      CALL FMIM(0,MXY(5))
      CALL FMIM(1,MXY(6))
      CALL FMDIG(NSTACK,KST)

!             Newton iteration.

      DO J = 1, KST-1
         NDIG = NSTACK(J)
         CALL FMMPY2(MXY(2),MXY(3),MXY(4))
         KSUB = 1
         CALL FMADD2_R2(MXY(6),MXY(4))
         KSUB = 0
         NDIG = NSTACK(MAX(1,J-1))
         CALL FMMPY2(MXY(3),MXY(4),MXY(5))
         NDIG = NSTACK(J)
         CALL FMADD2_R1(MXY(3),MXY(5))
      ENDDO

!             Karp's trick:
!             The standard last step would give 1/b and then a final full precision
!             multiply by a would be done.  That does 2 full mpy's at the final precision,
!             and one at next-to-last (1/2 of final) precision.

!             Instead combine the a* step into the last iteration and get
!             y = a*x   at 1/2 precision, then
!             y = y + x*(a - b*y)
!             where the x* mpy is at 1/2 precision and only b*y is at final precision.

      NDIG = NSTACK(MAX(1,KST-1))
      CALL FMMPY2(MXY(1),MXY(3),MXY(4))
      NDIG = NSTACK(KST)
      CALL FMMPY2(MXY(2),MXY(4),MXY(5))
      KSUB = 1
      CALL FMADD2(MXY(1),MXY(5),MXY(6))
      KSUB = 0
      NDIG = NSTACK(MAX(1,KST-1))
      CALL FMMPY2(MXY(3),MXY(6),MXY(5))
      NDIG = NSTACK(KST)
      CALL FMADD2(MXY(4),MXY(5),MXY(6))

      MXY(6)%MP(2) = MWA1

!             Because of rounding errors in the Newton iteration, if the quotient is very close
!             to a power of MBASE the exponent of the result might be off by one.
!             Check by doing low precision multiplications.

      ND2 = NDIG
      MXY(7)%MP(1) = -1
      MXY(8)%MP(1) = 1
      CALL FMEQU(MXY(6),MXY(4),NDSAVE,20)
      MXY(4)%MP(1) = 1
      NDIG = 20
      CALL FMMPY2(MXY(8),MXY(4),MXY(5))
      CALL FMADD2(MXY(5),MXY(7),MXY(9))
      MXY(9)%MP(1) = 1
      MXY(4)%MP(2) = MXY(4)%MP(2) + 1
      CALL FMMPY2(MXY(8),MXY(4),MXY(5))
      CALL FMADD2(MXY(5),MXY(7),MXY(10))
      IF (MXY(10)%MP(3) /= 0) MXY(10)%MP(1) = -1
      CALL FMADD2(MXY(10),MXY(9),MXY(5))
      IF (MXY(5)%MP(1) == 1) THEN
          CALL FMEQ(MXY(10),MXY(9))
          MXY(6)%MP(2) = MXY(6)%MP(2) + 1
      ENDIF
      MXY(4)%MP(2) = MXY(4)%MP(2) - 2
      CALL FMMPY2(MXY(8),MXY(4),MXY(5))
      CALL FMADD2(MXY(5),MXY(7),MXY(10))
      IF (MXY(10)%MP(3) /= 0) MXY(10)%MP(1) = -1
      CALL FMADD2(MXY(10),MXY(9),MXY(5))
      IF (MXY(5)%MP(1) == 1) THEN
          MXY(6)%MP(2) = MXY(6)%MP(2) - 1
      ENDIF
      NDIG = ND2

      CALL FMEQ(MXY(6),MWA)
      IF (KFLAG == 1) KFLAG = 0
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE FMDIVFFT

      SUBROUTINE FMDIVI(MA,IVAL,MB)

!  MB = MA / IVAL

!  Divide FM number MA by one word integer IVAL.

!  This routine is faster than FMDIV when the divisor is less than MXBASE (the square root of the
!  largest integer).
!  When IVAL is not less than MXBASE, FMDIV2 is used.  In this case, if IVAL is known to be a
!  product of two integers less than MXBASE, it is usually faster to make two calls to FMDIVI
!  with half-word factors than one call with their product.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB

      KFLAG = 0
      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMDIVI'
          CALL FMNTR(2,MA,MA,1,1)
          CALL FMNTRI(2,IVAL,0)
          CALL FMDIVN(MA,IVAL,MB)
          CALL FMNTR(1,MB,MB,1,1)
      ELSE
          CALL FMDIVN(MA,IVAL,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMDIVI

      SUBROUTINE FMDIVI_R1(MA,IVAL)

!  MA = MA / IVAL

!  Divide FM number MA by one word integer IVAL.

!  This routine is faster than FMDIV when the divisor is less than MXBASE (the square root of the
!  largest integer).
!  When IVAL is not less than MXBASE, FMDIV2 is used.  In this case, if IVAL is known to be a
!  product of two integers less than MXBASE, it is usually faster to make two calls to FMDIVI
!  with half-word factors than one call with their product.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL

      KFLAG = 0
      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMDIVI_R1'
          CALL FMNTR(2,MA,MA,1,1)
          CALL FMNTRI(2,IVAL,0)
          CALL FMDIVN_R1(MA,IVAL)
          CALL FMNTR(1,MA,MA,1,1)
      ELSE
          CALL FMDIVN_R1(MA,IVAL)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMDIVI_R1

      SUBROUTINE FMDIVN(MA,IVAL,MB)

!  Internal divide by integer routine.  MB = MA / IVAL

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MA1,MAS,MKT,MLR,MODINT,MVALP
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JRSSAV,KA,KB,KL,KPT,KPTWA,KR_RETRY,N1,NGUARD,NMVAL,NV2
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KR_RETRY = 0

!             Check for special cases.

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      N1 = NDIG + 1
      IF (MA%MP(2) == MUNKNO .OR. IVAL == 0) THEN
          MA1 = MA%MP(2)
          CALL FMIM(0,MB)
          MB%MP(2) = MUNKNO
          MB%MP(3) = 1
          KFLAG = -4
          IF (MA1 /= MUNKNO) THEN
              NAMEST(NCALL) = 'FMDIVI'
              CALL FMWARN
          ENDIF
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(3) == 0) THEN
          CALL FMEQ(MA,MB)
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (ABS(MA%MP(2)) < MEXPOV .AND. ABS(IVAL) > 1) GO TO 110

      IF (ABS(IVAL) == 1) THEN
          DO J = 0, N1
             MB%MP(J+1) = MA%MP(J+1)
          ENDDO
          MB%MP(1) = MA%MP(1)*IVAL
          IF (MA%MP(2) == MEXPOV) KFLAG = -5
          IF (MA%MP(2) == MEXPUN) KFLAG = -6
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPUN) THEN
          MAS = MA%MP(1)
          CALL FMIM(0,MB)
          MB%MP(2) = MEXPUN
          MB%MP(3) = 1
          IF ((MAS < 0 .AND. IVAL > 0) .OR. (MAS > 0 .AND. IVAL < 0)) MB%MP(1) = -1
          KFLAG = -6
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPOV) THEN
          NAMEST(NCALL) = 'FMDIVI'
          CALL FMIM(0,MB)
          MB%MP(2) = MUNKNO
          MB%MP(3) = 1
          KFLAG = -4
          CALL FMWARN
          JRSIGN = JRSSAV
          RETURN
      ENDIF

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD21
      ELSE
          NGUARD = NGRD52
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 10
          ENDIF
      ENDIF
      IF (NGUARD > NDIG+10) NGUARD = NDIG + 10

!             If ABS(IVAL) >= MXBASE use FMDIV.

      MVALP = ABS(IVAL)
      NMVAL = INT(MVALP)
      NV2 = NMVAL - 1
      IF (ABS(IVAL) > MXBASE .OR. NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL FMIM(IVAL,MXY(1))
          CALL FMDIV2(MA,MXY(1),MB)
          JRSIGN = JRSSAV
          RETURN
      ENDIF

!             Work with positive numbers.

      MAS = MA%MP(1)

!             Find the first significant digit of the quotient.

      MKT = MA%MP(3)
      IF (MKT >= MVALP) THEN
          KPT = 2
          GO TO 130
      ENDIF
      DO J = 3, N1
         MKT = MKT*MBASE + MA%MP(J+1)
         IF (MKT >= MVALP) THEN
             KPT = J
             GO TO 130
         ENDIF
      ENDDO
      KPT = N1

  120 KPT = KPT + 1
      MKT = MKT*MBASE
      IF (MKT < MVALP) GO TO 120

!             Do the rest of the division.

  130 KA = KPT + 1
      MWA%MP(2) = MA%MP(2) + 2 - KPT
      MWA%MP(3) = INT (MKT/MVALP)
      MODINT = MKT - MWA%MP(3)*MVALP
      KPTWA = 2
      IF (KA <= N1) THEN
          KL = 3 - KA

!             (Inner Loop)

          DO J = KA+1, N1+1
             MKT = MODINT*MBASE + MA%MP(J)
             MWA%MP(J+KL) = INT (MKT/MVALP)
             MODINT = MKT - MWA%MP(J+KL)*MVALP
          ENDDO
          KPTWA = KL + N1
      ENDIF

      KA = KPTWA + 1
      KB = N1 + NGUARD
      DO J = KA, KB
         MKT = MODINT*MBASE
         MWA%MP(J+1) = INT (MKT/MVALP)
         MODINT = MKT - MWA%MP(J+1)*MVALP
      ENDDO

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NGUARD < NDIG+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Round the result, put the sign on MB and return.

      MLR = 2*MWA%MP(NDIG+3) + 1
      IF ((MAS > 0 .AND. IVAL > 0) .OR. (MAS < 0 .AND. IVAL < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,0)
      ELSE IF (MLR >= MBASE) THEN
          IF (MLR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                  MWA%MP(N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,0)
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MB)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMDIVI'
          CALL FMWARN
      ENDIF
      MB%MP(1) = JRSIGN
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMDIVN

      SUBROUTINE FMDIVN_R1(MA,IVAL)

!  Internal divide by integer routine.  MA = MA / IVAL

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MA1,MAS,MKT,MLR,MODINT,MVALP
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JRSSAV,KA,KB,KL,KPT,KPTWA,KR_RETRY,N1,NGUARD,NMVAL,NV2
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KR_RETRY = 0

!             Check for special cases.

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      N1 = NDIG + 1
      IF (MA%MP(2) == MUNKNO .OR. IVAL == 0) THEN
          MA1 = MA%MP(2)
          CALL FMIM(0,MA)
          MA%MP(2) = MUNKNO
          MA%MP(3) = 1
          KFLAG = -4
          IF (MA1 /= MUNKNO) THEN
              NAMEST(NCALL) = 'FMDIVI_R1'
              CALL FMWARN
          ENDIF
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(3) == 0) THEN
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (ABS(MA%MP(2)) < MEXPOV .AND. ABS(IVAL) > 1) GO TO 110

      IF (ABS(IVAL) == 1) THEN
          MA%MP(1) = MA%MP(1)*IVAL
          IF (MA%MP(2) == MEXPOV) KFLAG = -5
          IF (MA%MP(2) == MEXPUN) KFLAG = -6
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPUN) THEN
          MAS = MA%MP(1)
          CALL FMIM(0,MA)
          MA%MP(2) = MEXPUN
          MA%MP(3) = 1
          IF ((MAS < 0 .AND. IVAL > 0) .OR. (MAS > 0 .AND. IVAL < 0)) MA%MP(1) = -1
          KFLAG = -6
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPOV) THEN
          NAMEST(NCALL) = 'FMDIVI_R1'
          CALL FMIM(0,MA)
          MA%MP(2) = MUNKNO
          MA%MP(3) = 1
          KFLAG = -4
          CALL FMWARN
          JRSIGN = JRSSAV
          RETURN
      ENDIF

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD21
      ELSE
          NGUARD = NGRD52
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 10
          ENDIF
      ENDIF
      IF (NGUARD > NDIG+10) NGUARD = NDIG + 10

!             If ABS(IVAL) >= MXBASE use FMDIV.

      MVALP = ABS(IVAL)
      NMVAL = INT(MVALP)
      NV2 = NMVAL - 1
      IF (ABS(IVAL) > MXBASE .OR. NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL FMIM(IVAL,MXY(1))
          CALL FMDIV2_R1(MA,MXY(1))
          JRSIGN = JRSSAV
          RETURN
      ENDIF

!             Work with positive numbers.

      MAS = MA%MP(1)

!             Find the first significant digit of the quotient.

      MKT = MA%MP(3)
      IF (MKT >= MVALP) THEN
          KPT = 2
          GO TO 130
      ENDIF
      DO J = 3, N1
         MKT = MKT*MBASE + MA%MP(J+1)
         IF (MKT >= MVALP) THEN
             KPT = J
             GO TO 130
         ENDIF
      ENDDO
      KPT = N1

  120 KPT = KPT + 1
      MKT = MKT*MBASE
      IF (MKT < MVALP) GO TO 120

!             Do the rest of the division.

  130 KA = KPT + 1
      MWA%MP(2) = MA%MP(2) + 2 - KPT
      MWA%MP(3) = INT (MKT/MVALP)
      MODINT = MKT - MWA%MP(3)*MVALP
      KPTWA = 2
      IF (KA <= N1) THEN
          KL = 3 - KA

!             (Inner Loop)

          DO J = KA+1, N1+1
             MKT = MODINT*MBASE + MA%MP(J)
             MWA%MP(J+KL) = INT (MKT/MVALP)
             MODINT = MKT - MWA%MP(J+KL)*MVALP
          ENDDO
          KPTWA = KL + N1
      ENDIF

      KA = KPTWA + 1
      KB = N1 + NGUARD
      DO J = KA, KB
         MKT = MODINT*MBASE
         MWA%MP(J+1) = INT (MKT/MVALP)
         MODINT = MKT - MWA%MP(J+1)*MVALP
      ENDDO

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NGUARD < NDIG+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Round the result, put the sign on MA and return.

      MLR = 2*MWA%MP(NDIG+3) + 1
      IF ((MAS > 0 .AND. IVAL > 0) .OR. (MAS < 0 .AND. IVAL < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,0)
      ELSE IF (MLR >= MBASE) THEN
          IF (MLR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                  MWA%MP(N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,0)
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MA)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMDIVI_R1'
          CALL FMWARN
      ENDIF
      MA%MP(1) = JRSIGN
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMDIVN_R1

      SUBROUTINE FMDM(X,MA)

!  Internal routine for converting double precision to multiple precision.  Called by FMDPM.

      USE ModLib_FMVALS
      IMPLICIT NONE

      DOUBLE PRECISION :: X
      TYPE(MULTI) :: MA

      DOUBLE PRECISION :: ONE,XBASE,Y,Y2,YT
      REAL (KIND(1.0D0)) :: MK,MN
      INTEGER :: J,K,N1,NE
      INTENT (IN) :: X
      INTENT (INOUT) :: MA

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      N1 = NDIG + 1

!             Check for X = + or - Infinity, or NaN.  Return unknown if so.

      IF (X > HUGE(X) .OR. X < -HUGE(X) .OR. (.NOT.(X == X))) THEN
          DO J = 2, NDIG
             MA%MP(J+2) = 0
          ENDDO
          KFLAG = -4
          MA%MP(2) = MUNKNO
          MA%MP(3) = 1
          MA%MP(1) = 1
          CALL FMWARN
          RETURN
      ENDIF

      ONE = 1.0D0
      XBASE = MBASE
      K = 0

!             NE-1 is the number of words at the current precision and base roughly equal to
!             machine precision.

      NE = INT(DLOGEB) + 3
      Y = X
      IF (X < 0.0) Y = -X

      IF (X == 0.0) THEN
          DO J = 1, N1
             MA%MP(J+1) = 0
          ENDDO
          GO TO 160
      ENDIF

!             Get the exponent.

      IF (Y > ONE) THEN
          IF (Y/XBASE < Y) THEN
  110         K = K + 1
              Y = Y/XBASE
              IF (Y > ONE) GO TO 110
              IF (Y < ONE) THEN
                  MA%MP(2) = K
                  GO TO 140
              ENDIF
              GO TO 130
          ELSE
              KFLAG = -4
              CALL FMWARN
              DO J = 1, N1
                 MA%MP(J+1) = 0
              ENDDO
              MA%MP(2) = MUNKNO
              MA%MP(3) = 1
              MA%MP(1) = 1
              RETURN
          ENDIF
      ENDIF

      IF (Y < ONE) THEN
          IF (Y*XBASE > Y) THEN
  120         K = K - 1
              Y = Y*XBASE
              IF (Y < ONE) GO TO 120
              IF (Y > ONE) THEN
                  K = K + 1
                  Y = Y/XBASE
                  MA%MP(2) = K
                  GO TO 140
              ENDIF
          ELSE
              KFLAG = -4
              CALL FMWARN
              DO J = 1, N1
                 MA%MP(J+1) = 0
              ENDDO
              MA%MP(2) = MUNKNO
              MA%MP(3) = 1
              MA%MP(1) = 1
              RETURN
          ENDIF
      ENDIF

  130 MA%MP(2) = K + 1
      MA%MP(3) = 1
      DO J = 3, N1
         MA%MP(J+1) = 0
      ENDDO
      GO TO 160

!             Build the rest of the number.

  140 DO J = 2, NE
         Y = Y*XBASE
         MK = AINT(Y)
         YT = -MK
         CALL FMDBL(Y,YT,Y2)
         Y = Y2
         MA%MP(J+1) = MK
         IF (J >= N1) GO TO 150
      ENDDO
      K = NE + 1
      DO J = K, N1
         MA%MP(J+1) = 0
      ENDDO

!             Normalize.

  150 IF (ABS(MA%MP(3)) >= MBASE) THEN
          K = N1 + 1
          DO J = 3, N1
             K = K - 1
             MA%MP(K+1) = MA%MP(K)
          ENDDO
          MN = AINT (MA%MP(3)/MBASE)
          MA%MP(4) = MA%MP(3) - MN*MBASE
          MA%MP(3) = MN
          MA%MP(2) = MA%MP(2) + 1
          GO TO 160
      ENDIF

      IF (MA%MP(3) == 0) THEN
          DO J = 2, NDIG
             MA%MP(J+1) = MA%MP(J+2)
          ENDDO
          MA%MP(2) = MA%MP(2) - 1
          MA%MP(N1+1) = 0
      ENDIF

  160 MA%MP(1) = 1
      IF (X < 0.0 .AND. MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0) MA%MP(1) = -1
      RETURN
      END SUBROUTINE FMDM

      SUBROUTINE FMDM2(X,MA)

!  Internal routine for converting double precision to multiple precision.  Called by FMDP2M.

      USE ModLib_FMVALS
      IMPLICIT NONE

      DOUBLE PRECISION :: X
      TYPE(MULTI) :: MA
      DOUBLE PRECISION :: F1,F2,Y,Y1,Y2,TWO20
      INTEGER :: J,J1,J2,JD,JEXP,K,KEXP,L,NDSAVE
      INTENT (IN) :: X
      INTENT (INOUT) :: MA
      TYPE(MULTI) :: MXY(4)

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

!             Increase the working precision.

      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = MAX(NGRD21,1)
          NDIG = MAX(NDIG+K,2)
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0

!             Special case for X = 0.

      IF (X == 0.0D0) THEN
          DO J = 1, NDSAVE+1
             MA%MP(J+1) = 0
          ENDDO
          GO TO 140
      ENDIF

!             Check for X = + or - Infinity, or NaN.  Return unknown if so.

      IF (X > HUGE(X) .OR. X < -HUGE(X) .OR. (.NOT.(X == X))) THEN
          DO J = 2, NDSAVE
             MA%MP(J+2) = 0
          ENDDO
          KFLAG = -4
          MA%MP(2) = MUNKNO
          MA%MP(3) = 1
          MA%MP(1) = 1
          CALL FMWARN
          NDIG = NDSAVE
          RETURN
      ENDIF

!             Special case for MBASE = 2.

      IF (MBASE == 2 .AND. RADIX(X) == 2) THEN
          NDIG = MAX(NDIG,DIGITS(X))
          Y = FRACTION(ABS(X))
          CALL FMI2M(0,MXY(4))
          DO J = 1, MIN(DIGITS(X),NDIG)
             Y = Y + Y
             MXY(4)%MP(J+2) = INT(Y)
             Y = Y - INT(Y)
          ENDDO
          MXY(4)%MP(2) = EXPONENT(X)
          CALL FMEQU(MXY(4),MA,NDIG,NDSAVE)
          GO TO 140
      ENDIF

!             Special case for MBASE = 10**L.

      K = MBASE
      L = 0
      DO
         IF (MOD(K,10) == 0) THEN
             L = L + 1
             K = K/10
             IF (K == 1) EXIT
         ELSE
             L = 0
             EXIT
         ENDIF
      ENDDO
      IF (L > 0) THEN
          NDIG = MAX(NDIG,INT(DIGITS(X)*0.30103/L)+1)
          Y = FRACTION(ABS(X))
          CALL FMI2M(0,MXY(4))
          DO J = 1, NDIG

!             Multiply by 10**L to get the next digit in base MBASE.
!             To avoid any rounding errors in double precision, do each multiply by 10 as
!             one multiply by 8 and one by 2, and keep two integer and two fraction results.
!             So 10*y is broken into 8*y + 2*y, since there will be no rounding with either
!             term in double precision on a binary machine.

             JD = 0
             DO K = 1, L
                Y1 = 8*Y
                Y2 = 2*Y
                J1 = Y1
                J2 = Y2
                F1 = Y1 - J1
                F2 = Y2 - J2
                JD = 10*JD + J1 + J2
                Y = F1 + F2
                IF (Y >= 1.0D0) THEN
                    JD = JD + 1
                    Y = Y - 1
                ENDIF
             ENDDO
             MXY(4)%MP(J+2) = JD
             IF (Y == 0) EXIT
          ENDDO
          K = INTMAX
          IF (MAXINT/MBASE < K) K = MAXINT/MBASE
          K = K/2
          J2 = 1
          JEXP = EXPONENT(X)
          DO J = 1, ABS(JEXP)
             J2 = 2*J2
             IF (J2 >= K .OR. J == ABS(JEXP)) THEN
                 IF (JEXP > 0) THEN
                     CALL FMMPYI_R1(MXY(4),J2)
                 ELSE
                     CALL FMDIVI_R1(MXY(4),J2)
                 ENDIF
                 J2 = 1
             ENDIF
          ENDDO
          CALL FMEQU(MXY(4),MA,NDIG,NDSAVE)
          GO TO 140
      ENDIF

      Y = ABS(X)
      TWO20 = 1048576.0D0

!             If this power of two is not representable at the current base and precision, use a
!             smaller one.

      IF (INT(NDIG*ALOGM2) < 20) THEN
          K = INT(NDIG*ALOGM2)
          TWO20 = 1.0D0
          DO J = 1, K
             TWO20 = TWO20*2.0D0
          ENDDO
      ENDIF

      KEXP = 0
      IF (Y > TWO20) THEN
  110     Y = Y/TWO20
          KEXP = KEXP + 1
          IF (Y > TWO20) GO TO 110
      ELSE IF (Y < 1.0D0) THEN
  120     Y = Y*TWO20
          KEXP = KEXP - 1
          IF (Y < 1.0D0) GO TO 120
      ENDIF

      K = INT(TWO20)
      CALL FMI2M(K,MXY(3))
      K = INT(Y)
      CALL FMI2M(K,MXY(1))
      Y = (Y-DBLE(K))*TWO20
      JEXP = 0

  130 K = INT(Y)
      CALL FMI2M(K,MXY(2))
      CALL FMMPY_R1(MXY(1),MXY(3))
      JEXP = JEXP + 1
      CALL FMADD_R1(MXY(1),MXY(2))
      Y = (Y-DBLE(K))*TWO20
      IF (JEXP <= 1000 .AND. Y /= 0.0D0) GO TO 130

      K = KEXP - JEXP
      IF (K >= 0) THEN
          IF (K == 0) THEN
              CALL FMEQ(MXY(1),MXY(4))
          ELSE IF (K == 1) THEN
              CALL FMMPY(MXY(1),MXY(3),MXY(4))
          ELSE IF (K == 2) THEN
              CALL FMSQR(MXY(3),MXY(2))
              CALL FMMPY(MXY(1),MXY(2),MXY(4))
          ELSE
              CALL FMIPWR(MXY(3),K,MXY(2))
              CALL FMMPY(MXY(1),MXY(2),MXY(4))
          ENDIF
      ELSE
          IF (K == -1) THEN
              CALL FMDIV(MXY(1),MXY(3),MXY(4))
          ELSE IF (K == -2) THEN
              CALL FMSQR(MXY(3),MXY(2))
              CALL FMDIV(MXY(1),MXY(2),MXY(4))
          ELSE
              CALL FMIPWR(MXY(3),-K,MXY(2))
              CALL FMDIV(MXY(1),MXY(2),MXY(4))
          ENDIF
      ENDIF
      CALL FMEQU(MXY(4),MA,NDIG,NDSAVE)

  140 MA%MP(1) = 1
      IF (X < 0.0 .AND. MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0) MA%MP(1) = -1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE FMDM2

      SUBROUTINE FMDP2M(X,MA)

!  MA = X

!  Convert a double precision floating point number to FM format.

!  This version tries to convert the double precision machine number to FM with accuracy of nearly
!  full FM precision. If conversion to FM with approximately double precision accuracy is good
!  enough, FMDPM is faster and uses less scratch space.

!  This routine assumes the machine's base for double precision is a power of two.

      USE ModLib_FMVALS
      IMPLICIT NONE

      DOUBLE PRECISION :: X
      TYPE(MULTI) :: MA
      INTENT (IN) :: X
      INTENT (INOUT) :: MA
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMDP2M'
      IF (NTRACE /= 0) CALL FMNTRR(2,X,1)

      CALL FMDM2(X,MA)

      IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMDP2M

      SUBROUTINE FMDPM(X,MA)

!  MA = X

!  Convert a double precision floating point number to FM format.

!  In general, the relative accuracy of the FM number returned is only the relative accuracy of a
!  machine precision number.  This may be true even if X can be represented exactly in the machine
!  floating point number system.

!  This version is faster than FMDP2M, but often less accurate.

      USE ModLib_FMVALS
      IMPLICIT NONE

      DOUBLE PRECISION :: X
      TYPE(MULTI) :: MA

      DOUBLE PRECISION :: Y,YT
      INTEGER :: J,K
      INTENT (IN) :: X
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMDPM'
      IF (NTRACE /= 0) CALL FMNTRR(2,X,1)

!             Check for X = + or - Infinity, or NaN.  Return unknown if so.

      IF (X > HUGE(X) .OR. X < -HUGE(X) .OR. (.NOT.(X == X))) THEN
          DO J = 2, NDIG
             MA%MP(J+2) = 0
          ENDDO
          KFLAG = -4
          MA%MP(2) = MUNKNO
          MA%MP(3) = 1
          MA%MP(1) = 1
          CALL FMWARN
          GO TO 110
      ENDIF

!             Check to see if X is exactly a small integer.  If so, converting as an integer is
!             better.  Also see if X is exactly a small integer divided by a small power of two.

      Y = 1048576.0D0
      IF (ABS(X) < Y) THEN
          K = INT(X)
          Y = K
          IF (Y == X) THEN
              CALL FMIM(K,MA)
              GO TO 110
          ENDIF
      ENDIF
      IF (ABS(X) < 1.0D0) THEN
          Y = 4096.0D0*X
          K = INT(Y)
          YT = K
          IF (Y == YT) THEN
              CALL FMIM(K,MA)
              CALL FMDIVI_R1(MA,4096)
              GO TO 110
          ENDIF
      ENDIF

      CALL FMDM(X,MA)

  110 IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMDPM

      SUBROUTINE FMENTR(NROUTN,MA,MB,NARGS,KNAM,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)

!  Do the argument checking and increasing of precision and overflow threshold upon entry to an
!  FM routine.

!  NROUTN - routine name of calling routine
!  MA     - first input argument
!  MB     - second input argument (optional)
!  NARGS  - number of input arguments
!  KNAM   - positive if the routine name is to be printed.
!  MC     - result argument
!  KRESLT - returned nonzero if the input arguments give the result
!           immediately (e.g., MA*0 or OVERFLOW*MB)
!  NDSAVE - saves the value of NDIG after NDIG is increased
!  MXSAVE - saves the value of MXEXP
!  KOVUN  - returned nonzero if an input argument is (+ or -) overflow
!           or underflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(9) :: NROUTN
      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: KNAM,NARGS,KRESLT,NDSAVE,KOVUN

      INTEGER :: K
      INTENT (IN) :: NROUTN,MA,MB,NARGS,KNAM
      INTENT (INOUT) :: MC,KRESLT,NDSAVE,MXSAVE,KOVUN
      TYPE(MULTI) :: MXY(2)


      NCALL = NCALL + 1
      NAMEST(NCALL) = NROUTN
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,NARGS,KNAM)
      CALL FMARGS(NROUTN,NARGS,MA,MB,KRESLT)

      IF (MBLOGS /= MBASE) CALL FMCONS
      KOVUN = 0
      IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
      IF (NARGS == 2) THEN
          IF (MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN) KOVUN = 1
      ENDIF

!             Increase the working precision.

      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = MAX(NGRD52-1,2)
          NDIG = MAX(NDIG+K,2)
      ENDIF

      IF (KRESLT /= 0) THEN
          IF (KRESLT == 9 .OR. KRESLT == 10 .OR. KRESLT >= 13) THEN
              IF (KRAD == 1) THEN
                  CALL FMPI(MXY(1))
              ELSE
                  CALL FMI2M(180,MXY(1))
              ENDIF
              IF (KRESLT <= 10) CALL FMDIVI_R1(MXY(1),2)
              IF (KRESLT >= 14) CALL FMDIVI_R1(MXY(1),4)
              IF ((KRESLT == 9 .OR. KRESLT == 14) .AND. MXY(1)%MP(2) /= MUNKNO .AND.  &
                  MXY(1)%MP(3) /= 0) MXY(1)%MP(1) = -MXY(1)%MP(1)
              CALL FMEQU(MXY(1),MC,NDIG,NDSAVE)
              NDIG = NDSAVE
              IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
              MXSAVE = MXEXP
              NCALL = NCALL - 1
              RETURN
          ENDIF

          NDIG = NDSAVE
          CALL FMRSLT(MA,MB,MC,KRESLT)
          IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
          MXSAVE = MXEXP
          NCALL = NCALL - 1
          RETURN
      ENDIF


!             Extend the overflow/underflow threshold.

      MXSAVE = MXEXP
      MXEXP = MXEXP2
      RETURN
      END SUBROUTINE FMENTR

      SUBROUTINE FMENTR2(NROUTN,MA,MB,NARGS,KNAM,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)

!  Do the argument checking and increasing of precision and overflow threshold upon entry to a
!  low-level FM routine.

!  NROUTN - routine name of calling routine
!  MA     - first input argument
!  MB     - second input argument (optional)
!  NARGS  - number of input arguments
!  KNAM   - positive if the routine name is to be printed.
!  MC     - result argument
!  KRESLT - returned nonzero if the input arguments give the result
!           immediately (e.g., MA*0 or OVERFLOW*MB)
!  NDSAVE - saves the value of NDIG after NDIG is increased
!  MXSAVE - saves the value of MXEXP
!  KOVUN  - returned nonzero if an input argument is (+ or -) overflow
!           or underflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(9) :: NROUTN
      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: KNAM,NARGS,KRESLT,NDSAVE,KOVUN

      INTEGER :: K
      INTENT (IN) :: NROUTN,MA,MB,NARGS,KNAM
      INTENT (INOUT) :: MC,KRESLT,NDSAVE,MXSAVE,KOVUN

      NCALL = NCALL + 1
      NAMEST(NCALL) = NROUTN
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,NARGS,KNAM)
      CALL FMARGS(NROUTN,NARGS,MA,MB,KRESLT)

      IF (MBLOGS /= MBASE) CALL FMCONS
      KOVUN = 0
      IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
      IF (NARGS == 2) THEN
          IF (MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN) KOVUN = 1
      ENDIF

!             Increase the working precision.

      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = MAX(NGRD52-1,2)
          NDIG = MAX(NDIG+K,2)
      ENDIF

      IF (KRESLT /= 0) THEN
          NDIG = NDSAVE
          CALL FMRSLT(MA,MB,MC,KRESLT)
          IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
          MXSAVE = MXEXP
          NCALL = NCALL - 1
          RETURN
      ENDIF


!             Extend the overflow/underflow threshold.

      MXSAVE = MXEXP
      MXEXP = MXEXP2
      RETURN
      END SUBROUTINE FMENTR2

      SUBROUTINE FMEQ(MA,MB)

!  MB = MA

!  This is the standard form of equality, where MA and MB both have precision NDIG.
!  Use FMEQU for assignments that also change precision.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      INTEGER :: J
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

!             If the user tries to change precision, one common mistake is to fail to increase the
!             size of all existing variables.  Check to make sure MA has a valid definition.

      IF (.NOT. ALLOCATED(MA%MP)) CALL FMINPUT_ERROR
      IF (SIZE(MA%MP) < NDIG+2) CALL FMINPUT_ERROR

!             Copy MA to MB.

      DO J = 1, NDIG+2
         MB%MP(J) = MA%MP(J)
      ENDDO

!             Check for overflow or underflow.

      IF (ABS(MB%MP(2)) > MXEXP) THEN
          IF (MB%MP(2) /= MUNKNO .OR. MB%MP(3) /= 1) THEN
              NCALL = NCALL + 1
              CALL FMTRAP(MB)
              NCALL = NCALL - 1
          ENDIF
          IF (MB%MP(2) == MUNKNO) KFLAG = -4
      ENDIF

      RETURN
      END SUBROUTINE FMEQ

      SUBROUTINE FMEQU(MA,MB,NDA,NDB)

!  Set MB (having NDB digits) equal to MA (having NDA digits).

!  If MB has less precision than MA the result is rounded to NDB digits.

!  If MB has more precision the result has zero digits padded on the right.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NDA,NDB
      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: M2,MBS,MKT
      INTEGER :: J,K,KB,L,N1
      INTENT (IN) :: MA,NDA,NDB
      INTENT (INOUT) :: MB

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' Error in FMEQU.'
          WRITE (KW,*) ' MA is not defined on input.'
          WRITE (KW,*) ' Call stack: '
          WRITE (KW,"(10(3X,A))") (NAMEST(J),J=1,NCALL)
          WRITE (KW,*) ' '
          STOP
      ENDIF
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDB+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDB+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDB+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             If the user tries to change precision, one common mistake is to fail to increase the
!             size of all existing variables.  Check to make sure MA has a valid definition.

      IF (.NOT. ALLOCATED(MA%MP)) CALL FMINPUT_ERROR
      IF (SIZE(MA%MP) < NDA+2) CALL FMINPUT_ERROR

!             Check for precision in range.

      IF (NDA < 1 .OR. NDB < 1) THEN
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMEQU'
          KFLAG = -1
          CALL FMWARN
          WRITE (KW,                                                     &
                 "(/' The two precisions in FMEQU were NDA =',I19,"  //  &
                  "' NDB =',I19/)"                                       &
                ) NDA,NDB
          DO J = 2, NDB
             MB%MP(J+2) = 0
          ENDDO
          KFLAG = -1
          MB%MP(2) = MUNKNO
          MB%MP(3) = 1
          MB%MP(1) = 1
          NCALL = NCALL - 1
          RETURN
      ENDIF
      MBS = MA%MP(1)
      MB%MP(1) = MBS

!             Check for special symbols.

      KFLAG = 0
      IF (ABS(MA%MP(2)) >= MEXPOV) THEN
          DO J = 2, NDB
             MB%MP(J+2) = 0
          ENDDO
          MB%MP(2) = MA%MP(2)
          MB%MP(3) = MA%MP(3)
          GO TO 150
      ENDIF

      IF (NDB == NDA) GO TO 130

      IF (NDB > NDA) GO TO 140

!             Round to NDB digits.

      N1 = NDB + 1
      DO J = 1, N1
         MB%MP(J+1) = MA%MP(J+1)
      ENDDO
      IF (KROUND == -1 .AND. NCALL <= 1) THEN
          IF (MA%MP(1) > 0) GO TO 150
          DO J = NDB+2, NDA+1
             IF (MA%MP(J+1) > 0) GO TO 110
          ENDDO
          GO TO 150
      ENDIF
      IF (KROUND == 2 .AND. NCALL <= 1) THEN
          IF (MA%MP(1) < 0) GO TO 150
          DO J = NDB+2, NDA+1
             IF (MA%MP(J+1) > 0) GO TO 110
          ENDDO
          GO TO 150
      ENDIF
      IF (KROUND == 0 .AND. NCALL <= 1) GO TO 150

      IF (INT(MBASE-AINT (MBASE/2)*2) /= 0) THEN
          M2 = AINT (MBASE/2)
          DO J = NDB+2, NDA+1
             IF (MA%MP(J+1) /= M2) EXIT
             IF (J == NDA+1) GO TO 110
          ENDDO
      ENDIF
      L = NDB + 2
      IF (2*(MA%MP(L+1)+1) < MBASE) GO TO 150
      M2 = 2
      IF (INT(MBASE-AINT (MBASE/M2)*M2) == 0) THEN
          IF (2*MA%MP(L+1) < MBASE) GO TO 150
          IF (2*MA%MP(L+1) == MBASE) THEN
              IF (L <= NDA) THEN
                  DO J = L, NDA
                     IF (MA%MP(J+2) > 0) GO TO 110
                  ENDDO
              ENDIF

!                       Round to even.

              IF (INT(MB%MP(N1+1)-AINT (MB%MP(N1+1)/M2)*M2) == 0) GO TO 150
          ENDIF
      ELSE
          IF (2*MA%MP(L+1)+1 == MBASE) THEN
              IF (L <= NDA) THEN
                  DO J = L, NDA
                     IF (2*(MA%MP(J+2)+1) < MBASE) GO TO 150
                     IF (2*MA%MP(J+2) > MBASE) GO TO 110
                  ENDDO
                  GO TO 150
              ENDIF
          ENDIF
      ENDIF

  110 MB%MP(NDB+2) = MB%MP(NDB+2) + 1

!             Check whether there was a carry in the rounded digit.

      KB = NDB + 1
      IF (KB >= 3) THEN
          K = KB + 1
          DO J = 3, KB
             K = K - 1
             IF (MB%MP(K+1) < MBASE) GO TO 120
             MKT = AINT (MB%MP(K+1)/MBASE)
             MB%MP(K) = MB%MP(K) + MKT
             MB%MP(K+1) = MB%MP(K+1) - MKT*MBASE
          ENDDO
      ENDIF

!             If there is a carry in the first digit then the exponent must be adjusted and the
!             number shifted right.

      IF (MB%MP(3) < MBASE) GO TO 120
      IF (KB >= 4) THEN
          K = KB + 1
          DO J = 4, KB
             K = K - 1
             MB%MP(K+1) = MB%MP(K)
          ENDDO
      ENDIF

      MKT = AINT (MB%MP(3)/MBASE)
      IF (KB >= 3) MB%MP(4) = MB%MP(3) - MKT*MBASE
      MB%MP(3) = MKT
      MB%MP(2) = MB%MP(2) + 1

  120 IF (MBS < 0 .AND. MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0) MB%MP(1) = -1
      GO TO 150

!             MA and MB have the same precision.

  130 DO J = 1, NDA+1
         MB%MP(J+1) = MA%MP(J+1)
      ENDDO
      GO TO 150

!             Extend to NDB digits by padding with zeros.

  140 DO J = 1, NDA+1
         MB%MP(J+1) = MA%MP(J+1)
      ENDDO
      DO J = NDA+2, NDB+1
         MB%MP(J+1) = 0
      ENDDO

!             Check for overflow or underflow.

  150 IF (ABS(MB%MP(2)) > MXEXP) THEN
          IF (MB%MP(2) /= MUNKNO .OR. MB%MP(3) /= 1) THEN
              IF (MB%MP(2) > MXEXP+1) THEN
                  IF (MB%MP(1) > 0) THEN
                      DO J = 2, NDB
                         MB%MP(J+2) = 0
                      ENDDO
                      MB%MP(2) = MEXPOV
                      MB%MP(3) = 1
                      MB%MP(1) = 1
                  ELSE
                      DO J = 2, NDB
                         MB%MP(J+2) = 0
                      ENDDO
                      MB%MP(2) = MEXPOV
                      MB%MP(3) = 1
                      MB%MP(1) = -1
                  ENDIF
                  KFLAG = -5
              ENDIF
              IF (MB%MP(2) < -MXEXP) THEN
                  IF (MB%MP(1) > 0) THEN
                      DO J = 2, NDB
                         MB%MP(J+2) = 0
                      ENDDO
                      MB%MP(2) = MEXPUN
                      MB%MP(3) = 1
                      MB%MP(1) = 1
                  ELSE
                      DO J = 2, NDB
                         MB%MP(J+2) = 0
                      ENDDO
                      MB%MP(2) = MEXPUN
                      MB%MP(3) = 1
                      MB%MP(1) = -1
                  ENDIF
                  KFLAG = -6
              ENDIF
          ENDIF
          IF (MB%MP(2) == MUNKNO) KFLAG = -4
      ENDIF

      RETURN
      END SUBROUTINE FMEQU

      SUBROUTINE FMEQU_R1(MA,NDA,NDB)

!  Change precision of MA from NDA digits on input to NDB digits on output.

!  If NDB is less than NDA the result is rounded to NDB digits.

!  If NDB is greater than NDA the result has zero digits padded on the right.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: NDA,NDB

      REAL (KIND(1.0D0)) :: M2,MBS,MKT
      REAL (KIND(1.0D0)), DIMENSION(:), ALLOCATABLE :: MA_COPY
      INTEGER :: J,K,KB,L,N1
      INTENT (INOUT) :: MA
      INTENT (IN) :: NDA,NDB

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' Error in FMEQU_R1.'
          WRITE (KW,*) ' MA is not defined on input.'
          WRITE (KW,*) ' Call stack: '
          WRITE (KW,"(10(3X,A))") (NAMEST(J),J=1,NCALL)
          WRITE (KW,*) ' '
          STOP
      ENDIF
      IF (SIZE(MA%MP) < NDB+2) THEN
          ALLOCATE(MA_COPY(NDA+2),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          DO J = 1, NDA+2
             MA_COPY(J) = MA%MP(J)
          ENDDO
          IF (.NOT. ALLOCATED(MA%MP)) THEN
              ALLOCATE(MA%MP(NDB+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MA%MP) < NDB+2) THEN
              DEALLOCATE(MA%MP)
              ALLOCATE(MA%MP(NDB+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          DO J = 1, NDA+2
             MA%MP(J) = MA_COPY(J)
          ENDDO
          DEALLOCATE(MA_COPY)
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             If the user tries to change precision, one common mistake is to fail to increase the
!             size of all existing variables.  Check to make sure MA has a valid definition.

      IF (.NOT. ALLOCATED(MA%MP)) CALL FMINPUT_ERROR
      IF (SIZE(MA%MP) < NDA+2) CALL FMINPUT_ERROR

!             Check for precision in range.

      IF (NDA < 1 .OR. NDB < 1) THEN
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMEQU'
          KFLAG = -1
          CALL FMWARN
          WRITE (KW,                                                     &
                 "(/' The two precisions in FMEQU were NDA =',I19,"  //  &
                  "' NDB =',I19/)"                                       &
                ) NDA,NDB
          DO J = 2, NDB
             MA%MP(J+2) = 0
          ENDDO
          KFLAG = -1
          MA%MP(2) = MUNKNO
          MA%MP(3) = 1
          MA%MP(1) = 1
          NCALL = NCALL - 1
          RETURN
      ENDIF
      MBS = MA%MP(1)

!             Check for special symbols.

      KFLAG = 0
      IF (ABS(MA%MP(2)) >= MEXPOV) THEN
          DO J = 2, NDB
             MA%MP(J+2) = 0
          ENDDO
          GO TO 140
      ENDIF

      IF (NDB == NDA) GO TO 140

      IF (NDB > NDA) GO TO 130

!             Round to NDB digits.

      N1 = NDB + 1
      IF (KROUND == -1 .AND. NCALL <= 1) THEN
          IF (MA%MP(1) > 0) GO TO 140
          DO J = NDB+2, NDA+1
             IF (MA%MP(J+1) > 0) GO TO 110
          ENDDO
          GO TO 140
      ENDIF
      IF (KROUND == 2 .AND. NCALL <= 1) THEN
          IF (MA%MP(1) < 0) GO TO 140
          DO J = NDB+2, NDA+1
             IF (MA%MP(J+1) > 0) GO TO 110
          ENDDO
          GO TO 140
      ENDIF
      IF (KROUND == 0 .AND. NCALL <= 1) GO TO 140

      IF (INT(MBASE-AINT (MBASE/2)*2) /= 0) THEN
          M2 = AINT (MBASE/2)
          DO J = NDB+2, NDA+1
             IF (MA%MP(J+1) /= M2) EXIT
             IF (J == NDA+1) GO TO 110
          ENDDO
      ENDIF
      L = NDB + 2
      IF (2*(MA%MP(L+1)+1) < MBASE) GO TO 140
      M2 = 2
      IF (INT(MBASE-AINT (MBASE/M2)*M2) == 0) THEN
          IF (2*MA%MP(L+1) < MBASE) GO TO 140
          IF (2*MA%MP(L+1) == MBASE) THEN
              IF (L <= NDA) THEN
                  DO J = L, NDA
                     IF (MA%MP(J+2) > 0) GO TO 110
                  ENDDO
              ENDIF

!                       Round to even.

              IF (INT(MA%MP(N1+1)-AINT (MA%MP(N1+1)/M2)*M2) == 0) GO TO 140
          ENDIF
      ELSE
          IF (2*MA%MP(L+1)+1 == MBASE) THEN
              IF (L <= NDA) THEN
                  DO J = L, NDA
                     IF (2*(MA%MP(J+2)+1) < MBASE) GO TO 140
                     IF (2*MA%MP(J+2) > MBASE) GO TO 110
                  ENDDO
                  GO TO 140
              ENDIF
          ENDIF
      ENDIF

  110 MA%MP(NDB+2) = MA%MP(NDB+2) + 1
      MA%MP(NDB+3) = 0

!             Check whether there was a carry in the rounded digit.

      KB = NDB + 1
      IF (KB >= 3) THEN
          K = KB + 1
          DO J = 3, KB
             K = K - 1
             IF (MA%MP(K+1) < MBASE) GO TO 120
             MKT = AINT (MA%MP(K+1)/MBASE)
             MA%MP(K) = MA%MP(K) + MKT
             MA%MP(K+1) = MA%MP(K+1) - MKT*MBASE
          ENDDO
      ENDIF

!             If there is a carry in the first digit then the exponent must be adjusted and the
!             number shifted right.

      IF (MA%MP(3) < MBASE) GO TO 120
      IF (KB >= 4) THEN
          K = KB + 1
          DO J = 4, KB
             K = K - 1
             MA%MP(K+1) = MA%MP(K)
          ENDDO
      ENDIF

      MKT = AINT (MA%MP(3)/MBASE)
      IF (KB >= 3) MA%MP(4) = MA%MP(3) - MKT*MBASE
      MA%MP(3) = MKT
      MA%MP(2) = MA%MP(2) + 1

  120 IF (MBS < 0 .AND. MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0) MA%MP(1) = -1
      GO TO 140

!             Extend to NDB digits by padding with zeros.

  130 DO J = NDA+2, NDB+1
         MA%MP(J+1) = 0
      ENDDO

!             Check for overflow or underflow.

  140 IF (ABS(MA%MP(2)) > MXEXP) THEN
          IF (MA%MP(2) /= MUNKNO .OR. MA%MP(3) /= 1) THEN
              IF (MA%MP(2) > MXEXP+1) THEN
                  IF (MA%MP(1) > 0) THEN
                      DO J = 2, NDB
                         MA%MP(J+2) = 0
                      ENDDO
                      MA%MP(2) = MEXPOV
                      MA%MP(3) = 1
                      MA%MP(1) = 1
                  ELSE
                      DO J = 2, NDB
                         MA%MP(J+2) = 0
                      ENDDO
                      MA%MP(2) = MEXPOV
                      MA%MP(3) = 1
                      MA%MP(1) = -1
                  ENDIF
                  KFLAG = -5
              ENDIF
              IF (MA%MP(2) < -MXEXP) THEN
                  IF (MA%MP(1) > 0) THEN
                      DO J = 2, NDB
                         MA%MP(J+2) = 0
                      ENDDO
                      MA%MP(2) = MEXPUN
                      MA%MP(3) = 1
                      MA%MP(1) = 1
                  ELSE
                      DO J = 2, NDB
                         MA%MP(J+2) = 0
                      ENDDO
                      MA%MP(2) = MEXPUN
                      MA%MP(3) = 1
                      MA%MP(1) = -1
                  ENDIF
                  KFLAG = -6
              ENDIF
          ENDIF
          IF (MA%MP(2) == MUNKNO) KFLAG = -4
      ENDIF

      RETURN
      END SUBROUTINE FMEQU_R1

      SUBROUTINE FMEXIT(MT,MC,NDSAVE,MXSAVE,KOVUN)

!  Upon exit from an FM routine the result MT (having precision NDIG) is rounded and returned in MC
!  (having precision NDSAVE).  The values of NDIG, MXEXP, and KACCSW are restored.  KOVUN is nonzero
!  if one of the routine's input arguments was overflow or underflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MT,MC
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: NDSAVE,KOVUN

      INTEGER :: KFSAVE,KWRNSV
      INTENT (IN) :: MT,NDSAVE,MXSAVE,KOVUN
      INTENT (INOUT) :: MC

      KWRNSV = KWARN
      KWARN = 0
      MXEXP = MXSAVE
      KFSAVE = KFLAG
      CALL FMEQU(MT,MC,NDIG,NDSAVE)
      IF (KFLAG /= -5 .AND. KFLAG /= -6) KFLAG = KFSAVE
      NDIG = NDSAVE
      KWARN = KWRNSV
      IF (KFLAG == 1) KFLAG = 0
      IF ((MC%MP(2) == MUNKNO .AND. KFLAG /= -9)     &
         .OR. (MC%MP(2) == MEXPUN .AND. KOVUN == 0)  &
         .OR. (MC%MP(2) == MEXPOV .AND. KOVUN == 0)) CALL FMWARN
      IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMEXIT

      SUBROUTINE FMEXP(MA,MB)

!  MB = EXP(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      CHARACTER(155) :: STRING
      REAL (KIND(1.0D0)) :: M1,MA1,MA2,MAS,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,K,KL,KOVUN,KR_RETRY,KRESLT,KT,KWRNSV,NDMB,NDSAVE,NDSV,NMETHD
      REAL :: XMA,XOV
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (NDIG > 1000) THEN
          NDSAVE = NDIG
          NDIG = NDIG + 100 + NDIG/100
          IF (.NOT. ALLOCATED(MWA%MP)) THEN
              ALLOCATE(MWA%MP(2*NDIG + 30),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MWA%MP) < 2*NDIG + 30) THEN
              DEALLOCATE(MWA%MP)
              ALLOCATE(MWA%MP(2*NDIG + 30),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          NDIG = NDSAVE
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMEXP'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMI2M(1,MXY(1))
          CALL FMADD(MXY(1),MA,MB)
          KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMEXP'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0) THEN
          CALL FMENTR('FMEXP    ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMEXP'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      MA1 = MA%MP(2)
      MA2 = MA%MP(3)
      MAS = MA%MP(1)

      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)

!             Check for obvious underflow or overflow.
!             XOV is LN(LN(slightly above overflow))
!             XMA is LN(LN(EXP(MA))) approximately.

      XOV = LOG(1.01*REAL(MXEXP)) + LOG(ALOGMB)
      M1 = 1
      XMA = LOG(REAL(MAX(ABS(MA2),M1))) - ALOGMB + REAL(MA1)*ALOGMB

  120 IF (XMA >= XOV) THEN
          CALL FMIM(0,MXY(2))
          IF (MAS > 0) THEN
              KFLAG = -5
              CALL FMST2M('OVERFLOW',MXY(2))
          ELSE
              KFLAG = -6
              CALL FMST2M('UNDERFLOW',MXY(2))
          ENDIF
          CALL FMEQU(MXY(2),MB,NDIG,NDSAVE)
          NDIG = NDSAVE
          MXEXP = MXSAVE
          CALL FMWARN
          IF (NTRACE /= 0) CALL FMNTR(1,MB,MB,1,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

!             Split MA into integer and fraction parts.  Work with a positive argument.
!             MXY(1) = integer part of ABS(MA)
!             MXY(2) = fraction part of ABS(MA)

      MXY(2)%MP(1) = 1
      CALL FMINT(MXY(2),MXY(1))
      CALL FMSUB_R1(MXY(2),MXY(1))

!             If the integer part is not zero, use FMIPWR to compute E**(MXY(1)).  If MXY(1) is too
!             large to represent as a one word integer, the definition of MXEXP insures that
!             E**(MXY(1)) overflows or underflows.

      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(1),KT)
      KWARN = KWRNSV
      IF (KFLAG /= 0) THEN
          XMA = XOV
          GO TO 120
      ENDIF
      IF (KT > 0) THEN

!             Compute IEXTRA, the number of extra digits required to get EXP(KT) correct to the
!             current precision.

          IEXTRA = INT(LOG(REAL(KT))/ALOGMB + 0.5)
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
          ENDIF
          NDIG = NDIG + IEXTRA

!             Check whether the current precision of e is large enough.

          IF (MBSE /= MBASE .OR. NDIG > NDIGE) THEN
              NDMB = INT(150.0*2.302585/ALOGMB)
              IF (NDMB >= NDIG) THEN
                  STRING = '2.718281828459045235360287471352662497757247'//  &
                  '09369995957496696762772407663035354759457138217852516'//  &
                  '6427427466391932003059921817413596629043572900334295261'
                  CALL FMST2M(STRING,MESAV)
                  MBSE = MBASE
                  NDIGE = NDIG
                  IF (ABS(MESAV%MP(2)) > 10) NDIGE = 0
              ELSE
                  NDSV = NDIG
                  NDIG = NDIG + 2 + NDIG/100
                  CALL FMI2M(1,MESAV)
                  CALL FMEXP2(MESAV,MXY(4))
                  CALL FMEQ(MXY(4),MESAV)
                  MBSE = MBASE
                  NDIGE = NDIG
                  IF (ABS(MESAV%MP(2)) > 10) NDIGE = 0
                  NDIG = NDSV
              ENDIF
          ENDIF
      ENDIF

!             Now do the fraction part of MA and combine the results.

      KWRNSV = KWARN
      KWARN = 0
      NMETHD = 1
      IF (NDIG > 50) NMETHD = 2
      IF (MXY(2)%MP(3) /= 0 .AND. KT > 0 .AND. NMETHD == 1) THEN
          CALL FMEXP2(MXY(2),MXY(4))
          CALL FMIPWR(MESAV,KT,MXY(3))
          CALL FMMPY(MXY(4),MXY(3),MXY(2))
      ELSE IF (MXY(2)%MP(3) /= 0 .AND. KT == 0 .AND. NMETHD == 1) THEN
          CALL FMEXP2(MXY(2),MXY(4))
          CALL FMEQ(MXY(4),MXY(2))
      ELSE IF (MXY(2)%MP(3) /= 0 .AND. KT > 0 .AND. NMETHD == 2) THEN
          NDSV = NDIG
          NDIG = NDIG + NGRD21
          CALL FMEQU_R1(MXY(2),NDSV,NDIG)
          IF (MXY(2)%MP(2) >= 0) THEN
              CALL FMCSH2(MXY(2),MXY(4))
              CALL FMSQR(MXY(4),MXY(3))
              CALL FMI2M(-1,MXY(1))
              CALL FMADD_R1(MXY(3),MXY(1))
              CALL FMSQRT_R1(MXY(3))
              CALL FMADD(MXY(4),MXY(3),MXY(2))
          ELSE
              CALL FMSNH2(MXY(2),MXY(4))
              CALL FMSQR(MXY(4),MXY(3))
              CALL FMI2M(1,MXY(1))
              CALL FMADD_R1(MXY(3),MXY(1))
              CALL FMSQRT_R1(MXY(3))
              CALL FMADD(MXY(4),MXY(3),MXY(2))
          ENDIF
          NDIG = NDSV
          CALL FMIPWR(MESAV,KT,MXY(3))
          CALL FMMPY_R1(MXY(2),MXY(3))
      ELSE IF (MXY(2)%MP(3) /= 0 .AND. KT == 0 .AND. NMETHD == 2) THEN
          NDSV = NDIG
          NDIG = NDIG + NGRD21
          CALL FMEQU_R1(MXY(2),NDSV,NDIG)
          IF (MXY(2)%MP(2) >= 0) THEN
              CALL FMCSH2(MXY(2),MXY(4))
              CALL FMSQR(MXY(4),MXY(3))
              CALL FMI2M(-1,MXY(1))
              CALL FMADD_R1(MXY(3),MXY(1))
              CALL FMSQRT_R1(MXY(3))
              CALL FMADD(MXY(4),MXY(3),MXY(2))
          ELSE
              CALL FMSNH2(MXY(2),MXY(4))
              CALL FMSQR(MXY(4),MXY(3))
              CALL FMI2M(1,MXY(1))
              CALL FMADD_R1(MXY(3),MXY(1))
              CALL FMSQRT_R1(MXY(3))
              CALL FMADD(MXY(4),MXY(3),MXY(2))
          ENDIF
          NDIG = NDSV
      ELSE IF (MXY(2)%MP(3) == 0 .AND. KT > 0) THEN
          CALL FMIPWR(MESAV,KT,MXY(2))
      ELSE
          CALL FMI2M(1,MXY(2))
      ENDIF

!             Invert if MA was negative.

      IF (MAS < 0) THEN
          CALL FMI2M(1,MXY(1))
          CALL FMDIV_R2(MXY(1),MXY(2))
      ENDIF
      KWARN = KWRNSV

!             Round the result and return.

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(2),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMEXP

      SUBROUTINE FMEXP2(MA,MB)

!  MB = EXP(MA)

!  Internal exponential routine (called with 0 < MA <= 1).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAXV
      REAL (KIND(0.0D0)) :: X1,X2,X3,F1,F2,F3,PI
      INTEGER :: J,J2,K,K2,KEXP,KTWO,L,L2,N2,NBIG,NBOT,NDSAV1,NDSAVE,NTERM,NTOP
      REAL :: ALOG2,ALOGT,B,T,TJ,XN
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      NDSAVE = NDIG
      IF (MA%MP(2) == 1) THEN

!             Here the special case EXP(1.0) is computed.

          T = NDIG
          XN = T*ALOGMB/LOG(T)
          K = INT(LOG(XN)/ALOGMB)
          NDIG = MAX(NDIG+K,2)
          NDSAV1 = NDIG

          IF (NDSAVE < 100) THEN

!             If precision is not very high, use the direct series  e = 1/0! + 1/1! + 1/2! + ...
!             Do as much of the work as possible using small integers to minimize the number of
!             FM calls.  Reduce NDIG while computing each term in the sum as the terms get smaller.

              CALL FMI2M(2,MXY(2))
              CALL FMI2M(1,MXY(1))
              J = 2
              NBIG = INT(MXBASE)

  110         NTOP = 1
              NBOT = J
  120         IF (NBOT > NBIG/(J+1)) GO TO 130
              J = J + 1
              NTOP = J*NTOP + 1
              NBOT = J*NBOT
              GO TO 120

  130         CALL FMCSDIVI_R1(MXY(1),NBOT)
              IF (NTOP > 1) THEN
                  CALL FMCSMPYI(MXY(1),NTOP,MXY(3))
                  NDIG = NDSAV1
                  CALL FMADD_R1(MXY(2),MXY(3))
                  NDIG = NDSAV1 - INT(MXY(2)%MP(2)-MXY(3)%MP(2))
              ELSE
                  NDIG = NDSAV1
                  CALL FMADD_R1(MXY(2),MXY(1))
                  NDIG = NDSAV1 - INT(MXY(2)%MP(2)-MXY(1)%MP(2))
              ENDIF
              IF (NDIG < NGRD22) NDIG = NGRD22
              IF (KFLAG /= 1) THEN
                  J = J + 1
                  GO TO 110
              ENDIF
              NDIG = NDSAVE
              KFLAG = 0
              CALL FMEQU(MXY(2),MB,NDSAV1,NDSAVE)
          ELSE

!             If precision is high, use the binary splitting method for summing the direct series.

!             Determine K, the number of terms to sum in the series for e.

              X1 = 1.184*NDIG*DLOGMB/LOG(NDIG*DLOGMB) + 1.95*NDIG**0.777 + 10
              PI = ACOS(-1.0D0)
              F1 = NDIG*DLOGMB - (X1 + 0.5)*LOG(X1) + X1 - LOG(2.0*PI)/2 - 1/(12.0*X1)
              IF (F1 < 0) THEN
                  X2 = 0.9*X1
              ELSE
                  X2 = 1.1*X1
              ENDIF
              F2 = NDIG*DLOGMB - (X2 + 0.5)*LOG(X2) + X2 - LOG(2.0*PI)/2 - 1/(12.0*X2)
              DO J = 1, 5
                 X3 = X2 - F2*(X2 - X1)/(F2 - F1)
                 F3 = NDIG*DLOGMB - (X3 + 0.5)*LOG(X3) + X3 - LOG(2.0*PI)/2 - 1/(12.0*X3)
                 IF (ABS(X3-X2) < 0.1) EXIT
                 X1 = X2
                 F1 = F2
                 X2 = X3
                 F2 = F3
              ENDDO

              K = X3 + 10
              CALL FMEXP2_TQ(0,K,MXY(1),MXY(2))
              IF (MXY(1)%MP(2) >= NDIG .AND. MXY(2)%MP(2) >= NDIG) THEN
                  CALL FMDIV_R2(MXY(1),MXY(2))
                  NDIG = NDSAVE
                  KFLAG = 0
                  CALL FMEQU(MXY(2),MB,NDSAV1,NDSAVE)
              ELSE
                  CALL IMI2FM(MXY(1),MXY(3))
                  CALL IMI2FM(MXY(2),MXY(4))
                  CALL FMDIV(MXY(3),MXY(4),MXY(2))
                  NDIG = NDSAVE
                  KFLAG = 0
                  CALL FMEQU(MXY(2),MB,NDSAV1,NDSAVE)
              ENDIF
          ENDIF
          RETURN
      ENDIF

!             Here is the general case.  Compute EXP(MA) where 0 < MA < 1.

!             Use the direct series
!                  EXP(X) = 1 + X + X**2/2! + X**3/3! + ...

!             The argument will be halved K2 times before the series is summed.  The series will be
!             added as J2 concurrent series.

      B = REAL(MBASE)
      K = NGRD52
      T = MAX(NDIG-K,2)
      ALOG2 = REAL(DLOGTW)
      ALOGT = LOG(T)
      TJ = 0.87*(NDIG*ALOGMT)**0.3333 - 1.3
      J2 = INT(TJ)
      J2 = MAX(1,MIN(J2,LJSUMS))
      K2 = MAX(2,INT(2.0*(NDIG*ALOGMT)**0.3333 - 0.5))

      TJ = -(REAL(MA%MP(2))*ALOGMB+LOG(REAL(MA%MP(3))/B +  &
             REAL(MA%MP(4))/(B*B)))/ALOG2 - 0.3
      IF (TJ >= K2) THEN
          L = K2
      ELSE IF (TJ > 0) THEN
          L = INT(TJ)
      ELSE
          L = 0
      ENDIF
      K2 = K2 - L
      IF (K2 <= 0) THEN
          K2 = 0
          J2 = INT(.43*SQRT(T*ALOGMB/(ALOGT+REAL(L)*ALOG2)) + .33)
          J2 = MAX(1,MIN(J2,LJSUMS))
      ENDIF

      N2 = INT(T*ALOGMB/(ALOGT+REAL(L)*ALOG2))
      L2 = INT(LOG(REAL(N2)+2.0D0**K2)/ALOGMB)
      NDIG = NDIG + L2
      NDSAV1 = NDIG

!             Halve the argument K2 times.

      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      KTWO = 1
      MAXV = MXBASE/2
      IF (K2 > 0) THEN
          DO J = 1, K2
             KTWO = 2*KTWO
             IF (KTWO > MAXV) THEN
                 CALL FMCSDIVI_R1(MXY(1),KTWO)
                 KTWO = 1
             ENDIF
          ENDDO
          IF (KTWO > 1) CALL FMCSDIVI_R1(MXY(1),KTWO)
      ENDIF

!             Sum the series X + X**2/2! + X**3/3! + ....
!             Split into J2 concurrent sums and reduce NDIG while computing each term in the sum as
!             the terms get smaller.

      CALL FMEQ(MXY(1),MXY(2))
      NTERM = 1
      DO J = 1, J2
         CALL FMCSDIVI_R1(MXY(2),NTERM)
         NTERM = NTERM + 1
         CALL FMEQ(MXY(2),MJSUMS(J))
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 150
      CALL FMIPWR(MXY(1),J2,MXY(3))

  140 CALL FMCSMPY_R1(MXY(2),MXY(3))
      DO J = 1, J2
         CALL FMCSDIVI_R1(MXY(2),NTERM)
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(2))
         IF (KFLAG /= 0) GO TO 150
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(2)%MP(2))
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 1
      ENDDO
      GO TO 140

!             Put the J2 separate sums back together.

  150 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(3))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(3),MXY(1))
         CALL FMADD_R1(MXY(3),MJSUMS(J2-J+1))
      ENDDO

!             Reverse the effect of halving the argument to compute EXP(MA).

      NDIG = NDSAV1
      IF (K2 > 0) THEN
          IF (NDSAVE <= 20) THEN
              CALL FMI2M(2,MXY(1))
              DO J = 1, K2
                 KEXP = MXY(3)%MP(2)
                 IF (MBASE == 2 .OR. KEXP > 0) THEN
                     CALL FMADD(MXY(3),MXY(1),MXY(2))
                 ELSE
                     DO K = 1, 3-KEXP
                        MXY(2)%MP(K) = MXY(1)%MP(K)
                     ENDDO
                     DO K = 4-KEXP, NDIG+2
                        MXY(2)%MP(K) = MXY(3)%MP(K-1+KEXP)
                     ENDDO
                 ENDIF
                 CALL FMCSMPY_R1(MXY(3),MXY(2))
              ENDDO
          ELSE
              DO J = 1, K2
                 CALL FMSQR(MXY(3),MXY(2))
                 CALL FMADD(MXY(3),MXY(3),MXY(1))
                 CALL FMADD(MXY(2),MXY(1),MXY(3))
              ENDDO
          ENDIF
      ENDIF
      CALL FMI2M(1,MXY(1))
      CALL FMADD(MXY(1),MXY(3),MXY(2))

      CALL FMEQU(MXY(2),MB,NDSAV1,NDSAVE)
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE FMEXP2

      RECURSIVE SUBROUTINE FMEXP2_TQ(A,B,MT,MQ)

!  This routine does the binary splitting for computing the constant e.
!  When A is zero, e is approximated by MT/MQ.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MT,MQ
      INTEGER :: A,B
      INTENT (IN) :: A,B
      INTENT (INOUT) :: MT,MQ
      TYPE(MULTI) :: MXY(4)
      INTEGER :: J,KM,RESULT_SIZE
      REAL (KIND(0.0D0)) :: DA,DB

      DA = A
      DB = B

      IF (B-A < 25) THEN
          RESULT_SIZE = ( (DB+0.5D0)*LOG(DB+1) - DB + 1/(12*(DB+1)) -  &
                        ( (DA+0.5D0)*LOG(DA+1) - DA + 1/(12*(DA+1)) ) + 10 ) / DLOGMB + 8
          RESULT_SIZE = MAX(5,RESULT_SIZE)
          IF (.NOT. ALLOCATED(MT%MP)) THEN
              ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MT%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MT%MP)
              ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MQ%MP)) THEN
              ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MQ%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MQ%MP)
              ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(1)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(1)%MP)
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(2)%MP)) THEN
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(2)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(2)%MP)
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(3)%MP)) THEN
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(3)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(3)%MP)
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(4)%MP)) THEN
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(4)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(4)%MP)
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          CALL IMI2M(1,MT)
          CALL IMI2M(1,MXY(1))
          DO J = 0, B-A-2, 2
             CALL IMMPYI(MXY(1),B-J,MXY(2))
             CALL IMADD(MT,MXY(2),MXY(3))
             CALL IMMPYI(MXY(2),B-J-1,MXY(1))
             CALL IMADD(MXY(3),MXY(1),MT)
          ENDDO
          IF (MOD(B-A,2) == 1) THEN
              CALL IMMPYI(MXY(1),A+1,MXY(2))
              CALL IMADD(MT,MXY(2),MXY(3))
              CALL IMEQ(MXY(3),MT)
          ENDIF

          IF (A == 0) THEN
              CALL IMI2M(1,MQ)
          ELSE
              CALL IMI2M(A,MQ)
          ENDIF
          DO J = A+1, B-1, 2
             CALL IMMPYI(MQ,J,MXY(1))
             CALL IMMPYI(MXY(1),J+1,MQ)
          ENDDO
          IF (MOD(B-A+1,2) == 0) THEN
              CALL IMMPYI(MQ,B,MXY(1))
              CALL IMEQ(MXY(1),MQ)
          ENDIF
          GO TO 110
      ENDIF

      KM = A/2 + B/2 + MOD(A,2)*MOD(B,2)
      CALL FMEXP2_TQ(A,KM-1,MXY(1),MXY(2))
      CALL FMEXP2_TQ(KM,B,MXY(3),MXY(4))

      CALL IM_OR_FM_MPY(MXY(2),MXY(4),MQ)

      CALL IM_OR_FM_MPY(MXY(1),MXY(4),MXY(2))
      CALL IM_OR_FM_ADD(MXY(3),MXY(2),MT)

  110 RETURN
      END SUBROUTINE FMEXP2_TQ

      FUNCTION FMFI(N)

!  Format integers for trace output.

      IMPLICIT NONE
      CHARACTER(40) :: FMFI,TEMP
      INTEGER :: J,N

      FMFI = ' '
      WRITE (TEMP,*) N
      DO J = 1, 40
         IF (TEMP(J:J) /= ' ') THEN
             FMFI(1:41-J) = TEMP(J:40)
             RETURN
         ENDIF
      ENDDO

      END FUNCTION FMFI

      SUBROUTINE FMFLAG(K)

!  Return the internal condition variable KFLAG to the user.

      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: K
      K = KFLAG
      RETURN
      END SUBROUTINE FMFLAG

      SUBROUTINE FMFFT_INIT(ROOTS_OF_UNITY,N)

!  Initialize roots of unity.

      IMPLICIT NONE
      INTEGER :: N
      COMPLEX (KIND(0.0D0)) :: ROOTS_OF_UNITY(0:N-1), S, ST
      DOUBLE PRECISION :: PI, T
      INTEGER :: ITS, J, JS, JSTEP, K, L, LEVEL, NR, N_DEF

      IF (N <= 100000) THEN
          RETURN
      ELSE
          L = NINT(2*LOG10(DBLE(N))-8)
          K = 2**L
          DO J = L, 1, -1
             IF (MOD(N,K) == 0) THEN
                 LEVEL = J
                 EXIT
             ENDIF
             K = K / 2
             LEVEL = 1
          ENDDO
      ENDIF

      N_DEF = N / 2**LEVEL
      IF (N < N_DEF) N_DEF = N
      ITS = NINT( LOG( DBLE(N) / N_DEF ) / LOG(2.0D0) )

      PI = ACOS(-1.0D0)
      T = 2*PI/N
      ST = CMPLX(-2*SIN(PI/N)**2,SIN(2*PI/N), KIND(0.0D0) )
      NR = N/2
      JSTEP = 32
      DO K = 0, NR/JSTEP - 1
         JS = K*JSTEP
         S = CMPLX( COS(JS*T) , SIN(JS*T) , KIND(0.0D0) )
         DO J = 0, JSTEP - 1
            ROOTS_OF_UNITY(J+JS) = S
            S = S + ST*S
         ENDDO
      ENDDO
      K = (NR/JSTEP)*JSTEP
      DO J = K, NR-1
         ROOTS_OF_UNITY(J) = CMPLX( COS(J*T) , SIN(J*T) , KIND(0.0D0) )
      ENDDO
      L = NR - 1
      DO J = 2, ITS
         JSTEP = 2**(J-1)
         DO K = 0, NR-1, JSTEP
            L = L + 1
            ROOTS_OF_UNITY(L) = ROOTS_OF_UNITY(K)
         ENDDO
      ENDDO

      END SUBROUTINE FMFFT_INIT

      SUBROUTINE FMFFT(A,N,ROOTS_OF_UNITY,A2)

!  A is returned as the FFT of the input array A(1:N)

!  This is not a general fft subroutine.  It is designed to be called by FM's multiplication
!  routines, and may not give correct results for arbitrary N.

      IMPLICIT NONE
      INTEGER :: N
      COMPLEX (KIND(0.0D0)) :: A(N), A2(N), ROOTS_OF_UNITY(0:N-1)
      INTEGER :: H, ITS, J, JLISTS, JS, JSTEP, K, K1, K2, L, LEVEL, LG, NL
      INTEGER, ALLOCATABLE :: FIRST(:)

      IF (N <= 100000) THEN
          CALL FMFFT2(A,N,A2)
          RETURN
      ELSE
          L = NINT(2*LOG10(DBLE(N))-8)
          K = 2**L
          DO J = L, 1, -1
             IF (MOD(N,K) == 0) THEN
                 LEVEL = J
                 EXIT
             ENDIF
             K = K / 2
             LEVEL = 1
          ENDDO
      ENDIF

      NL = 2**LEVEL
      LG = N/NL
      ALLOCATE( FIRST(NL) )

      JLISTS = 1
      FIRST(1) = 1
      H = 1
      DO J = 1, LEVEL
         DO K = JLISTS, 1, -1
            FIRST(2*K-1) = FIRST(K)
            FIRST(2*K)   = FIRST(K) + H
         ENDDO
         H = 2*H
         JLISTS = 2*JLISTS
      ENDDO

      DO J = 1, NL
         DO K = 0, LG-1
            A2(1+K+(J-1)*LG) = A(FIRST(J)+K*H)
         ENDDO
      ENDDO

      DO J = 1, NL
         CALL FMFFT2(A2(1+(J-1)*LG),LG,A)
      ENDDO

      ITS = NINT( LOG( DBLE(NL) ) / LOG(2.0D0) )
      JSTEP = ITS + 1
      DO L = 2, ITS, 2

         JSTEP = JSTEP - 1
         JS = -1
         IF (JSTEP > 1) JS = (N / 2**(JSTEP-1)) * (2**(JSTEP-1) - 1) - 1
         DO K = 2, NL, 2
            K1 = (K-2)*LG
            K2 = K1 + LG
            DO J = 1, LG
               A(J+K1) = A2(J+K1) + ROOTS_OF_UNITY(J+JS) * A2(J+K2)
               A(J+K2) = A2(J+K1) - ROOTS_OF_UNITY(J+JS) * A2(J+K2)
            ENDDO
         ENDDO

         LG = 2 * LG
         NL = NL / 2
         JSTEP = JSTEP - 1
         JS = -1
         IF (JSTEP > 1) JS = (N / 2**(JSTEP-1)) * (2**(JSTEP-1) - 1) - 1
         DO K = 2, NL, 2
            K1 = (K-2)*LG
            K2 = K1 + LG
            DO J = 1, LG
               A2(J+K1) = A(J+K1) + ROOTS_OF_UNITY(J+JS) * A(J+K2)
               A2(J+K2) = A(J+K1) - ROOTS_OF_UNITY(J+JS) * A(J+K2)
            ENDDO
         ENDDO

         LG = 2 * LG
         NL = NL / 2

      ENDDO

      IF (MOD(ITS,2) == 0) THEN
          DO J = 1, N
             A(J) = A2(J)
          ENDDO
      ELSE
          JSTEP = JSTEP - 1
          JS = -1
          IF (JSTEP > 1) JS = (N / 2**(JSTEP-1)) * (2**(JSTEP-1) - 1) - 1
          DO K = 2, NL, 2
             K1 = (K-2)*LG
             K2 = K1 + LG
             DO J = 1, LG
                A(J+K1) = A2(J+K1) + ROOTS_OF_UNITY(J+JS) * A2(J+K2)
                A(J+K2) = A2(J+K1) - ROOTS_OF_UNITY(J+JS) * A2(J+K2)
             ENDDO
          ENDDO
      ENDIF

      DEALLOCATE( FIRST )

      END SUBROUTINE FMFFT

      SUBROUTINE FMFFT2(A,N,W)

!  Internal routine used during very high precision multiplication.

!  A is returned as the FFT of the input array A(1:N).
!  W is a scratch array.
!  This is a slightly modified version of a Fast Fourier Transform routine found at www.netlib.org.

      IMPLICIT NONE
      INTEGER :: N
      COMPLEX (KIND(0.0D0)) :: A(N),W(N),S,S1,S2,S3,S4,ST,T
      REAL (KIND(0.0D0)) :: PI
      INTEGER :: D,E,F,G,H,I,J,K,L,M,O
      INTEGER, PARAMETER :: P(25) = (/ 2,3,5,7,11,13,17,19,23,29,31,37,41,  &
                                      43,47,53,59,61,67,71,73,79,83,89,97 /)
      INTEGER, PARAMETER :: NP = 25
      M = N
      F = 0
      PI = ACOS(-1.0D0)
  110 IF ( M == 1 ) GO TO 910
      DO I = 1,NP
         IF ( (M/P(I))*P(I) == M ) GO TO 120
      ENDDO
      L = M
      GO TO 130
  120 L = P(I)
  130 O = M
      M = M/L
      ST = CMPLX(-2*SIN(M*PI/N)**2,SIN(2*M*PI/N), KIND(1.0D0) )
      S1 = (1.0D0,0.0D0)
      S = S1
      S2 = (0.0D0,0.0D0)
      H = 0
      IF ( F == 1 ) GO TO 520
      IF ( L == 2 ) GO TO 140
      IF ( L == 3 ) GO TO 250
      GO TO 360
  140 IF ( M == 1 ) GO TO 230
      IF ( M == 2 ) GO TO 210
      IF ( M == 3 ) GO TO 190
      IF ( M == 4 ) GO TO 170
  150 J = -H
  160 I = H + 1
      H = H + M
      E = J + M
      IF (ABS(S-(1.0D0,0.0D0)) < 10*EPSILON(1.0D0)) THEN
          DO K = I, H
             W(K) = A(J+K) + A(E+K)
          ENDDO
      ELSE IF (ABS(S-(-1.0D0,0.0D0)) < 10*EPSILON(1.0D0)) THEN
          DO K = I, H
             W(K) = A(J+K) - A(E+K)
          ENDDO
      ELSE
          DO K = I, H
             W(K) = A(J+K) + S*A(E+K)
          ENDDO
      ENDIF
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 160
      IF ( H < N ) GO TO 150
      F = 1
      GO TO 110
  170 J = -H
  180 H = H + 1
      E = J + M
      W(H) = A(J+H) + S*A(E+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H)
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 180
      IF ( H < N ) GO TO 170
      F = 1
      GO TO 110
  190 J = -H
  200 H = H + 1
      E = J + M
      W(H) = A(J+H) + S*A(E+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H)
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 200
      IF ( H < N ) GO TO 190
      F = 1
      GO TO 110
  210 J = -H
  220 H = H + 1
      E = J + M
      W(H) = A(J+H) + S*A(E+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H)
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 220
      IF ( H < N ) GO TO 210
      F = 1
      GO TO 110
  230 J = -H
  240 H = H + 1
      E = J + M
      W(H) = A(J+H) + S*A(E+H)
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 240
      IF ( H < N ) GO TO 230
      F = 1
      GO TO 110
  250 IF ( M == 1 ) GO TO 340
      IF ( M == 2 ) GO TO 320
      IF ( M == 3 ) GO TO 300
      IF ( M == 4 ) GO TO 280
  260 J = -H
  270 I = H + 1
      H = H + M
      E = J + M
      D = E + M
      T = S*S
      DO K = I, H
         W(K) = A(J+K) + S*A(E+K) + T*A(D+K)
      ENDDO
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 270
      IF ( H < N ) GO TO 260
      F = 1
      GO TO 110
  280 J = -H
  290 H = H + 1
      E = J + M
      D = E + M
      T = S*S
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 290
      IF ( H < N ) GO TO 280
      F = 1
      GO TO 110
  300 J = -H
  310 H = H + 1
      E = J + M
      D = E + M
      T = S*S
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 310
      IF ( H < N ) GO TO 300
      F = 1
      GO TO 110
  320 J = -H
  330 H = H + 1
      E = J + M
      D = E + M
      T = S*S
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      H = H + 1
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 330
      IF ( H < N ) GO TO 320
      F = 1
      GO TO 110
  340 J = -H
  350 H = H + 1
      E = J + M
      D = E + M
      T = S*S
      W(H) = A(J+H) + S*A(E+H) + T*A(D+H)
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 350
      IF ( H < N ) GO TO 340
      F = 1
      GO TO 110
  360 IF ( M == 1 ) GO TO 490
      IF ( M == 2 ) GO TO 460
      IF ( M == 3 ) GO TO 430
      IF ( M == 4 ) GO TO 400
  370 J = -H
  380 I = H + 1
      H = H + M
      G = J + O
      DO K = I, H
         W(K) = A(J+K)
      ENDDO
      T = S
      J = J + M
  390 DO K = I, H
         W(K) = W(K) + T*A(J+K)
      ENDDO
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 390
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 380
      IF ( H < N ) GO TO 370
      F = 1
      GO TO 110
  400 J = -H
  410 T = (1.0D0,0.0D0)
      I = H + 1
      E = I + 1
      D = E + 1
      H = H + M
      G = J + O
      W(I) = (0.0D0,0.0D0)
      W(E) = (0.0D0,0.0D0)
      W(D) = (0.0D0,0.0D0)
      W(H) = (0.0D0,0.0D0)
  420 W(I) = W(I) + T*A(J+I)
      W(E) = W(E) + T*A(J+E)
      W(D) = W(D) + T*A(J+D)
      W(H) = W(H) + T*A(J+H)
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 420
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 410
      IF ( H < N ) GO TO 400
      F = 1
      GO TO 110
  430 J = -H
  440 T = (1.0D0,0.0D0)
      I = H + 1
      E = I + 1
      H = H + M
      G = J + O
      W(I) = (0.0D0,0.0D0)
      W(E) = (0.0D0,0.0D0)
      W(H) = (0.0D0,0.0D0)
  450 W(I) = W(I) + T*A(J+I)
      W(E) = W(E) + T*A(J+E)
      W(H) = W(H) + T*A(J+H)
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 450
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 440
      IF ( H < N ) GO TO 430
      F = 1
      GO TO 110
  460 J = -H
  470 T = (1.0D0,0.0D0)
      I = H + 1
      H = H + M
      G = J + O
      W(I) = (0.0D0,0.0D0)
      W(H) = (0.0D0,0.0D0)
  480 W(I) = W(I) + T*A(J+I)
      W(H) = W(H) + T*A(J+H)
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 480
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 470
      IF ( H < N ) GO TO 460
      F = 1
      GO TO 110
  490 J = -H
  500 T = (1.0D0,0.0D0)
      I = H + 1
      H = H + M
      G = J + O
      W(I) = (0.0D0,0.0D0)
  510 W(I) = W(I) + T*A(J+I)
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 510
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 500
      IF ( H < N ) GO TO 490
      F = 1
      GO TO 110
  520 IF ( L == 2 ) GO TO 530
      IF ( L == 3 ) GO TO 640
      GO TO 750
  530 IF ( M == 1 ) GO TO 620
      IF ( M == 2 ) GO TO 600
      IF ( M == 3 ) GO TO 580
      IF ( M == 4 ) GO TO 560
  540 J = -H
  550 I = H + 1
      H = H + M
      E = J + M
      DO K = I, H
         A(K) = W(J+K) + S*W(E+K)
      ENDDO
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 550
      IF ( H < N ) GO TO 540
      F = 0
      GO TO 110
  560 J = -H
  570 H = H + 1
      E = J + M
      A(H) = W(J+H) + S*W(E+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H)
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 570
      IF ( H < N ) GO TO 560
      F = 0
      GO TO 110
  580 J = -H
  590 H = H + 1
      E = J + M
      A(H) = W(J+H) + S*W(E+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H)
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 590
      IF ( H < N ) GO TO 580
      F = 0
      GO TO 110
  600 J = -H
  610 H = H + 1
      E = J + M
      A(H) = W(J+H) + S*W(E+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H)
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 610
      IF ( H < N ) GO TO 600
      F = 0
      GO TO 110
  620 J = -H
  630 H = H + 1
      E = J + M
      A(H) = W(J+H) + S*W(E+H)
      J = E
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 630
      IF ( H < N ) GO TO 620
      F = 0
      GO TO 110
  640 IF ( M == 1 ) GO TO 730
      IF ( M == 2 ) GO TO 710
      IF ( M == 3 ) GO TO 690
      IF ( M == 4 ) GO TO 670
  650 J = -H
  660 I = H + 1
      H = H + M
      E = J + M
      D = E + M
      T = S*S
      DO K = I, H
         A(K) = W(J+K) + S*W(E+K) + T*W(D+K)
      ENDDO
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 660
      IF ( H < N ) GO TO 650
      F = 0
      GO TO 110
  670 J = -H
  680 H = H + 1
      E = J + M
      D = E + M
      T = S*S
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 680
      IF ( H < N ) GO TO 670
      F = 0
      GO TO 110
  690 J = -H
  700 H = H + 1
      E = J + M
      D = E + M
      T = S*S
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 700
      IF ( H < N ) GO TO 690
      F = 0
      GO TO 110
  710 J = -H
  720 H = H + 1
      E = J + M
      D = E + M
      T = S*S
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      H = H + 1
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 720
      IF ( H < N ) GO TO 710
      F = 0
      GO TO 110
  730 J = -H
  740 H = H + 1
      E = J + M
      D = E + M
      T = S*S
      A(H) = W(J+H) + S*W(E+H) + T*W(D+H)
      J = D
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 740
      IF ( H < N ) GO TO 730
      F = 0
      GO TO 110
  750 IF ( M == 1 ) GO TO 880
      IF ( M == 2 ) GO TO 850
      IF ( M == 3 ) GO TO 820
      IF ( M == 4 ) GO TO 790
  760 J = -H
  770 I = H + 1
      H = H + M
      G = J + O
      DO K = I, H
         A(K) =  W(J+K)
      ENDDO
      T = S
      J = J + M
  780 DO K = I, H
         A(K) = A(K) + T*W(J+K)
      ENDDO
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 780
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 770
      IF ( H < N ) GO TO 760
      F = 0
      GO TO 110
  790 J = -H
  800 T = (1.0D0,0.0D0)
      I = H + 1
      E = I + 1
      D = E + 1
      H = H + M
      G = J + O
      A(I) = (0.0D0,0.0D0)
      A(E) = (0.0D0,0.0D0)
      A(D) = (0.0D0,0.0D0)
      A(H) = (0.0D0,0.0D0)
  810 A(I) = A(I) + T*W(J+I)
      A(E) = A(E) + T*W(J+E)
      A(D) = A(D) + T*W(J+D)
      A(H) = A(H) + T*W(J+H)
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 810
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 800
      IF ( H < N ) GO TO 790
      F = 0
      GO TO 110
  820 J = -H
  830 T = (1.0D0,0.0D0)
      I = H + 1
      E = I + 1
      H = H + M
      G = J + O
      A(I) = (0.0D0,0.0D0)
      A(E) = (0.0D0,0.0D0)
      A(H) = (0.0D0,0.0D0)
  840 A(I) = A(I) + T*W(J+I)
      A(E) = A(E) + T*W(J+E)
      A(H) = A(H) + T*W(J+H)
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 840
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 830
      IF ( H < N ) GO TO 820
      F = 0
      GO TO 110
  850 J = -H
  860 T = (1.0D0,0.0D0)
      I = H + 1
      H = H + M
      G = J + O
      A(I) = (0.0D0,0.0D0)
      A(H) = (0.0D0,0.0D0)
  870 A(I) = A(I) + T*W(J+I)
      A(H) = A(H) + T*W(J+H)
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 870
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 860
      IF ( H < N ) GO TO 850
      F = 0
      GO TO 110
  880 J = -H
  890 I = H + 1
      H = H + M
      G = J + O
      A(I) = W(J+I)
      T = S
      J = J + M
  900 A(I) = A(I) + T*W(J+I)
      T = T*S
      J = J + M
      IF ( J < G ) GO TO 900
      J = J - M
      S3 = ST*S
      S4 = S1 + S3
      S2 = S2 + ((S1-S4)+S3)
      S1 = S4
      S = S1 + S2
      IF ( J+H < N ) GO TO 890
      IF ( H < N ) GO TO 880
      F = 0
      GO TO 110
  910 IF ( F /= 0 ) THEN
          DO I = 1, N
             A(I) = W(I)
          ENDDO
      ENDIF
      RETURN
      END SUBROUTINE FMFFT2

      SUBROUTINE FMFORM(FORM,MA,STRING)

!  Convert an FM number (MA) to a character string base 10 (STRING) using character string
!  FORM format.

!  FORM can be one of these types:  Iw,  Fw.d,  Ew.d,  ESw.d,  1PEw.d  for positive integers w,d.

!  If Iw format is used and MA is not exactly an integer, then the nearest integer to MA is printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: FORM,STRING
      TYPE(MULTI) :: MA
      INTENT (IN) :: FORM,MA
      INTENT (INOUT) :: STRING
      DOUBLE PRECISION :: VAL
      INTEGER :: J,JF1SAV,JF2SAV,JPT,K1,K2,K3,KD,KSAVE,KWD,KWI,LAST,LB,LENGFM,LENGST,LFIRST,ND,NEXP
      TYPE(MULTI) :: MXY(2)


      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMFORM'

      KSAVE = KFLAG
      JF1SAV = JFORM1
      JF2SAV = JFORM2
      STRING = ' '
      LENGFM = LEN(FORM)
      LENGST = LEN(STRING)
      KWI = 75
      KWD = 40

      IF (INDEX(FORM,'I') > 0 .OR. INDEX(FORM,'i') > 0) THEN
          K1 = MAX(INDEX(FORM,'I'),INDEX(FORM,'i')) + 1
          K2 = LENGFM
          IF (K2 >= K1) THEN
              CALL FMST2D(FORM(K1:K2),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = LENGST
          ENDIF
          KWI = MAX(1,MIN(KWI,LENGST))
          JFORM1 = 2
          JFORM2 = 0
          KWD = KWI + 21
          IF (KWD+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWD+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWD + 50
          ENDIF
          CALL FMNINT(MA,MXY(1))
          IF (MXY(1)%MP(3) /= 0) THEN
              CALL FMOUT(MXY(1),CMBUFF,KWD)
          ELSE
              DO J = 1, KWD
                 CMBUFF(J) = ' '
              ENDDO
              CMBUFF(2) = '0'
          ENDIF
          LFIRST = 1
          LAST = 1
          DO J = 1, KWD
             IF (CMBUFF(KWD+1-J) /= ' ') LFIRST = KWD+1-J
             IF (CMBUFF(J) /= ' ') LAST = J
          ENDDO
          JPT = 1
          IF (LAST-LFIRST+1 > KWI) GO TO 110
          IF (LAST <= KWI) THEN
              DO J = LAST, LFIRST, -1
                 JPT = KWI - LAST + J
                 STRING(JPT:JPT) = CMBUFF(J)
              ENDDO
              DO J = 1, JPT-1
                 STRING(J:J) = ' '
              ENDDO
          ELSE
              DO J = LFIRST, LAST
                 JPT = KWI - LAST + J
                 STRING(JPT:JPT) = CMBUFF(J)
              ENDDO
          ENDIF
      ELSE IF (INDEX(FORM,'F') > 0 .OR. INDEX(FORM,'f') > 0) THEN
          K1 = MAX(INDEX(FORM,'F'),INDEX(FORM,'f')) + 1
          K2 = INDEX(FORM,'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KWI = MAX(1,MIN(KWI,LENGST))
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 2
          JFORM2 = KD
          ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
          IF (ND < 2) ND = 2
          NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
          LB = MAX(JFORM2+NEXP,ND+NEXP)
          KWD = LB
          IF (KWD+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWD+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWD + 50
          ENDIF
          CALL FMOUT(MA,CMBUFF,KWD)
          LFIRST = 1
          LAST = 1
          DO J = 1, KWD
             IF (CMBUFF(KWD+1-J) /= ' ') LFIRST = KWD+1-J
             IF (CMBUFF(J) /= ' ') LAST = J
          ENDDO
          IF (LAST-LFIRST+1 > KWI) THEN

!             Not enough room for this F format, or FMOUT converted it to E format to avoid showing
!             no significant digits.  See if a shortened form will fit in E format.

              NEXP = INT(LOG10((ABS(REAL(MA%MP(2)))+1)*LOG10(REAL(MBASE))+1)+1)
              ND = KWI - NEXP - 5
              IF (ND < 1) THEN
                  GO TO 110
              ELSE
                  JFORM1 = 0
                  JFORM2 = ND
                  IF (KWI+50 > LMBUFF) THEN
                      IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
                      ALLOCATE(CMBUFF(KWI+50),STAT=J)
                      IF (J /= 0) THEN
                          CALL FMDEFINE_ERROR
                      ENDIF
                      LMBUFF = KWI + 50
                  ENDIF
                  CALL FMOUT(MA,CMBUFF,KWI)
                  LFIRST = 1
                  LAST = 1
                  DO J = 1, KWI
                     IF (CMBUFF(KWI+1-J) /= ' ') LFIRST = KWI+1-J
                     IF (CMBUFF(J) /= ' ') LAST = J
                  ENDDO
              ENDIF
          ENDIF
          JPT = 1
          IF (LAST <= KWI) THEN
              DO J = LAST, LFIRST, -1
                 JPT = KWI - LAST + J
                 STRING(JPT:JPT) = CMBUFF(J)
              ENDDO
              DO J = 1, JPT-1
                 STRING(J:J) = ' '
              ENDDO
          ELSE
              DO J = LFIRST, LAST
                 JPT = KWI - LAST + J
                 STRING(JPT:JPT) = CMBUFF(J)
              ENDDO
          ENDIF
      ELSE IF (INDEX(FORM,'1PE') > 0 .OR. INDEX(FORM,'1pe') > 0 .OR. INDEX(FORM,'ES') > 0 .OR.  &
               INDEX(FORM,'es') > 0) THEN
          IF (INDEX(FORM,'1PE') > 0 .OR. INDEX(FORM,'1pe') > 0) THEN
              K1 = MAX(INDEX(FORM,'E'),INDEX(FORM,'e')) + 1
          ELSE
              K1 = MAX(INDEX(FORM,'S'),INDEX(FORM,'s')) + 1
          ENDIF
          K2 = INDEX(FORM,'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KWI = MAX(1,MIN(KWI,LENGST))
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 1
          JFORM2 = KD + 1
          IF (KWI+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWI+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWI + 50
          ENDIF
          CALL FMOUT(MA,CMBUFF,KWI)
          DO J = KWI, 1, -1
             IF (J > LENGST) THEN
                 IF (CMBUFF(J) /= ' ') GO TO 110
             ELSE
                 STRING(J:J) = CMBUFF(J)
             ENDIF
          ENDDO
      ELSE IF (INDEX(FORM,'E') > 0 .OR. INDEX(FORM,'e') > 0) THEN
          K1 = MAX(INDEX(FORM,'E'),INDEX(FORM,'e')) + 1
          K2 = INDEX(FORM,'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KWI = MAX(1,MIN(KWI,LENGST))
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 0
          JFORM2 = KD
          IF (KWI+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWI+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWI + 50
          ENDIF
          CALL FMOUT(MA,CMBUFF,KWI)
          DO J = KWI, 1, -1
             IF (J > LENGST) THEN
                 IF (CMBUFF(J) /= ' ') GO TO 110
             ELSE
                 STRING(J:J) = CMBUFF(J)
             ENDIF
          ENDDO
      ELSE
          GO TO 110
      ENDIF
      GO TO 120

!             Error condition.

  110 KFLAG = -8
      DO J = 1, LENGST
         STRING(J:J) = '*'
      ENDDO

  120 KFLAG = KSAVE
      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMFORM

      SUBROUTINE FMFPRT(FORM,MA)

!  Print an FM number (MA) on unit KW using character string FORM format.

!  FORM can be one of these types:  Iw,  Fw.d,  Ew.d,  ESw.d,  1PEw.d  for positive integers w,d.

!  If Iw format is used and MA is not exactly an integer, then the nearest integer to MA is printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      CHARACTER(20) :: FORM2
      DOUBLE PRECISION :: VAL
      INTEGER :: J,JF1SAV,JF2SAV,JPT,K,K1,K2,K3,KD,KSAVE,KWD,KWI,LAST,LB,LENGFM,LFIRST,ND,NEXP
      INTENT (IN) :: FORM,MA
      TYPE(MULTI) :: MXY(2)


      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMFPRT'

      KSAVE = KFLAG
      JF1SAV = JFORM1
      JF2SAV = JFORM2
      LENGFM = LEN(FORM)
      KWI = 75
      KWD = 40

      IF (INDEX(FORM,'I') > 0 .OR. INDEX(FORM,'i') > 0) THEN
          K1 = MAX(INDEX(FORM,'I'),INDEX(FORM,'i')) + 1
          K2 = LENGFM
          IF (K2 >= K1) THEN
              CALL FMST2D(FORM(K1:K2),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          JFORM1 = 2
          JFORM2 = 0
          KWD = KWI + 21
          IF (KWD+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWD+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWD + 50
          ENDIF
          CALL FMNINT(MA,MXY(1))
          IF (MXY(1)%MP(3) /= 0) THEN
              CALL FMOUT(MXY(1),CMBUFF,KWD)
          ELSE
              DO J = 1, KWD
                 CMBUFF(J) = ' '
              ENDDO
              CMBUFF(2) = '0'
          ENDIF
          LFIRST = 1
          LAST = 1
          DO J = 1, KWD
             IF (CMBUFF(KWD+1-J) /= ' ') LFIRST = KWD+1-J
             IF (CMBUFF(J) /= ' ') LAST = J
          ENDDO
          JPT = 1
          IF (LAST-LFIRST+1 > KWI) GO TO 110
          IF (LAST <= KWI) THEN
              DO J = LAST, LFIRST, -1
                 JPT = KWI - LAST + J
                 IF (JPT /= J) CMBUFF(JPT) = CMBUFF(J)
              ENDDO
              DO J = 1, JPT-1
                 CMBUFF(J) = ' '
              ENDDO
          ELSE
              DO J = LFIRST, LAST
                 JPT = KWI - LAST + J
                 IF (JPT /= J) CMBUFF(JPT) = CMBUFF(J)
              ENDDO
          ENDIF
      ELSE IF (INDEX(FORM,'F') > 0 .OR. INDEX(FORM,'f') > 0) THEN
          K1 = MAX(INDEX(FORM,'F'),INDEX(FORM,'f')) + 1
          K2 = INDEX(FORM(1:LENGFM),'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 2
          JFORM2 = KD
          ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
          IF (ND < 2) ND = 2
          NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
          LB = MAX(JFORM2+NEXP,ND+NEXP)
          KWD = LB
          IF (KWD+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWD+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWD + 50
          ENDIF
          CALL FMOUT(MA,CMBUFF,KWD)
          LFIRST = 1
          LAST = 1
          DO J = 1, KWD
             IF (CMBUFF(KWD+1-J) /= ' ') LFIRST = KWD+1-J
             IF (CMBUFF(J) /= ' ') LAST = J
          ENDDO
          IF (LAST-LFIRST+1 > KWI) THEN

!             Not enough room for this F format, or FMOUT converted it to E format to avoid showing
!             no significant digits.  See if a shortened form will fit in E format.

              NEXP = INT(LOG10((ABS(REAL(MA%MP(2)))+1)*LOG10(REAL(MBASE))+1)+1)
              ND = KWI - NEXP - 5
              IF (ND < 1) THEN
                  GO TO 110
              ELSE
                  JFORM1 = 0
                  JFORM2 = ND
                  IF (KWI+50 > LMBUFF) THEN
                      IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
                      ALLOCATE(CMBUFF(KWI+50),STAT=J)
                      IF (J /= 0) THEN
                          CALL FMDEFINE_ERROR
                      ENDIF
                      LMBUFF = KWI + 50
                  ENDIF
                  CALL FMOUT(MA,CMBUFF,KWI)
                  LFIRST = 1
                  LAST = 1
                  DO J = 1, KWI
                     IF (CMBUFF(KWI+1-J) /= ' ') LFIRST = KWI+1-J
                     IF (CMBUFF(J) /= ' ') LAST = J
                  ENDDO
              ENDIF
          ENDIF
          JPT = 1
          IF (LAST <= KWI) THEN
              DO J = LAST, LFIRST, -1
                 JPT = KWI - LAST + J
                 IF (JPT /= J) CMBUFF(JPT) = CMBUFF(J)
              ENDDO
              DO J = 1, JPT-1
                 CMBUFF(J) = ' '
              ENDDO
          ELSE
              DO J = LFIRST, LAST
                 JPT = KWI - LAST + J
                 IF (JPT /= J) CMBUFF(JPT) = CMBUFF(J)
              ENDDO
          ENDIF
      ELSE IF (INDEX(FORM,'1PE') > 0 .OR. INDEX(FORM,'1pe') > 0 .OR. INDEX(FORM,'ES') > 0 .OR.  &
               INDEX(FORM,'es') > 0) THEN
          IF (INDEX(FORM,'1PE') > 0 .OR. INDEX(FORM,'1pe') > 0) THEN
              K1 = MAX(INDEX(FORM,'E'),INDEX(FORM,'e')) + 1
          ELSE
              K1 = MAX(INDEX(FORM,'S'),INDEX(FORM,'s')) + 1
          ENDIF
          K2 = INDEX(FORM(1:LENGFM),'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 1
          JFORM2 = KD + 1
          IF (KWI+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWI+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWI + 50
          ENDIF
          CALL FMOUT(MA,CMBUFF,KWI)
      ELSE IF (INDEX(FORM,'E') > 0 .OR. INDEX(FORM,'e') > 0) THEN
          K1 = MAX(INDEX(FORM,'E'),INDEX(FORM,'e')) + 1
          K2 = INDEX(FORM(1:LENGFM),'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 0
          JFORM2 = KD
          IF (KWI+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWI+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWI + 50
          ENDIF
          CALL FMOUT(MA,CMBUFF,KWI)
      ELSE
          GO TO 110
      ENDIF
      GO TO 120

!             Error condition.

  110 KFLAG = -8
      DO J = 1, KWI
         CMBUFF(J) = '*'
      ENDDO

  120 LAST = KWI + 1
      WRITE (FORM2,"(' (6X,',I3,'A1) ')") KSWIDE-7
      IF (KFLAG /= -8) KFLAG = KSAVE
      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      DO J = KWI, 1, -1
         IF (CMBUFF(J) /= ' ' .OR. J == 1) THEN
             WRITE (KW,FORM2) (CMBUFF(K),K=1,J)
             NCALL = NCALL - 1
             RETURN
         ENDIF
      ENDDO
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMFPRT

      SUBROUTINE FMGCDI(N1,N2)

!  Find the Greatest Common Divisor of N1 and N2, and return both having been divided by their GCD.
!  Both must be positive.

      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: K1,K2,K3,N1,N2
      INTENT (INOUT) :: N1,N2

      K1 = MAX(N1,N2)
      K2 = MIN(N1,N2)
  110 K3 = MOD(K1,K2)
      IF (K3 == 0) THEN
          N1 = N1/K2
          N2 = N2/K2
          RETURN
      ELSE
          K1 = K2
          K2 = K3
          GO TO 110
      ENDIF
      END SUBROUTINE FMGCDI

      SUBROUTINE FMHTBL

!  Initialize two hash tables that are used for character look-up during input conversion.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: J,KPT

      CHARACTER :: LCHARS(21) = (/                                  &
                  '+','-','0','1','2','3','4','5','6','7','8','9',  &
                  '.','E','D','Q','M','e','d','q','m' /)
      INTEGER :: LTYPES(21) = (/ 1,1,2,2,2,2,2,2,2,2,2,2,3,4,4,4,4,4,4,4,4 /)
      INTEGER :: LVALS(21) = (/ 1,-1,0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,0,0,0 /)

      DO J = LHASH1, LHASH2
         KHASHT(J) = 5
         KHASHV(J) = 0
      ENDDO
      DO J = 1, 21
         KPT = ICHAR(LCHARS(J))
         IF (KPT < LHASH1 .OR. KPT > LHASH2) THEN
             WRITE (KW,                                                       &
                "(/' Error in input conversion.'/"                        //  &
                "' ICHAR function was out of range for the current',"     //  &
                "' dimensions.'/' ICHAR(''',A,''') gave the value ',"     //  &
                "I12,', which is outside the currently'/' dimensioned',"  //  &
                "' bounds of (',I5,':',I5,') for variables KHASHT ',"     //  &
                "'and KHASHV.'/' Re-define the two parameters ',"         //  &
                "'LHASH1 and LHASH2 so the dimensions will'/' contain',"  //  &
                "' all possible output values from ICHAR.'//)"                &
                   ) LCHARS(J),KPT,LHASH1,LHASH2
         ELSE
             KHASHT(KPT) = LTYPES(J)
             KHASHV(KPT) = LVALS(J)
         ENDIF
      ENDDO
      LHASH = 1
      RETURN
      END SUBROUTINE FMHTBL

      SUBROUTINE FMHYPOT(MA,MB,MC)

!  MC = sqrt( x^2 + y^2 )

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTEGER :: NDSAVE,MXSAVE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMHYPOT  '
      NDSAVE = NDIG
      NDIG = NDIG + NGRD52
      MXSAVE = MXEXP
      MXEXP = MXEXP2

!             Check for special cases.

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL FMST2M('UNKNOWN',MXY(1))
          GO TO 110
      ENDIF
      IF (MA%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPOV) THEN
          CALL FMST2M('OVERFLOW',MXY(1))
          GO TO 110
      ENDIF

      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
      CALL FMSQR_R1(MXY(1))
      CALL FMSQR_R1(MXY(2))
      CALL FMADD_R1(MXY(1),MXY(2))
      CALL FMSQRT_R1(MXY(1))

  110 MXEXP = MXSAVE
      CALL FMEQU(MXY(1),MC,NDIG,NDSAVE)
      NDIG = NDSAVE
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMHYPOT

      SUBROUTINE FMI2M(IVAL,MA)

!  MA = IVAL

!  Convert an integer to FM format.

!  The conversion is exact if IVAL is less than MBASE**NDIG, otherwise the result is
!  an approximation.

!  This routine performs the trace printing for the conversion.  FMIM is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMI2M'
          CALL FMNTRI(2,IVAL,1)

          CALL FMIM(IVAL,MA)

          CALL FMNTR(1,MA,MA,1,1)
      ELSE
          CALL FMIM(IVAL,MA)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMI2M

      SUBROUTINE FMIM(IVAL,MA)

!  MA = IVAL.  Internal integer conversion routine.

!  The conversion is exact if IVAL is less than MBASE**NDIG, otherwise FMDM is used to get
!  an approximation.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL

      DOUBLE PRECISION :: X
      REAL (KIND(1.0D0)) :: MK,ML,MVAL
      INTEGER :: J,JM2,KB,KB1,N1,NMVAL,NV2
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      N1 = NDIG + 1

      MVAL = ABS(IVAL)
      NMVAL = INT(MVAL)
      NV2 = NMVAL - 1
      IF (ABS(IVAL) > MXBASE .OR. NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL FMIMS(IVAL,MA)
          RETURN
      ENDIF

!             Check for small IVAL.

      IF (MVAL < MBASE) THEN
          DO J = 3, N1
             MA%MP(J+1) = 0
          ENDDO
          IF (IVAL >= 0) THEN
              MA%MP(3) = IVAL
              MA%MP(1) = 1
          ELSE
              MA%MP(3) = -IVAL
              MA%MP(1) = -1
          ENDIF
          IF (IVAL == 0) THEN
              MA%MP(2) = 0
          ELSE
              MA%MP(2) = 1
          ENDIF
          RETURN
      ENDIF

!             Compute and store the digits, right to left.

      MA%MP(2) = 0
      J = NDIG + 1

  110 MK = AINT (MVAL/MBASE)
      ML = MVAL - MK*MBASE
      MA%MP(2) = MA%MP(2) + 1
      MA%MP(J+1) = ML
      IF (MK > 0) THEN
          MVAL = MK
          J = J - 1
          IF (J >= 2) GO TO 110

!             Here IVAL cannot be expressed exactly.

          X = IVAL
          CALL FMDM(X,MA)
          RETURN
      ENDIF

!             Normalize MA.

      KB = N1 - J + 2
      JM2 = J - 2
      DO J = 2, KB
         MA%MP(J+1) = MA%MP(J+JM2+1)
      ENDDO
      KB1 = KB + 1
      IF (KB1 <= N1) THEN
          DO J = KB1, N1
             MA%MP(J+1) = 0
          ENDDO
      ENDIF

      MA%MP(1) = 1
      IF (IVAL < 0 .AND. MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0) MA%MP(1) = -1

      RETURN
      END SUBROUTINE FMIM

      SUBROUTINE FMIMS(IVAL,MA)

!  MA = IVAL.  Internal integer conversion routine.

!  This routine is called when M-variable precision is less than Integer precision.  This often
!  happens when single precision is chosen for M-variables.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL

      DOUBLE PRECISION :: X
      REAL (KIND(1.0D0)) :: ML
      INTEGER :: J,JM2,KB,KB1,KBASE,KMK,KVAL,N1
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      N1 = NDIG + 1

!             Check for small IVAL.

      KVAL = ABS(IVAL)
      KBASE = INT(MBASE)
      IF (KVAL < KBASE) THEN
          DO J = 3, N1
             MA%MP(J+1) = 0
          ENDDO
          IF (IVAL >= 0) THEN
              MA%MP(3) = IVAL
              MA%MP(1) = 1
          ELSE
              MA%MP(3) = -IVAL
              MA%MP(1) = -1
          ENDIF
          IF (IVAL == 0) THEN
              MA%MP(2) = 0
          ELSE
              MA%MP(2) = 1
          ENDIF
          RETURN
      ENDIF

!             Compute and store the digits, right to left.

      MA%MP(2) = 0
      J = NDIG + 1

  110 KMK = (KVAL/KBASE)
      ML = KVAL - KMK*KBASE
      MA%MP(2) = MA%MP(2) + 1
      MA%MP(J+1) = ML
      IF (KMK > 0) THEN
          KVAL = KMK
          J = J - 1
          IF (J >= 2) GO TO 110

!             Here IVAL cannot be expressed exactly.

          X = IVAL
          CALL FMDM(X,MA)
          RETURN
      ENDIF

!             Normalize MA.

      KB = N1 - J + 2
      JM2 = J - 2
      DO J = 2, KB
         MA%MP(J+1) = MA%MP(J+JM2+1)
      ENDDO
      KB1 = KB + 1
      IF (KB1 <= N1) THEN
          DO J = KB1, N1
             MA%MP(J+1) = 0
          ENDDO
      ENDIF

      MA%MP(1) = 1
      IF (IVAL < 0 .AND. MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0) MA%MP(1) = -1

      RETURN
      END SUBROUTINE FMIMS

      SUBROUTINE FMINP(LINE,MA,LA,LB)

!  Convert an array of characters to floating point multiple precision format.

!  LINE is an A1 character array of length LB to be converted to FM format and returned in MA.
!  LA is a pointer telling the routine where in the array to begin the conversion.  This allows
!     more than one number to be stored in an array and converted in place.
!  LB is a pointer to the last character of the field for that number.

!  The input number may be in integer or any real format.

!  KESWCH = 1  causes input to FMINP with no digits before the exponent letter to be treated as if
!              there were a leading '1'.  This is sometimes better for interactive input:
!              'E7' converts to 10.0**7.
!         = 0  causes a leading zero to be assumed.  This gives compatibility with Fortran:
!              'E7' converts to 0.0.

!  In exponential format the 'E' may also be 'D', 'Q', or 'M'.

!  So that FMINP will convert any output from FMOUT, LINE is tested to see if the input is one of
!  the special symbols +OVERFLOW, -OVERFLOW, +UNDERFLOW, -UNDERFLOW, or UNKNOWN.
!  For user input the abbreviations OVFL, UNFL, UNKN may be used.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: LA,LB
      CHARACTER :: LINE(LB)
      TYPE(MULTI) :: MA

      REAL (KIND(1.0D0)) :: MBSAVE,MXSAV1,MXSAV2
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KMN,KOF,KPOWER,KPT,KR_RETRY,KSPEC,KSTART,KSTOP,  &
                 KTYPE,KUF,KUK,KWRNSV,L,ND,NDSAV1,NDSAVE,NEW_MBASE,NEW_NDIG,NTRSAV
      INTENT (IN) :: LINE,LA,LB
      INTENT (INOUT) :: MA

      CHARACTER :: KOVFL(4) = (/ 'O','V','F','L' /)
      CHARACTER :: KUNFL(4) = (/ 'U','N','F','L' /)
      CHARACTER :: KUNKN(4) = (/ 'U','N','K','N' /)
      CHARACTER :: LOVFL(4) = (/ 'o','v','f','l' /)
      CHARACTER :: LUNFL(4) = (/ 'u','n','f','l' /)
      CHARACTER :: LUNKN(4) = (/ 'u','n','k','n' /)
      CHARACTER(9) :: NAMEST_SAVE(0:50)
      TYPE(MULTI) :: MXY(5)

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

!             To avoid recursion, FMINP calls only internal arithmetic routines (FMADD2,
!             FMMPY2, ...), so no trace printout is done during a call to FMINP.

      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMINP'
      NDSAVE = NDIG
      KWRNSV = KWARN
      KWARN = 0
      KR_RETRY = 0
      KFLAG = 0
      MXSAV1 = MXEXP
      MXSAV2 = MXEXP2
      IF (MXEXP < 100000) THEN
          MXEXP  = 201000
          MXEXP2 = 201000
      ELSE
          MXEXP = MXEXP2
      ENDIF

!             Initialize two hash tables that are used for character look-up during
!             input conversion.

      IF (LHASH == 0) CALL FMHTBL

!             Check for special symbols.

      KSPEC = 0
      KMN = 1
      KOF = 1
      KUF = 1
      KUK = 1
      DO J = LA, LB
         KPT = ICHAR(LINE(J))
         IF (KPT >= LHASH1 .AND. KPT <= LHASH2) THEN
             KTYPE = KHASHT(KPT)
             IF (KTYPE == 2) GO TO 110
         ENDIF
         IF (LINE(J) == '-') KMN = -1
         IF (LINE(J) == KOVFL(KOF) .OR. LINE(J) == LOVFL(KOF)) THEN
             KOF = KOF + 1
             IF (KOF == 5) THEN
                 KSPEC = 1
                 CALL FMIM(0,MXY(5))
                 MXY(5)%MP(2) = MEXPOV
                 MXY(5)%MP(3) = 1
                 MXY(5)%MP(1) = KMN
                 GO TO 130
             ENDIF
         ENDIF
         IF (LINE(J) == KUNFL(KUF) .OR. LINE(J) == LUNFL(KUF)) THEN
             KUF = KUF + 1
             IF (KUF == 5) THEN
                 KSPEC = 1
                 CALL FMIM(0,MXY(5))
                 MXY(5)%MP(2) = MEXPUN
                 MXY(5)%MP(3) = 1
                 MXY(5)%MP(1) = KMN
                 GO TO 130
             ENDIF
         ENDIF
         IF (LINE(J) == KUNKN(KUK) .OR. LINE(J) == LUNKN(KUK)) THEN
             KUK = KUK + 1
             IF (KUK == 5) THEN
                 KSPEC = 1
                 CALL FMIM(0,MXY(5))
                 MXY(5)%MP(2) = MUNKNO
                 MXY(5)%MP(3) = 1
                 GO TO 130
             ENDIF
         ENDIF
      ENDDO

!             Increase the working precision.

  110 K = NGRD52
      NDIG = MAX(NDIG+K,2)

  120 IF (KR_RETRY >= 1) THEN
          NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      NDSAV1 = NDIG
      KSTART = LA
      KSTOP = LB

!             If MBASE is a power of ten then call FMINP2 for faster input conversion.

      KPOWER = INT(LOG10(DBLE(MBASE)) + 0.5D0)
      IF (MBASE == 10**KPOWER) THEN
          K = 0
          DO J = LA, LB
             IF (LINE(J) /= ' ') THEN
                 DO L = LB, LA, -1
                    IF (LINE(L) /= ' ') THEN
                        K = (L-J+1)*LOG(10.0D0)/LOG(DBLE(MBASE))
                        EXIT
                    ENDIF
                 ENDDO
                 EXIT
             ENDIF
          ENDDO
          IF (NDIG <= K+9) THEN
              NDIG = K + 10
              NDSAV1 = NDIG
          ENDIF
          CALL FMIM(0,MXY(1))
          CALL FMIM(0,MXY(2))
          CALL FMIM(0,MXY(3))
          CALL FMIM(0,MXY(4))
          CALL FMIM(0,MXY(5))
          CALL FMINP2(MXY,LINE,KSTART,KSTOP,KPOWER)
          IF (MXY(5)%MP(2) == MUNKNO) GO TO 140
          GO TO 130
      ENDIF

!             If MBASE is not a power of ten then call FMINP2 first using a power of ten base,
!             then change to base MBASE.

      MBSAVE = MBASE
      KPOWER = INT(LOG10(DBLE(MXBASE)/4) + 0.5D0)
      MBASE = 10**KPOWER
      NDIG = NDSAV1*LOG(DBLE(MBSAVE))/LOG(DBLE(MBASE)) + 3
      K = 0
      DO J = LA, LB
         IF (LINE(J) /= ' ') THEN
             DO L = LB, LA, -1
                IF (LINE(L) /= ' ') THEN
                    K = (L-J+1)*LOG(10.0D0)/LOG(DBLE(MBASE))
                    EXIT
                ENDIF
             ENDDO
             EXIT
         ENDIF
      ENDDO
      IF (NDIG <= K+9) THEN
          NDIG = K + 10
      ENDIF
      CALL FMCONS
      CALL FMIM(0,MXY(1))
      CALL FMIM(0,MXY(2))
      CALL FMIM(0,MXY(3))
      CALL FMIM(0,MXY(4))
      CALL FMIM(0,MXY(5))
      J = MXEXP2
      K = J*LOG(DBLE(MBSAVE))/LOG(DBLE(MBASE)) + 0.5
      MXEXP2 = K
      CALL FMINP2(MXY,LINE,KSTART,KSTOP,KPOWER)
      MXEXP2 = J
      NEW_MBASE = MBSAVE
      NEW_NDIG = MAX(NDSAV1,NDIG*NINT(LOG(DBLE(MBASE))/LOG(DBLE(MBSAVE))))
      ND = 2
      DO J = NDIG, 3, -1
         IF (MXY(5)%MP(J+2) /= 0) THEN
             ND = J
             EXIT
         ENDIF
      ENDDO
      NDIG = ND
      NTRSAV = NTRACE
      NTRACE = 0
      J = NCALL
      NAMEST_SAVE(0:NCALL) = NAMEST(0:NCALL)
      NCALL = 0
      IF (ABS(MXY(5)%MP(2)) < MEXPOV) THEN
          CALL FMCHANGEBASE(MXY(5),MXY(4),NEW_MBASE,NEW_NDIG)
      ELSE
          CALL FMEQU(MXY(5),MXY(4),NDIG,NEW_NDIG)
      ENDIF
      NCALL = J
      NAMEST(0:NCALL) = NAMEST_SAVE(0:NCALL)
      NTRACE = NTRSAV
      MBASE = MBSAVE
      NDIG = NEW_NDIG
      CALL FMCONS
      CALL FMEQ(MXY(4),MXY(5))
      IF (MXY(5)%MP(2) == MUNKNO) GO TO 140

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  130 IF (KSPEC == 0) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(5)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 120
      ENDIF
      MXEXP = MXSAV1
      MXEXP2 = MXSAV2
      IF (INT(MBASE-AINT (MBASE/2)*2) == 0) THEN
          CALL FMEQU(MXY(5),MA,NDIG,NDSAVE)
      ELSE
          CALL FMEQU(MXY(5),MA,NDIG-1,NDSAVE)
      ENDIF
      IF (KSPEC == 0) THEN
          IF (MA%MP(2) == MUNKNO) GO TO 140
      ELSE
          KFLAG = 0
      ENDIF
      NDIG = NDSAVE
      KWARN = KWRNSV
      IF (KFLAG == 1) KFLAG = 0
      IF (MA%MP(3) == 0) MA%MP(1) = 1
      NCALL = NCALL - 1
      RETURN

!             Error in converting the number.

  140 CALL FMIM(0,MXY(5))
      CALL FMEQU(MXY(5),MA,NDIG,NDSAVE)
      MA%MP(2) = MUNKNO
      MA%MP(3) = 1
      KWARN = KWRNSV
      KFLAG = -7
      NAMEST(NCALL) = 'FMINP'
      CALL FMWARN
      NDIG = NDSAVE
      MXEXP = MXSAV1
      MXEXP2 = MXSAV2
      IF (KFLAG == 1) KFLAG = 0
      IF (MA%MP(3) == 0) MA%MP(1) = 1
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMINP

      SUBROUTINE FMINP2(MXY,LINE,KSTART,KSTOP,KPOWER)

!  Internal routine for input conversion for a power of ten MBASE.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MXY(5)
      INTEGER :: KSTART,KSTOP,KPOWER
      CHARACTER :: LINE(KSTOP)

      INTEGER :: J,JSTATE,K,KDFLAG,KEXP,KF1,KF1DIG,KF2,KF2DIG,KF2PT,KNZDIG,KPT,KSHIFT,KSIGN,  &
                 KSIGNX,KTYPE,KVAL,LARGE,LNZD
      INTENT (IN) :: LINE,KSTART,KSTOP,KPOWER
      INTENT (INOUT) :: MXY

!  Simulate a finite-state automaton to scan the input line and build the number.
!  States of the machine:

!  1.  Initial entry to the subroutine
!  2.  Sign of the number
!  3.  Scanning digits before a decimal point
!  4.  Decimal point
!  5.  Scanning digits after a decimal point
!  6.  E, D, Q, or M -- precision indicator before the exponent
!  7.  Sign of the exponent
!  8.  Scanning exponent
!  9.  Syntax error

!  Character types recognized by the machine:

!  1.  Sign (+,-)
!  2.  Numeral (0,1,...,9)
!  3.  Decimal point (.)
!  4.  Precision indicator (E,D,Q,M)
!  5.  Illegal character for number

!  All blanks are ignored.  The analysis of the number proceeds as follows:  If the simulated
!  machine is in state JSTATE and a character of type JTYPE is encountered the new state of the
!  machine is given by JTRANS(JSTATE,JTYPE).

!  In this initialization, the array is loaded by columns.

!          State   1  2  3  4  5  6  7  8

      INTEGER :: JTRANS(8,4) = RESHAPE(  (/     &
                   2, 9, 7, 7, 7, 7, 9, 9,      &
                   3, 3, 3, 5, 5, 8, 8, 8,      &
                   4, 4, 4, 9, 9, 9, 9, 9,      &
                   6, 6, 6, 6, 6, 9, 9, 9   /)  &
        , (/ 8,4 /) )

      JSTATE = 1
      KDFLAG = 0
      KSIGN = 1
      KSIGNX = 1
      KF1 = 0
      KNZDIG = 0
      LNZD = 0
      KF1DIG = 0
      KF2 = 0
      KF2DIG = 0
      KF2PT = 2
      KEXP = 0
      LARGE = INT(INTMAX/10)

!             Scan the number.

      DO J = KSTART, KSTOP
         IF (LINE(J) == ' ') CYCLE
         KPT = ICHAR(LINE(J))
         IF (KPT < LHASH1 .OR. KPT > LHASH2) THEN
             WRITE (KW,                                                       &
                "(/' Error in input conversion.'/"                        //  &
                "' ICHAR function was out of range for the current',"     //  &
                "' dimensions.'/' ICHAR(''',A,''') gave the value ',"     //  &
                "I12,', which is outside the currently'/' dimensioned',"  //  &
                "' bounds of (',I5,':',I5,') for variables KHASHT ',"     //  &
                "'and KHASHV.'/' Re-define the two parameters ',"         //  &
                "'LHASH1 and LHASH2 so the dimensions will'/' contain',"  //  &
                "' all possible output values from ICHAR.'//)"                &
                   ) LINE(J),KPT,LHASH1,LHASH2
             KTYPE = 5
             KVAL  = 0
         ELSE
             KTYPE = KHASHT(KPT)
             KVAL  = KHASHV(KPT)
         ENDIF

         IF (KTYPE >= 5) GO TO 110

         JSTATE = JTRANS(JSTATE,KTYPE)

         SELECT CASE (JSTATE)

!             State 2.  Sign of the number.

         CASE (2)
             KSIGN = KVAL

!             State 3.  Digits before a decimal point.

         CASE (3)
             KDFLAG = 1
             KF1 = 10*KF1 + KVAL
             IF (KVAL > 0) LNZD = 1
             IF (KVAL > 0 .OR. KNZDIG /= 0) THEN
                 KNZDIG = 1
                 KF1DIG = KF1DIG + 1
             ENDIF
             IF (KF1DIG == KPOWER) THEN
                 MXY(2)%MP(2) = MXY(2)%MP(2) + 1
                 K = MXY(2)%MP(2)
                 IF (K < NDIG) THEN
                     MXY(2)%MP(K+2) = KF1
                 ENDIF
                 KF1 = 0
                 KF1DIG = 0
             ENDIF

!             State 4.  Decimal point

         CASE (4)
             CYCLE

!             State 5.  Digits after a decimal point.

         CASE (5)
             KDFLAG = 1
             IF (KVAL > 0) LNZD = 1
             IF (KF2PT > NDIG+1) CYCLE
             KF2 = 10*KF2 + KVAL
             KF2DIG = KF2DIG + 1
             IF (KF2DIG == KPOWER) THEN
                 MXY(3)%MP(KF2PT+1) = KF2
                 IF (KF2 == 0 .AND. KF2PT == 2) THEN
                     MXY(3)%MP(2) = MXY(3)%MP(2) - 1
                 ELSE
                     KF2PT = KF2PT + 1
                 ENDIF
                 KF2 = 0
                 KF2DIG = 0
             ENDIF

!             State 6.  Precision indicator.

         CASE (6)
             IF (KDFLAG == 0 .AND. KESWCH == 1) THEN
                 LNZD = 1
                 CALL FMIM(1,MXY(2))
             ENDIF

!             State 7.  Sign of the exponent.

         CASE (7)
             KSIGNX = KVAL

!             State 8.  Digits of the exponent.

         CASE (8)
             IF (KEXP >= LARGE) THEN
                 IF (LNZD == 0) THEN
                     CALL FMIM(0,MXY(5))
                     RETURN
                 ENDIF
                 CALL FMINP3(LINE,KSTART,KSTOP,MXY(5))
                 RETURN
             ENDIF
             KEXP = 10*KEXP + KVAL
             IF (KEXP >= 0.75D0*MXEXP2*DLOGMB/DLOGTN .AND. LNZD /= 0) THEN
                 CALL FMINP3(LINE,KSTART,KSTOP,MXY(5))
                 RETURN
             ENDIF

         CASE DEFAULT
             GO TO 110

         END SELECT

      ENDDO

!             Form the number and return.  MXY(5) = KSIGN*(MXY(2) + MXY(3))*10.0**(KSIGNX*KEXP)

      IF (KF1DIG /= 0) THEN
          MXY(2)%MP(2) = MXY(2)%MP(2) + 1
          KSHIFT = 10**(KPOWER-KF1DIG)
          K = MXY(2)%MP(2)
          IF (K < NDIG) MXY(2)%MP(2+INT(K)) = KF1*KSHIFT
          IF (KSHIFT > 1) THEN
              CALL FMDIVN_R1(MXY(2),KSHIFT)
          ENDIF
      ENDIF
      IF (MXY(2)%MP(3) == 0) THEN
          MXY(2)%MP(1) = 1
          MXY(2)%MP(2) = 0
      ELSE
          MXY(2)%MP(1) = 1
      ENDIF

      IF (KF2DIG /= 0) THEN
          KSHIFT = 10**(KPOWER-KF2DIG)
          MXY(3)%MP(KF2PT+1) = KF2*KSHIFT
      ENDIF
      IF (MXY(3)%MP(3) == 0) THEN
          MXY(3)%MP(1) = 1
          MXY(3)%MP(2) = 0
      ELSE
          MXY(3)%MP(1) = 1
      ENDIF

      IF (KEXP /= 0) THEN
          IF (KSIGNX == 1) THEN
              MXY(4)%MP(2) = INT(KEXP/KPOWER) + 1
              MXY(4)%MP(3) = 10**(MOD(KEXP,KPOWER))
          ELSE
              MXY(4)%MP(2) = -INT((KEXP-1)/KPOWER)
              KSHIFT = 10**(MOD(KEXP,KPOWER))
              IF (KSHIFT > 1) THEN
                  MXY(4)%MP(3) = MBASE/KSHIFT
              ELSE
                  MXY(4)%MP(3) = 1
              ENDIF
          ENDIF
      ENDIF

      CALL FMADD2(MXY(2),MXY(3),MXY(5))

      IF (KEXP > 0) CALL FMMPY2_R1(MXY(5),MXY(4))
      MXY(5)%MP(1) = KSIGN

      RETURN

!             Error in converting the number.

  110 CALL FMIM(0,MXY(5))
      MXY(5)%MP(2) = MUNKNO
      MXY(5)%MP(3) = 1
      MXY(5)%MP(1) = 1
      KFLAG = -7
      RETURN
      END SUBROUTINE FMINP2

      SUBROUTINE FMINP3(LINE,KSTART,KSTOP,MA)

!  Internal routine to see if the input character string in LINE would overflow or underflow
!  with the current base and precision.

!  MA is returned as + or - over/underflow or unknown.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: KSTART,KSTOP
      CHARACTER :: LINE(KSTOP)
      INTEGER :: J,JSTATE,KDIGFL,KPT,KSIGN,KSIGNX,KTYPE,KVAL,N1,N2
      DOUBLE PRECISION :: X,F1,F2,FEXP,FLARGE

      INTEGER :: JTRANS(8,4) = RESHAPE(  (/     &
                   2, 9, 7, 7, 7, 7, 9, 9,      &
                   3, 3, 3, 5, 5, 8, 8, 8,      &
                   4, 4, 4, 9, 9, 9, 9, 9,      &
                   6, 6, 6, 6, 6, 9, 9, 9   /)  &
        , (/ 8,4 /) )

      CHARACTER :: KBLANK = ' '
      INTENT (IN) :: LINE,KSTART,KSTOP
      INTENT (INOUT) :: MA

!             X will approximate the log of the magnitude of the number represented by LINE.

      JSTATE = 1
      KSIGN = 1
      F1 = 0
      F2 = 0
      N1 = 0
      N2 = 0
      KSIGNX = 1
      FEXP = 0
      FLARGE = HUGE(F1)/31

!             KDIGFL will be 1 if any digits are found before 'E'.

      KDIGFL = 0

!             Initialize two hash tables that are used for character look-up during
!             input conversion.

      IF (LHASH == 0) CALL FMHTBL

!             Scan the number.

      DO J = KSTART, KSTOP
         IF (LINE(J) == KBLANK) CYCLE
         KPT = ICHAR(LINE(J))
         IF (KPT < LHASH1 .OR. KPT > LHASH2) THEN
             WRITE (KW,                                                       &
                "(/' Error in input conversion.'/"                        //  &
                "' ICHAR function was out of range for the current',"     //  &
                "' dimensions.'/' ICHAR(''',A,''') gave the value ',"     //  &
                "I12,', which is outside the currently'/' dimensioned',"  //  &
                "' bounds of (',I5,':',I5,') for variables KHASHT ',"     //  &
                "'and KHASHV.'/' Re-define the two parameters ',"         //  &
                "'LHASH1 and LHASH2 so the dimensions will'/' contain',"  //  &
                "' all possible output values from ICHAR.'//)"                &
                   ) LINE(J),KPT,LHASH1,LHASH2
             KTYPE = 5
             KVAL  = 0
         ELSE
             KTYPE = KHASHT(KPT)
             KVAL  = KHASHV(KPT)
         ENDIF
         IF (KTYPE >= 5) GO TO 110

         JSTATE = JTRANS(JSTATE,KTYPE)

         SELECT CASE (JSTATE)

!             State 2.  Sign of the number.

         CASE (2)
             KSIGN = KVAL

!             State 3.  Digits before a decimal point.

         CASE (3)
             KDIGFL = 1
             IF (F1 < FLARGE) THEN
                 F1 = 10.0D0*F1 + KVAL
             ELSE
                 N1 = N1 + 1
             ENDIF

!             State 4.  Decimal point

         CASE (4)
             CYCLE

!             State 5.  Digits after a decimal point.

         CASE (5)
             KDIGFL = 1
             IF (F2 < FLARGE) THEN
                 F2 = 10.0D0*F2 + KVAL
                 N2 = N2 + 1
             ENDIF

!             State 6.  Precision indicator.

         CASE (6)
             IF (KDIGFL == 0) F1 = 1.0D0

!             State 7.  Sign of the exponent.

         CASE (7)
             KSIGNX = KVAL

!             State 8.  Digits of the exponent.

         CASE (8)
             IF (FEXP < FLARGE) THEN
                 FEXP = 10*FEXP + KVAL
             ENDIF

         CASE DEFAULT
             GO TO 110

         END SELECT

      ENDDO

!             Check to see if the number would over/underflow.

      IF (F1 > 0) THEN
          X = LOG(F1) + (FEXP+N1)*LOG(10.0D0)
      ELSE IF (F2 > 0) THEN
          X = LOG(F2) - N2*LOG(10.0D0) + FEXP*LOG(10.0D0)
      ENDIF
      IF (X > (MXEXP2/2.0D0)*LOG(DBLE(MBASE)) .AND. KSIGNX > 0) THEN
          CALL FMIM(0,MA)
          MA%MP(2) = MEXPOV
          MA%MP(3) = 1
          MA%MP(1) = KSIGN
          KFLAG = -5
      ELSE IF (X > (MXEXP2/2.0D0)*LOG(DBLE(MBASE)) .AND. KSIGNX < 0) THEN
          CALL FMIM(0,MA)
          MA%MP(2) = MEXPUN
          MA%MP(3) = 1
          MA%MP(1) = KSIGN
          KFLAG = -6
      ELSE
          GO TO 110
      ENDIF

      RETURN

!             Error in converting the number.

  110 CALL FMIM(0,MA)
      MA%MP(2) = MUNKNO
      MA%MP(3) = 1
      MA%MP(1) = 1
      KFLAG = -7
      RETURN
      END SUBROUTINE FMINP3

      SUBROUTINE FMINPUT_ERROR
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: E(2) = 0
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) ' ***  Error in a program using the FM package  ***'
      WRITE (*,*) ' '
      WRITE (*,*) ' A multiple precision number is undefined in an expression or as an input'
      WRITE (*,*) ' argument to a subprogram.'
      WRITE (*,*) ' '
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
      IF (E(-NDIG) > -314159) WRITE (*,*) ' Negative array subscript.'
      STOP
      END SUBROUTINE FMINPUT_ERROR

      SUBROUTINE FMINT(MA,MB)

!  MB = INT(MA)

!  The integer part of MA is computed and returned in MB as a multiple precision floating
!  point number.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      INTEGER :: J,KA,KB,KRESLT,N1
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMINT'
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMARGS('FMINT    ',1,MA,MB,KRESLT)
          IF (KRESLT /= 0) THEN
              CALL FMRSLT(MA,MA,MB,KRESLT)
              IF (NTRACE /= 0) CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
              RETURN
          ENDIF
      ENDIF

      N1 = NDIG + 1

!             If MA is less than one in magnitude, return zero.

      IF (MA%MP(2) <= 0) THEN
          DO J = 1, N1
             MB%MP(J+1) = 0
          ENDDO
          GO TO 110
      ENDIF

!             If the radix point is off the right end of MA then MA is already an integer.
!             Return MA.

      IF (MA%MP(2) >= NDIG) THEN
          DO J = 1, N1
             MB%MP(J+1) = MA%MP(J+1)
          ENDDO
          GO TO 110
      ENDIF

!             Here MA has both integer and fraction parts.  Replace the digits right of the
!             radix point by zeros.

      KA = INT(MA%MP(2)) + 2
      KB = KA - 1
      DO J = 1, KB
         MB%MP(J+1) = MA%MP(J+1)
      ENDDO

      DO J = KA, N1
         MB%MP(J+1) = 0
      ENDDO

  110 MB%MP(1) = MA%MP(1)
      IF (MB%MP(3) == 0) MB%MP(1) = 1
      IF (NTRACE /= 0) CALL FMNTR(1,MB,MB,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMINT

      SUBROUTINE FMIPWR(MA,IVAL,MB)

!  MB = MA ** IVAL

!  This routine performs the trace printing for integer power.  FMIPWR2 is used to do
!  the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMIPWR'
          CALL FMNTR(2,MA,MA,1,1)
          CALL FMNTRI(2,IVAL,0)

          CALL FMIPWR2(MA,IVAL,MB)

          CALL FMNTR(1,MB,MB,1,1)
      ELSE
          CALL FMIPWR2(MA,IVAL,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMIPWR

      SUBROUTINE FMIPWR2(MA,IVAL,MB)

!  MB = MA ** IVAL

!  Raise an FM number to an integer power.
!  The binary multiplication method used requires an average of 1.5 * LOG2(IVAL) multiplications.
!  MA may be negative.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JSIGN,K,KL,KR_RETRY,KWRNSV,NDSAVE
      REAL :: XVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0

!             Check for special cases.

      IF (MA%MP(2) == MUNKNO .OR. (IVAL <= 0 .AND. MA%MP(3) == 0)) THEN
          KFLAG = -4
          IF (IVAL <= 0 .AND. MA%MP(3) == 0) THEN
              NAMEST(NCALL) = 'FMIPWR'
              CALL FMWARN
          ENDIF
          CALL FMIM(0,MB)
          MB%MP(2) = MUNKNO
          MB%MP(3) = 1
          RETURN
      ENDIF

      IF (IVAL == 0) THEN
          CALL FMIM(1,MB)
          RETURN
      ENDIF

      IF (ABS(IVAL) == 1) THEN
          KWRNSV = KWARN
          KWARN = 0
          IF (IVAL == 1) THEN
              CALL FMEQ(MA,MB)
          ELSE
              CALL FMIM(1,MXY(1))
              CALL FMDIV2(MXY(1),MA,MB)
          ENDIF
          KWARN = KWRNSV
          RETURN
      ENDIF

      IF (MA%MP(3) == 0) THEN
          CALL FMEQ(MA,MB)
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPOV) THEN
          JSIGN = 1
          IF (MA%MP(1) < 0) JSIGN = -1
          CALL FMIM(0,MB)
          IF (IVAL > 0) THEN
              CALL FMIM(1,MB)
              MB%MP(2) = MEXPOV
              MB%MP(3) = 1
              MB%MP(1) = JSIGN**MOD(IVAL,2)
              KFLAG = -5
          ELSE
              CALL FMIM(1,MB)
              MB%MP(2) = MEXPUN
              MB%MP(3) = 1
              MB%MP(1) = JSIGN**MOD(IVAL,2)
              KFLAG = -6
          ENDIF
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPUN) THEN
          JSIGN = 1
          IF (MA%MP(1) < 0) JSIGN = -1
          CALL FMIM(0,MB)
          IF (IVAL > 0) THEN
              CALL FMIM(1,MB)
              MB%MP(2) = MEXPUN
              MB%MP(3) = 1
              MB%MP(1) = JSIGN**MOD(IVAL,2)
              KFLAG = -6
          ELSE
              CALL FMIM(1,MB)
              MB%MP(2) = MEXPOV
              MB%MP(3) = 1
              MB%MP(1) = JSIGN**MOD(IVAL,2)
              KFLAG = -5
          ENDIF
          RETURN
      ENDIF
      KR_RETRY = 0

!             Increase the working precision.

      NDSAVE = NDIG
  110 IF (NCALL == 1) THEN
          XVAL = ABS(IVAL)
          K = INT((5.0*REAL(DLOGTN) + LOG(XVAL))/ALOGMB + NGRD52 - 1)
          NDIG = MAX(NDIG+K,2)
          IF (KR_RETRY >= 1) THEN
              NDIG = MAX(NDIG,2*NDSAVE+10)
          ENDIF
      ELSE
          XVAL = ABS(IVAL)
          IF (XVAL > 10.0 .OR. REAL(MBASE) <= 999.0) THEN
              K = INT(LOG(XVAL)/ALOGMB + 1.0)
              NDIG = NDIG + K
          ENDIF
      ENDIF

!             Initialize.

      K = ABS(IVAL)
      KWRNSV = KWARN
      KWARN = 0
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

!             Handle small exponents by hand.

      IF (K == 2) THEN
          CALL FMSQR2(MXY(1),MXY(2))
          GO TO 130
      ENDIF
      IF (K == 3) THEN
          CALL FMSQR2(MXY(1),MXY(2))
          CALL FMMPY2_R1(MXY(2),MXY(1))
          GO TO 130
      ENDIF
      IF (K == 4) THEN
          CALL FMSQR2(MXY(1),MXY(2))
          CALL FMSQR2_R1(MXY(2))
          GO TO 130
      ENDIF
      IF (K == 5) THEN
          CALL FMSQR2(MXY(1),MXY(2))
          CALL FMSQR2_R1(MXY(2))
          CALL FMMPY2_R1(MXY(2),MXY(1))
          GO TO 130
      ENDIF

      IF (MOD(K,2) == 0) THEN
          CALL FMIM(1,MXY(2))
      ELSE
          CALL FMEQ(MXY(1),MXY(2))
      ENDIF

!             This is the multiplication loop.

  120 K = K/2
      CALL FMSQR2_R1(MXY(1))
      IF (MOD(K,2) == 1) CALL FMMPY2_R2(MXY(1),MXY(2))
      IF (K > 1) GO TO 120

!             Invert if the exponent is negative.

  130 IF (IVAL < 0) THEN
          CALL FMIM(1,MXY(1))
          CALL FMDIV2_R2(MXY(1),MXY(2))
      ENDIF
      KWARN = KWRNSV

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Round the result and return.

      CALL FMEQU(MXY(2),MB,NDIG,NDSAVE)
      NDIG = NDSAVE
      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMIPWR'
          CALL FMWARN
      ENDIF
      RETURN
      END SUBROUTINE FMIPWR2

      SUBROUTINE FMLG10(MA,MB)

!  MB = LOG10(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      REAL :: X
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0 .OR. MA%MP(1) < 0) THEN
          CALL FMENTR('FMLG10   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMLG10'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF

      IF (MA%MP(2) == 0 .OR. MA%MP(2) == 1) THEN
          X = REAL(MBASE)
          X = X**(INT(MA%MP(2))-1)*(REAL(MA%MP(3))+REAL(MA%MP(4))/X)
      ELSE
          X = 2.0
      ENDIF
      IF (X > 0.9 .AND. X < 1.1) NDIG = NDIG + 1

      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)

      CALL FMLN(MXY(2),MXY(3))
      IF (MBASE /= MBSLI .OR. NDIG > NDIGLI) THEN
          CALL FMLNI(10,MXY(1))
      ELSE
          CALL FMADD(MLN2,MLN5,MXY(1))
      ENDIF
      CALL FMDIV(MXY(3),MXY(1),MXY(2))

!             Round the result and return.

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(2),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMLG10

      SUBROUTINE FMLN(MA,MB)

!  MB = LOG(MA)     (Natural logarithm)

      USE ModLib_FMVALS
      IMPLICIT NONE

      DOUBLE PRECISION :: Y
      TYPE(MULTI) :: MA,MB
      INTEGER :: JEXP(8,4),KEXP(4),NSTACK(49)
      REAL (KIND(1.0D0)) :: MA1,MXSAVE
      DOUBLE PRECISION :: ERR,XV(8)
      INTEGER :: IEXTRA,IVAL,J,J2,K,K2,K2EXP,KE1,KL,KM1,KR_RETRY,KOVUN,KRESLT,KSCALE,KST,  &
                 KWRNSV,LAST,M,N1,N3,NDSAV1,NDSAVE,NDSV,NMETHD,NTERM
      REAL :: TJ,X
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(6),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0 .OR. MA%MP(1) < 0) THEN
          CALL FMENTR('FMLN     ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMLN'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      MA1 = MA%MP(2)
      CALL FMEQU(MA,MXY(5),NDSAVE,NDIG)

!             Check to see if the argument is a small integer.  If so use FMLNI.

      KM1 = 0
      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(5),IVAL)
      KWARN = KWRNSV
      IF (KFLAG == 0 .AND. IVAL < MXBASE) THEN
          CALL FMLNI(IVAL,MXY(6))
          GO TO 180
      ENDIF

!             See if the argument can be scaled to a small integer.

      N3 = NDIG + 3
      N1 = NDIG + 1
      DO J = 2, N1
         IF (MXY(5)%MP(N3-J+1) /= 0) THEN
             LAST = N3 - J - 1
             GO TO 120
         ENDIF
      ENDDO

  120 KSCALE = INT(MA1) - LAST
      MXY(5)%MP(2) = LAST
      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(5),IVAL)
      KWARN = KWRNSV
      IF (KFLAG == 0 .AND. IVAL < MXBASE) THEN
          CALL FMLNI(IVAL,MXY(4))
          IF (IVAL == 1) KM1 = 1
          K2EXP = 0
          GO TO 170
      ENDIF

      NMETHD = 1
      IF (NDIG*DLOGMB/DLOGTN > 110000) NMETHD = 2
      IF (NMETHD == 2) GO TO 150
      IF (NMETHD == 3) GO TO 160

!             Method 1.  Convert MA to a value x close to 1, then use the Taylor series:
!                        Ln(x) = 2*( t + t^3/3 + t^5/5 + ...), where t = (x-1)/(x+1).

!             The argument will be moved closer to 1 by removing the base mbase exponent, and then
!             multiplying by powers of 2, 3, 5, 7 before the series is summed.  The series will be
!             added as J2 concurrent series.

      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
      MXY(2)%MP(2) = 0
      CALL FMM2DP(MXY(2),Y)
      KE1 = 0
      IF (Y*MBASE < 1 + 1.0D-7) THEN
          Y = Y * MBASE
          MXY(2)%MP(2) = 1
          KE1 = 1
      ENDIF
      JEXP(1,1:4) = (/    1,   1,  -1,   0 /)
      JEXP(2,1:4) = (/    2,   2,  -1,  -1 /)
      JEXP(3,1:4) = (/    6,   0,  -5,   2 /)
      JEXP(4,1:4) = (/   -5,  -1,  -2,   4 /)
      JEXP(5,1:4) = (/    3, -13,  10,  -2 /)
      JEXP(6,1:4) = (/   51, -13,  -1, -10 /)
      JEXP(7,1:4) = (/    9, -28,  37, -18 /)
      JEXP(8,1:4) = (/ -193,  -6,   5,  68 /)
      KEXP(1:4) = 0
      XV(1:8) = (/ 1.2000000000000000D0, 1.0285714285714286D0, 1.0035200000000000D0,  &
                   1.0004166666666667D0, 1.0000406160495965D0, 1.0000070529094230D0,  &
                   1.0000006193987026D0, 1.0000001178537107D0  /)
      DO
         IF (7*Y <= 1 + 1.0D-7) THEN
             Y = 7*Y
             KEXP(4) = KEXP(4) + 1
         ELSE
             EXIT
         ENDIF
      ENDDO
      IF (5*Y <= 1 + 1.0D-7) THEN
          Y = 5*Y
          KEXP(3) = KEXP(3) + 1
      ENDIF
      IF (3*Y <= 1 + 1.0D-7) THEN
          Y = 3*Y
          KEXP(2) = KEXP(2) + 1
      ENDIF
      IF (2*Y <= 1 + 1.0D-7) THEN
          Y = 2*Y
          KEXP(1) = KEXP(1) + 1
      ENDIF
      K2 = 5 + SQRT(NDSAVE*DLOGMB/DLOGTN)/15
      IF (K2 < 5) K2 = 5
      IF (K2 > 8) K2 = 8
      DO J = 1, K2
         K = -LOG(Y)/LOG(XV(J))
         Y = Y * 2.0D0**(K*JEXP(J, 1))
         KEXP(1) = KEXP(1) + K*JEXP(J, 1)
         Y = Y * 3.0D0**(K*JEXP(J, 2))
         KEXP(2) = KEXP(2) + K*JEXP(J, 2)
         Y = Y * 5.0D0**(K*JEXP(J, 3))
         KEXP(3) = KEXP(3) + K*JEXP(J, 3)
         Y = Y * 7.0D0**(K*JEXP(J, 4))
         KEXP(4) = KEXP(4) + K*JEXP(J, 4)
      ENDDO
      CALL FMI2M(1,MXY(3))
      IF (KEXP(1) > 0) THEN
          CALL FMCSMPYIN_R1(MXY(2),2,KEXP(1))
      ELSE IF (KEXP(1) < 0) THEN
          CALL FMCSMPYIN_R1(MXY(3),2,-KEXP(1))
      ENDIF
      IF (KEXP(2) > 0) THEN
          CALL FMCSMPYIN_R1(MXY(2),3,KEXP(2))
      ELSE IF (KEXP(2) < 0) THEN
          CALL FMCSMPYIN_R1(MXY(3),3,-KEXP(2))
      ENDIF
      IF (KEXP(3) > 0) THEN
          CALL FMCSMPYIN_R1(MXY(2),5,KEXP(3))
      ELSE IF (KEXP(3) < 0) THEN
          CALL FMCSMPYIN_R1(MXY(3),5,-KEXP(3))
      ENDIF
      IF (KEXP(4) > 0) THEN
          CALL FMCSMPYIN_R1(MXY(2),7,KEXP(4))
      ELSE IF (KEXP(4) < 0) THEN
          CALL FMCSMPYIN_R1(MXY(3),7,-KEXP(4))
      ENDIF
      IF (KEXP(1) < 0 .OR. KEXP(2) < 0 .OR. KEXP(3) < 0 .OR. KEXP(4) < 0) THEN
          CALL FMDIV_R1(MXY(2),MXY(3))
      ENDIF
      CALL FMI2M(1,MXY(3))
      CALL FMSUB(MXY(2),MXY(3),MXY(4))
      CALL FMADD(MXY(2),MXY(3),MXY(5))
      CALL FMDIV(MXY(4),MXY(5),MXY(1))

      TJ = 1.25D0 + (NDSAVE*DLOGMB/DLOGTN)**0.6D0/18
      J2 = INT(TJ)
      J2 = MAX(1,MIN(J2,LJSUMS))
      NDSAV1 = NDIG

!             Split into J2 concurrent sums and reduce NDIG while computing each term in the sum as
!             the terms get smaller.

      CALL FMI2M(1,MXY(2))
      NTERM = 1
      DO J = 1, J2
         CALL FMCSDIVI(MXY(2),NTERM,MJSUMS(J))
         NTERM = NTERM + 2
      ENDDO
      CALL FMIPWR(MXY(1),2*J2,MXY(3))

  130 IF (NTERM > 2*J2+1) THEN
          CALL FMCSMPY_R1(MXY(2),MXY(3))
      ELSE
          CALL FMEQ(MXY(3),MXY(2))
      ENDIF
      DO J = 1, J2
         CALL FMCSDIVI(MXY(2),NTERM,MXY(4))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0) GO TO 140
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 2
      ENDDO
      GO TO 130

!             Put the J2 separate sums back together.

  140 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMSQR(MXY(1),MXY(4))
      CALL FMEQ(MJSUMS(J2),MXY(3))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(3),MXY(4))
         CALL FMADD_R1(MXY(3),MJSUMS(J2-J+1))
      ENDDO
      CALL FMMPY(MXY(3),MXY(1),MXY(6))
      CALL FMMPYI_R1(MXY(6),2)
      K = MA%MP(2) - KE1
      J = NDIG
      NDIG = NDIG + NGRD21
      CALL FMEQU_R1(MXY(6),J,NDIG)
      IF (K /= 0) THEN
          IF (NDIGLB >= NDIG .AND. MBASE == MBSLB) THEN
              CALL FMMPYI(MLBSAV,K,MXY(5))
          ELSE
              CALL FMLNI(INT(MBASE),MLBSAV)
              MBSLB = MBASE
              NDIGLB = NDIG
              CALL FMMPYI(MLBSAV,K,MXY(5))
          ENDIF
          CALL FMADD_R1(MXY(6),MXY(5))
      ENDIF
      IF (NDIGLI < NDIG .OR. MBASE /= MBSLI) THEN
          CALL FMLNI(210,MXY(2))
      ENDIF
      IF (KEXP(1) /= 0) THEN
          CALL FMCSMPYI(MLN2,KEXP(1),MXY(3))
          CALL FMSUB_R1(MXY(6),MXY(3))
      ENDIF
      IF (KEXP(2) /= 0) THEN
          CALL FMCSMPYI(MLN3,KEXP(2),MXY(3))
          CALL FMSUB_R1(MXY(6),MXY(3))
      ENDIF
      IF (KEXP(3) /= 0) THEN
          CALL FMCSMPYI(MLN5,KEXP(3),MXY(3))
          CALL FMSUB_R1(MXY(6),MXY(3))
      ENDIF
      IF (KEXP(4) /= 0) THEN
          CALL FMCSMPYI(MLN7,KEXP(4),MXY(3))
          CALL FMSUB_R1(MXY(6),MXY(3))
      ENDIF
      GO TO 180

!             Method 2.  Use AGM iteration.

  150 CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      MXY(1)%MP(2) = 0
      CALL FMM2DP(MXY(1),Y)
      M = MAX(1,INT((NDIG*DLOGMB/36 - LOG(Y))/DLOGTW + 1))
      CALL FMCSMPYIN_R1(MXY(1),2,M)
      CALL FMI2M(1,MXY(2))
      CALL FMDIV(MXY(2),MXY(1),MXY(4))
      CALL FMEQ(MXY(4),MXY(2))
      CALL FMSQR(MXY(2),MXY(5))
      CALL FMSQR(MXY(5),MXY(3))
      CALL FMSQR(MXY(3),MXY(5))
      CALL FMMPY(MXY(4),MXY(5),MXY(6))
      CALL FMADD_R1(MXY(2),MXY(6))
      CALL FMSQR_R1(MXY(5))
      CALL FMADD_R1(MXY(3),MXY(5))
      CALL FMMPY_R1(MXY(5),MXY(6))
      CALL FMADD_R1(MXY(2),MXY(5))
      CALL FMMPYI_R1(MXY(2),2)
      CALL FMMPYI_R1(MXY(3),2)
      CALL FMI2M(1,MXY(5))
      CALL FMADD_R1(MXY(3),MXY(5))
      CALL FMMPY(MXY(2),MXY(3),MXY(5))
      CALL FMSQR_R1(MXY(2))
      CALL FMSQR_R1(MXY(3))
      CALL FMADD(MXY(2),MXY(3),MXY(4))
      CALL FMDIVI(MXY(4),2,MXY(2))
      CALL FMEQ(MXY(5),MXY(3))
      DO
         CALL FMSUB(MXY(3),MXY(2),MXY(4))
         IF (MXY(4)%MP(3) == 0 .OR.  &
             MXY(4)%MP(2) - MAX(MXY(2)%MP(2),MXY(3)%MP(2)) <= -NDIG+1) EXIT
         CALL FMDIVI_R1(MXY(4),2)
         CALL FMMPY(MXY(2),MXY(3),MXY(5))
         CALL FMSQRT(MXY(5),MXY(3))
         CALL FMADD_R1(MXY(2),MXY(4))
      ENDDO

      CALL FMPI(MXY(1))
      CALL FMMPYI_R1(MXY(2),4)
      CALL FMDIV_R1(MXY(1),MXY(2))
      IF (NDIGLI >= NDIG .AND. MBASE == MBSLI) THEN
          CALL FMMPYI(MLN2,M,MXY(3))
      ELSE
          CALL FMLNI(2,MXY(2))
          CALL FMMPYI(MXY(2),M,MXY(3))
      ENDIF
      CALL FMSUB(MXY(1),MXY(3),MXY(6))
      K = MA%MP(2)
      IF (K /= 0) THEN
          IF (NDIGLB >= NDIG .AND. MBASE == MBSLB) THEN
              CALL FMMPYI(MLBSAV,K,MXY(5))
          ELSE
              CALL FMLNI(INT(MBASE),MLBSAV)
              MBSLB = MBASE
              NDIGLB = NDIG
              CALL FMMPYI(MLBSAV,K,MXY(5))
          ENDIF
          CALL FMADD_R1(MXY(6),MXY(5))
      ENDIF
      GO TO 180

!             Method 3.  Use Newton iteration.

  160 MA1 = MA%MP(2)
      CALL FMEQU(MA,MXY(5),NDSAVE,NDIG)

!             Compute IEXTRA, the number of extra digits required.

      CALL FMI2M(1,MXY(4))
      CALL FMSUB_R1(MXY(4),MXY(5))
      IF (MA%MP(2) == 0 .OR. MA%MP(2) == 1) THEN
          X = REAL(MBASE)
          X = X**(INT(MA%MP(2))-1)*(REAL(MA%MP(3))+REAL(MA%MP(4))/X)
      ELSE
          X = 2.0
      ENDIF
      IEXTRA = MAX(0-INT(MXY(4)%MP(2)),0)
      IF (X > 0.9 .AND. X < 1.1) IEXTRA = IEXTRA + 1
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(5),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA

!             Scale the argument to lie between e/2 and e to speed up the calls to FMEXP.

      MXY(5)%MP(2) = 1
      KSCALE = INT(MA1) - 1
      CALL FMM2DP(MXY(5),Y)
      K2EXP = INT(LOG(2.0*REAL(Y)/2.71828)/0.693147)
      IF (Y < 1.359141) THEN
          K2EXP = -1
          CALL FMMPYI_R1(MXY(5),2)
          Y = 2.0D0*Y
      ELSE
          K2 = 2**K2EXP
          CALL FMDIVI_R1(MXY(5),K2)
          Y = Y/K2
      ENDIF

!             Generate the initial approximation.

      CALL FMI2M(0,MXY(2))
      CALL FMI2M(0,MXY(6))
      Y = LOG(Y)
      CALL FMDPM(Y,MXY(4))
      CALL FMDIG(NSTACK,KST)

!             Newton iteration.

      DO J = 1, KST
         NDIG = NSTACK(J)
         CALL FMEXP(MXY(4),MXY(6))
         CALL FMSUB(MXY(5),MXY(6),MXY(2))
         CALL FMDIV_R2(MXY(2),MXY(6))
         CALL FMADD_R1(MXY(4),MXY(6))
      ENDDO

!             Compute LN(MBASE**KSCALE).

  170 IF ((MBSLB /= MBASE .OR. NDIGLB < NDIG) .AND. KSCALE /= 0) THEN
          NDSV = NDIG
          NDIG = NDIG + 2 + NDIG/100
          CALL FMLNI(INT(MBASE),MLBSAV)
          MBSLB = MBASE
          NDIGLB = NDIG
          IF (ABS(MLBSAV%MP(2)) > 10) NDIGLB = 0
          NDIG = NDSV
      ENDIF

      IF (KSCALE /= 0 .AND. KM1 == 0) THEN
          CALL FMMPYI(MLBSAV,KSCALE,MXY(6))
          CALL FMADD_R2(MXY(4),MXY(6))
      ELSE IF (KSCALE /= 0 .AND. KM1 == 1) THEN
          CALL FMMPYI(MLBSAV,KSCALE,MXY(6))
      ELSE IF (KSCALE == 0 .AND. KM1 == 0) THEN
          CALL FMEQ(MXY(4),MXY(6))
      ELSE IF (KSCALE == 0 .AND. KM1 == 1) THEN
          CALL FMI2M(0,MXY(6))
      ENDIF

      IF (K2EXP /= 0) THEN
          IF (MBASE /= MBSLI .OR. NDIG > NDIGLI) THEN
              CALL FMLNI(2,MXY(4))
          ENDIF
          CALL FMMPYI(MLN2,K2EXP,MXY(4))
          CALL FMADD_R1(MXY(6),MXY(4))
      ENDIF

!             Round the result and return.

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  180 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(6)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(6),MB,NDSAVE,MXSAVE,KOVUN)
      IF (KFLAG == 1) KFLAG = 0
      RETURN
      END SUBROUTINE FMLN

      SUBROUTINE FMLNI(IVAL,MA)

!  MA = LOG(IVAL)

!  Compute the natural logarithm of an integer IVAL.

!  If IVAL has only powers of 2, 3, 5, and 7 in its factorization then FMLNI is faster than FMLN.
!  Otherwise, if IVAL >= MXBASE (i.e., IVAL does not fit in 1/2 word) then FMLN is usually faster.

!  Use FMLN instead of FMLNI if 10*IVAL would cause integer overflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      CHARACTER(155) :: STRING
      INTEGER :: INT2,J,J2,J3,J5,J7,JTEMP2,JTEMP3,JTEMP5,JTEMP7,K,K2,K3,K5,K7,KDELTA,KL,  &
                 KR_RETRY,LAST,N,ND,NDMB,NDSAVE,NDSV,NT
      REAL :: XVAL
      DOUBLE PRECISION :: ERR
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA
      TYPE(MULTI) :: MXY(6)

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMLNI'
      IF (NTRACE /= 0) CALL FMNTRI(2,IVAL,1)

!             Check for special cases.

      IF (IVAL <= 0) THEN
          KFLAG = -4
          CALL FMWARN
          CALL FMST2M('UNKNOWN',MA)
          IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

      IF (IVAL == 1) THEN
          CALL FMI2M(0,MA)
          IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

!             Increase the working precision.

      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = NGRD52
          NDIG = MAX(NDIG+K,2)
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF

!             Find integers K2, K3, K5, and K7 such that
!                NT = 2**K2 * 3**K3 * 5**K5 * 7**K7
!             is a good approximation of IVAL.
!             KDELTA = ABS(IVAL - NT).

      INT2 = IVAL
      IF (IVAL > INTMAX/100) INT2 = IVAL/100
      KDELTA = INT2
      NT = 0
      K2 = 0
      K3 = 0
      K5 = 0
      K7 = 0

!             Start the search loop.

      XVAL = INT2
      LAST = INT(LOG(DBLE(XVAL))/DLOGTW + 2.0D0)

      JTEMP7 = 1
      DO J7 = 1, LAST
         IF (JTEMP7 > INT2 .AND. ABS(JTEMP7-INT2) > KDELTA) GO TO 150

         JTEMP5 = JTEMP7
         DO J5 = 1, LAST
            IF (JTEMP5 > INT2 .AND. ABS(JTEMP5-INT2) > KDELTA) GO TO 140

            JTEMP3 = JTEMP5
            DO J3 = 1, LAST
               IF (JTEMP3 > INT2 .AND. ABS(JTEMP3-INT2) > KDELTA) GO TO 130

               JTEMP2 = JTEMP3
               DO J2 = 1, LAST
                  IF (ABS(JTEMP2-INT2) <= KDELTA) THEN
                      IF (ABS(JTEMP2-INT2) == KDELTA .AND. JTEMP2 < INT2) GO TO 120
                      KDELTA = ABS(JTEMP2-INT2)
                      NT = JTEMP2
                      K2 = J2 - 1
                      K3 = J3 - 1
                      K5 = J5 - 1
                      K7 = J7 - 1
                      IF (KDELTA == 0) GO TO 150
                  ENDIF
                  IF (JTEMP2 > INT2) GO TO 120

                  JTEMP2 = 2*JTEMP2
               ENDDO

  120          JTEMP3 = 3*JTEMP3
            ENDDO

  130       JTEMP5 = 5*JTEMP5
         ENDDO

  140    JTEMP7 = 7*JTEMP7
      ENDDO

!             If IVAL was too close to the integer overflow limit, restore NT to an
!             approximation of IVAL.

  150 IF (INT2 /= IVAL) THEN
          IF (NT <= INT2) THEN
              NT = NT*100
              K2 = K2 + 2
              K5 = K5 + 2
          ELSE IF (NT <= IVAL/98) THEN
              NT = NT*98
              K2 = K2 + 1
              K7 = K7 + 2
          ELSE
              NT = NT*70
              K2 = K2 + 1
              K5 = K5 + 1
              K7 = K7 + 1
          ENDIF
      ENDIF

!             End of the search.
!             Now compute LN(NT) as a linear combination of LN(2), LN(3), LN(5), and LN(7).

      IF (MBASE /= MBSLI .OR. NDIG > NDIGLI) THEN
          NDMB = INT(150.0*2.302585/ALOGMB)
          IF (NDMB >= NDIG) THEN
              NDSV = NDIG
              NDIG = NDMB
              STRING = '0.693147180559945309417232121458176568075500'//  &
              '13436025525412068000949339362196969471560586332699641'//  &
              '8687542001481020570685733685520235758130557032670751635'
              CALL FMST2M(STRING,MLN2)
              STRING = '1.098612288668109691395245236922525704647490'//  &
              '55782274945173469433363749429321860896687361575481373'//  &
              '2088787970029065957865742368004225930519821052801870767'
              CALL FMST2M(STRING,MLN3)
              STRING = '1.609437912434100374600759333226187639525601'//  &
              '35426851772191264789147417898770765776463013387809317'//  &
              '9610799966303021715562899724005229324676199633616617464'
              CALL FMST2M(STRING,MLN5)
              STRING = '1.945910149055313305105352743443179729637084'//  &
              '72958186118845939014993757986275206926778765849858787'//  &
              '1526993061694205851140911723752257677786843148958095164'
              CALL FMST2M(STRING,MLN7)
              MBSLI = MBASE
              NDIGLI = NDIG
              IF (ABS(MLN2%MP(2)) > 10 .OR. ABS(MLN3%MP(2)) > 10 .OR.  &
                  ABS(MLN5%MP(2)) > 10 .OR. ABS(MLN7%MP(2)) > 10) NDIGLI = 0
          ELSE
              NDSV = NDIG
              NDIG = NDIG + 2 + NDIG/100
              MBSLI = MBASE
              NDIGLI = NDIG

!                 If precision is high, use the binary splitting method.

              IF (NDIG < 40) THEN

                  CALL FMLNI2(1,126,MLN2)
                  CALL FMLNI2(1,225,MLN3)
                  CALL FMLNI2(1,2401,MLN5)
                  CALL FMLNI2(1,4375,MLN7)

!                    Get Ln(2).

                  CALL FMMPYI_R1(MLN2,-72)
                  CALL FMMPYI(MLN3,-27,MXY(3))
                  CALL FMADD_R1(MLN2,MXY(3))
                  CALL FMMPYI(MLN5,19,MXY(3))
                  CALL FMADD_R1(MLN2,MXY(3))
                  CALL FMMPYI(MLN7,-31,MXY(3))
                  CALL FMADD_R1(MLN2,MXY(3))

!                    Get Ln(3).

                  CALL FMMPYI_R1(MLN3,-3)
                  CALL FMMPYI(MLN2,19,MXY(3))
                  CALL FMADD_R1(MLN3,MXY(3))
                  CALL FMSUB_R1(MLN3,MLN5)
                  CALL FMADD_R1(MLN3,MLN7)
                  CALL FMDIVI_R1(MLN3,12)

!                    Get Ln(5).

                  CALL FMSUB_R1(MLN5,MLN2)
                  CALL FMMPYI(MLN3,27,MXY(3))
                  CALL FMADD_R1(MLN5,MXY(3))
                  CALL FMMPYI(MLN7,-4,MXY(3))
                  CALL FMADD_R1(MLN5,MXY(3))
                  CALL FMDIVI_R1(MLN5,18)

!                    Get Ln(7).

                  CALL FMSUB_R2(MLN2,MLN7)
                  CALL FMMPYI(MLN3,7,MXY(3))
                  CALL FMADD_R1(MLN7,MXY(3))
                  CALL FMMPYI(MLN5,-4,MXY(3))
                  CALL FMADD_R1(MLN7,MXY(3))
              ELSE
                  CALL FMLNI3
              ENDIF
          ENDIF
          IF (ABS(MLN2%MP(2)) > 10 .OR. ABS(MLN3%MP(2)) > 10 .OR.  &
              ABS(MLN5%MP(2)) > 10 .OR. ABS(MLN7%MP(2)) > 10) NDIGLI = 0
          NDIG = NDSV
      ENDIF

!             If NT /= IVAL then the final step is to compute LN(IVAL/NT) and then use
!             LN(IVAL) = LN(IVAL/NT) + LN(NT).

      IF (NT /= IVAL) THEN
          ND = NT - IVAL
          IF (NDIG < 40) THEN
              CALL FMLNI2(ND,NT,MXY(3))
          ELSE
              IF (IVAL == NT-1) THEN
                  N = 2*IVAL + 1
                  K = NDIG*DLOGMB/LOG(DBLE(N)**2) + 10
                  CALL FMLNI3_QBT(0,K,N,MXY(1),MXY(2),MXY(3))
                  CALL IMI2FM(MXY(1),MXY(4))
                  CALL IMI2FM(MXY(2),MXY(5))
                  CALL IMI2FM(MXY(3),MXY(6))
                  CALL FMDIV(MXY(6),MXY(5),MXY(2))
                  CALL FMDIV(MXY(2),MXY(4),MXY(1))
                  CALL FMMPYI(MXY(1),-2,MXY(2))
                  CALL FMDIVI(MXY(2),N,MXY(3))
              ELSE IF (IVAL == NT+1) THEN
                  N = 2*IVAL - 1
                  K = NDIG*DLOGMB/LOG(DBLE(N)**2) + 10
                  CALL FMLNI3_QBT(0,K,N,MXY(1),MXY(2),MXY(3))
                  CALL IMI2FM(MXY(1),MXY(4))
                  CALL IMI2FM(MXY(2),MXY(5))
                  CALL IMI2FM(MXY(3),MXY(6))
                  CALL FMDIV(MXY(6),MXY(5),MXY(2))
                  CALL FMDIV(MXY(2),MXY(4),MXY(1))
                  CALL FMMPYI(MXY(1),2,MXY(2))
                  CALL FMDIVI(MXY(2),N,MXY(3))
              ELSE IF (IVAL == NT-2) THEN
                  N = IVAL + 1
                  K = NDIG*DLOGMB/LOG(DBLE(N)**2) + 10
                  CALL FMLNI3_QBT(0,K,N,MXY(1),MXY(2),MXY(3))
                  CALL IMI2FM(MXY(1),MXY(4))
                  CALL IMI2FM(MXY(2),MXY(5))
                  CALL IMI2FM(MXY(3),MXY(6))
                  CALL FMDIV(MXY(6),MXY(5),MXY(2))
                  CALL FMDIV(MXY(2),MXY(4),MXY(1))
                  CALL FMMPYI(MXY(1),-2,MXY(2))
                  CALL FMDIVI(MXY(2),N,MXY(3))
              ELSE IF (IVAL == NT+2) THEN
                  N = IVAL - 1
                  K = NDIG*DLOGMB/LOG(DBLE(N)**2) + 10
                  CALL FMLNI3_QBT(0,K,N,MXY(1),MXY(2),MXY(3))
                  CALL IMI2FM(MXY(1),MXY(4))
                  CALL IMI2FM(MXY(2),MXY(5))
                  CALL IMI2FM(MXY(3),MXY(6))
                  CALL FMDIV(MXY(6),MXY(5),MXY(2))
                  CALL FMDIV(MXY(2),MXY(4),MXY(1))
                  CALL FMMPYI(MXY(1),2,MXY(2))
                  CALL FMDIVI(MXY(2),N,MXY(3))
              ELSE
                  CALL FMLNI4(IVAL,NT,MXY(3))
              ENDIF
          ENDIF
      ENDIF

      CALL FMMPYI(MLN2,K2,MXY(2))
      CALL FMMPYI(MLN3,K3,MXY(1))
      CALL FMADD_R1(MXY(2),MXY(1))
      CALL FMMPYI(MLN5,K5,MXY(1))
      CALL FMADD_R1(MXY(2),MXY(1))
      CALL FMMPYI(MLN7,K7,MXY(1))
      IF (NT /= IVAL) CALL FMADD_R1(MXY(2),MXY(3))
      CALL FMADD(MXY(2),MXY(1),MXY(3))

!             Round and move the result to MA.


!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEQU(MXY(3),MA,NDIG,NDSAVE)
      NDIG = NDSAVE
      IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMLNI

      SUBROUTINE FMLNI2(INT1,INT2,MA)

!  MA = LN(1 - INT1/INT2)

!  Taylor series for computing the logarithm of a rational number near 1.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: INT1,INT2
      TYPE(MULTI) :: MA
      INTEGER :: J,NDSAVE
      INTENT (IN) :: INT1,INT2
      INTENT (INOUT) :: MA
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMI2M(INT1,MXY(2))
      CALL FMDIVI_R1(MXY(2),INT2)
      CALL FMEQ(MXY(2),MA)
      NDSAVE = NDIG
      J = 1

  110 J = J + 1
      IF (INT1 /= 1) CALL FMMPYI_R1(MXY(2),INT1)
      CALL FMDIVI_R1(MXY(2),INT2)
      CALL FMDIVI(MXY(2),J,MXY(1))
      NDIG = NDSAVE
      CALL FMADD_R1(MA,MXY(1))
      NDIG = NDSAVE - INT(MA%MP(2)-MXY(1)%MP(2))
      IF (NDIG < NGRD22) NDIG = NGRD22
      IF (KFLAG /= 1) GO TO 110

      NDIG = NDSAVE
      IF (MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0) MA%MP(1) = -MA%MP(1)
      RETURN
      END SUBROUTINE FMLNI2

      SUBROUTINE FMLNI3

!  Binary splitting version.  Faster for large NDIG.

!  Compute the four saved constants Ln(2), Ln(3), Ln(5), Ln(7).

      USE ModLib_FMVALS
      IMPLICIT NONE

      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: K,NDSAVE,N
      TYPE(MULTI) :: MXY(6)

      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = MAX(NGRD52-1,2)
          NDIG = MAX(NDIG+K,2)
      ENDIF
      MXSAVE = MXEXP
      MXEXP = MXEXP2

!             Determine K, the number of terms to sum in the series for
!             Ln( (N+1) / (N-1) ) = (2/N)*( 1 + 1/(3*d) + 1/(5*d**2) + ...), with d = N**2.
!             Four calls are made:  N = 251, 449, 4801, 8549.

      N = 251
      K = NDIG*DLOGMB/LOG(DBLE(N)**2) + 10
      CALL FMLNI3_QBT(0,K,N,MXY(1),MXY(2),MXY(3))
      CALL IMI2FM(MXY(1),MXY(4))
      CALL IMI2FM(MXY(2),MXY(5))
      CALL IMI2FM(MXY(3),MXY(6))
      CALL FMDIV(MXY(6),MXY(5),MXY(2))
      CALL FMDIV(MXY(2),MXY(4),MXY(1))
      CALL FMMPYI(MXY(1),2,MXY(2))
      CALL FMDIVI(MXY(2),N,MLN2)

      N = 449
      K = NDIG*DLOGMB/LOG(DBLE(N)**2) + 10
      CALL FMLNI3_QBT(0,K,N,MXY(1),MXY(2),MXY(3))
      CALL IMI2FM(MXY(1),MXY(4))
      CALL IMI2FM(MXY(2),MXY(5))
      CALL IMI2FM(MXY(3),MXY(6))
      CALL FMDIV(MXY(6),MXY(5),MXY(2))
      CALL FMDIV(MXY(2),MXY(4),MXY(1))
      CALL FMMPYI(MXY(1),2,MXY(2))
      CALL FMDIVI(MXY(2),N,MLN3)

      N = 4801
      K = NDIG*DLOGMB/LOG(DBLE(N)**2) + 10
      CALL FMLNI3_QBT(0,K,N,MXY(1),MXY(2),MXY(3))
      CALL IMI2FM(MXY(1),MXY(4))
      CALL IMI2FM(MXY(2),MXY(5))
      CALL IMI2FM(MXY(3),MXY(6))
      CALL FMDIV(MXY(6),MXY(5),MXY(2))
      CALL FMDIV(MXY(2),MXY(4),MXY(1))
      CALL FMMPYI(MXY(1),2,MXY(2))
      CALL FMDIVI(MXY(2),N,MLN5)

      N = 8749
      K = NDIG*DLOGMB/LOG(DBLE(N)**2) + 10
      CALL FMLNI3_QBT(0,K,N,MXY(1),MXY(2),MXY(3))
      CALL IMI2FM(MXY(1),MXY(4))
      CALL IMI2FM(MXY(2),MXY(5))
      CALL IMI2FM(MXY(3),MXY(6))
      CALL FMDIV(MXY(6),MXY(5),MXY(2))
      CALL FMDIV(MXY(2),MXY(4),MXY(1))
      CALL FMMPYI(MXY(1),2,MXY(2))
      CALL FMDIVI(MXY(2),N,MLN7)

!             Get Ln(2).

      CALL FMMPYI_R1(MLN2,72)
      CALL FMMPYI(MLN3,27,MXY(3))
      CALL FMADD_R1(MLN2,MXY(3))
      CALL FMMPYI(MLN5,-19,MXY(3))
      CALL FMADD_R1(MLN2,MXY(3))
      CALL FMMPYI(MLN7,31,MXY(3))
      CALL FMADD_R1(MLN2,MXY(3))

!             Get Ln(3).

      CALL FMMPYI_R1(MLN3,3)
      CALL FMMPYI(MLN2,19,MXY(3))
      CALL FMADD_R1(MLN3,MXY(3))
      CALL FMADD_R1(MLN3,MLN5)
      CALL FMSUB_R1(MLN3,MLN7)
      CALL FMDIVI_R1(MLN3,12)

!             Get Ln(5).

      CALL FMADD_R1(MLN5,MLN2)
      CALL FMMPYI(MLN3,27,MXY(3))
      CALL FMSUB_R2(MXY(3),MLN5)
      CALL FMMPYI(MLN7,4,MXY(3))
      CALL FMADD_R1(MLN5,MXY(3))
      CALL FMDIVI_R1(MLN5,18)

!             Get Ln(7).

      CALL FMADD_R2(MLN2,MLN7)
      CALL FMMPYI(MLN3,7,MXY(3))
      CALL FMADD_R1(MLN7,MXY(3))
      CALL FMMPYI(MLN5,-4,MXY(3))
      CALL FMADD_R1(MLN7,MXY(3))

      MXEXP = MXSAVE
      NDIG = NDSAVE
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMLNI3

      RECURSIVE SUBROUTINE FMLNI3_QBT(A,B,N,MQ,MB,MT)

!  This routine does the binary splitting for computing the constant Ln( (N+1) / (N-1) ).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MQ,MB,MT
      INTEGER :: A,B,N
      INTENT (IN) :: A,B,N
      INTENT (INOUT) :: MQ,MB,MT
      TYPE(MULTI) :: MXY(6)
      INTEGER :: J,KM,RESULT_SIZE
      REAL (KIND(0.0D0)) :: DA,DB

      DA = A
      DB = B
      RESULT_SIZE = ( (DB - DA + 1)*LOG(DBLE(N)**2) ) / DLOGMB + 7
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MQ%MP)) THEN
          ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MQ%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MQ%MP)
          ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      J = RESULT_SIZE
      RESULT_SIZE = ( (DB+1.5D0)*LOG(DB+2) - DB + 1/(12*(DB+2)) -          &
                      ( (DA+0.5D0)*LOG(DA+1) - DA + 1/(12*(DA+1)) ) - 1 +  &
                      (DB-DA+1)*LOG(2.0D0) ) / DLOGMB + 7
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      RESULT_SIZE = RESULT_SIZE + J
      IF (.NOT. ALLOCATED(MT%MP)) THEN
          ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MT%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MT%MP)
          ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (B-A < 6) THEN
          IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(1)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(1)%MP)
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(2)%MP)) THEN
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(2)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(2)%MP)
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(3)%MP)) THEN
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(3)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(3)%MP)
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(4)%MP)) THEN
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(4)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(4)%MP)
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(5)%MP)) THEN
              ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(5)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(5)%MP)
              ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          CALL IMI2M(N,MXY(2))
          CALL IMSQR(MXY(2),MXY(1))
          CALL IMI2M(B-A+1,MXY(2))
          CALL IMPWR(MXY(1),MXY(2),MQ)

          CALL IMI2M(1,MB)
          DO J = A+1, B, 2
             CALL IMMPYI(MB,2*J-1,MXY(1))
             CALL IMMPYI(MXY(1),2*J+1,MB)
          ENDDO
          IF (MOD(B-A,2) == 0) THEN
              CALL IMMPYI(MB,2*B+1,MXY(1))
              CALL IMEQ(MXY(1),MB)
          ENDIF

!             MT is the sum
!             MXY(2) is (N*N)^(J+1)

          CALL IMDIVI(MB,2*B+1,MXY(1))
          CALL IMMPYI(MXY(1),N,MXY(2))
          CALL IMMPYI(MXY(2),N,MT)
          CALL IMI2M(N,MXY(1))
          CALL IMSQR(MXY(1),MXY(2))
          DO J = 1, B-A
             CALL IMDIVI(MB,2*B+1-2*J,MXY(3))
             CALL IMMPYI(MXY(2),N,MXY(4))
             CALL IMMPYI(MXY(4),N,MXY(2))
             CALL IMMPY(MXY(2),MXY(3),MXY(4))
             CALL IMADD(MT,MXY(4),MXY(5))
             CALL IMEQ(MXY(5),MT)
          ENDDO
          RETURN
      ENDIF

      KM = A/2 + B/2 + MOD(A,2)*MOD(B,2)
      CALL FMLNI3_QBT(A,KM-1,N,MXY(1),MXY(2),MXY(3))
      CALL FMLNI3_QBT(KM,B,N,MXY(4),MXY(5),MXY(6))
      CALL IMMPY(MXY(1),MXY(4),MQ)
      CALL IMMPY(MXY(2),MXY(5),MB)
      CALL IMMPY(MXY(5),MXY(4),MT)
      CALL IMMPY(MXY(3),MT,MXY(1))
      CALL IMMPY(MXY(2),MXY(6),MXY(3))
      CALL IMADD(MXY(1),MXY(3),MT)

      RETURN
      END SUBROUTINE FMLNI3_QBT

      SUBROUTINE FMLNI4(IVAL,NT,MA)

!  Binary splitting version.  Faster for large NDIG.

!  MA = Ln(IVAL/NT).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL,NT
      INTENT (IN) :: IVAL,NT
      INTENT (INOUT) :: MA
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: A,B,K,NDSAVE,LEVEL_OF_RECURSION
      TYPE(MULTI) :: MXY(7)

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = MAX(NGRD52-1,2)
          NDIG = MAX(NDIG+K,2)
      ENDIF
      MXSAVE = MXEXP
      MXEXP = MXEXP2

!             Determine K, the number of terms to sum in the series for
!             Ln( (B+A) / (B-A) ) = (2*A/B)*( 1 + 1/(3*d) + 1/(5*d^2) + ...), with d = (B/A)^2.

      A = ABS(IVAL - NT)
      B = IVAL + NT
      CALL FMGCDI(A,B)
      IF (IVAL-NT < 0) A = -A
      K = NDIG*DLOGMB/LOG(DBLE(B)**2/DBLE(A)**2) + 10
      LEVEL_OF_RECURSION = 0
      CALL FMLNI4_PQBT(0,K,A,B,MXY(1),MXY(2),MXY(3),MXY(4),LEVEL_OF_RECURSION)
      CALL IMI2FM(MXY(2),MXY(5))
      CALL IMI2FM(MXY(3),MXY(6))
      CALL IMI2FM(MXY(4),MXY(7))
      CALL FMDIV(MXY(7),MXY(6),MXY(2))
      CALL FMDIV(MXY(2),MXY(5),MXY(1))
      CALL FMMPYI(MXY(1),2*A,MXY(2))
      CALL FMDIVI(MXY(2),B,MXY(3))

      CALL FMEQU(MXY(3),MA,NDIG,NDSAVE)
      MXEXP = MXSAVE
      NDIG = NDSAVE
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMLNI4

      RECURSIVE SUBROUTINE FMLNI4_PQBT(A,B,C,D,MP,MQ,MB,MT,LEVEL_OF_RECURSION)

!  This routine does the binary splitting for computing the constant Ln( (B+A) / (B-A) ).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MP,MQ,MB,MT
      INTEGER :: A,B,C,D
      INTENT (IN) :: A,B,C,D
      INTENT (INOUT) :: MP,MQ,MB,MT
      TYPE(MULTI) :: MXY(8)
      INTEGER :: J,KM,RESULT_SIZE,LEVEL_OF_RECURSION
      REAL (KIND(0.0D0)) :: DA,DB

      DA = A
      DB = B
      RESULT_SIZE = ( (DB - DA + 1)*LOG(DBLE(C)**2) ) / DLOGMB + 7
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MP%MP)) THEN
          ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MP%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MP%MP)
          ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      RESULT_SIZE = ( (DB - DA + 1)*LOG(DBLE(D)**2) ) / DLOGMB + 7
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MQ%MP)) THEN
          ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MQ%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MQ%MP)
          ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      J = RESULT_SIZE
      RESULT_SIZE = ( (DB+1.5D0)*LOG(DB+2) - DB + 1/(12*(DB+2)) -          &
                      ( (DA+0.5D0)*LOG(DA+1) - DA + 1/(12*(DA+1)) ) - 1 +  &
                      (DB-DA+1)*LOG(2.0D0) ) / DLOGMB + 7
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      RESULT_SIZE = RESULT_SIZE + J
      IF (.NOT. ALLOCATED(MT%MP)) THEN
          ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MT%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MT%MP)
          ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      LEVEL_OF_RECURSION = LEVEL_OF_RECURSION + 1

      IF (B-A < 6) THEN
          IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(1)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(1)%MP)
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(2)%MP)) THEN
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(2)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(2)%MP)
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(3)%MP)) THEN
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(3)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(3)%MP)
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(4)%MP)) THEN
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(4)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(4)%MP)
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(5)%MP)) THEN
              ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(5)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(5)%MP)
              ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          CALL IMI2M(C,MXY(2))
          CALL IMSQR(MXY(2),MXY(1))
          CALL IMI2M(B-A+1,MXY(2))
          CALL IMPWR(MXY(1),MXY(2),MP)

          CALL IMI2M(D,MXY(2))
          CALL IMSQR(MXY(2),MXY(1))
          CALL IMI2M(B-A+1,MXY(2))
          CALL IMPWR(MXY(1),MXY(2),MQ)

          CALL IMI2M(1,MB)
          DO J = A+1, B, 2
             CALL IMMPYI(MB,2*J-1,MXY(1))
             CALL IMMPYI(MXY(1),2*J+1,MB)
          ENDDO
          IF (MOD(B-A,2) == 0) THEN
              CALL IMMPYI(MB,2*B+1,MXY(1))
              CALL IMEQ(MXY(1),MB)
          ENDIF

          CALL IMI2M(D,MXY(1))
          CALL IMSQR(MXY(1),MXY(2))
          CALL IMI2M(B-A+1,MXY(1))
          CALL IMPWR(MXY(2),MXY(1),MXY(3))

          CALL IMI2M(1,MXY(4))
          CALL IMI2M(0,MT)

          DO J = A, B
             CALL IMDIVI(MB,2*J+1,MXY(1))
             CALL IMMPY(MXY(1),MXY(3),MXY(2))
             CALL IMMPY(MXY(2),MXY(4),MXY(1))
             CALL IMADD(MT,MXY(1),MXY(5))
             CALL IMEQ(MXY(5),MT)
             CALL IMDIVI(MXY(3),D,MXY(5))
             CALL IMDIVI(MXY(5),D,MXY(3))
             CALL IMMPYI(MXY(4),C,MXY(5))
             CALL IMMPYI(MXY(5),C,MXY(4))
          ENDDO
          GO TO 110
      ENDIF

      KM = A/2 + B/2 + MOD(A,2)*MOD(B,2)
      CALL FMLNI4_PQBT(A,KM-1,C,D,MXY(1),MXY(2),MXY(3),MXY(4),LEVEL_OF_RECURSION)
      CALL FMLNI4_PQBT(KM,B,C,D,MXY(5),MXY(6),MXY(7),MXY(8),LEVEL_OF_RECURSION)

!             MP is not needed in FMLNI4, so this multiplication can be skipped at the top level
!             of the recursion.

      IF (LEVEL_OF_RECURSION > 1) THEN
          CALL IMMPY(MXY(1),MXY(5),MP)
      ELSE
          CALL IMI2M(0,MP)
      ENDIF
      CALL IMMPY(MXY(2),MXY(6),MQ)
      CALL IMMPY(MXY(3),MXY(7),MB)
      CALL IMMPY(MXY(6),MXY(7),MT)
      CALL IMMPY(MXY(4),MT,MXY(5))
      CALL IMMPY(MXY(1),MXY(3),MT)
      CALL IMMPY(MXY(8),MT,MXY(2))
      CALL IMADD(MXY(5),MXY(2),MT)

  110 LEVEL_OF_RECURSION = LEVEL_OF_RECURSION - 1
      RETURN
      END SUBROUTINE FMLNI4_PQBT

      SUBROUTINE FMM2DP(MA,X)

!  X = MA

!  Convert an FM number to double precision.

!  If KFLAG = -4 is returned for a value of MA that is in the range of the machine's double
!  precision number system, change the definition of DPMAX in routine FMSET to reflect the
!  current machine's range.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      DOUBLE PRECISION :: X

      INTEGER :: KRESLT
      INTENT (IN) :: MA
      INTENT (INOUT) :: X

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMM2DP'
      KRESLT = 0
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMARGS('FMM2DP   ',1,MA,MA,KRESLT)
      ENDIF
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
      IF (KRESLT /= 0) THEN

!             Here no valid result can be returned.
!             Set X to some value that the user is likely to recognize as wrong.

          X = DBLE(RUNKNO)
          KFLAG = -4
          IF (MA%MP(2) /= MUNKNO) CALL FMWARN
          IF (NTRACE /= 0) CALL FMNTRR(1,X,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

      CALL FMMD(MA,X)

      IF (NTRACE /= 0) CALL FMNTRR(1,X,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMM2DP

      SUBROUTINE FMM2I(MA,IVAL)

!  IVAL = MA

!  Convert an FM number to integer.

!  KFLAG =  0 is returned if the conversion is exact.
!        = -4 is returned if MA is larger than INTMAX in magnitude.  IVAL = IUNKNO is returned
!             as an indication that IVAL could not be computed without integer overflow.
!        =  2 is returned if MA is smaller than INTMAX in magnitude but MA is not an integer.
!             The next integer toward zero is returned in IVAL.
!  It is sometimes convenient to call FMM2I to see if an FM number can be represented as a one-word
!  integer, by checking KFLAG upon return.  To avoid an unwanted error message being printed in the
!  KFLAG=-4 case, set KWARN=0 before the call to FMM2I and reset it after the call.

!  This routine performs the trace printing for the conversion.  FMMI is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      INTENT (IN) :: MA
      INTENT (INOUT) :: IVAL

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMM2I'
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)

      CALL FMMI(MA,IVAL)

      IF (NTRACE /= 0) CALL FMNTRI(1,IVAL,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMM2I

      SUBROUTINE FMM2SP(MA,X)

!  X = MA

!  Convert an FM number to single precision.

!  MA is converted and the result is returned in X.

!  If KFLAG = -4 is returned for a value of MA that is in the range of the machine's single
!  precision number system, change the definition of SPMAX in routine FMSET to reflect the
!  current machine's range.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      REAL :: X

      DOUBLE PRECISION :: Y
      INTEGER :: KRESLT
      INTENT (IN) :: MA
      INTENT (INOUT) :: X

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMM2SP'
      KRESLT = 0
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMARGS('FMM2SP   ',1,MA,MA,KRESLT)
      ENDIF
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
      IF (KRESLT /= 0) THEN

!             Here no valid result can be returned.
!             Set X to some value that the user is likely to recognize as wrong.

          X = RUNKNO
          KFLAG = -4
          IF (MA%MP(2) /= MUNKNO) CALL FMWARN
          Y = DBLE(X)
          IF (NTRACE /= 0) CALL FMNTRR(1,Y,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

      CALL FMMD(MA,Y)
      X = REAL(Y)

      IF (NTRACE /= 0) THEN
          Y = DBLE(X)
          CALL FMNTRR(1,Y,1)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMM2SP

      SUBROUTINE FMMAX(MA,MB,MC)

!  MC = MAX(MA,MB)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      INTEGER :: KWRNSV
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMMAX'
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)

      KWRNSV = KWARN
      KWARN = 0
      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL FMST2M('UNKNOWN',MC)
          KFLAG = -4
      ELSE IF (FMCOMP(MA,'<',MB)) THEN
          CALL FMEQ(MB,MC)
      ELSE
          CALL FMEQ(MA,MC)
      ENDIF

      KWARN = KWRNSV
      IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMMAX

      SUBROUTINE FMMD(MA,X)

!  X = MA

!  Internal routine for conversion to double precision.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      DOUBLE PRECISION :: X

      DOUBLE PRECISION :: AQ(2),XQ(2),YQ(2),Y1(2),Y2(2),XBASE,PMAX,DLOGDP,  &
                          A1,A2,C,C1,C2,C21,C22,Q1,Q2,T,Z1,Z2
      REAL (KIND(1.0D0)) :: MA1,MAS
      INTEGER :: J,K,KWRNSV,NCASE
      INTENT (IN) :: MA
      INTENT (INOUT) :: X

!             Check to see if MA is in range for single or double precision.

      IF (MBLOGS /= MBASE) CALL FMCONS
      PMAX = DPMAX
      IF (NCALL > 0) THEN
          IF (NAMEST(NCALL) == 'FMM2SP') PMAX = DBLE(SPMAX)
      ENDIF
      DLOGDP = LOG(PMAX)
      MA1 = MA%MP(2)
      NCASE = 0
      X = DBLE(RUNKNO)
      IF (DBLE(MA%MP(2)-1)*DLOGMB > DLOGDP) THEN
          KFLAG = -4
          X = DBLE(RUNKNO)
          CALL FMWARN
          RETURN
      ELSE IF (DBLE(MA%MP(2)+1)*DLOGMB > DLOGDP) THEN
          MA1 = MA1 - 2
          NCASE = 1
      ELSE IF (DBLE(MA%MP(2)+1)*DLOGMB < -DLOGDP) THEN
          KFLAG = -10
          X = 0.0D0
          CALL FMWARN
          RETURN
      ELSE IF (DBLE(MA%MP(2)-1)*DLOGMB < -DLOGDP) THEN
          MA1 = MA1 + 2
          NCASE = 2
      ENDIF

!             Try FMMI first so that small integers will be converted quickly.

      KWRNSV = KWARN
      KWARN = 0
      CALL FMMI(MA,J)
      KWARN = KWRNSV
      IF (KFLAG == 0) THEN
          X = J
          RETURN
      ENDIF
      KFLAG = 0

!             General case.
!             In order to get the correctly rounded X, the arithmetic for computing X is done
!             with simulated quadruple-precision using the arrays of length 2.

      MAS = MA%MP(1)
      XBASE = MBASE
      XQ = (/ 0.0D0 , 0.0D0 /)
      YQ = (/ 1.0D0 , 0.0D0 /)
      C = RADIX(X)**(DIGITS(X) - DIGITS(X)/2) + 1
      K = (LOG(DBLE(RADIX(X)))/DLOGMB)*DIGITS(X) + NGRD52
      DO J = 2, MIN(K+1,NDIG+1)
         Z1 = YQ(1) / XBASE
         T = XBASE*C
         A1 = (XBASE - T) + T
         A2 = XBASE - A1
         T = Z1*C
         C1 = (Z1 - T) + T
         C2 = Z1 - C1
         T = C2*C
         C21 = (C2 - T) + T
         C22 = C2 - C21
         Q1 = XBASE*Z1
         Q2 = ((((A1*C1 - Q1) + A1*C2) + C1*A2) + C21*A2) + C22*A2
         Z2 = ((((YQ(1)-Q1) - Q2) + YQ(2))) / XBASE
         YQ(1) = Z1 + Z2
         YQ(2) = (Z1-YQ(1)) + Z2
         T = YQ(1)*C
         A1 = (YQ(1) - T) + T
         A2 = YQ(1) - A1
         T = DBLE(MA%MP(J+1))*C
         C1 = (DBLE(MA%MP(J+1)) - T) + T
         C2 = DBLE(MA%MP(J+1)) - C1
         T = C2*C
         C21 = (C2 - T) + T
         C22 = C2 - C21
         Q1 = YQ(1)*DBLE(MA%MP(J+1))
         Q2 = ((((A1*C1 - Q1) + A1*C2) + C1*A2) + C21*A2) + C22*A2
         Z2 = YQ(2)*DBLE(MA%MP(J+1)) + Q2
         AQ(1) = Q1 + Z2
         AQ(2) = (Q1-AQ(1)) + Z2
         Z1 = XQ(1) + AQ(1)
         Q1 = XQ(1) - Z1
         Z2 = (((Q1+AQ(1)) + (XQ(1)-(Q1+Z1))) + XQ(2)) + AQ(2)
         XQ(1) = Z1 + Z2
         XQ(2) = (Z1-XQ(1)) + Z2
      ENDDO

      Y1 = (/ XBASE , 0.0D0 /)
      K = ABS(MA1)
      IF (MOD(K,2) == 0) THEN
          Y2 = (/ 1.0D0 , 0.0D0 /)
      ELSE
          Y2 = (/ XBASE , 0.0D0 /)
      ENDIF

  110 K = K/2
      T = Y1(1)*C
      A1 = (Y1(1) - T) + T
      A2 = Y1(1) - A1
      T = Y1(1)*C
      C1 = (Y1(1) - T) + T
      C2 = Y1(1) - C1
      T = C2*C
      C21 = (C2 - T) + T
      C22 = C2 - C21
      Q1 = Y1(1)*Y1(1)
      Q2 = ((((A1*C1 - Q1) + A1*C2) + C1*A2) + C21*A2) + C22*A2
      Z2 = ((Y1(1) + Y1(2))*Y1(2) + Y1(2)*Y1(1)) + Q2
      Y1(1) = Q1 + Z2
      Y1(2) = (Q1-Y1(1)) + Z2
      IF (MOD(K,2) == 1) THEN
          T = Y1(1)*C
          A1 = (Y1(1) - T) + T
          A2 = Y1(1) - A1
          T = Y2(1)*C
          C1 = (Y2(1) - T) + T
          C2 = Y2(1) - C1
          T = C2*C
          C21 = (C2 - T) + T
          C22 = C2 - C21
          Q1 = Y1(1)*Y2(1)
          Q2 = ((((A1*C1 - Q1) + A1*C2) + C1*A2) + C21*A2) + C22*A2
          Z2 = ((Y1(1) + Y1(2))*Y2(2) + Y1(2)*Y2(1)) + Q2
          Y2(1) = Q1 + Z2
          Y2(2) = (Q1-Y2(1)) + Z2
      ENDIF
      IF (K > 1) GO TO 110

      IF (MA1 < 0) THEN
          Z1 = XQ(1) / Y2(1)
          T = Y2(1)*C
          A1 = (Y2(1) - T) + T
          A2 = Y2(1) - A1
          T = Z1*C
          C1 = (Z1 - T) + T
          C2 = Z1 - C1
          T = C2*C
          C21 = (C2 - T) + T
          C22 = C2 - C21
          Q1 = Y2(1)*Z1
          Q2 = ((((A1*C1 - Q1) + A1*C2) + C1*A2) + C21*A2) + C22*A2
          Z2 = ((((XQ(1)-Q1) - Q2) + XQ(2)) - Z1*Y2(2)) / (Y2(1) + Y2(2))
          AQ(1) = Z1 + Z2
          AQ(2) = (Z1-AQ(1)) + Z2
      ELSE
          T = XQ(1)*C
          A1 = (XQ(1) - T) + T
          A2 = XQ(1) - A1
          T = Y2(1)*C
          C1 = (Y2(1) - T) + T
          C2 = Y2(1) - C1
          T = C2*C
          C21 = (C2 - T) + T
          C22 = C2 - C21
          Q1 = XQ(1)*Y2(1)
          Q2 = ((((A1*C1 - Q1) + A1*C2) + C1*A2) + C21*A2) + C22*A2
          Z2 = ((XQ(1) + XQ(2))*Y2(2) + XQ(2)*Y2(1)) + Q2
          AQ(1) = Q1 + Z2
          AQ(2) = (Q1-AQ(1)) + Z2
      ENDIF

      X = AQ(1) + AQ(2)

      IF (MAS < 0) X = -X

!             Check the result if it is near overflow or underflow.

      IF (NCASE == 1) THEN
          IF (X <= PMAX/(XBASE*XBASE)) THEN
              X = X*XBASE*XBASE
          ELSE
              KFLAG = -4
              X = DBLE(RUNKNO)
              CALL FMWARN
          ENDIF
      ELSE IF (NCASE == 2) THEN
          IF (X >= (1.0D0/PMAX)*XBASE*XBASE) THEN
              X = X/(XBASE*XBASE)
          ELSE
              KFLAG = -10
              X = 0.0D0
              CALL FMWARN
          ENDIF
      ENDIF
      RETURN
      END SUBROUTINE FMMD

      SUBROUTINE FMMI(MA,IVAL)

!  IVAL = MA.  Internal FM to integer conversion routine.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL

      INTEGER :: J,KA,KB,LARGE,N1
      INTENT (IN) :: MA
      INTENT (INOUT) :: IVAL

      KFLAG = 0
      N1 = NDIG + 1
      LARGE = INT(INTMAX/MBASE)
      IVAL = 0
      IF (MA%MP(2) <= 0) THEN
          IF (MA%MP(3) /= 0) KFLAG = 2
          RETURN
      ENDIF

      KB = INT(MA%MP(2)) + 1
      IVAL = INT(ABS(MA%MP(3)))
      IF (KB >= 3) THEN
          DO J = 3, KB
             IF (IVAL > LARGE) THEN
                 KFLAG = -4
                 IF (MA%MP(2) /= MUNKNO) CALL FMWARN
                 IVAL = IUNKNO
                 RETURN
             ENDIF
             IF (J <= N1) THEN
                 IVAL = IVAL*INT(MBASE)
                 IF (IVAL > INTMAX-MA%MP(J+1)) THEN
                     KFLAG = -4
                     IF (MA%MP(2) /= MUNKNO) CALL FMWARN
                     IVAL = IUNKNO
                     RETURN
                 ELSE
                     IVAL = IVAL + INT(MA%MP(J+1))
                 ENDIF
             ELSE
                 IVAL = IVAL*INT(MBASE)
             ENDIF
          ENDDO
      ENDIF

      IF (MA%MP(1) < 0) IVAL = -IVAL

!             Check to see if MA is an integer.

      KA = KB + 1
      IF (KA <= N1) THEN
          DO J = KA, N1
             IF (MA%MP(J+1) /= 0) THEN
                 KFLAG = 2
                 RETURN
             ENDIF
          ENDDO
      ENDIF

      RETURN
      END SUBROUTINE FMMI

      SUBROUTINE FMMIN(MA,MB,MC)

!  MC = MIN(MA,MB)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      INTEGER :: KWRNSV
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMMIN'
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)

      KWRNSV = KWARN
      KWARN = 0
      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL FMST2M('UNKNOWN',MC)
          KFLAG = -4
      ELSE IF (FMCOMP(MA,'>',MB)) THEN
          CALL FMEQ(MB,MC)
      ELSE
          CALL FMEQ(MA,MC)
      ENDIF

      KWARN = KWRNSV
      IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMMIN

      SUBROUTINE FMMOD(MA,MB,MC)

!  MC = MA(MOD MB).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MVB,MVC,MVY,MVZ,MXSAVE
      INTEGER :: J,K,KB,KE,KN,KOVUN,KRESLT,KWRNSV,NDSAVE,NTRSAV
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(4)

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB) THEN
          CALL FMENTR('FMMOD    ',MA,MB,2,1,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMMOD'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          IF (MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KWRNSV = KWARN
      KWARN = 0

      IF (MB%MP(2) > MA%MP(2) .AND. MB%MP(3) /= 0) THEN
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      ELSE

!             Special cases when MB is a small integer.

          CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
          CALL FMEQU(MB,MXY(3),NDSAVE,NDIG)
          MXY(2)%MP(1) = 1
          MXY(3)%MP(1) = 1

          CALL FMM2I(MXY(3),KB)
          IF (KFLAG == 0 .AND. KB < MXBASE) THEN
              IF (KB == 1 .OR. KB == -1) THEN
                  IF (MXY(2)%MP(2) >= NDIG) THEN
                      CALL FMI2M(0,MXY(1))
                      GO TO 130
                  ELSE
                      CALL FMINT(MXY(2),MXY(3))
                      CALL FMSUB(MXY(2),MXY(3),MXY(1))
                      IF (MA%MP(1) < 0 .AND. MXY(1)%MP(2) /= MUNKNO .AND.  &
                          MXY(1)%MP(3) /= 0) MXY(1)%MP(1) = -MXY(1)%MP(1)
                      GO TO 130
                  ENDIF
              ELSE IF (MXY(2)%MP(2) == MEXPOV .OR. KB == 0) THEN
                  KFLAG = -4
                  KWARN = KWRNSV
                  MXEXP = MXSAVE
                  CALL FMWARN
                  NDIG = NDSAVE
                  CALL FMST2M('UNKNOWN',MC)
                  IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
                  NCALL = NCALL - 1
                  RETURN
              ELSE IF (MXY(2)%MP(2) > NDIG.AND.MOD(INT(MBASE),KB) == 0) THEN
                  CALL FMI2M(0,MXY(1))
                  GO TO 130
              ENDIF
              IF (MXY(2)%MP(2) < NDIG) THEN
                  DO J = INT(MXY(2)%MP(2))+1, NDIG+1
                     IF (MXY(2)%MP(J+1) /= 0) GO TO 120
                  ENDDO
              ENDIF
              KE = MIN(INT(MXY(2)%MP(2)),NDIG)
              MVB = KB
              MVC = MOD(MXY(2)%MP(3),MVB)
              DO J = 3, KE+1
                 MVC = MOD(MVC*MBASE+MXY(2)%MP(J+1),MVB)
              ENDDO
              IF (MVC == 0) THEN
                   CALL FMI2M(0,MXY(1))
                   GO TO 130
              ENDIF
              KN = INT(MXY(2)%MP(2)) - KE
              MVY = MOD(MBASE,MVB)
              MVZ = 1
              IF (MOD(KN,2) == 1) MVZ = MVY

              IF (MVY /= 1) THEN
  110             KN = KN/2
                  MVY = MOD(MVY*MVY,MVB)
                  IF (MOD(KN,2) == 1) MVZ = MOD(MVZ*MVY,MVB)
                  IF (KN > 1) GO TO 110
              ENDIF
              MVZ = MOD(MVZ*MVC,MVB)
              KE = INT(MVZ)
              CALL FMI2M(KE,MXY(1))
              IF (MA%MP(1) < 0 .AND. MXY(1)%MP(2) /= MUNKNO .AND.  &
                  MXY(1)%MP(3) /= 0) MXY(1)%MP(1) = -MXY(1)%MP(1)
              GO TO 130
          ENDIF
          IF (MA%MP(2)-MB%MP(2) > 3*10**5) THEN
              KFLAG = -4
              CALL FMWARN
              CALL FMST2M('UNKNOWN',MXY(1))
              GO TO 130
          ENDIF

!             General case.

  120     IF (MA%MP(3) /= 0) THEN
              NDIG = NDIG + INT(MA%MP(2)-MB%MP(2))
          ENDIF
          IF (MB%MP(3) == 0) THEN
              KFLAG = -9
              IF (MA%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN .OR.   &
                  MB%MP(3) == 0) KFLAG = -4
              KWARN = KWRNSV
              MXEXP = MXSAVE
              CALL FMWARN
              NDIG = NDSAVE
              CALL FMST2M('UNKNOWN',MC)
              IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
              NCALL = NCALL - 1
              RETURN
          ENDIF

          CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
          CALL FMEQU(MB,MXY(3),NDSAVE,NDIG)

          MXY(2)%MP(1) = 1
          MXY(3)%MP(1) = 1
          CALL FMDIV(MXY(2),MXY(3),MXY(1))
          CALL FMINT(MXY(1),MXY(4))
          CALL FMMPY_R1(MXY(4),MXY(3))
          CALL FMSUB(MXY(2),MXY(4),MXY(1))

!             Due to rounding, MXY(1) may not be between 0 and MB here.

          NTRSAV = NTRACE
          NTRACE = 0
          IF (FMCOMP(MXY(1),'>=',MXY(3))) THEN
              NTRACE = NTRSAV
              CALL FMSUB_R1(MXY(1),MXY(3))
          ENDIF
          NTRACE = NTRSAV
          IF (MXY(1)%MP(1) < 0) CALL FMADD_R1(MXY(1),MXY(3))
          IF (MA%MP(1) < 0 .AND. MXY(1)%MP(2) /= MUNKNO .AND.  &
              MXY(1)%MP(3) /=0) MXY(1)%MP(1) = -MXY(1)%MP(1)
      ENDIF

  130 IF (KFLAG == 1) KFLAG = 0
      KWARN = KWRNSV
      CALL FMEXIT(MXY(1),MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMMOD

      SUBROUTINE FMMOVE(MW,MA)

!  Move a result from a work area (MW) to MA.

!  If the result has MW%MP(3)=0, then it is shifted and the exponent adjusted when it is
!  moved to MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MW

      INTEGER :: J,N1,N2
      INTENT (IN) :: MW
      INTENT (INOUT) :: MA

      IF (MW%MP(3) /= 0) THEN
          N1 = NDIG + 1

!             Major (Inner Loop)

          DO J = 2, N1+1
             MA%MP(J) = MW%MP(J)
          ENDDO
      ELSE
          N2 = NDIG + 2
          DO J = 3, N2
             MA%MP(J) = MW%MP(J+1)
          ENDDO
          IF (MA%MP(3) /= 0) THEN
              MA%MP(2) = MW%MP(2) - 1
          ELSE
              MA%MP(2) = 0
          ENDIF
      ENDIF

      MA%MP(1) = 1
      IF (ABS(MA%MP(2)) > MXEXP) CALL FMTRAP(MA)

      RETURN
      END SUBROUTINE FMMOVE

      SUBROUTINE FMMPY(MA,MB,MC)

!  MC = MA * MB

!  When one of the numbers MA, MB is known to have more zero digits (base MBASE) than the other,
!  it is faster if MB is the one with more zero digits.

!  This routine performs the trace printing for multiplication.
!  FMMPY2 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMMPY'
          CALL FMNTR(2,MA,MB,2,1)

          CALL FMMPY2(MA,MB,MC)

          CALL FMNTR(1,MC,MC,1,1)
      ELSE
          CALL FMMPY2(MA,MB,MC)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMMPY

      SUBROUTINE FMMPY2(MA,MB,MC)

!  Internal multiplication routine.  MC = MA * MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      REAL (KIND(1.0D0)) :: MAS,MBS,MR,MS,MT1,MT2
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JRSSAV,K,KL,KR_RETRY,KRESLT,KT,KT1,KT2,KSHIFT,L,N1,NGUARD
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      KR_RETRY = 0
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR. KDEBUG == 1) THEN
          CALL FMARGS('FMMPY    ',2,MA,MB,KRESLT)
          IF (KRESLT /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMMPY'
              CALL FMRSLT(MA,MB,MC,KRESLT)
              JRSIGN = JRSSAV
              NCALL = NCALL - 1
              RETURN
          ENDIF
      ELSE IF (MA%MP(3) == 0 .OR. MB%MP(3) == 0) THEN
          CALL FMIM(0,MC)
          JRSIGN = JRSSAV
          RETURN
      ENDIF
      KFLAG = 0

!             Save the sign of MA and MB and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD22
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 2
          ELSE IF (MBASE < 10**6) THEN
              NGUARD = MIN(NGUARD+1,NDIG+2)
          ENDIF
      ENDIF
      IF (MA%MP(3)*MB%MP(3) < MBASE .AND. NGUARD < 3) NGUARD = 3

      N1 = NDIG + 1

      IF (MBASE*MBASE <= MXBASE/(4*MBASE)) THEN

!             If a small base is being used (for example, using base 2 to check some machine
!             precision results), it is faster to switch to a larger base (like 2^24) while
!             doing the operation.

          IF (NDIGL /= NDIG .OR. MBASEL /= MBASE .OR. NGUARL /= NGUARD) THEN
              MBASEL = MBASE
              NDIGL = NDIG
              NGUARL = NGUARD
              DO J = 2, 1000
                 MR = MBASE*MBASEL
                 IF (4*MR > MXBASE) THEN
                     N21 = J - 1
                     NDIG = (NDIGL-1)/N21 + 1
                     IF (NDIG < 2) NDIG = 2
                     NGRDN = (NDIGL+NGUARD-1)/N21 + 2 - NDIG
                     IF (NGRDN < 1) NGRDN = 1
                     EXIT
                 ENDIF
                 MBASE = MR
              ENDDO
              MBASEN = MBASE
              NDIGN = NDIG
          ELSE
              MBASE = MBASEN
              NDIG = NDIGN
          ENDIF
          IF (MBLOGS /= MBASE) CALL FMCONS
          J = NDIG
          NDIG = NDIG + NGRDN + 5
          IF (.NOT. ALLOCATED(MPMA%MP)) THEN
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMA%MP) < NDIG+2) THEN
              DEALLOCATE(MPMA%MP)
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MPMB%MP)) THEN
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMB%MP) < NDIG+2) THEN
              DEALLOCATE(MPMB%MP)
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          NDIG = J
          MPMA%MP(1) = MA%MP(1)
          MPMB%MP(1) = MB%MP(1)
          MPMA%MP(2) = 0
          MPMB%MP(2) = 0
          L = 2 - N21
          DO J = 2, NDIGL+2-N21, N21
             MT1 = MA%MP(J+1)
             MT2 = MB%MP(J+1)
             DO K = J+1, J+N21-1
                MT1 = MT1*MBASEL + MA%MP(K+1)
                MT2 = MT2*MBASEL + MB%MP(K+1)
             ENDDO
             MPMA%MP(3+J/N21) = MT1
             MPMB%MP(3+J/N21) = MT2
             L = J
          ENDDO
          DO J = 3+L/N21, NDIG+NGRDN+1
             MPMA%MP(J+1) = 0
             MPMB%MP(J+1) = 0
          ENDDO
          IF (L+N21 <= NDIGL+1) THEN
              MT1 = 0
              MT2 = 0
              DO J = L+N21, L+2*N21-1
                 IF (J <= NDIGL+1) THEN
                     MT1 = MT1*MBASEL + MA%MP(J+1)
                     MT2 = MT2*MBASEL + MB%MP(J+1)
                 ELSE
                     MT1 = MT1*MBASEL
                     MT2 = MT2*MBASEL
                 ENDIF
              ENDDO
              MPMA%MP(3+(L+N21)/N21) = MT1
              MPMB%MP(3+(L+N21)/N21) = MT2
          ENDIF
          CALL FMMPY3(MPMA,MPMB,NGRDN,KSHIFT)
          IF (MWA%MP(3) == 0) THEN
              DO J = 3, 1+NDIG+NGRDN
                 MWA%MP(J) = MWA%MP(J+1)
              ENDDO
              MWA%MP(NDIG+NGRDN+2) = 0
              KSHIFT = 0
          ENDIF
          IF (MBASEL == 2 .AND. MBASE < INTMAX) THEN
              DO J = 1+NDIG+NGRDN, 2, -1
                 KT1 = MWA%MP(J+1)
                 KT = 2 + (J-2)*N21
                 KT2 = N21 + KT - 1
                 DO K = KT, MIN(1+(J-1)*N21,NDIGL+NGUARD+2)
                    MWA%MP(K+1) = IBITS(KT1,KT2-K,1)
                 ENDDO
              ENDDO
          ELSE
              MS = MBASEL**(N21-1)
              DO J = 1+NDIG+NGRDN, 2, -1
                 MR = MS
                 MT1 = MWA%MP(J+1)
                 DO K = 2+(J-2)*N21, MIN(1+(J-1)*N21,NDIGL+NGUARD+2)
                    MWA%MP(K+1) = AINT (MT1/MR)
                    MT1 = MT1 - MWA%MP(K+1)*MR
                    MR = AINT (MR/MBASEL)
                 ENDDO
              ENDDO
          ENDIF
          KSHIFT = 0
          IF (MWA%MP(3) == 0) KSHIFT = 1
          MWA%MP(2) = MA%MP(2) + MB%MP(2)
          NDIG = NDIGL
          MBASE = MBASEL
          IF (MBLOGS /= MBASE) CALL FMCONS
      ELSE

!             This is the normal case, where the base is not small.

          CALL FMMPY3(MA,MB,NGUARD,KSHIFT)
      ENDIF

!             The multiplication is complete.
!             Round the result, move it to MC, and append the correct sign.

      IF ((MAS > 0 .AND. MBS > 0) .OR. (MAS < 0 .AND. MBS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NGUARD < NDIG+2) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      MR = 2*MWA%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWA%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(KSHIFT+N1+1) = MWA%MP(KSHIFT+N1+1) + 1
                  MWA%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MC)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMMPY'
          CALL FMWARN
      ENDIF

      MC%MP(1) = 1
      IF (MAS*MBS < 0 .AND. MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0)  &
          MC%MP(1) = -1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMMPY2

      SUBROUTINE FMMPY_R1(MA,MB)

!  MA = MA * MB

!  When one of the numbers MA, MB is known to have more zero digits (base MBASE) than the other,
!  it is faster if MB is the one with more zero digits.

!  This routine performs the trace printing for multiplication.
!  FMMPY2_R1 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMMPY_R1'
          CALL FMNTR(2,MA,MB,2,1)

          CALL FMMPY2_R1(MA,MB)

          NAMEST(NCALL) = 'FMMPY_R1'
          CALL FMNTR(1,MA,MA,1,1)
      ELSE
          CALL FMMPY2_R1(MA,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMMPY_R1

      SUBROUTINE FMMPY2_R1(MA,MB)

!  Internal multiplication routine.  MA = MA * MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAS,MBS,MR,MS,MT1,MT2
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JRSSAV,K,KL,KR_RETRY,KRESLT,KT,KT1,KT2,KSHIFT,L,N1,NGUARD
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      KR_RETRY = 0
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR. KDEBUG == 1) THEN
          CALL FMARGS('FMMPY    ',2,MA,MB,KRESLT)
          IF (KRESLT /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMMPY_R1'
              CALL FMRSLT(MA,MB,MXY(1),KRESLT)
              CALL FMEQ(MXY(1),MA)
              JRSIGN = JRSSAV
              NCALL = NCALL - 1
              RETURN
          ENDIF
      ELSE IF (MA%MP(3) == 0 .OR. MB%MP(3) == 0) THEN
          CALL FMIM(0,MA)
          JRSIGN = JRSSAV
          RETURN
      ENDIF
      KFLAG = 0

!             Save the sign of MA and MB and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD22
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 2
          ELSE IF (MBASE < 10**6) THEN
              NGUARD = MIN(NGUARD+1,NDIG+2)
          ENDIF
      ENDIF
      IF (MA%MP(3)*MB%MP(3) < MBASE .AND. NGUARD < 3) NGUARD = 3

      N1 = NDIG + 1

      IF (MBASE*MBASE <= MXBASE/(4*MBASE)) THEN

!             If a small base is being used (for example, using base 2 to check some machine
!             precision results), it is faster to switch to a larger base (like 2^24) while
!             doing the operation.

          IF (NDIGL /= NDIG .OR. MBASEL /= MBASE .OR. NGUARL /= NGUARD) THEN
              MBASEL = MBASE
              NDIGL = NDIG
              NGUARL = NGUARD
              DO J = 2, 1000
                 MR = MBASE*MBASEL
                 IF (4*MR > MXBASE) THEN
                     N21 = J - 1
                     NDIG = (NDIGL-1)/N21 + 1
                     IF (NDIG < 2) NDIG = 2
                     NGRDN = (NDIGL+NGUARD-1)/N21 + 2 - NDIG
                     IF (NGRDN < 1) NGRDN = 1
                     EXIT
                 ENDIF
                 MBASE = MR
              ENDDO
              MBASEN = MBASE
              NDIGN = NDIG
          ELSE
              MBASE = MBASEN
              NDIG = NDIGN
          ENDIF
          IF (MBLOGS /= MBASE) CALL FMCONS
          J = NDIG
          NDIG = NDIG + NGRDN + 5
          IF (.NOT. ALLOCATED(MPMA%MP)) THEN
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMA%MP) < NDIG+2) THEN
              DEALLOCATE(MPMA%MP)
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MPMB%MP)) THEN
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMB%MP) < NDIG+2) THEN
              DEALLOCATE(MPMB%MP)
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          NDIG = J
          MPMA%MP(1) = MA%MP(1)
          MPMB%MP(1) = MB%MP(1)
          MPMA%MP(2) = 0
          MPMB%MP(2) = 0
          L = 2 - N21
          DO J = 2, NDIGL+2-N21, N21
             MT1 = MA%MP(J+1)
             MT2 = MB%MP(J+1)
             DO K = J+1, J+N21-1
                MT1 = MT1*MBASEL + MA%MP(K+1)
                MT2 = MT2*MBASEL + MB%MP(K+1)
             ENDDO
             MPMA%MP(3+J/N21) = MT1
             MPMB%MP(3+J/N21) = MT2
             L = J
          ENDDO
          DO J = 3+L/N21, NDIG+NGRDN+1
             MPMA%MP(J+1) = 0
             MPMB%MP(J+1) = 0
          ENDDO
          IF (L+N21 <= NDIGL+1) THEN
              MT1 = 0
              MT2 = 0
              DO J = L+N21, L+2*N21-1
                 IF (J <= NDIGL+1) THEN
                     MT1 = MT1*MBASEL + MA%MP(J+1)
                     MT2 = MT2*MBASEL + MB%MP(J+1)
                 ELSE
                     MT1 = MT1*MBASEL
                     MT2 = MT2*MBASEL
                 ENDIF
              ENDDO
              MPMA%MP(3+(L+N21)/N21) = MT1
              MPMB%MP(3+(L+N21)/N21) = MT2
          ENDIF
          CALL FMMPY3(MPMA,MPMB,NGRDN,KSHIFT)
          IF (MWA%MP(3) == 0) THEN
              DO J = 3, 1+NDIG+NGRDN
                 MWA%MP(J) = MWA%MP(J+1)
              ENDDO
              MWA%MP(NDIG+NGRDN+2) = 0
              KSHIFT = 0
          ENDIF
          IF (MBASEL == 2 .AND. MBASE < INTMAX) THEN
              DO J = 1+NDIG+NGRDN, 2, -1
                 KT1 = MWA%MP(J+1)
                 KT = 2 + (J-2)*N21
                 KT2 = N21 + KT - 1
                 DO K = KT, MIN(1+(J-1)*N21,NDIGL+NGUARD+2)
                    MWA%MP(K+1) = IBITS(KT1,KT2-K,1)
                 ENDDO
              ENDDO
          ELSE
              MS = MBASEL**(N21-1)
              DO J = 1+NDIG+NGRDN, 2, -1
                 MR = MS
                 MT1 = MWA%MP(J+1)
                 DO K = 2+(J-2)*N21, MIN(1+(J-1)*N21,NDIGL+NGUARD+2)
                    MWA%MP(K+1) = AINT (MT1/MR)
                    MT1 = MT1 - MWA%MP(K+1)*MR
                    MR = AINT (MR/MBASEL)
                 ENDDO
              ENDDO
          ENDIF
          KSHIFT = 0
          IF (MWA%MP(3) == 0) KSHIFT = 1
          MWA%MP(2) = MA%MP(2) + MB%MP(2)
          NDIG = NDIGL
          MBASE = MBASEL
          IF (MBLOGS /= MBASE) CALL FMCONS
      ELSE

!             This is the normal case, where the base is not small.

          CALL FMMPY3(MA,MB,NGUARD,KSHIFT)
      ENDIF

!             The multiplication is complete.
!             Round the result, move it to MA, and append the correct sign.

      IF ((MAS > 0 .AND. MBS > 0) .OR. (MAS < 0 .AND. MBS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NGUARD < NDIG+2) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      MR = 2*MWA%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWA%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(KSHIFT+N1+1) = MWA%MP(KSHIFT+N1+1) + 1
                  MWA%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MA)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMMPY_R1'
          CALL FMWARN
      ENDIF

      MA%MP(1) = 1
      IF (MAS*MBS < 0 .AND. MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0)  &
          MA%MP(1) = -1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMMPY2_R1

      SUBROUTINE FMMPY_R2(MA,MB)

!  MB = MA * MB

!  When one of the numbers MA, MB is known to have more zero digits (base MBASE) than the other,
!  it is faster if MB is the one with more zero digits.

!  This routine performs the trace printing for multiplication.
!  FMMPY2_R2 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMMPY_R2'
          CALL FMNTR(2,MA,MB,2,1)

          CALL FMMPY2_R2(MA,MB)

          NAMEST(NCALL) = 'FMMPY_R2'
          CALL FMNTR(1,MB,MB,1,1)
      ELSE
          CALL FMMPY2_R2(MA,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMMPY_R2

      SUBROUTINE FMMPY2_R2(MA,MB)

!  Internal multiplication routine.  MB = MA * MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAS,MBS,MR,MS,MT1,MT2
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JRSSAV,K,KL,KR_RETRY,KRESLT,KT,KT1,KT2,KSHIFT,L,N1,NGUARD
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      KR_RETRY = 0
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR. KDEBUG == 1) THEN
          CALL FMARGS('FMMPY    ',2,MA,MB,KRESLT)
          IF (KRESLT /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMMPY_R2'
              CALL FMRSLT(MA,MB,MXY(1),KRESLT)
              CALL FMEQ(MXY(1),MB)
              JRSIGN = JRSSAV
              NCALL = NCALL - 1
              RETURN
          ENDIF
      ELSE IF (MA%MP(3) == 0 .OR. MB%MP(3) == 0) THEN
          CALL FMIM(0,MB)
          JRSIGN = JRSSAV
          RETURN
      ENDIF
      KFLAG = 0

!             Save the sign of MA and MB and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD22
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 2
          ELSE IF (MBASE < 10**6) THEN
              NGUARD = MIN(NGUARD+1,NDIG+2)
          ENDIF
      ENDIF
      IF (MA%MP(3)*MB%MP(3) < MBASE .AND. NGUARD < 3) NGUARD = 3

      N1 = NDIG + 1

      IF (MBASE*MBASE <= MXBASE/(4*MBASE)) THEN

!             If a small base is being used (for example, using base 2 to check some machine
!             precision results), it is faster to switch to a larger base (like 2^24) while
!             doing the operation.

          IF (NDIGL /= NDIG .OR. MBASEL /= MBASE .OR. NGUARL /= NGUARD) THEN
              MBASEL = MBASE
              NDIGL = NDIG
              NGUARL = NGUARD
              DO J = 2, 1000
                 MR = MBASE*MBASEL
                 IF (4*MR > MXBASE) THEN
                     N21 = J - 1
                     NDIG = (NDIGL-1)/N21 + 1
                     IF (NDIG < 2) NDIG = 2
                     NGRDN = (NDIGL+NGUARD-1)/N21 + 2 - NDIG
                     IF (NGRDN < 1) NGRDN = 1
                     EXIT
                 ENDIF
                 MBASE = MR
              ENDDO
              MBASEN = MBASE
              NDIGN = NDIG
          ELSE
              MBASE = MBASEN
              NDIG = NDIGN
          ENDIF
          IF (MBLOGS /= MBASE) CALL FMCONS
          J = NDIG
          NDIG = NDIG + NGRDN + 5
          IF (.NOT. ALLOCATED(MPMA%MP)) THEN
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMA%MP) < NDIG+2) THEN
              DEALLOCATE(MPMA%MP)
              ALLOCATE(MPMA%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MPMB%MP)) THEN
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MPMB%MP) < NDIG+2) THEN
              DEALLOCATE(MPMB%MP)
              ALLOCATE(MPMB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          NDIG = J
          MPMA%MP(1) = MA%MP(1)
          MPMB%MP(1) = MB%MP(1)
          MPMA%MP(2) = 0
          MPMB%MP(2) = 0
          L = 2 - N21
          DO J = 2, NDIGL+2-N21, N21
             MT1 = MA%MP(J+1)
             MT2 = MB%MP(J+1)
             DO K = J+1, J+N21-1
                MT1 = MT1*MBASEL + MA%MP(K+1)
                MT2 = MT2*MBASEL + MB%MP(K+1)
             ENDDO
             MPMA%MP(3+J/N21) = MT1
             MPMB%MP(3+J/N21) = MT2
             L = J
          ENDDO
          DO J = 3+L/N21, NDIG+NGRDN+1
             MPMA%MP(J+1) = 0
             MPMB%MP(J+1) = 0
          ENDDO
          IF (L+N21 <= NDIGL+1) THEN
              MT1 = 0
              MT2 = 0
              DO J = L+N21, L+2*N21-1
                 IF (J <= NDIGL+1) THEN
                     MT1 = MT1*MBASEL + MA%MP(J+1)
                     MT2 = MT2*MBASEL + MB%MP(J+1)
                 ELSE
                     MT1 = MT1*MBASEL
                     MT2 = MT2*MBASEL
                 ENDIF
              ENDDO
              MPMA%MP(3+(L+N21)/N21) = MT1
              MPMB%MP(3+(L+N21)/N21) = MT2
          ENDIF
          CALL FMMPY3(MPMA,MPMB,NGRDN,KSHIFT)
          IF (MWA%MP(3) == 0) THEN
              DO J = 3, 1+NDIG+NGRDN
                 MWA%MP(J) = MWA%MP(J+1)
              ENDDO
              MWA%MP(NDIG+NGRDN+2) = 0
              KSHIFT = 0
          ENDIF
          IF (MBASEL == 2 .AND. MBASE < INTMAX) THEN
              DO J = 1+NDIG+NGRDN, 2, -1
                 KT1 = MWA%MP(J+1)
                 KT = 2 + (J-2)*N21
                 KT2 = N21 + KT - 1
                 DO K = KT, MIN(1+(J-1)*N21,NDIGL+NGUARD+2)
                    MWA%MP(K+1) = IBITS(KT1,KT2-K,1)
                 ENDDO
              ENDDO
          ELSE
              MS = MBASEL**(N21-1)
              DO J = 1+NDIG+NGRDN, 2, -1
                 MR = MS
                 MT1 = MWA%MP(J+1)
                 DO K = 2+(J-2)*N21, MIN(1+(J-1)*N21,NDIGL+NGUARD+2)
                    MWA%MP(K+1) = AINT (MT1/MR)
                    MT1 = MT1 - MWA%MP(K+1)*MR
                    MR = AINT (MR/MBASEL)
                 ENDDO
              ENDDO
          ENDIF
          KSHIFT = 0
          IF (MWA%MP(3) == 0) KSHIFT = 1
          MWA%MP(2) = MA%MP(2) + MB%MP(2)
          NDIG = NDIGL
          MBASE = MBASEL
          IF (MBLOGS /= MBASE) CALL FMCONS
      ELSE

!             This is the normal case, where the base is not small.

          CALL FMMPY3(MA,MB,NGUARD,KSHIFT)
      ENDIF

!             The multiplication is complete.
!             Round the result, move it to MB, and append the correct sign.

      IF ((MAS > 0 .AND. MBS > 0) .OR. (MAS < 0 .AND. MBS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NGUARD < NDIG+2) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      MR = 2*MWA%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWA%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(KSHIFT+N1+1) = MWA%MP(KSHIFT+N1+1) + 1
                  MWA%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MB)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMMPY_R2'
          CALL FMWARN
      ENDIF

      MB%MP(1) = 1
      IF (MAS*MBS < 0 .AND. MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0)  &
          MB%MP(1) = -1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMMPY2_R2

      SUBROUTINE FMMPY3(MA,MB,NGUARD,KSHIFT)

!  Internal multiplication of MA*MB.  The result is returned in MWA.  Both MA and MB are positive.

!  NGUARD is the number of guard digits that will be used.
!  KSHIFT = 1 is returned if a left shift is pending (i.e., MWA%MP(3)=0).  The shift will
!             be done in FMMOVE.  KSHIFT = 0 is returned if no shift is pending.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: NGUARD,KSHIFT

      REAL (KIND(1.0D0)) :: MAXMWA,MBJ,MBKJ,MBNORM,MBP1,MK,MKT,MMAX,MT
      INTEGER :: J,JM1,K,KB,KI,KJ,KL,KNZ,KWA,L,N1,NMETHD,NZDA,NZDB
      REAL :: C
      INTENT (IN) :: MA,MB,NGUARD
      INTENT (INOUT) :: KSHIFT

      N1 = NDIG + 1
      MWA%MP(2) = MA%MP(2) + MB%MP(2)

!             Check for using an FFT-based method if precision is very high.

      C = 900
      IF (NDIG >= C) THEN
          NZDA = 2
          NZDB = NDIG
          DO J = NDIG, 2, -1
             IF (MA%MP(J+2) /= 0) THEN
                 NZDA = J
                 EXIT
             ENDIF
          ENDDO
          DO J = 2, NDIG
             IF (MB%MP(J+2) == 0) NZDB = NZDB - 1
          ENDDO
          C = 0.9 * C / LOG(C)
          IF (REAL(NZDA)*NZDB < C*NDIG*LOG(REAL(NDIG))) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          NMETHD = 1
      ENDIF
      IF (NMETHD == 2) THEN
          CALL FMMPYFFT(MA,MB)
          IF (MWA%MP(3) == 0) THEN
              KSHIFT = 1
          ELSE
              KSHIFT = 0
          ENDIF
          RETURN
      ENDIF

      L = N1 + NGUARD
      MWA%MP(L+2) = 0

!             The multiplication loop begins here.

!             MBNORM is the minimum number of digits that can be multiplied before normalization
!                    is required.
!             MAXMWA is an upper bound on the size of values in MWA divided by (MBASE-1).  It is
!                    used to determine whether to normalize before the next digit is multiplied.

      MBP1 = MBASE + 1
      MBNORM = (MAXINT/(MBP1*MBP1))
      MMAX = INTMAX - MBASE
      MMAX = MIN((MAXINT/MBP1 - MBP1),MMAX)
      IF (MBNORM >= 2) THEN
          MBJ = MB%MP(3)

!             Count the trailing zeros in MA.

          IF (MA%MP(N1+1) /= 0) THEN
              KNZ = N1
          ELSE
              DO J = NDIG, 2, -1
                 IF (MA%MP(J+1) /= 0) THEN
                     KNZ = J
                     GO TO 110
                 ENDIF
              ENDDO
          ENDIF

  110     MWA%MP(3) = 0
          DO K = NDIG+2, L
             MWA%MP(K+1) = 0
          ENDDO

!             (Inner Loop)

          DO K = 3, N1+1
             MWA%MP(K+1) = MA%MP(K)*MBJ
          ENDDO
          MAXMWA = MBJ
          DO J = 3, N1
             MBJ = MB%MP(J+1)
             IF (MBJ /= 0) THEN
                 MAXMWA = MAXMWA + MBJ
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)

!                       Major (Inner Loop)

                 DO K = J+2, J+KL
                    MWA%MP(K) = MWA%MP(K) + MA%MP(K-JM1)*MBJ
                 ENDDO
             ENDIF

             IF (MAXMWA > MMAX) THEN
                 MAXMWA = 0
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)

!                       Here normalization is only required for the range of digits currently
!                       changing in MWA.

                 DO KB = JM1+KL, JM1+2, -1
                    MKT = INT (MWA%MP(KB+1)/MBASE)
                    MWA%MP(KB) = MWA%MP(KB) + MKT
                    MWA%MP(KB+1) = MWA%MP(KB+1) - MKT*MBASE
                 ENDDO
             ENDIF
          ENDDO

!             Perform the final normalization.  (Inner Loop)

          DO KB = L+1, 4, -1
             MKT = INT (MWA%MP(KB)/MBASE)
             MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
             MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
          ENDDO

      ELSE

!             If normalization must be done for each digit, combine the two loops and normalize as
!             the digits are multiplied.

          DO J = 2, L
             MWA%MP(J+1) = 0
          ENDDO
          KJ = NDIG + 2
          DO J = 2, N1
             KJ = KJ - 1
             MBKJ = MB%MP(KJ+1)
             IF (MBKJ == 0) CYCLE
             KL = L - KJ + 1
             IF (KL > N1) KL = N1
             KI = KL + 2
             KWA = KL+ KJ + 1
             MK = 0
             DO K = 2, KL
                MT = MA%MP(KI-K+1)*MBKJ + MWA%MP(KWA-K+1) + MK
                MK = INT (MT/MBASE)
                MWA%MP(KWA-K+1) = MT - MBASE*MK
             ENDDO
             MWA%MP(KWA-KL) = MK
          ENDDO

      ENDIF

!             Set KSHIFT = 1 if a shift left is necessary.

      IF (MWA%MP(3) == 0) THEN
          KSHIFT = 1
      ELSE
          KSHIFT = 0
      ENDIF

      RETURN
      END SUBROUTINE FMMPY3

      SUBROUTINE FMMPYD(MA,MB,MC,MD,ME)

!  Double multiplication routine.  MD = MA * MB,   ME = MA * MC

!  It is usually slightly faster to do two multiplications that have a common factor with one call.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC,MD,ME

      REAL (KIND(1.0D0)) :: MAS,MAXMWA,MBS,MBJ,MBKJ,MBNORM,MBP1,MCJ,MCKJ,MCS,  &
                            MKB,MKC,MKT,MMAX,MR,MT,MTEMP
      DOUBLE PRECISION :: ERR
      REAL :: C
      INTEGER :: J,JM1,JRSSAV,K,KB,KI,KJ,KL,KNZ,KOVUN,KSHIFT,KWA,L,N1,NGUARD
      INTENT (IN) :: MA,MB,MC
      INTENT (INOUT) :: MD,ME

      IF (.NOT. ALLOCATED(MD%MP)) THEN
          ALLOCATE(MD%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MD%MP) < NDIG+2) THEN
          DEALLOCATE(MD%MP)
          ALLOCATE(MD%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(ME%MP)) THEN
          ALLOCATE(ME%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(ME%MP) < NDIG+2) THEN
          DEALLOCATE(ME%MP)
          ALLOCATE(ME%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWD%MP)) THEN
          ALLOCATE(MWD%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWD%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWD%MP)
          ALLOCATE(MWD%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      JRSSAV = JRSIGN
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMMPYD'
          CALL FMNTR(2,MA,MB,2,1)
          IF (ABS(NTRACE) >= 2 .AND. NCALL <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL FMNTRJ(MC,NDIG)
              ELSE
                  CALL FMPRNT(MC)
              ENDIF
          ENDIF
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      C = 1000
      IF (NDIG >= C .OR.                                           &
          ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR.  &
          ABS(MC%MP(2)) > MEXPAB .OR. MBASE*MBASE <= MXBASE/(4*MBASE)) THEN
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN .OR.  &
              MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN .OR.  &
              MC%MP(2) == MEXPOV .OR. MC%MP(2) == MEXPUN) KOVUN = 1
          IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO .OR.  &
              MC%MP(2) == MUNKNO) KOVUN = 2
          NCALL = NCALL + 1
          CALL FMMPY2(MA,MB,MWD)
          KB = KFLAG
          CALL FMMPY2(MA,MC,ME)
          NCALL = NCALL - 1
          IF (((KFLAG < 0 .OR. KB < 0) .AND. KOVUN == 0) .OR.  &
              ((KFLAG == -4 .OR. KB == -4) .AND. KOVUN == 1)) THEN
              IF (KFLAG == -4 .OR. KB == -4) THEN
                  KFLAG = -4
              ELSE IF (KFLAG == -5 .OR. KB == -5) THEN
                  KFLAG = -5
              ELSE
                  KFLAG = MIN(KFLAG,KB)
              ENDIF
              NAMEST(NCALL) = 'FMMPYD'
              CALL FMWARN
          ENDIF
          CALL FMEQ(MWD,MD)
          GO TO 140
      ENDIF
      IF (MA%MP(3) == 0) THEN
          CALL FMIM(0,MD)
          CALL FMIM(0,ME)
          GO TO 140
      ENDIF
      IF (MB%MP(3) == 0) THEN
          CALL FMMPY2(MA,MC,ME)
          CALL FMIM(0,MD)
          GO TO 140
      ENDIF
      IF (MC%MP(3) == 0) THEN
          CALL FMMPY2(MA,MB,MD)
          CALL FMIM(0,ME)
          GO TO 140
      ENDIF
      KFLAG = 0

!             NGUARD is the number of guard digits used.

      IF (NCALL > 1) THEN
          NGUARD = NGRD22
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (MBASE < 10**6) NGUARD = MIN(NGUARD+1,NDIG+2)
      ENDIF
      IF ((MA%MP(3)*MB%MP(3) < MBASE .OR.  &
           MA%MP(3)*MC%MP(3) < MBASE) .AND. NGUARD < 3) NGUARD = 3

!             Save the sign of MA, MB, and MC and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)
      MCS = MC%MP(1)

      N1 = NDIG + 1
      MWA%MP(2) = MA%MP(2) + MB%MP(2)
      MWD%MP(2) = MA%MP(2) + MC%MP(2)
      L = NDIG + 1 + NGUARD
      MWA%MP(L+2) = 0
      MWD%MP(L+2) = 0

!             The multiplication loop begins here.

!             MBNORM is the minimum number of digits that can be multiplied before normalization
!                    is required.
!             MAXMWA is an upper bound on the size of values in MWA divided by (MBASE-1).  It is
!                    used to determine whether to normalize before the next digit is multiplied.

      MBP1 = MBASE + 1
      MBNORM = (MAXINT/(MBP1*MBP1))
      MMAX = INTMAX - MBASE
      MMAX = MIN((MAXINT/MBP1 - MBP1),MMAX)
      IF (MBNORM >= 2) THEN
          MBJ = MB%MP(3)
          MCJ = MC%MP(3)

!             Count the trailing zeros in MA.

          IF (MA%MP(N1+1) /= 0) THEN
              KNZ = N1
          ELSE
              DO J = NDIG, 2, -1
                 IF (MA%MP(J+1) /= 0) THEN
                     KNZ = J
                     GO TO 110
                 ENDIF
              ENDDO
          ENDIF

  110     MWA%MP(3) = 0
          MWD%MP(3) = 0
          DO K = NDIG+2, L
             MWA%MP(K+1) = 0
             MWD%MP(K+1) = 0
          ENDDO

!             (Inner Loop)

          DO K = 2, N1
             MTEMP = MA%MP(K+1)
             MWA%MP(K+2) = MTEMP*MBJ
             MWD%MP(K+2) = MTEMP*MCJ
          ENDDO
          IF (MBJ > MCJ) THEN
              MAXMWA = MBJ
          ELSE
              MAXMWA = MCJ
          ENDIF
          DO J = 3, N1
             MBJ = MB%MP(J+1)
             MCJ = MC%MP(J+1)
             IF (MBJ > MCJ) THEN
                 MAXMWA = MAXMWA + MBJ
             ELSE
                 MAXMWA = MAXMWA + MCJ
             ENDIF
             JM1 = J - 1
             KL = MIN(KNZ,L-JM1)

!                       Major (Inner Loop)

             DO K = J+2, J+KL
                MTEMP = MA%MP(K-JM1)
                MWA%MP(K) = MWA%MP(K) + MTEMP*MBJ
                MWD%MP(K) = MWD%MP(K) + MTEMP*MCJ
             ENDDO

             IF (MAXMWA > MMAX) THEN
                 MAXMWA = 0

!                       Here normalization is only required for the range of digits currently
!                       changing in MWA.

                 DO KB = JM1+KL, JM1+2, -1
                    MKT = INT (MWA%MP(KB+1)/MBASE)
                    MWA%MP(KB) = MWA%MP(KB) + MKT
                    MWA%MP(KB+1) = MWA%MP(KB+1) - MKT*MBASE
                    MKT = INT (MWD%MP(KB+1)/MBASE)
                    MWD%MP(KB) = MWD%MP(KB) + MKT
                    MWD%MP(KB+1) = MWD%MP(KB+1) - MKT*MBASE
                 ENDDO
             ENDIF
          ENDDO

!             Perform the final normalization.  (Inner Loop)

          DO KB = L+1, 4, -1
             MKT = INT (MWA%MP(KB)/MBASE)
             MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
             MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
             MKT = INT (MWD%MP(KB)/MBASE)
             MWD%MP(KB-1) = MWD%MP(KB-1) + MKT
             MWD%MP(KB) = MWD%MP(KB) - MKT*MBASE
          ENDDO

      ELSE

!             If normalization must be done for each digit, combine the two loops and normalize as
!             the digits are multiplied.

          DO J = 2, L
             MWA%MP(J+1) = 0
             MWD%MP(J+1) = 0
          ENDDO
          KJ = NDIG + 2
          DO J = 2, N1
             KJ = KJ - 1
             MBKJ = MB%MP(KJ+1)
             MCKJ = MC%MP(KJ+1)
             KL = L - KJ + 1
             IF (KL > N1) KL = N1
             KI = KL + 2
             KWA = KL+ KJ + 1
             MKB = 0
             MKC = 0
             DO K = 2, KL
                MT = MA%MP(KI-K+1)*MBKJ + MWA%MP(KWA-K+1) + MKB
                MKB = INT (MT/MBASE)
                MWA%MP(KWA-K+1) = MT - MBASE*MKB
                MT = MA%MP(KI-K+1)*MCKJ + MWD%MP(KWA-K+1) + MKC
                MKC = INT (MT/MBASE)
                MWD%MP(KWA-K+1) = MT - MBASE*MKC
             ENDDO
             MWA%MP(KWA-KL) = MKB
             MWD%MP(KWA-KL) = MKC
          ENDDO

      ENDIF

!             Set KSHIFT = 1 if a shift left is necessary.

      IF (MWA%MP(3) == 0) THEN
          KSHIFT = 1
      ELSE
          KSHIFT = 0
      ENDIF

!             The multiplications are complete.

      IF ((MAS > 0 .AND. MBS > 0) .OR. (MAS < 0 .AND. MBS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) THEN
              CALL FMMPY2(MA,MB,MD)
              GO TO 120
          ENDIF
      ENDIF
      MR = 2*MWA%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWA%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(KSHIFT+N1+1) = MWA%MP(KSHIFT+N1+1) + 1
                  MWA%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MD)

  120 IF ((MAS > 0 .AND. MCS > 0) .OR. (MAS < 0 .AND. MCS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (MWD%MP(3) == 0) THEN
          KSHIFT = 1
      ELSE
          KSHIFT = 0
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWD%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) THEN
              CALL FMMPY2(MA,MC,ME)
              GO TO 130
          ENDIF
      ENDIF
      MR = 2*MWD%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWD,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWD%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWD%MP(KSHIFT+N1+1) = MWD%MP(KSHIFT+N1+1) + 1
                  MWD%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWD,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWD,ME)

  130 IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMMPYD'
          CALL FMWARN
      ENDIF

      MD%MP(1) = 1
      IF (MAS*MBS < 0 .AND. MD%MP(2) /= MUNKNO .AND. MD%MP(3) /= 0)  &
          MD%MP(1) = -1
      ME%MP(1) = 1
      IF (MAS*MCS < 0 .AND. ME%MP(2) /= MUNKNO .AND. ME%MP(3) /= 0)  &
          ME%MP(1) = -1

  140 IF (NTRACE /= 0) THEN
          CALL FMNTR(1,MD,MD,1,1)
          IF (ABS(NTRACE) >= 1 .AND. NCALL <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL FMNTRJ(ME,NDIG)
              ELSE
                  CALL FMPRNT(ME)
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMMPYD

      SUBROUTINE FMMPYE(MA,MB,MC,MD,ME,MF,MG)

!  Triple multiplication routine.

!      ME = MA * MB,   MF = MA * MC,   MG = MA * MD

!  It is usually slightly faster to do three multiplications that have a common factor with
!  one call.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC,MD,ME,MF,MG

      REAL (KIND(1.0D0)) :: MAS,MAXJ,MAXMWA,MBS,MBJ,MBKJ,MBNORM,MBP1,  &
                            MCJ,MCKJ,MCS,MDJ,MDKJ,MDS,MKB,MKC,MKD,MKT,MMAX,MR,MT,MTEMP
      DOUBLE PRECISION :: ERR
      REAL :: C
      INTEGER :: J,JM1,JRSSAV,K,KB,KI,KJ,KL,KNZ,KOVUN,KSHIFT,KWA,L,N1,NGUARD
      INTENT (IN) :: MA,MB,MC,MD
      INTENT (INOUT) :: ME,MF,MG

      IF (.NOT. ALLOCATED(ME%MP)) THEN
          ALLOCATE(ME%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(ME%MP) < NDIG+2) THEN
          DEALLOCATE(ME%MP)
          ALLOCATE(ME%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MF%MP)) THEN
          ALLOCATE(MF%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MF%MP) < NDIG+2) THEN
          DEALLOCATE(MF%MP)
          ALLOCATE(MF%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MG%MP)) THEN
          ALLOCATE(MG%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MG%MP) < NDIG+2) THEN
          DEALLOCATE(MG%MP)
          ALLOCATE(MG%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWD%MP)) THEN
          ALLOCATE(MWD%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWD%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWD%MP)
          ALLOCATE(MWD%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWE%MP)) THEN
          ALLOCATE(MWE%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWE%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWE%MP)
          ALLOCATE(MWE%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      JRSSAV = JRSIGN
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMMPYE'
          CALL FMNTR(2,MA,MB,2,1)
          IF (ABS(NTRACE) >= 2 .AND. NCALL <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL FMNTRJ(MC,NDIG)
                  CALL FMNTRJ(MD,NDIG)
              ELSE
                  CALL FMPRNT(MC)
                  CALL FMPRNT(MD)
              ENDIF
          ENDIF
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      C = 1000
      IF (NDIG >= C .OR.                                           &
          ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR.  &
          ABS(MC%MP(2)) > MEXPAB .OR. ABS(MD%MP(2)) > MEXPAB .OR.  &
          MBASE*MBASE <= MXBASE/(4*MBASE)) THEN
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN .OR.  &
              MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN .OR.  &
              MC%MP(2) == MEXPOV .OR. MC%MP(2) == MEXPUN .OR.  &
              MD%MP(2) == MEXPOV .OR. MD%MP(2) == MEXPUN) KOVUN = 1
          IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO .OR.  &
              MC%MP(2) == MUNKNO .OR. MD%MP(2) == MUNKNO) KOVUN = 2
          NCALL = NCALL + 1
          CALL FMMPY2(MA,MB,MWD)
          KB = KFLAG
          CALL FMMPY2(MA,MC,MWE)
          KJ = KFLAG
          CALL FMMPY2(MA,MD,MG)
          NCALL = NCALL - 1
          IF (((KFLAG < 0 .OR. KB < 0 .OR. KJ < 0) .AND. KOVUN == 0)  &
              .OR. ((KFLAG == -4 .OR. KB == -4 .OR. KJ == -4) .AND.   &
              KOVUN == 1)) THEN
              IF (KFLAG == -4 .OR. KB == -4 .OR. KJ == -4) THEN
                  KFLAG = -4
              ELSE IF (KFLAG == -5 .OR. KB == -5 .OR. KJ == -5) THEN
                  KFLAG = -5
              ELSE
                  KFLAG = MIN(KFLAG,KB,KJ)
              ENDIF
              NAMEST(NCALL) = 'FMMPYE'
              CALL FMWARN
          ENDIF
          CALL FMEQ(MWD,ME)
          CALL FMEQ(MWE,MF)
          GO TO 150
      ENDIF
      IF (MA%MP(3) == 0) THEN
          CALL FMIM(0,ME)
          CALL FMIM(0,MF)
          CALL FMIM(0,MG)
          GO TO 150
      ENDIF
      IF (MB%MP(3) == 0 .OR. MC%MP(3) == 0 .OR. MD%MP(3) == 0) THEN
          CALL FMMPY2(MA,MB,MWD)
          CALL FMMPY2(MA,MC,MWE)
          CALL FMMPY2(MA,MD,MG)
          CALL FMEQ(MWD,ME)
          CALL FMEQ(MWE,MF)
          GO TO 150
      ENDIF
      KFLAG = 0

!             NGUARD is the number of guard digits used.

      IF (NCALL > 1) THEN
          NGUARD = NGRD22
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (MBASE < 10**6) NGUARD = MIN(NGUARD+1,NDIG+2)
      ENDIF
      IF ((MA%MP(3)*MB%MP(3) < MBASE .OR.  &
           MA%MP(3)*MC%MP(3) < MBASE .OR.  &
           MA%MP(3)*MD%MP(3) < MBASE) .AND. NGUARD < 3) NGUARD = 3

!             Save the signs and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)
      MCS = MC%MP(1)
      MDS = MD%MP(1)

      N1 = NDIG + 1
      MWA%MP(2) = MA%MP(2) + MB%MP(2)
      MWD%MP(2) = MA%MP(2) + MC%MP(2)
      MWE%MP(2) = MA%MP(2) + MD%MP(2)
      L = NDIG + 1 + NGUARD
      MWA%MP(L+2) = 0
      MWD%MP(L+2) = 0
      MWE%MP(L+2) = 0

!             The multiplication loop begins here.

!             MBNORM is the minimum number of digits that can be multiplied before normalization
!                    is required.
!             MAXMWA is an upper bound on the size of values in MWA divided by (MBASE-1).  It is
!                    used to determine whether to normalize before the next digit is multiplied.

      MBP1 = MBASE + 1
      MBNORM = (MAXINT/(MBP1*MBP1))
      MMAX = INTMAX - MBASE
      MMAX = MIN((MAXINT/MBP1 - MBP1),MMAX)
      IF (MBNORM >= 2) THEN
          MBJ = MB%MP(3)
          MCJ = MC%MP(3)
          MDJ = MD%MP(3)

!             Count the trailing zeros in MA.

          IF (MA%MP(N1+1) /= 0) THEN
              KNZ = N1
          ELSE
              DO J = NDIG, 2, -1
                 IF (MA%MP(J+1) /= 0) THEN
                     KNZ = J
                     GO TO 110
                 ENDIF
              ENDDO
          ENDIF

  110     MWA%MP(3) = 0
          MWD%MP(3) = 0
          MWE%MP(3) = 0
          DO K = NDIG+2, L
             MWA%MP(K+1) = 0
             MWD%MP(K+1) = 0
             MWE%MP(K+1) = 0
          ENDDO

!             (Inner Loop)

          DO K = 2, N1
             MTEMP = MA%MP(K+1)
             MWA%MP(K+2) = MTEMP*MBJ
             MWD%MP(K+2) = MTEMP*MCJ
             MWE%MP(K+2) = MTEMP*MDJ
          ENDDO
          MAXMWA = MBJ
          IF (MCJ > MAXMWA) MAXMWA = MCJ
          IF (MDJ > MAXMWA) MAXMWA = MDJ
          DO J = 3, N1
             MBJ = MB%MP(J+1)
             MCJ = MC%MP(J+1)
             MDJ = MD%MP(J+1)
             MAXJ = MBJ
             IF (MCJ > MAXJ) MAXJ = MCJ
             IF (MDJ > MAXJ) MAXJ = MDJ
             MAXMWA = MAXMWA + MAXJ
             JM1 = J - 1
             KL = MIN(KNZ,L-JM1)

!                       Major (Inner Loop)

             DO K = J+2, J+KL
                MTEMP = MA%MP(K-JM1)
                MWA%MP(K) = MWA%MP(K) + MTEMP*MBJ
                MWD%MP(K) = MWD%MP(K) + MTEMP*MCJ
                MWE%MP(K) = MWE%MP(K) + MTEMP*MDJ
             ENDDO

             IF (MAXMWA > MMAX) THEN
                 MAXMWA = 0

!                       Here normalization is only required for the range of digits currently
!                       changing in MWA.

                 DO KB = JM1+KL, JM1+2, -1
                    MKT = INT (MWA%MP(KB+1)/MBASE)
                    MWA%MP(KB) = MWA%MP(KB) + MKT
                    MWA%MP(KB+1) = MWA%MP(KB+1) - MKT*MBASE
                    MKT = INT (MWD%MP(KB+1)/MBASE)
                    MWD%MP(KB) = MWD%MP(KB) + MKT
                    MWD%MP(KB+1) = MWD%MP(KB+1) - MKT*MBASE
                    MKT = INT (MWE%MP(KB+1)/MBASE)
                    MWE%MP(KB) = MWE%MP(KB) + MKT
                    MWE%MP(KB+1) = MWE%MP(KB+1) - MKT*MBASE
                 ENDDO
             ENDIF
          ENDDO

!             Perform the final normalization.  (Inner Loop)

          DO KB = L+1, 4, -1
             MKT = INT (MWA%MP(KB)/MBASE)
             MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
             MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
             MKT = INT (MWD%MP(KB)/MBASE)
             MWD%MP(KB-1) = MWD%MP(KB-1) + MKT
             MWD%MP(KB) = MWD%MP(KB) - MKT*MBASE
             MKT = INT (MWE%MP(KB)/MBASE)
             MWE%MP(KB-1) = MWE%MP(KB-1) + MKT
             MWE%MP(KB) = MWE%MP(KB) - MKT*MBASE
          ENDDO

      ELSE

!             If normalization must be done for each digit, combine the two loops and normalize as
!             the digits are multiplied.

          DO J = 2, L
             MWA%MP(J+1) = 0
             MWD%MP(J+1) = 0
             MWE%MP(J+1) = 0
          ENDDO
          KJ = NDIG + 2
          DO J = 2, N1
             KJ = KJ - 1
             MBKJ = MB%MP(KJ+1)
             MCKJ = MC%MP(KJ+1)
             MDKJ = MD%MP(KJ+1)
             KL = L - KJ + 1
             IF (KL > N1) KL = N1
             KI = KL + 2
             KWA = KL+ KJ + 1
             MKB = 0
             MKC = 0
             MKD = 0
             DO K = 2, KL
                MT = MA%MP(KI-K+1)*MBKJ + MWA%MP(KWA-K+1) + MKB
                MKB = INT (MT/MBASE)
                MWA%MP(KWA-K+1) = MT - MBASE*MKB
                MT = MA%MP(KI-K+1)*MCKJ + MWD%MP(KWA-K+1) + MKC
                MKC = INT (MT/MBASE)
                MWD%MP(KWA-K+1) = MT - MBASE*MKC
                MT = MA%MP(KI-K+1)*MDKJ + MWE%MP(KWA-K+1) + MKD
                MKD = INT (MT/MBASE)
                MWE%MP(KWA-K+1) = MT - MBASE*MKD
             ENDDO
             MWA%MP(KWA-KL) = MKB
             MWD%MP(KWA-KL) = MKC
             MWE%MP(KWA-KL) = MKD
          ENDDO

      ENDIF

!             Set KSHIFT = 1 if a shift left is necessary.

      IF (MWA%MP(3) == 0) THEN
          KSHIFT = 1
      ELSE
          KSHIFT = 0
      ENDIF

!             The multiplications are complete.

      IF ((MAS > 0 .AND. MBS > 0) .OR. (MAS < 0 .AND. MBS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) THEN
              CALL FMMPY2(MA,MB,ME)
              GO TO 120
          ENDIF
      ENDIF
      MR = 2*MWA%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWA%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(KSHIFT+N1+1) = MWA%MP(KSHIFT+N1+1) + 1
                  MWA%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,ME)

  120 IF ((MAS > 0 .AND. MCS > 0) .OR. (MAS < 0 .AND. MCS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (MWD%MP(3) == 0) THEN
          KSHIFT = 1
      ELSE
          KSHIFT = 0
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWD%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) THEN
              CALL FMMPY2(MA,MC,MF)
              GO TO 130
          ENDIF
      ENDIF
      MR = 2*MWD%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWD,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWD%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWD%MP(KSHIFT+N1+1) = MWD%MP(KSHIFT+N1+1) + 1
                  MWD%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWD,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWD,MF)

  130 IF ((MAS > 0 .AND. MDS > 0) .OR. (MAS < 0 .AND. MDS < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (MWE%MP(3) == 0) THEN
          KSHIFT = 1
      ELSE
          KSHIFT = 0
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWE%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) THEN
              CALL FMMPY2(MA,MD,MG)
              GO TO 140
          ENDIF
      ENDIF
      MR = 2*MWE%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWE,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWE%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWE%MP(KSHIFT+N1+1) = MWE%MP(KSHIFT+N1+1) + 1
                  MWE%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWE,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWE,MG)

  140 IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMMPYE'
          CALL FMWARN
      ENDIF

      ME%MP(1) = 1
      IF (MAS*MBS < 0 .AND. ME%MP(2) /= MUNKNO .AND. ME%MP(3) /= 0)  &
          ME%MP(1) = -1
      MF%MP(1) = 1
      IF (MAS*MCS < 0 .AND. MF%MP(2) /= MUNKNO .AND. MF%MP(3) /= 0)  &
          MF%MP(1) = -1
      MG%MP(1) = 1
      IF (MAS*MDS < 0 .AND. MG%MP(2) /= MUNKNO .AND. MG%MP(3) /= 0)  &
          MG%MP(1) = -1

  150 IF (NTRACE /= 0) THEN
          CALL FMNTR(1,ME,ME,1,1)
          IF (ABS(NTRACE) >= 1 .AND. NCALL <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL FMNTRJ(MF,NDIG)
                  CALL FMNTRJ(MG,NDIG)
              ELSE
                  CALL FMPRNT(MF)
                  CALL FMPRNT(MG)
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMMPYE

      RECURSIVE SUBROUTINE FMMPYFFT(MA,MB)

!  Internal multiplication routine MA*MB for very high precision.
!  The result is returned in the internal work area MWA.
!  Fast Fourier transforms are used, and the number of digits carried is usually raised slightly,
!  because the FFT is faster when N has only small prime factors.

!  Use one of two methods, depending on the base MBASE.  When precision is high enough, the size of
!  the digits being convolved using FFT's must be reduced in order to keep the convolution products
!  from being too big to exactly recover the integer results.  The FFT operation has double
!  precision rounding errors, but the result of the convolution of two lists of integers is really
!  an integer.

!  For example, assume double precision carries 53 bits giving about 16 significant digit accuracy,
!  all the (positive) numbers in the two lists are less than K, and there are N numbers in each
!  list.  Then the convolution is an integer less than N*K*K.  A typical case might have MA and MB
!  in base 10**7 with 50,000 digits for about 350,000 significant digit precision.  This means
!  K = 10**7 and N = 5*10**4, so N*K*K = 5*10**18.  That is too big for this double precision.

!  Method 1:  If the base is a power of a small base ( MBASE = B**L for 2 <= B <= 19 ), then change
!             MA and MB to a base that is a smaller power of B to reduce the size of the individual
!             digits.  Changing to this smaller base is a fast O(N) operation.

!             In the example above, MA and MB could be changed to numbers with about 120,000 digits
!             in base 10**3.  Then N*K*K = 1.2*10**11, so even after losing 2 or 3 digits to
!             rounding in the FFT the results could be reliably rounded to the nearest integer.

!             This is the method used for the default FM power-of-ten base chosen in FMSET.
!             It is faster than method 2.

!  Method 2:  Here a fast change to a smaller base is not available, so each of MA and MB is split
!             into two pieces.  Each piece is a number with the same base and precision as MA and
!             MB have, but has artificially small digits.

!             Suppose the base is near 10**7 but not a power, say MBASE = B = 12345678, and MA or
!             MB is
!                       X = 1234567/B + 2345678/B**2 + ... + 9876543/B**N

!             Let K = SQRT(B) = 3513 be the upper bound on the size of the digits in the two
!             pieces, X1 and X2.  We write X = X1 + K*X2 by defining the digits of X1 to be the
!             digits of X mod K, and the digits of X2 to be the digits of X / K.  That gives

!                       X1 = 1504/B + 2507/B**2 + ... + 1500/B**N
!                       X2 =  351/B +  667/B**2 + ... + 2811/B**N

!             Now, X*Y = ( X1 + K*X2 ) ( Y1 + K*Y2 )
!                      =   X1*Y1 + K*(X1*Y2 + X2*Y1) + K**2*X2*Y2

!             Since the digits of X1 and X2 are formed one at a time from the corresponding digits
!             of X, generating X1 and X2 is a fast O(N) operation.

!             The terms in these products are still written in base b, but the digits are small,
!             no more than K.  These four multiplications are reduced to three, computing
!             X1*Y1, X2*Y2, and (X1+X2)*(Y1+Y2).
!             Then X1*Y2 + X2*Y1 = (X1+X2)*(Y1+Y2) - X1*Y1 - X2*Y2.  See Knuth, V 2, section 4.3.3.

!             Method 2 is recursive, since if N is large enough N*K*K may still be too large for
!             the double precision rounding errors.  In that case another splitting is done, giving
!             digits less than SQRT(AINT(SQRT(B))) = 59 in this example.

!             For B = 12345678 and 53-bit double precision, the first splitting is done for all N,
!             since B**2 > 10**14 is already too close to 16-digit integers.
!             A second splitting is done for N larger than about 4*10**5 (about 2.8 million decimal
!             digits), and a third for N > 1.4*10**9 (about 10 billion decimals).

      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      TYPE(MULTI) :: MXY(8)
      INTEGER :: J,K,L,NDSAVE
      DOUBLE PRECISION :: B,BL,D,DMAX


!             Check to see if the base is a power of a small integer.

      DO J = 2, 19
         IF (J == 2 .OR. J == 10) THEN
             B = 12 - J
         ELSE
             B = J
         ENDIF
         BL = 1
         DO L = 1, 100
            BL = BL*B
            IF (MBASE == BL) THEN
                CALL FMMPYFFT1(MA,MB,B,L)
                MWA%MP(2) = MA%MP(2) + MB%MP(2)
                RETURN
            ENDIF
            IF (MBASE < BL) EXIT
         ENDDO
      ENDDO

!             Use method 2.

!             Find the maximum size of the digits in MA and MB.

      DMAX = 0
      DO J = 1, NDIG
         D = MA%MP(J+2)
         IF (D > DMAX) DMAX = D
         D = MB%MP(J+2)
         IF (D > DMAX) DMAX = D
      ENDDO

!             Check to see if splitting is needed.

      IF (DMAX**2 > 1/(4.0D3*NDIG*EPSILON(1.0D0))) THEN
          K = INT(SQRT(DMAX))
          NDSAVE = NDIG
          NDIG = 2*NDIG + 2
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
          CALL FMEQU(MB,MXY(3),NDSAVE,NDIG)
          CALL FMEQU(MB,MXY(4),NDSAVE,NDIG)
          CALL FMIM(0,MXY(5))
          CALL FMIM(0,MXY(6))
          CALL FMIM(0,MXY(7))
          CALL FMIM(0,MXY(8))
          DO J = 1, NDIG
             L = MXY(1)%MP(J+2)
             MXY(1)%MP(J+2) = MOD(L,K)
             MXY(2)%MP(J+2) = L/K
             L = MXY(3)%MP(J+2)
             MXY(3)%MP(J+2) = MOD(L,K)
             MXY(4)%MP(J+2) = L/K
          ENDDO

!             Normalize any of these having a zero leading digit.

          IF (MXY(1)%MP(3) == 0) CALL FMMPYFFT_NORM(MXY(1))
          IF (MXY(2)%MP(3) == 0) CALL FMMPYFFT_NORM(MXY(2))
          IF (MXY(3)%MP(3) == 0) CALL FMMPYFFT_NORM(MXY(3))
          IF (MXY(4)%MP(3) == 0) CALL FMMPYFFT_NORM(MXY(4))

          NDIG = NDSAVE
          CALL FMMPYFFT(MXY(1),MXY(3))
          NDIG = 2*NDIG + 2
          CALL FMMOVE(MWA,MXY(5))
          NDIG = NDSAVE
          CALL FMMPYFFT(MXY(2),MXY(4))
          NDIG = 2*NDIG + 2
          CALL FMMOVE(MWA,MXY(6))
          CALL FMADD2(MXY(1),MXY(2),MXY(7))
          CALL FMADD2(MXY(3),MXY(4),MXY(8))
          NDIG = NDSAVE
          CALL FMMPYFFT(MXY(7),MXY(8))
          NDIG = 2*NDIG + 2
          CALL FMMOVE(MWA,MXY(7))
          KSUB = 1
          CALL FMADD2(MXY(7),MXY(5),MXY(1))
          CALL FMADD2(MXY(1),MXY(6),MXY(2))
          KSUB = 0
          CALL FMMPYFFTI(MXY(6),K,MXY(3))
          CALL FMADD2(MXY(3),MXY(2),MXY(4))
          CALL FMMPYFFTI(MXY(4),K,MXY(3))
          CALL FMADD2(MXY(3),MXY(5),MXY(4))
          MWA%MP(1) = MXY(4)%MP(1)
          MWA%MP(2) = MXY(4)%MP(2)
          IF (MWA%MP(3) == 0) MWA%MP(2) = MWA%MP(2) + 1
          NDIG = NDSAVE
      ELSE
          CALL FMMPYFFT2(MA,MB)
          MWA%MP(2) = MA%MP(2) + MB%MP(2)
      ENDIF

      RETURN
      END SUBROUTINE FMMPYFFT

      SUBROUTINE FMMPYFFT1(MA,MB,B,L)

      USE ModLib_FMVALS
      IMPLICIT NONE

!  Internal multiplication routine MA*MB for very high precision.
!  The base for the arithmetic (MBASE) is a power of B, B**L, for 2 <= B <= 19.  This includes the
!  usual case where the default base chosen in FMSET is a power of 10.
!  Fast Fourier transforms are used, and the number of digits carried is usually raised slightly,
!  because the FFT is faster when N has only small prime factors.

      TYPE(MULTI) :: MA,MB
      INTEGER :: J,K,K2,K3,K5,KA,L,L1,L2,L3,L5,N,N2,ND,NUM,NUMAR,NUMAI,NUMBR,NUMBI
      REAL (KIND(0.0D0)) :: D2,D3,DMIN,DPROD
      REAL (KIND(0.0D0)) :: B,BASE,C,D,T,T2,THETA,TMA,TMB
      COMPLEX (KIND(0.0D0)), DIMENSION(:), ALLOCATABLE :: CX,CY,CZ,ROOTS_OF_UNITY
      COMPLEX (KIND(0.0D0)) :: CI,H1,H2,ST,W0,W

!             Initialize guard digits in MWA.

      DO J = 2*NDIG, MIN(2*NDIG+30, SIZE(MWA%MP)-2)
         MWA%MP(J+2) = 0
      ENDDO

!             If the base and/or number of digits is too large, rounding errors in the FFT
!             calculation will cause the result to be wrong.
!             Reduce the base if necessary.

      ND = NDIG
      BASE = MBASE
      IF (MBASE**2 > 1/(1.0D3*ND*EPSILON(1.0D0))) THEN
          BASE = 1/SQRT(1.0D3*ND*EPSILON(1.0D0))
          K = LOG(BASE)/LOG(B)
          BASE = B**K
          ND = NDIG*DBLE(L)/K + 1
      ENDIF

!             Choose the number of digits to use for the FFT.  Make the size of the array have no
!             prime factors other than 2, 3, or 5.

      L2 = LOG(10.0D0*ND)/LOG(2.0D0) + 2
      L3 = LOG(10.0D0*ND)/LOG(3.0D0) + 2
      L5 = 0
      IF (ND <= 100000) THEN
          L1 = 1
      ELSE
          L1 = MAX(2,NINT(2*LOG10(DBLE(ND))-8))
      ENDIF
      DMIN = HUGE(2.0D0)
      D2 = 2.0D0 ** L1
      DO K2 = L1, L2
         D3 = 1.0D0
         DO K3 = 0, L3
            DPROD = D2*D3
            DO K5 = 0, L5
               IF (DPROD > DMIN) EXIT
               IF (DPROD >= ND) THEN
                   IF (DPROD < DMIN) DMIN = DPROD
               ENDIF
               DPROD = 5*DPROD
            ENDDO
            D3 = 3*D3
         ENDDO
         D2 = 2*D2
      ENDDO
      N = DMIN
      N2 = N*2
      ALLOCATE(CX(N),STAT=J)
      IF (J /= 0) THEN
          CALL FMDEFINE_ERROR
      ENDIF
      ALLOCATE(CY(N),STAT=J)
      IF (J /= 0) THEN
          CALL FMDEFINE_ERROR
      ENDIF
      ALLOCATE(CZ(N),STAT=J)
      IF (J /= 0) THEN
          CALL FMDEFINE_ERROR
      ENDIF
      ALLOCATE(ROOTS_OF_UNITY(0:N-1),STAT=J)
      IF (J /= 0) THEN
          CALL FMDEFINE_ERROR
      ENDIF
      CALL FMFFT_INIT(ROOTS_OF_UNITY,N)

!             Pad the lists of digits with zeros, then pack the length 2*N real arrays into
!             length N complex arrays to speed up the FFT operations.

      IF (BASE == MBASE) THEN
          DO J = NDIG/2, N
             CX(J) = 0
             CY(J) = 0
          ENDDO
          DO J = 2, NDIG, 2
             CX(J/2) = CMPLX( MA%MP(J+1) , MA%MP(J+2) , KIND(0.0D0) )
             CY(J/2) = CMPLX( MB%MP(J+1) , MB%MP(J+2) , KIND(0.0D0) )
          ENDDO
          IF (MOD(NDIG,2) == 1) THEN
              CX(NDIG/2+1) = MA%MP(NDIG+2)
              CY(NDIG/2+1) = MB%MP(NDIG+2)
          ENDIF
      ELSE
          DO J = NDIG/2, N
             CX(J) = 0
             CY(J) = 0
          ENDDO
          D = MBASE/BASE
          C = MBASE*D
          TMA = MA%MP(3)*MBASE + MA%MP(4)
          NUMAR = TMA/C
          TMA = TMA - C*NUMAR
          TMB = MB%MP(3)*MBASE + MB%MP(4)
          NUMBR = TMB/C
          TMB = TMB - C*NUMBR
          K = 0
          DO J = 3, NDIG
             DO
                IF (C >= BASE) THEN
                    C = C/BASE
                ELSE
                    TMA = TMA*MBASE + MA%MP(J+2)
                    TMB = TMB*MBASE + MB%MP(J+2)
                    C = C*MBASE
                    EXIT
                ENDIF
                NUMAI = TMA/C
                TMA = TMA - C*NUMAI
                NUMBI = TMB/C
                TMB = TMB - C*NUMBI
                IF (NUMAR < 0) THEN
                    NUMAR = NUMAI
                    NUMBR = NUMBI
                ELSE
                    K = K + 1
                    CX(K) = CMPLX( DBLE(NUMAR) , DBLE(NUMAI) , KIND(0.0D0) )
                    CY(K) = CMPLX( DBLE(NUMBR) , DBLE(NUMBI) , KIND(0.0D0) )
                    NUMAR = -1
                ENDIF
             ENDDO
          ENDDO
          DO
             IF (C >= BASE) THEN
                 C = C/BASE
             ELSE
                 NUMAI = TMA*(BASE/C)
                 NUMBI = TMB*(BASE/C)
                 IF (NUMAR < 0) THEN
                     K = K + 1
                     CX(K) = CMPLX( DBLE(NUMAI) , 0.0D0 , KIND(0.0D0) )
                     CY(K) = CMPLX( DBLE(NUMBI) , 0.0D0 , KIND(0.0D0) )
                 ELSE
                     K = K + 1
                     CX(K) = CMPLX( DBLE(NUMAR) , DBLE(NUMAI) , KIND(0.0D0) )
                     CY(K) = CMPLX( DBLE(NUMBR) , DBLE(NUMBI) , KIND(0.0D0) )
                 ENDIF
                 EXIT
             ENDIF
             NUMAI = TMA/C
             TMA = TMA - C*NUMAI
             NUMBI = TMB/C
             TMB = TMB - C*NUMBI
             IF (NUMAR < 0) THEN
                 NUMAR = NUMAI
                 NUMBR = NUMBI
             ELSE
                 K = K + 1
                 CX(K) = CMPLX( DBLE(NUMAR) , DBLE(NUMAI) , KIND(0.0D0) )
                 CY(K) = CMPLX( DBLE(NUMBR) , DBLE(NUMBI) , KIND(0.0D0) )
                 NUMAR = -1
             ENDIF
          ENDDO
      ENDIF

!             Transform.

      CALL FMFFT(CX,N,ROOTS_OF_UNITY,CZ)
      IF (KSQR /= 1) THEN
          CALL FMFFT(CY,N,ROOTS_OF_UNITY,CZ)
      ENDIF

!             Unpack the two transforms.

      THETA = ACOS(-1.0D0)/N
      W0 = CMPLX( COS(THETA) , SIN(THETA) , KIND(0.0D0) )
      W = W0
      ST = CMPLX( -2*SIN(THETA/2)**2 , SIN(THETA) , KIND(0.0D0) )
      CI = CMPLX( 0.0D0 , 1.0D0 , KIND(0.0D0) )
      DO J = 2, N/2
         H1 = 0.5D0*(CX(J)+CONJG(CX(N+2-J)))
         H2 = -0.5D0*CI*(CX(J)-CONJG(CX(N+2-J)))
         CX(J) = H1 + W*H2
         CX(N+2-J) = CONJG(H1 - W*H2)
         IF (KSQR /= 1) THEN
             H1 = 0.5D0*(CY(J)+CONJG(CY(N+2-J)))
             H2 = -0.5D0*CI*(CY(J)-CONJG(CY(N+2-J)))
             CY(J) = H1 + W*H2
             CY(N+2-J) = CONJG(H1 - W*H2)
         ENDIF
         W = W + ST*W
      ENDDO
      CX(1) = CMPLX( REAL(CX(1))+AIMAG(CX(1)) , REAL(CX(1))-AIMAG(CX(1)) , KIND(0.0D0) )
      IF (KSQR /= 1) THEN
          CY(1) = CMPLX( REAL(CY(1))+AIMAG(CY(1)) , REAL(CY(1))-AIMAG(CY(1)) , KIND(0.0D0) )
      ENDIF

!             Multiply.

      IF (KSQR /= 1) THEN
          CZ(1) = CMPLX( REAL(CX(1))*REAL(CY(1)) , AIMAG(CX(1))*AIMAG(CY(1)) , KIND(0.0D0) )
          DO J = 2, N
             CZ(J) = CX(J)*CY(J)
          ENDDO
      ELSE
          CZ(1) = CMPLX( REAL(CX(1))*REAL(CX(1)) , AIMAG(CX(1))*AIMAG(CX(1)) , KIND(0.0D0) )
          DO J = 2, N
             CZ(J) = CX(J)*CX(J)
          ENDDO
      ENDIF

!             Pack the product for input to the final FFT.

      THETA = -ACOS(-1.0D0)/N
      W0 = CMPLX( COS(THETA) , SIN(THETA) , KIND(0.0D0) )
      W = W0
      ST = CMPLX( -2*SIN(THETA/2)**2 , SIN(THETA) , KIND(0.0D0) )
      CI = CMPLX( 0.0D0 , 1.0D0 , KIND(0.0D0) )
      DO J = 2, N/2
         H1 = 0.5D0*(CZ(J)+CONJG(CZ(N+2-J)))
         H2 = 0.5D0*CI*(CZ(J)-CONJG(CZ(N+2-J)))
         CZ(J) = H1 + W*H2
         CZ(N+2-J) = CONJG(H1 - W*H2)
         W = W + ST*W
      ENDDO
      CZ(1) = CMPLX( REAL(CZ(1))+AIMAG(CZ(1)) , REAL(CZ(1))-AIMAG(CZ(1)) , KIND(0.0D0) )*0.5D0

!             Transform the product to get the convolution of the original inputs.

      CALL FMFFT(CZ,N,ROOTS_OF_UNITY,CY)
      T = 1.0D0/N
      DO J = 1, N
         CZ(J) = CZ(J)*T
      ENDDO

!             Normalize the digits.

!             The CZ array holds the result in scrambled order, with unnormalized digits.
!             The leading two digits of the convolution are in CZ(1), then the next two
!             are in CZ(N), then CZ(N-1), ..., CZ(2).
!             The imaginary part of CZ(2) is zero (up to roundoff), for a total of 2*N-1
!             digits in the convolution.
!             These digits can be as big as N*BASE^2 here.
!
!             First normalize and unscramble.

      IF (BASE == MBASE) THEN
          D = AINT( 0.5D0+REAL(CZ(2)) , KIND(0.0D0) )
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = N2
          IF (KA <= SIZE(MWA%MP)-3) THEN
              MWA%MP(KA+2) = T2
          ENDIF
          DO J = 3, N
             D = AINT( 0.5D0+AIMAG(CZ(J)) , KIND(0.0D0) ) + T
             T = AINT( D/BASE, KIND(0.0D0) )
             T2 = D - T*BASE
             KA = KA - 1
             IF (KA <= SIZE(MWA%MP)-3) THEN
                 MWA%MP(KA+2) = T2
             ENDIF
             D = AINT( 0.5D0+REAL(CZ(J)) , KIND(0.0D0) ) + T
             T = AINT( D/BASE, KIND(0.0D0) )
             T2 = D - T*BASE
             KA = KA - 1
             IF (KA <= SIZE(MWA%MP)-3) THEN
                 MWA%MP(KA+2) = T2
             ENDIF
          ENDDO
          D = AINT( 0.5D0+AIMAG(CZ(1)) , KIND(0.0D0) ) + T
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = KA - 1
          MWA%MP(KA+2) = T2
          D = AINT( 0.5D0+REAL(CZ(1)) , KIND(0.0D0) ) + T
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = KA - 1
          MWA%MP(KA+2) = T2
          KA = KA - 1
          MWA%MP(KA+2) = T
      ELSE
          CI = CMPLX( 0.0D0 , 1.0D0 , KIND(0.0D0) )
          D = AINT( 0.5D0+REAL(CZ(2)) , KIND(0.0D0) )
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = N
          CX(KA) = CI*T2
          DO J = 3, N
             D = AINT( 0.5D0+AIMAG(CZ(J)) , KIND(0.0D0) ) + T
             T = AINT( D/BASE, KIND(0.0D0) )
             T2 = D - T*BASE
             CX(KA) = T2 + CX(KA)
             D = AINT( 0.5D0+REAL(CZ(J)) , KIND(0.0D0) ) + T
             T = AINT( D/BASE, KIND(0.0D0) )
             T2 = D - T*BASE
             KA = KA - 1
             CX(KA) = CI*T2
          ENDDO
          D = AINT( 0.5D0+AIMAG(CZ(1)) , KIND(0.0D0) ) + T
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          CX(KA) = T2 + CX(KA)
          D = AINT( 0.5D0+REAL(CZ(1)) , KIND(0.0D0) ) + T
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = KA - 1
          CX(KA) = T + CI*T2
          TMA = 0
          C = 1
          K = 0
          DO J = 1, N2
             IF (MOD(J,2) == 1) THEN
                 NUM = REAL(CX((J+1)/2))
             ELSE
                 NUM = AIMAG(CX((J+1)/2))
             ENDIF
             TMA = TMA*BASE + NUM
             C = C*BASE
             IF (C < MBASE) CYCLE
             C = C/MBASE
             NUM = TMA/C
             TMA = TMA - C*NUM
             K = K + 1
             IF (K <= SIZE(MWA%MP)-3 .AND. K <= 2*NDIG+2) THEN
                 MWA%MP(K+2) = NUM
             ELSE
                 EXIT
             ENDIF
          ENDDO
          K = K + 1
          IF (K <= SIZE(MWA%MP)-3 .AND. K <= 2*NDIG+2) MWA%MP(K+2) = TMA*(MBASE/C)
      ENDIF

      DEALLOCATE(CX,CY,CZ,ROOTS_OF_UNITY)
      RETURN
      END SUBROUTINE FMMPYFFT1

      SUBROUTINE FMMPYFFT2(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE

!  Internal multiplication routine MA*MB for very high precision.
!  Fast Fourier transforms are used, and the number of digits carried is usually raised slightly,
!  because the FFT is faster when N has only small prime factors.
!  This routine is used for method 2 of FMMPYFFT, where the base is not a power of a small number
!  and a different kind of splitting is used to avoid fft convolutions becoming too large.

      TYPE(MULTI) :: MA,MB
      INTEGER :: J,K,K2,K3,K5,KA,L1,L2,L3,L5,N,N2,ND,NUM,NUMAR,NUMAI,NUMBR,NUMBI
      REAL (KIND(0.0D0)) :: D2,D3,DMIN,DPROD
      REAL (KIND(0.0D0)) :: BASE,C,D,T,T2,THETA,TMA,TMB
      COMPLEX (KIND(0.0D0)), DIMENSION(:), ALLOCATABLE :: CX,CY,CZ,ROOTS_OF_UNITY
      COMPLEX (KIND(0.0D0)) :: CI,H1,H2,ST,W0,W

      ND = NDIG
      BASE = MBASE

!             Initialize guard digits in MWA.

      DO J = 2*NDIG, MIN(2*NDIG+30,SIZE(MWA%MP)-2)
         MWA%MP(J+2) = 0
      ENDDO

!             Choose the number of digits to use for the FFT.  Make the size of the array have no
!             prime factors other than 2, 3, or 5.

      L2 = LOG(10.0D0*ND)/LOG(2.0D0) + 2
      L3 = LOG(10.0D0*ND)/LOG(3.0D0) + 2
      L5 = 0
      IF (ND <= 100000) THEN
          L1 = 1
      ELSE
          L1 = MAX(2,NINT(2*LOG10(DBLE(ND))-8))
      ENDIF
      DMIN = HUGE(2.0D0)
      D2 = 2.0D0
      DO K2 = L1, L2
         D3 = 1.0D0
         DO K3 = 0, L3
            DPROD = D2*D3
            DO K5 = 0, L5
               IF (DPROD > DMIN) EXIT
               IF (DPROD >= ND) THEN
                   IF (DPROD < DMIN) DMIN = DPROD
               ENDIF
               DPROD = 5*DPROD
            ENDDO
            D3 = 3*D3
         ENDDO
         D2 = 2*D2
      ENDDO
      N = DMIN
      N2 = N*2
      ALLOCATE(CX(N),STAT=J)
      IF (J /= 0) THEN
          CALL FMDEFINE_ERROR
      ENDIF
      ALLOCATE(CY(N),STAT=J)
      IF (J /= 0) THEN
          CALL FMDEFINE_ERROR
      ENDIF
      ALLOCATE(CZ(N),STAT=J)
      IF (J /= 0) THEN
          CALL FMDEFINE_ERROR
      ENDIF
      ALLOCATE(ROOTS_OF_UNITY(0:N-1),STAT=J)
      IF (J /= 0) THEN
          CALL FMDEFINE_ERROR
      ENDIF
      CALL FMFFT_INIT(ROOTS_OF_UNITY,N)

!             Pad the lists of digits with zeros, then pack the length 2*N real arrays into
!             length N complex arrays to speed up the FFT operations.

      IF (BASE == MBASE) THEN
          DO J = NDIG/2, N
             CX(J) = 0
             CY(J) = 0
          ENDDO
          DO J = 2, NDIG, 2
             CX(J/2) = CMPLX( MA%MP(J+1) , MA%MP(J+2) , KIND(0.0D0) )
             CY(J/2) = CMPLX( MB%MP(J+1) , MB%MP(J+2) , KIND(0.0D0) )
          ENDDO
          IF (MOD(NDIG,2) == 1) THEN
              CX(NDIG/2+1) = MA%MP(NDIG+2)
              CY(NDIG/2+1) = MB%MP(NDIG+2)
          ENDIF
      ELSE
          DO J = NDIG/2, N
             CX(J) = 0
             CY(J) = 0
          ENDDO
          D = MBASE/BASE
          C = MBASE*D
          TMA = MA%MP(3)*MBASE + MA%MP(4)
          NUMAR = TMA/C
          TMA = TMA - C*NUMAR
          TMB = MB%MP(3)*MBASE + MB%MP(4)
          NUMBR = TMB/C
          TMB = TMB - C*NUMBR
          K = 0
          DO J = 3, NDIG
             DO
                IF (C >= BASE) THEN
                    C = C/BASE
                ELSE
                    TMA = TMA*MBASE + MA%MP(J+2)
                    TMB = TMB*MBASE + MB%MP(J+2)
                    C = C*MBASE
                    EXIT
                ENDIF
                NUMAI = TMA/C
                TMA = TMA - C*NUMAI
                NUMBI = TMB/C
                TMB = TMB - C*NUMBI
                IF (NUMAR < 0) THEN
                    NUMAR = NUMAI
                    NUMBR = NUMBI
                ELSE
                    K = K + 1
                    CX(K) = CMPLX( DBLE(NUMAR) , DBLE(NUMAI) , KIND(0.0D0) )
                    CY(K) = CMPLX( DBLE(NUMBR) , DBLE(NUMBI) , KIND(0.0D0) )
                    NUMAR = -1
                ENDIF
             ENDDO
          ENDDO
          DO
             IF (C >= BASE) THEN
                 C = C/BASE
             ELSE
                 NUMAI = TMA*(BASE/C)
                 NUMBI = TMB*(BASE/C)
                 IF (NUMAR < 0) THEN
                     K = K + 1
                     CX(K) = CMPLX( DBLE(NUMAI) , 0.0D0 , KIND(0.0D0) )
                     CY(K) = CMPLX( DBLE(NUMBI) , 0.0D0 , KIND(0.0D0) )
                 ELSE
                     K = K + 1
                     CX(K) = CMPLX( DBLE(NUMAR) , DBLE(NUMAI) , KIND(0.0D0) )
                     CY(K) = CMPLX( DBLE(NUMBR) , DBLE(NUMBI) , KIND(0.0D0) )
                 ENDIF
                 EXIT
             ENDIF
             NUMAI = TMA/C
             TMA = TMA - C*NUMAI
             NUMBI = TMB/C
             TMB = TMB - C*NUMBI
             IF (NUMAR < 0) THEN
                 NUMAR = NUMAI
                 NUMBR = NUMBI
             ELSE
                 K = K + 1
                 CX(K) = CMPLX( DBLE(NUMAR) , DBLE(NUMAI) , KIND(0.0D0) )
                 CY(K) = CMPLX( DBLE(NUMBR) , DBLE(NUMBI) , KIND(0.0D0) )
                 NUMAR = -1
             ENDIF
          ENDDO
      ENDIF

!             Transform.

      CALL FMFFT(CX,N,ROOTS_OF_UNITY,CZ)
      IF (KSQR /= 1) THEN
          CALL FMFFT(CY,N,ROOTS_OF_UNITY,CZ)
      ENDIF

!             Unpack the two transforms.

      THETA = ACOS(-1.0D0)/N
      W0 = CMPLX( COS(THETA) , SIN(THETA) , KIND(0.0D0) )
      W = W0
      ST = CMPLX( -2*SIN(THETA/2)**2 , SIN(THETA) , KIND(0.0D0) )
      CI = CMPLX( 0.0D0 , 1.0D0 , KIND(0.0D0) )
      DO J = 2, N/2
         H1 = 0.5D0*(CX(J)+CONJG(CX(N+2-J)))
         H2 = -0.5D0*CI*(CX(J)-CONJG(CX(N+2-J)))
         CX(J) = H1 + W*H2
         CX(N+2-J) = CONJG(H1 - W*H2)
         IF (KSQR /= 1) THEN
             H1 = 0.5D0*(CY(J)+CONJG(CY(N+2-J)))
             H2 = -0.5D0*CI*(CY(J)-CONJG(CY(N+2-J)))
             CY(J) = H1 + W*H2
             CY(N+2-J) = CONJG(H1 - W*H2)
         ENDIF
         W = W + ST*W
      ENDDO
      CX(1) = CMPLX( REAL(CX(1))+AIMAG(CX(1)) , REAL(CX(1))-AIMAG(CX(1)) , KIND(0.0D0) )
      IF (KSQR /= 1) THEN
          CY(1) = CMPLX( REAL(CY(1))+AIMAG(CY(1)) , REAL(CY(1))-AIMAG(CY(1)) , KIND(0.0D0) )
      ENDIF

!             Multiply.

      IF (KSQR /= 1) THEN
          CZ(1) = CMPLX( REAL(CX(1))*REAL(CY(1)) , AIMAG(CX(1))*AIMAG(CY(1)) , KIND(0.0D0) )
          DO J = 2, N
             CZ(J) = CX(J)*CY(J)
          ENDDO
      ELSE
          CZ(1) = CMPLX( REAL(CX(1))*REAL(CX(1)) , AIMAG(CX(1))*AIMAG(CX(1)) , KIND(0.0D0) )
          DO J = 2, N
             CZ(J) = CX(J)*CX(J)
          ENDDO
      ENDIF

!             Pack the product for input to the final FFT.

      THETA = -ACOS(-1.0D0)/N
      W0 = CMPLX( COS(THETA) , SIN(THETA) , KIND(0.0D0) )
      W = W0
      ST = CMPLX( -2*SIN(THETA/2)**2 , SIN(THETA) , KIND(0.0D0) )
      CI = CMPLX( 0.0D0 , 1.0D0 , KIND(0.0D0) )
      DO J = 2, N/2
         H1 = 0.5D0*(CZ(J)+CONJG(CZ(N+2-J)))
         H2 = 0.5D0*CI*(CZ(J)-CONJG(CZ(N+2-J)))
         CZ(J) = H1 + W*H2
         CZ(N+2-J) = CONJG(H1 - W*H2)
         W = W + ST*W
      ENDDO
      CZ(1) = CMPLX( REAL(CZ(1))+AIMAG(CZ(1)) , REAL(CZ(1))-AIMAG(CZ(1)) , KIND(0.0D0) )*0.5D0

!             Transform the product to get the convolution of the original inputs.

      CALL FMFFT(CZ,N,ROOTS_OF_UNITY,CY)
      T = 1.0D0/N
      DO J = 1, N
         CZ(J) = CZ(J)*T
      ENDDO

!             Normalize the digits.

!             The CZ array holds the result in scrambled order, with unnormalized digits.
!             The leading two digits of the convolution are in CZ(1), then the next two
!             are in CZ(N), then CZ(N-1), ..., CZ(2).
!             The imaginary part of CZ(2) is zero (up to roundoff), for a total of 2*N-1
!             digits in the convolution.
!             These digits can be as big as N*BASE^2 here.
!
!             First normalize and unscramble.

      IF (BASE == MBASE) THEN
          D = AINT( 0.5D0+REAL(CZ(2)) , KIND(0.0D0) )
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = N2
          IF (KA <= SIZE(MWA%MP)-3) THEN
              MWA%MP(KA+2) = T2
          ENDIF
          DO J = 3, N
             D = AINT( 0.5D0+AIMAG(CZ(J)) , KIND(0.0D0) ) + T
             T = AINT( D/BASE, KIND(0.0D0) )
             T2 = D - T*BASE
             KA = KA - 1
             IF (KA <= SIZE(MWA%MP)-3) THEN
                 MWA%MP(KA+2) = T2
             ENDIF
             D = AINT( 0.5D0+REAL(CZ(J)) , KIND(0.0D0) ) + T
             T = AINT( D/BASE, KIND(0.0D0) )
             T2 = D - T*BASE
             KA = KA - 1
             IF (KA <= SIZE(MWA%MP)-3) THEN
                 MWA%MP(KA+2) = T2
             ENDIF
          ENDDO
          D = AINT( 0.5D0+AIMAG(CZ(1)) , KIND(0.0D0) ) + T
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = KA - 1
          MWA%MP(KA+2) = T2
          D = AINT( 0.5D0+REAL(CZ(1)) , KIND(0.0D0) ) + T
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = KA - 1
          MWA%MP(KA+2) = T2
          KA = KA - 1
          MWA%MP(KA+2) = T
      ELSE
          CI = CMPLX( 0.0D0 , 1.0D0 , KIND(0.0D0) )
          D = AINT( 0.5D0+REAL(CZ(2)) , KIND(0.0D0) )
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = N
          CX(KA) = CI*T2
          DO J = 3, N
             D = AINT( 0.5D0+AIMAG(CZ(J)) , KIND(0.0D0) ) + T
             T = AINT( D/BASE, KIND(0.0D0) )
             T2 = D - T*BASE
             CX(KA) = T2 + CX(KA)
             D = AINT( 0.5D0+REAL(CZ(J)) , KIND(0.0D0) ) + T
             T = AINT( D/BASE, KIND(0.0D0) )
             T2 = D - T*BASE
             KA = KA - 1
             CX(KA) = CI*T2
          ENDDO
          D = AINT( 0.5D0+AIMAG(CZ(1)) , KIND(0.0D0) ) + T
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          CX(KA) = T2 + CX(KA)
          D = AINT( 0.5D0+REAL(CZ(1)) , KIND(0.0D0) ) + T
          T = AINT( D/BASE, KIND(0.0D0) )
          T2 = D - T*BASE
          KA = KA - 1
          CX(KA) = T + CI*T2
          TMA = 0
          C = 1
          K = 0
          DO J = 1, N2
             IF (MOD(J,2) == 1) THEN
                 NUM = REAL(CX((J+1)/2))
             ELSE
                 NUM = AIMAG(CX((J+1)/2))
             ENDIF
             TMA = TMA*BASE + NUM
             C = C*BASE
             IF (C < MBASE) CYCLE
             C = C/MBASE
             NUM = TMA/C
             TMA = TMA - C*NUM
             K = K + 1
             IF (K <= SIZE(MWA%MP)-3 .AND. K <= 2*NDIG+2) THEN
                 MWA%MP(K+2) = NUM
             ELSE
                 EXIT
             ENDIF
          ENDDO
          K = K + 1
          IF (K <= SIZE(MWA%MP)-3 .AND. K <= 2*NDIG+2) MWA%MP(K+2) = TMA*(MBASE/C)
      ENDIF

      DEALLOCATE(CX,CY,CZ,ROOTS_OF_UNITY)
      RETURN
      END SUBROUTINE FMMPYFFT2

      SUBROUTINE FMMPYFFTI(MA,IVAL,MB)

!  MB = MA * IVAL

!  Multiply FM number MA by one word integer IVAL.

!  This routine is used during fft-based multiplication.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MAS,MCARRY,MKT,MLR,MVAL
      INTEGER :: J,JRSSAV,KA,KB,KC,KSHIFT,N1,NGUARD
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      NCALL = NCALL + 1
      KFLAG = 0
      N1 = NDIG + 1

!             Check for special cases.

      IF (MA%MP(3) == 0) THEN
          CALL FMEQ(MA,MB)
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (ABS(MA%MP(2)) < MEXPOV .AND. ABS(IVAL) > 1) GO TO 110

      IF (MA%MP(2) == MUNKNO) THEN
          CALL FMIM(0,MB)
          MB%MP(2) = MUNKNO
          MB%MP(3) = 1
          KFLAG = -4
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (IVAL == 0) THEN
          CALL FMIM(0,MB)
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (ABS(IVAL) == 1) THEN
          DO J = 1, NDIG+2
             MB%MP(J) = MA%MP(J)
          ENDDO
          IF (MA%MP(2) == MEXPOV) KFLAG = -5
          IF (MA%MP(2) == MEXPUN) KFLAG = -6
          MB%MP(1) = MA%MP(1)*IVAL
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPOV) THEN
          MAS = MA%MP(1)
          KFLAG = -5
          CALL FMIM(0,MB)
          MB%MP(2) = MEXPOV
          MB%MP(3) = 1
          IF ((MAS < 0 .AND. IVAL > 0) .OR. (MAS > 0 .AND. IVAL < 0)) MB%MP(1) = -1
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPUN) THEN
          KFLAG = -4
          CALL FMIM(0,MB)
          MB%MP(2) = MUNKNO
          MB%MP(3) = 1
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

!             Work with positive numbers.

  110 MAS = MA%MP(1)
      MVAL = ABS(IVAL)

!             To leave room for the normalization, shift the product to the right KSHIFT
!             places in MWA.

      KSHIFT = INT((LOG(DBLE(MA%MP(3)+1)*DBLE(MVAL)))/DLOGMB)
      MWA%MP(2) = MA%MP(2) + KSHIFT
      KA = 2 + KSHIFT
      KB = N1 + KSHIFT
      KC = NDIG + 5
      DO J = KB, KC
         MWA%MP(J+1) = 0
      ENDDO

      MCARRY = 0

!             This is the main multiplication loop.

      DO J = KB, KA, -1
         MKT = MA%MP(J-KSHIFT+1)*MVAL + MCARRY
         MCARRY = INT (MKT/MBASE)
         MWA%MP(J+1) = MKT - MCARRY*MBASE
      ENDDO

!             Resolve the final carry.

      DO J = KA-1, 2, -1
         MKT = INT (MCARRY/MBASE)
         MWA%MP(J+1) = MCARRY - MKT*MBASE
         MCARRY = MKT
      ENDDO

!             Now the first significant digit in the product is in
!             MWA%MP(3) or MWA%MP(4).
!             Round the result and move it to MB.

      IF ((MAS > 0 .AND. IVAL > 0) .OR. (MAS < 0 .AND. IVAL < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (MWA%MP(3) == 0) THEN
          MLR = 2*MWA%MP(NDIG+4) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              NGUARD = KSHIFT - 1
              CALL FMRND(MWA,NDIG,NGUARD,1)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+2) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+2) = MWA%MP(N1+2) + 1
                      MWA%MP(N1+3) = 0
                  ENDIF
              ELSE
                  NGUARD = KSHIFT - 1
                  CALL FMRND(MWA,NDIG,NGUARD,1)
              ENDIF
          ENDIF
      ELSE
          MLR = 2*MWA%MP(NDIG+3) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,KSHIFT,0)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                      MWA%MP(N1+2) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,KSHIFT,0)
              ENDIF
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MB)

!             Put the sign on the result.

      MB%MP(1) = JRSIGN
      NCALL = NCALL - 1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMMPYFFTI

      SUBROUTINE FMMPYFFT_NORM(MA)

!  Normalize a number formed by the x1,x2 splitting done in FMMPYFFT.

      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTEGER :: J,KZ

      KZ = 0
      DO J = 1, NDIG
         IF (MA%MP(J+2) == 0) THEN
             KZ = J
         ELSE
             EXIT
         ENDIF
      ENDDO
      IF (KZ == 0) RETURN
      IF (KZ == NDIG) THEN
          CALL FMIM(0,MA)
          RETURN
      ENDIF
      DO J = 1, NDIG-KZ
         MA%MP(J+2) = MA%MP(J+KZ+2)
      ENDDO
      DO J = NDIG-KZ+1, NDIG
         MA%MP(J+2) = 0
      ENDDO
      MA%MP(2) = MA%MP(2) - KZ

      RETURN
      END SUBROUTINE FMMPYFFT_NORM

      SUBROUTINE FMMPYI(MA,IVAL,MB)

!  MB = MA * IVAL

!  Multiply FM number MA by one word integer IVAL.

!  This routine is faster than FMMPY when IVAL*MBASE is a one word integer.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MAS,MCARRY,MKT,MLR,MVAL
      INTEGER :: J,JRSSAV,KA,KB,KC,KSHIFT,N1,NGUARD,NMVAL,NV2
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMMPYI'
          CALL FMNTR(2,MA,MA,1,1)
          CALL FMNTRI(2,IVAL,0)
      ENDIF
      KFLAG = 0
      N1 = NDIG + 1

!             Check for special cases.

      IF (MA%MP(3) == 0) THEN
          CALL FMEQ(MA,MB)
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MB,MB,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (ABS(MA%MP(2)) < MEXPOV .AND. ABS(IVAL) > 1) GO TO 110

      IF (MA%MP(2) == MUNKNO) THEN
          CALL FMST2M('UNKNOWN',MB)
          KFLAG = -4
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MB,MB,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (IVAL == 0) THEN
          CALL FMIM(0,MB)
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MB,MB,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (ABS(IVAL) == 1) THEN
          DO J = 1, NDIG+2
             MB%MP(J) = MA%MP(J)
          ENDDO
          IF (MA%MP(2) == MEXPOV) KFLAG = -5
          IF (MA%MP(2) == MEXPUN) KFLAG = -6
          MB%MP(1) = MA%MP(1)*IVAL
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MB,MB,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPOV) THEN
          MAS = MA%MP(1)
          KFLAG = -5
          CALL FMST2M('OVERFLOW',MB)
          IF ((MAS < 0 .AND. IVAL > 0) .OR. (MAS > 0 .AND. IVAL < 0)) MB%MP(1) = -1
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MB,MB,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPUN) THEN
          NAMEST(NCALL) = 'FMMPYI'
          KFLAG = -4
          CALL FMWARN
          CALL FMST2M('UNKNOWN',MB)
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MB,MB,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

!             Work with positive numbers.

  110 MAS = MA%MP(1)
      MVAL = ABS(IVAL)
      NMVAL = INT(MVAL)
      NV2 = NMVAL - 1

!             To leave room for the normalization, shift the product to the right KSHIFT
!             places in MWA.

      KSHIFT = INT((LOG(DBLE(MA%MP(3)+1)*DBLE(MVAL)))/DLOGMB)

!             If IVAL is too big use FMMPY.

      IF (KSHIFT > NDIG .OR. MVAL > MAXINT/MBASE .OR.  &
          NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL FMIM(IVAL,MXY(1))
          CALL FMMPY2(MA,MXY(1),MB)
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MB,MB,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      MWA%MP(2) = MA%MP(2) + KSHIFT
      KA = 2 + KSHIFT
      KB = N1 + KSHIFT
      KC = NDIG + 5
      DO J = KB, KC
         MWA%MP(J+1) = 0
      ENDDO

      MCARRY = 0

!             This is the main multiplication loop.

      DO J = KB, KA, -1
         MKT = MA%MP(J-KSHIFT+1)*MVAL + MCARRY
         MCARRY = INT (MKT/MBASE)
         MWA%MP(J+1) = MKT - MCARRY*MBASE
      ENDDO

!             Resolve the final carry.

      DO J = KA-1, 2, -1
         MKT = INT (MCARRY/MBASE)
         MWA%MP(J+1) = MCARRY - MKT*MBASE
         MCARRY = MKT
      ENDDO

!             Now the first significant digit in the product is in
!             MWA%MP(3) or MWA%MP(4).
!             Round the result and move it to MB.

      IF ((MAS > 0 .AND. IVAL > 0) .OR. (MAS < 0 .AND. IVAL < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (MWA%MP(3) == 0) THEN
          MLR = 2*MWA%MP(NDIG+4) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              NGUARD = KSHIFT - 1
              CALL FMRND(MWA,NDIG,NGUARD,1)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+2) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+2) = MWA%MP(N1+2) + 1
                      MWA%MP(N1+3) = 0
                  ENDIF
              ELSE
                  NGUARD = KSHIFT - 1
                  CALL FMRND(MWA,NDIG,NGUARD,1)
              ENDIF
          ENDIF
      ELSE
          MLR = 2*MWA%MP(NDIG+3) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,KSHIFT,0)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                      MWA%MP(N1+2) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,KSHIFT,0)
              ENDIF
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MB)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMMPYI'
          CALL FMWARN
      ENDIF

!             Put the sign on the result.

      MB%MP(1) = JRSIGN
      IF (NTRACE /= 0) THEN
          CALL FMNTR(1,MB,MB,1,1)
      ENDIF
      NCALL = NCALL - 1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMMPYI

      SUBROUTINE FMMPYI_R1(MA,IVAL)

!  MA = MA * IVAL

!  Multiply FM number MA by one word integer IVAL.

!  This routine is faster than FMMPY when IVAL*MBASE is a one word integer.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      REAL (KIND(1.0D0)) :: MAS,MCARRY,MKT,MLR,MVAL
      INTEGER :: J,JRSSAV,KA,KB,KC,KSHIFT,N1,NGUARD,NMVAL,NV2
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      JRSSAV = JRSIGN
      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMMPYI_R1'
          CALL FMNTR(2,MA,MA,1,1)
          CALL FMNTRI(2,IVAL,0)
      ENDIF
      KFLAG = 0
      N1 = NDIG + 1

!             Check for special cases.

      IF (MA%MP(3) == 0) THEN
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MA,MA,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (ABS(MA%MP(2)) < MEXPOV .AND. ABS(IVAL) > 1) GO TO 110

      IF (MA%MP(2) == MUNKNO) THEN
          CALL FMST2M('UNKNOWN',MA)
          KFLAG = -4
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MA,MA,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (IVAL == 0) THEN
          CALL FMIM(0,MA)
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MA,MA,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (ABS(IVAL) == 1) THEN
          IF (MA%MP(2) == MEXPOV) KFLAG = -5
          IF (MA%MP(2) == MEXPUN) KFLAG = -6
          MA%MP(1) = MA%MP(1)*IVAL
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MA,MA,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPOV) THEN
          MAS = MA%MP(1)
          KFLAG = -5
          CALL FMST2M('OVERFLOW',MA)
          IF ((MAS < 0 .AND. IVAL > 0) .OR. (MAS > 0 .AND. IVAL < 0)) MA%MP(1) = -1
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MA,MA,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      IF (MA%MP(2) == MEXPUN) THEN
          NAMEST(NCALL) = 'FMMPYI_R1'
          KFLAG = -4
          CALL FMWARN
          CALL FMST2M('UNKNOWN',MA)
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MA,MA,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

!             Work with positive numbers.

  110 MAS = MA%MP(1)
      MVAL = ABS(IVAL)
      NMVAL = INT(MVAL)
      NV2 = NMVAL - 1

!             To leave room for the normalization, shift the product to the right KSHIFT
!             places in MWA.

      KSHIFT = INT((LOG(DBLE(MA%MP(3)+1)*DBLE(MVAL)))/DLOGMB)

!             If IVAL is too big use FMMPY.

      IF (KSHIFT > NDIG .OR. MVAL > MAXINT/MBASE .OR.  &
          NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL FMIM(IVAL,MXY(1))
          CALL FMMPY2_R1(MA,MXY(1))
          IF (NTRACE /= 0) THEN
              CALL FMNTR(1,MA,MA,1,1)
          ENDIF
          NCALL = NCALL - 1
          JRSIGN = JRSSAV
          RETURN
      ENDIF

      MWA%MP(2) = MA%MP(2) + KSHIFT
      KA = 2 + KSHIFT
      KB = N1 + KSHIFT
      KC = NDIG + 5
      DO J = KB, KC
         MWA%MP(J+1) = 0
      ENDDO

      MCARRY = 0

!             This is the main multiplication loop.

      DO J = KB, KA, -1
         MKT = MA%MP(J-KSHIFT+1)*MVAL + MCARRY
         MCARRY = INT (MKT/MBASE)
         MWA%MP(J+1) = MKT - MCARRY*MBASE
      ENDDO

!             Resolve the final carry.

      DO J = KA-1, 2, -1
         MKT = INT (MCARRY/MBASE)
         MWA%MP(J+1) = MCARRY - MKT*MBASE
         MCARRY = MKT
      ENDDO

!             Now the first significant digit in the product is in
!             MWA%MP(3) or MWA%MP(4).
!             Round the result and move it to MA.

      IF ((MAS > 0 .AND. IVAL > 0) .OR. (MAS < 0 .AND. IVAL < 0)) THEN
          JRSIGN = 1
      ELSE
          JRSIGN = -1
      ENDIF
      IF (MWA%MP(3) == 0) THEN
          MLR = 2*MWA%MP(NDIG+4) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              NGUARD = KSHIFT - 1
              CALL FMRND(MWA,NDIG,NGUARD,1)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+2) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+2) = MWA%MP(N1+2) + 1
                      MWA%MP(N1+3) = 0
                  ENDIF
              ELSE
                  NGUARD = KSHIFT - 1
                  CALL FMRND(MWA,NDIG,NGUARD,1)
              ENDIF
          ENDIF
      ELSE
          MLR = 2*MWA%MP(NDIG+3) + 1
          IF (KROUND == -1 .OR. KROUND == 2) THEN
              CALL FMRND(MWA,NDIG,KSHIFT,0)
          ELSE IF (MLR >= MBASE) THEN
              IF (MLR-1 > MBASE .AND. MWA%MP(N1+1) < MBASE-1) THEN
                  IF (KROUND /= 0) THEN
                      MWA%MP(N1+1) = MWA%MP(N1+1) + 1
                      MWA%MP(N1+2) = 0
                  ENDIF
              ELSE
                  CALL FMRND(MWA,NDIG,KSHIFT,0)
              ENDIF
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MA)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMMPYI_R1'
          CALL FMWARN
      ENDIF

!             Put the sign on the result.

      MA%MP(1) = JRSIGN
      IF (NTRACE /= 0) THEN
          CALL FMNTR(1,MA,MA,1,1)
      ENDIF
      NCALL = NCALL - 1
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMMPYI_R1

      SUBROUTINE FMNINT(MA,MB)

!  MB = NINT(MA)  --  MB is returned as the nearest integer to MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MA2,MXSAVE
      INTEGER :: K,KOVUN,KRESLT,KWRNSV,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMENTR('FMNINT   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMNINT'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF

      KWRNSV = KWARN
      KWARN = 0
      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
      IF (NDSAVE > INT(MA%MP(2))) THEN
          MA2 = MA%MP(1)
          MXY(2)%MP(1) = 1
          CALL FMI2M(1,MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMADD(MXY(2),MXY(1),MXY(3))
          CALL FMINT(MXY(3),MXY(2))
          IF (MA2 < 0 .AND. MXY(2)%MP(2) /= MUNKNO .AND. MXY(2)%MP(3) /= 0)  &
              MXY(2)%MP(1) = -MXY(2)%MP(1)
      ENDIF
      KWARN = KWRNSV

!             Round the result and return.

      CALL FMEXIT(MXY(2),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMNINT

      SUBROUTINE FMNORM2(X,N,MB)

!  MB = sqrt( x(1)^2 + x(2)^2 + ... + x(n)^2 )

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: N
      TYPE(MULTI) :: X(N),MB
      INTEGER :: J,NDSAVE,MXSAVE
      INTENT (IN) :: X,N
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMNORM2  '
      NDSAVE = NDIG
      NDIG = NDIG + NGRD52
      MXSAVE = MXEXP
      MXEXP = MXEXP2

!             Check for special cases.

      DO J = 1, N
         IF (X(J)%MP(2) == MUNKNO) THEN
             CALL FMST2M('UNKNOWN',MXY(1))
             GO TO 110
         ENDIF
      ENDDO
      DO J = 1, N
         IF (X(J)%MP(2) == MEXPOV) THEN
             CALL FMST2M('OVERFLOW',MXY(1))
             GO TO 110
         ENDIF
      ENDDO

      CALL FMI2M(0,MXY(2))
      DO J = 1, N
         CALL FMEQU(X(J),MXY(1),NDSAVE,NDIG)
         CALL FMSQR_R1(MXY(1))
         CALL FMADD_R1(MXY(2),MXY(1))
      ENDDO
      CALL FMSQRT_R1(MXY(2))

  110 MXEXP = MXSAVE
      CALL FMEQU(MXY(2),MB,NDIG,NDSAVE)
      NDIG = NDSAVE
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMNORM2

      FUNCTION FMNTERMS(Y,C1,C2,JN,CONV)

!  Internal routine for estimating the number of terms needed in a series.

      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION :: A,B,C,FB,FC,TOL,Y,Z,FMNTERMS
      DOUBLE PRECISION, EXTERNAL :: FMDPLG
      INTEGER :: C1,C2,JN,CONV

      IF (CONV /= 1) GO TO 110

!             Convergent series.

      Z = Y*LOG(Y) - FMDPLG(Y+1)
      C = 1
      IF (C2 == 1) THEN
          C = (SQRT(4*Y + JN**2) - JN)/2
          Z = C*LOG(Y) - FMDPLG(C+1) - FMDPLG(C+JN+1)
      ELSE IF (C2 == -1) THEN
          Z = (Y*Y/2)*LOG(Y) + FMDPLG(Y*Y/4+1) - FMDPLG(Y*Y/2+1)
      ENDIF
      IF (ABS(Y) < 1) Z = 0
      TOL = Z - NDIG*DLOGMB
      A = Y/C1
      IF (C2 == 1) THEN
          A = C
      ENDIF
      IF (A < 1) A = 1
      B = 2*A
      IF (C2 == 0) THEN
          FB = C1*B*LOG(Y) - FMDPLG(C1*B+1)
      ELSE IF (C2 == 1) THEN
          FB = C1*B*LOG(Y) - FMDPLG(C1*B+1) - FMDPLG(C1*(B+JN)+1)
      ELSE IF (C2 == -1) THEN
          FB = C1*B*LOG(Y) + FMDPLG(B+1) - FMDPLG(C1*B+1)
      ENDIF
      DO WHILE (FB > TOL)
         A = B
         B = 2*B
         IF (C2 == 0) THEN
             FB = C1*B*LOG(Y) - FMDPLG(C1*B+1)
         ELSE IF (C2 == 1) THEN
             FB = C1*B*LOG(Y) - FMDPLG(C1*B+1) - FMDPLG(C1*(B+JN)+1)
         ELSE IF (C2 == -1) THEN
             FB = C1*B*LOG(Y) + FMDPLG(B+1) - FMDPLG(C1*B+1)
         ENDIF
      ENDDO
      DO WHILE (B-A > 1)
         C = (A+B)/2
         IF (C2 == 0) THEN
             FC = C1*C*LOG(Y) - FMDPLG(C1*C+1)
         ELSE IF (C2 == 1) THEN
             FC = C1*C*LOG(Y) - FMDPLG(C1*C+1) - FMDPLG(C1*(C+JN)+1)
         ELSE IF (C2 == -1) THEN
             FC = C1*C*LOG(Y) + FMDPLG(C+1) - FMDPLG(C1*C+1)
         ELSE
             FC = C1*C*LOG(Y) - FMDPLG(C1*C+1)
         ENDIF
         IF (FC < TOL) THEN
             B = C
         ELSE
             A = C
         ENDIF
      ENDDO
      FMNTERMS = (A+B)/2
      RETURN

!             Asymptotic series.

  110 TOL = -NDIG*DLOGMB
      A = 1
      B = Y/C1
      IF (C2 == 1) THEN
          B = Y/2
      ENDIF
      IF (C2 == 0) THEN
          FB = FMDPLG(C1*B+1) - C1*B*LOG(Y)
      ELSE IF (C2 == 1) THEN
          IF (-C1*B + JN >= 0) THEN
              FB = FMDPLG(C1*B+JN+1) - C1*B*LOG(Y) - FMDPLG(C1*B+1) - FMDPLG(-C1*B+JN+1)
          ELSE
              FB = FMDPLG(C1*B+JN+1) + FMDPLG(C1*B-JN+1) - C1*B*LOG(Y) - FMDPLG(C1*B+1)
          ENDIF
      ENDIF
      DO WHILE (B-A > 1)
         IF (ABS(FB)/1.0D10 > ABS(TOL) .AND. B/1.0D10 > A*1.1) THEN
             C = B/1.0D10
         ELSE IF (ABS(FB)/1.0D2 > ABS(TOL) .AND. B/1.0D2 > A*1.1) THEN
             C = B/1.0D2
         ELSE
             C = (A+B)/2
         ENDIF
         IF (C2 == 0) THEN
             FC = FMDPLG(C1*C+1) - C1*C*LOG(Y)
         ELSE IF (C2 == 1) THEN
             IF (-C1*C + JN >= 0) THEN
                 FC = FMDPLG(C1*C+JN+1) - C1*C*LOG(Y) - FMDPLG(C1*C+1) - FMDPLG(-C1*C+JN+1)
             ELSE
                 FC = FMDPLG(C1*C+JN+1) + FMDPLG(C1*C-JN+1) - C1*C*LOG(Y) - FMDPLG(C1*C+1)
             ENDIF
         ENDIF
         IF (FC < TOL) THEN
             B = C
         ELSE
             A = C
         ENDIF
      ENDDO
      FMNTERMS = (A+B)/2

      END FUNCTION FMNTERMS

      SUBROUTINE FMNTR(NTR,MA,MB,NARG,KNAM)

!  Print FM numbers in base 10 format using FMOUT for conversion.
!  This is used for trace output from the FM routines.

!  NTR =  1 if a result of an FM call is to be printed.
!      =  2 to print input argument(s) to an FM call.

!  MA  -  the FM number to be printed.

!  MB  -  an optional second FM number to be printed.

!  NARG - the number of arguments.  NARG = 1 if only MA is to be printed, and NARG = 2 if
!         both MA and MB are to be printed.

!  KNAM - positive if the routine name is to be printed.


!  NTRACE and LVLTRC (in module FMVALS) control trace printout.

!  NTRACE = 0        No printout except warnings and errors.

!  NTRACE = 1        The result of each call to one of the routines is printed in base 10,
!                    using FMOUT.

!  NTRACE = -1       The result of each call to one of the routines is printed in internal
!                    base MBASE format.

!  NTRACE = 2        The input arguments and result of each call to one of the routines is
!                    printed in base 10, using FMOUT.

!  NTRACE = -2       The input arguments and result of each call to one of the routines is
!                    printed in base MBASE format.

!  LVLTRC defines the call level to which the trace is done.  LVLTRC = 1 means only FM routines
!         called directly by the user are traced, LVLTRC = K prints traces for FM routines with
!         call levels up to and including level K.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: KNAM,NTR,NARG

      CHARACTER(9) :: NAME
      INTENT (IN) :: NTR,MA,MB,NARG,KNAM

      IF (NTRACE == 0) RETURN
      IF (NCALL > LVLTRC) RETURN
      IF (NTR == 2 .AND. ABS(NTRACE) == 1) RETURN

      IF (NTR == 2) THEN
          IF (KNAM > 0) THEN
              NAME = NAMEST(NCALL)
              IF (KROUND == 1) THEN
                  WRITE (KW,"(' Input to ',A)") TRIM(NAME)
              ELSE IF (KROUND == 2) THEN
                  WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward +infinity)'
              ELSE IF (KROUND == 0) THEN
                  WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward zero)'
              ELSE IF (KROUND == -1) THEN
                  WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward -infinity)'
              ELSE
                  WRITE (KW,"(' Input to ',A)") TRIM(NAME)
              ENDIF
          ENDIF
      ELSE
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,                                               &
                     "(' ',A,12X,'Call level =',I2,5X,'MBASE =',"  //  &
                     "I10,5X,'NDIG =',I10)"                            &
                    ) NAME,NCALL,INT(MBASE),NDIG
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'NDIG =',I10,4X,'KFLAG =',I3)"           &
                    ) NAME,NCALL,INT(MBASE),NDIG,KFLAG
          ENDIF
      ENDIF

!             Check for base MBASE internal format trace.

      IF (NTRACE < 0) THEN
          CALL FMNTRJ(MA,NDIG)
          IF (NARG == 2) CALL FMNTRJ(MB,NDIG)
      ENDIF

!             Check for base 10 trace using FMOUT.

      IF (NTRACE > 0) THEN
          CALL FMPRNT(MA)
          IF (NARG == 2) THEN
              CALL FMPRNT(MB)
          ENDIF
      ENDIF

      RETURN
      END SUBROUTINE FMNTR

      SUBROUTINE FMNTRI(NTR,N,KNAM)

!  Internal routine for trace output of integer variables.

!  NTR = 1 for output values
!        2 for input values

!  N     Integer to be printed.

!  KNAM  is positive if the routine name is to be printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NTR,N,KNAM

      CHARACTER(9) :: NAME
      INTENT (IN) :: NTR,N,KNAM

      IF (NTRACE == 0) RETURN
      IF (NCALL > LVLTRC) RETURN
      IF (NTR == 2 .AND. ABS(NTRACE) == 1) RETURN

      IF (NTR == 2 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KROUND == 1) THEN
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ELSE IF (KROUND == 2) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward +infinity)'
          ELSE IF (KROUND == 0) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward zero)'
          ELSE IF (KROUND == -1) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward -infinity)'
          ELSE
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ENDIF
      ENDIF
      IF (NTR == 1 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,                                               &
                     "(' ',A,12X,'Call level =',I2,5X,'MBASE =',"  //  &
                     "I10,5X,'NDIG =',I10)"                            &
                    ) NAME,NCALL,INT(MBASE),NDIG
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'NDIG =',I10,4X,'KFLAG =',I3)"           &
                    ) NAME,NCALL,INT(MBASE),NDIG,KFLAG
          ENDIF
      ENDIF

      WRITE (KW,"(1X,I20)") N

      RETURN
      END SUBROUTINE FMNTRI

      SUBROUTINE FMNTRJ(MA,ND)

!  Print trace output in internal base MBASE format.  The number to be printed is in MA.

!  ND is the number of base MBASE digits to be printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: ND

      CHARACTER(99) :: FORM
      CHARACTER(40), EXTERNAL :: FMFI
      CHARACTER(40) :: ST1,ST2
      INTEGER :: J,L,N
      INTENT (IN) :: MA,ND

      L = INT(LOG10(DBLE(MBASE-1))) + 2
      N = (KSWIDE-23)/L
      IF (N > 10) N = 5*(N/5)
      IF (ND <= N) THEN
          WRITE (FORM,"(' (1X,I19,I',I2,',',I3,'I',I2,') ')") L+2, N-1, L
      ELSE
          WRITE (FORM,"(' (1X,I19,I',I2,',',I3,'I',I2,'/"  //  &
                      "(22X,',I3,'I',I2,')) ')"                &
                ) L+2, N-1, L, N, L
      ENDIF

      ST1 = FMFI(INT(MA%MP(1)))
      ST2 = FMFI(INT(MA%MP(2)))
      WRITE (KW,"(A,A,A,A,A)") '            Sign = ',TRIM(ST1),  &
                 '   Exponent = ',TRIM(ST2),'   Digits:'
      WRITE (FORM,*) '(13X,', N, 'I', L, ')'
      WRITE (KW,FORM) (INT(MA%MP(J)),J=3,ND+2)

      RETURN
      END SUBROUTINE FMNTRJ

      SUBROUTINE FMNTRR(NTR,X,KNAM)

!  Internal routine for trace output of real variables.

!  NTR - 1 for output values
!        2 for input values

!  X   - Double precision value to be printed if NX == 1

!  KNAM - Positive if the routine name is to be printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NTR,KNAM
      DOUBLE PRECISION :: X

      CHARACTER(9) :: NAME
      INTENT (IN) :: NTR,X,KNAM

      IF (NTRACE == 0) RETURN
      IF (NCALL > LVLTRC) RETURN
      IF (NTR == 2 .AND. ABS(NTRACE) == 1) RETURN

      IF (NTR == 2 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KROUND == 1) THEN
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ELSE IF (KROUND == 2) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward +infinity)'
          ELSE IF (KROUND == 0) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward zero)'
          ELSE IF (KROUND == -1) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward -infinity)'
          ELSE
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ENDIF
      ENDIF
      IF (NTR == 1 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,                                               &
                     "(' ',A,12X,'Call level =',I2,5X,'MBASE =',"  //  &
                     "I10,5X,'NDIG =',I10)"                            &
                    ) NAME,NCALL,INT(MBASE),NDIG
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'NDIG =',I10,4X,'KFLAG =',I3)"           &
                    ) NAME,NCALL,INT(MBASE),NDIG,KFLAG
          ENDIF
      ENDIF

      WRITE (KW,"(1X,D30.20)") X

      RETURN
      END SUBROUTINE FMNTRR

      SUBROUTINE FMOUT(MA,LINE,LB)

!  Convert a floating multiple precision number to a character array for output.

!  MA   is an FM number to be converted to an A1 character array in base 10 format.
!  LINE is the character(1) array in which the result is returned.
!  LB   is the length of LINE.

! JFORM1 and JFORM2 (in module FMVALS) determine the format of LINE.

! JFORM1 = 0  normal setting  ( .314159M+6 )
!        = 1  ES  format      ( 3.14159M+5 )
!        = 2  F   format      ( 314159.000 )

! JFORM2 = number of significant digits to display (if JFORM1 = 0, 1)
!        = number of digits after the decimal point (if JFORM1 = 2)

!          If JFORM2 == 0 and JFORM1 /= 2 then a default number of digits is chosen.
!          The default is roughly the full precision of MA.

!          If JFORM2 == 0 and JFORM1 == 2 then the number is returned in integer format with no
!          decimal point.  Rounding is done as with other settings, so the value displayed is the
!          nearest integer to MA.

!  If JFORM1 == 2 and MA is too large or too small to display in the requested format, it is
!  converted using JFORM1=0, JFORM2=0.

!  LINE should be dimensioned at least LOG10(MBASE)*NDIG + 15 on a 32-bit machine to allow for up to
!  10 digit exponents.  Replace 15 by 20 if 48-bit integers are used, 25 for 64-bit integers, ....

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: LB
      CHARACTER :: LINE(LB)

      CHARACTER :: KCHAR
      REAL (KIND(1.0D0)) :: MBSAVE,MEXP,MEXP10,MKT,MS1,MS2,MSD2,MT10,MXSAVE
      INTEGER :: J,JDPT,JF1SAV,JF2SAV,K,K1,K2,KA,KB,KC,KDIGIT,KEXP,KMS2SD,KWRNSV,L,  &
                 NEW_MBASE,NEW_NDIG,ND,NDE,NDE2,NDSAVE,NPOWER,NSD1,NSD2,NVAL,NTRSAV, &
                 NWORD,NWORD1,NWORD2
      DOUBLE PRECISION :: X

      CHARACTER :: NUMB(10) = (/ '0','1','2','3','4','5','6','7','8','9' /)
      CHARACTER :: NUNKNO(12) = (/ ' ',' ',' ','U','N','K','N','O','W','N',' ',' ' /)
      CHARACTER :: NEXPOV(12) = (/ ' ',' ',' ','O','V','E','R','F','L','O','W',' ' /)
      CHARACTER :: NEXPUN(12) = (/ ' ',' ',' ','U','N','D','E','R','F','L','O','W' /)
      CHARACTER(9) :: NAMEST_SAVE(0:50)
      INTENT (IN) :: MA,LB
      INTENT (INOUT) :: LINE
      TYPE(MULTI) :: MXY(2)


!             To avoid recursion, FMOUT calls only internal arithmetic routines
!             (FMADD2, FMMPY2, ...), so no trace printout is done during a call to FMOUT.

      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMOUT'
      KWRNSV = KWARN
      KWARN = 0
      DO J = 1, LB
         LINE(J) = ' '
      ENDDO

!             Check for special cases.

      IF (MA%MP(2) == MUNKNO) THEN
          DO J = 1, 12
             LINE(J) = NUNKNO(J)
          ENDDO
          KWARN = KWRNSV
          NCALL = NCALL - 1
          RETURN
      ENDIF
      IF (MA%MP(2) == MEXPOV) THEN
          DO J = 1, 12
             LINE(J) = NEXPOV(J)
          ENDDO
          LINE(2) = '+'
          IF (MA%MP(1) < 0) LINE(2) = '-'
          KWARN = KWRNSV
          NCALL = NCALL - 1
          RETURN
      ENDIF
      IF (MA%MP(2) == MEXPUN) THEN
          DO J = 1, 12
             LINE(J) = NEXPUN(J)
          ENDDO
          LINE(2) = '+'
          IF (MA%MP(1) < 0) LINE(2) = '-'
          KWARN = KWRNSV
          NCALL = NCALL - 1
          RETURN
      ENDIF
      IF (MA%MP(3) == 0 .AND. JFORM1 == 2 .AND. JFORM2 == 0) THEN
          LINE(2) = '0'
          KWARN = KWRNSV
          NCALL = NCALL - 1
          RETURN
      ENDIF

      JF1SAV = JFORM1
      JF2SAV = JFORM2
      MBSAVE = MBASE
      NDSAVE = NDIG
      MXSAVE = MXEXP

!             ND is the number of base 10 digits required.

  110 ND = JFORM2
      IF (JFORM1 == 2 .AND. MA%MP(2) > 0) THEN
          ND = JFORM2 + INT(REAL(MA%MP(2))*LOG10(REAL(MBASE))) + 1
          IF (ND <= 1 .AND. JFORM2 == 0) ND = 10
      ENDIF
      IF (ND <= 1) THEN
          K = INT(REAL(NDIG)*LOG10(REAL(MBASE)))
          ND = MAX(K,JFORM2)
      ENDIF
      IF (JFORM2 <= 0 .AND. JFORM1 <= 1) ND = INT(1.1 + REAL(NDIG-1)*LOG10(REAL(MBASE)))
      IF (ND < 2) ND = 2

      IF (LB < ND+6) THEN
          IF (JFORM1 == 2) THEN
              JFORM1 = 0
              JFORM2 = 0
              GO TO 110
          ENDIF
          GO TO 170
      ENDIF

!             Convert to the base that is the largest power of 10 less than MXBASE and build the
!             output number.

      NPOWER = INT(LOG10(REAL(MXBASE)/4))
      MXEXP = MXEXP2
      MBASE = 10**NPOWER
      IF (MBLOGS /= MBASE) CALL FMCONS
      NDIG = ND/NPOWER + 3
      K = NDSAVE*LOG(DBLE(MBSAVE))/LOG(DBLE(MBASE))
      IF (NDIG + 3000 < K) THEN
          NDIG = ND/NPOWER + 3000
      ELSE IF (NDIG < K) THEN
          NDIG = K
      ENDIF
      IF (NDIG < 2) NDIG = 2

      IF (MA%MP(3) == 0) THEN
          CALL FMIM(0,MXY(2))
          GO TO 120
      ENDIF

!             Check to see if MA is already in a base that is a power of ten.
!             If so, the conversion can be skipped.

      K = NPOWER
      DO J = 1, K
         MBASE = 10**J
         IF (MBASE == MBSAVE) THEN
             IF (MBLOGS /= MBASE) CALL FMCONS
             NPOWER = J
             NDIG = MAX(NDSAVE,ND/NPOWER+3)
             IF (NDIG < 2) NDIG = 2
             IF (NDIG > NDSAVE) THEN
                 CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
             ELSE
                 CALL FMIM(0,MXY(2))
                 DO K1 = 1, MIN(NDIG,SIZE(MA%MP)-3)
                    MXY(2)%MP(K1+2) = MA%MP(K1+2)
                 ENDDO
                 MXY(2)%MP(1) = MA%MP(1)
                 MXY(2)%MP(2) = MA%MP(2)
             ENDIF
             MXY(2)%MP(1) = 1
             GO TO 120
         ENDIF
      ENDDO

      NEW_MBASE = MBASE
      NEW_NDIG = NDIG
      MBASE = MBSAVE
      NDIG = MIN(NDSAVE,NINT(NEW_NDIG*LOG(DBLE(NEW_MBASE))/LOG(DBLE(MBASE))))
      IF (MBLOGS /= MBASE) CALL FMCONS
      NTRSAV = NTRACE
      NTRACE = 0
      J = NCALL
      NAMEST_SAVE(0:NCALL) = NAMEST(0:NCALL)
      NCALL = 0
      CALL FMCHANGEBASE(MA,MXY(2),NEW_MBASE,NEW_NDIG)
      NCALL = J
      NAMEST(0:NCALL) = NAMEST_SAVE(0:NCALL)
      NTRACE = NTRSAV
      MBASE = NEW_MBASE
      NDIG = NEW_NDIG
      IF (MBLOGS /= MBASE) CALL FMCONS

!             Now MXY(2) is the value of MA converted to a power of ten base.

!             Convert it to a character string base 10 for output.

!             MEXP10 is the base 10 exponent.
!             KMS2SD is the number of base 10 significant digits in the first digit of MXY(2).

  120 MS1 = MXY(2)%MP(2)
  130 MEXP10 = NPOWER*MXY(2)%MP(2)
      KMS2SD = NPOWER
      K = INT(MBASE)
      DO J = 1, NPOWER
         K = K/10
         IF (MXY(2)%MP(3) < K .AND. MXY(2)%MP(3) /= 0) THEN
             MEXP10 = MEXP10 - 1
             KMS2SD = KMS2SD - 1
         ENDIF
      ENDDO

!             For printing using JFORM1 = 1, reduce the exponent to account for the fact that the
!             decimal point and first significant digit will later be swapped.

      IF (JFORM1 == 1 .AND. MXY(2)%MP(3) /= 0) MEXP10 = MEXP10 - 1

!             Find the position in the unpacked number for rounding.
!             NWORD is the word in which rounding is done, or zero if no rounding is necessary.
!                   NWORD is set to -1 if JFORM1 is 2 (F format) but no significant digits would
!                   be printed.  This case defaults to JFORM1 = 0.
!             NVAL gives the position within that word where rounding occurs.
!             NSD1 is the maximum number of base 10 S.D.'s in NWORD digits of base 10**NPOWER.
!             NSD2 is the number of base 10 S.D.'s needed to get ND base 10 digits after
!                  the decimal.

      NSD2 = ND
      IF (JFORM1 == 2) THEN
          MSD2 = JFORM2 + MEXP10
          IF (MSD2 > ND) THEN
              NSD2 = ND
          ELSE
              NSD2 = INT(MSD2)
          ENDIF
          NWORD = (NSD2-KMS2SD-1+NPOWER)/NPOWER + 2
          IF (NWORD < 2) NWORD = -1
          IF (NWORD > NDIG) NWORD = 0
          IF (NWORD >= 2 .AND. NSD2 <= 0) NWORD = -1
      ELSE
          NWORD = (ND-KMS2SD-1+NPOWER)/NPOWER + 2
      ENDIF
      NSD1 = KMS2SD + NPOWER*(NWORD-2)
      IF (NWORD < 2) THEN
          NVAL = 0
      ELSE
          NVAL = 10**(NSD1-NSD2)
      ENDIF

!             Now do the base 10 rounding.

      IF (NWORD >= 2) THEN
          X = 0
          IF (NVAL > 1) X = MOD(INT(MXY(2)%MP(NWORD+1)),NVAL)
          IF (NWORD < NDIG+1) THEN
              X = X + DBLE(MXY(2)%MP(NWORD+2))/DBLE(MBASE)
          ENDIF
          X = X/NVAL
          IF (KROUND == 1 .AND. X < 0.5) GO TO 150
          IF (KROUND == 1 .AND. X == 0.5) THEN
              DO J = NWORD+1, NDIG
                 IF (MXY(2)%MP(J+2) /= 0) GO TO 140
              ENDDO
              J = MOD(INT(MXY(2)%MP(NWORD+1))/NVAL,10)
              IF (MOD(J,2) /= 0) GO TO 140
              GO TO 150
          ENDIF
          IF (KROUND == 0) GO TO 150
          IF (KROUND == -1 .AND. MA%MP(1) > 0) GO TO 150
          IF (KROUND ==  2 .AND. MA%MP(1) < 0) GO TO 150
          IF (KROUND == -1 .AND. X == 0.0) THEN
              DO J = NWORD+1, NDIG
                 IF (MXY(2)%MP(J+2) /= 0) GO TO 140
              ENDDO
              GO TO 150
          ENDIF
          IF (KROUND ==  2 .AND. X == 0.0) THEN
              DO J = NWORD+1, NDIG
                 IF (MXY(2)%MP(J+2) /= 0) GO TO 140
              ENDDO
              GO TO 150
          ENDIF
  140     MS2 = MXY(2)%MP(3)
          MXY(2)%MP(NWORD+1) = INT(MXY(2)%MP(NWORD+1)/NVAL)*NVAL
          IF (NWORD+3 <= SIZE(MXY(2)%MP)) THEN
              MXY(2)%MP(NWORD+2) = 0
              IF (NWORD+4 <= SIZE(MXY(2)%MP)) THEN
                  MXY(2)%MP(NWORD+3) = 0
              ENDIF
              IF (KROUND /= 1) THEN
                  DO J = NWORD, SIZE(MXY(2)%MP)-3
                     MXY(2)%MP(J+2) = 0
                  ENDDO
              ENDIF
          ENDIF
          MXY(2)%MP(NWORD+1) = MXY(2)%MP(NWORD+1) + NVAL
          IF (MXY(2)%MP(NWORD+1) >= MBASE) THEN
              NWORD1 = NWORD - 1
              NWORD2 = NWORD - 2
              IF (NWORD > 2) THEN
                  CALL FMEQU_R1(MXY(2),NWORD1,NWORD2)
              ELSE
                  MXY(2)%MP(2) = MXY(2)%MP(2) + 1
                  MXY(2)%MP(3) = INT(MXY(2)%MP(3)/MBASE)
                  MXY(2)%MP(4) = 0
              ENDIF
          ENDIF
          IF (MXY(2)%MP(2) /= MS1 .OR. MXY(2)%MP(3) /= MS2) GO TO 130
      ENDIF

!             Build the base 10 character string.

  150 IF (MA%MP(1) < 0) LINE(1) = '-'
      LINE(2) = '.'
      K = 10**KMS2SD
      L = 2
      IF (NWORD == -1) NSD2 = ND
      DO J = 1, NSD2
         K = K/10
         IF (K == 0) THEN
             K = INT(MBASE)/10
             L = L + 1
         ENDIF
         KDIGIT = INT(MXY(2)%MP(L+1))/K
         MXY(2)%MP(L+1) = MOD(INT(MXY(2)%MP(L+1)),K)
         LINE(J+2) = NUMB(KDIGIT+1)
      ENDDO

      KA = NSD2 + 3
      KB = ND + 2
      IF (KB >= KA) THEN
          DO J = KA, KB
             LINE(J) = NUMB(1)
          ENDDO
      ENDIF

      LINE(ND+3) = CMCHAR
      LINE(ND+4) = '+'
      IF (MEXP10 < 0) LINE(ND+4) = '-'
      IF (MA%MP(3) == 0) LINE(ND+4) = ' '

!             Build the digits of the base 10 exponent backwards, then reverse them.

      NDE = 1
      MEXP = ABS(MEXP10)
      MT10 = 10
      DO J = 1, LB
         MKT = AINT (MEXP/MT10)
         KDIGIT = INT(MEXP-MKT*MT10)
         LINE(ND+4+J) = NUMB(KDIGIT+1)
         MEXP = MKT
         IF (MEXP == 0) EXIT

         IF (ND+5+J > LB) THEN
             DO K = 1, LB
                LINE(K) = '*'
             ENDDO
             GO TO 160
         ENDIF

         NDE = NDE + 1
      ENDDO

      NDE2 = NDE/2
      IF (NDE2 < 1) GO TO 160
      K1 = ND + 4
      K2 = ND + 5 + NDE
      DO J = 1, NDE2
         K1 = K1 + 1
         K2 = K2 - 1
         KCHAR = LINE(K1)
         LINE(K1) = LINE(K2)
         LINE(K2) = KCHAR
      ENDDO

!             If JFORM1 is 1 put the first digit left of the decimal.

  160 IF (JFORM1 == 1) THEN
          KCHAR = LINE(2)
          LINE(2) = LINE(3)
          LINE(3) = KCHAR
      ENDIF

!             If JFORM1 is 2 put the number into fixed format.

      IF (JFORM1 == 2 .AND. JFORM2 >= 0) THEN
          IF (MEXP10 <= -JFORM2 .OR. MEXP10+2 > LB .OR. LINE(1) == '*') THEN
              JFORM1 = 0
              JFORM2 = 0
              MBASE = MBSAVE
              IF (MBLOGS /= MBASE) CALL FMCONS
              NDIG = NDSAVE
              MXEXP = MXSAVE
              DO J = 1, LB
                 LINE(J) = ' '
              ENDDO
              GO TO 110
          ENDIF
          KA = ND + 3
          DO J = KA, LB
             LINE(J) = NUMB(1)
          ENDDO

          KEXP = INT(MEXP10)
          IF (MEXP10 > 0) THEN
              DO J = 1, KEXP
                 LINE(J+1) = LINE(J+2)
              ENDDO
              LINE(KEXP+2) = '.'
          ENDIF

          IF (MEXP10 < 0) THEN
              KEXP = -INT(MEXP10)
              KA = 3 + KEXP
              KB = LB + 1
              KC = KB - KEXP
              DO J = KA, LB
                 KB = KB - 1
                 KC = KC - 1
                 LINE(KB) = LINE(KC)
                 LINE(KC) = NUMB(1)
              ENDDO
          ENDIF

          JDPT = 0
          DO J = 1, LB
             IF (LINE(J) == '.') JDPT = J
             IF (JDPT > 0 .AND. J > JDPT+JFORM2) LINE(J) = ' '
          ENDDO
          IF (JFORM2 == 0 .AND. JDPT > 0) LINE(KEXP+2) = ' '

      ENDIF

!             Restore values and return

      GO TO 180

!             LINE is not big enough to hold the number of digits specified.

  170 KFLAG = -8
      DO J = 1, LB
         LINE(J) = '*'
      ENDDO
      KWARN = KWRNSV
      CALL FMWARN

  180 MBASE = MBSAVE
      IF (MBLOGS /= MBASE) CALL FMCONS
      NDIG = NDSAVE
      MXEXP = MXSAVE
      KWARN = KWRNSV
      NCALL = NCALL - 1
      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      RETURN
      END SUBROUTINE FMOUT

      SUBROUTINE FMPACK(MA,MP)

!  MA is packed two base MBASE digits per word and returned in MP.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MP

      INTEGER :: J,KP
      INTENT (IN) :: MA
      INTENT (INOUT) :: MP

      IF (.NOT. ALLOCATED(MP%MP)) THEN
          ALLOCATE(MP%MP(NDIG/2+4),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MP%MP) < NDIG/2+4) THEN
          DEALLOCATE(MP%MP)
          ALLOCATE(MP%MP(NDIG/2+4),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KP = 2
      MP%MP(1) = MA%MP(1)
      MP%MP(2) = MA%MP(2)
      MP%MP(3) = ABS(MA%MP(3))*MBASE + MA%MP(4)
      IF (NDIG >= 4) THEN
          DO J = 4, NDIG, 2
             KP = KP + 1
             MP%MP(KP+1) = MA%MP(J+1)*MBASE + MA%MP(J+2)
          ENDDO
      ENDIF
      IF (MOD(NDIG,2) == 1) MP%MP(KP+2) = MA%MP(NDIG+2)*MBASE
      RETURN
      END SUBROUTINE FMPACK

      SUBROUTINE FMPI(MA)

!  MA = pi

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      CHARACTER(155) :: STRING
      INTEGER :: K,NDMB,NDSAVE,NDSV
      INTENT (INOUT) :: MA
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMPI'
      IF (ABS(NTRACE) >= 2 .AND. NCALL <= LVLTRC) THEN
          WRITE (KW,"(' Input to FMPI')")
      ENDIF

!             Increase the working precision.

      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = NGRD52
          NDIG = MAX(NDIG+K,2)
      ENDIF

!             Check to see if pi has previously been computed in base MBASE with
!             sufficient precision.

      IF (MBSPI == MBASE .AND. NDIGPI >= NDIG) THEN
          CALL FMEQU(MPISAV,MA,NDIGPI,NDSAVE)
      ELSE
          NDMB = INT(150.0*2.302585/ALOGMB)
          IF (NDMB >= NDIG) THEN
              NDSV = NDIG
              NDIG = NDMB
              STRING = '3.141592653589793238462643383279502884197169'//  &
              '39937510582097494459230781640628620899862803482534211'//  &
              '7067982148086513282306647093844609550582231725359408128'
              CALL FMST2M(STRING,MPISAV)
              MBSPI = MBASE
              NDIGPI = NDIG
              IF (ABS(MPISAV%MP(2)) > 10) NDIGPI = 0
          ELSE
              NDSV = NDIG
              NDIG = NDIG + 2 + NDIG/100
              CALL FMPI2(MXY(1))
              CALL FMEQ(MXY(1),MPISAV)
              MBSPI = MBASE
              NDIGPI = NDIG
              IF (ABS(MPISAV%MP(2)) > 10) NDIGPI = 0
          ENDIF
          CALL FMEQU(MPISAV,MA,NDIG,NDSAVE)
          NDIG = NDSV
      ENDIF

      NDIG = NDSAVE
      IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMPI

      SUBROUTINE FMPI2(MPI)

!  Internal routine to compute pi.
!  The formula used is due to S. Ramanujan.  For low to moderate precision, this routine uses
!  the series
!                                                (4n)!(1103+26390n)
!  1/pi = (sqrt(8)/9801) * sum(n=0 to infinity) --------------------
!                                               ((n!)**4)(396**(4n))
!
!  For higher precision, see routine FMPI3.
!
!  The result is returned in MPI.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MPI
      INTENT (INOUT) :: MPI
      DOUBLE PRECISION :: X
      REAL (KIND(1.0D0)) :: MX
      INTEGER :: NSTACK(49),J,K,KST,LARGE,N,NDIGRD,NDSAVE,NMETHD
      TYPE(MULTI) :: MXY(4)

      IF (.NOT. ALLOCATED(MPI%MP)) THEN
          ALLOCATE(MPI%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MPI%MP) < NDIG+2) THEN
          DEALLOCATE(MPI%MP)
          ALLOCATE(MPI%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

!             Check for using binary splitting if precision is high.

      IF (NDIG >= 100) THEN
          NMETHD = 2
      ELSE
          NMETHD = 1
      ENDIF
      IF (NMETHD == 2) THEN
          CALL FMPI3(MPI)
          RETURN
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      NDSAVE = NDIG
      N = -1
      CALL FMI2M(1103,MPI)
      CALL FMI2M(0,MXY(1))
      CALL FMI2M(1,MXY(2))
      CALL FMI2M(26390,MXY(3))
      CALL FMI2M(1103,MXY(4))
      MX = MXBASE**2/MBASE
      IF (MX > MXEXP2) MX = MXEXP2

  110 N = N + 1
      LARGE = INT(MX)/(4*N + 3)
      J = 4*N + 1
      IF (J > LARGE) THEN
          CALL FMMPYI_R1(MXY(2),J)
          J = J + 1
          CALL FMMPYI_R1(MXY(2),J)
          J = J + 1
          CALL FMMPYI_R1(MXY(2),J)
      ELSE IF (J*(J+1) > LARGE) THEN
          K = J*(J+1)
          CALL FMMPYI_R1(MXY(2),K)
          J = J + 2
          CALL FMMPYI_R1(MXY(2),J)
      ELSE
          K = J*(J+1)*(J+2)
          CALL FMMPYI_R1(MXY(2),K)
      ENDIF

      J = N + 1
      LARGE = INT(MXBASE)/J
      IF (J > LARGE) THEN
          CALL FMDIVI_R1(MXY(2),J)
          CALL FMDIVI_R1(MXY(2),J)
          CALL FMDIVI_R1(MXY(2),J)
      ELSE IF (J*J > LARGE) THEN
          K = J*J
          CALL FMDIVI_R1(MXY(2),K)
          CALL FMDIVI_R1(MXY(2),J)
      ELSE
          K = J*J*J
          CALL FMDIVI_R1(MXY(2),K)
      ENDIF

!             Break 4/396**4 into 1/(2178*2178*1296).

      J = 2178
      LARGE = INT(MXBASE)/J
      IF (J > LARGE) THEN
          CALL FMDIVI_R1(MXY(2),J)
          CALL FMDIVI_R1(MXY(2),J)
          CALL FMDIVI_R1(MXY(2),1296)
      ELSE
          K = J*J
          CALL FMDIVI_R1(MXY(2),K)
          CALL FMDIVI_R1(MXY(2),1296)
      ENDIF

      NDIGRD = NDIG
      NDIG = NDSAVE
      CALL FMADD_R2(MXY(3),MXY(4))
      NDIG = NDIGRD
      CALL FMMPY(MXY(2),MXY(4),MXY(1))

      NDIG = NDSAVE
      CALL FMADD_R1(MPI,MXY(1))
      NDIG = MAX(NGRD22,NDSAVE - INT(MPI%MP(2) - MXY(1)%MP(2)))
      IF (KFLAG /= 1) GO TO 110
      NDIG = NDSAVE

      CALL FMI2M(8,MXY(2))
      X = 8
      X = SQRT(X)
      CALL FMDPM(X,MXY(4))
      CALL FMDIG(NSTACK,KST)
      DO J = 1, KST
         NDIG = NSTACK(J)
         CALL FMDIV(MXY(2),MXY(4),MXY(1))
         CALL FMADD_R1(MXY(4),MXY(1))
         CALL FMDIVI_R1(MXY(4),2)
      ENDDO
      CALL FMI2M(9801,MXY(3))
      CALL FMMPY_R1(MPI,MXY(4))
      CALL FMDIV_R2(MXY(3),MPI)

      RETURN
      END SUBROUTINE FMPI2

      SUBROUTINE FMPI3(MPI)
      USE ModLib_FMVALS
      IMPLICIT NONE

!  Internal pi routine for very high precision.

!  The algorithm used is Chudnovskys' Ramanujan-style series with binary splitting.

!                                                  (-1)**n*(6n)!(13591409+545140134*n)
!  1/pi = (12/640320**(3/2)) * sum(n=0 to infinity) -----------------------------------
!                                                     (n!)**3*(3*n)!*(640320**(3n))
!  The result is returned in MPI.

      TYPE(MULTI) :: MPI
      INTENT (INOUT) :: MPI
      INTEGER :: K,LEVEL_OF_RECURSION
      TYPE(MULTI) :: MXY(4)

      NDIG = NDIG + 100
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG + 30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG + 30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG + 30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      NDIG = NDIG - 100

!             Determine K, the number of terms to sum in the series for pi.

      K = NDIG*DLOGMB/3.2654441D+1 + 10
      LEVEL_OF_RECURSION = 0
      CALL FMPI3_PQT(0,K,MXY(1),MXY(2),MXY(3),LEVEL_OF_RECURSION)

      IF (MXY(2)%MP(2) >= NDIG .AND. MXY(3)%MP(2) >= NDIG) THEN
          CALL FMDIV(MXY(2),MXY(3),MXY(1))
          CALL FMI2M(640320,MXY(2))
          CALL FMSQRT(MXY(2),MXY(3))
          CALL FMMPYI_R1(MXY(3),53360)
          CALL FMMPY(MXY(1),MXY(3),MPI)
      ELSE
          IF (MXY(2)%MP(2) >= NDIG) THEN
              CALL FMEQ(MXY(2),MXY(1))
          ELSE
              CALL IMI2FM(MXY(2),MXY(1))
          ENDIF
          IF (MXY(3)%MP(2) >= NDIG) THEN
              CALL FMEQ(MXY(3),MXY(4))
          ELSE
              CALL IMI2FM(MXY(3),MXY(4))
          ENDIF
          CALL FMDIV(MXY(1),MXY(4),MXY(3))
          CALL FMI2M(640320,MXY(1))
          CALL FMSQRT(MXY(1),MXY(2))
          CALL FMMPYI_R1(MXY(2),53360)
          CALL FMMPY(MXY(2),MXY(3),MPI)
      ENDIF
      RETURN
      END SUBROUTINE FMPI3

      RECURSIVE SUBROUTINE FMPI3_PQT(A,B,MP,MQ,MT,LEVEL_OF_RECURSION)

!  This routine does the binary splitting for computing the constant pi.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MP,MQ,MT
      INTEGER :: A,B,LEVEL_OF_RECURSION
      INTENT (IN) :: A,B
      INTENT (INOUT) :: MP,MQ,MT
      TYPE(MULTI) :: MXY(6)
      INTEGER :: J,KA,KM,RESULT_SIZE
      REAL (KIND(0.0D0)) :: DA,DB

      DA = A
      DB = B
      LEVEL_OF_RECURSION = LEVEL_OF_RECURSION + 1

      IF (B-A < 10) THEN
          RESULT_SIZE = ( (DB-DA)*4.276666 + 3*( (DB+0.5D0)*LOG(DB+1) - DB + 1/(12*(DB+1)) -  &
                        ( (DA+0.5D0)*LOG(DA+1) - DA + 1/(12*(DA+1)) ) ) ) * 1.01 / DLOGMB + 15
          RESULT_SIZE = MAX(5,RESULT_SIZE) + 5*LOG(DBLE(NDIG))/DLOGMB + 15
          IF (.NOT. ALLOCATED(MP%MP)) THEN
              ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MP%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MP%MP)
              ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          RESULT_SIZE = ( (DB-DA)*36.93111 + 3*( (DB+0.5D0)*LOG(DB+1) - DB + 1/(12*(DB+1)) -  &
                        ( (DA+0.5D0)*LOG(DA+1) - DA + 1/(12*(DA+1)) ) ) ) * 1.01 / DLOGMB + 15
          RESULT_SIZE = MAX(5,RESULT_SIZE) + 5*LOG(DBLE(NDIG))/DLOGMB + 15
          IF (.NOT. ALLOCATED(MQ%MP)) THEN
              ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MQ%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MQ%MP)
              ALLOCATE(MQ%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MT%MP)) THEN
              ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MT%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MT%MP)
              ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(1)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(1)%MP)
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(2)%MP)) THEN
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(2)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(2)%MP)
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(3)%MP)) THEN
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(3)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(3)%MP)
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(4)%MP)) THEN
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(4)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(4)%MP)
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(5)%MP)) THEN
              ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(5)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(5)%MP)
              ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          CALL IMI2M(1,MP)
          KA = A
          IF (KA == 0) KA = 1
          DO J = KA, B
             CALL IMMPYI(MP,-(6*J-5),MXY(2))
             CALL IMMPYI(MXY(2),2*J-1,MXY(1))
             CALL IMMPYI(MXY(1),6*J-1,MP)
          ENDDO

          CALL IMI2M(640320,MXY(1))
          CALL IMSQR(MXY(1),MXY(3))
          CALL IMMPY(MXY(3),MXY(1),MXY(2))
          CALL IMDIVI(MXY(2),24,MXY(1))
          CALL IMI2M(1,MQ)
          DO J = KA, B
             CALL IMMPYI(MQ,J,MXY(3))
             CALL IMMPYI(MXY(3),J,MXY(2))
             CALL IMMPYI(MXY(2),J,MXY(3))
             CALL IMMPY(MXY(3),MXY(1),MQ)
          ENDDO

          CALL IMI2M(0,MT)
          IF (A == 0) THEN
              CALL IMMPYI(MQ,13591409,MT)
          ENDIF
          CALL IMEQ(MQ,MXY(2))
          DO J = KA, B
             CALL IMMPYI(MXY(2),-(6*J-5),MXY(3))
             CALL IMMPYI(MXY(3),2*J-1,MXY(4))
             CALL IMMPYI(MXY(4),6*J-1,MXY(2))
             CALL IMDIVI(MXY(2),J,MXY(3))
             CALL IMDIVI(MXY(3),J,MXY(4))
             CALL IMDIVI(MXY(4),J,MXY(3))
             CALL IMDIV(MXY(3),MXY(1),MXY(2))
             CALL IMI2M(545140134,MXY(3))
             CALL IMMPYI(MXY(3),J,MXY(4))
             CALL IMI2M(13591409,MXY(3))
             CALL IMADD(MXY(3),MXY(4),MXY(5))
             CALL IMMPY(MXY(2),MXY(5),MXY(3))
             CALL IMADD(MT,MXY(3),MXY(4))
             CALL IMEQ(MXY(4),MT)
          ENDDO

          GO TO 110
      ENDIF

      KM = A/2 + B/2 + MOD(A,2)*MOD(B,2)
      CALL FMPI3_PQT(A,KM-1,MXY(1),MXY(2),MXY(3),LEVEL_OF_RECURSION)
      CALL FMPI3_PQT(KM,B,MXY(4),MXY(5),MXY(6),LEVEL_OF_RECURSION)

!             MP is not needed in FMPI3, so this multiplication can be skipped at the top level
!             of the recursion.

      IF (LEVEL_OF_RECURSION > 1) THEN
          CALL IM_OR_FM_MPY(MXY(1),MXY(4),MP)
      ELSE
          CALL IMI2M(0,MP)
      ENDIF
      CALL IM_OR_FM_MPY(MXY(2),MXY(5),MQ)

      CALL IM_OR_FM_MPY(MXY(5),MXY(3),MXY(2))
      CALL IM_OR_FM_MPY(MXY(1),MXY(6),MXY(4))
      CALL IM_OR_FM_ADD(MXY(2),MXY(4),MT)

  110 LEVEL_OF_RECURSION = LEVEL_OF_RECURSION - 1
      RETURN
      END SUBROUTINE FMPI3_PQT

      SUBROUTINE FMPRNT(MA)

!  Print MA in base 10 format.

!  FMPRNT can be called directly by the user for easy output in M format.
!  MA is converted using FMOUT and printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA

      CHARACTER(20) :: FORM
      INTEGER :: J,K,KSAVE,L,LAST,LB,ND,NEXP
      INTENT (IN) :: MA

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMPRNT'
      KSAVE = KFLAG
      ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
      IF (ND < 2) ND = 2
      NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
      LB = MAX(JFORM2+NEXP,ND+NEXP)
      IF (LB+50 > LMBUFF) THEN
          IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
          ALLOCATE(CMBUFF(LB+50),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          LMBUFF = LB + 50
      ENDIF
      CALL FMOUT(MA,CMBUFF,LB)
      KFLAG = KSAVE
      LAST = LB + 1
      WRITE (FORM,"(' (6X,',I3,'A1) ')") KSWIDE-7
      DO J = 1, LB
         IF (CMBUFF(LAST-J) /= ' ' .OR. J == LB) THEN
             L = LAST - J
             WRITE (KW,FORM) (CMBUFF(K),K=1,L)
             NCALL = NCALL - 1
             RETURN
         ENDIF
      ENDDO
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMPRNT

      SUBROUTINE FMPWR(MA,MB,MC)

!  MC = MA ** MB

!  If MB can be expressed exactly as a one word integer, then FMIPWR is used.  This is much faster
!  when MB is small, and using FMIPWR allows MA to be negative.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,INTMB,J,K,KFL,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NDSAVE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(4)
      LOGICAL, EXTERNAL :: FMCOMP

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

!             Convert MB to an integer before changing NDIG.

      KWRNSV = KWARN
      KWARN = 0
      CALL FMMI(MB,INTMB)
      KWARN = KWRNSV
      KFL = KFLAG
      KR_RETRY = 0

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(1) >= 0 .AND. MA%MP(3) > 0 .AND.  &
          MA%MP(2) /= MUNKNO .AND. MB%MP(2) /= MUNKNO) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          NCALL = NCALL + 1
          IF (MB%MP(2) == MEXPUN) THEN
              CALL FMEQ(MB,MXY(3))
              IF (MA%MP(2) <= 0) CALL FMMPYI_R1(MXY(3),-1)
          ELSE
              CALL FMLN(MA,MXY(1))
              CALL FMMPY(MXY(1),MB,MXY(3))
          ENDIF
          NCALL = NCALL - 1
          KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (MXY(3)%MP(2) < -NDIG) THEN
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMPWR'
                  CALL FMNTR(2,MA,MB,2,1)
                  NCALL = NCALL - 1
              ENDIF
              J = NTRACE
              NTRACE = 0
              K = KWARN
              KWARN = 0
              CALL FMI2M(1,MXY(1))
              CALL FMSUB(MA,MXY(1),MXY(2))
              IF (MXY(2)%MP(3) == 0) THEN
                  CALL FMI2M(1,MXY(2))
              ELSE
                  CALL FMADD(MXY(1),MXY(3),MXY(2))
              ENDIF
              IF (MXY(2)%MP(2) /= MUNKNO) THEN
                  KFLAG = 0
                  NTRACE = J
                  KWARN = K
                  CALL FMEQ(MXY(2),MC)
                  IF (NTRACE /= 0) THEN
                      NCALL = NCALL + 1
                      NAMEST(NCALL) = 'FMPWR'
                      CALL FMNTR(1,MC,MC,1,1)
                      NCALL = NCALL - 1
                  ENDIF
                  RETURN
              ENDIF
          ENDIF
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR.  &
          MA%MP(3) == 0 .OR. MA%MP(1) < 0) THEN
          CALL FMENTR('FMPWR    ',MA,MB,2,1,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMPWR'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          IF (MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF

!             If the exponent is large or the base is very large, raise the precision.

      IF (MA%MP(2) /= 0) THEN
          IEXTRA = MAX(0,INT(MB%MP(2)))+INT(LOG(ABS(REAL(MA%MP(2))))/ALOGMB)
      ELSE
          IEXTRA = MAX(0,INT(MB%MP(2)))
      ENDIF
      IF (MB%MP(2)-NDIG > LOG(ALOGMB*REAL(MXEXP2))) THEN
          IEXTRA = 0
      ENDIF
      NDIG = NDIG + IEXTRA

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF

!             If the exponent is a small integer, call FMIPWR.

      KWRNSV = KWARN
      KWARN = 0

      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)

      IF (KFL == 0) THEN
          CALL FMIPWR(MXY(2),INTMB,MXY(4))
      ELSE IF (MXY(2)%MP(3) == 0) THEN
          CALL FMST2M('UNKNOWN',MXY(4))
          KFLAG = -4
      ELSE
          CALL FMEQU(MB,MXY(1),NDSAVE,NDIG)
          IF (MXY(2)%MP(1) < 0) THEN
              CALL FMINT(MXY(1),MXY(3))
              IF (FMCOMP(MXY(1),'==',MXY(3))) THEN
                  CALL FMI2M(2,MXY(3))
                  CALL FMMOD(MXY(1),MXY(3),MXY(4))
                  J = -1
                  IF (MXY(4)%MP(3) == 0) J = 1
                  CALL FMMPYI_R1(MXY(2),-1)
                  CALL FMLN(MXY(2),MXY(3))
                  CALL FMMPY_R1(MXY(3),MXY(1))
                  CALL FMEXP(MXY(3),MXY(4))
                  IF (J == -1) CALL FMMPYI_R1(MXY(4),-1)
              ELSE
                  CALL FMST2M('UNKNOWN',MXY(4))
                  KFLAG = -4
              ENDIF
          ELSE
              CALL FMLN(MXY(2),MXY(3))
              CALL FMMPY_R1(MXY(3),MXY(1))
              CALL FMEXP(MXY(3),MXY(4))
          ENDIF
      ENDIF
      KWARN = KWRNSV

!             Round the result and return.

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(4)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(4),MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMPWR

      SUBROUTINE FMRDC(MA,JSIN,JCOS,JSWAP)

!  Reduce MA using various trigonometric identities to an equivalent angle between 0 and 45 degrees.
!  The reduction is done in radians if KRAD (in module FMVALS) is 1, in degrees if KRAD is 0.
!  JSIN and JCOS are returned +1 or -1 and JSWAP is returned to indicate that the sin and cos
!  functions have been interchanged as follows:

!  JSWAP = 0 means   SIN(MA) = JSIN*SIN(returned value of MA)
!                    COS(MA) = JCOS*COS(returned value of MA)

!  JSWAP = 1 means   SIN(MA) = JSIN*COS(returned value of MA)
!                    COS(MA) = JCOS*SIN(returned value of MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: JSIN,JCOS,JSWAP
      DOUBLE PRECISION :: X
      INTEGER :: J,NDSAVE,NDSV
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (INOUT) :: MA
      INTENT (INOUT) :: JSIN,JCOS,JSWAP
      TYPE(MULTI) :: MXY(5)


      IF (MBLOGS /= MBASE) CALL FMCONS
      JSIN = 1
      JCOS = 1
      JSWAP = 0
      NDSAVE = NDIG
      IF (KRAD == 0) THEN
          CALL FMI2M(360,MXY(2))
          CALL FMEQ(MA,MXY(4))
          CALL FMMOD(MXY(4),MXY(2),MA)
      ENDIF

      NDIG = NDIG + MAX(0,INT(MA%MP(2)))

!             If MA is less than 1/MBASE, no reduction is needed.

      IF (MA%MP(2) < 0) THEN
          NDIG = NDSAVE
          IF (MA%MP(1) < 0) THEN
              MA%MP(1) = 1
              JSIN = -1
          ENDIF
          RETURN
      ENDIF

      J = 1
      IF (KRAD == 1) THEN
  110     IF (MBSPI /= MBASE .OR. NDIGPI < NDIG)  THEN
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMPI(MXY(4))
              NDIG = NDSV
          ENDIF
          CALL FMEQU(MA,MXY(4),NDSAVE,NDIG)
          IF (MA%MP(1) < 0) JSIN = -1
          MXY(4)%MP(1) = 1
          IF (MXY(4)%MP(2) == 0) THEN
              CALL FMM2DP(MXY(4),X)
              IF (X <= 0.75) THEN
                  NDIG = NDSAVE
                  CALL FMEQ(MXY(4),MA)
                  RETURN
              ENDIF
          ENDIF
          CALL FMADD(MPISAV,MPISAV,MXY(2))
          IF (FMCOMP(MXY(4),'>=',MXY(2))) THEN
              CALL FMDIV(MXY(4),MXY(2),MXY(1))
              CALL FMINT(MXY(1),MXY(5))
              CALL FMMPY_R1(MXY(5),MXY(2))
              CALL FMSUB_R1(MXY(4),MXY(5))
          ENDIF
          CALL FMEQ(MPISAV,MXY(3))
          IF (FMCOMP(MXY(4),'>=',MXY(3))) THEN
              JSIN = -JSIN
              CALL FMSUB_R2(MXY(2),MXY(4))
          ENDIF
          CALL FMDIVI_R1(MXY(2),4)
          IF (FMCOMP(MXY(4),'>=',MXY(2))) THEN
              JCOS = -JCOS
              CALL FMSUB_R2(MXY(3),MXY(4))
          ENDIF
          CALL FMDIVI_R1(MXY(3),4)
          IF (FMCOMP(MXY(4),'>=',MXY(3))) THEN
              JSWAP = 1
              CALL FMSUB_R2(MXY(2),MXY(4))
          ENDIF

!             If the reduced argument is close to zero, then cancellation has produced an
!             inaccurate value.
!             Raise NDIG and do the reduction again.

          IF (J == 1 .AND. (MXY(4)%MP(2) < 0 .OR. MXY(4)%MP(3) == 0)) THEN
              J = 2
              IF (MXY(4)%MP(3) == 0) THEN
                  NDIG = 2*NDIG
              ELSE
                  NDIG = NDIG - INT(MXY(4)%MP(2))
              ENDIF
              JSIN = 1
              JCOS = 1
              JSWAP = 0
              GO TO 110
          ENDIF

      ELSE

          CALL FMEQU(MA,MXY(4),NDSAVE,NDIG)
          IF (MA%MP(1) < 0) JSIN = -1
          MXY(4)%MP(1) = 1
          IF (MXY(4)%MP(2) == 0) THEN
              CALL FMM2DP(MXY(4),X)
              IF (X <= 44.0) THEN
                  NDIG = NDSAVE
                  CALL FMEQ(MXY(4),MA)
                  RETURN
              ENDIF
          ENDIF
          CALL FMI2M(360,MXY(2))
          IF (FMCOMP(MXY(4),'>=',MXY(2))) THEN
              CALL FMDIV(MXY(4),MXY(2),MXY(1))
              CALL FMINT(MXY(1),MXY(5))
              CALL FMMPY_R1(MXY(5),MXY(2))
              CALL FMSUB_R1(MXY(4),MXY(5))
          ENDIF
          CALL FMI2M(180,MXY(3))
          IF (FMCOMP(MXY(4),'>=',MXY(3))) THEN
              JSIN = -JSIN
              CALL FMSUB_R2(MXY(2),MXY(4))
          ENDIF
          CALL FMI2M(90,MXY(2))
          IF (FMCOMP(MXY(4),'>=',MXY(2))) THEN
              JCOS = -JCOS
              CALL FMSUB_R2(MXY(3),MXY(4))
          ENDIF
          CALL FMI2M(45,MXY(3))
          IF (FMCOMP(MXY(4),'>=',MXY(3))) THEN
              JSWAP = 1
              CALL FMSUB_R2(MXY(2),MXY(4))
          ENDIF

      ENDIF

!             Round the result and return.

      CALL FMEQU(MXY(4),MA,NDIG,NDSAVE)
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE FMRDC

      SUBROUTINE FMREAD(KREAD,MA)

!  Read MA on unit KREAD.  Multi-line numbers will have '&' as the last nonblank character on all
!  but the last line.  Only one number is allowed on the line(s).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: KREAD

      CHARACTER :: LINE(132)
      INTEGER :: J,K,L2,LB,NDSAVE
      INTENT (IN) :: KREAD
      INTENT (INOUT) :: MA
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMREAD'
      NDSAVE = NDIG
      NDIG = MAX(NDIG+NGRD52,2)
      LB = 0

  110 READ (KREAD,"(132A1)",ERR=120,END=120) LINE

!             Scan the line and look for '&'

      DO J = 1, 132
         IF (LINE(J) == '&') GO TO 110
         IF (LINE(J) /= ' ') THEN
             LB = LB + 1
             IF (LB > LMBUFF) THEN

!                If CMBUFF runs out of space, try to re-allocate it with a bigger size.

                 IF (LMBUFF > 0) THEN
                     ALLOCATE(MOVE_CMBUFF(LMBUFF),STAT=K)
                     IF (K /= 0) THEN
                         CALL FMDEFINE_ERROR
                     ENDIF
                     DO K = 1, LMBUFF
                        MOVE_CMBUFF(K) = CMBUFF(K)
                     ENDDO
                     DEALLOCATE(CMBUFF)
                     L2 = MAX(10000,2*LMBUFF)
                     ALLOCATE(CMBUFF(L2),STAT=K)
                     IF (K /= 0) THEN
                         CALL FMDEFINE_ERROR
                     ENDIF
                     DO K = 1, L2
                        CMBUFF(K) = ' '
                     ENDDO
                     DO K = 1, LMBUFF
                        CMBUFF(K) = MOVE_CMBUFF(K)
                     ENDDO
                     DEALLOCATE(MOVE_CMBUFF)
                     LMBUFF = L2
                 ELSE
                     ALLOCATE(CMBUFF(10000),STAT=K)
                     IF (K /= 0) THEN
                         CALL FMDEFINE_ERROR
                     ENDIF
                     LMBUFF = 10000
                 ENDIF
             ENDIF
             CMBUFF(LB) = LINE(J)
         ENDIF
      ENDDO

      NCALL = NCALL - 1
      CALL FMINP(CMBUFF,MXY(1),1,LB)
      NCALL = NCALL + 1

      CALL FMEQU(MXY(1),MA,NDIG,NDSAVE)
      NDIG = NDSAVE
      NCALL = NCALL - 1
      RETURN

!             If there is an error, return UNKNOWN.

  120 KFLAG = -4
      CALL FMWARN
      NDIG = NDSAVE
      CALL FMST2M('UNKNOWN',MA)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMREAD

      SUBROUTINE FMRND(MW,ND,NGUARD,KSHIFT)

!  Round MW to ND digits (base MBASE).

!  MW is non-negative and has ND+NGUARD+KSHIFT digits.

!  NGUARD is the number of guard digits carried.
!  KSHIFT is 1 if a left shift is pending when MW%MP(3)=0.

!  Round to position MW%MP(1+ND+1+KSHIFT) using the guard digits
!  MW%MP(1+ND+2+KSHIFT), ..., MW%MP(1+ND+1+NGUARD+KSHIFT).

!  This routine is designed to be called only from within the FM package.
!  The user should call FMEQU to round numbers.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MW
      INTEGER :: ND,NGUARD,KSHIFT

      REAL (KIND(1.0D0)) :: M2,MKT
      INTEGER :: J,K,KB,L
      INTENT (IN) :: ND,KSHIFT,NGUARD
      INTENT (INOUT) :: MW

      IF (KROUND == -1) THEN
          IF (JRSIGN == 1) RETURN
          DO J = ND+2+KSHIFT, ND+1+NGUARD+KSHIFT
             IF (MW%MP(J+1) > 0) THEN
                 MW%MP(KSHIFT+ND+2) = MW%MP(KSHIFT+ND+2) + 1
                 MW%MP(KSHIFT+ND+3) = 0
                 IF (MW%MP(KSHIFT+ND+2) < MBASE) RETURN
                 L = ND + 2 + KSHIFT
                 GO TO 120
             ENDIF
          ENDDO
          RETURN
      ENDIF

      IF (KROUND == 2) THEN
          IF (JRSIGN == -1) RETURN
          DO J = ND+2+KSHIFT, ND+1+NGUARD+KSHIFT
             IF (MW%MP(J+1) > 0) THEN
                 MW%MP(KSHIFT+ND+2) = MW%MP(KSHIFT+ND+2) + 1
                 MW%MP(KSHIFT+ND+3) = 0
                 IF (MW%MP(KSHIFT+ND+2) < MBASE) RETURN
                 L = ND + 2 + KSHIFT
                 GO TO 120
             ENDIF
          ENDDO
          RETURN
      ENDIF

      IF (KROUND == 0) RETURN
      L = ND + 2 + KSHIFT
      IF (2*(MW%MP(L+1)+1) < MBASE) RETURN
      IF (2*MW%MP(L+1) > MBASE) THEN
          MW%MP(L) = MW%MP(L) + 1
          MW%MP(L+1) = 0
          IF (MW%MP(L) < MBASE) RETURN
          GO TO 120
      ENDIF

!             If the first guard digit gives a value close to 1/2 then further guard digits must
!             be examined.

      M2 = 2
      IF (INT(MBASE-AINT (MBASE/M2)*M2) == 0) THEN
          IF (2*MW%MP(L+1) < MBASE) RETURN
          IF (2*MW%MP(L+1) == MBASE) THEN
              IF (NGUARD >= 2) THEN
                  DO J = 2, NGUARD
                     IF (MW%MP(J+L) > 0) GO TO 110
                  ENDDO
              ENDIF

!                       Round to even.

              IF (INT(MW%MP(L)-AINT (MW%MP(L)/M2)*M2) == 0) RETURN
          ENDIF
      ELSE
          IF (2*MW%MP(L+1)+1 == MBASE) THEN
              IF (NGUARD >= 2) THEN
                  DO J = 2, NGUARD
                     IF (2*(MW%MP(J+L)+1) < MBASE) RETURN
                     IF (2*MW%MP(J+L) > MBASE) GO TO 110
                  ENDDO
                  IF (NGUARD <= NDIG) RETURN
                  M2 = 2
                  IF (INT(MW%MP(L)-AINT (MW%MP(L)/M2)*M2) == 0) THEN
                      RETURN
                  ELSE
                      GO TO 110
                  ENDIF
              ENDIF
          ENDIF
      ENDIF

!             Round up.

  110 MW%MP(L) = MW%MP(L) + 1
      MW%MP(L+1) = 0

!             Check whether there was a carry in the rounded digit.

  120 KB = L - 1
      IF (KB >= 3) THEN
          K = KB + 1
          DO J = 3, KB
             K = K - 1
             IF (MW%MP(K+1) < MBASE) RETURN
             MKT = AINT (MW%MP(K+1)/MBASE)
             MW%MP(K) = MW%MP(K) + MKT
             MW%MP(K+1) = MW%MP(K+1) - MKT*MBASE
          ENDDO
      ENDIF

!             If there is a carry in the first digit then the exponent must be adjusted and the
!             number shifted right.

      IF (MW%MP(3) >= MBASE) THEN
          IF (KB >= 4) THEN
              K = KB + 1
              DO J = 4, KB
                 K = K - 1
                 MW%MP(K+1) = MW%MP(K)
              ENDDO
          ENDIF

          MKT = AINT (MW%MP(3)/MBASE)
          IF (KB >= 3) MW%MP(4) = MW%MP(3) - MKT*MBASE
          MW%MP(3) = MKT
          MW%MP(2) = MW%MP(2) + 1
      ENDIF

      RETURN
      END SUBROUTINE FMRND

      SUBROUTINE FMRPWR(MA,IVAL,JVAL,MB)

!  MB = MA ** (IVAL/JVAL)   rational exponentiation.

!  This routine is faster than FMPWR when IVAL and JVAL are small integers.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL,JVAL
      DOUBLE PRECISION :: X,F
      REAL (KIND(1.0D0)) :: MA1,MA2,MAS,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: NSTACK(49),IJSIGN,INVERT,IVAL2,J,JVAL2,K,KL,KOVUN,KR_RETRY,KST,  &
                 KWRNSV,L,LVAL,NDSAVE
      REAL :: XVAL
      INTENT (IN) :: MA,IVAL,JVAL
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KR_RETRY = 0

      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMRPWR'
      IF (NTRACE /= 0) THEN
          CALL FMNTR(2,MA,MA,1,1)
          CALL FMNTRI(2,IVAL,0)
          CALL FMNTRI(2,JVAL,0)
      ENDIF
      KOVUN = 0
      IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
      NDSAVE = NDIG
      MXSAVE = MXEXP
      MXEXP = MXEXP2

  110 IF (NCALL == 1) THEN
          XVAL = MAX(ABS(IVAL),ABS(JVAL))
          IF (XVAL == 0.0) XVAL = 1.0
          K = INT((5.0*REAL(DLOGTN) + 2.0*LOG(XVAL))/ALOGMB + 2.0)
          NDIG = MAX(NDIG+K,2)
          IF (KR_RETRY >= 1) THEN
              NDIG = MAX(NDIG,2*NDSAVE+10)
          ENDIF
      ELSE
          XVAL = MAX(ABS(IVAL),ABS(JVAL))
          IF (XVAL == 0.0) XVAL = 1.0
          K = INT(LOG(XVAL)/ALOGMB + 1.0)
          NDIG = NDIG + K
      ENDIF

      MAS = MA%MP(1)
      MA1 = MA%MP(2)
      MA2 = MA%MP(3)
      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)

!             Use GCD-reduced positive exponents.

      IJSIGN = 1
      IVAL2 = ABS(IVAL)
      JVAL2 = ABS(JVAL)
      IF (IVAL > 0 .AND. JVAL < 0) IJSIGN = -1
      IF (IVAL < 0 .AND. JVAL > 0) IJSIGN = -1
      IF (IVAL2 > 0 .AND. JVAL2 > 0) CALL FMGCDI(IVAL2,JVAL2)

!             Check for special cases.

  120 IF (MA1 == MUNKNO .OR. JVAL2 == 0 .OR. (IJSIGN <= 0 .AND. MA2 == 0)) THEN
          CALL FMST2M('UNKNOWN',MXY(4))
          KFLAG = -4
          GO TO 130
      ENDIF

      IF (IVAL2 == 0) THEN
          CALL FMIM(1,MXY(4))
          GO TO 130
      ENDIF

      IF (JVAL2 == 1) THEN
          CALL FMIPWR(MXY(2),IJSIGN*IVAL2,MXY(4))
          GO TO 130
      ENDIF

      IF (MA2 == 0) THEN
          CALL FMEQ(MA,MXY(4))
          GO TO 130
      ENDIF

      IF (MAS < 0) THEN
          IF (MOD(JVAL2,2) == 0) THEN
              JVAL2 = 0
              GO TO 120
          ENDIF
      ENDIF

      IF (MA1 == MEXPOV) THEN
          IF (IVAL2 < JVAL2) THEN
              JVAL2 = 0
              GO TO 120
          ENDIF
          CALL FMIM(0,MXY(4))
          IF (IJSIGN == 1 .AND. MAS > 0) THEN
              CALL FMST2M('OVERFLOW',MXY(4))
              KFLAG = -5
          ELSE IF (IJSIGN == -1 .AND. MAS > 0) THEN
              CALL FMST2M('UNDERFLOW',MXY(4))
              KFLAG = -6
          ELSE IF (IJSIGN == 1 .AND. MAS < 0) THEN
              IF (MOD(IVAL2,2) == 0) THEN
                  CALL FMST2M('OVERFLOW',MXY(4))
                  KFLAG = -5
              ELSE
                  CALL FMST2M('-OVERFLOW',MXY(4))
                  KFLAG = -5
              ENDIF
          ELSE IF (IJSIGN == -1 .AND. MAS < 0) THEN
              IF (MOD(IVAL2,2) == 0) THEN
                  CALL FMST2M('UNDERFLOW',MXY(4))
                  KFLAG = -6
              ELSE
                  CALL FMST2M('-UNDERFLOW',MXY(4))
                  KFLAG = -6
              ENDIF
          ENDIF
          GO TO 130
      ENDIF

      IF (MA1 == MEXPUN) THEN
          IF (IVAL2 < JVAL2) THEN
              JVAL2 = 0
              GO TO 120
          ENDIF
          CALL FMIM(0,MXY(4))
          IF (IJSIGN == 1 .AND. MAS > 0) THEN
              CALL FMST2M('UNDERFLOW',MXY(4))
              KFLAG = -6
          ELSE IF (IJSIGN == -1 .AND. MAS > 0) THEN
              CALL FMST2M('OVERFLOW',MXY(4))
              KFLAG = -5
          ELSE IF (IJSIGN == 1 .AND. MAS < 0) THEN
              IF (MOD(IVAL2,2) == 0) THEN
                  CALL FMST2M('UNDERFLOW',MXY(4))
                  KFLAG = -6
              ELSE
                  CALL FMST2M('-UNDERFLOW',MXY(4))
                  KFLAG = -6
              ENDIF
          ELSE IF (IJSIGN == -1 .AND. MAS < 0) THEN
              IF (MOD(IVAL2,2) == 0) THEN
                  CALL FMST2M('OVERFLOW',MXY(4))
                  KFLAG = -5
              ELSE
                  CALL FMST2M('-OVERFLOW',MXY(4))
                  KFLAG = -5
              ENDIF
          ENDIF
          GO TO 130
      ENDIF

!             Invert MA if MA > 1 and IVAL or JVAL is large.

      INVERT = 0
      IF (MA%MP(2) > 0) THEN
          IF (IVAL > 5 .OR. JVAL > 5) THEN
              INVERT = 1
              CALL FMI2M(1,MXY(1))
              CALL FMDIV_R2(MXY(1),MXY(2))
          ENDIF
      ENDIF

!             Generate the first approximation to ABS(MA)**(1/JVAL2).

      MA1 = MXY(2)%MP(2)
      MXY(2)%MP(2) = 0
      MXY(2)%MP(1) = 1
      CALL FMM2DP(MXY(2),X)
      L = INT(MA1/JVAL2)
      F = MA1/DBLE(JVAL2) - L
      X = X**(1.0D0/JVAL2) * DBLE(MBASE)**F
      CALL FMDPM(X,MXY(4))
      MXY(4)%MP(2) = MXY(4)%MP(2) + L
      MXY(2)%MP(2) = MA1

!             Initialize.

      CALL FMI2M(0,MXY(3))
      CALL FMDIG(NSTACK,KST)

!             Newton iteration.

      DO J = 1, KST
         NDIG = NSTACK(J)
         IF (J < KST) NDIG = NDIG + 1
         LVAL = JVAL2 - 1
         CALL FMIPWR(MXY(4),LVAL,MXY(3))
         CALL FMDIV_R2(MXY(2),MXY(3))
         CALL FMMPYI_R1(MXY(4),LVAL)
         CALL FMADD_R1(MXY(4),MXY(3))
         CALL FMDIVI_R1(MXY(4),JVAL2)
      ENDDO

      IF (MXY(4)%MP(2) /= MUNKNO .AND. MXY(4)%MP(3) /= 0 .AND. MAS < 0)  &
          MXY(4)%MP(1) = -MXY(4)%MP(1)
      CALL FMIPWR(MXY(4),IJSIGN*IVAL2,MXY(3))
      CALL FMEQ(MXY(3),MXY(4))
      IF (INVERT == 1) THEN
          CALL FMI2M(1,MXY(1))
          CALL FMDIV_R2(MXY(1),MXY(4))
      ENDIF

!             Round the result and return.

  130 KWRNSV = KWARN
      IF (MA1 == MUNKNO) KWARN = 0

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(4)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(4),MB,NDSAVE,MXSAVE,KOVUN)
      IF (KFLAG == 1) KFLAG = 0
      KWARN = KWRNSV
      RETURN
      END SUBROUTINE FMRPWR

      SUBROUTINE FMRSLT(MA,MB,MC,KRESLT)

!  Handle results that are special cases, such as overflow, underflow, and unknown.

!  MA and MB are the input arguments to an FM subroutine.

!  MC is the result that is returned.

!  KRESLT is the result code from FMARGS.  Result codes handled here:

!   0 - Perform the normal operation
!   1 - The result is the first input argument
!   2 - The result is the second input argument
!   3 - The result is -OVERFLOW
!   4 - The result is +OVERFLOW
!   5 - The result is -UNDERFLOW
!   6 - The result is +UNDERFLOW
!   7 - The result is -1.0
!   8 - The result is +1.0
!  11 - The result is 0.0
!  12 - The result is UNKNOWN

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTEGER :: KRESLT

      INTEGER :: KFSAVE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      KFSAVE = KFLAG
      IF (KRESLT == 1) THEN
          CALL FMEQ(MA,MC)
          IF (NAMEST(NCALL) == 'FMADD' .OR. NAMEST(NCALL) == 'FMADD_R1' .OR.  &
              NAMEST(NCALL) == 'FMADD_R2' .OR. NAMEST(NCALL) == 'FMSUB' .OR.  &
              NAMEST(NCALL) == 'FMSUB_R1' .OR. NAMEST(NCALL) == 'FMSUB_R2') THEN
              KFLAG = 1
          ELSE
              KFLAG = KFSAVE
          ENDIF
          RETURN
      ENDIF

      IF (KRESLT == 2) THEN
          CALL FMEQ(MB,MC)
          IF (NAMEST(NCALL) == 'FMADD' .OR. NAMEST(NCALL) == 'FMADD_R1' .OR.  &
              NAMEST(NCALL) == 'FMADD_R2') THEN
              KFLAG = 1
          ELSE
              KFLAG = KFSAVE
          ENDIF
          IF (NAMEST(NCALL) == 'FMSUB' .OR. NAMEST(NCALL) == 'FMSUB_R1' .OR.  &
              NAMEST(NCALL) == 'FMSUB_R2') THEN
              IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0)  &
                  MC%MP(1) = -MC%MP(1)
              KFLAG = KFSAVE
          ENDIF
          RETURN
      ENDIF

      IF (KRESLT == 3 .OR. KRESLT == 4) THEN
          CALL FMIM(0,MC)
          MC%MP(2) = MEXPOV
          MC%MP(3) = 1
          IF (KRESLT == 3) MC%MP(1) = -1
          KFLAG = KFSAVE
          RETURN
      ENDIF

      IF (KRESLT == 5 .OR. KRESLT == 6) THEN
          CALL FMIM(0,MC)
          MC%MP(2) = MEXPUN
          MC%MP(3) = 1
          IF (KRESLT == 5) MC%MP(1) = -1
          KFLAG = KFSAVE
          RETURN
      ENDIF

      IF (KRESLT == 7) THEN
          CALL FMIM(-1,MC)
          KFLAG = KFSAVE
          RETURN
      ENDIF

      IF (KRESLT == 8) THEN
          CALL FMIM(1,MC)
          KFLAG = KFSAVE
          RETURN
      ENDIF

      IF (KRESLT == 11) THEN
          CALL FMIM(0,MC)
          KFLAG = KFSAVE
          RETURN
      ENDIF

      IF (KRESLT == 12 .OR. KRESLT < 0 .OR. KRESLT > 15) THEN
          CALL FMIM(0,MC)
          MC%MP(2) = MUNKNO
          MC%MP(3) = 1
          KFLAG = KFSAVE
          RETURN
      ENDIF

      RETURN
      END SUBROUTINE FMRSLT

      SUBROUTINE FMSETVAR(STRING)

!  Change the value of one of the internal FM variables.
!  STRING must have the format  ' variablename = value ', with no embedded blanks in variablename.

      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: STRING
      CHARACTER(9) :: VARNAME
      INTEGER :: IVAL,J,KPTEQ,KPT1,KPT2
      DOUBLE PRECISION :: DVAL
      REAL (KIND(1.0D0)) :: MVAL

      CHARACTER(52) :: LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
      INTENT (IN) :: STRING

!             Find the equal sign.

      KPTEQ = INDEX(STRING,'=')
      IF (KPTEQ <= 0) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' Cannot find the equal sign in FMSETVAR.'
          WRITE (KW,*) ' Input string:  ',STRING
          RETURN
      ENDIF

!             Find the variable name.

      KPT1 = 0
      KPT2 = 0
      DO J = 1, KPTEQ-1
         IF (KPT1 == 0 .AND. STRING(J:J) /= ' ') KPT1 = J
      ENDDO
      DO J = KPTEQ-1, 1, -1
         IF (KPT2 == 0 .AND. STRING(J:J) /= ' ') KPT2 = J
      ENDDO
      IF (KPT1 == 0) THEN
          WRITE (KW,*) ' '
          WRITE (KW,*) ' Cannot find the variable name in FMSETVAR.'
          WRITE (KW,*) ' Input string:  ',STRING
          RETURN
      ENDIF
      VARNAME = ' '
      DO J = KPT1, KPT2
         IVAL = INDEX(LETTERS,STRING(J:J))
         IF (IVAL > 26 .AND. IVAL <= 52) THEN
             VARNAME(J-KPT1+1:J-KPT1+1) = LETTERS(IVAL-26:IVAL-26)
         ELSE
             VARNAME(J-KPT1+1:J-KPT1+1) = STRING(J:J)
         ENDIF
      ENDDO

!             CMCHAR is a special case, since the value is a character.

      IF (VARNAME == 'CMCHAR') THEN
          KPT1 = 0
          KPT2 = 0
          DO J = KPTEQ+1, LEN(STRING)
             IF (KPT1 == 0 .AND. STRING(J:J) /= ' ') KPT1 = J
          ENDDO
          DO J = LEN(STRING), KPTEQ+1, -1
             IF (KPT2 == 0 .AND. STRING(J:J) /= ' ') KPT2 = J
          ENDDO
          IF (KPT1 == KPT2 .AND. INDEX(LETTERS,STRING(KPT1:KPT2)) > 0) THEN
              CMCHAR = STRING(KPT1:KPT2)
              RETURN
          ELSE
              WRITE (KW,*) ' '
              WRITE (KW,*) ' Only a single letter is allowed after the equal sign in FMSETVAR.'
              WRITE (KW,*) ' Input string:  ',STRING
              RETURN
          ENDIF
      ENDIF

!             Convert the value after the equal sign.

      IF (KPTEQ+1 <= LEN(STRING)) THEN
          IF (INDEX(STRING(KPTEQ+1:LEN(STRING)),'=') /= 0) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' Only a single equal sign is allowed in FMSETVAR.'
              WRITE (KW,*) ' Input string:  ',STRING
              RETURN
          ENDIF
          CALL FMST2D(STRING(KPTEQ+1:LEN(STRING)),DVAL)
          IF (KFLAG /= 0) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' Invalid value after the equal sign in FMSETVAR.'
              WRITE (KW,*) ' Input string:  ',STRING
              RETURN
          ENDIF
      ELSE
          WRITE (KW,*) ' '
          WRITE (KW,*) ' Cannot find a value after the equal sign in FMSETVAR.'
          WRITE (KW,*) ' Input string:  ',STRING
          RETURN
      ENDIF

!             Check the list of variable names.

      IF (VARNAME == 'JFORM1') THEN
          JFORM1 = NINT(DVAL)
          IF (JFORM1 < 0 .OR. JFORM1 > 2) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',JFORM1,' is an invalid value for JFORM1'
              JFORM1 = 1
              WRITE (KW,*) '            Valid values are 0,1,2.  JFORM1 was set to ',JFORM1
          ENDIF
      ELSE IF (VARNAME == 'JFORM2') THEN
          JFORM2 = NINT(DVAL)
          IF (JFORM2 < 0) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',JFORM2,' is an invalid value for JFORM2'
              JFORM2 = 1
              WRITE (KW,*) '            It should be nonnegative.  JFORM2 was set to ',JFORM2
          ENDIF
      ELSE IF (VARNAME == 'JFORMZ') THEN
          JFORMZ = NINT(DVAL)
          IF (JFORMZ < 1 .OR. JFORMZ > 3) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',JFORMZ,' is an invalid value for JFORMZ'
              JFORMZ = 1
              WRITE (KW,*) '            Valid values are 1,2,3.  JFORMZ was set to ',JFORMZ
          ENDIF
      ELSE IF (VARNAME == 'JPRNTZ') THEN
          JPRNTZ = NINT(DVAL)
          IF (JPRNTZ < 1 .OR. JPRNTZ > 2) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',JPRNTZ,' is an invalid value for JPRNTZ'
              JPRNTZ = 1
              WRITE (KW,*) '            Valid values are 1,2.  JPRNTZ was set to ',JPRNTZ
          ENDIF
      ELSE IF (VARNAME == 'KDEBUG') THEN
          KDEBUG = NINT(DVAL)
          IF (KDEBUG < 0 .OR. KDEBUG > 1) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',KDEBUG,' is an invalid value for KDEBUG'
              KDEBUG = 1
              WRITE (KW,*) '            Valid values are 0,1.  KDEBUG was set to ',KDEBUG
          ENDIF
      ELSE IF (VARNAME == 'KESWCH') THEN
          KESWCH = NINT(DVAL)
          IF (KESWCH < 0 .OR. KESWCH > 1) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',KESWCH,' is an invalid value for KESWCH'
              KESWCH = 1
              WRITE (KW,*) '            Valid values are 0,1.  KESWCH was set to ',KESWCH
          ENDIF
      ELSE IF (VARNAME == 'KRAD') THEN
          KRAD = NINT(DVAL)
          IF (KRAD < 0 .OR. KRAD > 1) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',KRAD,' is an invalid value for KRAD'
              KRAD = 1
              WRITE (KW,*) '            Valid values are 0,1.  KRAD was set to ',KRAD
          ENDIF
      ELSE IF (VARNAME == 'KROUND') THEN
          KROUND = NINT(DVAL)
          IF (KROUND < -1 .OR. KROUND > 2) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',KROUND,' is an invalid value for KROUND'
              KROUND = 1
              WRITE (KW,*) '            Valid values are -1,0,1,2.  KROUND was set to ',KROUND
          ENDIF
      ELSE IF (VARNAME == 'KRPERF') THEN
          KRPERF = NINT(DVAL)
          IF (KRPERF < 0 .OR. KRPERF > 1) THEN
              KRPERF = 0
          ENDIF
          WRITE (KW,*) ' '
          WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
          WRITE (KW,*) '            KRPERF is no longer used.  Now perfect rounding is ',  &
                       'always done.'
          WRITE (KW,*) ' '
      ELSE IF (VARNAME == 'KSWIDE') THEN
          KSWIDE = NINT(DVAL)
          IF (KSWIDE < 10) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',KSWIDE,' is an invalid value for KSWIDE'
              KSWIDE = 80
              WRITE (KW,*) '            It should be 10 or more.  KSWIDE was set to ',KSWIDE
          ENDIF
      ELSE IF (VARNAME == 'KW') THEN
          KW = NINT(DVAL)
      ELSE IF (VARNAME == 'KWARN') THEN
          KWARN = NINT(DVAL)
          IF (KWARN < 0 .OR. KWARN > 2) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',KWARN,' is an invalid value for KWARN'
              KWARN = 1
              WRITE (KW,*) '            Valid values are 0,1,2.  KWARN was set to ',KWARN
          ENDIF
      ELSE IF (VARNAME == 'LVLTRC') THEN
          LVLTRC = NINT(DVAL)
          IF (LVLTRC < 0) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',LVLTRC,' is an invalid value for LVLTRC'
              LVLTRC = 1
              WRITE (KW,*) '            It should be nonnegative.  LVLTRC was set to ',LVLTRC
          ENDIF
      ELSE IF (VARNAME == 'NDIG') THEN
          IVAL = NDIG
          NDIG = NINT(DVAL)
          IF (NDIG < 2) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',NDIG,' is an invalid value for NDIG'
              NDIG = IVAL
              WRITE (KW,*) '            It should be > 1.  NDIG was not changed from ',NDIG
          ENDIF
      ELSE IF (VARNAME == 'NTRACE') THEN
          NTRACE = NINT(DVAL)
          IF (NTRACE < -2 .OR. NTRACE > 2) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',NTRACE,' is an invalid value for NTRACE'
              NTRACE = 0
              WRITE (KW,*) '            Valid values are -2,-1,0,1,2.  NTRACE was set to ',NTRACE
          ENDIF
      ELSE IF (VARNAME == 'MBASE') THEN
          MVAL = MBASE
          MBASE = AINT (0.5 + DVAL)
          IF (MBASE < 2) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',MBASE,' is an invalid value for MBASE'
              MBASE = MVAL
              WRITE (KW,*) '            It should be > 1.  MBASE was not changed from ',MBASE
          ENDIF
          IF (MBASE > MXBASE) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',MBASE,' is an invalid value for MBASE'
              MBASE = MVAL
              WRITE (KW,*) '            It should be <= ',MXBASE,  &
                           '.  MBASE was not changed from ',MBASE
          ENDIF
      ELSE IF (VARNAME == 'MXEXP') THEN
          MXEXP = AINT (DVAL)
          IF (MXEXP < 10 .OR. MXEXP > MXEXP2/2.01D0) THEN
              WRITE (KW,*) ' '
              WRITE (KW,*) ' FMSETVAR:  Input string:  ',STRING
              WRITE (KW,*) '            ',MXEXP,' is an invalid value for MXEXP'
              MXEXP = INT(MXEXP2/2.01D0)
              WRITE (KW,*) '            Valid values are 10 to ',  &
                           INT(MXEXP2/2.01D0),'  MXEXP was set to ',MXEXP
          ENDIF
      ELSE
          WRITE (KW,*) ' Variable name not recognized in FMSETVAR.'
          WRITE (KW,*) ' Input string:  ',STRING
          RETURN
      ENDIF

      CALL FMCONS
      RETURN
      END SUBROUTINE FMSETVAR

      SUBROUTINE FMSIGN(MA,MB,MC)

!  MC = SIGN(MA,MB)

!  MC is set to ABS(MA) if MB is positive or zero, or -ABS(MA) if MB is negative.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      INTEGER :: KWRNSV

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMSIGN'
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)

      KWRNSV = KWARN
      KWARN = 0
      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL FMST2M('UNKNOWN',MC)
          KFLAG = -4
      ELSE IF (MB%MP(1) >= 0) THEN
          CALL FMEQ(MA,MC)
          MC%MP(1) = 1
      ELSE
          CALL FMEQ(MA,MC)
          IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0) MC%MP(1) = -1
      ENDIF

      KWARN = KWRNSV
      IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMSIGN

      SUBROUTINE FMSIN(MA,MB)

!  MB = SIN(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAS,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JCOS,JSIN,JSWAP,K,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NDSAVE,NDSV
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(6)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG .AND. KRAD == 1) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMSIN'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMMPY(MXY(1),MA,MXY(2))
          CALL FMDIVI(MXY(2),-6,MXY(3))
          IF (MXY(3)%MP(2) > MEXPUN) THEN
              CALL FMADD(MA,MXY(3),MB)
          ELSE IF (MA%MP(2) == MEXPUN) THEN
              CALL FMEQ(MA,MB)
          ELSE IF (MA%MP(1) < 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMSUB(MXY(1),MXY(2),MXY(3))
              MXY(3)%MP(2) = MA%MP(2) + MXY(3)%MP(2)
              CALL FMEQ(MXY(3),MB)
          ELSE IF (MA%MP(1) >= 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMSUB(MXY(1),MXY(2),MXY(3))
              MXY(3)%MP(2) = MA%MP(2) + MXY(3)%MP(2)
              CALL FMEQ(MXY(3),MB)
          ELSE
              CALL FMEQ(MA,MB)
          ENDIF
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (MB%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMSIN'
              KFLAG = -4
              CALL FMWARN
              NCALL = NCALL - 1
          ELSE IF (ABS(MB%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMSIN'
              IF (MB%MP(2) == MEXPOV) KFLAG = -5
              IF (MB%MP(2) == MEXPUN) KFLAG = -6
              CALL FMWARN
              NCALL = NCALL - 1
          ENDIF
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMSIN'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0) THEN
          CALL FMENTR('FMSIN    ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMSIN'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      MAS = MA%MP(1)
      CALL FMEQU(MA,MXY(6),NDSAVE,NDIG)
      MXY(6)%MP(1) = 1
      CALL FMEQ(MXY(6),MXY(5))
      IF (MA%MP(2) > 3*10**5 .AND. KRAD == 1) THEN
          KFLAG = -4
          CALL FMST2M('UNKNOWN',MXY(6))
          GO TO 120
      ENDIF

!             Reduce the argument, convert to radians if the input is in degrees, and evaluate
!             the function.

      KWRNSV = KWARN
      KWARN = 0
      CALL FMRDC(MXY(6),JSIN,JCOS,JSWAP)
      IF (KROUND /= 1 .AND. KRAD /= 1) THEN
          CALL FMI2M(30,MXY(1))
          CALL FMSUB(MXY(6),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0 .AND. JSWAP == 0) THEN
              CALL FMST2M('0.5',MXY(1))
              CALL FMMPYI(MXY(1),JSIN,MXY(6))
              GO TO 120
          ENDIF
      ENDIF
      KWARN = KWRNSV
      IF (MXY(6)%MP(2) == MUNKNO) THEN
          IF (KRAD /= 1 .OR. JSWAP == 0) THEN
              CALL FMEQ(MXY(5),MXY(6))
              CALL FMRDC(MXY(6),JSIN,JCOS,JSWAP)
              GO TO 120
          ENDIF
          IF (MBSPI /= MBASE .OR. NDIGPI < NDIG)  THEN
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMPI(MXY(3))
              NDIG = NDSV
          ENDIF
          CALL FMDIV(MXY(5),MPISAV,MXY(3))
          CALL FMMPYI_R1(MXY(3),2)
          CALL FMNINT(MXY(3),MXY(2))
          CALL FMMPY(MXY(2),MPISAV,MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMSUB_R2(MXY(5),MXY(1))
          IF (MXY(1)%MP(3) == 0) CALL FMULP(MXY(5),MXY(1))
          CALL FMI2M(1,MXY(3))
          CALL FMSQR_R1(MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMSUB_R2(MXY(3),MXY(1))
          CALL FMSUB_R1(MXY(1),MXY(3))
          IF (MXY(1)%MP(3) == 0) THEN
              CALL FMI2M(JSIN,MXY(6))
          ELSE
              CALL FMEQ(MXY(5),MXY(6))
              CALL FMRDC(MXY(6),JSIN,JCOS,JSWAP)
          ENDIF
          GO TO 120
      ENDIF
      IF (KRAD == 0) THEN
          IF (MBSPI /= MBASE .OR. NDIGPI < NDIG)  THEN
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMPI(MXY(4))
              NDIG = NDSV
          ENDIF
          CALL FMMPY_R1(MXY(6),MPISAV)
          CALL FMDIVI_R1(MXY(6),180)
      ENDIF
      IF (MXY(6)%MP(2) /= MUNKNO) THEN
          IF (JSWAP == 0) THEN
              IF (MXY(6)%MP(2) < 0 .OR. NDIG <= 50) THEN
                  CALL FMSIN2(MXY(6),MXY(4))
                  CALL FMEQ(MXY(4),MXY(6))
              ELSE
                  CALL FMCOS2(MXY(6),MXY(4))
                  CALL FMI2M(1,MXY(2))
                  CALL FMSQR_R1(MXY(4))
                  CALL FMSUB_R2(MXY(2),MXY(4))
                  CALL FMSQRT(MXY(4),MXY(6))
              ENDIF
          ELSE
              CALL FMCOS2(MXY(6),MXY(4))
              CALL FMEQ(MXY(4),MXY(6))
          ENDIF
      ENDIF

!             Append the sign, round, and return.

      IF (JSIN == -1 .AND. MXY(6)%MP(2) /= MUNKNO .AND. MXY(6)%MP(3) /= 0)  &
          MXY(6)%MP(1) = -MXY(6)%MP(1)
  120 IF (MAS < 0 .AND. MXY(6)%MP(2) /= MUNKNO .AND. MXY(6)%MP(3) /= 0)  &
          MXY(6)%MP(1) = -MXY(6)%MP(1)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(6)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(6),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMSIN

      SUBROUTINE FMSIN2(MA,MB)

!  Internal subroutine for MB = SIN(MA) where 0 <= MA <= 1.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAXV
      INTEGER :: J,J2,K,K2,KTHREE,KWRNSV,L,L2,LARGE,N2,NBOT,NDSAV1,NDSAVE,NTERM
      REAL :: ALOG3,ALOGT,B,T,TJ
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (MA%MP(3) == 0) THEN
          CALL FMEQ(MA,MB)
          RETURN
      ENDIF
      NDSAVE = NDIG
      KWRNSV = KWARN
      KWARN = 0

!             Use the direct series
!                  SIN(X) = X - X**3/3! + X**5/5! - ...

!             The argument will be divided by 3**K2 before the series is summed.  The series will be
!             added as J2 concurrent series.

      B = REAL(MBASE)
      K = NGRD52
      T = MAX(NDIG-K,2)
      ALOG3 = LOG(3.0)
      ALOGT = LOG(T)
      TJ = 0.55*(NDIG*ALOGMT)**0.3333
      J2 = INT(TJ)
      J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      K2 = MAX(1,INT(0.62*(NDIG*ALOGMT)**0.3333 - 0.8))

      TJ = -(REAL(MA%MP(2))*ALOGMB+LOG(REAL(MA%MP(3))/B +  &
             REAL(MA%MP(4))/(B*B)))/ALOG3 - 0.3
      IF (TJ >= K2) THEN
          L = K2
      ELSE IF (TJ > 0) THEN
          L = INT(TJ)
      ELSE
          L = 0
      ENDIF
      K2 = K2 - L
      IF (K2 <= 0) THEN
          K2 = 0
          J2 = INT(.43*SQRT(T*ALOGMB/(ALOGT+REAL(L)*ALOG3)) + .33)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ENDIF

      N2 = INT(T*ALOGMB/(ALOGT+REAL(L)*ALOG3))
      L2 = INT((LOG(1+REAL(N2)/3.0D0**K2)+K2*LOG(3.0D0))/ALOGMB)
      NDIG = NDIG + L2
      NDSAV1 = NDIG

!             Divide the argument by 3**K2.

      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      KTHREE = 1
      MAXV = MXBASE/3
      IF (K2 > 0) THEN
          DO J = 1, K2
             KTHREE = 3*KTHREE
             IF (KTHREE > MAXV) THEN
                 CALL FMCSDIVI_R1(MXY(1),KTHREE)
                 KTHREE = 1
             ENDIF
          ENDDO
          IF (KTHREE > 1) CALL FMCSDIVI_R1(MXY(1),KTHREE)
      ENDIF

!             Split into J2 concurrent sums and reduce NDIG while computing each term in the sum
!             as the terms get smaller.

      CALL FMI2M(1,MJSUMS(1))
      CALL FMI2M(1,MXY(2))
      NTERM = 3
      DO J = 2, J2
         NBOT = NTERM*(NTERM-1)
         CALL FMCSDIVI_R1(MXY(2),NBOT)
         CALL FMEQ(MXY(2),MJSUMS(J))
         NTERM = NTERM + 2
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 120
      CALL FMIPWR(MXY(1),2*J2,MXY(3))
      IF (MXY(3)%MP(2) < -NDIG) GO TO 120

  110 CALL FMCSMPY_R1(MXY(2),MXY(3))
      DO J = 1, J2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(2),NTERM)
             NBOT = NTERM - 1
             CALL FMCSDIVI_R1(MXY(2),NBOT)
         ELSE
             NBOT = NTERM*(NTERM-1)
             CALL FMCSDIVI_R1(MXY(2),NBOT)
         ENDIF
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(2))
         IF (KFLAG /= 0) GO TO 120
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(2)%MP(2))
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 2
      ENDDO
      GO TO 110

!             Put the J2 separate sums back together.

  120 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMSQR(MXY(1),MXY(2))
      IF (MXY(2)%MP(2) /= MUNKNO .AND. MXY(2)%MP(3) /= 0)  &
          MXY(2)%MP(1) = -MXY(2)%MP(1)
      CALL FMEQ(MJSUMS(J2),MXY(3))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(3),MXY(2))
         CALL FMADD_R1(MXY(3),MJSUMS(J2-J+1))
      ENDDO
      CALL FMCSMPY_R1(MXY(3),MXY(1))

!             Reverse the effect of reducing the argument to compute SIN(MA).

      NDIG = NDSAV1
      IF (K2 > 0) THEN
          CALL FMI2M(3,MXY(1))
          DO J = 1, K2
             CALL FMSQR(MXY(3),MXY(2))
             CALL FMCSMPYI_R1(MXY(2),-4)
             CALL FMADD_R2(MXY(1),MXY(2))
             CALL FMCSMPY_R1(MXY(3),MXY(2))
          ENDDO
      ENDIF

      CALL FMEQU(MXY(3),MB,NDSAV1,NDSAVE)
      NDIG = NDSAVE
      KWARN = KWRNSV

      RETURN
      END SUBROUTINE FMSIN2

      SUBROUTINE FMSINH(MA,MB)

!  MB = SINH(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAS,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE,NMETHD
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMSINH'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMMPY(MXY(1),MA,MXY(2))
          CALL FMDIVI(MXY(2),6,MXY(3))
          IF (MXY(3)%MP(2) > MEXPUN) THEN
              CALL FMADD(MA,MXY(3),MB)
          ELSE IF (MA%MP(2) == MEXPUN) THEN
              CALL FMEQ(MA,MB)
          ELSE IF (MA%MP(1) < 0 .AND. KROUND == -1) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMADD(MXY(1),MXY(2),MB)
              MB%MP(2) = MA%MP(2) + MB%MP(2)
          ELSE IF (MA%MP(1) >= 0 .AND. KROUND == 2) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMADD(MXY(1),MXY(2),MB)
              MB%MP(2) = MA%MP(2) + MB%MP(2)
          ELSE
              CALL FMEQ(MA,MB)
          ENDIF
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMSINH'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMENTR('FMSINH   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMSINH'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      MAS = MA%MP(1)
      CALL FMEQU(MA,MXY(3),NDSAVE,NDIG)
      IF (MA%MP(3) == 0) THEN
          GO TO 120
      ENDIF
      MXY(3)%MP(1) = 1

!             Use a series for small arguments, FMEXP for large ones.

      IF (MXY(3)%MP(2) == MUNKNO) GO TO 120
      IF (MBASE > 99) THEN
          IF (MXY(3)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE IF (MXY(3)%MP(2) >= 2) THEN
              NMETHD = 2
          ELSE IF (ABS(MXY(3)%MP(3)) < 10) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          IF (MXY(3)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ENDIF

      IF (NMETHD == 1) THEN
          IF (MXY(3)%MP(2) < 0 .OR. NDIG <= 50) THEN
              CALL FMSNH2(MXY(3),MXY(4))
              CALL FMEQ(MXY(4),MXY(3))
          ELSE
              CALL FMCSH2(MXY(3),MXY(4))
              CALL FMI2M(1,MXY(2))
              CALL FMSQR_R1(MXY(4))
              CALL FMSUB_R1(MXY(4),MXY(2))
              CALL FMSQRT(MXY(4),MXY(3))
          ENDIF
      ELSE
          CALL FMEXP(MXY(3),MXY(5))
          CALL FMEQ(MXY(5),MXY(3))
          IF (MXY(3)%MP(2) == MEXPOV) THEN
              GO TO 120
          ELSE IF (MXY(3)%MP(2) == MEXPUN) THEN
              MXY(3)%MP(2) = MEXPOV
              GO TO 120
          ENDIF
          IF (INT(MXY(3)%MP(2)) <= (NDIG+1)/2) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMDIV_R1(MXY(1),MXY(3))
              CALL FMSUB_R1(MXY(3),MXY(1))
          ENDIF
          CALL FMDIVI_R1(MXY(3),2)
      ENDIF

!             Round and return.

  120 IF (MAS < 0 .AND. MXY(3)%MP(2) /= MUNKNO .AND. MXY(3)%MP(3) /= 0)  &
          MXY(3)%MP(1) = -MXY(3)%MP(1)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMSINH

      SUBROUTINE FMSNH2(MA,MB)

!  Internal subroutine for MB = SINH(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAXV
      INTEGER :: J,J2,K,K2,KTHREE,KWRNSV,L,L2,LARGE,N2,NBOT,NDSAV1,NDSAVE,NTERM
      REAL :: ALOG3,ALOGT,B,T,TJ
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (MA%MP(3) == 0) THEN
          CALL FMEQ(MA,MB)
          RETURN
      ENDIF
      NDSAVE = NDIG
      KWRNSV = KWARN
      KWARN = 0

!             Use the direct series
!                  SINH(X) = X + X**3/3! + X**5/5! - ...

!             The argument will be divided by 3**K2 before the series is summed.  The series will be
!             added as J2 concurrent series.

      B = REAL(MBASE)
      K = NGRD52
      T = MAX(NDIG-K,2)
      ALOG3 = LOG(3.0)
      ALOGT = LOG(T)
      TJ = 0.67*(NDIG*ALOGMT)**0.3333 - 0.3
      J2 = INT(TJ)
      J2 = MAX(2,MIN(J2,LJSUMS))
      K2 = MAX(2,INT(0.6*(NDIG*ALOGMT)**0.3333 - 0.8))

      TJ = -(REAL(MA%MP(2))*ALOGMB+LOG(REAL(MA%MP(3))/B +  &
             REAL(MA%MP(4))/(B*B)))/ALOG3 - 0.3
      IF (TJ >= K2) THEN
          L = K2
      ELSE IF (TJ > 0) THEN
          L = INT(TJ)
      ELSE
          L = 0
      ENDIF
      K2 = K2 - L
      IF (K2 <= 0) THEN
          K2 = 0
          J2 = INT(.43*SQRT(T*ALOGMB/(ALOGT+REAL(L)*ALOG3)) + .33)
          J2 = MAX(1,MIN(J2,LJSUMS))
      ENDIF

      N2 = INT(T*ALOGMB/(ALOGT+REAL(L)*ALOG3))
      L2 = INT(LOG(REAL(N2)+3.0D0**K2)/ALOGMB)
      NDIG = NDIG + L2
      NDSAV1 = NDIG

!             Divide the argument by 3**K2.

      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      KTHREE = 1
      MAXV = MXBASE/3
      IF (K2 > 0) THEN
          DO J = 1, K2
             KTHREE = 3*KTHREE
             IF (KTHREE > MAXV) THEN
                 CALL FMCSDIVI_R1(MXY(1),KTHREE)
                 KTHREE = 1
             ENDIF
          ENDDO
          IF (KTHREE > 1) CALL FMCSDIVI_R1(MXY(1),KTHREE)
      ENDIF

!             Split into J2 concurrent sums and reduce NDIG while computing each term in the sum
!             as the terms get smaller.

      CALL FMEQ(MXY(1),MXY(2))
      NTERM = 1
      DO J = 1, J2
         NBOT = NTERM*(NTERM-1)
         IF (NBOT > 1) CALL FMCSDIVI_R1(MXY(2),NBOT)
         NTERM = NTERM + 2
         CALL FMEQ(MXY(2),MJSUMS(J))
      ENDDO
      CALL FMSQR_R1(MXY(1))
      IF (MXY(1)%MP(2) < -NDIG) GO TO 120
      CALL FMIPWR(MXY(1),J2,MXY(3))

  110 CALL FMCSMPY_R1(MXY(2),MXY(3))
      DO J = 1, J2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(2),NTERM)
             NBOT = NTERM - 1
             CALL FMCSDIVI_R1(MXY(2),NBOT)
         ELSE
             NBOT = NTERM*(NTERM-1)
             CALL FMCSDIVI_R1(MXY(2),NBOT)
         ENDIF
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(2))
         IF (KFLAG /= 0) GO TO 120
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(2)%MP(2))
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 2
      ENDDO
      GO TO 110

!             Put the J2 separate sums back together.

  120 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(3))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(3),MXY(1))
         CALL FMADD_R1(MXY(3),MJSUMS(J2-J+1))
      ENDDO

!             Reverse the effect of reducing the argument to compute SINH(MA).

      NDIG = NDSAV1
      IF (K2 > 0) THEN
          CALL FMI2M(3,MXY(1))
          DO J = 1, K2
             CALL FMSQR(MXY(3),MXY(2))
             CALL FMCSMPYI_R1(MXY(2),4)
             CALL FMADD_R2(MXY(1),MXY(2))
             CALL FMCSMPY_R1(MXY(3),MXY(2))
          ENDDO
      ENDIF

      CALL FMEQU(MXY(3),MB,NDSAV1,NDSAVE)
      NDIG = NDSAVE
      KWARN = KWRNSV

      RETURN
      END SUBROUTINE FMSNH2

      SUBROUTINE FMSP2M(X,MA)

!  MA = X

!  Convert a single precision number to FM format.

!  This version tries to convert the single precision machine number to FM with accuracy of nearly
!  full FM precision.
!  If conversion to FM with approximately double precision accuracy is good enough, it is faster to
!  CALL FMDPM(DBLE(X),MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      REAL :: X
      TYPE(MULTI) :: MA

      DOUBLE PRECISION :: XDP,Y,YT
      INTEGER :: J,K
      INTENT (IN) :: X
      INTENT (INOUT) :: MA

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMSP2M'
      XDP = DBLE(X)
      IF (NTRACE /= 0) CALL FMNTRR(2,XDP,1)

!             Check for X = + or - Infinity, or NaN.  Return unknown if so.

      IF (X > HUGE(X) .OR. X < -HUGE(X) .OR. (.NOT.(X == X))) THEN
          DO J = 2, NDIG
             MA%MP(J+2) = 0
          ENDDO
          KFLAG = -4
          MA%MP(2) = MUNKNO
          MA%MP(3) = 1
          MA%MP(1) = 1
          CALL FMWARN
          GO TO 110
      ENDIF

!             Check to see if X is exactly a small integer.  If so, converting as an integer
!             is better.  Also see if X is exactly a small integer divided by a small power of two.

      Y = MXEXP2
      IF (ABS(XDP) < Y) THEN
          K = INT(XDP)
          Y = K
          IF (Y == XDP) THEN
              CALL FMIM(K,MA)
              GO TO 110
          ENDIF
      ENDIF
      IF (ABS(XDP) < 1.0D0) THEN
          Y = 4096.0D0 * XDP
          K = INT(Y)
          YT = K
          IF (Y == YT) THEN
              CALL FMIM(K,MA)
              CALL FMDIVI_R1(MA,4096)
              GO TO 110
          ENDIF
      ENDIF

      CALL FMDM2(XDP,MA)

  110 IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMSP2M

      SUBROUTINE FMSQR(MA,MB)

!  MB = MA*MA    Faster than using FMMPY.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMSQR'
          CALL FMNTR(2,MA,MA,1,1)

          CALL FMSQR2(MA,MB)

          CALL FMNTR(1,MB,MB,1,1)
      ELSE
          CALL FMSQR2(MA,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMSQR

      SUBROUTINE FMSQR2(MA,MB)

!  MB = MA*MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAXMAX,MAXMWA,MBJ,MBKJ,MBNORM,MBP1,MK,MKA,MKT,MMAX,MR,MT
      DOUBLE PRECISION :: ERR
      REAL :: C
      INTEGER :: J,JM1,JRSSAV,K,KB,KI,KJ,KL,KNZ,KOVUN,KR_RETRY,KSHIFT,KWA,L,N1,NGUARD,NMETHD,NZDA
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KSQR = 1
      IF (MBLOGS /= MBASE) CALL FMCONS
      KR_RETRY = 0
      JRSSAV = JRSIGN
      IF (ABS(MA%MP(2)) > MEXPAB .OR. KDEBUG == 1 .OR. MBASE*MBASE <= MXBASE/(4*MBASE)) THEN
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          IF (MA%MP(2) == MUNKNO) KOVUN = 2
          NCALL = NCALL + 1
          CALL FMMPY2(MA,MA,MB)
          NCALL = NCALL - 1
          IF ((KFLAG < 0 .AND. KOVUN == 0) .OR. (KFLAG == -4 .AND. KOVUN == 1)) THEN
              NAMEST(NCALL) = 'FMSQR'
              CALL FMWARN
          ENDIF
          GO TO 130
      ELSE IF (MA%MP(3) == 0) THEN
          CALL FMEQ(MA,MB)
          GO TO 130
      ENDIF
      KFLAG = 0

!             Check for using an FFT-based method if precision is very high.

      C = 1300
      IF (NDIG >= C) THEN
          NZDA = 0
          DO J = 2, NDIG
             IF (MA%MP(J+2) == 0) NZDA = NZDA + 1
          ENDDO
          IF (NDIG-NZDA < 50 .OR. REAL(NZDA)/NDIG > 0.8) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          NMETHD = 1
      ENDIF
      IF (NMETHD == 2) THEN
          CALL FMMPY2(MA,MA,MB)
          GO TO 130
      ENDIF

      MAXMAX = 0
      N1 = NDIG + 1
      MWA%MP(2) = MA%MP(2) + MA%MP(2)

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD22
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 2
          ELSE IF (MBASE < 10**6) THEN
              NGUARD = MIN(NGUARD+1,NDIG+2)
          ENDIF
      ENDIF
      IF (MA%MP(3)*MA%MP(3) < MBASE .AND. NGUARD < 3) NGUARD = 3

      L = N1 + NGUARD
      MWA%MP(L+2) = 0

!             The multiplication loop begins here.

!             MBNORM is the minimum number of digits that can be multiplied before normalization
!                    is required.
!             MAXMWA is an upper bound on the size of values in MWA divided by (MBASE-1).  It is
!                    used to determine whether to normalize before the next digit is multiplied.

      MBP1 = MBASE + 1
      MBNORM = (MAXINT/(MBP1*MBP1))
      MMAX = INTMAX - MBASE
      MMAX = MIN((MAXINT/MBP1 - MBP1),MMAX)
      IF (MBNORM >= 2) THEN
          MBJ = MA%MP(3)

!             Count the trailing zeros in MA.

          IF (MA%MP(N1+1) /= 0) THEN
              KNZ = N1
          ELSE
              DO J = NDIG, 2, -1
                 IF (MA%MP(J+1) /= 0) THEN
                     KNZ = J
                     GO TO 120
                 ENDIF
              ENDDO
          ENDIF

  120     MWA%MP(3) = 0
          MWA%MP(4) = 0
          DO K = NDIG+2, L
             MWA%MP(K+1) = 0
          ENDDO

!             (Inner Loop)

          DO K = 4, N1+1
             MWA%MP(K+1) = MA%MP(K)*MBJ
          ENDDO
          MAXMWA = MBJ
          DO J = 3, MIN(L/2,N1)
             MBJ = MA%MP(J+1)
             IF (MBJ /= 0) THEN
                 MAXMWA = MAXMWA + MBJ
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)

!                       Major (Inner Loop)

                 DO K = 2*J+1, JM1+KL+1
                    MWA%MP(K) = MWA%MP(K) + MA%MP(K-JM1)*MBJ
                 ENDDO
             ENDIF

             IF (MAXMWA > MMAX) THEN
                 MAXMAX = MAX(MAXMAX,MAXMWA)
                 MAXMWA = 0
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)

!                       Normalization is only required for the range of digits currently
!                       changing in MWA.

                 DO KB = JM1+KL, 2*J, -1
                    MKT = INT (MWA%MP(KB+1)/MBASE)
                    MWA%MP(KB) = MWA%MP(KB) + MKT
                    MWA%MP(KB+1) = MWA%MP(KB+1) - MKT*MBASE
                 ENDDO
             ENDIF
          ENDDO

!             Double MWA, add the square terms, and perform the final normalization.  (Inner Loop)

          IF (2*MAX(MAXMAX,MAXMWA)+MBASE > MMAX) THEN
              DO KB = L+1, 5, -1
                 MKT = INT (MWA%MP(KB)/MBASE)
                 MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
                 MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
              ENDDO
          ENDIF

          DO J = 4, L, 2
             IF (J/2 <= N1) THEN
                 MKA = MA%MP(1+J/2)
                 MWA%MP(J) = 2*MWA%MP(J) + MKA*MKA
                 MWA%MP(J+1) = 2*MWA%MP(J+1)
             ELSE
                 MWA%MP(J) = 2*MWA%MP(J)
                 MWA%MP(J+1) = 2*MWA%MP(J+1)
             ENDIF
          ENDDO
          IF (MOD(L,2) == 1) THEN
              IF ((L+1)/2 <= N1) THEN
                  MKA = MA%MP(1+(L+1)/2)
                  MWA%MP(L+1) = 2*MWA%MP(L+1) + MKA*MKA
              ELSE
                  MWA%MP(L+1) = 2*MWA%MP(L+1)
              ENDIF
          ENDIF

          DO KB = L+1, 4, -1
             MKT = INT (MWA%MP(KB)/MBASE)
             MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
             MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
          ENDDO

      ELSE

!             If normalization must be done for each digit, combine the two loops and normalize as
!             the digits are multiplied.

          DO J = 2, L
             MWA%MP(J+1) = 0
          ENDDO
          KJ = NDIG + 2
          DO J = 2, N1
             KJ = KJ - 1
             MBKJ = MA%MP(KJ+1)
             IF (MBKJ == 0) CYCLE
             KL = L - KJ + 1
             IF (KL > N1) KL = N1
             KI = KL + 2
             KWA = KL+ KJ + 1
             MK = 0
             DO K = 2, KL
                MT = MA%MP(KI-K+1)*MBKJ + MWA%MP(KWA-K+1) + MK
                MK = INT (MT/MBASE)
                MWA%MP(KWA-K+1) = MT - MBASE*MK
             ENDDO
             MWA%MP(KWA-KL) = MK
          ENDDO

      ENDIF

!             Set KSHIFT = 1 if a shift left is necessary.

      IF (MWA%MP(3) == 0) THEN
          KSHIFT = 1
      ELSE
          KSHIFT = 0
      ENDIF

!             The multiplication is complete.  Round the result and move it to MB.

      JRSIGN = 1

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NGUARD < NDIG+2) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      MR = 2*MWA%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWA%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(KSHIFT+N1+1) = MWA%MP(KSHIFT+N1+1) + 1
                  MWA%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MB)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMSQR'
          CALL FMWARN
      ENDIF

  130 MB%MP(1) = 1
      KSQR = 0
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMSQR2

      SUBROUTINE FMSQR_R1(MA)

!  MA = MA*MA    Faster than using FMMPY.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMSQR_R1'
          CALL FMNTR(2,MA,MA,1,1)

          CALL FMSQR2_R1(MA)

          NAMEST(NCALL) = 'FMSQR_R1'
          CALL FMNTR(1,MA,MA,1,1)
      ELSE
          CALL FMSQR2_R1(MA)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMSQR_R1

      SUBROUTINE FMSQR2_R1(MA)

!  MA = MA*MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA

      REAL (KIND(1.0D0)) :: MAXMAX,MAXMWA,MBJ,MBKJ,MBNORM,MBP1,MK,MKA,MKT,MMAX,MR,MT
      DOUBLE PRECISION :: ERR
      REAL :: C
      INTEGER :: J,JM1,JRSSAV,K,KB,KI,KJ,KL,KNZ,KOVUN,KR_RETRY,KSHIFT,KWA,L,N1,NGUARD,NMETHD,NZDA
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KSQR = 1
      IF (MBLOGS /= MBASE) CALL FMCONS
      KR_RETRY = 0
      JRSSAV = JRSIGN
      IF (ABS(MA%MP(2)) > MEXPAB .OR. KDEBUG == 1 .OR. MBASE*MBASE <= MXBASE/(4*MBASE)) THEN
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          IF (MA%MP(2) == MUNKNO) KOVUN = 2
          NCALL = NCALL + 1
          CALL FMMPY2(MA,MA,MXY(1))
          CALL FMEQ(MXY(1),MA)
          NCALL = NCALL - 1
          IF ((KFLAG < 0 .AND. KOVUN == 0) .OR. (KFLAG == -4 .AND. KOVUN == 1)) THEN
              NAMEST(NCALL) = 'FMSQR_R1'
              CALL FMWARN
          ENDIF
          GO TO 130
      ELSE IF (MA%MP(3) == 0) THEN
          GO TO 130
      ENDIF
      KFLAG = 0

!             Check for using an FFT-based method if precision is very high.

      C = 1300
      IF (NDIG >= C) THEN
          NZDA = 0
          DO J = 2, NDIG
             IF (MA%MP(J+2) == 0) NZDA = NZDA + 1
          ENDDO
          IF (NDIG-NZDA < 50 .OR. REAL(NZDA)/NDIG > 0.8) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          NMETHD = 1
      ENDIF
      IF (NMETHD == 2) THEN
          CALL FMMPY2(MA,MA,MXY(1))
          CALL FMEQ(MXY(1),MA)
          GO TO 130
      ENDIF

      MAXMAX = 0
      N1 = NDIG + 1
      MWA%MP(2) = MA%MP(2) + MA%MP(2)

!             NGUARD is the number of guard digits used.

  110 IF (NCALL > 1) THEN
          NGUARD = NGRD22
          IF (NGUARD > NDIG) NGUARD = NDIG
      ELSE
          NGUARD = NGRD52
          IF (NGUARD > NDIG) NGUARD = NDIG
          IF (KR_RETRY >= 1) THEN
              NGUARD = NDIG + 2
          ELSE IF (MBASE < 10**6) THEN
              NGUARD = MIN(NGUARD+1,NDIG+2)
          ENDIF
      ENDIF
      IF (MA%MP(3)*MA%MP(3) < MBASE .AND. NGUARD < 3) NGUARD = 3

      L = N1 + NGUARD
      MWA%MP(L+2) = 0

!             The multiplication loop begins here.

!             MBNORM is the minimum number of digits that can be multiplied before normalization
!                    is required.
!             MAXMWA is an upper bound on the size of values in MWA divided by (MBASE-1).  It is
!                    used to determine whether to normalize before the next digit is multiplied.

      MBP1 = MBASE + 1
      MBNORM = (MAXINT/(MBP1*MBP1))
      MMAX = INTMAX - MBASE
      MMAX = MIN((MAXINT/MBP1 - MBP1),MMAX)
      IF (MBNORM >= 2) THEN
          MBJ = MA%MP(3)

!             Count the trailing zeros in MA.

          IF (MA%MP(N1+1) /= 0) THEN
              KNZ = N1
          ELSE
              DO J = NDIG, 2, -1
                 IF (MA%MP(J+1) /= 0) THEN
                     KNZ = J
                     GO TO 120
                 ENDIF
              ENDDO
          ENDIF

  120     MWA%MP(3) = 0
          MWA%MP(4) = 0
          DO K = NDIG+2, L
             MWA%MP(K+1) = 0
          ENDDO

!             (Inner Loop)

          DO K = 4, N1+1
             MWA%MP(K+1) = MA%MP(K)*MBJ
          ENDDO
          MAXMWA = MBJ
          DO J = 3, MIN(L/2,N1)
             MBJ = MA%MP(J+1)
             IF (MBJ /= 0) THEN
                 MAXMWA = MAXMWA + MBJ
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)

!                       Major (Inner Loop)

                 DO K = 2*J+1, JM1+KL+1
                    MWA%MP(K) = MWA%MP(K) + MA%MP(K-JM1)*MBJ
                 ENDDO
             ENDIF

             IF (MAXMWA > MMAX) THEN
                 MAXMAX = MAX(MAXMAX,MAXMWA)
                 MAXMWA = 0
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)

!                       Normalization is only required for the range of digits currently
!                       changing in MWA.

                 DO KB = JM1+KL, 2*J, -1
                    MKT = INT (MWA%MP(KB+1)/MBASE)
                    MWA%MP(KB) = MWA%MP(KB) + MKT
                    MWA%MP(KB+1) = MWA%MP(KB+1) - MKT*MBASE
                 ENDDO
             ENDIF
          ENDDO

!             Double MWA, add the square terms, and perform the final normalization.  (Inner Loop)

          IF (2*MAX(MAXMAX,MAXMWA)+MBASE > MMAX) THEN
              DO KB = L+1, 5, -1
                 MKT = INT (MWA%MP(KB)/MBASE)
                 MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
                 MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
              ENDDO
          ENDIF

          DO J = 4, L, 2
             IF (J/2 <= N1) THEN
                 MKA = MA%MP(1+J/2)
                 MWA%MP(J) = 2*MWA%MP(J) + MKA*MKA
                 MWA%MP(J+1) = 2*MWA%MP(J+1)
             ELSE
                 MWA%MP(J) = 2*MWA%MP(J)
                 MWA%MP(J+1) = 2*MWA%MP(J+1)
             ENDIF
          ENDDO
          IF (MOD(L,2) == 1) THEN
              IF ((L+1)/2 <= N1) THEN
                  MKA = MA%MP(1+(L+1)/2)
                  MWA%MP(1+L) = 2*MWA%MP(1+L) + MKA*MKA
              ELSE
                  MWA%MP(1+L) = 2*MWA%MP(1+L)
              ENDIF
          ENDIF

          DO KB = L+1, 4, -1
             MKT = INT (MWA%MP(KB)/MBASE)
             MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
             MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
          ENDDO

      ELSE

!             If normalization must be done for each digit, combine the two loops and normalize
!             as the digits are multiplied.

          DO J = 2, L
             MWA%MP(J+1) = 0
          ENDDO
          KJ = NDIG + 2
          DO J = 2, N1
             KJ = KJ - 1
             MBKJ = MA%MP(KJ+1)
             IF (MBKJ == 0) CYCLE
             KL = L - KJ + 1
             IF (KL > N1) KL = N1
             KI = KL + 2
             KWA = KL+ KJ + 1
             MK = 0
             DO K = 2, KL
                MT = MA%MP(KI-K+1)*MBKJ + MWA%MP(KWA-K+1) + MK
                MK = INT (MT/MBASE)
                MWA%MP(KWA-K+1) = MT - MBASE*MK
             ENDDO
             MWA%MP(KWA-KL) = MK
          ENDDO

      ENDIF

!             Set KSHIFT = 1 if a shift left is necessary.

      IF (MWA%MP(3) == 0) THEN
          KSHIFT = 1
      ELSE
          KSHIFT = 0
      ENDIF

!             The multiplication is complete.  Round the result and move it to MA.

      JRSIGN = 1

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NGUARD,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MWA%MP(J+KSHIFT+NDIG+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NGUARD < NDIG+2) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      MR = 2*MWA%MP(KSHIFT+NDIG+3) + 1
      IF (KROUND == -1 .OR. KROUND == 2) THEN
          CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
      ELSE IF (MR >= MBASE) THEN
          IF (MR-1 > MBASE .AND. MWA%MP(KSHIFT+N1+1) < MBASE-1) THEN
              IF (KROUND /= 0) THEN
                  MWA%MP(KSHIFT+N1+1) = MWA%MP(KSHIFT+N1+1) + 1
                  MWA%MP(KSHIFT+N1+2) = 0
              ENDIF
          ELSE
              CALL FMRND(MWA,NDIG,NGUARD,KSHIFT)
          ENDIF
      ENDIF
      CALL FMMOVE(MWA,MA)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'FMSQR_R1'
          CALL FMWARN
      ENDIF

  130 MA%MP(1) = 1
      KSQR = 0
      JRSIGN = JRSSAV
      RETURN
      END SUBROUTINE FMSQR2_R1

      SUBROUTINE FMSQRT(MA,MB)

!  MB = SQRT(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: DC,DP,ERR,X,XB
      REAL (KIND(1.0D0)) :: MA1,MKE,MXSAVE
      INTEGER :: NSTACK(49),J,JPT,K,KL,KMA1,KN,KOVUN,KRESLT,KST,KR_RETRY,NDSAVE,NMETHD
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0 .OR. MA%MP(1) < 0) THEN
          CALL FMENTR2('FMSQRT   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'FMSQRT'
              CALL FMNTR(2,MA,MA,1,1)
          ENDIF
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0
  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF

      MA1 = MA%MP(2)

      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)

      NMETHD = 1
      IF (MBASE >= 1000 .AND. MBASE <= MXBASE/1.5D0 .AND. NDIG*ALOGMT < 1400) THEN
          NMETHD = 4
      ELSE IF (NDIG*ALOGMT > 50000) THEN
          NMETHD = 3
      ELSE IF (NDIG*ALOGMT > 2750) THEN
          NMETHD = 2
      ENDIF
      IF (NMETHD == 2) GO TO 130
      IF (NMETHD == 3) GO TO 140
      IF (NMETHD == 4) GO TO 150

!             Method 1.  Ordinary Newton iteration.

!             Generate the first approximation.

  120 MXY(2)%MP(2) = 0
      CALL FMM2DP(MXY(2),X)
      X = SQRT(X)
      MKE = MA1/2
      KMA1 = INT(ABS(MA1))
      IF (MOD(KMA1,2) == 1) THEN
          XB = MBASE
          X = X*SQRT(XB)
          MKE = (MA1-1)/2
      ENDIF
      CALL FMDPM(X,MXY(3))
      MXY(3)%MP(2) = MXY(3)%MP(2) + MKE

!             Initialize.

      CALL FMI2M(0,MXY(1))
      MXY(2)%MP(2) = MA1
      CALL FMDIG(NSTACK,KST)

!             Newton iteration.

      DO J = 1, KST
         NDIG = NSTACK(J)
         CALL FMCSDIV(MXY(2),MXY(3),MXY(1))
         CALL FMCSADD_R1(MXY(3),MXY(1))
         CALL FMCSDIVI_R1(MXY(3),2)
      ENDDO

      GO TO 160

!             Method 2.  Modified Newton iteration.
!                        x2 = x1 - (x1^2 - a) / (2*x1)
!                        where the square is done at full current precision,
!                        and the division is done at half current precision.

!             Generate the first approximation.

  130 MXY(2)%MP(2) = 0
      CALL FMM2DP(MXY(2),X)
      X = SQRT(X)
      MKE = MA1/2
      KMA1 = INT(ABS(MA1))
      IF (MOD(KMA1,2) == 1) THEN
          XB = MBASE
          X = X*SQRT(XB)
          MKE = (MA1-1)/2
      ENDIF
      CALL FMDPM(X,MXY(3))
      MXY(3)%MP(2) = MXY(3)%MP(2) + MKE

!             Initialize.

      CALL FMI2M(0,MXY(1))
      CALL FMI2M(0,MXY(4))
      MXY(2)%MP(2) = MA1
      CALL FMDIG(NSTACK,KST)

!             Newton iteration.

      DO J = 1, KST
         NDIG = NSTACK(J)
         CALL FMSQR(MXY(3),MXY(1))
         CALL FMSUB_R1(MXY(1),MXY(2))
         IF (J > 1) NDIG = NSTACK(J-1)
         CALL FMADD(MXY(3),MXY(3),MXY(4))
         CALL FMDIV_R2(MXY(1),MXY(4))
         NDIG = NSTACK(J)
         CALL FMSUB_R1(MXY(3),MXY(4))
      ENDDO

      GO TO 160

!             Method 3.  Karp's method.  Newton iteration for 1/sqrt(a)
!                        x2 = x1 + (1 - a*x1**2)*x1/2
!                        where a*x1**2 is done at full current precision,
!                        and *x1/2 is done at half current precision.

!             Generate the first approximation to 1/sqrt(a).

  140 MXY(2)%MP(2) = 0
      CALL FMM2DP(MXY(2),X)
      X = 1.0D0/SQRT(X)
      MKE = -MA1/2
      KMA1 = INT(ABS(MA1))
      IF (MOD(KMA1,2) == 1) THEN
          XB = MBASE
          X = X*SQRT(XB)
          MKE = -(MA1+1)/2
      ENDIF
      CALL FMDPM(X,MXY(3))
      MXY(3)%MP(2) = MXY(3)%MP(2) + MKE

!             Initialize.

      CALL FMI2M(1,MXY(1))
      CALL FMI2M(0,MXY(4))
      MXY(2)%MP(2) = MA1
      CALL FMDIG(NSTACK,KST)

!             Newton iteration for 1/sqrt(a).

      DO J = 1, KST-1
         NDIG = NSTACK(J)
         CALL FMSQR(MXY(3),MXY(4))
         CALL FMMPY_R2(MXY(2),MXY(4))
         CALL FMSUB_R2(MXY(1),MXY(4))
         IF (J > 1) NDIG = NSTACK(J-1)
         CALL FMMPY_R2(MXY(3),MXY(4))
         CALL FMDIVI_R1(MXY(4),2)
         NDIG = NSTACK(J)
         CALL FMADD_R1(MXY(3),MXY(4))
      ENDDO

!             Karp's method for speeding up the last iteration and getting sqrt(a).
!             For the last iteration, combine a*x1 to get
!             a*x1 + x1*(a - (a*x1)**2)/2
!             where only the square, -, and + need full precision.

      NDIG = NSTACK(KST)
      IF (KST > 1) NDIG = NSTACK(KST-1)
      CALL FMMPY(MXY(2),MXY(3),MXY(4))
      NDIG = NSTACK(KST)
      CALL FMSQR(MXY(4),MXY(1))
      CALL FMSUB_R2(MXY(2),MXY(1))
      IF (KST > 1) NDIG = NSTACK(KST-1)
      CALL FMMPY_R2(MXY(3),MXY(1))
      CALL FMDIVI_R1(MXY(1),2)
      NDIG = NSTACK(KST)
      CALL FMADD(MXY(4),MXY(1),MXY(3))
      GO TO 160

!             Method 4.  Direct method.

  150 CALL FMI2M(1,MXY(1))
      CALL FMI2M(1,MXY(3))
      CALL FMI2M(1,MXY(4))
      MXY(1)%MP(3) = MXY(2)%MP(3)
      MXY(1)%MP(2) = 1
      DC = MXY(1)%MP(3)
      JPT = 2
      KN = MAX(1,INT(MAXINT/(2*MBASE**2)-5))
      IF (MOD(INT(ABS(MXY(2)%MP(2))),2) == 0) THEN
          MXY(1)%MP(2) = 1
          MXY(1)%MP(3) = MXY(2)%MP(3) * MBASE + MXY(2)%MP(4)
          DC = MXY(1)%MP(3)
          JPT = 3
      ENDIF
      X = INT(SQRT(DC))
      MKE = 2
      IF (MOD(MXY(2)%MP(2),MKE) == 0) THEN
          MXY(3)%MP(2) = MXY(2)%MP(2) / 2
      ELSE
          MXY(3)%MP(2) = (MXY(2)%MP(2)+1) / 2
      ENDIF
      MXY(3)%MP(3) = X
      MXY(4)%MP(2) = 1
      MKE = X
      MXY(4)%MP(3) = MKE*MKE
      MXY(1)%MP(3) = MXY(1)%MP(3) - MXY(4)%MP(3)
      DO J = 2, NDIG
         MXY(1)%MP(2) = MXY(1)%MP(2) + 2
         IF (JPT <= NDIG) THEN
             MXY(1)%MP(J+2) = MXY(2)%MP(JPT+2)
         ENDIF
         IF (JPT+1 <= NDIG) THEN
             MXY(1)%MP(J+3) = MXY(2)%MP(JPT+3)
         ENDIF
         JPT = JPT + 2
         DC = MXY(1)%MP(3)
         IF (NDIG >= 4) THEN
             DC = ((DC*MBASE + MXY(1)%MP(4))*MBASE + MXY(1)%MP(5))*MBASE +  &
                   MXY(1)%MP(6)
         ELSE IF (NDIG == 3) THEN
             DC = ((DC*MBASE + MXY(1)%MP(4))*MBASE + MXY(1)%MP(5))*MBASE
         ELSE
             DC = ((DC*MBASE + MXY(1)%MP(4))*MBASE)*MBASE
         ENDIF
         IF (J <= 4) THEN
             DP = MXY(3)%MP(3)
             IF (NDIG >= 3) THEN
                 DP = (DP*MBASE + MXY(3)%MP(4))*MBASE + MXY(3)%MP(5)
             ELSE
                 DP = (DP*MBASE + MXY(3)%MP(4))*MBASE
             ENDIF
         ENDIF
         IF (J <= 2) THEN
             DC = DC*DBLE(MBASE)**INT(MXY(1)%MP(2)-4)
             DP = DP*DBLE(MBASE)**(J-4)
             X = FLOOR(DP*MBASE*(SQRT(1+DC/(DP*MBASE)**2)-1))
         ELSE
             IF (INT(MXY(1)%MP(2))-J-1 /= 0) THEN
                 DC = DC*DBLE(MBASE)**(INT(MXY(1)%MP(2))-J-1)
             ENDIF
             X = FLOOR(DC/(2.0D0*DP))
         ENDIF
         IF (ABS(2.0D0 * X * MBASE) >= MAXINT) THEN
             GO TO 120
         ENDIF
         MXY(4)%MP(2) = J
         KL = MIN(J,NDIG/2+2+NDIG/10)
         DO K = 3, KL+1
            MKE = X
            MXY(4)%MP(K) = 2 * MKE * MXY(3)%MP(K)
         ENDDO
         MKE = X
         MXY(4)%MP(J+2) = MKE * MKE
         MXY(3)%MP(J+2) = X
         IF (J == NDIG) EXIT
         IF (MXY(1)%MP(2) - MXY(4)%MP(2) /= 1) THEN
             GO TO 120
         ENDIF
         MXY(1)%MP(2) = MXY(1)%MP(2) - 1
         MXY(1)%MP(3) = MXY(1)%MP(3) * MBASE + MXY(1)%MP(4) -  &
                                  MXY(4)%MP(3)
         KL = MIN(J,NDIG/2+2+NDIG/10)
         DO K = 4, KL+2
            MXY(1)%MP(K) = MXY(1)%MP(K+1) - MXY(4)%MP(K)
         ENDDO
         MXY(1)%MP(J+3) = 0
         IF (MOD(J,KN) == 0) THEN
             DO K = J+2, 4, -1
                IF (MOD(MXY(1)%MP(K),MBASE) == 0) THEN
                    KL = MXY(1)%MP(K) / MBASE
                ELSE IF (MXY(1)%MP(K) > 0) THEN
                    KL = MXY(1)%MP(K) / MBASE
                ELSE
                    KL = MXY(1)%MP(K) / MBASE - 1
                ENDIF
                MXY(1)%MP(K-1) = MXY(1)%MP(K-1) + KL
                MXY(1)%MP(K) = MXY(1)%MP(K) - KL*MBASE
             ENDDO
         ENDIF
      ENDDO
      DO K = NDIG+2, 4, -1
         IF (MXY(3)%MP(K) >= 0 .AND. MXY(3)%MP(K) < MBASE) CYCLE
         IF (MOD(MXY(3)%MP(K),MBASE) == 0) THEN
             KL = MXY(3)%MP(K) / MBASE
         ELSE IF (MXY(3)%MP(K) > 0) THEN
             KL = MXY(3)%MP(K) / MBASE
         ELSE
             KL = MXY(3)%MP(K) / MBASE - 1
         ENDIF
         MXY(3)%MP(K-1) = MXY(3)%MP(K-1) + KL
         MXY(3)%MP(K) = MXY(3)%MP(K) - KL*MBASE
      ENDDO

!             Round the result and return.

  160 MXY(3)%MP(1) = 1

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(3),MB,NDSAVE,MXSAVE,0)
      IF (KFLAG == 1) KFLAG = 0
      RETURN
      END SUBROUTINE FMSQRT

      SUBROUTINE FMSQRT_R1(MA)

!  MA = SQRT(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      DOUBLE PRECISION :: DC,DP,ERR,X,XB
      REAL (KIND(1.0D0)) :: MA1,MKE,MXSAVE
      INTEGER :: NSTACK(49),J,JPT,K,KL,KMA1,KN,KOVUN,KRESLT,KST,KR_RETRY,NDSAVE,NMETHD
      INTENT (INOUT) :: MA
      TYPE(MULTI) :: MXY(4)


      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0 .OR. MA%MP(1) < 0) THEN
          CALL FMENTR2('FMSQRT_R1',MA,MA,1,1,MXY(3),KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              CALL FMEQ(MXY(3),MA)
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'FMSQRT_R1'
              CALL FMNTR(2,MA,MA,1,1)
          ENDIF
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF
      KR_RETRY = 0
  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF

      MA1 = MA%MP(2)

      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)

      NMETHD = 1
      IF (MBASE >= 1000 .AND. MBASE <= MXBASE/1.5D0 .AND. NDIG*ALOGMT < 1400) THEN
          NMETHD = 4
      ELSE IF (NDIG*ALOGMT > 50000) THEN
          NMETHD = 3
      ELSE IF (NDIG*ALOGMT > 2750) THEN
          NMETHD = 2
      ENDIF
      IF (NMETHD == 2) GO TO 130
      IF (NMETHD == 3) GO TO 140
      IF (NMETHD == 4) GO TO 150

!             Method 1.  Ordinary Newton iteration.

!             Generate the first approximation.

  120 MXY(2)%MP(2) = 0
      CALL FMM2DP(MXY(2),X)
      X = SQRT(X)
      MKE = MA1/2
      KMA1 = INT(ABS(MA1))
      IF (MOD(KMA1,2) == 1) THEN
          XB = MBASE
          X = X*SQRT(XB)
          MKE = (MA1-1)/2
      ENDIF
      CALL FMDPM(X,MXY(3))
      MXY(3)%MP(2) = MXY(3)%MP(2) + MKE

!             Initialize.

      CALL FMI2M(0,MXY(1))
      MXY(2)%MP(2) = MA1
      CALL FMDIG(NSTACK,KST)

!             Newton iteration.

      DO J = 1, KST
         NDIG = NSTACK(J)
         CALL FMCSDIV(MXY(2),MXY(3),MXY(1))
         CALL FMCSADD_R1(MXY(3),MXY(1))
         CALL FMCSDIVI_R1(MXY(3),2)
      ENDDO

      GO TO 160

!             Method 2.  Modified Newton iteration.
!                        x2 = x1 - (x1^2 - a) / (2*x1)
!                        where the square is done at full current precision,
!                        and the division is done at half current precision.

!             Generate the first approximation.

  130 MXY(2)%MP(2) = 0
      CALL FMM2DP(MXY(2),X)
      X = SQRT(X)
      MKE = MA1/2
      KMA1 = INT(ABS(MA1))
      IF (MOD(KMA1,2) == 1) THEN
          XB = MBASE
          X = X*SQRT(XB)
          MKE = (MA1-1)/2
      ENDIF
      CALL FMDPM(X,MXY(3))
      MXY(3)%MP(2) = MXY(3)%MP(2) + MKE

!             Initialize.

      CALL FMI2M(0,MXY(1))
      CALL FMI2M(0,MXY(4))
      MXY(2)%MP(2) = MA1
      CALL FMDIG(NSTACK,KST)

!             Newton iteration.

      DO J = 1, KST
         NDIG = NSTACK(J)
         CALL FMSQR(MXY(3),MXY(1))
         CALL FMSUB_R1(MXY(1),MXY(2))
         IF (J > 1) NDIG = NSTACK(J-1)
         CALL FMADD(MXY(3),MXY(3),MXY(4))
         CALL FMDIV_R2(MXY(1),MXY(4))
         NDIG = NSTACK(J)
         CALL FMSUB_R1(MXY(3),MXY(4))
      ENDDO

      GO TO 160

!             Method 3.  Karp's method.  Newton iteration for 1/sqrt(a)
!                        x2 = x1 + (1 - a*x1**2)*x1/2
!                        where a*x1**2 is done at full current precision,
!                        and *x1/2 is done at half current precision.

!             Generate the first approximation to 1/sqrt(a).

  140 MXY(2)%MP(2) = 0
      CALL FMM2DP(MXY(2),X)
      X = 1.0D0/SQRT(X)
      MKE = -MA1/2
      KMA1 = INT(ABS(MA1))
      IF (MOD(KMA1,2) == 1) THEN
          XB = MBASE
          X = X*SQRT(XB)
          MKE = -(MA1+1)/2
      ENDIF
      CALL FMDPM(X,MXY(3))
      MXY(3)%MP(2) = MXY(3)%MP(2) + MKE

!             Initialize.

      CALL FMI2M(1,MXY(1))
      CALL FMI2M(0,MXY(4))
      MXY(2)%MP(2) = MA1
      CALL FMDIG(NSTACK,KST)

!             Newton iteration for 1/sqrt(a).

      DO J = 1, KST-1
         NDIG = NSTACK(J)
         CALL FMSQR(MXY(3),MXY(4))
         CALL FMMPY_R2(MXY(2),MXY(4))
         CALL FMSUB_R2(MXY(1),MXY(4))
         IF (J > 1) NDIG = NSTACK(J-1)
         CALL FMMPY_R2(MXY(3),MXY(4))
         CALL FMDIVI_R1(MXY(4),2)
         NDIG = NSTACK(J)
         CALL FMADD_R1(MXY(3),MXY(4))
      ENDDO

!             Karp's method for speeding up the last iteration and getting sqrt(a).
!             For the last iteration, combine a*x1 to get
!             a*x1 + x1*(a - (a*x1)**2)/2
!             where only the square, -, and + need full precision.

      NDIG = NSTACK(KST)
      IF (KST > 1) NDIG = NSTACK(KST-1)
      CALL FMMPY(MXY(2),MXY(3),MXY(4))
      NDIG = NSTACK(KST)
      CALL FMSQR(MXY(4),MXY(1))
      CALL FMSUB_R2(MXY(2),MXY(1))
      IF (KST > 1) NDIG = NSTACK(KST-1)
      CALL FMMPY_R2(MXY(3),MXY(1))
      CALL FMDIVI_R1(MXY(1),2)
      NDIG = NSTACK(KST)
      CALL FMADD(MXY(4),MXY(1),MXY(3))
      GO TO 160

!             Method 4.  Direct method.

  150 CALL FMI2M(1,MXY(1))
      CALL FMI2M(1,MXY(3))
      CALL FMI2M(1,MXY(4))
      MXY(1)%MP(3) = MXY(2)%MP(3)
      MXY(1)%MP(2) = 1
      DC = MXY(1)%MP(3)
      JPT = 2
      KN = MAX(1,INT(MAXINT/(2*MBASE**2)-5))
      IF (MOD(INT(ABS(MXY(2)%MP(2))),2) == 0) THEN
          MXY(1)%MP(2) = 1
          MXY(1)%MP(3) = MXY(2)%MP(3) * MBASE + MXY(2)%MP(4)
          DC = MXY(1)%MP(3)
          JPT = 3
      ENDIF
      X = INT(SQRT(DC))
      MKE = 2
      IF (MOD(MXY(2)%MP(2),MKE) == 0) THEN
          MXY(3)%MP(2) = MXY(2)%MP(2) / 2
      ELSE
          MXY(3)%MP(2) = (MXY(2)%MP(2)+1) / 2
      ENDIF
      MXY(3)%MP(3) = X
      MXY(4)%MP(2) = 1
      MKE = X
      MXY(4)%MP(3) = MKE*MKE
      MXY(1)%MP(3) = MXY(1)%MP(3) - MXY(4)%MP(3)
      DO J = 2, NDIG
         MXY(1)%MP(2) = MXY(1)%MP(2) + 2
         IF (JPT <= NDIG) THEN
             MXY(1)%MP(J+2) = MXY(2)%MP(JPT+2)
         ENDIF
         IF (JPT+1 <= NDIG) THEN
             MXY(1)%MP(J+3) = MXY(2)%MP(JPT+3)
         ENDIF
         JPT = JPT + 2
         DC = MXY(1)%MP(3)
         IF (NDIG >= 4) THEN
             DC = ((DC*MBASE + MXY(1)%MP(4))*MBASE + MXY(1)%MP(5))*MBASE +  &
                   MXY(1)%MP(6)
         ELSE IF (NDIG == 3) THEN
             DC = ((DC*MBASE + MXY(1)%MP(4))*MBASE + MXY(1)%MP(5))*MBASE
         ELSE
             DC = ((DC*MBASE + MXY(1)%MP(4))*MBASE)*MBASE
         ENDIF
         IF (J <= 4) THEN
             DP = MXY(3)%MP(3)
             IF (NDIG >= 3) THEN
                 DP = (DP*MBASE + MXY(3)%MP(4))*MBASE + MXY(3)%MP(5)
             ELSE
                 DP = (DP*MBASE + MXY(3)%MP(4))*MBASE
             ENDIF
         ENDIF
         IF (J <= 2) THEN
             DC = DC*DBLE(MBASE)**INT(MXY(1)%MP(2)-4)
             DP = DP*DBLE(MBASE)**(J-4)
             X = FLOOR(DP*MBASE*(SQRT(1+DC/(DP*MBASE)**2)-1))
         ELSE
             IF (INT(MXY(1)%MP(2))-J-1 /= 0) THEN
                 DC = DC*DBLE(MBASE)**(INT(MXY(1)%MP(2))-J-1)
             ENDIF
             X = FLOOR(DC/(2.0D0*DP))
         ENDIF
         IF (ABS(2.0D0 * X * MBASE) >= MAXINT) THEN
             GO TO 120
         ENDIF
         MXY(4)%MP(2) = J
         KL = MIN(J,NDIG/2+2+NDIG/10)
         DO K = 3, KL+1
            MKE = X
            MXY(4)%MP(K) = 2 * MKE * MXY(3)%MP(K)
         ENDDO
         MKE = X
         MXY(4)%MP(J+2) = MKE * MKE
         MXY(3)%MP(J+2) = X
         IF (J == NDIG) EXIT
         IF (MXY(1)%MP(2) - MXY(4)%MP(2) /= 1) THEN
             GO TO 120
         ENDIF
         MXY(1)%MP(2) = MXY(1)%MP(2) - 1
         MXY(1)%MP(3) = MXY(1)%MP(3) * MBASE + MXY(1)%MP(4) -  &
                                  MXY(4)%MP(3)
         KL = MIN(J,NDIG/2+2+NDIG/10)
         DO K = 4, KL+2
            MXY(1)%MP(K) = MXY(1)%MP(K+1) - MXY(4)%MP(K)
         ENDDO
         MXY(1)%MP(J+3) = 0
         IF (MOD(J,KN) == 0) THEN
             DO K = J+2, 4, -1
                IF (MOD(MXY(1)%MP(K),MBASE) == 0) THEN
                    KL = MXY(1)%MP(K) / MBASE
                ELSE IF (MXY(1)%MP(K) > 0) THEN
                    KL = MXY(1)%MP(K) / MBASE
                ELSE
                    KL = MXY(1)%MP(K) / MBASE - 1
                ENDIF
                MXY(1)%MP(K-1) = MXY(1)%MP(K-1) + KL
                MXY(1)%MP(K) = MXY(1)%MP(K) - KL*MBASE
             ENDDO
         ENDIF
      ENDDO
      DO K = NDIG+2, 4, -1
         IF (MXY(3)%MP(K) >= 0 .AND. MXY(3)%MP(K) < MBASE) CYCLE
         IF (MOD(MXY(3)%MP(K),MBASE) == 0) THEN
             KL = MXY(3)%MP(K) / MBASE
         ELSE IF (MXY(3)%MP(K) > 0) THEN
             KL = MXY(3)%MP(K) / MBASE
         ELSE
             KL = MXY(3)%MP(K) / MBASE - 1
         ENDIF
         MXY(3)%MP(K-1) = MXY(3)%MP(K-1) + KL
         MXY(3)%MP(K) = MXY(3)%MP(K) - KL*MBASE
      ENDDO

!             Round the result and return.

  160 MXY(3)%MP(1) = 1

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(3),MA,NDSAVE,MXSAVE,0)
      IF (KFLAG == 1) KFLAG = 0
      RETURN
      END SUBROUTINE FMSQRT_R1

      SUBROUTINE FMST2D(STRING,X)

!  STRING contains a free-format number that is converted to double precision and returned in X.

!  The input number may be in integer or any real format. The convention is made that if no digits
!  appear before 'E' then 1.0 is assumed.  For example 'E6' is converted as '1.0E+6'.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: STRING
      INTEGER :: J,JSTATE,KDIGFL,KEXP,KPT,KSIGN,KSIGNX,KSTART,KSTOP,KTYPE,KVAL,N2
      DOUBLE PRECISION :: X,F1,F2

      INTEGER :: JTRANS(8,4) = RESHAPE(  (/     &
                   2, 9, 7, 7, 7, 7, 9, 9,      &
                   3, 3, 3, 5, 5, 8, 8, 8,      &
                   4, 4, 4, 9, 9, 9, 9, 9,      &
                   6, 6, 6, 6, 6, 9, 9, 9   /)  &
        , (/ 8,4 /) )

      CHARACTER :: KBLANK = ' '
      INTENT (IN) :: STRING
      INTENT (INOUT) :: X

      JSTATE = 1
      KSIGN = 1
      F1 = 0.0D0
      F2 = 0.0D0
      N2 = 0
      KSIGNX = 1
      KEXP = 0
      KSTART = 1
      KSTOP = LEN(STRING)
      KFLAG = 0

!             KDIGFL will be 1 if any digits are found before 'E'.

      KDIGFL = 0

!             Initialize two hash tables that are used for character look-up during
!             input conversion.

      IF (LHASH == 0) CALL FMHTBL

!             Scan the number.

      DO J = KSTART, KSTOP
         IF (STRING(J:J) == KBLANK) CYCLE
         KPT = ICHAR(STRING(J:J))
         IF (KPT < LHASH1 .OR. KPT > LHASH2) THEN
             WRITE (KW,                                                       &
                "(/' Error in input conversion.'/"                        //  &
                "' ICHAR function was out of range for the current',"     //  &
                "' dimensions.'/' ICHAR(''',A,''') gave the value ',"     //  &
                "I12,', which is outside the currently'/' dimensioned',"  //  &
                "' bounds of (',I5,':',I5,') for variables KHASHT ',"     //  &
                "'and KHASHV.'/' Re-define the two parameters ',"         //  &
                "'LHASH1 and LHASH2 so the dimensions will'/' contain',"  //  &
                "' all possible output values from ICHAR.'//)"                &
                   ) STRING(J:J),KPT,LHASH1,LHASH2
             KTYPE = 5
             KVAL  = 0
         ELSE
             KTYPE = KHASHT(KPT)
             KVAL  = KHASHV(KPT)
         ENDIF
         IF (KTYPE >= 5) GO TO 110

         JSTATE = JTRANS(JSTATE,KTYPE)

         SELECT CASE (JSTATE)

!             State 2.  Sign of the number.

         CASE (2)
             KSIGN = KVAL

!             State 3.  Digits before a decimal point.

         CASE (3)
             KDIGFL = 1
             F1 = 10.0D0*F1 + KVAL

!             State 4.  Decimal point

         CASE (4)
             CYCLE

!             State 5.  Digits after a decimal point.

         CASE (5)
             KDIGFL = 1
             F2 = 10.0D0*F2 + KVAL
             N2 = N2 + 1

!             State 6.  Precision indicator.

         CASE (6)
             IF (KDIGFL == 0) F1 = 1.0D0

!             State 7.  Sign of the exponent.

         CASE (7)
             KSIGNX = KVAL

!             State 8.  Digits of the exponent.

         CASE (8)
             KEXP = 10*KEXP + KVAL

         CASE DEFAULT
             GO TO 110

         END SELECT

      ENDDO

!             Form the number and return.

      KEXP = KSIGNX*KEXP
      X = KSIGN*(F1 + F2/10.0D0**N2)*10.0D0**KEXP

      RETURN

!             Error in converting the number.

  110 X = -1.0D+31
      KFLAG = -4
      RETURN
      END SUBROUTINE FMST2D

      SUBROUTINE FMST2M(STRING,MA)

!  MA = STRING

!  Convert a character string to FM format.
!  This is often more convenient than using FMINP, which converts an array of character(1) values.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: STRING
      TYPE(MULTI) :: MA

      INTEGER :: J,LB,KFSAVE
      INTENT (IN) :: STRING
      INTENT (INOUT) :: MA

      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMST2M'
      LB = LEN(STRING)
      IF (LB > LMBUFF) THEN
          IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
          ALLOCATE(CMBUFF(LB),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          LMBUFF = LB
      ENDIF
      KFSAVE = KFLAG

      DO J = 1, LB
         CMBUFF(J) = STRING(J:J)
      ENDDO
      NCALL = NCALL - 1
      CALL FMINP(CMBUFF,MA,1,LB)
      NCALL = NCALL + 1

      IF (KFSAVE /= 0) KFLAG = KFSAVE
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMST2M

      SUBROUTINE FMSUB(MA,MB,MC)

!  MC = MA - MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      INTEGER :: KFLG1
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMSUB'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)

          KFLG1 = 0
          IF (MB%MP(2) > MA%MP(2) .OR. MA%MP(3) == 0) KFLG1 = 1
          IF (MB%MP(3) == 0) KFLG1 = 0

!             FMADD2 will negate MB and add.

          KSUB = 1
          CALL FMADD2(MA,MB,MC)
          KSUB = 0

!             If MA was smaller than MB, then KFLAG = 1 returned from FMADD means the result from
!             FMSUB is the opposite of the input argument of larger magnitude, so reset KFLAG.

          IF (KFLAG == 1 .AND. KFLG1 == 1) KFLAG = 0

          IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
      ELSE
          KFLG1 = 0
          IF (MB%MP(2) > MA%MP(2) .OR. MA%MP(3) == 0) KFLG1 = 1
          IF (MB%MP(3) == 0) KFLG1 = 0
          KSUB = 1
          CALL FMADD2(MA,MB,MC)
          KSUB = 0
          IF (KFLAG == 1 .AND. KFLG1 == 1) KFLAG = 0
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMSUB

      SUBROUTINE FMSUB_R1(MA,MB)

!  MA = MA - MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      INTEGER :: KFLG1
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMSUB_R1'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)

          KFLG1 = 0
          IF (MB%MP(2) > MA%MP(2) .OR. MA%MP(3) == 0) KFLG1 = 1
          IF (MB%MP(3) == 0) KFLG1 = 0

!             FMADD2 will negate MB and add.

          KSUB = 1
          CALL FMADD2_R1(MA,MB)
          KSUB = 0

!             If MA was smaller than MB, then KFLAG = 1 returned from FMADD means the result from
!             FMSUB is the opposite of the input argument of larger magnitude, so reset KFLAG.

          IF (KFLAG == 1 .AND. KFLG1 == 1) KFLAG = 0

          IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
      ELSE
          KFLG1 = 0
          IF (MB%MP(2) > MA%MP(2) .OR. MA%MP(3) == 0) KFLG1 = 1
          IF (MB%MP(3) == 0) KFLG1 = 0
          KSUB = 1
          CALL FMADD2_R1(MA,MB)
          KSUB = 0
          IF (KFLAG == 1 .AND. KFLG1 == 1) KFLAG = 0
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMSUB_R1

      SUBROUTINE FMSUB_R2(MA,MB)

!  MB = MA - MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      INTEGER :: KFLG1

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'FMSUB_R2'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,2,1)

          KFLG1 = 0
          IF (MB%MP(2) > MA%MP(2) .OR. MA%MP(3) == 0) KFLG1 = 1
          IF (MB%MP(3) == 0) KFLG1 = 0

!             FMADD2 will negate MB and add.

          KSUB = 1
          CALL FMADD2_R2(MA,MB)
          KSUB = 0

!             If MA was smaller than MB, then KFLAG = 1 returned from FMADD means the result from
!             FMSUB is the opposite of the input argument of larger magnitude, so reset KFLAG.

          IF (KFLAG == 1 .AND. KFLG1 == 1) KFLAG = 0

          IF (NTRACE /= 0) CALL FMNTR(1,MB,MB,1,1)
      ELSE
          KFLG1 = 0
          IF (MB%MP(2) > MA%MP(2) .OR. MA%MP(3) == 0) KFLG1 = 1
          IF (MB%MP(3) == 0) KFLG1 = 0
          KSUB = 1
          CALL FMADD2_R2(MA,MB)
          KSUB = 0
          IF (KFLAG == 1 .AND. KFLG1 == 1) KFLAG = 0
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMSUB_R2

      SUBROUTINE FMTAN(MA,MB)

!  MB = TAN(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAS,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JCOS,JSIN,JSWAP,K,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE,NDSV,NTRY
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KR_RETRY = 0

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG .AND. KRAD == 1) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMTAN'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMMPY(MXY(1),MA,MXY(2))
          CALL FMDIVI(MXY(2),3,MXY(3))
          IF (MXY(3)%MP(2) > MEXPUN) THEN
              CALL FMADD(MA,MXY(3),MB)
          ELSE IF (MA%MP(2) == MEXPUN) THEN
              CALL FMEQ(MA,MB)
          ELSE IF (MA%MP(1) < 0 .AND. KROUND == -1) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMADD(MXY(1),MXY(2),MB)
              MB%MP(2) = MA%MP(2) + MB%MP(2)
          ELSE IF (MA%MP(1) >= 0 .AND. KROUND == 2) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMADD(MXY(1),MXY(2),MB)
              MB%MP(2) = MA%MP(2) + MB%MP(2)
          ELSE
              CALL FMEQ(MA,MB)
          ENDIF
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMTAN'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB .OR. MA%MP(3) == 0) THEN
          CALL FMENTR('FMTAN    ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMTAN'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52-1,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF

      MAS = MA%MP(1)
      NTRY = 1
      IF (MA%MP(2) > 3*10**5 .AND. KRAD == 1) THEN
          KFLAG = -4
          CALL FMST2M('UNKNOWN',MXY(5))
          GO TO 130
      ENDIF

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
  120 CALL FMEQU(MA,MXY(5),NDSAVE,NDIG)
      MXY(5)%MP(1) = 1

!             Reduce the argument, convert to radians if the input is in degrees, and evaluate
!             the function.

      CALL FMRDC(MXY(5),JSIN,JCOS,JSWAP)
      IF (KROUND /= 1 .AND. KRAD /= 1) THEN
          CALL FMI2M(45,MXY(1))
          CALL FMSUB(MXY(5),MXY(1),MXY(2))
          IF (MXY(2)%MP(3) == 0) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMMPYI(MXY(1),JSIN/JCOS,MXY(5))
              GO TO 130
          ENDIF
      ENDIF
      IF (MXY(5)%MP(2) == MUNKNO) GO TO 130
      IF (MXY(5)%MP(3) == 0) THEN
          IF (JSWAP == 1) THEN
              KFLAG = -4
              CALL FMWARN
              CALL FMST2M('UNKNOWN',MXY(5))
          ENDIF
          GO TO 130
      ENDIF
      IF (KRAD == 0) THEN
          IF (MBSPI /= MBASE .OR. NDIGPI < NDIG)  THEN
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMPI(MXY(4))
              NDIG = NDSV
          ENDIF
          CALL FMMPY_R1(MXY(5),MPISAV)
          CALL FMDIVI_R1(MXY(5),180)
      ENDIF
      IF (MXY(5)%MP(2) /= MUNKNO) THEN
          IF (JSWAP == 0) THEN
              IF (MXY(5)%MP(2) < 0) THEN
                  CALL FMSIN2(MXY(5),MXY(4))
                  MXY(4)%MP(1) = JSIN*MXY(4)%MP(1)
                  CALL FMSQR(MXY(4),MXY(2))
                  CALL FMI2M(1,MXY(1))
                  CALL FMSUB_R2(MXY(1),MXY(2))
                  IF (MXY(2)%MP(2) < 0 .AND. NTRY == 1) THEN
                      NTRY = 2
                      NDIG = NDIG - MXY(2)%MP(2)
                      GO TO 120
                  ENDIF
                  CALL FMSQRT(MXY(2),MXY(3))
                  MXY(3)%MP(1) = JCOS*MXY(3)%MP(1)
                  CALL FMDIV(MXY(4),MXY(3),MXY(5))
              ELSE
                  CALL FMCOS2(MXY(5),MXY(4))
                  MXY(4)%MP(1) = JCOS*MXY(4)%MP(1)
                  CALL FMSQR(MXY(4),MXY(2))
                  CALL FMI2M(1,MXY(1))
                  CALL FMSUB_R2(MXY(1),MXY(2))
                  IF (MXY(2)%MP(2) < 0 .AND. NTRY == 1) THEN
                      NTRY = 2
                      NDIG = NDIG - MXY(2)%MP(2)
                      GO TO 120
                  ENDIF
                  CALL FMSQRT(MXY(2),MXY(3))
                  MXY(3)%MP(1) = JSIN*MXY(3)%MP(1)
                  CALL FMDIV(MXY(3),MXY(4),MXY(5))
              ENDIF
          ELSE
              IF (MXY(5)%MP(2) < 0) THEN
                  CALL FMSIN2(MXY(5),MXY(4))
                  MXY(4)%MP(1) = JCOS*MXY(4)%MP(1)
                  CALL FMSQR(MXY(4),MXY(2))
                  CALL FMI2M(1,MXY(1))
                  CALL FMSUB_R2(MXY(1),MXY(2))
                  IF (MXY(2)%MP(2) < 0 .AND. NTRY == 1) THEN
                      NTRY = 2
                      NDIG = NDIG - MXY(2)%MP(2)
                      GO TO 120
                  ENDIF
                  CALL FMSQRT(MXY(2),MXY(3))
                  MXY(3)%MP(1) = JSIN*MXY(3)%MP(1)
                  CALL FMDIV(MXY(3),MXY(4),MXY(5))
              ELSE
                  CALL FMCOS2(MXY(5),MXY(4))
                  MXY(4)%MP(1) = JSIN*MXY(4)%MP(1)
                  CALL FMSQR(MXY(4),MXY(2))
                  CALL FMI2M(1,MXY(1))
                  CALL FMSUB_R2(MXY(1),MXY(2))
                  IF (MXY(2)%MP(2) < 0 .AND. NTRY == 1) THEN
                      NTRY = 2
                      NDIG = NDIG - MXY(2)%MP(2)
                      GO TO 120
                  ENDIF
                  CALL FMSQRT(MXY(2),MXY(3))
                  MXY(3)%MP(1) = JCOS*MXY(3)%MP(1)
                  CALL FMDIV(MXY(4),MXY(3),MXY(5))
              ENDIF
          ENDIF
      ENDIF

!             Round and return.

  130 IF (MAS < 0 .AND. MXY(5)%MP(2) /= MUNKNO .AND. MXY(5)%MP(3) /= 0)  &
          MXY(5)%MP(1) = -MXY(5)%MP(1)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(5)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(5),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMTAN

      SUBROUTINE FMTANH(MA,MB)

!  MB = TANH(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAS,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NDSAVE
      LOGICAL :: LCOMP
      LOGICAL, EXTERNAL :: FMCOMP
      REAL :: X,XT
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      TYPE(MULTI) :: MXY(5)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KR_RETRY = 0

      IF (MBLOGS /= MBASE) CALL FMCONS

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMTANH'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMMPY(MXY(1),MA,MXY(2))
          CALL FMDIVI(MXY(2),-3,MXY(3))
          IF (MXY(3)%MP(2) > MEXPUN) THEN
              CALL FMADD(MA,MXY(3),MB)
          ELSE IF (MA%MP(2) == MEXPUN) THEN
              CALL FMEQ(MA,MB)
          ELSE IF (MA%MP(1) < 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMSUB(MXY(1),MXY(2),MXY(3))
              MXY(3)%MP(2) = MA%MP(2) + MXY(3)%MP(2)
              CALL FMEQ(MXY(3),MB)
          ELSE IF (MA%MP(1) >= 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
              CALL FMEQ(MA,MXY(1))
              MXY(1)%MP(2) = 0
              CALL FMULP(MXY(1),MXY(2))
              CALL FMSUB(MXY(1),MXY(2),MXY(3))
              MXY(3)%MP(2) = MA%MP(2) + MXY(3)%MP(2)
              CALL FMEQ(MXY(3),MB)
          ELSE
              CALL FMEQ(MA,MB)
          ENDIF
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (MB%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMTANH'
              KFLAG = -4
              CALL FMWARN
              NCALL = NCALL - 1
          ELSE IF (ABS(MB%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMTANH'
              IF (MB%MP(2) == MEXPOV) KFLAG = -5
              IF (MB%MP(2) == MEXPUN) KFLAG = -6
              CALL FMWARN
              NCALL = NCALL - 1
          ENDIF
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMTANH'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      J = NTRACE
      NTRACE = 0
      CALL FMABS(MA,MXY(1))
      CALL FMDP2M(DLOGMB*NDIG,MXY(2))
      LCOMP = FMCOMP(MXY(1),'>',MXY(2))
      NTRACE = J
      IF (KROUND /= 1 .AND. LCOMP .AND. MA%MP(2) /= MUNKNO) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMTANH'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          IF (MA%MP(1) < 0 .AND. (KROUND == 0 .OR. KROUND == 2)) THEN
              CALL FMI2M(-1,MXY(1))
              CALL FMTINY(MXY(2))
              CALL FMADD(MXY(1),MXY(2),MXY(3))
          ELSE IF (MA%MP(1) > 0 .AND. (KROUND == 0 .OR. KROUND == -1)) THEN
              CALL FMI2M(1,MXY(1))
              CALL FMTINY(MXY(2))
              CALL FMSUB(MXY(1),MXY(2),MXY(3))
          ELSE IF (MA%MP(1) < 0) THEN
              CALL FMI2M(-1,MXY(3))
          ELSE
              CALL FMI2M(1,MXY(3))
          ENDIF
          CALL FMEQ(MXY(3),MB)
          KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMTANH'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMENTR('FMTANH   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMTANH'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = MAX(NGRD52,2)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF

      KWRNSV = KWARN
      KWARN = 0
      MAS = MA%MP(1)

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(5),NDSAVE,NDIG)
      IF (MA%MP(3) == 0) THEN
          GO TO 120
      ENDIF
      MXY(5)%MP(1) = 1

      IF (MA%MP(2) >= 1) THEN
          XT = REAL((NDIG+1)/2)*ALOGMB
          K = INT(LOG(XT)/ALOGMB)
          IF (MA%MP(2) > K+1) THEN
              CALL FMI2M(1,MXY(5))
              GO TO 120
          ELSE
              X = REAL(MXY(5)%MP(3)*MBASE+MXY(5)%MP(4))*  &
                  REAL(MBASE)**INT(MXY(5)%MP(2)-2)
              IF (X > XT+5.0) THEN
                  CALL FMI2M(1,MXY(5))
                  GO TO 120
              ENDIF
          ENDIF
      ENDIF
      IF (MXY(5)%MP(2) == 0 .AND. NDIG < 50) THEN
          CALL FMEXP2(MXY(5),MXY(3))
          CALL FMSQR_R1(MXY(3))
          CALL FMI2M(1,MXY(1))
          CALL FMSUB(MXY(3),MXY(1),MXY(2))
          CALL FMADD_R2(MXY(3),MXY(1))
          CALL FMDIV(MXY(2),MXY(1),MXY(5))
          GO TO 120
      ENDIF
      IF (MXY(5)%MP(2) >= 0 .AND. MXY(5)%MP(3) /= 0) THEN
          CALL FMCOSH(MXY(5),MXY(4))
          IF (MXY(4)%MP(2) > NDIG) THEN
              IF (MAS > 0) THEN
                  CALL FMI2M(1,MXY(5))
                  GO TO 120
              ELSE
                  CALL FMI2M(-1,MXY(5))
                  GO TO 120
              ENDIF
          ENDIF
          CALL FMSQR(MXY(4),MXY(2))
          CALL FMI2M(-1,MXY(1))
          CALL FMADD_R1(MXY(2),MXY(1))
          CALL FMSQRT_R1(MXY(2))
          CALL FMDIV(MXY(2),MXY(4),MXY(5))
      ELSE
          CALL FMSINH(MXY(5),MXY(4))
          CALL FMSQR(MXY(4),MXY(2))
          CALL FMI2M(1,MXY(1))
          CALL FMADD_R1(MXY(2),MXY(1))
          CALL FMSQRT_R1(MXY(2))
          CALL FMDIV(MXY(4),MXY(2),MXY(5))
      ENDIF

!             Round and return.

  120 KWARN = KWRNSV
      IF (MAS < 0 .AND. MXY(5)%MP(2) /= MUNKNO .AND. MXY(5)%MP(3) /= 0)  &
          MXY(5)%MP(1) = -MXY(5)%MP(1)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(5)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXIT(MXY(5),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMTANH

      SUBROUTINE FMTINY(MA)

!     MA = The smallest positive representable FM number using the current base and precision.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      INTEGER :: J,N1

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMTINY'

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      N1 = NDIG + 1
      DO J = 3, N1
         MA%MP(J+1) = 0
      ENDDO
      MA%MP(2) = -MXEXP
      MA%MP(1) = 1
      MA%MP(3) = 1

      IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMTINY

      SUBROUTINE FMTRAP(MA)

!  If MA has overflowed or underflowed, replace it by the appropriate symbol.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA

      IF (NCALL <= 0) RETURN
      IF (MA%MP(2) > MXEXP+1) THEN
          IF (MA%MP(1) > 0) THEN
              CALL FMIM(0,MA)
              MA%MP(2) = MEXPOV
              MA%MP(3) = 1
          ELSE
              CALL FMIM(0,MA)
              MA%MP(2) = MEXPOV
              MA%MP(3) = 1
              MA%MP(1) = -1
          ENDIF
          KFLAG = -5
      ENDIF
      IF (MA%MP(2) < -MXEXP) THEN
          IF (MA%MP(1) > 0) THEN
              CALL FMIM(0,MA)
              MA%MP(2) = MEXPUN
              MA%MP(3) = 1
          ELSE
              CALL FMIM(0,MA)
              MA%MP(2) = MEXPUN
              MA%MP(3) = 1
              MA%MP(1) = -1
          ENDIF
          KFLAG = -6
      ENDIF

      RETURN
      END SUBROUTINE FMTRAP

      SUBROUTINE FMULP(MA,MB)

!  MB = The value of one Unit in the Last Place of MA at the current base and precision.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MA1
      INTEGER :: J,KWRNSV,N1
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < 2*NDIG+30) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(2*NDIG+30),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMULP'
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,1)

      MA1 = MA%MP(2)
      N1 = NDIG + 1
      DO J = 3, N1
         MWA%MP(J+1) = 0
      ENDDO
      MWA%MP(3) = 1
      MWA%MP(2) = MA%MP(2) - NDIG + 1
      IF (MA%MP(3) == 0 .OR. MA%MP(2) >= MEXPOV) THEN
          KFLAG = -4
          IF (MA1 /= MUNKNO) CALL FMWARN
          CALL FMST2M('UNKNOWN',MB)
      ELSE
          KWRNSV = KWARN
          IF (MA1 == MEXPUN) KWARN = 0
          IF (MA%MP(1) < 0) THEN
              CALL FMMOVE(MWA,MB)
              MB%MP(1) = 1
              IF (MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0) MB%MP(1) = -1
          ELSE
              CALL FMMOVE(MWA,MB)
              MB%MP(1) = 1
          ENDIF
          IF (KFLAG < 0) THEN
              NAMEST(NCALL) = 'FMULP'
              CALL FMWARN
          ENDIF
          KWARN = KWRNSV
      ENDIF

      IF (NTRACE /= 0) CALL FMNTR(1,MB,MB,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMULP

      SUBROUTINE FMUNPK(MP,MA)

!  MP is unpacked and the value returned in MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MP

      INTEGER :: J,KP
      INTENT (IN) :: MP
      INTENT (INOUT) :: MA

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KP = 2
      MA%MP(1) = MP%MP(1)
      MA%MP(2) = MP%MP(2)
      MA%MP(3) = AINT (ABS(MP%MP(3))/MBASE)
      MA%MP(4) = ABS(MP%MP(3)) - MA%MP(3)*MBASE
      IF (NDIG >= 4) THEN
          DO J = 4, NDIG, 2
             KP = KP + 1
             MA%MP(J+1) = AINT (MP%MP(KP+1)/MBASE)
             MA%MP(J+2) = MP%MP(KP+1) - MA%MP(J+1)*MBASE
          ENDDO
      ENDIF
      IF (MOD(NDIG,2) == 1) THEN
          MA%MP(NDIG+2) = AINT (MP%MP(KP+2)/MBASE)
      ENDIF
      RETURN
      END SUBROUTINE FMUNPK

      SUBROUTINE FMVARS

!  Write the values of the FM global variables in module FMVALS.

      USE ModLib_FMVALS
      IMPLICIT NONE

      WRITE (KW,*) ' '
      WRITE (KW,*) ' Current values of the FM global variables.'
      WRITE (KW,*) ' '
      WRITE (KW,*) ' ALOGM2 = ',ALOGM2
      WRITE (KW,*) ' ALOGMB = ',ALOGMB
      WRITE (KW,*) ' ALOGMT = ',ALOGMT
      WRITE (KW,*) ' ALOGMX = ',ALOGMX
      WRITE (KW,*) ' CMCHAR = ',CMCHAR
      WRITE (KW,*) ' DLOGEB = ',DLOGEB
      WRITE (KW,*) ' DLOGMB = ',DLOGMB
      WRITE (KW,*) ' DLOGPI = ',DLOGPI
      WRITE (KW,*) ' DLOGTN = ',DLOGTN
      WRITE (KW,*) ' DLOGTP = ',DLOGTP
      WRITE (KW,*) ' DLOGTW = ',DLOGTW
      WRITE (KW,*) ' DPEPS  = ',DPEPS
      WRITE (KW,*) ' DPMAX  = ',DPMAX
      WRITE (KW,*) ' DPPI   = ',DPPI
      WRITE (KW,*) ' INTMAX = ',INTMAX
      WRITE (KW,*) ' IUNKNO = ',IUNKNO
      WRITE (KW,*) ' JFORM1 = ',JFORM1
      WRITE (KW,*) ' JFORM2 = ',JFORM2
      WRITE (KW,*) ' JFORMZ = ',JFORMZ
      WRITE (KW,*) ' JPRNTZ = ',JPRNTZ
      WRITE (KW,*) ' KDEBUG = ',KDEBUG
      WRITE (KW,*) ' KESWCH = ',KESWCH
      WRITE (KW,*) ' KFLAG  = ',KFLAG
      WRITE (KW,*) ' KRAD   = ',KRAD
      WRITE (KW,*) ' KROUND = ',KROUND
      WRITE (KW,*) ' KSUB   = ',KSUB
      WRITE (KW,*) ' KSWIDE = ',KSWIDE
      WRITE (KW,*) ' KW     = ',KW
      WRITE (KW,*) ' KWARN  = ',KWARN
      WRITE (KW,*) ' LHASH  = ',LHASH
      WRITE (KW,*) ' LHASH1 = ',LHASH1
      WRITE (KW,*) ' LHASH2 = ',LHASH2
      WRITE (KW,*) ' LJSUMS = ',LJSUMS
      WRITE (KW,*) ' LMBERN = ',LMBERN
      WRITE (KW,*) ' LMBUFF = ',LMBUFF
      WRITE (KW,*) ' LMBUFZ = ',LMBUFZ
      WRITE (KW,*) ' LVLTRC = ',LVLTRC
      WRITE (KW,*) ' MAXINT = ',MAXINT
      WRITE (KW,*) ' MBASE  = ',MBASE
      WRITE (KW,*) ' MBLOGS = ',MBLOGS
      WRITE (KW,*) ' MBS2PI = ',MBS2PI
      WRITE (KW,*) ' MBSBRN = ',MBSBRN
      WRITE (KW,*) ' MBSE   = ',MBSE
      WRITE (KW,*) ' MBSEUL = ',MBSEUL
      WRITE (KW,*) ' MBSGAM = ',MBSGAM
      WRITE (KW,*) ' MBSLB  = ',MBSLB
      WRITE (KW,*) ' MBSLI  = ',MBSLI
      WRITE (KW,*) ' MBSPI  = ',MBSPI
      WRITE (KW,*) ' MEXPAB = ',MEXPAB
      WRITE (KW,*) ' MEXPOV = ',MEXPOV
      WRITE (KW,*) ' MEXPUN = ',MEXPUN
      WRITE (KW,*) ' MUNKNO = ',MUNKNO
      WRITE (KW,*) ' MXBASE = ',MXBASE
      WRITE (KW,*) ' MXEXP  = ',MXEXP
      WRITE (KW,*) ' MXEXP2 = ',MXEXP2
      WRITE (KW,*) ' NCALL  = ',NCALL
      WRITE (KW,*) ' NDG2PI = ',NDG2PI
      WRITE (KW,*) ' NDGEUL = ',NDGEUL
      WRITE (KW,*) ' NDGGAM = ',NDGGAM
      WRITE (KW,*) ' NDIG   = ',NDIG
      WRITE (KW,*) ' NDIGE  = ',NDIGE
      WRITE (KW,*) ' NDIGLB = ',NDIGLB
      WRITE (KW,*) ' NDIGLI = ',NDIGLI
      WRITE (KW,*) ' NDIGPI = ',NDIGPI
      WRITE (KW,*) ' NGRD21 = ',NGRD21
      WRITE (KW,*) ' NGRD22 = ',NGRD22
      WRITE (KW,*) ' NGRD52 = ',NGRD52
      WRITE (KW,*) ' NTRACE = ',NTRACE
      WRITE (KW,*) ' NUMBRN = ',NUMBRN
      WRITE (KW,*) ' RUNKNO = ',RUNKNO
      WRITE (KW,*) ' SPMAX  = ',SPMAX
      WRITE (KW,*) ' '
      WRITE (KW,*) ' RADIX(1) = ',RADIX(1),'    DIGITS(1) = ',DIGITS(1)
      WRITE (KW,*) '    HUGE(1) = ',HUGE(1)
      WRITE (KW,*) ' RADIX(1.0) = ',RADIX(1.0),'    DIGITS(1.0) = ',DIGITS(1.0)
      WRITE (KW,*) '    HUGE(1.0)    = ',HUGE(1.0)
      WRITE (KW,*) '    TINY(1.0)    = ',TINY(1.0)
      WRITE (KW,*) '    EPSILON(1.0) = ',EPSILON(1.0)
      WRITE (KW,*) ' RADIX(1.0D0) = ',RADIX(1.0D0),'    DIGITS(1.0D0) = ',DIGITS(1.0D0)
      WRITE (KW,*) '    HUGE(1.0D0)    = ',HUGE(1.0D0)
      WRITE (KW,*) '    TINY(1.0D0)    = ',TINY(1.0D0)
      WRITE (KW,*) '    EPSILON(1.0D0) = ',EPSILON(1.0D0)
      WRITE (KW,*) ' '

      RETURN
      END SUBROUTINE FMVARS

      SUBROUTINE FMWARN

!  Called by one of the FM routines to print a warning message if any error condition arises
!  in that routine.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(9) :: NAME
      INTEGER :: NCS

      IF (KFLAG >= 0 .OR. NCALL /= 1 .OR. KWARN <= 0) RETURN
      NCS = NCALL
      NAME = NAMEST(NCALL)
      WRITE (KW,                                   &
             "(/' Error of type KFLAG =',I3,"  //  &
             "' in FM package in routine ',A/)"    &
            ) KFLAG,TRIM(NAME)

  110 NCALL = NCALL - 1
      IF (NCALL > 0) THEN
          NAME = NAMEST(NCALL)
          WRITE (KW,"( ' called from ',A)") TRIM(NAME)
          GO TO 110
      ENDIF

      IF (KFLAG == -1) THEN
          WRITE (KW,"(' NDIG must be at least 2'/)")
      ELSE IF (KFLAG == -2) THEN
          WRITE (KW,"(' MBASE must be between 2 and',I10/)") INT(MXBASE)
      ELSE IF (KFLAG == -3) THEN
          WRITE (KW,                                                     &
                 "(' An input argument is not a valid FM number.',"  //  &
                 "'  Its exponent is out of range.'/)"                   &
                )
          WRITE (KW,"(' UNKNOWN has been returned.'/)")
      ELSE IF (KFLAG == -4 .OR. KFLAG == -7) THEN
          WRITE (KW,"(' Invalid input argument for this routine.'/)")
          WRITE (KW,"(' UNKNOWN has been returned.'/)")
      ELSE IF (KFLAG == -5) THEN
          WRITE (KW,"(' The result has overflowed.'/)")
      ELSE IF (KFLAG == -6) THEN
          WRITE (KW,"(' The result has underflowed.'/)")
      ELSE IF (KFLAG == -8 .AND. NAME == 'FMOUT') THEN
          WRITE (KW,                                                          &
                 "(' The result array is not big enough to hold the',"    //  &
                 "' output character string'/' in the current format.'/"  //  &
                 "' The result ''***...***'' has been returned.'/)"           &
                )
      ELSE IF (KFLAG == -8 .AND. NAME == 'FMREAD') THEN
          WRITE (KW,                                                        &
                 "(' The CMBUFF array is not big enough to hold the',"  //  &
                 "' input character string'/"                           //  &
                 "' UNKNOWN has been returned.'/)"                          &
                )
      ELSE IF (KFLAG == -9) THEN
          WRITE (KW,                                                &
                 "(' Precision could not be raised enough to'"  //  &
                 ",' provide all requested guard digits.'/)"        &
                )
          WRITE (KW,                                        &
                 "(I23,' digits were requested (NDIG).'/)"  &
                ) NDIG
          WRITE (KW,"(' UNKNOWN has been returned.'/)")
      ELSE IF (KFLAG == -10) THEN
          IF (NAMEST(NCS) == 'FMM2SP') THEN
              WRITE (KW,                                                     &
                     "(' An FM number was too small in magnitude to ',"  //  &
                     "'convert to single precision.'/)"                      &
                    )
          ELSE
              WRITE (KW,                                                     &
                     "(' An FM number was too small in magnitude to ',"  //  &
                     "'convert to double precision.'/)"                      &
                    )
          ENDIF
          WRITE (KW,"(' Zero has been returned.'/)")
      ENDIF

      NCALL = NCS
      IF (KWARN >= 2) THEN
          STOP
      ENDIF
      RETURN
      END SUBROUTINE FMWARN

      SUBROUTINE FMWRIT(KWRITE,MA)

!  Write MA on unit KWRITE.  Multi-line numbers will have '&' as the last nonblank character on all
!  but the last line.  These numbers can then be read easily using FMREAD.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: KWRITE
      TYPE(MULTI) :: MA

      INTEGER :: J,JF1SAV,JF2SAV,K,KSAVE,L,LAST,LB,ND,NDSAVE,NEXP
      INTENT (IN) :: MA,KWRITE

      TYPE(MULTI) :: MXY(2)


      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMWRIT'
      NDSAVE = NDIG
      NDIG = MAX(NDIG+NGRD52,2)

      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      KSAVE = KFLAG
      ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
      IF (ND < 2) ND = 2
      NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
      LB = ND + NEXP

      JF1SAV = JFORM1
      JF2SAV = JFORM2
      JFORM1 = 1
      JFORM2 = ND + 6
      IF (LB > LMBUFF) THEN
          IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
          ALLOCATE(CMBUFF(LB),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          LMBUFF = LB
      ENDIF

      CALL FMOUT(MXY(1),CMBUFF,LB)

      KFLAG = KSAVE
      NDIG = NDSAVE
      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      LAST = LB + 1
      DO J = 1, LB
         IF (CMBUFF(LAST-J) /= ' ' .OR. J == LB) THEN
             L = LAST - J
             IF (MOD(L,73) /= 0) THEN
                 WRITE (KWRITE,"(4X,73A1,' &')") (CMBUFF(K),K=1,L)
             ELSE
                 IF (L > 73) WRITE (KWRITE,"(4X,73A1,' &')") (CMBUFF(K),K=1,L-73)
                 WRITE (KWRITE,"(4X,73A1)") (CMBUFF(K),K=L-72,L)
             ENDIF
             NCALL = NCALL - 1
             RETURN
         ENDIF
      ENDDO
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMWRIT


!  Here are the routines that work with packed FM numbers.  All names are the same as unpacked
!  versions with 'FM' replaced by 'FP'.

!  This packed format is not available when using the FM, IM, or ZM derived types.

      SUBROUTINE FPABS(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMABS(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPABS

      SUBROUTINE FPACOS(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMACOS(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPACOS

      SUBROUTINE FPACOSH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMACOSH(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPACOSH

      SUBROUTINE FPADD(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMADD(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPADD

      SUBROUTINE FPADD_R1(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMADD_R1(MPA,MPB)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPADD_R1

      SUBROUTINE FPADD_R2(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMADD_R2(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPADD_R2

      SUBROUTINE FPADDI(MA,L)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      INTENT (IN) :: L
      INTEGER :: L
      CALL FMUNPK(MA,MPA)
      CALL FMADDI(MPA,L)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPADDI

      SUBROUTINE FPASIN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMASIN(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPASIN

      SUBROUTINE FPASINH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMASINH(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPASINH

      SUBROUTINE FPATAN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMATAN(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPATAN

      SUBROUTINE FPATANH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMATANH(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPATANH

      SUBROUTINE FPATN2(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMATN2(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPATN2

      SUBROUTINE FPBIG(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      CALL FMBIG(MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPBIG

      SUBROUTINE FPCHSH(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      CALL FMUNPK(MA,MPA)
      CALL FMCHSH(MPA,MPB,MPC)
      CALL FMPACK(MPB,MB)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPCHSH

      FUNCTION FPCOMP(MA,LREL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      LOGICAL :: FPCOMP
      LOGICAL, EXTERNAL :: FMCOMP
      CHARACTER(*) :: LREL
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA,MB,LREL
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      FPCOMP = FMCOMP(MPA,LREL,MPB)
      RETURN
      END FUNCTION FPCOMP

      SUBROUTINE FPCOS(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMCOS(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPCOS

      SUBROUTINE FPCOSH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMCOSH(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPCOSH

      SUBROUTINE FPCSSN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      CALL FMUNPK(MA,MPA)
      CALL FMCSSN(MPA,MPB,MPC)
      CALL FMPACK(MPB,MB)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPCSSN

      SUBROUTINE FPDIG(NSTACK,KST)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: NSTACK(49),KST
      CALL FMDIG(NSTACK,KST)
      RETURN
      END SUBROUTINE FPDIG

      SUBROUTINE FPDIM(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMDIM(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPDIM

      SUBROUTINE FPDIV(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMDIV(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPDIV

      SUBROUTINE FPDIV_R1(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMDIV_R1(MPA,MPB)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPDIV_R1

      SUBROUTINE FPDIV_R2(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMDIV_R2(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPDIV_R2

      SUBROUTINE FPDIVI(MA,IVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      INTEGER :: IVAL
      CALL FMUNPK(MA,MPA)
      CALL FMDIVI(MPA,IVAL,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPDIVI

      SUBROUTINE FPDIVI_R1(MA,IVAL)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      INTEGER :: IVAL
      CALL FMUNPK(MA,MPA)
      CALL FMDIVI_R1(MPA,IVAL)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPDIVI_R1

      SUBROUTINE FPDP2M(X,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION :: X
      TYPE(MULTI) :: MA
      INTENT (IN) :: X
      INTENT (INOUT) :: MA
      CALL FMDP2M(X,MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPDP2M

      SUBROUTINE FPDPM(X,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      DOUBLE PRECISION :: X
      TYPE(MULTI) :: MA
      INTENT (IN) :: X
      INTENT (INOUT) :: MA
      CALL FMDPM(X,MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPDPM

      SUBROUTINE FPEQ(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMEQ(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPEQ

      SUBROUTINE FPEQU(MA,MB,NDA,NDB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: NDA,NDB
      INTENT (IN) :: MA,NDA,NDB
      INTENT (INOUT) :: MB
      CALL FPEQU_UNPCK(MA,NDA,MPA)
      CALL FMEQU_R1(MPA,NDA,NDB)
      CALL FPEQU_PACK(MPA,NDB,MB)
      RETURN
      END SUBROUTINE FPEQU

      SUBROUTINE FPEQU_R1(MA,NDA,NDB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTEGER :: NDA,NDB
      INTENT (INOUT) :: MA
      INTENT (IN) :: NDA,NDB
      CALL FPEQU_UNPCK(MA,NDA,MPA)
      CALL FMEQU_R1(MPA,NDA,NDB)
      CALL FPEQU_PACK(MPA,NDB,MA)
      RETURN
      END SUBROUTINE FPEQU_R1

      SUBROUTINE FPEQU_PACK(MA,ND,MP)

!  MA (with ND digits) is packed two base MBASE digits per word and returned in MP.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MP

      INTEGER :: J,KP,ND
      INTENT (IN) :: MA,ND
      INTENT (INOUT) :: MP

      IF (.NOT. ALLOCATED(MP%MP)) THEN
          ALLOCATE(MP%MP(ND/2+4),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MP%MP) < ND/2+4) THEN
          DEALLOCATE(MP%MP)
          ALLOCATE(MP%MP(ND/2+4),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KP = 2
      MP%MP(1) = MA%MP(1)
      MP%MP(2) = MA%MP(2)
      MP%MP(3) = ABS(MA%MP(3))*MBASE + MA%MP(4)
      IF (ND >= 4) THEN
          DO J = 4, ND, 2
             KP = KP + 1
             MP%MP(KP+1) = MA%MP(J+1)*MBASE + MA%MP(J+2)
          ENDDO
      ENDIF
      IF (MOD(ND,2) == 1) MP%MP(KP+2) = MA%MP(ND+2)*MBASE
      RETURN
      END SUBROUTINE FPEQU_PACK

      SUBROUTINE FPEQU_UNPCK(MP,ND,MA)

!  MP (with ND digits) is unpacked and the value returned in MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MP

      INTEGER :: J,KP,ND
      INTENT (IN) :: MP,ND
      INTENT (INOUT) :: MA

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(ND+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < ND+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(ND+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KP = 2
      MA%MP(1) = MP%MP(1)
      MA%MP(2) = MP%MP(2)
      MA%MP(3) = AINT (ABS(MP%MP(3))/MBASE)
      MA%MP(4) = ABS(MP%MP(3)) - MA%MP(3)*MBASE
      IF (ND >= 4) THEN
          DO J = 4, ND, 2
             KP = KP + 1
             MA%MP(J+1) = AINT (MP%MP(KP+1)/MBASE)
             MA%MP(J+2) = MP%MP(KP+1) - MA%MP(J+1)*MBASE
          ENDDO
      ENDIF
      IF (MOD(ND,2) == 1) THEN
          MA%MP(ND+2) = AINT (MP%MP(KP+2)/MBASE)
      ENDIF
      RETURN
      END SUBROUTINE FPEQU_UNPCK

      SUBROUTINE FPEXP(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMEXP(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPEXP

      SUBROUTINE FPFLAG(K)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: K
      K = KFLAG
      RETURN
      END SUBROUTINE FPFLAG

      SUBROUTINE FPFORM(FORM,MA,STRING)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM,STRING
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA,FORM
      INTENT (INOUT) :: STRING
      CALL FMUNPK(MA,MPA)
      CALL FMFORM(FORM,MPA,STRING)
      RETURN
      END SUBROUTINE FPFORM

      SUBROUTINE FPFPRT(FORM,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA,FORM
      CALL FMUNPK(MA,MPA)
      CALL FMFPRT(FORM,MPA)
      RETURN
      END SUBROUTINE FPFPRT

      SUBROUTINE FPHYPOT(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMHYPOT(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPHYPOT

      SUBROUTINE FPI2M(IVAL,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: IVAL
      TYPE(MULTI) :: MA
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA
      CALL FMI2M(IVAL,MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPI2M

      SUBROUTINE FPINP(LINE,MA,LA,LB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: LA,LB
      CHARACTER :: LINE(LB)
      TYPE(MULTI) :: MA
      INTENT (IN) :: LINE,LA,LB
      INTENT (INOUT) :: MA
      CALL FMINP(LINE,MPA,LA,LB)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPINP

      SUBROUTINE FPINT(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMINT(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPINT

      SUBROUTINE FPIPWR(MA,IVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      INTEGER :: IVAL
      CALL FMUNPK(MA,MPA)
      CALL FMIPWR(MPA,IVAL,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPIPWR

      SUBROUTINE FPLG10(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMLG10(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPLG10

      SUBROUTINE FPLN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMLN(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPLN

      SUBROUTINE FPLNI(IVAL,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: IVAL
      TYPE(MULTI) :: MA
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA
      CALL FMLNI(IVAL,MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPLNI

      SUBROUTINE FPM2DP(MA,X)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      DOUBLE PRECISION :: X
      INTENT (IN) :: MA
      INTENT (INOUT) :: X
      CALL FMUNPK(MA,MPA)
      CALL FMM2DP(MPA,X)
      RETURN
      END SUBROUTINE FPM2DP

      SUBROUTINE FPM2I(MA,IVAL)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA
      INTENT (INOUT) :: IVAL
      INTEGER :: IVAL
      CALL FMUNPK(MA,MPA)
      CALL FMM2I(MPA,IVAL)
      RETURN
      END SUBROUTINE FPM2I

      SUBROUTINE FPM2SP(MA,X)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA
      INTENT (INOUT) :: X
      REAL :: X
      CALL FMUNPK(MA,MPA)
      CALL FMM2SP(MPA,X)
      RETURN
      END SUBROUTINE FPM2SP

      SUBROUTINE FPMAX(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMMAX(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPMAX

      SUBROUTINE FPMIN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMMIN(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPMIN

      SUBROUTINE FPMOD(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMMOD(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPMOD

      SUBROUTINE FPMPY(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMMPY(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPMPY

      SUBROUTINE FPMPY_R1(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMMPY_R1(MPA,MPB)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPMPY_R1

      SUBROUTINE FPMPY_R2(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMMPY_R2(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPMPY_R2

      SUBROUTINE FPMPYI(MA,IVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      INTEGER :: IVAL
      CALL FMUNPK(MA,MPA)
      CALL FMMPYI(MPA,IVAL,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPMPYI

      SUBROUTINE FPMPYI_R1(MA,IVAL)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      INTEGER :: IVAL
      CALL FMUNPK(MA,MPA)
      CALL FMMPYI_R1(MPA,IVAL)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPMPYI_R1

      SUBROUTINE FPNINT(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMNINT(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPNINT

      SUBROUTINE FPOUT(MA,LINE,LB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA,LB
      INTENT (INOUT) :: LINE
      INTEGER :: LB
      CHARACTER :: LINE(LB)
      CALL FMUNPK(MA,MPA)
      CALL FMOUT(MPA,LINE,LB)
      RETURN
      END SUBROUTINE FPOUT

      SUBROUTINE FPPI(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      CALL FMPI(MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPPI

      SUBROUTINE FPPRNT(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA
      CALL FMUNPK(MA,MPA)
      CALL FMPRNT(MPA)
      RETURN
      END SUBROUTINE FPPRNT

      SUBROUTINE FPPWR(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMPWR(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPPWR

      SUBROUTINE FPREAD(KREAD,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KREAD
      TYPE(MULTI) :: MA
      INTENT (IN) :: KREAD
      INTENT (INOUT) :: MA
      CALL FMREAD(KREAD,MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPREAD

      SUBROUTINE FPRPWR(MA,KVAL,JVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA,KVAL,JVAL
      INTENT (INOUT) :: MB
      INTEGER :: KVAL,JVAL
      CALL FMUNPK(MA,MPA)
      CALL FMRPWR(MPA,KVAL,JVAL,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPRPWR

      SUBROUTINE FPSET(NPREC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: NPREC
      INTENT (IN) :: NPREC
      CALL FMSET(NPREC)
      RETURN
      END SUBROUTINE FPSET

      SUBROUTINE FPSETVAR(STRING)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: STRING
      INTENT (IN) :: STRING
      CALL FMSETVAR(STRING)
      RETURN
      END SUBROUTINE FPSETVAR

      SUBROUTINE FPSIGN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMSIGN(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPSIGN

      SUBROUTINE FPSIN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMSIN(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPSIN

      SUBROUTINE FPSINH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMSINH(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPSINH

      SUBROUTINE FPSP2M(X,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      REAL :: X
      TYPE(MULTI) :: MA
      INTENT (IN) :: X
      INTENT (INOUT) :: MA
      CALL FMSP2M(X,MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPSP2M

      SUBROUTINE FPSQR(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMSQR(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPSQR

      SUBROUTINE FPSQR_R1(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      CALL FMUNPK(MA,MPA)
      CALL FMSQR_R1(MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPSQR_R1

      SUBROUTINE FPSQRT(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMSQRT(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPSQRT

      SUBROUTINE FPSQRT_R1(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      CALL FMUNPK(MA,MPA)
      CALL FMSQRT_R1(MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPSQRT_R1

      SUBROUTINE FPST2M(STRING,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: STRING
      TYPE(MULTI) :: MA
      INTENT (IN) :: STRING
      INTENT (INOUT) :: MA
      CALL FMST2M(STRING,MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPST2M

      SUBROUTINE FPSUB(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMSUB(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPSUB

      SUBROUTINE FPSUB_R1(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMSUB_R1(MPA,MPB)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPSUB_R1

      SUBROUTINE FPSUB_R2(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMSUB_R2(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPSUB_R2

      SUBROUTINE FPTAN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMTAN(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPTAN

      SUBROUTINE FPTANH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMTANH(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPTANH

      SUBROUTINE FPTINY(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      CALL FMTINY(MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPTINY

      SUBROUTINE FPULP(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMULP(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPULP

      SUBROUTINE FPVARS
      USE ModLib_FMVALS
      IMPLICIT NONE
      CALL FMVARS
      RETURN
      END SUBROUTINE FPVARS

      SUBROUTINE FPWRIT(KWRITE,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KWRITE
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA,KWRITE
      CALL FMUNPK(MA,MPA)
      CALL FMWRIT(KWRITE,MPA)
      RETURN
      END SUBROUTINE FPWRIT


!  The random number routines use integer multiple precision arithmetic.

      SUBROUTINE FM_RANDOM_NUMBER(VALUE)

!  FM_RANDOM_NUMBER generates pseudo-random numbers uniform on (0,1).
!  VALUE is returned as the next random (double precision) number.
!  Neither zero nor one will be returned in VALUE.

!  This version uses the FM package to implement a multiplicative congruential generator.
!  Both the modulus and the multiplier are 49-digit primes, and the period is over 1.0E+49.

!  This generator passes the spectral test, with mu(2), ..., mu(6) =
!    3.40,   4.35,   3.98,   3.19,   3.20.
!  It also has passed Marsaglia's DieHard test suite for random generators.

!  The typical usage is to call FM_RANDOM_SEED_PUT once with SEED defined as an integer array of
!  length 7 containing seven seed values used to initialize the generator.  A default seed is used
!  if no call to FM_RANDOM_SEED_PUT is done.  Then each call to FM_RANDOM_NUMBER returns the next
!  random value.

!  This example seeds the generator and then fills the array R with random values between 0 and 1.

!        SEED = (/ 314159,265358,979323,846264,338327,950288,419716 /)
!        CALL FM_RANDOM_SEED_PUT(SEED)
!        DO J = 1, N
!           CALL FM_RANDOM_NUMBER(R(J))
!        ENDDO

!  In a FM_RANDOM_SEED_GET call, the seed array is returned that would later restart the generator
!  in FM_RANDOM_NUMBER at the same place in the sequence.

!      SEED = (/ 314159,265358,979323,846264,338327,950288,419716 /)
!      CALL FM_RANDOM_SEED_PUT(SEED)
!      DO J = 1, 100
!         CALL FM_RANDOM_NUMBER(R(J))
!      ENDDO

!      CALL FM_RANDOM_SEED_GET(SEED)
!      DO J = 101, 200
!         CALL FM_RANDOM_NUMBER(R(J))
!      ENDDO

!      CALL FM_RANDOM_SEED_PUT(SEED)
!      DO J = 201, 300
!         CALL FM_RANDOM_NUMBER(R(J))
!      ENDDO

!  Here the seed is saved after 100 calls.  The seed is used to re-set the generator after 200 calls
!  to the same state it had after 100 calls, and R(201), ..., R(300) is the same sequence as
!  R(101), ..., R(200).

      USE ModLib_FMVALS
      IMPLICIT NONE

      DOUBLE PRECISION :: VALUE,DPX
      INTEGER :: POS_OF_LAST_DIGIT,J,JBASE,LAST_DIGIT_OF_X
      INTEGER :: SEED(7) = (/314159,265358,979323,846264,338327,950288,419716/)
      SAVE JBASE,SEED
      REAL (KIND(1.0D0)) :: MSAVE
      LOGICAL, EXTERNAL :: IMCOMP
      TYPE(MULTI) :: MXY(2)


      NCALL = NCALL + 1
      IF (NCALL <= LVLTRC .AND. ABS(NTRACE) >= 2) THEN
          NAMEST(NCALL) = 'FM_RANDOM'
          WRITE (KW,"(' Input to FM_RANDOM_NUMBER')")
      ENDIF

!             Variables (in module FMVALS).

!             MBRAND is the base (value of MBASE) used for computing the random numbers.  This
!                    allows the user to change MBASE without causing problems in FM_RANDOM_NUMBER.

!             The multiplicative congruence used by the generator is
!                 X = ( A*X + C ) mod M
!             where all four are integers.
!             MRNX is X  (the current random integer)
!             MRNA is A  (the multiplier, defined in FM_RANDOM_SEED_PUT)
!             MRNC is C  (C = 1 for this generator)
!             MRNM is M  (the modulus, defined in FM_RANDOM_SEED_PUT)

!             X is the current value of the random sequence.
!             VALUE is then returned as approximately X/M.

      MSAVE = MBASE
      MBASE = MBRAND
      IF (MSAVE /= MBASE) CALL FMCONS

!             START_RANDOM_SEQUENCE =  0  for normal operation. Get the next  random value.
!                                   =  1  for the first call after the user has called
!                                         FM_RANDOM_SEED. Use that value in MRNX to initialize.
!                                   = -1  for the first user call if there was no initializing call
!                                         to FM_RANDOM_SEED.  Use a default seed to initialize MRNX.

      IF (START_RANDOM_SEQUENCE /= 0) THEN
          IF (START_RANDOM_SEQUENCE == -1) THEN
              CALL FM_RANDOM_SEED_PUT(SEED)
          ENDIF
          START_RANDOM_SEQUENCE = 0
      ENDIF
      JBASE = INT(MBASE) - 1

!             Get the next number in the sequence.

  110 CALL IMMPYM(MRNA,MRNX,MRNM,MXY(2))
      POS_OF_LAST_DIGIT = INT(MXY(2)%MP(2)) + 2
      DO J = 1, POS_OF_LAST_DIGIT
         MRNX%MP(J) = MXY(2)%MP(J)
      ENDDO
      LAST_DIGIT_OF_X = INT(MRNX%MP(POS_OF_LAST_DIGIT))
      IF (LAST_DIGIT_OF_X == LAST_DIGIT_OF_M_M1) THEN
          CALL IMADD(MRNX,MRNC,MXY(1))
          CALL IMEQ(MXY(1),MRNX)
          IF (IMCOMP(MRNX,'>=',MRNM)) THEN
              CALL IMSUB(MRNX,MRNM,MXY(1))
              CALL IMEQ(MXY(1),MRNX)
          ENDIF
      ELSE IF (LAST_DIGIT_OF_X < JBASE) THEN
          MRNX%MP(POS_OF_LAST_DIGIT) = MRNX%MP(POS_OF_LAST_DIGIT) + 1
      ELSE
          CALL IMADD(MRNX,MRNC,MXY(1))
          CALL IMEQ(MXY(1),MRNX)
      ENDIF

!             Convert to double precision.

      DPX = MRNX%MP(3)
      DO J = 4, POS_OF_LAST_DIGIT
         DPX = MBASE*DPX + MRNX%MP(J)
      ENDDO

      DPX = DPX*DPM
      IF (DPX >= 1.0D0 .OR. DPX <= 0.0D0) GO TO 110

      VALUE = DPX

      IF (NCALL <= LVLTRC .AND. ABS(NTRACE) >= 1) THEN
          WRITE (KW,                                              &
                 "(' ',A,5X,'Call level =',I2,5X,'MBASE =',I10)"  &
                ) 'FM_RANDOM_NUMBER',NCALL,INT(MBASE)
          WRITE (KW,"(1X,D30.20)") VALUE
      ENDIF
      NCALL = NCALL - 1

      IF (MSAVE /= MBASE) THEN
          MBASE = MSAVE
          CALL FMCONS
      ENDIF
      RETURN
      END SUBROUTINE FM_RANDOM_NUMBER

      SUBROUTINE FM_RANDOM_SEED_GET(SEED)

!  Return SEED(1) through SEED(7) as the current state of the generator.
!  See the comments in routine FM_RANDOM_NUMBER.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: SEED(7)
      REAL (KIND(1.0D0)) :: MSAVE
      INTEGER :: SCR(7) = (/462643,383279,502884,197163,141592,653589,793238/)
      INTEGER :: J,K
      TYPE(MULTI) :: MXY(3)


      MSAVE = MBASE
      MBASE = MBRAND
      IF (MBLOGS /= MBASE) CALL FMCONS
      K = 10**7
      CALL IMI2M(K,MXY(2))
      CALL IMEQ(MRNX,MXY(1))
      DO J = 7, 1, -1
         CALL IMMOD(MXY(1),MXY(2),MXY(3))
         CALL IMM2I(MXY(3),SEED(J))
         SEED(J) = SEED(J) - SCR(J)
         IF (SEED(J) < 0) THEN
             SEED(J) = SEED(J) + K
         ENDIF
         CALL IMDIVI(MXY(1),K,MXY(3))
         CALL IMEQ(MXY(3),MXY(1))
      ENDDO
      MBASE = MSAVE
      IF (MBLOGS /= MBASE) CALL FMCONS
      RETURN
      END SUBROUTINE FM_RANDOM_SEED_GET

      SUBROUTINE FM_RANDOM_SEED_PUT(SEED)

!  Use SEED(1) through SEED(7) to initialize the FM_RANDOM_NUMBER generator.
!  See the comments in routine FM_RANDOM_NUMBER.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: SEED(7)
      REAL (KIND(1.0D0)) :: MSAVE
      INTEGER :: SCR(7) = (/462643,383279,502884,197163,141592,653589,793238/)
      INTEGER :: J,K,L
      TYPE(MULTI) :: MXY(2)


      MSAVE = MBASE
      MBASE = MBRAND
      IF (MBLOGS /= MBASE) CALL FMCONS
      CALL IMST2M('1424133622579837639401183671018194926834820238197',MRNA)
      CALL IMST2M('2070613773952029032014000773560846464373793273739',MRNM)
      K = 10**7
      L = ABS(SEED(1)) + SCR(1)
      IF (L <= K) THEN
          CALL IMI2M(L,MRNX)
      ELSE
          CALL IMI2M(L-K,MRNX)
      ENDIF
      DO J = 2, 7
         CALL IMMPYI(MRNX,K,MXY(2))
         L = ABS(SEED(J)) + SCR(J)
         IF (L <= K) THEN
             CALL IMI2M(L,MXY(1))
         ELSE
             CALL IMI2M(L-K,MXY(1))
         ENDIF
         CALL IMADD(MXY(2),MXY(1),MRNX)
      ENDDO
      CALL IMMOD(MRNX,MRNM,MXY(2))
      CALL IMEQ(MXY(2),MRNX)
      START_RANDOM_SEQUENCE = 1
      J = MRNM%MP(2)
      LAST_DIGIT_OF_M_M1 = INT(MRNM%MP(J+2)) - 1
      CALL IMI2M(1,MRNC)
      CALL IMM2DP(MRNM,DPM)
      DPM = 1.0D0/DPM

      MBASE = MSAVE
      IF (MBLOGS /= MBASE) CALL FMCONS
      RETURN
      END SUBROUTINE FM_RANDOM_SEED_PUT

      SUBROUTINE FM_RANDOM_SEED_SIZE(SIZE)

!  Return the size of the SEED array used by the FM_RANDOM_NUMBER generator.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: SIZE

      SIZE = 7
      RETURN
      END SUBROUTINE FM_RANDOM_SEED_SIZE

