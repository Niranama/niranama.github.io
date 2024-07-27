!!##############################################################################
!!# ****************************************************************************
!!# <name> fparser </name>
!!# ****************************************************************************
!!#
!!# <purpose>
!!#
!!# This public domain function parser module is intended for applications
!!# where a set of mathematical expressions is specified at runtime and is
!!# then evaluated for a large number of variable values. This is done by
!!# compiling the set of function strings into byte code, which is interpreted
!!# very efficiently for the various variable values. The evaluation is
!!# straightforward and no recursions are done (uses stack arithmetic).
!!#
!!# ------------------------------------------------------------------------ \\
!!# Copyright notice \\
!!# ------------------------------------------------------------------------ \\
!!# (a) This module is based on the "Fortran 90 function parser V1.1"
!!#     written by Roland Schmehl < Roland.Schmehl@mach.uni-karlsruhe.de >
!!#     The original Fortran90 source code is available from:
!!#     http://itsextern.its.uni-karlsruhe.de/~schmehl/functionparser.html
!!#
!!#     However the "Fortran 90 function parser V1.1" only recognises the
!!#     (single argument) Fortran 90 intrinsic functions abs, exp, log10,
!!#     log, sqrt, sinh, cosh, tanh, sin, cos, tan, asin, acos, atan.
!!#
!!#     The function parser concept is based on a C++ class library written
!!#     by Warp < warp@iki.fi > available from:
!!#     http://warp.povusers.org/FunctionParser/
!!#
!!# (b) This FParser module is an extension of the `Fortran 90 function parser
!!#     V1.1` which implements most of the features available in the `function
!!#     parser library for C++ V2.8` written by Warp. The optimiser included
!!#     in the C++ library and the recursive evaluation of functions by means
!!#     of eval(...) is not implemented in this version.
!!#
!!# ------------------------------------------------------------------------ \\
!!# Basic usage \\
!!# ------------------------------------------------------------------------ \\
!!#
!!# Step 0 - Module Import \\
!!# ---------------------- \\
!!# In all program units where you want to use the function parser procedures
!!# and variables you must import the module by:
!!#
!!# <code>
!!#  use fparser
!!# </code>
!!#
!!# This command imports only 6 public names: fparser_create, fparser_release,
!!# fparser_parseFunction, fparser_evalFunction, fparser_ErrorMsg and EvalErrType
!!# which are explained in the following. The remainder of the
!!# module is hidden to the calling program.
!!#
!!# Step 1 - Initialization \\
!!# ----------------------- \\
!!# The parser module has to be initialised for the simultaneous evaluation of
!!# n functions by calling the module subroutine initp one time in your Fortran
!!# code:
!!#
!!# <code>
!!#  call fparser_create (Parser, n)
!!# </code>
!!#
!!# This allocates i=1,...,n internal data structures used by the byte-compiler
!!# and subsequently by the bytecode-interpreter in the bytecode object Comp.
!!#
!!# Step 2 - Function parsing \\
!!# ------------------------- \\
!!# The i-th function string FuncStr is parsed (checked and compiled) into the
!!# i-th bytecode by calling the module subroutine parsef:
!!#
!!# <code>
!!#  call fparser_parseFunction (Parser, i, FuncStr, Var)
!!# </code>
!!#
!!# The variable names as they appear in the string FuncStr have to be passed
!!# in the one-dimensional string array Var (zero size of Var is acceptable).
!!# The number of variables is implicitly passed by the dimension of this array.
!!# For some notes on the syntax of the function string see below.
!!#
!!# Step 3 - Function evaluation \\
!!# ---------------------------- \\
!!# The i-th function value is evaluated for a specific set of variable values
!!# by calling the module function evalf:
!!#
!!# <code>
!!#  a = fparser_evalFunction (Parser, i, Val)
!!# </code>
!!#
!!# The variable values are passed in the one-dimensional array Val which must
!!# have the same dimension as array Var.
!!#
!!# ------------------------------------------------------------------------ \\
!!# Error handling \\
!!# ------------------------------------------------------------------------ \\
!!#
!!# An error in the function parsing step leads to a detailed error message
!!# (Type and position of error) and program termination.
!!#
!!# An error during function evaluation returns a function value of 0.0 and
!!# sets the error flag EvalErrType (part of the t_fparser derived type) to
!!# a value > 0 (EvalErrType = 0 indicates no error). An error message from the
!!# bytecode-interpreter can be obtained by calling the character function
!!# fparser_ErrorMsg (Parser) with the parser object as an argument.
!!#
!!# ------------------------------------------------------------------------ \\
!!# Function string syntax \\
!!# ------------------------------------------------------------------------ \\
!!#
!!# Although they have to be passed as array elements of the same declared
!!# length (Fortran 90 restriction), the variable names can be of arbitrary
!!# actual length for the parser. Parsing for variables is case sensitive.
!!#
!!# The syntax of the function string is similar to the Fortran convention.
!!# Mathematical Operators recognised are +, -, *, /, %, ** or alternatively
!!# ^, whereas symbols for brackets must be (), [] or {}. Note that the
!!# parser does not check if, e.g. ( is closed by ) or ]. At the moment,
!!# different brackets may be used only to improve readability of the function
!!# string.
!!#
!!# Operations are evaluated in the correct order:
!!#
!!# <verb>
!!#  ()             expressions in brackets first
!!#  -A             unary minus (or plus)
!!#  A**B A^B       exponentiation (A raised to the power B)
!!#  A*B  A/B  A%B  multiplication, division and modulo
!!#  A+B  A-B       addition and subtraction
!!#  A=B  A!=B  A < B  A <= B  A > B  A >= B
!!#                 comparison between A and B (result is either 0 or 1)
!!#  A&B            result is 1 if int(A) and int(B) differ from 0, else 0.
!!#  A|B            result is 1 if int(A) or int(B) differ from 0, else 0.
!!# </verb>
!!#
!!# The function string can contain integer or real constants. To be recognised
!!# as explicit constants these must conform to the format
!!#
!!# <verb>
!!#  [+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
!!# </verb>
!!#
!!# where nnn means any number of digits. The mantissa must contain at least
!!# one digit before or following an optional decimal point. Valid exponent
!!# identifiers are 'e', 'E', 'd' or 'D'. If they appear they must be followed
!!# by a valid exponent!
!!#
!!# Note that the function parser is case insensitive.
!!# The following mathematical functions are supported
!!#
!!# <verb>
!!# abs(A)    : Absolute value of A. If A is negative, returns -A otherwise
!!#             returns A.
!!# acos(A)   : Arc-cosine of A. Returns the angle, measured in radians,
!!#             whose cosine is A.
!!# acosh(A)  : Same as acos() but for hyperbolic cosine.
!!# acot(A)   : Arc-cotangent of A (equivalent to 1/atan(A)).
!!# acoth(A)  : Same as acot() but for hyperbolic cotangens.
!!# acsc(A)   : Same as csc(A) but for hyperbolic cosecant.
!!# aint(A)   : Truncate A to a whole number
!!# anint(A)  : Rounds A to the closest integer. 0.5 is rounded to 1.
!!# asec(A)   : Same as sec(A) but for hyperbolic secant.
!!# asin(A)   : Arc-sine of A. Returns the angle, measured in radians, whose
!!#             sine is A.
!!# asinh(A)  : Same as asin() but for hyperbolic sine.
!!# atan(A)   : Arc-tangent of (A). Returns the angle, measured in radians,
!!#             whose tangent is (A).
!!# atan2(A,B): Arc-tangent of A/B. The two main differences to atan() is
!!#             that it will return the right angle depending on the signs of
!!#             A and B (atan() can only return values betwen -pi/2 and pi/2),
!!#             and that the return value of pi/2 and -pi/2 are possible.
!!# atanh(A)  : Same as atan() but for hyperbolic tangent.
!!# ceil(A)   : Ceiling of A. Returns the smallest integer greater than A.
!!#             Rounds up to the next higher integer.
!!# conj(A)   : Complex conjugate of complex number A.
!!# cos(A)    : Cosine of A. Returns the cosine of the angle A, where A is
!!#             measured in radians.
!!# cosh(A)   : Same as cos() but for hyperbolic cosine.
!!# cot(A)    : Cotangent of A (equivalent to 1/tan(A)).
!!# coth(A)   : Same as cot() but for hyperbolic tangens.
!!# csc(A)    : Cosecant of A (equivalent to 1/sin(A)).
!!# exp(A)    : Exponential of A. Returns the value of e raised to the power
!!#             A where e is the base of the natural logarithm, i.e. the
!!#             non-repeating value approximately equal to 2.71828182846.
!!# floor(A)  : Floor of A. Returns the largest integer less than A. Rounds
!!#             down to the next lower integer.
!!# if(A,B,C) : If int(A) differs from 0, the return value of this function is B,
!!#             else C. Only the parameter which needs to be evaluated is
!!#             evaluated, the other parameter is skipped.
!!# imag(A)   : Imaginary part of complex number A.
!!# log(A)    : Natural (base e) logarithm of A.
!!# log10(A)  : Base 10 logarithm of A.
!!# max(A,B)  : If A > B, the result is A, else B.
!!# min(A,B)  : If A < B, the result is A, else B.
!!# real(A)   : Real part of complex number A.
!!# sec(A)    : Secant of A (equivalent to 1/cos(A)).
!!# sin(A)    : Sine of A. Returns the sine of the angle A, where A is
!!#             measured in radians.
!!# sinh(A)   : Same as sin() but for hyperbolic sine.
!!# sign(A)   : Sign of A.
!!# sqrt(A)   : Square root of A. Returns the value whose square is A.
!!# tan(A)    : Tangent of A. Returns the tangent of the angle A, where A
!!#             is measured in radians.
!!# tanh(A)   : Same as tan() but for hyperbolic tangent.
!!# rrand(A,B): Reproducable pseudo-random number; B'th random number with
!!#             Random-Seed A.
!!# </verb>
!!#
!!# The parser also supports a number of standard constants in the function
!!# string. All constants start with an underscore '_'. The following constants
!!# are defined by default:
!!#
!!# <verb>
!!# _PI       : Gives the number $pi$.
!!# _EXP      : Gives the number $e$
!!# _INFTY    : Gives the maximum possible number in double precision, defined
!!#             in fsystem by SYS_INFINITY_DP.
!!# </verb>
!!#
!!# In addition, the user can define his own global constant which are available
!!# throughout the complete function parser as it is the case for the standard
!!# constant defined above.
!!#
!!# The parser also supports user-defined expressions which are globally
!!# available throughout the complete function parser. All expressions must
!!# start with '@' to indicate that the following expression should be
!!# looked-up from the list of predefined expressions.
!!#
!!# The following routines can be found in this module:
!!#
!!# 1.) fparser_init
!!#     -> Initialise the sub-system for function parsers
!!#
!!# 2.) fparser_done
!!#     -> Release the sub-system for function parsers
!!#
!!# 3.) fparser_defineConstant
!!#     -> Define special constants which are available for all function parsers
!!#
!!# 4.) fparser_defineExpression
!!#     -> Define special expressions which are available for all function parsers
!!#
!!# 5.) fparser_create
!!#     -> Create function parser
!!#
!!# 6.) fparser_release
!!#     -> Release function parser
!!#
!!# 7.) fparser_parseFunction = fparser_parseFunctionByName /
!!#                             fparser_parseFunctionByNumber
!!#     -> Parse function string and compile it into bytecode
!!#
!!# 8.) fparser_evalFunction = fparser_evalFuncScDbleByName /
!!#                            fparser_evalFuncScDbleByNumber /
!!#                            fparser_evalFuncBlDbleByName /
!!#                            fparser_evalFuncBlDbleByNumber /
!!#                            fparser_evalFuncScCmplByName /
!!#                            fparser_evalFuncScCmplByNumber /
!!#                            fparser_evalFuncBlCmplByName /
!!#                            fparser_evalFuncBlCmplByNumber
!!#     -> Evaluate precompiled bytecode
!!#
!!# 9.) fparser_ErrorMsg
!!#     -> Get error message from function parser
!!#
!!# 10.) fparser_PrintByteCode = fparser_PrintByteCodeByName /
!!#                              fparser_PrintByteCodeByNumber
!!#      -> Print the bytecode stack (very technical!)
!!#
!!# 11.) fparser_parseFileForKeyword
!!#      -> Parse input file for keyword
!!#
!!# 12.) fparser_getFunctionNumber
!!#      -> Return the internal number of the function
!!#
!!# 13.) fparser_initPerfConfig
!!#      -> Initialises the global performance configuration
!!#
!!# The following internal routines can be found in this module:
!!#
!!# 1.) CheckSyntax
!!#     -> Check syntax of function string before compiling bytecode
!!#
!!# 2.) isOperator
!!#     -> Return size of operator and 0 otherwise
!!#
!!# 3.) MathFunctionIndex
!!#     -> Return index of mathematical function and 0 otherwise
!!#
!!# 4.) MathFunctionParameters
!!#     -> Return number of required function parameters
!!#
!!# 5.) ConstantIndex
!!#     -> Return index of predefined constant and 0 otherwise
!!#
!!# 6.) ExpressionIndex
!!#     -> Return index of predefined expression and 0 otherwise
!!#
!!# 7.) VariableIndex
!!#     -> Return index of variable
!!#
!!# 8.) RemoveSpaces
!!#     -> Remove spaces from string
!!#
!!# 9.) Replace
!!#     -> Replace all appearances of one character set by another
!!#        character set in a given string
!!#
!!# 10.) Compile
!!#      -> Compile function string into bytecode
!!#
!!# 11.) incStackPtr
!!#     -> Increase stack pointer
!!#
!!# 12.) AddCompiledByte
!!#     -> Add compiled byte to bytecode stack
!!#
!!# 13.) RemoveCompiledByte
!!#     -> Remove last compiled byte from bytecode stack
!!#
!!# 14.) AddImmediate
!!#     -> Add immediate to immediate stack
!!#
!!# 15.) AddFunctionOpcode
!!#     -> Add function opcode to bytecode stack
!!#
!!# 16.) RealNum
!!#     -> Get real number from string
!!#
!!# 17.) FunctionSize
!!#     -> Get the total size of the function
!!#
!!# 18.) CompileExpression
!!#      -> Compile ','
!!#
!!# 19.) CompileOr
!!#      -> Compile '|'
!!#
!!# 20.) CompileAnd
!!#      -> Compile '&'
!!#
!!# 21.) CompileComparison
!!#      -> Compile '=', '<', and '>'
!!#
!!# 22.) CompileAddition
!!#      -> Compile '+' and '-'
!!#
!!# 23.) CompileMult
!!#      -> Compile '*', '/', and '%'
!!#
!!# 24.) CompileUnaryMinus
!!#      -> Compile unary '-'
!!#
!!# 25.) CompilePow
!!#      -> Compile '^'
!!#
!!# 26.) CompileElement
!!#      -> Compile mathematical function, variable, constant and number
!!#
!!# 27.) CompileFunctionParameters
!!#      -> Compile function parameters
!!#
!!# 28.) CompileIf
!!#      -> Compile if-then-else
!!#
!!# 29.) evalFunctionScDble /
!!#      evalFunctionScCmpl
!!#      -> Evaluate function for scalar data (real-/complex valued)
!!#
!!# 30.) evalFunctionBlDble /
!!#      evalFunctionBlCmpl
!!#      -> Evaluate function for multi-component data (real-/complex valued)
!!#
!!# </purpose>
!!##############################################################################

MODULE ModLib_FeatFlow_Parser

  USE ModLib_FeatFlow_System
  USE ModLib_FeatFlow_GenOutput
  USE ModLib_FeatFlow_IO
  USE ModLib_FeatFlow_PerfConfig
  USE ModLib_FeatFlow_StackInt

  ! Most compilers do not fully support complex numbers. Therefore,
  ! software implementations of the functions a[cos,sin,tan][h] are
  ! provided at the bottom of this module. Depending on the compiler
  ! either intrinsic of explicitly provided functions are called.

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: t_fparser
  PUBLIC :: fparser_initPerfConfig
  PUBLIC :: fparser_init
  PUBLIC :: fparser_done
  PUBLIC :: fparser_parseFileForKeyword
  PUBLIC :: fparser_defineConstant
  PUBLIC :: fparser_defineExpression
  PUBLIC :: fparser_create
  PUBLIC :: fparser_release
  PUBLIC :: fparser_parseFunction
  PUBLIC :: fparser_evalFunction
  PUBLIC :: fparser_evalFunctionFixed
  PUBLIC :: fparser_ErrorMsg
  PUBLIC :: fparser_PrintByteCode
  PUBLIC :: fparser_getFunctionNumber

  PUBLIC :: fparser_evalFuncBlDbleFixName
  PUBLIC :: fparser_evalFuncBlDbleFixNumber
  PUBLIC :: fparser_evalFuncBlCmplFixName
  PUBLIC :: fparser_evalFuncBlCmplFixNumber

  !****************************************************************************
  !****************************************************************************
  !****************************************************************************

  INTERFACE fparser_defineConstant
    MODULE PROCEDURE fparser_defineConstantDble
    MODULE PROCEDURE fparser_defineConstantCmpl
  END INTERFACE fparser_defineConstant

  INTERFACE fparser_evalFunction
    MODULE PROCEDURE fparser_evalFuncScDbleByName
    MODULE PROCEDURE fparser_evalFuncScDbleByNumber
    MODULE PROCEDURE fparser_evalFuncBlDbleByName
    MODULE PROCEDURE fparser_evalFuncBlDbleByNumber
    MODULE PROCEDURE fparser_evalFuncScCmplByName
    MODULE PROCEDURE fparser_evalFuncScCmplByNumber
    MODULE PROCEDURE fparser_evalFuncBlCmplByName
    MODULE PROCEDURE fparser_evalFuncBlCmplByNumber
  END INTERFACE fparser_evalFunction

  INTERFACE fparser_evalFunctionFixed
    MODULE PROCEDURE fparser_evalFuncBlDbleFixName
    MODULE PROCEDURE fparser_evalFuncBlDbleFixNumber
    MODULE PROCEDURE fparser_evalFuncBlCmplFixName
    MODULE PROCEDURE fparser_evalFuncBlCmplFixNumber
  END INTERFACE fparser_evalFunctionFixed

  INTERFACE fparser_parseFunction
    MODULE PROCEDURE fparser_parseFunctionByName
    MODULE PROCEDURE fparser_parseFunctionByNumber
  END INTERFACE

  INTERFACE fparser_printByteCode
    MODULE PROCEDURE fparser_printByteCodeByName
    MODULE PROCEDURE fparser_printByteCodeByNumber
  END INTERFACE fparser_printByteCode

  !****************************************************************************
  !****************************************************************************
  !****************************************************************************

!<constants>

!<constantblock description="Global constants for parser">

  ! *** LEGACY CONSTANT, use the more flexible performance configuration ***
  ! Number of items to handle simultaneously when evaluating functions
  INTEGER, PARAMETER, PUBLIC :: FPAR_NITEMSIM       = 256

  ! Length of string
  INTEGER, PARAMETER, PUBLIC :: FPAR_STRLEN         = 2048

  ! Maximum number of predefined/user-defined constants
  INTEGER, PARAMETER, PUBLIC :: FPAR_MAXCONSTS      = 128

  ! Maximum number of predefined/user-defined expressions
  INTEGER, PARAMETER, PUBLIC :: FPAR_MAXEXPRESSIONS = 128

  ! Length of constant name
  INTEGER, PARAMETER, PUBLIC :: FPAR_CONSTLEN       = 32

  ! Length of expression name
  INTEGER, PARAMETER, PUBLIC :: FPAR_EXPRLEN        = 32

  ! Length of function name
  INTEGER, PARAMETER, PUBLIC :: FPAR_FUNCLEN        = 32

  ! Length of variable name
  INTEGER, PARAMETER, PUBLIC :: FPAR_VARLEN         = 32

!</constantblock>


!<constantblock description="types for parser expressions">

  ! Constant
  INTEGER, PARAMETER, PUBLIC :: FPAR_CONSTANT   = 1

  ! Expression
  INTEGER, PARAMETER, PUBLIC :: FPAR_EXPRESSION = 2

  ! Functions (including symbolic variables)
  INTEGER, PARAMETER, PUBLIC :: FPAR_FUNCTION   = 3

!</constantblock>


!<constantblock description="data type for parser bytecode">

  ! Data type of bytecode
  INTEGER, PARAMETER :: is = SELECTED_INT_KIND(1)

!</constantblock>


!<constantblock description="keywords for parser">

  INTEGER(is), PARAMETER :: cImmed       =  1, &
                            cJump        =  2, &
                            cNeg         =  3, &
                            cDeg         =  4, &
                            cRad         =  5, &
                            cAdd         =  6, & ! <-- first dyadic operator: A .OP. B
                            cSub         =  7, &
                            cMul         =  8, &
                            cDiv         =  9, &
                            cMod         = 10
  INTEGER(is), PARAMETER :: cPow         = 11, &
                            cNEqual      = 12, & ! NOTE: != must be prior to =
                            cEqual       = 13, &
                            cLessOrEq    = 14, & ! NOTE: <= must be prior to <
                            cLess        = 15, &
                            cGreaterOrEq = 16, & ! NOTE: >= must be prior to >
                            cGreater     = 17, &
                            cNot         = 18, &
                            cAnd         = 19, &
                            cOr          = 20    ! --> last dyadic operator: A.OP. B
  INTEGER(is), PARAMETER :: cIf          = 21, & ! <-- if-then-else
                            cMin         = 22, & ! <-- first dyadic operator: .OP.(A,B)
                            cMax         = 23, &
                            cRrand       = 24, &
                            cCmplx       = 25, &
                            cAtan2       = 26, & ! --> last dyadic operator: .OP.(A,B)
                            cAbs         = 27, & ! <-- monadic operator: .OP.(A)
                            cAnint       = 28, &
                            cAint        = 29, &
                            CEXP         = 30, &
                            cLog10       = 31, &
                            CLOG         = 32, &
                            CSQRT        = 33, &
                            cCeil        = 34, &
                            cFloor       = 35
  INTEGER(is), PARAMETER :: cAsinh       = 36, &
                            cAsin        = 37, &
                            cSinh        = 38, &
                            CSIN         = 39, &
                            cACosh       = 40, &
                            cAcos        = 41, &
                            cCosh        = 42, &
                            CCOS         = 43
  INTEGER(is), PARAMETER :: cAtanh       = 44, &
                            cAtan        = 45, &
                            cTanh        = 46, &
                            cTan         = 47, &
                            cAcoth       = 48, &
                            cAcot        = 49, &
                            cCoth        = 50, &
                            cCot         = 51
  INTEGER(is), PARAMETER :: cAsech       = 52, &
                            cAsec        = 53, &
                            cSech        = 54, &
                            cSec         = 55, &
                            cAcsch       = 56, &
                            cAcsc        = 57, &
                            cCsch        = 58, &
                            cCsc         = 59, &
                            cReal        = 60, &
                            cImag        = 61, &
                            cConj        = 62, &
                            cSign        = 63, & ! --> last monadic operator: .OP.(A)
                            VarBegin     = 64

!</constantblock>


!<constantblock description="symbols for parser operands">

  CHARACTER (LEN=2), DIMENSION(cAdd:cOr), PARAMETER :: Ops = (/ '+ ', &
                                                                '- ', &
                                                                '* ', &
                                                                '/ ', &
                                                                '% ', &
                                                                '^ ', &
                                                                '!=', &
                                                                '= ', &
                                                                '<=', &
                                                                '< ', &
                                                                '>=', &
                                                                '> ', &
                                                                '! ', &
                                                                '& ', &
                                                                '| ' /)

!</constantblock>


!<constantblock description="function names for parser">

  CHARACTER (LEN=5), DIMENSION(cIf:cSign), PARAMETER :: Funcs = (/ 'if   ', &
                                                                   'min  ', &
                                                                   'max  ', &
                                                                   'rrand', &
                                                                   'cmplx', &
                                                                   'atan2', &
                                                                   'abs  ', &
                                                                   'anint', &
                                                                   'aint ', &
                                                                   'exp  ', &
                                                                   'log10', &
                                                                   'log  ', &
                                                                   'sqrt ', &
                                                                   'ceil ', &
                                                                   'floor', &
                                                                   'asinh', &
                                                                   'asin ', &
                                                                   'sinh ', &
                                                                   'sin  ', &
                                                                   'acosh', &
                                                                   'acos ', &
                                                                   'cosh ', &
                                                                   'cos  ', &
                                                                   'atanh', &
                                                                   'atan ', &
                                                                   'tanh ', &
                                                                   'tan  ', &
                                                                   'acoth', &
                                                                   'acot ', &
                                                                   'coth ', &
                                                                   'cot  ', &
                                                                   'asech', &
                                                                   'asec ', &
                                                                   'sech ', &
                                                                   'sec  ', &
                                                                   'acsch', &
                                                                   'acsc ', &
                                                                   'csch ', &
                                                                   'csc  ', &
                                                                   'real ', &
                                                                   'imag ', &
                                                                   'conj ', &
                                                                   'sign '/)

!</constantblock>


!<constantblock description="predefined constant names for parser; an underscore '_' is automatically added">

  CHARACTER(LEN=FPAR_CONSTLEN), DIMENSION(3) :: PredefinedConsts = (/ 'pi        ', &
                                                                      'exp       ', &
                                                                      'infty     ' /)

!</constantblock>


!<constantblock description="predefined real-valued constant values for parser">

  REAL(DP), DIMENSION(3), PARAMETER :: PredefinedConstvals = (/&
      3.141592653589793115997963468544185161590576171875_DP, &
      2.718281828459045090795598298427648842334747314453125_DP, &
      SYS_INFINITY_DP/)

!</constantblock>


!<constantblock description="predefined expression names for parser; an at-sign '@' is automatically added">

  CHARACTER(LEN=FPAR_CONSTLEN), DIMENSION(1) :: PredefinedExpressions = (/ 'null      ' /)

!</constantblock>


!<constantblock description="predefined expressions for parser">

  CHARACTER(LEN=FPAR_CONSTLEN), DIMENSION(1) :: PredefinedExpressionvals = (/ '0         ' /)

!</constantblock>

!</constants>

  !****************************************************************************
  !****************************************************************************
  !****************************************************************************

!<publicvars>

  ! Global number of predefined/user-defined constants
  INTEGER, SAVE :: nconstants = 0

  ! Global number of predefined/user-defined expressions
  INTEGER, SAVE :: nexpressions = 0

  ! Global constant names for parser
  CHARACTER(LEN=FPAR_CONSTLEN), DIMENSION(FPAR_MAXCONSTS), SAVE :: CconstantName  = '     '

  ! Global constant values for parser (real-valued)
  REAL(DP), DIMENSION(FPAR_MAXCONSTS), SAVE :: DconstantValue = 0.0_DP

  ! Global constant values for parser (complex-valued)
  COMPLEX(DP), DIMENSION(FPAR_MAXCONSTS), SAVE :: ZconstantValue = 0.0_DP

  ! Global expression name for parser
  CHARACTER(LEN=FPAR_EXPRLEN), DIMENSION(FPAR_MAXCONSTS), SAVE :: CexpressionName = ''

  ! Global expression string for parser
  CHARACTER(LEN=FPAR_STRLEN), DIMENSION(FPAR_MAXEXPRESSIONS), SAVE :: CexpressionString

!</publicvars>

  !****************************************************************************
  !****************************************************************************
  !****************************************************************************

!<types>

!<typeblock>

  ! Type block for storing all information of the function parser
  TYPE t_fparser
    PRIVATE

    ! Array of function parser components.
    ! Each component is used to handle one function string at a time
    TYPE(t_fparserComponent), DIMENSION(:), POINTER :: Rcomp => NULL()

    ! Array of function names corresponding to the individual components.
    CHARACTER(LEN=FPAR_FUNCLEN), DIMENSION(:), POINTER :: ScompName => NULL()

    ! Number of parser components
    INTEGER :: ncomp = 0

    ! Maximum number of components
    INTEGER :: nncomp = 0

  END TYPE t_fparser

!</typeblock>

!<typeblock>

  ! Type block for storing the bytecode of the function parser for one component
  TYPE t_fparserComponent
    PRIVATE

    ! Size of bytecode
    INTEGER :: ibytecodeSize = 0

    ! Size of immediates
    INTEGER :: iimmedSize = 0

    ! Stack size
    INTEGER :: istackSize = 0

    ! Stack pointer
    INTEGER :: istackPtr = 0

    ! Use degree conversion DEG <-> RAD for some functions
    LOGICAL :: buseDegreeConversion = .false.

    ! Is vectorizable
    LOGICAL :: bisVectorizable = .true.

    ! Is complex-valued
    LOGICAL :: bisComplex = .false.

    ! Bytecode
    INTEGER(is), DIMENSION(:), POINTER :: IbyteCode => NULL()

    ! Immediates (real-valued)
    REAL(DP), DIMENSION(:), POINTER :: Dimmed => NULL()

    ! Immediates (complex-valued)
    COMPLEX(DP), DIMENSION(:), POINTER :: Zimmed => NULL()
  END TYPE t_fparserComponent
!</typeblock>

!</types>

  !************************************************************************

  ! global performance configuration
  TYPE(t_perfconfig), TARGET, SAVE :: fparser_perfconfig

  !************************************************************************

CONTAINS

  !****************************************************************************

!<subroutine>

  SUBROUTINE fparser_initPerfConfig(rperfconfig)

!<description>
  ! This routine initialises the global performance configuration
!</description>

!<input>
  ! OPTIONAL: performance configuration that should be used to initialise
  ! the global performance configuration. If not present, the values of
  ! the legacy constants is used.
  TYPE(t_perfconfig), INTENT(in), OPTIONAL :: rperfconfig
!</input>
!</subroutine>

    IF (PRESENT(rperfconfig)) THEN
      fparser_perfconfig = rperfconfig
    ELSE
      CALL pcfg_initPerfConfig(fparser_perfconfig)
      fparser_perfconfig%NELEMSIM = FPAR_NITEMSIM
    END IF

  END SUBROUTINE fparser_initPerfConfig

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_init()

!<description>
    ! Initialise function parser
!</description>

!</subroutine>

    ! local variables
    INTEGER :: i

    ! Initialise predefined constants
    DO i = LBOUND(PredefinedConsts, 1),&
           UBOUND(PredefinedConsts, 1)
      CALL fparser_defineConstant(PredefinedConsts(i),&
                                  PredefinedConstvals(i))
    END DO

    ! Initialise predefined expressions
    DO i = LBOUND(PredefinedExpressions, 1),&
           UBOUND(PredefinedExpressions, 1)
      CALL fparser_defineExpression(PredefinedExpressions(i),&
                                    PredefinedExpressionvals(i))
    END DO

  END SUBROUTINE fparser_init

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_done()

!<description>
    ! Release function parser
!</description>
!</subroutine>

    ! Reset constants
    nconstants     = 0
    Cconstantname  = '     '
    DconstantValue = 0.0_DP
    ZconstantValue = CMPLX(0.0_DP,0.0_DP,DP)

    ! Reset expressions
    nexpressions      = 0
    CexpressionName   = ''
    CexpressionString = ''

  END SUBROUTINE fparser_done

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_parseFileForKeyword(rfparser, sfilename, ckeyword, itype)

!<description>
    ! Parse the file for the given keyword and make it a constant, a
    ! predefined expression or a function depending on the variable itype
!</description>

!<input>
    ! Name of parameter file to be parsed
    CHARACTER(LEN=*), INTENT(in) :: sfilename

    ! Name of keyword to parser for
    CHARACTER(LEN=*), INTENT(in) :: ckeyword

    ! Type of keyword: FPAR_CONSTANT, FPAR_EXPRESSION, FPAR_FUNCTION
    INTEGER, INTENT(in) :: itype
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparser), INTENT(inout) :: rfparser
!</inputoutput>
!</subroutine>

    ! local variables
    CHARACTER(FPAR_VARLEN), DIMENSION(:), ALLOCATABLE :: Svariables
    CHARACTER(SYS_STRLEN) :: skeyword
    CHARACTER(FPAR_CONSTLEN) :: sconstName
    CHARACTER(FPAR_EXPRLEN) :: sexpressionName
    CHARACTER(FPAR_FUNCLEN) :: sfunctionName
    CHARACTER(FPAR_STRLEN) :: sdata,svalue,svariable
    INTEGER :: iunit,ios,ipos,jpos,kpos,ivar,idatalen,icomp
    REAL(DP) :: dvalue
    COMPLEX(DP) :: zvalue

    ! Try to open the file
    CALL io_openFileForReading(sfilename, iunit, .true.)

    ! Oops...
    IF (iunit .eq. -1) THEN
      CALL output_line('Unable to open input file!',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_parseFileForKeyword')
      CALL sys_halt()
    END IF

    ! Read through the complete input file and look for global
    ! definitions of constants and fixed expressions
    ios = 0
    readline: DO WHILE(ios .eq. 0)

      ! Read next line in file
      CALL io_readlinefromfile(iunit, sdata, idatalen, ios)

      ! Check for keyword defconst or defexp
      ipos = SCAN(sdata(1:idatalen), ":")
      IF (ipos .eq. 0) CYCLE

      CALL sys_tolower(sdata(1:MAX(1, ipos-1)), skeyword)
      IF (TRIM(ADJUSTL(skeyword)) .eq. ckeyword) THEN

        ! We found a keyword that will be applied to the parser
        SELECT CASE(itype)
        CASE (FPAR_CONSTANT)

          ! Split the line into name and value
          jpos = SCAN(sdata(1:idatalen), "=")
          sconstName = TRIM(ADJUSTL(sdata(ipos+1:jpos-1)))
          svalue = TRIM(ADJUSTL(sdata(jpos+1:)))

          ! Is this a real-valued constant?
          READ(svalue,*,iostat=ios) dvalue
          IF (ios .eq. 0) THEN
            CALL fparser_defineConstant(sconstName, dvalue)
          ELSE
            ! Is this a complex-valued constant?
            READ(svalue,*,iostat=ios) zvalue
            IF (ios .eq. 0) THEN
              CALL fparser_defineConstant(sconstName, zvalue)
            ELSE
              CALL output_line('Constant is neither real- nor complex-valued!',&
                  OU_CLASS_ERROR, OU_MODE_STD,'fparser_parseFileForKeyword')
              CALL sys_halt()
            END IF
          END IF

        CASE (FPAR_EXPRESSION)
          ! Split the line into name and value
          jpos = SCAN(sdata(1:idatalen), "=")
          sexpressionName = TRIM(ADJUSTL(sdata(ipos+1:jpos-1)))
          svalue = TRIM(ADJUSTL(sdata(jpos+1:)))

          ! Concatenate multi-line expressions
          DO WHILE(ios .eq. 0)
            ! Get length of expression
            ipos = LEN_TRIM(svalue)

            ! Check if expression is continued in the following line
            IF (svalue(MAX(1, ipos-2):ipos) .eq. '...') THEN
              ipos = ipos-2
            ELSE
              EXIT
            END IF

            ! Read next line in file
            CALL io_readlinefromfile(iunit, sdata, idatalen, ios)
            IF (ios .ne. 0) THEN
              CALL output_line('Syntax error in input file!',&
                  OU_CLASS_ERROR, OU_MODE_STD,'fparser_parseFileForKeyword')
              CALL sys_halt()
            END IF

            ! Append line
            svalue(ipos:) = TRIM(ADJUSTL(sdata(1:idatalen)))
          END DO

          CALL fparser_defineExpression(sexpressionName, svalue)

        CASE (FPAR_FUNCTION)

          ! Split the line into name, expression and symbolic variables
          jpos = SCAN(sdata(1:idatalen), "=")
          sfunctionName = TRIM(ADJUSTL(sdata(ipos+1:jpos-1)))
          svalue(1:) = sdata(jpos+1:idatalen)

          ! Check if function name already exists
          DO icomp = 1, rfparser%ncomp
            IF (TRIM(ADJUSTL(sfunctionName)) .eq.&
                TRIM(rfparser%ScompName(icomp))) CYCLE readline
          END DO

          ! Concatenate multi-line expressions
          DO WHILE(ios .eq. 0)
            ! Get length of expression
            ipos = LEN_TRIM(svalue)

            ! Check if expression is continued in the following line
            IF (svalue(MAX(1, ipos-2):ipos) .eq. '...') THEN
              ipos = ipos-2
            ELSE
              EXIT
            END IF

            ! Read next line in file
            CALL io_readlinefromfile(iunit, sdata, idatalen, ios)
            IF (ios .ne. 0) THEN
              CALL output_line('Syntax error in input file!',&
                  OU_CLASS_ERROR, OU_MODE_STD,'fparser_parseFileForKeyword')
              CALL sys_halt()
            END IF

            ! Append line
            svalue(ipos:) = TRIM(ADJUSTL(sdata(1:idatalen)))
          END DO

          ! Extract the symbolic variables
          jpos = SCAN(svalue(1:ipos), ";", .true.)
          svariable = svalue(jpos+1:ipos)

          kpos = SCAN(svariable, ","); ivar = 0
          DO WHILE (kpos .ne. 0)
            ivar = ivar+1
            svariable = TRIM(ADJUSTL(svariable(kpos+1:LEN_TRIM(svariable))))
            kpos = SCAN(svariable, ",")
          END DO

          ! Allocate temporal memory
          ALLOCATE(Svariables(ivar+1))

          ! Initialise symbolic variables
          svariable = svalue(jpos+1:ipos)

          kpos = SCAN(svariable, ","); ivar = 0
          DO WHILE (kpos .ne. 0)
            ivar = ivar+1
            Svariables(ivar) = TRIM(ADJUSTL(svariable(1:kpos-1)))
            svariable = TRIM(ADJUSTL(svariable(kpos+1:LEN_TRIM(svariable))))
            kpos = SCAN(svariable, ",")
          END DO
          Svariables(ivar+1) = TRIM(ADJUSTL(svariable(1:LEN_TRIM(svariable))))

          ! Parse the function
          CALL fparser_parseFunction(rfparser, sfunctionName, svalue(1:jpos-1), Svariables)

          ! Deallocate(temporal memory
          DEALLOCATE(Svariables)


        CASE default
          CALL output_line('Invalid type of expression!',&
              OU_CLASS_ERROR, OU_MODE_STD,'fparser_parseFileForKeyword')
          CALL sys_halt()
        END SELECT

      END IF
    END DO readline

    ! Close file
    CLOSE (iunit)

  END SUBROUTINE fparser_parseFileForKeyword

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_defineConstantDble(sname, dvalue)

!<description>
    ! Define a new real-valued constant for the function parser.
    ! This subroutine checks if the given constant is already defined.
!</description>

!<input>
    ! Name of the constant
    CHARACTER(LEN=FPAR_CONSTLEN), INTENT(in) :: sname

    ! Value of the constant
    REAL(DP), INTENT(in) :: dvalue
!</input>
!</subroutine>

    ! local variables
    CHARACTER(LEN=LEN(sname)) :: sstring
    INTEGER :: iconst

    ! Check if there is enough space
    IF (nconstants .lt. FPAR_MAXCONSTS) THEN
      nconstants = nconstants+1
    ELSE
      CALL output_line('No space left for definition of constant!',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_defineConstantDble')
      CALL sys_halt()
    END IF

    ! Prepare constant
    CALL sys_tolower(sname, sstring)

    ! Check if constant is already defined
    DO iconst = 1, nconstants-1
      IF (CconstantName(iconst) .eq. sstring) THEN
        ! If it is already defined, then it must not have a different value
        IF(DconstantValue(iconst) .ne. dvalue) THEN
          CALL output_line('Constant is already defined with different value!',&
              OU_CLASS_ERROR, OU_MODE_STD,'fparser_defineConstantDble')
          CALL sys_halt()
        ELSE
          nconstants = nconstants-1
          RETURN
        END IF
      END IF
    END DO

    ! Apply constant value and constant name
    CconstantName(nconstants)  = sstring
    DconstantValue(nconstants) = dvalue
    ZconstantValue(nconstants) = CMPLX(dvalue,0.0_DP,DP)

  END SUBROUTINE fparser_defineConstantDble

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_defineConstantCmpl(sname, zvalue)

!<description>
    ! Define a new complex-valued constant for the function parser.
    ! This subroutine checks if the given constant is already defined.
!</description>

!<input>
    ! Name of the constant
    CHARACTER(LEN=FPAR_CONSTLEN), INTENT(in) :: sname

    ! Value of the constant
    COMPLEX(DP), INTENT(in) :: zvalue
!</input>
!</subroutine>

    ! local variables
    CHARACTER(LEN=LEN(sname)) :: sstring
    INTEGER :: iconst

    ! Check if there is enough space
    IF (nconstants .lt. FPAR_MAXCONSTS) THEN
      nconstants = nconstants+1
    ELSE
      CALL output_line('No space left for definition of constant!',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_defineConstantCmpl')
      CALL sys_halt()
    END IF

    ! Prepare constant
    CALL sys_tolower(sname, sstring)

    ! Check if constant is already defined
    DO iconst = 1, nconstants-1
      IF (CconstantName(iconst) .eq. sstring) THEN
        ! If it is already defined, then it must not have a different value
        IF(ZconstantValue(iconst) .ne. zvalue) THEN
          CALL output_line('Constant is already defined with different value!',&
              OU_CLASS_ERROR, OU_MODE_STD,'fparser_defineConstantCmpl')
          CALL sys_halt()
        ELSE
          nconstants = nconstants-1
          RETURN
        END IF
      END IF
    END DO

    ! Apply constant value and constant name
    CconstantName(nconstants)  = sstring
    DconstantValue(nconstants) = REAL(zvalue)
    ZconstantValue(nconstants) = zvalue

  END SUBROUTINE fparser_defineConstantCmpl

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_defineExpression(sname, svalue)

!<description>
    ! Define a new expression for the function parser.
    ! This subroutine checks if the given expression is already defined.
!</description>

!<input>
    ! Name of the expression
    CHARACTER(LEN=FPAR_EXPRLEN), INTENT(in) :: sname

    ! String of the expression
    CHARACTER(LEN=*), INTENT(in) :: svalue
!</input>
!</subroutine>

    ! local variables
    CHARACTER(LEN=LEN(sname)) :: sexpression
    CHARACTER(LEN=LEN(svalue)) :: sstring
    INTEGER :: iexpression

    ! Check if there is enough space
    IF (nexpressions .lt. FPAR_MAXEXPRESSIONS) THEN
      nexpressions = nexpressions+1
    ELSE
      CALL output_line('No space left for definition of expression!',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_defineExpression')
      CALL sys_halt()
    END IF

    ! Prepare expression string
    CALL sys_tolower(sname, sexpression)
    CALL sys_tolower(svalue, sstring)

    ! Replace human readable function names by 1-Char. format
    CALL Replace ('**','^ ', sstring)
    CALL Replace ('[','(',   sstring)
    CALL Replace (']',')',   sstring)
    CALL Replace ('{','(',   sstring)
    CALL Replace ('}',')',   sstring)

    ! Condense function string
    CALL RemoveSpaces (sstring)

    ! Check if expressions is already defined
    DO iexpression = 1, nexpressions-1
      IF (CexpressionName(iexpression) .eq. sexpression) THEN
        ! If it is already defined, then it must not have a different value
        IF(CexpressionString(iexpression) .ne. sstring) THEN
          CALL output_line('Expression is already defined with different string!',&
              OU_CLASS_ERROR, OU_MODE_STD,'fparser_defineExpression')
          CALL sys_halt()
        ELSE
          nexpressions = nexpressions-1
          RETURN
        END IF
      END IF
    END DO

    ! Apply expressions string and expression name
    CexpressionName(nexpressions)   = sexpression
    CexpressionString(nexpressions) = sstring

  END SUBROUTINE fparser_defineExpression

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_create (rfparser, nncomp)

!<description>
    ! Initialise function parser for nncomp functions.
!</description>

!<input>
    ! Number of functions
    INTEGER, INTENT(in) :: nncomp
!</input>

!<output>
    ! Function parser object
    TYPE(t_fparser), INTENT(out) :: rfparser
!</output>
!</subroutine>

    ! Set number of components
    rfparser%nncomp = nncomp
    rfparser%ncomp  = 0

    ! Allocate arrays
    ALLOCATE (rfparser%Rcomp(nncomp))
    ALLOCATE (rfparser%ScompName(nncomp))

  END SUBROUTINE fparser_create

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_release (rfparser)

!<description>
    ! Release function parser and all of its coponents
!</description>

!<inputoutput>
    ! Function parser
    TYPE(t_fparser), INTENT(inout) :: rfparser
!</inputoutput>
!</subroutine>

    ! local variables
    INTEGER :: icomp

    ! Check that pointer is associated and return otherwise
    IF (.NOT.ASSOCIATED(rfparser%Rcomp)) RETURN

    ! Loop over all components and deallocate arrays
    DO icomp = 1, rfparser%nncomp
      IF (ASSOCIATED(rfparser%Rcomp(icomp)%IbyteCode)) DEALLOCATE(rfparser%Rcomp(icomp)%IbyteCode)
      IF (ASSOCIATED(rfparser%Rcomp(icomp)%Dimmed))    DEALLOCATE(rfparser%Rcomp(icomp)%Dimmed)
      IF (ASSOCIATED(rfparser%Rcomp(icomp)%Zimmed))    DEALLOCATE(rfparser%Rcomp(icomp)%Zimmed)
    END DO

    ! Deallocate memory
    DEALLOCATE(rfparser%Rcomp)
    DEALLOCATE(rfparser%ScompName)

    ! Reset scalar data
    rfparser%nncomp = 0
    rfparser%ncomp  = 0

  END SUBROUTINE fparser_release

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_parseFunctionByName (rfparser, scompName, sfunctionString,&
                                          Svariables, buseDegrees, icomp)

!<description>
    ! Parse function string sfuncStr and compile it into bytecode
!</description>

!<input>
    ! Function identifier
    CHARACTER (LEN=*), INTENT(in) :: scompName

    ! Function string
    CHARACTER (LEN=*), INTENT(in) :: sfunctionString

    ! Array with variable names
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Svariables

    ! OPTIONAL
    LOGICAL, INTENT(in), OPTIONAL :: buseDegrees
!</input>

!<inputoutput>
    ! Function parser
    TYPE (t_fparser), INTENT(inout) :: rfparser
!</inputoutput>

!<output>
    ! OPTIONAL: Function identifier
    INTEGER, OPTIONAL :: icomp
!</output>

!</subroutine>

    ! Check if there is space for a new component
    IF (rfparser%ncomp .eq. rfparser%nncomp) THEN
      CALL output_line('No free components left!',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_parseFunctionByName')
      CALL sys_halt()
    END IF

    ! Increase component counter
    rfparser%ncomp = rfparser%ncomp+1

    ! Store function name
    CALL sys_tolower(TRIM(ADJUSTL(scompName)), rfparser%ScompName(rfparser%ncomp))

    ! Parse function
    CALL fparser_parseFunction (rfparser, rfparser%ncomp, sfunctionString,&
                                Svariables, buseDegrees)

    ! Return function identifier
    IF (PRESENT(icomp)) icomp = rfparser%ncomp

  END SUBROUTINE fparser_parseFunctionByName

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_parseFunctionByNumber (rfparser, icomp, sfunctionString,&
                                            Svariables, buseDegrees)

!<description>
    ! Parse ith function string sfuncStr and compile it into bytecode
!</description>

!<input>
    ! Function identifier
    INTEGER, INTENT(in) :: icomp

    ! Function string
    CHARACTER (LEN=*), INTENT(in) :: sfunctionString

    ! Array with variable names
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Svariables

    ! OPTIONAL
    LOGICAL, INTENT(in), OPTIONAL :: buseDegrees
!</input>

!<inputoutput>
    ! Function parser
    TYPE (t_fparser), INTENT(inout) :: rfparser
!</inputoutput>
!</subroutine>

    ! local variables
    CHARACTER (LEN=LEN(sfunctionString)) :: sstring

    ! Check if component is valid
    IF (icomp .lt. 1 .OR. icomp .gt. rfparser%nncomp) THEN
      CALL output_line('Component number is out of range',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_parseFunctionByNumber')
      CALL sys_halt()
    END IF

    ! Local copy of function string
    sstring = sfunctionString

    ! Replace human readable function names by 1-Char. format
    CALL Replace ('**','^ ', sstring)
    CALL Replace ('[','(',   sstring)
    CALL Replace (']',')',   sstring)
    CALL Replace ('{','(',   sstring)
    CALL Replace ('}',')',   sstring)

    ! Condense function string
    CALL RemoveSpaces (sstring)

    ! Check for valid syntax; this prevents the bytecode compiler
    ! from running into endless loops or other problems
    CALL CheckSyntax (sstring, Svariables)

    ! Check if conversion to degrees is required
    IF (PRESENT(buseDegrees))&
        rfparser%Rcomp(icomp)%buseDegreeConversion = buseDegrees

    ! If syntax is correct, then compile into bytecode
    CALL Compile (rfparser%Rcomp(icomp), sstring, Svariables)

  END SUBROUTINE fparser_parseFunctionByNumber

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncScDbleByName (rfparser, scompName, Dvalue, dresult)

!<description>
    ! Evaluate bytecode of function named scompName for the
    ! real-valued values passed in array Dvalue(:).
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function name
    CHARACTER(LEN=*), INTENT(in) :: scompName

    ! Variable values
    REAL(DP), DIMENSION(:), INTENT(in) :: Dvalue
!</input>

!<output>
    ! Evaluated function
    REAL(DP), INTENT(out)  :: dresult
!</output>
!</subroutine>

    ! local variables
    INTEGER :: icomp

    ! Lookup function by name
    icomp = fparser_getFunctionNumber(rfparser, scompName)

    ! Evaluate function by number
    CALL fparser_evalFunction (rfparser, icomp, Dvalue, dresult)

  END SUBROUTINE fparser_evalFuncScDbleByName

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncScCmplByName (rfparser, scompName, Zvalue, zresult)

!<description>
    ! Evaluate bytecode of function named scompName for the
    ! complex-valued values passed in array Zvalues(:).
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function name
    CHARACTER(LEN=*), INTENT(in) :: scompName

    ! Variable values
    COMPLEX(DP), DIMENSION(:), INTENT(in) :: Zvalue
!</input>

!<output>
    ! Evaluated function
    COMPLEX(DP), INTENT(out)  :: zresult
!</output>
!</subroutine>

    ! local variables
    INTEGER :: icomp

    ! Lookup function by name
    icomp = fparser_getFunctionNumber(rfparser, scompName)

    ! Evaluate function by number
    CALL fparser_evalFunction (rfparser, icomp, Zvalue, zresult)

  END SUBROUTINE fparser_evalFuncScCmplByName

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncBlDbleByName (rfparser, scompName, idim, DValueBlock,&
                                           Dresult, DvalueScalar, rperfconfig)

!<description>
    ! Evaluate bytecode of function named scompName for the
    ! real-valued array of values passed in array DvalueBlock(:,:). In
    ! some situations, there a variables, such as nodal coordinates,
    ! which are different for each component of the resulting vector
    ! and those which are the same, e.g., time variable.  The latter
    ! ones can be passed to the DvalueScalar argument which is used
    ! uniformly for each component of Dresult.
    !
    ! WARNING: The ordering of the variables must be identical to that given
    ! during the byte-code compilation. Care must be taken by the user since
    ! this cannot be checked be the function parser. Hence, if both ValBlock
    ! and DvalueScalar should be used, then the first variables must stored as
    ! blocks whereas the last variables can be scalar. This sound slightly
    ! complicated but here is an example:
    !
    ! Suppose you want to evaluate a function f=f(x,y,t). You know that x,y
    ! corresponds to the coordinate vector and t denotes the time. Then
    ! you should order your variables according to [x,y,t]. If the function
    ! should be evaluated for a set of variables then DvalueBlock=[x,y] and
    ! DvalueScalar=[t] works fine.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function name
    CHARACTER(LEN=*), INTENT(in) :: scompName

    ! Orientation of the stored values
    ! idim =1 : DvalueBlock is organised as (x1:xN),(y1:yN),...
    ! idim =2 : DvalueBlock is organised as (x1,y1),(x2,y2),...,(xN,yN)
    INTEGER, INTENT(in) :: idim

    ! Variable values (must have the same dimension as Dresult)
    REAL(DP), DIMENSION(:,:), INTENT(in) :: DvalueBlock

    ! Variable values. This is a vector of scalar variables
    ! which is the same for all components of Res, e.g. the time variable.
    REAL(DP), DIMENSION(:), INTENT(in), OPTIONAL :: DvalueScalar

    ! OPTIONAL: local performance configuration. If not given, the
    ! global performance configuration is used.
    TYPE(t_perfconfig), INTENT(in), TARGET, OPTIONAL :: rperfconfig
!</input>

!<output>
    ! Evaluated function
    REAL(DP), DIMENSION(:), INTENT(out) :: Dresult
!</output>
!</subroutine>

    ! local variables
    INTEGER :: icomp

    ! Lookup function by name
    icomp = fparser_getFunctionNumber(rfparser, scompName)

    ! Evaluate function by number
    CALL fparser_evalFunction (rfparser, icomp, idim, DvalueBlock,&
                               Dresult, DvalueScalar, rperfconfig)

  END SUBROUTINE fparser_evalFuncBlDbleByName

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncBlCmplByName (rfparser, scompName, idim, ZValueBlock,&
                                           Zresult, ZvalueScalar, rperfconfig)

!<description>
    ! Evaluate bytecode of function named scompName for the array of
    ! complex-valued values passed in array ZvalueBlock(:,:). In some
    ! situations, there a variables, such as nodal coordinates, which
    ! are different for each component of the resulting vector and
    ! those which are the same, e.g., time variable.  The latter ones
    ! can be passed to the ZvalueScalar argument which is used
    ! uniformly for each component of Dresult.
    !
    ! WARNING: The ordering of the variables must be identical to that given
    ! during the byte-code compilation. Care must be taken by the user since
    ! this cannot be checked be the function parser. Hence, if both ValBlock
    ! and DvalueScalar should be used, then the first variables must stored as
    ! blocks whereas the last variables can be scalar. This sound slightly
    ! complicated but here is an example:
    !
    ! Suppose you want to evaluate a function f=f(x,y,t). You know that x,y
    ! corresponds to the coordinate vector and t denotes the time. Then
    ! you should order your variables according to [x,y,t]. If the function
    ! should be evaluated for a set of variables then ZvalueBlock=[x,y] and
    ! ZvalueScalar=[t] works fine.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function name
    CHARACTER(LEN=*), INTENT(in) :: scompName

    ! Orientation of the stored values
    ! idim =1 : DvalueBlock is organised as (x1:xN),(y1:yN),...
    ! idim =2 : DvalueBlock is organised as (x1,y1),(x2,y2),...,(xN,yN)
    INTEGER, INTENT(in) :: idim

    ! Variable values (must have the same dimension as Dresult)
    COMPLEX(DP), DIMENSION(:,:), INTENT(in) :: ZvalueBlock

    ! Variable values. This is a vector of scalar variables
    ! which is the same for all components of Res, e.g. the time variable.
    COMPLEX(DP), DIMENSION(:), INTENT(in), OPTIONAL :: ZvalueScalar

    ! OPTIONAL: local performance configuration. If not given, the
    ! global performance configuration is used.
    TYPE(t_perfconfig), INTENT(in), TARGET, OPTIONAL :: rperfconfig
!</input>

!<output>
    ! Evaluated function
    COMPLEX(DP), DIMENSION(:), INTENT(out) :: Zresult
!</output>
!</subroutine>

    ! local variables
    INTEGER :: icomp

    ! Lookup function by name
    icomp = fparser_getFunctionNumber(rfparser, scompName)

    ! Evaluate function by number
    CALL fparser_evalFunction (rfparser, icomp, idim, ZvalueBlock,&
                               Zresult, ZvalueScalar, rperfconfig)

  END SUBROUTINE fparser_evalFuncBlCmplByName

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncBlDbleFixName (rfparser, scompName, n1, n2, DValueBlock,&
                                            n3, Dresult, DvalueScalar, rperfconfig)

!<description>
    ! Evaluate bytecode of function named scompName for the array of
    ! real-valued values passed in array DvalueBlock(:,:). In some
    ! situations, there a variables, such as nodal coordinates, which
    ! are different for each component of the resulting vector and
    ! those which are the same, e.g., time variable.  The latter ones
    ! can be passed to the DvalueScalar argument which is used uniformly
    ! for each component of Dresult.
    !
    ! WARNING: The ordering of the variables must be identical to that given
    ! during the byte-code compilation. Care must be taken by the user since
    ! this cannot be checked be the function parser. Hence, if both ValBlock
    ! and DvalueScalar should be used, then the first variables must stored as
    ! blocks whereas the last variables can be scalar. This sound slightly
    ! complicated but here is an example:
    !
    ! Suppose you want to evaluate a function f=f(x,y,t). You know that x,y
    ! corresponds to the coordinate vector and t denotes the time. Then
    ! you should order your variables according to [x,y,t]. If the function
    ! should be evaluated for a set of variables then DvalueBlock=[x,y] and
    ! DvalueScalar=[t] works fine.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function name
    CHARACTER(LEN=*), INTENT(in) :: scompName

    ! Array dimensions
    INTEGER, INTENT(in) :: n1,n2,n3

    ! Variable values (must have the same dimension as Dresult)
    REAL(DP), DIMENSION(n1,n2), INTENT(in) :: DvalueBlock

    ! Variable values. This is a vector of scalar variables
    ! which is the same for all components of Res, e.g. the time variable.
    REAL(DP), DIMENSION(:), INTENT(in), OPTIONAL :: DvalueScalar

    ! OPTIONAL: local performance configuration. If not given, the
    ! global performance configuration is used.
    TYPE(t_perfconfig), INTENT(in), TARGET, OPTIONAL :: rperfconfig
!</input>

!<output>
    ! Evaluated function
    REAL(DP), DIMENSION(n3), INTENT(out) :: Dresult
!</output>
!</subroutine>

    ! local variables
    INTEGER :: icomp

    ! Lookup function by name
    icomp = fparser_getFunctionNumber(rfparser, scompName)

    ! Evaluate function by number
    IF (n1 .eq. n3) THEN
      CALL fparser_evalFunction (rfparser, icomp, 1, DvalueBlock,&
                                 Dresult, DvalueScalar, rperfconfig)
    ELSE IF (n2 .eq. n3) THEN
      CALL fparser_evalFunction (rfparser, icomp, 2, DvalueBlock,&
                                 Dresult, DvalueScalar, rperfconfig)
    ELSE
      CALL output_line('Invalid array dimensions!',&
          OU_CLASS_ERROR,OU_MODE_STD,'fparser_evalFuncBlDbleFixName')
      CALL sys_halt()
    END IF

  END SUBROUTINE fparser_evalFuncBlDbleFixName

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncBlCmplFixName (rfparser, scompName, n1, n2, ZValueBlock,&
                                            n3, Zresult, zvalueScalar, rperfconfig)

!<description>
    ! Evaluate bytecode of function named scompName for the array of
    ! real-valued values passed in array ZvalueBlock(:,:). In some
    ! situations, there a variables, such as nodal coordinates, which
    ! are different for each component of the resulting vector and
    ! those which are the same, e.g., time variable.  The latter ones
    ! can be passed to the ZvalueScalar argument which is used uniformly
    ! for each component of Zresult.
    !
    ! WARNING: The ordering of the variables must be identical to that given
    ! during the byte-code compilation. Care must be taken by the user since
    ! this cannot be checked be the function parser. Hence, if both ValBlock
    ! and DvalueScalar should be used, then the first variables must stored as
    ! blocks whereas the last variables can be scalar. This sound slightly
    ! complicated but here is an example:
    !
    ! Suppose you want to evaluate a function f=f(x,y,t). You know that x,y
    ! corresponds to the coordinate vector and t denotes the time. Then
    ! you should order your variables according to [x,y,t]. If the function
    ! should be evaluated for a set of variables then ZvalueBlock=[x,y] and
    ! ZvalueScalar=[t] works fine.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function name
    CHARACTER(LEN=*), INTENT(in) :: scompName

    ! Array dimensions
    INTEGER, INTENT(in) :: n1,n2,n3

    ! Variable values (must have the same dimension as Zresult)
    COMPLEX(DP), DIMENSION(n1,n2), INTENT(in) :: ZvalueBlock

    ! Variable values. This is a vector of scalar variables
    ! which is the same for all components of Zresult, e.g. the time variable.
    COMPLEX(DP), DIMENSION(:), INTENT(in), OPTIONAL :: ZvalueScalar

    ! OPTIONAL: local performance configuration. If not given, the
    ! global performance configuration is used.
    TYPE(t_perfconfig), INTENT(in), TARGET, OPTIONAL :: rperfconfig
!</input>

!<output>
    ! Evaluated function
    COMPLEX(DP), DIMENSION(n3), INTENT(out) :: Zresult
!</output>
!</subroutine>

    ! local variables
    INTEGER :: icomp

    ! Lookup function by name
    icomp = fparser_getFunctionNumber(rfparser, scompName)

    ! Evaluate function by number
    IF (n1 .eq. n3) THEN
      CALL fparser_evalFunction (rfparser, icomp, 1, ZvalueBlock,&
                                 Zresult, ZvalueScalar, rperfconfig)
    ELSE IF (n2 .eq. n3) THEN
      CALL fparser_evalFunction (rfparser, icomp, 2, ZvalueBlock,&
                                 Zresult, ZvalueScalar, rperfconfig)
    ELSE
      CALL output_line('Invalid array dimensions!',&
          OU_CLASS_ERROR,OU_MODE_STD,'fparser_evalFuncBlCmplFixName')
      CALL sys_halt()
    END IF

  END SUBROUTINE fparser_evalFuncBlCmplFixName

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncScDbleByNumber (rfparser, icomp, Dvalue, dresult)

!<description>
    ! Evaluate bytecode of component icomp for the real-valued values
    ! passed in array Dvalue(:). Note that this function is a wrapper
    ! for the working routine evalFunctionScDble. It is used to adjust
    ! the dimensions of the global stack memory if required.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function identifier
    INTEGER, INTENT(in) :: icomp

    ! Variable values
    REAL(DP), DIMENSION(:), INTENT(in) :: Dvalue
!</input>

!<output>
    ! Evaluated function
    REAL(DP), INTENT(out)  :: dresult
!</output>
!</subroutine>

    ! local variables
    REAL(DP), DIMENSION(:), ALLOCATABLE :: Dstack
    INTEGER :: EvalErrType

    ! Check if component is valid
    IF (icomp .lt. 1 .OR. icomp .gt. rfparser%nncomp) THEN
      CALL output_line('Component number is out of range',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_evalFuncScDbleByNumber')
      CALL sys_halt()
    END IF

    ! Allocate temporal memory
    ALLOCATE(Dstack(rfparser%Rcomp(icomp)%istackSize+1))

    ! Invoke working routine
    CALL evalFunctionScDble(rfparser%Rcomp(icomp), Dstack,&
                            Dvalue, EvalErrType, dresult)

    ! Deallocate temporal memory
    DEALLOCATE(Dstack)

    ! Check if evaluation was successful
    IF (EvalErrType .ne. 0) THEN
      CALL output_line('An error occured during function evaluation!',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_evalFuncScDbleByNumber')
      CALL sys_halt()
    END IF

  END SUBROUTINE fparser_evalFuncScDbleByNumber

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncScCmplByNumber (rfparser, icomp, Zvalue, zresult)

!<description>
    ! Evaluate bytecode of component icomp for the complex-valued values
    ! passed in array Zvalue(:). Note that this function is a wrapper
    ! for the working routine evalFunctionScCmpl. It is used to adjust
    ! the dimensions of the global stack memory if required.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function identifier
    INTEGER, INTENT(in) :: icomp

    ! Variable values
    COMPLEX(DP), DIMENSION(:), INTENT(in) :: Zvalue
!</input>

!<output>
    ! Evaluated function
    COMPLEX(DP), INTENT(out)  :: zresult
!</output>
!</subroutine>

    ! local variables
    COMPLEX(DP), DIMENSION(:), ALLOCATABLE :: Zstack
    INTEGER :: EvalErrType

    ! Check if component is valid
    IF (icomp .lt. 1 .OR. icomp .gt. rfparser%nncomp) THEN
      CALL output_line('Component number is out of range',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_evalFuncScCmplByNumber')
      CALL sys_halt()
    END IF

    ! Allocate temporal memory
    ALLOCATE(Zstack(rfparser%Rcomp(icomp)%istackSize+1))

    ! Invoke working routine
    CALL evalFunctionScCmpl(rfparser%Rcomp(icomp), Zstack,&
                            Zvalue, EvalErrType, zresult)

    ! Deallocate temporal memory
    DEALLOCATE(Zstack)

    ! Check if evaluation was successful
    IF (EvalErrType .ne. 0) THEN
      CALL output_line('An error occured during function evaluation!',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_evalFuncScCmplByNumber')
      CALL sys_halt()
    END IF

  END SUBROUTINE fparser_evalFuncScCmplByNumber

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncBlDbleByNumber (rfparser, icomp, idim, DvalueBlock,&
                                             Dresult, DvalueScalar, rperfconfig)

!<description>
    ! Evaluate bytecode of component icomp for the array of
    ! real-valued values passed in array DvaluelBlock(:,:). Note that
    ! this function is a wrapper for the working routine
    ! evalFunctionBlDble. It is used to adjust the dimensions of the
    ! global stack memory if required. In some situations, there a
    ! variables, such as nodal coordinates, which are different for
    ! each component of the resulting vector and those which are the
    ! same, e.g., time variable.  The latter ones can be passed to the
    ! DvalueScalar argument which is used uniformly for each component of
    ! Dresult.
    !
    ! WARNING: The ordering of the variables must be identical to that given
    ! during the byte-code compilation. Care must be taken by the user since
    ! this cannot be checked be the function parser. Hence, if both ValBlock
    ! and ValScalar should be used, then the first variables must stored as
    ! blocks whereas the last variables can be scalar. This sound slightly
    ! complicated but here is an example:
    !
    ! Suppose you want to evaluate a function f=f(x,y,t). You know that x,y
    ! corresponds to the coordinate vector and t denotes the time. Then
    ! you should order your variables according to [x,y,t]. If the function
    ! should be evaluated for a set of variables then DvalueBlock=[x,y] and
    ! DvalueScalar=[t] works fine.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function identifier
    INTEGER, INTENT(in) :: icomp

    ! Orientation of the stored values
    ! idim =1 : DvalueBlock is organised as (x1:xN),(y1:yN),...
    ! idim =2 : DvalueBlock is organised as (x1,y1),(x2,y2),...,(xN,yN)
    INTEGER, INTENT(in) :: idim

    ! Variable values (must have the same dimension as Dresult)
    REAL(DP), DIMENSION(:,:), INTENT(in) :: DvalueBlock

    ! Variable values. This is a vector of scalar variables which is
    ! the same for all components of Dresult, e.g. the time variable.
    REAL(DP), DIMENSION(:), INTENT(in), OPTIONAL :: DvalueScalar

    ! OPTIONAL: local performance configuration. If not given, the
    ! global performance configuration is used.
    TYPE(t_perfconfig), INTENT(in), TARGET, OPTIONAL :: rperfconfig
!</input>

!<output>
    ! Evaluated function
    REAL(DP), DIMENSION(:), INTENT(out) :: Dresult
!</output>
!</subroutine>

    ! local variables
    REAL(DP), DIMENSION(:,:), ALLOCATABLE :: Dstack
    REAL(DP), DIMENSION(:), ALLOCATABLE :: DvalueTemp
    INTEGER :: iValSet,iValMax,nvalue,iblockSize,isizeValueScalar,isizeValueBlock
    INTEGER :: EvalErrType

    ! Pointer to the performance configuration
    TYPE(t_perfconfig), POINTER :: p_rperfconfig

    IF (PRESENT(rperfconfig)) THEN
      p_rperfconfig => rperfconfig
    ELSE
      p_rperfconfig => fparser_perfconfig
    END IF

    ! Check if component is valid
    IF (icomp .lt. 1 .OR. icomp .gt. rfparser%nncomp) THEN
      CALL output_line('Component number is out of range',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_evalFuncBlDbleByNumber')
      CALL sys_halt()
    END IF

    ! Get total number of variable sets
    nvalue = SIZE(DvalueBlock, idim)

    ! Initialise error flag
    EvalErrType = 0

    ! Check if the compiled function is vectorizable
    IF (rfparser%Rcomp(icomp)%bisVectorizable) THEN
      ! ...ok, vectorization of the bytecode is admissible.

      ! What is the organization of ValBlock(:,:)
      IF (idim .eq. 1) THEN
        !$omp parallel default(shared)&
        !$omp private(Dstack,iValMax,iblockSize)&
        !$omp reduction(max:EvalErrType)

        ! Allocate temporal memory
        ALLOCATE(Dstack(p_rperfconfig%NITEMSIM,rfparser%Rcomp(icomp)%iStackSize+1))

        !$omp do schedule(static,1)
        DO iValSet = 1, nvalue, p_rperfconfig%NITEMSIM

          ! Initialization
          iValMax    = MIN(iValSet+p_rperfconfig%NITEMSIM-1, nvalue)
          iblockSize = iValMax-iValSet+1

          ! Invoke working routine
          CALL evalFunctionBlDble(rfparser%Rcomp(icomp), iblockSize, Dstack,&
                                  DvalueBlock(iValSet:iValMax,:), idim, EvalErrType,&
                                  Dresult(iValSet:iValMax), DvalueScalar)
        END DO
        !$omp end do

        ! Deallocate temporal memory
        DEALLOCATE(Dstack)
        !$omp end parallel
      ELSE
        !$omp parallel default(shared)&
        !$omp private(Dstack,iValMax,iblockSize)&
        !$omp reduction(max:EvalErrType)

        ! Allocate temporal memory
        ALLOCATE(Dstack(p_rperfconfig%NITEMSIM,rfparser%Rcomp(icomp)%iStackSize+1))

        !$omp do schedule(static,1)
        DO iValSet = 1, nvalue, p_rperfconfig%NITEMSIM

          ! Initialization
          iValMax    = MIN(iValSet+p_rperfconfig%NITEMSIM-1, nvalue)
          iblockSize = iValMax-iValSet+1

          ! Invoke working routine
          CALL evalFunctionBlDble(rfparser%Rcomp(icomp), iblockSize, Dstack,&
                                  DvalueBlock(:, iValSet:iValMax), idim, EvalErrType,&
                                  Dresult(iValSet:iValMax), DvalueScalar)
        END DO
        !$omp end do

        ! Deallocate temporal memory
        DEALLOCATE(Dstack)
        !$omp end parallel
      END IF

    ELSE   ! The compiled function cannot be vectorised

      ! Allocate temporal memory
      ALLOCATE(Dstack(rfparser%Rcomp(icomp)%iStackSize+1,1))

      ! The compiled bytecode cannot be vectorised. Hence, evaluate
      ! the function separately for each set of variables. Here, the
      ! organization of the array DvalueBlock(:,:) is
      ! important. Moreover, if the optional parameter DvalueScalar is
      ! given, then we have to combine those variables from DvalBlock
      ! and DvalueScalar.

      IF (PRESENT(DvalueScalar)) THEN

        ! Allocate auxiliary array
        isizeValueBlock  = SIZE(DvalueBlock,3-idim)
        isizeValueScalar = SIZE(DvalueScalar)
        ALLOCATE(DvalueTemp(isizeValueBlock+isizeValueScalar))

        IF (idim .eq. 1) THEN
          DO iValSet = 1, nvalue

            DvalueTemp(1:isizeValueBlock)  = DvalueBlock(iValSet,1:isizeValueBlock)
            DvalueTemp(isizeValueBlock+1:) = DvalueScalar

            ! Invoke working routine
            CALL evalFunctionScDble(rfparser%Rcomp(icomp), Dstack(:,1),&
                                    DvalueTemp, EvalErrType, Dresult(iValSet))
          END DO
        ELSE
          DO iValSet = 1, nvalue

            DvalueTemp(1:isizeValueBlock)  = DvalueBlock(:,iValSet)
            DvalueTemp(isizeValueBlock+1:) = DvalueScalar

            ! Invoke working routine
            CALL evalFunctionScDble(rfparser%Rcomp(icomp), Dstack(:,1),&
                                    DvalueTemp, EvalErrType, Dresult(iValSet))
          END DO
        END IF

        ! Deallocate auxiliary array
        DEALLOCATE(DvalueTemp)

      ELSE

        IF (idim .eq. 1) THEN
          DO iValSet = 1, nvalue

            ! Invoke working routine
            CALL evalFunctionScDble(rfparser%Rcomp(icomp), Dstack(:,1),&
                                    DvalueBlock(iValSet,:), EvalErrType,&
                                    Dresult(iValSet))
          END DO
        ELSE
          DO iValSet = 1, nvalue

            ! Invoke working routine
            CALL evalFunctionScDble(rfparser%Rcomp(icomp), Dstack(:,1),&
                                    DvalueBlock(:, iValSet), EvalErrType,&
                                    Dresult(iValSet))
          END DO
        END IF

      END IF

      ! Deallocate temporal memory
      DEALLOCATE(Dstack)

    END IF

    ! Check if evaluation was successful
    IF (EvalErrType .ne. 0) THEN
      CALL output_line('An error occured during function evaluation!',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_evalFuncBlDbleByNumber')
      CALL sys_halt()
    END IF

  END SUBROUTINE fparser_evalFuncBlDbleByNumber

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncBlCmplByNumber (rfparser, icomp, idim, ZvalueBlock,&
                                             Zresult, ZvalueScalar, rperfconfig)

!<description>
    ! Evaluate bytecode of component icomp for the array of
    ! complex-valued values passed in array ZvaluelBlock(:,:). Note that
    ! this function is a wrapper for the working routine
    ! evalFunctionBlCmpl. It is used to adjust the dimensions of the
    ! global stack memory if required. In some situations, there a
    ! variables, such as nodal coordinates, which are different for
    ! each component of the resulting vector and those which are the
    ! same, e.g., time variable.  The latter ones can be passed to the
    ! ZvalueScalar argument which is used uniformly for each component of
    ! Zresult.
    !
    ! WARNING: The ordering of the variables must be identical to that given
    ! during the byte-code compilation. Care must be taken by the user since
    ! this cannot be checked be the function parser. Hence, if both ValBlock
    ! and ValScalar should be used, then the first variables must stored as
    ! blocks whereas the last variables can be scalar. This sound slightly
    ! complicated but here is an example:
    !
    ! Suppose you want to evaluate a function f=f(x,y,t). You know that x,y
    ! corresponds to the coordinate vector and t denotes the time. Then
    ! you should order your variables according to [x,y,t]. If the function
    ! should be evaluated for a set of variables then ZvalueBlock=[x,y] and
    ! ZvalueScalar=[t] works fine.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function identifier
    INTEGER, INTENT(in) :: icomp

    ! Orientation of the stored values
    ! idim =1 : DvalueBlock is organised as (x1:xN),(y1:yN),...
    ! idim =2 : DvalueBlock is organised as (x1,y1),(x2,y2),...,(xN,yN)
    INTEGER, INTENT(in) :: idim

    ! Variable values (must have the same dimension as Zresult)
    COMPLEX(DP), DIMENSION(:,:), INTENT(in) :: ZvalueBlock

    ! Variable values. This is a vector of scalar variables which is
    ! the same for all components of Zresult, e.g. the time variable.
    COMPLEX(DP), DIMENSION(:), INTENT(in), OPTIONAL :: ZvalueScalar

    ! OPTIONAL: local performance configuration. If not given, the
    ! global performance configuration is used.
    TYPE(t_perfconfig), INTENT(in), TARGET, OPTIONAL :: rperfconfig
!</input>

!<output>
    ! Evaluated function
    COMPLEX(DP), DIMENSION(:), INTENT(out) :: Zresult
!</output>
!</subroutine>

    ! local variables
    COMPLEX(DP), DIMENSION(:,:), ALLOCATABLE :: Zstack
    COMPLEX(DP), DIMENSION(:), ALLOCATABLE :: ZvalueTemp
    INTEGER :: iValSet,iValMax,nvalue,iblockSize,isizeValueScalar,isizeValueBlock
    INTEGER :: EvalErrType

    ! Pointer to the performance configuration
    TYPE(t_perfconfig), POINTER :: p_rperfconfig

    IF (PRESENT(rperfconfig)) THEN
      p_rperfconfig => rperfconfig
    ELSE
      p_rperfconfig => fparser_perfconfig
    END IF

    ! Check if component is valid
    IF (icomp .lt. 1 .OR. icomp .gt. rfparser%nncomp) THEN
      CALL output_line('Component number is out of range',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_evalFuncBlCmplByNumber')
      CALL sys_halt()
    END IF

    ! Get total number of variable sets
    nvalue = SIZE(ZvalueBlock, idim)

    ! Initialise error flag
    EvalErrType = 0

    ! Check if the compiled function is vectorizable
    IF (rfparser%Rcomp(icomp)%bisVectorizable) THEN
      ! ...ok, vectorization of the bytecode is admissible.

      ! What is the organization of ValBlock(:,:)
      IF (idim .eq. 1) THEN
        !$omp parallel default(shared)&
        !$omp private(Zstack,iValMax,iblockSize)&
        !$omp reduction(max:EvalErrType)

        ! Allocate temporal memory
        ALLOCATE(Zstack(p_rperfconfig%NITEMSIM,rfparser%Rcomp(icomp)%iStackSize+1))

        !$omp do schedule(static,1)
        DO iValSet = 1, nvalue, p_rperfconfig%NITEMSIM

          ! Initialization
          iValMax    = MIN(iValSet+p_rperfconfig%NITEMSIM-1, nvalue)
          iblockSize = iValMax-iValSet+1

          ! Invoke working routine
          CALL evalFunctionBlCmpl(rfparser%Rcomp(icomp), iblockSize, Zstack,&
                                  ZvalueBlock(iValSet:iValMax,:), idim, EvalErrType,&
                                  Zresult(iValSet:iValMax), ZvalueScalar)
        END DO
        !$omp end do

        ! Deallocate temporal memory
        DEALLOCATE(Zstack)
        !$omp end parallel
      ELSE
        !$omp parallel default(shared)&
        !$omp private(Zstack,iValMax,iblockSize)&
        !$omp reduction(max:EvalErrType)

        ! Allocate temporal memory
        ALLOCATE(Zstack(p_rperfconfig%NITEMSIM,rfparser%Rcomp(icomp)%iStackSize+1))

        !$omp do schedule(static,1)
        DO iValSet = 1, nvalue, p_rperfconfig%NITEMSIM

          ! Initialization
          iValMax    = MIN(iValSet+p_rperfconfig%NITEMSIM-1, nvalue)
          iblockSize = iValMax-iValSet+1

          ! Invoke working routine
          CALL evalFunctionBlCmpl(rfparser%Rcomp(icomp), iblockSize, Zstack,&
                                  ZvalueBlock(:, iValSet:iValMax), idim, EvalErrType,&
                                  Zresult(iValSet:iValMax), ZvalueScalar)
        END DO
        !$omp end do

        ! Deallocate temporal memory
        DEALLOCATE(Zstack)
        !$omp end parallel
      END IF

    ELSE   ! The compiled function cannot be vectorised

      ! Allocate temporal memory
      ALLOCATE(Zstack(rfparser%Rcomp(icomp)%iStackSize+1,1))

      ! The compiled bytecode cannot be vectorised. Hence, evaluate
      ! the function separately for each set of variables. Here, the
      ! organization of the array ZvalueBlock(:,:) is
      ! important. Moreover, if the optional parameter ZvalueScalar is
      ! given, then we have to combine those variables from
      ! ZvalueBlock and ZvalueScalar.

      IF (PRESENT(ZvalueScalar)) THEN

        ! Allocate auxiliary array
        isizeValueBlock  = SIZE(ZvalueBlock,3-idim)
        isizeValueScalar = SIZE(ZvalueScalar)
        ALLOCATE(ZvalueTemp(isizeValueBlock+isizeValueScalar))

        IF (idim .eq. 1) THEN
          DO iValSet = 1, nvalue

            ZvalueTemp(1:isizeValueBlock)  = ZvalueBlock(iValSet,1:isizeValueBlock)
            ZvalueTemp(isizeValueBlock+1:) = ZvalueScalar

            ! Invoke working routine
            CALL evalFunctionScCmpl(rfparser%Rcomp(icomp), Zstack(:,1),&
                                    ZvalueTemp, EvalErrType, Zresult(iValSet))
          END DO
        ELSE
          DO iValSet = 1, nvalue

            ZvalueTemp(1:isizeValueBlock)  = ZvalueBlock(:,iValSet)
            ZvalueTemp(isizeValueBlock+1:) = ZvalueScalar

            ! Invoke working routine
            CALL evalFunctionScCmpl(rfparser%Rcomp(icomp), Zstack(:,1),&
                                    ZvalueTemp, EvalErrType, Zresult(iValSet))
          END DO
        END IF

        ! Deallocate auxiliary array
        DEALLOCATE(ZvalueTemp)

      ELSE

        IF (idim .eq. 1) THEN
          DO iValSet = 1, nvalue

            ! Invoke working routine
            CALL evalFunctionScCmpl(rfparser%Rcomp(icomp), Zstack(:,1),&
                                    ZvalueBlock(iValSet,:), EvalErrType,&
                                    Zresult(iValSet))
          END DO
        ELSE
          DO iValSet = 1, nvalue

            ! Invoke working routine
            CALL evalFunctionScCmpl(rfparser%Rcomp(icomp), Zstack(:,1),&
                                    ZvalueBlock(:, iValSet), EvalErrType,&
                                    Zresult(iValSet))
          END DO
        END IF

      END IF

      ! Deallocate temporal memory
      DEALLOCATE(Zstack)

    END IF

    ! Check if evaluation was successful
    IF (EvalErrType .ne. 0) THEN
      CALL output_line('An error occured during function evaluation!',&
          OU_CLASS_ERROR, OU_MODE_STD,'fparser_evalFuncBlCmplByNumber')
      CALL sys_halt()
    END IF

  END SUBROUTINE fparser_evalFuncBlCmplByNumber

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncBlDbleFixNumber (rfparser, icomp, n1, n2, DvalueBlock,&
                                              n3, Dresult, DvalueScalar, rperfconfig)

!<description>
    ! Evaluate bytecode of component icomp for the array of
    ! real-valued values passed in array DvaluelBlock(:,:). Note that
    ! this function is a wrapper for the working routine
    ! evalFunctionBlDble. It is used to adjust the dimensions of the
    ! global stack memory if required. In some situations, there a
    ! variables, such as nodal coordinates, which are different for
    ! each component of the resulting vector and those which are the
    ! same, e.g., time variable.  The latter ones can be passed to the
    ! DvalueScalar argument which is used uniformly for each component
    ! of Dresult.
    !
    ! WARNING: The ordering of the variables must be identical to that given
    ! during the byte-code compilation. Care must be taken by the user since
    ! this cannot be checked be the function parser. Hence, if both ValBlock
    ! and ValScalar should be used, then the first variables must stored as
    ! blocks whereas the last variables can be scalar. This sound slightly
    ! complicated but here is an example:
    !
    ! Suppose you want to evaluate a function f=f(x,y,t). You know that x,y
    ! corresponds to the coordinate vector and t denotes the time. Then
    ! you should order your variables according to [x,y,t]. If the function
    ! should be evaluated for a set of variables then DvalueBlock=[x,y] and
    ! DvalueScalar=[t] works fine.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function identifier
    INTEGER, INTENT(in) :: icomp

    ! Array dimensions
    INTEGER, INTENT(in) :: n1,n2,n3

    ! Variable values (must have the same dimension as Dresult)
    REAL(DP), DIMENSION(n1,n2), INTENT(in) :: DvalueBlock

    ! Variable values. This is a vector of scalar variables which is
    ! the same for all components of Dresult, e.g. the time variable.
    REAL(DP), DIMENSION(:), INTENT(in), OPTIONAL :: DvalueScalar

    ! OPTIONAL: local performance configuration. If not given, the
    ! global performance configuration is used.
    TYPE(t_perfconfig), INTENT(in), TARGET, OPTIONAL :: rperfconfig
!</input>

!<output>
    ! Evaluated function
    REAL(DP), DIMENSION(n3), INTENT(out) :: Dresult
!</output>
!</subroutine>

    ! Evaluate function by number
    IF (n1 .eq. n3) THEN
      CALL fparser_evalFunction (rfparser, icomp, 1, DvalueBlock,&
                                 Dresult, DvalueScalar, rperfconfig)
    ELSE IF (n2 .eq. n3) THEN
      CALL fparser_evalFunction (rfparser, icomp, 2, DvalueBlock,&
                                 Dresult, DvalueScalar, rperfconfig)
    ELSE
      CALL output_line('Invalid array dimensions!',&
          OU_CLASS_ERROR,OU_MODE_STD,'fparser_evalFuncBlDbleFixNumber')
      CALL sys_halt()
    END IF

  END SUBROUTINE fparser_evalFuncBlDbleFixNumber

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_evalFuncBlCmplFixNumber (rfparser, icomp, n1, n2, ZvalueBlock,&
                                              n3, Zresult, ZvalueScalar, rperfconfig)

!<description>
    ! Evaluate bytecode of component icomp for the array of
    ! complex-valued values passed in array ZvaluelBlock(:,:). Note that
    ! this function is a wrapper for the working routine
    ! evalFunctionBlCmpl. It is used to adjust the dimensions of the
    ! global stack memory if required. In some situations, there a
    ! variables, such as nodal coordinates, which are different for
    ! each component of the resulting vector and those which are the
    ! same, e.g., time variable.  The latter ones can be passed to the
    ! ZvalueScalar argument which is used uniformly for each component
    ! of Zresult.
    !
    ! WARNING: The ordering of the variables must be identical to that given
    ! during the byte-code compilation. Care must be taken by the user since
    ! this cannot be checked be the function parser. Hence, if both ValBlock
    ! and ValScalar should be used, then the first variables must stored as
    ! blocks whereas the last variables can be scalar. This sound slightly
    ! complicated but here is an example:
    !
    ! Suppose you want to evaluate a function f=f(x,y,t). You know that x,y
    ! corresponds to the coordinate vector and t denotes the time. Then
    ! you should order your variables according to [x,y,t]. If the function
    ! should be evaluated for a set of variables then ZvalueBlock=[x,y] and
    ! ZvalueScalar=[t] works fine.
!</description>

!<input>
    ! Function parser
    TYPE (t_fparser), INTENT(in) :: rfparser

    ! Function identifier
    INTEGER, INTENT(in) :: icomp

    ! Array dimensions
    INTEGER, INTENT(in) :: n1,n2,n3

    ! Variable values (must have the same dimension as Zresult)
    COMPLEX(DP), DIMENSION(n1,n2), INTENT(in) :: ZvalueBlock

    ! Variable values. This is a vector of scalar variables which is
    ! the same for all components of Zresult, e.g. the time variable.
    COMPLEX(DP), DIMENSION(:), INTENT(in), OPTIONAL :: ZvalueScalar

    ! OPTIONAL: local performance configuration. If not given, the
    ! global performance configuration is used.
    TYPE(t_perfconfig), INTENT(in), TARGET, OPTIONAL :: rperfconfig
!</input>

!<output>
    ! Evaluated function
    COMPLEX(DP), DIMENSION(n3), INTENT(out) :: Zresult
!</output>
!</subroutine>

    ! Evaluate function by number
    IF (n1 .eq. n3) THEN
      CALL fparser_evalFunction (rfparser, icomp, 1, ZvalueBlock,&
                                 Zresult, ZvalueScalar, rperfconfig)
    ELSE IF (n2 .eq. n3) THEN
      CALL fparser_evalFunction (rfparser, icomp, 2, ZvalueBlock,&
                                 Zresult, ZvalueScalar, rperfconfig)
    ELSE
      CALL output_line('Invalid array dimensions!',&
          OU_CLASS_ERROR,OU_MODE_STD,'fparser_evalFuncBlCmplFixNumber')
      CALL sys_halt()
    END IF

  END SUBROUTINE fparser_evalFuncBlCmplFixNumber

  ! *****************************************************************************

!<function>

  FUNCTION fparser_ErrorMsg (EvalErrType) RESULT (smessage)

!<description>
    ! Return error message of function parser
!</description>

    ! local constants
    CHARACTER (LEN=*), DIMENSION(4), PARAMETER :: m = (/ 'Division by zero       ', &
                                                         'Illegal argument       ', &
                                                         'Complex-valued argument', &
                                                         'Illegal operation      '/)

!<input>
    ! Error identifier
    INTEGER, INTENT(in) :: EvalErrType
!</input>

!<result>
    ! Error messages
    CHARACTER (LEN=LEN(m)) :: smessage
!</result>
!</function>

    IF (EvalErrType .lt. 1 .OR. EvalErrType .gt. SIZE(m)) THEN
      smessage = ''
    ELSE
      smessage = m(EvalErrType)
    END IF

  END FUNCTION fparser_ErrorMsg

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_PrintByteCodeByName(rfparser, scompName)

!<description>
    ! Print the compiled bytecode stack
!</description>

!<input>
    ! Function parser
    TYPE(t_fparser), INTENT(in) :: rfparser

    ! Function name
    CHARACTER(LEN=*), INTENT(in) :: scompName
!</input>
!</subroutine>

    ! local variables
    INTEGER :: icomp

    ! Lookup function by name
    icomp = fparser_getFunctionNumber(rfparser, scompName)

    ! Print bytecode
    CALL fparser_PrintByteCodeByNumber(rfparser, icomp)

  END SUBROUTINE fparser_PrintByteCodeByName

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE fparser_PrintByteCodeByNumber(rfparser, icomp)

!<description>
    ! Print the compiled bytecode stack
!</description>

!<input>
    ! Function parser
    TYPE(t_fparser), INTENT(in) :: rfparser

    ! Function identifier
    INTEGER, INTENT(in) :: icomp
!</input>
!</subroutine>

    ! local variables
    TYPE(t_fparserComponent), POINTER :: p_Comp
    CHARACTER(LEN=5) :: n
    INTEGER :: iinstPtr, idataPtr, istackPtr, nparams
    INTEGER(is) :: iopCode

    nparams   = 1
    idataPtr  = 1
    istackPtr = 0
    iinstPtr  = 0
    p_Comp    => rfparser%Rcomp(icomp)

    DO WHILE(iinstPtr .lt. p_Comp%ibytecodeSize)
      iinstPtr = iinstPtr+1

      WRITE(*,FMT='(I8.8,1X,":",1X)', ADVANCE="NO") iinstPtr

      iopCode = p_Comp%IbyteCode(iinstPtr)
      SELECT CASE(iopCode)

      CASE(cIf)
        WRITE(*,FMT='(A,1X,T10,I8.8)') "jz", p_Comp%IbyteCode(iinstPtr+1)+1
        iinstPtr = iinstPtr+2

      CASE(cJump)
        WRITE(*,FMT='(A,1X,T10,I8.8)') "jump", p_Comp%IbyteCode(iinstPtr+1)+1
        iinstPtr = iinstPtr+2

      CASE(cImmed)
        IF (p_Comp%bisComplex) THEN
          WRITE(*,FMT='(A,1X,T10,A,G16.8,A,G16.8,A)') "push", "(",&
              REAL(p_Comp%Zimmed(idataPtr)), ",",&
              AIMAG(p_Comp%Zimmed(idataPtr)), ")"
        ELSE
          WRITE(*,FMT='(A,1X,T10,G16.8)') "push", p_Comp%Dimmed(idataPtr)
        END IF
        idataPtr = idataPtr+1

      CASE default
        IF (iopCode .lt. VarBegin) THEN
          SELECT CASE(iopCode)
          CASE(cNEG);         n = "neg"
          CASE(cADD);         n = "add"
          CASE(cSUB);         n = "sub"
          CASE(cMUL);         n = "mul"
          CASE(cDIV);         n = "div"
          CASE(cMOD);         n = "mod"
          CASE(cPOW);         n = "pow"
          CASE(cEqual);       n = "eq"
          CASE(cNEqual);      n = "ne"
          CASE(cLess);        n = "lt"
          CASE(cLessOrEq);    n = "le"
          CASE(cGreater);     n = "gt"
          CASE(cGreaterOrEq); n = "ge"
          CASE(cAND);         n = "and"
          CASE(cOR);          n = "or"
          CASE(cNOT);         n = "not"
          CASE(cDEG);         n = "deg"
          CASE(cRAD);         n = "rad"

          CASE default
            n       = Funcs(iopCode)
            nparams = MathFunctionParameters(iopCode)
          END SELECT
          WRITE(*,FMT='(A,T10,A,"  (",I1,") ")') TRIM(n), "Par", nparams

        ELSE
          WRITE(*,FMT='(A,T10,A,1X,I4.4)') "push", "Var", iopCode-VarBegin+1
        END IF

      END SELECT
    END DO

  END SUBROUTINE fparser_PrintByteCodeByNumber

  ! *****************************************************************************

!<function>

  FUNCTION fparser_getFunctionNumber (rfparser, scompName, bquiet) RESULT(icomp)

!<description>
    ! This function returns the internal number of the component which
    ! correspones to the function with name scompName
!</description>

!<input>
    ! Function parser
    TYPE(t_fparser), INTENT(in) :: rfparser

    ! Function name
    CHARACTER(LEN=*), INTENT(in) :: scompName

    ! OPTIONAL: Specifies whether a warning should be printed when released an
    ! empty vector (bquiet = .false.) or whether to remain silent in this case.
    ! If not specified, bquiet = .false. is used.
    LOGICAL, OPTIONAL, INTENT(in) :: bquiet
!</input>

!<result>
    ! Function identifier
    INTEGER :: icomp
!</result>
!</function>

    ! local variable
    LOGICAL :: bwarn
    CHARACTER(LEN=LEN(scompName)) :: sname

    ! Convert to lower case
    CALL sys_tolower(scompName, sname)

    ! Lookup function
    DO icomp = 1, rfparser%ncomp
      IF (TRIM(ADJUSTL(sname)) .eq. TRIM(rfparser%ScompName(icomp))) RETURN
    END DO

    ! If we end up here, then the function is not available
    icomp = 0

    ! Shout or shut up?
    bwarn = .true.
    IF(PRESENT(bquiet)) bwarn = .NOT. bquiet

    IF (bwarn) CALL output_line('Function is not available',&
        OU_CLASS_WARNING, OU_MODE_STD,'fparser_getFunctionNumber')

  END FUNCTION fparser_getFunctionNumber

  ! *****************************************************************************
  ! *****************************************************************************
  ! *****************************************************************************

!<subroutine>

  RECURSIVE SUBROUTINE CheckSyntax (sfunctionString, Svariables)

!<description>
    ! Check syntax of function string, returns 0 if syntax is ok
!</description>

!<input>
    ! Function string without spaces
    CHARACTER (LEN=*), INTENT(in) :: sfunctionString

    ! Array with variable names
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>
!</subroutine>

    ! local variables
    TYPE(t_stackInt) :: rstack
    INTEGER(is) :: n,iopSize
    CHARACTER (LEN=1) :: c
    REAL(DP) :: dnumber
    LOGICAL :: berror
    INTEGER :: ifunctionIndex,ifunctionIndex2
    INTEGER :: iparenthCount,ib,in,ifunctionLength,idummy

    ! Initialization
    ifunctionIndex  = 1
    iparenthCount   = 0
    ifunctionLength = LEN_TRIM(sfunctionString)
    CALL stack_create(rstack, MAX(5, INT(ifunctionLength/4._DP)))

    DO
      IF (ifunctionIndex .gt. ifunctionLength) THEN
        CALL output_line('Invalid function string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
            OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
        CALL sys_halt()
      END IF
      c = sfunctionString(ifunctionIndex:ifunctionIndex)

      ! Check for valid operand (must appear)

      ! Check for leading - or !
      IF (c .eq. '-' .OR. c .eq. '!') THEN
        ifunctionIndex = ifunctionIndex+1
        IF (ifunctionIndex .gt. ifunctionLength) THEN
          CALL output_line('Premature end of string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        c = sfunctionString(ifunctionIndex:ifunctionIndex)
      END IF

      ! Check for math function
      n = MathFunctionIndex (sfunctionString(ifunctionIndex:))
      IF (n .gt. 0) THEN
        ! Math function found
        ifunctionIndex = ifunctionIndex+LEN_TRIM(Funcs(n))
        IF (ifunctionIndex .gt. ifunctionLength) THEN
          CALL output_line('Premature end of string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        c = sfunctionString(ifunctionIndex:ifunctionIndex)
        IF (c .ne. '(') THEN
          CALL output_line('Expecting ( after function '//sfunctionString(ifunctionIndex:)//'!',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        ifunctionIndex2 = ifunctionIndex+1
        IF (ifunctionIndex2 .gt. ifunctionLength) THEN
          CALL output_line('Premature end of string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        IF (sfunctionString(ifunctionIndex2:ifunctionIndex2) .eq. ')') THEN
          ifunctionIndex = ifunctionIndex2+1
          IF (ifunctionIndex .gt. ifunctionLength) THEN
            CALL output_line('Premature end of string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
                OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
            CALL sys_halt()
          END IF
          c = sfunctionString(ifunctionIndex:ifunctionIndex)

          ! Ugly, but other methods would just be uglier ...
          goto 2
        END IF

        ! Push counter for parenthesss to stack
        CALL stack_push(rstack, iparenthCount+1)
      END IF

      ! Check for opening parenthesis
      IF (c .eq. '(') THEN
        iparenthCount = iparenthCount+1
        ifunctionIndex = ifunctionIndex+1
        IF (ifunctionIndex .gt. ifunctionLength) THEN
          CALL output_line('Premature end of string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        IF (sfunctionString(ifunctionIndex:ifunctionIndex) .eq. ')') THEN
          CALL output_line('Empty parantheses in string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        CYCLE
      END IF

      ! Check for number
      IF (SCAN(c,'0123456789.') .gt. 0) THEN
        dnumber = RealNum (sfunctionString(ifunctionIndex:), ib, in, berror)
        IF (berror) THEN
          CALL output_line('Invalid number format in string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        ifunctionIndex = ifunctionIndex+in-1
        IF (ifunctionIndex .gt. ifunctionLength) EXIT
        c = sfunctionString(ifunctionIndex:ifunctionIndex)

!!$        ! Check if this is an imaginary number z = (a,b) = a+bi
!!$        if (c .eq. ',') then
!!$          ifunctionIndex = ifunctionIndex+1
!!$          if (ifunctionIndex .gt. ifunctionLength) exit
!!$          c = sfunctionString(ifunctionIndex:ifunctionIndex)
!!$
!!$          ! Ugly, but other methods would just be uglier ...
!!$          goto 1
!!$        end if
      ELSE IF (c .eq. '_') THEN
        ! Check for constant
        n = ConstantIndex (sfunctionString(ifunctionIndex:))
        IF (n .eq. 0) THEN
          CALL output_line('Invalid constant in string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        ifunctionIndex = ifunctionIndex+LEN_TRIM(CconstantName(n))+1
        IF (ifunctionIndex .gt. ifunctionLength) EXIT
        c = sfunctionString(ifunctionIndex:ifunctionIndex)
      ELSE IF (c .eq. '@') THEN
        ! Check for expression
        n = ExpressionIndex (sfunctionString(ifunctionIndex:))
        IF (n .eq. 0) THEN
          CALL output_line('Invalid expression in string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        CALL CheckSyntax(CexpressionString(n), Svariables)
        ifunctionIndex = ifunctionIndex+LEN_TRIM(CexpressionName(n))+1
        IF (ifunctionIndex .gt. ifunctionLength) EXIT
        c = sfunctionString(ifunctionIndex:ifunctionIndex)
      ELSE
        ! Check for variable
        n = VariableIndex (sfunctionString(ifunctionIndex:), Svariables, ib, in)
        IF (n .eq. 0) THEN
          CALL output_line('Invalid element in string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        ifunctionIndex = ifunctionIndex+in-1
        IF (ifunctionIndex .gt. ifunctionLength) EXIT
        c = sfunctionString(ifunctionIndex:ifunctionIndex)
      END IF

      ! Check for closing parenthesis
      DO WHILE (c .eq. ')')
        IF (.NOT.stack_empty(rstack)) THEN
          CALL stack_top(rstack, idummy)
          IF(idummy .eq. iparenthCount) CALL stack_pop(rstack, idummy)
        END IF
        iparenthCount = iparenthCount-1
        IF (iparenthCount .lt. 0) THEN
          CALL output_line('Mismatched parenthesis in string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        IF (sfunctionString(ifunctionIndex-1:ifunctionIndex-1) .eq. '(') THEN
          CALL output_line('Empty parentheses in string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
              OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
          CALL sys_halt()
        END IF
        ifunctionIndex = ifunctionIndex+1
        IF (ifunctionIndex .gt. ifunctionLength) EXIT
        c = sfunctionString(ifunctionIndex:ifunctionIndex)
      END DO

      ! Now, we have a legal operand: A legal operator or the end of
      ! string must follow
2     IF (ifunctionIndex .gt. ifunctionLength) EXIT

      ! Check operators
      iopSize = 0
      IF (.NOT.stack_empty(rstack)) THEN
        CALL stack_top(rstack, idummy)
        IF (c .eq. ',' .AND. idummy .eq. iparenthCount) THEN
          iopSize = 1
        ELSE
          iopSize = isOperator(sfunctionString(ifunctionIndex:))
        END IF
      ELSE
        iopSize = isOperator(sfunctionString(ifunctionIndex:))
      END IF

      IF (iopSize .eq. 0) THEN
        CALL output_line('Operator expected in string '//&
            TRIM(ADJUSTL(sfunctionString))//' !',&
            OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
        CALL sys_halt()
      END IF

      ! Now, we have an operand and an operator: the next loop will check for another
      ! operand (must appear)
      ifunctionIndex = ifunctionIndex+iopSize
    END DO

    ! Sanity check if the number of opening and closing brackets is the same
    IF (iparenthCount .ne. 0) THEN
      CALL output_line('Missing ) in string '//&
              TRIM(ADJUSTL(sfunctionString))//' !',&
          OU_CLASS_ERROR, OU_MODE_STD,'CheckSyntax')
      CALL sys_halt()
    END IF

    CALL stack_release(rstack)

  END SUBROUTINE CheckSyntax

  ! *****************************************************************************

!<function>

  FUNCTION isOperator (sfunctionString) RESULT (n)

!<description>
    ! Return 0 if given string is not an operator, else the size of the
    ! operator
!</description>

!<input>
    ! Operator string
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString
!</input>

!<result>
    ! Length of operator, 0 if string is no operator
    INTEGER(is) :: n
!</result>
!</function>

    INTEGER(is) :: j

    ! Check all operators
    DO j = cAdd, cOr
      IF (INDEX(sfunctionString,TRIM(Ops(j))) .eq. 1) THEN
        n=LEN_TRIM(Ops(j))
        RETURN
      END IF
    END DO
    n = 0

  END FUNCTION isOperator

  ! *****************************************************************************

!<function>

  FUNCTION MathFunctionIndex (sfunctionString) RESULT (n)

!<description>
    ! Return index of math function beginnig at 1st position of string str
!</description>

!<input>
    ! Math function string
    CHARACTER (LEN=*), INTENT(in) :: sfunctionString
!</input>

!<result>
    ! Index of math function
    INTEGER(is) :: n
!</result>
!</function>

    ! local variables
    CHARACTER (LEN=LEN(sfunctionString)) :: sfunctionName

    ! Check all math functions
    CALL sys_tolower(sfunctionString, sfunctionName)
    DO n = cIf, cSign
      IF (INDEX(sfunctionName,TRIM(Funcs(n))) .eq. 1) RETURN
    END DO
    n = 0

  END FUNCTION MathFunctionIndex

  ! *****************************************************************************

!<function>

  FUNCTION MathFunctionParameters (ifunctionIndex) RESULT (nparameters)

!<description>
    ! Return number of required parameters
!</description>

!<input>
    ! Index of function
    INTEGER(is) :: ifunctionIndex
!</input>

!<result>
    ! Number if required parameters
    INTEGER :: nparameters
!</result>
!</function>

    SELECT CASE(ifunctionIndex)
    CASE(cIf)
      nparameters = 3

    CASE(cMin:cAtan2)
      nparameters = 2

    CASE(cAbs:cSign)
      nparameters = 1

    CASE default
      nparameters = 0
      CALL output_line('Not a function',&
          OU_CLASS_WARNING, OU_MODE_STD,'MathFunctionParameters')
    END SELECT

  END FUNCTION MathFunctionParameters

  ! *****************************************************************************

!<function>

  FUNCTION ConstantIndex (sfunctionString) RESULT (n)

!<description>
    ! Return index of predefined constants beginnig at 1st position of
    ! string sfunctinString
!</description>

!<input>
    ! Math function string
    CHARACTER (LEN=*), INTENT(in) :: sfunctionString
!</input>

!<result>
    ! Index of math function
    INTEGER(is) :: n
!</result>
!</function>

    ! local variables
    CHARACTER (LEN=LEN(sfunctionString)-1) :: sconstantName

    ! Check all math functions
    CALL sys_tolower(sfunctionString(2:), sconstantName)
    DO n = 1, nconstants
      IF (INDEX(sconstantName,TRIM(CconstantName(n))) .eq. 1) RETURN
    END DO
    n = 0

  END FUNCTION ConstantIndex

  ! *****************************************************************************

!<function>

  FUNCTION ExpressionIndex (sfunctionString) RESULT (n)

!<description>
    ! Return index of predefined expression beginnig at 1st position
    ! of string sfunctionString
!</description>

!<input>
    ! Math function string
    CHARACTER (LEN=*), INTENT(in) :: sfunctionString
!</input>

!<result>
    ! Index of math function
    INTEGER(is) :: n
!</result>
!</function>

    ! local variables
    CHARACTER (LEN=LEN(sfunctionString)-1) :: sexpressionName

    ! Check all math functions
    CALL sys_tolower(sfunctionString(2:), sexpressionName)
    DO n = 1, nexpressions
      IF (INDEX(sexpressionName,TRIM(CexpressionName(n))) .eq. 1) RETURN
    END DO
    n = 0

  END FUNCTION ExpressionIndex

  ! *****************************************************************************

!<function>

  FUNCTION VariableIndex (sstring, Svariables, ibegin, inext) RESULT (n)

!<description>
    ! Return index of variable at begin of string sfunctionString
    ! (returns 0 if no variable found)
!</description>

!<input>
    ! String
    CHARACTER (LEN=*), INTENT(in) :: sstring

    ! Array with variable names
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<output>
    ! OPTIONAL: Start position of variable name
    INTEGER, OPTIONAL, INTENT(out) :: ibegin

    ! OPTIONAL: Position of character after name
    INTEGER, OPTIONAL, INTENT(out) :: inext
!</output>

!<result>
    ! Index of variable
    INTEGER(is) :: n
!</result>
!</function>

    ! local variables
    INTEGER :: j,ib,in,istringlen

    n = 0
    istringlen = LEN_TRIM(sstring)
    IF (istringlen .gt. 0) THEN
      ! Search for first character in str
      DO ib = 1, istringlen
        ! When lstr>0 at least 1 char in str
        IF (sstring(ib:ib) .ne. ' ') EXIT
      END DO

      ! Search for name terminators
      DO in = ib, istringlen
        IF (SCAN(sstring(in:in),'*+-/%^),&|<>=! ') > 0) EXIT
      END DO
      DO j = 1, SIZE(Svariables)
        IF (sstring(ib:in-1) .eq. Svariables(j)) THEN
          ! Variable name found
          n = j
          EXIT
        END IF
      END DO
    END IF

    IF (PRESENT(ibegin)) ibegin = ib
    IF (PRESENT(inext))  inext  = in

  END FUNCTION VariableIndex

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE RemoveSpaces (sfunctionString)

!<description>
    ! Remove Spaces from string, remember positions of characters in
    ! old string
!</description>

!<inputoutput>
    ! String from which spaces should be removed
    CHARACTER (LEN=*), INTENT(inout) :: sfunctionString
!</inputoutput>
!</subroutine>

    ! local variables
    INTEGER :: k,istringlen

    istringlen = LEN_TRIM(sfunctionString)
    k = 1
    DO WHILE (sfunctionString(k:istringlen) .ne. ' ')
      IF (sfunctionString(k:k) .eq. ' ') THEN
        sfunctionString(k:istringlen)  = sfunctionString(k+1:istringlen)//' ' ! Move 1 character to left
        k = k-1
      END IF
      k = k+1
    END DO

  END SUBROUTINE RemoveSpaces

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE Replace (ca, cb, sfunctionString)

!<description>
    ! Replace ALL appearances of character set ca in string sfunctionString by character set cb
!</description>

!<input>
    ! Source characters
    CHARACTER (LEN=*), INTENT(in) :: ca

    ! Destination characters
    CHARACTER (LEN=LEN(ca)), INTENT(in) :: cb
!</input>

!<inputoutput>
    ! String
    CHARACTER (LEN=*), INTENT(inout) :: sfunctionString
!</inputoutput>
!</subroutine>

    ! local variables
    INTEGER :: j,lca

    lca = LEN(ca)
    DO j = 1, LEN_TRIM(sfunctionString)-lca+1
      IF (sfunctionString(j:j+lca-1) .eq. ca) sfunctionString(j:j+lca-1) = cb
    END DO

  END SUBROUTINE Replace

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE Compile (rcomp, sfunctionString, Svariables)

!<description>
    ! Compile i-th function string into bytecode
!</description>

!<input>
    ! Function string
    CHARACTER (LEN=*), INTENT(in) :: sfunctionString

    ! Array with variable names
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser component
    TYPE (t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>
!</subroutine>

    ! local variables
    INTEGER(is), DIMENSION(:), POINTER :: IbyteCode
    REAL(DP), DIMENSION(:),    POINTER :: Dimmed
    COMPLEX(DP), DIMENSION(:), POINTER :: Zimmed
    INTEGER :: ind,isize

    ! (Re-)initialise the bytecode structure (if required)
    IF (ASSOCIATED(rcomp%IbyteCode)) DEALLOCATE (rcomp%IbyteCode)
    IF (ASSOCIATED(rcomp%Dimmed))    DEALLOCATE (rcomp%Dimmed)
    IF (ASSOCIATED(rcomp%Zimmed))    DEALLOCATE (rcomp%Zimmed)
    rcomp%ibytecodeSize = 0
    rcomp%iimmedSize    = 0
    rcomp%istackSize    = 0
    rcomp%istackPtr     = 0
    rcomp%bisVectorizable = .true.

    ! Neither the stack for the bytecode nor the stack for the
    ! immediate expressions can exceed the size of the function
    ! string. Hence, allocate some initial memory
    isize = FunctionSize(sfunctionString)
    ALLOCATE(rcomp%IbyteCode(isize), rcomp%Dimmed(isize), rcomp%Zimmed(isize))

    ! Compile function string into bytecode
    ind = CompileExpression(rcomp, sfunctionString, 1, Svariables)

    ! Adjust memory size of bytecode stack
    IF (rcomp%ibytecodeSize .eq. 0) THEN
      DEALLOCATE(rcomp%IbyteCode)
    ELSE
      ! Resize IbyteCode
      ALLOCATE(IbyteCode(rcomp%ibytecodeSize))
      IbyteCode = rcomp%IbyteCode(1:rcomp%ibytecodeSize)
      DEALLOCATE(rcomp%IbyteCode)
      ALLOCATE(rcomp%IbyteCode(rcomp%ibytecodeSize))
      rcomp%IbyteCode = IbyteCode
      DEALLOCATE(IbyteCode)
    END IF

    ! Adjust memory size of immediate stack
    IF (rcomp%iimmedSize .eq. 0) THEN
      DEALLOCATE(rcomp%Dimmed)
      DEALLOCATE(rcomp%Zimmed)
    ELSE
      ! Resize Dimmed
      ALLOCATE(Dimmed(rcomp%iimmedSize))
      Dimmed = rcomp%Dimmed(1:rcomp%iimmedSize)
      DEALLOCATE(rcomp%Dimmed)
      ALLOCATE(rcomp%Dimmed(rcomp%iimmedSize))
      rcomp%Dimmed = Dimmed
      DEALLOCATE(Dimmed)

      ! Resize Zimmed
      ALLOCATE(Zimmed(rcomp%iimmedSize))
      Zimmed = rcomp%Zimmed(1:rcomp%iimmedSize)
      DEALLOCATE(rcomp%Zimmed)
      ALLOCATE(rcomp%Zimmed(rcomp%iimmedSize))
      rcomp%Zimmed = Zimmed
      DEALLOCATE(Zimmed)
    END IF

  END SUBROUTINE Compile

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE incStackPtr (rcomp)

!<description>
    ! Increase stack pointer
!</description>

!<inputoutput>
    TYPE (t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>
!</subroutine>

    rcomp%istackPtr = rcomp%istackPtr+1
    IF (rcomp%istackPtr .gt. rcomp%istackSize) rcomp%istackSize = rcomp%istackSize+1

  END SUBROUTINE incStackPtr

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE AddCompiledByte (rcomp, ibyte)

!<description>
    ! Add compiled byte to bytecode
!</description>

!<input>
    ! Value of byte to be added
    INTEGER(is), INTENT(in) :: ibyte
!</input>

!<inputoutput>
    TYPE (t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>
!</subroutine>

    ! local variables
    REAL(DP) :: daux
    INTEGER, DIMENSION(:), ALLOCATABLE :: p_Irandom1,p_Irandom2
    INTEGER :: iaux

    rcomp%ibytecodeSize = rcomp%ibytecodeSize + 1
    rcomp%IbyteCode(rcomp%ibytecodeSize) = ibyte

    ! Try to optimise the compiled bytecode. Check the bytecode
    ! instruction and compute some values on-the-fly of this is
    ! possible. If rcomp%bisComplex is false, we compute only the
    ! real-valued path and set the complex-valued path equal to the
    ! real-valued path. Some functions return complex-valued
    ! results. In this case rcomp%bisComplex is set to true and the
    ! real-valued path is overwritten by SYS_INFINITY_DP.
    SELECT CASE(ibyte)
      !------------------------------------------------------------
      ! Functions
      !------------------------------------------------------------
    CASE (cAbs)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = ABS(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = ABS(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAcos)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Acos(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .lt. -1.0_DP .OR. &
            rcomp%Dimmed(rcomp%iimmedSize) .gt.  1.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zacos(rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          ! Real-valued path
          rcomp%Dimmed(rcomp%iimmedSize) = ACOS(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zacos(rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAsin)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Asin(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .lt. -1.0_DP .OR. &
            rcomp%Dimmed(rcomp%iimmedSize) .gt.  1.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zasin(rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = ASIN(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zasin(rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAtan)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zatan(rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = ATAN(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zatan(rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAtan2)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex) THEN
          CALL output_line('Invalid complex argument for ATAN2!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = ATAN2(rcomp%Dimmed(rcomp%iimmedSize),&
                                                 rcomp%Dimmed(rcomp%iimmedSize-1))
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAcot)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zatan(1.0_dp/rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = ATAN(1.0_DP/rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zatan(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAcoth)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zatanh(1.0_dp/rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = datanh(1.0_DP/rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zatanh(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAcosh)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Acosh(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .lt. 1.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zacosh(rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = dacosh(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zacosh(rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAcsc)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Acsc(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .gt. -SYS_PI/2.0_DP .AND. &
            rcomp%Dimmed(rcomp%iimmedSize) .lt.  SYS_PI/2.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zasin(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          ! Real-valued path
          rcomp%Dimmed(rcomp%iimmedSize) =  ASIN(1.0_DP/rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zasin(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAcsch)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zasinh(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          ! Real-valued path
          rcomp%Dimmed(rcomp%iimmedSize) = dasinh(1.0_DP/rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zasinh(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAsec)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Asec(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .gt. -1.0_DP .AND. &
            rcomp%Dimmed(rcomp%iimmedSize) .lt.  1.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zacos(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          ! Real-valued path
          rcomp%Dimmed(rcomp%iimmedSize) =  ACOS(1.0_DP/rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zacos(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAsech)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Asech(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .lt. 0.0_DP .OR. &
            rcomp%Dimmed(rcomp%iimmedSize) .gt. 1.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zacosh(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          ! Real-valued path
          rcomp%Dimmed(rcomp%iimmedSize) = dacosh(1.0_DP/rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zacosh(1.0_DP/rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAsinh)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Asinh(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .le. 0.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zasinh(rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = dasinh(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zasinh(rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAtanh)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = zatanh(rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = datanh(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = zatanh(rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAnint)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for ANINT!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize) = ANINT(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize),0.0_DP,DP)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAint)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for AINT!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize) = AINT(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize),0.0_DP,DP)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cCeil)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(CEILING(REAL(rcomp%Zimmed(rcomp%iimmedSize))),&
                                                 CEILING(AIMAG(rcomp%Zimmed(rcomp%iimmedSize))),DP)
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = CEILING(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize),0.0_DP,DP)
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (Ccmplx)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN

          ! Not available for complex-valued case
          IF (IsComplex(rcomp%Zimmed(rcomp%iimmedSize)) .OR.&
              IsComplex(rcomp%Zimmed(rcomp%iimmedSize-1))) THEN
            CALL output_line('Invalid complex argument for CMPLX!',&
                OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
            CALL sys_halt()
          END IF

          rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(REAL(rcomp%Zimmed(rcomp%iimmedSize-1)),&
                                                   REAL(rcomp%Zimmed(rcomp%iimmedSize)),DP)
          rcomp%Dimmed(rcomp%iimmedSize-1) = SYS_INFINITY_DP

        ELSE
          rcomp%bisComplex =.true.
          rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),&
                                                   rcomp%Dimmed(rcomp%iimmedSize),DP)
          rcomp%Dimmed(rcomp%iimmedSize-1) = SYS_INFINITY_DP
        END IF
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (Cconj)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        ! Nothing needs to be done for the real-valued case
        rcomp%Zimmed(rcomp%iimmedSize) = CONJG(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (CCOS)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = COS(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = COS(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cCosh)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) =  COSH(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = zcosh(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cCot)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = 1.0_DP/ TAN(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = 1.0_DP/ztan(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cCoth)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = 1.0_DP/ TANH(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = 1.0_DP/ztanh(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cCsc)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = 1.0_DP/SIN(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = 1.0_DP/SIN(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cCsch)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = 1.0_DP/ SINH(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = 1.0_DP/zsinh(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (CEXP)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = EXP(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = EXP(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cFloor)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(FLOOR(REAL(rcomp%Zimmed(rcomp%iimmedSize))),&
                                                 FLOOR(AIMAG(rcomp%Zimmed(rcomp%iimmedSize))),DP)
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = FLOOR(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize),0.0_DP,DP)
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cIf)
      ! No optimization possible

    CASE (cImag)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) =       AIMAG(rcomp%Zimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(AIMAG(rcomp%Zimmed(rcomp%iimmedSize)),0.0_DP,DP)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (CLOG)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Log(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .le. 0.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = LOG(rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = LOG(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = LOG(rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cLog10)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Log10(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .lt. 0.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = LOG(rcomp%Zimmed(rcomp%iimmedSize))/LOG(10.0_DP)
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = LOG10(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = LOG(rcomp%Zimmed(rcomp%iimmedSize))/LOG(10.0_DP)
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cMax)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          IF (ABS(rcomp%Zimmed(rcomp%iimmedSize)) .ge.&
              ABS(rcomp%Zimmed(rcomp%iimmedSize-1))) THEN
            rcomp%Zimmed(rcomp%iimmedSize-1) = rcomp%Zimmed(rcomp%iimmedSize)
          END IF
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize-1) = MAX(rcomp%Dimmed(rcomp%iimmedSize),&
                                                 rcomp%Dimmed(rcomp%iimmedSize-1))
          rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        END IF
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cMin)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex) THEN
          CALL output_line('Invalid complex argument for MAX!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = MIN(rcomp%Dimmed(rcomp%iimmedSize),&
                                               rcomp%Dimmed(rcomp%iimmedSize-1))
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cReal)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) =       REAL(rcomp%Zimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(REAL(rcomp%Zimmed(rcomp%iimmedSize)),0.0_DP,DP)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cRrand)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for RRAND!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        CALL RANDOM_SEED (SIZE=iaux)
        ALLOCATE (p_Irandom1(iaux))
        ALLOCATE (p_Irandom2(iaux))
        CALL RANDOM_SEED (get=p_Irandom1)

        p_Irandom2(:) = 0
        p_Irandom2(1) = INT(rcomp%Dimmed(rcomp%iimmedSize-1))
        CALL RANDOM_SEED (put=p_Irandom2)
        daux = 0.0_DP
        DO iaux=1,MAX(1,INT(rcomp%Dimmed(rcomp%iimmedSize)))
          CALL RANDOM_NUMBER (daux)
        END DO

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = daux
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(daux,0.0_DP,DP)

        CALL RANDOM_SEED (put=p_Irandom1)
        DEALLOCATE(p_Irandom1,p_Irandom2)

        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cSec)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = 1.0_DP/COS(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = 1.0_DP/COS(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cSech)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = 1.0_DP/COSH(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = 1.0_DP/zcosh(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cSign)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = SIGN(1.0_DP,rcomp%Dimmed(rcomp%iimmedSize))

        ! Compute sign(A) = A./abs(A) following the definition in MATLAB
        rcomp%Zimmed(rcomp%iimmedSize) = rcomp%Zimmed(rcomp%iimmedSize)/&
                                         ABS(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (CSIN)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = SIN(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = SIN(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cSinh)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) =  SINH(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = zsinh(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (CSQRT)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Sqrt(A) gives complex result?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .lt. 0.0_DP) rcomp%bisComplex = .true.

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
          rcomp%Zimmed(rcomp%iimmedSize) = SQRT(rcomp%Zimmed(rcomp%iimmedSize))
        ELSE
          rcomp%Dimmed(rcomp%iimmedSize) = SQRT(rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize) = SQRT(rcomp%Zimmed(rcomp%iimmedSize))
        END IF
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cTan)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) =  TAN(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = ztan(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cTanh)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) =  TANH(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = ztanh(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

      !------------------------------------------------------------
      ! Misc
      !------------------------------------------------------------
    CASE (cImmed, cJump)
      ! No optimization needed

      !------------------------------------------------------------
      ! Operators
      !------------------------------------------------------------
    CASE (cNeg)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = -(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = -(rcomp%Zimmed(rcomp%iimmedSize))
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAdd)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize-1) = rcomp%Dimmed(rcomp%iimmedSize-1)+&
                                           rcomp%Dimmed(rcomp%iimmedSize)
        rcomp%Zimmed(rcomp%iimmedSize-1) = rcomp%Zimmed(rcomp%iimmedSize-1)+&
                                           rcomp%Zimmed(rcomp%iimmedSize)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cSub)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize-1) = rcomp%Dimmed(rcomp%iimmedSize-1)-&
                                           rcomp%Dimmed(rcomp%iimmedSize)
        rcomp%Zimmed(rcomp%iimmedSize-1) = rcomp%Zimmed(rcomp%iimmedSize-1)-&
                                           rcomp%Zimmed(rcomp%iimmedSize)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cMul)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize-1) = rcomp%Dimmed(rcomp%iimmedSize-1)*&
                                           rcomp%Dimmed(rcomp%iimmedSize)
        rcomp%Zimmed(rcomp%iimmedSize-1) = rcomp%Zimmed(rcomp%iimmedSize-1)*&
                                           rcomp%Zimmed(rcomp%iimmedSize)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cDiv)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Division by zero?
        IF (rcomp%Dimmed(rcomp%iimmedSize) .eq. 0.0_DP) THEN
          CALL output_line('Division by zero!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF
        rcomp%Dimmed(rcomp%iimmedSize-1) = rcomp%Dimmed(rcomp%iimmedSize-1)/&
                                           rcomp%Dimmed(rcomp%iimmedSize)

        ! Division by zero?
        IF (rcomp%Zimmed(rcomp%iimmedSize) .eq. CMPLX(0.0_DP,0.0_DP,DP)) THEN
          CALL output_line('Division by zero!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF
        rcomp%Zimmed(rcomp%iimmedSize-1) = rcomp%Zimmed(rcomp%iimmedSize-1)/&
                                           rcomp%Zimmed(rcomp%iimmedSize)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cMod)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! MOD(A,0) not available
        IF (rcomp%Dimmed(rcomp%iimmedSize) .eq. 0.0_DP) THEN
          CALL output_line('Invalid argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = MOD(rcomp%Dimmed(rcomp%iimmedSize-1),&
                                               rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cPow)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize-1) = rcomp%Dimmed(rcomp%iimmedSize-1)**&
                                           rcomp%Dimmed(rcomp%iimmedSize)
        rcomp%Zimmed(rcomp%iimmedSize-1) = rcomp%Zimmed(rcomp%iimmedSize-1)**&
                                           rcomp%Zimmed(rcomp%iimmedSize)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cEqual)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble((REAL(rcomp%Zimmed(rcomp%iimmedSize-1)) .eq.&
                                                         REAL(rcomp%Zimmed(rcomp%iimmedSize))) .AND.&
                                                       (AIMAG(rcomp%Zimmed(rcomp%iimmedSize-1)) .eq.&
                                                        AIMAG(rcomp%Zimmed(rcomp%iimmedSize))))
          rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        ELSE
          ! Copy data from real-valued case
          rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble(rcomp%Dimmed(rcomp%iimmedSize-1) .eq.&
                                                        rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        END IF
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cNEqual)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Different treatment for real- and complex-valued case
        IF(rcomp%bisComplex) THEN
          rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble((REAL(rcomp%Zimmed(rcomp%iimmedSize-1)) .ne.&
                                                         REAL(rcomp%Zimmed(rcomp%iimmedSize)))  .OR.&
                                                       (AIMAG(rcomp%Zimmed(rcomp%iimmedSize-1)) .ne.&
                                                        AIMAG(rcomp%Zimmed(rcomp%iimmedSize))))
          rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        ELSE
          ! Copy data from real-valued case
          rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble(rcomp%Dimmed(rcomp%iimmedSize-1) .ne.&
                                                        rcomp%Dimmed(rcomp%iimmedSize))
          rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        END IF
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cLess)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble(rcomp%Dimmed(rcomp%iimmedSize-1) .lt.&
                                                      rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cLessOrEq)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble(rcomp%Dimmed(rcomp%iimmedSize-1) .le.&
                                                      rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cGreater)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble(rcomp%Dimmed(rcomp%iimmedSize-1) .gt.&
                                                      rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cGreaterOrEq)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble(rcomp%Dimmed(rcomp%iimmedSize-1) .ge.&
                                                      rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cAnd)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble(DbleToLogc(rcomp%Dimmed(rcomp%iimmedSize-1)) .AND.&
                                                      DbleToLogc(rcomp%Dimmed(rcomp%iimmedSize)))
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cOr)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed .AND.&
          rcomp%IbyteCode(rcomp%ibytecodeSize-2) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize-1) = LogcToDble(DbleToLogc(rcomp%Dimmed(rcomp%iimmedSize-1)) .OR.&
                                                      DbleToLogc(rcomp%Dimmed(rcomp%iimmedSize)))
        rcomp%Zimmed(rcomp%iimmedSize-1) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize-1),0.0_DP,DP)
        CALL RemoveCompiledImmediate(rcomp)
        CALL RemoveCompiledByte(rcomp)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cNot)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize) = LogcToDble(.NOT.DbleToLogc(rcomp%Dimmed(rcomp%iimmedSize)))
        rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize),0.0_DP,DP)
        CALL RemoveCompiledByte(rcomp)
      END IF

      !------------------------------------------------------------
      ! Degrees-radians conversion
      !------------------------------------------------------------
    CASE (cDeg)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        ! Copy data from real-valued case
        rcomp%Dimmed(rcomp%iimmedSize) = RadToDeg(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize),0.0_DP,DP)
        CALL RemoveCompiledByte(rcomp)
      END IF

    CASE (cRad)
      IF (rcomp%IbyteCode(rcomp%ibytecodeSize-1) .eq. cImmed) THEN

        ! Not available for complex-valued case
        IF (rcomp%bisComplex .AND. IsComplex(rcomp%Zimmed(rcomp%iimmedSize))) THEN
          CALL output_line('Invalid complex argument for MOD!',&
              OU_CLASS_ERROR, OU_MODE_STD,'AddCompiledByte')
          CALL sys_halt()
        END IF

        rcomp%Dimmed(rcomp%iimmedSize) = DegToRad(rcomp%Dimmed(rcomp%iimmedSize))
        rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(rcomp%Dimmed(rcomp%iimmedSize),0.0_DP,DP)
        CALL RemoveCompiledByte(rcomp)
      END IF
    END SELECT

  END SUBROUTINE AddCompiledByte

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE RemoveCompiledByte (rcomp)

!<description>
    ! Remove last compiled byte from bytecode
!</description>

!<inputoutput>
    TYPE (t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>
!</subroutine>

    rcomp%IbyteCode(rcomp%ibytecodeSize) = 0
    rcomp%ibytecodeSize = rcomp%ibytecodeSize - 1

  END SUBROUTINE RemoveCompiledByte

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE AddImmediateDouble (rcomp, dimmediate)

!<description>
    ! Add double-valued immediate
!</description>

!<input>
    ! Value of byte to be added
    REAL(DP), INTENT(in) :: dimmediate
!</input>

!<inputoutput>
    TYPE (t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>
!</subroutine>

    rcomp%iimmedSize               = rcomp%iimmedSize + 1
    rcomp%Dimmed(rcomp%iimmedSize) = dimmediate
    rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(dimmediate,0.0_DP,DP)

  END SUBROUTINE AddImmediateDouble

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE AddImmediateComplex (rcomp, zimmediate)

!<description>
    ! Add complex-valued immediate
!</description>

!<input>
    ! Value of byte to be added
    COMPLEX(DP), INTENT(in) :: zimmediate
!</input>

!<inputoutput>
    TYPE (t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>
!</subroutine>

    rcomp%iimmedSize               = rcomp%iimmedSize + 1
    rcomp%Zimmed(rcomp%iimmedSize) = SYS_INFINITY_DP
    rcomp%Zimmed(rcomp%iimmedSize) = zimmediate

    ! Complex-valued case
    rcomp%bisComplex = .true.

  END SUBROUTINE AddImmediateComplex

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE RemoveCompiledImmediate (rcomp)

!<description>
    ! Remove last compiled immediate from immediate stack
!</description>

!<inputoutput>
    TYPE (t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>
!</subroutine>

    rcomp%Dimmed(rcomp%iimmedSize) = 0.0_DP
    rcomp%Zimmed(rcomp%iimmedSize) = CMPLX(0.0_DP,0.0_DP,DP)
    rcomp%iimmedSize               = rcomp%iimmedSize - 1

  END SUBROUTINE RemoveCompiledImmediate

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE AddFunctionOpcode (rcomp, iopcode)

!<description>
    ! Add compiled byte to bytecode
!</description>

!<input>
    ! Value of opcode to be added
    INTEGER(is), INTENT(in) :: iopcode
!</input>

!<inputoutput>
    TYPE (t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>
!</subroutine>

    IF (rcomp%buseDegreeConversion) THEN
      SELECT CASE(iopcode)
      CASE(CCOS, Ccosh, cCot, cCoth,&
           cCsc, cCsch, cSec, cSech,&
           CSIN, cSinh, cTan, cTanh)
        CALL AddCompiledByte(rcomp, cRad)
      END SELECT
    END IF

    CALL AddCompiledByte(rcomp, iopcode)

    IF (rcomp%buseDegreeConversion) THEN
      SELECT CASE(iopcode)
      CASE(cAcos, cAcosh, cAcot, cAcoth,&
           cAcsc, cACsch, cAsec, cAsech,&
           cAsin, cAsinh, cAtan, cAtanh, cAtan2)
        CALL AddCompiledByte(rcomp, cDeg)
      END SELECT
    END IF

  END SUBROUTINE AddFunctionOpcode

  ! *****************************************************************************

!<function>

  FUNCTION RealNum (sfunctionString, ibegin, inext, berror) RESULT (dresult)

!<description>
    ! Get real number from string
    ! Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
!</description>

!<input>
    ! String
    CHARACTER (LEN=*), INTENT(in) :: sfunctionString
!</input>

!<output>
    ! OPTIONAL: Start position of real number
    INTEGER, OPTIONAL, INTENT(out) :: ibegin

    ! OPTIONAL: 1st character after real number
    INTEGER, OPTIONAL, INTENT(out) :: inext

    ! OPTIONAL: Error flag
    LOGICAL, OPTIONAL, INTENT(out) :: berror
!</output>

!<result>
    ! Real number
    REAL(DP) :: dresult
!</result>
!</function>

    ! local variables
    INTEGER :: ib,in,istat
    LOGICAL :: Bflag,               & ! .T. at begin of number in string
               InMan,               & ! .T. in mantissa of number
               Pflag,               & ! .T. after 1st '.' encountered
               Eflag,               & ! .T. at exponent identifier 'eEdD'
               InExp,               & ! .T. in exponent of number
               DInMan,              & ! .T. if at least 1 digit in mant.
               DInExp,              & ! .T. if at least 1 digit in exp.
               err                    ! Local error flag


    Bflag=.true.; InMan=.false.; Pflag=.false.; Eflag=.false.; InExp=.false.
    DInMan=.false.; DInExp=.false.
    ib   = 1
    in   = 1
    DO WHILE (in .le. LEN_TRIM(sfunctionString))
      SELECT CASE (sfunctionString(in:in))
      CASE (' ') ! Only leading blanks permitted
        ib = ib+1
        IF (InMan .OR. Eflag .OR. InExp) EXIT
      CASE ('+','-') ! Permitted only
        IF     (Bflag) THEN
          InMan=.true.; Bflag=.false. ! - at beginning of mantissa
        ELSE IF (Eflag) THEN
          InExp=.true.; Eflag=.false. ! - at beginning of exponent
        ELSE
          EXIT ! - otherwise CALL sys_halt()
        END IF
      CASE ('0':'9') ! Mark
        IF     (Bflag) THEN
          InMan=.true.; Bflag=.false. ! - beginning of mantissa
        ELSE IF (Eflag) THEN
          InExp=.true.; Eflag=.false. ! - beginning of exponent
        END IF
        IF (InMan) DInMan=.true. ! Mantissa contains digit
        IF (InExp) DInExp=.true. ! Exponent contains digit
      CASE ('.')
        IF     (Bflag) THEN
          Pflag=.true. ! - mark 1st appearance of '.'
          InMan=.true.; Bflag=.false. !   mark beginning of mantissa
        ELSE IF (InMan .AND..NOT.Pflag) THEN
          Pflag=.true. ! - mark 1st appearance of '.'
        ELSE
          EXIT ! - otherwise CALL sys_halt()
        END IF
      CASE ('e','E','d','D') ! Permitted only
        IF (InMan) THEN
          Eflag=.true.; InMan=.false. ! - following mantissa
        ELSE
          EXIT ! - otherwise CALL sys_halt()
        END IF
      CASE default
        EXIT ! CALL sys_halt() at all other characters
      END SELECT
      in = in+1
    END DO
    err = (ib .gt. in-1) .OR. (.NOT.DInMan) .OR.&
          ((Eflag.OR.InExp).AND..NOT.DInExp)
    IF (err) THEN
      dresult = 0.0_DP
    ELSE
      READ(sfunctionString(ib:in-1),*, IOSTAT=istat) dresult
      err = istat .ne. 0
    END IF
    IF (PRESENT(ibegin)) ibegin = ib
    IF (PRESENT(inext))  inext  = in
    IF (PRESENT(berror)) berror = err

  END FUNCTION RealNum

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION FunctionSize (sfunctionString) RESULT (isize)

!<description>
    ! Return the size of the total function including external expressions
!</description>

!<input>
    ! Function string
    CHARACTER (LEN=*), INTENT(in) :: sfunctionString
!</input>

!<result>
    ! Size of function string
    INTEGER :: isize
!</result>
!</function>

    ! local variables
    INTEGER :: ind,n
    CHARACTER(LEN=1) :: c

    ! Determine size of given expression
    isize = LEN_TRIM(sfunctionString)

    ! "Parse" string for externally defined expressions
    DO ind = 1, isize
      c = sfunctionString(ind:ind)
      IF (c .eq. '@') THEN
        n = ExpressionIndex (sfunctionString(ind:))
        IF (n .eq. 0) THEN
          CALL output_line('Invalid expression!',&
              OU_CLASS_ERROR, OU_MODE_STD,'FunctionSize')
          CALL sys_halt()
        END IF
        isize = isize+FunctionSize(CexpressionString(n))
      END IF
    END DO

  END FUNCTION FunctionSize

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileExpression(rcomp, sfunctionString, ind, Svariables,&
                                       bstopAtComma) RESULT(ind2)

!<description>
    ! Compiles ','
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables

    ! OPTIONAL: stop at comma
    LOGICAL, INTENT(in), OPTIONAL :: bstopAtComma
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    INTEGER :: ifunctionLength

    ifunctionLength = LEN_TRIM(sfunctionString)

    ind2 = CompileOr(rcomp, sfunctionString, ind, Svariables)
    IF(ind2 > ifunctionLength) RETURN

    IF (PRESENT(bstopAtComma)) THEN
      IF (bstopAtComma) RETURN
    END IF

    DO WHILE (sfunctionString(ind2:ind2) .eq. ',')
      ind2 = CompileOr(rcomp, sfunctionString, ind2+1, Svariables)
      IF (ind2 .gt. ifunctionLength) RETURN
    END DO

  END FUNCTION CompileExpression

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileOr(rcomp, sfunctionString, ind, Svariables) RESULT(ind2)

!<description>
    ! Compiles '|'
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    INTEGER :: ifunctionLength

    ifunctionLength = LEN_TRIM(sfunctionString)

    ind2 = CompileAnd(rcomp, sfunctionString, ind, Svariables)
    IF (ind2 .gt. ifunctionLength) RETURN

    DO WHILE(sfunctionString(ind2:ind2) .eq. '|')
      ind2 = CompileAnd(rcomp, sfunctionString, ind2+1, Svariables)

      CALL AddCompiledByte(rcomp, cOr)
      rcomp%istackPtr = rcomp%istackPtr-1
      IF (ind2 .gt. ifunctionLength) RETURN
    END DO

  END FUNCTION CompileOr

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileAnd(rcomp, sfunctionString, ind, Svariables) RESULT(ind2)

!<description>
    ! Compiles '&'
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    INTEGER :: ifunctionLength

    ifunctionLength = LEN_TRIM(sfunctionString)

    ind2 = CompileComparison(rcomp, sfunctionString, ind, Svariables)
    IF (ind2 .gt. ifunctionLength) RETURN

    DO WHILE(sfunctionString(ind2:ind2) .eq. '&')
      ind2 = CompileComparison(rcomp, sfunctionString, ind2+1, Svariables)

      CALL AddCompiledByte(rcomp, cAnd)
      rcomp%istackPtr = rcomp%istackPtr-1
      IF (ind2 .gt. ifunctionLength) RETURN
    END DO

  END FUNCTION CompileAnd

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileComparison(rcomp, sfunctionString, ind, Svariables) RESULT(ind2)

!<description>
    ! Compiles '=', '<' and '>'
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    CHARACTER(LEN=1) :: c
    INTEGER(is) :: iopSize
    INTEGER :: ifunctionLength

    ifunctionLength = LEN_TRIM(sfunctionString)

    ind2 = CompileAddition(rcomp, sfunctionString, ind, Svariables)
    IF (ind2 .gt. ifunctionLength) RETURN

    c=sfunctionString(ind2:ind2)
    DO WHILE(c .eq. '=' .OR. c .eq. '<' .OR. c .eq. '>' .OR. c .eq. '!')
      iopSize = MERGE(2, 1, sfunctionString(ind2+1:ind2+1) .eq. '=')
      ind2 = CompileAddition(rcomp, sfunctionString, ind2+iopSize, Svariables)

      SELECT CASE(c)
      CASE('=')
        CALL AddCompiledByte(rcomp, cEqual)

      CASE('<')
        CALL AddCompiledByte(rcomp, MERGE(cLess, cLessOrEq, iopSize .eq. 1))

      CASE('>')
        CALL AddCompiledByte(rcomp, MERGE(cGreater, cGreaterOrEq, iopSize .eq. 1))

      CASE('!')
        CALL AddCompiledByte(rcomp, cNEqual)
      END SELECT
      rcomp%istackPtr = rcomp%istackPtr-1

      IF (ind2 .gt. ifunctionLength) RETURN
      c=sfunctionString(ind2:ind2)
    END DO

  END FUNCTION CompileComparison

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileAddition(rcomp, sfunctionString, ind, Svariables) RESULT(ind2)

!<description>
    ! Compiles '+' and '-'
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    CHARACTER(LEN=1) :: c
    INTEGER :: ifunctionLength

    ifunctionLength = LEN_TRIM(sfunctionString)

    ind2 = CompileMult(rcomp, sfunctionString, ind, Svariables)
    IF (ind2 .gt. ifunctionLength) RETURN

    c=sfunctionString(ind2:ind2)
    DO WHILE(c .eq. '+' .OR. c .eq. '-')
      ind2 = CompileMult(rcomp, sfunctionString, ind2+1, Svariables)

      CALL AddCompiledByte(rcomp, MERGE(cAdd, cSub, c .eq. '+'))
      rcomp%istackPtr = rcomp%istackPtr-1

      IF (ind2 .gt. ifunctionLength) RETURN
      c=sfunctionString(ind2:ind2)
    END DO

  END FUNCTION CompileAddition

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileMult(rcomp, sfunctionString, ind, Svariables) RESULT(ind2)

!<description>
    ! Compiles '*', '/' and '%'
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    CHARACTER(LEN=1) :: c
    INTEGER :: ifunctionLength

    ifunctionLength = LEN_TRIM(sfunctionString)

    ind2 = CompileUnaryMinus(rcomp, sfunctionString, ind, Svariables)
    IF (ind2 .gt. ifunctionLength) RETURN

    c=sfunctionString(ind2:ind2)
    DO WHILE(c .eq. '*' .OR. c .eq. '/' .OR. c .eq. '%')
      ind2 = CompileUnaryMinus(rcomp, sfunctionString, ind2+1, Svariables)

      SELECT CASE(c)
      CASE('*')
        CALL AddCompiledByte(rcomp, cMul)

      CASE('/')
        CALL AddCompiledByte(rcomp, cDiv)

      CASE('%')
        CALL AddCompiledByte(rcomp, cMod)

      END SELECT
      rcomp%istackPtr = rcomp%istackPtr-1

      IF (ind2 .gt. ifunctionLength) RETURN
      c=sfunctionString(ind2:ind2)
    END DO

  END FUNCTION CompileMult

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileUnaryMinus(rcomp, sfunctionString, ind, Svariables) RESULT(ind2)

!<description>
    ! Compiles unary '-'
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    CHARACTER(LEN=1) :: c
    INTEGER :: ifunctionLength

    ifunctionLength = LEN_TRIM(sfunctionString)

    c=sfunctionString(ind:ind)
    IF (c .eq. '-' .OR. c .eq. '!') THEN
      ind2 = ind+1
      IF (ind2 .gt. ifunctionLength) RETURN
      ind2 = CompilePow(rcomp, sfunctionString, ind2, Svariables)

      ! If we are negating a constant, negate the constant itself
      IF (c .eq. '-' .AND. rcomp%IbyteCode(rcomp%ibytecodeSize) .eq. cImmed) THEN
        rcomp%Dimmed(rcomp%iimmedSize) = -rcomp%Dimmed(rcomp%iimmedSize)

        ! If we are negating a negation, we can remove both
      ELSE IF (c .eq. '-' .AND. rcomp%IbyteCode(rcomp%ibytecodeSize) .eq. cNeg) THEN
        CALL RemoveCompiledByte(rcomp)

      ELSE
        CALL AddCompiledByte(rcomp, MERGE(cNeg, cNot, c .eq. '-'))

      END IF
      RETURN
    END IF

    ind2 = CompilePow(rcomp, sfunctionString, ind, Svariables)

  END FUNCTION CompileUnaryMinus

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompilePow(rcomp, sfunctionString, ind, Svariables) RESULT(ind2)

!<description>
    ! Compiles '^'
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    INTEGER :: ifunctionLength

    ifunctionLength = LEN_TRIM(sfunctionString)

    ind2 = CompileElement(rcomp, sfunctionString, ind, Svariables)
    IF (ind2 .gt. ifunctionLength) RETURN

    DO WHILE(sfunctionString(ind2:ind2) .eq. '^')
      ind2 = CompileUnaryMinus(rcomp, sfunctionString, ind2+1, Svariables)

      CALL AddCompiledByte(rcomp, cPow)
      rcomp%istackPtr = rcomp%istackPtr-1
      IF (ind2 .gt. ifunctionLength) RETURN
    END DO

  END FUNCTION CompilePow

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileElement(rcomp, sfunctionString, ind, Svariables) RESULT(ind2)

!<description>
    ! Compiles element
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    CHARACTER(LEN=1) :: c
    REAL(DP) :: dnumber
    INTEGER(is) :: n
    INTEGER :: ind1,ind0,ib,in,nparams
    LOGICAL :: berror

    ind1 = ind
    c = sfunctionString(ind1:ind1)
    IF (c .eq. '(') THEN
      ind1 = CompileExpression(rcomp, sfunctionString, ind1+1, Svariables, .false.)
      ind2 = ind1+1   ! sfunctionString(ind1:ind1) is ')'
      RETURN
    END IF

    ! Check for numbers
    IF (SCAN(c,'0123456789,') > 0) THEN
      dnumber = RealNum (sfunctionString(ind1:), ib, in, berror)
      IF (berror) THEN
        CALL output_line('Invalid number format!',&
            OU_CLASS_ERROR, OU_MODE_STD,'CompileElement')
        CALL sys_halt()
      END IF
      ind2 = ind1+in-1

!!$      ! Check if this is an imaginary number z = (a,b) = a+bi
!!$      c = sfunctionString(ind2:ind2)
!!$      if (c .eq. ',') then
!!$        ind1 = ind2+1
!!$        dnumber2 = RealNum (sfunctionString(ind1:), ib, in, berror)
!!$        if (berror) then
!!$          call output_line('Invalid number format!',&
!!$              OU_CLASS_ERROR, OU_MODE_STD,'CompileElement')
!!$          call sys_halt()
!!$        end if
!!$        ind2 = ind1+in-1
!!$
!!$        call AddImmediateComplex(rcomp, cmplx(dnumber,dnumber2,DP))
!!$        call AddCompiledByte(rcomp, cImmed)
!!$        call incstackPtr(rcomp)
!!$        return
!!$      end if

      CALL AddImmediateDouble(rcomp, dnumber)
      CALL AddCompiledByte(rcomp, cImmed)
      CALL incstackPtr(rcomp)
      RETURN

    ELSE
      ! Then string must be function, variable or constant

      ! Check for mathematical functions
      n = MathFunctionIndex(sfunctionString(ind1:))
      IF (n .gt. 0) THEN
        ind2 = ind1+LEN_TRIM(Funcs(n))

        ! Check for IF-THEN-ELSE
        IF (n .eq. cIf) THEN
          ind2 = CompileIf(rcomp, sfunctionString, ind2+1, Svariables)
          ! IF-THEN-ELSE cannot be vectorised, note that!
          rcomp%bisVectorizable = .false.
          RETURN
        END IF

        nparams = MathFunctionParameters(n)
        ind2 = CompileFunctionParameters(rcomp, sfunctionString, ind2+1, Svariables, nparams)
        CALL AddFunctionOpcode(rcomp, n)
        RETURN
      END IF

      ! Check for predefined constant
      n = ConstantIndex(sfunctionString(ind1:))
      IF (n .gt. 0) THEN
        ind2 = ind1+LEN_TRIM(CconstantName(n))+1
        CALL AddImmediateDouble(rcomp, DconstantValue(n))
        CALL AddCompiledByte(rcomp, cImmed)
        CALL incStackPtr(rcomp)
        RETURN
      END IF

      ! Check for predefined expressions
      n = ExpressionIndex(sfunctionString(ind1:))
      IF (n .gt. 0) THEN
        ind2 = ind1+LEN_TRIM(CexpressionName(n))+1

        ! Recursively compile the given expression
        ind0 = CompileExpression(rcomp, CexpressionString(n), 1, Svariables)

        ! Proceed with compilation of mathematical function Func afterwards
        RETURN
      END IF

      ! Check for variables
      n = VariableIndex(sfunctionString(ind1:), Svariables, ib, in)
      IF (n > 0) n = VarBegin+n-1
      CALL AddCompiledByte(rcomp, n)
      CALL incStackPtr(rcomp)
      ind2 = ind1+in-1
      RETURN
    END IF

    CALL output_line('An unexpected error occured!',&
        OU_CLASS_ERROR, OU_MODE_STD,'CompileElement')
    CALL sys_halt()

  END FUNCTION CompileElement

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileFunctionParameters(rcomp, sfunctionString, ind, Svariables,&
                                               nparams) RESULT(ind2)

!<description>
    ! Compiles function parameters
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables

    ! Number of required parameters
    INTEGER, INTENT(in) :: nparams
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    INTEGER :: iStackPtr

    ind2 = ind
    IF (nparams .gt. 0) THEN

      iStackPtr = rcomp%istackPtr
      ind2 = CompileExpression(rcomp, sfunctionString, ind, Svariables, .false.)

      IF (rcomp%istackPtr .ne. iStackPtr+nparams) THEN
        CALL output_line('Illegal number of parameters to function!',&
            OU_CLASS_ERROR, OU_MODE_STD,'CompileFunctionParameters')
        CALL sys_halt()
      END IF

      rcomp%istackPtr = rcomp%istackPtr-(nparams-1)

    ELSE

      CALL incStackPtr(rcomp)

    END IF
    ind2=ind2+1

  END FUNCTION CompileFunctionParameters

  ! *****************************************************************************

!<function>

  RECURSIVE FUNCTION CompileIf(rcomp, sfunctionString, ind, Svariables) RESULT(ind2)

!<description>
    ! Compiles if()
!</description>

!<input>
    ! Function substring
    CHARACTER(LEN=*), INTENT(in) :: sfunctionString

    ! Begin position substring
    INTEGER, INTENT(in) :: ind

    ! Array with variable names
    CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Svariables
!</input>

!<inputoutput>
    ! Function parser
    TYPE(t_fparserComponent), INTENT(inout) :: rcomp
!</inputoutput>

!<result>
    INTEGER :: ind2
!</result>
!</function>

    ! local variables
    INTEGER :: ifunctionLength,curibytecodeSize,curibytecodeSize2,curiimmedSize2

    ifunctionLength = LEN_TRIM(sfunctionString)

    ind2 = CompileExpression(rcomp, sfunctionString, ind, Svariables, .true.) ! Condition branch
    IF (ind2 .gt. ifunctionLength) RETURN

    IF (sfunctionString(ind2:ind2) .ne. ',') THEN
      CALL output_line('Illegal number of parameters to function!',&
          OU_CLASS_ERROR, OU_MODE_STD,'CompileIf')
      CALL sys_halt()
    END IF
    CALL AddCompiledByte(rcomp, cIf)
    curibytecodeSize = rcomp%ibytecodeSize
    CALL AddCompiledByte(rcomp, 0_is) ! Jump index will be set below
    CALL AddCompiledByte(rcomp, 0_is) ! Immed jump index will be set below
    rcomp%istackPtr = rcomp%istackPtr-1

    ind2 = CompileExpression(rcomp, sfunctionString, ind2+1, Svariables, .true.) ! Then branch
    IF (ind2 .gt. ifunctionLength) RETURN

    IF (sfunctionString(ind2:ind2) .ne. ',') THEN
      CALL output_line('Illegal number of parameters to function!',&
          OU_CLASS_ERROR, OU_MODE_STD,'CompileIf')
      CALL sys_halt()
    END IF
    CALL AddCompiledByte(rcomp, cJump)
    curibytecodeSize2 = rcomp%ibytecodeSize
    curiimmedSize2 = rcomp%iimmedSize
    CALL AddCompiledByte(rcomp, 0_is) ! Jump index will be set below
    CALL AddCompiledByte(rcomp, 0_is) ! Immed jump index will be set below
    rcomp%istackPtr = rcomp%istackPtr-1

    ind2 = CompileExpression(rcomp, sfunctionString, ind2+1, Svariables, .true.) ! Else branch
    IF (ind2 .gt. ifunctionLength) RETURN

    IF (sfunctionString(ind2:ind2) .ne. ')') THEN
      CALL output_line('Illegal number of parameters to function!',&
          OU_CLASS_ERROR, OU_MODE_STD,'CompileIf')
      CALL sys_halt()
    END IF

    ! Set jump indices
    IF (ASSOCIATED(rcomp%IbyteCode)) THEN
      rcomp%IbyteCode(curibytecodeSize+1)  = curibytecodeSize2+2
      rcomp%IbyteCode(curibytecodeSize+2)  = curiimmedSize2+1
      rcomp%IbyteCode(curibytecodeSize2+1) = rcomp%ibytecodeSize
      rcomp%IbyteCode(curibytecodeSize2+2) = rcomp%iimmedSize+1
    END IF

    ind2=ind2+1

  END FUNCTION CompileIf

  ! *****************************************************************************

!<function>

  ELEMENTAL FUNCTION DbleToLogc(d) RESULT(l)

!<description>
    ! This function transforms a Double into a Logical
!</description>

!<input>
    ! Double variable
    REAL(DP), INTENT(in) :: d
!</input>

!<result>
    ! Logical variable
    LOGICAL :: l
!</result>
!</function>

    ! Convert all nonzero double values to .true. and 0.0_DP to
    ! .false. This behavior is consistent with the MATLAB
    ! implementation.
    l = .NOT.(ABS(d) .le. 1e-12)

    ! Previous implementation
    ! l = (abs(1-d) .le. 1e-12)

  END FUNCTION DbleToLogc

  ! *****************************************************************************

!<function>

  ELEMENTAL FUNCTION CmplToLogc(c) RESULT(l)

!<description>
    ! This function transforms a Complex into a Logical
!</description>

!<input>
    ! Complex variable
    COMPLEX(DP), INTENT(in) :: c
!</input>

!<result>
    ! Logical variable
    LOGICAL :: l
!</result>
!</function>

    ! Convert all nonzero complex values to .true. and (0.0_DP,0.0_DP)
    ! to .false. This behavior is consistent with the MATLAB
    ! implementation.
    l = .NOT.(ABS(c) .le. 1e-12)

    ! Previous implementation
    ! l = (abs(1-c) .le. 1e-12)

  END FUNCTION CmplToLogc

  ! *****************************************************************************

!<function>

  ELEMENTAL FUNCTION LogcToDble(l) RESULT(d)

!<description>
    ! This function transforms a Logical into a Double
!</description>

!<input>
    ! Logical variable
    LOGICAL, INTENT(in) :: l
!</input>

!<result>
    ! Double variable
    REAL(DP) :: d
!</result>
!</function>

    d = MERGE(1.0_DP, 0.0_DP, l)

  END FUNCTION LogcToDble

  ! *****************************************************************************

!<function>

  ELEMENTAL FUNCTION DegToRad(d) RESULT(r)

!<description>
    ! This function converts DEG to RAD
!</description>

!<input>
    ! DEG
    REAL(DP), INTENT(in) :: d
!</input>

!<result>
    ! RAD
    REAL(DP) :: r
!</result>
!</function>

    r = d * (3.141592653589793115997963468544185161590576171875_DP / 180._DP)
  END FUNCTION DegToRad

  ! *****************************************************************************

!<function>

  ELEMENTAL FUNCTION RadToDeg(r) RESULT(d)

!<description>
    ! This function converts RAD to DEG
!</description>

!<input>
    ! RAD
    REAL(DP), INTENT(in) :: r
!</input>

!<result>
    ! DEG
    REAL(DP) :: d
!</result>
!</function>

    d = r * (180._DP / 3.141592653589793115997963468544185161590576171875_DP)

  END FUNCTION RadToDeg

  ! *****************************************************************************

!<function>

  ELEMENTAL FUNCTION IsComplex(c) RESULT(l)

!<description>
    ! This function returns .true. if the imaginary part of the given
    ! value c is not zero. Otherwise it returns .false.
!</description>

!<input>
    ! complex value
    COMPLEX(DP), INTENT(in) :: c
!</input>

!<result>
    ! .true. if the imaginary part of c is not zero
    LOGICAL :: l
!</result>
!</function>

    l = (AIMAG(c).ne.0.0_DP)

  END FUNCTION IsComplex

  ! *****************************************************************************

!<function>

  ELEMENTAL FUNCTION IsReal(c) RESULT(l)

!<description>
    ! This function returns .true. if the imaginary part of the given
    ! value c is zero. Otherwise it returns .false.
!</description>

!<input>
    ! complex value
    COMPLEX(DP), INTENT(in) :: c
!</input>

!<result>
    ! .true. if the imaginary part of c is zero
    LOGICAL :: l
!</result>
!</function>

    l = (AIMAG(c).eq.0.0_DP)

  END FUNCTION IsReal

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE evalFunctionScDble (rcomp, Dstack, Dvalue, EvalErrType, dresult)

!<description>
    ! Evaluate bytecode for the values passed in array Val(:).
    ! Assume that all intermediate values and the final result
    ! dresult is real-valued.
    !
    ! REMARK: If intermediate values become complex-valued then this
    ! subroutine exists with a non-zero error code EvalErrType.
!</description>

!<input>
    ! Component of function parser
    TYPE(t_fparserComponent), INTENT(in) :: rcomp

    ! Variable values
    REAL(DP), DIMENSION(:), INTENT(in) :: Dvalue
!</input>

!<inputoutput>
    ! Stack memory
    REAL(DP), DIMENSION(:), INTENT(inout) :: Dstack
!</inputoutput>

!<output>
    ! Error code for function evaluation
    INTEGER, INTENT(out) :: EvalErrType

    ! Evaluated function
    REAL(DP), INTENT(out) :: dresult
!</output>
!</subroutine>

    ! local variables
    INTEGER  :: iinstPtr,istackPtr,idataPtr
    INTEGER  :: ijumpAddr,iimmedAddr
    REAL(DP) :: daux
    INTEGER :: iaux
    INTEGER, DIMENSION(:), ALLOCATABLE :: p_Irandom1,p_Irandom2

    ! Initialization
    idataPtr  = 1
    istackPtr = 0
    iinstPtr  = 0

    ! Initialize error type
    EvalErrType = 0

    ! Repeat until complete bytecode has been processed
    DO WHILE(iinstPtr .lt. rcomp%ibytecodeSize)
      iinstPtr = iinstPtr+1

      ! What kind of bytecode are we?
      SELECT CASE (rcomp%IbyteCode(iinstPtr))
        !------------------------------------------------------------
        ! Functions
        !------------------------------------------------------------
      CASE (cAbs)
        Dstack(istackPtr) = ABS(Dstack(istackPtr))

      CASE (cAcos)
        IF ((Dstack(istackPtr) .lt. -1.0_DP) .OR. &
            (Dstack(istackPtr) .gt.  1.0_DP)) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = ACOS(Dstack(istackPtr))

      CASE (cAsin)
        IF ((Dstack(istackPtr) .lt. -1.0_DP) .OR. &
            (Dstack(istackPtr) .gt.  1.0_DP)) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = ASIN(Dstack(istackPtr))

      CASE (cAtan)
        Dstack(istackPtr) = ATAN(Dstack(istackPtr))

      CASE (cAtan2)
        Dstack(istackPtr-1) = ATAN2(Dstack(istackPtr-1), Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cAcot)
        Dstack(istackPtr) = ATAN(1.0_DP/Dstack(istackPtr))

      CASE (cAcoth)
        Dstack(istackPtr) = datanh(1.0_DP/Dstack(istackPtr))

      CASE (cAcosh)
        IF (Dstack(istackPtr) .lt. 1.0_DP) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = dacosh(Dstack(istackPtr))

      CASE (cAcsc)
        IF (Dstack(istackPtr) .gt. -SYS_PI/2.0_DP .AND.&
            Dstack(istackPtr) .lt.  SYS_PI/2.0_DP) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = ASIN(1.0_DP/Dstack(istackPtr))

      CASE (cAcsch)
        Dstack(istackPtr) = dasinh(1.0_DP/Dstack(istackPtr))

      CASE (cAsec)
        IF ((Dstack(istackPtr) .gt. -1.0_DP) .AND. &
            (Dstack(istackPtr) .lt.  1.0_DP)) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = ACOS(Dstack(istackPtr))

      CASE (cAsech)
        IF ((Dstack(istackPtr) .lt. 0.0_DP) .OR. &
            (Dstack(istackPtr) .gt. 1.0_DP)) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = dacosh(1.0_DP/Dstack(istackPtr))

      CASE (cAsinh)
        IF ( Dstack(istackPtr) .le. 0._DP) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = dasinh(Dstack(istackPtr))

      CASE (cAtanh)
        Dstack(istackPtr) = datanh(Dstack(istackPtr))

      CASE (cAnint)
        Dstack(istackPtr) = ANINT(Dstack(istackPtr))

      CASE (cAint)
        Dstack(istackPtr) = AINT(Dstack(istackPtr))

      CASE (cCeil)
        Dstack(istackPtr) = CEILING(Dstack(istackPtr))

      CASE (Ccmplx)
        ! Illegal operation
        EvalErrType = 3
        dresult     = SYS_INFINITY_DP

      CASE (Cconj)
        ! Nothing needs to be done for the real-valued case

      CASE (CCOS)
        Dstack(istackPtr) = COS(Dstack(istackPtr))

      CASE (cCosh)
        Dstack(istackPtr) = COSH(Dstack(istackPtr))

      CASE (cCot)
        Dstack(istackPtr) = 1.0_DP/TAN(Dstack(istackPtr))

      CASE (cCoth)
        Dstack(istackPtr) = 1.0_DP/TANH(Dstack(istackPtr))

      CASE (cCsc)
        Dstack(istackPtr) = 1.0_DP/SIN(Dstack(istackPtr))

      CASE (cCsch)
        Dstack(istackPtr) = 1.0_DP/SINH(Dstack(istackPtr))

      CASE (CEXP)
        Dstack(istackPtr) = EXP(Dstack(istackPtr))

      CASE (cFloor)
        Dstack(istackPtr) = FLOOR(Dstack(istackPtr))

      CASE (cIf)
        iinstPtr = iinstPtr+1;   ijumpAddr  = rcomp%IbyteCode(iinstPtr)
        iinstPtr = iinstPtr+1;   iimmedAddr = rcomp%IbyteCode(iinstPtr)
        IF (.NOT.DbleToLogc(Dstack(istackPtr))) THEN
          iinstPtr = ijumpAddr
          idataPtr = iimmedAddr
        END IF
        istackPtr = istackPtr-1

      CASE (cImag)
        Dstack(istackPtr) = 0.0_DP

      CASE (CLOG)
        IF (Dstack(istackPtr) .le. 0._DP) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = LOG(Dstack(istackPtr))

      CASE (cLog10)
        IF (Dstack(istackPtr) .le. 0._DP) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = LOG10(Dstack(istackPtr))

      CASE (cMax)
        Dstack(istackPtr-1) = MAX(Dstack(istackPtr-1), Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cMin)
        Dstack(istackPtr-1) = MIN(Dstack(istackPtr-1), Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cReal)
        ! Nothing needs to be done for the real-valued case

      CASE (cRrand)
        CALL RANDOM_SEED (SIZE=iaux)
        ALLOCATE (p_Irandom1(iaux))
        ALLOCATE (p_Irandom2(iaux))
        CALL RANDOM_SEED (get=p_Irandom1)

        p_Irandom2(:) = 0
        p_Irandom2(1) = INT(Dstack(istackPtr-1))
        CALL RANDOM_SEED (put=p_Irandom2)

        daux = 0.0_DP
        DO iaux=1,MAX(1,INT(Dstack(istackPtr)))
          CALL RANDOM_NUMBER (daux)
        END DO
        Dstack(istackPtr-1) = daux

        CALL RANDOM_SEED (put=p_Irandom1)
        DEALLOCATE(p_Irandom1,p_Irandom2)

        istackPtr = istackPtr-1

      CASE (cSec)
        Dstack(istackPtr) = 1.0_DP/COS(Dstack(istackPtr))

      CASE (cSech)
        Dstack(istackPtr) = 1.0_DP/COSH(Dstack(istackPtr))

      CASE (cSign)
        Dstack(istackPtr) = SIGN(1.0_DP,Dstack(istackPtr))

      CASE (CSIN)
        Dstack(istackPtr) = SIN(Dstack(istackPtr))

      CASE(cSinh)
        Dstack(istackPtr) = SINH(Dstack(istackPtr))

      CASE(CSQRT)
        IF (Dstack(istackPtr) .lt. 0.0_DP) THEN
          EvalErrType = 3
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr) = SQRT(Dstack(istackPtr))

      CASE (cTan)
        Dstack(istackPtr) = TAN(Dstack(istackPtr))

      CASE (cTanh)
        Dstack(istackPtr) = TANH(Dstack(istackPtr))

      !------------------------------------------------------------
      ! Misc
      !------------------------------------------------------------
      CASE (cImmed)
        istackPtr         = istackPtr+1
        Dstack(istackPtr) = rcomp%Dimmed(idataPtr)
        idataPtr          = idataPtr+1

      CASE (cJump)
        idataPtr = rcomp%IbyteCode(iinstPtr+2)
        iinstPtr = rcomp%IbyteCode(iinstPtr+1)

      !------------------------------------------------------------
      ! Operators
      !------------------------------------------------------------
      CASE (cNeg)
        Dstack(istackPtr) = -Dstack(istackPtr)

      CASE (cAdd)
        Dstack(istackPtr-1) = Dstack(istackPtr-1)+Dstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cSub)
        Dstack(istackPtr-1) = Dstack(istackPtr-1)-Dstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cMul)
        Dstack(istackPtr-1) = Dstack(istackPtr-1)*Dstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cDiv)
        IF (Dstack(istackPtr) .eq. 0.0_DP) THEN
          EvalErrType = 1
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr-1) = Dstack(istackPtr-1)/Dstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cMod)
        IF (Dstack(istackPtr) .eq. 0.0_DP) THEN
          EvalErrType = 1
          dresult     = SYS_INFINITY_DP
        END IF
        Dstack(istackPtr-1) = MOD(Dstack(istackPtr-1), Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cPow)
        Dstack(istackPtr-1) = Dstack(istackPtr-1)**Dstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cEqual)
        Dstack(istackPtr-1) = LogcToDble(Dstack(istackPtr-1) .eq. Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cNEqual)
        Dstack(istackPtr-1) = LogcToDble(Dstack(istackPtr-1) .ne. Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cLess)
        Dstack(istackPtr-1) = LogcToDble(Dstack(istackPtr-1) .lt. Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cLessOrEq)
        Dstack(istackPtr-1) = LogcToDble(Dstack(istackPtr-1) .le. Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cGreater)
        Dstack(istackPtr-1) = LogcToDble(Dstack(istackPtr-1) .gt. Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cGreaterOrEq)
        Dstack(istackPtr-1) = LogcToDble(Dstack(istackPtr-1) .ge. Dstack(istackPtr))
        istackPtr = istackPtr-1

      CASE (cAnd)
        Dstack(istackPtr-1) = LogcToDble(DbleToLogc(Dstack(istackPtr-1)) .AND. &
                                         DbleToLogc(Dstack(istackPtr)) )
        istackPtr = istackPtr-1

      CASE (cOr)
        Dstack(istackPtr-1) = LogcToDble(DbleToLogc(Dstack(istackPtr-1)) .OR. &
                                         DbleToLogc(Dstack(istackPtr)) )
        istackPtr = istackPtr-1

      CASE (cNot)
        Dstack(istackPtr) = LogcToDble( .NOT. DbleToLogc(Dstack(istackPtr)) )

      !------------------------------------------------------------
      ! Degrees-radians conversion
      !------------------------------------------------------------
      CASE (cDeg)
        Dstack(istackPtr) = RadToDeg(Dstack(istackPtr))

      CASE (cRad)
        Dstack(istackPtr) = DegToRad(Dstack(istackPtr))

      CASE default
        istackPtr = istackPtr+1
        Dstack(istackPtr) = DValue(rcomp%IbyteCode(iinstPtr)-VarBegin+1)
      END SELECT
      IF (EvalErrType .ne. 0) RETURN
    END DO

    EvalErrType = 0
    dresult = Dstack(istackPtr)

  END SUBROUTINE evalFunctionScDble

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE evalFunctionScCmpl (rcomp, Zstack, Zvalue, EvalErrType, zresult)

!<description>
    ! Evaluate bytecode for the values passed in array Val(:).
    ! Assume that intermediate values and/or the final result
    ! zresult is complex-valued.
!</description>

!<input>
    ! Component of function parser
    TYPE(t_fparserComponent), INTENT(in) :: rcomp

    ! Variable values
    COMPLEX(DP), DIMENSION(:), INTENT(in) :: Zvalue
!</input>

!<inputoutput>
    ! Stack memory
    COMPLEX(DP), DIMENSION(:), INTENT(inout) :: Zstack
!</inputoutput>

!<output>
    ! Error code for function evaluation
    INTEGER, INTENT(out) :: EvalErrType

    ! Evaluated function
    COMPLEX(DP), INTENT(out) :: zresult
!</output>
!</subroutine>

    ! local variables
    INTEGER  :: iinstPtr,istackPtr,idataPtr
    INTEGER  :: ijumpAddr,iimmedAddr

    ! Initialization
    idataPtr  = 1
    istackPtr = 0
    iinstPtr  = 0

    ! Initialize error type
    EvalErrType = 0

    ! Repeat until complete bytecode has been processed
    DO WHILE(iinstPtr .lt. rcomp%ibytecodeSize)
      iinstPtr = iinstPtr+1

      ! What kind of bytecode are we?
      SELECT CASE (rcomp%IbyteCode(iinstPtr))
        !------------------------------------------------------------
        ! Functions
        !------------------------------------------------------------
      CASE (cAbs)
        Zstack(istackPtr) = ABS(Zstack(istackPtr))

      CASE (cAcos)
        Zstack(istackPtr) = zacos(Zstack(istackPtr))

      CASE (cAsin)
        Zstack(istackPtr) = zasin(Zstack(istackPtr))

      CASE (cAtan)
        Zstack(istackPtr) = zatan(Zstack(istackPtr))

      CASE (cAtan2)
        IF (IsComplex(Zstack(istackPtr-1)) .OR.&
            IsComplex(Zstack(istackPtr))) THEN
          ! ATAN2 is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = CMPLX(ATAN2(REAL(Zstack(istackPtr-1)),&
                                          REAL(zstack(istackPtr))),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cAcot)
        Zstack(istackPtr) = zatan(1.0_DP/Zstack(istackPtr))

      CASE (cAcoth)
        Zstack(istackPtr) = zatanh(1.0_DP/Zstack(istackPtr))

      CASE (cAcosh)
        Zstack(istackPtr) = zacosh(Zstack(istackPtr))

      CASE (cAcsc)
        Zstack(istackPtr) = zasin(1.0_DP/Zstack(istackPtr))

      CASE (cAcsch)
        Zstack(istackPtr) = zasinh(1.0_DP/Zstack(istackPtr))

      CASE (cAsec)
        Zstack(istackPtr) = zacos(Zstack(istackPtr))

      CASE (cAsech)
        Zstack(istackPtr) = zacosh(1.0_DP/Zstack(istackPtr))

      CASE (cAsinh)
        Zstack(istackPtr) = zasinh(Zstack(istackPtr))

      CASE (cAtanh)
        Zstack(istackPtr) = zatanh(Zstack(istackPtr))

      CASE (cAnint)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! ANINT are not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr) = CMPLX(ANINT(AIMAG(zstack(istackPtr))),0.0_DP,DP)

      CASE (cAint)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! AINT are not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr) = CMPLX(AINT(AIMAG(zstack(istackPtr))),0.0_DP,DP)

      CASE (cCeil)
        Zstack(istackPtr) = CMPLX(CEILING(REAL(Zstack(istackPtr))),&
                                  CEILING(AIMAG(Zstack(istackPtr))),DP)

      CASE (Ccmplx)
        IF (IsComplex(Zstack(istackPtr)) .OR.&
            IsComplex(Zstack(istackPtr-1))) THEN
            ! CMPLX cannot be applied to a complex value
            EvalErrType = 2
            zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = CMPLX(REAL(Zstack(istackPtr-1)),&
                                    REAL(zstack(istackPtr)),DP)
        istackPtr = istackPtr-1

      CASE (Cconj)
        Zstack(istackPtr) = CONJG(Zstack(istackPtr))

      CASE (CCOS)
        Zstack(istackPtr) = COS(Zstack(istackPtr))

      CASE (cCosh)
        Zstack(istackPtr) = zcosh(Zstack(istackPtr))

      CASE (cCot)
        Zstack(istackPtr) = 1.0_DP/ztan(Zstack(istackPtr))

      CASE (cCoth)
        Zstack(istackPtr) = 1.0_DP/ztanh(Zstack(istackPtr))

      CASE (cCsc)
        Zstack(istackPtr) = 1.0_DP/SIN(Zstack(istackPtr))

      CASE (cCsch)
        Zstack(istackPtr) = 1.0_DP/zsinh(Zstack(istackPtr))

      CASE (CEXP)
        Zstack(istackPtr) = EXP(Zstack(istackPtr))

      CASE (cFloor)
        Zstack(istackPtr) = CMPLX(FLOOR(REAL(Zstack(istackPtr))),&
                                  FLOOR(AIMAG(Zstack(istackPtr))),DP)

      CASE (cIf)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! IF is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF

        iinstPtr = iinstPtr+1;   ijumpAddr  = rcomp%IbyteCode(iinstPtr)
        iinstPtr = iinstPtr+1;   iimmedAddr = rcomp%IbyteCode(iinstPtr)
        IF (.NOT.DbleToLogc(REAL(Zstack(istackPtr)))) THEN
          iinstPtr = ijumpAddr
          idataPtr = iimmedAddr
        END IF
        istackPtr = istackPtr-1

      CASE (cImag)
        Zstack(istackPtr) = CMPLX(AIMAG(Zstack(istackPtr)),0.0_DP,DP)

      CASE (CLOG)
        Zstack(istackPtr) = LOG(Zstack(istackPtr))

      CASE (cLog10)
        Zstack(istackPtr) = LOG(Zstack(istackPtr))/LOG(10.0_DP)

      CASE (cMax)
        IF (ABS(Zstack(istackPtr)) .gt. ABS(Zstack(istackPtr-1))) THEN
          Zstack(istackPtr-1) = Zstack(istackPtr)
        END IF
        istackPtr = istackPtr-1

      CASE (cMin)
        IF (ABS(Zstack(istackPtr)) .lt. ABS(Zstack(istackPtr-1))) THEN
          Zstack(istackPtr-1) = Zstack(istackPtr)
        END IF
        istackPtr = istackPtr-1

      CASE (cReal)
        Zstack(istackPtr) = CMPLX(REAL(Zstack(istackPtr)),0.0_DP,DP)

      CASE (cRrand)
        ! RRAND is not supported in complex-valued case
        EvalErrType = 2
        zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)

      CASE (cSec)
        Zstack(istackPtr) = 1.0_DP/COS(Zstack(istackPtr))

      CASE (cSech)
        Zstack(istackPtr) = 1.0_DP/zcosh(Zstack(istackPtr))

      CASE (cSign)
        Zstack(istackPtr) = Zstack(istackPtr)/ABS(Zstack(istackPtr))

      CASE (CSIN)
        Zstack(istackPtr) = SIN(Zstack(istackPtr))

      CASE(cSinh)
        Zstack(istackPtr) = zsinh(Zstack(istackPtr))

      CASE(CSQRT)
        Zstack(istackPtr) = SQRT(Zstack(istackPtr))

      CASE (cTan)
        Zstack(istackPtr) = ztan(Zstack(istackPtr))

      CASE (cTanh)
        Zstack(istackPtr) = ztanh(Zstack(istackPtr))

      !------------------------------------------------------------
      ! Misc
      !------------------------------------------------------------
      CASE (cImmed)
        istackPtr         = istackPtr+1
        Zstack(istackPtr) = rcomp%Zimmed(idataPtr)
        idataPtr          = idataPtr+1

      CASE (cJump)
        idataPtr = rcomp%IbyteCode(iinstPtr+2)
        iinstPtr = rcomp%IbyteCode(iinstPtr+1)

      !------------------------------------------------------------
      ! Operators
      !------------------------------------------------------------
      CASE (cNeg)
        Zstack(istackPtr) = -Zstack(istackPtr)

      CASE (cAdd)
        Zstack(istackPtr-1) = Zstack(istackPtr-1)+Zstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cSub)
        Zstack(istackPtr-1) = Zstack(istackPtr-1)-Zstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cMul)
        Zstack(istackPtr-1) = Zstack(istackPtr-1)*Zstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cDiv)
        IF (Zstack(istackPtr) .eq. CMPLX(0.0_DP,0.0_DP,DP)) THEN
          EvalErrType = 1
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = Zstack(istackPtr-1)/Zstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cMod)
        IF (IsComplex(Zstack(istackPtr)) .OR.&
            IsComplex(Zstack(istackPtr-1))) THEN
          ! MOD is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF

        IF (AIMAG(Zstack(istackPtr)) .eq. 0.0_DP) THEN
          EvalErrType = 1
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = CMPLX(MOD(AIMAG(Zstack(istackPtr-1)),&
                                        AIMAG(Zstack(istackPtr))),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cPow)
        Zstack(istackPtr-1) = Zstack(istackPtr-1)**Zstack(istackPtr)
        istackPtr = istackPtr-1

      CASE (cEqual)
        Zstack(istackPtr-1) = CMPLX(LogcToDble((REAL(Zstack(istackPtr-1)) .eq.&
                                                REAL(Zstack(istackPtr))) .AND.&
                                              (AIMAG(Zstack(istackPtr-1)) .eq.&
                                               AIMAG(Zstack(istackPtr)))),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cNEqual)
        Zstack(istackPtr-1) = CMPLX(LogcToDble((REAL(Zstack(istackPtr-1)) .ne.&
                                                REAL(Zstack(istackPtr)))  .OR.&
                                              (AIMAG(Zstack(istackPtr-1)) .ne.&
                                               AIMAG(Zstack(istackPtr)))),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cLess)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! LESS is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(istackPtr-1)) .lt.&
                                               REAL(Zstack(istackPtr))),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cLessOrEq)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! LESSOREQUAL is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(istackPtr-1)) .le.&
                                               REAL(Zstack(istackPtr))),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cGreater)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! GREATER is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(istackPtr-1)) .gt.&
                                               REAL(Zstack(istackPtr))),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cGreaterOrEq)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! GREATEROREQUAL is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(istackPtr-1)) .ge.&
                                               REAL(Zstack(istackPtr))),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cAnd)
        IF (IsComplex(Zstack(istackPtr)) .OR.&
            IsComplex(Zstack(istackPtr-1))) THEN
          ! AND is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = CMPLX(LogcToDble(CmplToLogc(Zstack(istackPtr-1)) .AND. &
                                               CmplToLogc(Zstack(istackPtr)) ),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cOr)
        IF (IsComplex(Zstack(istackPtr)) .OR.&
            IsComplex(Zstack(istackPtr-1))) THEN
          ! OR is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr-1) = CMPLX(LogcToDble(CmplToLogc(Zstack(istackPtr-1)) .OR. &
                                               CmplToLogc(Zstack(istackPtr)) ),0.0_DP,DP)
        istackPtr = istackPtr-1

      CASE (cNot)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! NOT is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr) = CMPLX(LogcToDble( .NOT. CmplToLogc(Zstack(istackPtr)) ),0.0_DP,DP)

      !------------------------------------------------------------
      ! Degrees-radians conversion
      !------------------------------------------------------------
      CASE (cDeg)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! DEG is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr) = CMPLX(RadToDeg(REAL(Zstack(istackPtr))),0.0_DP,DP)

      CASE (cRad)
        IF (IsComplex(Zstack(istackPtr))) THEN
          ! RAD is not supported in complex-valued case
          EvalErrType = 2
          zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
        END IF
        Zstack(istackPtr) = CMPLX(DegToRad(REAL(Zstack(istackPtr))),0.0_DP,DP)

      CASE default
        istackPtr = istackPtr+1
        Zstack(istackPtr) = ZValue(rcomp%IbyteCode(iinstPtr)-VarBegin+1)
      END SELECT
      IF (EvalErrType .ne. 0) RETURN
    END DO

    EvalErrType = 0
    zresult = Zstack(istackPtr)

  END SUBROUTINE evalFunctionScCmpl

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE evalFunctionBlDble (rcomp, iblockSize, Dstack, DvalueBlock, idim,&
                                 EvalErrType, Dresult, DvalueScalar)

!<description>
    ! Evaluate bytecode for an array of values passed in DvalueBlock(:,:).
!</description>

!<input>
    ! Component of function parser
    TYPE(t_fparserComponent), INTENT(in) :: rcomp

    ! Variable values
    REAL(DP), DIMENSION(:,:), INTENT(in) :: DvalueBlock

    ! Size of the vector block
    INTEGER, INTENT(in) :: iblockSize

    ! Orientation of the stored values
    ! idim =1 : DvalueBlock is organised as (x1:xN),(y1:yN),...
    ! idim =2 : DvalueBlock is organised as (x1,y1),(x2,y2),...,(xN,yN)
    INTEGER, INTENT(in) :: idim

    ! Vector of scalar variable values
    REAL(DP), DIMENSION(:), INTENT(in), OPTIONAL :: DvalueScalar
!</input>

!<inputoutput>
    ! Stack memory
    REAL(DP), DIMENSION(:,:), INTENT(inout) :: Dstack
!</inputoutput>

!<output>
    ! Error code for function evaluation
    INTEGER, INTENT(out) :: EvalErrType

    ! Evaluated function
    REAL(DP), DIMENSION(:), INTENT(out) :: Dresult
!</output>
!</subroutine>

    ! local variables
    INTEGER  :: iinstPtr,idataPtr,istackPtr,iblock,istartValueScalar,iVariable
    REAL(DP) :: daux
    INTEGER :: iaux
    INTEGER, DIMENSION(:), ALLOCATABLE :: p_Irandom1,p_Irandom2

    ! Initialization
    idataPtr  = 1
    istackPtr = 0
    iinstPtr  = 0

    ! Initialize error type
    EvalErrType = 0

    ! This is tricky. istartValueScalar indicates the number of the first
    ! variable which is passed as scalar. Hence, if the optional parameter
    ! DvalueScalar is missing, then istartValueScalar pointers to SIZE(DvalueBlock)+1.
    ! Obviously, no variable beyond this value is addressed.
    istartValueScalar = SIZE(DvalueBlock,3-idim)+1

    ! Repeat until complete bytecode has been processed
    DO WHILE(iinstPtr .lt. rcomp%ibytecodeSize)
      iinstPtr = iinstPtr+1

      SELECT CASE (rcomp%IbyteCode(iinstPtr))
        !------------------------------------------------------------
        ! Functions
        !------------------------------------------------------------
      CASE (cAbs)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = ABS(Dstack(iblock,istackPtr))
        END DO

      CASE (cAcos)
        DO iblock = 1, iblockSize
          IF ((Dstack(iblock,istackPtr) .lt. -1._DP) .OR.&
              (Dstack(iblock,istackPtr) .gt.  1._DP)) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          ELSE
            Dstack(iblock,istackPtr) = ACOS(Dstack(iblock,istackPtr))
          END IF
        END DO

      CASE (cAsin)
        DO iblock = 1, iblockSize
          IF ((Dstack(iblock,istackPtr) .lt. -1._DP) .OR.&
              (Dstack(iblock,istackPtr) .gt.  1._DP)) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          ELSE
            Dstack(iblock,istackPtr) = ASIN(Dstack(iblock,istackPtr))
          END IF
        END DO

      CASE (cAtan)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = ATAN(Dstack(iblock,istackPtr))
        END DO

      CASE (cAtan2)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = ATAN2(Dstack(iblock,istackPtr -1),&
                                               Dstack(iblock,istackPtr))
        END DO
        istackPtr = istackPtr-1

      CASE (cAcot)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = ATAN(1.0_DP/Dstack(iblock,istackPtr))
         END DO

       CASE (cAcoth)
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = datanh(1.0_DP/Dstack(iblock,istackPtr))
        END DO

      CASE (cAcosh)
        DO iblock = 1, iblockSize
          IF (Dstack(iblock,istackPtr) .lt. 1.0_DP) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          ELSE
            Dstack(iblock,istackPtr) = dacosh(Dstack(iblock,istackPtr))
          END IF
        END DO

      CASE (cAcsc)
        DO iblock = 1, iblockSize
          IF (Dstack(iblock,istackPtr) .gt. -SYS_PI/2.0_DP .AND.&
              Dstack(iblock,istackPtr) .lt.  SYS_PI/2.0_DP) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          END IF
          Dstack(iblock,istackPtr) = ASIN(1.0_DP/Dstack(iblock,istackPtr))
        END DO

      CASE (cAcsch)
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = dasinh(1.0_DP/Dstack(iblock,istackPtr))
        END DO

      CASE (cAsec)
        DO iblock = 1, iblockSize
          IF ((Dstack(iblock,istackPtr) .gt. -1.0_DP) .AND. &
              (Dstack(iblock,istackPtr) .lt.  1.0_DP)) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          END IF
          Dstack(iblock,istackPtr) = ACOS(Dstack(iblock,istackPtr))
        END DO

      CASE (cAsech)
        DO iblock = 1, iblockSize
          IF ((Dstack(iblock,istackPtr) .lt. 0.0_DP) .OR. &
              (Dstack(iblock,istackPtr) .gt. 1.0_DP)) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          END IF
          Dstack(iblock,istackPtr) = dacosh(1.0_DP/Dstack(iblock,istackPtr))
        END DO

      CASE (cAsinh)
        DO iblock = 1, iblockSize
          IF ( Dstack(iblock,istackPtr) .le. 0._DP) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          END IF
          Dstack(iblock,istackPtr) = dasinh(Dstack(iblock,istackPtr))
        END DO

      CASE (cAtanh)
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = datanh(Dstack(iblock,istackPtr))
        END DO

      CASE (cAnint)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = ANINT(Dstack(iblock,istackPtr))
        END DO

      CASE (cAint)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = AINT(Dstack(iblock,istackPtr))
        END DO

      CASE (cCeil)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = CEILING(Dstack(iblock,istackPtr))
        END DO

      CASE (Ccmplx)
        ! Illegal operation
        EvalErrType = 3
        Dresult     = SYS_INFINITY_DP

      CASE (Cconj)
        ! Nothing needs to be done for the real-valued case

      CASE (CCOS)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = COS(Dstack(iblock,istackPtr))
        END DO

      CASE (cCosh)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = COSH(Dstack(iblock,istackPtr))
        END DO

      CASE (cCot)
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = 1.0_DP/TAN(Dstack(iblock,istackPtr))
        END DO

      CASE (cCoth)
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = 1.0_DP/TANH(Dstack(iblock,istackPtr))
        END DO

      CASE (cCsc)
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = 1.0_DP/SIN(Dstack(iblock,istackPtr))
        END DO

      CASE (cCsch)
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = 1.0_DP/SINH(Dstack(iblock,istackPtr))
        END DO

      CASE (CEXP)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = EXP(Dstack(iblock,istackPtr))
        END DO

      CASE (cFloor)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = FLOOR(Dstack(iblock,istackPtr))
        END DO

      CASE (cIf)
        ! IF-THEN-ELSE cannot be vectorised which should be noted during
        ! bytecode compilation. If we reach this point, then something
        ! went wrong before.
        CALL output_line('IF-THEN-ELSE cannot be vectorised!',&
            OU_CLASS_ERROR, OU_MODE_STD,'evalFunctionBlDble')
        CALL sys_halt()

      CASE (CLOG)
        DO iblock = 1, iblockSize
          IF (Dstack(iblock,istackPtr) .le. 0._DP) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          ELSE
            Dstack(iblock,istackPtr) = LOG(Dstack(iblock,istackPtr))
          END IF
        END DO

      CASE (cLog10)
        DO iblock = 1, iblockSize
          IF (Dstack(iblock,istackPtr) .le. 0._DP) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          ELSE
            Dstack(iblock,istackPtr) = LOG10(Dstack(iblock,istackPtr))
          END IF
        END DO

      CASE (cMax)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = MAX(Dstack(iblock,istackPtr-1),&
                                             Dstack(iblock,istackPtr))
        END DO
        istackPtr = istackPtr-1

      CASE (cMin)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = MIN(Dstack(iblock,istackPtr-1),&
                                             Dstack(iblock,istackPtr))
        END DO
        istackPtr = istackPtr-1

      CASE (cRrand)
        CALL RANDOM_SEED (SIZE=iaux)
        ALLOCATE (p_Irandom1(iaux))
        ALLOCATE (p_Irandom2(iaux))
        CALL RANDOM_SEED (get=p_Irandom1)

        DO iblock = 1, iblockSize
          p_Irandom2(:) = 0
          p_Irandom2(1) = INT(Dstack(iblock,istackPtr-1))
          CALL RANDOM_SEED (put=p_Irandom2)
          daux = 0.0_DP
          DO iaux=1,MAX(1,INT(Dstack(iblock,istackPtr)))
            CALL RANDOM_NUMBER (daux)
          END DO
          Dstack(iblock,istackPtr-1) = daux
        END DO

        CALL RANDOM_SEED (put=p_Irandom1)
        DEALLOCATE(p_Irandom1,p_Irandom2)

        istackPtr = istackPtr-1

      CASE (cSec)
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = 1._DP/COS(Dstack(iblock,istackPtr))
        END DO

      CASE (cSech)
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = 1._DP/COSH(Dstack(iblock,istackPtr))
        END DO

      CASE (cSign)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = SIGN(1._DP,Dstack(iblock,istackPtr))
        END DO

      CASE (CSIN)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = SIN(Dstack(iblock,istackPtr))
        END DO

      CASE(cSinh)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = SINH(Dstack(iblock,istackPtr))
        END DO

      CASE(CSQRT)
        DO iblock = 1, iblockSize
          IF (Dstack(iblock,istackPtr) .lt. 0._DP) THEN
            EvalErrType = 3
            Dresult(iblock) = SYS_INFINITY_DP
          ELSE
            Dstack(iblock,istackPtr) = SQRT(Dstack(iblock,istackPtr))
          END IF
        END DO

      CASE (cTan)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = TAN(Dstack(iblock,istackPtr))
        END DO

      CASE (cTanh)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = TANH(Dstack(iblock,istackPtr))
        END DO

        !------------------------------------------------------------
        ! Misc
        !------------------------------------------------------------
      CASE (cImmed)
        istackPtr = istackPtr+1
        DO iblock = 1, iblockSize
          Dstack(iblock,istackPtr) = rcomp%Dimmed(idataPtr)
        END DO
        idataPtr = idataPtr+1

      CASE (cJump)
        idataPtr = rcomp%IbyteCode(iinstPtr+2)
        iinstPtr = rcomp%IbyteCode(iinstPtr+1)

        !------------------------------------------------------------
        ! Operators
        !------------------------------------------------------------
      CASE (cNeg)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = -Dstack(iblock,istackPtr)
        END DO

      CASE (cAdd)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = Dstack(iblock,istackPtr-1)+&
                                         Dstack(iblock,istackPtr)
        END DO
        istackPtr = istackPtr-1

      CASE (cSub)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = Dstack(iblock,istackPtr-1)-&
                                         Dstack(iblock,istackPtr)
        END DO
        istackPtr = istackPtr-1

      CASE (cMul)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = Dstack(iblock,istackPtr-1)*&
                                         Dstack(iblock,istackPtr)
        END DO
        istackPtr = istackPtr-1

      CASE (cDiv)
        DO iblock = 1, iblockSize
          IF (Dstack(iblock,istackPtr) .eq. 0._DP) THEN
            EvalErrType = 1
            Dresult(iblock) = SYS_INFINITY_DP
          ELSE
            Dstack(iblock,istackPtr-1) = Dstack(iblock,istackPtr-1)/&
                                          Dstack(iblock,istackPtr)
          END IF
        END DO
        istackPtr = istackPtr-1

      CASE (cMod)
        DO iblock = 1, iblockSize
          IF (Dstack(iblock,istackPtr) .eq. 0._DP) THEN
            EvalErrType = 1
            Dresult(iblock) = SYS_INFINITY_DP
          ELSE
            Dstack(iblock,istackPtr-1) = MOD(Dstack(iblock,istackPtr-1),&
                                              Dstack(iblock,istackPtr))
          END IF
        END DO
        istackPtr = istackPtr-1

      CASE (cPow)
        DO  iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = Dstack(iblock,istackPtr-1)**Dstack(iblock,istackPtr)
        END DO
        istackPtr = istackPtr-1

      CASE (cEqual)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = LogcToDble(Dstack(iblock,istackPtr-1) .eq.&
                                                    Dstack(iblock,istackPtr))
        END DO
        istackPtr = istackPtr-1

      CASE (cNEqual)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = LogcToDble(Dstack(iblock,istackPtr-1) .ne.&
                                                    Dstack(iblock,istackPtr))
        END DO
        istackPtr = istackPtr-1

      CASE (cLess)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = LogcToDble(Dstack(iblock,istackPtr-1) .lt.&
                                                    Dstack(iblock,istackPtr))
        END DO
        istackPtr = istackPtr-1

      CASE (cLessOrEq)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = LogcToDble(Dstack(iblock,istackPtr-1) .le.&
                                                    Dstack(iblock,istackPtr))
        END DO
        istackPtr = istackPtr-1

      CASE (cGreater)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = LogcToDble(Dstack(iblock,istackPtr-1) .gt.&
                                                    Dstack(iblock,istackPtr))
        END DO
        istackPtr = istackPtr-1

      CASE (cGreaterOrEq)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = LogcToDble(Dstack(iblock,istackPtr-1) .ge.&
                                                    Dstack(iblock,istackPtr))
        END DO
        istackPtr = istackPtr-1

      CASE (cAnd)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = LogcToDble(DbleToLogc(Dstack(iblock,istackPtr-1)) .AND. &
                                                    DbleToLogc(Dstack(iblock,istackPtr)) )
        END DO
        istackPtr = istackPtr-1

      CASE (cOr)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr-1) = LogcToDble(DbleToLogc(Dstack(iblock,istackPtr-1)) .OR. &
                                                    DbleToLogc(Dstack(iblock,istackPtr)) )
        END DO
        istackPtr = istackPtr-1

      CASE (cNot)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = LogcToDble( .NOT. DbleToLogc(Dstack(iblock,istackPtr)) )
        END DO

        !------------------------------------------------------------
        ! Degrees-radians conversion
        !------------------------------------------------------------
      CASE (cDeg)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = RadToDeg(Dstack(iblock,istackPtr))
        END DO


      CASE (cRad)
        DO iblock = 1, iblockSize
           Dstack(iblock,istackPtr) = DegToRad(Dstack(iblock,istackPtr))
        END DO


      CASE default
        istackPtr  = istackPtr+1
        iVariable = rcomp%IbyteCode(iinstPtr)-VarBegin+1

        ! Do we have to process one of the scalar variables of one of the block variables
        IF (iVariable .ge. istartValueScalar) THEN
          DO iblock = 1, iblockSize
             Dstack(iblock,istackPtr) = DvalueScalar(iVariable-istartValueScalar+1)
          END DO
        ELSE
          IF (idim .eq. 1) THEN
            DO iblock = 1, iblockSize
               Dstack(iblock,istackPtr) = DvalueBlock(iblock,iVariable)
            END DO
          ELSE
            DO iblock = 1, iblockSize
               Dstack(iblock,istackPtr) = DvalueBlock(iVariable, iblock)
            END DO
          END IF
        END IF
      END SELECT
      IF (EvalErrType .ne. 0) RETURN
    END DO

    DO iblock = 1, iblockSize
       Dresult(iblock) = Dstack(iblock,istackPtr)
    END DO

  END SUBROUTINE evalFunctionBlDble

  ! *****************************************************************************

!<subroutine>

  SUBROUTINE evalFunctionBlCmpl (rcomp, iblockSize, Zstack, ZvalueBlock, idim,&
                                 EvalErrType, Zresult, ZvalueScalar)

!<description>
    ! Evaluate bytecode for an array of values passed in ZvalueBlock(:,:).
!</description>

!<input>
    ! Component of function parser
    TYPE(t_fparserComponent), INTENT(in) :: rcomp

    ! Variable values
    COMPLEX(DP), DIMENSION(:,:), INTENT(in) :: ZvalueBlock

    ! Size of the vector block
    INTEGER, INTENT(in) :: iblockSize

    ! Orientation of the stored values
    ! idim =1 : ZvalueBlock is organised as (x1:xN),(y1:yN),...
    ! idim =2 : ZvalueBlock is organised as (x1,y1),(x2,y2),...,(xN,yN)
    INTEGER, INTENT(in) :: idim

    ! Vector of scalar variable values
    COMPLEX(DP), DIMENSION(:), INTENT(in), OPTIONAL :: ZvalueScalar
!</input>

!<inputoutput>
    ! Stack memory
    COMPLEX(DP), DIMENSION(:,:), INTENT(inout) :: Zstack
!</inputoutput>

!<output>
    ! Error code for function evaluation
    INTEGER, INTENT(out) :: EvalErrType

    ! Evaluated function
    COMPLEX(DP), DIMENSION(:), INTENT(out) :: Zresult
!</output>
!</subroutine>

    ! local variables
    INTEGER  :: iinstPtr,idataPtr,istackPtr,iblock,istartValueScalar,iVariable

    ! Initialization
    idataPtr  = 1
    istackPtr = 0
    iinstPtr  = 0

    ! Initialize error type
    EvalErrType = 0

    ! This is tricky. istartValueScalar indicates the number of the first
    ! variable which is passed as scalar. Hence, if the optional parameter
    ! DvalueScalar is missing, then istartValueScalar pointers to SIZE(DvalueBlock)+1.
    ! Obviously, no variable beyond this value is addressed.
    istartValueScalar = SIZE(ZvalueBlock,3-idim)+1

    ! Repeat until complete bytecode has been processed
    DO WHILE(iinstPtr .lt. rcomp%ibytecodeSize)
      iinstPtr = iinstPtr+1

      SELECT CASE (rcomp%IbyteCode(iinstPtr))
        !------------------------------------------------------------
        ! Functions
        !------------------------------------------------------------
      CASE (cAbs)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr) = ABS(Zstack(iblock,istackPtr))
        END DO

      CASE (cAcos)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zacos(Zstack(iblock,istackPtr))
        END DO

      CASE (cAsin)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zasin(Zstack(iblock,istackPtr))
        END DO

      CASE (cAtan)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zatan(Zstack(iblock,istackPtr))
        END DO

      CASE (cAtan2)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr-1)) .OR.&
              IsComplex(Zstack(iblock,istackPtr))) THEN
            ! ATAN2 is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr-1) = CMPLX(ATAN2(REAL(Zstack(iblock,istackPtr -1)),&
                                                    REAL(Zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cAcot)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr) = zatan(1.0_DP/Zstack(iblock,istackPtr))
         END DO

       CASE (cAcoth)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zatanh(1.0_DP/Zstack(iblock,istackPtr))
        END DO

      CASE (cAcosh)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zacosh(Zstack(iblock,istackPtr))
        END DO

      CASE (cAcsc)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zasin(1.0_DP/Zstack(iblock,istackPtr))
        END DO

      CASE (cAcsch)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zasinh(1.0_DP/Zstack(iblock,istackPtr))
        END DO

      CASE (cAsec)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zacos(Zstack(iblock,istackPtr))
        END DO

      CASE (cAsech)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zacosh(1.0_DP/Zstack(iblock,istackPtr))
        END DO

      CASE (cAsinh)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zasinh(Zstack(iblock,istackPtr))
        END DO

      CASE (cAtanh)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zatanh(Zstack(iblock,istackPtr))
        END DO

      CASE (cAnint)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr))) THEN
            ! ANINT are not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr) = CMPLX(AINT(AIMAG(zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO

      CASE (cAint)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr))) THEN
            ! AINT are not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr) = CMPLX(AINT(AIMAG(zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO

      CASE (cCeil)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = CMPLX(CEILING(REAL(Zstack(iblock,istackPtr))),&
                                           CEILING(AIMAG(Zstack(iblock,istackPtr))),DP)
        END DO

      CASE (Ccmplx)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr)) .OR.&
              IsComplex(Zstack(iblock,istackPtr-1))) THEN
            ! CMPLX cannot be applied to a complex value
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr-1) = CMPLX(REAL(Zstack(iblock,istackPtr-1)),&
                                             REAL(zstack(iblock,istackPtr)),DP)
        END DO
        istackPtr = istackPtr-1

      CASE (Cconj)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = CONJG(Zstack(iblock,istackPtr))
        END DO

      CASE (CCOS)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = COS(Zstack(iblock,istackPtr))
        END DO

      CASE (cCosh)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = zcosh(Zstack(iblock,istackPtr))
        END DO

      CASE (cCot)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = 1.0_DP/ztan(Zstack(iblock,istackPtr))
        END DO

      CASE (cCoth)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = 1.0_DP/ztanh(Zstack(iblock,istackPtr))
        END DO

      CASE (cCsc)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = 1.0_DP/SIN(Zstack(iblock,istackPtr))
        END DO

      CASE (cCsch)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = 1.0_DP/zsinh(Zstack(iblock,istackPtr))
        END DO

      CASE (CEXP)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = EXP(Zstack(iblock,istackPtr))
        END DO

      CASE (cFloor)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = CMPLX(FLOOR(REAL(Zstack(iblock,istackPtr))),&
                                           FLOOR(AIMAG(Zstack(iblock,istackPtr))),DP)
        END DO

      CASE (cIf)
        ! IF-THEN-ELSE cannot be vectorised which should be noted during
        ! bytecode compilation. If we reach this point, then something
        ! went wrong before.
        CALL output_line('IF-THEN-ELSE cannot be vectorised!',&
            OU_CLASS_ERROR, OU_MODE_STD,'evalFunctionBlDble')
        CALL sys_halt()

      CASE (cImag)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = CMPLX(AIMAG(Zstack(iblock,istackPtr)),0.0_DP,DP)
        END DO

      CASE (CLOG)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = LOG(Zstack(iblock,istackPtr))
        END DO

      CASE (cLog10)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = LOG(Zstack(iblock,istackPtr))/LOG(10.0_DP)
        END DO

      CASE (cMax)
        DO iblock = 1, iblockSize
          IF (ABS(Zstack(iblock,istackPtr)) .gt. ABS(Zstack(iblock,istackPtr-1))) THEN
            Zstack(iblock,istackPtr-1) = Zstack(iblock,istackPtr)
          END IF
        END DO
        istackPtr = istackPtr-1

      CASE (cMin)
        DO iblock = 1, iblockSize
          IF (ABS(Zstack(iblock,istackPtr)) .lt. ABS(Zstack(iblock,istackPtr-1))) THEN
            Zstack(iblock,istackPtr-1) = Zstack(iblock,istackPtr)
          END IF
        END DO
        istackPtr = istackPtr-1

      CASE (cReal)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = CMPLX(REAL(Zstack(iblock,istackPtr)),0.0_DP,DP)
        END DO

      CASE (cRrand)
        ! RRAND is not supported in complex-valued case
        EvalErrType = 2
        Zresult     = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)

      CASE (cSec)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = 1._DP/COS(Zstack(iblock,istackPtr))
        END DO

      CASE (cSech)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = 1._DP/zcosh(Zstack(iblock,istackPtr))
        END DO

      CASE (cSign)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr) = Zstack(iblock,istackPtr)/ABS(Zstack(iblock,istackPtr))
        END DO

      CASE (CSIN)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr) = SIN(Zstack(iblock,istackPtr))
        END DO

      CASE(cSinh)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr) = zsinh(Zstack(iblock,istackPtr))
        END DO

      CASE(CSQRT)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = SQRT(Zstack(iblock,istackPtr))
        END DO

      CASE (cTan)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr) = ztan(Zstack(iblock,istackPtr))
        END DO

      CASE (cTanh)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr) = ztanh(Zstack(iblock,istackPtr))
        END DO

        !------------------------------------------------------------
        ! Misc
        !------------------------------------------------------------
      CASE (cImmed)
        istackPtr = istackPtr+1
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr) = rcomp%Dimmed(idataPtr)
        END DO
        idataPtr = idataPtr+1

      CASE (cJump)
        idataPtr = rcomp%IbyteCode(iinstPtr+2)
        iinstPtr = rcomp%IbyteCode(iinstPtr+1)

        !------------------------------------------------------------
        ! Operators
        !------------------------------------------------------------
      CASE (cNeg)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr) = -Zstack(iblock,istackPtr)
        END DO

      CASE (cAdd)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr-1) = Zstack(iblock,istackPtr-1)+&
                                         Zstack(iblock,istackPtr)
        END DO
        istackPtr = istackPtr-1

      CASE (cSub)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr-1) = Zstack(iblock,istackPtr-1)-&
                                         Zstack(iblock,istackPtr)
        END DO
        istackPtr = istackPtr-1

      CASE (cMul)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr-1) = Zstack(iblock,istackPtr-1)*&
                                         Zstack(iblock,istackPtr)
        END DO
        istackPtr = istackPtr-1

      CASE (cDiv)
        DO iblock = 1, iblockSize
          IF (Zstack(iblock,istackPtr) .eq. CMPLX(0.0_DP,0.0_DP,DP)) THEN
            EvalErrType = 1
            Zresult(iblock) = SYS_INFINITY_DP
          ELSE
            Zstack(iblock,istackPtr-1) = Zstack(iblock,istackPtr-1)/&
                                          Zstack(iblock,istackPtr)
          END IF
        END DO
        istackPtr = istackPtr-1

      CASE (cMod)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr)) .OR.&
              IsComplex(Zstack(iblock,istackPtr-1))) THEN
            EvalErrType = 1
            Zresult(iblock) = SYS_INFINITY_DP
          ELSE IF (AIMAG(Zstack(iblock,istackPtr)) .eq. 0.0_DP) THEN
            EvalErrType = 1
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr-1) = CMPLX(MOD(AIMAG(Zstack(iblock,istackPtr-1)),&
                                                 AIMAG(Zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cPow)
        DO  iblock = 1, iblockSize
           Zstack(iblock,istackPtr-1) = Zstack(iblock,istackPtr-1)**Zstack(iblock,istackPtr)
        END DO
        istackPtr = istackPtr-1

      CASE (cEqual)
        DO iblock = 1, iblockSize
           Zstack(iblock,istackPtr-1) = CMPLX(LogcToDble((REAL(Zstack(iblock,istackPtr-1)) .eq.&
                                                          REAL(Zstack(iblock,istackPtr))) .AND.&
                                                        (AIMAG(Zstack(iblock,istackPtr-1)) .eq.&
                                                         AIMAG(Zstack(iblock,istackPtr)))),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cNEqual)
        DO iblock = 1, iblockSize
          Zstack(iblock,istackPtr-1) = CMPLX(LogcToDble((REAL(Zstack(iblock,istackPtr-1)) .ne.&
                                                         REAL(Zstack(iblock,istackPtr)))  .OR.&
                                                       (AIMAG(Zstack(iblock,istackPtr-1)) .ne.&
                                                        AIMAG(Zstack(iblock,istackPtr)))),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cLess)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr))) THEN
            ! LESS is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(iblock,istackPtr-1)) .lt.&
                                                        REAL(Zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cLessOrEq)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr))) THEN
            ! LESSOREQUAL is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(iblock,istackPtr-1)) .le.&
                                                        REAL(Zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cGreater)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr))) THEN
            ! GREATER is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(iblock,istackPtr-1)) .gt.&
                                                        REAL(Zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cGreaterOrEq)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr))) THEN
            ! GREATEROREQUAL is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(iblock,istackPtr-1)) .ge.&
                                                        REAL(Zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cAnd)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr)) .OR.&
              IsComplex(Zstack(iblock,istackPtr-1))) THEN
            ! AND is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr-1) = CMPLX(LogcToDble(CmplToLogc(Zstack(iblock,istackPtr-1)) .AND. &
                                                        CmplToLogc(Zstack(iblock,istackPtr)) ),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cOr)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr)) .OR.&
              IsComplex(Zstack(iblock,istackPtr-1))) THEN
            ! OR is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr-1) = CMPLX(LogcToDble(CmplToLogc(Zstack(iblock,istackPtr-1)) .OR. &
                                                        CmplToLogc(Zstack(iblock,istackPtr)) ),0.0_DP,DP)
        END DO
        istackPtr = istackPtr-1

      CASE (cNot)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr))) THEN
            ! NOT is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr) = CMPLX(RadToDeg(REAL(Zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO

        !------------------------------------------------------------
        ! Degrees-radians conversion
        !------------------------------------------------------------
      CASE (cDeg)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr))) THEN
            ! DEG is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr) = CMPLX(RadToDeg(REAL(Zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO

      CASE (cRad)
        DO iblock = 1, iblockSize
          IF (IsComplex(Zstack(iblock,istackPtr))) THEN
            ! RAD is not supported in complex-valued case
            EvalErrType = 2
            Zresult(iblock) = CMPLX(SYS_INFINITY_DP,SYS_INFINITY_DP,DP)
          END IF
          Zstack(iblock,istackPtr) = CMPLX(DegToRad(REAL(Zstack(iblock,istackPtr))),0.0_DP,DP)
        END DO

      CASE default
        istackPtr  = istackPtr+1
        iVariable = rcomp%IbyteCode(iinstPtr)-VarBegin+1

        ! Do we have to process one of the scalar variables of one of the block variables
        IF (iVariable .ge. istartValueScalar) THEN
          DO iblock = 1, iblockSize
             Zstack(iblock,istackPtr) = ZvalueScalar(iVariable-istartValueScalar+1)
          END DO
        ELSE
          IF (idim .eq. 1) THEN
            DO iblock = 1, iblockSize
               Zstack(iblock,istackPtr) = ZvalueBlock(iblock,iVariable)
            END DO
          ELSE
            DO iblock = 1, iblockSize
               Zstack(iblock,istackPtr) = ZvalueBlock(iVariable, iblock)
            END DO
          END IF
        END IF
      END SELECT
      IF (EvalErrType .ne. 0) RETURN
    END DO

    DO iblock = 1, iblockSize
       Zresult(iblock) = Zstack(iblock,istackPtr)
    END DO

  END SUBROUTINE evalFunctionBlCmpl

  ! *****************************************************************************
  ! *****************************************************************************
  ! *****************************************************************************

!<function>

  ELEMENTAL FUNCTION dacosh(dx)

!<description>
    ! Real-valued inverse hyperbolic cosine functions (available in Fortran 2008)
!</description>

!<input>
    REAL(DP), INTENT(in) :: dx
!</input>

!<result>
    REAL(DP) :: dacosh
!</result>
!</function>

    dacosh = LOG(dx+SQRT(dx**2-1.0_DP))

  END FUNCTION dacosh

  ! ***************************************************************************

  !<function>

  ELEMENTAL FUNCTION dasinh(dx)

!<description>
    ! Real-valued inverse hyperbolic sine functions (available in Fortran 2008)
!</description>

!<input>
    REAL(DP), INTENT(in) :: dx
!</input>

!<result>
    REAL(DP) :: dasinh
!</result>
!</function>

    dasinh = LOG(dx+SQRT(dx**2+1.0_DP))

  END FUNCTION dasinh

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION datanh(dx)

!<description>
    ! Real-valued inverse hyperbolic tangens functions (available in Fortran 2008)
!</description>

!<input>
    REAL(DP), INTENT(in) :: dx
!</input>

!<result>
    REAL(DP) :: datanh
!</result>
!</function>

    datanh = 0.5_DP*LOG((1+dx)/(1-dx))

  END FUNCTION datanh

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION zacos(zx)

!<description>
    ! Complex-valued inverse cosine functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: zacos
!</result>
!</function>

    zacos = -CMPLX(0.0_DP,1.0_DP,DP)*LOG(zx+CMPLX(0.0_DP,1.0_DP,DP)*SQRT(1.0-zx**2))

  END FUNCTION zacos

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION zasin(zx)

!<description>
    ! Complex-valued inverse sine functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: zasin
!</result>
!</function>

    zasin = -CMPLX(0.0_DP,1.0_DP,DP)*LOG(CMPLX(0.0_DP,1.0_DP,DP)*zx+SQRT(1.0-zx**2))

  END FUNCTION zasin

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION zatan(zx)

!<description>
    ! Complex-valued inverse tangens functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: zatan
!</result>
!</function>

    zatan = 0.5_DP*CMPLX(0.0_DP,1.0_DP,DP)*LOG((CMPLX(0.0_DP,1.0_DP,DP)+zx)/&
                                               (CMPLX(0.0_DP,1.0_DP,DP)-zx))

  END FUNCTION zatan

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION zacosh(zx)

!<description>
    ! Complex-valued inverse hyperbolic cosine functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: zacosh
!</result>
!</function>

    zacosh = LOG(zx+SQRT(zx**2-1.0_DP))

  END FUNCTION zacosh

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION zasinh(zx)

!<description>
    ! Complex-valued inverse hyperbolic sine functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: zasinh
!</result>
!</function>

    zasinh = LOG(zx+SQRT(zx**2+1.0_DP))

  END FUNCTION zasinh

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION zatanh(zx)

!<description>
    ! Complex-valued inverse hyperbolic tangens functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: zatanh
!</result>
!</function>

    zatanh = 0.5_DP*LOG((1+zx)/(1-zx))

  END FUNCTION zatanh

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION zcosh(zx)

!<description>
    ! Complex-valued hyperbolic sine functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: zcosh
!</result>
!</function>

    zcosh = COS(CMPLX(0.0_DP,1.0_DP,DP)*zx)

  END FUNCTION zcosh

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION zsinh(zx)

!<description>
    ! Complex-valued hyperbolic sine functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: zsinh
!</result>
!</function>

    zsinh = -CMPLX(0.0_DP,1.0_DP,DP)*SIN(CMPLX(0.0_DP,1.0_DP,DP)*zx)

  END FUNCTION zsinh

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION ztan(zx)

!<description>
    ! Complex-valued tangens functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: ztan
!</result>
!</function>

    ztan = SIN(zx)/COS(zx)

  END FUNCTION ztan

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION ztanh(zx)

!<description>
    ! Complex-valued hyperbolic tangens functions (available in Fortran 2008)
!</description>

!<input>
    COMPLEX(DP), INTENT(in) :: zx
!</input>

!<result>
    COMPLEX(DP) :: ztanh
!</result>
!</function>

    ztanh = zsinh(zx)/zcosh(zx)

  END FUNCTION ztanh

END MODULE ModLib_FeatFlow_Parser
