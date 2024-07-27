
SUBMODULE (ModLib_FENIA) SubLib_EquationParsing

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule contains routines that ....

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
!
MODULE FUNCTION EQP_Calculator(NUMBER_POSITION,NUMBER_VALUE, &
    FUNCTION_POSITION,FUNCTION_TYPE,OPERATOR_POSITION,OPERATOR_TYPE)

! EQP_Calculator takes data extracted from an equation parser and calculates
! the result of the parsed equation.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! number_position: integer, array (1D). Contains the positions of the initial
!    digit of every number in the function string.
! number_value: real, array (1D). Contains the numerical values of every number
!    in the string. The elements of this array are respective to the
!    number's position that is stored in array "number_position".
! function_position: integer, array (1D). Contains the positions of the initial
!    character of every intrinsic function in the function string.
! function_type: character, array (1D). Contains a character string key that
!    indicates which function is at each position stored in array
!    "function_position".
! operator_position: integer, array (1D). Contains the positions of the
!    mathematical operators in the function string.
! operator_type: character, array (1D). Contains a character key that indicates
!     which operator is at each position stored in array
!    "operator_position".

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! EQP_Calculator: real, scalar. The result of the parsed equation.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function EQP_Calculator(number_position,number_value, &
!    function_position,function_type,operator_position,operator_type)
!use Config, only: srk
!implicit none
!real(srk):: EQP_Calculator
!real(srk),pointer:: number_value(:)
!integer,pointer:: number_position(:)
!integer,pointer:: function_position(:),operator_position(:)
!character,pointer:: function_type(:)*(*),operator_type(:)*(*)
!end function EQP_Calculator
!end interface

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE


! Argument variables:
REAL(SRK):: EQP_Calculator
REAL(SRK),POINTER:: NUMBER_VALUE(:)
INTEGER,POINTER:: NUMBER_POSITION(:)
INTEGER,POINTER:: FUNCTION_POSITION(:),OPERATOR_POSITION(:)
CHARACTER,POINTER:: FUNCTION_TYPE(:)*(*),OPERATOR_TYPE(:)*(*)

! Private variables:
INTEGER:: I,J,K,M,N

! ------------------------------------------------------------------------------

! Error control:

IF (SIZE(NUMBER_VALUE)/=SIZE(NUMBER_POSITION)) THEN
WRITE(*,*)"EQP_Calculator"
WRITE(*,*)"ERROR: number values do not match the number positions."
WRITE(*,*)"numbers found at positions: ",NUMBER_POSITION
WRITE(*,*)"given number values: ",NUMBER_VALUE
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIZE(FUNCTION_TYPE)/=SIZE(FUNCTION_POSITION)) THEN
WRITE(*,*)"EQP_Calculator"
WRITE(*,*)"ERROR: function types do not match the function positions."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIZE(OPERATOR_TYPE)/=SIZE(OPERATOR_POSITION)) THEN
WRITE(*,*)"EQP_Calculator"
WRITE(*,*)"ERROR: operator types do not match the operator positions."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY(FUNCTION_TYPE=='').OR.ANY(OPERATOR_TYPE=='')) THEN
WRITE(*,*)"EQP_Calculator"
WRITE(*,*)"ERROR: invalid type data."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY(NUMBER_POSITION==0).OR.ANY(FUNCTION_POSITION==0).OR. &
    & ANY(OPERATOR_POSITION==0)) THEN
WRITE(*,*)"EQP_Calculator"
WRITE(*,*)"ERROR: invalid position data."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! Abscent function or operator information is supported because it is possible
! for the parsed sting to be a single number.
IF ((SIZE(NUMBER_VALUE)==0).OR.(SIZE(NUMBER_POSITION)==0)) THEN
WRITE(*,*)"EQP_Calculator"
WRITE(*,*)"ERROR: missing data."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! This warnings check can be obnoxious:
!if (all(number_value==0)) then
!write(*,*)"EQP_Calculator"
!write(*,*)"WARNING: all number values are zero."
!write(*,*)"Continue..."
!if (warnings_pause) read(*,*)
!end if

! ------------------------------------------------------------------------------

! If the parsed string is a single number, return early.
IF ((SIZE(NUMBER_VALUE)==1).AND.(SIZE(FUNCTION_TYPE)==0)) THEN
IF (SIZE(OPERATOR_TYPE)/=0) THEN
    IF (SIZE(OPERATOR_TYPE)==1) THEN

        IF (ALL(OPERATOR_TYPE(1)/=(/'+','-'/))) THEN
        WRITE(*,*)"EQP_Calculator"
        WRITE(*,*)"ERROR: operator invalid for a single number."
        WRITE(*,*)"Program terminated."
        READ(*,*)
        STOP
        END IF

        IF (OPERATOR_POSITION(1)/=1) THEN
        WRITE(*,*)"EQP_Calculator"
        WRITE(*,*)"ERROR: incorrectly placed operator."
        WRITE(*,*)"operator :",OPERATOR_TYPE(1)
        WRITE(*,*)"operator position :",OPERATOR_POSITION(1)
        WRITE(*,*)"Program terminated."
        READ(*,*)
        STOP
        END IF

        IF (OPERATOR_TYPE(1)=='+') EQP_Calculator=NUMBER_VALUE(1)
        IF (OPERATOR_TYPE(1)=='-') EQP_Calculator=-NUMBER_VALUE(1)
        RETURN

    ELSE

        WRITE(*,*)"EQP_Calculator"
        WRITE(*,*)"ERROR: operator misuse"
        WRITE(*,*)"Program terminated."
        READ(*,*)
        STOP

    END IF
ELSE
    EQP_Calculator=NUMBER_VALUE(1)
    RETURN
END IF
END IF

! ------------------------------------------------------------------------------

! Intrinsic functions:

ELIMINATE_FUNCTIONS: DO I=1,SIZE(FUNCTION_POSITION)

DO J=1,SIZE(NUMBER_POSITION)
IF (NUMBER_POSITION(J)==FUNCTION_POSITION(I)+LEN_TRIM(FUNCTION_TYPE(I))) THEN

    DO K=1,SIZE(OPERATOR_POSITION)
    IF (NUMBER_POSITION(J)==(OPERATOR_POSITION(K)+1)) THEN
        IF (OPERATOR_TYPE(K)=='-') THEN
            NUMBER_VALUE(J)=-NUMBER_VALUE(J)
            OPERATOR_POSITION(K)=0
            OPERATOR_TYPE=''
            EXIT
        ELSE
            WRITE(*,*)"EQP_Calculator"
            WRITE(*,*)"ERROR: incorrectly placed operator."
            WRITE(*,*)"operator :",OPERATOR_TYPE(M)
            WRITE(*,*)"operator position :",OPERATOR_POSITION(M)
            WRITE(*,*)"Program terminated."
            READ(*,*)
            STOP
        END IF
    END IF
    END DO

    SELECT CASE(TRIM(FUNCTION_TYPE(I)))
        CASE('asin')
            NUMBER_VALUE(J)=ASIN(NUMBER_VALUE(J))
        CASE('acos')
            NUMBER_VALUE(J)=ACOS(NUMBER_VALUE(J))
        CASE('atan')
            NUMBER_VALUE(J)=ATAN(NUMBER_VALUE(J))
        CASE('sinh')
            NUMBER_VALUE(J)=SINH(NUMBER_VALUE(J))
        CASE('cosh')
            NUMBER_VALUE(J)=COSH(NUMBER_VALUE(J))
        CASE('tanh')
            NUMBER_VALUE(J)=TANH(NUMBER_VALUE(J))
        CASE('sin')
            NUMBER_VALUE(J)=SIN(NUMBER_VALUE(J))
        CASE('cos')
            NUMBER_VALUE(J)=COS(NUMBER_VALUE(J))
        CASE('tan')
            NUMBER_VALUE(J)=TAN(NUMBER_VALUE(J))
        CASE('exp')
            NUMBER_VALUE(J)=EXP(NUMBER_VALUE(J))
        CASE('abs')
            NUMBER_VALUE(J)=ABS(NUMBER_VALUE(J))
        CASE('asinh')
            NUMBER_VALUE(J)=ASINH(NUMBER_VALUE(J))
        CASE('acosh')
            NUMBER_VALUE(J)=ACOSH(NUMBER_VALUE(J))
        CASE('fact')
            NUMBER_VALUE(J)=FACT(NUMBER_VALUE(J))
        CASE('ln')
            IF (NUMBER_VALUE(J)<=0.0) THEN
            WRITE(*,*)"ApplyFunctionReal"
            WRITE(*,*)"ERROR: invalid argument in natural logarithm"
            WRITE(*,*)"given argument : ",NUMBER_VALUE(J)
            WRITE(*,*)"Program terminated."
            READ(*,*)
            STOP
            END IF
            NUMBER_VALUE(J)=LOG(NUMBER_VALUE(J))
        CASE('log')
            IF (NUMBER_VALUE(J)<=0.0) THEN
            WRITE(*,*)"ApplyFunctionReal"
            WRITE(*,*)"ERROR: invalid argument in base 10 logarithm"
            WRITE(*,*)"given argument : ",NUMBER_VALUE(J)
            WRITE(*,*)"Program terminated."
            READ(*,*)
            STOP
            END IF
            NUMBER_VALUE(J)=LOG10(NUMBER_VALUE(J))
        CASE('sqrt')
            IF (NUMBER_VALUE(J)<0.0) THEN
            WRITE(*,*)"ApplyFunctionReal"
            WRITE(*,*)"ERROR: invalid argument in square root"
            WRITE(*,*)"given argument : ",NUMBER_VALUE(J)
            WRITE(*,*)"Program terminated."
            READ(*,*)
            STOP
            END IF
            NUMBER_VALUE(J)=SQRT(NUMBER_VALUE(J))
    END SELECT

    NUMBER_POSITION(J)=FUNCTION_POSITION(I)
    CYCLE ELIMINATE_FUNCTIONS

END IF
END DO

END DO ELIMINATE_FUNCTIONS

! ------------------------------------------------------------------------------

! Minus signs that are not used as operators:

ELIMINATE_NEGATIVES: DO I=1,SIZE(OPERATOR_POSITION)
IF (OPERATOR_TYPE(I)=='-') THEN
    IF (OPERATOR_POSITION(I)==1) THEN

        DO J=1,SIZE(NUMBER_POSITION)
        IF (NUMBER_POSITION(J)==(OPERATOR_POSITION(I)+1)) THEN
            NUMBER_POSITION(J)=OPERATOR_POSITION(I)
            NUMBER_VALUE(J)=-NUMBER_VALUE(J)
            OPERATOR_POSITION(I)=0
            OPERATOR_TYPE(I)=''
            CYCLE ELIMINATE_NEGATIVES
        END IF
        END DO

    ELSE

        DO K=1,SIZE(OPERATOR_POSITION)
        IF (OPERATOR_POSITION(K)==OPERATOR_POSITION(I)-1) THEN

        IF (ALL(OPERATOR_TYPE(K)/=(/'+','-'/))) THEN
            DO J=1,SIZE(NUMBER_POSITION)
            IF (NUMBER_POSITION(J)==(OPERATOR_POSITION(I)+1)) THEN
                NUMBER_POSITION(J)=OPERATOR_POSITION(I)
                NUMBER_VALUE(J)=-NUMBER_VALUE(J)
                OPERATOR_POSITION(I)=0
                OPERATOR_TYPE(I)=''
                CYCLE ELIMINATE_NEGATIVES
            END IF
            END DO
        ELSE
            WRITE(*,*)"EQP_Calculator"
            WRITE(*,*)"ERROR: incorrectly placed operator."
            WRITE(*,*)"operator :",OPERATOR_TYPE(I)
            WRITE(*,*)"operator position :",OPERATOR_POSITION(I)
            WRITE(*,*)"Program terminated."
            READ(*,*)
            STOP
        END IF

        END IF
        END DO

    END IF
END IF
END DO ELIMINATE_NEGATIVES

! ------------------------------------------------------------------------------

! Power:

N=MAXVAL(OPERATOR_POSITION)+1

POWER: DO

IF (ALL(OPERATOR_TYPE/='^')) EXIT
N=N-1
IF (N<MINVAL(OPERATOR_POSITION)) EXIT POWER
I=0

DO J=1,SIZE(OPERATOR_POSITION)
    IF (N==OPERATOR_POSITION(J)) THEN
        I=J
        EXIT
    END IF
END DO

IF ((I/=0).AND.(OPERATOR_TYPE(I)=='^')) THEN
    J=OPERATOR_POSITION(I)
    DO
        J=J-1
        DO K=1,SIZE(NUMBER_POSITION)
        IF (NUMBER_POSITION(K)==J) THEN
            DO M=1,SIZE(NUMBER_POSITION)
            IF (NUMBER_POSITION(M)==OPERATOR_POSITION(I)+1) THEN

                NUMBER_VALUE(K)=NUMBER_VALUE(K)**NUMBER_VALUE(M)
                NUMBER_POSITION(M)=0
                CYCLE POWER

            END IF
            END DO
        END IF
        END DO
    END DO
END IF

END DO POWER

! ------------------------------------------------------------------------------

! Multiplication and division:

N=MINVAL(OPERATOR_POSITION)-1

MULT_DIV: DO

IF ((ALL(OPERATOR_TYPE/='*')).AND.(ALL(OPERATOR_TYPE/='/'))) EXIT MULT_DIV
N=N+1
IF (N>MAXVAL(OPERATOR_POSITION)) EXIT MULT_DIV
I=0

DO J=1,SIZE(OPERATOR_POSITION)
    IF (N==OPERATOR_POSITION(J)) THEN
        I=J
        EXIT
    END IF
END DO

IF ((I/=0).AND.((OPERATOR_TYPE(I)=='*').OR.(OPERATOR_TYPE(I)=='/'))) THEN
    J=OPERATOR_POSITION(I)
    DO
        J=J-1
        DO K=1,SIZE(NUMBER_POSITION)
        IF (NUMBER_POSITION(K)==J) THEN
            DO M=1,SIZE(NUMBER_POSITION)
            IF (NUMBER_POSITION(M)==OPERATOR_POSITION(I)+1) THEN

                IF (OPERATOR_TYPE(I)=='*') NUMBER_VALUE(K)= &
                    & NUMBER_VALUE(K)*NUMBER_VALUE(M)
                IF (OPERATOR_TYPE(I)=='/') NUMBER_VALUE(K)= &
                    & NUMBER_VALUE(K)/NUMBER_VALUE(M)

                NUMBER_POSITION(M)=0
                CYCLE MULT_DIV

            END IF
            END DO
        END IF
        END DO
    END DO
END IF

END DO MULT_DIV

! ------------------------------------------------------------------------------

! Addition and substraction:

N=MINVAL(OPERATOR_POSITION)-1

ADD_SUB: DO

IF ((ALL(OPERATOR_TYPE/='+')).AND.(ALL(OPERATOR_TYPE/='-'))) EXIT ADD_SUB
N=N+1
IF (N>MAXVAL(OPERATOR_POSITION)) EXIT ADD_SUB
I=0

DO J=1,SIZE(OPERATOR_POSITION)
    IF (N==OPERATOR_POSITION(J)) THEN
        I=J
        EXIT
    END IF
END DO

IF ((I/=0).AND.((OPERATOR_TYPE(I)=='+').OR.(OPERATOR_TYPE(I)=='-'))) THEN
    J=OPERATOR_POSITION(I)
    DO
        J=J-1
        DO K=1,SIZE(NUMBER_POSITION)
        IF (NUMBER_POSITION(K)==J) THEN
            DO M=1,SIZE(NUMBER_POSITION)
            IF (NUMBER_POSITION(M)==OPERATOR_POSITION(I)+1) THEN

                IF (OPERATOR_TYPE(I)=='+') NUMBER_VALUE(K)= &
                    & NUMBER_VALUE(K)+NUMBER_VALUE(M)
                IF (OPERATOR_TYPE(I)=='-') NUMBER_VALUE(K)= &
                    & NUMBER_VALUE(K)-NUMBER_VALUE(M)

                NUMBER_POSITION(M)=0
                CYCLE ADD_SUB

            END IF
            END DO
        END IF
        END DO
    END DO
END IF

END DO ADD_SUB

! ------------------------------------------------------------------------------

IF (COUNT(NUMBER_POSITION/=0)/=1) THEN
WRITE(*,*)"EQP_Calculator"
WRITE(*,*)"ERROR: internal error"
WRITE(*,*)"dumping debugging info:"
WRITE(*,*)'number_position: ',NUMBER_POSITION
WRITE(*,*)'number_value: ',NUMBER_VALUE
WRITE(*,*)'function_position: ',FUNCTION_POSITION
WRITE(*,*)'function_type: ',FUNCTION_TYPE
WRITE(*,*)'operator_position: ',OPERATOR_POSITION
WRITE(*,*)'operator_type: ',OPERATOR_TYPE
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

DO I=1,SIZE(NUMBER_POSITION)
    IF (NUMBER_POSITION(I)/=0) THEN
        EQP_Calculator=NUMBER_VALUE(I)
        EXIT
    END IF
END DO

END FUNCTION EQP_Calculator

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION EQP_EquationParser(EQUATION,VARIABLE_NAMES,VARIABLE_VALUES)

! EQP_EquationParser parses a given equation string and evaluates it.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! equation: character, scalar. The equation that will be evaluated in the form
!    of a character string.
! variable_names: character, array (1D). The names of the variables as they
!    appear in the equation.
! variable_values: real, array (1D). The values of the variables. They should
!    be respective to their names as given in array 'variable_names'.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! EQP_EquationParser: real, scalar. The result of the equation.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function EQP_EquationParser(equation,variable_names,variable_values)
!use Config, only: srk
!implicit none
!character,intent(in):: equation*(*)
!character,pointer:: variable_names(:)*(*)
!real(srk),pointer:: variable_values(:)
!real(srk):: EQP_EquationParser
!end function EQP_EquationParser
!end interface

!character,pointer:: names(:)*50
!real(srk),pointer:: values(:)
!real(srk):: res
!allocate(values(3))
!allocate(names(3))
!!The "ooo" endings are used to test subroutine EQP_StringFilter.
!names(1)='var_ooo' !=12
!names(2)='var_o' !=23
!names(3)='var_oo' !=34
!values(1)= 12.0
!values(2)= 23.0
!values(3)= 34.0
!write(*,*)'1234567890123456789012345678901234567890'
!write(*,*)'4387.34*var_ooo+cos(65.4)/389.384-var_oo+var_o'
!res=EQP_EquationParser( &
!    & '4387.34 * var_ooo + &
!    & cos(65.4+3.1) / 389.384 - &
!    & var_oo + var_o',names,values)
!write(*,*)res
!write(*,*)4387.34*values(1)+cos(65.4+3.1)/389.384-values(3)+values(2)

! Rules:
! [1] The letter 'E' may NOT be used do indicate powers of 10. For example,
! number '0.0015' may NOT be written as '1.5E-3'. Write '1.5*10^-3' instead.
! [2] The minus sign may be used to declare negative numbers.
! [3] Supported sumbols:
! 1. plus sign: +
! 2. minus sign: -
! 3. multiplication sign: *
! 4. division sign: /
! 5. power: ^
! 6. opening parenthesis: (
! 7. closing parenthesis: )
! [4] Supported intrinsic functions:
! 1. neper logarithm: ln()
! 2. base-10 logarithm: log()
! 3. power of e: exp()
! 4. absolute value: abs()
! 5. square root: sqrt()
! 6. factorial: fact()
! 7. sine: sin()
! 8. cosine: cos()
! 9. tangent: tan()
! 10. inverse sine: asin()
! 11. inverse cosine: acos()
! 12. inverse tangent: atan()
! 13. hyperbolic sine: sinh()
! 14. hyperbolic cosine: cosh()
! 15. hyperbolic tangent: tanh()
! 16. inverse hyperbolic sine: asinh()
! 17. inverse hyperbolic cosine: acosh()

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Parameters:
CHARACTER,PARAMETER:: INTRINSIC_FUNCTIONS(17)*5=(/ &
    & 'ln   ','sin  ','cos  ','tan  ','log  ','exp  ','abs  ', &
    & 'asin ','acos ','atan ','sinh ','cosh ','tanh ','sqrt ','fact ', &
    & 'asinh','acosh'/)
CHARACTER,PARAMETER:: OPERATORS(5)*(1)=(/'+','-','*','/','^'/)
CHARACTER,PARAMETER:: PARENTHESIS(2)*(1)=(/'(',')'/)
CHARACTER,PARAMETER:: NUMERALS(11)*(1)= &
    & (/'1','2','3','4','5','6','7','8','9','0','.'/)

! Argument variables:
CHARACTER,INTENT(IN):: EQUATION*(*)
CHARACTER,POINTER:: VARIABLE_NAMES(:)*(*)
REAL(SRK),POINTER:: VARIABLE_VALUES(:)
REAL(SRK):: EQP_EquationParser

! Private variables:
CHARACTER:: INTERNAL_EQUATION*(LEN(EQUATION)),NO_BLANKS*(LEN(EQUATION))
INTEGER:: I,J,K,ON,OFF

REAL(SRK),POINTER:: NUMBER_VALUE(:)
INTEGER,POINTER:: NUMBER_POSITION(:)
CHARACTER,POINTER:: VARIABLE_TYPE(:)*(LEN(VARIABLE_NAMES))
INTEGER,POINTER:: VARIABLE_POSITION(:)
CHARACTER,POINTER:: FUNCTION_TYPE(:)*(LEN(INTRINSIC_FUNCTIONS))
INTEGER,POINTER:: FUNCTION_POSITION(:)
CHARACTER,POINTER:: OPERATOR_TYPE(:)*(LEN(OPERATORS))
INTEGER,POINTER:: OPERATOR_POSITION(:)
CHARACTER,POINTER:: PARENTHESIS_TYPE(:)*(LEN(PARENTHESIS))
INTEGER,POINTER:: PARENTHESIS_POSITION(:)

REAL(SRK),POINTER:: ACTIVE_NUMBER_VALUE(:)
INTEGER,POINTER:: ACTIVE_NUMBER_POSITION(:)
CHARACTER,POINTER:: ACTIVE_FUNCTION_TYPE(:)*(LEN(INTRINSIC_FUNCTIONS))
INTEGER,POINTER:: ACTIVE_FUNCTION_POSITION(:)
CHARACTER,POINTER:: ACTIVE_OPERATOR_TYPE(:)*(LEN(OPERATORS))
INTEGER,POINTER:: ACTIVE_OPERATOR_POSITION(:)

! ------------------------------------------------------------------------------

! Error control:

IF (LEN_TRIM(EQUATION)==0) THEN
WRITE(*,*)"EQP_EquationParser"
WRITE(*,*)"ERROR: missing equation."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SIZE(VARIABLE_NAMES)==0).OR.(SIZE(VARIABLE_VALUES)==0)) THEN
WRITE(*,*)"EQP_EquationParser"
WRITE(*,*)"ERROR: empty input array(s)."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIZE(VARIABLE_NAMES)/=SIZE(VARIABLE_VALUES)) THEN
WRITE(*,*)"EQP_EquationParser"
WRITE(*,*)"ERROR: amount of variables and amount of values do not match."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY(VARIABLE_NAMES=='')) THEN
WRITE(*,*)"EQP_EquationParser"
WRITE(*,*)"ERROR: missing variable's name."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! This warnings check can be obnoxious:
!if (all(variable_values==0)) then
!write(*,*)"EQP_EquationParser"
!write(*,*)"WARNING: all variable's values are equal to zero."
!write(*,*)"Continue..."
!if (warnings_pause) read(*,*)
!end if

! An additional warning check could search for variables that are defined in
! arrays "variable_names" and "variable_values" but do not appear in the string.
! However this may be obnoxious sometimes, preventing the flexible use of the
! function.

! ------------------------------------------------------------------------------

! Variable initialization:
NO_BLANKS=''
J=0
DO I=1,LEN(EQUATION) ! Remove blank characters from the equation string.
    IF (EQUATION(I:I)/='') THEN
        J=J+1
        NO_BLANKS(J:J)=EQUATION(I:I)
    END IF
END DO
INTERNAL_EQUATION=NO_BLANKS

! Prepare equation string for number recognition.

! Remove variables:
CALL EQP_STRINGFILTER(INTERNAL_EQUATION,VARIABLE_NAMES, &
    & VARIABLE_POSITION,VARIABLE_TYPE)
! Remove intrinsic functions:
CALL EQP_STRINGFILTER(INTERNAL_EQUATION,INTRINSIC_FUNCTIONS, &
    & FUNCTION_POSITION,FUNCTION_TYPE)
! Remove operators:
CALL EQP_STRINGFILTER(INTERNAL_EQUATION,OPERATORS, &
    & OPERATOR_POSITION,OPERATOR_TYPE)
! Remove parentheses:
CALL EQP_STRINGFILTER(INTERNAL_EQUATION,PARENTHESIS, &
    & PARENTHESIS_POSITION,PARENTHESIS_TYPE)

! ------------------------------------------------------------------------------

! Error control:

! If the amount of opening parentheses is not equal to the amount of closing
! parentheses, then an error should occur for unmatching parentheses.
IF (COUNT(PARENTHESIS_TYPE==')')/=COUNT(PARENTHESIS_TYPE=='(')) THEN
WRITE(*,*)"EQP_EquationParser"
WRITE(*,*)"ERROR: unmatching parentheses"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! After taking out of the string all the variables, the intrinsic functions,
! the operators and the parentheses, only the numbers should be left.
DO I=1,LEN_TRIM(INTERNAL_EQUATION)
IF (INTERNAL_EQUATION(I:I)=='') CYCLE
IF (ALL(INTERNAL_EQUATION(I:I)/=NUMERALS)) THEN
WRITE(*,*)"EQP_EquationParser"
WRITE(*,*)"ERROR: undefined variable(s): ",TRIM(INTERNAL_EQUATION)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END DO

! ------------------------------------------------------------------------------

! Numbers:

! After taking out of the string all the numbers the string should be clear.
J=1
DO
    IF (INTERNAL_EQUATION=='') EXIT
    CALL LinkedListReal(NUMERICAL_VALUE=NumberReal(INTERNAL_EQUATION))

    DO I=J,LEN_TRIM(INTERNAL_EQUATION)
    IF (INTERNAL_EQUATION(I:I)/='') THEN
        CALL LinkedListInteger(NUMERICAL_VALUE=I)
        DO J=I,LEN_TRIM(INTERNAL_EQUATION)
            IF (INTERNAL_EQUATION(J:J)=='') THEN
                EXIT
            ELSE
                INTERNAL_EQUATION(J:J)=''
            END IF
        END DO
        EXIT
    END IF
    END DO
END DO

! The values of the variables are added to the numbers' array. Their locations
! are also added to the respective array.
DO I=1,SIZE(VARIABLE_TYPE)
DO J=1,SIZE(VARIABLE_NAMES)
    IF (VARIABLE_TYPE(I)==VARIABLE_NAMES(J)) THEN
        CALL LinkedListReal(NUMERICAL_VALUE=VARIABLE_VALUES(J))
        CALL LinkedListInteger(NUMERICAL_VALUE=VARIABLE_POSITION(I))
    END IF
END DO
END DO

CALL LinkedListReal(LIST_POINTER=NUMBER_VALUE)
CALL LinkedListInteger(LIST_POINTER=NUMBER_POSITION)

! ------------------------------------------------------------------------------

! A fresh equation string is obtained:
INTERNAL_EQUATION=NO_BLANKS

! Calculate all brackets, starting from the innermost. If no more brackets are
! present, calculate the final result and exit.
CALCULATE: DO

ON=0
OFF=0

! Scan for a pair of matching brackets, execute the calculations, and move on to
! the next pair of matching brackets.
BRACKETS_SCAN: DO I=1,LEN_TRIM(INTERNAL_EQUATION)

IF (INTERNAL_EQUATION(I:I)=='(') ON=I
IF (INTERNAL_EQUATION(I:I)==')') OFF=I

! A pair of brackets that does not contain any other brackets is located:
BRACKETS_FOUND: IF ((ON/=0).AND.(OFF/=0)) THEN

! Parse the equation in the brackets:

BRACKET_FUNCTIONS: DO K=ON+1,OFF-1
    DO J=1,SIZE(FUNCTION_POSITION)
    IF (FUNCTION_POSITION(J)==K) THEN
        CALL LinkedListInteger(NUMERICAL_VALUE=FUNCTION_POSITION(J))
        CALL LinkedListCharacter(CHARACTER_VALUE=FUNCTION_TYPE(J))
        FUNCTION_POSITION(J)=0
        CYCLE BRACKET_FUNCTIONS
    END IF
    END DO
END DO BRACKET_FUNCTIONS
CALL LinkedListInteger(LIST_POINTER=ACTIVE_FUNCTION_POSITION)
CALL LinkedListCharacter(LIST_POINTER=ACTIVE_FUNCTION_TYPE)

BRACKET_OPERATORS: DO K=ON+1,OFF-1
    DO J=1,SIZE(OPERATOR_POSITION)
    IF (OPERATOR_POSITION(J)==K) THEN
        CALL LinkedListInteger(NUMERICAL_VALUE=OPERATOR_POSITION(J))
        CALL LinkedListCharacter(CHARACTER_VALUE=OPERATOR_TYPE(J))
        OPERATOR_POSITION(J)=0
        CYCLE BRACKET_OPERATORS
    END IF
    END DO
END DO BRACKET_OPERATORS
CALL LinkedListInteger(LIST_POINTER=ACTIVE_OPERATOR_POSITION)
CALL LinkedListCharacter(LIST_POINTER=ACTIVE_OPERATOR_TYPE)

BRACKET_NUMBERS: DO K=ON+1,OFF-1
    DO J=1,SIZE(NUMBER_POSITION)
    IF (NUMBER_POSITION(J)==K) THEN
        CALL LinkedListInteger(NUMERICAL_VALUE=NUMBER_POSITION(J))
        CALL LinkedListReal(NUMERICAL_VALUE=NUMBER_VALUE(J))
        NUMBER_POSITION(J)=0
        CYCLE BRACKET_NUMBERS
    END IF
    END DO
END DO BRACKET_NUMBERS
CALL LinkedListInteger(LIST_POINTER=ACTIVE_NUMBER_POSITION)
CALL LinkedListReal(LIST_POINTER=ACTIVE_NUMBER_VALUE)

! Execute calculations:
EQP_EquationParser=EQP_Calculator( &
    & ACTIVE_NUMBER_POSITION,ACTIVE_NUMBER_VALUE, &
    & ACTIVE_FUNCTION_POSITION,ACTIVE_FUNCTION_TYPE, &
    & ACTIVE_OPERATOR_POSITION,ACTIVE_OPERATOR_TYPE)

! The brackets are deleted:
INTERNAL_EQUATION(ON:ON)=''
INTERNAL_EQUATION(OFF:OFF)=''

! The calculated number and its position are added to respective arrays:
! (At this point, due to the code structure, the usage of the linked list
! functions is not possible: )
CALL ArrayExpandInteger(NUMBER_POSITION,ON)
CALL ArrayExpandReal(NUMBER_VALUE,EQP_EquationParser)

EXIT BRACKETS_SCAN ! Scan for another pair of brackets

END IF BRACKETS_FOUND

END DO BRACKETS_SCAN

! ------------------------------------------------------------------------------

! If there are no brackets in the string, the final result is calculated:
IF (ON==0.AND.OFF==0) THEN

! Final result functions:
DO J=1,SIZE(FUNCTION_POSITION)
IF (FUNCTION_POSITION(J)/=0) THEN
    CALL LinkedListInteger(NUMERICAL_VALUE=FUNCTION_POSITION(J))
    CALL LinkedListCharacter(CHARACTER_VALUE=FUNCTION_TYPE(J))
END IF
END DO
CALL LinkedListInteger(LIST_POINTER=ACTIVE_FUNCTION_POSITION)
CALL LinkedListCharacter(LIST_POINTER=ACTIVE_FUNCTION_TYPE)

! Final result operators:
DO J=1,SIZE(OPERATOR_POSITION)
IF (OPERATOR_POSITION(J)/=0) THEN
    CALL LinkedListInteger(NUMERICAL_VALUE=OPERATOR_POSITION(J))
    CALL LinkedListCharacter(CHARACTER_VALUE=OPERATOR_TYPE(J))
END IF
END DO
CALL LinkedListInteger(LIST_POINTER=ACTIVE_OPERATOR_POSITION)
CALL LinkedListCharacter(LIST_POINTER=ACTIVE_OPERATOR_TYPE)

! Final result numbers:
DO J=1,SIZE(NUMBER_POSITION)
IF (NUMBER_POSITION(J)/=0) THEN
    CALL LinkedListInteger(NUMERICAL_VALUE=NUMBER_POSITION(J))
    CALL LinkedListReal(NUMERICAL_VALUE=NUMBER_VALUE(J))
END IF
END DO
CALL LinkedListInteger(LIST_POINTER=ACTIVE_NUMBER_POSITION)
CALL LinkedListReal(LIST_POINTER=ACTIVE_NUMBER_VALUE)

! Execute calculations:
EQP_EquationParser=EQP_Calculator( &
    & ACTIVE_NUMBER_POSITION,ACTIVE_NUMBER_VALUE, &
    & ACTIVE_FUNCTION_POSITION,ACTIVE_FUNCTION_TYPE, &
    & ACTIVE_OPERATOR_POSITION,ACTIVE_OPERATOR_TYPE)

EXIT CALCULATE

END IF

END DO CALCULATE

END FUNCTION EQP_EquationParser

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION EQP_InequalityParser(INEQUALITY,VARIABLE_NAMES,VARIABLE_VALUES, &
    & TOLERANCE)

! EQP_InequalityParser parses a given inequality string and evaluates it
! returning .true. if the inequality is valid, or .false. if the inequality is
! invalid.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! equation: character, scalar. The equation that will be evaluated in the form
!    of a character string.
! variable_names: character, array (1D). The names of the variables as they
!    appear in the equation.
! variable_values: real, array (1D). The values of the variables. They should
!    be respective to their names as given in array 'variable_names'.

! INPUT (OPTIONAL):
! tolerance: real, scalar. If present, the specified number will be regarded as
!    the tolerance for treating the equalities.

! OUTPUT (REQUIRED):
! EQP_EquationParser: real, scalar. The result of the equation.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function EQP_InequalityParser(inequality,variable_names,variable_values, &
!    & tolerance)
!use Config, only: srk
!implicit none
!character,intent(in):: inequality*(*)
!character,pointer:: variable_names(:)*(*)
!real(srk),pointer:: variable_values(:)
!real(srk),optional:: tolerance
!logical:: EQP_InequalityParser
!end function EQP_InequalityParser
!end interface

!character,pointer:: names(:)*50
!real(srk),pointer:: values(:)
!integer:: i
!logical:: res
!allocate(values(1))
!allocate(names(1))
!names='x'
!do i=-5,5,1
!values=i
!res=EQP_InequalityParser('x^2-4>=0',names,values)
!write(*,*)i,res
!write(*,*)
!end do

! Rules:
! [1] Supported inequality symbols:
! 1. equal to: =
! 2. less than: <
! 3. greater than: >
! 4. less or equal than: <=
! 5. greater or equal than: >=
! [2] The letter 'E' may NOT be used do indicate powers of 10. For example,
! number '0.0015' may NOT be written as '1.5E-3'. Write '1.5*10^-3' instead.
! [3] The minus sign may be used to declare negative numbers.
! [4] Supported sumbols:
! 1. plus sign: +
! 2. minus sign: -
! 3. multiplication sign: *
! 4. division sign: /
! 5. power: ^
! 6. opening parenthesis: (
! 7. closing parenthesis: )
! [5] Supported intrinsic functions:
! 1. neper logarithm: ln()
! 2. 10 base logarithm: log()
! 3. power of e: exp()
! 4. absolute value: abs()
! 5. square root: sqrt()
! 6. factorial: fact()
! 7. sine: sin()
! 8. cosine: cos()
! 9. tangent: tan()
! 10. inverse sine: asin()
! 11. inverse cosine: acos()
! 12. inverse tangent: atan()
! 13. hyperbolic sine: sinh()
! 14. hyperbolic cosine: cosh()
! 15. hyperbolic tangent: tanh()
! 16. inverse hyperbolic sine: asinh()
! 17. inverse hyperbolic cosine: acosh()

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Parameters:
CHARACTER,PARAMETER:: INEQUALITY_OPERATORS(5)*(2)=(/'= ','> ','< ','<=','>='/)

! Argument variables:
CHARACTER,INTENT(IN):: INEQUALITY*(*)
CHARACTER,POINTER:: VARIABLE_NAMES(:)*(*)
REAL(SRK),POINTER:: VARIABLE_VALUES(:)
REAL(SRK),OPTIONAL:: TOLERANCE
LOGICAL:: EQP_InequalityParser

! Private variables:
CHARACTER:: INTERNAL_INEQUALITY*(LEN(INEQUALITY)),NO_BLANKS*(LEN(INEQUALITY))
INTEGER:: I,J,PURE_LENGTH

CHARACTER,POINTER:: OPERATOR_TYPE(:)*(LEN(INEQUALITY_OPERATORS))
INTEGER,POINTER:: OPERATOR_POSITION(:)
REAL(SRK):: LEFT,RIGHT

! ------------------------------------------------------------------------------

! Variable initialization:
NO_BLANKS=''
J=0
DO I=1,LEN(INEQUALITY) ! Remove blank characters from the equation string.
    IF (INEQUALITY(I:I)/='') THEN
        J=J+1
        NO_BLANKS(J:J)=INEQUALITY(I:I)
    END IF
END DO
INTERNAL_INEQUALITY=NO_BLANKS
PURE_LENGTH=LEN_TRIM(INTERNAL_INEQUALITY)

! Remove inequality operators:
CALL EQP_STRINGFILTER(INTERNAL_INEQUALITY,INEQUALITY_OPERATORS, &
    & OPERATOR_POSITION,OPERATOR_TYPE)

! ------------------------------------------------------------------------------

! Error control:

! Exactly one inequality operator should exist:
IF (SIZE(OPERATOR_POSITION)/=1) THEN
WRITE(*,*)"EQP_InequalityParser"
WRITE(*,*)"ERROR: exactly one inequality operator should exist."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! The inequality operator should be somewhere in the middle of the string:
IF ((OPERATOR_POSITION(1)==1).OR.(OPERATOR_POSITION(1)>=PURE_LENGTH)) THEN
WRITE(*,*)"EQP_InequalityParser"
WRITE(*,*)"ERROR: improperly placed inequality operator."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(TOLERANCE)).AND. &
    & ((OPERATOR_TYPE(1)=='<').OR.(OPERATOR_TYPE(1)=='>'))) THEN
WRITE(*,*)"EQP_InequalityParser"
WRITE(*,*)"WARNING: no equalities specified to apply the given tolerance."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

! ------------------------------------------------------------------------------

! Calculate the two parts of the inequality:
LEFT=EQP_EquationParser(INTERNAL_INEQUALITY( &
    & 1:(OPERATOR_POSITION(1)-1)), &
    & VARIABLE_NAMES,VARIABLE_VALUES)

RIGHT=EQP_EquationParser(INTERNAL_INEQUALITY( &
    & (OPERATOR_POSITION(1)+LEN_TRIM(OPERATOR_TYPE(1))):PURE_LENGTH), &
    & VARIABLE_NAMES,VARIABLE_VALUES)

! Perform the inequality check:
SELECT CASE(TRIM(OPERATOR_TYPE(1)))
    CASE('=')
        IF (PRESENT(TOLERANCE)) THEN
            IF ((((LEFT-TOLERANCE)<=RIGHT).AND. &
                & (RIGHT<=(LEFT+TOLERANCE))).OR. &
                & (((RIGHT-TOLERANCE)<=LEFT).AND. &
                & (LEFT<=(RIGHT+TOLERANCE)))) THEN
                EQP_InequalityParser=.TRUE.
            ELSE
                EQP_InequalityParser=.FALSE.
            END IF
        ELSE
            IF (LEFT==RIGHT) THEN
                EQP_InequalityParser=.TRUE.
            ELSE
                EQP_InequalityParser=.FALSE.
            END IF
        END IF
    CASE('<=')
        IF (PRESENT(TOLERANCE)) THEN
            IF ((LEFT<=RIGHT).OR. &
                & (((LEFT-TOLERANCE)<=RIGHT).AND. &
                & (RIGHT<=(LEFT+TOLERANCE))).OR. &
                & (((RIGHT-TOLERANCE)<=LEFT).AND. &
                & (LEFT<=(RIGHT+TOLERANCE)))) THEN
                EQP_InequalityParser=.TRUE.
            ELSE
                EQP_InequalityParser=.FALSE.
            END IF
        ELSE
            IF (LEFT<=RIGHT) THEN
                EQP_InequalityParser=.TRUE.
            ELSE
                EQP_InequalityParser=.FALSE.
            END IF
        END IF
    CASE('>=')
        IF (PRESENT(TOLERANCE)) THEN
            IF ((LEFT>=RIGHT).OR. &
                & (((LEFT-TOLERANCE)<=RIGHT).AND. &
                & (RIGHT<=(LEFT+TOLERANCE))).OR. &
                & (((RIGHT-TOLERANCE)<=LEFT).AND. &
                & (LEFT<=(RIGHT+TOLERANCE)))) THEN
                EQP_InequalityParser=.TRUE.
            ELSE
                EQP_InequalityParser=.FALSE.
            END IF
        ELSE
            IF (LEFT>=RIGHT) THEN
                EQP_InequalityParser=.TRUE.
            ELSE
                EQP_InequalityParser=.FALSE.
            END IF
        END IF
    CASE('<')
        IF (LEFT<RIGHT) THEN
            EQP_InequalityParser=.TRUE.
        ELSE
            EQP_InequalityParser=.FALSE.
        END IF
    CASE('>')
        IF (LEFT>RIGHT) THEN
            EQP_InequalityParser=.TRUE.
        ELSE
            EQP_InequalityParser=.FALSE.
        END IF
END SELECT

END FUNCTION EQP_InequalityParser

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE EQP_STRINGFILTER(STRING,WORDS,LOCATIONS_POINTER,INVENTORY_POINTER)

! EQP_StringFilter takes a given string and a set of character variables and
! locates the occurences of the variables in the string. The locations of the
! occurences are indicated by the position of the first character of the
! variable in the string. An inventory with the respective character variables
! for every occurence is also created. The string's elements that match the
! variables are then set to blank, thus creating a new version of the string.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! string: character, scalar. The string that will be searched.
! words: character, array (1D). The set of character variables that will be
!    searched for in the string.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! string: character, scalar. The input string after the filtering.
! locations: integer, pointer (1D). The locations of the occurences of the
!    given words in the string indicated by the location of the word's first
!    character in the string.
! inventory: character, pointer (1D). The inventory that contains the character
!    variable that is respective for every recorded occurence.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!subroutine EQP_StringFilter(string,words,locations_pointer,inventory_pointer)
!implicit none
!character,intent(in):: words(:)*(*)
!character,intent(inout):: string*(*)
!character,pointer:: inventory_pointer(:)*(*)
!integer,pointer:: locations_pointer(:)
!end subroutine EQP_StringFilter
!end interface

!integer:: i
!character,parameter:: intrinsic_functions(17)*5=(/ &
!    & 'asinh','acosh','asin ','acos ','atan ','sinh ','cosh ', &
!    & 'tanh ','sqrt ','fact ','sin  ','cos  ','tan  ','log  ', &
!    & 'exp  ','abs  ','ln   '/)
!character:: equation*(100)='cos(1)+acosh(2)+cos(3)+acos(4)'
!character,pointer:: inventory_pointer(:)*(len(intrinsic_functions))
!integer,pointer:: locations_pointer(:)
!write(*,*)'1234567890123456789012345678901234567890'
!write(*,*)trim(equation)
!call EQP_StringFilter(equation,intrinsic_functions, &
!    locations_pointer,inventory_pointer)
!write(*,*)trim(equation)
!write(*,*)locations_pointer
!write(*,*)(trim(inventory_pointer(i))//' ',i=1,size(inventory_pointer))
!write(*,*)

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: WORDS(:)*(*)
CHARACTER,INTENT(INOUT):: STRING*(*)
CHARACTER,POINTER:: INVENTORY_POINTER(:)*(*)
INTEGER,POINTER:: LOCATIONS_POINTER(:)

! Private variables:
INTEGER:: I,J
LOGICAL:: WORD_FOUND
CHARACTER:: KEY*(LEN(WORDS)),INTERNAL_WORDS(SIZE(WORDS))*(LEN(WORDS))

! Variable initialization:
WORD_FOUND=.FALSE.
NULLIFY(INVENTORY_POINTER,LOCATIONS_POINTER)
INTERNAL_WORDS=WORDS

! ------------------------------------------------------------------------------

! Error control:

IF (SIZE(WORDS)==0) THEN
WRITE(*,*)"EQP_StringFilter"
WRITE(*,*)"ERROR: empty input array."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY(LEN_TRIM(WORDS)==0)) THEN
WRITE(*,*)"EQP_StringFilter"
WRITE(*,*)"ERROR: missing character variable"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (LEN_TRIM(STRING)==0) THEN
WRITE(*,*)"EQP_StringFilter"
WRITE(*,*)"ERROR: empty string."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! The following error checks can be obnoxious sometimes:

!if (all(len_trim(words)>len_trim(string))) then
!write(*,*)"EQP_StringFilter"
!write(*,*)"ERROR: string is too small to contain the given words."
!write(*,*)"string: ",trim(string)
!write(*,*)"words: ",(trim(words(i))//' ',i=1,size(words))
!write(*,*)"Program terminated."
!read(*,*)
!stop
!end if

!if (any(len_trim(words)>len_trim(string))) then
!write(*,*)"EQP_StringFilter"
!write(*,*)"ERROR: string is too small to contain some of the given words."
!write(*,*)"string: ",trim(string)
!write(*,*)"words: ",(trim(words(i))//' ',i=1,size(words))
!write(*,*)"Continue..."
!if (warnings_pause) read(*,*)
!end if

! ------------------------------------------------------------------------------

! The algorithm applied is robust only when array "words" is ordered so that
! the variables with the smallest len_trim() value come first.
DO I=1,SIZE(WORDS)-1
    IF (LEN_TRIM(WORDS(I))>LEN_TRIM(WORDS(I+1))) THEN
        CALL HeapsortCharacterLength(INTERNAL_WORDS)
        EXIT
    END IF
END DO

! The Fortran intrinsic functions "scan" and "index" do not exhibit the behavior
! desired in the present case.

DO I=SIZE(INTERNAL_WORDS),1,-1

    IF (LEN_TRIM(INTERNAL_WORDS(I))>LEN_TRIM(STRING)) CYCLE
    KEY=''

    DO J=1,LEN_TRIM(STRING)

        KEY=KEY(2:LEN_TRIM(INTERNAL_WORDS(I)))//STRING(J:J)

        IF (TRIM(KEY)==TRIM(INTERNAL_WORDS(I))) THEN
        WORD_FOUND=.TRUE.
        CALL LinkedListCharacter &
            & (CHARACTER_VALUE=TRIM(INTERNAL_WORDS(I)))
        CALL LinkedListInteger &
            & (NUMERICAL_VALUE=J-LEN_TRIM(INTERNAL_WORDS(I))+1)
        STRING(J-LEN_TRIM(INTERNAL_WORDS(I))+1:J)=''
        END IF

    END DO

END DO

IF (WORD_FOUND) THEN
    CALL LinkedListCharacter(LIST_POINTER=INVENTORY_POINTER)
    CALL LinkedListInteger(LIST_POINTER=LOCATIONS_POINTER)
ELSE
! Output pointers should not be left undefined if there is nothing to output.
    ALLOCATE(INVENTORY_POINTER(0))
    ALLOCATE(LOCATIONS_POINTER(0))
END IF

END SUBROUTINE EQP_STRINGFILTER

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


END SUBMODULE SubLib_EquationParsing
