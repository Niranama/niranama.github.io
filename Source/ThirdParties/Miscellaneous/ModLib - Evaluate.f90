
MODULE ModLib_Evaluate

!** PURPOSE OF THIS MODULE:
    ! contains routines that evaluate mathematical expressions and
    ! perform string manipulations by George Benthien
    ! Note: The two modules are combined into one here.

!** SYNOPSIS:
    !---  Fortran Character String Utilities ---
    ! A number of executable programs accept input from a user-provided
    ! ASCII text file. These input files specify which operations to
    ! perform and supply the necessary input data. The string routines
    ! supplied here were developed to simplify the processing of these
    ! input files. In particular, there are subroutines for parsing
    ! lines in the input file into their component parts (e.g., commands,
    ! arguments, parameters) based on prescribed delimiters. There are
    ! also subroutines for performing various string manipulations
    ! as well as routines for evaluating mathematical expressions
    ! contained in strings. The Fortran routines are contained in
    ! two modules. The module strings contains the parsing and basic
    ! manipulation routines. The module evaluate contains routines
    ! for evaluating mathematical expressions contained in strings.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE   ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures for expression and equation evaluations
    PUBLIC :: ValueP
    PUBLIC :: EvalExpr
    PUBLIC :: DefParam
    PUBLIC :: EvalEqn
    PUBLIC :: GetParam
    PUBLIC :: ListVar
    PUBLIC :: IErr
    ! procedure for string handling and manipulation
    PUBLIC :: Parse
    PUBLIC :: Compact
    PUBLIC :: RemoveSpaces
    PUBLIC :: Value
    PUBLIC :: ShiftString
    PUBLIC :: InsertString
    PUBLIC :: DeleteSubString
    PUBLIC :: DeleteAllSubString
    PUBLIC :: UpperCase
    PUBLIC :: LowerCase
    PUBLIC :: ReadLine
    PUBLIC :: Match
    PUBLIC :: WriteNumber
    PUBLIC :: TrimZero
    PUBLIC :: WriteQ
    PUBLIC :: IsLetter
    PUBLIC :: IsDigit
    PUBLIC :: Split
    PUBLIC :: RemoveBackslash
    PUBLIC :: S_Replace_One

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    INTEGER, PARAMETER  :: NumTok = 100 ! Maximum number of tokens
    INTEGER, PARAMETER  :: SP = KIND(1.0E0)
    INTEGER, PARAMETER  :: DP = KIND(1.0D0)
    INTEGER, PARAMETER  :: I4B = SELECTED_INT_KIND(9)
    INTEGER, PARAMETER  :: I8B = SELECTED_INT_KIND(18)

!** DERIVED TYPE DEFINITIONS
    TYPE Item
        CHARACTER(LEN=24)   :: Char
        CHARACTER           :: Type
    END TYPE Item

    TYPE Param
        CHARACTER(LEN=24)   :: Symbol
        COMPLEX(DP)         :: Value
    END TYPE Param

!** INTERFACE DEFINITIONS:
    INTERFACE DefParam
        MODULE PROCEDURE StrDef       ! value given by expression
        MODULE PROCEDURE ValDef_Dc    ! Double precision complex value
        MODULE PROCEDURE ValDef_Sc    ! Single precision complex value
        MODULE PROCEDURE ValDef_Dr    ! Double precision real value
        MODULE PROCEDURE ValDef_Sr    ! Single precision real value
        MODULE PROCEDURE ValDef_Di    ! Double precision integer value
        MODULE PROCEDURE ValDef_Si    ! Single precision integer value
    END INTERFACE

    INTERFACE EvalExpr
        MODULE PROCEDURE EvalExpr_Dc  ! Double precision complex result
        MODULE PROCEDURE EvalExpr_Sc  ! Single precision complex result
        MODULE PROCEDURE EvalExpr_Dr  ! Double precision real result
        MODULE PROCEDURE EvalExpr_Sr  ! Single precision real result
        MODULE PROCEDURE EvalExpr_Di  ! Double precision integer result
        MODULE PROCEDURE EvalExpr_Si  ! Single precision integer result
    END INTERFACE

    INTERFACE GetParam
        MODULE PROCEDURE GetParam_Dc  ! Double precision complex result
        MODULE PROCEDURE GetParam_Sc  ! Single precision complex result
        MODULE PROCEDURE GetParam_Dr  ! Double precision real result
        MODULE PROCEDURE GetParam_Sr  ! Single precision real result
        MODULE PROCEDURE GetParam_Di  ! Double precision integer result
        MODULE PROCEDURE GetParam_Si  ! Single precision integer result
    END INTERFACE

    INTERFACE Value  ! Generic operator for converting a number string to a
                     ! number. Calling syntax is 'call value(numstring,number,ios)'
                     ! where 'numstring' is a number string and 'number' is a
                     ! real number or an integer (single or double precision).
       MODULE PROCEDURE Value_Dr
       MODULE PROCEDURE Value_Sr
       MODULE PROCEDURE Value_Di
       MODULE PROCEDURE Value_Si
    END INTERFACE

    INTERFACE WriteNumber  ! Generic interface for writing a number to a string. The
                        ! number is left justified in the string. The calling syntax
                        ! is 'call WriteNumber(number,string,format)' where 'number' is
                        ! a real number or an integer, 'string' is a character string
                        ! containing the result, and 'format' is the format desired,
                        ! e.g., 'e15.6' or 'i5'.
       MODULE PROCEDURE Write_Dr
       MODULE PROCEDURE Write_Sr
       MODULE PROCEDURE Write_Di
       MODULE PROCEDURE Write_Si
    END INTERFACE

    INTERFACE WriteQ  ! Generic interface equating a name to a numerical value. The
                      ! calling syntax is 'call WriteQ(unit,name,value,format)' where
                      ! unit is the integer output unit number, 'name' is the variable
                      ! name, 'value' is the real or integer value of the variable,
                      ! and 'format' is the format of the value. The result written to
                      ! the output unit has the form <name> = <value>.
       MODULE PROCEDURE WriteQ_Dr
       MODULE PROCEDURE WriteQ_Sr
       MODULE PROCEDURE WriteQ_Di
       MODULE PROCEDURE WriteQ_Si
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    SAVE
    TYPE(Param)     :: Params(100)          ! Symbol table
    INTEGER         :: nParams=0
    INTEGER         :: Itop
    INTEGER         :: Ibin
    COMPLEX(DP)     :: ValStack(NumTok)     ! Stack used in evaluation of expression
    TYPE(Item)      :: OpStack(NumTok)      ! Operator stack used in conversion to postfix
    INTEGER         :: IErr                 ! Error flag

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           EXPRESSION EVALUATION ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!** DESCRIPTION FOR EVALUATION MODULE:
    ! This module contains routines for evaluating mathematical expressions
    ! contained in strings, e.g., "cos((a+b)^2+1.6*log(c))". These strings
    ! expressions can contain numbers (e.g., 1.5 or 1.5e6), previously defined
    ! parameters (like a,b,c above), arithmetic operators (+,-,*,/,^), and any
    ! of the functions (cos, sin, sqrt, exp, log, ln, abs, ang, real, imag, conjg,
    ! complex). They can also contain nested levels of parentheses. The module
    ! also contains routines for defining and retrieving user-specified parameters.
    ! The quantity pi and the imaginary unit i are pre-defined parameters.

SUBROUTINE EvalExpr_DC(Expr,val)    ! Evaluate expression Expr for
                                    ! val double precision complex

CHARACTER (LEN=*),INTENT(IN) :: Expr
COMPLEX(DP) :: val
CHARACTER (LEN=LEN(Expr)+1) :: tempstr
INTEGER :: isp(NumTok)          ! On stack priority of operators in OpStack
INTEGER :: lstr
COMPLEX(DP) :: cval,oper1,oper2
REAL(DP) :: valr,vali
TYPE(Item):: token(NumTok)      ! List of tokens ( a token is an operator or
                                ! operand) in postfix order
TYPE(Item) :: x,junk,tok
INTEGER(I4B) :: ntok, icp, insp, isum, i

IErr=0
token(1:)%Char=' '

IF(nParams == 0) THEN                  ! Initialize symbol table
  Params(1)%Symbol='PI'
  Params(1)%Value=(3.14159265358979_DP,0.0_DP)
  Params(2)%Symbol='I'
  Params(2)%Value=(0.0_DP,1.0_DP)
  nParams=2
END IF

IF(LEN_TRIM(Expr) == 0) THEN           ! expression empty
  IErr=1
  WRITE(*,*) 'Error: expression being evaluated is empty'
  RETURN
END IF

tempstr=ADJUSTL(Expr)
CALL RemoveSpaces(tempstr)   ! Removes spaces, tabs, and control characters

! ****************************************************************************
! STEP 1:  Convert string to token array. Each token is either an operator or
!          an operand. Token array will be in postfix (reverse Polish) order.
!*****************************************************************************

ntok=0
Ibin=0
Itop=0
DO
  lstr=LEN_TRIM(tempstr)
  CALL Get_Next_Token(tempstr(1:lstr),tok,icp,insp)
  SELECT CASE(tok%Type)
  CASE('S')
    ntok=ntok+1
    token(ntok)=tok
  CASE('E')
    DO
      IF(Itop < 1)EXIT
      CALL PopOp(x)        ! Output remaining operators on stack
      ntok=ntok+1
      token(ntok)=x
    END DO
    ntok=ntok+1
    token(ntok)=tok
    EXIT
  CASE('R')  ! Token is right parenenthesis
    DO
      IF(Itop .le. 0 .OR. OpStack(Itop)%Type == 'L') EXIT  ! Output operators on stack down
      CALL PopOp(x)                       ! to left parenthesis
      ntok=ntok+1
      token(ntok)=x
    END DO
    CALL PopOp(junk)                      ! Remove left parenthesis from stack
    IF(Itop .gt. 0 .AND. OpStack(Itop)%Type == 'F') THEN    ! Output function name if present
      CALL PopOp(x)
      ntok=ntok+1
      token(ntok)=x
    END IF
  CASE('D')  ! Token is comma
    DO
      IF(Itop .le. 0 .OR. OpStack(Itop)%Type == 'L') EXIT  ! Output operators on stack down
      CALL PopOp(x)                       ! to left parenthesis
      ntok=ntok+1
      token(ntok)=x
    END DO
  CASE('U','B','L','F') ! Token is operator, left parenthesis or function name
    DO
      IF(Itop .le. 0 .OR. isp(Itop) < icp) EXIT            ! Output operators on stack having
      CALL PopOp(x)                       ! an instack priority that is
      ntok=ntok+1                         ! greater than or equal to the
      token(ntok)=x                       ! priority of the incoming operator
    END DO
    CALL PushOp(tok)     ! Put incoming operator on stack
    isp(Itop)=insp
  END SELECT
END DO

isum=0                                 ! Error check for matching parentheses
DO i=1,ntok
  IF(token(i)%Type == 'L' ) isum=isum+1
  IF(token(i)%Type == 'R' ) isum=isum-1
END DO
IF(isum /= 0) THEN
  IErr=2
  WRITE(*,*) 'Error in the evaluation of the expression ',TRIM(Expr)
  WRITE(*,*) "Parentheses don't match"
  WRITE(*,*)
  RETURN
END IF


!*****************************************************************************
! STEP 2: Evaluate token string in postfix order
!*****************************************************************************

Itop=0
DO i=1,ntok
  x=token(i)
  SELECT CASE(x%Type)
  CASE('E')  ! Token is end token
    IF(Itop>1) THEN
      IErr=12
      WRITE(*,*) 'Error: missing operator in expression ',TRIM(Expr)
      WRITE(*,*)
      RETURN
    END IF
    CALL PopVal(val)               ! Final result left on stack of values
    EXIT
  CASE('S')  ! Token is operand
    CALL ValueP(x%Char,cval)       ! Evaluate operand
    IF(IErr/=0) RETURN
    CALL PushVal(cval)             ! Put value of operand on stack
  CASE('B')  ! Token is a binary operator
    IF(Itop < 2) THEN
      IErr=5
      WRITE(*,*) 'Error in evaluation of expression ',TRIM(Expr)
      WRITE(*,*) 'Less than two operands for binary operator  '&
                 ,TRIM(x%Char)
      WRITE(*,*)
      RETURN
    END IF
    CALL PopVal(oper1)             ! Pull off top two values from stack
    CALL PopVal(oper2)
    SELECT CASE(TRIM(x%Char))      ! Perform operation on values
    CASE('^')
      cval=oper2**oper1
    CASE('*')
      cval=oper2*oper1
    CASE('/')
      IF(oper1 == (0._DP,0._DP)) THEN
        IErr=10
        WRITE(*,*) 'Error in expression ',TRIM(Expr)
        WRITE(*,*) 'Division by zero'
        WRITE(*,*)
        RETURN
      END IF
      cval=oper2/oper1
    CASE('+')
      cval=oper2+oper1
    CASE('-')
      cval=oper2-oper1
    END SELECT
    CALL PushVal(cval)             ! Put result back on stack
  CASE('U')  ! Token is unary operator
    IF(Itop == 0) THEN
      IErr=6
      WRITE(*,*) 'Error in expression ',TRIM(Expr)
      WRITE(*,*) 'No operand for unary operator ',TRIM(x%Char)
      WRITE(*,*)
      RETURN
    ELSE
      CALL PopVal(oper1)           ! Pull top value off stack
    END IF
    SELECT CASE(TRIM(x%Char))      ! Operate on value
    CASE('+')
      cval=oper1
    CASE('-')
      cval=-oper1
    END SELECT
    CALL PushVal(cval)             ! Put result back on stack
  CASE('F')  ! Token is a function name
    IF(Itop == 0) THEN
      IErr=7
      WRITE(*,*) 'Error in expression ',TRIM(Expr)
      WRITE(*,*) 'Missing argument(s) for function ',TRIM(x%Char)
      WRITE(*,*)
      RETURN
    ELSE
      CALL PopVal(oper1)           ! Pull top value off stack
    END IF
    tempstr=UpperCase(x%Char)
    SELECT CASE(TRIM(tempstr))      ! Evaluate function
    CASE('SIN')
      cval=SIN(oper1)
    CASE('COS')
      cval=COS(oper1)
    CASE('TAN')
      oper2=COS(oper1)
      IF(ABS(oper2) == 0.0_DP) THEN
        IErr=14
        WRITE(*,*) 'Error: argument of tan function a multiple',&
        ' of pi/2 in expression ',TRIM(Expr)
        WRITE(*,*)
        RETURN
      ELSE
        cval=SIN(oper1)/oper2
      END IF
    CASE('SQRT')
      IF(REAL(oper1,DP) < 0. .AND. AIMAG(oper1)==0.) THEN
        IErr=9
        WRITE(*,*) 'Warning: square root of negative real number',&
                   ' in expression ',TRIM(Expr)
        WRITE(*,*)
      END IF
      cval=SQRT(oper1)
    CASE('ABS')
      cval=ABS(oper1)
    CASE('LN')
      IF(REAL(oper1,DP) <= 0. .AND. AIMAG(oper1)==0.) THEN
        IErr=8
        WRITE(*,*) 'Error: negative real or zero argument for',&
                   ' natural logarithm in expression ',TRIM(Expr)
        WRITE(*,*)
        RETURN
      END IF
      cval=LOG(oper1)
    CASE('LOG')
      IF(REAL(oper1,DP) <= 0. .AND. AIMAG(oper1)==0.) THEN
        IErr=8
        WRITE(*,*) 'Error: negative real or zero argument for base',&
                   '10 logarithm in expression ',TRIM(Expr)
        WRITE(*,*)
        RETURN
      END IF
      cval=LOG(oper1)/2.30258509299405_DP
    CASE('EXP')
      cval=EXP(oper1)
    CASE('COMPLEX')
      IF(Itop == 0) THEN
        IErr=7
        WRITE(*,*) 'Error in expression ',TRIM(Expr)
        WRITE(*,*) 'Missing argument(s) for function ',TRIM(x%Char)
        WRITE(*,*)
        RETURN
      ELSE
        CALL PopVal(oper2)  ! Pull second argument off stack
      END IF
      valr=REAL(oper2,DP)
      vali=REAL(oper1,DP)
      cval=CMPLX(valr,vali,DP)
    CASE('CONJG')
      cval=CONJG(oper1)
    CASE('ANG')
      cval=ATAN2(AIMAG(oper1),REAL(oper1,DP))
    CASE('REAL')
      cval=REAL(oper1,DP)
    CASE('IMAG')
      cval=AIMAG(oper1)
    CASE DEFAULT ! Undefined function
      IErr=13
      WRITE(*,*) 'Error: the function ',TRIM(x%Char), ' is undefined',&
                 ' in the expression ',TRIM(Expr)
      WRITE(*,*)
      RETURN
    END SELECT
    CALL PushVal(cval)    ! Put result back on stack
  END SELECT
END DO

END SUBROUTINE EvalExpr_Dc

!**********************************************************************

SUBROUTINE Get_Next_Token(str,tok,icp,isp)

CHARACTER(LEN=*) :: str
CHARACTER :: cop,chtemp
TYPE(Item) :: tok
INTEGER :: icp
INTEGER(I4B) :: isp, lstr, ipos, ntok, inext

lstr=LEN_TRIM(str)
IF(lstr == 0) THEN
  tok%Char='#'             ! Output end token
  tok%Type='E'
  RETURN
END IF
ipos=SCAN(str,'+-*/^(),')  ! Look for an arithmetic operator
                           ! + - * / ^ ( ) or ,
IF (ipos > 0) cop=str(ipos:ipos)
SELECT CASE (ipos)
CASE(0)    ! Operators not present
  ntok=ntok+1
  tok%Char=str
  tok%Type='S'
  str=''
  icp=0
  isp=0
CASE(1)
  tok%Char=cop
  SELECT CASE(cop)
  CASE('+','-')
    IF(Ibin==0) THEN
      tok%Type='U'
      icp=4
      isp=3
    ELSE
      tok%Type='B'
      icp=1
      isp=1
    END IF
    Ibin=0
  CASE('*','/')
    tok%Type='B'
    icp=2
    isp=2
    Ibin=0
  CASE('^')
    tok%Type='B'
    icp=4
    isp=3
    Ibin=0
  CASE('(')
    tok%Type='L'
    icp=4
    isp=0
    Ibin=0
  CASE(')')
    tok%Type='R'
    icp=0
    isp=0
    Ibin=1
  CASE(',')
    tok%Type='D'
    icp=0
    isp=0
    Ibin=0
  END SELECT
  str=str(2:)
CASE(2:)
  SELECT CASE(cop)
  CASE('(')
    tok%Char=str(1:ipos-1)
    tok%Type='F'
    icp=4
    isp=0
    Ibin=0
    str=str(ipos:)
  CASE('+','-')
    chtemp=UpperCase(str(ipos-1:ipos-1))
    IF(IsLetter(str(1:1)).EQV..TRUE. .OR. chtemp/='E') THEN
      tok%Char=str(1:ipos-1)
      tok%Type='S'
      icp=0
      isp=0
      Ibin=1
      str=str(ipos:)
    ELSE
      inext=SCAN(str(ipos+1:),'+-*/^(),')
      IF(inext==0) THEN
        tok%Char=str
        tok%Type='S'
        icp=0
        isp=0
        Ibin=0
        str=''
      ELSE
        tok%Char=str(1:ipos+inext-1)
        tok%Type='S'
        icp=0
        isp=0
        Ibin=1
        str=str(ipos+inext:)
      END IF
    END IF
  CASE DEFAULT
    tok%Char=str(1:ipos-1)
    tok%Type='S'
    icp=0
    isp=0
    Ibin=1
    str=str(ipos:)
  END SELECT
END SELECT

END SUBROUTINE Get_Next_Token

!**********************************************************************

SUBROUTINE EvalExpr_SC(Expr,val)    ! Evaluate expression Expr for
                                    ! val single precision complex
CHARACTER(LEN=*) :: Expr
COMPLEX(SP) :: val
COMPLEX(DP) :: vald

CALL EvalExpr_Dc(Expr,vald)
val=vald

END SUBROUTINE EvalExpr_Sc

!**********************************************************************

SUBROUTINE EvalExpr_SR(Expr,val)    ! Evaluate expression Expr for
                                    ! val single precision real
CHARACTER(LEN=*) :: Expr
REAL(SP) :: val
COMPLEX(DP) :: vald

CALL EvalExpr_Dc(Expr,vald)
val=REAL(vald)

END SUBROUTINE EvalExpr_Sr

!**********************************************************************

SUBROUTINE EvalExpr_DR(Expr,val)    ! Evaluate expression Expr for
                                    ! val double precision real
CHARACTER(LEN=*) :: Expr
REAL(DP) :: val
COMPLEX(DP) :: vald

CALL EvalExpr_Dc(Expr,vald)
val=REAL(vald,DP)

END SUBROUTINE EvalExpr_Dr

!**********************************************************************

SUBROUTINE EvalExpr_SI(Expr,ival)   ! Evaluate expression Expr for
                                    ! ival single precision integer
CHARACTER(LEN=*) :: Expr
INTEGER(I4B) :: ival
COMPLEX(DP) :: vald

CALL EvalExpr_Dc(Expr,vald)
ival=NINT(REAL(vald,DP),I4B)

END SUBROUTINE EvalExpr_Si

!**********************************************************************

SUBROUTINE EvalExpr_DI(Expr,ival)   ! Evaluate expression Expr for
                                    ! ival double precision integer
CHARACTER(LEN=*) :: Expr
INTEGER(I8B) :: ival
COMPLEX(DP) :: vald

CALL EvalExpr_Dc(Expr,vald)
ival=NINT(REAL(vald,DP),I8B)

END SUBROUTINE EvalExpr_Di

!**********************************************************************

SUBROUTINE ValDef_DC(sym,val)    ! Associates sym with val in symbol table,
                                 ! val double precision complex
CHARACTER(LEN=*) :: sym
CHARACTER(LEN=LEN_TRIM(sym)) :: usym
COMPLEX(DP) :: val
INTEGER(I4B) :: i

IErr=0
IF(nParams == 0) THEN               ! Initialize symbol table
  Params(1)%Symbol='PI'
  Params(1)%Value=(3.14159265358979_DP,0.0_DP)
  Params(2)%Symbol='I'
  Params(2)%Value=(0.0_DP,1.0_DP)
  nParams=2
END IF

! Assign val to sym if sym is already in symbol table
usym=UpperCase(sym)
IF(IsLetter(sym(1:1)).EQV..FALSE. .OR. LEN_TRIM(sym)>24) THEN
  IErr=11
  WRITE(*,*) 'Error: symbol ',TRIM(sym),' has improper format'
  WRITE(*,*)
  RETURN
END IF
DO i=1,nParams
  IF(TRIM(usym)==TRIM(Params(i)%Symbol)) THEN
    Params(i)%Value=val
    RETURN
  END IF
END DO

nParams=nParams+1    ! Otherwise assign val to new symbol sym
Params(nParams)%Symbol=usym
Params(nParams)%Value=val

END SUBROUTINE ValDef_Dc

!**********************************************************************

SUBROUTINE ValDef_SC(sym,val)     ! Associates sym with val in symbol table,
                                  ! val single precision complex
CHARACTER(LEN=*) :: sym
COMPLEX(SP) :: val
COMPLEX(DP) :: vald

vald=val
CALL ValDef_Dc(sym,vald)

END SUBROUTINE ValDef_Sc

!**********************************************************************

SUBROUTINE ValDef_DR(sym,val)    ! Associates sym with val in symbol table,
                                 ! val double precision real
CHARACTER(LEN=*) :: sym
REAL(DP) :: val
COMPLEX(DP) :: vald

vald=CMPLX(val,0.0_DP,DP)
CALL ValDef_Dc(sym,vald)

END SUBROUTINE ValDef_Dr

!**********************************************************************

SUBROUTINE ValDef_SR(sym,val)    ! Associates sym with val in symbol table,
                                 ! val single precision real
CHARACTER(LEN=*) :: sym
REAL(SP) :: val
COMPLEX(DP) :: vald

vald=CMPLX(val,0.0,DP)
CALL ValDef_Dc(sym,vald)

END SUBROUTINE ValDef_Sr

!**********************************************************************

SUBROUTINE ValDef_DI(sym,ival)   ! Associates sym with ival in symbol table,
                                 ! ival double precision integer
CHARACTER(LEN=*) :: sym
INTEGER(I8B) :: ival
COMPLEX(DP) :: vald

vald=CMPLX(REAL(ival,DP),0.0_DP,DP)
CALL ValDef_Dc(sym,vald)

END SUBROUTINE ValDef_Di

!**********************************************************************

SUBROUTINE ValDef_SI(sym,ival)   ! Associates sym with ival in symbol table,
                                 ! ival single precision integer
CHARACTER(LEN=*) :: sym
INTEGER(I4B) :: ival
COMPLEX(DP) :: vald

vald=CMPLX(REAL(ival,DP),0.0,DP)
CALL ValDef_Dc(sym,vald)

END SUBROUTINE ValDef_Si

!**********************************************************************

SUBROUTINE StrDef(sym,Expr)      ! Associates sym with the value of the
                                 ! expression Expr

CHARACTER(LEN=*) :: sym,Expr
COMPLEX(DP) :: val

IF(nParams == 0) THEN            ! Initialize symbol table
  Params(1)%Symbol='PI'
  Params(1)%Value=(3.14159265358979_DP,0.0_DP)
  Params(2)%Symbol='I'
  Params(2)%Value=(0.0_DP,1.0_DP)
  nParams=2
END IF

CALL EvalExpr_Dc(Expr,val)       ! val is value of expression Expr
IF(IErr==0 .OR. IErr==9) THEN
  CALL ValDef_Dc(sym,val)          ! Assign val to symbol sym
END IF

END SUBROUTINE StrDef

!**********************************************************************

SUBROUTINE ValueP(xinchar,cval)  ! Finds double precision complex value
                                 ! corresponding to number string xinchar
                                 ! or value in symbol table corresponding
                                 ! to symbol name xinchar.

CHARACTER (LEN=*):: xinchar
COMPLEX(DP) :: cval
REAL(DP) :: rval
INTEGER(I4B) :: ios

IErr=0

IF(IsLetter(xinchar(1:1)).EQV..TRUE.) THEN   ! xinchar is a symbol
  CALL GetParam(xinchar,cval)
ELSE                               ! xinchar is a number string
  CALL Value(xinchar,rval,ios)     ! rval is the value of xinchar
  IF(ios > 0) THEN
    IErr=3
    WRITE(*,*) 'Error: number string ',TRIM(xinchar),' does not correspond to a valid number'
    WRITE(*,*)
  END IF
  cval=CMPLX(rval,0.0_DP,DP)
  RETURN
END IF

END SUBROUTINE ValueP

!**********************************************************************

SUBROUTINE PushOp(op)  ! Puts an operator on operator stack

TYPE(Item):: op

Itop=Itop+1
IF(Itop > NumTok) THEN
  WRITE(*,*) 'Error: operator stack overflow in evaluation of expression'
  WRITE(*,*)
  RETURN
END IF
OpStack(Itop)=op

END SUBROUTINE PushOp

!**********************************************************************

SUBROUTINE PopOp(op) ! Takes top operator of operator stack and assigns it to op

TYPE(Item):: op

op=OpStack(Itop)
Itop=Itop-1

END SUBROUTINE PopOp

!**********************************************************************

SUBROUTINE PushVal(val) ! Puts value on value stack

COMPLEX(DP) :: val

Itop=Itop+1
IF(Itop > NumTok) THEN
  WRITE(*,*) 'Error: value stack overflow in evaluation of expression'
  WRITE(*,*)
  RETURN
END IF
ValStack(Itop)=val

END SUBROUTINE PushVal

!**********************************************************************

SUBROUTINE PopVal(val) ! Takes top value off value stack and assigns it to val

COMPLEX(DP) :: val

val=ValStack(Itop)
Itop=Itop-1

END SUBROUTINE PopVal

!**********************************************************************

SUBROUTINE GetParam_DC(sym,var)  ! Find double precision complex value var
                                 ! corresponding to symbol sym

CHARACTER(LEN=*) :: sym
CHARACTER(LEN=LEN_TRIM(sym)) :: usym
COMPLEX(DP) :: var
INTEGER(I4B) :: ifind, j
IErr=0
sym=ADJUSTL(sym)
IF(IsLetter(sym(1:1)).EQV..FALSE. .OR. LEN_TRIM(sym)>24) THEN
  IErr=11
  WRITE(*,*) 'Error: symbol ',TRIM(sym),' has incorrect format'
  WRITE(*,*)
  RETURN
END IF
ifind=0
usym=UpperCase(sym)
DO j=1,nParams
  IF(TRIM(usym) == TRIM(Params(j)%Symbol)) THEN
    var=Params(j)%Value
    ifind=j
    EXIT
  END IF
END DO
IF(ifind == 0) THEN
  IErr=4
  WRITE(*,*) 'Error: symbol ',TRIM(sym), ' not in symbol table'
  WRITE(*,*)
  RETURN
END IF

END SUBROUTINE GetParam_Dc

!**********************************************************************

SUBROUTINE GetParam_SC(sym,var)  ! Find single precision complex value var
                                 ! corresponding to symbol sym


CHARACTER(LEN=*) :: sym
COMPLEX(SP) :: var
COMPLEX(DP) :: vard

CALL GetParam_Dc(sym,vard)
var=vard

END SUBROUTINE GetParam_Sc

!**********************************************************************

SUBROUTINE GetParam_DR(sym,var)  ! Find double precision real value var
                                 ! corresponding to symbol sym


CHARACTER(LEN=*) :: sym
REAL(DP) :: var
COMPLEX(DP) :: vard

CALL GetParam_Dc(sym,vard)
var=REAL(vard,DP)

END SUBROUTINE GetParam_Dr

!**********************************************************************

SUBROUTINE GetParam_SR(sym,var)  ! Find single precision real value var
                                 ! corresponding to symbol sym


CHARACTER(LEN=*) :: sym
REAL(SP) :: var
COMPLEX(DP) :: vard

CALL GetParam_Dc(sym,vard)
var=REAL(vard)

END SUBROUTINE GetParam_Sr

!**********************************************************************

SUBROUTINE GetParam_DI(sym,ivar)  ! Find double precision integer value ivar
                                  ! corresponding to symbol sym


CHARACTER(LEN=*) :: sym
INTEGER(I8B) :: ivar
COMPLEX(DP) :: vard

CALL GetParam_Dc(sym,vard)
ivar=NINT(REAL(vard,DP),I8B)

END SUBROUTINE GetParam_Di

!**********************************************************************

SUBROUTINE GetParam_SI(sym,ivar)  ! Find single precision integer value ivar
                                  ! corresponding to symbol sym

CHARACTER(LEN=*) :: sym
INTEGER(I4B) :: ivar
COMPLEX(DP) :: vard

CALL GetParam_Dc(sym,vard)
ivar=NINT(REAL(vard,DP),I4B)

END SUBROUTINE GetParam_Si

!**********************************************************************

SUBROUTINE EvalEqn(Eqn)  ! Evaluate an equation

CHARACTER(LEN=*) :: Eqn
CHARACTER(LEN=LEN(Eqn)) :: Args(2)
INTEGER(I4B) :: nArgs

CALL Parse(Eqn,'=',Args,nArgs)   ! Seperate right- and left-hand-sides
CALL DefParam(ADJUSTL(Args(1)),Args(2)) ! Evaluate right-hand-side and
                                        ! assign to symbol on the
                                        ! left-hand-side.
END SUBROUTINE EvalEqn

!**********************************************************************

SUBROUTINE ListVar      ! List all variables and their values
INTEGER(I4B) :: i

WRITE(*,'(/a)') ' VARIABLE LIST:'
IF(nParams == 0) THEN            ! Initialize symbol table
  Params(1)%Symbol='PI'
  Params(1)%Value=(3.14159265358979_DP,0.0_DP)
  Params(2)%Symbol='I'
  Params(2)%Value=(0.0_DP,1.0_DP)
  nParams=2
END IF
DO i=1,nParams
  WRITE(*,*) TRIM(Params(i)%Symbol),' = ',Params(i)%Value
END DO

END SUBROUTINE ListVar

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           STRING MANIPULATION ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!** DESCRIPTION FOR STRING MODULE:
    ! This module contains routines for parsing input lines such as
    !
    !                       cmd arg1, arg2, arg3
    !
    ! into the component strings "cmd", "arg1", "arg2", "arg3" based on
    ! prescribed delimiters such as commas and spaces. There are also
    ! routines for converting number strings into numerical values
    ! (integers, reals, single or double precision).
    ! Other routines in this module
    !   - convert strings to uppercase or lowercase
    !   - remove portions of strings
    !   - insert strings into other strings
    !   - find matching delimiters such as (), [], {}, <>
    !   - determine whether a character is a letter or a digit (0-9).

SUBROUTINE Parse(Str,Delims,Args,NArgs)

! Parses the string 'Str' into arguments Args(1), ..., Args(NArgs) based on
! the delimiters contained in the string 'Delims'. Preceding a delimiter in
! 'Str' by a backslash (\) makes this particular instance not a delimiter.
! The integer output variable NArgs contains the number of arguments found.

CHARACTER(LEN=*)                :: Str,Delims
CHARACTER(LEN=LEN_TRIM(Str))    :: StrSav
CHARACTER(LEN=*),DIMENSION(:)   :: Args
INTEGER                         :: Na, I, NArgs, LenStr

StrSav=Str
CALL Compact(Str)
Na=SIZE(Args)
DO I=1,Na
  Args(I)=' '
END DO
NArgs=0
LenStr=LEN_TRIM(Str)
IF(LenStr==0) RETURN
!k=0

DO
   IF(LEN_TRIM(Str) == 0) EXIT
   NArgs=NArgs+1
   CALL Split(Str,Delims,Args(NArgs))
   CALL RemoveBackslash(Args(NArgs))
END DO
Str=StrSav

END SUBROUTINE Parse

!**********************************************************************

SUBROUTINE Compact(Str)

! Converts multiple spaces and tabs to single spaces; deletes control characters;
! removes initial spaces.

CHARACTER(LEN=*)                :: Str
CHARACTER(LEN=1)                :: Ch
CHARACTER(LEN=LEN_TRIM(Str))    :: OutStr
INTEGER                         :: I, Ich, Isp, K, LenStr

Str=ADJUSTL(Str)
LenStr=LEN_TRIM(Str)
OutStr=' '
Isp=0
K=0

DO I=1,LenStr
  Ch=Str(I:I)
  Ich=IACHAR(Ch)

  SELECT CASE(Ich)

    CASE(9,32)     ! space or tab character
      IF(Isp==0) THEN
        K=K+1
        OutStr(K:K)=' '
      END IF
      Isp=1

    CASE(33:)      ! not a space, quote, or control character
      K=K+1
      OutStr(K:K)=Ch
      Isp=0

  END SELECT

END DO

Str=ADJUSTL(OutStr)

END SUBROUTINE Compact

!**********************************************************************

SUBROUTINE RemoveSpaces(Str)

! Removes spaces, tabs, and control characters in string Str

CHARACTER(LEN=*)                :: Str
CHARACTER(LEN=1)                :: Ch
CHARACTER(LEN=LEN_TRIM(Str))    :: OutStr
INTEGER                         :: I, Ich, K, LenStr

Str=ADJUSTL(Str)
LenStr=LEN_TRIM(Str)
OutStr=' '
K=0

DO I=1,LenStr
  Ch=Str(I:I)
  Ich=IACHAR(Ch)
  SELECT CASE(Ich)
    CASE(0:32)  ! space, tab, or control character
         CYCLE
    CASE(33:)
      K=K+1
      OutStr(K:K)=Ch
  END SELECT
END DO

Str=ADJUSTL(OutStr)

END SUBROUTINE RemoveSpaces

!**********************************************************************

SUBROUTINE Value_Dr(Str,RNum,Ios)

! Converts number string to a double precision real number

CHARACTER(LEN=*)    :: Str
REAL(DP)            :: RNum
INTEGER             :: Ios, ILen, Ipos

ILen=LEN_TRIM(Str)
Ipos=SCAN(Str,'Ee')
IF(.NOT.IsDigit(Str(ILen:ILen)) .AND. Ipos/=0) THEN
   Ios=3
   RETURN
END IF
READ(Str,*,IOSTAT=Ios) RNum

END SUBROUTINE Value_Dr

!**********************************************************************

SUBROUTINE Value_Sr(Str,RNum,Ios)

! Converts number string to a single precision real number

CHARACTER(LEN=*)    :: Str
REAL(SP)            :: RNum
REAL(DP)            :: RNumd
INTEGER             :: Ios

CALL Value_Dr(Str,RNumd,Ios)
IF( ABS(RNumd) > HUGE(RNum) ) THEN
  Ios=15
  RETURN
END IF
IF( ABS(RNumd) < TINY(RNum) ) RNum=0.0_SP
RNum=RNumd

END SUBROUTINE Value_Sr

!**********************************************************************

SUBROUTINE Value_Di(Str,INum,Ios)

! Converts number string to a double precision integer value

CHARACTER(LEN=*)    :: Str
INTEGER(I8B)        :: INum
REAL(DP)            :: RNum
INTEGER             :: Ios

CALL Value_Dr(Str,RNum,Ios)
IF(ABS(RNum)>HUGE(INum)) THEN
  Ios=15
  RETURN
END IF
INum=NINT(RNum,I8B)

END SUBROUTINE Value_Di

!**********************************************************************

SUBROUTINE Value_Si(Str,INum,Ios)

! Converts number string to a single precision integer value

CHARACTER(LEN=*)    :: Str
INTEGER(I4B)        :: INum
REAL(DP)            :: RNum
INTEGER             :: Ios

CALL Value_Dr(Str,RNum,Ios)
IF(ABS(RNum)>HUGE(INum)) THEN
  Ios=15
  RETURN
END IF
INum=NINT(RNum,I4B)

END SUBROUTINE Value_Si

!**********************************************************************

SUBROUTINE ShiftString(Str,N)

! Shifts characters in in the string 'Str' n positions (positive values
! denote a right shift and negative values denote a left shift). Characters
! that are shifted off the end are lost. Positions opened up by the shift
! are replaced by spaces.

CHARACTER(LEN=*)    :: Str
INTEGER             :: N, NAbs, LenStr

LenStr=LEN(Str)
NAbs=ABS(N)
IF(NAbs>=LenStr) THEN
  Str=REPEAT(' ',LenStr)
  RETURN
END IF
IF(N<0) Str=Str(NAbs+1:)//REPEAT(' ',NAbs)  ! shift left
IF(N>0) Str=REPEAT(' ',NAbs)//Str(:LenStr-NAbs)  ! shift right
RETURN

END SUBROUTINE ShiftString

!**********************************************************************

SUBROUTINE InsertString(Str,Strins,Loc)

! Inserts the string 'Strins' into the string 'Str' at position 'loc'.
! Characters in 'Str' starting at position 'loc' are shifted right to
! make room for the inserted string. Trailing spaces of 'Strins' are
! removed prior to insertion

CHARACTER(LEN=*)        :: Str, Strins
CHARACTER(LEN=LEN(Str)) :: TempStr
INTEGER                 :: Loc, LenStr

LenStr=LEN_TRIM(Strins)
TempStr=Str(Loc:)
CALL ShiftString(TempStr,LenStr)
TempStr(1:LenStr)=Strins(1:LenStr)
Str(Loc:)=TempStr
RETURN

END SUBROUTINE InsertString

!**********************************************************************

SUBROUTINE DeleteSubString(Str,SubStr)

! Deletes first occurrence of substring 'SubStr' from string 'Str' and
! shifts characters left to fill hole. Trailing spaces or blanks are
! not considered part of 'SubStr'.

CHARACTER(LEN=*)    :: Str, SubStr
INTEGER             :: Ipos, LenSubStr

LenSubStr=LEN_TRIM(SubStr)
Ipos=INDEX(Str,SubStr)
IF(Ipos==0) RETURN
IF(Ipos == 1) THEN
   Str=Str(LenSubStr+1:)
ELSE
   Str=Str(:Ipos-1)//Str(Ipos+LenSubStr:)
END IF
RETURN

END SUBROUTINE DeleteSubString

!**********************************************************************

SUBROUTINE DeleteAllSubString(Str,SubStr)

! Deletes all occurrences of substring 'SubStr' from string 'Str' and
! shifts characters left to fill holes.

CHARACTER(LEN=*)    :: Str, SubStr
INTEGER             :: Ipos, LenSubStr

LenSubStr=LEN_TRIM(SubStr)
DO
   Ipos=INDEX(Str,SubStr)
   IF(Ipos == 0) EXIT
   IF(Ipos == 1) THEN
      Str=Str(LenSubStr+1:)
   ELSE
      Str=Str(:Ipos-1)//Str(Ipos+LenSubStr:)
   END IF
END DO
RETURN

END SUBROUTINE DeleteAllSubString

!**********************************************************************

FUNCTION UpperCase(Str) RESULT(UcStr)

! convert string to upper case

CHARACTER (LEN=*)               :: Str
CHARACTER (LEN=LEN_TRIM(Str))   :: UcStr
INTEGER                         :: I, ILen, IOffset, IQuote, Iqc, Iav

ILen=LEN_TRIM(Str)
IOffset=IACHAR('A')-IACHAR('a')
IQuote=0
UcStr=Str
DO I=1,ILen
  Iav=IACHAR(Str(I:I))
  IF(IQuote==0 .AND. (Iav==34 .OR.Iav==39)) THEN
    IQuote=1
    Iqc=Iav
    CYCLE
  END IF
  IF(IQuote==1 .AND. Iav==Iqc) THEN
    IQuote=0
    CYCLE
  END IF
  IF (IQuote==1) CYCLE
  IF(Iav >= IACHAR('a') .AND. Iav <= IACHAR('z')) THEN
    UcStr(I:I)=ACHAR(Iav+IOffset)
  ELSE
    UcStr(I:I)=Str(I:I)
  END IF
END DO
RETURN

END FUNCTION UpperCase

!**********************************************************************

FUNCTION LowerCase(Str) RESULT(LcStr)

! convert string to lower case

CHARACTER (LEN=*)               :: Str
CHARACTER (LEN=LEN_TRIM(Str))   :: LcStr
INTEGER                         :: I, ILen, IOffset, IQuote, Iqc, Iav

ILen=LEN_TRIM(Str)
IOffset=IACHAR('A')-IACHAR('a')
IQuote=0
LcStr=Str
DO I=1,ILen
  Iav=IACHAR(Str(I:I))
  IF(IQuote==0 .AND. (Iav==34 .OR.Iav==39)) THEN
    IQuote=1
    Iqc=Iav
    CYCLE
  END IF
  IF(IQuote==1 .AND. Iav==Iqc) THEN
    IQuote=0
    CYCLE
  END IF
  IF (IQuote==1) CYCLE
  IF(Iav >= IACHAR('A') .AND. Iav <= IACHAR('Z')) THEN
    LcStr(I:I)=ACHAR(Iav-IOffset)
  ELSE
    LcStr(I:I)=Str(I:I)
  END IF
END DO
RETURN

END FUNCTION LowerCase

!**********************************************************************

SUBROUTINE ReadLine(NUnitR,Line,Ios)

! Reads line from unit=NUnitR, ignoring blank lines
! and deleting comments beginning with an exclamation point(!)

CHARACTER (LEN=*)   :: Line
INTEGER             :: NUnitR, Ios, Ipos

DO
  READ(NUnitR,'(a)', IOSTAT=Ios) Line      ! read input line
  IF(Ios /= 0) RETURN
  Line=ADJUSTL(Line)
  Ipos=INDEX(Line,'!')
  IF(Ipos == 1) CYCLE
  IF(Ipos /= 0) Line=Line(:Ipos-1)
  IF(LEN_TRIM(Line) /= 0) EXIT
END DO
RETURN

END SUBROUTINE ReadLine

!**********************************************************************

SUBROUTINE Match(Str,Ipos,IMatch)

! Sets IMatch to the position in string of the delimiter matching the delimiter
! in position Ipos. Allowable delimiters are (), [], {}, <>.

CHARACTER(LEN=*)    :: Str
INTEGER             :: IMatch, Ipos
INTEGER             :: I, LenStr, IDelim2, IStart, IEnd, Inc, ISum
CHARACTER           :: Delim1, Delim2, Ch

LenStr=LEN_TRIM(Str)
Delim1=Str(Ipos:Ipos)
SELECT CASE(Delim1)
   CASE('(')
      IDelim2=IACHAR(Delim1)+1
      IStart=Ipos+1
      IEnd=LenStr
      Inc=1
   CASE(')')
      IDelim2=IACHAR(Delim1)-1
      IStart=Ipos-1
      IEnd=1
      Inc=-1
   CASE('[','{','<')
      IDelim2=IACHAR(Delim1)+2
      IStart=Ipos+1
      IEnd=LenStr
      Inc=1
   CASE(']','}','>')
      IDelim2=IACHAR(Delim1)-2
      IStart=Ipos-1
      IEnd=1
      Inc=-1
   CASE DEFAULT
      WRITE(*,*) Delim1,' is not a valid delimiter'
      RETURN
END SELECT
IF(IStart < 1 .OR. IStart > LenStr) THEN
   WRITE(*,*) Delim1,' has no matching delimiter'
   RETURN
END IF
Delim2=ACHAR(IDelim2) ! matching delimiter

ISum=1
DO I=IStart,IEnd,Inc
   Ch=Str(I:I)
   IF(Ch /= Delim1 .AND. Ch /= Delim2) CYCLE
   IF(Ch == Delim1) ISum=ISum+1
   IF(Ch == Delim2) ISum=ISum-1
   IF(ISum == 0) EXIT
END DO
IF(ISum /= 0) THEN
   WRITE(*,*) Delim1,' has no matching delimiter'
   RETURN
END IF
IMatch=I

RETURN

END SUBROUTINE Match

!**********************************************************************

SUBROUTINE Write_Dr(RNum,Str,Fmt)

! Writes double precision real number RNum to string Str using format Fmt

REAL(DP)            :: RNum
CHARACTER(LEN=*)    :: Str, Fmt
CHARACTER(LEN=80)   :: Formt

Formt='('//TRIM(Fmt)//')'
WRITE(Str,Formt) RNum
Str=ADJUSTL(Str)

END SUBROUTINE Write_Dr

!***********************************************************************

SUBROUTINE Write_Sr(RNum,Str,Fmt)

! Writes single precision real number RNum to string Str using format Fmt

REAL(SP)            :: RNum
CHARACTER(LEN=*)    :: Str, Fmt
CHARACTER(LEN=80)   :: Formt

Formt='('//TRIM(Fmt)//')'
WRITE(Str,Formt) RNum
Str=ADJUSTL(Str)

END SUBROUTINE Write_Sr

!***********************************************************************

SUBROUTINE Write_Di(INum,Str,Fmt)

! Writes double precision integer INum to string Str using format Fmt

INTEGER(I8B)        :: INum
CHARACTER(LEN=*)    :: Str, Fmt
CHARACTER(LEN=80)   :: Formt

Formt='('//TRIM(Fmt)//')'
WRITE(Str,Formt) INum
Str=ADJUSTL(Str)

END SUBROUTINE Write_Di

!***********************************************************************

SUBROUTINE Write_Si(INum,Str,Fmt)

! Writes single precision integer INum to string Str using format Fmt

INTEGER(I4B)        :: INum
CHARACTER(LEN=*)    :: Str, Fmt
CHARACTER(LEN=80)   :: Formt

Formt='('//TRIM(Fmt)//')'
WRITE(Str,Formt) INum
Str=ADJUSTL(Str)

END SUBROUTINE Write_Si

!***********************************************************************

SUBROUTINE TrimZero(Str)

! Deletes nonsignificant trailing zeroes from number string Str. If number
! string ends in a decimal point, one trailing zero is added.

CHARACTER(LEN=*)    :: Str
CHARACTER           :: Ch
CHARACTER(LEN=10)   :: Exp
INTEGER             :: I, Ipos, LenStr

Ipos=SCAN(Str,'eE')
IF(Ipos>0) THEN
   Exp=Str(Ipos:)
   Str=Str(1:Ipos-1)
END IF
LenStr=LEN_TRIM(Str)
DO I=LenStr,1,-1
   Ch=Str(I:I)
   IF(Ch=='0') CYCLE
   IF(Ch=='.') THEN
      Str=Str(1:I)//'0'
      IF(Ipos>0) Str=TRIM(Str)//TRIM(Exp)
      EXIT
   END IF
   Str=Str(1:I)
   EXIT
END DO
IF(Ipos>0) Str=TRIM(Str)//TRIM(EXP)

END SUBROUTINE TrimZero

!**********************************************************************

SUBROUTINE WriteQ_Dr(Unit,NameStr,Value,Fmt)

! Writes a string of the form <name> = value to unit

REAL(DP)            :: Value
INTEGER             :: Unit
CHARACTER(LEN=*)    :: NameStr, Fmt
CHARACTER(LEN=32)   :: TempStr

CALL WriteNumber(Value,TempStr,Fmt)
CALL TrimZero(TempStr)
WRITE(Unit,*) TRIM(NameStr)//' = '//TRIM(TempStr)

END SUBROUTINE WriteQ_Dr

!**********************************************************************

SUBROUTINE WriteQ_Sr(Unit,NameStr,Value,Fmt)

! Writes a string of the form <name> = value to unit

REAL(SP)            :: Value
INTEGER             :: Unit
CHARACTER(LEN=*)    :: NameStr, Fmt
CHARACTER(LEN=32)   :: TempStr

CALL WriteNumber(Value,TempStr,Fmt)
CALL TrimZero(TempStr)
WRITE(Unit,*) TRIM(NameStr)//' = '//TRIM(TempStr)

END SUBROUTINE WriteQ_Sr

!**********************************************************************

SUBROUTINE WriteQ_Di(Unit,NameStr,IValue,Fmt)

! Writes a string of the form <name> = ivalue to unit

INTEGER(I8B)        :: IValue
INTEGER             :: Unit
CHARACTER(LEN=*)    :: NameStr, Fmt
CHARACTER(LEN=32)   :: TempStr

CALL WriteNumber(IValue,TempStr,Fmt)
CALL TrimZero(TempStr)
WRITE(Unit,*) TRIM(NameStr)//' = '//TRIM(TempStr)

END SUBROUTINE WriteQ_Di

!**********************************************************************

SUBROUTINE WriteQ_Si(Unit,NameStr,IValue,Fmt)

! Writes a string of the form <name> = ivalue to unit

INTEGER(I4B)        :: IValue
INTEGER             :: Unit
CHARACTER(LEN=*)    :: NameStr, Fmt
CHARACTER(LEN=32)   :: TempStr

CALL WriteNumber(IValue,TempStr,Fmt)
CALL TrimZero(TempStr)
WRITE(Unit,*) TRIM(NameStr)//' = '//TRIM(TempStr)

END SUBROUTINE WriteQ_Si

!**********************************************************************

FUNCTION IsLetter(Ch) RESULT(Res)

! Returns .TRUE. if ch is a letter and .FALSE. otherwise

CHARACTER   :: Ch
LOGICAL     :: Res

SELECT CASE(Ch)
CASE('A':'Z','a':'z')
  Res=.TRUE.
CASE DEFAULT
  Res=.FALSE.
END SELECT
RETURN

END FUNCTION IsLetter

!**********************************************************************

FUNCTION IsDigit(Ch) RESULT(Res)

! Returns .TRUE. if ch is a digit (0,1,...,9) and .FALSE. otherwise

CHARACTER   :: Ch
LOGICAL     :: Res

SELECT CASE(Ch)
CASE('0':'9')
  Res=.TRUE.
CASE DEFAULT
  Res=.FALSE.
END SELECT
RETURN

END FUNCTION IsDigit

!**********************************************************************

SUBROUTINE Split(Str,Delims,Before,Sep)

! Routine finds the first instance of a character from 'Delims' in the
! the string 'Str'. The characters before the found delimiter are
! output in 'Before'. The characters after the found delimiter are
! output in 'Str'. The optional output character 'Sep' contains the
! found delimiter. A delimiter in 'Str' is treated like an ordinary
! character if it is preceded by a backslash (\). If the backslash
! character is desired in 'Str', then precede it with another backslash.

CHARACTER(LEN=*)    :: Str, Delims, Before
CHARACTER, OPTIONAL :: Sep
LOGICAL             :: Pres
CHARACTER           :: Ch, Cha
INTEGER             :: I, Ibsl, K, Ipos, Iposa, LenStr

Pres=PRESENT(Sep)
Str=ADJUSTL(Str)
CALL Compact(Str)
LenStr=LEN_TRIM(Str)
IF(LenStr == 0) RETURN        ! string Str is empty
K=0
Ibsl=0                        ! backslash initially inactive
Before=' '
DO I=1,LenStr
   Ch=Str(I:I)
   IF(Ibsl == 1) THEN          ! backslash active
      K=K+1
      Before(K:K)=Ch
      Ibsl=0
      CYCLE
   END IF
   IF(Ch == '\') THEN          ! backslash with backslash inactive
      K=K+1
      Before(K:K)=Ch
      Ibsl=1
      CYCLE
   END IF
   Ipos=INDEX(Delims,Ch)
   IF(Ipos == 0) THEN          ! character is not a delimiter
      K=K+1
      Before(K:K)=Ch
      CYCLE
   END IF
   IF(Ch /= ' ') THEN          ! character is a delimiter that is not a space
      Str=Str(I+1:)
      IF(Pres) Sep=Ch
      EXIT
   END IF
   Cha=Str(I+1:I+1)            ! character is a space delimiter
   Iposa=INDEX(Delims,Cha)
   IF(Iposa > 0) THEN          ! next character is a delimiter
      Str=Str(I+2:)
      IF(Pres) Sep=Cha
      EXIT
   ELSE
      Str=Str(I+1:)
      IF(Pres) Sep=Ch
      EXIT
   END IF
END DO
IF(I >= LenStr) Str=''
Str=ADJUSTL(Str)              ! remove initial spaces
RETURN

END SUBROUTINE Split

!**********************************************************************

SUBROUTINE RemoveBackslash(Str)

! Removes backslash (\) characters. Double backslashes (\\) are replaced
! by a single backslash.

CHARACTER(LEN=*)                :: Str
CHARACTER(LEN=1)                :: Ch
CHARACTER(LEN=LEN_TRIM(Str))    ::OutStr
INTEGER                         :: I, Ibsl, K, LenStr


Str=ADJUSTL(Str)
LenStr=LEN_TRIM(Str)
OutStr=' '
K=0
Ibsl=0                        ! backslash initially inactive

DO I=1,LenStr
  Ch=Str(I:I)
  IF(Ibsl == 1) THEN          ! backslash active
   K=K+1
   OutStr(K:K)=Ch
   Ibsl=0
   CYCLE
  END IF
  IF(Ch == '\') THEN          ! backslash with backslash inactive
   Ibsl=1
   CYCLE
  END IF
  K=K+1
  OutStr(K:K)=Ch              ! non-backslash with backslash inactive
END DO

Str=ADJUSTL(OutStr)

END SUBROUTINE RemoveBackslash

!**********************************************************************

SUBROUTINE S_Replace_One ( S1, SUB1, SUB2, S2 )

!*****************************************************************************80
!
!! S_Replace_One replaces the first occurrence of SUB1 with SUB2.
!
!  Discussion:
!
!    The input and output strings may coincide.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 November 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the initial string.
!
!    Input, character ( len = * ) SUB1, the string to be replaced.
!
!    Input, character ( len = * ) SUB2, the replacement string.
!
!    Output, character ( len = * ) S2, the final string.
!
  IMPLICIT NONE

  INTEGER ( I4B ) I1
  INTEGER ( I4B ) I2
  INTEGER ( I4B ) I3
  INTEGER ( I4B ) I4
  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2
  CHARACTER ( LEN = 255 ) S3
  CHARACTER ( LEN = * ) SUB1
  CHARACTER ( LEN = * ) SUB2

  S3 = ' '

  I1 = INDEX ( S1, SUB1 )

  IF ( I1 == 0 ) THEN

    S3 = S1

  ELSE

    S3(1:I1-1) = S1(1:I1-1)

    I2 = LEN_TRIM ( SUB2 )
    S3(I1:I1+I2-1) = SUB2(1:I2)

    I3 = I1 + LEN_TRIM ( SUB1 )
    I4 = LEN_TRIM ( S1 )

    S3(I1+I2:I1+I2+1+I4-I3) = S1(I3:I4)

  END IF

  S2 = S3

  RETURN
END SUBROUTINE S_Replace_One

!*************************************************************

END MODULE ModLib_Evaluate

!******************************************************************************
