
MODULE ModTool_F90PreProcess

!** PURPOSE OF THIS MODULE:
    ! This module contains Fortran 90 utility to pre-process free source form code with
    !   similar facilities to the C cpp pre-processor, and polishing.
    ! This is a re-engineered version of Michel Olagnon's F90ppr.

    !** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_IO_Handlers
    USE ModBase_Memory_Handlers,    ONLY: MemAlloc, MemFree, MemResize
    USE ModBase_ChrStr
    USE Class_FvlStr

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures
    PUBLIC :: F90_Preprocessor

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'ModTool_F90PreProcess'
    ! ----------------------------------!
    !       Preprocessor_Identity       !
    ! ----------------------------------!
    !
    ! Identity of f90ppr utility
    !
    tCharStar, PARAMETER    :: ZP_Version = "@(#) fppridnt.f90    V-1.5 15/09/07 Michel Olagnon"
    tCharStar, PARAMETER    :: ZP_Usage   = "( usage: f90ppr < file.F90  > file.f90 )"
    tCharStar, PARAMETER    :: ZP_Help    = '(                                  &
        &"Fortran 90 utility to pre-process free source form code with"/        &
        &"similar facilities to the C cpp pre-processor, and polishing"/        &
        &"____________________________________________________________________"/&
        &"Copyright (C) 1995, ..., 2015 M. Olagnon"/                            &
        &"This program is free software; you can redistribute it and/or modify"/&
        &"it under the terms of the GNU General Public License as published by"/&
        &"the Free Software Foundation; either version 2 of the License, or"/   &
        &"(at your option) any later version."//                                &
        &"This program is distributed in the hope that it will be useful,"/     &
        &"but WITHOUT ANY WARRANTY; without even the implied warranty of"/      &
        &"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"/       &
        &"GNU General Public License for more details."//                       &
        &"You should have received a copy of the GNU General Public License"/   &
        &"along with this program; if not, write to the Free Software"/         &
        &"Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA."//          &
        &"Originally written by Michel Olagnon, from Ifremer, France,"/         &
        &"who would be pleased to receive your comments and corrections."/      &
        &" M. Olagnon (Michel.Olagnon@ifremer.fr)"/                             &
        &"____________________________________________________________________"/&
        &"                    version 1.5 of 2015/09/07"/                       &
        &"        Pre-process some (F90 code + macros) into f90 code"/          &
        &"____________________________________________________________________"/&
        &"Note: If you do not like code to start in column 7, remember that,"/  &
        &"      had Diophantes left a 6 characters margin, then mathematicians"/&
        &"      might have spared much efforts on A**N = B**N + C**N ..."/      &
        &"      My margin is wide to let you put your corrections there."/      &
        &"____________________________________________________________________")'
    !
    ! ----------------------------------!
    !       Preprocessor_Parameters     !
    ! ----------------------------------!
    !
    !  Parameters for f90ppr utility
    !
    !
    !  Case processing
    !
    tCharLen(26), PARAMETER :: ZP_Lower = "abcdefghijklmnopqrstuvwxyz"
    tCharLen(26), PARAMETER :: ZP_Upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    tInteger,     PARAMETER :: KC_Lower = -1   ! case processing: to lower
    tInteger,     PARAMETER :: KC_Upper =  1   ! case processing: to upper
    tInteger,     PARAMETER :: KC_Leave =  0   ! case processing: leave as is
    !
    !  Logical units
    !
    tInteger,     PARAMETER :: LU_Error   = 0   ! logical unit for stderr
    tInteger,     PARAMETER :: LU_StdIn   = 5   ! logical unit for stdin
    tInteger,     PARAMETER :: LU_File    = 10  ! logical unit for final file
    tInteger,     PARAMETER :: LU_IncBase = 7   ! base logical unit for include

!** INTERFACE DEFINITIONS
    !  interface to add_tok
    ABSTRACT INTERFACE
        SUBROUTINE Add_Tok (ZTOK, LTOK, KKTOK)
            IMPORT
            !  add token to current stream, and reduce if end of statement
            tInteger,       INTENT (IN) :: LTOK, KKTOK
            tCharLen(LTOK), INTENT (IN) :: ZTOK
        END SUBROUTINE Add_Tok
    END INTERFACE
    !  interface to lexer procedures
    INTERFACE
        MODULE SUBROUTINE Add_Token (ZTOK, LTOK, KKTOK)
            tInteger,        INTENT (IN)    :: LTOK, KKTOK
            tCharLen (LTOK), INTENT (IN)    :: ZTOK
        END SUBROUTINE
        MODULE SUBROUTINE Lexing_FreeForm (AddTok, IFSTP, KSTA)
            PROCEDURE (Add_Tok)     :: AddTok
            tInteger, INTENT (IN)   :: IFSTP ! strip-out comments ?
            tInteger, INTENT (OUT)  :: KSTA ! status code
        END SUBROUTINE
        MODULE SUBROUTINE Lexing_FixedForm (AddTok, IFSTP, KSTA)
            PROCEDURE (Add_Tok)     :: AddTok
            tInteger, INTENT (IN)   :: IFSTP    ! strip-out comments ?
            tInteger, INTENT (OUT)  :: KSTA     ! status code
        END SUBROUTINE
    END INTERFACE
    !  interface to parser procedures
    INTERFACE
        MODULE SUBROUTINE Reducing_Statement (NTOK)
            tInteger, INTENT (INOUT)    :: NTOK
        END SUBROUTINE
        MODULE SUBROUTINE Parsing_Statement (NTOK, KSSTT)
            tInteger, INTENT (INOUT)    :: NTOK
            tInteger, INTENT (OUT)      :: KSSTT
        END SUBROUTINE
        MODULE SUBROUTINE Initializing_Names
        END SUBROUTINE
        MODULE SUBROUTINE Testing_Identifier (ZNAM, KCTOK, KRTOK, KWNAM, INAM)
            tCharStar, INTENT (IN)  :: ZNAM
            tInteger,  INTENT (IN)  :: KCTOK, KRTOK
            tInteger,  INTENT (OUT) :: KWNAM, INAM
        END SUBROUTINE
        MODULE SUBROUTINE Initializing_Commands
        END SUBROUTINE
        MODULE SUBROUTINE Rebuilding_Statement (NTOK, KSSTT)
            tInteger, INTENT (IN)   :: NTOK
            tInteger, INTENT (IN)   :: KSSTT
        END SUBROUTINE
        MODULE SUBROUTINE Writing_Statement (ZLAB, LLAB, ZSTT, LSTT, ZCMT, LCMT, NIndentCurrI)
            tCharStar, INTENT (IN)  :: ZLAB, ZSTT, ZCMT
            tInteger,  INTENT (IN)  :: LLAB, LSTT, LCMT, NIndentCurrI
        END SUBROUTINE
    END INTERFACE
    !  interface to expression-evaluation procedures
    INTERFACE
        MODULE SUBROUTINE Processing_Command (IWNAM, NTOK)
            tInteger, INTENT (IN)   :: IWNAM
            tInteger, INTENT (IN)   :: NTOK
        END SUBROUTINE
        MODULE SUBROUTINE Initializing_Expressions
        END SUBROUTINE
        MODULE SUBROUTINE Outputing_Error (ZSTR)
            tCharStar, INTENT (IN)  :: ZSTR
        END SUBROUTINE
    END INTERFACE

!** GENERIC DECLARATIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! ----------------------------------!
    !       Preprocessor_Variables      !
    ! ----------------------------------!
    !
    !  Current status variables in f90ppr utility
    !
    tInteger        :: LU_Input   = LU_StdIn    ! current input unit
    tInteger        :: LU_Include = LU_IncBase  ! current include unit
    !
    !  Format
    !
    tInteger        :: IfFixedIn    = 0     ! are we reading fixed format ?
    tInteger        :: IfFixedOut   = 0     ! are we writing fixed format ?
    tInteger        :: IfLineNumber = 0     ! are we numbering lines ?

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
!
SUBROUTINE F90_Preprocessor ()
!
!  Pre-process standard input, applying some pretty-printing by default
!  and user specified commands to F90 source code, and output result
!  on standard output.
!
    IMPLICIT NONE
!
! local variables
    tInteger                        :: IOUNIT
    tCharLen (80)                   :: INPFILE
    tCharLen (80)                   :: OUTFILE
    tCharLen (:), ALLOCATABLE       :: FILENAME
    tInteger                        :: IFSTP, KSTA
    PROCEDURE (Add_Tok), POINTER    :: AddTok => NULL ()
!
! execution
!
    WRITE (LU_Error, "(a)") "This is f90ppr: " // ZP_Version
    WRITE (LU_Error, "(a)") ZP_Usage
    WRITE (*,*) 'please enter input file name'
    READ (*,*) INPFILE
    WRITE (*,*) 'please enter output file name'
    READ (*,*) OUTFILE
    FILENAME = TRIM (INPFILE) // '.F90'
    OPEN (NEWUNIT=IOUNIT, FILE=FILENAME, STATUS='OLD', ACTION='READ')
    FILENAME = TRIM (OUTFILE) // '.F90'
    OPEN (UNIT=LU_File, FILE=FILENAME, STATUS='NEW', ACTION='WRITE')
    LU_Input = IOUNIT
!
!
!  Initialize names, directives, expression evaluation
!
    CALL Initializing_Names
    CALL Initializing_Commands
    CALL Initializing_Expressions
!
!  Loop on (possibly multiple instructions) input lines
!
    IFSTP = 0   ! do not strip-out embedded comments
    KSTA  = 0
    AddTok => Add_Token
    DO
        IF (IfFixedIn == 0) THEN
            CALL Lexing_FreeForm (AddTok, IFSTP, KSTA)
        ELSE
            CALL Lexing_FixedForm (AddTok, IFSTP, KSTA)
        END IF
        IF (KSTA /= 0) EXIT
    END DO
    NULLIFY (AddTok)
    CLOSE (IOUNIT)
    CLOSE (LU_File)
    RETURN
!
END SUBROUTINE F90_Preprocessor
!
!******************************************************************************
!
FUNCTION Hashing_String (ZSTR) RESULT(HashCode)
!
!  A hash function for use in f90ppr
!
    IMPLICIT NONE
!
! arguments
    tCharStar, INTENT (IN)  :: ZSTR
    tInteger                :: HashCode
!
! parameters
    tCharLen (26), PARAMETER    :: ZLWC = "abcdefghijklmnopqrstuvwxyz"
    tCharLen (26), PARAMETER    :: ZUPC = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
!
! local variables
    tInteger    :: JHSH, ISTR, IRNK, ICHR
!
! execution
!
    JHSH = 0
    DO ISTR = 1, LEN (ZSTR)
        IRNK = INDEX (ZLWC, ZSTR(ISTR:ISTR))
        IF (IRNK > 0) THEN
            ICHR = IACHAR (ZUPC(IRNK:IRNK))
        ELSE
            ICHR = IACHAR (ZSTR(ISTR:ISTR))
        END IF
        JHSH = JHSH * 17 + ICHR - 45
        JHSH = MODULO (JHSH, 4091)
    END DO
    HashCode = JHSH + 1
    RETURN
END FUNCTION Hashing_String
!
!******************************************************************************
!
FUNCTION Is_Same_String (ZSTR1, ZSTR2) RESULT(Flag)
!
!  Case insensitive compare
!
    IMPLICIT NONE
!
! arguments
    tCharStar, INTENT (IN)  :: ZSTR1, ZSTR2
    tLogical                :: Flag
!
! local variables
    tCharLen (1)    :: ZCHR1, ZCHR2
    tInteger        :: LSTR1, LSTR2, ISTR, IRNK
!
! execution
!
    LSTR1 = LEN_TRIM (ZSTR1)
    LSTR2 = LEN_TRIM (ZSTR2)
    IF (LSTR1 == LSTR2) THEN
        Flag = .TRUE.
        DO ISTR = 1, LSTR1
            ZCHR1 = ZSTR1 (ISTR:ISTR)
            IRNK = INDEX (ZP_Lower, ZCHR1)
            IF (IRNK > 0) ZCHR1 = ZP_Upper (IRNK:IRNK)
            ZCHR2 = ZSTR2 (ISTR:ISTR)
            IRNK = INDEX (ZP_Lower, ZCHR2)
            IF (IRNK > 0) ZCHR2 = ZP_Upper (IRNK:IRNK)
            Flag = (ZCHR1 == ZCHR2)
            IF ( .NOT. Flag) EXIT
        END DO
    ELSE
        Flag = .FALSE.
    END IF
    RETURN
END FUNCTION Is_Same_String
!
!******************************************************************************
!
END MODULE ModTool_F90PreProcess
