!!##############################################################################
!!# ****************************************************************************
!!# <name> fsystem </name>
!!# ****************************************************************************
!!#
!!# <purpose>
!!# This module contains system routines like time measurement,
!!# string/value conversions and auxiliary routines.
!!#
!!# On start of the main program, the routine sys_init() must be called
!!# once to initialise internal values!
!!#
!!# The following routines can be found here
!!#
!!#  1.) sys_init = sys_init_simple /
!!#                 sys_init_ext
!!#      -> Initialise system-wide settings
!!#
!!#  2.) sys_halt
!!#      -> Halts the application. Replacement for STOP commands in F90.
!!#         Can be configured how to halt.
!!#         E.g. if the global variable sys_haltmode is set to SYS_HALT_THROWFPE,
!!#         this routine will stop the program by a floating point exception,
!!#         which prints the stack trace to the terminal on some compilers.
!!#
!!#  3.) sys_version
!!#      -> Get kernel version number
!!#
!!#  4.) sys_throwFPE
!!#      -> Throw a floating point exception
!!#
!!#  5.) sys_permute
!!#      -> Compute a random permutation of a given sequence
!!#
!!#  6.) sys_toupper = sys_toupper_replace /
!!#                    sys_toupper_copy
!!#      -> Convert a string to uppercase
!!#
!!#  7.) sys_tolower = sys_tolower_replace /
!!#                    sys_tolower_copy
!!#      -> Convert a string to lowercase
!!#
!!#  8.) sys_upcase
!!#      -> Convert a string to uppercase, function version
!!#
!!#  9.) sys_lowcase
!!#      -> Convert a string to lowercase, function version
!!#
!!# 10.) sys_charreplace
!!#      -> Replaces characters in a string
!!#
!!# 11.) sys_getFreeUnit
!!#      -> Determine a free file handle for use in an OPEN() command
!!#
!!# 12.) sys_fileExists
!!#      -> Check if file with a given name exists
!!#
!!# 13.) sys_flush
!!#      -> Flush file (if available)
!!#
!!# 14.) sys_stringToDouble
!!#      -> Convert string to double value
!!#
!!# 15.) sys_stringToSingle
!!#      -> Convert string to single value
!!#
!!# 16.) sys_d   ,sys_sd  ,sys_sdP  ,sys_sdE ,sys_sdEP ,sys_sdL ,sys_sdEL,
!!#      sys_r   ,sys_s3  ,sys_s5   ,sys_s6  ,sys_s14  ,sys_s18 ,sys_s32,
!!#      sys_s54 ,sys_s61 ,sys_s63  ,sys_s84 ,
!!#      sys_s2E ,sys_s4E ,sys_s6E  ,sys_s10E
!!#      -> String routines to convert double precision numbers to strings
!!#
!!# 17.) sys_si  ,sys_si0 ,sys_sli ,sys_sli0 ,sys_siL ,sys_si0L ,
!!#      sys_i03 ,sys_i04 ,sys_i05 ,sys_i1   ,sys_i2  ,sys_i3   ,sys_i4,
!!#      sys_i6  ,sys_i8  ,sys_i10 ,sys_i12  ,sys_i16 ,sys_i64
!!#      -> String routines to convert integer numbers to strings
!!#
!!# 18.) sys_sliL, sys_sli0L, sys_li12
!!#      -> String routines to convert long integer numbers to strings
!!#
!!# 19.) sys_sl
!!#      -> String routines to convert a logical to a strings
!!#
!!# 20.) sys_smem, sys_smemL
!!#      -> String routines to convert long integers representing memory usage
!!#         to strings.
!!#
!!# 21.) sys_getenv_string
!!#      -> Retrieves an environment variable from the system
!!#
!!# 22.) sys_silsb, sys_simsb
!!#      -> String routine to convert integer numbers to string in bit
!!#         representation (LSB/MSB)
!!#
!!# 23.) sys_adjustl, sys_adjustr
!!#      -> Extended version of ADJUSTL/ADJUSTR that allow to specify
!!#         the length of the resulting string as parameter
!!#
!!# 24.) sys_dequote
!!#      -> De-quote a string; remove any quotation marks
!!#
!!# 25.) sys_stringToCharArray
!!#      -> Converts a string into a character array
!!#
!!# 26.) sys_charArrayToString
!!#      -> Converts a character array into a string
!!#
!!# 27.) sys_stringToHash / sys_stringToHashI64
!!#      -> Converts a string to a hash function
!!#
!!# 28.) sys_countTokens / sys_getNextToken
!!#      -> Splits a string into several tokens and returns them one by one.
!!#
!!# 29.) sys_inumberic
!!#      -> Checks if a string represents a numeric value
!!#
!!# 30.) sys_ncommandLineArgs
!!#      -> Get number of command line arguments
!!#
!!# 31.) sys_getcommandLineArg =
!!#      -> Get a command line argument
!!#
!!# 32.) sys_parseCommandLineArg
!!#      -> Parses a command line argument for an option
!!#
!!# </purpose>
!!##############################################################################

MODULE ModLib_FeatFlow_System

!$ use omp_lib

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: sys_permute
  PUBLIC :: sys_halt
  PUBLIC :: sys_throwFPE
  PUBLIC :: sys_init
  PUBLIC :: sys_version
  PUBLIC :: sys_toupper
  PUBLIC :: sys_tolower
  PUBLIC :: sys_upcase
  PUBLIC :: sys_lowcase
  PUBLIC :: sys_charreplace
  PUBLIC :: sys_getFreeUnit
  PUBLIC :: sys_fileExists
  PUBLIC :: sys_flush
  PUBLIC :: sys_stringToDouble
  PUBLIC :: sys_stringToSingle
  PUBLIC :: sys_smem
  PUBLIC :: sys_smemL
  PUBLIC :: sys_sl
  PUBLIC :: sys_sd
  PUBLIC :: sys_sdP
  PUBLIC :: sys_sdE
  PUBLIC :: sys_sdEP
  PUBLIC :: sys_si
  PUBLIC :: sys_si0
  PUBLIC :: sys_sli
  PUBLIC :: sys_sli0
  PUBLIC :: sys_sdL
  PUBLIC :: sys_sdEL
  PUBLIC :: sys_siL
  PUBLIC :: sys_si0L
  PUBLIC :: sys_sliL
  PUBLIC :: sys_sli0L
  PUBLIC :: sys_i03
  PUBLIC :: sys_i04
  PUBLIC :: sys_i05
  PUBLIC :: sys_i1
  PUBLIC :: sys_i2
  PUBLIC :: sys_i3
  PUBLIC :: sys_i4
  PUBLIC :: sys_i6
  PUBLIC :: sys_i8
  PUBLIC :: sys_i10
  PUBLIC :: sys_i12
  PUBLIC :: sys_i16
  PUBLIC :: sys_i64
  PUBLIC :: sys_li12
  PUBLIC :: sys_s3
  PUBLIC :: sys_s5
  PUBLIC :: sys_s6
  PUBLIC :: sys_s14
  PUBLIC :: sys_s18
  PUBLIC :: sys_s32
  PUBLIC :: sys_s54
  PUBLIC :: sys_s61
  PUBLIC :: sys_s63
  PUBLIC :: sys_s84
  PUBLIC :: sys_d
  PUBLIC :: sys_r
  PUBLIC :: sys_s2E
  PUBLIC :: sys_s4E
  PUBLIC :: sys_s6E
  PUBLIC :: sys_s10E
  PUBLIC :: sys_adjustr
  PUBLIC :: sys_adjustl
  PUBLIC :: sys_getenv_string
  PUBLIC :: sys_ncommandLineArgs
  PUBLIC :: sys_getcommandLineArg
  PUBLIC :: sys_parseCommandLineArg
  PUBLIC :: sys_silsb
  PUBLIC :: sys_simsb
  PUBLIC :: sys_dequote
  PUBLIC :: sys_stringToCharArray
  PUBLIC :: sys_charArrayToString
  PUBLIC :: sys_stringToHash
  PUBLIC :: sys_stringToHashI64
  PUBLIC :: sys_getNextToken
  PUBLIC :: sys_countTokens
  PUBLIC :: sys_triml
  PUBLIC :: sys_trimr
  PUBLIC :: sys_isNumeric
  PUBLIC :: sys_isNAN

  PUBLIC :: t_genericObject
  PUBLIC :: t_sysconfig

!<constants>

!<constantblock description="constants for logical values">

  ! logical value 'true'
  INTEGER, PARAMETER, PUBLIC :: YES = 0

  ! logical value 'false'
  INTEGER, PARAMETER, PUBLIC :: NO = 1

!</constantblock>

!<constantblock description="Kind values for floats">

  ! kind value for 32 bit float (single precision)
  INTEGER, PARAMETER, PUBLIC :: SP = SELECTED_REAL_KIND(6,37)

  ! kind value for 64 bit float (double precision)
  INTEGER, PARAMETER, PUBLIC :: DP = SELECTED_REAL_KIND(15,307)

!!DIR$ IF DEFINED (ENABLE_QUADPREC)
  ! kind value for 80/128 bit float (quad precision)
  INTEGER, PARAMETER, PUBLIC :: QP = SELECTED_REAL_KIND(18,4931)
!!DIR$ ELSE
  ! set QP equal to DP to avoid compiler problems
!  INTEGER, PARAMETER, PUBLIC :: QP = DP
!!DIR$ ENDIF

  ! Note: Depending on the platform and the compiler, QP is either an 80
  ! or an 128 bit float. The g95 and gfortran compilers use 80 floats
  ! for QP, while the ifc compiler uses 128 bit floats.

!</constantblock>

!<constantblock description="Kind values for integers">

  ! kind value for 8 bit integer
  INTEGER, PARAMETER, PUBLIC :: I8 = SELECTED_INT_KIND(2)

  ! kind value for 16 bit integer
  INTEGER, PARAMETER, PUBLIC :: I16 = SELECTED_INT_KIND(4)

  ! kind value for 32 bit integer
  INTEGER, PARAMETER, PUBLIC :: I32 = SELECTED_INT_KIND(8)

  ! kind value for 64 bit integer
  INTEGER, PARAMETER, PUBLIC :: I64 = SELECTED_INT_KIND(10)

!</constantblock>

!<constantblock description="system flags">

  ! constant for a system beep
  CHARACTER(LEN=1), PARAMETER, PUBLIC :: BEEP = ACHAR(7)

  ! constant for breaking line in a string
  CHARACTER(LEN=1), PARAMETER, PUBLIC :: NEWLINE = ACHAR(10)

  ! standard length for strings in FEAT
  INTEGER, PARAMETER, PUBLIC :: SYS_STRLEN = 256

  ! standard length for name strings in FEAT
  INTEGER, PARAMETER, PUBLIC :: SYS_NAMELEN = 32

  ! minimal difference to unity for real values
  REAL(SP), PARAMETER, PUBLIC :: SYS_EPSREAL_SP = EPSILON(1.0_SP)
  REAL(DP), PARAMETER, PUBLIC :: SYS_EPSREAL_DP = EPSILON(1.0_DP)
  REAL(QP), PARAMETER, PUBLIC :: SYS_EPSREAL_QP = EPSILON(1.0_QP)

  ! minimal positive values for real variables
  REAL(SP), PARAMETER, PUBLIC :: SYS_MINREAL_SP = TINY(1.0_SP)
  REAL(DP), PARAMETER, PUBLIC :: SYS_MINREAL_DP = TINY(1.0_DP)
  REAL(QP), PARAMETER, PUBLIC :: SYS_MINREAL_QP = TINY(1.0_QP)

  ! maximal values for real variables
  REAL(SP), PARAMETER, PUBLIC :: SYS_MAXREAL_SP = HUGE(1.0_SP)
  REAL(DP), PARAMETER, PUBLIC :: SYS_MAXREAL_DP = HUGE(1.0_DP)
  REAL(QP), PARAMETER, PUBLIC :: SYS_MAXREAL_QP = HUGE(1.0_QP)

  ! maximal values for integer variables
  INTEGER,      PARAMETER, PUBLIC :: SYS_MAXINT = HUGE(1)
  INTEGER(I8),  PARAMETER, PUBLIC :: SYS_MAXI8  = HUGE(1_I8)
  INTEGER(I16), PARAMETER, PUBLIC :: SYS_MAXI16 = HUGE(1_I16)
  INTEGER(I32), PARAMETER, PUBLIC :: SYS_MAXI32 = HUGE(1_I32)
  INTEGER(I64), PARAMETER, PUBLIC :: SYS_MAXI64 = HUGE(1_I64)

  ! mathematical constant Pi
  REAL(DP), PUBLIC :: SYS_PI

  ! internal constant for infinity
  INTEGER,      PARAMETER, PUBLIC :: SYS_INFINITY_INT = HUGE(1)
  INTEGER(I8),  PARAMETER, PUBLIC :: SYS_INFINITY_I8  = HUGE(1_I8)
  INTEGER(I16), PARAMETER, PUBLIC :: SYS_INFINITY_I16 = HUGE(1_I16)
  INTEGER(I32), PARAMETER, PUBLIC :: SYS_INFINITY_I32 = HUGE(1_I32)
  INTEGER(I64), PARAMETER, PUBLIC :: SYS_INFINITY_I64 = HUGE(1_I64)
  REAL(SP),     PARAMETER, PUBLIC :: SYS_INFINITY_SP  = HUGE(1.0_SP)
  REAL(DP),     PARAMETER, PUBLIC :: SYS_INFINITY_DP  = HUGE(1.0_DP)
  REAL(QP),     PARAMETER, PUBLIC :: SYS_INFINITY_QP  = HUGE(1.0_QP)

  ! flag for appending data to a file
  INTEGER, PARAMETER, PUBLIC :: SYS_APPEND = 0

  ! flag for replacing a file
  INTEGER, PARAMETER, PUBLIC :: SYS_REPLACE = 1

!</constantblock>

!<constantblock description="Constants for the sys_haltmode variable">

  ! Halts the program by the CALL sys_halt() command
  INTEGER, PARAMETER, PUBLIC :: SYS_HALT_STOP     = 0

  ! Halts the program by sys_throwFPE. On some compilers, this helps with
  ! debugging as the compiler will print a stack trace to the terminal
  ! that allows tracing back where an error came from.
  INTEGER, PARAMETER, PUBLIC :: SYS_HALT_THROWFPE = 1

!</constantblock>

!</constants>

!************************************************************************

!<types>

!<typeblock>

  ! Generic object type
  TYPE t_genericObject

    ! Since Fortran lacks a 'void pointer', an arbitrary pointer type
    ! may be used as long as all pointers in Fortran habe the same
    ! length. We tacidly assume this until some compiler complains.
    INTEGER, POINTER :: p_ptr => NULL()

  END TYPE

!</typeblock>

!<typeblock>

  ! Global project settings and system information
  TYPE t_sysconfig

    ! project id
    CHARACTER(LEN=SYS_STRLEN) :: sprojectID    = ""

    ! project directory
    CHARACTER(LEN=SYS_STRLEN) :: sprojectDir   = ""

    ! starting time of this project
    INTEGER                   :: iprojectStart = 0

    ! Starting time of this project (long time runs).
    ! Format: year / month / day / time-difference to UTC /
    !         hours / minutes / seconds / milliseconds.
    INTEGER, DIMENSION(8)     :: iprojectStartLong = 0

  END TYPE

!</typeblock>

!</types>

!************************************************************************

!<publicvars>
  ! global system configuration
  TYPE (t_sysconfig), TARGET, PUBLIC, SAVE :: sys_sysconfig

  ! Halt mode. This variable defines the way, sys_halt halts the program.
  ! One of the SYS_HALT_xxxx constants.
!!DIR$ IF DEFINED(DEBUG) || defined(_DEBUG)
!  integer, public, save :: sys_haltmode = SYS_HALT_THROWFPE
!!DIR$ ELSE
  INTEGER, PUBLIC, SAVE :: sys_haltmode = SYS_HALT_STOP
!!DIR$ ENDIF

  ! The Fortran system_clock timer, like all integer timers, has a cycle
  ! time of real(max)/real(rate) seconds. After max clock cycles the
  ! clock will start counting again from zero. This is the maximum time
  ! span that can be measured when using system_clock manually.
  !
  ! Note: Timing routines in the statistics module automatically
  ! respect this setting but do not explicitely use this variable.
  REAL(DP), PUBLIC, SAVE :: sys_dtimeMax = 0.0_DP

!DIR$ IF DEFINED (ENABLE_COPROCESSOR_SUPPORT)
  ! Number of coprocessor devices
  INTEGER, PUBLIC, SAVE :: sys_NumberOfCoprocDevices = 1
!DIR$ ENDIF

!</publicvars>

!************************************************************************

  INTERFACE sys_init
    MODULE PROCEDURE sys_init_simple
    MODULE PROCEDURE sys_init_ext
  END INTERFACE

  INTERFACE sys_toupper
    MODULE PROCEDURE sys_toupper_replace
    MODULE PROCEDURE sys_toupper_copy
  END INTERFACE

  INTERFACE sys_tolower
    MODULE PROCEDURE sys_tolower_replace
    MODULE PROCEDURE sys_tolower_copy
  END INTERFACE

  INTERFACE sys_isNAN
!DIR$ IF DEFINED (ENABLE_QUADPRE)
    MODULE PROCEDURE sys_isNANQP
!DIR$ ENDIF
    MODULE PROCEDURE sys_isNANDP
    MODULE PROCEDURE sys_isNANSP
  END INTERFACE sys_isNAN

CONTAINS

!************************************************************************

!<subroutine>

  SUBROUTINE sys_permute(k,Idata)

!<description>
    ! This routine computes the permutation of the initial set Idata
    ! that corresponds to the factoriodic number k.
!</description>

!<input>
    ! factoriodic number
    INTEGER(I32), INTENT(in) :: k
!</input>

!<inputoutput>
    ! initial and permuted set
    INTEGER(I32), DIMENSION(:), INTENT(inout) :: Idata
!</inputoutput>
!</subroutine>

    ! local variables
    INTEGER(I32) :: factorial,i,j,iswap

    DO j=2,SIZE(Idata)

      IF (j-1 .lt. k) THEN
        factorial = 0._I64
      ELSE IF(j-1 .eq. k) THEN
        factorial = 1._I64
      ELSE
        factorial=k+1
        DO i=k+2,j-1
          factorial=factorial*i
        END DO
      END IF

      i = j-MOD(factorial,j)
      iswap    = Idata(j)
      Idata(j) = Idata(i)
      IData(i) = iswap
    END DO
  END SUBROUTINE sys_permute

!************************************************************************

!<subroutine>

  SUBROUTINE sys_halt()

!<description>
    ! This routine halts the application like the CALL sys_halt() command in
    ! Fortran 90. The routine can be configured how to halt the application.
    ! For this purpose, the main program can set the global variable
    ! sys_haltmode to one of the SYS_HALT_xxxx constants.
!</description>

!</subroutine>

    SELECT CASE (sys_haltmode)
    CASE (SYS_HALT_STOP)
      STOP
    CASE (SYS_HALT_THROWFPE)
      CALL sys_throwFPE()
    END SELECT

  END SUBROUTINE sys_halt

!************************************************************************

!<subroutine>

  PURE SUBROUTINE sys_throwFPE()

!<description>
    ! This routine throws a floating point exception for debugging
    ! purposes to prevent the debugger to exit the program.
!</description>

!</subroutine>

    ! local variables
    INTEGER :: i1,i2

    i1=1
    i2=0
    i1=i1/i2

  END SUBROUTINE sys_throwFPE

!************************************************************************

!<subroutine>

  SUBROUTINE sys_init_simple()

!<description>
    ! This subroutine initialises internal data structures
    ! with standard values.
!</description>

!</subroutine>

    CALL sys_init_ext("","")

  END SUBROUTINE sys_init_simple

!************************************************************************

!<subroutine>

  SUBROUTINE sys_init_ext(sprojectID,sprojectDir)

!<description>
    ! Extended initialisation.
    ! This subroutine initialises internal data structures.
    ! The value of the project name and directory are set according to the
    ! parameters.
    ! The command line parameters are read and stored into
    ! sys_scommandLineArgs.
!</description>

!<input>
    ! An ID string of the project.
    CHARACTER(LEN=*), INTENT(in) :: sprojectID

    ! The directory of the project. "" means 'current directory'.
    CHARACTER(LEN=*), INTENT(in) :: sprojectDir
!</input>

!</subroutine>

    ! local variables
    INTEGER :: icount ! current system time
    INTEGER :: irate  ! approx. number of system clock ticks per second
    INTEGER :: icmax  ! largest possible value of icount

    ! system_clock is not a FEAT, but a basic FORTRAN 90 routine
    CALL SYSTEM_CLOCK(icount,irate,icmax)

    ! compute maximam measurable timespan
    sys_dtimeMax = REAL(icmax,DP)/REAL(irate,DP)

    ! use data_and_time to measure long time runs
    CALL DATE_AND_TIME(values=sys_sysconfig%iprojectStartLong)

    ! Initialise the global sysconfig structure
    sys_sysconfig%sprojectID    = sprojectID
    sys_sysconfig%sprojectDir   = sprojectDir
    sys_sysconfig%iprojectStart = icount

    ! Set value of Pi = 3.14..
    SYS_PI=ASIN(1.0_DP)*2.0_DP

  END SUBROUTINE sys_init_ext

!************************************************************************************


!<subroutine>

  SUBROUTINE sys_version(ifeatVersionHigh, ifeatVersionMiddle, ifeatVersionLow, &
                         sreldate)

!<description>
    ! This subroutine returns the library version information.
!</description>

!<output>

    ! high version number
    INTEGER :: ifeatVersionHigh

    ! middle version number
    INTEGER :: ifeatVersionMiddle

    ! low version number
    INTEGER :: ifeatVersionLow

    ! release date
    CHARACTER(LEN=*) :: sreldate

!</output>

!</subroutine>

    ifeatVersionHigh   = 0
    ifeatVersionMiddle = 0
    ifeatVersionLow    = 2

    sreldate="01.01.2009 RC0"

  END SUBROUTINE sys_version

!************************************************************************

!<subroutine>

  SUBROUTINE sys_toupper_replace (str)

!<description>
    ! Convert a string to upper case.
    ! The given string is replaced by its uppercase version.
!</description>

!<inputoutput>

    ! The string that is to make uppercase
    CHARACTER(LEN=*), INTENT(inout) :: str

!</inputoutput>

!</subroutine>

    ! local variables
    INTEGER, PARAMETER :: up2low = IACHAR("a") - IACHAR("A")
    INTEGER :: i
    CHARACTER    :: c

    DO i=1,LEN(str)
      c = str(i:i)
      IF ((c .ge. "a") .AND. (c .le. "z")) THEN
        str(i:i) = ACHAR (IACHAR(c) - up2low)
      END IF
    END DO

  END SUBROUTINE sys_toupper_replace

!************************************************************************

!<subroutine>

  SUBROUTINE sys_toupper_copy (str,strUpper)

!<description>
    ! Convert a string to upper case.
!</description>

!<input>

    ! The string that is to make uppercase
    CHARACTER(LEN=*), INTENT(in) :: str

!</input>

!<output>

    ! Uppercase version of the given string
    CHARACTER(LEN=*), INTENT(out) :: strUpper

!</output>

!</subroutine>

    ! local variables
    INTEGER, PARAMETER :: up2low = IACHAR("a") - IACHAR("A")
    INTEGER :: i
    CHARACTER    :: c

    IF (LEN(str) .gt. LEN(strUpper)) THEN
      WRITE (*,*) "sys_toupper_copy: target string is too short"
      CALL sys_halt()
    END IF

    ! Initialise string
    strUpper = ""

    DO i=1,LEN(str)
      c = str(i:i)
      IF ((c .ge. "a") .AND. (c .le. "z")) THEN
        strUpper(i:i) = ACHAR (IACHAR(c) - up2low)
      ELSE
        strUpper(i:i) = c
      END IF
    END DO

  END SUBROUTINE sys_toupper_copy

!************************************************************************

!<subroutine>

  SUBROUTINE sys_tolower_replace (str)

!<description>
    ! Convert a string to lower case.
    ! The given string is replaced by its lowercase version.
!</description>

!<inputoutput>

    ! The string that is to make lowercase
    CHARACTER(LEN=*), INTENT(inout) :: str

!</inputoutput>

!</subroutine>

    ! local variables
    INTEGER, PARAMETER :: up2low = IACHAR("a") - IACHAR("A")
    INTEGER :: i
    CHARACTER    :: c

    DO i=1,LEN(str)
      c = str(i:i)
      IF ((c .ge. "A") .AND. (c .le. "Z")) THEN
        str(i:i) = ACHAR (IACHAR(c) + up2low)
      END IF
    END DO

  END SUBROUTINE sys_tolower_replace

!************************************************************************

!<subroutine>

  SUBROUTINE sys_tolower_copy (str,strLower)

!<description>
    ! Convert a string to lower case.
!</description>

!<input>

    ! The string that is to make lowercase
    CHARACTER(LEN=*), INTENT(in) :: str

!</input>

!<output>

    ! Lowercase version of the given string
    CHARACTER(LEN=*), INTENT(out) :: strLower

!</output>

!</subroutine>

    ! local variables

    INTEGER, PARAMETER :: up2low = IACHAR("a") - IACHAR("A")
    INTEGER :: i
    CHARACTER    :: c

    IF (LEN(str) .gt. LEN(strLower)) THEN
      WRITE (*,*) "sys_tolower_copy: target string is too short"
      CALL sys_halt()
    END IF

    ! Initialise string
    strLower = ""

    DO i=1,LEN(str)
      c = str(i:i)
      IF ((c .ge. "A") .AND. (c .le. "Z")) THEN
        strLower(i:i) = ACHAR (IACHAR(c) + up2low)
      ELSE
        strLower(i:i) = c
      END IF
    END DO

  END SUBROUTINE sys_tolower_copy

!******************************************************************************

!<function>

  PURE FUNCTION sys_upcase(sinput) RESULT(soutput)

!<description>
    ! This routine converts a given string to its uppercase version.
!</description>

!<input>

    ! input string
    CHARACTER(LEN=*), INTENT(in) :: sinput

!</input>

!<output>

    ! output string
    CHARACTER(LEN=LEN(sinput)) :: soutput

!</output>
!</function>

    ! index variable
    INTEGER :: i

    soutput = " "   ! initialise string
    DO I = 1,LEN(sinput)
      IF(sinput(i:i) .ge. "a" .AND. sinput(i:i) .le. "z") THEN
        soutput(i:i) = ACHAR(IACHAR(sinput(i:i)) - 32)
      ELSE
        soutput(i:i) = sinput(i:i)
      END IF
    END DO

  END FUNCTION sys_upcase

!******************************************************************************

!<function>

  PURE FUNCTION sys_lowcase(sinput) RESULT(soutput)

!<description>
    ! This routine converts a given string to its uppercase version.
!</description>

!<input>

    ! input string
    CHARACTER(LEN=*), INTENT(in) :: sinput

!</input>

!<output>

    ! output string
    CHARACTER(LEN=LEN(sinput)) :: soutput

!</output>
!</function>

    ! index variable
    INTEGER :: i

    soutput = " "   ! initialise string
    DO i = 1,LEN(sinput)
      IF(sinput(i:i) .ge. "A" .AND. sinput(i:i) .le. "Z") THEN
        soutput(i:i) = ACHAR(IACHAR(sinput(i:i)) + 32)
      ELSE
        soutput(i:i) = sinput(i:i)
      END IF
    END DO

  END FUNCTION sys_lowcase

!******************************************************************************

!<function>

  PURE FUNCTION sys_charreplace(sinput,scharsource,schardest) RESULT(soutput)

!<description>
    ! Replaces all characers scharsource in sinput by schardest.
    ! Case sensitive.
!</description>

!<input>
    ! input string
    CHARACTER(LEN=*), INTENT(in) :: sinput

    ! Character to be searched for.
    CHARACTER, INTENT(in) :: scharsource

    ! Detinatiion character, all scarsource characters in sinput should be
    ! replaced by.
    CHARACTER, INTENT(in) :: schardest
!</input>

!<output>
    ! output string
    CHARACTER(LEN=LEN(sinput)) :: soutput
!</output>
!</function>

    !index variable
    INTEGER :: i

    soutput = " "   !initialise string
    DO I = 1,LEN(sinput)
      IF(sinput(i:i) .eq. scharsource) THEN
        soutput(i:i) = schardest
      ELSE
        soutput(i:i) = sinput(i:i)
      END IF
    END DO

  END FUNCTION sys_charreplace

!************************************************************************

!<function>

  INTEGER FUNCTION sys_getFreeUnit()

!<description>
    !This routine tries to find a free unit (for file input/output). If a free unit is
    !found, it is returned, otherwise -1 is returned.
!</description>

!<result>
    !number of free unit (-1 if no free unit available)
!</result>

!</function>

    LOGICAL :: bexists, bopened!flags indicating errors
    INTEGER :: itry !free unit candidate

    sys_getFreeUnit = -1
    DO itry = 20,10000
      !does unit exist?
      INQUIRE(unit=itry, exist=bexists)
      IF (bexists) THEN
        !is unit already opened?
        INQUIRE(unit=itry, opened=bopened)
        IF (.NOT. bopened) THEN
          !free unit found
          sys_getFreeUnit = itry
          !exit do-loop
          EXIT
        END IF
      END IF
    ENDDO
    IF (sys_getFreeUnit .eq. -1) THEN
      WRITE (6,*) "*** WARNING! No free unit between 1 and 10000 found! ***"
    END IF

  END FUNCTION sys_getFreeUnit


!************************************************************************

!<function>

  LOGICAL FUNCTION sys_fileExists(iunit,sname)

!<description>
    !This function checks if there is a file connected to unit iunit, which we
    !can access for reading.
!</description>

!<input>

    !unit the file shall be attached to
    INTEGER, INTENT(in) :: iunit

    !name of the file to look at
    CHARACTER (LEN=*), INTENT(in) :: sname

!</input>

!<result>
    !  .TRUE. if the file is accessable for reading,
    !  .FALSE. otherwise
!</result>
!</function>

    INTEGER :: iostat !status variable for opening procedure

    OPEN(iunit,FILE=sname,IOSTAT=iostat,STATUS="OLD",ACTION="READ")
    sys_fileExists=(iostat .eq. 0)
    CLOSE(iunit)

  END FUNCTION sys_fileExists


!************************************************************************

!<subroutine>
  SUBROUTINE sys_flush(iunit)

!<description>
    ! This routine flushes the buffers associated with an open output unit.
    ! This normally happens when the file is closed or the program ends,
    ! but this routine ensures the buffers are flushed before any other
    ! processing occurs.
!</description>

!<input>

    ! Unit connected to the file to write to
    INTEGER, INTENT(in) :: iunit

!</input>
!</subroutine>

!DIR$ IF DEFINED (HAS_INTRINSIC_FLUSH)
    CALL FLUSH(iunit)
!DIR$ ENDIF

  END SUBROUTINE sys_flush

!************************************************************************

!<function>

  FUNCTION sys_stringToDouble(svalue,sformat) RESULT(dvalue)

!<description>
    ! This routine converts a given string that provides a valid
    ! IEEE 745 representation of a real number into a double value.
!</description>

!<input>

    ! string containing the real number
    CHARACTER(LEN=*), INTENT(in) :: svalue

    ! format description to use for conversion
    CHARACTER(LEN=*), INTENT(in) :: sformat

!</input>

!<result>
    ! double precision value
    REAL(DP) :: dvalue
!</result>
!</function>

    ! local variables
    CHARACTER(LEN=LEN(svalue)+3) :: svalueTemp
    INTEGER :: ipos

    ! Check if string contains a 'dot'
    IF (SCAN(svalue,".") .ne. 0) THEN

      ! Read original string
      READ(svalue,sformat) dvalue
    ELSE
      ! Check if string is given in scientific notation
      ipos = SCAN(svalue,"dDeE")

      IF (ipos .eq. 0) THEN
        ! Append '.E0' to convert string into scientific notation
        svalueTemp = TRIM(svalue)//".E0"

      ELSE IF (ipos .eq. 1) THEN
        ! Prepend '1.' to convert string into scientific notation
        svalueTemp = "1."//ADJUSTL(svalue)
      ELSE
        ! Insert '.' to convert string into scientific notation
        svalueTemp = svalue(1:ipos-1)//"."//svalue(ipos:)
      END IF

      ! Read modified string
      READ(svalueTemp,sformat) dvalue
    END IF

  END FUNCTION sys_stringToDouble

!************************************************************************

!<function>

  FUNCTION sys_stringToSingle(svalue,sformat) RESULT(fvalue)

!<description>
    ! This routine converts a given string that provides a valid
    ! IEEE 745 representation of a real number into a single value.
!</description>

!<input>

    ! string containing the real number
    CHARACTER(LEN=*), INTENT(in) :: svalue

    ! format description to use for conversion
    CHARACTER(LEN=*), INTENT(in) :: sformat

!</input>

!<result>
    ! single precision value
    REAL(SP) :: fvalue
!</result>
!</function>

    ! local variables
    CHARACTER(LEN=LEN(svalue)+3) :: svalueTemp
    INTEGER :: ipos

    ! Check if string contains a 'dot'
    IF (SCAN(svalue,".") .ne. 0) THEN

      ! Read original string
      READ(svalue,sformat) fvalue
    ELSE
      ! Check if string is given in scientific notation
      ipos = SCAN(svalue,"dDeE")

      IF (ipos .eq. 0) THEN
        ! Append '.E0' to convert string into scientific notation
        svalueTemp = TRIM(svalue)//".E0"

      ELSE IF (ipos .eq. 1) THEN
        ! Prepend '1.' to convert string into scientific notation
        svalueTemp = "1."//ADJUSTL(svalue)
      ELSE
        ! Insert '.' to convert string into scientific notation
        svalueTemp = svalue(1:ipos-1)//"."//svalue(ipos:)
      END IF

      ! Read modified string
      READ(svalueTemp,sformat) fvalue
    END IF

  END FUNCTION sys_stringToSingle

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_smem(imem,ndigits) RESULT(sout)

!<description>
  ! This routine converts 64 bit integer representing memory usage to
  ! a string.
!</description>

!<input>
  ! The memory usage in bytes.
  INTEGER(I64), INTENT(in) :: imem

  ! OPTIONAL: The number of digits. Must be 0 <= ndigits <= 3.
  ! If not given, ndigits = 2 is used.
  INTEGER, OPTIONAL, INTENT(in) :: ndigits
!</input>

!</function>

  INTEGER :: k,nld,ntd
  INTEGER(I64) :: ii,jj,itds
  CHARACTER(LEN=32) :: sformat
  CHARACTER(LEN=8) :: spost
  CHARACTER(LEN=2) :: snld, sntd

    ! Get the number of trailing digits
    ntd = 2
    IF(PRESENT(ndigits)) THEN
      IF(ndigits .lt. 0) THEN
        ntd = 0
      ELSE IF(ndigits .gt. 3) THEN
        ntd = 3
      ELSE
        ntd = ndigits
      END IF
    END IF

    ! Calculate trailing digits scale
    itds = (10_I64)**ntd

    ! Find out in which range the memory usage is:
    ii = ABS(imem)
    jj = 0
    DO k = 0, 6
      IF(ii .lt. 1024_I64) EXIT
      jj = (itds * MOD(ii, 1024_I64)) / 1024_I64
      ii = ii / 1024_I64
    END DO

    ! What do we have here?
    SELECT CASE(k)
    CASE (0)
      spost = " Bytes"
    CASE (1)
      spost = " KB"   ! "Kilobytes"
    CASE (2)
      spost = " MB"   ! "Megabytes"
    CASE (3)
      spost = " GB"   ! "Gigabytes"
    CASE (4)
      spost = " TB"   ! "Terabytes"
    CASE (5)
      spost = " PB"   ! "Petabytes"
    CASE (6)
      spost = " EB"   ! "Exabytes"
    END SELECT

    ! "Count" the number of leading digits
    IF(ii .lt. 10_I64) THEN
      nld = 1
    ELSE IF(ii .lt. 100_I64) THEN
      nld = 2
    ELSE IF(ii .lt. 1000_I64) THEN
      nld = 3
    ELSE
      nld = 4
    END IF

    ! If the memory usage was negative (nice idea), then the number
    ! of leading digits has to be increased by 1 for the sign.
    IF(imem .lt. 0_I64) THEN
      nld = nld + 1
      ii = -ii
    END IF

    ! Prepare snld and sntd
    WRITE(snld,"(i1)") nld
    WRITE(sntd,"(i1)") ntd

    ! Now what format are we going to print?
    IF((k .eq. 0) .OR. (ntd .eq. 0)) THEN

      ! Print something like "xxx KB"
      sformat = "(i" // TRIM(snld) // ",""" // TRIM(spost) // """)"
      WRITE(sout, sformat) ii

    ELSE

      ! Print something like "xxx.yy KB"
      sformat = "(i" // TRIM(snld) // ",""."",i" // TRIM(sntd) // "." &
                // TRIM(sntd) // ",""" // TRIM(spost) // """)"
      WRITE(sout, sformat) ii, jj

    END IF

  END FUNCTION sys_smem

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_smemL(imem,ndigits) RESULT(sout)

!<description>
  ! This routine converts 64 bit integer representing memory usage to
  ! a string.
!</description>

!<input>
  ! The memory usage in bytes.
  INTEGER(I64), INTENT(in) :: imem

  ! OPTIONAL: The number of digits. Must be 0 <= ndigits <= 3.
  ! If not given, ndigits = 2 is used.
  INTEGER, OPTIONAL, INTENT(in) :: ndigits
!</input>

!</function>

    IF(PRESENT(ndigits)) THEN
      sout = ADJUSTL(sys_smem(imem,ndigits))
    ELSE
      sout = ADJUSTL(sys_smem(imem))
    END IF

  END FUNCTION

!************************************************************************
! Main conversion routines:
!
! sys_sl  :  logical   => string
! First parameter: logical value to convert,

! sys_sd  :  real      => string,
! sys_sdE :  real      => string (scientific notation)
! First parameter: real value to convert,
! Second paramter: number of decimals places

! sys_si  :  int       => string
! sys_si0 :  int       => string (filled with zeros)
! sys_sli :  long int  => string
! sys_sli0:  long int  => string (filled with zeros)
! First parameter: integer value to convert,
! Second paramter: number of total digits (filled with white spaces or zeros)

! All routines exist also in a *L version which do basically the same,
! but return a left-adjusted string (fixed length of 32 characters)
!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_sl(lvalue) RESULT(soutput)

!<description>
    ! This routine converts a logical value to a string.
!</description>

!<input>

    ! value to be converted
    LOGICAL, INTENT(in) :: lvalue
!</input>
!</function>

    IF (lvalue) THEN
      soutput = "true"
    ELSE
      soutput = "false"
    END IF
  END FUNCTION sys_sl

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_sd(dvalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts a double value to a string with idigits
    ! decimal places.
!</description>

!<result>
    ! String representation of the value, filled with white spaces.
    ! At most 32 characters supported.
!</result>

!<input>

    ! value to be converted
    REAL(DP), INTENT(in) :: dvalue

    !number of decimals
    INTEGER, INTENT(in)  :: idigits
!</input>
!</function>

    CHARACTER (LEN=16) :: sformat
    CHARACTER (LEN=2)  :: saux

    ! idigits can not be simply adjusted to 16 because some compilers
    ! do not accept that idigits is changed within this function if
    ! the function is called with a hard-coded integer instead of a
    ! variable, i.e.
    !   sys_sli0(foo, 1)
    ! would result in a crash
    IF (idigits .gt. 16) THEN
      WRITE(6, *) "*** WARNING! Too many decimal places requested in sys_sd! ***"
      WRITE(saux, "(i2)") 16
    ELSE
      WRITE(saux, "(i2)") idigits
    END IF

    sformat = "(f32." // TRIM(saux) // ")"
    WRITE (unit = soutput, fmt = TRIM(sformat)) dvalue
  END FUNCTION sys_sd

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_sdP(dvalue, ipositions, idigits) RESULT(soutput)

!<description>
    ! This routine converts a double value to a string with length
    ! iposition and idigits decimal places.
!</description>

!<result>
    ! String representation of the value, filled with white spaces.
    ! At most 32 characters supported.
!</result>

!<input>

    ! value to be converted
    REAL(DP), INTENT(in) :: dvalue

    ! number of positions in the string
    INTEGER, INTENT(in)  :: ipositions

    ! number of decimals
    INTEGER, INTENT(in)  :: idigits
!</input>
!</function>

    CHARACTER (LEN=16) :: sformat
    CHARACTER (LEN=2)  :: saux,saux2

    ! idigits can not be simply adjusted to 16 because some compilers
    ! do not accept that idigits is changed within this function if
    ! the function is called with a hard-coded integer instead of a
    ! variable, i.e.
    !   sys_sli0(foo, 1)
    ! would result in a crash
    IF (idigits .gt. 16) THEN
      WRITE(6, *) "*** WARNING! Too many decimal places requested in sys_sdP! ***"
      WRITE(saux, "(i2)") 16
    ELSE
      WRITE(saux, "(i2)") idigits
    END IF

    IF (ipositions .gt. 32) THEN
      WRITE(6, *) "*** WARNING! Too long string requested in sys_sdP! ***"
      WRITE(saux2, "(i2)") 32
    ELSE
      WRITE(saux2, "(i2)") ipositions
    END IF

    sformat = "(f"//TRIM(saux2)//"." // TRIM(saux) // ")"
    WRITE (unit = soutput, fmt = TRIM(sformat)) dvalue
  END FUNCTION sys_sdP

!************************************************************************

!<function>

  CHARACTER (LEN=24) FUNCTION sys_sdE(dvalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts a double value to a string with idigits
    ! decimal places in scientific notation.
!</description>

!<result>
    ! String representation of the value, filled with white spaces.
    ! At most 24 characters supported.
!</result>

!<input>

    ! value to be converted
    REAL(DP), INTENT(in) :: dvalue

    !number of decimals
    INTEGER, INTENT(in)  :: idigits
!</input>
!</function>

    CHARACTER (LEN=16) :: sformat
    CHARACTER (LEN=2)  :: saux

    ! idigits can not be simply adjusted to 16 because some compilers
    ! do not accept that idigits is changed within this function if
    ! the function is called with a hard-coded integer instead of a
    ! variable, i.e.
    !   sys_sli0(foo, 1)
    ! would result in a crash
    IF (idigits .gt. 16) THEN
      WRITE(6, *) "*** WARNING! Too many decimal places requested in sys_sdE! ***"
      WRITE(saux, "(i2)") 16
    ELSE
      WRITE(saux, "(i2)") idigits
    END IF

    sformat = "(es24." // TRIM(saux) // ")"
    WRITE (unit = soutput, fmt = TRIM(sformat)) dvalue
  END FUNCTION sys_sdE

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_sdEP(dvalue, ipositions, idigits) RESULT(soutput)

!<description>
    ! This routine converts a double value to a string with length
    ! iposition and idigits decimal places in scientific notation.
!</description>

!<result>
    ! String representation of the value, filled with white spaces.
    ! At most 32 characters supported.
!</result>

!<input>

    ! value to be converted
    REAL(DP), INTENT(in) :: dvalue

    ! number of positions in the string
    INTEGER, INTENT(in)  :: ipositions

    ! number of decimals
    INTEGER, INTENT(in)  :: idigits

!</input>
!</function>

    CHARACTER (LEN=16) :: sformat
    CHARACTER (LEN=2)  :: saux,saux2

    ! idigits can not be simply adjusted to 16 because some compilers
    ! do not accept that idigits is changed within this function if
    ! the function is called with a hard-coded integer instead of a
    ! variable, i.e.
    !   sys_sli0(foo, 1)
    ! would result in a crash
    IF (idigits .gt. 16) THEN
      WRITE(6, *) "*** WARNING! Too many decimal places requested in sys_sdEP! ***"
      WRITE(saux, "(i2)") 16
    ELSE
      WRITE(saux, "(i2)") idigits
    END IF

    IF (ipositions .gt. 24) THEN
      WRITE(6, *) "*** WARNING! Too long string requested in sys_sdEP! ***"
      WRITE(saux2, "(i2)") 24
    ELSE
      WRITE(saux2, '(i2)') ipositions
    END IF

    sformat = "(es"//TRIM(saux2)//"." // TRIM(saux) // ")"
    WRITE (unit = soutput, fmt = TRIM(sformat)) dvalue
  END FUNCTION sys_sdEP

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_si(ivalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts an integer value to a string of length idigits.
!</description>

!<result>
    ! String representation of the value, filled with white spaces.
    ! At most 32 characters supported.
!</result>

!<input>

    ! value to be converted
    INTEGER, INTENT(in) :: ivalue

    !number of decimals
    INTEGER, INTENT(in) :: idigits
!</input>
!</function>

    CHARACTER (LEN=16) :: sformat
    CHARACTER (LEN=2)  :: saux
    ! idigits can not be simply adjusted to 16 because some compilers
    ! do not accept that idigits is changed within this function if
    ! the function is called with a hard-coded integer instead of a
    ! variable, i.e.
    !   sys_sli0(foo, 1)
    ! would result in a crash
    IF (idigits .gt. 16) THEN
      WRITE(6, *) "*** WARNING! Too many decimal places requested in sys_si! ***"
      WRITE(saux, "(i2)") 16
    ELSE IF (idigits .lt. 10) THEN
      WRITE(saux, "(i1)") idigits
    ELSE
      WRITE(saux, "(i2)") idigits
    END IF

    sformat = "(i" // TRIM(saux) // ")"
    WRITE (unit = soutput, fmt = TRIM(sformat)) ivalue

  END FUNCTION sys_si

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_si0(ivalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts an integer value to a string of length idigits.
!</description>

!<result>
    ! String representation of the value, filled with zeros.
    ! At most 32 characters supported.
!</result>

!<input>

    ! value to be converted
    INTEGER, INTENT(in) :: ivalue

    !number of decimals
    INTEGER, INTENT(in) :: idigits
!</input>
!</function>

    CHARACTER (LEN=16) :: sformat
    CHARACTER (LEN=2)  :: saux

    ! idigits can not be simply adjusted to 16 because some compilers
    ! do not accept that idigits is changed within this function if
    ! the function is called with a hard-coded integer instead of a
    ! variable, i.e.
    !   sys_sli0(foo, 1)
    ! would result in a crash
    IF (idigits .gt. 16) THEN
      WRITE(6, *) "*** WARNING! Too many decimal places requested in sys_si0! ***"
      WRITE(saux, "(i2)") 16
    ELSE
      WRITE(saux, "(i2)") idigits
    END IF

    sformat = "(i" // TRIM(saux) // "." // TRIM(saux) // ")"
    WRITE (unit = soutput, fmt = TRIM(sformat)) ivalue
  END FUNCTION sys_si0

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_sli(ivalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts a long integer value to a string of length idigits.
!</description>

!<result>
    ! String representation of the value, filled with white spaces.
    ! At most 32 characters supported.
!</result>

!<input>

    ! value to be converted
    INTEGER(I64), INTENT(in) :: ivalue

    !number of decimals
    INTEGER, INTENT(in)      :: idigits
!</input>
!</function>

    CHARACTER (LEN=16) :: sformat
    CHARACTER (LEN=2)  :: saux

    ! idigits can not be simply adjusted to 16 because some compilers
    ! do not accept that idigits is changed within this function if
    ! the function is called with a hard-coded integer instead of a
    ! variable, i.e.
    !   sys_sli0(foo, 1)
    ! would result in a crash
    IF (idigits .gt. 32) THEN
      WRITE(6, *) "*** WARNING! Too many decimal places requested in sys_sli! ***"
      WRITE(saux, "(i2)") 32
    ELSE
      WRITE(saux, "(i2)") idigits
    END IF

    sformat = "(i" // TRIM(saux) // ")"
    WRITE (unit = soutput, fmt = TRIM(sformat)) ivalue
  END FUNCTION sys_sli

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_sli0(ivalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts a long integer value to a string of length idigits.
!</description>

!<result>
    ! String representation of the value, filled with zeros.
    ! At most 32 characters supported.
!</result>

!<input>

    ! value to be converted
    INTEGER(I64), INTENT(in) :: ivalue

    !number of decimals
    INTEGER, INTENT(in)      :: idigits
!</input>
!</function>

    CHARACTER (LEN=16) :: sformat
    CHARACTER (LEN=2)  :: saux

    ! idigits can not be simply adjusted to 16 because some compilers
    ! do not accept that idigits is changed within this function if
    ! the function is called with a hard-coded integer instead of a
    ! variable, i.e.
    !   sys_sli0(foo, 1)
    ! would result in a crash
    IF (idigits .gt. 16) THEN
      WRITE(6, *) "*** WARNING! Too many decimal places requested in sys_sli0! ***"
      WRITE(saux, "(i2)") 16
    ELSE
      WRITE(saux, "(i2)") idigits
    END IF

    sformat = "(i" // TRIM(saux) // "." // TRIM(saux) // ")"
    WRITE (unit = soutput, fmt = TRIM(sformat)) ivalue
  END FUNCTION sys_sli0

!************************************************************************

!************************************************************************
! Left-adjusted versions of the main conversion routines,
! just add capital L to function name
!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_sdL(dvalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts a double value to a string with idigits
    ! decimal places.
!</description>

!<result>
    ! String representation of the value (left-aligned),
    ! fixed length of 32 characters
!</result>

!<input>

    ! value to be converted
    REAL(DP), INTENT(in) :: dvalue

    !number of decimals
    INTEGER, INTENT(in)  :: idigits
!</input>
!</function>

    soutput = ADJUSTL(sys_sd(dvalue, idigits))
  END FUNCTION sys_sdL

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_sdEL(dvalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts a double value to a string with idigits
    ! decimal places in scientific notation.
!</description>

!<result>
    ! String representation of the value (left-aligned),
    ! fixed length of 32 characters
!</result>

!<input>

    ! value to be converted
    REAL(DP), INTENT(in) :: dvalue

    !number of decimals
    INTEGER, INTENT(in)  :: idigits
!</input>
!</function>

    soutput = ADJUSTL(sys_sdE(dvalue, idigits))
  END FUNCTION sys_sdEL

!************************************************************************

!<function>

  FUNCTION sys_siL(ivalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts an integer value to a string of length idigits,
    ! filled up with white spaces.
!</description>

!<input>
    ! value to be converted
    INTEGER, INTENT(in) :: ivalue

    !number of decimals
    INTEGER, INTENT(in) :: idigits
!</input>

!<result>
    ! String representation of the value (left-aligned),
    ! fixed length of idigits characters
    CHARACTER (LEN=idigits) :: soutput
!</result>

!</function>

    soutput = ADJUSTL(sys_si(ivalue, idigits))
  END FUNCTION sys_siL

!************************************************************************

!<function>

  FUNCTION sys_si0L(ivalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts an integer value to a string of length idigits,
    ! filled up with zeros.
!</description>

!<input>

    ! value to be converted
    INTEGER, INTENT(in) :: ivalue

    !number of decimals
    INTEGER, INTENT(in) :: idigits
!</input>

!<result>
    ! String representation of the value (left-aligned),
    ! fixed length of idigits characters
    CHARACTER (LEN=idigits) :: soutput
!</result>
!</function>

    soutput = ADJUSTL(sys_si0(ivalue, idigits))
  END FUNCTION sys_si0L

!************************************************************************

!<function>

  FUNCTION sys_sliL(ivalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts a long integer value to a string of length idigits.
!</description>

!<input>
    ! value to be converted
    INTEGER(I64), INTENT(in) :: ivalue

    !number of decimals
    INTEGER, INTENT(in)      :: idigits
!</input>

!<result>
    ! String representation of the value (left-aligned),
    ! fixed length of idigits characters
    CHARACTER (LEN=idigits) :: soutput
!</result>
!</function>

    soutput = ADJUSTL(sys_sli(ivalue, idigits))
  END FUNCTION sys_sliL

!************************************************************************

!<function>

  FUNCTION sys_sli0L(ivalue, idigits) RESULT(soutput)

!<description>
    ! This routine converts a long integer value to a string of length idigits.
!</description>

!<input>
    ! value to be converted
    INTEGER(I64), INTENT(in) :: ivalue

    !number of decimals
    INTEGER, INTENT(in)      :: idigits
!</input>

!<result>
    ! String representation of the value (left-aligned),
    ! fixed length of idigits characters
    CHARACTER (LEN=idigits) :: soutput
!</result>
!</function>

    soutput = ADJUSTL(sys_sli0(ivalue, idigits))
  END FUNCTION sys_sli0L

!************************************************************************
! Wrapper functions to be downward-compatible
! (documentation is omitted in purpose, it would just inflate this
!  file and we do not want them to be used any more)
!
!  sys_i0[3-5]
!  sys_i[1-4,6,8] sys_i64
!  sys_li12
!  sys_s[3,5,6] sys_s1[4,8] sys_s32 sys_s54 sys_s6[1,3] sys_s84
!  sys_d, sys_r

  ! First: int => string

  CHARACTER (LEN=3) FUNCTION sys_i03(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i03 = TRIM(sys_si0L(ivalue, 3))
  END FUNCTION sys_i03

  CHARACTER (LEN=4) FUNCTION sys_i04(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i04 = TRIM(sys_si0L(ivalue, 4))
  END FUNCTION sys_i04

  CHARACTER (LEN=5) FUNCTION sys_i05(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i05 = TRIM(sys_si0L(ivalue, 5))
  END FUNCTION sys_i05

  CHARACTER (LEN=1) FUNCTION sys_i1(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i1 = TRIM(sys_siL(ivalue, 1))
  END FUNCTION sys_i1

  CHARACTER (LEN=2) FUNCTION sys_i2(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i2 = TRIM(sys_siL(ivalue, 2))
  END FUNCTION sys_i2

  CHARACTER (LEN=3) FUNCTION sys_i3(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i3 = TRIM(sys_siL(ivalue, 3))
  END FUNCTION sys_i3

  CHARACTER (LEN=4) FUNCTION sys_i4(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i4 = TRIM(sys_siL(ivalue, 4))
  END FUNCTION sys_i4

  CHARACTER (LEN=6) FUNCTION sys_i6(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i6 = TRIM(sys_siL(ivalue, 6))
  END FUNCTION sys_i6

  CHARACTER (LEN=8) FUNCTION sys_i8(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i8 = TRIM(sys_siL(ivalue, 8))
  END FUNCTION sys_i8

  CHARACTER (LEN=10) FUNCTION sys_i10(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i10 = TRIM(sys_siL(ivalue, 10))
  END FUNCTION sys_i10

  CHARACTER (LEN=12) FUNCTION sys_i12(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i12 = TRIM(sys_siL(ivalue, 12))
  END FUNCTION sys_i12

  CHARACTER (LEN=16) FUNCTION sys_i16(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i16 = TRIM(sys_siL(ivalue, 16))
  END FUNCTION sys_i16

  CHARACTER (LEN=64) FUNCTION sys_i64(ivalue)
    INTEGER, INTENT(in) :: ivalue
    sys_i64 = TRIM(sys_siL(ivalue, 64))
  END FUNCTION sys_i64

  CHARACTER (LEN=12) FUNCTION sys_li12(ivalue)
    INTEGER(I64) :: ivalue
    sys_li12 = TRIM(sys_sliL(ivalue, 12))
  END FUNCTION sys_li12

  ! Now: real => string

  CHARACTER (LEN=3) FUNCTION sys_s3(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s3 = TRIM(sys_sdL(dvalue, 1))
  END FUNCTION sys_s3

  CHARACTER (LEN=5) FUNCTION sys_s5(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s5 = TRIM(sys_sdL(dvalue, 2))
  END FUNCTION sys_s5

  CHARACTER (LEN=6) FUNCTION sys_s6(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s6 = TRIM(sys_sdL(dvalue, 2))
  END FUNCTION sys_s6

  CHARACTER (LEN=7) FUNCTION sys_s14(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s14 = TRIM(sys_sdL(dvalue, 4))
  END FUNCTION sys_s14

  CHARACTER (LEN=11) FUNCTION sys_s18(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s18 = TRIM(sys_sdL(dvalue, 8))
  END FUNCTION sys_s18

  CHARACTER (LEN=7) FUNCTION sys_s32(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s32 = TRIM(sys_sdL(dvalue, 2))
  END FUNCTION sys_s32

  CHARACTER (LEN=11) FUNCTION sys_s54(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s54 = TRIM(sys_sdL(dvalue, 4))
  END FUNCTION sys_s54

  CHARACTER (LEN=10) FUNCTION sys_s61(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s61 = TRIM(sys_sdL(dvalue, 1))
  END FUNCTION sys_s61

  CHARACTER (LEN=10) FUNCTION sys_s63(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s63 = TRIM(sys_sdL(dvalue, 3))
  END FUNCTION sys_s63

  CHARACTER (LEN=14) FUNCTION sys_s84(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s84 = TRIM(sys_sdL(dvalue, 4))
  END FUNCTION sys_s84

  CHARACTER (LEN=16) FUNCTION sys_d(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_d = TRIM(sys_sdEL(dvalue, 8))
  END FUNCTION sys_d

  CHARACTER (LEN=16) FUNCTION sys_r(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_r = TRIM(sys_sdL(dvalue, 12))
  END FUNCTION sys_r

  ! Now: real => string (in scientific notation)

  CHARACTER (LEN=9) FUNCTION sys_s2E(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s2E = TRIM(sys_sdEL(dvalue, 2))
  END FUNCTION sys_s2E

  CHARACTER (LEN=11) FUNCTION sys_s4E(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s4E = TRIM(sys_sdEL(dvalue, 2))
  END FUNCTION sys_s4E

  CHARACTER (LEN=13) FUNCTION sys_s6E(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s6E = TRIM(sys_sdEL(dvalue, 2))
  END FUNCTION sys_s6E

  CHARACTER (LEN=17) FUNCTION sys_s10E(dvalue)
    REAL(DP), INTENT(in) :: dvalue
    sys_s10E = TRIM(sys_sdEL(dvalue, 2))
  END FUNCTION sys_s10E

!************************************************************************

!<function>

  FUNCTION sys_adjustr(sstring,nchars) RESULT(soutput)

!<description>
    ! Extended ADJUSTR.
    ! Right-adjusts a string onto a length on nchars characters
!</description>

!<input>
    ! value to be converted
    CHARACTER(LEN=*), INTENT(in) :: sstring

    ! number of characters
    INTEGER, INTENT(in)      :: nchars
!</input>

!<result>
    ! String representation of the value (right-aligned),
    ! fixed length of nchars characters
    CHARACTER (LEN=nchars) :: soutput
!</result>
!</function>

    WRITE(soutput,"(A"//TRIM(sys_siL(nchars,10))//")") TRIM(ADJUSTL(sstring))

  END FUNCTION

!************************************************************************

!<function>

  FUNCTION sys_adjustl(sstring,nchars) RESULT(soutput)

!<description>
    ! Extended ADJUSTR.
    ! Left-adjusts a string onto a length on nchars characters
!</description>

!<input>
    ! value to be converted
    CHARACTER(LEN=*), INTENT(in) :: sstring

    ! number of characters
    INTEGER, INTENT(in)      :: nchars
!</input>

!<result>
    ! String representation of the value (right-aligned),
    ! fixed length of nchars characters
    CHARACTER (LEN=nchars) :: soutput
!</result>
!</function>

    soutput = TRIM(ADJUSTL(sstring))

  END FUNCTION

!************************************************************************

!<function>
  LOGICAL FUNCTION sys_getenv_string(svar, sresult)

  !<description>
    ! This functions returns the string value of a given enviroment variable. The routine
    ! returns .TRUE., if the variable exists, otherwise .FALSE. .
  !</description>

  !<input>
    ! Name of the enviroment variable
    CHARACTER(LEN=*), INTENT(in) :: svar
  !</input>

  !<output>
    ! Value of the enviroment variable
    CHARACTER(LEN=*), INTENT(out) :: sresult
  !</output>

  !<result>
    ! exit status
  !</result>
!</function>

    CHARACTER(LEN=MAX(SYS_STRLEN,LEN(sresult))) :: svalueInEnv

    INTEGER :: nstatus

    CALL GET_ENVIRONMENT_VARIABLE(TRIM(svar), svalueInEnv, status=nstatus)

    SELECT CASE (nstatus)
    CASE (0)
      ! Copy string only up to first whitespace character
!      read(svalueInEnv, '(A)') sresult

      ! Copy complete string
      sresult = svalueInEnv
      sys_getenv_string = .true.

    CASE (1)
      ! Environment variable does not exist
      sresult = ""
      sys_getenv_string = .false.

    CASE default
      !  2: Processor does not support environment variables
      ! >2: Some error occurred
      ! -1: variable svalueInEnv too short to absorb environment variables` content
      sresult = ""
      sys_getenv_string = .false.

    END SELECT

  END FUNCTION sys_getenv_string

!************************************************************************

!<function>
  INTEGER FUNCTION sys_ncommandLineArgs()

  !<description>
    ! Calculates the number of command line arguments.
  !</description>
!</function>

!!DIR$ IF DEFINED (HAS_INTRINSIC_IARGC)
!!DIR$ ELSE
    ! Definition of iargc needed for
    ! * Sun Fortran 95 8.1,
    ! * Compaq Fortran Compiler X5.4A-1684-46B5P,
    ! * Portland Group pgf90 6.0-5
!    INTEGER(I32) :: IARGC
!    EXTERNAL IARGC ! generic Fortran routine to get the arguments from program call
!!DIR$ ENDIF

    sys_ncommandLineArgs=IARGC()

  END FUNCTION

!************************************************************************

!<subroutine>
  SUBROUTINE sys_getcommandLineArg(iarg,soption,svalue,iformat,sdefault)

  !<description>
    ! Fetches command line argument iarg from the command line.
    !
    ! The return value of this function depends on the format of the command line
    ! argument. If only "soption" is specified and nothing else, the parameter
    ! is returned as it is. If "svalue" is specified, there are three cases:
    !
    ! a) Simple option: "option", or svalue not specified
    !  Here, soption = "option" and
    !        svalue  = "".
    !        iformat = 0.
    !  Can be used to store e.g. paths like "./data".
    !
    ! b) Short options: "-option" or "-option=value".
    !  Here, soption = "option" and
    !        svalue  = "value" or "" if no value is specified.
    !        iformat = 1.
    !
    ! c) long options: "--option" or "--option=value".
    !  Here, soption = "option" and
    !        svalue  = "value" or "" if no value is specified.
    !        iformat = 2.
    !
  !</description>

  !<input>
    ! Index of the command line argument. Must be in the range 1..sys_ncommandLineArgs().
    INTEGER, INTENT(in) :: iarg

    ! OPTIONAL: A default value for the command line argument the iarg command
    ! line parameter does not exist.
    CHARACTER(LEN=*), INTENT(in), OPTIONAL :: sdefault
  !</input>

  !<output>
    ! The command line argument.
    CHARACTER(LEN=*), INTENT(out) :: soption

    ! OPTIONAL: Value of the option
    CHARACTER(LEN=*), INTENT(out), OPTIONAL :: svalue

    ! OPTIONAL: Type of the command line argument.
    ! A value -1 indicates that the command line arguzment does not exist and no sdefault
    ! is specified.
    ! A value 0 indicates a direct option.
    ! A value 1 indicates that the command line parameter
    ! is of short form ("-key" or "-key=value").
    ! A value 2 indicates that the command line parameter
    ! is of long form ("--key" or "--key=value").
    INTEGER, INTENT(out), OPTIONAL :: iformat
  !</output>

!</subroutine>

    ! local variables
    CHARACTER(LEN=SYS_STRLEN) :: stmp

    IF ((iarg .lt. 1) .OR. (iarg .gt. sys_ncommandLineArgs())) THEN
      ! Return the default or an empty string.
      IF (PRESENT(sdefault)) THEN
        stmp = sdefault
      ELSE
        soption = ""
        IF (PRESENT(iformat)) iformat = -1
        IF (PRESENT(svalue)) svalue = ""
        RETURN
      END IF
    ELSE
      ! Get the option -- at first to stmp.
      CALL GETARG(iarg,stmp)
    END IF

    ! Parse the argument
    CALL sys_parseCommandLineArg(stmp,soption,svalue,iformat)

  END SUBROUTINE

!************************************************************************

!<subroutine>
  SUBROUTINE sys_parseCommandLineArg(sarg,soption,svalue,iformat)

  !<description>
    ! Parses a command line option sarg.
    !
    ! The return value of this function depends on the format of the command line
    ! argument. If only "soption" is specified and nothing else, the parameter
    ! is returned as it is. If "svalue" is specified, there are three cases:
    !
    ! a) Simple option: "option", or svalue not specified
    !  Here, soption = "option" and
    !        svalue  = "".
    !        iformat = 0.
    !  Can be used to store e.g. paths like "./data".
    !
    ! b) Short options: "-option" or "-option=value".
    !  Here, soption = "option" and
    !        svalue  = "value" or "" if no value is specified.
    !        iformat = 1.
    !
    ! c) long options: "--option" or "--option=value".
    !  Here, soption = "option" and
    !        svalue  = "value" or "" if no value is specified.
    !        iformat = 2.
  !</description>

  !<input>
    ! Command line option to parse.
    CHARACTER(LEN=*), INTENT(in) :: sarg
  !</input>

  !<output>
    ! The command line argument.
    CHARACTER(LEN=*), INTENT(out) :: soption

    ! OPTIONAL: Value of the option
    CHARACTER(LEN=*), INTENT(out), OPTIONAL :: svalue

    ! OPTIONAL: Type of the command line argument.
    ! A value -1 indicates that the command line arguzment does not exist and no sdefault
    ! is specified.
    ! A value 0 indicates a direct option.
    ! A value 1 indicates that the command line parameter
    ! is of short form ("-key" or "-key=value").
    ! A value 2 indicates that the command line parameter
    ! is of long form ("--key" or "--key=value").
    INTEGER, INTENT(out), OPTIONAL :: iformat
  !</output>

!</subroutine>

    ! local variables
    INTEGER :: isarglen,idx

    IF (.NOT. PRESENT (svalue)) THEN

      ! Do not parse, just return as it is.
      soption = sarg
      IF (PRESENT(iformat)) iformat = 0
      RETURN

    END IF

    isarglen = LEN_TRIM(sarg)

    IF (isarglen .ge. 2) THEN
      IF (sarg(1:2).eq."--") THEN

        idx=2
        DO WHILE (idx .lt. isarglen)
          IF (sarg(idx+1:idx+1) .eq. "=") EXIT
          idx=idx+1
        ENDDO

        soption = sarg(3:idx)
        IF (PRESENT(svalue)) THEN
          IF (idx+2 .le. isarglen) THEN
            svalue = sarg(idx+2:)
          ELSE
            svalue = ""
          END IF
        END IF

        IF (PRESENT(iformat)) iformat = 2

        RETURN
      END IF
    END IF

    IF (isarglen .ge. 1) THEN
      IF (sarg(1:1).eq."-") THEN

        idx = 1
        DO WHILE (idx .lt. isarglen)
          IF (sarg(idx+1:idx+1) .eq. "=") EXIT
          idx=idx+1
        ENDDO

        soption = sarg(2:idx)
        IF (PRESENT(svalue)) THEN
          IF (idx+2 .le. isarglen) THEN
            svalue = sarg(idx+2:)
          ELSE
            svalue = ""
          END IF
        END IF
        IF (PRESENT(iformat)) iformat = 1

        RETURN
      END IF
    END IF

    soption = sarg
    IF (PRESENT(svalue)) svalue = ""
    IF (PRESENT(iformat)) iformat = 0

  END SUBROUTINE

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_silsb(ivalue) RESULT(soutput)

!<description>
    ! This routine converts an integer value to a string of length 32
    ! using LSB representation
!</description>

!<result>
    ! LSB Bit representation of the integer value with 32 bits.
!</result>

!<input>

    ! value to be converted
    INTEGER, INTENT(in) :: ivalue

!</input>
!</function>

    ! local variables
    INTEGER :: i

    DO i = 1, MIN(32, BIT_SIZE(ivalue))
      IF (BTEST(ivalue, i-1)) THEN
        soutput(i:i) = "1"
      ELSE
        soutput(i:i) = "0"
      END IF
    END DO

    DO i = MIN(32, BIT_SIZE(ivalue))+1, 32
      soutput(i:i) = "0"
    END DO

  END FUNCTION

!************************************************************************

!<function>

  CHARACTER (LEN=32) FUNCTION sys_simsb(ivalue) RESULT(soutput)

!<description>
    ! This routine converts an integer value to a string of length 32
    ! using MSB representation
!</description>

!<result>
    ! MSB Bit representation of the integer value with 32 bits.
!</result>

!<input>

    ! value to be converted
    INTEGER, INTENT(in) :: ivalue

!</input>
!</function>

    ! local variables
    INTEGER :: i

    DO i = 1, MIN(32, BIT_SIZE(ivalue))
      IF (BTEST(ivalue, i-1)) THEN
        soutput(32-i+1:32-i+1) = "1"
      ELSE
        soutput(32-i+1:32-i+1) = "0"
      END IF
    END DO

    DO i = MIN(32, BIT_SIZE(ivalue))+1, 32
      soutput(32-i+1:32-i+1) = "0"
    END DO

  END FUNCTION

!************************************************************************

!<subroutine>

  SUBROUTINE sys_dequote (sstring)

!<description>
  ! Removes possible quotation marks around a string.
!</description>

!<inputoutput>
  ! String to de-quote
  CHARACTER(LEN=*), INTENT(inout) :: sstring
!</inputoutput>

!</subroutine>

  CHARACTER(LEN=LEN(sstring)+1) :: sstring2

    ! Adjust the string
    sstring2=TRIM(ADJUSTL(sstring))

    ! Does the string start with a quotation mark?
    IF ((sstring2(1:1) .eq. "'") .OR. &
        (sstring2(1:1) .eq. """")) THEN
      ! Re-read the string, remove them.
      READ (sstring2,*) sstring
    ELSE
      ! Just transfer the string, it is ok.
      sstring = sstring2
    END IF

  END SUBROUTINE

!************************************************************************

!<subroutine>

  SUBROUTINE sys_stringToCharArray (sstring,schararray,slength)

!<description>
  ! Converts a string to a character array.
!</description>

!<input>
  ! String to convert
  CHARACTER(LEN=*), INTENT(in) :: sstring

  ! OPTIONAL: Length of the string.
  ! If not present, the default string length is used.
  INTEGER, INTENT(in), OPTIONAL :: slength
!</input>

!<output>
  ! Character array that receives the converted string.
  ! Must be at least as long as the string or as slength.
  CHARACTER, DIMENSION(:), INTENT(out) :: schararray
!</output>

!</subroutine>

    INTEGER :: i,j

    IF (PRESENT(slength)) THEN
      j = slength
    ELSE
      j = MIN(LEN_TRIM(sstring),SIZE(schararray))
    END IF

    ! Copy all characters.
    DO i=1,j
      schararray(i) = sstring(i:i)
    END DO

    ! Fill up the rest with spaces. This emulates a string copy.
    schararray(j+1:) = " "

  END SUBROUTINE

!************************************************************************

!<subroutine>

  SUBROUTINE sys_charArrayToString (schararray,sstring,slength)

!<description>
  ! Converts a character array to a string.
!</description>

!<input>
  ! Character array to convert
  CHARACTER, DIMENSION(:), INTENT(in) :: schararray

  ! OPTIONAL: Length of the string.
  ! If not present, the default string length is used.
  INTEGER, INTENT(in), OPTIONAL :: slength
!</input>

!<output>
  ! Character array that receives the converted string.
  ! Must be at least as long as the character array or slength.
  CHARACTER(LEN=*), INTENT(out) :: sstring
!</output>

!</subroutine>

    INTEGER :: i,j

    IF (PRESENT(slength)) THEN
      j = slength
    ELSE
      j = MIN(SIZE(schararray),LEN(sstring))
    END IF

    ! Copy all characters.
    sstring = ""
    DO i=1,j
      sstring(i:i) = schararray(i)
    END DO

  END SUBROUTINE

!************************************************************************

!<function>

  FUNCTION sys_stringToHash(svalue, ifold) RESULT(ivalue)

!<description>
    ! This routine converts a given string into a hash function.
!</description>

!<input>
    ! string
    CHARACTER(LEN=*), INTENT(in) :: svalue

    ! folding number
    INTEGER, INTENT(in) :: ifold
!</input>

!<result>
    ! hash value
    INTEGER :: ivalue
!</result>
!</function>

    ! local variables
    INTEGER(I64) :: ihash,imult
    INTEGER :: strlen,j,k
    CHARACTER :: c

    ! Hash algorithm: folding on a string, summed 4 bytes at a time
    strlen = LEN(svalue)/4
    ihash  = 0_I64
    DO j = 1, strlen
      imult = 1_I64
      DO k = 1, 4
        c = svalue(4*(j-1)+k:4*(j-1)+k)
        ihash = ihash + ICHAR(c)*imult
        imult = 256_I64 * imult
      END DO
    END DO

    imult = 1_I64
    DO k = 1, LEN(svalue)-4*strlen
      c = svalue(4*strlen+k:4*strlen+k)
      ihash = ihash+ICHAR(c)*imult
      imult = 256_I64 * imult
    END DO

    ivalue = MOD(ihash,INT(ifold,I64))

  END FUNCTION

!************************************************************************

!<function>

  FUNCTION sys_stringToHashI64(svalue, ifold) RESULT(ivalue)

!<description>
    ! This routine converts a given string into a hash function.
!</description>

!<input>
    ! string
    CHARACTER(LEN=*), INTENT(in) :: svalue

    ! folding number
    INTEGER(I64), INTENT(in) :: ifold
!</input>

!<result>
    ! hash value
    INTEGER :: ivalue
!</result>
!</function>

    ! local variables
    INTEGER(I64) :: ihash,imult
    INTEGER :: strlen,j,k
    CHARACTER :: c

    ! Hash algorithm: folding on a string, summed 4 bytes at a time
    strlen = LEN(svalue)/4
    ihash  = 0_I64
    DO j = 1, strlen
      imult = 1_I64
      DO k = 1, 4
        c = svalue(4*(j-1)+k:4*(j-1)+k)
        ihash = ihash + ICHAR(c)*imult
        imult = 256_I64 * imult
      END DO
    END DO

    imult = 1_I64
    DO k = 1, LEN(svalue)-4*strlen
      c = svalue(4*strlen+k:4*strlen+k)
      ihash = ihash+ICHAR(c)*imult
      imult = 256_I64 * imult
    END DO

    ivalue = MOD(ihash,ifold)

  END FUNCTION

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE sys_getNextToken (sstring,stoken,istart,cseparator,bquoting)

!<description>
  ! Splits a string into several tokens and returns them one by one.
  ! For the first call, istart=1 must be set. The routine returns
  ! every token found in stoken. The optional parameter cseperator
  ! defines the seperator character between the tokens; by default,
  ! a whitespace is used. Whitespaces surrounding the tokens are ignored.
  ! Seperators in quotation marks are ignored except if bquoting is
  ! set to FALSE.
  !
  ! Example:
  ! <verb>
  !    istart = 1
  !    call sys_getNextToken (sstring,stoken1,istart)
  !    call sys_getNextToken (sstring,stoken2,istart)
  !    call sys_getNextToken (sstring,stoken3,istart)
  ! </verb>
!</description>

!<input>
  ! A string with parameters, e.g. "0.1 0.2 0.3",...
  CHARACTER(LEN=*), INTENT(in) :: sstring

  ! String that receives the parameter.
  CHARACTER(LEN=*), INTENT(out) :: stoken

  ! OPTIONAL: Character used to separate the tokens.
  ! By default, this is a space.
  CHARACTER, INTENT(in), OPTIONAL :: cseparator

  ! OPTIONAL: Ignore whitespaces surrounded by quotation
  ! marks. Default is TRUE.
  LOGICAL, INTENT(in), OPTIONAL :: bquoting
!</input>

!<inputoutput>
  ! On input: Must be set to =1 for the first call.
  !   Position from where to search for the next parameter
  ! On output:
  !   =0, if this was the last parameter
  !   Otherwise: position in sstring where the next parameter starts.
  INTEGER, INTENT(inout) :: istart
!</inputoutput>

!</subroutine>

    INTEGER :: i,slen
    LOGICAL :: bquot
    CHARACTER :: csep, ccurrentquote

    ! Get optional parameters
    csep = " "
    bquot = .true.
    ccurrentquote = " "

    IF (PRESENT(cseparator)) csep = cseparator
    IF (PRESENT(bquoting)) bquot = bquoting

    ! If istart=0, do not do anything. There is nothing in the string
    stoken = ""
    IF (istart .le. 0) RETURN

    ! If the string length is =0, finish.
    slen = LEN_TRIM(sstring)
    IF (slen .eq. 0) RETURN

    ! Skip whitespaces.
    DO WHILE (istart .le. slen)
      IF (sstring(istart:istart) .eq. " ") THEN
        istart = istart + 1
      ELSE
        EXIT
      END IF
    END DO

    ! End of the string?
    i = istart
    DO WHILE (i .le. slen)

      ! Process quotation marks.
      IF (bquot) THEN

        ! Is this a quotation mark?
        IF ((sstring(i:i) .eq. """") .OR. (sstring(i:i) .eq. "'")) THEN
          ! If quoting is inactive, activate it.
          IF (ccurrentquote .eq. " ") THEN
            ccurrentquote = sstring(i:i)
          ELSE
            ! If the quotation character matches the currently
            ! found one, deactivate quoting. Otherwise ignore.
            IF (ccurrentquote .eq. sstring(i:i)) THEN
              ccurrentquote = " "
            END IF
          END IF
        END IF

        ! Process until the next separator.
        ! Ignore separators in quotation marks.
        IF ((ccurrentquote .ne. " ") .OR. (sstring(i:i) .ne. csep)) THEN
          i = i + 1
        ELSE
          EXIT
        END IF

      ELSE

        ! Process until the next separator
        IF (sstring(i:i) .ne. csep) THEN
          i = i + 1
        ELSE
          EXIT
        END IF

      END IF

    END DO

    ! Copy
    IF (istart .le. slen) THEN
      stoken = sstring(istart:i-1)
    END IF

    ! Put istart behind the parameter or set it to 0.
    IF (i .ge. slen) THEN
      ! No more parameters
      istart = 0
    ELSE
      ! Put istart behind the parameter -- on the whitespace which
      ! is skipped in the next loop.
      istart = i+1
    END IF

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE sys_countTokens (sstring,ntokens,cseparator,bquoting)

!<description>
  ! Returns the number of tokens in sstring
!</description>

!<input>
  ! A string with parameters, e.g. "0.1 0.2 0.3",...
  CHARACTER(LEN=*), INTENT(in) :: sstring

  ! OPTIONAL: Character used to separate the tokens.
  ! By default, this is a space.
  CHARACTER, INTENT(in), OPTIONAL :: cseparator

  ! OPTIONAL: Ignore whitespaces surrounded by quotation
  ! marks. Default is TRUE.
  LOGICAL, INTENT(in), OPTIONAL :: bquoting
!</input>

!<output>
  ! Number of parameters in sstring
  INTEGER, INTENT(out) :: ntokens
!</output>

!</subroutine>

    INTEGER :: slen, istart
    LOGICAL :: bquot
    CHARACTER :: cchar, csep, ccurrentquote

    ! Get optional parameters
    csep = " "
    bquot = .true.
    ccurrentquote = " "

    IF (PRESENT(cseparator)) csep = cseparator
    IF (PRESENT(bquoting)) bquot = bquoting

    ! If the string length is =0, finish.
    ntokens = 0

    slen = LEN_TRIM(sstring)
    IF (slen .eq. 0) RETURN

    ! Find all substrings
    istart = 1

    DO WHILE(istart .le. slen)

      ! Skip whitespaces.
      DO WHILE (istart .le. slen)
        IF (sstring(istart:istart) .eq. " ") THEN
          istart = istart + 1
        ELSE
          EXIT
        END IF
      END DO

      ! Cancel if we reached the string end
      IF (istart .gt. slen) EXIT

      ! One more
      ntokens = ntokens + 1

      ! End of the string?
      DO WHILE (istart .le. slen)

        ! fetch the current character
        cchar = sstring(istart:istart)

        ! increase position
        istart = istart + 1

        ! Process quotation marks.
        IF (bquot) THEN

          ! Is this a quotation mark?
          IF ((cchar .eq. """") .OR. (cchar .eq. "'")) THEN
            ! If quoting is inactive, activate it.
            IF (ccurrentquote .eq. " ") THEN
              ccurrentquote = cchar
            ELSE IF (ccurrentquote .eq. cchar) THEN
              ! If the quotation character matches the currently
              ! found one, deactivate quoting. Otherwise ignore.
              ccurrentquote = " "
            END IF
          END IF


          ! Process until the next separator.
          ! Ignore separators in quotation marks.
          IF ((ccurrentquote .eq. " ") .AND. (cchar .eq. csep)) EXIT

        ELSE IF (cchar .eq. csep) THEN
          ! Process until the next separator
           EXIT
        END IF

      END DO

    END DO

  END SUBROUTINE

  ! ***************************************************************************

!<function>

  FUNCTION sys_triml (sstring,nchar) RESULT(sout)

!<description>
  ! This function return nchar characters of the given string sstring
!</description>

!<input>
  ! A string
  CHARACTER(LEN=*), INTENT(in) :: sstring

  ! Number of characters to return
  INTEGER, INTENT(in) :: nchar
!</input>

!<returns>
  ! The trimmed string
  CHARACTER (LEN=nchar) :: sout
!</returns>

!</function>

    sout = sstring(1:nchar)

  END FUNCTION

  ! ***************************************************************************

!<function>

  FUNCTION sys_trimr (sstring,nchar) RESULT(sout)

!<description>
  ! This function return nchar characters of the given string sstring
!</description>

!<input>
  ! A string
  CHARACTER(LEN=*), INTENT(in) :: sstring

  ! Number of characters to return
  INTEGER, INTENT(in) :: nchar
!</input>

!<returns>
  ! The trimmed string
  CHARACTER (LEN=nchar) :: sout
!</returns>

!</function>

    ! local variable
    INTEGER :: nlen

    nlen = LEN(sstring)
    sout = sstring(nlen-nchar+1:nlen)

  END FUNCTION

  ! ***************************************************************************

!<function>

  FUNCTION sys_isNumeric(sstring)

!<description>
  ! This function checks if the given string represents a numeric value
!</description>

!<input>
  ! String representation
  CHARACTER(LEN=*), INTENT(in) :: sstring
!</input>

!<result>
  ! True if sstring represents a numeric value
  LOGICAL :: sys_isNumeric
!</result>
!</function>

    ! local variables
    REAL(DP) :: x
    INTEGER :: e

    READ(sstring, *, iostat=e) x
    sys_isNumeric = (e .eq. 0)

  END FUNCTION sys_isNumeric

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION sys_isNANQP(qx)

!!DIR$ IF DEFINED (HAS_INTRINSIC_IEEE_ARITHMETIC)
  USE, INTRINSIC :: ieee_arithmetic
!!DIR$ ENDIF

!<description>
  ! This function checks if the given data is not-a-number
!</description>

!<input>
  ! Quadprec value
  REAL(QP), INTENT(in) :: qx
!</input>

!<result>
  ! True if given data is not-a-number
  LOGICAL :: sys_isNANQP
!</result>
!</function>

!!DIR$ IF DEFINED (HAS_INTRINSIC_IEEE_ARITHMETIC)
  sys_isNANQP = ieee_is_nan(qx)
!!DIR$ ELSE
!!DIR$ IF DEFINED (HAS_INTRINSIC_ISNAN)
!!DIR$ ELSE
!  INTERFACE
!    ELEMENTAL FUNCTION ISNAN (d) RESULT (b)
!    INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15,307)
!!DIR$ IF DEFINED (ENABLE_QUADPREC)
!    ! kind value for 80/128 bit float (quad precision)
!    INTEGER, PARAMETER :: QP = SELECTED_REAL_KIND(18,4931)
!!DIR$ ELSE
    ! set QP equal to DP to avoid compiler problems
!    INTEGER, PARAMETER :: QP = DP
!!DIR$ ENDIF
!    REAL(QP), INTENT(in) :: d
!    LOGICAL :: b
!    END FUNCTION
!  END INTERFACE
!!DIR$ ENDIF
!  sys_isNANQP = ISNAN(qx)
!!DIR$ ENDIF
  END FUNCTION sys_isNANQP

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION sys_isNANDP(dx)

!!DIR$ IF DEFINED (HAS_INTRINSIC_IEEE_ARITHMETIC)
  USE, INTRINSIC :: ieee_arithmetic
!!DIR$ ENDIF

!<description>
  ! This function checks if the given data is not-a-number
!</description>

!<input>
  ! Double value
  REAL(DP), INTENT(in) :: dx
!</input>

!<result>
  ! True if given data is not-a-number
  LOGICAL :: sys_isNANDP
!</result>
!</function>

!!DIR$ IF DEFINED (HAS_INTRINSIC_IEEE_ARITHMETIC)
  sys_isNANDP = ieee_is_nan(dx)
!!DIR$ ELSE
!!DIR$ IF DEFINED (HAS_INTRINSIC_ISNAN)
!!DIR$ ELSE
!  INTERFACE
!    ELEMENTAL FUNCTION ISNAN (d) RESULT (b)
!    INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15,307)
!    REAL(DP), INTENT(in) :: d
!    LOGICAL :: b
!    END FUNCTION
!  END INTERFACE
!!DIR$ ENDIF
!  sys_isNANDP = ISNAN(dx)
!!DIR$ ENDIF
  END FUNCTION sys_isNANDP

  ! ***************************************************************************

!<function>

  ELEMENTAL FUNCTION sys_isNANSP(fx)

!!DIR$ IF DEFINED (HAS_INTRINSIC_IEEE_ARITHMETIC)
  USE, INTRINSIC :: ieee_arithmetic
!!DIR$ ENDIF

!<description>
  ! This function checks if the given data is not-a-number
!</description>

!<input>
  ! Single value
  REAL(SP), INTENT(in) :: fx
!</input>

!<result>
  ! True if given data is not-a-number
  LOGICAL :: sys_isNANSP
!</result>
!</function>

!!DIR$ IF DEFINED (HAS_INTRINSIC_IEEE_ARITHMETIC)
  sys_isNANSP = ieee_is_nan(fx)
!!DIR$ ELSE
!!DIR$ IF DEFINED (HAS_INTRINSIC_ISNAN)
!!DIR$ ELSE
!  INTERFACE
!    ELEMENTAL FUNCTION ISNAN (d) RESULT (b)
!    INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(6,37)
!    REAL(SP), INTENT(in) :: d
!    LOGICAL :: b
!    END FUNCTION
!  END INTERFACE
!!DIR$ ENDIF
!  sys_isNANSP = ISNAN(fx)
!!DIR$ ENDIF
  END FUNCTION sys_isNANSP

END MODULE ModLib_FeatFlow_System
