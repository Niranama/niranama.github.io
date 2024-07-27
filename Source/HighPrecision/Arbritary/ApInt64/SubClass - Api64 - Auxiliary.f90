
SUBMODULE (Class_ApInt64) SubClass_Api64_Auxiliary

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for various auxiliary routines
!   of the <a href="../module/class_apint64.html">ApInt64</a> type.

!** USE STATEMENTS:
    USE ModBase_SIntUtil,               I32_ToChar => ToDecStrSigned
    USE ModBase_Memory_Handlers
    USE ModBase_PrgnBuilder
    USE, INTRINSIC :: ISO_FORTRAN_ENV

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName  = 'SubClass_Api64_Auxiliary'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_U32_To_ApInt64(Big, Sign, U32)
!            IMPORT
            TYPE(ApInt64), INTENT(INOUT)    :: Big
            tInteger,      INTENT(IN)       :: Sign
            tInteger,      INTENT(IN)       :: U32
        END SUBROUTINE Assign_U32_To_ApInt64
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_U64_To_ApInt64(Big, Sign, U64)
!            IMPORT
            TYPE(ApInt64), INTENT(INOUT)    :: Big
            tInteger,      INTENT(IN)       :: Sign
            tLong,         INTENT(IN)       :: U64
        END SUBROUTINE Assign_U64_To_ApInt64
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_U128_To_ApInt64(Big, Sign, U128)
!            IMPORT
            TYPE(ApInt64), INTENT(INOUT)    :: Big
            tInteger,      INTENT(IN)       :: Sign
            TYPE(UInt128), INTENT(IN)       :: U128
        END SUBROUTINE Assign_U128_To_ApInt64
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_U32_To_ApInt64_NoSign(Big, U32)
!            IMPORT
            TYPE(ApInt64), INTENT(INOUT)    :: Big
            tInteger,      INTENT(IN)       :: U32
        END SUBROUTINE Assign_U32_To_ApInt64_NoSign
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_U64_To_ApInt64_NoSign(Big, U64)
!            IMPORT
            TYPE(ApInt64), INTENT(INOUT)    :: Big
            tLong,         INTENT(IN)       :: U64
        END SUBROUTINE Assign_U64_To_ApInt64_NoSign
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_U128_To_ApInt64_NoSign(Big, U128)
!            IMPORT
            TYPE(ApInt64), INTENT(INOUT)    :: Big
            TYPE(UInt128), INTENT(IN)       :: U128
        END SUBROUTINE Assign_U128_To_ApInt64_NoSign
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_I32_To_ApInt64(Big, I32)
!            IMPORT
            TYPE(ApInt64), INTENT(INOUT)    :: Big
            tInteger,      INTENT(IN)       :: I32
        END SUBROUTINE Assign_I32_To_ApInt64
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_I64_To_ApInt64(Big, I64)
!            IMPORT
            TYPE(ApInt64), INTENT(INOUT)    :: Big
            tLong,         INTENT(IN)       :: I64
        END SUBROUTINE Assign_I64_To_ApInt64
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_I128_To_ApInt64(Big, I128)
!            IMPORT
            TYPE(ApInt64), INTENT(INOUT)    :: Big
            TYPE(SInt128), INTENT(IN)       :: I128
        END SUBROUTINE Assign_I128_To_ApInt64
        !------------------------------------------------------------
    END INTERFACE

!** GENERIC DECLARATIONS:
    INTERFACE  AssignUnsigned
        MODULE PROCEDURE Assign_U32_To_ApInt64, Assign_U64_To_ApInt64
        MODULE PROCEDURE Assign_U32_To_ApInt64_NoSign, Assign_U64_To_ApInt64_NoSign
        MODULE PROCEDURE Assign_U128_To_ApInt64, Assign_U128_To_ApInt64_NoSign
    END INTERFACE
    INTERFACE  AssignSigned
        MODULE PROCEDURE Assign_I32_To_ApInt64, Assign_I64_To_ApInt64
        MODULE PROCEDURE Assign_I128_To_ApInt64
    END INTERFACE

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE SUBROUTINES OR FUNCTIONS:

!------------------------------------------------------------------------------
!
!                           AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION ApInt64_Copy(Source, Capacity) RESULT(Destination)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a copy of the ApInt64 number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% the source number
    CLASS(ApInt64),   INTENT(IN)    :: Source
    !> capacity (size of magnitude array) of the destination number <br>
    !  if specified, must be greater than capacity of the source number
    tIndex, OPTIONAL, INTENT(IN)    :: Capacity
    !% the destination number
    TYPE(ApInt64)                   :: Destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: DstSize

!** FLOW

    ! get storage size
    DstSize = SIZE(Source%Digit, KIND=kIndex)
    IF (PRESENT(Capacity)) THEN
        IF (Capacity > DstSize) DstSize = Capacity
    END IF
    IF (DstSize < 1_kIndex) DstSize = 1_kIndex
    ! allocate storage size
    CALL MemAlloc(Destination%Digit, DstSize, StartID=0_kIndex)
    ! get source info
    IF (ALLOCATED(Source%Digit)) THEN
        Destination%Sign   = Source%Sign
        Destination%Length = Source%Length
        Destination%Digit(0:Source%Length-1) = Source%Digit(0:Source%Length-1)
        IF (DstSize > Source%Length) Destination%Digit(Source%Length:DstSize-1) = 0_kLong
    ELSE
        ! set destination to zero if source storage has not yet been alllocated
        Destination%Sign   = 1
        Destination%Length = 1_kIndex
        Destination%Digit  = 0_kLong
    END IF

    RETURN

END FUNCTION ApInt64_Copy

!******************************************************************************

MODULE SUBROUTINE ApInt64_Write(Big, Unit, IOStat, IOMsg, ShowComponent, Prefix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write the ApInt64 number to the screen (or the specified unit).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64),      INTENT(IN)     :: Big
    tSInt32,   OPTIONAL, INTENT(IN)     :: Unit             !! output logical unit
    tSInt32,   OPTIONAL, INTENT(OUT)    :: IOStat           !! io stat
    tCharStar, OPTIONAL, INTENT(OUT)    :: IOMsg            !! io message
    tLogical,  OPTIONAL, INTENT(IN)     :: ShowComponent
    !^ flag indicating whether to write the upper and lower components. <br>
    ! - If flag is present and true, write components of the object. <br>
    ! - Otherwise, write the object as a decimal string.
    tCharStar, OPTIONAL, INTENT(IN)     :: Prefix           !! prefix string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical        :: AsString
    tInteger        :: OutUnit
    tInteger        :: IO_Stat
    tCharLen(128)   :: IO_Msg
    tInteger        :: I
    tCharAlloc      :: DigStr
    tCharAlloc      :: DispStr

!** FLOW

    ! set defaults
    OutUnit  = OUTPUT_UNIT
    AsString = TrueVal

    ! check optional input
    IF (PRESENT(ShowComponent)) THEN
        IF (ShowComponent) AsString = FalseVal
    END IF
    IF (PRESENT(Unit)) OutUnit = Unit

    ! write the object
    IF (AsString) THEN
        IF (PRESENT(Prefix)) THEN
            DispStr = Prefix // ToDecString(Big)
        ELSE
            DispStr = ' Big = ' // ToDecString(Big)
        END IF
        WRITE(UNIT=OutUnit, FMT='(A)', IOSTAT=IO_Stat, IOMSG=IO_Msg) DispStr
    ELSE
        DispStr = '-: '
        IF (PRESENT(Prefix)) DispStr = Prefix
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'Sign             = ', Big%Sign
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'Number of digits = ', Big%Length
        IF (ALLOCATED(Big%Digit)) THEN
            DO I = 0, Big%Length-1
                DigStr = DispStr // 'Digit(' // I32_ToChar(I) // ') = '
                WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
                      DigStr, Big%Digit(I)
            END DO
        END IF
    END IF

    ! return output if requested
    IF (PRESENT(IOStat)) IOStat = IO_Stat
    IF (PRESENT(IOMsg))  IOMsg  = IO_Msg

    RETURN

END SUBROUTINE ApInt64_Write

!******************************************************************************

MODULE FUNCTION ZeroApInt64() RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the ApInt64 number with value of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64)       :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Big%Sign   = 1
    Big%Length = 1_kIndex
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)
    Big%Digit(0) = 0_kLong

    RETURN

END FUNCTION ZeroApInt64

!******************************************************************************

MODULE FUNCTION OneApInt64() RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the ApInt64 number with value of one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64)       :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Big%Sign   = 1
    Big%Length = 1_kIndex
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)
    Big%Digit(0) = 1_kLong

    RETURN

END FUNCTION OneApInt64

!------------------------------------------------------------------------------
!
!                           MISCELLANEOUS ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION ApInt64_Is_Zero_II(Big) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the ApInt64 number has value of zero or not.

!** Technical Note:
!   Zero can have many forms:
!   - The most common form is set through ZeroApInt64() where Digit(0) = 0 and Length = 1.
!   - If Digit has not yet been allocated or Length is less than 1, the number is also
!     considered to be zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: Big
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        IF (Big%Length >= 1_kIndex) THEN
            Flag = (Big%Length == 1_kIndex).AND.(Big%Digit(0) == 0_kLong)
        ELSE
            ! length is less than 1
            Big = ZeroApInt64()
            Flag = TrueVal
        END IF
    ELSE
        ! digit not yet allocated
        Big = ZeroApInt64()
        Flag = TrueVal
    END IF

    RETURN

END FUNCTION ApInt64_Is_Zero_II

!******************************************************************************

MODULE FUNCTION ApInt64_Is_One(Big) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the ApInt64 number has value of one or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: Big
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        Flag = (Big%Length == 1_kIndex).AND.(Big%Digit(0) == 1_kLong) &
                                       .AND.(Big%Sign == 1)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION ApInt64_Is_One

!******************************************************************************

MODULE FUNCTION ApInt64_Is_Positive(Big) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the ApInt64 number has positive value or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: Big
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        Flag = (.NOT.IsZero(Big)).AND.(Big%Sign == 1)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION ApInt64_Is_Positive

!******************************************************************************

MODULE FUNCTION ApInt64_Is_Negative(Big) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the ApInt64 number has negative value or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: Big
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        Flag = (.NOT.IsZero(Big)).AND.(Big%Sign == -1)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION ApInt64_Is_Negative

!******************************************************************************

MODULE FUNCTION ApInt64_Random_Number(Prng, Positive, Length) RESULT(BigRnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate the ApIn64 number with random value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% pseudo-random number generator
    CLASS(BaseRNG), OPTIONAL, INTENT(INOUT) :: Prng
    TARGET                                  :: Prng
    !% flag indicating whether the number has positive value or not
    tLogical,       OPTIONAL, INTENT(IN)    :: Positive
    !% number indicating the length of magnitude array
    tIndex,         OPTIONAL, INTENT(IN)    :: Length
    !% the ApInt64 number with random value
    TYPE(ApInt64)                           :: BigRnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! simply call the type-bound procedure
    CALL BigRnd%RandNum(Prng, Positive, Length)

    RETURN

END FUNCTION ApInt64_Random_Number

!******************************************************************************

MODULE SUBROUTINE ApInt64_RandNumSub(BigRnd, Prng, Positive, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate the ApIn64 number with random value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% the ApInt64 number with random value
    CLASS(ApInt64),           INTENT(OUT)   :: BigRnd
    !% pseudo-random number generator
    CLASS(BaseRNG), OPTIONAL, INTENT(INOUT) :: Prng
    TARGET                                  :: Prng
    !% flag indicating whether the number has positive value or not
    tLogical,       OPTIONAL, INTENT(IN)    :: Positive
    !% number indicating the length of magnitude array
    tIndex,         OPTIONAL, INTENT(IN)    :: Length

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: RngAlgo = I64_MT
    tInteger, PARAMETER :: MaxLen  = 50

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BaseRNG), ALLOCATABLE, TARGET :: LocRng   ! local Prng
    CLASS(BaseRNG), POINTER             :: BigRng   ! pointer to Prng
    tInteger                            :: BigSign
    tIndex                              :: BigLen

!** FLOW

    ! set pointer to working Prng
    IF (PRESENT(Prng)) THEN
        BigRng => Prng
        CALL BigRng%Initialize()
    ELSE
        LocRng = CreateLongRng(RngAlgo)
        BigRng => LocRng
    END IF

    BigSign = 0
    BigLen  = 0_kIndex
    IF (PRESENT(Positive)) THEN
        IF (Positive) THEN
            BigSign = 1
        ELSE
            BigSign = -1
        END IF
    END IF
    IF (BigSign == 0) THEN
        IF (BigRng%NextLogical()) THEN
            BigSign = 1
        ELSE
            BigSign = -1
        END IF
    END IF
    IF (PRESENT(Length)) THEN
        BigLen = Length
    END IF
    IF (BigLen < 1_kIndex) THEN
        BigLen = BigRng%NextInteger(1, MaxLen)
    END IF
    BigRnd = Generate_Random_Number(BigRng, BigSign, BigLen)

    ! free working variables
    NULLIFY(BigRng)
    IF (ALLOCATED(LocRng)) DEALLOCATE(LocRng)

    RETURN

END SUBROUTINE ApInt64_RandNumSub

!******************************************************************************

FUNCTION Generate_Random_Number(Prng, Sign, Length) RESULT(BigRnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate random number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: Prng
    tInteger,       INTENT(IN)      :: Sign
    tIndex,         INTENT(IN)      :: Length
    TYPE(ApInt64)                   :: BigRnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW

    BigRnd%Sign   = Sign
    BigRnd%Length = Length
    CALL MemAlloc(BigRnd%Digit, Length, StartID=0_kIndex)
    DO I = 0, Length-1
        BigRnd%Digit(I) = Prng%NextLong()
    END DO

    RETURN

END FUNCTION Generate_Random_Number

!******************************************************************************

MODULE FUNCTION ApInt64_GetLength(Num) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of digits counted as the ApInt number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: Num
    tIndex                      :: Length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Length = Num%Length

    RETURN

END FUNCTION ApInt64_GetLength

!******************************************************************************

END SUBMODULE SubClass_Api64_Auxiliary

!******************************************************************************
