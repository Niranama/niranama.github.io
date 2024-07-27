
SUBMODULE (Class_FvlStr) SubClass_FvlStr_Miscellaneous

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of user-defined input/output procedures
!   and other miscellaneous procedures for the *FvlStr* object.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na
    
!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** SUBMODULE SUBROUTINES OR FUNCTIONS:

!--------------------------------------------------------------------------------------
!                   USER-DEFINED INPUT/OUTPUT PROCEDURES
!--------------------------------------------------------------------------------------

MODULE SUBROUTINE Write_Unformatted(vStr,IOUnit,IOStat,IOMsg)

!** PURPOSE OF THIS ROUTINE:
    !^ To write a character string of the FvlStr object to a connected unformatted unit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)       :: vStr     !! FvlStr object
    tInteger,      INTENT(IN)       :: IOUnit   !! connected io unit number
    tInteger,      INTENT(OUT)      :: IOStat   !! status of io operation
    tCharStar,     INTENT(INOUT)    :: IOMsg    !! an io message if is IOStat is non-zero

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (ALLOCATED(vStr%cStr)) THEN

        ! first, write length of the character string
        WRITE(UNIT=IOUnit, IOSTAT=IOStat, IOMSG=IOMsg) LEN(vStr%cStr)

        IF (IOStat /= 0) THEN
            CALL Handle_ErrLevel('Write_Unformatted', ModName, ErrSevere, &
                       'An error occurred while writing the length of the character string.')
        END IF

        ! then, write the character string
        WRITE(UNIT=IOUnit, IOSTAT=IOStat, IOMSG=IOMsg) vStr%cStr

    END IF

    RETURN

END SUBROUTINE Write_Unformatted

!******************************************************************************

MODULE SUBROUTINE Read_Unformatted(vStr,IOUnit,IOStat,IOMsg)

!** PURPOSE OF THIS ROUTINE:
    !^ To read a character sequence from a connected unformatted unit into the FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
    tInteger,      INTENT(IN)       :: IOUnit   !! connected io unit number
    tInteger,      INTENT(OUT)      :: IOStat   !! status of io operation
    tCharStar,     INTENT(INOUT)    :: IOMsg    !! an io message if is IOStat is non-zero

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex          :: RecLen

!** FLOW:

    ! first, read the record length
    READ(UNIT=IOUnit, IOSTAT=IOStat, IOMSG=IOMsg) RecLen

    IF (IOStat /= 0) THEN
        vStr%cStr = ''
        CALL Handle_ErrLevel('Read_Unformatted', ModName, ErrSevere, &
                   'An error occurred while reading the length of the record.')
    END IF

    ! read record data
    ALLOCATE(tCharLen(RecLen) :: vStr%cStr)
    READ(UNIT=IOUnit, IOSTAT=IOStat, IOMSG=IOMsg) vStr%cStr

    RETURN

END SUBROUTINE Read_Unformatted

!******************************************************************************

MODULE SUBROUTINE Write_Formatted(vStr,IOUnit,IOType,VList,IOStat,IOMsg)

!** PURPOSE OF THIS ROUTINE:
    !^ To write a character string of the FvlStr object to a connected formatted unit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)       :: vStr     !! FvlStr object
    tInteger,      INTENT(IN)       :: IOUnit   !! connected io unit number
    tCharStar,     INTENT(IN)       :: IOType   !! type of io
    tInteger,      INTENT(IN)       :: VList(:) !! list of integer array from part of the DT edit descriptor
    tInteger,      INTENT(OUT)      :: IOStat   !! status of io operation
    tCharStar,     INTENT(INOUT)    :: IOMsg    !! an io message if is IOStat is non-zero

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (ALLOCATED(vStr%cStr).AND.(vStr%Length() > 0)) THEN

        SELECT CASE (IOType)
        CASE ('LISTDIRECTED')
            ! write the character string
            WRITE(UNIT=IOUnit, FMT=*, IOSTAT=IOStat, IOMSG=IOMsg) vStr%cStr
        CASE ('DT')
            ! write the character string
            WRITE(UNIT=IOUnit, FMT='(A)', IOSTAT=IOStat, IOMSG=IOMsg) vStr%cStr
        CASE DEFAULT
            CALL Handle_ErrLevel('Write_Formatted', ModName, ErrSevere, &
                       'The routine does not support the specified IOType ('// IOType // ').')
        END SELECT

        ! add dummy statements
        ASSOCIATE(VArray => VList); END ASSOCIATE

    END IF

    RETURN

END SUBROUTINE Write_Formatted

!******************************************************************************

MODULE SUBROUTINE Read_Formatted(vStr,IOUnit,IOType,VList,IOStat,IOMsg)

!** PURPOSE OF THIS ROUTINE:
    !^ To read a character sequence from a connected formatted unit into the FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
    tInteger,      INTENT(IN)       :: IOUnit   !! connected io unit number
    tCharStar,     INTENT(IN)       :: IOType   !! type of io
    tInteger,      INTENT(IN)       :: VList(:) !! list of integer array from part of the DT edit descriptor
    tInteger,      INTENT(OUT)      :: IOStat   !! status of io operation
    tCharStar,     INTENT(INOUT)    :: IOMsg    !! an io message if is IOStat is non-zero

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (IOType)
    CASE ('LISTDIRECTED')
        CALL ReadRecord(IOUnit, vStr%cStr, IOStat, IOMsg)
    CASE ('DT')
        CALL ReadRecord(IOUnit, vStr%cStr, IOStat, IOMsg)
    CASE DEFAULT
        CALL Handle_ErrLevel('Read_Formatted', ModName, ErrSevere, &
                   'The routine does not support the specified IOType ('// IOType // ').')
    END SELECT

    ! add dummy statements
    ASSOCIATE(VArray => VList); END ASSOCIATE

    RETURN

    CONTAINS

    SUBROUTINE ReadRecord(IOUnit,RecDat,IOStat,IOMsg)

    !** PURPOSE OF THIS ROUTINE:
        !^ To read a whole record from a connected formatted unit.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger,   INTENT(IN)      :: IOUnit   !! connected io unit number
        tCharAlloc, INTENT(INOUT)   :: RecDat   !! data record
        tInteger,   INTENT(OUT)     :: IOStat   !! status of io operation
        tCharStar,  INTENT(INOUT)   :: IOMsg    !! an io message if is IOStat is non-zero

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tIndex, PARAMETER   :: BuffSize = 256

    !** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        tCharLen(BuffSize)  :: Buffer
        tIndex              :: nChar

    !** FLOW:

        ! initialize
        RecDat = ''
        nChar = 0
        IOStat = 0

        DO WHILE (IOStat >= 0)

            ! read record data without advancing
            READ(UNIT=IOUnit, FMT='(A)', IOSTAT=IOStat, IOMSG=IOMsg, &
                 ADVANCE='NO', SIZE=nChar) Buffer

            ! check io status
            IF (IOStat > 0) EXIT

            ! add buffer to record data
            RecDat = RecDat // Buffer(1:nChar)

        END DO

        IF (IS_IOSTAT_EOR(IOStat)) THEN
            ! reset IOStat for end of record status
            IOStat = 0
        END IF

        RETURN

    END SUBROUTINE ReadRecord

    !******************************************************************************

END SUBROUTINE Read_Formatted

!******************************************************************************

MODULE SUBROUTINE WriteOutput(vStr,IOUnit,IOStat,IOMsg)

!** PURPOSE OF THIS ROUTINE:
    !^ To write a character string of the FvlStr object to a connected formatted unit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
    tInteger,            INTENT(IN)     :: IOUnit   !! connected io unit number
    tInteger,  OPTIONAL, INTENT(OUT)    :: IOStat   !! status of io operation
    tCharStar, OPTIONAL, INTENT(INOUT)  :: IOMsg    !! an io message if is IOStat is non-zero

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger        :: IO_Stat      ! status of io operation
    tCharLen(128)   :: IO_Msg       ! an io message if is IOStat is non-zero

!** FLOW:

    IF (ALLOCATED(vStr%cStr).AND.(vStr%Length() > 0)) THEN

        ! write the character string
        WRITE(UNIT=IOUnit, FMT='(A)', IOSTAT=IO_Stat, IOMSG=IO_Msg) vStr%cStr

        ! report io information
        IF (PRESENT(IOStat)) IOStat = IO_Stat
        IF (PRESENT(IOMsg))  IOMsg  = IO_Msg

    END IF

    RETURN

END SUBROUTINE WriteOutput

!******************************************************************************

MODULE SUBROUTINE ReadInput(vStr,IOUnit,IOStat,IOMsg)

!** PURPOSE OF THIS ROUTINE:
    !^ To read a character sequence from a connected formatted unit into the FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(INOUT) :: vStr     !! FvlStr object
    tInteger,             INTENT(IN)    :: IOUnit   !! connected io unit number
    tInteger,   OPTIONAL, INTENT(OUT)   :: IOStat   !! status of io operation
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: IOMsg    !! an io message

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc      :: RecDat
    tInteger        :: IO_Stat  ! status of io operation
    tCharLen(256)   :: IO_Msg   ! an io message if is IOStat is non-zero

!** FLOW:

    ! read record
    CALL ReadRecord(RecDat, IO_Stat, IO_Msg, IOUnit)

    ! transfer optional output
    IF (PRESENT(IOStat)) IOStat = IO_Stat
    IF (PRESENT(IOMsg))  IOMsg  = TRIM(IO_Msg)

    IF (IO_Stat == 0) THEN
        ! transfer output
        vStr%cStr = RecDat
    ELSEIF (.NOT.IS_IOSTAT_END(IO_Stat)) THEN
        CALL Handle_ErrLevel('ReadInput', ModName, ErrSevere, 'Error message = '// TRIM(IO_Msg))
    END IF

    RETURN

    CONTAINS

    SUBROUTINE ReadRecord(RecDat,IOStat,IOMsg,IOUnit)

    !** PURPOSE OF THIS ROUTINE:
        ! To read a whole record from a connected formatted unit.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharAlloc, INTENT(INOUT)   :: RecDat   ! data record
        tInteger,   INTENT(OUT)     :: IOStat   ! status of io operation
        tCharStar,  INTENT(INOUT)   :: IOMsg    ! an io message if is IOStat is non-zero
        tInteger,   INTENT(IN)      :: IOUnit   ! connected io unit number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tIndex, PARAMETER   :: BuffSize = 256

    !** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        tCharLen(BuffSize)  :: Buffer
        tIndex              :: nChar

    !** FLOW:

        ! initialize
        RecDat = ''
        nChar = 0
        IOStat = 0

        DO WHILE (IOStat >= 0)

            ! read record data without advancing
            READ(UNIT=IOUnit, FMT='(A)', IOSTAT=IOStat, IOMSG=IOMsg, &
                 ADVANCE='NO', SIZE=nChar) Buffer

            ! check io status
            IF (IOStat > 0) EXIT

            ! add buffer to record data
            RecDat = RecDat // Buffer(1:nChar)

        END DO

        IF (IS_IOSTAT_EOR(IOStat)) THEN
            ! reset IOStat for end of record status
            IOStat = 0
        END IF

        RETURN

    END SUBROUTINE ReadRecord

    !******************************************************************************

END SUBROUTINE ReadInput

!******************************************************************************

MODULE SUBROUTINE ReadComnandLine(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To read a character sequence from a command line into the FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger        :: IOStat   ! status of io operation
    tCharLen(256)   :: IOMsg    ! an io message if is IOStat is non-zero
    tCharLen(256)   :: RecDat

!** FLOW:

    READ(UNIT=*, FMT='(A)', IOSTAT=IOStat, IOMSG=IOMsg) RecDat

    IF (IOStat == 0) THEN
        ! transfer output
        vStr%cStr = TRIM(ADJUSTL(RecDat))
    ELSE
        CALL Handle_ErrLevel('ReadComnandLine', ModName, ErrSevere, 'Error message = '// TRIM(IOMsg))
    END IF

    RETURN

END SUBROUTINE ReadComnandLine

!--------------------------------------------------------------------------------------
!                               AUXILIARY ROUTINES
!--------------------------------------------------------------------------------------

MODULE SUBROUTINE SwapFvlStr(AVal,BVal)

!** PURPOSE OF THIS SUBROUTINE
    !^ To swap values of two FvlStr objects.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT)  :: AVal    !! value of A
    TYPE(FvlStr), INTENT(INOUT)  :: BVal    !! value of B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: TempVal

!** FLOW:

    ! swap value
    TempVal = AVal%cStr
    AVal%cStr = BVal%cStr
    BVal%cStr = TempVal

    RETURN

END SUBROUTINE SwapFvlStr

!******************************************************************************

MODULE SUBROUTINE SwapFvlStrArray(AVal,BVal)

!** PURPOSE OF THIS SUBROUTINE
    !^ To swap values of two arrays of FvlStr objects.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT)  :: AVal(:)  !! array A of FvlStr objects
    TYPE(FvlStr), INTENT(INOUT)  :: BVal(:)  !! array B of FvlStr objects

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, N

!** FLOW:

    N = MIN(SIZE(AVal), SIZE(BVal))
    DO I = 1, N
        CALL Swap(Aval(I), BVal(I))
    END DO

    RETURN

END SUBROUTINE SwapFvlStrArray

!******************************************************************************

MODULE SUBROUTINE FvlStrArray_From_ChrStrArray(vStr,cStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To assign an array of FvlStr objects from an array of character strings. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(OUT)   :: vStr(:)          !! array of FvlStr objects
    tCharStar,    INTENT(IN)    :: cStr(SIZE(vStr)) !! array of character strings

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW

    DO I = 1, SIZE(vStr)
        vStr(I)%cStr = cStr(I)
    END DO

    RETURN

END SUBROUTINE FvlStrArray_From_ChrStrArray

!******************************************************************************

MODULE SUBROUTINE FvlStrArray_From_ChrStr(vStr,cStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To assign an array of FvlStr objects from a character string. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(OUT)   :: vStr(:)  !! array of FvlStr objects
    tCharStar,    INTENT(IN)    :: cStr     !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW

    DO I = 1, SIZE(vStr)
        vStr(I)%cStr = cStr
    END DO

    RETURN

END SUBROUTINE FvlStrArray_From_ChrStr

!******************************************************************************

END SUBMODULE SubClass_FvlStr_Miscellaneous

!******************************************************************************
