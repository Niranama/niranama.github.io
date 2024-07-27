
MODULE ModBase_IO_Handlers
       
!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains common routines used to handle basic input and output operations.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SIntUtil,   ONLY: ToChar => ToDecStrSigned

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! open and close procedures
    PUBLIC :: OpenInputFile
    PUBLIC :: OpenNewOutputFile
    PUBLIC :: OpenOutputFile
    PUBLIC :: OpenNewFile
    PUBLIC :: OpenFile
    PUBLIC :: CloseFile
    PUBLIC :: CloseOpenFiles
    ! get unit number procedures
    PUBLIC :: GetNewIOUnit
    PUBLIC :: GetFileIOUnit
    PUBLIC :: GetIOUnits_AllOpenFile
    ! inquiry procedures
    PUBLIC :: IsFileOpen
    PUBLIC :: DoesFileExist
    ! reading and writing procedures
    PUBLIC :: Output_Message

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    !  Largest allowed Unit number (or a large number, if none)
    tInteger,  PARAMETER    :: MaxUnitNumber = 1000
    ! name for audit file
    tCharStar, PARAMETER    :: AudFileName = LibName // '.Aud'
    ! Size of character variable for storing error messages.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE IsFileOpen
        !^ **Function Interface**: IsFileOpen <br>
        !  **Purpose**:  To check whether the specified file is open or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsFileOpen(FileIOUnit) <br>
        !   --->    IF (IsFileOpen(FileName)) DoSomething
        MODULE PROCEDURE IsFileOpen_UnitNumber
        MODULE PROCEDURE IsFileOpen_FileName
    END INTERFACE
    INTERFACE CloseFile
        !^ **Subroutine Interface**: CloseFile <br>
        !  **Purpose**:  To close the specified file. <br>
        !  **Usage**: <br>
        !   --->    CALL CloseFile(FileIOUnit) <br>
        !   --->    CALL CloseFile(FileName))
        MODULE PROCEDURE CloseFile_Unit
        MODULE PROCEDURE CloseFile_Name
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

FUNCTION OpenInputFile(Name) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a file associated with the specified name for reading.
    !   A valid (positive) unit number is return if the specified file
    !   is opened successfully.  Otherwise, it is set to -1. 

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
    tInteger                :: IOUnit   !! unit number

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tInteger            :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! first, check the input
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (.NOT.DoesFileExist(Name)) THEN
        CALL Output_Message('---             WARNING ERROR MESSAGE           ---')
        CALL Output_Message('The specified file (' // Name // ') does not exist.')
        CALL Output_Message('---------------------------------------------------')
        IOUnit = -1
        RETURN
    END IF
    
    IF (IsFileOpen(Name)) THEN
        ! the specified file is already open so get the unit number associated with it
        IOUnit = GetFileIOUnit(Name)
    ELSE
        ! open file for reading
        IOUnit = GetNewIOUnit()
        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='OLD', ACTION='READ', &
             IOSTAT=iStat, IOMSG=cMsg)
    
        ! check io status
        IF (iStat /= 0) THEN
            CALL Output_Message('---       WARNING ERROR MESSAGE          ---')
            CALL Output_Message('Cannot open ' // Name // ' as an input file.')
            CALL Output_Message('IOSTAT = ' // ToChar(iStat))
            CALL Output_Message('IOMSG  = ' // cMsg) 
            CALL Output_Message('--------------------------------------------')
            IOUnit = -1
        END IF
    END IF

    RETURN
   
END FUNCTION OpenInputFile

!******************************************************************************

FUNCTION OpenNewOutputFile(Name,DirAcc,RecLen) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a new file associated with the specified name for writing.
    !   A valid (positive) unit number is return if the specified file
    !   is opened successfully.  Otherwise, it is set to -1. 

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: Name     !! file name
    tLogical, OPTIONAL, INTENT(IN)  :: DirAcc
    !^  flag indicating access mode <br>
    ! - true if requesting direct access mode <br>
    ! - false if requesting sequential access mode <br>
    ! - default = FalseVal <br>
    ! For direct access mode, the file is opened for an unformatted file. <br>
    ! For sequential access mode, the file is opened for a formatted file.
    tInteger, OPTIONAL, INTENT(IN)  :: RecLen
    !^  (fixed) length of records for direct access mode. <br>
    !   If DirAcc is present and true, RecLen must be specified.
    tInteger                        :: IOUnit   !! unit number
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical            :: Sequential
    tInteger            :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! set default, check the input
    Sequential = TrueVal
    IF (PRESENT(DirAcc)) Sequential = .NOT.DirAcc
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (DoesFileExist(Name)) THEN
        CALL Output_Message('---                     WARNING ERROR MESSAGE                   ---')
        CALL Output_Message('Cannot open an existing file (' // Name // ') as a new output file.')
        CALL Output_Message('-------------------------------------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (.NOT.Sequential.AND..NOT.PRESENT(RecLen)) THEN
        CALL Output_Message('---               WARNING ERROR MESSAGE              ---')
        CALL Output_Message('DirAcc is present and true, but RecLen is not specified.')
        CALL Output_Message('--------------------------------------------------------')
        IOUnit = -1
        RETURN
    END IF

    ! open file for writing
    IF (Sequential) THEN
        ! open sequential and formatted file
        IOUnit = GetNewIOUnit()
        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='NEW', ACTION='WRITE', &
             IOSTAT=iStat, IOMSG=cMsg)
    ELSE
        ! open direct access and unformatted file
        IF (PRESENT(RecLen)) THEN
            IOUnit = GetNewIOUnit()
            OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='NEW', ACTION='WRITE', &
                 ACCESS='DIRECT', RECL=RecLen, IOSTAT=iStat, IOMSG=cMsg)
        END IF
    END IF
    
    ! check io status
    IF (iStat /= 0) THEN
        CALL Output_Message('---          WARNING ERROR MESSAGE           ---')
        CALL Output_Message('Cannot open ' // Name // ' as a new output file.')
        CALL Output_Message('IOSTAT = ' // ToChar(iStat))
        CALL Output_Message('IOMSG  = ' // cMsg) 
        CALL Output_Message('------------------------------------------------')
        IOUnit = -1
    END IF

    RETURN
   
END FUNCTION OpenNewOutputFile

!******************************************************************************

FUNCTION OpenOutputFile(Name,Append,DirAcc,RecLen) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a file associated with the specified name for writing.
    !   A valid (positive) unit number is return if the specified file
    !   is opened successfully.  Otherwise, it is set to -1. <br>
    ! ------------------------------------------------------------------------------ <br>
    ! **Important note**: <br>
    !   Using this routine requires a careful consideration
    !   according to the technical notes below. <br>
    ! ------------------------------------------------------------------------------ <br>
    ! **Technical Notes**: <br>
    ! (1) If the specified file does not exists, a new output file will be opened and
    !   the argument Append is ignored. <br>
    ! (2) If the specified file exists and currently is open, all optional arguments
    !   are ignored and the unit number associated with it will be returned. <br>
    ! (3) If the specified file exists (but is not open) and Append is not present or false,
    !   the existing file will be deleted and a file with the same name will be open as a
    !   new output file. That is the existing one is replaced by the new one. <br>
    ! (3.1) However, If DirAcc is present and true but RecLen is NOT specified, the existing
    !   file will be left as it is, and the unit number is set to -1. <br>
    ! (4) If the specified file exists (but is not open) and Append is present and true,
    !   the existing file will be opened for writing where the new data are written after
    !   existing data. Both DirAcc and RecLen arguments are ignored since, for an existing
    !   file, existing access mode and format will be retained.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: Name     !! file name
    tLogical, OPTIONAL, INTENT(IN)  :: Append
    !^  flag indicating the file position <br>
    ! - true if the file is positioned at its terminal point <br>
    ! - false if the file is positioned at its initial point <br>
    ! - default = FalseVal
    tLogical, OPTIONAL, INTENT(IN)  :: DirAcc
    !^  flag indicating access mode <br>
    ! - true if requesting direct access mode <br>
    ! - false if requesting sequential access mode <br>
    ! - default = FalseVal <br>
    ! For direct access mode, the file is opened for an unformatted file. <br>
    ! For sequential access mode, the file is opened for a formatted file.
    tInteger, OPTIONAL, INTENT(IN)  :: RecLen
    !^  (fixed) length of records for direct access mode. <br>
    !   If DirAcc is present and true, RecLen must be specified.
    tInteger                        :: IOUnit   !! unit number
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical            :: Sequential
    tLogical            :: AsIs
    tInteger            :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! set defaults, check the input
    AsIs       = TrueVal
    Sequential = TrueVal
    IF (PRESENT(Append)) AsIs = .NOT.Append
    IF (PRESENT(DirAcc)) Sequential = .NOT.DirAcc
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    END IF
    
    IF (DoesFileExist(Name)) THEN
        IF (IsFileOpen(Name)) THEN
            ! technical note #2
            IOUnit = GetFileIOUnit(Name)
        ELSE
            IF (AsIs) THEN
                ! technical note #3
                IF (.NOT.Sequential.AND..NOT.PRESENT(RecLen)) THEN
                    ! technical note #3.1
                    CALL Output_Message('---               WARNING ERROR MESSAGE              ---')
                    CALL Output_Message('DirAcc is present and true, but RecLen is not specified.')
                    CALL Output_Message('--------------------------------------------------------')
                    IOUnit = -1
                    RETURN
                END IF
                IF (Sequential) THEN
                    ! replace existing file with new sequential and formatted file
                    IOUnit = GetNewIOUnit()
                    OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='REPLACE', ACTION='WRITE', &
                         IOSTAT=iStat, IOMSG=cMsg)
                ELSE
                    ! replace existing file with new direct access and unformatted file
                    IF (PRESENT(RecLen)) THEN
                        IOUnit = GetNewIOUnit()
                        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='REPLACE', ACTION='WRITE', &
                             ACCESS='DIRECT', RECL=RecLen, IOSTAT=iStat, IOMSG=cMsg)
                    END IF
                END IF
            ELSE
                ! technical note #4
                IOUnit = GetNewIOUnit()
                OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='OLD', ACTION='WRITE', &
                     POSITION='APPEND', IOSTAT=iStat, IOMSG=cMsg)
            END IF

            ! check io status
            IF (iStat /= 0) THEN
                CALL Output_Message('---         WARNING ERROR MESSAGE         ---')
                CALL Output_Message('Cannot open ' // Name // ' as an output file.')
                CALL Output_Message('IOSTAT = ' // ToChar(iStat))
                CALL Output_Message('IOMSG  = ' // cMsg) 
                CALL Output_Message('---------------------------------------------')
                IOUnit = -1
            END IF
        END IF
    ELSE
        ! technical note #1
        IOUnit = OpenNewOutputFile(Name,DirAcc,RecLen)
    END IF
    
    RETURN
   
END FUNCTION OpenOutputFile

!******************************************************************************

FUNCTION OpenNewFile(Name,DirAcc,RecLen) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a new file associated with the specified name for both reading and writing.
    !   A valid (positive) unit number is return if the specified file is opened successfully.
    !   Otherwise, it is set to -1. 

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: Name     !! file name
    tLogical, OPTIONAL, INTENT(IN)  :: DirAcc
    !^ flag indicating access mode <br>
    ! - true if requesting direct access mode <br>
    ! - false if requesting sequential access mode <br>
    ! - default = FalseVal <br>
    ! For direct access mode, the file is opened for an unformatted file. <br>
    ! For sequential access mode, the file is opened for a formatted file.
    tInteger, OPTIONAL, INTENT(IN)  :: RecLen
    !^  (fixed) length of records for direct access mode. <br>
    !   If DirAcc is present and true, RecLen must be specified.
    tInteger                        :: IOUnit   !! unit number
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical            :: Sequential
    tInteger            :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! set default, check the input
    Sequential = TrueVal
    IF (PRESENT(DirAcc)) Sequential = .NOT.DirAcc
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (DoesFileExist(Name)) THEN
        CALL Output_Message('---                 WARNING ERROR MESSAGE                ---')
        CALL Output_Message('Cannot open an existing file (' // Name // ') as a new file.')
        CALL Output_Message('------------------------------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (.NOT.Sequential.AND..NOT.PRESENT(RecLen)) THEN
        CALL Output_Message('---               WARNING ERROR MESSAGE              ---')
        CALL Output_Message('DirAcc is present and true, but RecLen is not specified.')
        CALL Output_Message('--------------------------------------------------------')
        IOUnit = -1
        RETURN
    END IF

    ! open file for reading and writing
    IF (Sequential) THEN
        ! open sequential and formatted file
        IOUnit = GetNewIOUnit()
        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='NEW', ACTION='READWRITE', &
             IOSTAT=iStat, IOMSG=cMsg)
    ELSE
        ! open direct access and unformatted file
        IF (PRESENT(RecLen)) THEN
            IOUnit = GetNewIOUnit()
            OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='NEW', ACTION='READWRITE', &
                 ACCESS='DIRECT', RECL=RecLen, IOSTAT=iStat, IOMSG=cMsg)
        END IF
    END IF
    
    ! check io status
    IF (iStat /= 0) THEN
        CALL Output_Message('---       WARNING ERROR MESSAGE       ---')
        CALL Output_Message('Cannot open ' // Name // ' as a new file.')
        CALL Output_Message('IOSTAT = ' // ToChar(iStat))
        CALL Output_Message('IOMSG  = ' // cMsg) 
        CALL Output_Message('-----------------------------------------')
        IOUnit = -1
    END IF

    RETURN
   
END FUNCTION OpenNewFile

!******************************************************************************

FUNCTION OpenFile(Name,Append,DirAcc,RecLen) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a file associated with the specified name for both reading and writing.
    !   A valid (positive) unit number is return if the specified file is opened successfully.
    !   Otherwise, it is set to -1. <br>
    ! ------------------------------------------------------------------------------ <br>
    ! **Important note**: <br>
    !   Using this routine requires a careful consideration
    !   according to the technical notes below. <br>
    ! ------------------------------------------------------------------------------ <br>
    ! **Technical Notes**: <br>
    ! (1) If the specified file does not exists, a new file will be opened and
    !   the argument Append is ignored. <br>
    ! (2) If the specified file exists and currently is open, all optional arguments
    !   are ignored and the unit number associated with it will be returned. <br>
    ! (3) If the specified file exists (but is not open) and Append is not present,
    !   the existing file will be open with "AsIs" position.  Both DirAcc and RecLen arguments
    !   are ignored since, for an existing file, existing access mode and format will be retained. <br>
    ! (4) If the specified file exists (but is not open) and Append is present and true,
    !   the existing file will be opened with "Append" position. Both DirAcc and RecLen arguments
    !   are ignored since, for an existing file, existing access mode and format will be retained. <br>
    ! (5) If the specified file exists (but is not open) and Append is present and false,
    !   the existing file will be deleted and a file with the same name will be open as a
    !   new file. That is the existing one is replaced by the new one. <br>
    ! (5.1) However, If DirAcc is present and true but RecLen is NOT specified, the existing
    !   file will be left as it is, and the unit number is set to -1.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: Name     !! file name
    tLogical, OPTIONAL, INTENT(IN)  :: Append
    !^  flag indicating the file position <br>
    ! - true if the file is positioned at its terminal point <br>
    ! - false if the file is positioned at its initial point <br>
    ! - default = FalseVal
    tLogical, OPTIONAL, INTENT(IN)  :: DirAcc
    !^  flag indicating access mode <br>
    ! - true if requesting direct access mode <br>
    ! - false if requesting sequential access mode <br>
    ! - default = FalseVal <br>
    ! For direct access mode, the file is opened for an unformatted file. <br>
    ! For sequential access mode, the file is opened for a formatted file.
    tInteger, OPTIONAL, INTENT(IN)  :: RecLen
    !^  (fixed) length of records for direct access mode. <br>
    !   If DirAcc is present and true, RecLen must be specified.
    tInteger                        :: IOUnit   !! unit number
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical            :: Sequential
    tInteger            :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! set defaults, check the input
    Sequential = TrueVal
    IF (PRESENT(DirAcc)) Sequential = .NOT.DirAcc
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    END IF
    
    IF (DoesFileExist(Name)) THEN
        IF (IsFileOpen(Name)) THEN
            ! technical note #2
            IOUnit = GetFileIOUnit(Name)
        ELSE
            IF (PRESENT(Append)) THEN
                IF (Append) THEN
                    ! technical note #4
                    IOUnit = GetNewIOUnit()
                    OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='OLD', ACTION='READWRITE', &
                         POSITION='APPEND', IOSTAT=iStat, IOMSG=cMsg)
                ELSE
                    ! technical note #5
                    IF (.NOT.Sequential.AND..NOT.PRESENT(RecLen)) THEN
                        ! technical note #5.1
                        CALL Output_Message('---               WARNING ERROR MESSAGE              ---')
                        CALL Output_Message('DirAcc is present and true, but RecLen is not specified.')
                        CALL Output_Message('--------------------------------------------------------')
                        IOUnit = -1
                        RETURN
                    END IF
                    IF (Sequential) THEN
                        ! replace existing file with new sequential and formatted file
                        IOUnit = GetNewIOUnit()
                        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='REPLACE', ACTION='READWRITE', &
                             IOSTAT=iStat, IOMSG=cMsg)
                    ELSE
                        ! replace existing file with new direct access and unformatted file
                        IF (PRESENT(RecLen)) THEN
                            IOUnit = GetNewIOUnit()
                            OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='REPLACE', ACTION='READWRITE', &
                                 ACCESS='DIRECT', RECL=RecLen, IOSTAT=iStat, IOMSG=cMsg)
                        END IF
                    END IF
                END IF
            ELSE
                ! technical note #3
                IOUnit = GetNewIOUnit()
                OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='OLD', ACTION='READWRITE', &
                     POSITION='ASIS', IOSTAT=iStat, IOMSG=cMsg)
            END IF
            
            ! check io status
            IF (iStat /= 0) THEN
                CALL Output_Message('---            WARNING ERROR MESSAGE            ---')
                CALL Output_Message('Cannot open ' // Name // ' for reading and writing.')
                CALL Output_Message('IOSTAT = ' // ToChar(iStat))
                CALL Output_Message('IOMSG  = ' // cMsg) 
                CALL Output_Message('---------------------------------------------------')
                IOUnit = -1
            END IF
        END IF
    ELSE
        ! technical note #1
        IOUnit = OpenNewFile(Name,DirAcc,RecLen)
    END IF
    
    RETURN
   
END FUNCTION OpenFile

!******************************************************************************

SUBROUTINE CloseFile_Unit(IOUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To close a file associated with the specified unit number.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger, INTENT(IN)    :: IOUnit   !! io unit number
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
   
!** FLOW

    CLOSE(UNIT = IOUnit)
   
    RETURN

END SUBROUTINE CloseFile_Unit

!******************************************************************************

SUBROUTINE CloseFile_Name(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To close a file associated with the specified unit number.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: IOUnit
   
!** FLOW

    IOUnit = GetFileIOUnit(Name)
    CLOSE(UNIT = IOUnit)
   
    RETURN

END SUBROUTINE CloseFile_Name

!******************************************************************************

SUBROUTINE CloseOpenFiles()

!** PURPOSE OF THIS SUBROUTINE:
    !^ To scan potential unit numbers and closes any that are still open.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    ! na
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical    :: EXISTS, OPENED
    tInteger    :: IOUnit
    tInteger    :: IOS
   
!** FLOW

    DO IOUnit = 1, MaxUnitNumber
        INQUIRE (UNIT = IOUnit, EXIST = EXISTS,  OPENED = OPENED, IOSTAT = IOS)
        IF (EXISTS .AND. OPENED .AND. IOS == 0) CLOSE(IOUnit)
    END DO
   
    RETURN

END SUBROUTINE CloseOpenFiles

!******************************************************************************

FUNCTION GetFileIOUnit(Name) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^ To return a unit number of the file associated with the specified name.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
    tInteger                :: IOUnit   !! unit number 
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    INQUIRE (FILE = Name, NUMBER = IOUnit)

    RETURN
   
END FUNCTION GetFileIOUnit

!******************************************************************************

FUNCTION GetNewIOUnit() RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^ To return a input/output unit number that can exist and is not connected.

!** FUNCTION ARGUMENT DEFINITIONS:
    tInteger            :: IOUnit   !! io unit number
     
!** FUNCTION PARAMETER DEFINITIONS:
    !  IO Status Values:
    tInteger, PARAMETER :: END_OF_RECORD = -2
    tInteger, PARAMETER :: END_OF_FILE = -1
    !  Indicate default input and output units:
    tInteger, PARAMETER :: DEFAULT_INPUT_UNIT = 5
    tInteger, PARAMETER :: DEFAULT_OUTPUT_UNIT = 6
    !  Indicate number and value of pre-connected units
    tInteger, PARAMETER :: NUMBER_OF_PRECONNECTED_UNITS = 2
    tInteger, PARAMETER :: PRECONNECTED_UNITS (NUMBER_OF_PRECONNECTED_UNITS) = (/ 5, 6 /)

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical    :: EXISTS, OPENED
    tInteger    :: IOS

!** FLOW

    DO IOUnit = 11, MaxUnitNumber
        IF (IOUnit == DEFAULT_INPUT_UNIT .OR. IOUnit == DEFAULT_OUTPUT_UNIT) CYCLE
        IF (ANY (IOUnit == PRECONNECTED_UNITS)) CYCLE
        INQUIRE (UNIT = IOUnit, EXIST = EXISTS,  OPENED = OPENED, IOSTAT = IOS)
        IF (EXISTS .AND. .NOT. OPENED .AND. IOS == 0) RETURN      ! result is set in IOUnit
    END DO
   
    IOUnit = -1

    RETURN
   
END FUNCTION GetNewIOUnit

!******************************************************************************

FUNCTION GetIOUnits_AllOpenFile(IOUnits) RESULT(nUnits)

!** PURPOSE OF THIS FUNCTION:
    !^ To return a number of currently opened files and their associated unit numbers.

!** FUNCTION ARGUMENT DEFINITIONS:
    tInteger, ALLOCATABLE, INTENT(OUT)  :: IOUnits(:)   !! unit numbers of opened files
    tInteger                            :: nUnits       !! number of opened files
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tInteger    :: Step, iUnit

!** FLOW

    DO Step = 1, 2
        nUnits = 0
        DO iUnit = 11, MaxUnitNumber
            IF (IsFileOpen(iUnit)) THEN
                IF (Step == 1) THEN
                    ! only count the opened file
                    nUnits = nUnits + 1
                ELSE
                    ! count and get the unit number of the opened file
                    nUnits = nUnits + 1
                    IOUnits(nUnits) = iUnit
                END IF
            END IF
        END DO
        IF (Step == 1) THEN
            ! allocate output data
            ALLOCATE(IOUnits(nUnits))
        END IF
    END DO

    RETURN
   
END FUNCTION GetIOUnits_AllOpenFile

!******************************************************************************

FUNCTION IsFileOpen_UnitNumber(IOUnit) RESULT(Status)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether the file associated with the specified unit number is open or not

!** FUNCTION ARGUMENT DEFINITIONS:
    tInteger, INTENT(IN)    :: IOUnit   !! io unit number
    tLogical                :: Status   !! status flag; true if the file is open
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    INQUIRE (UNIT = IOUnit, OPENED = Status)
    
    RETURN
   
END FUNCTION IsFileOpen_UnitNumber

!******************************************************************************

FUNCTION IsFileOpen_FileName(Name) RESULT(Status)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether the file associated with the specified name is open or not

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
    tLogical                :: Status   !! status flag; true if the file is open
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    INQUIRE (FILE = Name, OPENED = Status)
    
    RETURN
   
END FUNCTION IsFileOpen_FileName

!******************************************************************************

FUNCTION DoesFileExist(Name) RESULT(Status)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether the file associated with the specified name exists or not

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
    tLogical                :: Status   !! status flag; true if the file exists
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    INQUIRE (FILE = Name, EXIST = Status)
    
    RETURN
   
END FUNCTION DoesFileExist

!******************************************************************************

SUBROUTINE Output_Message(Message,IOUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To display the specified message on default output (an audit file) or the
    !   indicated file unit number if specified.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)      :: Message
    tInteger, OPTIONAL, INTENT(INOUT)   :: IOUnit
          
!** SUBROUTINE PARAMETER DEFINITIONS:
    tCharStar, PARAMETER    :: OutputFormat = '(2X,A)'
  
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: OutputUnit

!** FLOW
    
    IF (PRESENT(IOUnit)) THEN
        
        ! check whether the specified unit number is connected to an open file
        IF (.NOT.IsFileOpen(IOUnit)) THEN

            ! open file
            IOUnit = OpenOutputFile('UserOutput.Txt',Append=TrueVal)

        END IF
        
        ! write message to the specified file unit number
        IF (IOUnit /= -1) THEN
            WRITE(IOUnit,OutputFormat) TRIM(ADJUSTL(Message))
        ELSE
            ! error occurred while opening file
            WRITE(*,*) 'Error occurred while opening a user-specified file.'
            WRITE(*,*) 'The specified message is "' // TRIM(ADJUSTL(Message)) // '".'
        END IF

    ELSE
        
        ! write to the log file so check whether the log file is currently open or not
        IF (IsFileOpen(AudFileName)) THEN
            
            ! get existing unit number for the opened file
            OutputUnit = GetFileIOUnit(AudFileName)
            
            ! then write message to the audit file
            WRITE(OutputUnit,OutputFormat) TRIM(ADJUSTL(Message))
            
        ELSE
            
            ! open file
            OutputUnit = OpenOutputFile(AudFileName,Append=TrueVal)
        
            IF (OutputUnit /= -1) THEN
                WRITE(OutputUnit,OutputFormat) TRIM(ADJUSTL(Message))
            ELSE
                ! error occurred while opening file
                WRITE(*,*) 'Error occurred while opening an audit file.'
                WRITE(*,*) 'The specified message is "' // TRIM(ADJUSTL(Message)) // '".'
            END IF

            ! then write message to the audit file
            WRITE(OutputUnit,OutputFormat) TRIM(ADJUSTL(Message))
            
        END IF
        
    ENDIF

    RETURN

END SUBROUTINE Output_Message

!******************************************************************************

END MODULE ModBase_IO_Handlers

!******************************************************************************
