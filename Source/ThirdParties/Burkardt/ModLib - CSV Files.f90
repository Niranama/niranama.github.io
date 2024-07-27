
MODULE ModLib_CSV_Files
       
!** PURPOSE OF THIS MODULE:
    ! contains utility subroutines for read and write CSV files
    !
    ! Note: most of these routines are from CSV_IO package by
    !   John Burkardt

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_IO_Handlers

    IMPLICIT NONE    ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! high-level routines
    PUBLIC :: ReportCSVData
    ! low-level routines
    PUBLIC :: CSV_Value_Count
    PUBLIC :: CSV_File_Close_Read
    PUBLIC :: CSV_File_Close_Write
    PUBLIC :: CSV_File_Header_Write
    PUBLIC :: CSV_File_Line_Count
    PUBLIC :: CSV_File_Record_Write
    PUBLIC :: CSV_File_Open_Read
    PUBLIC :: CSV_File_Open_Write
    PUBLIC :: CSV_Record_Append_I4
    PUBLIC :: CSV_Record_Append_Real
    PUBLIC :: CSV_Record_Append_S
    
    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    CHARACTER(LEN=*), PARAMETER     :: ModName = 'ModLib_CSV_Files'
    INTEGER,          PARAMETER     :: SP = KIND(1.0E0)
    INTEGER,          PARAMETER     :: DP = KIND(1.0D0)

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    INTERFACE ReportCSVData
        MODULE PROCEDURE ReportCSVData1D
        MODULE PROCEDURE ReportCSVData2D
    END INTERFACE
    INTERFACE CSV_Record_Append_Real
        MODULE PROCEDURE CSV_Record_Append_R4
        MODULE PROCEDURE CSV_Record_Append_R8
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
    
!**************************************************************************************
!*****   The followings are high-level routines to perform almost complete task.  *****
!**************************************************************************************

SUBROUTINE ReportCSVData1D(NVar, VarName, VarVal, FileName)

!** PURPOSE OF THIS SUBROUTINE:
    !    To report data for debugging

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER,           INTENT(IN)    :: NVar
    CHARACTER(LEN=*),  INTENT(IN)    :: VarName(NVar)
    REAL(DP),          INTENT(IN)    :: VarVal(NVar)
    CHARACTER(LEN=*),  OPTIONAL     :: FILEName

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=80)   :: CSVFILEName
    INTEGER             :: CSVFILEUnit
    CHARACTER(LEN=200)  :: RECORD
    INTEGER             :: I, IPOS

!** FLOW
    
    IF (PRESENT(FileName)) THEN
        IPOS = INDEX(FileName,'.CSV')
        IF (IPOS.NE.0) THEN
            CSVFILEName = TRIM(FileName)
        ELSE
            CSVFILEName = TRIM(FileName) //'.CSV'
        END IF
    ELSE
        CSVFILEName = 'ReportData1D.CSV'
    END IF

    ! open file and write header
    CALL CSV_FILE_OPEN_WRITE ( CSVFILEName, CSVFILEUnit )
    RECORD = ' '
    CALL CSV_RECORD_APPEND_S ( 'Variables', RECORD )
    CALL CSV_RECORD_APPEND_S ( 'Values', RECORD )
    CALL CSV_FILE_RECORD_WRITE ( CSVFILEUnit, RECORD )
    ! write given input data
    DO I = 1, NVar
        RECORD = ' '
        CALL CSV_RECORD_APPEND_S ( VarName(I), RECORD )
        CALL CSV_RECORD_APPEND_REAL ( VarVal(I), RECORD )
        CALL CSV_FILE_RECORD_WRITE ( CSVFILEUnit, RECORD )
    END DO
    ! close file
    CALL CSV_FILE_CLOSE_WRITE ( CSVFILEUnit )

    RETURN

END SUBROUTINE ReportCSVData1D

!**************************************************************************************

SUBROUTINE ReportCSVData2D(NRow, NCol, VarVal, FileName, RowHeader, ColHeader)

!** PURPOSE OF THIS SUBROUTINE:
    !    To report data for debugging

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER,            INTENT(IN)    :: NRow
    INTEGER,            INTENT(IN)    :: NCol
    REAL(DP),           INTENT(IN)    :: VarVal(NRow,NCol)
    CHARACTER(LEN=80),  OPTIONAL    :: FILEName
    CHARACTER(LEN=15),  OPTIONAL    :: RowHeader(NRow)
    CHARACTER(LEN=15),  OPTIONAL    :: ColHeader(NCol)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=80)   :: CSVFILEName
    INTEGER             :: CSVFILEUnit
    CHARACTER(LEN=200)  :: RECORD
    INTEGER             :: I, J, IPOS

!** FLOW
    
    IF (PRESENT(FileName)) THEN
        IPOS = INDEX(FileName,'.CSV')
        IF (IPOS.NE.0) THEN
            CSVFILEName = TRIM(FileName)
        ELSE
            CSVFILEName = TRIM(FileName) //'.CSV'
        END IF
    ELSE
        CSVFILEName = 'ReportData2D.CSV'
    END IF

    ! open file and write column header
    CALL CSV_FILE_OPEN_WRITE ( CSVFILEName, CSVFILEUnit )
    IF (PRESENT(RowHeader).AND.PRESENT(RowHeader)) THEN
        RECORD = ' '
        CALL CSV_RECORD_APPEND_S ( '***', RECORD )
        DO J = 1, NCol
            CALL CSV_RECORD_APPEND_S ( ColHeader(J), RECORD )
        END DO
        CALL CSV_FILE_RECORD_WRITE ( CSVFILEUnit, RECORD )
    ELSEIF (PRESENT(ColHeader)) THEN
        RECORD = ' '
        DO J = 1, NCol
            CALL CSV_RECORD_APPEND_S ( ColHeader(J), RECORD )
        END DO
        CALL CSV_FILE_RECORD_WRITE ( CSVFILEUnit, RECORD )
    END IF
    ! write given input data
    DO I = 1, NRow
        RECORD = ' '
        IF (PRESENT(RowHeader)) THEN
            CALL CSV_RECORD_APPEND_S ( RowHeader(I), RECORD )
        END IF
        DO J = 1, NCol
            CALL CSV_RECORD_APPEND_REAL ( VarVal(I,J), RECORD )
        END DO
        CALL CSV_FILE_RECORD_WRITE ( CSVFILEUnit, RECORD )
    END DO
    ! close file
    CALL CSV_FILE_CLOSE_WRITE ( CSVFILEUnit )

    RETURN

END SUBROUTINE ReportCSVData2D

!**************************************************************************************
!*****   The followings are low-level routines to perform each specific task.     *****
!**************************************************************************************

SUBROUTINE CSV_Value_Count ( CSV_Record, CSV_Record_Status, Value_Count )

!*****************************************************************************
!
!  CSV_COUNT counts the number of values in a CSV RECORD.
!
    IMPLICIT NONE

    CHARACTER CSV_Char
    CHARACTER CSV_Char_Old
    INTEGER CSV_Len
    INTEGER CSV_Loc
    CHARACTER(LEN=*) CSV_Record
    INTEGER CSV_Record_Status
    CHARACTER :: TAB = ACHAR( 9 )
    INTEGER Value_Count
    INTEGER Word_Length
    !
    !  No values so far.
    !
    Value_Count = 0
    !
    !  We begin in "unquoted" Status.
    !
    CSV_Record_Status = 0
    !
    !  How many CHARACTERs in the RECORD?
    !
    CSV_Len = Len_TRIM( CSV_Record )
    !
    !  Count number of CHARACTERs in each word.
    !
    Word_Length = 0
    !
    !  Consider each CHARACTER.
    !
    CSV_Char_Old = ','

    DO CSV_Loc = 1, CSV_Len

        CSV_Char = CSV_Record(CSV_Loc:CSV_Loc)
        !
        !  Each comma divides one value from another.
        !
        IF ( CSV_Char_Old == ',' ) THEN

            Value_Count = Value_Count + 1
            Word_Length = 0
        !
        !  For quotes, try using CSV_Record_Status to count the number of
        !  quoted CHARACTERs.
        !
        ELSE IF ( CSV_Char == '"' ) THEN

            IF ( 0 < CSV_Record_Status ) THEN
                CSV_Record_Status = 0
            ELSE
                CSV_Record_Status = CSV_Record_Status + 1
            END IF
        !
        !  Ignore blanks
        !
        ELSE IF ( CSV_Char == ' ' .OR. CSV_Char == TAB ) THEN
        !
        !  Add CHARACTER to Length of word.
        !
        ELSE

            Word_Length = Word_Length + 1

            IF ( Value_Count == 0 ) THEN
                Value_Count = 1
            END IF

        END IF

        CSV_Char_Old = CSV_Char

    END DO

    RETURN
  
END SUBROUTINE CSV_Value_Count

!**************************************************************************************

SUBROUTINE CSV_File_Close_Read( CSV_File_Unit )

!*****************************************************************************
!
!  CSV_File_Close_Read closes a CSV File for Reading.
!
!  PARAMETERs:
!
!    Input, INTEGER CSV_File_Unit, the Unit number
!
    IMPLICIT NONE

    INTEGER CSV_File_Unit

    CLOSE( Unit = CSV_File_Unit )

    RETURN

END SUBROUTINE CSV_File_Close_Read

!**************************************************************************************

SUBROUTINE CSV_File_Close_Write( CSV_File_Unit )

!*****************************************************************************
!
!  CSV_File_Close_Write closes a CSV File for writing.
!
!  PARAMETERs:
!
!    Input, INTEGER CSV_File_Unit, the Unit number
!
    IMPLICIT NONE

    INTEGER CSV_File_Unit

    CLOSE( Unit = CSV_File_Unit )

    RETURN

END SUBROUTINE CSV_File_Close_Write

!**************************************************************************************

SUBROUTINE CSV_File_Header_Write( CSV_File_Unit, Header )

!*****************************************************************************
!
!  CSV_File_Header_Write writes a Header to a CSV File.
!
!  PARAMETERs:
!
!    Input, INTEGER CSV_File_Unit, the Unit number
!
!    Input, CHARACTER(LEN=*) Header, the Header.
!
    IMPLICIT NONE

    INTEGER CSV_File_Unit
    CHARACTER(LEN=*) Header

    WRITE( CSV_File_Unit, '(a)' ) TRIM( Header )

    RETURN

END SUBROUTINE CSV_File_Header_Write

!**************************************************************************************

SUBROUTINE CSV_File_Line_Count ( CSV_File_Name, Line_SpcVm )

!*****************************************************************************
!
!  CSV_File_Line_Count counts the number of Lines in a CSV File.
!
!  PARAMETERs:
!
!    Input, CHARACTER(LEN=*) CSV_File_Name, the name of the File.
!
!    Output, INTEGER Line_SpcVm, the number of Lines.
!
    IMPLICIT NONE

    CHARACTER(LEN=*) CSV_File_Name
    INTEGER IError
    INTEGER Input_Status
    INTEGER Input_Unit
    CHARACTER ( Len = 1023 ) Line
    INTEGER Line_SpcVm

    Line_SpcVm = -1

    Input_Unit = GetNewIOUnit ( )

    OPEN( Unit = Input_Unit, File = CSV_File_Name, Status = 'Old', &
         IOStat = Input_Status )

    IF ( Input_Status /= 0 ) THEN
        CALL Handle_ErrOpen('CSV_File_Line_Count', ModName, CSV_File_Name, ErrSevere)
    END IF

    Line_SpcVm = 0

    DO

        READ( Input_Unit, '(a)', IOStat = Input_Status ) Line

        IF ( Input_Status /= 0 ) THEN
            IError = Line_SpcVm
            EXIT
        END IF

        Line_SpcVm = Line_SpcVm + 1

    END DO

    CLOSE( Unit = Input_Unit )

    RETURN
  
END SUBROUTINE CSV_File_Line_Count

!**************************************************************************************

SUBROUTINE CSV_File_Record_Write( CSV_File_Unit, RECORD )

!*****************************************************************************
!
!  CSV_File_Record_Write writes a RECORD to a CSV File.
!
!  PARAMETERs:
!
!    Input, INTEGER CSV_File_Unit, the Unit number
!
!    Input, CHARACTER(LEN=*) RECORD, the RECORD.
!
    IMPLICIT NONE

    INTEGER CSV_File_Unit
    CHARACTER(LEN=*) RECORD

    WRITE( CSV_File_Unit, '(a)' ) TRIM( RECORD )

    RETURN
  
END SUBROUTINE CSV_File_Record_Write

!**************************************************************************************

SUBROUTINE CSV_File_Open_Read( CSV_File_Name, CSV_File_Unit )

!*****************************************************************************
!
!  CSV_File_Open_Read opens a CSV File for Reading.
!
!  PARAMETERs:
!
!    Input, CHARACTER(LEN=*) CSV_File_Name, the name of the File.
!
!    Output, INTEGER CSV_File_Unit, the Unit number
!
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)  :: CSV_File_Name
    INTEGER,          INTENT(OUT) :: CSV_File_Unit
    INTEGER                       :: CSV_File_Status

    CSV_File_Unit = GetNewIOUnit ( )

    OPEN( Unit=CSV_File_Unit, File=CSV_File_Name, Status='Old', IOStat=CSV_File_Status )

    IF ( CSV_File_Status /= 0 ) THEN
        CSV_File_Unit = - 1
        CALL Handle_ErrOpen('CSV_File_Open_Read', ModName, CSV_File_Name, ErrSevere)
    END IF

    RETURN
  
END SUBROUTINE CSV_File_Open_Read

!**************************************************************************************

SUBROUTINE CSV_File_Open_Write( CSV_File_Name, CSV_File_Unit )

!*****************************************************************************
!
!  CSV_File_Open_Write opens a CSV File for writing.
!
!  PARAMETERs:
!
!    Input, CHARACTER(LEN=*) CSV_File_Name, the name of the File.
!
!    Output, INTEGER CSV_File_Unit, the Unit number
!
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)  :: CSV_File_Name
    INTEGER,          INTENT(OUT) :: CSV_File_Unit
    INTEGER                       :: CSV_File_Status

    CSV_File_Unit = GetNewIOUnit ( )

    OPEN( Unit = CSV_File_Unit, File = CSV_File_Name, Status = 'Replace', &
         IOStat = CSV_File_Status )

    IF ( CSV_File_Status /= 0 ) THEN
        CSV_File_Unit = - 1
        CALL Handle_ErrOpen('CSV_File_Open_Write', ModName, CSV_File_Name, ErrSevere)
    END IF

    RETURN
  
END SUBROUTINE CSV_File_Open_Write

!**************************************************************************************

SUBROUTINE CSV_Record_Append_I4 ( I4, RECORD )

!*****************************************************************************
!
!  CSV_Record_Append_I4 appends an I4 to a CSV RECORD.
!
!  PARAMETERs:
!
!    Input, INTEGER I4, the integer to be appended
!
!    Input/output, CHARACTER(LEN=*) RECORD, the CSV RECORD.
!
    IMPLICIT NONE

    CHARACTER(LEN=5) FMat
    INTEGER I
    INTEGER I4
    INTEGER I4_Len
    CHARACTER(LEN=*) RECORD
    !
    !  Locate last used location in RECORD.
    !
    I = Len_TRIM( RECORD )
    !
    !  Append comma.
    !
    IF ( 0 < I ) THEN
        I = I + 1
        RECORD(I:I) = ','
    END IF
    !
    !  Determine "width" of I4.
    !
    I4_Len = I4_Width ( I4 )
    !
    !  Create format for I4.
    !
    WRITE( FMat, '(a,i2,a)' ) '(i', I4_Len, ')'
    !
    !  Write I4 to RECORD.
    !
    WRITE( RECORD(I+1:I+I4_Len), FMat ) I4

    RETURN
  
END SUBROUTINE CSV_Record_Append_I4

!**************************************************************************************

SUBROUTINE CSV_Record_Append_R4 ( R4, RECORD )

!*****************************************************************************
!
!  CSV_Record_Append_R4 appends an R4 to a CSV RECORD.
!
!  PARAMETERs:
!
!    Input, REAL(SP) R4, the value to be appended
!
!    Input/output, CHARACTER(LEN=*) RECORD, the CSV RECORD.
!
    IMPLICIT NONE

    CHARACTER(LEN=5) FMat
    INTEGER I
    INTEGER I4
    INTEGER I4_Len
    REAL(SP) R4
    CHARACTER(LEN=*) RECORD
    !
    !  Locate last used location in RECORD.
    !
    I = Len_TRIM( RECORD )
    !
    !  Append comma.
    !
    IF ( 0 < I ) THEN
        I = I + 1
        RECORD(I:I) = ','
    END IF

    IF ( R4 == 0.0E+00 ) THEN
        I = I + 1
        RECORD(I:I) = '0'
    ELSE IF ( R4 == REAL( INT( R4 ), KIND = 4 ) ) THEN
        I4 = INT( R4 )
        I4_Len = I4_Width ( I4 )
        WRITE( FMat, '(a,i2,a)' ) '(i', I4_Len, ')'
        WRITE( RECORD(I+1:I+I4_Len), FMat ) I4
    ELSE
        WRITE( RECORD(I+1:I+14), '(g14.6)' ) R4
    END IF

    RETURN
  
END SUBROUTINE CSV_Record_Append_R4

!**************************************************************************************

SUBROUTINE CSV_Record_Append_R8 ( R8, RECORD )

!*****************************************************************************
!
!  CSV_Record_Append_R8 appends an R8 to a CSV RECORD.
!
!  PARAMETERs:
!
!    Input, REAL(DP) R8, the value to be appended
!
!    Input/output, CHARACTER(LEN=*) RECORD, the CSV RECORD.
!
    IMPLICIT NONE

    CHARACTER(LEN=5) FMat
    INTEGER I
    INTEGER I4
    INTEGER I4_Len
    REAL(DP) R8
    CHARACTER(LEN=*) RECORD
    !
    !  Locate last used location in RECORD.
    !
    I = Len_TRIM( RECORD )
    !
    !  Append comma.
    !
    IF ( 0 < I ) THEN
        I = I + 1
        RECORD(I:I) = ','
    END IF

    IF ( R8 == 0.0D+00 ) THEN
        I = I + 1
        RECORD(I:I+1) = ' 0'
    ELSE IF ( R8 == REAL( INT( R8 ), KIND = 8 ) ) THEN
        I4 = INT( R8 )
        I4_Len = I4_Width ( I4 ) + 1
        WRITE( FMat, '(a,i2,a)' ) '(i', I4_Len, ')'
        WRITE( RECORD(I+1:I+I4_Len), FMat ) I4
    ELSE
        WRITE( RECORD(I+1:I+20), '(g20.13)' ) R8
    END IF

    RETURN
  
END SUBROUTINE CSV_Record_Append_R8

!**************************************************************************************

SUBROUTINE CSV_Record_Append_S ( S, RECORD )

!*****************************************************************************
!
!  CSV_Record_Append_S appends a string to a CSV RECORD.
!
!  PARAMETERs:
!
!    Input, CHARACTER(LEN=*) S, the string to be appended
!
!    Input/output, CHARACTER(LEN=*) RECORD, the CSV RECORD.
!
    IMPLICIT NONE

    INTEGER I
    CHARACTER(LEN=*) RECORD
    CHARACTER(LEN=*) S
    INTEGER S_Len
    !
    !  Locate last used location in RECORD.
    !
    I = Len_TRIM( RECORD )
    !
    !  Append a comma.
    !
    IF ( 0 < I ) THEN
        I = I + 1
        RECORD(I:I) = ','
    END IF
    !
    !  Prepend a quote.
    !
    I = I + 1
    RECORD(I:I) = '"'
    !
    !  Write S to RECORD.
    !
    S_Len = Len_TRIM( S )
    RECORD(I+1:I+S_Len) = S(1:S_Len)
    I = I + S_Len
    !
    !  Postpend a quote
    !
    I = I + 1
    RECORD(I:I) = '"'

    RETURN
  
END SUBROUTINE CSV_Record_Append_S

!**************************************************************************************

INTEGER FUNCTION I4_Log_10 ( I )

!*****************************************************************************
!
!  I4_Log_10 RETURNs the integer part of the logarithm base 10 of an I4.
!
!  Discussion:
!
!    I4_Log_10 ( I ) + 1 is the number of decimal digits in I.
!
!    An I4 is an INTEGER value.
!
!  Example:
!
!        I  I4_Log_10
!    -----  --------
!        0    0
!        1    0
!        2    0
!        9    0
!       10    1
!       11    1
!       99    1
!      100    2
!      101    2
!      999    2
!     1000    3
!     1001    3
!     9999    3
!    10000    4
!
!  PARAMETERs:
!
!    Input, INTEGER I, the number whose logarithm base 10
!    is desired.
!
!    Output, INTEGER I4_Log_10, the integer part of the
!    logarithm base 10 of the absolute value of X.
!
    IMPLICIT NONE

    INTEGER I
    INTEGER I_Abs
    INTEGER Ten_Pow

    IF ( I == 0 ) THEN

        I4_Log_10 = 0

    ELSE

        I4_Log_10 = 0
        Ten_Pow = 10

        I_Abs = ABS( I )

        DO WHILE ( Ten_Pow <= I_Abs )
            I4_Log_10 = I4_Log_10 + 1
            Ten_Pow = Ten_Pow * 10
        END DO

    END IF

    RETURN
  
END FUNCTION I4_Log_10

!**************************************************************************************

INTEGER FUNCTION I4_Width ( I )

!*****************************************************************************
!
!  I4_Width RETURNs the "width" of an I4.
!
!  Discussion:
!
!    The width of an integer is the number of CHARACTERs necessary to print it.
!
!    The width of an integer can be useful when setting the appropriate output
!    format for a vector or array of values.
!
!    An I4 is an INTEGER value.
!
!  Example:
!
!        I  I4_Width
!    -----  -------
!    -1234    5
!     -123    4
!      -12    3
!       -1    2
!        0    1
!        1    1
!       12    2
!      123    3
!     1234    4
!    12345    5
!
!  PARAMETERs:
!
!    Input, INTEGER I, the number whose width is desired.
!
!    Output, INTEGER I4_Width, the number of CHARACTERs
!    necessary to represent the integer in base 10, including a negative
!    sign if necessary.
!
    IMPLICIT NONE

    INTEGER I

    IF ( 0 < I ) THEN
        I4_Width = I4_Log_10 ( I ) + 1
    ELSE IF ( I == 0 ) THEN
        I4_Width = 1
    ELSE IF ( I < 0 ) THEN
        I4_Width = I4_Log_10 ( I ) + 2
    END IF

    RETURN
  
END FUNCTION I4_Width

!**************************************************************************************

END MODULE ModLib_CSV_Files

!******************************************************************************
