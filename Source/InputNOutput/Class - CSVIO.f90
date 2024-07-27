
MODULE Class_CSVIO

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *CSVIO* derived type and its related routines.
!   The *CSVIO* type is an IO (input/output) type that can be used to handle
!   reading from and writing to CSV (comma-separated values) files. <br>
!   The *CSVIO* type uses Fortran convention and a CSV format mostly following
!   that specified by the RFC 4180.  This means that the *CSVIO* type reads
!   and write a line as a record and, by default, it uses the *comma* and
!   *double quote* as separator (delimiter) and quote characters, respectively.
!   However, these two characters can be changed when initializing the *CSVIO*
!   object. <br>
!   <br>
!  **Usage Overview**: <br>
!   The *CSVIO* type can be used to write a CSV file by first adding field values
!   to the *CSVIO* object.  The *CSVIO* type provides the *AddField* and *AddFields*
!   to add the field values.  The *AddField* method add a single field value to the
!   *CSVIO* object whereas the *AddFields* method can add a row or a table of field
!   values to the *CSVIO* object.  After adding all field values to the *CSVIO* object,
!   a user can then use the *WriteOutput* method to write those field values to a file. <br>
!   Similarly but in a somewhat reverse order, the *CSVIO* type can be used to read
!   a CSV file by first calling the *ReadCSVInput* method.  The *ReadCSVInput* method
!   not only reads all data from the file, but also tokenizes each line of data into
!   tokens (i.e. field values) and then stores those tokens in the *CSVIO* object.
!   A user can then use one of the *GetField* methods to retrieve field values.  The
!   *CSVIO* type provides four *GetField* methods: the *GetField*, the *GetRowFields*, 
!   the *GetColFields* and the *GetTableFields* methods. <br>
!   Additionally, the *CSVIO* type provides two additional methods.  The *Initialize*
!   method can be used to initialize the *CSVIO* object to its initial state while the
!   the *Reset* method can be used to reset the *CSVIO* object to its initial state.
!   The only difference between these two methods is that the *Initialize* method can
!   be used to change separator and/or quote characters from their default/current
!   values whereas the *Reset* method cannot. <br>
!   Both methods are NOT necessary if a single *read* or *write* operation is to be
!   performed where the default separator and quote characters are used.  If different
!   character(s) is/are needed, the *Initialize* method must then be called first.  If
!   multiple *read* and/or *write* operations are needed, a user should call either the
!   *Initialize* method or the *Reset* method to start another operation. <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers
    USE ModBase_CharUtil
    USE ModBase_ChrStr
    USE ModBase_IO_Handlers
    USE ModBase_PolyMemHandlers,    ONLY: PolyAlloc => MemAlloc
    USE ModBase_Memory_Handlers
    USE ModBase_DoublyLinkedLists,  ONLY: QueueAssignable => ListAssignable
    USE Class_Assignable
    USE Class_FvlStr

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: CSVIO
    ! abstract interfaces
    PUBLIC :: Val2Str
    PUBLIC :: Str2Val

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_CSVIO'

!** DERIVED TYPE DEFINITIONS
    !> The *CSVIO* type is an IO (input/output) type that can be used to handle
    !  reading from and writing to CSV (comma-separated values) files.
    TYPE CSVIO
        PRIVATE
        !% a character used to enclose a string field with a quote
        tChar                           :: Quote     = CHR_DOUBLEQUOTE
        !% a character used as a separator
        tChar                           :: Separator = CHR_COMMA
        !% total number of rows in the file
        tIndex                          :: TotRow    = 0_kIndex
        !% total number of columns in the file
        tIndex                          :: TotCol    = 0_kIndex
        !% current column
        tIndex                          :: iCol      = 0_kIndex
        !% current io unit number
        tInteger                        :: IOUnit    = 0
        !% flag indicating whether to add blank fields
        tLogical                        :: AddBlank  = FalseVal
        !% current record
        TYPE(FvlStr)                    :: CurrRecord
        !% field values used for reading; allocated to the *FvlStr* type
        CLASS(Assignable), ALLOCATABLE  :: FieldVal(:,:)
        !% queue of records
        TYPE(QueueAssignable)           :: RecordQueue
        !------------------------------------------------------------------------------
        !> *NOTE*: The following two components can actually be local variables but
        !  this will cause an error when the multiple-file optimization is used. <br>
        !------------------------------------------------------------------------------
        !% working record used for reading
        TYPE(FvlStr)                    :: WrkRecord
        !% working row fields used when reading
        TYPE(FvlStr),      ALLOCATABLE  :: WrkFieldVal(:)
    CONTAINS
        !----------------------------------------------------------------------
        !                           Private Procedures
        !----------------------------------------------------------------------
        ! opening and closing file procedures
        PROCEDURE, PRIVATE  :: OpenCSVOutput
        PROCEDURE, PRIVATE  :: OpenCSVInput
        ! adding and writing fields/records procedures
        PROCEDURE, PRIVATE  :: WriteRecord
        PROCEDURE, PRIVATE  :: AddFieldValues1D
        PROCEDURE, PRIVATE  :: AddFieldValues2D
        !----------------------------------------------------------------------
        !                           Public Procedures
        !----------------------------------------------------------------------
        !----------------------------------------
        ! -----     common procedures       -----
        !----------------------------------------
        !> **Subroutine Interface**: Initialize <br>
        !  **Purpose**:  To initialize the CSVIO object.  Optionally, a user can specify
        !       a quote character and/or a separator character to be used in place of 
        !       the default characters. <br>
        !  **Usage**: <br>
        !   ! use default quote (") and separator (,) characters <br>
        !   --->    CALL CSVObj%Initialize() <br>
        !   ! specify different quote (') character <br>
        !   --->    CALL CSVObj%Initialize(Quote="'") <br>
        !   ! specify different separator (;) character <br>
        !   --->    CALL CSVObj%Initialize(Separator=';') <br>
        !   ! specify different quote (') and separator (|) characters <br>
        !   --->    CALL CSVObj%Initialize(Quote="'", Separator='|') <br>
        PROCEDURE   :: Initialize       => InitializeCSVFile
        !> **Subroutine Interface**: Reset <br>
        !  **Purpose**:  To reset the CSVIO object to its initial state where all stored
        !       values/records are deallocated and/or destroyed. <br>
        !  **Usage**: <br>
        !   --->    CALL CSVObj%Reset() <br>
        PROCEDURE   :: Reset            => ResetCSVFile
        !-------------------------------------------------
        ! -----     writing-related procedures       -----
        !-------------------------------------------------
        !> **Subroutine Interface**: AddField <br>
        !  **Purpose**:  To add a field value to the current record.  Currently, the method
        !       internally supports only Fortran intrinsic types and the *FvlStr*  type as
        !       a type of the field value.  However, a user can specify a procedure to convert
        !       the field value to a Fortran character string if the type of the field is
        !       different from those supported. <br>
        !  **Usage**: <br>
        !   ! type of FieldVal is supported internally <br>
        !   --->    CALL CSVObj%AddField(FieldVal) <br>
        !   ! type of FieldVal is NOT supported internally <br>
        !   --->    CALL CSVObj%AddField(FieldVal, ToStrProc) <br>
        !  **Note**: After calling the *AddField* method multiple times, a user may want to add a
        !       field value to a new record (i.e. a record next to the current one).  In this
        !       case, the user must call the *AddRecord* method explicitly before calling the
        !       *AddField* method to add the field value to the next record. <br>
        PROCEDURE   :: AddField         => AddFieldValue
        !> **Subroutine Interface**: AddRecord <br>
        !  **Purpose**:  To add the current record to the record queue and update components of
        !       the CSVIO object as necessary.  See note for the *AddField* method. <br>
        !  **Usage**: <br>
        !   --->    CALL CSVObj%AddRecord() <br>
        PROCEDURE   :: AddRecord
        !> **Subroutine Interface**: AddFields <br>
        !  **Purpose**:  To add a row or a table of field values to the record queue.  If the type 
        !       of the specified field values is not one of Fortran intrinsic types or the *FvlStr*
        !       type, a user must specify the *ToStr* procedure argument in order to convert the
        !       field values to Fortran character strings. <br>
        !  **Usage**: <br>
        !   ! type of FieldVals is supported internally <br>
        !   --->    CALL CSVObj%AddFields(FieldVals) <br>
        !   ! type of FieldVals is NOT supported internally <br>
        !   --->    CALL CSVObj%AddFields(FieldVals, ToStrProc) <br>
        !  **Note**: Unlike the *AddField* method, the *AddFields* method calls the *AddRecord*
        !       method internally so the user does not need to. <br>
        GENERIC     :: AddFields        => AddFieldValues1D, &
                                           AddFieldValues2D
        !> **Subroutine Interface**: WriteOutput <br>
        !  **Purpose**:  To open the output file and write all records stored in the CSVIO
        !       object to it. <br>
        !  **Usage**: <br>
        !   ! append records to an existing file <br>
        !   --->    CALL CSVObj%WriteOutput(FileName) <br>
        !   ! write records to a new file <br>
        !   --->    CALL CSVObj%WriteOutput(FileName, NewFile=.TRUE.) <br>
        !  **Note**: This method should only be called after the *AddField* and/or *AddFields*
        !       method(s) has/have already been called. <br>
        PROCEDURE   :: WriteOutput      => WriteCSVOutput
        !-------------------------------------------------
        ! -----     reading-related procedures       -----
        !-------------------------------------------------
        !> **Subroutine Interface**: ReadCSVInput <br>
        !  **Purpose**:  To open the input file and read records from it.  The routine also
        !       perform a tokenization of the records and store the tokens in its field value
        !       component, which will be readily available to be retrieved by a user. <br>
        !  **Usage**: <br>
        !   --->    CALL CSVObj%ReadCSVInput(FileName) <br>
        !  **Note**: This method should be called before one of the *GetField* methods is called. <br>
        PROCEDURE   :: ReadInput        => ReadCSVInput
        !> **Function Interface**: GetField <br>
        !  **Purpose**:  To retrieve a field value from the stored field values according to the
        !       specified row and column and return a logical flag indicating whether there is an
        !       error occurred when converting from a stored field value to the user-specified
        !       field value. <br>
        !       Currently, the routine internally supports only Fortran intrinsic types and the
        !       *FvlStr* type as a valid type of the user-specified field value.  However, a user
        !       can supply a user-written procedure to convert the field value to a Fortran character
        !       string if the type of the field is different from those supported. <br>
        !  **Usage**: <br>
        !   ! type of FieldVal is supported internally <br>
        !   --->    ErrFlag = CSVObj%GetField(iRow, jCol, FieldVal) <br>
        !   --->    ErrFlag = CSVObj%GetField(iRow, jCol, FieldVal, ErrMsg) <br>
        !   ! type of FieldVal is NOT supported internally <br>
        !   --->    ErrFlag = CSVObj%GetField(iRow, jCol, FieldVal, ToVal=ToValProc) <br>
        !   --->    ErrFlag = CSVObj%GetField(iRow, jCol, FieldVal, ErrMsg, ToValProc) <br>
        !  **Note**: If the type of the user-specified field value is either the Fortran *CHARACTER*
        !       type or the *FvlStr* type, an error will never occur. <br>
        PROCEDURE   :: GetField         => GetFieldValue
        !> **Function Interface**: GetRowFields <br>
        !  **Purpose**:  To retrieve field values of the specified row from the stored field values
        !       and return a logical flag indicating whether there is an error occurred when converting
        !       from the stored field values to the user-specified field values. <br>
        !       In order to use this method, types of the stored field values in the specified row
        !       must be the same and the *Mold* argument is used to provide the concrete type of the
        !       *FieldVal* argument (which represents the user-specified field values) when it is
        !       allocated. <br>
        !       If the concrete type of the user-specified field values is not one of Fortran intrinsic
        !       types or the *FvlStr* type, a user must specify the *ToVal* procedure argument in order
        !       to convert Fortran character strings to the user-specified field values. <br>
        !  **Usage**: <br>
        !   ! type of FieldVal is supported internally <br>
        !   --->    ErrFlag = CSVObj%GetRowFields(iRow, FieldVals, Mold) <br>
        !   --->    ErrFlag = CSVObj%GetRowFields(iRow, FieldVals, Mold, ErrMsg) <br>
        !   ! type of FieldVal is NOT supported internally <br>
        !   --->    ErrFlag = CSVObj%GetRowFields(iRow, FieldVals, Mold, ToVal=ToValProc) <br>
        !   --->    ErrFlag = CSVObj%GetRowFields(iRow, FieldVals, Mold, ErrMsg, ToValProc) <br>
        !  **Note**: If the type of the user-specified field value is either the Fortran *CHARACTER*
        !       type or the *FvlStr* type, an error will never occur. <br>
        PROCEDURE   :: GetRowFields     => GetFieldValues1DRow
        !> **Function Interface**: GetColFields <br>
        !  **Purpose**:  To retrieve field values of the specified column from the stored field values
        !       and return a logical flag indicating whether there is an error occurred when converting
        !       from the stored field values to the user-specified field values. <br>
        !       In order to use this method, types of the stored field values in the specified column
        !       must be the same and the *Mold* argument is used to provide the concrete type of the
        !       *FieldVal* argument (which represents the user-specified field values) when it is
        !       allocated. <br>
        !       If the concrete type of the user-specified field values is not one of Fortran intrinsic
        !       types or the *FvlStr* type, a user must specify the *ToVal* procedure argument in order
        !       to convert Fortran character strings to the user-specified field values. <br>
        !  **Usage**: <br>
        !   ! type of FieldVal is supported internally <br>
        !   --->    ErrFlag = CSVObj%GetColFields(jCol, FieldVals, Mold) <br>
        !   --->    ErrFlag = CSVObj%GetColFields(jCol, FieldVals, Mold, SkipHeader=.TRUE.) <br>
        !   --->    ErrFlag = CSVObj%GetColFields(jCol, FieldVals, Mold, ErrMsg=ErrMsg) <br>
        !   --->    ErrFlag = CSVObj%GetColFields(jCol, FieldVals, Mold, .TRUE., ErrMsg) <br>
        !   ! type of FieldVal is NOT supported internally <br>
        !   --->    ErrFlag = CSVObj%GetColFields(jCol, FieldVals, Mold, ToVal=ToValProc) <br>
        !   --->    ErrFlag = CSVObj%GetColFields(jCol, FieldVals, Mold, .TRUE., ToVal=ToValProc) <br>
        !   --->    ErrFlag = CSVObj%GetColFields(jCol, FieldVals, Mold, ErrMsg=ErrMsg, ToVal=ToValProc) <br>
        !   --->    ErrFlag = CSVObj%GetColFields(jCol, FieldVals, Mold, .TRUE., ErrMsg, ToValProc) <br>
        !  **Note**: If the type of the user-specified field value is either the Fortran *CHARACTER*
        !       type or the *FvlStr* type, an error will never occur. <br>
        PROCEDURE   :: GetColFields     => GetFieldValues1DColumn
        !> **Function Interface**: GetTableFields <br>
        !  **Purpose**:  To retrieve all field values from the stored field values and return a logical
        !       flag indicating whether there is an error occurred when converting from the stored field
        !       values to the user-specified field values. <br>
        !       In order to use this routine, types of all stored field values (maybe with the exception
        !       of the first row) must be the same and the *Mold* argument is used to provide the concrete
        !       type of the *FieldVal* argument (which represents the user-specified field values) when
        !       it is allocated. <br>
        !       If the concrete type of the user-specified field values is not one of Fortran intrinsic
        !       types or the *FvlStr* type, a user must specify the *ToVal* procedure argument in order
        !       to convert Fortran character strings to the user-specified field values. <br>
        !       If types of the stored field values in the first row are different from types of field 
        !       values in other rows, the *SkipHeader* argument should be present and is set to true.
        !       In this case, a user may separately retrieve the header (i.e. the stored field values
        !       in the first row) using the *GetRowFields* method. <br>
        !  **Usage**: <br>
        !   ! type of FieldVal is supported internally <br>
        !   --->    ErrFlag = CSVObj%GetTableFields(FieldVals, Mold) <br>
        !   --->    ErrFlag = CSVObj%GetTableFields(FieldVals, Mold, SkipHeader=.TRUE.) <br>
        !   --->    ErrFlag = CSVObj%GetTableFields(FieldVals, Mold, ErrMsg=ErrMsg) <br>
        !   --->    ErrFlag = CSVObj%GetTableFields(FieldVals, Mold, .TRUE., ErrMsg) <br>
        !   ! type of FieldVal is NOT supported internally <br>
        !   --->    ErrFlag = CSVObj%GetTableFields(FieldVals, Mold, ToVal=ToValProc) <br>
        !   --->    ErrFlag = CSVObj%GetTableFields(FieldVals, Mold, .TRUE., ToVal=ToValProc) <br>
        !   --->    ErrFlag = CSVObj%GetTableFields(FieldVals, Mold, ErrMsg=ErrMsg, ToVal=ToValProc) <br>
        !   --->    ErrFlag = CSVObj%GetTableFields(FieldVals, Mold, .TRUE., ErrMsg, ToValProc) <br>
        !  **Note**: If the type of the user-specified field value is either the Fortran *CHARACTER*
        !       type or the *FvlStr* type, an error will never occur. <br>
        PROCEDURE   :: GetTableFields   => GetFieldValues2D
        ! ---------------------------------------------------------------------
        ! -----                 Final Procedure                           -----
        ! ---------------------------------------------------------------------
        FINAL               :: FinalizeCSVFile
    END TYPE CSVIO

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !^ *Val2Str* is an interface for a procedure to convert a field value
        !  to a Fortran character string.
        SUBROUTINE Val2Str(FieldVal, FieldStr)
            IMPORT
            CLASS(*),   INTENT(IN)  :: FieldVal !! field value
            tCharAlloc, INTENT(OUT) :: FieldStr !! field string
        END SUBROUTINE
        !^ *Str2Val* is an interface for a procedure to convert a Fortran
        !  character string to a field value.
        FUNCTION Str2Val(FieldStr, FieldVal, ErrMsg) RESULT(ErrFlag)
            IMPORT
            tCharAlloc,           INTENT(IN)    :: FieldStr !! field string
            CLASS(*),             INTENT(OUT)   :: FieldVal !! field value
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if an error occurred
            tLogical                            :: ErrFlag  !! true if an error occurred
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE InitializeCSVFile(CSVObj, Quote, Separator)

!** PURPOSE OF THIS FUNCTION:
    !^ To initialize the CSVIO object.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO),    INTENT(INOUT)  :: CSVObj   !! CSVIO object
    tChar, OPTIONAL, INTENT(IN)     :: Quote
    !^ a character used to enclose a string for a string field; default is a double quotation mark (").
    tChar, OPTIONAL, INTENT(IN)     :: Separator
    !^ a character used to separate a field; default is a comma (,).

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (PRESENT(Quote))     CSVObj%Quote     = Quote
    IF (PRESENT(Separator)) CSVObj%Separator = Separator
    CALL CSVObj%Reset()

    RETURN
   
END SUBROUTINE InitializeCSVFile

!******************************************************************************

SUBROUTINE ResetCSVFile(CSVObj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the CSVIO object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CSVIO), INTENT(INOUT)  :: CSVObj   !! CSVIO object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! reset the object components
    CSVObj%TotRow = 0_kIndex
    CSVObj%TotCol = 0_kIndex
    CSVObj%iCol   = 0_kIndex
    CSVObj%IOUnit = 0
    CSVObj%AddBlank  = FalseVal
    
    ! free up memory
    CALL MemFree(CSVObj%FieldVal)
    CALL CSVObj%RecordQueue%Destruct()
    IF (ALLOCATED(CSVObj%WrkFieldVal)) DEALLOCATE(CSVObj%WrkFieldVal)
    
    RETURN

END SUBROUTINE ResetCSVFile

!******************************************************************************

SUBROUTINE OpenCSVOutput(CSVObj, Name, NewFile)

!** PURPOSE OF THIS FUNCTION:
    !^ To open a CSV file for writing. 

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO),       INTENT(INOUT)   :: CSVObj   !! CSVIO object
    tCharStar,          INTENT(IN)      :: Name     !! output file name
    tLogical, OPTIONAL, INTENT(IN)      :: NewFile
    !^ flag indicating whether to open a new file or not. <br>
    !  - True if requesting to open a specified file name as a new CSV file.
    !    If there is an existing file with the same name, that existing file
    !    will then be deleted upon opening the file. <br>
    !  - False if requesting to open a specified file name as an existing CSV file.
    !    When a new data field is added, it will be appended to the existing data
    !    starting with a new record (row). <br>
    !  Default value is false. <br>
     
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc  :: CSVFileName
    tLogical    :: Append
    tIndex      :: iPos

!** FLOW

    ! check if a CSV extension is specified with the file name
    iPos = INDEX(Name, '.CSV')
    IF (iPos /= 0_kIndex) THEN
        CSVFILEName = CropBlanks(Name)
    ELSE
        CSVFILEName = CropBlanks(Name) // '.CSV'
    END IF
    
    ! check optional input
    Append = TrueVal
    IF (PRESENT(NewFile)) Append = .NOT.NewFile
    
    ! open the output file
    CSVObj%IOUnit = OpenOutputFile(CSVFILEName, Append)

    RETURN
   
END SUBROUTINE OpenCSVOutput

!******************************************************************************

SUBROUTINE OpenCSVInput(CSVObj,Name)

!** PURPOSE OF THIS FUNCTION:
    !^ To open a CSV file for reading.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO), INTENT(INOUT) :: CSVObj   !! CSVIO object
    tCharStar,    INTENT(IN)    :: Name     !! input file name
     
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc  :: CSVFileName
    tIndex      :: iPos

!** FLOW

    ! check if a CSV extension is specified with the file name
    iPos = INDEX(Name,'.CSV')
    IF (iPos /= 0_kIndex) THEN
        CSVFILEName = CropBlanks(Name)
    ELSE
        CSVFILEName = CropBlanks(Name) // '.CSV'
    END IF
    
    ! open the output file
    CSVObj%IOUnit = OpenInputFile(Name)

    RETURN
   
END SUBROUTINE OpenCSVInput

!******************************************************************************

SUBROUTINE AddFieldValue(CSVObj, FieldVal, ToStr)

!** PURPOSE OF THIS FUNCTION:
    !^ To add a field value to the current record.  Currently, the routine internally supports
    !  only Fortran intrinsic types and the *FvlStr*  type as a type of the field value.
    !  However, a user can supply a user-written procedure to convert the field value to a
    !  Fortran character string if the type of the field is different from those supported.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO),   INTENT(INOUT)   :: CSVObj   !! CSVIO object
    CLASS(*),       INTENT(IN)      :: FieldVal !! field value
    PROCEDURE(Val2Str), OPTIONAL    :: ToStr    !! procedure to convert field value to string
     
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc  :: FieldString
    tCharAlloc  :: CurrRecord
    tIndex      :: nCount

!** FLOW

    ! convert field value to field string
    IF (PRESENT(ToStr)) THEN
        CALL ToStr(FieldVal, FieldString)
    ELSE
        SELECT TYPE (Value => FieldVal)
        TYPE IS (tCharStar)
            nCount = CountCharacters(Value, CSVObj%Quote)
            IF (nCount == 2_kIndex) THEN
                ! value already in quotes
                FieldString = Value
            ELSE
                ! value not yet in quotes
                FieldString = CSVObj%Quote // Value // CSVObj%Quote
            END IF
        TYPE IS (FvlStr)
            nCount = Value%CountCharacters(CSVObj%Quote)
            IF (nCount == 2_kIndex) THEN
                ! value already in quotes
                FieldString = Value
            ELSE
                ! value not yet in quotes
                FieldString = CSVObj%Quote // Value%ToCharString() // CSVObj%Quote
            END IF
        TYPE IS (tInteger)
            FieldString = Value
        TYPE IS (tLong)
            FieldString = Value
        TYPE IS (tByte)
            FieldString = Value
        TYPE IS (tShort)
            FieldString = Value
        TYPE IS (tSingle)
            FieldString = Value
        TYPE IS (tDouble)
            FieldString = Value
        TYPE IS (tQuad)
            FieldString = Value
        TYPE IS (tCmpxSingle)
            FieldString = Value
        TYPE IS (tCmpxDouble)
            FieldString = Value
        TYPE IS (tCmpxQuad)
            FieldString = Value
        TYPE IS (tLogical)
            FieldString = Value
        CLASS DEFAULT
            CALL Handle_ErrLevel('AddFieldValue', ModName, ErrWarning, &
                                 'Type of the specified field value is NOT supported.')
            RETURN
        END SELECT
    END IF
    
    ! add field string to current record
    IF (CSVObj%iCol == 0_kIndex) THEN
        CurrRecord = FieldString
    ELSE
        CurrRecord = CSVObj%CurrRecord%ToCharString() // CSVObj%Separator // FieldString
    END IF
    
    ! save current record and increment iCol
    CSVObj%CurrRecord = CurrRecord
    CSVObj%iCol = CSVObj%iCol + 1_kIndex

    RETURN
   
END SUBROUTINE AddFieldValue

!******************************************************************************

SUBROUTINE WriteRecord(CSVObj)

!** PURPOSE OF THIS FUNCTION:
    !^ To write the current record to the opened output file.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO), INTENT(INOUT)  :: CSVObj   !! CSVIO object
     
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! write record
    CALL CSVObj%CurrRecord%WriteOutput(CSVObj%IOUnit)
    
    RETURN
   
END SUBROUTINE WriteRecord

!******************************************************************************

SUBROUTINE AddRecord(CSVObj)

!** PURPOSE OF THIS FUNCTION:
    !^ To add the current record to the record queue and
    !  update CSVObj components as necessary.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO), INTENT(INOUT) :: CSVObj   !! CSVIO object
     
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! add record to the record queue
    CALL CSVObj%RecordQueue%EnQueue(CSVObj%CurrRecord)
    
    ! update CSVObj components
    CSVObj%TotRow = CSVObj%TotRow + 1_kIndex
    IF (CSVObj%iCol > CSVObj%TotCol) THEN
        CSVObj%TotCol = CSVObj%iCol
        IF (CSVObj%TotRow > 1_kIndex) CSVObj%AddBlank = TrueVal
    END IF
    CSVObj%iCol       = 0_kIndex
    CSVObj%CurrRecord = ''
    
    RETURN
   
END SUBROUTINE AddRecord

!******************************************************************************

SUBROUTINE AddFieldValues1D(CSVObj, FieldVal, ToStr)

!** PURPOSE OF THIS FUNCTION:
    !^ To add field values to the current record and then add the record to the record queue.
    !  If the type of field values is not one of Fortran intrinsic types or the *FvlStr* type,
    !  a user must specify the *ToStr* procedure argument in order to convert the field values
    !  to Fortran character strings.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO),   INTENT(INOUT)   :: CSVObj       !! CSVIO object
    CLASS(*),       INTENT(IN)      :: FieldVal(:)  !! field values
    PROCEDURE(Val2Str), OPTIONAL    :: ToStr        !! procedure to convert field value to string
     
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW

    ! add field values
    DO I = 1, SIZE(FieldVal)
        CALL CSVObj%AddField(FieldVal(I), ToStr)
    END DO
    
    ! add record
    CALL CSVObj%AddRecord()
    
    RETURN
   
END SUBROUTINE AddFieldValues1D

!******************************************************************************

SUBROUTINE AddFieldValues2D(CSVObj, FieldVal, ToStr)

!** PURPOSE OF THIS FUNCTION:
    !^ To add a table of field values to the record queue.  If the type of field values is not
    !  one of Fortran intrinsic types or the *FvlStr* type, a user must specify the *ToStr*
    !  procedure argument in order to convert the field values to Fortran character strings.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO),   INTENT(INOUT)   :: CSVObj           !! CSVIO object
    CLASS(*),       INTENT(IN)      :: FieldVal(:,:)    !! field values
    PROCEDURE(Val2Str), OPTIONAL    :: ToStr            !! procedure to convert field value to string

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW

    ! add a row of field values
    DO I = 1, SIZE(FieldVal,DIM=1)
        CALL CSVObj%AddFields(FieldVal(I,:), ToStr)
    END DO
    
    RETURN
   
END SUBROUTINE AddFieldValues2D

!******************************************************************************

SUBROUTINE WriteCSVOutput(CSVObj, FileName, NewFile)

!** PURPOSE OF THIS FUNCTION:
    !^ To open the output file and write all stored records to it. 

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO),       INTENT(INOUT)   :: CSVObj   !! CSVIO object
    tCharStar,          INTENT(IN)      :: FileName !! output file name
    tLogical, OPTIONAL, INTENT(IN)      :: NewFile
    !^ flag indicating whether to open a new file or not. <br>
    !  - True if requesting to open a specified file name as a new CSV file.
    !    If there is an existing file with the same name, that existing file
    !    will then be deleted upon opening the file. <br>
    !  - False if requesting to open a specified file name as an existing CSV file.
    !    When a new data field is added, it will be appended to the existing data
    !    starting with a new record (row). <br>
    !  Default value is false. <br>
     
!** SUBROUTINE PARAMETER DEFINITIONS:
    tLogical, PARAMETER :: Protect = TrueVal
    tLogical, PARAMETER :: ExclMrk = FalseVal

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: I, J
    tIndex      :: nCount
    tIndex      :: nBlank
    tLogical    :: Success

!** FLOW
    
    ! open the output file
    CALL CSVObj%OpenCSVOutput(FileName, NewFile)
    
    ! add the current record if it has not yet been added to the record queue
    IF (CSVObj%iCol > 0_kIndex)  CALL CSVObj%AddRecord()
    
    ! get records, modify (as necessary) and write them to the output file
    IF ((CSVObj%TotRow > 0).AND.(CSVObj%TotRow == CSVObj%RecordQueue%GetSize())) THEN
        DO I = 1, CSVObj%TotRow
            ! get a record from the record queue
            Success = CSVObj%RecordQueue%DeQueue(CSVObj%CurrRecord)
            IF (.NOT.Success) CYCLE
            ! check whether each row contains the same number of columns
            IF (CSVObj%AddBlank) THEN
                nCount = CSVObj%CurrRecord%CountCharacters(CSVObj%Separator, Protect, ExclMrk)
                nBlank = CSVObj%TotCol - (nCount + 1)
                IF (nBlank >= 1) THEN
                    ! add blank(s)
                    DO J = 1, nBlank
                        CALL CSVObj%AddField(CHR_SPACE)
                    END DO
                END IF
            END IF
            ! write record to the file
            CALL CSVObj%WriteRecord()
        END DO
    END IF

    ! close the currently opened file
    CALL CloseFile(CSVObj%IOUnit)
    
    RETURN

END SUBROUTINE WriteCSVOutput

!******************************************************************************

SUBROUTINE ReadCSVInput(CSVObj,FileName)

!** PURPOSE OF THIS FUNCTION:
    !^ To open the input file and read records from it.  The routine also tokenizes
    !  the records and store the tokens in its field value component, which will be
    !  readily available to be retrieved by a user.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO), INTENT(INOUT)  :: CSVObj   ! CSVIO object
    tCharStar,    INTENT(IN)     :: FileName ! file name
    
!** SUBROUTINE PARAMETER DEFINITIONS:
    tLogical, PARAMETER :: Protect = TrueVal
    tLogical, PARAMETER :: ExclMrk = FalseVal
    
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    TYPE(QueueAssignable)   :: RecordQueue
    tIndex                  :: I
    tIndex                  :: nCount, nCol
    tInteger                :: IOStat
    tLogical                :: Success

!** FLOW

    ! open the input file
    CALL CSVObj%OpenCSVInput(FileName)
    
    ! read records from the input file
    CSVObj%TotCol = 0
    DO
        ! read a record
        CALL CSVObj%WrkRecord%ReadInput(CSVObj%IOUnit, IOStat=IOStat)

        ! determine maximum number of columns
        nCount = CSVObj%WrkRecord%CountCharacters(CSVObj%Separator, Protect, ExclMrk)
        nCol = nCount + 1
        IF (nCol > CSVObj%TotCol) CSVObj%TotCol = nCol
        
        ! check if it is the end of file
        IF (IS_IOSTAT_END(IOStat)) THEN
            EXIT
        ELSEIF (IOStat /= 0) THEN
            CALL Handle_ErrLevel('ReadCSVInput', ModName, ErrSevere, &
                                 'Error occurred while reading input record.')
            RETURN
        END IF

        ! save the record in the record list
        CALL RecordQueue%EnQueue(CSVObj%WrkRecord)
    END DO

    ! close the currently opened file
    CALL CloseFile(CSVObj%IOUnit)
    
    ! allocate field values in CSVIO object
    CSVObj%TotRow = RecordQueue%GetSize()
    CALL MemAlloc(CSVObj%FieldVal, CSVObj%TotRow, CSVObj%TotCol, CSVObj%WrkRecord)
    
    ! tokenize records and store field values
    DO I = 1, CSVObj%TotRow
        ! get a record from the record queue
        IF (.NOT.RecordQueue%IsEmpty()) THEN
            Success = RecordQueue%DeQueue(CSVObj%WrkRecord)
            IF (.NOT.Success) CYCLE
            ! tokenize a record
            nCount = CSVObj%WrkRecord%Split(CSVObj%Separator, CSVObj%WrkFieldVal, Protect, ExclMrk)
            nCol = nCount + 1
            ! store record field values
            CSVObj%FieldVal(I,1:nCol) = CSVObj%WrkFieldVal
            IF (CSVObj%TotCol > nCol) THEN
                SELECT TYPE (FieldVal => CSVObj%FieldVal)
                TYPE IS (FvlStr)
                    FieldVal(I,nCol+1:CSVObj%TotCol) = ' '
                END SELECT
            END IF
        END IF
    END DO
    IF (ALLOCATED(CSVObj%WrkFieldVal)) DEALLOCATE(CSVObj%WrkFieldVal)

    RETURN
   
END SUBROUTINE ReadCSVInput

!******************************************************************************

FUNCTION GetFieldValue(CSVObj, iRow, jCol, FieldVal, ErrMsg, ToVal) RESULT(ErrFlag)

!** PURPOSE OF THIS FUNCTION:
    !^ To retrieve a field value from the stored field values according to the specified
    !  row and column and return a logical flag indicating whether there is an error
    !  occurred when converting from a stored field value to the user-specified field value. <br>
    !  Currently, the routine internally supports only Fortran intrinsic types and the
    !  *FvlStr* type as a valid type of the user-specified field value.  However, a user
    !  can supply a user-written procedure to convert the field value to a Fortran character
    !  string if the type of the field is different from those supported.  It should be noted
    !  that if the type of the user-specified field value is either the Fortran *CHARACTER*
    !  type or the *FvlStr* type, an error will never occur. <br>

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO), INTENT(INOUT)         :: CSVObj   !! CSVIO object
    tIndex,       INTENT(IN)            :: iRow     !! row of the desired field
    tIndex,       INTENT(IN)            :: jCol     !! column of the desired field
    CLASS(*),     INTENT(OUT)           :: FieldVal !! field value
    tCharAlloc,   INTENT(OUT), OPTIONAL :: ErrMsg   !! message if an error occurred
    PROCEDURE(Str2Val),        OPTIONAL :: ToVal    !! procedure to convert string to field value
    tLogical                            :: ErrFlag  !! true if an error occurred
     
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! check input validity
    IF (.NOT.ALLOCATED(CSVObj%FieldVal)) THEN
        CALL Handle_ErrLevel('GetFieldValue', ModName, ErrSevere, &
                'No field values have been stored yet.  Must call routine "ReadCSVInput" first.')
        RETURN
    ELSEIF ((iRow > CSVObj%TotRow).OR.(iRow < 1)) THEN
        CALL Handle_ErrLevel('GetFieldValue', ModName, ErrSevere, &
                'The specified row is not in the applicable range.')
        RETURN
    ELSEIF ((jCol > CSVObj%TotCol).OR.(jCol < 1)) THEN
        CALL Handle_ErrLevel('GetFieldValue', ModName, ErrSevere, &
                'The specified column is not in the applicable range.')
        RETURN
    END IF

    ! convert field value to field string
    SELECT TYPE (FieldVals => CSVObj%FieldVal)
    TYPE IS (FvlStr)
        IF (PRESENT(ToVal)) THEN
            BLOCK
                tCharAlloc  :: FieldString
                FieldString = FieldVals(iRow,jCol)%ToCharString()
                ErrFlag = ToVal(FieldString, FieldVal, ErrMsg)
            END BLOCK
        ELSE
            SELECT TYPE (Value => FieldVal)
            TYPE IS (tCharStar)
                BLOCK
                    tCharAlloc  :: FieldString
                    FieldString = FieldVals(iRow,jCol)%ToCharString()
                    Value = FieldString
                END BLOCK
                ErrFlag = FalseVal
                IF (PRESENT(ErrMsg)) ErrMsg = 'No error occurred.'
            TYPE IS (FvlStr)
                Value = FieldVals(iRow,jCol)
                ErrFlag = FalseVal
                IF (PRESENT(ErrMsg)) ErrMsg = 'No error occurred.'
            TYPE IS (tInteger)
                Value = FieldVals(iRow,jCol)%ParseInteger(ErrFlag, ErrMsg)
            TYPE IS (tLong)
                Value = FieldVals(iRow,jCol)%ParseLong(ErrFlag, ErrMsg)
            TYPE IS (tByte)
                Value = FieldVals(iRow,jCol)%ParseByte(ErrFlag, ErrMsg)
            TYPE IS (tShort)
                Value = FieldVals(iRow,jCol)%ParseShort(ErrFlag, ErrMsg)
            TYPE IS (tSingle)
                Value = FieldVals(iRow,jCol)%ParseRSingle(ErrFlag, ErrMsg)
            TYPE IS (tDouble)
                Value = FieldVals(iRow,jCol)%ParseRDouble(ErrFlag, ErrMsg)
            TYPE IS (tQuad)
                Value = FieldVals(iRow,jCol)%ParseRQuad(ErrFlag, ErrMsg)
            TYPE IS (tCmpxSingle)
                Value = FieldVals(iRow,jCol)%ParseCSingle(ErrFlag, ErrMsg)
            TYPE IS (tCmpxDouble)
                Value = FieldVals(iRow,jCol)%ParseCDouble(ErrFlag, ErrMsg)
            TYPE IS (tCmpxQuad)
                Value = FieldVals(iRow,jCol)%ParseCQuad(ErrFlag, ErrMsg)
            TYPE IS (tLogical)
                Value = FieldVals(iRow,jCol)%ParseLogical()
            CLASS DEFAULT
                ErrFlag = TrueVal
                IF (PRESENT(ErrMsg)) THEN
                    ErrMsg = 'Type of the specified field value is NOT supported.'
                ELSE
                    CALL Handle_ErrLevel('GetFieldValue', ModName, ErrWarning, &
                            'Type of the specified field value is NOT supported.')
                END IF
            END SELECT
        END IF
    END SELECT
    
    RETURN
   
END FUNCTION GetFieldValue

!******************************************************************************

FUNCTION GetFieldValues1DRow(CSVObj, iRow, FieldVal, Mold, ErrMsg, ToVal) RESULT(ErrFlag)

!** PURPOSE OF THIS FUNCTION:
    !^ To retrieve field values of the specified row from the stored field values and return
    !  a logical flag indicating whether there is an error occurred when converting from the
    !  stored field values to the user-specified field values. <br>
    !  In order to use this routine, types of the stored field values in the specified row
    !  must be the same and the *Mold* argument is used to provide the concrete type of the
    !  *FieldVal* argument (which represents the user-specified field values) when it is
    !  allocated. <br>
    !  If the concrete type of the user-specified field values is not one of Fortran intrinsic
    !  types or the *FvlStr* type, a user must specify the *ToVal* procedure argument in order
    !  to convert Fortran character strings to the user-specified field values.  It should be
    !  noted that if the type of the user-specified field values is either the Fortran *CHARACTER*
    !  type or the *FvlStr* type, an error will never occur. <br>

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO),          INTENT(INOUT)    :: CSVObj       !! CSVIO object
    tIndex,                INTENT(IN)       :: iRow         !! row of the desired fields
    CLASS(*), ALLOCATABLE, INTENT(OUT)      :: FieldVal(:)  !! field values
    CLASS(*),              INTENT(IN)       :: Mold         !! mold
    tCharAlloc, OPTIONAL,  INTENT(OUT)      :: ErrMsg       !! message if an error occurred
    PROCEDURE(Str2Val),    OPTIONAL         :: ToVal        !! procedure to convert string to field value
    tLogical                                :: ErrFlag      !! true if an error occurred
     
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: jCol
    tIndex      :: fSize(1)

!** FLOW
    
    ! check input
    IF (.NOT.ALLOCATED(CSVObj%FieldVal)) THEN
        CALL Handle_ErrLevel('GetFieldValues1DRow', ModName, ErrSevere, &
                'No field values have been stored yet.  Must call routine "ReadCSVInput" first.')
        RETURN
    ELSEIF ((iRow > CSVObj%TotRow).OR.(iRow < 1)) THEN
        CALL Handle_ErrLevel('GetFieldValues1DRow', ModName, ErrSevere, &
                'The specified row is not in the applicable range.')
        RETURN
    END IF

    ! allocate output
    fSize(1) = CSVObj%TotCol
    CALL PolyAlloc(FieldVal, Mold, fSize)
    
    ! get field values
    DO jCol = 1, CSVObj%TotCol
        ErrFlag = CSVObj%GetField(iRow, jCol, FieldVal(jCol), ErrMsg, ToVal)
        IF (ErrFlag) EXIT
    END DO
    
    RETURN
   
END FUNCTION GetFieldValues1DRow

!******************************************************************************

FUNCTION GetFieldValues1DColumn(CSVObj, jCol, FieldVal, Mold, SkipHeader, ErrMsg, ToVal) RESULT(ErrFlag)

!** PURPOSE OF THIS FUNCTION:
    !^ To retrieve field values of the specified column from the stored field values and return
    !  a logical flag indicating whether there is an error occurred when converting from the
    !  stored field values to the user-specified field values. <br>
    !  In order to use this routine, types of the stored field values in the specified column
    !  must be the same and the *Mold* argument is used to provide the concrete type of the
    !  *FieldVal* argument (which represents the user-specified field values) when it is
    !  allocated. <br>
    !  If the concrete type of the user-specified field values is not one of Fortran intrinsic
    !  types or the *FvlStr* type, a user must specify the *ToVal* procedure argument in order
    !  to convert Fortran character strings to the user-specified field values.  It should be
    !  noted that if the type of the user-specified field values is either the Fortran *CHARACTER*
    !  type or the *FvlStr* type, an error will never occur. <br>

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO),          INTENT(INOUT)    :: CSVObj       !! CSVIO object
    tIndex,                INTENT(IN)       :: jCol         !! column of the desired fields
    CLASS(*), ALLOCATABLE, INTENT(OUT)      :: FieldVal(:)  !! field values
    CLASS(*),              INTENT(IN)       :: Mold         !! mold
    tLogical,   OPTIONAL,  INTENT(IN)       :: SkipHeader
    !^ flag indicating whether the field in the first row is excluded. <br>
    !  - True if the field in the first row is excluded.  This indicates
    !    that there is a header where the type of its fields is different
    !    from fields in other rows. <br>
    !  - False if to return all field values in the specified column. <br>
    !  Default is false. <br>
    tCharAlloc, OPTIONAL,  INTENT(OUT)      :: ErrMsg       !! message if an error occurred
    PROCEDURE(Str2Val),    OPTIONAL         :: ToVal        !! procedure to convert string to field value
    tLogical                                :: ErrFlag      !! true if an error occurred

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: iRow, Row1st
    tLogical    :: NoHeader
    tIndex      :: fSize(1)

!** FLOW
    
    ! check input
    IF (.NOT.ALLOCATED(CSVObj%FieldVal)) THEN
        CALL Handle_ErrLevel('GetFieldValues1DColumn', ModName, ErrSevere, &
                'No field values have been stored yet.  Must call routine "ReadCSVInput" first.')
        RETURN
    ELSEIF ((jCol > CSVObj%TotCol).OR.(jCol < 1)) THEN
        CALL Handle_ErrLevel('GetFieldValues1DColumn', ModName, ErrSevere, &
                'The specified column is not in the applicable range.')
        RETURN
    END IF

    ! check optional input
    NoHeader = FalseVal
    IF (PRESENT(SkipHeader)) NoHeader = SkipHeader
    IF (NoHeader) THEN
        Row1st = 2
    ELSE
        Row1st = 1
    END IF

    ! allocate output
    fSize(1) = CSVObj%TotRow-Row1st+1_kIndex
    CALL PolyAlloc(FieldVal, Mold, fSize)
    
    ! get field values
    DO iRow = Row1st, CSVObj%TotRow
        ErrFlag = CSVObj%GetField(iRow, jCol, FieldVal(iRow), ErrMsg, ToVal)
        IF (ErrFlag) EXIT
    END DO
    
    RETURN
   
END FUNCTION GetFieldValues1DColumn

!******************************************************************************

FUNCTION GetFieldValues2D(CSVObj, FieldVal, Mold, SkipHeader, ErrMsg, ToVal) RESULT(ErrFlag)

!** PURPOSE OF THIS FUNCTION:
    !^ To retrieve all field values from the stored field values and return a logical
    !  flag indicating whether there is an error occurred when converting from the
    !  stored field values to the user-specified field values. <br>
    !  In order to use this routine, types of all stored field values (maybe with the
    !  exception of the first row) must be the same and the *Mold* argument is used to
    !  provide the concrete type of the *FieldVal* argument (which represents the
    !  user-specified field values) when it is allocated. <br>
    !  If the concrete type of the user-specified field values is not one of Fortran
    !  intrinsic types or the *FvlStr* type, a user must specify the *ToVal* procedure
    !  argument in order to convert Fortran character strings to the user-specified
    !  field values.  It should be noted that if the type of the user-specified field
    !  values is either the Fortran *CHARACTER* type or the *FvlStr* type, an error
    !  will never occur. <br>
    !  If types of the stored field values in the first row are different from types
    !  of field values in other rows, the *SkipHeader* argument should be present and
    !  is set to true.  In this case, a user may separately retrieve the header (i.e.
    !  the stored field values in the first row) using the *GetRowFields* method. <br>

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(CSVIO),          INTENT(INOUT)    :: CSVObj           !! CSVIO object
    CLASS(*), ALLOCATABLE, INTENT(OUT)      :: FieldVal(:,:)    !! field values
    CLASS(*),              INTENT(IN)       :: Mold             !! mold
    tLogical,   OPTIONAL,  INTENT(IN)       :: SkipHeader
    !^ flag indicating whether the fields in the first row are excluded. <br>
    !  - True if the fields in the first row are excluded.  This indicates
    !    that there is a header where the type of its fields is different
    !    from fields in other rows. <br>
    !  - False if to return all field values in the specified column. <br>
    !  Default is false. <br>
    tCharAlloc, OPTIONAL,  INTENT(OUT)      :: ErrMsg           !! message if an error occurred
    PROCEDURE(Str2Val),    OPTIONAL         :: ToVal            !! procedure to convert string to field value
    tLogical                                :: ErrFlag          !! true if an error occurred

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: iRow, jCol, Row1st
    tLogical    :: NoHeader
    tIndex      :: fSize(2)

!** FLOW
    
    ! check input
    IF (.NOT.ALLOCATED(CSVObj%FieldVal)) THEN
        CALL Handle_ErrLevel('GetFieldValues2D', ModName, ErrSevere, &
                'No field values have been stored yet.  Must call routine "ReadCSVInput" first.')
        RETURN
    END IF
    
    ! check optional input
    NoHeader = FalseVal
    IF (PRESENT(SkipHeader)) NoHeader = SkipHeader
    IF (NoHeader) THEN
        Row1st = 2
    ELSE
        Row1st = 1
    END IF

    ! allocate output
    fSize(1) = CSVObj%TotRow-Row1st+1_kIndex
    fSize(2) = CSVObj%TotCol
    CALL PolyAlloc(FieldVal, Mold, fSize)
    
    ! get field values
    DO iRow = Row1st, CSVObj%TotRow
        DO jCol = 1, CSVObj%TotCol
            ErrFlag = CSVObj%GetField(iRow, jCol, FieldVal(iRow,jCol), ErrMsg, ToVal)
            IF (ErrFlag) EXIT
        END DO
    END DO
    
    RETURN
   
END FUNCTION GetFieldValues2D

!--------------------------------------------------------------------------------------
!                           FINAL PROCEDURE
!--------------------------------------------------------------------------------------

SUBROUTINE FinalizeCSVFile(CSVObj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the CSVIO object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(CSVIO), INTENT(INOUT)   :: CSVObj   !! CSVIO object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! reset the object components
    CSVObj%Quote     = CHR_DOUBLEQUOTE
    CSVObj%Separator = CHR_COMMA
    CALL CSVObj%Reset()
    
    RETURN

END SUBROUTINE FinalizeCSVFile

!******************************************************************************

END MODULE Class_CSVIO

!******************************************************************************
