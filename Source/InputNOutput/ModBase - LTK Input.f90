
MODULE ModBase_LTKInput

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains input-processing routines to provide the capabilities of
!   reading the input data dictionary and input file and supplying other routines
!   with the data contained therein. <br>
!   The input syntax is designed to allow for future flexibility without necessitating
!   massive (or any) changes to this code.  Two files are used as key elements: <br>
!   (1) the input data dictionary will specify the sections and objects that will be
!   allowed in the actual  input file, and <br>
!   (2) the actual input data file will be processed with the data therein being
!   supplied to other routines. <br>
!   This module is a modified version of the *InputProcessor* module of the ASHRAE's
!   Loads Toolkit package [1], which is an earlier and lite version of a module with
!   the same name of the (Fortran) *EnergyPlus* simulation program [2, 3].  To use
!   and understand this module and its associated input files correctly, a user is
!   referred to the EnergyPlus Interface Developer Guide and Input-Output Reference [4],
!   which provide an overview of the EnergyPlus input processing, its general rules to
!   the two input files required and its specific rules to each particular input file. <br>
!   It should be noted that, unlike the EnergyPlus program, this module (based on an
!   earlier version) requires two input files (one with the extension *idd* providing
!   the data dictionary and other with the extension *idf* providing the actual data).
!   However, according to the Input-Output Reference [4], the current version of the
!   EnergyPlus program actually is compiled with a specific version of the *idd* file
!   (i.e. the idd file is rather embedded in the code) and; hence, it only requires
!   one input file (an *idf* file) to be processed. <br>
!    <br>
!   **REFERENCES**: <br>
!   [1] <a href="https://experts.illinois.edu/en/publications/ashrae-toolkit-for-building-load-calculations">
!       C.O. Pedersen, D.E. Fisher, R.J. Liesen, and R.K. Strand. 2003.  ASHRAE Toolkit for
!       Building Load Calculations.  ASHRAE Transaction, vol. 109, part 1, pp. 583-589.</a> <br>
!   [2] <a href="https://energyplus.net/">EnergyPlus: a whole building energy simulation program.</a> <br>
!   [3] <a href="https://sourceforge.net/projects/epx/files/EnergyPlus/">EnergyPlus 8.1
!       Source Code (Last version of Fortran Code).</a> <br>
!   [4] <a href="https://energyplus.net/documentation">EnergyPlus Documentation.</a> <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_MiscUtil
    USE ModBase_Error_Handlers
    USE ModBase_IO_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  ProcessInput
    PUBLIC  GetNumSectionsFound
    PUBLIC  GetNumSectionsinInput
    PUBLIC  GetListofSectionsinInput
    PUBLIC  FindIteminList
    PUBLIC  FindItem
    PUBLIC  GetNumObjectsFound
    PUBLIC  GetObjectItem
    PUBLIC  GetObjectItemNum
    PUBLIC  GetObjectItemfromFile

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar,   PARAMETER  :: ModName = 'Base_InputProcessor'
    ! constants for string manipulation
    tCharStar,   PARAMETER  :: PathChar  = '\'
    tInteger,    PARAMETER  :: PathLimit = 255
    tInteger,    PARAMETER  :: MaxInputLineLength = 2048    ! Maximum number of characters in an input line (used by "ModBase_LTKInput" module)
    tInteger,    PARAMETER  :: MaxNameLength      = 256     ! Maximum name length in characters -- should be the same as MaxAlphaArgLength in "ModBase_LTKInput" module
    ! constants for string manipulation
    tCharStar,   PARAMETER  :: UpperCase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    tCharStar,   PARAMETER  :: LowerCase = 'abcdefghijklmnopqrstuvwxyz'
    ! constant names for files
    tCharStar,   PARAMETER  :: ProgramPath = ' '                ! path for program
    ! Starting number of Objects allowed in IDD as well as the increment when max is reached
    tInteger,    PARAMETER  :: ObjectDefAllocInc = 100    
    ! Starting number of Sections allowed in IDD as well as the increment when max is reached
    tInteger,    PARAMETER  :: SectionDefAllocInc = 20
    ! Initial number of Sections allowed in IDF as well as the increment when max is reached
    tInteger,    PARAMETER  :: SectionsIDFAllocInc = 20
    ! Initial number of Objects allowed in IDF as well as the increment when max is reached
    tInteger,    PARAMETER  :: ObjectsIDFAllocInc = 500
    ! Maximum number of characters in an Object Name
    tInteger,    PARAMETER  :: MaxObjectNameLength = MaxNameLength
    ! Maximum number of characters in a Section Name
    tInteger,    PARAMETER  :: MaxSectionNameLength = MaxNameLength
    ! Maximum number of characters in an Alpha Argument
    tInteger,    PARAMETER  :: MaxAlphaArgLength = MaxNameLength
    tCharLen(1), PARAMETER  :: Blank = ' '

!** DERIVED TYPE DEFINITIONS
    TYPE ObjectsDefinition
        ! Name of the Object
        tCharLen(MaxObjectNameLength)   :: Name
        ! Number of parameters to be processed for each object
        tInteger                        :: NumParams
        ! Number of Alpha elements in the object
        tInteger                        :: NumAlpha
        ! Number of Numeric elements in the object
        tInteger                        :: NumNumeric
        ! Version number for this Definition
        tInteger                        :: VersionNum
        ! Positionally, whether the argument is alpha (true) or numeric (false)
        tLogical,            POINTER    :: AlphaorNumeric(:) => NULL()
        ! Used to range check and default numeric fields
        TYPE(RangeCheckDef), POINTER    :: NumRangeChks(:)   => NULL()
        ! Number of this object found in IDF
        tInteger                        :: NumFound
    END TYPE

    TYPE RangeCheckDef
        ! Name of the field
        tCharLen(MaxObjectNameLength)   :: FieldName
        ! which field number this is
        tInteger                        :: FieldNumber
        ! true when Min/Max has been added
        tLogical                        :: MinMaxChk
        ! appropriate Min/Max Strings
        tCharLen(20)                    :: MinMaxString(2)
        ! appropriate Min/Max Values
        tReal                           :: MinMaxValue(2)
        ! = 0 (none/invalid), = 1 \min, = 2 \min>, = 3 \max, = 4 \max<
        tInteger                        :: WhichMinMax(2)
        ! true when default has been entered
        tLogical                        :: DefaultChk
        ! Default value
        tReal                           :: Default
    END TYPE

    TYPE SectionsDefinition
       tCharLen(MaxSectionNameLength)   :: Name     ! Name of the Section
       tInteger                         :: NumFound ! Number of this object found in IDF
    END TYPE

    TYPE FileSectionsDefinition
       tCharLen(MaxSectionNameLength)   :: Name          ! Name of this section
       tInteger                         :: FirstRecord  ! Record number of first object in section
       tInteger                         :: LastRecord   ! Record number of last object in section
    END TYPE
    ! This derived type will be saved for each "object" input
    ! The arrays (Alphas, Numbers) will be dimensioned to be the size expected from the definition.
    TYPE LineDefinition
        ! Object name for this record
       tCharLen(MaxObjectNameLength)        :: Name
       ! Number of alphas on this record
       tInteger                             :: NumAlphas
       ! Number of numbers on this record
       tInteger                             :: NumNumbers
       ! Storage for the alphas
       tCharLen(MaxAlphaArgLength), POINTER :: Alphas(:)  => NULL()
       ! Storage for the numbers
       tReal,                       POINTER :: Numbers(:) => NULL()
    END TYPE

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! +++++ Integer Variables +++++
    ! Count of number of object definitions found in the IDD
    tInteger        :: NumObjectDefs            = 0
     ! Count of number of section defintions found in the IDD
    tInteger        :: NumSectionDefs           = 0
    ! Current "max" object defs (IDD), when reached will be reallocated and new Max set
    tInteger        :: MaxObjectDefs            = 0
    ! Current "max" section defs (IDD), when reached will be reallocated and new Max set
    tInteger        :: MaxSectionDefs           = 0
    ! Unit number for reading IDD file
    tInteger        :: IDDFile                  = 0
    ! Unit number for reading IDF file
    tInteger        :: IDFFile                  = 0
    ! Count of number of lines in IDF
    tInteger        :: NumLines                 = 0
    ! Current "max" IDF records (lines), when reached will be reallocated and new Max set
    tInteger        :: MaxIDFRecords            = 0
    ! Count of number of IDF records
    tInteger        :: NumIDFRecords            = 0
    ! Current "max" IDF sections (lines), when reached will be reallocated and new Max set
    tInteger        :: MaxIDFSections           = 0
    ! Count of number of IDF records
    tInteger        :: NumIDFSections           = 0
    ! Unit number of the file echoing the IDD and input records
    tInteger        :: EchoInputFile            = 0
    ! Actual input line length or position of comment character
    tInteger        :: InputLineLength          = 0
    ! Count of max alpha args found in the IDD
    tInteger        :: MaxAlphaArgsFound        = 0
    ! Count of max numeric args found in the IDD
    tInteger        :: MaxNumericArgsFound      = 0
    ! Count of number of "out of range" errors found
    tInteger        :: NumOutOfRangeErrorsFound = 0
    ! +++++ Real Variables +++++
    !na
    ! +++++ Character Variables +++++
    ! name of the idd file
    tCharLen(80)                                :: IDDFileName
    ! name of the idf file
    tCharLen(80)                                :: IDFFileName
    ! name of the audit file
    tCharLen(80)                                :: LogFileName
    ! Valid indicators for Alpha or Numeric fields (A or N)
    tCharLen(4)                                 :: AlphaNum
    ! Each line can be up to MaxInputLineLength characters long
    tCharLen(MaxInputLineLength)                :: InputLine
    ! Current Field Name (IDD)
    tCharLen(MaxObjectNameLength)               :: CurrentFieldName
    tCharLen(MaxSectionNameLength), ALLOCATABLE :: ListofSections(:)
    tCharLen(MaxObjectNameLength),  ALLOCATABLE :: ListofObjects(:)
    tCharAlloc                                  :: ErrMsg
    ! +++++ Logical Variables +++++
    ! If errors found during parse of IDF, will fatal at end
    tLogical :: OverallErrorFlag        = FalseVal
    ! Usually True, if the IDD is backspaced, then is set to false, then back to true
    tLogical :: EchoInputLine           = TrueVal
    ! Module level reporting logical, can be turned off from outside the module (and then
    ! must be turned back on).
    tLogical :: ReportRangeCheckErrors  = TrueVal
    ! Set to true when ReadInputLine has just scanned a "field"
    tLogical :: FieldSet                = FalseVal
    ! +++++ Derived Types Variables +++++
    ! Contains all the Valid Objects on the IDD
    TYPE(ObjectsDefinition),      ALLOCATABLE  :: ObjectDef(:)
    ! Contains all the Valid Sections on the IDD
    TYPE(SectionsDefinition),     ALLOCATABLE  :: SectionDef(:)
    ! lists the sections on file (IDF)
    TYPE(FileSectionsDefinition), ALLOCATABLE  :: SectionsonFile(:)
    ! Description of current record
    TYPE(LineDefinition)                       :: LineItem
    ! All the objects read from the IDF
    TYPE(LineDefinition),         ALLOCATABLE  :: IDFRecords(:)

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE ProcessInput(IDDName,IDFName,LOGName)

    ! PURPOSE OF THIS SUBROUTINE:
    !^ This subroutine processes the input files.  First, the
    !  input data dictionary is read and interpreted.  Using the structure
    !  from the data dictionary, the actual simulation input file is read. <br>
    !  This file is processed according to the "rules" in the data dictionary
    !  and stored in a local data structure which will be used during the simulation.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: IDDName  !! name of the input data dictionary (idd) file
    tCharStar, INTENT(IN)   :: IDFName  !! name of the actual input (idf) file
    tCharStar, INTENT(IN)   :: LOGName  !! name of the audit file

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(270)   :: FullName      ! full name of file to open, including path
    
    ! FLOW:
    
    ! check input validity
    IF (LEN_TRIM(IDDName) >= 80) THEN
        CALL Handle_ErrLevel('ProcessInput', ModName, ErrSevere, &
                   'Length of IDD file name cannot be greater than 80.')
    ELSE
        IDDFileName = IDDName
    END IF
    IF (LEN_TRIM(IDFName) >= 80) THEN
        CALL Handle_ErrLevel('ProcessInput', ModName, ErrSevere, &
                   'Length of IDF file name cannot be greater than 80.')
    ELSE
        IDFFileName = IDFName
    END IF
    IF (LEN_TRIM(LOGName) >= 80) THEN
        CALL Handle_ErrLevel('ProcessInput', ModName, ErrSevere, &
                   'Length of LOG file name cannot be greater than 80.')
    ELSE
        LOGFileName = LOGName
    END IF

    ! get unit number and open log file
    EchoInputFile = GetNewIOUnit()
    OPEN(UNIT=EchoInputFile, FILE=LogFileName)
    
    ! get unit number and open IDD file
    IDDFile = GetNewIOUnit()
    ! FullName is used to build file name with Path (from Base_Common module)
    IF (LEN_TRIM(ProgramPath) == 0) THEN
        FullName = IDDFileName
    ELSE
        FullName = ProgramPath(1:LEN_TRIM(ProgramPath)) // IDDFileName
    ENDIF
    OPEN(UNIT=IDDFile, FILE=FullName)
    NumLines = 0
    AlphaNum = 'ANan'

    WRITE(EchoInputFile,*) ' Processing Data Dictionary (' // TRIM(IDDFileName) // &
                           ') File -- Start'

    ! process IDD file
    CALL ProcessDataDicFile

    ALLOCATE(ListofSections(NumSectionDefs))
    ALLOCATE(ListofObjects(NumObjectDefs))
    ListofSections = SectionDef(1:NumSectionDefs)%Name
    ListofObjects = ObjectDef(1:NumObjectDefs)%Name

    CLOSE(UNIT=IDDFile)

    WRITE(EchoInputFile,*) ' Processing Data Dictionary (' // TRIM(IDDFileName) // &
                           ') File -- Complete'

    WRITE(EchoInputFile,*) ' Maximum number of Alpha Args = ', MaxAlphaArgsFound
    WRITE(EchoInputFile,*) ' Maximum number of Numeric Args = ', MaxNumericArgsFound
    WRITE(EchoInputFile,*) ' Number of Object Definitions = ', NumObjectDefs
    WRITE(EchoInputFile,*) ' Number of Section Definitions = ', NumSectionDefs

    WRITE(EchoInputFile,*) ' Processing Input Data File (' // TRIM(IDFFileName) // &
                           ') File -- Start'

    ! get unit number and open IDF file
    IDFFile = GetNewIOUnit()
    OPEN(UNIT=IDFFile, FILE=IDFFileName)
    NumLines = 0

    ! process IDF file
    CALL ProcessInputDataFile

    CALL ValidateSectionsInput

    WRITE(EchoInputFile,*) ' Processing Input Data File (' // TRIM(IDFFileName) // &
                           ') File -- Complete'
    WRITE(EchoInputFile,*) ' Number of IDF "Lines" = ', NumIDFRecords

    RETURN

END SUBROUTINE ProcessInput

!******************************************************************************

SUBROUTINE ProcessDataDicFile

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine processes data dictionary file.
    ! The structure of the sections and objects are stored in derived
    ! types (SectionDefs and ObjectDefs)

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical                                :: EndofFile = FalseVal  ! True when End of File has been reached (IDD or IDF)
    tInteger                                :: Pos                  ! Test of scanning position on the current input line
    tLogical                                :: BlankLine            ! True if it is a blank line
    TYPE(SectionsDefinition), ALLOCATABLE   :: TempSectionDef(:)    ! Like SectionDef, used during Re-allocation
    TYPE(ObjectsDefinition),  ALLOCATABLE   :: TempObjectDef(:)     ! Like ObjectDef, used during Re-allocation
    
    ! FLOW:
    
    MaxSectionDefs = SectionDefAllocInc
    MaxObjectDefs = ObjectDefAllocInc

    ALLOCATE(SectionDef(MaxSectionDefs))
    SectionDef%Name = ' '       ! Name of the section
    SectionDef%NumFound = 0     ! Number of this section found in IDF

    ALLOCATE(ObjectDef(MaxObjectDefs))
    ObjectDef%Name = ' '         ! Name of the object
    ObjectDef%NumParams = 0      ! Number of parameters to be processed for each object
    ObjectDef%NumAlpha = 0       ! Number of Alpha elements in the object
    ObjectDef%NumNumeric = 0     ! Number of Numeric elements in the object
    ObjectDef%VersionNum = 0     ! Version number for this Definition
    ObjectDef%NumFound = 0       ! Number of this object found in IDF

    NumObjectDefs = 0
    NumSectionDefs = 0

    DO WHILE (.NOT.EndofFile)
        
        ! read an input line
        CALL ReadInputLine(IDDFile, Pos, BlankLine, InputLineLength, EndofFile)
        IF (BlankLine .OR. EndofFile) CYCLE
        
        ! determine (first) position of comma or semicolon 
        Pos = SCAN(InputLine(1:InputLineLength), ',;')
        
        IF (Pos /= 0) THEN
            ! comma or semicolon found, check if it is semicolon
            IF (InputLine(Pos:Pos) == ';') THEN
                ! this is a section
                CALL AddSectionDef(InputLine(1:Pos-1))
                IF (NumSectionDefs == MaxSectionDefs) THEN
                    ALLOCATE(TempSectionDef(MaxSectionDefs+SectionDefAllocInc))
                    TempSectionDef%Name = ' '
                    TempSectionDef%NumFound = 0
                    TempSectionDef(1:MaxSectionDefs) = SectionDef
                    DEALLOCATE(SectionDef)
                    ALLOCATE(SectionDef(MaxSectionDefs+SectionDefAllocInc))
                    SectionDef = TempSectionDef
                    DEALLOCATE(TempSectionDef)
                    MaxSectionDefs = MaxSectionDefs + SectionDefAllocInc
                ENDIF
            ELSE
                ! this is an object
                CALL AddObjectDefandParse(InputLine(1:Pos-1), Pos, EndofFile)
                IF (NumObjectDefs == MaxObjectDefs) THEN
                    ALLOCATE(TempObjectDef(MaxObjectDefs+ObjectDefAllocInc))
                    TempObjectDef%Name = ' '         ! Name of the object
                    TempObjectDef%NumParams = 0      ! Number of parameters to be processed for each object
                    TempObjectDef%NumAlpha = 0       ! Number of Alpha elements in the object
                    TempObjectDef%NumNumeric = 0     ! Number of Numeric elements in the object
                    TempObjectDef%VersionNum = 0     ! Version number for this Definition
                    TempObjectDef%NumFound = 0       ! Number of this object found in input
                    TempObjectDef(1:MaxObjectDefs) = ObjectDef
                    DEALLOCATE(ObjectDef)
                    ALLOCATE(ObjectDef(MaxObjectDefs+ObjectDefAllocInc))
                    ObjectDef = TempObjectDef
                    DEALLOCATE(TempObjectDef)
                    MaxObjectDefs = MaxObjectDefs + ObjectDefAllocInc
                ENDIF
            ENDIF

        ELSE
            CALL DisplaySevereError(', or ; expected on this line', EchoInputFile)
        ENDIF

    END DO

    RETURN

END SUBROUTINE ProcessDataDicFile

!******************************************************************************

SUBROUTINE AddSectionDef(ProposedSection)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine adds a new section to SectionDefs.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: ProposedSection  ! Proposed Section to be added

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(MaxSectionNameLength)  :: SqueezedSection  ! Input Argument, Left-Justified and Uppercase
    tLogical                        :: ErrFlag          ! Local error flag.  When True, Proposed Section is not added to global list
    
    ! FLOW:
    
    SqueezedSection = MakeUpperCase(ADJUSTL(ProposedSection))
    IF (LEN_TRIM(ADJUSTL(ProposedSection)) > MaxSectionNameLength) THEN
        ErrMsg = 'Section length exceeds maximum, will be truncated = ' // &
                 TRIM(ProposedSection)
        CALL DisplayWarningError(ErrMsg, EchoInputFile)
        ErrMsg = 'Will be processed as Section = ' // TRIM(SqueezedSection)
        CALL DisplayMessage(ErrMsg, EchoInputFile)
    ENDIF
    ErrFlag = FalseVal

    IF (SqueezedSection /= Blank) THEN
        IF (FindItemInList(SqueezedSection,SectionDef%Name,NumSectionDefs) > 0) THEN
            ErrMsg = ' Already a Section called ' // TRIM(SqueezedSection) // &
                     '. This definition ignored.'
            CALL DisplayWarningError(ErrMsg, EchoInputFile)
            ! Error Condition
            ErrFlag = TrueVal
        ENDIF
    ELSE
        ErrMsg = 'Blank Sections not allowed.  Review "' // TRIM(LogFileName) // '" file.'
        CALL DisplaySevereError(ErrMsg, EchoInputFile)
        ErrFlag = TrueVal
    ENDIF

    IF (.NOT.ErrFlag) THEN
        NumSectionDefs = NumSectionDefs + 1
        SectionDef(NumSectionDefs)%Name = SqueezedSection
        SectionDef(NumSectionDefs)%NumFound = 0
    ENDIF

  RETURN

END SUBROUTINE AddSectionDef

!******************************************************************************

SUBROUTINE AddObjectDefandParse(ProposedObject,CurPos,EndofFile)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine processes data dictionary file.
    ! The structure of the sections and objects are stored in derived
    ! types (SectionDefs and ObjectDefs)

    ! SUBROUTINE ARGUMENT DEFINITIONS
    tCharStar, INTENT(IN)       :: ProposedObject   ! Proposed Object to Add
    tInteger,  INTENT(INOUT)    :: CurPos           ! Current position (initially at first ',') of InputLine
    tLogical,  INTENT(INOUT)    :: EndofFile        ! End of File marker

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical,            ALLOCATABLE, SAVE  :: AlphaorNumeric(:)    ! Array of argument designations, True is Alpha,
                                                                    ! False is numeric, saved in ObjectDef when done
    tLogical,            ALLOCATABLE, SAVE  :: TempAN(:)            ! Array (ref: AlphaOrNumeric) for re-allocation procedure
    TYPE(RangeCheckDef), ALLOCATABLE, SAVE  :: NumRangeChecks(:)    ! Structure for Range Check, Defaults of numeric fields
    TYPE(RangeCheckDef), ALLOCATABLE, SAVE  :: TempChecks(:)        ! Structure (ref: NumRangeChecks) for re-allocation procedure
    tCharLen(MaxObjectNameLength)   :: SqueezedObject   ! Input Object, Left Justified, UpperCase
    tInteger                        :: Count            ! Count on arguments, loop
    tInteger                        :: Pos              ! Position scanning variable
    tLogical                        :: EndofObjectDef   ! Set to true when ; has been found
    tLogical                        :: ErrFlag          ! Local Error condition flag, when true, object not added to Global list
    tCharLen(1)                     :: TargetChar       ! Single character scanned to test for current field TYPE(A or N)
    tLogical                        :: BlankLine        ! True when this line is "blank" (may have comment characters as first character on line)
    tLogical                        :: MinMax           ! Set to true when MinMax field has been found by ReadInputLine
    tLogical                        :: Default          ! Set to true when Default field has been found by ReadInputLine
    tCharLen(20)                    :: MinMaxString     ! Set from ReadInputLine
    tInteger                        :: WhichMinMax      ! = 0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
    tReal                           :: Value            ! Value returned by ReadInputLine (either min, max, or default)
    tLogical                        :: MinMaxError      ! Used to see if min, max, defaults have been set appropriately (True if error)
    tInteger, SAVE                  :: MaxANArgs = 100  ! Current count of Max args to object
    
    ! FLOW:
    
    IF (.NOT.ALLOCATED(AlphaorNumeric)) THEN
        ALLOCATE(AlphaorNumeric(0:MaxANArgs))
        ALLOCATE(NumRangeChecks(MaxANArgs))
    ENDIF

    SqueezedObject = MakeUpperCase(ADJUSTL(ProposedObject))
    IF (LEN_TRIM(ADJUSTL(ProposedObject)) > MaxObjectNameLength) THEN
        ErrMsg = 'Object length exceeds maximum, will be truncated = ' // TRIM(ProposedObject)
        CALL DisplayWarningError(ErrMsg, EchoInputFile)
        ErrMsg = 'Will be processed as Object = ' // TRIM(SqueezedObject)
        CALL DisplayMessage(ErrMsg, EchoInputFile)
    ENDIF
    ErrFlag = FalseVal

    IF (SqueezedObject /= Blank) THEN
        IF (FindItemInList(SqueezedObject, ObjectDef%Name, NumObjectDefs) > 0) THEN
            ErrMsg = 'Already an Object called ' // TRIM(SqueezedObject) // &
                     '. This definition ignored.'
            CALL DisplayWarningError(ErrMsg, EchoInputFile)
            ! Error Condition
            ErrFlag = TrueVal
            ! Rest of Object has to be processed. Error condition will be caught
            ! at end
        ENDIF
    ELSE
        ErrFlag = TrueVal
    ENDIF

    NumObjectDefs = NumObjectDefs + 1
    ObjectDef(NumObjectDefs)%Name = SqueezedObject
    ObjectDef(NumObjectDefs)%NumParams = 0
    ObjectDef(NumObjectDefs)%NumAlpha = 0
    ObjectDef(NumObjectDefs)%NumNumeric = 0
    ObjectDef(NumObjectDefs)%NumFound = 0
    ObjectDef(NumObjectDefs)%VersionNum = 0

    AlphaorNumeric = TrueVal

    NumRangeChecks%MinMaxChk = FalseVal
    NumRangeChecks%WhichMinMax(1) = 0
    NumRangeChecks%WhichMinMax(2) = 0
    NumRangeChecks%MinMaxString(1) = Blank
    NumRangeChecks%MinMaxString(2) = Blank
    NumRangeChecks%MinMaxValue(1) = Zero
    NumRangeChecks%MinMaxValue(2) = Zero
    NumRangeChecks%Default = Zero
    NumRangeChecks%DefaultChk = FalseVal
    NumRangeChecks%FieldName = Blank

    Count = 0
    EndofObjectDef = FalseVal
  
    ! Parse rest of Object Definition
    DO WHILE (.NOT.EndofFile .AND. .NOT.EndofObjectDef)

        IF (CurPos <= InputLineLength) THEN
            Pos = SCAN(InputLine(CurPos:InputLineLength), AlphaNum)
            IF (Pos > 0) THEN

                Count = Count + 1

                IF (Count > MaxANArgs) THEN   ! Reallocation
                    ALLOCATE(TempAN(0:MaxANArgs+ObjectDefAllocInc))
                    TempAN = FalseVal
                    TempAN(0:MaxANArgs) = AlphaorNumeric
                    DEALLOCATE(AlphaorNumeric)
                    ALLOCATE(TempChecks(MaxANArgs+ObjectDefAllocInc))
                    TempChecks%MinMaxChk = FalseVal
                    TempChecks%WhichMinMax(1) = 0
                    TempChecks%WhichMinMax(2) = 0
                    TempChecks%MinMaxString(1) = Blank
                    TempChecks%MinMaxString(2) = Blank
                    TempChecks%MinMaxValue(1) = Zero
                    TempChecks%MinMaxValue(2) = Zero
                    TempChecks%Default = Zero
                    TempChecks%DefaultChk = FalseVal
                    TempChecks%FieldName = Blank
                    TempChecks(1:MaxANArgs) = NumRangeChecks(1:MaxANArgs)
                    DEALLOCATE(NumRangeChecks)
                    ALLOCATE(AlphaorNumeric(0:MaxANArgs+ObjectDefAllocInc))
                    AlphaorNumeric = TempAN
                    DEALLOCATE(TempAN)
                    ALLOCATE(NumRangeChecks(MaxANArgs+ObjectDefAllocInc))
                    NumRangeChecks = TempChecks
                    DEALLOCATE(TempChecks)
                    MaxANArgs = MaxANArgs + ObjectDefAllocInc
                ENDIF

                TargetChar = InputLine(CurPos+Pos-1:CurPos+Pos-1)

                IF (TargetChar == 'A' .OR. TargetChar == 'a') THEN
                    AlphaorNumeric(Count) = TrueVal
                    ObjectDef(NumObjectDefs)%NumAlpha = ObjectDef(NumObjectDefs)%NumAlpha + 1
                ELSE
                    AlphaorNumeric(Count) = FalseVal
                    ObjectDef(NumObjectDefs)%NumNumeric = ObjectDef(NumObjectDefs)%NumNumeric + 1
                    IF (FieldSet) THEN
                        NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%FieldName = CurrentFieldName
                    END IF
                ENDIF

            ELSE
                CALL ReadInputLine(IDDFile, CurPos, BlankLine, InputLineLength, EndofFile,  &
                                   MinMax=MinMax, WhichMinMax=WhichMinMax,                  &
                                   MinMaxString=MinMaxString, Value=Value, Default=Default)
                IF (.NOT.AlphaorNumeric(Count)) THEN
                    ! only record for numeric fields
                    IF (MinMax) THEN
                        NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxChk = TrueVal
                        NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%FieldNumber = Count
                        IF (WhichMinMax <= 2) THEN   ! = 0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
                            NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%WhichMinMax(1) = WhichMinMax
                            NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxString(1) = MinMaxString
                            NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxValue(1) = Value
                        ELSE
                            NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%WhichMinMax(2) = WhichMinMax
                            NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxString(2) = MinMaxString
                            NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxValue(2) = Value
                        ENDIF
                    ENDIF
                    IF (Default) THEN
                        NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%DefaultChk = TrueVal
                        NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%Default = Value
                    ENDIF
                ENDIF
                CYCLE
            ENDIF

            !  For the moment dont care about descriptions on each object
            IF (CurPos <= InputLineLength) THEN
                CurPos = CurPos + Pos
                Pos = SCAN(InputLine(CurPos:InputLineLength), ',;')
            ELSE
                CALL ReadInputLine(IDDFile, CurPos, BlankLine, InputLineLength, EndofFile)
                IF (BlankLine .OR. EndofFile) CYCLE
                Pos = SCAN(InputLine(CurPos:InputLineLength), ',;')
            ENDIF
        ELSE
            CALL ReadInputLine(IDDFile, CurPos, BlankLine, InputLineLength, EndofFile)
            CYCLE
        ENDIF

        IF (Pos <= 0) THEN
            ! must be time to read another line
            CALL ReadInputLine(IDDFile, CurPos, BlankLine, InputLineLength, EndofFile)
            IF (BlankLine .OR. EndofFile) CYCLE
        ELSE
            IF (InputLine(CurPos+Pos-1:CurPos+Pos-1) == ';') THEN
                EndofObjectDef = TrueVal
            ENDIF
            CurPos = CurPos + Pos
        ENDIF

    END DO

    ! Reached end of object def but there may still be more \ lines to parse....
    ! Goes until next object is encountered ("not blankline") or end of IDDFile
    ! If last object is not numeric, then exit immediately....
    IF (.NOT.AlphaorNumeric(Count)) THEN
        BlankLine = TrueVal
        DO WHILE (BlankLine .AND. .NOT.EndofFile)
            ! It's a numeric object as last one...
            CALL ReadInputLine(IDDFile, CurPos, BlankLine, InputLineLength, EndofFile,  &
                               MinMax=MinMax, WhichMinMax=WhichMinMax,                  &
                               MinMaxString=MinMaxString, Value=Value, Default=Default)
            IF (MinMax) THEN
                NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxChk = TrueVal
                NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%FieldNumber = Count
                IF (WhichMinMax <= 2) THEN   ! = 0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
                    NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%WhichMinMax(1) = WhichMinMax
                    NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxString(1) = MinMaxString
                    NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxValue(1) = Value
                ELSE
                    NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%WhichMinMax(2) = WhichMinMax
                    NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxString(2) = MinMaxString
                    NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%MinMaxValue(2) = Value
                ENDIF
            ENDIF
            IF (Default) THEN
                NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%DefaultChk = TrueVal
                NumRangeChecks(ObjectDef(NumObjectDefs)%NumNumeric)%Default = Value
            ENDIF
        ENDDO
        IF (.NOT.BlankLine) THEN
            BACKSPACE(UNIT=IDDFile)
            EchoInputLine = FalseVal
        ENDIF
    ENDIF

    ObjectDef(NumObjectDefs)%NumParams = Count  ! Also the total of ObjectDef(..)%NumAlpha+ObjectDef(..)%NumNumeric
    MaxAlphaArgsFound = MAX(MaxAlphaArgsFound, ObjectDef(NumObjectDefs)%NumAlpha)
    MaxNumericArgsFound = MAX(MaxNumericArgsFound, ObjectDef(NumObjectDefs)%NumNumeric)
    ALLOCATE(ObjectDef(NumObjectDefs)%AlphaorNumeric(Count))
    ObjectDef(NumObjectDefs)%AlphaorNumeric = AlphaorNumeric(1:Count)
    ALLOCATE(ObjectDef(NumObjectDefs)%NumRangeChks(ObjectDef(NumObjectDefs)%NumNumeric))
    ObjectDef(NumObjectDefs)%NumRangeChks = NumRangeChecks(1:ObjectDef(NumObjectDefs)%NumNumeric)
    DO Count = 1, ObjectDef(NumObjectDefs)%NumNumeric
        IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxChk) THEN
            ! Checking MinMax Range (min vs. max and vice versa)
            MinMaxError = FalseVal
            ! check min against max
            IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(1) == 1) THEN
                ! min
                Value = ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(1)
                IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(2) == 3) THEN
                    IF (Value > ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(2)) &
                        MinMaxError = TrueVal
                ELSEIF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(2) == 4) THEN
                    IF (Value == ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(2)) &
                        MinMaxError = TrueVal
                ENDIF
            ELSEIF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(1) == 2) THEN
                ! min>
                Value = ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(1) + TINY(Value)  ! infintesimally bigger than min
                IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(2) == 3) THEN
                    IF (Value > ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(2)) &
                        MinMaxError = TrueVal
                ELSEIF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(2) == 4) THEN
                    IF (Value == ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(2)) &
                        MinMaxError = TrueVal
                ENDIF
            ENDIF
            ! check max against min
            IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(2) == 3) THEN
                ! max
                Value = ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(2)
                ! Check max value against min
                IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(1) == 1) THEN
                    IF (Value < ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(1)) &
                        MinMaxError = TrueVal
                ELSEIF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(1) == 2) THEN
                    IF (Value == ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(1)) &
                        MinMaxError = TrueVal
                ENDIF
            ELSEIF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(2) == 4) THEN
                ! max<
                Value = ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(2) - TINY(Value)  ! infintesimally bigger than min
                IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(1) == 1) THEN
                    IF (Value < ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(1)) &
                        MinMaxError = TrueVal
                ELSEIF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(1) == 2) THEN
                    IF (Value == ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(1)) &
                        MinMaxError = TrueVal
                ENDIF
            ENDIF
            ! check if error condition
            IF (MinMaxError) THEN
                !  Error stated min is not in range with stated max
                WRITE(MinMaxString,*) ObjectDef(NumObjectDefs)%NumRangeChks(Count)%FieldNumber
                MinMaxString = ADJUSTL(MinMaxString)
                ErrMsg = 'Field #' // TRIM(MinMaxString) //                          &
                         ' conflict in Min/Max specifications/values, in class = '// &
                         TRIM(ObjectDef(NumObjectDefs)%Name)
                CALL DisplaySevereError(ErrMsg, EchoInputFile)
                ErrFlag = TrueVal
            ENDIF
        ENDIF
        IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%DefaultChk) THEN
            ! Check Default against MinMaxRange
            MinMaxError = FalseVal
            Value = ObjectDef(NumObjectDefs)%NumRangeChks(Count)%Default
            IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(1) == 1) THEN
                IF (Value < ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(1)) &
                        MinMaxError = TrueVal
            ELSEIF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(1) == 2) THEN
                IF (Value <= ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(1)) &
                        MinMaxError = TrueVal
            ENDIF
            IF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(2) == 3) THEN
                IF (Value > ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(2)) &
                        MinMaxError = TrueVal
            ELSEIF (ObjectDef(NumObjectDefs)%NumRangeChks(Count)%WhichMinMax(2) == 4) THEN
                IF (Value >= ObjectDef(NumObjectDefs)%NumRangeChks(Count)%MinMaxValue(2)) &
                        MinMaxError = TrueVal
            ENDIF
            IF (MinMaxError) THEN
                !  Error stated default is not in min/max range
                WRITE(MinMaxString,*) ObjectDef(NumObjectDefs)%NumRangeChks(Count)%FieldNumber
                MinMaxString = ADJUSTL(MinMaxString)
                ErrMsg = 'Field #' // TRIM(MinMaxString) //                      &
                         ' default is invalid for Min/Max values, in class = '// &
                         TRIM(ObjectDef(NumObjectDefs)%Name)
                CALL DisplaySevereError(ErrMsg, EchoInputFile)
                ErrFlag = TrueVal
            ENDIF
        ENDIF
    END DO
      
    IF (ErrFlag) THEN
        ErrMsg = 'Errors occured in ObjectDefinition for Class = '// &
                 TRIM(ObjectDef(NumObjectDefs)%Name)
        CALL DisplaySevereError(ErrMsg, EchoInputFile)
        DEALLOCATE(ObjectDef(NumObjectDefs)%AlphaorNumeric)
        NumObjectDefs = NumObjectDefs - 1
    ENDIF

    RETURN

END SUBROUTINE AddObjectDefandParse

!******************************************************************************

SUBROUTINE ProcessInputDataFile

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine processes data dictionary file for EnergyPlus.
    ! The structure of the sections and objects are stored in derived
    ! types (SectionDefs and ObjectDefs)

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    TYPE(FileSectionsDefinition), ALLOCATABLE   :: TempSectionsonFile(:)    ! Used during reallocation procedure
    TYPE(LineDefinition),         ALLOCATABLE   :: TempIDFRecords(:)        ! Used during reallocation procedure
    tLogical    :: EndofFile = FalseVal
    tLogical    :: BlankLine
    tInteger    :: Pos
    
    ! FLOW:
    
    MaxIDFRecords = ObjectsIDFAllocInc
    NumIDFRecords = 0
    MaxIDFSections = SectionsIDFAllocInc
    NumIDFSections = 0

    ALLOCATE(SectionsonFile(MaxIDFSections))
    SectionsonFile%Name = ' '       ! Name of this section
    SectionsonFile%FirstRecord = 0  ! Record number of first object in section
    SectionsonFile%LastRecord = 0   ! Record number of last object in section
    ALLOCATE(IDFRecords(MaxIDFRecords))
    IDFRecords%Name = ' '           ! Object name for this record
    IDFRecords%NumAlphas = 0        ! Number of alphas on this record
    IDFRecords%NumNumbers = 0       ! Number of numbers on this record
    ALLOCATE(LineItem%Numbers(MaxNumericArgsFound))
    ALLOCATE(LineItem%Alphas(MaxAlphaArgsFound))

    DO WHILE (.NOT.EndofFile)
        CALL ReadInputLine(IDFFile, Pos, BlankLine, InputLineLength, EndofFile)
        IF (BlankLine .OR. EndofFile) CYCLE
        Pos = SCAN(InputLine, ',;')
        IF (Pos /= 0) THEN
            IF (InputLine(Pos:Pos) == ';') THEN
                CALL ValidateSection(InputLine(1:Pos-1))
                IF (NumIDFSections == MaxIDFSections) THEN
                    ALLOCATE(TempSectionsonFile(MaxIDFSections+SectionsIDFAllocInc))
                    TempSectionsonFile%Name = ' '        ! Name of this section
                    TempSectionsonFile%FirstRecord = 0   ! Record number of first object in section
                    TempSectionsonFile%LastRecord = 0    ! Record number of last object in section
                    TempSectionsonFile(1:MaxIDFSections) = SectionsonFile
                    DEALLOCATE(SectionsonFile)
                    ALLOCATE(SectionsonFile(MaxIDFSections+SectionsIDFAllocInc))
                    SectionsonFile = TempSectionsonFile
                    DEALLOCATE(TempSectionsonFile)
                    MaxIDFSections = MaxIDFSections + SectionsIDFAllocInc
                ENDIF
            ELSE
                CALL ValidateObjectandParse(InputLine(1:Pos-1), Pos, EndofFile)
                IF (NumIDFRecords == MaxIDFRecords) THEN
                    ALLOCATE(TempIDFRecords(MaxIDFRecords+ObjectsIDFAllocInc))
                    TempIDFRecords%Name = ' '          ! Object name for this record
                    TempIDFRecords%NumAlphas = 0       ! Number of alphas on this record
                    TempIDFRecords%NumNumbers = 0      ! Number of numbers on this record
                    TempIDFRecords(1:MaxIDFRecords) = IDFRecords
                    DEALLOCATE(IDFRecords)
                    ALLOCATE(IDFRecords(MaxIDFRecords+ObjectsIDFAllocInc))
                    IDFRecords = TempIDFRecords
                    DEALLOCATE(TempIDFRecords)
                    MaxIDFRecords = MaxIDFRecords + ObjectsIDFAllocInc
                ENDIF
            ENDIF
        ELSE
            !Error condition, no , or ; on first line
            CALL DisplaySevereError(', or ; expected on this line', EchoInputFile)
        ENDIF

    END DO

    IF (NumIDFSections > 0) THEN
        SectionsonFile(NumIDFSections)%LastRecord = NumIDFRecords
    ELSE
        CALL DisplayWarningError('No "Sections" found on input file', EchoInputFile)
    ENDIF

    IF (OverallErrorFlag) THEN
        CALL DisplaySevereError('Possible incorrect IDD File', EchoInputFile)
        CALL DisplaySevereError('Possible Invalid Numerics', EchoInputFile)
        ErrMsg = 'Errors occurred on processing IDF file. View "' // &
                 TRIM(LogFileName) // '" for details.'
        CALL DisplayFatalError(ErrMsg, EchoInputFile)
    ENDIF

    RETURN

END SUBROUTINE ProcessInputDataFile

!******************************************************************************

SUBROUTINE ValidateSection(ProposedSection)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine validates the section from the input data file
    ! with the list of objects from the data dictionary file.

    ! METHODOLOGY EMPLOYED:
    ! A "squeezed" string is formed and checked against the list of
    ! sections.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: ProposedSection

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(MaxSectionNameLength)  :: SqueezedSection
    tInteger                        :: Found
    
    ! FLOW:
    
    SqueezedSection = MakeUpperCase(ADJUSTL(ProposedSection))
    IF (LEN_TRIM(ADJUSTL(ProposedSection)) > MaxSectionNameLength) THEN
        ErrMsg = 'Section length exceeds maximum, will be truncated = ' // TRIM(ProposedSection)
        CALL DisplayWarningError(ErrMsg, EchoInputFile)
        ErrMsg = 'Will be processed as Section = ' // TRIM(SqueezedSection)
        CALL DisplayMessage(ErrMsg, EchoInputFile)
    ENDIF
    IF (SqueezedSection(1:3) /= 'END') THEN
        Found = FindIteminList(SqueezedSection, ListofSections, NumSectionDefs)
        IF (Found == 0) THEN
            ErrMsg = 'Did not find "' // TRIM(ProposedSection) // '" in list of Sections'
            CALL DisplaySevereError(ErrMsg, EchoInputFile)
        ELSE
            IF (NumIDFSections > 0) THEN
                SectionsonFile(NumIDFSections)%LastRecord = NumIDFRecords
            ENDIF
            SectionDef(Found)%NumFound = SectionDef(Found)%NumFound + 1
            NumIDFSections = NumIDFSections + 1
            SectionsonFile(NumIDFSections)%Name = ListofSections(Found)
            SectionsonFile(NumIDFSections)%FirstRecord = NumIDFRecords + 1
        ENDIF
    ENDIF

    RETURN

END SUBROUTINE ValidateSection

!******************************************************************************

SUBROUTINE ValidateObjectandParse(ProposedObject,CurPos,EndofFile)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine validates the proposed object from the IDF and then
    ! parses it, putting it into the internal InputProcessor Data structure.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)       :: ProposedObject
    tInteger,  INTENT(INOUT)    :: CurPos
    tLogical,  INTENT(INOUT)    :: EndofFile

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(MaxObjectNameLength)   :: SqueezedObject
    tCharLen(MaxAlphaArgLength)     :: SqueezedArg
    tInteger                        :: Found
    tInteger                        :: NumArg
    tInteger                        :: NumArgExpected
    tInteger                        :: NumAlpha
    tInteger                        :: NumNumeric
    tInteger                        :: Pos
    tLogical                        :: EndofObject
    tLogical                        :: BlankLine
    tLogical, SAVE                  :: ErrFlag = FalseVal
    tInteger                        :: LenLeft
    
    ! FLOW:
    
    SqueezedObject = MakeUpperCase(ADJUSTL(ProposedObject))
    IF (LEN_TRIM(ADJUSTL(ProposedObject)) > MaxObjectNameLength) THEN
        ErrMsg = 'Object length exceeds maximum, will be truncated = ' // TRIM(ProposedObject)
        CALL DisplayWarningError(ErrMsg, EchoInputFile)
        ErrMsg = 'Will be processed as Section = ' // TRIM(SqueezedObject)
        CALL DisplayMessage(ErrMsg, EchoInputFile)
    ENDIF

    Found = FindIteminList(SqueezedObject,ListofObjects,NumObjectDefs)
    IF (Found == 0) THEN
        ErrMsg = 'Did not find "' // TRIM(ProposedObject) // '" in list of Objects'
        CALL DisplaySevereError(ErrMsg, EchoInputFile)
        ! Will need to parse to next ;
        ErrFlag = TrueVal
    ELSE

        ! Start Parsing the Object according to definition

        ErrFlag = FalseVal
        LineItem%Name = SqueezedObject
        LineItem%Alphas = ' '
        LineItem%NumAlphas = 0
        LineItem%Numbers = Zero
        LineItem%NumNumbers = 0
        NumArgExpected = ObjectDef(Found)%NumParams
        ObjectDef(Found)%NumFound = ObjectDef(Found)%NumFound + 1
    ENDIF

    NumArg = 0
    NumAlpha = 0
    NumNumeric = 0
    EndofObject = FalseVal
    CurPos = CurPos + 1

    DO WHILE (.NOT.EndofFile .AND. .NOT.EndofObject)
        IF (CurPos <= InputLineLength) THEN
            Pos = SCAN(InputLine(CurPos:InputLineLength), ',;')
            !      IF (Pos == 0) CurPos = InputLineLength + 1
            IF (Pos == 0) THEN
                IF (InputLine(InputLineLength:InputLineLength) == '!' .OR.  &
                    InputLine(InputLineLength:InputLineLength) == '\') THEN
                    LenLeft = LEN_TRIM(InputLine(CurPos:InputLineLength-1))
                ELSE
                    LenLeft = LEN_TRIM(InputLine(CurPos:InputLineLength))
                ENDIF
                IF (LenLeft == 0) THEN
                    CurPos = InputLineLength + 1
                    CYCLE
                ELSE
                    IF (InputLine(InputLineLength:InputLineLength) == '!' .OR.  &
                        InputLine(InputLineLength:InputLineLength) == '\') THEN
                        Pos = InputLineLength - CurPos + 1
                        ErrMsg = 'Comma being inserted after:"'      // &
                                 InputLine(CurPos:InputLineLength-1) // &
                                 '" in Object = ' // TRIM(SqueezedObject)
                        CALL DisplayWarningError(ErrMsg, EchoInputFile)
                    ELSE
                        Pos = InputLineLength - CurPos + 2
                        ErrMsg = 'Comma being inserted after:'     // &
                                 InputLine(CurPos:InputLineLength) // &
                                 '" in Object = ' // TRIM(SqueezedObject)
                        CALL DisplayWarningError(ErrMsg, EchoInputFile)
                    ENDIF
                ENDIF
            ENDIF
        ELSE
            CALL ReadInputLine(IDFFile, CurPos, BlankLine, InputLineLength, EndofFile)
            CYCLE
        ENDIF
        IF (Pos > 0) THEN
            IF (.NOT.ErrFlag) THEN
                IF (CurPos <= CurPos+Pos-2) THEN
                    SqueezedArg = MakeUpperCase(ADJUSTL(InputLine(CurPos:CurPos+Pos-2)))
                    IF (LEN_TRIM(ADJUSTL(InputLine(CurPos:CurPos+Pos-2))) > MaxAlphaArgLength) THEN
                        ErrMsg = 'Alpha Argument length exceeds maximum, will be truncated = ' // &
                                 TRIM(InputLine(CurPos:CurPos+Pos-2))
                        CALL DisplayWarningError(ErrMsg, EchoInputFile)
                        ErrMsg = 'Will be processed as Alpha = ' // TRIM(SqueezedArg)
                        CALL DisplayMessage(ErrMsg, EchoInputFile)
                    ENDIF
                ELSE
                    SqueezedArg = ' '
                ENDIF
                IF (NumArg == NumArgExpected) THEN
                    ErrMsg = 'Error detected for Object = ' // TRIM(ObjectDef(Found)%Name)
                    CALL DisplaySevereError(ErrMsg, EchoInputFile)
                    ErrMsg = ' Maximum arguments reached for this object, trying to process ->' //&
                             TRIM(SqueezedArg) // '<-'
                    CALL DisplayMessage(ErrMsg, EchoInputFile)
                    ErrFlag = TrueVal
                ELSE
                    NumArg=NumArg + 1
                    IF (ObjectDef(Found)%AlphaorNumeric(NumArg)) THEN
                        IF (NumAlpha == ObjectDef(Found)%NumAlpha) THEN
                            ErrMsg = 'Error detected for Object = ' // TRIM(ObjectDef(Found)%Name)
                            CALL DisplaySevereError(ErrMsg, EchoInputFile)
                            ErrMsg = ' Too many Alphas for this object, trying to process ->' // &
                                     TRIM(SqueezedArg) // '<-'
                            CALL DisplayMessage(ErrMsg, EchoInputFile)
                            ErrFlag = TrueVal
                        ELSE
                            NumAlpha = NumAlpha + 1
                            LineItem%NumAlphas = NumAlpha
                            LineItem%Alphas(NumAlpha) = SqueezedArg
                        ENDIF
                    ELSE
                        IF (NumNumeric == ObjectDef(Found)%NumNumeric) THEN
                            ErrMsg = 'Error detected for Object = ' // TRIM(ObjectDef(Found)%Name)
                            CALL DisplaySevereError(ErrMsg, EchoInputFile)
                            ErrMsg = ' Too many Numbers for this object, trying to process ->' // &
                                     TRIM(SqueezedArg) // '<-'
                            CALL DisplayMessage(ErrMsg, EchoInputFile)
                            ErrFlag = TrueVal
                        ELSE
                            NumNumeric = NumNumeric + 1
                            LineItem%NumNumbers = NumNumeric
                            IF (SqueezedArg /= Blank) THEN
                                LineItem%Numbers(NumNumeric) = ProcessNumber(SqueezedArg, Errflag)
                            ELSE
                                IF (ObjectDef(Found)%NumRangeChks(NumNumeric)%DefaultChk) THEN
                                    LineItem%Numbers(NumNumeric) = ObjectDef(Found)%NumRangeChks(NumNumeric)%Default
                                ELSE
                                    LineItem%Numbers(NumNumeric) = Zero
                                ENDIF
                                ErrFlag = FalseVal
                            ENDIF
                            IF (ErrFlag) THEN
                                ErrMsg = 'Invalid Number in Input="' // TRIM(SqueezedArg) // &
                                         '" in object = ' // TRIM(ObjectDef(Found)%Name)
                                CALL DisplaySevereError(ErrMsg, EchoInputFile)
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF

            IF (InputLine(CurPos+Pos-1:CurPos+Pos-1) == ';') THEN
                EndofObject = TrueVal
            ENDIF
            CurPos = CurPos + Pos
        ENDIF

    END DO

    ! Store to IDFRecord Data Structure, ErrFlag is true if there was an error
    IF (.NOT.ErrFlag) THEN
        NumIDFRecords = NumIDFRecords + 1
        IDFRecords(NumIDFRecords)%Name = LineItem%Name
        IDFRecords(NumIDFRecords)%NumNumbers = LineItem%NumNumbers
        IDFRecords(NumIDFRecords)%NumAlphas = LineItem%NumAlphas
        ALLOCATE(IDFRecords(NumIDFRecords)%Alphas(LineItem%NumAlphas))
        ALLOCATE(IDFRecords(NumIDFRecords)%Numbers(LineItem%NumNumbers))
        IDFRecords(NumIDFRecords)%Alphas(1:LineItem%NumAlphas) = LineItem%Alphas(1:LineItem%NumAlphas)
        IDFRecords(NumIDFRecords)%Numbers(1:LineItem%NumNumbers) = LineItem%Numbers(1:LineItem%NumNumbers)
    ELSE
        OverallErrorFlag = TrueVal
    ENDIF

    RETURN

END SUBROUTINE ValidateObjectandParse

!******************************************************************************

SUBROUTINE ValidateSectionsInput

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine uses the data structure that is set up during
    ! IDF processing and makes sure that record pointers are accurate.
    ! They could be inaccurate if a 'section' is input without any
    ! 'objects' following.  The invalidity will show itself in the
    ! values of the FirstRecord and Last Record pointer.
    ! If FirstRecord>LastRecord, then no records (Objects) have been
    ! written to the SIDF file for that Section.

    ! METHODOLOGY EMPLOYED:
    ! Scan the SectionsonFile data structure and look for invalid
    ! FirstRecord,LastRecord items.  Reset those items to -1.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: Count
    
    ! FLOW:
    
    DO Count = 1, NumIDFSections
        IF (SectionsonFile(Count)%FirstRecord > SectionsonFile(Count)%LastRecord) THEN
            WRITE(EchoInputFile,*) ' Section ', Count, ' ',          &
                                   TRIM(SectionsonFile(Count)%Name), &
                                   ' had no object records'
            SectionsonFile(Count)%FirstRecord = -1
            SectionsonFile(Count)%LastRecord = -1
        ENDIF
    END DO

    RETURN

END SUBROUTINE ValidateSectionsInput

!******************************************************************************

FUNCTION GetNumSectionsFound(SectionWord) RESULT(NumSec)

    ! PURPOSE OF THIS SUBROUTINE:
    !^ This function returns the number of a particular section (in input data file)
    !  found in the current run.  If it can't find the section in list
    !  of sections, a -1 will be returned.

    ! METHODOLOGY EMPLOYED:
    ! Look up section in list of sections.  If there, return the
    ! number of sections of that kind found in the current input.
    ! If not, return -1.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: SectionWord
    tInteger                :: NumSec

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: Found
    
    ! FLOW:
    
    Found = FindIteminList(MakeUpperCase(SectionWord), ListofSections, NumSectionDefs)
    IF (Found == 0) THEN
        ErrMsg = 'Requested Section not found in Definitions: ' // TRIM(SectionWord)
        CALL DisplayFatalError(ErrMsg, EchoInputFile)
    ELSE
        NumSec = SectionDef(Found)%NumFound
    ENDIF

    RETURN

END FUNCTION GetNumSectionsFound

!******************************************************************************

FUNCTION GetNumSectionsinInput() RESULT(NumSec)

    ! PURPOSE OF THIS SUBROUTINE:
    !^ This function returns the number of sections in the entire input data file
    !  of the current run.

    ! METHODOLOGY EMPLOYED:
    ! Return value of NumIDFSections.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger    :: NumSec

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
    
    ! FLOW:
    
    NumSec = NumIDFSections

    RETURN

END FUNCTION GetNumSectionsinInput

!******************************************************************************

SUBROUTINE GetListofSectionsinInput(SectionList,NuminList)

    ! PURPOSE OF THIS SUBROUTINE:
    !^ This subroutine returns the list of sections as they occurred
    !  in the Input Data File (IDF).

    ! METHODOLOGY EMPLOYED:
    ! Look up object in list of objects.  If there, return the
    ! number of objects found in the current input.
    ! If not, return -1.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(OUT)  :: SectionList(:)
    tInteger,  INTENT(OUT)  :: NuminList

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: MaxAllowedOut
    
    ! FLOW:
    
    MaxAllowedOut = MIN(NumIDFSections,SIZE(SectionList))
    IF (MaxAllowedOut /= NumIDFSections) THEN
        ErrMsg = 'More in list than allowed in passed array - (GetListofSectionsinInput)'
        CALL DisplayWarningError(ErrMsg, EchoInputFile)
    ENDIF
    NuminList = MaxAllowedOut
    SectionList(1:MaxAllowedOut) = SectionsonFile(1:MaxAllowedOut)%Name

    RETURN

END SUBROUTINE GetListofSectionsinInput

!******************************************************************************

FUNCTION GetNumObjectsFound(ObjectWord) RESULT(NumObj)

    ! PURPOSE OF THIS SUBROUTINE:
    !^ This function returns the number of objects (in input data file)
    !  found in the current run.  If it can't find the object in list
    !  of objects, a -1 will be returned.

    ! METHODOLOGY EMPLOYED:
    ! Look up object in list of objects.  If there, return the
    ! number of objects found in the current input.  If not,
    ! return -1.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: ObjectWord
    tInteger                :: NumObj

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger                        :: Found
    tLogical, SAVE, ALLOCATABLE     :: TrackCalls(:)
    
    ! FLOW:
    
    IF (.NOT.ALLOCATED(TrackCalls)) THEN
        ALLOCATE(TrackCalls(NumObjectDefs))
        TrackCalls = FalseVal
        IF (NumObjectDefs == 0) THEN
            CALL ProcessInput(IDDFileName, IDFFileName, LogFileName)
        ENDIF
    ENDIF

    Found = FindIteminList(MakeUpperCase(ObjectWord), ListofObjects, NumObjectDefs)

    IF (Found == 0) THEN
        ErrMsg = 'Requested Object not found in Definitions: ' // TRIM(ObjectWord)
        CALL DisplayFatalError(ErrMsg, EchoInputFile)
    ELSE
        NumObj = ObjectDef(Found)%NumFound
        IF (TrackCalls(Found)) THEN
            ErrMsg = 'Multiple GetNumObjects for object = ' // TRIM(ObjectWord)
            CALL DisplayWarningError(ErrMsg, EchoInputFile)
        ELSE
            TrackCalls(Found) = TrueVal
        ENDIF
    ENDIF

    RETURN

END FUNCTION GetNumObjectsFound

!******************************************************************************

SUBROUTINE GetRecordLocations(Which,FirstRecord,LastRecord)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine returns the record location values (which will be
    ! passed to 'GetObjectItem') for a section from the list of inputted
    ! sections (sequential).

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger, INTENT(IN)    :: Which
    tInteger, INTENT(OUT)   :: FirstRecord
    tInteger, INTENT(OUT)   :: LastRecord

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
    
    ! FLOW:
    
    FirstRecord = SectionsonFile(Which)%FirstRecord
    LastRecord = SectionsonFile(Which)%LastRecord

    RETURN

END SUBROUTINE GetRecordLocations

!******************************************************************************

SUBROUTINE GetObjectItem(Object,Number,Alphas,NumAlphas,Numbers,NumNumbers,Status)

    ! PURPOSE OF THIS SUBROUTINE:
    !^ This subroutine gets the 'number' 'object' from the IDFRecord data structure.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Object
    tInteger,  INTENT(IN)   :: Number
    tCharStar, INTENT(OUT)  :: Alphas(:)
    tInteger,  INTENT(OUT)  :: NumAlphas
    tReal,     INTENT(OUT)  :: Numbers(:)
    tInteger,  INTENT(OUT)  :: NumNumbers
    tInteger,  INTENT(OUT)  :: Status

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger                                            :: Count, LoopIndex
    tCharLen(MaxObjectNameLength)                       :: ObjectWord
    tCharLen(MaxObjectNameLength)                       :: UCObject
    tCharLen(MaxObjectNameLength), SAVE, ALLOCATABLE    :: AlphaArgs(:)
    tReal,                         SAVE, ALLOCATABLE    :: NumberArgs(:)
    tInteger                                            :: MaxAlpha, MaxNumbers
    tInteger                                            :: Found
    
    ! FLOW:
    
    MaxAlpha = SIZE(Alphas,1)
    MaxNumbers  =SIZE(Numbers,1)

    IF (.NOT.ALLOCATED(AlphaArgs)) THEN
        IF (NumObjectDefs == 0) THEN
            CALL ProcessInput(IDDFileName, IDFFileName, LogFileName)
        ENDIF
        ALLOCATE(AlphaArgs(MaxAlphaArgsFound))
        ALLOCATE(NumberArgs(MaxNumericArgsFound))
    ENDIF

    Alphas(1:MaxAlpha) = ' '
    Numbers(1:MaxNumbers) = Zero
    Count = 0
    Status = -1
    UCOBject = MakeUpperCase(Object)

    IF (Number == 1) THEN
        WRITE(EchoInputFile,*) 'Getting object = ', TRIM(UCObject)
    ENDIF

    DO LoopIndex = 1, NumIDFRecords
        IF (IDFRecords(LoopIndex)%Name == UCObject) THEN
            Count = Count + 1
            IF (Count == Number) THEN
                ! Read this one
                CALL GetObjectItemfromFile(LoopIndex, ObjectWord, AlphaArgs, NumAlphas, &
                                           NumberArgs, NumNumbers)
                IF (NumAlphas > MaxAlpha .OR. NumNumbers > MaxNumbers) THEN
                    ErrMsg = 'Too many actual arguments for those expected on Object: ' // &
                             TRIM(ObjectWord) // ' (GetObjectItem)'
                    CALL DisplayWarningError(ErrMsg, EchoInputFile)
                ENDIF
                NumAlphas = MIN(MaxAlpha, NumAlphas)
                NumNumbers = MIN(MaxNumbers, NumNumbers)
                IF (NumAlphas > 0) THEN
                    Alphas(1:NumAlphas) = AlphaArgs(1:NumAlphas)
                ENDIF
                IF (NumNumbers > 0) THEN
                    Numbers(1:NumNumbers) = NumberArgs(1:NumNumbers)
                    Found = FindItem(UCObject, ObjectDef%Name, NumObjectDefs)
                    DO Count = 1, NumNumbers
                        IF (ObjectDef(Found)%NumRangeChks(Count)%MinMaxChk) THEN
                            CALL InternalRangeCheck(NumberArgs(Count), Count, Found, AlphaArgs(1))
                        ENDIF
                    ENDDO
                ENDIF
                Status = 1
                EXIT
            ENDIF
        ENDIF
    END DO

    RETURN

END SUBROUTINE GetObjectItem

!******************************************************************************

FUNCTION GetObjectItemNum(ObjType,ObjName) RESULT(NumObj)

    ! PURPOSE OF THIS SUBROUTINE:
    !^ Get the occurrence number of an object of type ObjType and name ObjName.

    ! METHODOLOGY EMPLOYED:
    ! Use internal IDF record structure for each object occurrence
    ! and compare the name with ObjName.

    ! SUBROUTINE ARGUMENTS:
    tCharStar, INTENT(IN)   :: ObjType   ! Object TYPE(ref: IDD Objects)
    tCharStar, INTENT(IN)   :: ObjName   ! Name of the object type
    tInteger                :: NumObj

    ! SUBROUTINE LOCAL VARIABLE DEFINITIONS
    tInteger                        :: NumObjOfType ! Total number of Object Type in IDF
    tInteger                        :: ObjNum       ! Loop index variable
    tInteger                        :: ItemNum      ! Item number for Object Name
    tInteger                        :: Found        ! Indicator for Object Type in list of Valid Objects
    tCharLen(MaxObjectNameLength)   :: UCObjType    ! Upper Case for ObjType
    
    ! FLOW:
    
    ItemNum = 0
    UCObjType = MakeUpperCase(ObjType)
    Found = FindIteminList(UCObjType, ListofObjects, NumObjectDefs)

    IF (Found /= 0) THEN

        NumObjOfType = ObjectDef(Found)%NumFound
        ItemNum = 0

        DO ObjNum = 1, NumIDFRecords
            IF (IDFRecords(ObjNum)%Name /= UCObjType) CYCLE
            ItemNum = ItemNum + 1
            IF (IDFRecords(ObjNum)%Alphas(1) == ObjName) EXIT
        END DO
    ENDIF

    NumObj = ItemNum

    RETURN

END FUNCTION GetObjectItemNum

!******************************************************************************

SUBROUTINE TellMeHowManyObjectItemArgs(Object,Number,NumAlpha,NumNumbers,Status)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine returns the number of arguments (alpha and numeric) for
    ! the referenced 'number' Object.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Object
    tInteger,  INTENT(IN)   :: Number
    tInteger,  INTENT(OUT)  :: NumAlpha
    tInteger,  INTENT(OUT)  :: NumNumbers
    tInteger,  INTENT(OUT)  :: Status

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger                        :: Count
    tInteger                        :: LoopIndex
    tCharLen(MaxObjectNameLength)   :: ObjectWord
    
    ! FLOW:
    
    Count = 0
    Status = -1
    DO LoopIndex = 1, NumIDFRecords
        IF (IDFRecords(LoopIndex)%Name == MakeUpperCase(Object)) THEN
            Count = Count + 1
            IF (Count == Number) THEN
                ! Read this one
                CALL GetObjectItemfromFile(LoopIndex, ObjectWord, NumAlpha=NumAlpha, &
                                           NumNumeric=NumNumbers)
                Status = 1
                EXIT
            ENDIF
        ENDIF
    END DO

    RETURN

END SUBROUTINE TellMeHowManyObjectItemArgs

!******************************************************************************

SUBROUTINE GetObjectItemfromFile(Which,ObjectWord,AlphaArgs,NumAlpha,NumericArgs,NumNumeric)

    ! PURPOSE OF THIS SUBROUTINE:
    !^ This subroutine "gets" the object instance from the data structure.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,  INTENT(IN)               :: Which
    tCharStar, INTENT(OUT)              :: ObjectWord
    tCharStar, INTENT(OUT), OPTIONAL    :: AlphaArgs(:)
    tInteger,  INTENT(OUT)              :: NumAlpha
    tReal,     INTENT(OUT), OPTIONAL    :: NumericArgs(:)
    tInteger,  INTENT(OUT)              :: NumNumeric

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
    
    ! FLOW:
    
    IF (Which >= 0 .AND. Which <= NumIDFRecords) THEN
        LineItem = IDFRecords(Which)
        ObjectWord = LineItem%Name
        NumAlpha = LineItem%NumAlphas
        NumNumeric = LineItem%NumNumbers
        IF (PRESENT(AlphaArgs)) THEN
            IF (NumAlpha >=1) THEN
                AlphaArgs(1:NumAlpha) = LineItem%Alphas(1:NumAlpha)
            ENDIF
        ENDIF
        IF (PRESENT(NumericArgs)) THEN
            IF (NumNumeric >= 1) THEN
                NumericArgs(1:NumNumeric) = LineItem%Numbers(1:NumNumeric)
            ENDIF
        ENDIF
    ELSE
        WRITE(EchoInputFile,*) ' Requested Record', Which, ' not in range, 1 -- ', NumIDFRecords
    ENDIF

    RETURN

END SUBROUTINE GetObjectItemfromFile

!******************************************************************************

! ++++ Utility Functions/Routines for Module +++

SUBROUTINE ReadInputLine(UnitNumber,CurPos,BlankLine,InputLineLength,EndofFile, &
                         MinMax,WhichMinMax,MinMaxString,Value,Default)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine reads a line in the specified file and checks for end of file

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,  INTENT(IN)               :: UnitNumber
    tInteger,  INTENT(INOUT)            :: CurPos
    tLogical,  INTENT(OUT)              :: EndofFile
    tLogical,  INTENT(OUT)              :: BlankLine
    tInteger,  INTENT(OUT)              :: InputLineLength
    tLogical,  INTENT(OUT), OPTIONAL    :: MinMax
    tInteger,  INTENT(OUT), OPTIONAL    :: WhichMinMax   ! = 0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
    tCharStar, INTENT(OUT), OPTIONAL    :: MinMaxString
    tReal,     INTENT(OUT), OPTIONAL    :: Value
    tLogical,  INTENT(OUT), OPTIONAL    :: Default

    ! SUBROUTINE PARAMETER DEFINITIONS:
    tCharLen(1), PARAMETER  :: TabChar = CHAR(9)

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger                        :: ReadStat
    tInteger                        :: Pos
    tInteger                        :: Slash
    tInteger                        :: P1
    tCharLen(MaxInputLineLength)    :: UCInputLine  ! Each line can be up to MaxInputLineLength characters long
    tLogical                        :: TabsInLine
    
    ! FLOW:
    
    READ(UnitNumber, '(A)', IOSTAT=ReadStat) InputLine
    P1 = SCAN(InputLine, TabChar)
    TabsInLine = FalseVal
    DO WHILE (P1>0)
        TabsInLine = TrueVal
        InputLine(P1:P1) = ' '
        P1 = SCAN(InputLine, TabChar)
    ENDDO
    BlankLine = FalseVal
    CurPos = 1
    IF (ReadStat == -1) THEN
        EndofFile = TrueVal
    ELSE
        IF (EchoInputLine) THEN
            NumLines = NumLines + 1
            WRITE(EchoInputFile, '(1X,I5,1X,A)') NumLines, TRIM(InputLine)
            IF (TabsInLine) WRITE(EchoInputFile, "(6X,'***** Tabs eliminated from above line')")
        ENDIF
        EchoInputLine = TrueVal
        InputLineLength = LEN_TRIM(InputLine)
        IF (InputLineLength == 0) THEN
            BlankLine = TrueVal
        ENDIF
        Pos = SCAN(InputLine, '!\~')
        Slash = INDEX(InputLine, '\')
        IF (Pos /= 0) THEN
            InputLineLength = Pos
            IF (Pos-1 > 0) THEN
                IF (LEN_TRIM(InputLine(1:Pos-1)) == 0) THEN
                    BlankLine = TrueVal
                ENDIF
            ELSE
                BlankLine = TrueVal
            ENDIF
            IF (Slash /= 0 .AND. Pos == Slash) THEN
                UCInputLine = MakeUpperCase(InputLine)
                IF (UCInputLine(Slash:Slash+5) == '\FIELD') THEN
                    ! Capture Field Name
                    CurrentFieldName = InputLine(Slash+6:)
                    CurrentFieldName = ADJUSTL(CurrentFieldName)
                    P1 = SCAN(CurrentFieldName, '!')
                    IF (P1 /= 0) CurrentFieldName(P1:) = Blank
                    FieldSet = TrueVal
                ELSE
                    FieldSet = FalseVal
                ENDIF
                IF (PRESENT(MinMax)) THEN
                    IF (UCInputLine(Pos:Pos+7)== '\MINIMUM' .OR.  &
                        UCInputLine(Pos:Pos+7)== '\MAXIMUM') THEN
                        MinMax = TrueVal
                        CALL ProcessMinMaxDefLine(UCInputLine(Pos:), WhichMinMax, &
                                                  MinMaxString, Value)
                    ELSE
                        MinMax = FalseVal
                    ENDIF
                ENDIF
                IF (PRESENT(Default)) THEN
                    IF (UCInputLine(Pos:Pos+7)== '\DEFAULT') THEN
                        ! WhichMinMax, MinMaxString not filled here
                        Default = TrueVal
                        CALL ProcessMinMaxDefLine(UCInputLine(Pos:), WhichMinMax, &
                                                  MinMaxString, Value)
                    ELSE
                        Default = FalseVal
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    RETURN

END SUBROUTINE ReadInputLine

!******************************************************************************

FUNCTION ProcessNumber(String,ErrorFlag) RESULT(RealNum)

    ! PURPOSE OF THIS FUNCTION:
    ! This function processes a string that should be numeric and
    ! returns the real value of the string.

    ! METHODOLOGY EMPLOYED:
    ! FUNCTION ProcessNumber translates the argument (a string)
    ! into a real number.  The string should consist of all
    ! numeric characters (except a decimal point).  Numerics
    ! with exponentiation (i.e. 1.2345E+03) are allowed but if
    ! it is not a valid number an error message along with the
    ! string causing the error is printed out and 0.0 is returned
    ! as the value.

    ! The Fortran input processor is used to make the conversion.

    ! REFERENCES:
    ! List directed Fortran input/output.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: String
    tLogical,  INTENT(OUT)  :: ErrorFlag
    tReal                   :: RealNum

    ! SUBROUTINE PARAMETER DEFINITIONS:
    tCharStar, PARAMETER    :: ValidNumerics = '0123456789.+-ED'//CHAR(9)

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tReal                   :: Temp
    tInteger                :: IoStatus
    tInteger                :: VerNumber
    tInteger                :: StringLen
    tCharLen(MaxNameLength) :: PString

    ! FLOW:
    
    RealNum = Zero
    !  Make sure the string has all what we think numerics should have
    PString = ADJUSTL(String)
    StringLen = LEN_TRIM(PString)
    ErrorFlag = FalseVal
    IF (StringLen == 0) RETURN
    VerNumber = VERIFY(PString(1:StringLen), ValidNumerics)
    IF (VerNumber == 0) THEN
        READ(PString,*,IOSTAT=IoStatus) Temp
        RealNum = Temp
        ErrorFlag = FalseVal
    ELSE
        RealNum = Zero
        ErrorFlag = TrueVal
    ENDIF
    IF (IoStatus /= 0) THEN
        RealNum = Zero
        ErrorFlag = TrueVal
    ENDIF

    RETURN

END FUNCTION ProcessNumber

!******************************************************************************

SUBROUTINE ProcessMinMaxDefLine(UCInputLine,WhichMinMax,MinMaxString,Value)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine processes the IDD lines that start with
    ! \minimum or \maximum and set up the parameters so that it can
    ! be automatically checked.

    ! REFERENCES:
    ! IDD Statements.
    !  \minimum         Minimum that includes the following value
    !  i.e. min >=
    !  \minimum>        Minimum that must be > than the following value
    !
    !  \maximum         Maximum that includes the following value
    !  i.e. max <=
    !  \maximum<        Maximum that must be < than the following value
    !

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: UCInputLine ! part of input line starting \min or \max
    tInteger,  INTENT(OUT)  :: WhichMinMax  ! = 0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
    tCharStar, INTENT(OUT)  :: MinMaxString
    tReal,     INTENT(OUT)  :: Value 

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: Pos
    tInteger    :: NSpace
    tLogical    :: ErrFlag
    
    ! FLOW:
    
    Pos = SCAN(UCInputLine,' ')
  
    SELECT CASE (UCInputLine(1:4))

    CASE('\MIN')
        WhichMinMax = 1
        IF (SCAN(UCInputLine,'>') /= 0) THEN
            Pos = SCAN(UCInputLine,'>') + 1
            WhichMinMax = 2
        ENDIF
        IF (WhichMinMax == 1) THEN
            MinMaxString = '> = '
        ELSE
            MinMaxString = '>'
        ENDIF

    CASE('\MAX')
        WhichMinMax=3
        IF (SCAN(UCInputLine,'<') /= 0) THEN
            POS = SCAN(UCInputLine,'<') + 1
            WhichMinMax = 4
        ENDIF
        IF (WhichMinMax == 3) THEN
            MinMaxString = '< = '
        ELSE
            MinMaxString = '<'
        ENDIF

    CASE('\DEF')
        WhichMinMax = 5
        MinMaxString = ' '

    CASE DEFAULT
        WhichMinMax = 0  ! invalid field
        MinMaxString = ' '
        Value = -999999.

    END SELECT

    IF (WhichMinMax /= 0) THEN
        NSpace = FindNonSpace(UCInputLine(Pos:))
        Pos = Pos + NSpace - 1
        NSpace = SCAN(UCInputLine(Pos:),' ')
        MinMaxString = TRIM(MinMaxString) // TRIM(UCInputLine(Pos:Pos+NSpace-1))
        Value = ProcessNumber(UCInputLine(Pos:Pos+NSpace-1), ErrFlag)
    ENDIF

    RETURN

END SUBROUTINE ProcessMinMaxDefLine

!******************************************************************************

FUNCTION FindIteminList(String,ListofItems,NumItems) RESULT(ItemNum)

    ! PURPOSE OF THIS FUNCTION:
    !^ This function looks up a string in a similar list of
    !  items and returns the index of the item in the list, if
    !  found.  This routine is not case insensitive and doesn't need
    !  for most inputs -- they are automatically turned to UPPERCASE.
    !  If you need case insensitivity use FindItem.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: String
    tCharStar, INTENT(IN)   :: ListofItems(:)
    tInteger,  INTENT(IN)   :: NumItems
    tInteger                :: ItemNum

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger Count
    
    ! FLOW:
    
    ItemNum = 0

    DO Count = 1, NumItems
        IF (String == ListofItems(Count)) THEN
            ItemNum = Count
            EXIT
        ENDIF
    END DO

    RETURN

END FUNCTION FindIteminList

!******************************************************************************

FUNCTION FindItem(String,ListofItems,NumItems) RESULT(ItemNum)

    ! PURPOSE OF THIS FUNCTION:
    !^ This function looks up a string in a similar list of
    !  items and returns the index of the item in the list, if
    !  found.  This routine is case insensitive -- it uses the
    !  SameString function to assure that both strings are in
    !  all upper case.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: String
    tCharStar, INTENT(IN)   :: ListofItems(:)
    tInteger,  INTENT(IN)   :: NumItems
    tInteger                :: ItemNum

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger Count
    
    ! FLOW:
    
    ItemNum = 0

    DO Count = 1, NumItems
        IF (SameString(String, ListofItems(Count))) THEN
            ItemNum = Count
            EXIT
        ENDIF
    END DO

    RETURN

END FUNCTION FindItem

!******************************************************************************

FUNCTION MakeUpperCase(InputString) RESULT(ResultString)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This function returns the Upper Case representation of the InputString.

    ! METHODOLOGY EMPLOYED:
    ! Uses the Intrinsic SCAN function to scan the lowercase representation of
    ! characters (DataGlobal) for each character in the given string.

    ! FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)           :: InputString  ! Input String
    tCharLen(MaxInputLineLength)    :: ResultString ! Result String, string is limited to
                                                    ! MaxInputLineLength because of PowerStation Compiler
                                                    ! otherwise could say (tCharLen(LEN(InputString))

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    tInteger    :: Count                ! Loop Counter
    tInteger    :: Pos                  ! Position in String representation
    tInteger    :: LengthInputString    ! Length (trimmed) of InputString
    
    ! FLOW:
    
    ResultString = ' '
    LengthInputString = LEN_TRIM(InputString)
    DO Count = 1, LengthInputString
        Pos = SCAN(LowerCase, InputString(Count:Count))
        IF (Pos /= 0) THEN
        ResultString(Count:Count) = UpperCase(Pos:Pos)
        ELSE
        ResultString(Count:Count) = InputString(Count:Count)
        ENDIF
    END DO
    ResultString = TRIM(ResultString)

    RETURN

END FUNCTION MakeUpperCase

!******************************************************************************

SUBROUTINE VerifyName(NameToVerify,NamesList,NumOfNames,ErrorFound,IsBlank,StringToDisplay)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine verifies that a new name can be added to the
    ! list of names for this item (i.e., that there isn't one of that
    ! name already and that this name is not blank).

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)       :: NameToVerify
    tCharStar, INTENT(IN)       :: NamesList(:)
    tInteger,  INTENT(IN)       :: NumOfNames
    tLogical,  INTENT(INOUT)    :: ErrorFound
    tLogical,  INTENT(OUT)      :: IsBlank
    tCharStar, INTENT(IN)       :: StringToDisplay

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: Found
    
    ! FLOW:
    
    IF (NumOfNames > 0) THEN
        Found = FindItemInList(NameToVerify, NamesList, NumOfNames)
        IF (Found /= 0) THEN
            ErrMsg = TRIM(StringToDisplay) // ', duplicate name = ' // TRIM(NameToVerify)
            CALL DisplaySevereError(ErrMsg, EchoInputFile)
            ErrorFound = TrueVal
        ENDIF
    ENDIF

    IF (NameToVerify == '     ') THEN
        ErrMsg = TRIM(StringToDisplay) // ', cannot be blank'
        CALL DisplaySevereError(ErrMsg, EchoInputFile)
        ErrorFound = TrueVal
        IsBlank = TrueVal
    ELSE
        IsBlank = FalseVal
    ENDIF

    RETURN

END SUBROUTINE VerifyName

!******************************************************************************

SUBROUTINE RangeCheck(ErrorsFound,WhatFieldString,WhatObjectString,ErrorLevel,  &
                      LowerBoundString,LowerBoundCond,UpperBoundString,UpperBoundCond)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine is a general purpose "range check" routine for GetInput routines.
    ! Using the standard "ErrorsFound" logical, this routine can produce a reasonable
    ! error message to describe the situation in addition to setting the ErrorsFound variable
    ! to true.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tLogical,  INTENT(OUT)          :: ErrorsFound      ! Set to true if error detected
    tCharStar, INTENT(IN)           :: WhatFieldString  ! Descriptive field for string
    tCharStar, INTENT(IN)           :: WhatObjectString ! Descriptive field for object, Zone Name, etc.
    tCharStar, INTENT(IN)           :: ErrorLevel       ! 'Warning','Severe','Fatal')
    tCharStar, INTENT(IN), OPTIONAL :: LowerBoundString ! String for error message, if applicable
    tLogical,  INTENT(IN), OPTIONAL :: LowerBoundCond   ! Condition for error condition, if applicable
    tCharStar, INTENT(IN), OPTIONAL :: UpperBoundString ! String for error message, if applicable
    tLogical,  INTENT(IN), OPTIONAL :: UpperBoundCond   ! Condition for error condition, if applicable

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(7)     :: ErrorString  ! Uppercase representation of ErrorLevel
    tLogical        :: Error
    tCharLen(120)   :: Message
    
    ! FLOW:
    
    Error = FalseVal
    IF (PRESENT(UpperBoundCond)) THEN
        IF (.NOT.UpperBoundCond) Error = TrueVal
    ENDIF
    IF (PRESENT(LowerBoundCond)) THEN
        IF (.NOT.LowerBoundCond) Error = TrueVal
    ENDIF

    IF (Error) THEN
        CALL ConvertCasetoUPPER(ErrorLevel, ErrorString)
        Message = 'Out of range value field = ' // TRIM(WhatFieldString) // ', range = '
        IF (PRESENT(LowerBoundString)) Message = TRIM(Message) // TRIM(LowerBoundString)
        IF (PRESENT(LowerBoundString) .AND. PRESENT(UpperBoundString)) THEN
            Message = TRIM(Message) // ' and ' // TRIM(UpperBoundString)
        ELSEIF (PRESENT(UpperBoundString)) THEN
            Message = TRIM(Message) // TRIM(UpperBoundString)
        ENDIF
        Message = TRIM(Message) // ', for item = ' // TRIM(WhatObjectString)

        SELECT CASE(ErrorString(1:1))
 
        CASE('W')
            CALL DisplayWarningError(TRIM(Message), EchoInputFile)

        CASE('S')
            CALL DisplaySevereError(TRIM(Message), EchoInputFile)
            ErrorsFound = TrueVal

        CASE('F')
            CALL DisplayFatalError(TRIM(Message), EchoInputFile)

        CASE DEFAULT
            CALL DisplaySevereError(TRIM(Message), EchoInputFile)
            ErrorsFound = TrueVal

        END SELECT

    ENDIF

    RETURN

END SUBROUTINE RangeCheck

!******************************************************************************

SUBROUTINE InternalRangeCheck(Value,FieldNumber,WhichObject,PossibleAlpha)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine is an internal range check that checks fields which have
    ! the \min and/or \max values set for appropriate values.

    ! METHODOLOGY EMPLOYED:
    ! Needs description, as appropriate.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tReal,     INTENT(IN)   :: Value
    tInteger,  INTENT(IN)   :: FieldNumber
    tInteger,  INTENT(IN)   :: WhichObject
    tCharStar, INTENT(IN)   :: PossibleAlpha

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical                        :: Error
    tCharLen(20)                    :: FieldString
    tCharLen(MaxObjectNameLength)   :: FieldNameString
    tCharLen(20)                    :: ValueString
    tCharLen(300)                   :: Message
    
    ! FLOW:
    
    Error = FalseVal
    IF (ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%WhichMinMax(1) == 1) THEN
        IF (Value < ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%MinMaxValue(1)) &
            Error = TrueVal
    ELSEIF (ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%WhichMinMax(1) == 2) THEN
        IF (Value <= ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%MinMaxValue(1)) &
            Error = TrueVal
    ENDIF
    IF (ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%WhichMinMax(2) == 3) THEN
        IF (Value > ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%MinMaxValue(2)) &
            Error = TrueVal
    ELSEIF (ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%WhichMinMax(2) == 4) THEN
        IF (Value >= ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%MinMaxValue(2)) &
            Error = TrueVal
    ENDIF

    IF (Error) THEN
        NumOutOfRangeErrorsFound = NumOutOfRangeErrorsFound + 1
        IF (ReportRangeCheckErrors) THEN
            WRITE(FieldString,*) FieldNumber
            FieldString = ADJUSTL(FieldString)
            FieldNameString = ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%FieldName
            WRITE(ValueString,*) Value
            ValueString = ADJUSTL(ValueString)
            IF (FieldNameString /= Blank) THEN
                Message = 'Out of range value numeric field#' // TRIM(FieldString) // &
                          '(' // TRIM(FieldNameString) // '), value = ' //            &
                          TRIM(ValueString) // ', range = '
            ELSE ! Field Name not recorded
                Message = 'Out of range value numeric field#' // TRIM(FieldString) // &
                          ', value = ' // TRIM(ValueString) // ', range = '
            ENDIF
            IF (ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%WhichMinMax(1) /= 0) &
                Message = TRIM(Message) // ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%MinMaxString(1)
            IF (ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%WhichMinMax(1) /= 0 .AND. &
                ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%WhichMinMax(2) /= 0) THEN
                Message = TRIM(Message) // ' and '//ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%MinMaxString(2)
            ELSEIF (ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%WhichMinMax(2) /= 0) THEN
                Message = TRIM(Message) // ObjectDef(WhichObject)%NumRangeChks(FieldNumber)%MinMaxString(2)
            ENDIF
            Message = TRIM(Message) // ', in ' // ObjectDef(WhichObject)%Name
            IF (ObjectDef(WhichObject)%AlphaorNumeric(1)) THEN
                Message = TRIM(Message) // ' = ' // PossibleAlpha
            ENDIF
            CALL DisplaySevereError(TRIM(Message), EchoInputFile)
        ENDIF
    ENDIF

    RETURN

END SUBROUTINE InternalRangeCheck

!******************************************************************************

SUBROUTINE TurnOnReportRangeCheckErrors

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine turns on the logical to report range check errors
    ! directly out of the InputProcessor.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
    
    ! FLOW:
    
    ReportRangeCheckErrors = TrueVal
  
    RETURN

END SUBROUTINE TurnOnReportRangeCheckErrors

!******************************************************************************

SUBROUTINE TurnOffReportRangeCheckErrors

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine turns off the logical to report range check errors
    ! directly out of the InputProcessor.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
    
    ! FLOW:
    
    ReportRangeCheckErrors = FalseVal
  
    RETURN

END SUBROUTINE TurnOffReportRangeCheckErrors

!******************************************************************************

FUNCTION GetNumRangeCheckErrorsFound() RESULT(NumErr)

    ! PURPOSE OF THIS FUNCTION:
    ! This function returns the number of OutOfRange errors found during
    ! input processing.

    ! FUNCTION ARGUMENT DEFINITIONS:
    tInteger    :: NumErr

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na
    
    ! FLOW:
    
    NumErr = NumOutOfRangeErrorsFound
  
    RETURN

END FUNCTION GetNumRangeCheckErrorsFound

!******************************************************************************

!==============================================================================
! The following routines allow access to the definition lines of the IDD and
! thus can be used to "report" on expected arguments for the Input Processor.

FUNCTION GetNumObjectsInIDD() RESULT(NumObj)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine returns the number of objects found in the IDD and
    ! can be used to allocate the array for determining the definitions.

    ! METHODOLOGY EMPLOYED:
    ! Essentially allows outside access to an internal variable of the InputProcessor.
    ! Used primarily by utility programs that use the InputProcessor outside of the
    ! "true" EnergyPlus code.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger    :: NumObj

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    ! FLOW:
    
    NumObj = NumObjectDefs

    RETURN

END FUNCTION GetNumObjectsInIDD

!******************************************************************************

SUBROUTINE GetListOfObjectsInIDD(ObjectNames,Number)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine returns the list of Object names that occur in the IDD.

    ! METHODOLOGY EMPLOYED:
    ! Essentially allows outside access to an internal variable of the InputProcessor.
    ! Used primarily by utility programs that use the InputProcessor outside of the
    ! "true" EnergyPlus code.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharLen(MaxObjectNameLength), INTENT(OUT)  :: ObjectNames(:)   ! List of Object Names (from IDD)
    tInteger,                      INTENT(OUT)  :: Number           ! Number in List

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
    
    ! FLOW:
    
    ObjectNames(1:NumObjectDefs) = ObjectDef(1:NumObjectDefs)%Name
    Number = NumObjectDefs
    
    RETURN

END SUBROUTINE GetListOfObjectsInIDD

!******************************************************************************

SUBROUTINE GetObjectDefInIDD(ObjectWord,NumArgs,AlphaorNumeric)

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine returns the "definition" of an Object from the IDD.  This is
    ! the "maximum" definition with total number of arguments, and whether each argument
    ! is "alpha" or "numeric".

    ! METHODOLOGY EMPLOYED:
    ! Essentially allows outside access to an internal variable of the InputProcessor.
    ! Used primarily by utility programs that use the InputProcessor outside of the
    ! "true" EnergyPlus code.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharLen(MaxObjectNameLength), INTENT(IN)   :: ObjectWord           ! Object for definition
    tInteger,                      INTENT(OUT)  :: NumArgs              ! How many arguments (max) this Object can have (returned)
    tLogical,                      INTENT(OUT)  :: AlphaorNumeric(:)    ! Array designating Alpha (true) or Numeric (false) for each

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: Which  ! to determine which object definition to use
    
    ! FLOW:
    
    Which = FindItemInList(ObjectWord, ObjectDef(1:NumObjectDefs)%Name, NumObjectDefs)
    NumArgs = ObjectDef(Which)%NumParams
    AlphaorNumeric(1:NumArgs) = ObjectDef(Which)%AlphaorNumeric(1:NumArgs)

    RETURN

END SUBROUTINE GetObjectDefInIDD

!******************************************************************************

FUNCTION FindNonSpace(String) RESULT(Pos)
          
    ! PURPOSE OF THIS FUNCTION:
    ! This function finds the first non-space character in the passed string
    ! and returns that position as the result to the calling program.

    ! METHODOLOGY EMPLOYED:
    ! Scan string for character not equal to blank.

    ! FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: String
    tInteger                :: Pos
                    
    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    tInteger    :: I, ILEN
    
    ! FLOW:
    
    Pos = 0
    ILEN = LEN_TRIM(String)
    DO I = 1, ILEN
        IF (String(I:I) .NE. ' ') THEN
            Pos = I
            EXIT
        END IF
    END DO

    RETURN

END FUNCTION FindNonSpace

!******************************************************************************

SUBROUTINE ConvertCasetoUpper(InputString,OutputString)
          
    ! PURPOSE OF THIS SUBROUTINE:
    ! Convert a string to upper case

    ! METHODOLOGY EMPLOYED:
    ! This routine is not dependent upon the ASCII
    ! code.  It works by storing the upper and lower case alphabet.  It
    ! scans the whole input string.  If it finds a character in the lower
    ! case alphabet, it makes an appropriate substitution.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: InputString
    tCharStar, INTENT(OUT)  :: OutputString
          
    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: A,B
    
    ! FLOW:
    
    DO A = 1, LEN_TRIM(InputString)
        B = INDEX(LowerCase, InputString(A:A))
        IF (B .NE. 0) THEN
            OutputString(A:A) = UpperCase(B:B)
        ELSE
            OutputString(A:A) = InputString(A:A)
        ENDIF
    END DO

    RETURN

END SUBROUTINE ConvertCasetoUpper

!******************************************************************************

END MODULE ModBase_LTKInput

!******************************************************************************
