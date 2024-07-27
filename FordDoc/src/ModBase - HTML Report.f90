
MODULE ModBase_HTML_Report

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various routines for reporting data to a HTML file. <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_IO_Handlers
    USE ModBase_Error_Handlers
    USE ModLib_Display,            ONLY: TOSTRING

    IMPLICIT NONE    ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: HTML_Report_Open_Exist
    PUBLIC  :: HTML_Report_Open_New
    PUBLIC  :: HTML_Report_Close
    PUBLIC  :: HTML_Report_Heading
    PUBLIC  :: HTML_Report_Paragraph
    PUBLIC  :: HTML_Report_List_Unordered
    PUBLIC  :: HTML_Report_List_Ordered
    PUBLIC  :: HTML_Report_List_Begin
    PUBLIC  :: HTML_Report_List_End
    PUBLIC  :: HTML_Report_List_Integer
    PUBLIC  :: HTML_Report_List_Real
    PUBLIC  :: HTML_Report_List_String
    PUBLIC  :: HTML_Report_Table_Begin
    PUBLIC  :: HTML_Report_Table_End
    PUBLIC  :: HTML_Report_Table_Row
    PUBLIC  :: HTML_Report_Table_RowInteger
    PUBLIC  :: HTML_Report_Table_RowReal

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Base_HTML_Report'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE HTML_Report_Open_New(FileName,Title,FileUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To open a new report file and write its title.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: FileName
    tCharStar, INTENT(IN)   :: Title
    tInteger,  INTENT(OUT)  :: FileUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! open new file
    FileUnit = OpenNewOutputFile(FileName)

    ! check if there is an error or not
    IF ( FileUnit == -1 ) THEN
        CALL Handle_ErrOpen('HTML_Report_Open', ModName, FileName, ErrSevere)
    END IF

    ! write title and style settings
    WRITE( FileUnit, '(A)' ) '<HTML>'
    WRITE( FileUnit, '(A)' ) '<HEAD>'
    WRITE( FileUnit, '(A)' ) '<TITLE>' // TRIM(Title) // '</TITLE>'
    WRITE( FileUnit, '(A)' ) '<STYLE>'
    WRITE( FileUnit, '(A)' ) 'BODY {Background-Color: PeachPuff;}'
    WRITE( FileUnit, '(A)' ) 'H1 {Color: Indigo; Text-Align: Center;}'
    WRITE( FileUnit, '(A)' ) 'H2, H3, H4, H5, H6 {Color: MidnightBlue;}'
    WRITE( FileUnit, '(A)' ) 'SPAN.REAL {Color: MediumBlue;}'
    WRITE( FileUnit, '(A)' ) 'SPAN.INTEGER {Color: Blue;}'
    WRITE( FileUnit, '(A)' ) 'SPAN.STRING {Color: Crimson;}'
    WRITE( FileUnit, '(A)' ) 'TABLE, TH, TD {Border: 2px Solid Black;}'
    WRITE( FileUnit, '(A)' ) 'TABLE {Width: 100%;}'
!    WRITE( FileUnit, '(A)' ) 'TH, TD {Padding: 15px;}'
    WRITE( FileUnit, '(A)' ) 'TH {Background-color: ForestGreen; Color: White;}'
    WRITE( FileUnit, '(A)' ) 'TD {Text-Align: Center;}'
    WRITE( FileUnit, '(A)' ) 'TR:NTH-CHILD(ODD) {Background-Color: GhostWhite;}'
    WRITE( FileUnit, '(A)' ) 'TR:NTH-CHILD(EVEN) {Background-Color: Ivory;}'
    WRITE( FileUnit, '(A)' ) '</STYLE>'
    WRITE( FileUnit, '(A)' ) '</HEAD>'
    WRITE( FileUnit, '(A)' ) '<BODY>'

    RETURN

END SUBROUTINE HTML_Report_Open_New

!******************************************************************************

SUBROUTINE HTML_Report_Open_Exist(FileUnit,FileName,Title,Replace)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To open an existing report file (and write its title).

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,            INTENT(INOUT)  :: FileUnit
    tCharStar,           INTENT(IN)     :: FileName
    tCharStar, OPTIONAL, INTENT(IN)     :: Title
    tLogical,  OPTIONAL, INTENT(IN)     :: Replace

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical    :: Append
    tLogical    :: FileExists

!** FLOW

    ! check input
    Append = TrueVal
    IF (PRESENT(Replace)) THEN
        IF (Replace) Append = FalseVal
    END IF
    FileExists = DoesFileExist(FileName)

    ! open file
    FileUnit = OpenOutputFile(FileName,Append)

    ! check if there is an error or not
    IF ( FileUnit == -1 ) THEN
        CALL Handle_ErrOpen('HTML_Report_Open', ModName, FileName, ErrSevere)
    END IF

    IF (((.NOT.FileExists).OR.(.NOT.Append)).AND.PRESENT(Title)) THEN
        ! write title and style settings
        WRITE( FileUnit, '(A)' ) '<HTML>'
        WRITE( FileUnit, '(A)' ) '<HEAD>'
        WRITE( FileUnit, '(A)' ) '<TITLE>' // TRIM(Title) // '</TITLE>'
        WRITE( FileUnit, '(A)' ) '<STYLE>'
        WRITE( FileUnit, '(A)' ) 'BODY {Background-Color: PeachPuff;}'
        WRITE( FileUnit, '(A)' ) 'H1 {Color: Indigo; Text-Align: Center;}'
        WRITE( FileUnit, '(A)' ) 'H2, H3, H4, H5, H6 {Color: MidnightBlue;}'
        WRITE( FileUnit, '(A)' ) 'SPAN.REAL {Color: MediumBlue;}'
        WRITE( FileUnit, '(A)' ) 'SPAN.INTEGER {Color: Blue;}'
        WRITE( FileUnit, '(A)' ) 'SPAN.STRING {Color: Crimson;}'
        WRITE( FileUnit, '(A)' ) 'TABLE, TH, TD {Border: 2px Solid Black;}'
        WRITE( FileUnit, '(A)' ) 'TABLE {Width: 100%;}'
        !WRITE( FileUnit, '(A)' ) 'TH, TD {Padding: 15px;}'
        WRITE( FileUnit, '(A)' ) 'TH {Background-color: ForestGreen; Color: White;}'
        WRITE( FileUnit, '(A)' ) 'TD {Text-Align: Center;}'
        WRITE( FileUnit, '(A)' ) 'TR:NTH-CHILD(ODD) {Background-Color: GhostWhite;}'
        WRITE( FileUnit, '(A)' ) 'TR:NTH-CHILD(EVEN) {Background-Color: Ivory;}'
        WRITE( FileUnit, '(A)' ) '</STYLE>'
        WRITE( FileUnit, '(A)' ) '</HEAD>'
        WRITE( FileUnit, '(A)' ) '<BODY>'
    END IF

    RETURN

END SUBROUTINE HTML_Report_Open_Exist

!******************************************************************************

SUBROUTINE HTML_Report_Close(FileUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To properly close the report file.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger, INTENT(IN)    :: FileUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! write closing parts
    WRITE( FileUnit, '(A)' ) '</BODY>'
    WRITE( FileUnit, '(A)' ) '</HTML>'

    ! open file
    CLOSE( FileUnit )

    RETURN

END SUBROUTINE HTML_Report_Close

!******************************************************************************

SUBROUTINE HTML_Report_Heading(FileUnit,Level,Title)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a heading based on the specified level.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,  INTENT(IN)   :: FileUnit
    tInteger,  INTENT(IN)   :: Level
    tCharStar, INTENT(IN)   :: Title

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Level)
    CASE (1)
        WRITE( FileUnit, '(A)' ) '<H1>' // TRIM(Title) // '</H1>'
    CASE (2)
        WRITE( FileUnit, '(A)' ) '<H2>' // TRIM(Title) // '</H2>'
    CASE (3)
        WRITE( FileUnit, '(A)' ) '<H3>' // TRIM(Title) // '</H3>'
    CASE (4)
        WRITE( FileUnit, '(A)' ) '<H4>' // TRIM(Title) // '</H4>'
    CASE (5)
        WRITE( FileUnit, '(A)' ) '<H5>' // TRIM(Title) // '</H5>'
    CASE (6)
        WRITE( FileUnit, '(A)' ) '<H6>' // TRIM(Title) // '</H6>'
    CASE DEFAULT
        WRITE( FileUnit, '(A)' ) '<H1>' // TRIM(Title) // '</H1>'
    END SELECT

    RETURN

END SUBROUTINE HTML_Report_Heading

!******************************************************************************

SUBROUTINE HTML_Report_Paragraph(FileUnit,TextArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a paragraph based on the given text array.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,  INTENT(IN)   :: FileUnit
    tCharStar, INTENT(IN)   :: TextArray(:)

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: I

!** FLOW

    ! write the start of a paragraph
    WRITE( FileUnit, '(A)' ) '<P>'

    ! write the body of a paragraph
    WRITE( FileUnit, '(A)' ) ( TRIM(TextArray(I)) ,I=1,SIZE(TextArray) )

    ! write the end of a paragraph
    WRITE( FileUnit, '(A)' ) '</P>'

    RETURN

END SUBROUTINE HTML_Report_Paragraph

!******************************************************************************

SUBROUTINE HTML_Report_List_Unordered(FileUnit,ListArray,ListStyle)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a list based on the given list array.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,           INTENT(IN)  :: FileUnit
    tCharStar,          INTENT(IN)  :: ListArray(:)
    tInteger, OPTIONAL, INTENT(IN)  :: ListStyle    ! optional list style type
                                                    ! = 1 for disc (bullet)
                                                    ! = 2 for circle
                                                    ! = 3 for square

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: I

!** FLOW

    ! write the start of a list
    IF (PRESENT(ListStyle)) THEN
        SELECT CASE (ListStyle)
        CASE (1)
            WRITE( FileUnit, '(A)' ) '<UL STYLE="List-Style-Type:Disc;">'
        CASE (2)
            WRITE( FileUnit, '(A)' ) '<UL STYLE="List-Style-Type:Circle;">'
        CASE (3)
            WRITE( FileUnit, '(A)' ) '<UL STYLE="List-Style-Type:Square;">'
        CASE DEFAULT
            WRITE( FileUnit, '(A)' ) '<UL STYLE="List-Style-Type:Disc;">'
        END SELECT
    ELSE
        ! set default list
        WRITE( FileUnit, '(A)' ) '<UL STYLE="List-Style-Type:Disc;">'
    END IF

    ! write the given list array
    WRITE( FileUnit, '(A)' ) ( '<LI>' // TRIM(ListArray(I)) // '</LI>' ,I=1,SIZE(ListArray) )

    ! write the end of a list
    WRITE( FileUnit, '(A)' ) '</UL>'

    RETURN

END SUBROUTINE HTML_Report_List_Unordered

!******************************************************************************

SUBROUTINE HTML_Report_List_Ordered(FileUnit,ListArray,ListStyle)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a list based on the given list array.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,           INTENT(IN)  :: FileUnit
    tCharStar,          INTENT(IN)  :: ListArray(:)
    tInteger, OPTIONAL, INTENT(IN)  :: ListStyle    ! optional list style type
                                                    ! = 1 for numbers
                                                    ! = 2 for upper-case letters
                                                    ! = 3 for lower-case letters
                                                    ! = 4 for upper-case roman numbers
                                                    ! = 5 for lower-case roman numbers

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: I

!** FLOW

    ! write the start of a list
    IF (PRESENT(ListStyle)) THEN
        SELECT CASE (ListStyle)
        CASE (1)
            WRITE( FileUnit, '(A)' ) '<OL TYPE="1">'
        CASE (2)
            WRITE( FileUnit, '(A)' ) '<OL TYPE="A">'
        CASE (3)
            WRITE( FileUnit, '(A)' ) '<OL TYPE="a">'
        CASE (4)
            WRITE( FileUnit, '(A)' ) '<OL TYPE="I">'
        CASE (5)
            WRITE( FileUnit, '(A)' ) '<OL TYPE="i">'
        CASE DEFAULT
            WRITE( FileUnit, '(A)' ) '<OL TYPE="1">'
        END SELECT
    ELSE
        ! set default list
        WRITE( FileUnit, '(A)' ) '<OL TYPE="1">'
    END IF

    ! write the given list array
    WRITE( FileUnit, '(A)' ) ( '<LI>' // TRIM(ListArray(I)) // '</LI>' ,I=1,SIZE(ListArray) )

    ! write the end of a list
    WRITE( FileUnit, '(A)' ) '</OL>'

    RETURN

END SUBROUTINE HTML_Report_List_Ordered

!******************************************************************************

SUBROUTINE HTML_Report_List_Begin(FileUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write the start of an unordered list that will usually be followed
    !  by calling HTML_Report_List_Real, HTML_Report_List_Integer,
    !  HTML_Report_List_String, and/or HTML_Report_List_End routines.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger, INTENT(IN)    :: FileUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! write the start of a list
    WRITE( FileUnit, '(A)' ) '<UL>'

    RETURN

END SUBROUTINE HTML_Report_List_Begin

!******************************************************************************

SUBROUTINE HTML_Report_List_End(FileUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write the end of an unordered list that will usually be called
    !  after calling HTML_Report_List_Begin, HTML_Report_List_Real,
    !  HTML_Report_List_Integer and/or HTML_Report_List_String routines
    !  first.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger, INTENT(IN)    :: FileUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! write the end of a list
    WRITE( FileUnit, '(A)' ) '</UL>'

    RETURN

END SUBROUTINE HTML_Report_List_End

!******************************************************************************

SUBROUTINE HTML_Report_List_Real(FileUnit,Text1st,Number,Text2nd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a list based on the given text(s) and a real number.
    !  The routine assumes that the given number is between the given
    !  two texts or after the first text if the second text is missing.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,            INTENT(IN) :: FileUnit
    tCharStar,           INTENT(IN) :: Text1st
    tReal,               INTENT(IN) :: Number
    tCharStar, OPTIONAL, INTENT(IN) :: Text2nd

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! write the start of a list
    IF (PRESENT(Text2nd)) THEN
        WRITE( FileUnit, '(A, 1PE12.5, A)' ) '<LI>' // TRIM(Text1st) // &
                         '<SPAN CLASS="REAL">', Number, TRIM(Text2nd) // '</SPAN></LI>'
    ELSE
        ! write the given list array
        WRITE( FileUnit, '(A, 1PE12.5, A)' ) '<LI>' // TRIM(Text1st) // &
                         '<SPAN CLASS="REAL">', Number, '</SPAN></LI>'
    END IF

    RETURN

END SUBROUTINE HTML_Report_List_Real

!******************************************************************************

SUBROUTINE HTML_Report_List_Integer(FileUnit,Text1st,Number,Text2nd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a list based on the given text(s) and an integer number
    !  The routine assumes that the given number is between the given
    !  two texts or after the first text if the second text is missing.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,            INTENT(IN) :: FileUnit
    tCharStar,           INTENT(IN) :: Text1st
    tInteger,            INTENT(IN) :: Number
    tCharStar, OPTIONAL, INTENT(IN) :: Text2nd

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! write the start of a list
    IF (PRESENT(Text2nd)) THEN
        WRITE( FileUnit, '(A, I12, A)' ) '<LI>' // TRIM(Text1st) // &
                         '<SPAN CLASS="INTEGER">', Number, TRIM(Text2nd) // '</SPAN></LI>'
    ELSE
        ! write the given list array
        WRITE( FileUnit, '(A, I12, A)' ) '<LI>' // TRIM(Text1st) // &
                         '<SPAN CLASS="INTEGER">', Number, '</SPAN></LI>'
    END IF

    RETURN

END SUBROUTINE HTML_Report_List_Integer

!******************************************************************************

SUBROUTINE HTML_Report_List_String(FileUnit,Text1st,String,Text2nd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a list based on the given text(s) and a string.
    !  The routine assumes that the given string is between the given
    !  two texts or after the first text if the second text is missing. <br>
    !  Note: the given string will be emphasized by giving different color
    !        from the other text(s).

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,            INTENT(IN) :: FileUnit
    tCharStar,           INTENT(IN) :: Text1st
    tCharStar,           INTENT(IN) :: String
    tCharStar, OPTIONAL, INTENT(IN) :: Text2nd

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! write the start of a list
    IF (PRESENT(Text2nd)) THEN
        WRITE( FileUnit, '(A)' ) '<LI>' // TRIM(Text1st) // '<SPAN CLASS="STRING">' &
                         // TRIM(String) // TRIM(Text2nd) // '</SPAN></LI>'
    ELSE
        ! write the given list array
        WRITE( FileUnit, '(A)' ) '<LI>' // TRIM(Text1st) // '<SPAN CLASS="STRING">' &
                         // TRIM(String) // '</SPAN></LI>'
    END IF

    RETURN

END SUBROUTINE HTML_Report_List_String

!******************************************************************************

SUBROUTINE HTML_Report_Table_Begin(FileUnit,Header,FirstCell,Title)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write the start of a table that will usually be followed
    !  by calling HTML_Report_Table_Row and HTML_Report_Table_End routines.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,            INTENT(IN) :: FileUnit
    tCharStar,           INTENT(IN) :: Header(:)
    tCharStar, OPTIONAL, INTENT(IN) :: FirstCell
    tCharStar, OPTIONAL, INTENT(IN) :: Title

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: I

!** FLOW

    ! write the start of a responsive table
    WRITE( FileUnit, '(A)' ) '<DIV STYLE="Overflow-X:Auto;">'
    WRITE( FileUnit, '(A)' ) '<TABLE>'
    IF (PRESENT(Title)) THEN
        WRITE( FileUnit, '(A)' ) '<CAPTION>' // TRIM(Title) // '</CAPTION>'
    END IF
    WRITE( FileUnit, '(A)' ) '<TR>'
    IF (PRESENT(FirstCell)) THEN
        WRITE( FileUnit, '(A)' ) '<TH>' // TRIM(FirstCell) // '</TH>'
    END IF
    WRITE( FileUnit, '(A)' ) ( '<TH>' // TRIM(Header(I)) // '</TH>', I = 1,SIZE(Header) )
    WRITE( FileUnit, '(A)' ) '</TR>'

    RETURN

END SUBROUTINE HTML_Report_Table_Begin

!******************************************************************************

SUBROUTINE HTML_Report_Table_End(FileUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write the start of a table that will usually be called
    !  after calling HTML_Report_Table_Begin and HTML_Report_Table_Row routines.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger, INTENT(IN)    :: FileUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! write the end of the table
    WRITE( FileUnit, '(A)' ) '</TABLE>'
    WRITE( FileUnit, '(A)' ) '</DIV>'

    RETURN

END SUBROUTINE HTML_Report_Table_End

!******************************************************************************

SUBROUTINE HTML_Report_Table_Row(FileUnit,RowCell)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write the rows of a table.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,  INTENT(IN)   :: FileUnit
    tCharStar, INTENT(IN)   :: RowCell(:)

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: I

!** FLOW

    ! write the start of a responsive table
    WRITE( FileUnit, '(A)' ) '<TR>'
    WRITE( FileUnit, '(A)' ) ( '<TD>' // TRIM(RowCell(I)) // '</TD>', I = 1,SIZE(RowCell) )
    WRITE( FileUnit, '(A)' ) '</TR>'

    RETURN

END SUBROUTINE HTML_Report_Table_Row

!******************************************************************************

SUBROUTINE HTML_Report_Table_RowReal(FileUnit,RowCell,FirstCell)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write the rows of a table with real data. <br>
    !  Note: Except for the first cell, all cell data are assumed to be
    !       real number.  If cell data will have both real and integer
    !       numbers, then they should be converted to string first and use
    !       HTML_Report_Table_Row instead of this routine.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,            INTENT(IN) :: FileUnit
    tReal,               INTENT(IN) :: RowCell(:)
    tCharStar, OPTIONAL, INTENT(IN) :: FirstCell

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: I

!** FLOW

    ! write the start of a responsive table
    WRITE( FileUnit, '(A)' ) '<TR>'
    IF (PRESENT(FirstCell)) THEN
        WRITE( FileUnit, '(A)' ) '<TD>' // TRIM(FirstCell) // '</TD>'
    END IF
    WRITE( FileUnit, '(A)' ) ( '<TD>' // TRIM(TOSTRING(RowCell(I),FMT='G25.16')) // '</TD>', I = 1,SIZE(RowCell) )
    WRITE( FileUnit, '(A)' ) '</TR>'

    RETURN

END SUBROUTINE HTML_Report_Table_RowReal

!******************************************************************************

SUBROUTINE HTML_Report_Table_RowInteger(FileUnit,RowCell,FirstCell)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write the rows of a table with integer data. <br>
    !  Note: Except for the first cell, all cell data are assumed to be
    !       integer number.  If cell data will have both real and integer
    !       numbers, then they should be converted to string first and use
    !       HTML_Report_Table_Row instead of this routine.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tInteger,            INTENT(IN) :: FileUnit
    tInteger,            INTENT(IN) :: RowCell(:)
    tCharStar, OPTIONAL, INTENT(IN) :: FirstCell

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: I

!** FLOW

    ! write the start of a responsive table
    WRITE( FileUnit, '(A)' ) '<TR>'
    IF (PRESENT(FirstCell)) THEN
        WRITE( FileUnit, '(A)' ) '<TD>' // TRIM(FirstCell) // '</TD>'
    END IF
    WRITE( FileUnit, '(A)' ) ( '<TD>' // TRIM(TOSTRING(RowCell(I))) // '</TD>', I = 1,SIZE(RowCell) )
    WRITE( FileUnit, '(A)' ) '</TR>'

    RETURN

END SUBROUTINE HTML_Report_Table_RowInteger

!******************************************************************************

END MODULE ModBase_HTML_Report

!******************************************************************************
