
SUBMODULE (ModLib_FENIA) SubLib_HypertextMarkup

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule contains routines that perform ...

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
!
MODULE SUBROUTINE HTML_Initialize(OUTPUT_FILE)

! HTML_Initialize is called to initialize an output HTML file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!	the ".html" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine HTML_Initialize(output_file)
!implicit none
!character,intent(in):: output_file*(*)
!end subroutine HTML_Initialize
!end interface

!call HTML_Initialize(output_file='output.html')
!call HTML_Finalize(output_file='output.html')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)

! Private variables:
INTEGER:: ERR,IO
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (CONNECTED) THEN
WRITE(*,*)"HTML_Initialize"
WRITE(*,*)"WARNING: HTML file has already been accessed."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<6) THEN
WRITE(*,*)"HTML_Initialize"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-4:LEN_TRIM(OUTPUT_FILE))/='.html') THEN
WRITE(*,*)"HTML_Initialize"
WRITE(*,*)"ERROR: file must have the .html extension"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A6)',IOSTAT=ERR)"<html>"

IF (ERR/=0) THEN
WRITE(*,*)"HTML_Initialize"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,'(A6)')"<head>"
WRITE(IO,'(A23)')"<style type='text/css'>"
WRITE(IO,'(A6)')"body {"
WRITE(IO,'(A34)')"margin-left: 3%; margin-right: 3%;"
WRITE(IO,'(A34)')"margin-top: 3%; margin-bottom: 3%;"
WRITE(IO,'(A21)')"font-family: verdana;"
WRITE(IO,'(A1)')"}"
WRITE(IO,'(A8)')"</style>"
WRITE(IO,'(A7)')"</head>"
WRITE(IO,'(A6)')"<body>"
WRITE(IO,*)

END SUBROUTINE HTML_Initialize

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE HTML_Heading(OUTPUT_FILE,STRING,LEVEL)

! HTML_Heading is called to write a heading in an output HTML file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output HTML file. It must have
!	the ".html" extension (otherwise an error is generated).
! string: character, scalar. The string of the heading.
! level: integer, scalar. The level of the heading. There are 6 supported
!	levels with level 1 being the most "important" and level 6 the less
!	"important".

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine HTML_Heading(output_file,string,level)
!implicit none
!character,intent(in):: output_file*(*),string*(*)
!integer,intent(in):: level
!end subroutine HTML_Heading
!end interface

!call HTML_Initialize(output_file='output.html')
!call HTML_Heading(output_file='output.html',string='Hello World',level=1)
!call HTML_Finalize(output_file='output.html')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),STRING*(*)
INTEGER,INTENT(IN):: LEVEL

! Private variables:
INTEGER:: IO,ERR
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"HTML_Heading"
WRITE(*,*)"WARNING: uninitialized HTML file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<6) THEN
WRITE(*,*)"HTML_Heading"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-4:LEN_TRIM(OUTPUT_FILE))/='.html') THEN
WRITE(*,*)"HTML_Heading"
WRITE(*,*)"ERROR: file must have the .html extension"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((LEVEL<1).OR.(LEVEL>6)) THEN
WRITE(*,*)"HTML_Heading"
WRITE(*,*)"ERROR: unsuported heading level"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (LEN_TRIM(STRING)==0) THEN
WRITE(*,*)"HTML_Heading"
WRITE(*,*)"ERROR: missing heading string"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A2,I1,A1)',IOSTAT=ERR)"<h",LEVEL,">"

IF (ERR/=0) THEN
WRITE(*,*)"HTML_Heading"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! Write the heading string:
WRITE(IO,*)TRIM(STRING)

! Note: some compilers may break a very long character string in multiple lines
! when printing it in a file. To avoid such behaviour, use the "advance=no"
! option in the write statement as follows. The code given will force the string
! to remain in one line.
!do i=1,len_trim(string)-1
!	write(io,'(A1)',advance='no')string(i:i)
!end do
!write(io,'(A1)')string(len_trim(string):len_trim(string))

WRITE(IO,'(A3,I1,A1)')"</h",LEVEL,">"
WRITE(IO,*)

END SUBROUTINE HTML_Heading

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE HTML_Link(OUTPUT_FILE,TEXT,LINK_STRING)

! HTML_Link is called to create a link in an HTML file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!	the ".html" extension (otherwise an error is generated).
! text: character, scalar. The text that will be attributed the link.
! link_string: character, scalar. The string specifying the link. It can be e.g.
!	a web adress, a local file, etc. The validity of the link is not being
!	checked by the present subroutine.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine HTML_Link(output_file,text,link_string)
!implicit none
!character,intent(in):: output_file*(*),text*(*),link_string*(*)
!end subroutine HTML_Link
!end interface

!call HTML_Link(output_file='output.html',text='click here', &
!	& link_string='plot.svg')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),TEXT*(*),LINK_STRING*(*)

! Private variables:
INTEGER:: ERR,IO
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"HTML_Link"
WRITE(*,*)"WARNING: uninitialized HTML file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<6) THEN
WRITE(*,*)"HTML_Link"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-4:LEN_TRIM(OUTPUT_FILE))/='.html') THEN
WRITE(*,*)"HTML_Link"
WRITE(*,*)"ERROR: file must have the .html extension"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((LEN_TRIM(LINK_STRING)==0).OR.(LEN_TRIM(TEXT)==0)) THEN
WRITE(*,*)"HTML_Link"
WRITE(*,*)"ERROR: missing link data"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A9)',IOSTAT=ERR)"<a href='"

IF (ERR/=0) THEN
WRITE(*,*)"HTML_Link"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)TRIM(LINK_STRING)
WRITE(IO,'(A2)')"'>"
WRITE(IO,*)TRIM(TEXT)
WRITE(IO,'(A4)')"</a>"
WRITE(IO,*)

END SUBROUTINE HTML_Link

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE HTML_Paragraph(OUTPUT_FILE,TEXT)

! HTML_Paragraph is called to write a paragraph of a given text in an output
! HTML file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output HTML file. It must have
!	the ".html" extension (otherwise an error is generated).
! text: character, scalar. The text of the paragraph.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine HTML_Paragraph(output_file,text)
!implicit none
!character,intent(in):: output_file*(*),text*(*)
!end subroutine HTML_Paragraph
!end interface

!call HTML_Initialize(output_file='output.html')
!call HTML_Paragraph(output_file='output.html',text='Hello World')
!call HTML_Finalize(output_file='output.html')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),TEXT*(*)

! Private variables:
INTEGER:: IO,ERR
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"HTML_Paragraph"
WRITE(*,*)"WARNING: uninitialized HTML file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<6) THEN
WRITE(*,*)"HTML_Paragraph"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-4:LEN_TRIM(OUTPUT_FILE))/='.html') THEN
WRITE(*,*)"HTML_Paragraph"
WRITE(*,*)"ERROR: file must have the .html extension"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (LEN_TRIM(TEXT)==0) THEN
WRITE(*,*)"HTML_Paragraph"
WRITE(*,*)"ERROR: missing heading text"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A3)',IOSTAT=ERR)"<p>"

IF (ERR/=0) THEN
WRITE(*,*)"HTML_Paragraph"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! Write the paragraph text:
WRITE(IO,*)TRIM(TEXT)

! Note: some compilers may break a very long character text in multiple lines
! when printing it in a file. To avoid such behaviour, use the "advance=no"
! option in the write statement as follows. The code given will force the text
! to remain in one line.
!do i=1,len_trim(text)-1
!	write(io,'(A1)',advance='no')text(i:i)
!end do
!write(io,'(A1)')text(len_trim(text):len_trim(text))

WRITE(IO,'(A4)')"</p>"
WRITE(IO,*)

END SUBROUTINE HTML_Paragraph

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE HTML_Table(OUTPUT_FILE,NUMBERS,HEADINGS,DECIMAL_PLACES,SCIENTIFIC)

! HTML_Table is called to create a two-dimensional table in an HTML file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".html" extension (otherwise an error is generated).
! numbers: real, array (2D). Contains the table's numerical data.
! headings: character, array (1D). Contains the headings of the table. The array
!    must have as many elements as the amount of columns of input variable
!    array "numbers".
! decimal_places: integer, array (1D). The amount of decimal places that the
!    formatted form of every column is required to have. The array must
!    have as many elements as the amount of columns of input variable array
!    "numbers".
! scientific: logical, array (1D). Each element specifies whether the scientific
!    notation will be used for the respective table column. The array must
!    have as many elements as the amount of columns of input variable array
!    "numbers".

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine HTML_Table(output_file,numbers,headings,decimal_places,scientific)
!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),headings(:)*(*)
!real(srk),intent(in):: numbers(:,:)
!integer,intent(in):: decimal_places(:)
!logical,intent(in):: scientific(:)
!end subroutine HTML_Table
!end interface

!real(srk):: x(30),y(30),numbers(30,2)

!x(1)=0.0 ; y(1)=0.000 ; x(11)=1.0 ; y(11)=0.841 ; x(21)=2.0 ; y(21)=0.909
!x(2)=0.1 ; y(2)=0.100 ; x(12)=1.1 ; y(12)=0.891 ; x(22)=2.1 ; y(22)=0.863
!x(3)=0.2 ; y(3)=0.199 ; x(13)=1.2 ; y(13)=0.932 ; x(23)=2.2 ; y(23)=0.808
!x(4)=0.3 ; y(4)=0.296 ; x(14)=1.3 ; y(14)=0.964 ; x(24)=2.3 ; y(24)=0.746
!x(5)=0.4 ; y(5)=0.389 ; x(15)=1.4 ; y(15)=0.985 ; x(25)=2.4 ; y(25)=0.675
!x(6)=0.5 ; y(6)=0.479 ; x(16)=1.5 ; y(16)=0.997 ; x(26)=2.5 ; y(26)=0.598
!x(7)=0.6 ; y(7)=0.565 ; x(17)=1.6 ; y(17)=1.000 ; x(27)=2.6 ; y(27)=0.516
!x(8)=0.7 ; y(8)=0.644 ; x(18)=1.7 ; y(18)=0.992 ; x(28)=2.7 ; y(28)=0.427
!x(9)=0.8 ; y(9)=0.717 ; x(19)=1.8 ; y(19)=0.974 ; x(29)=2.8 ; y(29)=0.335
!x(10)=0.9 ; y(10)=0.783 ; x(20)=1.9 ; y(20)=0.946 ; x(30)=2.9 ; y(30)=0.239

!numbers(:,1)=x
!numbers(:,2)=y

!call HTML_Initialize(output_file='output.html')
!call HTML_Table(output_file='output.html',numbers=numbers, &
!    & headings=(/'x-axis data','y-axis data'/),decimal_places=(/1,3/), &
!    & scientific=(/.false.,.false./))
!call HTML_Finalize(output_file='output.html')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),HEADINGS(:)*(*)
REAL(SRK),INTENT(IN):: NUMBERS(:,:)
INTEGER,INTENT(IN):: DECIMAL_PLACES(:)
LOGICAL,INTENT(IN):: SCIENTIFIC(:)

! Private variables:
INTEGER:: I,J,IO,ERR
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"HTML_Table"
WRITE(*,*)"WARNING: uninitialized HTML file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<6) THEN
WRITE(*,*)"HTML_Table"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-4:LEN_TRIM(OUTPUT_FILE))/='.html') THEN
WRITE(*,*)"HTML_Table"
WRITE(*,*)"ERROR: file must have the .html extension"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ALL(NUMBERS==0)) THEN
WRITE(*,*)"HTML_Table"
WRITE(*,*)"WARNING: all table numbers are equal to zero"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (ALL(HEADINGS=='')) THEN
WRITE(*,*)"HTML_Table"
WRITE(*,*)"WARNING: missing table headings"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((SIZE(NUMBERS,DIM=2)/=SIZE(HEADINGS)).OR. &
    & (SIZE(NUMBERS,DIM=2)/=SIZE(DECIMAL_PLACES)).OR. &
    & (SIZE(NUMBERS,DIM=2)/=SIZE(SCIENTIFIC))) THEN
WRITE(*,*)"HTML_Table"
WRITE(*,*)"ERROR: unmatching table contents and miscellaneous table information"
WRITE(*,*)"size(numbers,dim=2)",SIZE(NUMBERS,DIM=2)
WRITE(*,*)"size(headings)",SIZE(HEADINGS)
WRITE(*,*)"size(decimal_places)",SIZE(DECIMAL_PLACES)
WRITE(*,*)"size(scientific)",SIZE(SCIENTIFIC)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A50)',IOSTAT=ERR)"<table border='0' cellspacing='0' cellpadding='5'>"

IF (ERR/=0) THEN
WRITE(*,*)"HTML_Table"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,'(A19)')"<tr align='center'>"
DO I=1,SIZE(HEADINGS)
    WRITE(IO,*)"<th>"//TRIM(HEADINGS(I))//"</th>"
END DO
WRITE(IO,'(A5)')"</tr>"

DO I=1,SIZE(NUMBERS,DIM=1)
    WRITE(IO,'(A19)')"<tr align='center'>"
    DO J=1,SIZE(NUMBERS,DIM=2)
        WRITE(IO,*)"<td>"// &
            & TRIM(FormatReal(NUMBERS(I,J),DECIMAL_PLACES(J), &
            & SCIENTIFIC(J)))//"</td>"
    END DO
    WRITE(IO,'(A5)')"</tr>"
END DO

WRITE(IO,'(A8)')"</table>"
WRITE(IO,*)

END SUBROUTINE HTML_Table

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE HTML_Finalize(OUTPUT_FILE)

! HTML_Finalize is called to finalize an output HTML file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!	the ".html" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine HTML_Finalize(output_file)
!implicit none
!character,intent(in):: output_file*(*)
!end subroutine HTML_Finalize
!end interface

!call HTML_Initialize(output_file='output.html')
!call HTML_Finalize(output_file='output.html')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! External subprograms:
INTERFACE

FUNCTION FileManager(FILENAME,CONNECTED)
IMPLICIT NONE
CHARACTER,INTENT(IN):: FILENAME*(*)
LOGICAL,OPTIONAL,INTENT(OUT):: CONNECTED
INTEGER:: FileManager
END FUNCTION FileManager

END INTERFACE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)

! Private variables:
INTEGER:: ERR,IO
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"HTML_Finalize"
WRITE(*,*)"WARNING: uninitialized HTML file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<6) THEN
WRITE(*,*)"HTML_Finalize"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-4:LEN_TRIM(OUTPUT_FILE))/='.html') THEN
WRITE(*,*)"HTML_Finalize"
WRITE(*,*)"ERROR: file must have the .html extension"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A10)',IOSTAT=ERR)"<!--EOF-->"

IF (ERR/=0) THEN
WRITE(*,*)"HTML_Finalize"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,'(A7)')"</body>"
WRITE(IO,'(A7)')"</html>"

CLOSE(IO)

END SUBROUTINE HTML_Finalize

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


END SUBMODULE SubLib_HypertextMarkup
