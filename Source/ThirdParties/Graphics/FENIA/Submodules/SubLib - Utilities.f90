
SUBMODULE (ModLib_FENIA) SubLib_Utilities

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
MODULE SUBROUTINE AxesSetup(LOWER,UPPER, &
    & STARTS,STOPS,INTERVAL,DECIMAL_PLACES,SCIENTIFIC)

! AxesSetup provides information for the pretty-printing of a series of numbers.
! The numbers are intended to be used for the labeling of an axis of a cartesian
! plot diagram.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! lower: real, scalar. The ending value of the axis.
! upper: real, scalar. The initial value of the axis.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! interval: real, scalar. The interval between the ticks of the axis.
! decimal_places: integer, scalar. The amount of decimal places of the numbers
!    on the axis.
! scientific: logical, scalar. Specifies whether the scientific notation will
!    be used for the numbers on the axis.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!subroutine AxesSetup(lower,upper, &
!    & starts,stops,interval,decimal_places,scientific)
!!use Config, only: srk
!real(srk),intent(in):: lower,upper
!real(srk),intent(out):: starts,stops,interval
!integer,intent(out):: decimal_places
!logical,intent(out):: scientific
!end subroutine AxesSetup
!end interface

!real(srk):: x_start,x_end,y_start,y_end,x_tick_interval,y_tick_interval
!integer:: x_decimal_places,y_decimal_places
!logical:: scientific_x,scientific_y

!real(srk):: x(30),y(30)

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

!call AxesSetup(minval(x),maxval(x), &
!    & x_start,x_end,x_tick_interval,x_decimal_places,scientific_x)

!call AxesSetup(minval(y),maxval(y), &
!    & y_start,y_end,y_tick_interval,y_decimal_places,scientific_y)

!call SVG_CartesianPlot( &
!output_file='plot.svg', &
!x_points=x, &
!y_points=y, &
!x_title='x_axis', &
!y_title='y_axis', &
!x_start=x_start, &
!x_end=x_end, &
!y_start=y_start, &
!y_end=y_end, &
!x_decimal_places=x_decimal_places, &
!y_decimal_places=y_decimal_places, &
!scientific_x=scientific_x, &
!scientific_y=scientific_x, &
!x_tick_interval=x_tick_interval, &
!y_tick_interval=y_tick_interval, &
!draw_ticks=.false., &
!tick_length=10.0_srk, &
!tick_width=3.0_srk, &
!diagram_title='cartesian_diagram', &
!data_title='data_series', &
!title_diagram_font_size=40.0_srk, &
!title_axis_font_size=30.0_srk, &
!numbers_axis_font_size=25.0_srk, &
!font_rgb=(/0,0,0/), &
!axis_stroke_rgb=(/0,0,0/), &
!axis_stroke_width=3.0_srk, &
!draw_grid=.true., &
!grid_stroke_width=2.0_srk, &
!grid_stroke_rgb=(/127,127,127/), &
!shape_type='square', &
!shape_size=10.0_srk, &
!shape_stroke_width=1.0_srk, &
!shape_fill_rgb=(/255,0,0/), &
!shape_stroke_rgb=(/255,0,0/), &
!line_type='crooked', &
!line_stroke_width=2.0_srk, &
!line_stroke_rgb=(/0,0,255/) &
!)

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,default_string_length

IMPLICIT NONE

! Argument variables:
REAL(SRK),INTENT(IN):: LOWER,UPPER
REAL(SRK),INTENT(OUT):: STARTS,STOPS,INTERVAL
INTEGER,INTENT(OUT):: DECIMAL_PLACES
LOGICAL,INTENT(OUT):: SCIENTIFIC

! Private variables:
INTEGER,PARAMETER:: TICK_MIN=2,TICK_MAX=10
REAL(SRK):: INTERVAL_CANDIDATES(9),GAP,ORDER,AUX
REAL(SRK),ALLOCATABLE:: NUMBERS(:)
INTEGER:: TICK_AMOUNT(9),I,J,K
CHARACTER*(DEFAULT_STRING_LENGTH):: CHAR_STARTS,CHAR_STOPS
CHARACTER*(DEFAULT_STRING_LENGTH),ALLOCATABLE:: CHAR_NUMBERS(:)

! ------------------------------------------------------------------------------

! Error control:

IF (LOWER>=UPPER) THEN
WRITE(*,*)"AxesSetup"
WRITE(*,*)"ERROR: lower limit may not be greater or equal to upper limit."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

! The numbers are prioritized empirically:
INTERVAL_CANDIDATES=(/2.0_SRK,1.0_SRK,5.0_SRK,3.0_SRK,4.0_SRK,6.0_SRK,8.0_SRK, &
    & 10.0_SRK,0.1_SRK/)
GAP=UPPER-LOWER

! Get the order of magnitude of the distance to be covered:
IF (GAP>=1.0) THEN
    ORDER=0.1_SRK ! initialization
    DO
        ORDER=ORDER*10.0_SRK
        GAP=GAP/10.0_SRK
        IF (GAP<10.0) EXIT
    END DO
ELSE
    ORDER=1.0_SRK ! initialization
    DO
        ORDER=ORDER/10.0_SRK
        GAP=GAP*10.0_SRK
        IF (GAP>10.0) EXIT
    END DO
END IF

GAP=UPPER-LOWER
INTERVAL_CANDIDATES=INTERVAL_CANDIDATES*ORDER
TICK_AMOUNT(:)=NINT(GAP/INTERVAL_CANDIDATES(:))

J=9 ! Initialization. This value will apply if the loop below gives no result.
! Find the first suitable tick amount:
DO I=1,SIZE(TICK_AMOUNT)
    IF ((TICK_MIN<=TICK_AMOUNT(I)).AND.(TICK_AMOUNT(I)<=TICK_MAX)) THEN
        J=I
        EXIT
    END IF
END DO

! Store the final interval:
INTERVAL=INTERVAL_CANDIDATES(J)

! Get an initial guess of the start value:
STARTS=NINT(LOWER-INTERVAL/2.0_SRK)

! Count up to the end value:
STOPS=STARTS ! Initialization
DO
    STOPS=STOPS+INTERVAL
    IF (STOPS>=UPPER) EXIT
END DO

! Count down to the start value:
STARTS=STOPS ! Initialization
DO
    STARTS=STARTS-INTERVAL
    IF (STARTS<=LOWER) EXIT
END DO

! Favor the zero as the start or the end of the axes:
IF ((STARTS<0.0).AND.(0.0<=LOWER)) STARTS=0.0_SRK
IF ((UPPER<=0.0).AND.(0.0<STOPS)) STOPS=0.0_SRK

! Calculate the final actual amount of numbers needed:
K=1 ! Initialization
AUX=STARTS
DO
    K=K+1
    AUX=AUX+INTERVAL
    IF (AUX>=STOPS) EXIT
END DO

! Distribute the numbers in an array:
ALLOCATE(NUMBERS(K))
ALLOCATE(CHAR_NUMBERS(K))
NUMBERS(1)=STARTS
DO I=2,SIZE(NUMBERS)
    NUMBERS(I)=NUMBERS(I-1)+INTERVAL
END DO

! Get the decimal digit amount of the interval:
IF (ORDER>=1.0) THEN
    K=0 ! No decimals are necessary if the interval is greater than unity.
ELSE
    K=0 ! Initialization
    DO
        K=K+1
        ORDER=ORDER*10.0_SRK
        IF (NINT(ORDER)>=1) EXIT
    END DO
END IF

! ------------------------------------------------------------------------------

! From the FORTRAN specification:

! Real constants are produced with the effect of either an F edit descriptor or
! an E edit descriptor, depending on the magnitude x of the value and a range
! 10^d1 <= x <= 10^d2 , where d1 and d2 are processor-dependent integers. If the
! magnitude x is within this range or is zero, the constant is produced using
! 0PFw.d; otherwise, 1PEw.dEe is used.

! The Fw.d edit descriptor indicates that the field occupies w positions, the
! fractional part of which consists of d digits. When w is zero, the processor
! selects the field width. An exponent containing a D is processed identically
! to an exponent containing an E. Both E and D are permitted in output.

! Transform the numbers into strings without format:
CHAR_STARTS=StringReal(STARTS)
CHAR_STOPS=StringReal(STOPS)

! See if the processor decided to print the numbers in scientific format. Ignore
! the scientific notation for zeros.
SCIENTIFIC=.FALSE.
IF (STARTS/=0.0) THEN
    DO I=1,DEFAULT_STRING_LENGTH
        IF (ANY((/'E','e','D','d'/)==CHAR_STARTS(I:I))) THEN
            SCIENTIFIC=.TRUE.
            EXIT
        END IF
    END DO
END IF
IF (STOPS/=0.0) THEN
    DO I=1,DEFAULT_STRING_LENGTH
        IF (ANY((/'E','e','D','d'/)==CHAR_STOPS(I:I))) THEN
            SCIENTIFIC=.TRUE.
            EXIT
        END IF
    END DO
END IF

! Transform all the numbers to stings according to the aquired information:
SCAN_DECIMALS: DO I=0,9!k
    DO J=1,SIZE(NUMBERS)
        CHAR_NUMBERS(J)=TRIM(FormatReal(NUMBERS(J),I,SCIENTIFIC))
    END DO
    DO J=1,SIZE(NUMBERS)-1
        IF ((J>1).AND.(ANY(CHAR_NUMBERS(J)==CHAR_NUMBERS(1:J-1)))) &
            & CYCLE SCAN_DECIMALS
        IF (ANY(CHAR_NUMBERS(J)==CHAR_NUMBERS(J+1:SIZE(NUMBERS)))) &
            & CYCLE SCAN_DECIMALS
    END DO
! These lines will be executed only when loop "scan_decimals" is not "cycled":
    DECIMAL_PLACES=I
    EXIT SCAN_DECIMALS
END DO SCAN_DECIMALS

END SUBROUTINE AxesSetup

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION FileManager(FILENAME,CONNECTED)

! FileManager manages the file connections in a program and enables the
! reference of the I/O files by name. The function can be used exactly as the
! index number of an I/O file in an I/O command. FileManager and the OPEN or the
! CLOSE statements can be used together in a program without conflicts.

! SVG note:
! All I/O of the SVG library is streamed through the present function. To add
! extensions such as user interactivity upon file creation or overwriting, only
! the present function needs to be modified.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! filename: character, scalar. The I/O file's name.

! INPUT (OPTIONAL):
! connected: logical, scalar. Defines whether the file with the given filename
!    has already been connected or not.

! OUTPUT (REQUIRED):
! FileManager: integer, scalar. The index number of the given filename.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function FileManager(filename,connected)
!implicit none
!character,intent(in):: filename*(*)
!logical,optional,intent(out):: connected
!integer:: FileManager
!end function FileManager
!end interface

!write(FileManager('test.txt'),'(F5.3)')1.234
!rewind(FileManager('test.txt'))
!read(FileManager('test.txt'),*)a

! On some compilers the line:
!write(FileManager('test.txt'),*)'test.txt',FileManager('test.txt')
! may produce a "recursive I/O operation" error.

! ------------------------------------------------------------------------------

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: FILENAME*(*)
LOGICAL,OPTIONAL,INTENT(OUT):: CONNECTED
INTEGER:: FileManager

! Private variables:
INTEGER,SAVE:: COUNTER=0
LOGICAL:: NUMBER_TAKEN
INTEGER:: ERR

! ------------------------------------------------------------------------------

! Error control:

IF (LEN_TRIM(FILENAME)==0) THEN
WRITE(*,*)"FileManager"
WRITE(*,*)"ERROR: blank filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

! Find out if the file is connected:
INQUIRE(FILE=FILENAME,OPENED=NUMBER_TAKEN,IOSTAT=ERR)

IF (ERR/=0) THEN
WRITE(*,*)"FileManager"
WRITE(*,*)"ERROR: INQUIRE statement failed."
WRITE(*,*)"filename : ",TRIM(FILENAME)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT(CONNECTED)) CONNECTED=NUMBER_TAKEN

! If the file is not connected, find an empty slot and open it there. If the
! file is already connected, obtain the connection number.
ALREADY_CONNECTED: IF (NUMBER_TAKEN) THEN

! Get the connection number:
    INQUIRE(FILE=FILENAME,NUMBER=FileManager,IOSTAT=ERR)

    IF (ERR/=0) THEN
    WRITE(*,*)"FileManager"
    WRITE(*,*)"ERROR: INQUIRE statement failed."
    WRITE(*,*)"filename : ",TRIM(FILENAME)
    WRITE(*,*)"error flag : ",ERR
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

ELSE ALREADY_CONNECTED

    SEARCH_EMPTY_SLOT: DO

        COUNTER=COUNTER+1
        INQUIRE(UNIT=COUNTER,OPENED=NUMBER_TAKEN,IOSTAT=ERR)

        IF (ERR/=0) THEN
        WRITE(*,*)"FileManager"
        WRITE(*,*)"ERROR: INQUIRE statement failed."
        WRITE(*,*)"connection unit : ",COUNTER
        WRITE(*,*)"error flag : ",ERR
        WRITE(*,*)"Program terminated."
        READ(*,*)
        STOP
        END IF

        SLOT_FOUND: IF (.NOT.NUMBER_TAKEN) THEN

! If an empty slot is found, the requested file is opened.
            OPEN(COUNTER,FILE=FILENAME,IOSTAT=ERR)

            IF (ERR/=0) THEN
            WRITE(*,*)"FileManager"
            WRITE(*,*)"ERROR: file could not be opened."
            WRITE(*,*)"filename : ",TRIM(FILENAME)
            WRITE(*,*)"error flag : ",ERR
            WRITE(*,*)"Program terminated."
            READ(*,*)
            STOP
            END IF

            FileManager=COUNTER
            EXIT SEARCH_EMPTY_SLOT

        END IF SLOT_FOUND

    END DO SEARCH_EMPTY_SLOT

END IF ALREADY_CONNECTED

END FUNCTION FileManager

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION FormatReal(NUMERICAL_VALUE,DECIMAL_PLACES,SCIENTIFIC)

! FormatReal converts a given numerical value into an alphanumerical string
! using an explicitly defined format.

! SVG note:
! This function is NOT intended to be used for the printing of SVG object
! variables in the SVG file (use function StringReal for that purpose). The
! present function is intended for the pretty-printing of numerical values as
! SVG Text objects.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! numerical_value: real, scalar. The number that will be converted into a
!    string using the specified format. The user is responsible for
!    specifying the most appropriate format for the given number.
! decimal_places: integer, scalar. The amount of decimal places that the
!    formatted form of the given number is required to have.
! scientific: logical, scalar. Explicitly specifies whether the scientific
!    notation will be used for formating the given number or not.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! FormatReal: character, scalar. The formatted numerical value as a character
!    variable.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function FormatReal(numerical_value,decimal_places,scientific)
!!use Config, only: srk,default_string_length
!implicit none
!real(srk),intent(in):: numerical_value
!integer,intent(in):: decimal_places
!logical,intent(in):: scientific
!character:: FormatReal*(default_string_length)
!end function FormatReal
!end interface

!real:: numerical_value

!numerical_value=123456.145
!write(*,*)numerical_value
!write(*,*)trim(FormatReal(numerical_value,2,.false.))
!write(*,*)trim(FormatReal(numerical_value,2,.true.))

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,default_string_length

IMPLICIT NONE

! Argument variables:
REAL(SRK),INTENT(IN):: NUMERICAL_VALUE
INTEGER,INTENT(IN):: DECIMAL_PLACES
LOGICAL,INTENT(IN):: SCIENTIFIC
CHARACTER:: FormatReal*(DEFAULT_STRING_LENGTH)

! Private variables:
CHARACTER:: FORMAT_STRING*(DEFAULT_STRING_LENGTH)
INTEGER:: ERR

! ------------------------------------------------------------------------------

! Error control:

IF (NUMERICAL_VALUE/=NUMERICAL_VALUE) THEN
WRITE(*,*)"FormatReal"
WRITE(*,*)"ERROR: input is NaN."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (DEFAULT_STRING_LENGTH<DECIMAL_PLACES+7) THEN
WRITE(*,*)"FormatReal"
WRITE(*,*)"ERROR: the specified decimal places amount is incompatible to the"
WRITE(*,*)"value of global variable 'default_string_length'"
WRITE(*,*)"default_string_length: ",DEFAULT_STRING_LENGTH
WRITE(*,*)"decimal_places: ",DECIMAL_PLACES
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

IF (SCIENTIFIC) THEN
    FORMAT_STRING='(ES'//TRIM(StringInteger(DEFAULT_STRING_LENGTH))// &
        & '.'//TRIM(StringInteger(DECIMAL_PLACES))//')'
ELSE
    FORMAT_STRING='(F'//TRIM(StringInteger(DEFAULT_STRING_LENGTH))// &
        & '.'//TRIM(StringInteger(DECIMAL_PLACES))//')'
END IF

FormatReal=''
WRITE(FormatReal,FORMAT_STRING,IOSTAT=ERR)NUMERICAL_VALUE

IF (ERR/=0) THEN
WRITE(*,*)"FormatReal"
WRITE(*,*)"ERROR: unable to convert number into string."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! Delete the trailing decimal point if it is requested that the given number is
! printed as an integer:
IF ((DECIMAL_PLACES==0).AND.(.NOT.SCIENTIFIC)) FormatReal &
    (LEN_TRIM(FormatReal):LEN_TRIM(FormatReal))=''

FormatReal=ADJUSTL(FormatReal)

END FUNCTION FormatReal

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION NumberReal(STRING)

! NumberReal converts a given string with numerical characters into a number.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! string: character. The given character string.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! number: real. The number that is read from the given string.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function NumberReal(string)
!!use Config, only: srk
!implicit none
!character,intent(in):: string*(*)
!real(srk):: NumberReal
!end function NumberReal
!end interface

!write(*,*) NumberReal('17.34E-1')
!write(*,*) int(NumberReal('17.34E-1'))
!write(*,*) nint(NumberReal('17.34E-1'))
!write(*,*) NumberReal('17.34E-1 0.238')
!write(*,*) NumberReal('should_give_error')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: STRING*(*)
REAL(SRK):: NumberReal

! Private variables:
INTEGER:: ERR

! ------------------------------------------------------------------------------

! Error control:

IF (LEN_TRIM(STRING)==0) THEN
WRITE(*,*)"NumberReal"
WRITE(*,*)"ERROR: blank input string"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

READ(STRING,*,IOSTAT=ERR)NumberReal

IF (ERR/=0) THEN
WRITE(*,*)"NumberReal"
WRITE(*,*)"ERROR: unable to convert string into number."
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! This subprogram enables the transformation of a string into a numerical value
! in a function, thus saving lines and variables in the calling code. The
! returned number is a floating point number. If an integer is needed, the
! returned floating point number should be converted using the Fortran intrinsic
! functions INT() or NINT().

END FUNCTION NumberReal

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION StringInteger(NUMERICAL_VALUE)

! StringInteger converts a given number into an alphanumerical string.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! numerical_value: integer, scalar. The number that will be converted in a
!	string.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! StringInteger: character, scalar. The resulting character string.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function StringInteger(numerical_value)
!!use Config, only: default_string_length
!implicit none
!integer,intent(in):: numerical_value
!character:: StringInteger*(default_string_length)
!end function StringInteger
!end interface

!write(*,*)String(127)

! The length of the output StringInteger could have been empiricaly set equal to
! the amount of significant digits in the base of the numbering system used on
! the computer for the same type of integer variable as the input argument
! (using intrisic function DIGITS). This length is found to be enough for the
! representation of a number of the given precision. However, setting the string
! length manually is viewed as more portable.

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: default_string_length

IMPLICIT NONE

! Argument variables:
INTEGER,INTENT(IN):: NUMERICAL_VALUE
CHARACTER:: StringInteger*(DEFAULT_STRING_LENGTH)

! Private variables:
INTEGER:: ERR

! ------------------------------------------------------------------------------

! Error control:

IF (NUMERICAL_VALUE/=NUMERICAL_VALUE) THEN
WRITE(*,*)"StringInteger"
WRITE(*,*)"ERROR: input is NaN."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

StringInteger=''
WRITE(StringInteger,*,IOSTAT=ERR)NUMERICAL_VALUE

IF (ERR/=0) THEN
WRITE(*,*)"StringInteger"
WRITE(*,*)"ERROR: unable to convert number into string."
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

StringInteger=ADJUSTL(StringInteger)

! This subprogram enables the transformation of a numerical value into a string
! inside a function rather than in a subroutine, thus saving lines and variables
! in the calling code.

END FUNCTION StringInteger

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION StringReal(NUMERICAL_VALUE)

! StringReal converts a given number into an alphanumerical string.

! SVG note:
! Some subsets of the SVG specification (e.g. the SVG Tiny 1.2 specification)
! specify certain limitations for the floating point numbers in the SVG file.
! In order to avoid truncation errors in the calculations, the accuracy of the
! floating points used should be high, but in order to comply to certain
! specifications the numbers should be manipulated before being printed in the
! SVG file (bound checking and truncating). This manipulation should be done in
! this subroutine, since it is used for the output of all floating point numbers
! in the SVG library.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! numerical_value: real, scalar. The number that will be converted in a string.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! StringReal: character, scalar. The resulting character string.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function StringReal(numerical_value)
!!use Config, only: srk,default_string_length
!implicit none
!real(srk),intent(in):: numerical_value
!character:: StringReal*(default_string_length)
!end function StringReal
!end interface

!write(*,*)digits(1.0_srk),precision(1.0_srk),range(1.0_srk)
!write(*,*)'1234567890123456789012345678901234567890'
!write(*,*)StringReal(9.12345959458498594594859678123_srk)
!write(*,'(F30.20)')9.12345959458498594594859678123_srk

! The length of the output StringInteger could have been empiricaly set equal to
! the amount of significant digits in the base of the numbering system used on
! the computer for the same type of integer variable as the input argument
! (using intrisic function DIGITS). This length is found to be enough for the
! representation of a number of the given precision. However, setting the string
! length manually is viewed as more portable.

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,default_string_length

IMPLICIT NONE

! Argument variables:
REAL(SRK),INTENT(IN):: NUMERICAL_VALUE
CHARACTER:: StringReal*(DEFAULT_STRING_LENGTH)

! Private variables:
INTEGER:: ERR

! ------------------------------------------------------------------------------

! Error control:

IF (NUMERICAL_VALUE/=NUMERICAL_VALUE) THEN
WRITE(*,*)"StringReal"
WRITE(*,*)"ERROR: input is NaN."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

StringReal=''
WRITE(StringReal,*,IOSTAT=ERR)NUMERICAL_VALUE

IF (ERR/=0) THEN
WRITE(*,*)"StringReal"
WRITE(*,*)"ERROR: unable to convert number into string."
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

StringReal=ADJUSTL(StringReal)
IF (StringReal(LEN_TRIM(StringReal):LEN_TRIM(StringReal))=='.') &
    & StringReal=TRIM(StringReal)//'0'

! This subprogram enables the transformation of a numerical value into a string
! inside a function rather than in a subroutine, thus saving lines and variables
! in the calling code.

END FUNCTION StringReal

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


END SUBMODULE SubLib_Utilities
