
SUBMODULE (ModLib_FENIA) SubLib_ScalableVectorGraphics

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
MODULE SUBROUTINE SVG_ActionButton(OUTPUT_FILE,SCRIPT,TARGET_ID,X,Y,WIDTH,HEIGHT, &
    & STROKE_WIDTH,BUTTON_FILL_OPACITY,STROKE_RGB,BUTTON_FILL_RGB, &
    & STRING,FONT,FONT_SIZE,TEXT_FILL_OPACITY,TEXT_FILL_RGB)

! SVG_ActionButton is called to place an ActionButton in an SVG file. The
! respective ECMAscript function must be present in the SVG file

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! script: character, scalar. Specifies the script that defines the action that
!    will be taken upon the onclick event of the button.
! target_id: character, scalar. The id of the object that will be affected by
!    the predefined action.
! x: real, scalar. The x-axis coordinate of the button center. (px).
! y: real, scalar. The y-axis coordinate of the button center. (px).
! width: real, scalar. The button width. (px).
! height: real, scalar. he button height. (px).
! stroke_width: real, scalar. The thickness of the button line (px).
! button_fill_opacity: real, scalar. The opacity of the button fill
!    (dimensionless, 0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! button_fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB
!    channel values (dimensionless, 0<=fill_rgb(:)<=255).
! string: character, scalar. The string that will be drawn.
! font: character, scalar. The font that will be used for the rendering. The
!    selected font must be supported in the system that will be used to
!    render the SVG file.
! font_size: real, scalar. The size of the font.
! text_fill_opacity: real, scalar. The opacity of the text fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! text_fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB
!    channel values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_ActionButton(output_file,script,target_id,x,y,width,height, &
!    & stroke_width,button_fill_opacity,stroke_rgb,button_fill_rgb, &
!    & string,font,font_size,text_fill_opacity,text_fill_rgb)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),script*(*),string*(*),font*(*)
!character,intent(in):: target_id*(*)
!real(srk),intent(in):: x,y,width,height,stroke_width
!real(srk),intent(in):: button_fill_opacity,text_fill_opacity,font_size
!integer,intent(in):: stroke_rgb(3),button_fill_rgb(3),text_fill_rgb(3)
!end subroutine SVG_ActionButton
!end interface

!character*(default_string_length):: id='1'
!call SVG_Initialize(output_file='plot.svg')
!call SVG_Vanish(output_file='plot.svg')
!call SVG_ActionButton(output_file='plot.svg',script='vanish',target_id=id, &
!    & x=200.0_srk,y=500.0_srk,width=200.0_srk,height=50.0_srk, &
!    & stroke_width=5.0_srk,button_fill_opacity=1.0_srk, &
!    & stroke_rgb=(/0,0,255/),button_fill_rgb=(/255,255,255/), &
!    & string='hide circle',font='verdana',font_size=20.0_srk, &
!    & text_fill_opacity=1.0_srk,text_fill_rgb=(/0,0,0/))
!call SVG_Circle(output_file='plot.svg', &
!    & x=200.0_srk,y=100.0_srk,radius=50.0_srk, &
!    & stroke_width=10.0_srk,fill_opacity=1.0_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/255,255,255/), &
!    & rotate=(/0.0_srk,100.0_srk,100.0_srk/),id=id)
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),SCRIPT*(*),STRING*(*),FONT*(*)
CHARACTER,INTENT(IN):: TARGET_ID*(*)
REAL(SRK),INTENT(IN):: X,Y,WIDTH,HEIGHT,STROKE_WIDTH
REAL(SRK),INTENT(IN):: BUTTON_FILL_OPACITY,TEXT_FILL_OPACITY,FONT_SIZE
INTEGER,INTENT(IN):: STROKE_RGB(3),BUTTON_FILL_RGB(3),TEXT_FILL_RGB(3)

! Private variables:
INTEGER:: ERR,IO
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

!Edit this IF block in order to support more scripts.
IF (ALL(TRIM(SCRIPT)/=(/'vanish'/))) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"ERROR: unsupported script."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((Y>VIEWBOX_HEIGHT).OR.(X>VIEWBOX_WIDTH)) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (ANY((/WIDTH,HEIGHT/)<0.0)) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"ERROR: invalid shape parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/WIDTH,HEIGHT/)==0.0)) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"WARNING: SVG object will not be viewable."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<BUTTON_FILL_OPACITY).OR.(BUTTON_FILL_OPACITY<0.0).OR. &
    & (1.0<TEXT_FILL_OPACITY).OR.(TEXT_FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"ERROR: invalid paint parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((BUTTON_FILL_OPACITY==0.0).AND.(TEXT_FILL_OPACITY==0.0).AND. &
    & (STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(TEXT_FILL_RGB<0)).OR.(ANY(BUTTON_FILL_RGB<0)).OR. &
    & (ANY(STROKE_RGB<0)).OR.(ANY(STROKE_RGB>255)).OR. &
    & (ANY(TEXT_FILL_RGB>255)).OR.(ANY(BUTTON_FILL_RGB>255))) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

! Group the rounded rectangle and its text.
WRITE(IO,'(A2)',IOSTAT=ERR)"<g"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_ActionButton"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! Refer to the ECMAscript function.
! Edit this SELECT CASE block in order to support more scripts.
SELECT CASE (SCRIPT)
    CASE("vanish")
        WRITE(IO,*)"onclick='vanish(evt,"//'"'// &
            & TRIM(TARGET_ID)//'"'//")'"
END SELECT

WRITE(IO,'(A1)')">"
WRITE(IO,*)

! Print the rounded rectangle.
WRITE(IO,'(A5)',IOSTAT=ERR)"<rect"
WRITE(IO,*)"x='"//TRIM(StringReal(X-WIDTH/2.0_SRK))//"'"
WRITE(IO,*)"y='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y-HEIGHT/2.0_SRK))//"'"
WRITE(IO,*)"rx='"//TRIM(StringReal(HEIGHT/5.0_SRK))//"'"
WRITE(IO,*)"ry='"//TRIM(StringReal(HEIGHT/5.0_SRK))//"'"
WRITE(IO,*)"width='"//TRIM(StringReal(WIDTH))//"'"
WRITE(IO,*)"height='"//TRIM(StringReal(HEIGHT))//"'"
WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(BUTTON_FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(BUTTON_FILL_RGB(1)))//","// &
        & TRIM(StringInteger(BUTTON_FILL_RGB(2)))//","// &
        & TRIM(StringInteger(BUTTON_FILL_RGB(3)))//")'"
WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

! Print the button text.
WRITE(IO,'(A5)')"<text"
WRITE(IO,*)"x='"//TRIM(StringReal(X))//"'"
WRITE(IO,*)"y='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y+FONT_SIZE/3.0_SRK))//"'"
WRITE(IO,*)"font-family='"//TRIM(FONT)//"'"
WRITE(IO,*)"font-size='"//TRIM(StringReal(FONT_SIZE))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(TEXT_FILL_OPACITY))//"'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(TEXT_FILL_RGB(1)))//","// &
        & TRIM(StringInteger(TEXT_FILL_RGB(2)))//","// &
        & TRIM(StringInteger(TEXT_FILL_RGB(3)))//")'"
WRITE(IO,*)"text-anchor='middle'"
WRITE(IO,*)"font-weight='normal'"
!write(io,*)"font-style='italic'"
WRITE(IO,'(A1)')">"
WRITE(IO,*)TRIM(STRING)
WRITE(IO,'(A7)')"</text>"
WRITE(IO,*)

! End the grouping
WRITE(IO,'(A4)',IOSTAT=ERR)"</g>"
WRITE(IO,*)

END SUBROUTINE SVG_ActionButton

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_BezierQuadratic(OUTPUT_FILE,X,Y, &
    & STROKE_WIDTH,FILL_OPACITY,STROKE_RGB,FILL_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_BezierQuadratic is called to draw a quadratic Bezier curve in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, array (1D). The x-axis coordinates of the bezier curve control
!    points. (px).
! y: real, array (1D). The y-axis coordinates of the bezier curve control
!    points. (px).
! stroke_width: real, scalar. The thickness of the bezier curve (px).
! fill_opacity: real, scalar. The opacity of the fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_BezierQuadratic(output_file,x,y, &
!    & stroke_width,fill_opacity,stroke_rgb,fill_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x(:),y(:),stroke_width,fill_opacity
!integer,intent(in):: stroke_rgb(3),fill_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_BezierQuadratic
!end interface

!real(srk):: x(15),y(15)
!x(1)=50.0    ; y(1)=375.0
!x(2)=150.0   ; y(2)=375.0
!x(3)=150.0   ; y(3)=425.0
!x(4)=250.0   ; y(4)=425.0
!x(5)=250.0   ; y(5)=375.0
!x(6)=350.0   ; y(6)=375.0
!x(7)=350.0   ; y(7)=500.0
!x(8)=450.0   ; y(8)=500.0
!x(9)=450.0   ; y(9)=375.0
!x(10)=550.0  ; y(10)=375.0
!x(11)=550.0  ; y(11)=575.0
!x(12)=650.0  ; y(12)=575.0
!x(13)=650.0  ; y(13)=375.0
!x(14)=750.0  ; y(14)=375.0
!x(15)=750.0  ; y(15)=650.0
!call SVG_Initialize(output_file='plot.svg')
!call SVG_BezierQuadratic(output_file='plot.svg',x=x(1:15),y=y(1:15)-330.0, &
!    & stroke_width=5.0_srk,fill_opacity=1.0_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/255,255,255/), &
!    & rotate=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/))
!call SVG_BezierQuadratic(output_file='plot.svg',x=x(1:14),y=y(1:14)+100.0, &
!    & stroke_width=5.0_srk,fill_opacity=1.0_srk, &
!    & stroke_rgb=(/0,255,0/),fill_rgb=(/255,255,255/), &
!    & rotate=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X(:),Y(:),STROKE_WIDTH,FILL_OPACITY
INTEGER,INTENT(IN):: STROKE_RGB(3),FILL_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"ERROR: file must have the .svg extension"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"WARNING: negative coordinates"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(Y>VIEWBOX_HEIGHT)).OR.(ANY(X>VIEWBOX_WIDTH))) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (SIZE(X)/=SIZE(Y)) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"ERROR: coordinate arrays do not match"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"ERROR: invalid stroke parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<FILL_OPACITY).OR.(FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"ERROR: invalid paint parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FILL_OPACITY==0.0).AND.(STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(FILL_RGB<0)).OR.(ANY(STROKE_RGB<0)).OR. &
    & (ANY(FILL_RGB>255)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"ERROR: invalid RGB channel value"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIZE(X)<2) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"WARNING: can not plot a line with less than two points"
WRITE(*,*)"No line will be plotted."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ALL(X==0)).OR.(ALL(Y==0))) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"WARNING: all array points are equal to zero"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A9)',IOSTAT=ERR)"<path d='"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_BezierQuadratic"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename: ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"M "//TRIM(StringReal(X(1)))//" "// &
    & TRIM(StringReal(VIEWBOX_HEIGHT-Y(1)))

! Draw a quadratic Bezier ("Q") starting from the first point of the array
! ("M") to point "i+1" using point "i" as the control point.

IF (SIZE(X)>2) THEN
    DO I=2,SIZE(X)-1,2
        IF ((I+1)/=SIZE(X)) THEN
            WRITE(IO,*)"Q "//TRIM(StringReal(X(I)))//","// &
                & TRIM(StringReal(VIEWBOX_HEIGHT-Y(I)))//" "// &
                & TRIM(StringReal(X(I+1)))//","// &
                & TRIM(StringReal(VIEWBOX_HEIGHT-Y(I+1)))
        ELSE
            WRITE(IO,*)"Q "//TRIM(StringReal(X(I)))//","// &
                & TRIM(StringReal(VIEWBOX_HEIGHT-Y(I)))//" "// &
                & TRIM(StringReal(X(I+1)))//","// &
                & TRIM(StringReal(VIEWBOX_HEIGHT-Y(I+1)))//"'"
        END IF
    END DO
    IF (MOD(SIZE(X),2)==0) WRITE(IO,*)"T "// &
        & TRIM(StringReal(X(SIZE(X))))//" "// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-Y(SIZE(Y))))//"'"
END IF

WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(FILL_RGB(1)))//","// &
        & TRIM(StringInteger(FILL_RGB(2)))//","// &
        & TRIM(StringInteger(FILL_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_BezierQuadratic

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_CartesianPlot(OUTPUT_FILE, &
    & X_POINTS,Y_POINTS,X_TITLE,Y_TITLE, &
    & X_START,X_END,Y_START,Y_END, &
    & X_DECIMAL_PLACES,Y_DECIMAL_PLACES,SCIENTIFIC_X,SCIENTIFIC_Y, &
    & X_TICK_INTERVAL,Y_TICK_INTERVAL, &
    & DRAW_TICKS,TICK_LENGTH,TICK_WIDTH, &
    & DIAGRAM_TITLE,DATA_TITLE,TITLE_DIAGRAM_FONT_SIZE, &
    & TITLE_AXIS_FONT_SIZE,NUMBERS_AXIS_FONT_SIZE, &
    & FONT_RGB,AXIS_STROKE_RGB,AXIS_STROKE_WIDTH, &
    & DRAW_GRID,GRID_STROKE_WIDTH,GRID_STROKE_RGB, &
    & SHAPE_TYPE,SHAPE_SIZE,SHAPE_STROKE_WIDTH, &
    & SHAPE_FILL_RGB,SHAPE_STROKE_RGB, &
    & LINE_TYPE,LINE_STROKE_WIDTH,LINE_STROKE_RGB)

! SVG_CartesianPlot draws a cartesian diagram in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x_points: real, array (1D). The cartesian coordinates of the x-component of
!    the points that will be plotted. Arrays x_points and y_points must have
!    the same amount of elements and a 1-1 correspondance.
! y_points: real, array (1D). The cartesian coordinates of the y-component of
!    the points that will be plotted. Arrays x_points and y_points must have
!    the same amount of elements and a 1-1 correspondance.
! x_title: character, scalar. The title of the x-component.
! y_title: character, scalar. The title of the y-component.
! x_start: real, scalar. The initial value of the x-axis.
! x_end: real, scalar. The ending value of the x-axis.
! y_start: real, scalar. The initial value of the y-axis.
! y_end: real, scalar. The ending value of the y-axis.
! x_decimal_places: integer, scalar. The amount of decimal places of the numbers
!    on the x-axis.
! y_decimal_places: integer, scalar. The amount of decimal places of the numbers
!    on the y-axis.
! scientific_x: logical, scalar. Specifies whether the scientific notation will
!    be used for the numbers on the x-axis.
! scientific_y: logical, scalar. Specifies whether the scientific notation will
!    be used for the numbers on the y-axis.
! x_tick_interval: real, scalar. The interval between the ticks for the x axis.
! y_tick_interval: real, scalar. The interval between the ticks for the y axis.
! draw_ticks: logical, scalar. Specifies whether ticks will be drawn.
! tick_length: real, scalar. The length of the axes' tick.
! tick_width: real, scalar. The width of the axes' tick.
! diagram_title: character, scalar. The title of the graph.
! data_title: character, scalar. The title of the data series (for the legend).
! title_diagram_font_size: integer, scalar. The font size of the diagram title.
! title_axis_font_size: integer, scalar. The font size of the axes titles.
! number_axis_font_size: integer, scalar. The font size of the axes numbers.
! font_rgb: integer, array (1D) with 3 elements. Contains the RGB channel
!    values of the color of all the text in the graph.
! axis_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the line will be drawn.
! axis_stroke_width: integer, scalar. The thickness of the axes' line (px).
! draw_grid: logical, scalar. Specifies whether gridlines will be drawn.
! grid_stroke_width: integer, scalar. The width of the gridlines.
! grid_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the gridlines will be drawn.
! shape_type: character, scalar. Specifies which shape will be used to plot the
!    data.
! shape_size: integer, scalar. The shape's diameter (in pixels).
! shape_stroke_width: integer, scalar. The thickness of the line used to draw
!    the circle (in pixels).
! shape_fill_rgb: integer, array (1D) with 3 elements. Contains the RGB channel
!    values of the color with which the shape will be filled.
! shape_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the line of the circle will be
!    drawn.
! line_type: character, scalar. Specifies the kind of line that connects the
!    diagram points.
! line_stroke_width: integer, scalar. The width of the line that connects the
!    diagram points.
! line_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the line that connects the
!    diagram points will be drawn.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_CartesianPlot(output_file, &
!    & x_points,y_points,x_title,y_title, &
!    & x_start,x_end,y_start,y_end, &
!    & x_decimal_places,y_decimal_places,scientific_x,scientific_y, &
!    & x_tick_interval,y_tick_interval, &
!    & draw_ticks,tick_length,tick_width, &
!    & diagram_title,data_title,title_diagram_font_size, &
!    & title_axis_font_size,numbers_axis_font_size, &
!    & font_rgb,axis_stroke_rgb,axis_stroke_width, &
!    & draw_grid,grid_stroke_width,grid_stroke_rgb, &
!    & shape_type,shape_size,shape_stroke_width, &
!    & shape_fill_rgb,shape_stroke_rgb, &
!    & line_type,line_stroke_width,line_stroke_rgb)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),shape_type*(*),line_type*(*)
!character,intent(in):: diagram_title*(*),x_title*(*),y_title*(*)
!character,intent(in):: data_title*(*)
!real(srk),intent(in):: x_points(:),y_points(:),x_start,x_end,y_start,y_end
!real(srk),intent(in):: x_tick_interval,y_tick_interval,tick_length,tick_width
!real(srk),intent(in):: axis_stroke_width,grid_stroke_width
!real(srk),intent(in):: shape_size,shape_stroke_width,line_stroke_width
!real(srk),intent(in):: title_axis_font_size,numbers_axis_font_size
!real(srk),intent(in):: title_diagram_font_size
!integer,intent(in):: x_decimal_places,y_decimal_places
!integer,intent(in):: axis_stroke_rgb(3),grid_stroke_rgb(3),font_rgb(3)
!integer,intent(in):: shape_fill_rgb(3),shape_stroke_rgb(3),line_stroke_rgb(3)
!logical,intent(in):: draw_ticks,draw_grid,scientific_x,scientific_y
!end subroutine SVG_CartesianPlot
!end interface

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

!call SVG_CartesianPlot( &
!output_file='plot.svg', &
!x_points=x, &
!y_points=y, &
!x_title='x_axis', &
!y_title='y_axis', &
!x_start=0.0_srk, &
!x_end=3.0_srk, &
!y_start=0.0_srk, &
!y_end=1.1_srk, &
!x_decimal_places=1, &
!y_decimal_places=1, &
!scientific_x=.false., &
!scientific_y=.false., &
!x_tick_interval=0.3_srk, &
!y_tick_interval=0.2_srk, &
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
!use Config, only: srk,default_string_length,warnings_pause, &
!    & viewBox_width,viewBox_height

IMPLICIT NONE

! Parameters:
CHARACTER,PARAMETER:: FONT*(7)='verdana'
LOGICAL,PARAMETER:: CREATE_TOGGLE_BUTTONS=.TRUE.,CREATE_DATA_LINKS=.TRUE.

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),SHAPE_TYPE*(*),LINE_TYPE*(*)
CHARACTER,INTENT(IN):: DIAGRAM_TITLE*(*),X_TITLE*(*),Y_TITLE*(*)
CHARACTER,INTENT(IN):: DATA_TITLE*(*)
REAL(SRK),INTENT(IN):: X_POINTS(:),Y_POINTS(:),X_START,X_END,Y_START,Y_END
REAL(SRK),INTENT(IN):: X_TICK_INTERVAL,Y_TICK_INTERVAL,TICK_LENGTH,TICK_WIDTH
REAL(SRK),INTENT(IN):: AXIS_STROKE_WIDTH,GRID_STROKE_WIDTH
REAL(SRK),INTENT(IN):: SHAPE_SIZE,SHAPE_STROKE_WIDTH,LINE_STROKE_WIDTH
REAL(SRK),INTENT(IN):: TITLE_AXIS_FONT_SIZE,NUMBERS_AXIS_FONT_SIZE
REAL(SRK),INTENT(IN):: TITLE_DIAGRAM_FONT_SIZE
INTEGER,INTENT(IN):: X_DECIMAL_PLACES,Y_DECIMAL_PLACES
INTEGER,INTENT(IN):: AXIS_STROKE_RGB(3),GRID_STROKE_RGB(3),FONT_RGB(3)
INTEGER,INTENT(IN):: SHAPE_FILL_RGB(3),SHAPE_STROKE_RGB(3),LINE_STROKE_RGB(3)
LOGICAL,INTENT(IN):: DRAW_TICKS,DRAW_GRID,SCIENTIFIC_X,SCIENTIFIC_Y

! Private variables:
REAL(SRK):: RECTANGLE_WIDTH,RECTANGLE_HEIGHT,DIAGRAM_WIDTH,DIAGRAM_HEIGHT
REAL(SRK):: LEGEND_POSITION_X,LEGEND_POSITION_Y
REAL(SRK):: REAL_I,POINTS(SIZE(X_POINTS),2),XM,YM,AXIS_START
REAL(SRK),ALLOCATABLE:: BEZIER_POINTS(:,:)
INTEGER:: I

CHARACTER*(DEFAULT_STRING_LENGTH),ALLOCATABLE,TARGET,SAVE:: FILE_LIST(:)
CHARACTER*(DEFAULT_STRING_LENGTH),POINTER:: CHARACTER_POINTER_1D(:)
REAL(SRK),ALLOCATABLE,TARGET,SAVE:: LEGEND_SPACING(:)
REAL(SRK),POINTER:: REAL_POINTER_1D(:)
LOGICAL:: PLOT_AXES

CHARACTER*(DEFAULT_STRING_LENGTH):: CHARACTERS(2)
REAL(SRK):: NUMBERS(SIZE(X_POINTS),2),TICK_NUMBER,TICK_COORDINATE

LOGICAL,SAVE:: FIRST_RUN=.TRUE.

! Variable initialization
! axis_start: real, scalar. A value that specifies the margin left between the
!    border of the canvas and the lower left tip of the diagram (px).
DIAGRAM_WIDTH=VIEWBOX_HEIGHT
DIAGRAM_HEIGHT=VIEWBOX_HEIGHT
AXIS_START=2.0_SRK*TITLE_DIAGRAM_FONT_SIZE
RECTANGLE_WIDTH=DIAGRAM_WIDTH-2.0_SRK*AXIS_START
RECTANGLE_HEIGHT=RECTANGLE_WIDTH
PLOT_AXES=.FALSE.
IF (ALLOCATED(BEZIER_POINTS)) DEALLOCATE(BEZIER_POINTS)
IF (.NOT.(ALLOCATED(FILE_LIST))) ALLOCATE(FILE_LIST(0))
IF (.NOT.(ALLOCATED(LEGEND_SPACING))) ALLOCATE(LEGEND_SPACING(0))
LEGEND_POSITION_Y=DIAGRAM_HEIGHT-AXIS_START
LEGEND_POSITION_X=DIAGRAM_WIDTH-AXIS_START/2.0_SRK

! ------------------------------------------------------------------------------

! Error control:

IF (VIEWBOX_HEIGHT>=VIEWBOX_WIDTH) THEN
WRITE(*,*)"SVG_CartesianPlot"
WRITE(*,*)"ERROR: canvas height may not be bigger than canvas width"
WRITE(*,*)"for this type of graph"
WRITE(*,*)"canvas width: ",VIEWBOX_WIDTH
WRITE(*,*)"canvas height: ",VIEWBOX_HEIGHT
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((DIAGRAM_WIDTH-2.0_SRK*AXIS_START<=0).OR. &
    & (DIAGRAM_HEIGHT-2.0_SRK*AXIS_START<=0)) THEN
WRITE(*,*)"SVG_CartesianPlot"
WRITE(*,*)"ERROR: invalid axis geometry parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIZE(X_POINTS)/=SIZE(Y_POINTS)) THEN
WRITE(*,*)"SVG_CartesianPlot"
WRITE(*,*)"ERROR: x and y coordinates do not match"
WRITE(*,*)"size(x): ",SIZE(X_POINTS)
WRITE(*,*)"size(y): ",SIZE(Y_POINTS)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((ALL(X_POINTS==0.0)).OR.(ALL(Y_POINTS==0.0))) THEN
WRITE(*,*)"SVG_CartesianPlot"
WRITE(*,*)"WARNING: all array points are equal to zero"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (ALL(SHAPE_TYPE/=(/'none    ','circle  ','square  ','cross   ', &
    & 'ex      ','triangle','diamond '/))) THEN
WRITE(*,*)"SVG_CartesianPlot"
WRITE(*,*)"ERROR: incorrectly specified point shape"
WRITE(*,*)"shape_type: ",SHAPE_TYPE
WRITE(*,*)"supported values: 'none', 'circle', 'square',"
WRITE(*,*)"'cross', 'ex', 'triangle', 'diamond'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ALL(LINE_TYPE/=(/'none   ','crooked','bezier '/))) THEN
WRITE(*,*)"SVG_CartesianPlot"
WRITE(*,*)"ERROR: incorrectly specified line type"
WRITE(*,*)"line_type: ",LINE_TYPE
WRITE(*,*)"supported values: 'none', 'crooked', 'bezier'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SHAPE_TYPE=='none').AND.(LINE_TYPE=='none')) THEN
WRITE(*,*)"SVG_CartesianPlot"
WRITE(*,*)"WARNING: no data will be plotted"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((LEN_TRIM(X_TITLE)>DEFAULT_STRING_LENGTH).OR. &
    & (LEN_TRIM(Y_TITLE)>DEFAULT_STRING_LENGTH)) THEN
WRITE(*,*)"SVG_CartesianPlot"
WRITE(*,*)"ERROR: component title too long"
WRITE(*,*)"len_trim(x_title): ",LEN_TRIM(X_TITLE)
WRITE(*,*)"len_trim(y_title): ",LEN_TRIM(Y_TITLE)
WRITE(*,*)"default_string_length: ",DEFAULT_STRING_LENGTH
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

ADD_TO_DIAGRAM: IF (ANY(FILE_LIST==OUTPUT_FILE)) THEN

! Scan the filelist:
    DO I=1,SIZE(FILE_LIST)
    IF (FILE_LIST(I)==OUTPUT_FILE) THEN

! Resurrect the SVG document:
        CALL SVG_Resurrect(OUTPUT_FILE)
! Regulate spacing for the graph's legend:
        IF (LEN_TRIM(DATA_TITLE)/=0) LEGEND_SPACING(I)= &
            & LEGEND_SPACING(I)-1.5_SRK*NUMBERS_AXIS_FONT_SIZE
        EXIT

    END IF
    END DO

ELSE ADD_TO_DIAGRAM

! Initialize the diagram:
    PLOT_AXES=.TRUE.
    CALL SVG_Initialize(OUTPUT_FILE)

! Add the filename to the list of initialized files:
    CHARACTER_POINTER_1D=>FILE_LIST
    CALL ArrayExpandCharacter(CHARACTER_POINTER_1D,OUTPUT_FILE)
    DEALLOCATE(FILE_LIST)
    ALLOCATE(FILE_LIST(SIZE(CHARACTER_POINTER_1D)))
    FILE_LIST=CHARACTER_POINTER_1D

! Initialize a legend:
    REAL_POINTER_1D=>LEGEND_SPACING
    CALL ArrayExpandReal(REAL_POINTER_1D,LEGEND_POSITION_Y)
    DEALLOCATE(LEGEND_SPACING)
    ALLOCATE(LEGEND_SPACING(SIZE(REAL_POINTER_1D)))
    LEGEND_SPACING=REAL_POINTER_1D

END IF ADD_TO_DIAGRAM

! ------------------------------------------------------------------------------

AXES_PLOT: IF (PLOT_AXES) THEN

! Diagram title:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=DIAGRAM_TITLE, &
    & X=DIAGRAM_WIDTH/2.0_SRK,Y=DIAGRAM_HEIGHT-AXIS_START/2.0_SRK, &
    & FONT=FONT,FONT_SIZE=TITLE_DIAGRAM_FONT_SIZE,FILL_OPACITY=1.0_SRK, &
    & FILL_RGB=FONT_RGB,ANCHOR='middle')

! Draw the axes.

! The coordinates of the four square points are:
! x_a=axis_start                    (lower left square corner)
! y_a=axis_start                    (lower left square corner)
! x_b=axis_start                    (upper left square corner)
! y_b=axis_start+rectangle_height   (upper left square corner)
! x_c=axis_start+rectangle_width    (upper right square corner)
! y_c=axis_start+rectangle_height   (upper right square corner)
! x_d=axis_start+rectangle_width    (lower right square corner)
! y_d=axis_start                    (lower right square corner)

CALL SVG_GroupStart(OUTPUT_FILE)

! Left (AB) Vertical axis:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START,AXIS_START/), &
    & Y=(/AXIS_START,AXIS_START+RECTANGLE_HEIGHT/), &
    STROKE_WIDTH=AXIS_STROKE_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

! Upper (BC) Horizontal axis:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START,AXIS_START+RECTANGLE_WIDTH/), &
    & Y=(/AXIS_START+RECTANGLE_HEIGHT,AXIS_START+RECTANGLE_HEIGHT/), &
    STROKE_WIDTH=AXIS_STROKE_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

! Right (CD) Vertical axis:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START+RECTANGLE_WIDTH,AXIS_START+RECTANGLE_WIDTH/), &
    & Y=(/AXIS_START+RECTANGLE_HEIGHT,AXIS_START/), &
    STROKE_WIDTH=AXIS_STROKE_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

! Lower (AD) Horizontal axis:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START,AXIS_START+RECTANGLE_WIDTH/), &
    & Y=(/AXIS_START,AXIS_START/), &
    STROKE_WIDTH=AXIS_STROKE_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

! Axes numbers.

! Horizontal axis numbering:
TICK_NUMBER=X_START
REAL_I=0.0_SRK
DO WHILE (TICK_NUMBER<=X_END)
    IF ((X_START<TICK_NUMBER).AND.(TICK_NUMBER<X_END)) THEN
        TICK_COORDINATE=LinearIntExp(X_START,X_END, &
            & AXIS_START,AXIS_START+RECTANGLE_WIDTH,TICK_NUMBER)
    ELSE IF (TICK_NUMBER==X_START) THEN
        TICK_COORDINATE=AXIS_START
    ELSE IF (TICK_NUMBER>=X_END) THEN
        TICK_COORDINATE=AXIS_START+RECTANGLE_WIDTH
    END IF
    CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE, &
        & STRING=TRIM(FormatReal( &
        & TICK_NUMBER,X_DECIMAL_PLACES,SCIENTIFIC_X)), &
        & X=TICK_COORDINATE, &
        & Y=AXIS_START-5.0_SRK*NUMBERS_AXIS_FONT_SIZE/4.0_SRK, &
        & FONT=FONT,FONT_SIZE=NUMBERS_AXIS_FONT_SIZE, &
        & FILL_OPACITY=1.0_SRK,FILL_RGB=AXIS_STROKE_RGB,ANCHOR='middle')
    TICK_NUMBER=X_START+REAL_I*X_TICK_INTERVAL
    REAL_I=REAL_I+1.0_SRK
END DO

! Vertical axis numbering:
TICK_NUMBER=Y_START
REAL_I=0.0_SRK
DO WHILE (TICK_NUMBER<=Y_END)
    IF ((Y_START<TICK_NUMBER).AND.(TICK_NUMBER<Y_END)) THEN
        TICK_COORDINATE=LinearIntExp(Y_START,Y_END, &
            & AXIS_START,AXIS_START+RECTANGLE_HEIGHT,TICK_NUMBER)
    ELSE IF (TICK_NUMBER==Y_START) THEN
        TICK_COORDINATE=AXIS_START
    ELSE IF (TICK_NUMBER>=Y_END) THEN
        TICK_COORDINATE=AXIS_START+RECTANGLE_HEIGHT
    END IF
    CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE, &
        & STRING=TRIM(FormatReal( &
        & TICK_NUMBER,Y_DECIMAL_PLACES,SCIENTIFIC_Y)), &
        & X=AXIS_START-NUMBERS_AXIS_FONT_SIZE/2.0_SRK, &
        & Y=TICK_COORDINATE-NUMBERS_AXIS_FONT_SIZE/3.0_SRK, &
        & FONT=FONT,FONT_SIZE=NUMBERS_AXIS_FONT_SIZE, &
        & FILL_OPACITY=1.0_SRK,FILL_RGB=AXIS_STROKE_RGB,ANCHOR='end')
    TICK_NUMBER=Y_START+REAL_I*Y_TICK_INTERVAL
    REAL_I=REAL_I+1.0_SRK
END DO

! Draw the ticks:

TICK_DRAW: IF (DRAW_TICKS) THEN

! Ticks x-axis:
TICK_NUMBER=X_START
REAL_I=0.0_SRK
DO WHILE (TICK_NUMBER<X_END)
    IF ((X_START<TICK_NUMBER).AND.(TICK_NUMBER<X_END)) THEN
        TICK_COORDINATE=LinearIntExp(X_START,X_END, &
            & AXIS_START,AXIS_START+RECTANGLE_WIDTH,TICK_NUMBER)
        CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
            & X=(/TICK_COORDINATE,TICK_COORDINATE/), &
            & Y=(/AXIS_START,AXIS_START+TICK_LENGTH/), &
            & STROKE_WIDTH=TICK_WIDTH, &
            & STROKE_RGB=AXIS_STROKE_RGB)
    END IF
    TICK_NUMBER=X_START+REAL_I*X_TICK_INTERVAL
    REAL_I=REAL_I+1.0_SRK
END DO

! Ticks y-axis:
TICK_NUMBER=Y_START
REAL_I=0.0_SRK
DO WHILE (TICK_NUMBER<Y_END)
    IF ((Y_START<TICK_NUMBER).AND.(TICK_NUMBER<Y_END)) THEN
        TICK_COORDINATE=LinearIntExp(Y_START,Y_END, &
            & AXIS_START,AXIS_START+RECTANGLE_HEIGHT,TICK_NUMBER)
        CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
            & X=(/AXIS_START,AXIS_START+TICK_LENGTH/), &
            & Y=(/TICK_COORDINATE,TICK_COORDINATE/), &
            & STROKE_WIDTH=TICK_WIDTH, &
            & STROKE_RGB=AXIS_STROKE_RGB)
    END IF
    TICK_NUMBER=Y_START+REAL_I*Y_TICK_INTERVAL
    REAL_I=REAL_I+1.0_SRK
END DO

END IF TICK_DRAW

! Axes titles.

! Title x-axis:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=X_TITLE, &
    & X=DIAGRAM_WIDTH/2.0_SRK, &
    & Y=TITLE_AXIS_FONT_SIZE/2.0_SRK, &
    & FONT=FONT,FONT_SIZE=TITLE_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB,ANCHOR='middle')

! Title y-axis:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=Y_TITLE, &
    & X=TITLE_AXIS_FONT_SIZE/4.0_SRK, &
    & Y=DIAGRAM_HEIGHT/2.0_SRK, &
    & FONT=FONT,FONT_SIZE=TITLE_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB,ANCHOR='middle', &
    & ROTATE=(/-90.0_SRK,TITLE_AXIS_FONT_SIZE/4.0_SRK, &
    & DIAGRAM_HEIGHT/2.0_SRK/))

CALL SVG_GroupEnd(OUTPUT_FILE)

! Draw the gridlines:

GRID_DRAW: IF (DRAW_GRID) THEN

CALL SVG_GroupStart(OUTPUT_FILE)

TICK_NUMBER=X_START
REAL_I=0.0_SRK
DO WHILE (TICK_NUMBER<X_END)
    IF ((X_START<TICK_NUMBER).AND.(TICK_NUMBER<X_END)) THEN
        TICK_COORDINATE=LinearIntExp(X_START,X_END, &
            & AXIS_START,AXIS_START+RECTANGLE_WIDTH,TICK_NUMBER)
        CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
            & X=(/TICK_COORDINATE,TICK_COORDINATE/), &
            & Y=(/AXIS_START,AXIS_START+RECTANGLE_HEIGHT/), &
            & STROKE_WIDTH=GRID_STROKE_WIDTH, &
            & STROKE_RGB=GRID_STROKE_RGB)
    END IF
    TICK_NUMBER=X_START+REAL_I*X_TICK_INTERVAL
    REAL_I=REAL_I+1.0_SRK
END DO

TICK_NUMBER=Y_START
REAL_I=0.0_SRK
DO WHILE (TICK_NUMBER<Y_END)
    IF ((Y_START<TICK_NUMBER).AND.(TICK_NUMBER<Y_END)) THEN
        TICK_COORDINATE=LinearIntExp(Y_START,Y_END, &
            & AXIS_START,AXIS_START+RECTANGLE_HEIGHT,TICK_NUMBER)
        CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
            & X=(/AXIS_START,AXIS_START+RECTANGLE_WIDTH/), &
            & Y=(/TICK_COORDINATE,TICK_COORDINATE/), &
            & STROKE_WIDTH=GRID_STROKE_WIDTH, &
            & STROKE_RGB=GRID_STROKE_RGB)
    END IF
    TICK_NUMBER=Y_START+REAL_I*Y_TICK_INTERVAL
    REAL_I=REAL_I+1.0_SRK
END DO

CALL SVG_GroupEnd(OUTPUT_FILE)

END IF GRID_DRAW

END IF AXES_PLOT

! ------------------------------------------------------------------------------

COORDINATE_CONVERSION_LOOP: DO I=1,SIZE(X_POINTS)

POINTS(I,1)=LinearIntExp(X_START,X_END, &
    AXIS_START,AXIS_START+RECTANGLE_WIDTH,X_POINTS(I))

POINTS(I,2)=LinearIntExp(Y_START,Y_END, &
    AXIS_START,AXIS_START+RECTANGLE_HEIGHT,Y_POINTS(I))

END DO COORDINATE_CONVERSION_LOOP

! ------------------------------------------------------------------------------

CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=TRIM(DATA_TITLE))

! Diagram line:

SELECT CASE(TRIM(LINE_TYPE))

CASE('crooked')

CALL SVG_Polyline(OUTPUT_FILE=OUTPUT_FILE,X=POINTS(:,1),Y=POINTS(:,2), &
    & STROKE_WIDTH=LINE_STROKE_WIDTH,FILL_OPACITY=0.0_SRK, &
    & STROKE_RGB=LINE_STROKE_RGB,FILL_RGB=(/255,255,255/), &
    & ID=TRIM(DATA_TITLE))

CASE('bezier')

! In order for the Bezier curve to be drawn correctly, the array with the
! Bezier curve and control points needs to have an odd amount of elements.
IF (MOD(SIZE(X_POINTS),2)==0) THEN
ALLOCATE(BEZIER_POINTS(SIZE(X_POINTS)+1,2))
BEZIER_POINTS(SIZE(X_POINTS)+1,1)=POINTS(SIZE(X_POINTS),1)
BEZIER_POINTS(SIZE(X_POINTS)+1,2)=POINTS(SIZE(X_POINTS),2)
ELSE
ALLOCATE(BEZIER_POINTS(SIZE(X_POINTS),2))
END IF

DO I=1,SIZE(X_POINTS)
IF ((MOD(I,2)==0).AND.(I/=1).AND.(I/=SIZE(X_POINTS))) THEN

! The Bezier control points are calculated:
BEZIER_POINTS(I,:)=BezierQuadraticControl( &
    (/POINTS(I-1,1),POINTS(I-1,2)/), &
    (/POINTS(I+1,1),POINTS(I+1,2)/), &
    (/POINTS(I,1),POINTS(I,2)/))

ELSE

! The rest of the Bezier curve points are the same as the diagram points.
BEZIER_POINTS(I,:)=POINTS(I,:)

END IF
END DO

CALL SVG_BezierQuadratic(OUTPUT_FILE=OUTPUT_FILE, &
    & X=BEZIER_POINTS(:,1),Y=BEZIER_POINTS(:,2), &
    & STROKE_WIDTH=LINE_STROKE_WIDTH,FILL_OPACITY=0.0_SRK, &
    & STROKE_RGB=LINE_STROKE_RGB,FILL_RGB=(/255,255,255/), &
    & ID=TRIM(DATA_TITLE))

END SELECT

! Diagram points:

CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=(TRIM(DATA_TITLE)//'_points'))

CHARACTERS(1)=TRIM(X_TITLE)//'_coordinate'
CHARACTERS(2)=TRIM(Y_TITLE)//'_coordinate'

DIAGRAM_POINT_LOOP: DO I=1,SIZE(X_POINTS)

SELECT CASE(TRIM(SHAPE_TYPE))

CASE('circle')
CALL SVG_Circle(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),RADIUS=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/X_POINTS(I),Y_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('square')
CALL SVG_Square(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/X_POINTS(I),Y_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('diamond')
CALL SVG_Square(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ROTATE=(/45.0_SRK,POINTS(I,1),POINTS(I,2)/), &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/X_POINTS(I),Y_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('cross')
CALL SVG_Cross(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,STROKE_RGB=SHAPE_STROKE_RGB, &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/X_POINTS(I),Y_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('ex')
CALL SVG_Cross(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,STROKE_RGB=SHAPE_STROKE_RGB, &
    & ROTATE=(/45.0_SRK,POINTS(I,1),POINTS(I,2)/), &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/X_POINTS(I),Y_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('triangle')
CALL SVG_Triangle(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/X_POINTS(I),Y_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

END SELECT

END DO DIAGRAM_POINT_LOOP

CALL SVG_GroupEnd(OUTPUT_FILE)

CALL SVG_GroupEnd(OUTPUT_FILE)

! ------------------------------------------------------------------------------

! Make a legend:
CREATE_LEGEND: IF (LEN_TRIM(DATA_TITLE)/=0) THEN

CALL SVG_GroupStart(OUTPUT_FILE)

DO I=1,SIZE(FILE_LIST)
IF (FILE_LIST(I)==OUTPUT_FILE) THEN

! Legend lines:
IF (TRIM(LINE_TYPE)/='none') CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/LEGEND_POSITION_X-NUMBERS_AXIS_FONT_SIZE, &
    & LEGEND_POSITION_X+NUMBERS_AXIS_FONT_SIZE/), &
    & Y=(/LEGEND_SPACING(I),LEGEND_SPACING(I)/), &
    & STROKE_WIDTH=LINE_STROKE_WIDTH,STROKE_RGB=LINE_STROKE_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_line'))

! Legend points:
LEGEND_POINTS: IF (TRIM(SHAPE_TYPE)/='none') THEN

SELECT CASE(TRIM(SHAPE_TYPE))

CASE('circle')
CALL SVG_Circle(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),RADIUS=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('square')
CALL SVG_Square(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('diamond')
CALL SVG_Square(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ROTATE=(/45.0_SRK,LEGEND_POSITION_X,LEGEND_SPACING(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('cross')
CALL SVG_Cross(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,STROKE_RGB=SHAPE_STROKE_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('ex')
CALL SVG_Cross(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,STROKE_RGB=SHAPE_STROKE_RGB, &
    & ROTATE=(/45.0_SRK,LEGEND_POSITION_X,LEGEND_SPACING(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('triangle')
CALL SVG_Triangle(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

END SELECT

END IF LEGEND_POINTS

CALL SVG_GroupEnd(OUTPUT_FILE)

! Add text to the graph legend:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=DATA_TITLE, &
    & X=LEGEND_POSITION_X+2.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
    & Y=LEGEND_SPACING(I)-NUMBERS_AXIS_FONT_SIZE/3.0_SRK, &
    & FONT=FONT,FONT_SIZE=NUMBERS_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB)

! ------------------------------------------------------------------------------

! Create a toggle button.
IF (CREATE_TOGGLE_BUTTONS) THEN
    CALL SVG_Vanish(OUTPUT_FILE=OUTPUT_FILE)
    CALL SVG_ActionButton(OUTPUT_FILE=OUTPUT_FILE, &
        & SCRIPT='vanish',TARGET_ID=TRIM(DATA_TITLE), &
        & X=VIEWBOX_WIDTH-3.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
        & Y=LEGEND_SPACING(I), &
        & WIDTH=4.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
        & HEIGHT=NUMBERS_AXIS_FONT_SIZE, &
        & STROKE_WIDTH=GRID_STROKE_WIDTH,BUTTON_FILL_OPACITY=1.0_SRK, &
        & STROKE_RGB=AXIS_STROKE_RGB,BUTTON_FILL_RGB=(/255,255,255/), &
        & STRING='toggle',FONT='verdana', &
        & FONT_SIZE=2.0_SRK*NUMBERS_AXIS_FONT_SIZE/3.0_SRK, &
        & TEXT_FILL_OPACITY=1.0_SRK,TEXT_FILL_RGB=FONT_RGB)
END IF

! Create a link button to the raw data.
IF (CREATE_DATA_LINKS) THEN
! Create the HTML table:
    CHARACTERS(1)=TRIM(X_TITLE)
    CHARACTERS(2)=TRIM(Y_TITLE)
    NUMBERS(:,1)=X_POINTS
    NUMBERS(:,2)=Y_POINTS
    CALL HTML_Initialize(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html')
    CALL HTML_Heading(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html', &
        & STRING=TRIM(OUTPUT_FILE),LEVEL=3)
    CALL HTML_Heading(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html', &
        & STRING=TRIM(DIAGRAM_TITLE)//": "//TRIM(DATA_TITLE),LEVEL=4)
    CALL HTML_Table(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html', &
        & NUMBERS=NUMBERS,HEADINGS=CHARACTERS, &
        & DECIMAL_PLACES=(/X_DECIMAL_PLACES+3,Y_DECIMAL_PLACES+3/), &
        & SCIENTIFIC=(/.FALSE.,.FALSE./))
    CALL HTML_Finalize(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html')
! Create the link:
    CALL SVG_LinkButton(OUTPUT_FILE=OUTPUT_FILE,LINK_STRING= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html', &
        & X=VIEWBOX_WIDTH-7.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
        & Y=LEGEND_SPACING(I), &
        & WIDTH=3.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
        & HEIGHT=NUMBERS_AXIS_FONT_SIZE, &
        & STROKE_WIDTH=GRID_STROKE_WIDTH,BUTTON_FILL_OPACITY=1.0_SRK, &
        & STROKE_RGB=AXIS_STROKE_RGB,BUTTON_FILL_RGB=(/255,255,255/), &
        & STRING='data',FONT='verdana', &
        & FONT_SIZE=2.0_SRK*NUMBERS_AXIS_FONT_SIZE/3.0_SRK, &
        & TEXT_FILL_OPACITY=1.0_SRK,TEXT_FILL_RGB=FONT_RGB)
END IF

END IF
END DO

END IF CREATE_LEGEND

CALL SVG_Finalize(OUTPUT_FILE)

END SUBROUTINE SVG_CartesianPlot

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_CartesianPlotDefault(OUTPUT_FILE,X_POINTS,Y_POINTS, &
    & X_TITLE,Y_TITLE,DIAGRAM_TITLE,DATA_TITLE,SHAPE_TYPE,SHAPE_SIZE, &
    & SHAPE_STROKE_WIDTH,SHAPE_FILL_RGB,SHAPE_STROKE_RGB, &
    & LINE_TYPE,LINE_STROKE_WIDTH,LINE_STROKE_RGB)

! SVG_CartesianPlotDefault is used to auto-configure and execute subroutine
! SVG_CartesianPlot.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x_points: real, array (1D). The cartesian coordinates of the x-component of
!    the points that will be plotted. Arrays x_points and y_points must have
!    the same amount of elements and a 1-1 correspondance.
! y_points: real, array (1D). The cartesian coordinates of the y-component of
!    the points that will be plotted. Arrays x_points and y_points must have
!    the same amount of elements and a 1-1 correspondance.
! x_title: character, scalar. The title of the x-component.
! y_title: character, scalar. The title of the y-component.
! diagram_title: character, scalar. The title of the graph.
! data_title: character, scalar. The title of the data series (for the legend).
! shape_type: character, scalar. Specifies which shape will be used to plot the
!    data.
! shape_size: integer, scalar. The shape's diameter (in pixels).
! shape_stroke_width: integer, scalar. The thickness of the line used to draw
!    the circle (in pixels).
! shape_fill_rgb: integer, array (1D) with 3 elements. Contains the RGB channel
!    values of the color with which the shape will be filled.
! shape_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the line of the circle will be
!    drawn.
! line_type: character, scalar. Specifies the kind of line that connects the
!    diagram points.
! line_stroke_width: integer, scalar. The width of the line that connects the
!    diagram points.
! line_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the line that connects the
!    diagram points will be drawn.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_CartesianPlotDefault(output_file,x_points,y_points, &
!    & x_title,y_title,diagram_title,data_title,shape_type,shape_size, &
!    & shape_stroke_width,shape_fill_rgb,shape_stroke_rgb, &
!    & line_type,line_stroke_width,line_stroke_rgb)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),shape_type*(*),line_type*(*)
!character,intent(in):: diagram_title*(*),x_title*(*),y_title*(*),data_title*(*)
!real(srk),intent(in):: x_points(:),y_points(:)
!real(srk),intent(in):: shape_size,shape_stroke_width,line_stroke_width
!integer,intent(in):: shape_fill_rgb(3),shape_stroke_rgb(3),line_stroke_rgb(3)
!end subroutine SVG_CartesianPlotDefault
!end interface

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

!call SVG_CartesianPlotDefault( &
!x_points=x, &
!y_points=y, &
!output_file='plot.svg', &
!x_title='x_axis', &
!y_title='y_axis', &
!diagram_title='cartesian_diagram', &
!data_title='data_series', &
!shape_type='square', &
!shape_size=10.0_srk, &
!shape_stroke_width=1.0_srk, &
!shape_fill_rgb=(/255,0,0/), &
!shape_stroke_rgb=(/255,0,0/), &
!line_type='crooked', &
!line_stroke_width=2.0_srk, &
!line_stroke_rgb=(/0,0,255/))

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),SHAPE_TYPE*(*),LINE_TYPE*(*)
CHARACTER,INTENT(IN):: DIAGRAM_TITLE*(*),X_TITLE*(*),Y_TITLE*(*),DATA_TITLE*(*)
REAL(SRK),INTENT(IN):: X_POINTS(:),Y_POINTS(:)
REAL(SRK),INTENT(IN):: SHAPE_SIZE,SHAPE_STROKE_WIDTH,LINE_STROKE_WIDTH
INTEGER,INTENT(IN):: SHAPE_FILL_RGB(3),SHAPE_STROKE_RGB(3),LINE_STROKE_RGB(3)

! Private variables:
REAL(SRK):: X_START,X_END,Y_START,Y_END,X_TICK_INTERVAL,Y_TICK_INTERVAL
INTEGER:: X_DECIMAL_PLACES,Y_DECIMAL_PLACES
LOGICAL:: SCIENTIFIC_X,SCIENTIFIC_Y

! ------------------------------------------------------------------------------

! Configure the axes automaticaly.
CALL AxesSetup(MINVAL(X_POINTS),MAXVAL(X_POINTS), &
    & X_START,X_END,X_TICK_INTERVAL,X_DECIMAL_PLACES,SCIENTIFIC_X)

CALL AxesSetup(MINVAL(Y_POINTS),MAXVAL(Y_POINTS), &
    & Y_START,Y_END,Y_TICK_INTERVAL,Y_DECIMAL_PLACES,SCIENTIFIC_Y)

! Execute subroutine "SVG_CartesianPlot" with a set of predefined options.
CALL SVG_CartesianPlot( &
OUTPUT_FILE=OUTPUT_FILE, &
X_POINTS=X_POINTS, &
Y_POINTS=Y_POINTS, &
X_TITLE=X_TITLE, &
Y_TITLE=Y_TITLE, &
X_START=X_START, &
X_END=X_END, &
Y_START=Y_START, &
Y_END=Y_END, &
X_DECIMAL_PLACES=X_DECIMAL_PLACES, &
Y_DECIMAL_PLACES=Y_DECIMAL_PLACES, &
SCIENTIFIC_X=SCIENTIFIC_X, &
SCIENTIFIC_Y=SCIENTIFIC_Y, &
X_TICK_INTERVAL=X_TICK_INTERVAL, &
Y_TICK_INTERVAL=Y_TICK_INTERVAL, &
DRAW_TICKS=.TRUE., &
TICK_LENGTH=10.0_SRK, &
TICK_WIDTH=3.0_SRK, &
DIAGRAM_TITLE=DIAGRAM_TITLE, &
DATA_TITLE=DATA_TITLE, &
TITLE_DIAGRAM_FONT_SIZE=40.0_SRK, &
TITLE_AXIS_FONT_SIZE=30.0_SRK, &
NUMBERS_AXIS_FONT_SIZE=25.0_SRK, &
FONT_RGB=(/0,0,0/), &
AXIS_STROKE_RGB=(/0,0,0/), &
AXIS_STROKE_WIDTH=3.0_SRK, &
DRAW_GRID=.FALSE., &
GRID_STROKE_WIDTH=2.0_SRK, &
GRID_STROKE_RGB=(/127,127,127/), &
SHAPE_TYPE=SHAPE_TYPE, &
SHAPE_SIZE=SHAPE_SIZE, &
SHAPE_STROKE_WIDTH=SHAPE_STROKE_WIDTH, &
SHAPE_FILL_RGB=SHAPE_FILL_RGB, &
SHAPE_STROKE_RGB=SHAPE_STROKE_RGB, &
LINE_TYPE=LINE_TYPE, &
LINE_STROKE_WIDTH=LINE_STROKE_WIDTH, &
LINE_STROKE_RGB=LINE_STROKE_RGB &
)

END SUBROUTINE SVG_CartesianPlotDefault

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Circle(OUTPUT_FILE,X,Y,RADIUS, &
    & STROKE_WIDTH,FILL_OPACITY,STROKE_RGB,FILL_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Circle is called to draw an ellipse in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, scalar. The x-axis coordinate of the ellipse center. (px).
! y: real, scalar. The y-axis coordinate of the ellipse center. (px).
! radius: real, scalar. The circle radius (px).
! stroke_width: real, scalar. The thickness of the ellipse line (px).
! fill_opacity: real, scalar. The opacity of the ellipse fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Circle(output_file,x,y,radius, &
!    & stroke_width,fill_opacity,stroke_rgb,fill_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x,y,radius,stroke_width,fill_opacity
!integer,intent(in):: stroke_rgb(3),fill_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Circle
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Circle(output_file='plot.svg', &
!    & x=200.0_srk,y=200.0_srk,radius=100.0_srk, &
!    & stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!    & rotate=(/15.0_srk,100.0_srk,100.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X,Y,RADIUS,STROKE_WIDTH,FILL_OPACITY
INTEGER,INTENT(IN):: STROKE_RGB(3),FILL_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((Y>VIEWBOX_HEIGHT).OR.(X>VIEWBOX_WIDTH)) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (RADIUS<0.0) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: invalid shape parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (RADIUS==0.0) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"WARNING: SVG object will not be viewable."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<FILL_OPACITY).OR.(FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: invalid paint parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FILL_OPACITY==0.0).AND.(STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(FILL_RGB<0)).OR.(ANY(STROKE_RGB<0)).OR. &
    & (ANY(FILL_RGB>255)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

! Rotating a circle may sound vain, but this is not true when a rotation centre
! other than its radius is selected.

WRITE(IO,'(A7)',IOSTAT=ERR)"<circle"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Circle"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename: ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"cx='"//TRIM(StringReal(X))//"'"
WRITE(IO,*)"cy='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y))//"'"
WRITE(IO,*)"r='"//TRIM(StringReal(RADIUS))//"'"
WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(FILL_RGB(1)))//","// &
        & TRIM(StringInteger(FILL_RGB(2)))//","// &
        & TRIM(StringInteger(FILL_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_Circle

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_ColorBar(OUTPUT_FILE,TITLE,X,Y,WIDTH,HEIGHT,MINIMUM,MAXIMUM, &
    & DECIMAL_PLACES,SCIENTIFIC,FONT,FONT_SIZE,FONT_RGB,GRAYSCALE,ID)

! SVG_ColorBar is called to draw a color bar in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! title: character, scalar. Preferably, the name of the variable that is
!    depicted by the color bar.
! x: real, scalar. The x-axis coordinate of the color bar center. (px).
! y: real, scalar. The y-axis coordinate of the color bar center. (px).
! width: real, scalar. The color bar width. (px).
! height: real, scalar. he color bar height. (px).
! minimum: real, scalar. The minimum value that will be used for the color
!    mapping. This value will be mapped to blue (0,0,255), or to white
!    (255,255,255) if the grayscale mode is on.
! maximum: real, scalar. The maximum value that will be used for color
!    mapping. This value will be mapped to red (255,0,0), or to black
!    (0,0,0) if the grayscale mode is on.
! decimal_places: integer, scalar. The amount of decimal places of the color bar
!    numbers.
! scientific: logical, scalar. Specifies whether the scientific notation will be
!    applied for the printing of the color bar numbers or not.
! font: character, scalar. The font that will be used for the rendering of the
!    bar's minimum and maximum values and the bar's title. The selected font
!    must be supported in the system that will be used to render the SVG
!    file.
! font_size: real, scalar. The size of the font.
! font_rgb: integer, array (1D) with 3 elements. Contains the font RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! grayscale: logical, scalar. Specifies whether the color mapping will be done
!    in grayscale or not. If ommitted, the default value is .false. (the
!    color mapping will be done between blue and red). If variable
!    "grayscale" is present and set to .true., the collor mapping will be
!    done between white and black.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_ColorBar(output_file,title,x,y,width,height,minimum,maximum, &
!    & decimal_places,scientific,font,font_size,font_rgb,grayscale,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),title*(*),font*(*)
!real(srk),intent(in):: x,y,width,height,minimum,maximum,font_size
!integer,intent(in):: font_rgb(3),decimal_places
!logical,intent(in):: scientific
!logical,optional,intent(in):: grayscale
!character,optional,intent(in):: id*(*)
!end subroutine SVG_ColorBar
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_ColorBar(output_file='plot.svg',title='velocity', &
!    & x=300.0_srk,y=400.0_srk, &
!    & width=100.0_srk,height=500.0_srk,minimum=0.0_srk,maximum=3.14_srk, &
!    & decimal_places=2,scientific=.false.,font='verdana', &
!    & font_size=35.0_srk,font_rgb=(/0,0,0/),grayscale=.false.,id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Parameters:
INTEGER,PARAMETER:: RESOLUTION=100,STROKE_RGB(3)=(/255,255,255/)
REAL(SRK),PARAMETER:: STROKE_WIDTH=0.0_SRK

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),TITLE*(*),FONT*(*)
REAL(SRK),INTENT(IN):: X,Y,WIDTH,HEIGHT,MINIMUM,MAXIMUM,FONT_SIZE
INTEGER,INTENT(IN):: FONT_RGB(3),DECIMAL_PLACES
LOGICAL,INTENT(IN):: SCIENTIFIC
LOGICAL,OPTIONAL,INTENT(IN):: GRAYSCALE
CHARACTER,OPTIONAL,INTENT(IN):: ID*(*)

! Private variables:
REAL(SRK):: X_START,Y_START,REAL_RESOLUTION,REAL_I,T,RECT_HEIGHT
LOGICAL:: INTERNAL_GRAYSCALE
INTEGER:: I

! Variable initialization:
REAL_RESOLUTION=REAL(RESOLUTION)
RECT_HEIGHT=HEIGHT/REAL_RESOLUTION
X_START=X
Y_START=Y-HEIGHT/2.0_SRK
IF (PRESENT(GRAYSCALE)) THEN
    INTERNAL_GRAYSCALE=GRAYSCALE
ELSE
    INTERNAL_GRAYSCALE=.FALSE.
END IF

! ------------------------------------------------------------------------------

! Error control:

IF (MINIMUM>=MAXIMUM) THEN
WRITE(*,*)"SVG_ColorBar"
WRITE(*,*)"ERROR: minimum value is greater than (or equal to)"
WRITE(*,*)"the maximum value."
WRITE(*,*)"minimum value: ",MINIMUM
WRITE(*,*)"maximum value: ",MAXIMUM
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_ColorBar"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

IF (PRESENT(ID)) THEN
    CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=ID)
ELSE
    CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE)
END IF

!Title:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=TRIM(TITLE), &
    & X=X, &
    & Y=Y+HEIGHT/2.0_SRK+3.0_SRK*FONT_SIZE/3.0_SRK, &
    & FONT=FONT,FONT_SIZE=FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB, &
    & BOLD=.TRUE.,ITALIC=.FALSE.,ANCHOR='middle')

! Lower end (blue):
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE, &
    & STRING=FormatReal(MINIMUM,DECIMAL_PLACES,SCIENTIFIC), &
    & X=X+WIDTH/2.0_SRK+FONT_SIZE/2.0_SRK, &
    & Y=Y-HEIGHT/2.0_SRK+FONT_SIZE/6.0_SRK, &
    & FONT=FONT,FONT_SIZE=FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB, &
    & BOLD=.FALSE.,ITALIC=.FALSE.,ANCHOR='start')
! Upper end (red):
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE, &
    & STRING=FormatReal(MAXIMUM,DECIMAL_PLACES,SCIENTIFIC), &
    & X=X+WIDTH/2.0_SRK+FONT_SIZE/2.0_SRK, &
    & Y=Y+HEIGHT/2.0_SRK-FONT_SIZE/2.0_SRK-FONT_SIZE/6.0_SRK, &
    & FONT=FONT,FONT_SIZE=FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB, &
    & BOLD=.FALSE.,ITALIC=.FALSE.,ANCHOR='start')

! Plot the color bar:
DO I=1,RESOLUTION

REAL_I=REAL(I)
T=LinearIntExp(1.0_SRK,REAL_RESOLUTION,MINIMUM,MAXIMUM,REAL_I)

CALL SVG_Rect(OUTPUT_FILE=OUTPUT_FILE, &
    & X=X_START,Y=Y_START+REAL_I*RECT_HEIGHT, &
    & WIDTH=WIDTH,HEIGHT=RECT_HEIGHT, &
    & STROKE_WIDTH=STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=STROKE_RGB,FILL_RGB= &
    & SVG_ColorMap(REAL_I,1.0_SRK,REAL_RESOLUTION,INTERNAL_GRAYSCALE), &
    & SCALAR_NAME=(/'respective_scalar_value'/),SCALAR_VALUE=(/T/))

END DO

CALL SVG_GroupEnd(OUTPUT_FILE=OUTPUT_FILE)

END SUBROUTINE SVG_ColorBar

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION SVG_ColorMap(NUMERICAL_VALUE,MINIMUM,MAXIMUM,GRAYSCALE)

! SVG_ColorMap returns an RGB color array based on a given numerical value.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! numerical_value: real, scalar. The numerical value based on which the the RGB
!    color will be generated.
! minimum: real, scalar. The minimum value that will be used for the color
!    mapping. This value will be mapped to blue (0,0,255), or to white
!    (255,255,255) if the grayscale mode is on.
! maximum: real, scalar. The maximum value that will be used for color
!    mapping. This value will be mapped to red (255,0,0), or to black
!    (0,0,0) if the grayscale mode is on.

! INPUT (OPTIONAL):
! grayscale: logical, scalar. Specifies whether the color mapping will be done
!    in grayscale or not. If ommitted, the default value is .false. (the
!    color mapping will be done between blue and red). If variable
!    "grayscale" is present and set to .true., the collor mapping will be
!    done between white and black.

! OUTPUT (REQUIRED):
! SVG_ColorMap: integer, array (1D) with 3 elements. The RGB values of the
!    generated color.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function SVG_ColorMap(numerical_value,minimum,maximum,grayscale)
!!use Config, only: srk
!implicit none
!real(srk),intent(in):: numerical_value,minimum,maximum
!logical,optional,intent(in):: grayscale
!integer:: SVG_ColorMap(3)
!end function SVG_ColorMap
!end interface

!integer:: i
!real(srk):: j
!call SVG_Initialize(output_file='plot.svg')
!do i=1,100
!j=i
!call SVG_Rect(output_file='plot.svg', &
!    & x=150.0_srk,y=5.0_srk*j,width=100.0_srk,height=5.0_srk, &
!    & stroke_width=1.0_srk,fill_opacity=1.0_srk, &
!    & stroke_rgb=(/255,255,255/), &
!    & fill_rgb=SVG_ColorMap(j,1.0_srk,100.0_srk), &
!    & scalar_name=(/'i_value'/),scalar_value=(/j/))
!call SVG_Rect(output_file='plot.svg', &
!    & x=350.0_srk,y=5.0_srk*j,width=100.0_srk,height=5.0_srk, &
!    & stroke_width=1.0_srk,fill_opacity=1.0_srk, &
!    & stroke_rgb=(/255,255,255/), &
!    & fill_rgb=SVG_ColorMap(j,1.0_srk,100.0_srk,grayscale=.true.), &
!    & scalar_name=(/'i_value'/),scalar_value=(/j/))
!end do
!call SVG_Finalize(output_file='plot.svg')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Argument variables:
REAL(SRK),INTENT(IN):: NUMERICAL_VALUE,MINIMUM,MAXIMUM
LOGICAL,OPTIONAL,INTENT(IN):: GRAYSCALE
INTEGER:: SVG_ColorMap(3)

! Private variables:
REAL(SRK):: T
LOGICAL:: INTERNAL_GRAYSCALE

! Variable intialization:
IF (PRESENT(GRAYSCALE)) THEN
    INTERNAL_GRAYSCALE=GRAYSCALE
ELSE
    INTERNAL_GRAYSCALE=.FALSE.
END IF

! ------------------------------------------------------------------------------

! Error control:

IF (MINIMUM>=MAXIMUM) THEN
WRITE(*,*)"SVG_ColorMap"
WRITE(*,*)"ERROR: minimum value is greater than (or equal to)"
WRITE(*,*)"the maximum value."
WRITE(*,*)"minimum value: ",MINIMUM
WRITE(*,*)"maximum value: ",MAXIMUM
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((NUMERICAL_VALUE>MAXIMUM).OR.(NUMERICAL_VALUE<MINIMUM)) THEN
WRITE(*,*)"SVG_ColorMap"
WRITE(*,*)"WARNING: given value is beyond bounds"
WRITE(*,*)"value provided: ",NUMERICAL_VALUE
WRITE(*,*)"minimum value: ",MINIMUM
WRITE(*,*)"maximum value: ",MAXIMUM
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

! ------------------------------------------------------------------------------

! The parametrization of the RGB color map is done using the following linear
! relations between the RGB values and the parameter 't'. This produces a
! color map where the maximum value is red and the minimum value is blue.

! ---------------------------------------------------
! R    G    B    t    equation
! ---------------------------------------------------
! 255    0    0    0.00    G =  1020 * t
! 255    255    0    0.25    R = -1020 * t + 510
! 0    255    0    0.50    B =  1020 * t - 510
! 0    255    255    0.75    G = -1020 * t + 1020
! 0    0    255    1.00    -
! ---------------------------------------------------

WITHIN_LIMITS: IF ((MINIMUM<=NUMERICAL_VALUE).AND. &
    & (NUMERICAL_VALUE<=MAXIMUM)) THEN

! The value of parameter 't' is calculated based on the given data.
    T=LinearIntExp(MAXIMUM,MINIMUM,0.0_SRK,1.0_SRK,NUMERICAL_VALUE)

IF ((T>1.0).OR.(T<0.0)) THEN
WRITE(*,*)"SVG_ColorMap"
WRITE(*,*)"ERROR: internal error."
WRITE(*,*)"t: ",T
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

    IF (INTERNAL_GRAYSCALE) THEN
! In grayscale the largest value is mapped to black, and the smaller to white.
        SVG_ColorMap=NINT(255.0_SRK*T)
    ELSE
! A color is attributed depending on the value of parameter 't'.
        IF ((0.0<=T).AND.(T<0.25)) THEN
            SVG_ColorMap(1)=255
            SVG_ColorMap(2)=NINT(1020.0_SRK*T)
            SVG_ColorMap(3)=0
        ELSE IF ((0.25<=T).AND.(T<0.50)) THEN
            SVG_ColorMap(1)=NINT(-1020.0_SRK*T+510.0_SRK)
            SVG_ColorMap(2)=255
            SVG_ColorMap(3)=0
        ELSE IF ((0.50<=T).AND.(T<0.75)) THEN
            SVG_ColorMap(1)=0
            SVG_ColorMap(2)=255
            SVG_ColorMap(3)=NINT(1020.0_SRK*T-510.0_SRK)
        ELSE IF ((0.75<=T).AND.(T<=1.0)) THEN
            SVG_ColorMap(1)=0
            SVG_ColorMap(2)=NINT(-1020.0_SRK*T+1020.0_SRK)
            SVG_ColorMap(3)=255
        END IF
    END IF

ELSE WITHIN_LIMITS

! If the given value is larger than the given maximum value the attributed
! color is black. If the given value is smaller than the given minimum value
! the attributed color is white. Note that such extremes are not viewable in
! grayscale.
    IF (MAXIMUM<NUMERICAL_VALUE) THEN
        SVG_ColorMap=0
    ELSE IF (NUMERICAL_VALUE<MINIMUM) THEN
        SVG_ColorMap=255
    END IF

END IF WITHIN_LIMITS

END FUNCTION SVG_ColorMap

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Cross(OUTPUT_FILE,X,Y,SIDE,STROKE_WIDTH,STROKE_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Cross is called to draw a cross in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, scalar. The x-axis coordinates of the cross center (px).
! y: real, scalar. The y-axis coordinates of the cross center (px).
! side: real, scalar. The length of the cross side. (px).
! stroke_width: real, scalar. The thickness of the cross lines (px).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Cross(output_file,x,y,side,stroke_width,stroke_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x,y,side,stroke_width
!integer,intent(in):: stroke_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Cross
!end interface

!call SVG_Initialize('plot.svg')
!call SVG_Cross(output_file='plot.svg', &
!    & x=300.0_srk,y=300.0_srk,side=100.0_srk, &
!    & stroke_width=3.0_srk,stroke_rgb=(/255,0,0/), &
!    & rotate=(/45.0_srk,300.0_srk,300.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize('plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X,Y,SIDE,STROKE_WIDTH
INTEGER,INTENT(IN):: STROKE_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((Y>VIEWBOX_HEIGHT).OR.(X>VIEWBOX_WIDTH)) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (STROKE_WIDTH==0.0) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"WARNING: this stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(STROKE_RGB<0)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

! Group the line elements. The group defines the stroke width and rgb values as
! well as the roation and scalar attributes.

WRITE(IO,'(A2)',IOSTAT=ERR)"<g"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Cross"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A1)')">"
WRITE(IO,*)

! ------------------------------------------------------------------------------

! Type the lines.
WRITE(IO,'(A5)')"<line"
WRITE(IO,*)"x1='"//TRIM(StringReal(X))//"'"
WRITE(IO,*)"y1='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y-SIDE/2.0_SRK))//"'"
WRITE(IO,*)"x2='"//TRIM(StringReal(X))//"'"
WRITE(IO,*)"y2='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y+SIDE/2.0_SRK))//"'"
WRITE(IO,'(A2)')"/>"
WRITE(IO,*)
WRITE(IO,'(A5)')"<line"
WRITE(IO,*)"x1='"//TRIM(StringReal(X-SIDE/2.0_SRK))//"'"
WRITE(IO,*)"y1='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y))//"'"
WRITE(IO,*)"x2='"//TRIM(StringReal(X+SIDE/2.0_SRK))//"'"
WRITE(IO,*)"y2='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y))//"'"
WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

! End the group.
WRITE(IO,'(A4)')"</g>"
WRITE(IO,*)

END SUBROUTINE SVG_Cross

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Ellipse(OUTPUT_FILE,X,Y,RADIUS_X,RADIUS_Y, &
    & STROKE_WIDTH,FILL_OPACITY,STROKE_RGB,FILL_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Ellipse is called to draw an ellipse in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, scalar. The x-axis coordinate of the ellipse center. (px).
! y: real, scalar. The y-axis coordinate of the ellipse center. (px).
! radius_x: real, scalar. The x-axis ellipse radius (px).
! radius_y: real, scalar. The y-axis ellipse radius (px).
! stroke_width: real, scalar. The thickness of the ellipse line (px).
! fill_opacity: real, scalar. The opacity of the ellipse fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Ellipse(output_file,x,y,radius_x,radius_y, &
!    & stroke_width,fill_opacity,stroke_rgb,fill_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x,y,radius_x,radius_y,stroke_width,fill_opacity
!integer,intent(in):: stroke_rgb(3),fill_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Ellipse
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Ellipse(output_file='plot.svg', &
!    & x=200.0_srk,y=200.0_srk,radius_x=100.0_srk,radius_y=200.0_srk, &
!    & stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!    & rotate=(/45.0_srk,200.0_srk,200.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X,Y,RADIUS_X,RADIUS_Y,STROKE_WIDTH,FILL_OPACITY
INTEGER,INTENT(IN):: STROKE_RGB(3),FILL_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((Y>VIEWBOX_HEIGHT).OR.(X>VIEWBOX_WIDTH)) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (ANY((/RADIUS_X,RADIUS_Y/)<0.0)) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: invalid shape parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/RADIUS_X,RADIUS_Y/)==0.0)) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"WARNING: SVG object will not be viewable."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<FILL_OPACITY).OR.(FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: invalid paint parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FILL_OPACITY==0.0).AND.(STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(FILL_RGB<0)).OR.(ANY(STROKE_RGB<0)).OR. &
    & (ANY(FILL_RGB>255)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A8)',IOSTAT=ERR)"<ellipse"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Ellipse"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename: ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"cx='"//TRIM(StringReal(X))//"'"
WRITE(IO,*)"cy='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y))//"'"
WRITE(IO,*)"rx='"//TRIM(StringReal(RADIUS_X))//"'"
WRITE(IO,*)"ry='"//TRIM(StringReal(RADIUS_Y))//"'"
WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(FILL_RGB(1)))//","// &
        & TRIM(StringInteger(FILL_RGB(2)))//","// &
        & TRIM(StringInteger(FILL_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_Ellipse

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Finalize(OUTPUT_FILE)

! SVG_Finalize is called to finalize an output SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!	the ".svg" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Finalize(output_file)
!implicit none
!character,intent(in):: output_file*(*)
!end subroutine SVG_Finalize
!end interface

!! Create an empty SVG document:
!call SVG_Initialize(output_file='plot.svg')
!call SVG_Finalize(output_file='plot.svg')

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

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Finalize"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Finalize"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Finalize"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A10)',IOSTAT=ERR)"<!--EOF-->"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Finalize"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,'(A6)')"</svg>"

CLOSE(IO)

END SUBROUTINE SVG_Finalize

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_GridPixel(OUTPUT_FILE,X,Y, &
    & RESOLUTION_X,RESOLUTION_Y,PIXEL_WIDTH,PIXEL_HEIGHT, &
    & LINE_WIDTH,LINE_RGB,NODE_SIZE,NODE_RGB, &
    & CELL_PROPERTY,CELL_PROPERTY_NAME,CELL_COLOR_SCHEME, &
    & NODE_PROPERTY,NODE_PROPERTY_NAME,NODE_COLOR_SCHEME, &
    & ID_CELLS,ID_NODES)

! SVG_GridPixel is called to draw a pixel grid in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, scalar. The x-axis coordinate of the grid lower left end. (px).
! y: real, scalar. The y-axis coordinate of the grid lower left end. (px).
! resolution_x: integer, scalar. The horizontal resolution of the grid.
! resolution_y: integer, scalar. The vertical resolution of the grid.
! pixel_width: real, scalar. The pixel width.
! pixel_height: real, scalar. The pixel height.
! line_width: real, scalar. The thickness of the line (px). If equal to zero,
!    no lines will appear.
! line_rgb: integer, array (1D) with 3 elements. Contains the line stroke RGB
!    channel values (dimensionless, 0<=line_rgb(:)<=255).
! node_size: real, scalar. The size of the node points (px). If equal to zero,
!    no nodes will appear.
! node_rgb: integer, array (1D) with 3 elements. Contains the node stroke RGB
!    channel values (dimensionless, 0<=node_rgb(:)<=255). When variable
!    'node_color_scheme' is present and different than "none", variable
!    "node_rgb" is overruled.

! INPUT (OPTIONAL):
! cell_property:  real, array (2D). The values of the array that will be mapped
!    on the lattice cells using the color scheme defined in variable
!    "cell_color_scheme". The values of the array will also be stored in the
!    SVG cell objects as XML variables. The size of the array in every
!    dimension must be one element smaller than the respective dimension size
!    of variables "x" and "y". The size of the array must be set to
!    (resolution_x,resolution_y). This variable may not be omitted when
!    variables "cell_property_name" and "cell_color_scheme" are included in
!    the arguments and vica versa.
! cell_property_name: character, scalar. The name of the non-SVG variable that
!    will be stored in the SVG object as XML variable and will be mapped
!    using the color scheme defined in variable "cell_color_scheme". This
!    variable may not be omitted when variables "cell_color_scheme" and
!    "cell_property" are included in the arguments and vica versa.
! cell_color_scheme: character, scalar. Defines the color scheme used to map the
!    scalar variable on the lattice cells. Supported values: "blue_red",
!    "grayscale", "none". This variable may not be omitted when variables
!    "cell_property" and "cell_property_name" are included in the arguments
!    and vica versa.
! node_property:  real, array (2D). The values of the array that will be mapped
!    on the lattice nodes using the color scheme defined in variable
!    "node_color_scheme". The values of the array will also be stored in the
!    SVG node objects as XML variables. The size of the array must be set to
!    (resolution_x+1,resolution_y+1). This variable may not be omitted when
!    variables "node_property_name" and "node_color_scheme" are included in
!    the arguments and vica versa.
! node_property_name: character, scalar. The name of the non-SVG variable that
!    will be stored in the SVG object as XML variable and will be mapped
!    using the color scheme defined in variable "node_color_scheme". This
!    variable may not be omitted when variables "node_color_scheme" and
!    "node_property" are included in the arguments and vica versa.
! node_color_scheme: character, scalar. Defines the color scheme used to map the
!    scalar variable on the lattice nodes. Supported values: "blue_red",
!    "grayscale", "none". This variable may not be omitted when variables
!    "node_property" and "node_property_name" are included in the arguments
!    and vica versa. When variable 'node_color_scheme' is present and
!    different than "none", variable "node_rgb" is overruled.
! id_cells: character, scalar. The id of the grid cells. The user is responsible
!    for the consistency of the ids within an SVG document.
! id_nodes: character, scalar. The id of the grid nodes. The user is responsible
!    for the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_GridPixel(output_file,x,y, &
!    & resolution_x,resolution_y,pixel_width,pixel_height, &
!    & line_width,line_rgb,node_size,node_rgb, &
!    & cell_property,cell_property_name,cell_color_scheme, &
!    & node_property,node_property_name,node_color_scheme, &
!    & id_cells,id_nodes)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x,y,pixel_width,pixel_height,line_width,node_size
!integer,intent(in):: resolution_x,resolution_y,line_rgb(3),node_rgb(3)
!character,optional,intent(in):: cell_property_name*(*),node_property_name*(*)
!character,optional,intent(in):: cell_color_scheme*(*),node_color_scheme*(*)
!real(srk),optional,intent(in):: cell_property(:,:),node_property(:,:)
!character,optional,intent(in):: id_cells*(*),id_nodes*(*)
!end subroutine SVG_GridPixel
!end interface

!integer,parameter:: res_x=3,res_y=4
!integer:: i,j
!real(srk):: cell_property(res_x,res_y),node(res_x+1,res_y+1)
!real(srk):: pixel_width=90.0_srk,pixel_height=120.0_srk
!node(1,1)=1.0 ; node(1,2)=2.0 ; node(1,3)=3.0 ; node(1,4)=4.0  ; node(1,5)=5.0
!node(2,1)=1.0 ; node(2,2)=2.0 ; node(2,3)=3.0 ; node(2,4)=4.0  ; node(2,5)=5.0
!node(3,1)=1.0 ; node(3,2)=2.0 ; node(3,3)=3.0 ; node(3,4)=4.0  ; node(3,5)=5.0
!node(4,1)=1.0 ; node(4,2)=2.0 ; node(4,3)=3.0 ; node(4,4)=4.0  ; node(4,5)=5.0
!forall (i=1:res_x,j=1:res_y) cell_property(i,j)=sum(node(i:i+1,j:j+1))/4.0_srk
!call SVG_Initialize(output_file='plot.svg')
!! Print grid:
!call SVG_GridPixel(output_file='plot.svg',x=200.0_srk,y=200.0_srk, &
!    & resolution_x=res_x,resolution_y=res_y, &
!    & pixel_width=pixel_width,pixel_height=pixel_height, &
!    & line_width=3.0_srk,line_rgb=(/0,0,0/), &
!    & node_size=0.0_srk,node_rgb=(/255,0,255/), &
!    & cell_property=cell_property, &
!    & cell_property_name='cell_velocity_average', &
!    & cell_color_scheme='blue_red', &
!    & node_property=node,node_property_name='velocity', &
!    & node_color_scheme='none')
!! Print pixel centers:
!call SVG_GridPixel(output_file='plot.svg', &
!    & x=200.0_srk+pixel_width/2.0_srk,y=200.0_srk+pixel_height/2.0_srk, &
!    & resolution_x=res_x-1,resolution_y=res_y-1, &
!    & pixel_width=pixel_width,pixel_height=pixel_height, &
!    & line_width=0.0_srk,line_rgb=(/255,255,255/), &
!    & node_size=6.0_srk,node_rgb=(/255,255,255/))
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X,Y,PIXEL_WIDTH,PIXEL_HEIGHT,LINE_WIDTH,NODE_SIZE
INTEGER,INTENT(IN):: RESOLUTION_X,RESOLUTION_Y,LINE_RGB(3),NODE_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: CELL_PROPERTY_NAME*(*),NODE_PROPERTY_NAME*(*)
CHARACTER,OPTIONAL,INTENT(IN):: CELL_COLOR_SCHEME*(*),NODE_COLOR_SCHEME*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: CELL_PROPERTY(:,:),NODE_PROPERTY(:,:)
CHARACTER,OPTIONAL,INTENT(IN):: ID_CELLS*(*),ID_NODES*(*)

! Private variables:
INTEGER:: I,J,COLOR(3)
LOGICAL:: CELL_GRAYSCALE,NODE_GRAYSCALE
REAL(SRK):: CELL_MINIMUM,CELL_MAXIMUM,NODE_MINIMUM,NODE_MAXIMUM,OPACITY
REAL(SRK):: X_ARRAY(RESOLUTION_X+1,RESOLUTION_Y+1)
REAL(SRK):: Y_ARRAY(RESOLUTION_X+1,RESOLUTION_Y+1)

! Variable initialization:
IF (PRESENT(CELL_COLOR_SCHEME)) THEN
    CELL_MINIMUM=MINVAL(CELL_PROPERTY)
    CELL_MAXIMUM=MAXVAL(CELL_PROPERTY)
    IF (TRIM(CELL_COLOR_SCHEME)=='grayscale') THEN
        CELL_GRAYSCALE=.TRUE.
    ELSE
        CELL_GRAYSCALE=.FALSE.
    END IF
END IF
IF (PRESENT(NODE_COLOR_SCHEME)) THEN
    NODE_MINIMUM=MINVAL(NODE_PROPERTY)
    NODE_MAXIMUM=MAXVAL(NODE_PROPERTY)
    IF (TRIM(NODE_COLOR_SCHEME)=='grayscale') THEN
        NODE_GRAYSCALE=.TRUE.
    ELSE
        NODE_GRAYSCALE=.FALSE.
    END IF
END IF

! Calculate the coordinates of the pixel borders:
X_ARRAY(1,:)=X
DO I=2,RESOLUTION_X+1
    X_ARRAY(I,:)=X_ARRAY(I-1,:)+PIXEL_WIDTH
END DO
Y_ARRAY(:,1)=Y
DO I=2,RESOLUTION_Y+1
    Y_ARRAY(:,I)=Y_ARRAY(:,I-1)+PIXEL_HEIGHT
END DO

! ------------------------------------------------------------------------------

! Error control:

IF (ANY((/RESOLUTION_X,RESOLUTION_Y/)<=0)) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"ERROR: invalid resolution."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/PIXEL_WIDTH,PIXEL_HEIGHT/)<=0.0)) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"ERROR: invalid pixel dimensions."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(NODE_PROPERTY_NAME)).AND.((.NOT.(PRESENT(NODE_COLOR_SCHEME))).OR. &
    (.NOT.(PRESENT(NODE_PROPERTY))))) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"ERROR: variable 'node_property_name' may not appear in the arguments"
WRITE(*,*)"without variables 'node_color_scheme' and 'node_property'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(NODE_COLOR_SCHEME)).AND.((.NOT.(PRESENT(NODE_PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(NODE_PROPERTY))))) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"ERROR: variable 'node_color_scheme' may not appear in the arguments"
WRITE(*,*)"without variables 'node_property_name' and 'node_property'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(NODE_PROPERTY)).AND.((.NOT.(PRESENT(NODE_PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(NODE_COLOR_SCHEME))))) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"ERROR: variable 'node_property' may not appear in the arguments"
WRITE(*,*)"without variables 'node_property_name' and 'node_color_scheme'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(CELL_PROPERTY_NAME)).AND.((.NOT.(PRESENT(CELL_COLOR_SCHEME))).OR. &
    (.NOT.(PRESENT(CELL_PROPERTY))))) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"ERROR: variable 'cell_property_name' may not appear in the arguments"
WRITE(*,*)"without variables 'cell_color_scheme' and 'cell_property'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(CELL_COLOR_SCHEME)).AND.((.NOT.(PRESENT(CELL_PROPERTY))).OR. &
    (.NOT.(PRESENT(CELL_PROPERTY_NAME))))) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"ERROR: variable 'cell_color_scheme' may not appear in the arguments"
WRITE(*,*)"without variables 'cell_property' and 'cell_property_name'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(CELL_PROPERTY)).AND.((.NOT.(PRESENT(CELL_PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(CELL_COLOR_SCHEME))))) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"ERROR: variable 'cell_property' may not appear in the arguments"
WRITE(*,*)"without variables 'cell_property_name' and 'cell_color_scheme'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT(NODE_COLOR_SCHEME)) THEN
!Edit this IF block in order to support more color schemes.
    IF (ALL(TRIM(NODE_COLOR_SCHEME)/= &
        & (/'blue_red ','grayscale','none     '/))) THEN
    WRITE(*,*)"SVG_GridPixel"
    WRITE(*,*)"ERROR: unsupported color scheme for grid nodes."
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF
END IF

IF (PRESENT(CELL_COLOR_SCHEME)) THEN

    IF ((LINE_WIDTH==0.0).AND.(NODE_SIZE==0.0).AND. &
        & (CELL_COLOR_SCHEME=='none')) THEN
    WRITE(*,*)"SVG_GridPixel"
    WRITE(*,*)"WARNING: this combination of variables 'line_width',"
    WRITE(*,*)"'node_size' and 'cell_color_scheme' will render the lattice"
    WRITE(*,*)"invisible."
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

!Edit this IF block in order to support more color schemes.
    IF (ALL(TRIM(CELL_COLOR_SCHEME)/= &
        & (/'blue_red ','grayscale','none     '/))) THEN
    WRITE(*,*)"SVG_GridPixel"
    WRITE(*,*)"ERROR: unsupported color scheme for grid cells."
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

END IF

IF (PRESENT(NODE_PROPERTY)) THEN

    IF (LEN_TRIM(NODE_PROPERTY_NAME)==0) THEN
    WRITE(*,*)"SVG_GridPixel"
    WRITE(*,*)"WARNING: no node property name provided."
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF (ALL(NODE_PROPERTY==0)) THEN
    WRITE(*,*)"SVG_GridPixel"
    WRITE(*,*)"WARNING: all node_property points are equal to zero"
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF ((RESOLUTION_X+1/=SIZE(NODE_PROPERTY,DIM=1)).OR. &
        & (RESOLUTION_Y+1/=SIZE(NODE_PROPERTY,DIM=2))) THEN
    WRITE(*,*)"SVG_GridPixel"
    WRITE(*,*)"ERROR: false matrix resolution."
    WRITE(*,*)"resolution x: ",RESOLUTION_X
    WRITE(*,*)"resolution_y: ",RESOLUTION_Y
    WRITE(*,*)"size(node_property,dim=1): ",SIZE(NODE_PROPERTY,DIM=1)
    WRITE(*,*)"size(node_property,dim=2): ",SIZE(NODE_PROPERTY,DIM=2)
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

END IF

IF (PRESENT(CELL_PROPERTY)) THEN

    IF (LEN_TRIM(CELL_PROPERTY_NAME)==0) THEN
    WRITE(*,*)"SVG_GridPixel"
    WRITE(*,*)"WARNING: no cell property name provided."
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF (ALL(CELL_PROPERTY==0)) THEN
    WRITE(*,*)"SVG_GridPixel"
    WRITE(*,*)"WARNING: all node_property points are equal to zero"
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF ((RESOLUTION_X/=SIZE(CELL_PROPERTY,DIM=1)).OR. &
        & (RESOLUTION_Y/=SIZE(CELL_PROPERTY,DIM=2))) THEN
    WRITE(*,*)"SVG_GridPixel"
    WRITE(*,*)"ERROR: false matrix resolution."
    WRITE(*,*)"resolution x: ",RESOLUTION_X
    WRITE(*,*)"resolution_y: ",RESOLUTION_Y
    WRITE(*,*)"size(cell_property,dim=1): ",SIZE(CELL_PROPERTY,DIM=1)
    WRITE(*,*)"size(cell_property,dim=2): ",SIZE(CELL_PROPERTY,DIM=2)
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

END IF

IF (PRESENT(ID_NODES)) THEN
IF (LEN_TRIM(ID_NODES)==0) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"WARNING: invalid node id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

IF (PRESENT(ID_CELLS)) THEN
IF (LEN_TRIM(ID_CELLS)==0) THEN
WRITE(*,*)"SVG_GridPixel"
WRITE(*,*)"WARNING: invalid cell id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

! Draw the pixel cells.

COLOR=(/255,255,255/) ! Variable initialization

! Grid cells will be colored only if there is information to be displayed or
! stored in them.
IF (PRESENT(CELL_PROPERTY)) THEN
OPACITY=1.0
ELSE
OPACITY=0.0
END IF

IF ((LINE_WIDTH/=0.0).OR.(OPACITY/=0.0)) THEN

    IF (PRESENT(ID_CELLS)) THEN
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=ID_CELLS)
    ELSE
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE)
    END IF

    DO I=1,RESOLUTION_X
    DO J=1,RESOLUTION_Y
        IF (TRIM(CELL_COLOR_SCHEME)/='none') COLOR= &
            & SVG_ColorMap(CELL_PROPERTY(I,J), &
            & CELL_MINIMUM,CELL_MAXIMUM,CELL_GRAYSCALE)
! The cell color is the one specified by the used color scheme, defined by the
! input variable "cell_color_scheme". An input-defined color is not possible.
        CALL SVG_Rect(OUTPUT_FILE=OUTPUT_FILE, &
            & X=X_ARRAY(I,J)+PIXEL_WIDTH/2.0_SRK, &
            & Y=Y_ARRAY(I,J)+PIXEL_HEIGHT/2.0_SRK, &
            & WIDTH=PIXEL_WIDTH,HEIGHT=PIXEL_HEIGHT, &
            & STROKE_WIDTH=LINE_WIDTH,FILL_OPACITY=OPACITY, &
            & STROKE_RGB=LINE_RGB,FILL_RGB=COLOR, &
            & SCALAR_NAME=(/TRIM(CELL_PROPERTY_NAME)/), &
            & SCALAR_VALUE=(/CELL_PROPERTY(I,J)/))
    END DO
    END DO

    CALL SVG_GroupEnd(OUTPUT_FILE=OUTPUT_FILE)

END IF

! ------------------------------------------------------------------------------

! Draw the node points.

! Node points will not be drawn only if explicitly told so through input
! variable "node_size". Therefore, there is no ELSE in this IF-BLOCK.
IF (NODE_SIZE/=0.0) THEN

    COLOR=NODE_RGB ! Variable initialization

    IF (PRESENT(ID_NODES)) THEN
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=ID_NODES)
    ELSE
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE)
    END IF

    DO I=1,RESOLUTION_X+1
    DO J=1,RESOLUTION_Y+1

! Node points will be drawn regardless of whether there is information stored in
! them and displayed through variables "node_property", "node_property_name",
! and "node_color_scheme" or not. Therefore, there is an ELSE in this IF-BLOCK.
    IF (PRESENT(NODE_PROPERTY)) THEN

        IF (TRIM(NODE_COLOR_SCHEME)/='none') COLOR= &
            & SVG_ColorMap(NODE_PROPERTY(I,J), &
            & NODE_MINIMUM,NODE_MAXIMUM,NODE_GRAYSCALE)

! The node color is the one specified by the used color scheme, defined by the
! input variable "node_color_scheme".
        CALL SVG_Circle(OUTPUT_FILE=OUTPUT_FILE, &
            & X=X_ARRAY(I,J),Y=Y_ARRAY(I,J), &
            & RADIUS=NODE_SIZE, &
            & STROKE_WIDTH=0.0_SRK,FILL_OPACITY=1.0_SRK, &
            & STROKE_RGB=COLOR,FILL_RGB=COLOR, &
            & SCALAR_NAME=(/NODE_PROPERTY_NAME/), &
            & SCALAR_VALUE=(/NODE_PROPERTY(I,J)/))

    ELSE

! The node color is the one defined in the subroutine arguments through input
! variable "node_rgb".
        CALL SVG_Circle(OUTPUT_FILE=OUTPUT_FILE, &
            & X=X_ARRAY(I,J),Y=Y_ARRAY(I,J), &
            & RADIUS=NODE_SIZE, &
            & STROKE_WIDTH=0.0_SRK,FILL_OPACITY=1.0_SRK, &
            & STROKE_RGB=NODE_RGB,FILL_RGB=NODE_RGB)

    END IF

    END DO
    END DO

    CALL SVG_GroupEnd(OUTPUT_FILE=OUTPUT_FILE)

END IF

END SUBROUTINE SVG_GridPixel

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_GridStructured(OUTPUT_FILE,X,Y, &
    & LINE_WIDTH,LINE_RGB,NODE_SIZE,NODE_RGB, &
    & CELL_PROPERTY,CELL_PROPERTY_NAME,CELL_COLOR_SCHEME, &
    & NODE_PROPERTY,NODE_PROPERTY_NAME,NODE_COLOR_SCHEME, &
    & ID_LINES,ID_CELLS,ID_NODES)

! SVG_GridStructured is called to draw a structured grid in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, array (2D). The x-axis coordinates of the lattice nodes. (px). The
!    elements are in 1-1 correspondance with the elements of array "y".
! y: real, array (2D). The y-axis coordinates of the lattice nodes. (px). The
!    elements are in 1-1 correspondance with the elements of array "x".
! line_width: real, scalar. The thickness of the line (px). If equal to zero,
!    no lines will appear.
! line_rgb: integer, array (1D) with 3 elements. Contains the line stroke RGB
!    channel values (dimensionless, 0<=line_rgb(:)<=255).
! node_size: real, scalar. The size of the node points (px). If equal to zero,
!    no nodes will appear.
! node_rgb: integer, array (1D) with 3 elements. Contains the node stroke RGB
!    channel values (dimensionless, 0<=node_rgb(:)<=255). When variable
!    'node_color_scheme' is present and different than "none", variable
!    "node_rgb" is overruled.

! INPUT (OPTIONAL):
! cell_property:  real, array (2D). The values of the array that will be mapped
!    on the lattice cells using the color scheme defined in variable
!    "cell_color_scheme". The values of the array will also be stored in the
!    SVG cell objects as XML variables. The size of the array in every
!    dimension must be one element smaller than the respective dimension size
!    of variables "x" and "y". This variable may not be omitted when
!    variables "cell_property_name" and "cell_color_scheme" are included in
!    the arguments and vica versa.
! cell_property_name: character, scalar. The name of the non-SVG variable that
!    will be stored in the SVG object as XML variable and will be mapped
!    using the color scheme defined in variable "cell_color_scheme". This
!    variable may not be omitted when variables "cell_color_scheme" and
!    "cell_property" are included in the arguments and vica versa.
! cell_color_scheme: character, scalar. Defines the color scheme used to map the
!    scalar variable on the lattice cells. Supported values: "blue_red",
!    "grayscale", "none". This variable may not be omitted when variables
!    "cell_property" and "cell_property_name" are included in the arguments
!    and vica versa.
! node_property:  real, array (2D). The values of the array that will be mapped
!    on the lattice nodes using the color scheme defined in variable
!    "node_color_scheme". The values of the array will also be stored in the
!    SVG node objects as XML variables. The size of the array must be equal
!    to the size of variables "x" and "y" and their elements are 1-1
!    corresponding. This variable may not be omitted when variables
!    "node_property_name" and "node_color_scheme" are included in the
!    arguments and vica versa.
! node_property_name: character, scalar. The name of the non-SVG variable that
!    will be stored in the SVG object as XML variable and will be mapped
!    using the color scheme defined in variable "node_color_scheme". This
!    variable may not be omitted when variables "node_color_scheme" and
!    "node_property" are included in the arguments and vica versa.
! node_color_scheme: character, scalar. Defines the color scheme used to map the
!    scalar variable on the lattice nodes. Supported values: "blue_red",
!    "grayscale", "none". This variable may not be omitted when variables
!    "node_property" and "node_property_name" are included in the arguments
!    and vica versa. When variable 'node_color_scheme' is present and
!    different than "none", variable "node_rgb" is overruled.
! id_lines: character, scalar. The id of the grid lines. The user is responsible
!    for the consistency of the ids within an SVG document.
! id_cells: character, scalar. The id of the grid cells. The user is responsible
!    for the consistency of the ids within an SVG document.
! id_nodes: character, scalar. The id of the grid nodes. The user is responsible
!    for the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_GridStructured(output_file,x,y, &
!    & line_width,line_rgb,node_size,node_rgb, &
!    & cell_property,cell_property_name,cell_color_scheme, &
!    & node_property,node_property_name,node_color_scheme, &
!    & id_lines,id_cells,id_nodes)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x(:,:),y(:,:),line_width,node_size
!integer,intent(in):: line_rgb(3),node_rgb(3)
!character,optional,intent(in):: cell_property_name*(*),node_property_name*(*)
!character,optional,intent(in):: cell_color_scheme*(*),node_color_scheme*(*)
!real(srk),optional,intent(in):: cell_property(:,:),node_property(:,:)
!character,optional,intent(in):: id_lines*(*),id_cells*(*),id_nodes*(*)
!end subroutine SVG_GridStructured
!end interface

!real(srk):: x(4,5),y(4,5),node(4,5),cell_property(3,4)
!integer:: i,j
!x(1,1)=60.44  ; y(1,1)=328.63 ; x(1,2)=151.09 ; y (1,2)=351.29 ; x(1,3)=308.80
!y(1,3)=347.51 ; x(1,4)=384.34 ; y(1,4)=357.90 ; x (1,5)=466.50 ; y(1,5)=355.07
!x(2,1)=129.37 ; y(2,1)=247.41 ; x(2,2)=239.86 ; y (2,2)=275.74 ; x(2,3)=329.57
!y(2,3)=259.69 ; x(2,4)=392.84 ; y(2,4)=297.46 ; x (2,5)=488.22 ; y(2,5)=247.41
!x(3,1)=128.43 ; y(3,1)=141.65 ; x(3,2)=220.03 ; y (3,2)=173.76 ; x(3,3)=297.46
!y(3,3)=196.42 ; x(3,4)=400.40 ; y(3,4)=176.59 ; x (3,5)=482.55 ; y(3,5)=169.98
!x(4,1)=118.99 ; y(4,1)=68.94  ; x(4,2)=190.75 ; y (4,2)=80.27  ; x(4,3)=315.41
!y(4,3)=91.60  ; x(4,4)=401.34 ; y(4,4)=87.82  ; x (4,5)=475.94 ; y(4,5)=84.05
!node(1,1)=1.0 ; node(1,2)=2.0 ; node(1,3)=3.0 ; node(1,4)=4.0  ; node(1,5)=5.0
!node(2,1)=1.0 ; node(2,2)=2.0 ; node(2,3)=3.0 ; node(2,4)=4.0  ; node(2,5)=5.0
!node(3,1)=1.0 ; node(3,2)=2.0 ; node(3,3)=3.0 ; node(3,4)=4.0  ; node(3,5)=5.0
!node(4,1)=1.0 ; node(4,2)=2.0 ; node(4,3)=3.0 ; node(4,4)=4.0  ; node(4,5)=5.0
!forall (i=1:size(x,dim=1)-1,j=1:size(x,dim=2)-1) &
!    & cell_property(i,j)=sum(node(i:i+1,j:j+1))/4.0_srk
!call SVG_Initialize(output_file='plot.svg')
!call SVG_GridStructured(output_file='plot.svg',x=x,y=y+200.0_srk, &
!    & line_width=2.0_srk,line_rgb=(/0,0,0/), &
!    & node_size=6.0_srk,node_rgb=(/255,0,255/), &
!    & cell_property=cell_property, &
!    & cell_property_name='cell_velocity_average', &
!    & cell_color_scheme='blue_red', &
!    & node_property=node,node_property_name='velocity', &
!    & node_color_scheme='none')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X(:,:),Y(:,:),LINE_WIDTH,NODE_SIZE
INTEGER,INTENT(IN):: LINE_RGB(3),NODE_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: CELL_PROPERTY_NAME*(*),NODE_PROPERTY_NAME*(*)
CHARACTER,OPTIONAL,INTENT(IN):: CELL_COLOR_SCHEME*(*),NODE_COLOR_SCHEME*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: CELL_PROPERTY(:,:),NODE_PROPERTY(:,:)
CHARACTER,OPTIONAL,INTENT(IN):: ID_LINES*(*),ID_CELLS*(*),ID_NODES*(*)

! Private variables:
INTEGER:: I,J,COLOR(3)
LOGICAL:: CELL_GRAYSCALE,NODE_GRAYSCALE
REAL(SRK):: CELL_MINIMUM,CELL_MAXIMUM,NODE_MINIMUM,NODE_MAXIMUM
REAL(SRK):: POLYGON_VECTOR_Y(4),POLYGON_VECTOR_X(4)

! Variable initialization:
IF (PRESENT(CELL_COLOR_SCHEME)) THEN
    CELL_MINIMUM=MINVAL(CELL_PROPERTY)
    CELL_MAXIMUM=MAXVAL(CELL_PROPERTY)
    IF (TRIM(CELL_COLOR_SCHEME)=='grayscale') THEN
        CELL_GRAYSCALE=.TRUE.
    ELSE
        CELL_GRAYSCALE=.FALSE.
    END IF
END IF
IF (PRESENT(NODE_COLOR_SCHEME)) THEN
    NODE_MINIMUM=MINVAL(NODE_PROPERTY)
    NODE_MAXIMUM=MAXVAL(NODE_PROPERTY)
    IF (TRIM(NODE_COLOR_SCHEME)=='grayscale') THEN
        NODE_GRAYSCALE=.TRUE.
    ELSE
        NODE_GRAYSCALE=.FALSE.
    END IF
END IF

! ------------------------------------------------------------------------------

! Error control:

IF ((SIZE(X,DIM=1)/=SIZE(Y,DIM=1)).OR.(SIZE(X,DIM=2)/=SIZE(Y,DIM=2))) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"ERROR: false matrix resolution."
WRITE(*,*)"size(x,dim=1): ",SIZE(X,DIM=1)
WRITE(*,*)"size(x,dim=2): ",SIZE(X,DIM=2)
WRITE(*,*)"size(y,dim=1): ",SIZE(Y,DIM=1)
WRITE(*,*)"size(y,dim=2): ",SIZE(Y,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(NODE_PROPERTY_NAME)).AND.((.NOT.(PRESENT(NODE_COLOR_SCHEME))).OR. &
    (.NOT.(PRESENT(NODE_PROPERTY))))) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"ERROR: variable 'node_property_name' may not appear in the arguments"
WRITE(*,*)"without variables 'node_color_scheme' and 'node_property'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(NODE_COLOR_SCHEME)).AND.((.NOT.(PRESENT(NODE_PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(NODE_PROPERTY))))) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"ERROR: variable 'node_color_scheme' may not appear in the arguments"
WRITE(*,*)"without variables 'node_property_name' and 'node_property'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(NODE_PROPERTY)).AND.((.NOT.(PRESENT(NODE_PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(NODE_COLOR_SCHEME))))) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"ERROR: variable 'node_property' may not appear in the arguments"
WRITE(*,*)"without variables 'node_property_name' and 'node_color_scheme'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(CELL_PROPERTY_NAME)).AND.((.NOT.(PRESENT(CELL_COLOR_SCHEME))).OR. &
    (.NOT.(PRESENT(CELL_PROPERTY))))) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"ERROR: variable 'cell_property_name' may not appear in the arguments"
WRITE(*,*)"without variables 'cell_color_scheme' and 'cell_property'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(CELL_COLOR_SCHEME)).AND.((.NOT.(PRESENT(CELL_PROPERTY))).OR. &
    (.NOT.(PRESENT(CELL_PROPERTY_NAME))))) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"ERROR: variable 'cell_color_scheme' may not appear in the arguments"
WRITE(*,*)"without variables 'cell_property' and 'cell_property_name'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(CELL_PROPERTY)).AND.((.NOT.(PRESENT(CELL_PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(CELL_COLOR_SCHEME))))) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"ERROR: variable 'cell_property' may not appear in the arguments"
WRITE(*,*)"without variables 'cell_property_name' and 'cell_color_scheme'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT(NODE_COLOR_SCHEME)) THEN
!Edit this IF block in order to support more color schemes.
    IF (ALL(TRIM(NODE_COLOR_SCHEME)/= &
        & (/'blue_red ','grayscale','none     '/))) THEN
    WRITE(*,*)"SVG_GridStructured"
    WRITE(*,*)"ERROR: unsupported color scheme for grid nodes."
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF
END IF

IF (PRESENT(CELL_COLOR_SCHEME)) THEN

    IF ((LINE_WIDTH==0.0).AND.(NODE_SIZE==0.0).AND. &
        & (CELL_COLOR_SCHEME=='none')) THEN
    WRITE(*,*)"SVG_GridStructured"
    WRITE(*,*)"WARNING: this combination of variables 'line_width',"
    WRITE(*,*)"'node_size' and 'cell_color_scheme' will render the lattice"
    WRITE(*,*)"invisible."
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

!Edit this IF block in order to support more color schemes.
    IF (ALL(TRIM(CELL_COLOR_SCHEME)/= &
        & (/'blue_red ','grayscale','none     '/))) THEN
    WRITE(*,*)"SVG_GridStructured"
    WRITE(*,*)"ERROR: unsupported color scheme for grid cells."
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

END IF

IF (PRESENT(NODE_PROPERTY)) THEN

    IF (LEN_TRIM(NODE_PROPERTY_NAME)==0) THEN
    WRITE(*,*)"SVG_GridStructured"
    WRITE(*,*)"WARNING: no node property name provided."
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF (ALL(NODE_PROPERTY==0)) THEN
    WRITE(*,*)"SVG_GridStructured"
    WRITE(*,*)"WARNING: all node_property points are equal to zero"
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF ((SIZE(X,DIM=1)/=SIZE(NODE_PROPERTY,DIM=1)).OR. &
        & (SIZE(X,DIM=2)/=SIZE(NODE_PROPERTY,DIM=2))) THEN
    WRITE(*,*)"SVG_GridStructured"
    WRITE(*,*)"ERROR: false matrix resolution."
    WRITE(*,*)"size(x,dim=1): ",SIZE(X,DIM=1)
    WRITE(*,*)"size(x,dim=2): ",SIZE(X,DIM=2)
    WRITE(*,*)"size(node_property,dim=1): ",SIZE(NODE_PROPERTY,DIM=1)
    WRITE(*,*)"size(node_property,dim=2): ",SIZE(NODE_PROPERTY,DIM=2)
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

END IF

IF (PRESENT(CELL_PROPERTY)) THEN

    IF (LEN_TRIM(CELL_PROPERTY_NAME)==0) THEN
    WRITE(*,*)"SVG_GridStructured"
    WRITE(*,*)"WARNING: no cell property name provided."
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF (ALL(CELL_PROPERTY==0)) THEN
    WRITE(*,*)"SVG_GridStructured"
    WRITE(*,*)"WARNING: all node_property points are equal to zero"
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF ((SIZE(X,DIM=1)-1/=SIZE(CELL_PROPERTY,DIM=1)).OR. &
        & (SIZE(X,DIM=2)-1/=SIZE(CELL_PROPERTY,DIM=2))) THEN
    WRITE(*,*)"SVG_GridStructured"
    WRITE(*,*)"ERROR: false matrix resolution."
    WRITE(*,*)"size(x,dim=1)-1: ",SIZE(X,DIM=1)-1
    WRITE(*,*)"size(x,dim=2)-1: ",SIZE(X,DIM=2)-1
    WRITE(*,*)"size(cell_property,dim=1): ",SIZE(CELL_PROPERTY,DIM=1)
    WRITE(*,*)"size(cell_property,dim=2): ",SIZE(CELL_PROPERTY,DIM=2)
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

END IF

IF ((ALL(X==0)).OR.(ALL(Y==0))) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"WARNING: all coordinate points are equal to zero"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (PRESENT(ID_LINES)) THEN
IF (LEN_TRIM(ID_LINES)==0) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"WARNING: invalid line id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

IF (PRESENT(ID_NODES)) THEN
IF (LEN_TRIM(ID_NODES)==0) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"WARNING: invalid node id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

IF (PRESENT(ID_CELLS)) THEN
IF (LEN_TRIM(ID_CELLS)==0) THEN
WRITE(*,*)"SVG_GridStructured"
WRITE(*,*)"WARNING: invalid cell id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

! Draw the cells.

! Grid cells will be drawn only if there is information to be displayed or
! stored in them. Therefore, there is no ELSE in this IF-BLOCK.
IF (PRESENT(CELL_PROPERTY)) THEN

    COLOR=(/255,255,255/) ! Variable initialization

    IF (PRESENT(ID_CELLS)) THEN
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=ID_CELLS)
    ELSE
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE)
    END IF

    DO I=1,SIZE(X,DIM=1)-1
    DO J=1,SIZE(X,DIM=2)-1

        POLYGON_VECTOR_X(1)=X(I,J)
        POLYGON_VECTOR_X(2)=X(I,J+1)
        POLYGON_VECTOR_X(3)=X(I+1,J+1)
        POLYGON_VECTOR_X(4)=X(I+1,J)
        POLYGON_VECTOR_Y(1)=Y(I,J)
        POLYGON_VECTOR_Y(2)=Y(I,J+1)
        POLYGON_VECTOR_Y(3)=Y(I+1,J+1)
        POLYGON_VECTOR_Y(4)=Y(I+1,J)


        IF (TRIM(CELL_COLOR_SCHEME)/='none') COLOR= &
            & SVG_ColorMap(CELL_PROPERTY(I,J), &
            & CELL_MINIMUM,CELL_MAXIMUM,CELL_GRAYSCALE)

! The cell color is the one specified by the used color scheme, defined by the
! input variable "cell_color_scheme". An input-defined color is not possible.
        CALL SVG_Polygon(OUTPUT_FILE=OUTPUT_FILE, &
            & X=POLYGON_VECTOR_X,Y=POLYGON_VECTOR_Y, &
            & STROKE_WIDTH=0.0_SRK,FILL_OPACITY=1.0_SRK, &
            & STROKE_RGB=(/255,255,255/), &
            & FILL_RGB=COLOR, &
            & SCALAR_NAME=(/TRIM(CELL_PROPERTY_NAME)/), &
            & SCALAR_VALUE=(/CELL_PROPERTY(I,J)/))

    END DO
    END DO

    CALL SVG_GroupEnd(OUTPUT_FILE=OUTPUT_FILE)

END IF

! ------------------------------------------------------------------------------

! Draw the grid.

IF (LINE_WIDTH/=0.0) THEN

    IF (PRESENT(ID_LINES)) THEN
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=ID_LINES)
    ELSE
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE)
    END IF

! Vertical lines.
    DO I=1,SIZE(X,DIM=1)
    CALL SVG_Polyline(OUTPUT_FILE=OUTPUT_FILE,X=X(I,:),Y=Y(I,:), &
        & STROKE_WIDTH=LINE_WIDTH,FILL_OPACITY=0.0_SRK, &
        & STROKE_RGB=LINE_RGB,FILL_RGB=(/255,255,255/))
    END DO

! Horizontal lines.
    DO I=1,SIZE(X,DIM=2)
    CALL SVG_Polyline(OUTPUT_FILE=OUTPUT_FILE,X=X(:,I),Y=Y(:,I), &
        & STROKE_WIDTH=LINE_WIDTH,FILL_OPACITY=0.0_SRK, &
        & STROKE_RGB=LINE_RGB,FILL_RGB=(/255,255,255/))
    END DO

    CALL SVG_GroupEnd(OUTPUT_FILE=OUTPUT_FILE)

END IF

! ------------------------------------------------------------------------------

! Draw the node points.

! Node points will not be drawn only if explicitly told so through input
! variable "node_size". Therefore, there is no ELSE in this IF-BLOCK.
IF (NODE_SIZE/=0.0) THEN

    COLOR=NODE_RGB ! Variable initialization

    IF (PRESENT(ID_NODES)) THEN
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=ID_NODES)
    ELSE
        CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE)
    END IF

    DO I=1,SIZE(X,DIM=1)
    DO J=1,SIZE(X,DIM=2)

! Node points will be drawn regardless of whether there is information stored in
! them and displayed through variables "node_property", "node_property_name",
! and "node_color_scheme" or not. Therefore, there is an ELSE in this IF-BLOCK.
    IF (PRESENT(NODE_PROPERTY)) THEN

        IF (TRIM(NODE_COLOR_SCHEME)/='none') COLOR= &
            & SVG_ColorMap(NODE_PROPERTY(I,J), &
            & NODE_MINIMUM,NODE_MAXIMUM,NODE_GRAYSCALE)

! The node color is the one specified by the used color scheme, defined by the
! input variable "node_color_scheme".
        CALL SVG_Circle(OUTPUT_FILE=OUTPUT_FILE,X=X(I,J),Y=Y(I,J), &
            & RADIUS=NODE_SIZE, &
            & STROKE_WIDTH=0.0_SRK,FILL_OPACITY=1.0_SRK, &
            & STROKE_RGB=COLOR,FILL_RGB=COLOR, &
            & SCALAR_NAME=(/NODE_PROPERTY_NAME/), &
            & SCALAR_VALUE=(/NODE_PROPERTY(I,J)/))

    ELSE

! The node color is the one defined in the subroutine arguments through input
! variable "node_rgb".
        CALL SVG_Circle(OUTPUT_FILE=OUTPUT_FILE,X=X(I,J),Y=Y(I,J), &
            & RADIUS=NODE_SIZE, &
            & STROKE_WIDTH=0.0_SRK,FILL_OPACITY=1.0_SRK, &
            & STROKE_RGB=NODE_RGB,FILL_RGB=NODE_RGB)

    END IF

    END DO
    END DO

    CALL SVG_GroupEnd(OUTPUT_FILE=OUTPUT_FILE)

END IF

END SUBROUTINE SVG_GridStructured

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_GroupEnd(OUTPUT_FILE)

! SVG_GroupEnd is called to finalize a group of SVG objects.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!	the ".svg" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_GroupEnd(output_file)
!implicit none
!character,intent(in):: output_file*(*)
!end subroutine SVG_GroupEnd
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_GroupStart(output_file='plot.svg', &
!	& rotate=(/45.0_srk,200.0_srk,200.0_srk/), &
!	& scalar_name=(/'ellipse1_x','ellipse1_y', &
!	& 'square1_x ','square1_y '/), &
!	& scalar_value=(/100.0_srk,100.0_srk,300.0_srk,300.0_srk/))
!call SVG_Ellipse(output_file='plot.svg', &
!	& x=100.0_srk,y=100.0_srk,radius_x=100.0_srk,radius_y=200.0_srk, &
!	& stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!	& stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!	& rotate=(/45.0_srk,100.0_srk,100.0_srk/), &
!	& scalar_name=(/'property'/),scalar_value=(/3.14_srk/))
!call SVG_Square(output_file='plot.svg', &
!	& x=300.0_srk,y=300.0_srk,side=100.0_srk, &
!	& stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!	& stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!	& rotate=(/45.0_srk,300.0_srk,300.0_srk/), &
!	& scalar_name=(/'property'/),scalar_value=(/5.14_srk/))
!call SVG_GroupEnd(output_file='plot.svg')
!call SVG_Finalize(output_file='plot.svg')

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

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_GroupEnd"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_GroupEnd"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_GroupEnd"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A4)',IOSTAT=ERR)"</g>"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_GroupEnd"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)

END SUBROUTINE SVG_GroupEnd

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_GroupSelect(OUTPUT_FILE,TARGET_IDS,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_GroupSelect is called to create a group of selected SVG objects.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! target_ids: character, array (1D). The ids of the target SVG objects that will
!    be grouped.

! INPUT (OPTIONAL):
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG group object. The user is responsible
!    for the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_GroupSelect(output_file,target_ids,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!character,intent(in):: target_ids(:)*(*)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: scalar_value(:)
!end subroutine SVG_GroupSelect
!end interface

!real(srk):: x(4,5),y(4,5),node(4,5),dx(4,5),dy(4,5),x2(4,5),y2(4,5)
!character:: target_ids(4*5)*(default_string_length)
!integer:: i,j,k,color(3)
!x(1,1)=60.44  ; y(1,1)=328.63 ; x(1,2)=151.09 ; y (1,2)=351.29 ; x(1,3)=308.80
!y(1,3)=347.51 ; x(1,4)=384.34 ; y(1,4)=357.90 ; x (1,5)=466.50 ; y(1,5)=355.07
!x(2,1)=129.37 ; y(2,1)=247.41 ; x(2,2)=239.86 ; y (2,2)=275.74 ; x(2,3)=329.57
!y(2,3)=259.69 ; x(2,4)=392.84 ; y(2,4)=297.46 ; x (2,5)=488.22 ; y(2,5)=247.41
!x(3,1)=128.43 ; y(3,1)=141.65 ; x(3,2)=220.03 ; y (3,2)=173.76 ; x(3,3)=297.46
!y(3,3)=196.42 ; x(3,4)=400.40 ; y(3,4)=176.59 ; x (3,5)=482.55 ; y(3,5)=169.98
!x(4,1)=118.99 ; y(4,1)=68.94  ; x(4,2)=190.75 ; y (4,2)=80.27  ; x(4,3)=315.41
!y(4,3)=91.60  ; x(4,4)=401.34 ; y(4,4)=87.82  ; x (4,5)=475.94 ; y(4,5)=84.05
!dx(1,1)=1.0 ; dx(1,2)=2.0 ; dx(1,3)=3.0 ; dx(1,4)=0.0  ; dx(1,5)=-3.0
!dx(2,1)=1.0 ; dx(2,2)=2.0 ; dx(2,3)=3.0 ; dx(2,4)=0.0  ; dx(2,5)=-3.0
!dx(3,1)=1.0 ; dx(3,2)=2.0 ; dx(3,3)=3.0 ; dx(3,4)=0.0  ; dx(3,5)=-3.0
!dx(4,1)=1.0 ; dx(4,2)=2.0 ; dx(4,3)=3.0 ; dx(4,4)=0.0  ; dx(4,5)=-3.0
!dy(1,1)=-1.0 ; dy(1,2)=0.0 ; dy(1,3)=3.0 ; dy(1,4)=4.0  ; dy(1,5)=5.0
!dy(2,1)=-1.0 ; dy(2,2)=0.0 ; dy(2,3)=3.0 ; dy(2,4)=4.0  ; dy(2,5)=5.0
!dy(3,1)=-1.0 ; dy(3,2)=0.0 ; dy(3,3)=3.0 ; dy(3,4)=4.0  ; dy(3,5)=5.0
!dy(4,1)=-1.0 ; dy(4,2)=0.0 ; dy(4,3)=3.0 ; dy(4,4)=4.0  ; dy(4,5)=5.0
!node=sqrt(dx**2+dy**2)
!call SVG_Initialize(output_file='plot.svg')
!call SVG_Vanish(output_file='plot.svg')
!y=y+200_srk
!x2=x+dx*5.0_srk
!y2=y+dy*5.0_srk
!! Print the vectors.
!k=0
!do i=1,size(x,dim=1)
!do j=1,size(x,dim=2)
!    color= SVG_ColorMap(node(i,j), &
!        & minimum=minval(node),maximum=maxval(node),grayscale=.false.)
!! Conditional print of the <defs> element:
!    if (node(i,j)<2.0_srk) &
!        & write(FileManager(filename='plot.svg'),*)"<defs>"
!    k=k+1
!    target_ids(k)=trim(StringInteger(i))//trim(StringInteger(j))
!    call SVG_Vector(output_file='plot.svg', &
!        & x=(/x(i,j),x2(i,j)/),y=(/y(i,j),y2(i,j)/), &
!        & stroke_width=3.0_srk,stroke_rgb=color, &
!        & head_size=7.0_srk,change_marker=.true., &
!        & scalar_name=(/'velocity'/), &
!        & scalar_value=(/node(i,j)/), &
!        & id=target_ids(k))
!    if (node(i,j)<2.0_srk) &
!        & write(FileManager(filename='plot.svg'),*)"</defs>"
!end do
!end do
!call SVG_GroupSelect(output_file='plot.svg', &
!    & target_ids=target_ids(1:k),id='small_velocities')
!call SVG_ColorBar(output_file='plot.svg',title='velocity', &
!    & x=maxval(x)+150.0_srk,y=400.0_srk, &
!    & width=50.0_srk,height=300.0_srk, &
!    & minimum=minval(node),maximum=maxval(node), &
!    & decimal_places=2,scientific=.false., &
!    & font='verdana',font_size=25.0_srk,font_rgb=(/0,0,0/), &
!    & grayscale=.false.)
!call SVG_ActionButton(output_file='plot.svg',script='vanish', &
!    & target_id='small_velocities', &
!    & x=400.0_srk,y=100.0_srk,width=400.0_srk,height=50.0_srk, &
!    & stroke_width=5.0_srk,button_fill_opacity=1.0_srk, &
!    & stroke_rgb=(/0,0,255/),button_fill_rgb=(/255,255,255/), &
!    & string='toggle velocities .lt. 2.0',font='verdana',font_size=20.0_srk, &
!    & text_fill_opacity=1.0_srk,text_fill_rgb=(/0,0,0/))
!call SVG_Finalize(output_file='plot.svg')

! Example of SVG application:

!<?xml version="1.0" standalone="no"?>
!<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
!"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
!<svg width="10cm" height="3cm" viewBox="0 0 100 30" version="1.1"
!xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
!<rect x="20" y="10" id="1" width="10" height="10"/>
!<rect fill="blue" x="60" y="10" id="3" width="10" height="10"/>
!<g fill="red">
!<use xlink:href="#1" />
!<use xlink:href="#2" />
!<use xlink:href="#3" />
!</g>
!<rect x="40" y="10" id="2" width="10" height="10"/>
!</svg>

! Note:
! The present subroutine makes use of the <use> element in order to refer to
! specific objects in the SVG file. If the referred objects are not inside a
! <defs> block, they will be cloned to their specific locations. If this
! behaviour is not desired, the objects must be placed inside a <defs> block.

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
CHARACTER,INTENT(IN):: TARGET_IDS(:)*(*)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_GroupSelect"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_GroupSelect"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_GroupSelect"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIZE(TARGET_IDS)==0) THEN
WRITE(*,*)"SVG_GroupSelect"
WRITE(*,*)"ERROR: missing id data."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_GroupSelect"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_GroupSelect"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_GroupSelect"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_GroupSelect"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

! The properties of a <g> element (colors, strokes etc) will not overwrite the
! properties of the contained elements if they are defined explicitly within
! them. For example, specifying the fill color in a <g> element as "red" will
! not have any influence on a contained <rect> object where it is stated that
! fill="blue". The statement "opaciy=0.0" in a <g> element however, will
! dominate over the "fill-opacity=1.0" statement in a contained <rect> object
! because it is more general.
WRITE(IO,'(A2)',IOSTAT=ERR)"<g"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_GroupSelect"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A1)')">"
WRITE(IO,*)

! Group the requested objects by referring to them using their ids. Any given
! ids that do not belong to existing objects in the SVG file will be ignored.
! SVG uses a top-down direction for defining the objects. This means that any
! objects defined in the SVG document after the following statement appears in
! the SVG file will be virtually non-existent and will not be included in the
! group. Practically this means that it is better to call the SVG_GroupSelect
! subroutine just before finalizing the SVG file in order to assure that all
! objects are present at the time of the call.
DO I=1,SIZE(TARGET_IDS)
WRITE(IO,*)"<use xlink:href='#"//TRIM(TARGET_IDS(I))//"' />"
END DO
WRITE(IO,*)

WRITE(IO,'(A4)')"</g>"
WRITE(IO,*)

END SUBROUTINE SVG_GroupSelect

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_GroupStart(OUTPUT_FILE,ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_GroupStart is called to initialize a group of SVG objects.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the group is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_GroupStart(output_file,rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_GroupStart
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_GroupStart(output_file='plot.svg', &
!    & rotate=(/45.0_srk,200.0_srk,200.0_srk/), &
!    & scalar_name=(/'ellipse1_x','ellipse1_y', &
!    & 'square1_x ','square1_y '/), &
!    & scalar_value=(/100.0_srk,100.0_srk,300.0_srk,300.0_srk/),id='1')
!call SVG_Ellipse(output_file='plot.svg', &
!    & x=100.0_srk,y=100.0_srk,radius_x=100.0_srk,radius_y=200.0_srk, &
!    & stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!    & rotate=(/45.0_srk,100.0_srk,100.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/))
!call SVG_Square(output_file='plot.svg', &
!    & x=300.0_srk,y=300.0_srk,side=100.0_srk, &
!    & stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!    & rotate=(/45.0_srk,300.0_srk,300.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/5.14_srk/))
!call SVG_GroupEnd(output_file='plot.svg')
!call SVG_Finalize(output_file='plot.svg')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_GroupStart"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_GroupStart"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_GroupStart"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_GroupStart"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_GroupStart"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_GroupStart"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_GroupStart"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

IF ((PRESENT(ROTATE)).OR.(PRESENT(ID)).OR. &
    & ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE)))) THEN

    WRITE(IO,'(A2)',IOSTAT=ERR)"<g"

    IF (ERR/=0) THEN
    WRITE(*,*)"SVG_GroupStart"
    WRITE(*,*)"ERROR: writing to file was not possible."
    WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
    WRITE(*,*)"error flag : ",ERR
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

    IF (PRESENT(ROTATE)) THEN
        WRITE(IO,*)"transform='rotate("// &
            & TRIM(StringReal(ROTATE(1)))//","// &
            & TRIM(StringReal(ROTATE(2)))//","// &
            & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
    END IF

    IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
        DO I=1,SIZE(SCALAR_NAME)
            WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
                & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
        END DO
    END IF

    IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

    WRITE(IO,'(A1)')">"

ELSE

    WRITE(IO,'(A3)',IOSTAT=ERR)"<g>"

    IF (ERR/=0) THEN
    WRITE(*,*)"SVG_GroupStart"
    WRITE(*,*)"ERROR: writing to file was not possible."
    WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
    WRITE(*,*)"error flag : ",ERR
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

END IF

WRITE(IO,*)

END SUBROUTINE SVG_GroupStart

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Initialize(OUTPUT_FILE)

! SVG_Initialize is called to initialize an output SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Initialize(output_file)
!implicit none
!character,intent(in):: output_file*(*)
!end subroutine SVG_Initialize
!end interface

!! Create an empty SVG document:
!call SVG_Initialize(output_file='plot.svg')
!call SVG_Finalize(output_file='plot.svg')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

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
WRITE(*,*)"SVG_Initialize"
WRITE(*,*)"WARNING: SVG file has already been accessed."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Initialize"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Initialize"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/VIEWBOX_WIDTH,VIEWBOX_HEIGHT/)<0)) THEN
WRITE(*,*)"SVG_Initialize"
WRITE(*,*)"ERROR: viewBox may not have negative dimensions."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/VIEWBOX_WIDTH,VIEWBOX_HEIGHT/)==0)) THEN
WRITE(*,*)"SVG_Initialize"
WRITE(*,*)"WARNING: SVG viewBox will not be viewable."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A37)',IOSTAT=ERR)"<?xml version='1.0' standalone='no'?>"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Initialize"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,'(A46)')"<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'"
WRITE(IO,'(A51)')"'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>"
WRITE(IO,*)"<svg preserveAspectRatio='xMidYMid meet' viewBox='0,0,"// &
    & TRIM(StringReal(VIEWBOX_WIDTH))//","// &
    & TRIM(StringReal(VIEWBOX_HEIGHT))//"'"
WRITE(IO,*)"xmlns='http://www.w3.org/2000/svg'"
WRITE(IO,*)"xmlns:xlink='http://www.w3.org/1999/xlink'"
WRITE(IO,*)"version='1.1'>"

WRITE(IO,*)

END SUBROUTINE SVG_Initialize

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Line(OUTPUT_FILE,X,Y,STROKE_WIDTH,STROKE_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Line is called to draw a line in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, array (1D) with 2 elements. The x-axis coordinates of the line
!    beginning (1st element) and the line end (2nd element) (px).
! y: real, array (1D) with 2 elements. The y-axis coordinates of the line
!    beginning (1st element) and the line end (2nd element) (px).
! stroke_width: real, scalar. The thickness of the line (px).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Line(output_file,x,y,stroke_width,stroke_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x(2),y(2),stroke_width
!integer,intent(in):: stroke_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Line
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Line(output_file='plot.svg', &
!    & x=(/100.0_srk,500.0_srk/),y=(/100.0_srk,500.0_srk/), &
!    & stroke_width=3.0_srk,stroke_rgb=(/255,0,0/), &
!    & rotate=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X(2),Y(2),STROKE_WIDTH
INTEGER,INTENT(IN):: STROKE_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(Y>VIEWBOX_HEIGHT)).OR.(ANY(X>VIEWBOX_WIDTH))) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (STROKE_WIDTH==0.0) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"WARNING: this stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(STROKE_RGB<0)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A5)',IOSTAT=ERR)"<line"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Line"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename: ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"x1='"//TRIM(StringReal(X(1)))//"'"
WRITE(IO,*)"y1='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y(1)))//"'"
WRITE(IO,*)"x2='"//TRIM(StringReal(X(2)))//"'"
WRITE(IO,*)"y2='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y(2)))//"'"
WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_Line

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Link(OUTPUT_FILE,TARGET_IDS,LINK_STRING)

! SVG_Link is called to create a group of selected SVG objects.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! target_ids: character, array (1D). The ids of the SVG objects that will
!    be attributed the link.
! link_string: character, scalar. The string specifying the link. It can be e.g.
!    a web adress, a local file, etc. The validity of the link is not being
!    checked by the present subroutine. Web adresses must be specified in
!    full, e.g. "http://www.w3.org/". Linking with "www.w3.org" will result
!    in an -obviously broken- link to local file named "www.w3.org".

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Link(output_file,target_ids,link_string)
!implicit none
!character,intent(in):: output_file*(*),target_ids(:)*(*),link_string*(*)
!end subroutine SVG_Link
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Circle(output_file='plot.svg', &
!    & x=200.0_srk,y=200.0_srk,radius=100.0_srk, &
!    & stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!    & rotate=(/15.0_srk,100.0_srk,100.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Link(output_file='plot.svg',target_ids=(/'1'/), &
!    & link_string='http://www.w3.org/')
!call SVG_Finalize(output_file='plot.svg')

! Note:
! The present subroutine makes use of the <use> element in order to refer to
! specific objects in the SVG file. If the referred objects are not inside a
! <defs> block, they will be cloned to their specific locations. If this
! behaviour is not desired, the objects must be placed inside a <defs> block.

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),TARGET_IDS(:)*(*),LINK_STRING*(*)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Link"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Link"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Link"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIZE(TARGET_IDS)==0) THEN
WRITE(*,*)"SVG_Link"
WRITE(*,*)"ERROR: missing id data."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (LEN_TRIM(LINK_STRING)==0) THEN
WRITE(*,*)"SVG_Link"
WRITE(*,*)"WARNING: invalid link string."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A2)',IOSTAT=ERR)"<a"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Link"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"xlink:href='"//TRIM(LINK_STRING)//"'>"

! Link the requested objects by referring to them using their ids. Any given
! ids that do not belong to existing objects in the SVG file will be ignored.
! SVG uses a top-down direction for defining the objects. This means that any
! objects defined in the SVG document after the following statement appears in
! the SVG file will be virtually non-existent and will not be included in the
! group. Practically this means that it is better to call the SVG_Link
! subroutine just before finalizing the SVG file in order to assure that all
! objects are present at the time of the call.
DO I=1,SIZE(TARGET_IDS)
    WRITE(IO,*)"<use xlink:href='#"//TRIM(TARGET_IDS(I))//"' />"
END DO
WRITE(IO,'(A4)')"</a>"
WRITE(IO,*)

END SUBROUTINE SVG_Link

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_LinkButton(OUTPUT_FILE,LINK_STRING,X,Y,WIDTH,HEIGHT, &
    & STROKE_WIDTH,BUTTON_FILL_OPACITY,STROKE_RGB,BUTTON_FILL_RGB, &
    & STRING,FONT,FONT_SIZE,TEXT_FILL_OPACITY,TEXT_FILL_RGB)

! SVG_LinkButton is called to place a LinkButton in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! link_string: character, scalar. The string specifying the link. It can be e.g.
!    a web adress, a local file, etc. The validity of the link is not being
!    checked by the present subroutine. Web adresses must be specified in
!    full, e.g. "http://www.w3.org/". Linking with "www.w3.org" will result
!    in an -obviously broken- link to local file named "www.w3.org".
! x: real, scalar. The x-axis coordinate of the button center. (px).
! y: real, scalar. The y-axis coordinate of the button center. (px).
! width: real, scalar. The button width. (px).
! height: real, scalar. he button height. (px).
! stroke_width: real, scalar. The thickness of the button line (px).
! button_fill_opacity: real, scalar. The opacity of the button fill
!    (dimensionless, 0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! button_fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB
!    channel values (dimensionless, 0<=fill_rgb(:)<=255).
! string: character, scalar. The string that will be drawn.
! font: character, scalar. The font that will be used for the rendering. The
!    selected font must be supported in the system that will be used to
!    render the SVG file.
! font_size: real, scalar. The size of the font.
! text_fill_opacity: real, scalar. The opacity of the text fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! text_fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB
!    channel values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_LinkButton(output_file,link_string,x,y,width,height, &
!    & stroke_width,button_fill_opacity,stroke_rgb,button_fill_rgb, &
!    & string,font,font_size,text_fill_opacity,text_fill_rgb)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),link_string*(*),string*(*),font*(*)
!real(srk),intent(in):: x,y,width,height,stroke_width
!real(srk),intent(in):: button_fill_opacity,text_fill_opacity,font_size
!integer,intent(in):: stroke_rgb(3),button_fill_rgb(3),text_fill_rgb(3)
!end subroutine SVG_LinkButton
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_LinkButton(output_file='plot.svg',link_string='http://www.w3.org/', &
!    & x=200.0_srk,y=500.0_srk,width=200.0_srk,height=50.0_srk, &
!    & stroke_width=5.0_srk,button_fill_opacity=1.0_srk, &
!    & stroke_rgb=(/0,0,255/),button_fill_rgb=(/255,255,255/), &
!    & string='go home',font='verdana',font_size=20.0_srk, &
!    & text_fill_opacity=1.0_srk,text_fill_rgb=(/0,0,0/))
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),LINK_STRING*(*),STRING*(*),FONT*(*)
REAL(SRK),INTENT(IN):: X,Y,WIDTH,HEIGHT,STROKE_WIDTH
REAL(SRK),INTENT(IN):: BUTTON_FILL_OPACITY,TEXT_FILL_OPACITY,FONT_SIZE
INTEGER,INTENT(IN):: STROKE_RGB(3),BUTTON_FILL_RGB(3),TEXT_FILL_RGB(3)

! Private variables:
INTEGER:: ERR,IO
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((Y>VIEWBOX_HEIGHT).OR.(X>VIEWBOX_WIDTH)) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (ANY((/WIDTH,HEIGHT/)<0.0)) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"ERROR: invalid shape parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/WIDTH,HEIGHT/)==0.0)) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"WARNING: SVG object will not be viewable."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<BUTTON_FILL_OPACITY).OR.(BUTTON_FILL_OPACITY<0.0).OR. &
    & (1.0<TEXT_FILL_OPACITY).OR.(TEXT_FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"ERROR: invalid paint parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((BUTTON_FILL_OPACITY==0.0).AND.(TEXT_FILL_OPACITY==0.0).AND. &
    & (STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(TEXT_FILL_RGB<0)).OR.(ANY(BUTTON_FILL_RGB<0)).OR. &
    & (ANY(STROKE_RGB<0)).OR.(ANY(STROKE_RGB>255)).OR. &
    & (ANY(TEXT_FILL_RGB>255)).OR.(ANY(BUTTON_FILL_RGB>255))) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

! Create the link.
WRITE(IO,'(A2)',IOSTAT=ERR)"<a"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_LinkButton"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"xlink:href='"//TRIM(LINK_STRING)//"'>"
WRITE(IO,*)

! Group the rounded rectangle and its text.
WRITE(IO,'(A3)')"<g>"
WRITE(IO,*)

! Print the rounded rectangle.
WRITE(IO,'(A5)',IOSTAT=ERR)"<rect"
WRITE(IO,*)"x='"//TRIM(StringReal(X-WIDTH/2.0_SRK))//"'"
WRITE(IO,*)"y='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y-HEIGHT/2.0_SRK))//"'"
WRITE(IO,*)"rx='"//TRIM(StringReal(HEIGHT/5.0_SRK))//"'"
WRITE(IO,*)"ry='"//TRIM(StringReal(HEIGHT/5.0_SRK))//"'"
WRITE(IO,*)"width='"//TRIM(StringReal(WIDTH))//"'"
WRITE(IO,*)"height='"//TRIM(StringReal(HEIGHT))//"'"
WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(BUTTON_FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(BUTTON_FILL_RGB(1)))//","// &
        & TRIM(StringInteger(BUTTON_FILL_RGB(2)))//","// &
        & TRIM(StringInteger(BUTTON_FILL_RGB(3)))//")'"
WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

! Print the button text.
WRITE(IO,'(A5)')"<text"
WRITE(IO,*)"x='"//TRIM(StringReal(X))//"'"
WRITE(IO,*)"y='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y+FONT_SIZE/3.0_SRK))//"'"
WRITE(IO,*)"font-family='"//TRIM(FONT)//"'"
WRITE(IO,*)"font-size='"//TRIM(StringReal(FONT_SIZE))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(TEXT_FILL_OPACITY))//"'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(TEXT_FILL_RGB(1)))//","// &
        & TRIM(StringInteger(TEXT_FILL_RGB(2)))//","// &
        & TRIM(StringInteger(TEXT_FILL_RGB(3)))//")'"
WRITE(IO,*)"text-anchor='middle'"
WRITE(IO,*)"font-weight='normal'"
!write(io,*)"font-style='italic'"
WRITE(IO,'(A1)')">"
WRITE(IO,*)TRIM(STRING)
WRITE(IO,'(A7)')"</text>"
WRITE(IO,*)

! End the grouping
WRITE(IO,'(A4)',IOSTAT=ERR)"</g>"
WRITE(IO,*)

! End the link.
WRITE(IO,'(A4)')"</a>"
WRITE(IO,*)

END SUBROUTINE SVG_LinkButton

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Polygon(OUTPUT_FILE,X,Y, &
    & STROKE_WIDTH,FILL_OPACITY,STROKE_RGB,FILL_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Polygon is called to draw a crooked line in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, array (1D). The x-axis coordinates of the line points. (px).
! y: real, array (1D). The y-axis coordinates of the line points. (px).
! stroke_width: real, scalar. The thickness of the line (px).
! fill_opacity: real, scalar. The opacity of the fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Polygon(output_file,x,y, &
!    & stroke_width,fill_opacity,stroke_rgb,fill_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x(:),y(:),stroke_width,fill_opacity
!integer,intent(in):: stroke_rgb(3),fill_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Polygon
!end interface

!real(srk):: x(10),y(10)
!x(1)=350.0  ; y(1)=75.0
!x(2)=379.0  ; y(2)=161.0
!x(3)=469.0  ; y(3)=161.0
!x(4)=397.0  ; y(4)=215.0
!x(5)=423.0  ; y(5)=301.0
!x(6)=350.0  ; y(6)=250.0
!x(7)=277.0  ; y(7)=301.0
!x(8)=303.0  ; y(8)=215.0
!x(9)=231.0  ; y(9)=161.0
!x(10)=321.0 ; y(10)=161.0
!call SVG_Initialize(output_file='plot.svg')
!call SVG_Polygon(output_file='plot.svg',x=x,y=y, &
!    & stroke_width=5.0_srk,fill_opacity=1.0_srk, &
!    & stroke_rgb=(/0,0,0/),fill_rgb=(/255,0,0/), &
!    & rotate=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X(:),Y(:),STROKE_WIDTH,FILL_OPACITY
INTEGER,INTENT(IN):: STROKE_RGB(3),FILL_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(Y>VIEWBOX_HEIGHT)).OR.(ANY(X>VIEWBOX_WIDTH))) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (SIZE(X)/=SIZE(Y)) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: coordinate arrays do not match."
WRITE(*,*)"size(x): ",SIZE(X)
WRITE(*,*)"size(y): ",SIZE(Y)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<FILL_OPACITY).OR.(FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: invalid paint parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FILL_OPACITY==0.0).AND.(STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(FILL_RGB<0)).OR.(ANY(STROKE_RGB<0)).OR. &
    & (ANY(FILL_RGB>255)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"size(scalar_name): ",SIZE(SCALAR_NAME)
WRITE(*,*)"size(scalar_value): ",SIZE(SCALAR_VALUE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (SIZE(X)<3) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"WARNING: can not plot a polygon with less than three points"
WRITE(*,*)"No polygon will be plotted."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ALL(X==0)).OR.(ALL(Y==0))) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"WARNING: all array points are equal to zero"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A8)',IOSTAT=ERR)"<polygon"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Polygon"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"points='"
DO I=1,SIZE(X)-1
    WRITE(IO,*)TRIM(StringReal(X(I)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-Y(I)))
END DO
WRITE(IO,*)TRIM(StringReal(X(SIZE(X))))//","// &
    & TRIM(StringReal(VIEWBOX_HEIGHT-Y(SIZE(Y))))//"'"

WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(FILL_RGB(1)))//","// &
        & TRIM(StringInteger(FILL_RGB(2)))//","// &
        & TRIM(StringInteger(FILL_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_Polygon

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Polyline(OUTPUT_FILE,X,Y, &
    & STROKE_WIDTH,FILL_OPACITY,STROKE_RGB,FILL_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Polyline is called to draw a crooked line in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, array (1D). The x-axis coordinates of the line points. (px).
! y: real, array (1D). The y-axis coordinates of the line points. (px).
! stroke_width: real, scalar. The thickness of the line (px).
! fill_opacity: real, scalar. The opacity of the fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Polyline(output_file,x,y, &
!    & stroke_width,fill_opacity,stroke_rgb,fill_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x(:),y(:),stroke_width,fill_opacity
!integer,intent(in):: stroke_rgb(3),fill_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Polyline
!end interface

!real(srk):: x(15),y(15)
!x(1)=50.0    ; y(1)=375.0
!x(2)=150.0   ; y(2)=375.0
!x(3)=150.0   ; y(3)=425.0
!x(4)=250.0   ; y(4)=425.0
!x(5)=250.0   ; y(5)=375.0
!x(6)=350.0   ; y(6)=375.0
!x(7)=350.0   ; y(7)=500.0
!x(8)=450.0   ; y(8)=500.0
!x(9)=450.0   ; y(9)=375.0
!x(10)=550.0  ; y(10)=375.0
!x(11)=550.0  ; y(11)=575.0
!x(12)=650.0  ; y(12)=575.0
!x(13)=650.0  ; y(13)=375.0
!x(14)=750.0  ; y(14)=375.0
!x(15)=750.0  ; y(15)=650.0
!call SVG_Initialize(output_file='plot.svg')
!call SVG_Polyline(output_file='plot.svg',x=x,y=y, &
!    & stroke_width=5.0_srk,fill_opacity=1.0_srk, &
!    & stroke_rgb=(/0,0,0/),fill_rgb=(/255,0,0/), &
!    & rotate=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X(:),Y(:),STROKE_WIDTH,FILL_OPACITY
INTEGER,INTENT(IN):: STROKE_RGB(3),FILL_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: file must have the .svg extension"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"WARNING: negative coordinates"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(Y>VIEWBOX_HEIGHT)).OR.(ANY(X>VIEWBOX_WIDTH))) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (SIZE(X)/=SIZE(Y)) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: coordinate arrays do not match"
WRITE(*,*)"size(x): ",SIZE(X)
WRITE(*,*)"size(y): ",SIZE(Y)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: invalid stroke parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<FILL_OPACITY).OR.(FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: invalid paint parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FILL_OPACITY==0.0).AND.(STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(FILL_RGB<0)).OR.(ANY(STROKE_RGB<0)).OR. &
    & (ANY(FILL_RGB>255)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: invalid RGB channel value"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements"
WRITE(*,*)"size(scalar_name): ",SIZE(SCALAR_NAME)
WRITE(*,*)"size(scalar_value): ",SIZE(SCALAR_VALUE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (SIZE(X)<2) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"WARNING: can not plot a line with less than two points"
WRITE(*,*)"No line will be plotted."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ALL(X==0)).OR.(ALL(Y==0))) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"WARNING: all array points are equal to zero"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A9)',IOSTAT=ERR)"<polyline"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Polyline"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"points='"
DO I=1,SIZE(X)-1
    WRITE(IO,*)TRIM(StringReal(X(I)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-Y(I)))
END DO
WRITE(IO,*)TRIM(StringReal(X(SIZE(X))))//","// &
    & TRIM(StringReal(VIEWBOX_HEIGHT-Y(SIZE(Y))))//"'"

WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(FILL_RGB(1)))//","// &
        & TRIM(StringInteger(FILL_RGB(2)))//","// &
        & TRIM(StringInteger(FILL_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_Polyline

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Rect(OUTPUT_FILE,X,Y,WIDTH,HEIGHT, &
    & STROKE_WIDTH,FILL_OPACITY,STROKE_RGB,FILL_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Rect is called to draw a rectangle in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, scalar. The x-axis coordinate of the rectangle center. (px).
! y: real, scalar. The y-axis coordinate of the rectangle center. (px).
! width: real, scalar. The rectangle width. (px).
! height: real, scalar. he rectangle height. (px).
! stroke_width: real, scalar. The thickness of the rectangle line (px).
! fill_opacity: real, scalar. The opacity of the rectangle fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Rect(output_file,x,y,width,height, &
!    & stroke_width,fill_opacity,stroke_rgb,fill_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x,y,width,height,stroke_width,fill_opacity
!integer,intent(in):: stroke_rgb(3),fill_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Rect
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Rect(output_file='plot.svg', &
!    & x=200.0_srk,y=200.0_srk,width=100.0_srk,height=400.0_srk, &
!    & stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!    & rotate=(/45.0_srk,200.0_srk,200.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! Another difference to the SVG 1.1 specification is that the position of the
! Rect shape is not defined by the coordinates of the lower left corner, but the
! coordinates of the rectangle center.

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X,Y,WIDTH,HEIGHT,STROKE_WIDTH,FILL_OPACITY
INTEGER,INTENT(IN):: STROKE_RGB(3),FILL_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((Y>VIEWBOX_HEIGHT).OR.(X>VIEWBOX_WIDTH)) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (ANY((/WIDTH,HEIGHT/)<0.0)) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: invalid shape parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/WIDTH,HEIGHT/)==0.0)) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"WARNING: SVG object will not be viewable."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<FILL_OPACITY).OR.(FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: invalid paint parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FILL_OPACITY==0.0).AND.(STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(FILL_RGB<0)).OR.(ANY(STROKE_RGB<0)).OR. &
    & (ANY(FILL_RGB>255)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A5)',IOSTAT=ERR)"<rect"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Rect"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"x='"//TRIM(StringReal(X-WIDTH/2.0_SRK))//"'"
WRITE(IO,*)"y='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y-HEIGHT/2.0_SRK))//"'"
WRITE(IO,*)"width='"//TRIM(StringReal(WIDTH))//"'"
WRITE(IO,*)"height='"//TRIM(StringReal(HEIGHT))//"'"
WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(FILL_RGB(1)))//","// &
        & TRIM(StringInteger(FILL_RGB(2)))//","// &
        & TRIM(StringInteger(FILL_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_Rect

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Resurrect(OUTPUT_FILE)

! SVG_Resurrect is called to change an SVG file so that more data can be added
! to it. After resurrection, the SVG file will have to be re-finalized in order
! to be usable.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Resurrect(output_file)
!implicit none
!character,intent(in):: output_file*(*)
!end subroutine SVG_Resurrect
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Rect(output_file='plot.svg', &
!    & x=200.0_srk,y=300.0_srk,width=100.0_srk,height=400.0_srk, &
!    & stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!    & rotate=(/0.0_srk,200.0_srk,200.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')
!write(*,*)"view the file and press enter to continue"
!read(*,*)
!call SVG_Resurrect(output_file='plot.svg')
!call SVG_Rect(output_file='plot.svg', &
!    & x=500.0_srk,y=300.0_srk,width=50.0_srk,height=200.0_srk, &
!    & stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!    & rotate=(/0.0_srk,200.0_srk,200.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='2')
!call SVG_Finalize(output_file='plot.svg')

! ------------------------------------------------------------------------------

! External dependencies:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)

! Private variables:
CHARACTER:: A,KEY*(10),LOCK*(10)
INTEGER:: ERR,IO
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (CONNECTED) THEN
WRITE(*,*)"SVG_Resurrect"
WRITE(*,*)"WARNING: SVG file is open for editing."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Resurrect"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Resurrect"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

REWIND(IO)

LOCK='<!--EOF-->'
KEY=''

DO

READ(IO,'(A1)',ADVANCE='no',IOSTAT=ERR)A

IF (ERR==-1) THEN
WRITE(*,*)"SVG_Resurrect"
WRITE(*,*)"ERROR: SVG file is not finalized."
WRITE(*,*)"Data can be added without alterations."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
RETURN
END IF

IF (ERR>0) THEN
WRITE(*,*)ERR
WRITE(*,*)"SVG_Resurrect"
WRITE(*,*)"ERROR: reading from file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! blank spaces and tabs are being ignored.
IF (A=='    '.OR.A==' ') CYCLE

KEY=KEY(2:LEN(KEY))//A

IF (KEY==LOCK) EXIT

END DO

BACKSPACE(IO)
ENDFILE(IO)

RETURN

END SUBROUTINE SVG_Resurrect

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Square(OUTPUT_FILE,X,Y,SIDE, &
    & STROKE_WIDTH,FILL_OPACITY,STROKE_RGB,FILL_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Square is called to draw a rectangle in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, scalar. The x-axis coordinate of the rectangle center. (px).
! y: real, scalar. The y-axis coordinate of the rectangle center. (px).
! side: real, scalar. The length of the square side. (px).
! stroke_width: real, scalar. The thickness of the rectangle line (px).
! fill_opacity: real, scalar. The opacity of the rectangle fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Square(output_file,x,y,side, &
!    & stroke_width,fill_opacity,stroke_rgb,fill_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x,y,side,stroke_width,fill_opacity
!integer,intent(in):: stroke_rgb(3),fill_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Square
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Square(output_file='plot.svg', &
!    & x=200.0_srk,y=200.0_srk,side=100.0_srk, &
!    & stroke_width=30.0_srk,fill_opacity=0.5_srk, &
!    & stroke_rgb=(/255,0,0/),fill_rgb=(/0,0,255/), &
!    & rotate=(/45.0_srk,200.0_srk,200.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X,Y,SIDE,STROKE_WIDTH,FILL_OPACITY
INTEGER,INTENT(IN):: STROKE_RGB(3),FILL_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: file must have the .svg extension"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"WARNING: negative coordinates"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((Y>VIEWBOX_HEIGHT).OR.(X>VIEWBOX_WIDTH)) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (SIDE<0.0) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: invalid shape parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIDE==0.0) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"WARNING: SVG object will not be viewable."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: invalid stroke parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<FILL_OPACITY).OR.(FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: invalid paint parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FILL_OPACITY==0.0).AND.(STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(FILL_RGB<0)).OR.(ANY(STROKE_RGB<0)).OR. &
    & (ANY(FILL_RGB>255)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: invalid RGB channel value"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

! ------------------------------------------------------------------------------

! In the SVG 1.1 specification, there is no actual "Square" basic shape. Here,
! the square is represented by an SVG Rectangle basic shape.

WRITE(IO,'(A5)',IOSTAT=ERR)"<rect"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Square"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"x='"//TRIM(StringReal(X-SIDE/2.0_SRK))//"'"
WRITE(IO,*)"y='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y-SIDE/2.0_SRK))//"'"
WRITE(IO,*)"width='"//TRIM(StringReal(SIDE))//"'"
WRITE(IO,*)"height='"//TRIM(StringReal(SIDE))//"'"
WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(FILL_RGB(1)))//","// &
        & TRIM(StringInteger(FILL_RGB(2)))//","// &
        & TRIM(StringInteger(FILL_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_Square

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_TernaryPlot(OUTPUT_FILE, &
    & A_POINTS,B_POINTS,C_POINTS,A_TITLE,B_TITLE,C_TITLE, &
    & DIAGRAM_TITLE,DATA_TITLE,TITLE_DIAGRAM_FONT_SIZE, &
    & TITLE_AXIS_FONT_SIZE,NUMBERS_AXIS_FONT_SIZE, &
    & FONT_RGB,AXIS_STROKE_RGB,AXIS_STROKE_WIDTH, &
    & DRAW_TICKS,TICK_LENGTH,TICK_WIDTH, &
    & DRAW_GRID,GRID_STROKE_WIDTH,GRID_STROKE_RGB, &
    & SHAPE_TYPE,SHAPE_SIZE,SHAPE_STROKE_WIDTH, &
    & SHAPE_FILL_RGB,SHAPE_STROKE_RGB, &
    & LINE_TYPE,LINE_STROKE_WIDTH,LINE_STROKE_RGB)

! SVG_TernaryPlot draws a triangle diagram in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! a_points: real, array (1D). The triangular coordinates of the a-component of
!    the points that will be plotted. Arrays a_points, b_points and
!    c_points must have the same amount of elements and a 1-1
!    correspondance.
! b_points: real, array (1D). The triangular coordinates of the b-component of
!    the points that will be plotted. Arrays a_points, b_points and
!    c_points must have the same amount of elements and a 1-1
!    correspondance.
! c_points: real, array (1D). The triangular coordinates of the c-component of
!    the points that will be plotted. Arrays a_points, b_points and
!    c_points must have the same amount of elements and a 1-1
!    correspondance.
! a_title: character, scalar. The title of the a-component.
! b_title: character, scalar. The title of the b-component.
! c_title: character, scalar. The title of the c-component.
! diagram_title: character, scalar. The title of the graph.
! data_title: character, scalar. The title of the data series (for the legend).
! title_diagram_font_size: integer, scalar. The font size of the diagram title.
! title_axis_font_size: integer, scalar. The font size of the axes titles.
! number_axis_font_size: integer, scalar. The font size of the axes numbers.
! font_rgb: integer, array (1D) with 3 elements. Contains the RGB channel
!    values of the color of all the text in the graph.
! axis_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the line will be drawn.
! axis_stroke_width: integer, scalar. The thickness of the axes' line (px).
! draw_ticks: logical, scalar. Specifies whether ticks will be drawn.
! tick_length: real, scalar. The length of the axes' tick.
! tick_width: real, scalar. The width of the axes' tick.
! draw_grid: logical, scalar. Specifies whether gridlines will be drawn.
! grid_stroke_width: integer, scalar. The width of the gridlines.
! grid_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the gridlines will be drawn.
! shape_type: character, scalar. Specifies which shape will be used to plot the
!    data.
! shape_size: integer, scalar. The shape's diameter (in pixels).
! shape_stroke_width: integer, scalar. The thickness of the line used to draw
!    the circle (in pixels).
! shape_fill_rgb: integer, array (1D) with 3 elements. Contains the RGB channel
!    values of the color with which the shape will be filled.
! shape_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the line of the circle will be
!    drawn.
! line_type: character, scalar. Specifies the kind of line that connects the
!    diagram points.
! line_stroke_width: integer, scalar. The width of the line that connects the
!    diagram points.
! line_stroke_rgb: integer, array (1D) with 3 elements. Contains the RGB
!    channel values of the color with which the line that connects the
!    diagram points will be drawn.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_TernaryPlot(output_file, &
!    & a_points,b_points,c_points,a_title,b_title,c_title, &
!    & diagram_title,data_title,title_diagram_font_size, &
!    & title_axis_font_size,numbers_axis_font_size, &
!    & font_rgb,axis_stroke_rgb,axis_stroke_width, &
!    & draw_ticks,tick_length,tick_width, &
!    & draw_grid,grid_stroke_width,grid_stroke_rgb, &
!    & shape_type,shape_size,shape_stroke_width, &
!    & shape_fill_rgb,shape_stroke_rgb, &
!    & line_type,line_stroke_width,line_stroke_rgb)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),shape_type*(*),line_type*(*)
!character,intent(in):: diagram_title*(*),a_title*(*),b_title*(*),c_title*(*)
!character,intent(in):: data_title*(*)
!real(srk),intent(in):: a_points(:),b_points(:),c_points(:)
!real(srk),intent(in):: axis_stroke_width,grid_stroke_width
!real(srk),intent(in):: shape_size,shape_stroke_width,line_stroke_width
!real(srk),intent(in):: title_axis_font_size,numbers_axis_font_size
!real(srk),intent(in):: title_diagram_font_size
!real(srk),intent(in):: tick_length,tick_width
!integer,intent(in):: axis_stroke_rgb(3),grid_stroke_rgb(3),font_rgb(3)
!integer,intent(in):: shape_fill_rgb(3),shape_stroke_rgb(3),line_stroke_rgb(3)
!logical,intent(in):: draw_ticks,draw_grid
!end subroutine SVG_TernaryPlot
!end interface

!real(srk):: a_points(11),b_points(11),c_points(11)
!a_points(1)=1.0   ; b_points(1)=0.0   ; c_points(1)=0.0
!a_points(2)=0.8   ; b_points(2)=0.15  ; c_points(2)=0.05
!a_points(3)=0.65  ; b_points(3)=0.25  ; c_points(3)=0.1
!a_points(4)=0.455 ; b_points(4)=0.355 ; c_points(4)=0.19
!a_points(5)=0.3   ; b_points(5)=0.4   ; c_points(5)=0.3
!a_points(6)=0.2   ; b_points(6)=0.4   ; c_points(6)=0.4
!a_points(7)=0.12  ; b_points(7)=0.37  ; c_points(7)=0.51
!a_points(8)=0.08  ; b_points(8)=0.32  ; c_points(8)=0.6
!a_points(9)=0.035 ; b_points(9)=0.25  ; c_points(9)=0.715
!a_points(10)=0.01 ; b_points(10)=0.19 ; c_points(10)=0.8
!a_points(11)=0.0  ; b_points(11)=0.0  ; c_points(11)=1.0

!call SVG_TernaryPlot( &
!output_file='plot.svg', &
!a_points=a_points, &
!b_points=b_points, &
!c_points=c_points, &
!a_title='A', &
!b_title='B', &
!c_title='C', &
!diagram_title='ternary diagram', &
!data_title='LLE', &
!title_diagram_font_size=40.0_srk, &
!title_axis_font_size=30.0_srk, &
!numbers_axis_font_size=25.0_srk, &
!font_rgb=(/0,0,0/), &
!axis_stroke_rgb=(/0,0,0/), &
!axis_stroke_width=3.0_srk, &
!draw_ticks=.false., &
!tick_length=10.0_srk, &
!tick_width=3.0_srk, &
!draw_grid=.true., &
!grid_stroke_width=2.0_srk, &
!grid_stroke_rgb=(/127,127,127/), &
!shape_type='diamond', &
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
!use Config, only: srk,default_string_length,pi,warnings_pause, &
!    & viewBox_width,viewBox_height

IMPLICIT NONE

! Parameters:
REAL(SRK),PARAMETER:: ARITHMETICAL_TOLERANCE=1.0E-3
! The threshold declared above is used as safety against arithmetical
! inconsistencies.
CHARACTER,PARAMETER:: FONT*(7)='verdana'
LOGICAL,PARAMETER:: CREATE_TOGGLE_BUTTONS=.TRUE.,CREATE_DATA_LINKS=.TRUE.

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),SHAPE_TYPE*(*),LINE_TYPE*(*)
CHARACTER,INTENT(IN):: DIAGRAM_TITLE*(*),A_TITLE*(*),B_TITLE*(*),C_TITLE*(*)
CHARACTER,INTENT(IN):: DATA_TITLE*(*)
REAL(SRK),INTENT(IN):: A_POINTS(:),B_POINTS(:),C_POINTS(:)
REAL(SRK),INTENT(IN):: AXIS_STROKE_WIDTH,GRID_STROKE_WIDTH
REAL(SRK),INTENT(IN):: SHAPE_SIZE,SHAPE_STROKE_WIDTH,LINE_STROKE_WIDTH
REAL(SRK),INTENT(IN):: TITLE_AXIS_FONT_SIZE,NUMBERS_AXIS_FONT_SIZE
REAL(SRK),INTENT(IN):: TITLE_DIAGRAM_FONT_SIZE
REAL(SRK),INTENT(IN):: TICK_LENGTH,TICK_WIDTH
INTEGER,INTENT(IN):: AXIS_STROKE_RGB(3),GRID_STROKE_RGB(3),FONT_RGB(3)
INTEGER,INTENT(IN):: SHAPE_FILL_RGB(3),SHAPE_STROKE_RGB(3),LINE_STROKE_RGB(3)
LOGICAL,INTENT(IN):: DRAW_TICKS,DRAW_GRID

! Private variables:
REAL(SRK):: TRIANGLE_BASE,TRIANGLE_HEIGHT,DIAGRAM_WIDTH,DIAGRAM_HEIGHT
REAL(SRK):: LEGEND_POSITION_X,LEGEND_POSITION_Y
REAL(SRK):: REAL_I,POINTS(SIZE(A_POINTS),2),XM,YM,AXIS_START
REAL(SRK),ALLOCATABLE:: BEZIER_POINTS(:,:)
INTEGER:: I

CHARACTER*(DEFAULT_STRING_LENGTH),ALLOCATABLE,TARGET,SAVE:: FILE_LIST(:)
CHARACTER*(DEFAULT_STRING_LENGTH),POINTER:: CHARACTER_POINTER_1D(:)
REAL(SRK),ALLOCATABLE,TARGET,SAVE:: LEGEND_SPACING(:)
REAL(SRK),POINTER:: REAL_POINTER_1D(:)
LOGICAL:: PLOT_AXES

CHARACTER*(DEFAULT_STRING_LENGTH):: CHARACTERS(3)
REAL(SRK):: NUMBERS(SIZE(A_POINTS),3)

LOGICAL,SAVE:: FIRST_RUN=.TRUE.

! Variable initialization
! axis_start: real, scalar. A value that specifies the margin left between the
!    border of the canvas and the lower left tip of the diagram (px).
DIAGRAM_WIDTH=VIEWBOX_HEIGHT
DIAGRAM_HEIGHT=VIEWBOX_HEIGHT
AXIS_START=2.0_SRK*TITLE_AXIS_FONT_SIZE
TRIANGLE_BASE=DIAGRAM_WIDTH-2.0_SRK*AXIS_START
TRIANGLE_HEIGHT=SQRT(3.0_SRK)*TRIANGLE_BASE/2.0_SRK
PLOT_AXES=.FALSE.
IF (ALLOCATED(BEZIER_POINTS)) DEALLOCATE(BEZIER_POINTS)
IF (.NOT.(ALLOCATED(FILE_LIST))) ALLOCATE(FILE_LIST(0))
IF (.NOT.(ALLOCATED(LEGEND_SPACING))) ALLOCATE(LEGEND_SPACING(0))
LEGEND_POSITION_Y=DIAGRAM_HEIGHT-AXIS_START
LEGEND_POSITION_X=DIAGRAM_WIDTH-AXIS_START

! ------------------------------------------------------------------------------

! Error control for arithmetical consistency:

FIRST_RUN=.FALSE. ! Delete this line to turn on some possibly obnoxious
                  ! arithmetical tolerance warnings.

IF (FIRST_RUN) THEN

    IF ((ANY(A_POINTS<0)).OR.(ANY(B_POINTS<0)).OR.(ANY(C_POINTS<0)).OR. &
        & (ANY(A_POINTS>1)).OR.(ANY(B_POINTS>1)).OR.(ANY(C_POINTS>1))) &
        & THEN
    WRITE(*,*)"SVG_TernaryPlot"
    WRITE(*,*)"WARNING: input fractions out of bounds."
    WRITE(*,*)"A tolerance level will be set."
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    FIRST_RUN=.FALSE.
    END IF

    DO I=1,SIZE(A_POINTS)
        IF (SUM((/A_POINTS(I),B_POINTS(I),C_POINTS(I)/))/=1) THEN
        WRITE(*,*)"SVG_TernaryPlot"
        WRITE(*,*)"WARNING: sum of a(i), b(i) and c(i) not equal to 1."
        WRITE(*,*)"sum(a+b+c): ", &
            & SUM((/A_POINTS(I),B_POINTS(I),C_POINTS(I)/))
        WRITE(*,*)"A tolerance level will be set."
        WRITE(*,*)"Continue..."
        IF (WARNINGS_PAUSE) READ(*,*)
        FIRST_RUN=.FALSE.
        EXIT
        END IF
    END DO

END IF

IF ((ANY(A_POINTS<0.0-ARITHMETICAL_TOLERANCE)).OR. &
    & (ANY(B_POINTS<0.0-ARITHMETICAL_TOLERANCE)).OR. &
    & (ANY(C_POINTS<0.0-ARITHMETICAL_TOLERANCE)).OR. &
    & (ANY(A_POINTS>1.0+ARITHMETICAL_TOLERANCE)).OR. &
    & (ANY(B_POINTS>1.0+ARITHMETICAL_TOLERANCE)).OR. &
    & (ANY(C_POINTS>1.0+ARITHMETICAL_TOLERANCE))) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"ERROR: molar fractions values are beyond tolerance levels."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

DO I=1,SIZE(A_POINTS)
IF ((A_POINTS(I)+B_POINTS(I)+C_POINTS(I)>1.0+ARITHMETICAL_TOLERANCE).OR. &
    & (A_POINTS(I)+B_POINTS(I)+C_POINTS(I)<1.0-ARITHMETICAL_TOLERANCE)) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"ERROR: molar fraction sums are beyond tolerance levels."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END DO

! ------------------------------------------------------------------------------

! Error control:

IF (VIEWBOX_HEIGHT>=VIEWBOX_WIDTH) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"ERROR: canvas height may not be bigger than canvas width"
WRITE(*,*)"for this type of graph"
WRITE(*,*)"canvas width: ",VIEWBOX_WIDTH
WRITE(*,*)"canvas height: ",VIEWBOX_HEIGHT
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((DIAGRAM_WIDTH-2.0_SRK*AXIS_START<=0).OR. &
    & (DIAGRAM_HEIGHT-2.0_SRK*AXIS_START<=0)) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"ERROR: invalid axis geometry parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SIZE(A_POINTS)/=SIZE(B_POINTS)).OR. &
    & (SIZE(A_POINTS)/=SIZE(C_POINTS)).OR. &
    & SIZE(B_POINTS)/=SIZE(C_POINTS)) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"ERROR: a, b and c coordinates do not match"
WRITE(*,*)"size(a): ",SIZE(A_POINTS)
WRITE(*,*)"size(b): ",SIZE(B_POINTS)
WRITE(*,*)"size(c): ",SIZE(C_POINTS)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((ALL(A_POINTS==0.0)).OR.(ALL(B_POINTS==0.0)).OR.(ALL(C_POINTS==0.0))) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"WARNING: all array points are equal to zero"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (ALL(SHAPE_TYPE/=(/'none    ','circle  ','square  ','cross   ', &
    & 'ex      ','triangle','diamond '/))) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"ERROR: incorrectly specified point shape"
WRITE(*,*)"shape_type: ",SHAPE_TYPE
WRITE(*,*)"supported values: 'none', 'circle', 'square',"
WRITE(*,*)"'cross', 'ex', 'triangle', 'diamond'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ALL(LINE_TYPE/=(/'none   ','crooked','bezier '/))) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"ERROR: incorrectly specified line type"
WRITE(*,*)"line_type: ",LINE_TYPE
WRITE(*,*)"supported values: 'none', 'crooked', 'bezier'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SHAPE_TYPE=='none').AND.(LINE_TYPE=='none')) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"WARNING: no data will be plotted"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((LEN_TRIM(A_TITLE)>DEFAULT_STRING_LENGTH).OR. &
    & (LEN_TRIM(B_TITLE)>DEFAULT_STRING_LENGTH).OR. &
    & (LEN_TRIM(C_TITLE)>DEFAULT_STRING_LENGTH)) THEN
WRITE(*,*)"SVG_TernaryPlot"
WRITE(*,*)"ERROR: component title too long"
WRITE(*,*)"len_trim(a_title): ",LEN_TRIM(A_TITLE)
WRITE(*,*)"len_trim(b_title): ",LEN_TRIM(B_TITLE)
WRITE(*,*)"len_trim(c_title): ",LEN_TRIM(C_TITLE)
WRITE(*,*)"default_string_length: ",DEFAULT_STRING_LENGTH
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

ADD_TO_DIAGRAM: IF (ANY(FILE_LIST==OUTPUT_FILE)) THEN

! Scan the filelist:
    DO I=1,SIZE(FILE_LIST)
    IF (FILE_LIST(I)==OUTPUT_FILE) THEN

! Resurrect the SVG document:
        CALL SVG_Resurrect(OUTPUT_FILE)
! Regulate spacing for the graph's legend:
        IF (LEN_TRIM(DATA_TITLE)/=0) LEGEND_SPACING(I)= &
            & LEGEND_SPACING(I)-1.5_SRK*NUMBERS_AXIS_FONT_SIZE
        EXIT

    END IF
    END DO

ELSE ADD_TO_DIAGRAM

! Initialize the diagram:
    PLOT_AXES=.TRUE.
    CALL SVG_Initialize(OUTPUT_FILE)

! Add the filename to the list of initialized files:
    CHARACTER_POINTER_1D=>FILE_LIST
    CALL ArrayExpandCharacter(CHARACTER_POINTER_1D,OUTPUT_FILE)
    DEALLOCATE(FILE_LIST)
    ALLOCATE(FILE_LIST(SIZE(CHARACTER_POINTER_1D)))
    FILE_LIST=CHARACTER_POINTER_1D

! Initialize a legend:
    REAL_POINTER_1D=>LEGEND_SPACING
    CALL ArrayExpandReal(REAL_POINTER_1D,LEGEND_POSITION_Y)
    DEALLOCATE(LEGEND_SPACING)
    ALLOCATE(LEGEND_SPACING(SIZE(REAL_POINTER_1D)))
    LEGEND_SPACING=REAL_POINTER_1D

END IF ADD_TO_DIAGRAM

! ------------------------------------------------------------------------------

AXES_PLOT: IF (PLOT_AXES) THEN

! Diagram title:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=DIAGRAM_TITLE, &
    & X=DIAGRAM_WIDTH/2.0_SRK,Y=DIAGRAM_HEIGHT-AXIS_START, &
    & FONT=FONT,FONT_SIZE=TITLE_DIAGRAM_FONT_SIZE,FILL_OPACITY=1.0_SRK, &
    & FILL_RGB=FONT_RGB,ANCHOR='middle')

! Draw the axes.

! The coordinates of the three triangle points are:
! x_a=axis_start                    (lower left triangle corner)
! y_a=axis_start                    (lower left triangle corner)
! x_b=axis_start+triangle_base/2.0  (upper triangle corner)
! y_b=axis_start+triangle_height    (upper triangle corner)
! x_c=axis_start+triangle_base      (lower right triangle corner)
! y_c=axis_start                    (lower right triangle corner)

CALL SVG_GroupStart(OUTPUT_FILE)

! Horizontal axis:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START,AXIS_START+TRIANGLE_BASE/), &
    & Y=(/AXIS_START,AXIS_START/), &
    STROKE_WIDTH=AXIS_STROKE_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

! Forward slash axis:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START,AXIS_START+TRIANGLE_BASE/2.0_SRK/), &
    & Y=(/AXIS_START,AXIS_START+TRIANGLE_HEIGHT/), &
    & STROKE_WIDTH=AXIS_STROKE_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

! Backward slash axis:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START+TRIANGLE_BASE,AXIS_START+TRIANGLE_BASE/2.0_SRK/), &
    & Y=(/AXIS_START,AXIS_START+TRIANGLE_HEIGHT/), &
    & STROKE_WIDTH=AXIS_STROKE_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

! Axes numbers.
AXES_NUMBERS_LOOP: DO I=0,10

! Left triangle side numbering:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE, &
    & STRING=TRIM(FormatReal(REAL(10-I)/10.0_SRK,1,.FALSE.)), &
    & X=AXIS_START+REAL(I)*TRIANGLE_BASE/20.0_SRK- &
        & NUMBERS_AXIS_FONT_SIZE/2.0_SRK, &
    & Y=AXIS_START+REAL(I)*TRIANGLE_HEIGHT/10.0- &
        & NUMBERS_AXIS_FONT_SIZE/3.0_SRK, &
    & FONT=FONT,FONT_SIZE=NUMBERS_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=AXIS_STROKE_RGB,ANCHOR='end')

! Right triangle side numbering:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE, &
    & STRING=TRIM(FormatReal(REAL(I)/10.0_SRK,1,.FALSE.)), &
    & X=AXIS_START+REAL(10-I)*TRIANGLE_BASE/20.0_SRK+ &
        & TRIANGLE_BASE/2.0_SRK+NUMBERS_AXIS_FONT_SIZE/2.0_SRK, &
    & Y=AXIS_START+REAL(I)*TRIANGLE_HEIGHT/10.0_SRK- &
        & NUMBERS_AXIS_FONT_SIZE/3.0_SRK, &
    & FONT=FONT,FONT_SIZE=NUMBERS_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=AXIS_STROKE_RGB)

! Triangle base numbering:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE, &
    & STRING=TRIM(FormatReal(REAL(I)/10.0_SRK,1,.FALSE.)), &
    & X=AXIS_START+REAL(I)*TRIANGLE_BASE/10.0_SRK, &
    & Y=AXIS_START-5.0_SRK*NUMBERS_AXIS_FONT_SIZE/4.0_SRK, &
    & FONT=FONT,FONT_SIZE=NUMBERS_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=AXIS_STROKE_RGB,ANCHOR='middle')

END DO AXES_NUMBERS_LOOP

! Axes titles.

! A title (lower left triangle corner):
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=A_TITLE, &
    & X=AXIS_START-LEN_TRIM(A_TITLE)*2.0_SRK*TITLE_AXIS_FONT_SIZE, &
    & Y=AXIS_START-3.0_SRK*TITLE_AXIS_FONT_SIZE/2.0_SRK, &
    & FONT=FONT,FONT_SIZE=TITLE_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB)

! B title (upper triangle corner):
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=B_TITLE, &
    & X=DIAGRAM_WIDTH/2.0_SRK, &
    & Y=AXIS_START+TRIANGLE_HEIGHT+TITLE_AXIS_FONT_SIZE, &
    & FONT=FONT,FONT_SIZE=TITLE_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB,ANCHOR='middle')

! C title (lower right triangle corner):
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=C_TITLE, &
    & X=TRIANGLE_BASE+AXIS_START+ &
        & LEN_TRIM(C_TITLE)*3.0_SRK*TITLE_AXIS_FONT_SIZE/2.0_SRK, &
    & Y=AXIS_START-3.0_SRK*TITLE_AXIS_FONT_SIZE/2.0_SRK, &
    & FONT=FONT,FONT_SIZE=TITLE_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB)

! Axes' ticks

TICK_DRAW: IF (DRAW_TICKS) THEN

TICK_LOOP: DO I=1,9

REAL_I=I

! Left ticks:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START+REAL_I*TRIANGLE_BASE/20.0_SRK, &
        & AXIS_START+REAL_I*TRIANGLE_BASE/20.0_SRK+TICK_LENGTH/), &
    & Y=(/AXIS_START+REAL_I*TRIANGLE_HEIGHT/10.0_SRK, &
        & AXIS_START+REAL_I*TRIANGLE_HEIGHT/10.0_SRK/), &
    & STROKE_WIDTH=TICK_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

! Right ticks:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START+TRIANGLE_BASE-REAL_I*TRIANGLE_BASE/20.0_SRK-TICK_LENGTH, &
        & AXIS_START+TRIANGLE_BASE-REAL_I*TRIANGLE_BASE/20.0_SRK/), &
    & Y=(/AXIS_START+REAL_I*TRIANGLE_HEIGHT/10.0_SRK, &
        & AXIS_START+REAL_I*TRIANGLE_HEIGHT/10.0_SRK/), &
    & STROKE_WIDTH=TICK_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

! Bottom ticks:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START+REAL_I*TRIANGLE_BASE/10.0_SRK, &
    & AXIS_START+REAL_I*TRIANGLE_BASE/10.0_SRK/), &
    & Y=(/AXIS_START, &
    & AXIS_START+TICK_LENGTH/), &
    & STROKE_WIDTH=TICK_WIDTH,STROKE_RGB=AXIS_STROKE_RGB)

END DO TICK_LOOP

END IF TICK_DRAW

CALL SVG_GroupEnd(OUTPUT_FILE)

! Draw the gridlines:

GRID_DRAW: IF (DRAW_GRID) THEN

CALL SVG_GroupStart(OUTPUT_FILE)

GRIDLINES_LOOP: DO I=1,9

REAL_I=I

! Horizontal grid lines:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START+REAL_I*TRIANGLE_BASE/20.0_SRK, &
        & AXIS_START+TRIANGLE_BASE-REAL_I*TRIANGLE_BASE/20.0_SRK/), &
    & Y=(/AXIS_START+REAL_I*TRIANGLE_HEIGHT/10.0_SRK, &
        & AXIS_START+REAL_I*TRIANGLE_HEIGHT/10.0_SRK/), &
    & STROKE_WIDTH=GRID_STROKE_WIDTH,STROKE_RGB=GRID_STROKE_RGB)

! Forward slash grid lines:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START+REAL_I*TRIANGLE_BASE/10.0_SRK, &
    & AXIS_START+TRIANGLE_BASE/2.0_SRK+REAL_I*TRIANGLE_BASE/20.0_SRK/), &
    & Y=(/AXIS_START, &
    & AXIS_START+TRIANGLE_HEIGHT-REAL_I*TRIANGLE_HEIGHT/10.0_SRK/), &
    & STROKE_WIDTH=GRID_STROKE_WIDTH,STROKE_RGB=GRID_STROKE_RGB)

! Backward slash grid lines:
CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/AXIS_START+TRIANGLE_BASE-REAL_I*TRIANGLE_BASE/10.0_SRK, &
    & AXIS_START+TRIANGLE_BASE/2.0_SRK-REAL_I*TRIANGLE_BASE/20.0_SRK/), &
    & Y=(/AXIS_START, &
    & AXIS_START+TRIANGLE_HEIGHT-REAL_I*TRIANGLE_HEIGHT/10.0_SRK/), &
    & STROKE_WIDTH=GRID_STROKE_WIDTH,STROKE_RGB=GRID_STROKE_RGB)

END DO GRIDLINES_LOOP

CALL SVG_GroupEnd(OUTPUT_FILE)

END IF GRID_DRAW

END IF AXES_PLOT

! ------------------------------------------------------------------------------

! Task: calculate the coordinates of the points in cartesian coordinates by
! using the triangular coordinates.
! Solution:
! The cartesian coordinates of each point can be calculated based only on the
! b and c triangular coordinates of the points.

! The y cartesian coordinates of each point can be calculated quickly from
! the triangular coordiates of the b component:
POINTS(:,2)=AXIS_START+B_POINTS(:)*TRIANGLE_HEIGHT

COORDINATE_CONVERSION_LOOP: DO I=1,SIZE(A_POINTS)

! For every point, calculate the cross point between the AB side of the
! triangle and a horizontal line at y=points(i,2). The array x2 in the
! following call uses the A and C points of the triangle (since actual points
! are needed to define the y=points(i,2) line).
CALL CrossPoint( &
    X1=(/AXIS_START,AXIS_START+TRIANGLE_BASE/2.0_SRK/), &
    X2=(/AXIS_START,AXIS_START+TRIANGLE_BASE/), &
    Y1=(/AXIS_START,AXIS_START+TRIANGLE_HEIGHT/), &
    Y2=(/POINTS(I,2),POINTS(I,2)/), &
    XM=XM,YM=YM)

! Calculate the x cartesian coordinate of each point by using the triangular
! coordiate of the c component and the fact that the angle between the AB side
! of the triangle and all hozontal lines is 60 degrees.
POINTS(I,1)=XM+(C_POINTS(I)*TRIANGLE_HEIGHT)/ABS((SIN(PI/3.0_SRK)))

END DO COORDINATE_CONVERSION_LOOP

! ------------------------------------------------------------------------------

CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=TRIM(DATA_TITLE))

! Diagram line:

SELECT CASE(TRIM(LINE_TYPE))

CASE('crooked')

CALL SVG_Polyline(OUTPUT_FILE=OUTPUT_FILE,X=POINTS(:,1),Y=POINTS(:,2), &
    & STROKE_WIDTH=LINE_STROKE_WIDTH,FILL_OPACITY=0.0_SRK, &
    & STROKE_RGB=LINE_STROKE_RGB,FILL_RGB=(/255,255,255/), &
    & ID=TRIM(DATA_TITLE))

CASE('bezier')

! In order for the Bezier curve to be drawn correctly, the array with the
! Bezier curve and control points needs to have an odd amount of elements.
IF (MOD(SIZE(A_POINTS),2)==0) THEN
ALLOCATE(BEZIER_POINTS(SIZE(A_POINTS)+1,2))
BEZIER_POINTS(SIZE(A_POINTS)+1,1)=POINTS(SIZE(A_POINTS),1)
BEZIER_POINTS(SIZE(A_POINTS)+1,2)=POINTS(SIZE(A_POINTS),2)
ELSE
ALLOCATE(BEZIER_POINTS(SIZE(A_POINTS),2))
END IF

DO I=1,SIZE(A_POINTS)
IF ((MOD(I,2)==0).AND.(I/=1).AND.(I/=SIZE(A_POINTS))) THEN

! The Bezier control points are calculated:
BEZIER_POINTS(I,:)=BezierQuadraticControl( &
    (/POINTS(I-1,1),POINTS(I-1,2)/), &
    (/POINTS(I+1,1),POINTS(I+1,2)/), &
    (/POINTS(I,1),POINTS(I,2)/))

ELSE

! The rest of the Bezier curve points are the same as the diagram points.
BEZIER_POINTS(I,:)=POINTS(I,:)

END IF
END DO

CALL SVG_BezierQuadratic(OUTPUT_FILE=OUTPUT_FILE, &
    & X=BEZIER_POINTS(:,1),Y=BEZIER_POINTS(:,2), &
    & STROKE_WIDTH=LINE_STROKE_WIDTH,FILL_OPACITY=0.0_SRK, &
    & STROKE_RGB=LINE_STROKE_RGB,FILL_RGB=(/255,255,255/), &
    & ID=TRIM(DATA_TITLE))

END SELECT

! Diagram points:

CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=(TRIM(DATA_TITLE)//'_points'))

CHARACTERS(1)=TRIM(A_TITLE)//'_coordinate'
CHARACTERS(2)=TRIM(B_TITLE)//'_coordinate'
CHARACTERS(3)=TRIM(C_TITLE)//'_coordinate'

DIAGRAM_POINT_LOOP: DO I=1,SIZE(A_POINTS)

SELECT CASE(TRIM(SHAPE_TYPE))

CASE('circle')
CALL SVG_Circle(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),RADIUS=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/A_POINTS(I),B_POINTS(I),C_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('square')
CALL SVG_Square(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/A_POINTS(I),B_POINTS(I),C_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('diamond')
CALL SVG_Square(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ROTATE=(/45.0_SRK,POINTS(I,1),POINTS(I,2)/), &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/A_POINTS(I),B_POINTS(I),C_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('cross')
CALL SVG_Cross(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,STROKE_RGB=SHAPE_STROKE_RGB, &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/A_POINTS(I),B_POINTS(I),C_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('ex')
CALL SVG_Cross(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,STROKE_RGB=SHAPE_STROKE_RGB, &
    & ROTATE=(/45.0_SRK,POINTS(I,1),POINTS(I,2)/), &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/A_POINTS(I),B_POINTS(I),C_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

CASE('triangle')
CALL SVG_Triangle(OUTPUT_FILE=OUTPUT_FILE, &
    & X=POINTS(I,1),Y=POINTS(I,2),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & SCALAR_NAME=CHARACTERS, &
    & SCALAR_VALUE=(/A_POINTS(I),B_POINTS(I),C_POINTS(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_'//TRIM(StringInteger(I))))

END SELECT

END DO DIAGRAM_POINT_LOOP

CALL SVG_GroupEnd(OUTPUT_FILE)

CALL SVG_GroupEnd(OUTPUT_FILE)

! ------------------------------------------------------------------------------

! Make a legend:
CREATE_LEGEND: IF (LEN_TRIM(DATA_TITLE)/=0) THEN

CALL SVG_GroupStart(OUTPUT_FILE)

DO I=1,SIZE(FILE_LIST)
IF (FILE_LIST(I)==OUTPUT_FILE) THEN

! Legend lines:
IF (TRIM(LINE_TYPE)/='none') CALL SVG_Line(OUTPUT_FILE=OUTPUT_FILE, &
    & X=(/LEGEND_POSITION_X-NUMBERS_AXIS_FONT_SIZE, &
    & LEGEND_POSITION_X+NUMBERS_AXIS_FONT_SIZE/), &
    & Y=(/LEGEND_SPACING(I),LEGEND_SPACING(I)/), &
    & STROKE_WIDTH=LINE_STROKE_WIDTH,STROKE_RGB=LINE_STROKE_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_line'))

! Legend points:
LEGEND_POINTS: IF (TRIM(SHAPE_TYPE)/='none') THEN

SELECT CASE(TRIM(SHAPE_TYPE))

CASE('circle')
CALL SVG_Circle(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),RADIUS=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('square')
CALL SVG_Square(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('diamond')
CALL SVG_Square(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ROTATE=(/45.0_SRK,LEGEND_POSITION_X,LEGEND_SPACING(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('cross')
CALL SVG_Cross(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,STROKE_RGB=SHAPE_STROKE_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('ex')
CALL SVG_Cross(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,STROKE_RGB=SHAPE_STROKE_RGB, &
    & ROTATE=(/45.0_SRK,LEGEND_POSITION_X,LEGEND_SPACING(I)/), &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

CASE('triangle')
CALL SVG_Triangle(OUTPUT_FILE=OUTPUT_FILE, &
    & X=LEGEND_POSITION_X,Y=LEGEND_SPACING(I),SIDE=SHAPE_SIZE, &
    & STROKE_WIDTH=SHAPE_STROKE_WIDTH,FILL_OPACITY=1.0_SRK, &
    & STROKE_RGB=SHAPE_STROKE_RGB,FILL_RGB=SHAPE_FILL_RGB, &
    & ID=(TRIM(DATA_TITLE)//'_legend_point'))

END SELECT

END IF LEGEND_POINTS

CALL SVG_GroupEnd(OUTPUT_FILE)

! Add text to the graph legend:
CALL SVG_Text(OUTPUT_FILE=OUTPUT_FILE,STRING=DATA_TITLE, &
    & X=LEGEND_POSITION_X+2.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
    & Y=LEGEND_SPACING(I)-NUMBERS_AXIS_FONT_SIZE/3.0_SRK, &
    & FONT=FONT,FONT_SIZE=NUMBERS_AXIS_FONT_SIZE, &
    & FILL_OPACITY=1.0_SRK,FILL_RGB=FONT_RGB)

! ------------------------------------------------------------------------------

! Create a toggle button.
IF (CREATE_TOGGLE_BUTTONS) THEN
    CALL SVG_Vanish(OUTPUT_FILE=OUTPUT_FILE)
    CALL SVG_ActionButton(OUTPUT_FILE=OUTPUT_FILE, &
        & SCRIPT='vanish',TARGET_ID=TRIM(DATA_TITLE), &
        & X=VIEWBOX_WIDTH-3.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
        & Y=LEGEND_SPACING(I), &
        & WIDTH=4.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
        & HEIGHT=NUMBERS_AXIS_FONT_SIZE, &
        & STROKE_WIDTH=GRID_STROKE_WIDTH,BUTTON_FILL_OPACITY=1.0_SRK, &
        & STROKE_RGB=AXIS_STROKE_RGB,BUTTON_FILL_RGB=(/255,255,255/), &
        & STRING='toggle',FONT='verdana', &
        & FONT_SIZE=2.0_SRK*NUMBERS_AXIS_FONT_SIZE/3.0_SRK, &
        & TEXT_FILL_OPACITY=1.0_SRK,TEXT_FILL_RGB=FONT_RGB)
END IF

! Create a link button to the raw data.
IF (CREATE_DATA_LINKS) THEN
! Create the HTML table:
    CHARACTERS(1)=TRIM(A_TITLE)
    CHARACTERS(2)=TRIM(B_TITLE)
    CHARACTERS(3)=TRIM(C_TITLE)
    NUMBERS(:,1)=A_POINTS
    NUMBERS(:,2)=B_POINTS
    NUMBERS(:,3)=C_POINTS
    CALL HTML_Initialize(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html')
    CALL HTML_Heading(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html', &
        & STRING=TRIM(OUTPUT_FILE),LEVEL=3)
    CALL HTML_Heading(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html', &
        & STRING=TRIM(DIAGRAM_TITLE)//": "//TRIM(DATA_TITLE),LEVEL=4)
    CALL HTML_Table(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html', &
        & NUMBERS=NUMBERS,HEADINGS=CHARACTERS, &
        & DECIMAL_PLACES=(/4,4,4/), &
        & SCIENTIFIC=(/.FALSE.,.FALSE.,.FALSE./))
    CALL HTML_Finalize(OUTPUT_FILE= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html')
! Create the link:
    CALL SVG_LinkButton(OUTPUT_FILE=OUTPUT_FILE,LINK_STRING= &
        & TRIM(OUTPUT_FILE)//'_'//TRIM(DATA_TITLE)//'.html', &
        & X=VIEWBOX_WIDTH-7.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
        & Y=LEGEND_SPACING(I), &
        & WIDTH=3.0_SRK*NUMBERS_AXIS_FONT_SIZE, &
        & HEIGHT=NUMBERS_AXIS_FONT_SIZE, &
        & STROKE_WIDTH=GRID_STROKE_WIDTH,BUTTON_FILL_OPACITY=1.0_SRK, &
        & STROKE_RGB=AXIS_STROKE_RGB,BUTTON_FILL_RGB=(/255,255,255/), &
        & STRING='data',FONT='verdana', &
        & FONT_SIZE=2.0_SRK*NUMBERS_AXIS_FONT_SIZE/3.0_SRK, &
        & TEXT_FILL_OPACITY=1.0_SRK,TEXT_FILL_RGB=FONT_RGB)
END IF

END IF
END DO

END IF CREATE_LEGEND

CALL SVG_Finalize(OUTPUT_FILE)

END SUBROUTINE SVG_TernaryPlot

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Text(OUTPUT_FILE,STRING,X,Y, &
    & FONT,FONT_SIZE,FILL_OPACITY,FILL_RGB, &
    & BOLD,ITALIC,ANCHOR,ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Text is called to draw a text string in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! string: character, scalar. The string that will be drawn.
! x: real, scalar. The x-axis coordinate of the text position (px). See also
!    optional variable "anchor" for the text position definition.
! y: real, scalar. The y-axis coordinate of the text position (px). See also
!    optional variable "anchor" for the text position definition.
! font: character, scalar. The font that will be used for the rendering. The
!    selected font must be supported in the system that will be used to
!    render the SVG file.
! font_size: real, scalar. The size of the font.
! fill_opacity: real, scalar. The opacity of the text fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! bold: logical, scalar. When .true. the boldface type of the specified font
!    will be used for the rendering.
! italic: logical, scalar. When .true. the italic type of the specified font
!    will be used for the rendering.
! anchor: character, scalar. Specifies the alignment of the text respective to
!    the provided text position. Supported values: "start", "middle", "end".
!    If ommitted, the default value is "start".
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Text(output_file,string,x,y, &
!    & font,font_size,fill_opacity,fill_rgb, &
!    & bold,italic,anchor,rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),string*(*),font*(*)
!real(srk),intent(in):: x,y,fill_opacity,font_size
!integer,intent(in):: fill_rgb(3)
!logical,optional,intent(in):: bold,italic
!character,optional,intent(in):: scalar_name(:)*(*),anchor*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Text
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Text(output_file='plot.svg',string='hello world', &
!    & x=100.0_srk,y=200.0_srk,font='verdana',font_size=45.0_srk, &
!    & fill_opacity=1.0_srk,fill_rgb=(/255,0,0/), &
!    & bold=.false.,italic=.true.,anchor='middle', &
!    & rotate=(/-25.0_srk,100.0_srk,200.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),STRING*(*),FONT*(*)
REAL(SRK),INTENT(IN):: X,Y,FILL_OPACITY,FONT_SIZE
INTEGER,INTENT(IN):: FILL_RGB(3)
LOGICAL,OPTIONAL,INTENT(IN):: BOLD,ITALIC
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ANCHOR*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED,PRESENT_ANCHOR

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)
PRESENT_ANCHOR=PRESENT(ANCHOR)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (LEN_TRIM(STRING)==0) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"WARNING: missing string, nothing will be printed."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((Y>VIEWBOX_HEIGHT).OR.(X>VIEWBOX_WIDTH)) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((1.0<FILL_OPACITY).OR.(FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"ERROR: invalid paint parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (FILL_OPACITY==0.0) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"WARNING: this fill-opacity will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(FILL_RGB<0)).OR.(ANY(FILL_RGB>255))) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT_ANCHOR) THEN
IF (ALL(ANCHOR/=(/"start ", "middle", "end   "/))) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"WARNING: anchor value not supported."
WRITE(*,*)"This setting will be ignored."
WRITE(*,*)"Continue..."
PRESENT_ANCHOR=.FALSE.
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

! In the SVG 1.1 specification there is no special treatment of superscripts and
! subscripts. Moreover, at the moment of this writing, the various SVG viewers
! exhibited different interpretations of the SVG 1.1 specification concerning
! the rendering of the "tspan" element. Calculating the positions of all
! superscripts and subscripts "by hand" in the fortran code would implicitly
! mean the authoring of a typesetting tool, which is not the purpose of this
! library. Sadly therefore, there is no support for suprescripts, subscripts and
! other mathematic notation for the time being.

WRITE(IO,'(A5)',IOSTAT=ERR)"<text"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Text"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"x='"//TRIM(StringReal(X))//"'"
WRITE(IO,*)"y='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y))//"'"
WRITE(IO,*)"font-family='"//TRIM(FONT)//"'"
WRITE(IO,*)"font-size='"//TRIM(StringReal(FONT_SIZE))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(FILL_OPACITY))//"'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(FILL_RGB(1)))//","// &
        & TRIM(StringInteger(FILL_RGB(2)))//","// &
        & TRIM(StringInteger(FILL_RGB(3)))//")'"

IF (PRESENT_ANCHOR) WRITE(IO,*)"text-anchor='"//TRIM(ANCHOR)//"'"

IF (PRESENT(BOLD)) THEN
IF (BOLD) WRITE(IO,*)"font-weight='bold'"
END IF

IF (PRESENT(ITALIC)) THEN
IF (ITALIC) WRITE(IO,*)"font-style='italic'"
END IF

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A1)')">"

WRITE(IO,*)TRIM(STRING)

WRITE(IO,'(A7)')"</text>"
WRITE(IO,*)

END SUBROUTINE SVG_Text

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Triangle(OUTPUT_FILE,X,Y,SIDE, &
    & STROKE_WIDTH,FILL_OPACITY,STROKE_RGB,FILL_RGB, &
    & ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Triangle is called to draw a triangle in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, scalar. The x-axis coordinate of the triangle center. (px).
! y: real, scalar. The y-axis coordinate of the triangle center. (px).
! side: real, scalar. The length of the triangle side. (px).
! stroke_width: real, scalar. The thickness of the triangle line (px).
! fill_opacity: real, scalar. The opacity of the fill (dimensionless,
!    0.0<=fill_opacity<=1.0).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).
! fill_rgb: integer, array (1D) with 3 elements. Contains the fill RGB channel
!    values (dimensionless, 0<=fill_rgb(:)<=255).

! INPUT (OPTIONAL):
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Triangle(output_file,x,y,side, &
!    & stroke_width,fill_opacity,stroke_rgb,fill_rgb, &
!    & rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x,y,side,stroke_width,fill_opacity
!integer,intent(in):: stroke_rgb(3),fill_rgb(3)
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Triangle
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Triangle(output_file='plot.svg', &
!    & x=350.0_srk,y=350.0_srk,side=50.0_srk, &
!    & stroke_width=5.0_srk,fill_opacity=1.0_srk, &
!    & stroke_rgb=(/0,0,0/),fill_rgb=(/255,0,0/), &
!    & rotate=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height,pi

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X,Y,SIDE,STROKE_WIDTH,FILL_OPACITY
INTEGER,INTENT(IN):: STROKE_RGB(3),FILL_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
REAL(SRK):: X_POINTS(3),Y_POINTS(3),VARIABLE
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! The code below defines an equilateral triangle whose user-defined center is
! its barycenter.
!variable=(side/2.0_srk)/cos(pi/6.0_srk)
!x_points(1)=x
!y_points(1)=y+variable
!x_points(2)=x+side/2.0_srk
!y_points(2)=y-variable*sin(pi/6.0_srk)
!x_points(3)=x-side/2.0_srk
!y_points(3)=y-variable*sin(pi/6.0_srk)

! The code below defines a triangle whose user-defined side is its bottom side
! that is equal to its height.
VARIABLE=SIDE/2.0_SRK
X_POINTS(1)=X
Y_POINTS(1)=Y+VARIABLE
X_POINTS(2)=X+VARIABLE
Y_POINTS(2)=Y-VARIABLE
X_POINTS(3)=X-VARIABLE
Y_POINTS(3)=Y-VARIABLE

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((Y>VIEWBOX_HEIGHT).OR.(X>VIEWBOX_WIDTH)) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (SIDE<0.0) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: invalid shape parameters"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIDE==0.0) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"WARNING: SVG object will not be viewable."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH<0.0) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((1.0<FILL_OPACITY).OR.(FILL_OPACITY<0.0)) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: invalid paint parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FILL_OPACITY==0.0).AND.(STROKE_WIDTH==0.0)) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"WARNING: this combination of fill-opacity and"
WRITE(*,*)"stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(FILL_RGB<0)).OR.(ANY(STROKE_RGB<0)).OR. &
    & (ANY(FILL_RGB>255)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"size(scalar_name): ",SIZE(SCALAR_NAME)
WRITE(*,*)"size(scalar_value): ",SIZE(SCALAR_VALUE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A8)',IOSTAT=ERR)"<polygon"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Triangle"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"points='"
DO I=1,SIZE(X_POINTS)-1
    WRITE(IO,*)TRIM(StringReal(X_POINTS(I)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-Y_POINTS(I)))
END DO
WRITE(IO,*)TRIM(StringReal(X_POINTS(SIZE(X_POINTS))))//","// &
    & TRIM(StringReal(VIEWBOX_HEIGHT-Y_POINTS(SIZE(Y_POINTS))))//"'"

WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"fill-opacity='"//TRIM(StringReal(FILL_OPACITY))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(STROKE_RGB(1)))//","// &
        & TRIM(StringInteger(STROKE_RGB(2)))//","// &
        & TRIM(StringInteger(STROKE_RGB(3)))//")'"
WRITE(IO,*)"fill='rgb("// &
        & TRIM(StringInteger(FILL_RGB(1)))//","// &
        & TRIM(StringInteger(FILL_RGB(2)))//","// &
        & TRIM(StringInteger(FILL_RGB(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_Triangle

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Vanish(OUTPUT_FILE)

! SVG_Vanish is called to print the "vanish" ECMAScript function in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!	the ".svg" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Vanish(output_file)
!implicit none
!character,intent(in):: output_file*(*)
!end subroutine SVG_Vanish
!end interface

!real(srk):: id=1.0
!call SVG_Initialize(output_file='plot.svg')
!call SVG_Vanish(output_file='plot.svg')
!call SVG_HideButton(output_file='plot.svg',id=id, &
!	& x=200.0_srk,y=500.0_srk,width=200.0_srk,height=50.0_srk, &
!	& stroke_width=5.0_srk,button_fill_opacity=1.0_srk, &
!	& stroke_rgb=(/0,0,255/),button_fill_rgb=(/255,255,255/), &
!	& string='toggle circle',font='verdana',font_size=20.0_srk, &
!	& text_fill_opacity=1.0_srk,text_fill_rgb=(/0,0,0/))
!call SVG_Circle(output_file='plot.svg', &
!	& x=200.0_srk,y=100.0_srk,radius=50.0_srk, &
!	& stroke_width=10.0_srk,fill_opacity=1.0_srk, &
!	& stroke_rgb=(/255,0,0/),fill_rgb=(/255,255,255/), &
!	& rotate=(/0.0_srk,100.0_srk,100.0_srk/), &
!	& scalar_name=(/'id'/),scalar_value=(/id/))
!call SVG_Finalize(output_file='plot.svg')

! Example of SVG application:

!<svg viewBox='0 0 800 800'
!xmlns='http://www.w3.org/2000/svg' version='1.1'>
!
!<script type='text/ecmascript'> <![CDATA[
!function vanish(evt,id) {
!	var document = evt.target.ownerDocument;
!	var object = document.getElementById(id);
!	var CurrentOpacity = object.getAttribute('opacity');
!	if (CurrentOpacity == '0')
!		object.setAttribute('opacity', '1');
!	else
!		object.setAttribute('opacity', '0');
!}
!]]> </script>
!
!<circle onclick='vanish(evt,"1.0")' cx='10' cy='20' r='10' fill='black' />
!<circle onclick='vanish(evt,"2.0")' cx='50' cy='20' r='10' fill='green' />
!
!<g id="1.0">
!	<circle id="2.0" cx='200' cy='225' r='100' fill='red' />
!	<circle cx='500' cy='225' r='100' fill='blue' />
!</g>
!
!</svg>

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

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Vanish"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Vanish"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Vanish"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A41)',IOSTAT=ERR)"<script type='text/ecmascript'> <![CDATA["

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Vanish"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"function vanish(evt,id) {"
WRITE(IO,*)"    var document=evt.target.ownerDocument;"
WRITE(IO,*)"    var object=document.getElementById(id);"
WRITE(IO,*)"    var CurrentOpacity=object.getAttribute('opacity');"
WRITE(IO,*)"if (CurrentOpacity=='0')"
WRITE(IO,*)"    object.setAttribute('opacity','1');"
WRITE(IO,*)"else"
WRITE(IO,*)"    object.setAttribute('opacity','0');"
WRITE(IO,*)"}"
WRITE(IO,*)"]]> </script>"
WRITE(IO,*)

END SUBROUTINE SVG_Vanish

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_Vector(OUTPUT_FILE,X,Y,STROKE_WIDTH,STROKE_RGB,HEAD_SIZE, &
    & CHANGE_MARKER,ROTATE,SCALAR_NAME,SCALAR_VALUE,ID)

! SVG_Vector is called to draw a vector in an SVG file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, array (1D) with 2 elements. The x-axis coordinates of the vector
!    beginning (1st element) and the vector end (2nd element) (px).
! y: real, array (1D) with 2 elements. The y-axis coordinates of the vector
!    beginning (1st element) and the vector end (2nd element) (px).
! head_size: real, scalar. The size of the arrow head, respective to the
!    specified stroke width (dimensionless).
! stroke_width: real, scalar. The thickness of the vector line (px).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255).

! INPUT (OPTIONAL):
! change_marker: logical, scalar. If present and .true., it defines that
!    the arrowhead properties will be set according to input variables
!    "head_size" and "stroke_rgb". If abscent or .false. variables
!    "head_size" and "stroke_rgb" will be ignored and the arrowhead used in
!    the last call will be recycled. If variable "change_marker" is abscent
!    or .false. upon the first call of the subroutine, then the arrowhead
!    will be defined by the values of variables "head_size" and "stroke_rgb"
!    in that call.
! rotate: real, array (1D) with 3 elements. The first element is the relative to
!    the horizontal level angle in degrees that the object is going to be
!    rotated by. The second and the third elements are the x- and
!    y-coordinates of the rotation center respectively.
! scalar_name: character, array (1D). The names of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (1D). The values of the non-SVG variables that
!    will be stored in the SVG object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_Vector(output_file,x,y,stroke_width,stroke_rgb,head_size, &
!    & change_marker,rotate,scalar_name,scalar_value,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x(2),y(2),head_size,stroke_width
!integer,intent(in):: stroke_rgb(3)
!logical,optional,intent(in):: change_marker
!character,optional,intent(in):: scalar_name(:)*(*),id*(*)
!real(srk),optional,intent(in):: rotate(3),scalar_value(:)
!end subroutine SVG_Vector
!end interface

!call SVG_Initialize(output_file='plot.svg')
!call SVG_Vector(output_file='plot.svg', &
!    & x=(/100.0_srk,500.0_srk/),y=(/100.0_srk,500.0_srk/), &
!    & head_size=10.0_srk,stroke_width=3.0_srk,stroke_rgb=(/255,0,0/), &
!    & change_marker=.true.,rotate=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='1')
!! The second vector is different than the first, but the changes are not
!! applied: change_marker=.false.
!call SVG_Vector(output_file='plot.svg', &
!    & x=(/100.0_srk,200.0_srk/),y=(/100.0_srk,500.0_srk/), &
!    & head_size=10.0_srk,stroke_width=3.0_srk,stroke_rgb=(/0,0,255/), &
!    & change_marker=.false.,rotate=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='2')
!! The third vector is the same as the second, but now the changes are
!! applied: change_marker=.true.
!call SVG_Vector(output_file='plot.svg', &
!    & x=(/100.0_srk,100.0_srk/),y=(/100.0_srk,500.0_srk/), &
!    & head_size=10.0_srk,stroke_width=3.0_srk,stroke_rgb=(/0,0,255/), &
!    & change_marker=.true.,rotate=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & scalar_name=(/'property'/),scalar_value=(/3.14_srk/),id='3')
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause,viewBox_width,viewBox_height

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X(2),Y(2),HEAD_SIZE,STROKE_WIDTH
INTEGER,INTENT(IN):: STROKE_RGB(3)
LOGICAL,OPTIONAL,INTENT(IN):: CHANGE_MARKER
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: ROTATE(3),SCALAR_VALUE(:)

! Private variables:
INTEGER:: ERR,IO,I
LOGICAL:: CONNECTED,INTERNAL_CHANGE_MARKER
INTEGER,SAVE:: VECTOR_ID=0,INTERNAL_COLOR(3)

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)
INTERNAL_CHANGE_MARKER=.FALSE.
IF (PRESENT(CHANGE_MARKER)) THEN
IF (CHANGE_MARKER) INTERNAL_CHANGE_MARKER=.TRUE.
END IF
IF (VECTOR_ID==0) INTERNAL_CHANGE_MARKER=.TRUE.

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"WARNING: uninitialized SVG file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.svg') THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"ERROR: file must have the .svg extension."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/X,Y/)<0.0)) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"WARNING: negative coordinates."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(Y>VIEWBOX_HEIGHT)).OR.(ANY(X>VIEWBOX_WIDTH))) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"WARNING: coordinates beyond viewBox"
WRITE(*,*)"x: ",X
WRITE(*,*)"y: ",Y
WRITE(*,*)"viewBox_height: ",VIEWBOX_HEIGHT
WRITE(*,*)"viewBox_width: ",VIEWBOX_WIDTH
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((STROKE_WIDTH<0.0).OR.(HEAD_SIZE<0.0)) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"ERROR: invalid stroke parameters."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (HEAD_SIZE==0.0) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"WARNING: this head size will render the vector point invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (STROKE_WIDTH==0.0) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"WARNING: this stroke-width will render the object invisible."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ANY(STROKE_RGB<0)).OR.(ANY(STROKE_RGB>255))) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"ERROR: invalid RGB channel value."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE)) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

! ------------------------------------------------------------------------------

! Note:
! According to the SVG 1.1 specification the marker symbols do not inherit
! properties (like e.g. color) from the object implementing them. This means
! that the color of the marker should be set separatelly, something that causes
! problems in the case where the line of the vector has to vary in color (e.g.
! in the case of property collor mapping). Therefore, every vector line must
! refer to its own marker symbol that is printed in the SVG file using the
! vector line properties and a unique object id.

IF (INTERNAL_CHANGE_MARKER) THEN

! Increase the id_number
    VECTOR_ID=VECTOR_ID+1
    INTERNAL_COLOR=STROKE_RGB

! Define the marker symbol for the arrowhead:
    WRITE(IO,'(A6)')"<defs>"
    WRITE(IO,*)"<marker"
    WRITE(IO,*)" id='ArrowHead_"//TRIM(StringInteger(VECTOR_ID))//"'"
    WRITE(IO,*)" viewBox='0 0 10 10'"
    WRITE(IO,*)" refX='5'"
    WRITE(IO,*)" refY='5'"
    WRITE(IO,*)" markerUnits='strokeWidth'"
    WRITE(IO,*)" markerWidth='"//TRIM(StringReal(HEAD_SIZE))//"'"
    WRITE(IO,*)" markerHeight='"//TRIM(StringReal(HEAD_SIZE))//"'"
    WRITE(IO,*)" fill='rgb("// &
            & TRIM(StringInteger(INTERNAL_COLOR(1)))//","// &
            & TRIM(StringInteger(INTERNAL_COLOR(2)))//","// &
            & TRIM(StringInteger(INTERNAL_COLOR(3)))//")'"
    WRITE(IO,*)" orient='auto'>"
    WRITE(IO,*)" <path"
    WRITE(IO,*)"  d='M 0,10 Q 5,5 10,10 L 5,0 z'"
    WRITE(IO,*)"  transform='rotate(90,5,5)'/>"
    WRITE(IO,*)"</marker>"
    WRITE(IO,'(A7)')"</defs>"
    WRITE(IO,*)

END IF

! Draw the vector.
WRITE(IO,'(A5)',IOSTAT=ERR)"<line"

IF (ERR/=0) THEN
WRITE(*,*)"SVG_Vector"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename: ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"x1='"//TRIM(StringReal(X(1)))//"'"
WRITE(IO,*)"y1='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y(1)))//"'"
WRITE(IO,*)"x2='"//TRIM(StringReal(X(2)))//"'"
WRITE(IO,*)"y2='"//TRIM(StringReal(VIEWBOX_HEIGHT-Y(2)))//"'"
WRITE(IO,*)"stroke-width='"//TRIM(StringReal(STROKE_WIDTH))//"'"
WRITE(IO,*)"stroke='rgb("// &
        & TRIM(StringInteger(INTERNAL_COLOR(1)))//","// &
        & TRIM(StringInteger(INTERNAL_COLOR(2)))//","// &
        & TRIM(StringInteger(INTERNAL_COLOR(3)))//")'"

IF (PRESENT(ROTATE)) THEN
    WRITE(IO,*)"transform='rotate("// &
        & TRIM(StringReal(ROTATE(1)))//","// &
        & TRIM(StringReal(ROTATE(2)))//","// &
        & TRIM(StringReal(VIEWBOX_HEIGHT-ROTATE(3)))//")'"
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)TRIM(SCALAR_NAME(I))//"='"// &
            & TRIM(StringReal(SCALAR_VALUE(I)))//"'"
    END DO
END IF

! Insert the arrow point.
WRITE(IO,*)"marker-end='url(#ArrowHead_"//TRIM(StringInteger(VECTOR_ID))//")'"

IF (PRESENT(ID)) WRITE(IO,*)"id='"//TRIM(ID)//"'"

WRITE(IO,'(A2)')"/>"
WRITE(IO,*)

END SUBROUTINE SVG_Vector

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_VectorFieldPixel(OUTPUT_FILE,X,Y, &
    & RESOLUTION_X,RESOLUTION_Y,PIXEL_WIDTH,PIXEL_HEIGHT, &
    & ARRAY1,ARRAY2,DATA_TYPE,HEAD_SIZE,STROKE_WIDTH,STROKE_RGB, &
    & PROPERTY_NAME,PROPERTY_VALUE,COLOR_SCHEME,ID)

! SVG_VectorFieldPixel is called to draw a structured vector field in
! an SVG file. This subroutine can be called in combination with subroutine
! "SVG_GridPixel" to plot the grid's vectors.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, scalar. The x-axis coordinate of the field lower left end. (px).
! y: real, scalar. The y-axis coordinate of the field lower left end. (px).
! resolution_x: integer, scalar. The horizontal resolution of the vector field.
! resolution_y: integer, scalar. The vertical resolution of the vector field.
! pixel_width: real, scalar. The pixel width.
! pixel_height: real, scalar. The pixel height.
! array1: real, array (2D). The type1 data of vector definition. The elements
!    are in 1-1 correspondance with the elements of array "array2". See
!    definition of variable "data_type".  The size of the array must be set
!    to (resolution_x,resolution_y).
! array2: real, array (2D). The type2 data of vector definition. The elements
!    are in 1-1 correspondance with the elements of array "array1". See
!    definition of variable "data_type".  The size of the array must be set
!    to (resolution_x,resolution_y).
! data_type: character, scalar. Defines the kind of data defined in variables
!    "array1" and "array2": Possible options are:
!    "dx_dy": The vector is defined as a differential dx/dy, like e.g.
!        velocities. Variable "array1" holds the x-component of the
!        vector, and variable "array2" the y-component.
!    "size_angle": The vector is defined by its size and its angle respective
!        to the x-axis. Variable "array1" holds the vector size and
!        variable "array2" the angle in radians.
!    "end_point": The vector is defined by its end point. Variable "array1"
!        holds the x-coordinate of the vector end, and variable "array2"
!        the y-coordinate.
! head_size: real, scalar. The size of the arrow head, respective to the
!    specified stroke width (dimensionless).
! stroke_width: real, scalar. The thickness of the vector line (px).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255). When variable
!    'color_scheme' is present and different than "none", variable
!    "stroke_rgb" is overruled.

! INPUT (OPTIONAL):
! property_value:  real, array (2D). The values of the array that will be mapped
!    on the lattice vectors using the color scheme defined in variable
!    "color_scheme". The values of the array will also be stored in the
!    SVG node objects as XML variables. The size of the array must be equal
!    to the size of variables "array1" and "array2" and their elements are
!    1-1 corresponding. This variable may not be omitted when variables
!    "property_name" and "color_scheme" are included in the arguments and
!    vica versa.
! property_name: character, scalar. The name of the non-SVG variable that will
!    be stored in the SVG object as XML variable and will be mapped using the
!    color scheme defined in variable "color_scheme". This variable may not
!    be omitted when variables "color_scheme" and "property_value" are
!    included in the arguments and vica versa.
! color_scheme: character, scalar. Defines the color scheme used to map the
!    scalar variable on the lattice nodes. Supported values: "blue_red",
!    "grayscale", "none". This variable may not be omitted when variables
!    "property_value" and "property_name" are included in the arguments and
!    vica versa. When variable 'color_scheme' is present and different than
!    "none", variable "stroke_rgb" is overruled.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_VectorFieldPixel(output_file,x,y, &
!    & resolution_x,resolution_y,pixel_width,pixel_height, &
!    & array1,array2,data_type,head_size,stroke_width,stroke_rgb, &
!    & property_name,property_value,color_scheme,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),data_type*(*)
!real(srk),intent(in):: x,y,array1(:,:),array2(:,:),pixel_width,pixel_height
!real(srk),intent(in):: head_size,stroke_width
!integer,intent(in):: resolution_x,resolution_y,stroke_rgb(3)
!character,optional,intent(in):: property_name*(*),color_scheme*(*),id*(*)
!real(srk),optional,intent(in):: property_value(:,:)
!end subroutine SVG_VectorFieldPixel
!end interface

!integer,parameter:: res_x=3,res_y=4
!integer:: i,j
!real(srk):: cell_property(res_x,res_y),node(res_x+1,res_y+1),dx(4,5),dy(4,5)
!real(srk):: pixel_width=90.0_srk,pixel_height=120.0_srk
!node(1,1)=1.0 ; node(1,2)=2.0 ; node(1,3)=3.0 ; node(1,4)=4.0  ; node(1,5)=5.0
!node(2,1)=1.0 ; node(2,2)=2.0 ; node(2,3)=3.0 ; node(2,4)=4.0  ; node(2,5)=5.0
!node(3,1)=1.0 ; node(3,2)=2.0 ; node(3,3)=3.0 ; node(3,4)=4.0  ; node(3,5)=5.0
!node(4,1)=1.0 ; node(4,2)=2.0 ; node(4,3)=3.0 ; node(4,4)=4.0  ; node(4,5)=5.0
!dx(1,1)=1.0 ; dx(1,2)=2.0 ; dx(1,3)=3.0 ; dx(1,4)=0.0  ; dx(1,5)=-3.0
!dx(2,1)=1.0 ; dx(2,2)=2.0 ; dx(2,3)=3.0 ; dx(2,4)=0.0  ; dx(2,5)=-3.0
!dx(3,1)=1.0 ; dx(3,2)=2.0 ; dx(3,3)=3.0 ; dx(3,4)=0.0  ; dx(3,5)=-3.0
!dx(4,1)=1.0 ; dx(4,2)=2.0 ; dx(4,3)=3.0 ; dx(4,4)=0.0  ; dx(4,5)=-3.0
!dy(1,1)=-1.0 ; dy(1,2)=0.0 ; dy(1,3)=3.0 ; dy(1,4)=4.0  ; dy(1,5)=5.0
!dy(2,1)=-1.0 ; dy(2,2)=0.0 ; dy(2,3)=3.0 ; dy(2,4)=4.0  ; dy(2,5)=5.0
!dy(3,1)=-1.0 ; dy(3,2)=0.0 ; dy(3,3)=3.0 ; dy(3,4)=4.0  ; dy(3,5)=5.0
!dy(4,1)=-1.0 ; dy(4,2)=0.0 ; dy(4,3)=3.0 ; dy(4,4)=4.0  ; dy(4,5)=5.0
!node=sqrt(dx**2+dy**2)
!forall (i=1:res_x,j=1:res_y) cell_property(i,j)=sum(node(i:i+1,j:j+1))/4.0_srk
!call SVG_Initialize(output_file='plot.svg')
!! Print grid:
!call SVG_GridPixel(output_file='plot.svg',x=200.0_srk,y=200.0_srk, &
!    & resolution_x=res_x,resolution_y=res_y, &
!    & pixel_width=pixel_width,pixel_height=pixel_height, &
!    & line_width=3.0_srk,line_rgb=(/0,0,0/), &
!    & node_size=3.0_srk,node_rgb=(/255,0,255/), &
!    & cell_property=cell_property, &
!    & cell_property_name='cell_velocity_average', &
!    & cell_color_scheme='none', &
!    & node_property=node,node_property_name='velocity', &
!    & node_color_scheme='blue_red')
!! Print vectors:
!call SVG_VectorFieldPixel(output_file='plot.svg',x=200.0_srk,y=200.0_srk, &
!    & resolution_x=res_x+1,resolution_y=res_y+1, &
!    & pixel_width=pixel_width,pixel_height=pixel_height, &
!    & array1=dx*5.0_srk,array2=dy*5.0_srk,data_type='dx_dy', &
!    & head_size=7.0_srk,stroke_width=3.0_srk,stroke_rgb=(/0,0,0/), &
!    & property_name='velocity',property_value=node,color_scheme='blue_red')
!call SVG_ColorBar(output_file='plot.svg',title='velocity', &
!    & x=200.0_srk+res_x*pixel_width+150.0_srk,y=400.0_srk, &
!    & width=50.0_srk,height=300.0_srk, &
!    & minimum=minval(node),maximum=maxval(node), &
!    & decimal_places=2,scientific=.false., &
!    & font='verdana',font_size=25.0_srk,font_rgb=(/0,0,0/), &
!    & grayscale=.false.)
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),DATA_TYPE*(*)
REAL(SRK),INTENT(IN):: X,Y,ARRAY1(:,:),ARRAY2(:,:),PIXEL_WIDTH,PIXEL_HEIGHT
REAL(SRK),INTENT(IN):: HEAD_SIZE,STROKE_WIDTH
INTEGER,INTENT(IN):: RESOLUTION_X,RESOLUTION_Y,STROKE_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: PROPERTY_NAME*(*),COLOR_SCHEME*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: PROPERTY_VALUE(:,:)

! Private variables:
REAL(SRK):: X_ARRAY(RESOLUTION_X+1,RESOLUTION_Y+1)
REAL(SRK):: Y_ARRAY(RESOLUTION_X+1,RESOLUTION_Y+1)
REAL(SRK):: X2(RESOLUTION_X,RESOLUTION_Y),Y2(RESOLUTION_X,RESOLUTION_Y)
REAL(SRK):: MINIMUM,MAXIMUM
INTEGER:: I,J,COLOR(3)
LOGICAL:: GRAYSCALE

! Variable initialization:
IF (PRESENT(COLOR_SCHEME)) THEN
    MINIMUM=MINVAL(PROPERTY_VALUE)
    MAXIMUM=MAXVAL(PROPERTY_VALUE)
    IF (TRIM(COLOR_SCHEME)=='grayscale') THEN
        GRAYSCALE=.TRUE.
    ELSE
        GRAYSCALE=.FALSE.
    END IF
END IF

! Calculate the coordinates of the vector beginnings:
X_ARRAY(1,:)=X
DO I=2,RESOLUTION_X+1
    X_ARRAY(I,:)=X_ARRAY(I-1,:)+PIXEL_WIDTH
END DO
Y_ARRAY(:,1)=Y
DO I=2,RESOLUTION_Y+1
    Y_ARRAY(:,I)=Y_ARRAY(:,I-1)+PIXEL_HEIGHT
END DO

! ------------------------------------------------------------------------------

! Error control:

IF (ANY((/RESOLUTION_X,RESOLUTION_Y/)<=0)) THEN
WRITE(*,*)"SVG_VectorFieldPixel"
WRITE(*,*)"ERROR: invalid resolution."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (ANY((/PIXEL_WIDTH,PIXEL_HEIGHT/)<=0.0)) THEN
WRITE(*,*)"SVG_VectorFieldPixel"
WRITE(*,*)"ERROR: invalid pixel dimensions."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((RESOLUTION_X/=SIZE(ARRAY1,DIM=1)).OR. &
    & (RESOLUTION_Y/=SIZE(ARRAY1,DIM=2))) THEN
WRITE(*,*)"SVG_VectorFieldPixel"
WRITE(*,*)"ERROR: false matrix resolution."
WRITE(*,*)"resolution_x: ",RESOLUTION_X
WRITE(*,*)"resolution_y: ",RESOLUTION_Y
WRITE(*,*)"size(array1,dim=1): ",SIZE(ARRAY1,DIM=1)
WRITE(*,*)"size(array1,dim=2): ",SIZE(ARRAY1,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SIZE(ARRAY2,DIM=1)/=SIZE(ARRAY1,DIM=1)).OR. &
    & (SIZE(ARRAY2,DIM=2)/=SIZE(ARRAY1,DIM=2))) THEN
WRITE(*,*)"SVG_VectorFieldPixel"
WRITE(*,*)"ERROR: false matrix resolution."
WRITE(*,*)"size(array1,dim=1): ",SIZE(ARRAY1,DIM=1)
WRITE(*,*)"size(array1,dim=2): ",SIZE(ARRAY1,DIM=2)
WRITE(*,*)"size(array2,dim=1): ",SIZE(ARRAY2,DIM=1)
WRITE(*,*)"size(array2,dim=2): ",SIZE(ARRAY2,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(PROPERTY_VALUE)).AND.((.NOT.(PRESENT(PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(COLOR_SCHEME))))) THEN
WRITE(*,*)"SVG_VectorFieldPixel"
WRITE(*,*)"ERROR: variable 'property_value' may not appear in the arguments"
WRITE(*,*)"without variables 'property_name' and 'color_scheme'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(PROPERTY_NAME)).AND.((.NOT.(PRESENT(PROPERTY_VALUE))).OR. &
    (.NOT.(PRESENT(COLOR_SCHEME))))) THEN
WRITE(*,*)"SVG_VectorFieldPixel"
WRITE(*,*)"ERROR: variable 'property_name' may not appear in the arguments"
WRITE(*,*)"without variables 'property_value' and 'color_scheme'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(COLOR_SCHEME)).AND.((.NOT.(PRESENT(PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(PROPERTY_VALUE))))) THEN
WRITE(*,*)"SVG_VectorFieldPixel"
WRITE(*,*)"ERROR: variable 'color_scheme' may not appear in the arguments"
WRITE(*,*)"without variables 'property_name' and 'property_value'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT(COLOR_SCHEME)) THEN
!Edit this IF block in order to support more color schemes.
    IF (ALL(TRIM(COLOR_SCHEME)/= &
        & (/'blue_red ','grayscale','none     '/))) THEN
    WRITE(*,*)"SVG_VectorFieldPixel"
    WRITE(*,*)"ERROR: unsupported color scheme for grid nodes."
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF
END IF

!Edit this IF block in order to support more data types.
IF (ALL(TRIM(DATA_TYPE)/= &
    & (/'dx_dy     ','size_angle','end_point '/))) THEN
WRITE(*,*)"SVG_VectorFieldPixel"
WRITE(*,*)"ERROR: unsupported color scheme for grid nodes."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT(PROPERTY_VALUE)) THEN

    IF (LEN_TRIM(PROPERTY_NAME)==0) THEN
    WRITE(*,*)"SVG_VectorFieldPixel"
    WRITE(*,*)"WARNING: no vector property name provided."
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF (ALL(PROPERTY_VALUE==0)) THEN
    WRITE(*,*)"SVG_VectorFieldPixel"
    WRITE(*,*)"WARNING: all vector property values are equal to zero"
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF ((RESOLUTION_X/=SIZE(PROPERTY_VALUE,DIM=1)).OR. &
        & (RESOLUTION_Y/=SIZE(PROPERTY_VALUE,DIM=2))) THEN
    WRITE(*,*)"SVG_VectorFieldPixel"
    WRITE(*,*)"ERROR: false matrix resolution."
    WRITE(*,*)"resolution_x: ",RESOLUTION_X
    WRITE(*,*)"resolution_y: ",RESOLUTION_Y
    WRITE(*,*)"size(property_value,dim=1): ",SIZE(PROPERTY_VALUE,DIM=1)
    WRITE(*,*)"size(property_value,dim=2): ",SIZE(PROPERTY_VALUE,DIM=2)
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_VectorFieldPixel"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

! Transform the data according to their type:
SELECT CASE (TRIM(DATA_TYPE))
    CASE('dx_dy')
        X2=X_ARRAY+ARRAY1
        Y2=Y_ARRAY+ARRAY2
! This transformation could be implemented through the "rotate" input variable
! of subroutine "SVG_Vector", but it is more elegant to do all the
! transformations here.
    CASE('size_angle')
        X2=X_ARRAY+SIN(ARRAY1)*ARRAY2
        Y2=Y_ARRAY+COS(ARRAY1)*ARRAY2
    CASE('end_point')
        X2=ARRAY1
        Y2=ARRAY2
END SELECT

! Print the vectors.

IF (PRESENT(ID)) THEN
    CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=ID)
ELSE
    CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE)
END IF

COLOR=STROKE_RGB ! Variable initialization

DO I=1,RESOLUTION_X
DO J=1,RESOLUTION_Y
IF (PRESENT(PROPERTY_VALUE)) THEN

    IF (TRIM(COLOR_SCHEME)/='none') COLOR= &
        & SVG_ColorMap(PROPERTY_VALUE(I,J), &
        & MINIMUM,MAXIMUM,GRAYSCALE)

    CALL SVG_Vector(OUTPUT_FILE=OUTPUT_FILE, &
        & X=(/X_ARRAY(I,J),X2(I,J)/),Y=(/Y_ARRAY(I,J),Y2(I,J)/), &
        & STROKE_WIDTH=STROKE_WIDTH,STROKE_RGB=COLOR, &
        & HEAD_SIZE=HEAD_SIZE,CHANGE_MARKER=.TRUE., &
        & SCALAR_NAME=(/PROPERTY_NAME/), &
        & SCALAR_VALUE=(/PROPERTY_VALUE(I,J)/))

ELSE

    CALL SVG_Vector(OUTPUT_FILE=OUTPUT_FILE, &
        & X=(/X_ARRAY(I,J),X2(I,J)/),Y=(/Y_ARRAY(I,J),Y2(I,J)/), &
        & STROKE_WIDTH=STROKE_WIDTH,STROKE_RGB=STROKE_RGB, &
        & HEAD_SIZE=HEAD_SIZE,CHANGE_MARKER=.FALSE.)

END IF
END DO
END DO

CALL SVG_GroupEnd(OUTPUT_FILE)

END SUBROUTINE SVG_VectorFieldPixel

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE SVG_VectorFieldStructured(OUTPUT_FILE,X,Y, &
    & ARRAY1,ARRAY2,DATA_TYPE,HEAD_SIZE,STROKE_WIDTH,STROKE_RGB, &
    & PROPERTY_NAME,PROPERTY_VALUE,COLOR_SCHEME,ID)

! SVG_VectorFieldStructured is called to draw a structured vector field in
! an SVG file. This subroutine can be called in combination with subroutine
! "SVG_GridStructured" to plot the grid's vectors.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output SVG file. It must have
!    the ".svg" extension (otherwise an error is generated).
! x: real, array (2D). The x-axis coordinates of the beginning of the vectors
!    (px). The elements are in 1-1 correspondance with the elements of
!    arrays "y", "array1" and "array2".
! y: real, array (2D). The y-axis coordinates of the beginning of the vectors
!    (px). The elements are in 1-1 correspondance with the elements of
!    arrays "x", "array1" and "array2".
! array1: real, array (2D). The type1 data of vector definition. The elements
!    are in 1-1 correspondance with the elements of arrays "x", "y" and
!    "array2". See definition of variable "data_type".
! array2: real, array (2D). The type2 data of vector definition. The elements
!    are in 1-1 correspondance with the elements of arrays "x", "y" and
!    "array1". See definition of variable "data_type".
! data_type: character, scalar. Defines the kind of data defined in variables
!    "array1" and "array2": Possible options are:
!    "dx_dy": The vector is defined as a differential dx/dy, like e.g.
!        velocities. Variable "array1" holds the x-component of the
!        vector, and variable "array2" the y-component.
!    "size_angle": The vector is defined by its size and its angle respective
!        to the x-axis. Variable "array1" holds the vector size and
!        variable "array2" the angle.
!    "end_point": The vector is defined by its end point. Variable "array1"
!        holds the x-coordinate of the vector end, and variable "array2"
!        the y-coordinate.
! head_size: real, scalar. The size of the arrow head, respective to the
!    specified stroke width (dimensionless).
! stroke_width: real, scalar. The thickness of the vector line (px).
! stroke_rgb: integer, array (1D) with 3 elements. Contains the stroke RGB
!    channel values (dimensionless, 0<=stroke_rgb(:)<=255). When variable
!    'color_scheme' is present and different than "none", variable
!    "stroke_rgb" is overruled.

! INPUT (OPTIONAL):
! property_value:  real, array (2D). The values of the array that will be mapped
!    on the lattice vectors using the color scheme defined in variable
!    "color_scheme". The values of the array will also be stored in the
!    SVG node objects as XML variables. The size of the array must be equal
!    to the size of variables "x" and "y" and their elements are 1-1
!    corresponding. This variable may not be omitted when variables
!    "property_name" and "color_scheme" are included in the arguments and
!    vica versa.
! property_name: character, scalar. The name of the non-SVG variable that will
!    be stored in the SVG object as XML variable and will be mapped using the
!    color scheme defined in variable "color_scheme". This variable may not
!    be omitted when variables "color_scheme" and "property_value" are
!    included in the arguments and vica versa.
! color_scheme: character, scalar. Defines the color scheme used to map the
!    scalar variable on the lattice nodes. Supported values: "blue_red",
!    "grayscale", "none". This variable may not be omitted when variables
!    "property_value" and "property_name" are included in the arguments and
!    vica versa. When variable 'color_scheme' is present and different than
!    "none", variable "stroke_rgb" is overruled.
! id: character, scalar. The id of the SVG object. The user is responsible for
!    the consistency of the ids within an SVG document.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine SVG_VectorFieldStructured(output_file,x,y, &
!    & array1,array2,data_type,head_size,stroke_width,stroke_rgb, &
!    & property_name,property_value,color_scheme,id)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*),data_type*(*)
!real(srk),intent(in):: x(:,:),y(:,:),array1(:,:),array2(:,:)
!real(srk),intent(in):: head_size,stroke_width
!integer,intent(in):: stroke_rgb(3)
!real(srk),optional,intent(in):: property_value(:,:),id*(*)
!character,optional,intent(in):: property_name*(*),color_scheme*(*)
!end subroutine SVG_VectorFieldStructured
!end interface

!real(srk):: x(4,5),y(4,5),node(4,5),dx(4,5),dy(4,5),cell_average(3,4)
!integer:: i,j
!x(1,1)=60.44  ; y(1,1)=328.63 ; x(1,2)=151.09 ; y (1,2)=351.29 ; x(1,3)=308.80
!y(1,3)=347.51 ; x(1,4)=384.34 ; y(1,4)=357.90 ; x (1,5)=466.50 ; y(1,5)=355.07
!x(2,1)=129.37 ; y(2,1)=247.41 ; x(2,2)=239.86 ; y (2,2)=275.74 ; x(2,3)=329.57
!y(2,3)=259.69 ; x(2,4)=392.84 ; y(2,4)=297.46 ; x (2,5)=488.22 ; y(2,5)=247.41
!x(3,1)=128.43 ; y(3,1)=141.65 ; x(3,2)=220.03 ; y (3,2)=173.76 ; x(3,3)=297.46
!y(3,3)=196.42 ; x(3,4)=400.40 ; y(3,4)=176.59 ; x (3,5)=482.55 ; y(3,5)=169.98
!x(4,1)=118.99 ; y(4,1)=68.94  ; x(4,2)=190.75 ; y (4,2)=80.27  ; x(4,3)=315.41
!y(4,3)=91.60  ; x(4,4)=401.34 ; y(4,4)=87.82  ; x (4,5)=475.94 ; y(4,5)=84.05
!dx(1,1)=1.0 ; dx(1,2)=2.0 ; dx(1,3)=3.0 ; dx(1,4)=0.0  ; dx(1,5)=-3.0
!dx(2,1)=1.0 ; dx(2,2)=2.0 ; dx(2,3)=3.0 ; dx(2,4)=0.0  ; dx(2,5)=-3.0
!dx(3,1)=1.0 ; dx(3,2)=2.0 ; dx(3,3)=3.0 ; dx(3,4)=0.0  ; dx(3,5)=-3.0
!dx(4,1)=1.0 ; dx(4,2)=2.0 ; dx(4,3)=3.0 ; dx(4,4)=0.0  ; dx(4,5)=-3.0
!dy(1,1)=-1.0 ; dy(1,2)=0.0 ; dy(1,3)=3.0 ; dy(1,4)=4.0  ; dy(1,5)=5.0
!dy(2,1)=-1.0 ; dy(2,2)=0.0 ; dy(2,3)=3.0 ; dy(2,4)=4.0  ; dy(2,5)=5.0
!dy(3,1)=-1.0 ; dy(3,2)=0.0 ; dy(3,3)=3.0 ; dy(3,4)=4.0  ; dy(3,5)=5.0
!dy(4,1)=-1.0 ; dy(4,2)=0.0 ; dy(4,3)=3.0 ; dy(4,4)=4.0  ; dy(4,5)=5.0
!node=sqrt(dx**2+dy**2)
!forall (i=1:size(x,dim=1)-1,j=1:size(x,dim=2)-1) &
!    & cell_average(i,j)=sum(node(i:i+1,j:j+1))/4.0_srk
!call SVG_Initialize(output_file='plot.svg')
!call SVG_GridStructured(output_file='plot.svg',x=x,y=y+200.0_srk, &
!    & line_width=2.0_srk,line_rgb=(/0,0,0/), &
!    & node_size=6.0_srk,node_rgb=(/0,0,0/), &
!    & cell_property=cell_average, &
!    & cell_property_name='cell_velocity_average', &
!    & cell_color_scheme='none', &
!    & node_property=node,node_property_name='velocity', &
!    & node_color_scheme='none')
!call SVG_VectorFieldStructured(output_file='plot.svg',x=x,y=y+200.0_srk, &
!    & array1=dx*5.0_srk,array2=dy*5.0_srk,data_type='dx_dy', &
!    & head_size=7.0_srk,stroke_width=3.0_srk,stroke_rgb=(/0,0,0/), &
!    & property_name='velocity',property_value=node,color_scheme='blue_red')
!call SVG_ColorBar(output_file='plot.svg',title='velocity', &
!    & x=maxval(x)+150.0_srk,y=400.0_srk, &
!    & width=50.0_srk,height=300.0_srk, &
!    & minimum=minval(node),maximum=maxval(node), &
!    & decimal_places=2,scientific=.false., &
!    & font='verdana',font_size=25.0_srk,font_rgb=(/0,0,0/), &
!    & grayscale=.false.)
!call SVG_Finalize(output_file='plot.svg')

! In contrast to the SVG 1.1 specification, the zero point of the y-axis in the
! present library is at the lower left point of the SVG viewBox. The
! y-coordinates are calculated with the help of variable "viewBox_height" from
! module "Config".

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*),DATA_TYPE*(*)
REAL(SRK),INTENT(IN):: X(:,:),Y(:,:),ARRAY1(:,:),ARRAY2(:,:)
REAL(SRK),INTENT(IN):: HEAD_SIZE,STROKE_WIDTH
INTEGER,INTENT(IN):: STROKE_RGB(3)
CHARACTER,OPTIONAL,INTENT(IN):: PROPERTY_NAME*(*),COLOR_SCHEME*(*),ID*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: PROPERTY_VALUE(:,:)

! Private variables:
REAL(SRK):: X2(SIZE(X,DIM=1),SIZE(X,DIM=2)),Y2(SIZE(X,DIM=1),SIZE(X,DIM=2))
REAL(SRK):: MINIMUM,MAXIMUM
INTEGER:: I,J,COLOR(3)
LOGICAL:: GRAYSCALE

! Variable initialization:
IF (PRESENT(COLOR_SCHEME)) THEN
    MINIMUM=MINVAL(PROPERTY_VALUE)
    MAXIMUM=MAXVAL(PROPERTY_VALUE)
    IF (TRIM(COLOR_SCHEME)=='grayscale') THEN
        GRAYSCALE=.TRUE.
    ELSE
        GRAYSCALE=.FALSE.
    END IF
END IF

! ------------------------------------------------------------------------------

! Error control:

IF ((SIZE(X,DIM=1)/=SIZE(Y,DIM=1)).OR.(SIZE(X,DIM=2)/=SIZE(Y,DIM=2))) THEN
WRITE(*,*)"SVG_VectorFieldStructured"
WRITE(*,*)"ERROR: false matrix resolution."
WRITE(*,*)"size(x,dim=1): ",SIZE(X,DIM=1)
WRITE(*,*)"size(x,dim=2): ",SIZE(X,DIM=2)
WRITE(*,*)"size(y,dim=1): ",SIZE(Y,DIM=1)
WRITE(*,*)"size(y,dim=2): ",SIZE(Y,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SIZE(X,DIM=1)/=SIZE(ARRAY1,DIM=1)).OR. &
    & (SIZE(X,DIM=2)/=SIZE(ARRAY1,DIM=2))) THEN
WRITE(*,*)"SVG_VectorFieldStructured"
WRITE(*,*)"ERROR: false matrix resolution."
WRITE(*,*)"size(x,dim=1): ",SIZE(X,DIM=1)
WRITE(*,*)"size(x,dim=2): ",SIZE(X,DIM=2)
WRITE(*,*)"size(array1,dim=1): ",SIZE(ARRAY1,DIM=1)
WRITE(*,*)"size(array1,dim=2): ",SIZE(ARRAY1,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SIZE(ARRAY2,DIM=1)/=SIZE(ARRAY1,DIM=1)).OR. &
    & (SIZE(ARRAY2,DIM=2)/=SIZE(ARRAY1,DIM=2))) THEN
WRITE(*,*)"SVG_VectorFieldStructured"
WRITE(*,*)"ERROR: false matrix resolution."
WRITE(*,*)"size(array1,dim=1): ",SIZE(ARRAY1,DIM=1)
WRITE(*,*)"size(array1,dim=2): ",SIZE(ARRAY1,DIM=2)
WRITE(*,*)"size(array2,dim=1): ",SIZE(ARRAY2,DIM=1)
WRITE(*,*)"size(array2,dim=2): ",SIZE(ARRAY2,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(PROPERTY_VALUE)).AND.((.NOT.(PRESENT(PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(COLOR_SCHEME))))) THEN
WRITE(*,*)"SVG_VectorFieldStructured"
WRITE(*,*)"ERROR: variable 'property_value' may not appear in the arguments"
WRITE(*,*)"without variables 'property_name' and 'color_scheme'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(PROPERTY_NAME)).AND.((.NOT.(PRESENT(PROPERTY_VALUE))).OR. &
    (.NOT.(PRESENT(COLOR_SCHEME))))) THEN
WRITE(*,*)"SVG_VectorFieldStructured"
WRITE(*,*)"ERROR: variable 'property_name' may not appear in the arguments"
WRITE(*,*)"without variables 'property_value' and 'color_scheme'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(COLOR_SCHEME)).AND.((.NOT.(PRESENT(PROPERTY_NAME))).OR. &
    (.NOT.(PRESENT(PROPERTY_VALUE))))) THEN
WRITE(*,*)"SVG_VectorFieldStructured"
WRITE(*,*)"ERROR: variable 'color_scheme' may not appear in the arguments"
WRITE(*,*)"without variables 'property_name' and 'property_value'"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT(COLOR_SCHEME)) THEN
!Edit this IF block in order to support more color schemes.
    IF (ALL(TRIM(COLOR_SCHEME)/= &
        & (/'blue_red ','grayscale','none     '/))) THEN
    WRITE(*,*)"SVG_VectorFieldStructured"
    WRITE(*,*)"ERROR: unsupported color scheme for grid nodes."
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF
END IF

!Edit this IF block in order to support more data types.
IF (ALL(TRIM(DATA_TYPE)/= &
    & (/'dx_dy     ','size_angle','end_point '/))) THEN
WRITE(*,*)"SVG_VectorFieldStructured"
WRITE(*,*)"ERROR: unsupported color scheme for grid nodes."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT(PROPERTY_VALUE)) THEN

    IF (LEN_TRIM(PROPERTY_NAME)==0) THEN
    WRITE(*,*)"SVG_VectorFieldStructured"
    WRITE(*,*)"WARNING: no vector property name provided."
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF (ALL(PROPERTY_VALUE==0)) THEN
    WRITE(*,*)"SVG_VectorFieldStructured"
    WRITE(*,*)"WARNING: all vector property values are equal to zero"
    WRITE(*,*)"Continue..."
    IF (WARNINGS_PAUSE) READ(*,*)
    END IF

    IF ((SIZE(X,DIM=1)/=SIZE(PROPERTY_VALUE,DIM=1)).OR. &
        & (SIZE(X,DIM=2)/=SIZE(PROPERTY_VALUE,DIM=2))) THEN
    WRITE(*,*)"SVG_VectorFieldStructured"
    WRITE(*,*)"ERROR: false matrix resolution."
    WRITE(*,*)"size(x,dim=1): ",SIZE(X,DIM=1)
    WRITE(*,*)"size(x,dim=2): ",SIZE(X,DIM=2)
    WRITE(*,*)"size(property_value,dim=1): ",SIZE(PROPERTY_VALUE,DIM=1)
    WRITE(*,*)"size(property_value,dim=2): ",SIZE(PROPERTY_VALUE,DIM=2)
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

END IF

IF ((ALL(X==0)).OR.(ALL(Y==0))) THEN
WRITE(*,*)"SVG_VectorFieldStructured"
WRITE(*,*)"WARNING: all coordinate points are equal to zero"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (PRESENT(ID)) THEN
IF (LEN_TRIM(ID)==0) THEN
WRITE(*,*)"SVG_VectorFieldStructured"
WRITE(*,*)"WARNING: invalid id."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF
END IF

! ------------------------------------------------------------------------------

! Transform the data according to their type:
SELECT CASE (TRIM(DATA_TYPE))
    CASE('dx_dy')
        X2=X+ARRAY1
        Y2=Y+ARRAY2
! This transformation could be implemented through the "rotate" input variable
! of subroutine "SVG_Vector", but it is more elegant to do all the
! transformations here.
    CASE('size_angle')
        X2=X+SIN(ARRAY1)*ARRAY2
        Y2=Y+COS(ARRAY1)*ARRAY2
    CASE('end_point')
        X2=ARRAY1
        Y2=ARRAY2
END SELECT

! Print the vectors.

IF (PRESENT(ID)) THEN
    CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE,ID=ID)
ELSE
    CALL SVG_GroupStart(OUTPUT_FILE=OUTPUT_FILE)
END IF

COLOR=STROKE_RGB ! Variable initialization

DO I=1,SIZE(X,DIM=1)
DO J=1,SIZE(X,DIM=2)
IF (PRESENT(PROPERTY_VALUE)) THEN

    IF (TRIM(COLOR_SCHEME)/='none') COLOR= &
        & SVG_ColorMap(PROPERTY_VALUE(I,J), &
        & MINIMUM,MAXIMUM,GRAYSCALE)

    CALL SVG_Vector(OUTPUT_FILE=OUTPUT_FILE, &
        & X=(/X(I,J),X2(I,J)/),Y=(/Y(I,J),Y2(I,J)/), &
        & STROKE_WIDTH=STROKE_WIDTH,STROKE_RGB=COLOR, &
        & HEAD_SIZE=HEAD_SIZE,CHANGE_MARKER=.TRUE., &
        & SCALAR_NAME=(/PROPERTY_NAME/), &
        & SCALAR_VALUE=(/PROPERTY_VALUE(I,J)/))

ELSE

    CALL SVG_Vector(OUTPUT_FILE=OUTPUT_FILE, &
        & X=(/X(I,J),X2(I,J)/),Y=(/Y(I,J),Y2(I,J)/), &
        & STROKE_WIDTH=STROKE_WIDTH,STROKE_RGB=STROKE_RGB, &
        & HEAD_SIZE=HEAD_SIZE,CHANGE_MARKER=.FALSE.)

END IF
END DO
END DO

CALL SVG_GroupEnd(OUTPUT_FILE)

END SUBROUTINE SVG_VectorFieldStructured

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


END SUBMODULE SubLib_ScalableVectorGraphics
