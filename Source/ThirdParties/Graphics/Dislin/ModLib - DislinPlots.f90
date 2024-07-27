!****************************************************************************************
!*   :: Purpose ::                                                                      *
!*   Module offering a simplified procedure for Dislin-based x--y scatter (curve) plots *
!*   using a few preset common plot settings. Several curves/data sets can be plotted   *
!*   on the same axis system with curve-specific properties provided.                   *
!*                                                                                      *
!*  This module requires an installed and linked DISLIN graphics library                *
!*  (https://www.dislin.de/index.html). See website for details about Dislin and        *
!*  installation instructions on different platforms.                                   *
!*                                                                                      *
!*   :: Authors ::                                                                      *
!*   Andi Zuend (andreas.zuend@mcgill.ca)                                               *
!*   Dept. Atmospheric and Oceanic Sciences, McGill University                          *
!*                                                                                      *
!*   :: License ::                                                                      *
!*   This program is free software: you can redistribute it and/or modify it under the  *
!*   terms of the GNU General Public License as published by the Free Software          *
!*   Foundation, either version 3 of the License, or (at your option) any later         *
!*   version.                                                                           *
!*   The module code is distributed in the hope that it will be useful, but             *
!*   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or      *
!*   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more      *
!*   details.                                                                           *
!*   You should have received a copy of the GNU General Public License along with this  *
!*   program. If not, see <http://www.gnu.org/licenses/>.                               *
!*                                                                                      *
!*   :: List of subroutines and functions contained in this module:                     *
!*   --------------------------------------------------------------                     *
!*   -  subroutine  add_plot_xydata                                                     *
!*   -  subroutine  dislin_plot                                                         *
!*                                                                                      *
!****************************************************************************************
MODULE ModLib_DislinPlots

!use Mod_NumPrec, only : wp

IMPLICIT NONE

!define a working precision (wp) level to be used with floating point (real) variables, e.g. 1.0 should be stated as 1.0_wp.
!number_of_digits = desired minimum level of precision in terms of number of floating point decimal digits.
INTEGER,PARAMETER,PRIVATE :: NUMBER_OF_DIGITS = 12
INTEGER,PARAMETER,PUBLIC  :: WP = SELECTED_REAL_KIND(NUMBER_OF_DIGITS)

!public module variables and types:
TYPE, PRIVATE :: PLOT_XYDATA                                    !the (second) array dimension denotes the data SET (CURVE) NO.
    INTEGER,DIMENSION(:),ALLOCATABLE        :: NPOINTS          !stores number of data points of a data set to BE PLOTTED
    REAL(WP),DIMENSION(:,:),ALLOCATABLE     :: XVAL             !x coordinates of x--y data
    REAL(WP),DIMENSION(:,:),ALLOCATABLE     :: YVAL             !y coordinates of x--y data
    CHARACTER(LEN=75),DIMENSION(:),ALLOCATABLE :: LEGTEXT       !legend entry text for a curve
    REAL(WP),DIMENSION(:),ALLOCATABLE :: PEN_WIDTH              !pen width; typical values are between 1.0 and 9.0
    INTEGER,DIMENSION(:,:),ALLOCATABLE :: RGB_COLOR             !first dimension is red, green, blue values
                                                                !(array of 3 values each in range [0, 255])
    CHARACTER(LEN=13),DIMENSION(:),ALLOCATABLE :: LINE_STYLE    !options: 'solid', 'dotted', 'dashed', 'dashed_MEDIUM'
    CHARACTER(LEN=7),DIMENSION(:),ALLOCATABLE :: PLOT_SYMB      !plot curves w/ or w/o symbols or symbols only
                                                                !(options: 'symbols', 'curve', 'both')
    INTEGER,DIMENSION(:),ALLOCATABLE :: SYMBOL_ID               !ID (number) of symbol type to be plotted (see DISLIN MANUAL)
END TYPE PLOT_XYDATA

TYPE(PLOT_XYDATA),ALLOCATABLE,PRIVATE :: XY_DATA

!public procedures:
PUBLIC :: ADD_PLOT_XYDATA, DISLIN_PLOT
!-----------------------------------------------------------

    CONTAINS

    !****************************************************************************************
    !*   :: Purpose ::                                                                      *
    !*   Public subroutine to define the x--y data arrays and a selection of curve/symbol   *
    !*   properties for a specific data set (curve) to be added to a Dislin plot page.      *
    !*   The data will be stored in xy_data and later used within 'dislin_plot' to generate *
    !*   the plot. xy_data will be increased in the number of curves dimension with each    *
    !*   call of this subroutine (until a reset at the end of the subroutine dislin_plot)   *
    !*                                                                                      *
    !*   Note: Dislin offers many additional plot types and controls of plot properties.    *
    !*         Here we use a typically useful set of options for x--y scatter/curve plots.  *
    !*                                                                                      *
    !*   :: Authors ::                                                                      *
    !*   Andi Zuend (andreas.zuend@mcgill.ca)                                               *
    !*   Dept. Atmospheric and Oceanic Sciences, McGill University                          *
    !*                                                                                      *
    !*   -> created:        2021-06-29                                                      *
    !*   -> latest changes: 2022-03-23                                                      *
    !*                                                                                      *
    !****************************************************************************************
    SUBROUTINE ADD_PLOT_XYDATA(XV, YV, LTEXT, PEN_WID, RGB_COL, LSTYLE, PLOT_SYMB, SYMB_ID)

    IMPLICIT NONE
    !interface arguments:
    REAL(WP),DIMENSION(:),INTENT(IN) :: XV, YV                  !arrays of x and y values of data set to be adDED
    CHARACTER(LEN=*),INTENT(IN) :: LTEXT                        !legend entry text for data set
    REAL(WP),OPTIONAL,INTENT(IN) :: PEN_WID                     !pen width; typical values are between 1.0 and 9.0
    INTEGER,DIMENSION(3),OPTIONAL,INTENT(IN) :: RGB_COL         !color array of the 3 r,g,b values, each in raNGE [0, 255])
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: LSTYLE              !line style; options: 'solid', 'dotted', 'dashED', 'DASHED_MEDIUM'
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: PLOT_SYMB           !plot curves w/ or w/o symbols or symbols only
                                                                !(options: 'symbols', 'curve', 'both')
    INTEGER,OPTIONAL,INTENT(IN) :: SYMB_ID                      !the symbol ID for dislin (15 = open circle, 5 = OPEN DIAMOND,
                                                                !3 = +, 4 = X, 16 = filled square, 21 = filled CIRCLE, ETC.)

    !local variables:
    INTEGER :: K, NDSET, NP, NP_OLD, ISTAT
    LOGICAL :: IS_ALLOCATED
    TYPE(PLOT_XYDATA),ALLOCATABLE :: TEMP_DATA
    !...................................

    K = SIZE(XV)

    !check whether a xy_data variable has been allocated already:
    IF ( ALLOCATED(XY_DATA) ) THEN
        IS_ALLOCATED = .TRUE.
        NDSET = SIZE(XY_DATA%XVAL, DIM=2)
        NP_OLD = SIZE(XY_DATA%XVAL, DIM=1)
        !update ndset and np (with room for data set to be added to xy_data):
        NDSET = NDSET +1
        NP = MAX(K, NP_OLD)
    ELSE
        IS_ALLOCATED = .FALSE.
        NDSET = 1
        NP = K
    ENDIF

    ALLOCATE( TEMP_DATA, STAT=ISTAT)
    ALLOCATE( TEMP_DATA%NPOINTS(NDSET), TEMP_DATA%XVAL(NP,NDSET), TEMP_DATA%YVAL(NP,NDSET),  &
        & TEMP_DATA%LEGTEXT(NDSET), TEMP_DATA%PEN_WIDTH(NDSET), TEMP_DATA%RGB_COLOR(3,NDSET), &
        & TEMP_DATA%LINE_STYLE(NDSET), TEMP_DATA%PLOT_SYMB(NDSET), TEMP_DATA%SYMBOL_ID(NDSET), STAT=ISTAT )

    IF (IS_ALLOCATED) THEN
        !copy the data for the first ndset-1 entries to the temporary array
        TEMP_DATA%NPOINTS(1:NDSET-1)       = XY_DATA%NPOINTS(1:NDSET-1)
        TEMP_DATA%XVAL(1:NP_OLD,1:NDSET-1) = XY_DATA%XVAL(1:NP_OLD,1:NDSET-1)
        TEMP_DATA%YVAL(1:NP_OLD,1:NDSET-1) = XY_DATA%YVAL(1:NP_OLD,1:NDSET-1)
        TEMP_DATA%LEGTEXT(1:NDSET-1)       = XY_DATA%LEGTEXT(1:NDSET-1)
        TEMP_DATA%PEN_WIDTH(1:NDSET-1)     = XY_DATA%PEN_WIDTH(1:NDSET-1)
        TEMP_DATA%RGB_COLOR(:,1:NDSET-1)   = XY_DATA%RGB_COLOR(:,1:NDSET-1)
        TEMP_DATA%LINE_STYLE(1:NDSET-1)    = XY_DATA%LINE_STYLE(1:NDSET-1)
        TEMP_DATA%PLOT_SYMB(1:NDSET-1)     = XY_DATA%PLOT_SYMB(1:NDSET-1)
        TEMP_DATA%SYMBOL_ID(1:NDSET-1)     = XY_DATA%SYMBOL_ID(1:NDSET-1)
    ENDIF
    !move the data into enlarged xy_data using memory-friendly move_alloc:
    CALL MOVE_ALLOC(TEMP_DATA, XY_DATA)     !temp_data will be deallocated

    !finally add the new data set to xy_data, including accounting for optional arguments:
    XY_DATA%NPOINTS(NDSET) = K
    XY_DATA%XVAL(1:K,NDSET) = XV
    XY_DATA%YVAL(1:K,NDSET) = YV
    XY_DATA%LEGTEXT(NDSET) = ''                                 !initialize whole string
    XY_DATA%LEGTEXT(NDSET)(1:MIN(75, LEN_TRIM(LTEXT))) = LTEXT(1:MIN(75, LEN_TRIM(LTEXT)))
    IF (PRESENT(PEN_WID)) THEN
        XY_DATA%PEN_WIDTH(NDSET) = PEN_WID
    ELSE
        XY_DATA%PEN_WIDTH(NDSET) = 1.0_WP                       !set a default value
    ENDIF
    IF (PRESENT(RGB_COL)) THEN
        XY_DATA%RGB_COLOR(1:3,NDSET) = RGB_COL(1:3)
    ELSE
        XY_DATA%RGB_COLOR(1:3,NDSET) = [0, 0, 0]                !default (black)
    ENDIF
    IF (PRESENT(LSTYLE)) THEN
        XY_DATA%LINE_STYLE(NDSET) = ''
        XY_DATA%LINE_STYLE(NDSET)(1:MIN(75, LEN_TRIM(LSTYLE))) = LSTYLE(1:MIN(75, LEN_TRIM(LSTYLE)))
    ELSE
        XY_DATA%LINE_STYLE(NDSET) = 'solid'
    ENDIF
    IF (PRESENT(PLOT_SYMB)) THEN
        XY_DATA%PLOT_SYMB(NDSET) = ''
        XY_DATA%PLOT_SYMB(NDSET)(1:MIN(75, LEN_TRIM(PLOT_SYMB))) = PLOT_SYMB(1:MIN(75, LEN_TRIM(PLOT_SYMB)))
    ELSE
        XY_DATA%PLOT_SYMB(NDSET) = 'curve'
    ENDIF
    IF (PRESENT(SYMB_ID)) THEN
        XY_DATA%SYMBOL_ID(NDSET) = SYMB_ID
    ELSE
        XY_DATA%SYMBOL_ID(NDSET) = 15
    ENDIF

    END SUBROUTINE ADD_PLOT_XYDATA
    !-----------------------------------------------------------


    !****************************************************************************************
    !*   :: Purpose ::                                                                      *
    !*   Public subroutine to generate a x--y scatter plot with the previously loaded data  *
    !*   (via calls to subroutine 'add_plot_xydata'                                         *
    !*                                                                                      *
    !*   Note: Dislin offers many additional plot types and controls of plot properties.    *
    !*         Here we use a typically useful set of options for x--y scatter plots.        *
    !*                                                                                      *
    !*   :: Authors ::                                                                      *
    !*   Andi Zuend (andreas.zuend@mcgill.ca)                                               *
    !*   Dept. Atmospheric and Oceanic Sciences, McGill University                          *
    !*                                                                                      *
    !*   -> created:        2021-06-29                                                      *
    !*   -> latest changes: 2023-03-15                                                      *
    !*                                                                                      *
    !****************************************************************************************
    SUBROUTINE DISLIN_PLOT(XLABEL, YLABEL, YAXIS_MOD, XAXIS_LIMITS, YAXIS_LIMITS, &
        & LEGEND_POSITION, METAFILE, OUT_FILE_NAME)

    IMPLICIT NONE
    !interface arguments:
    CHARACTER(LEN=*),INTENT(IN) :: XLABEL, YLABEL       !text for axis labels;
    REAL(WP),INTENT(IN) :: YAXIS_MOD                    !set scaling of default length of y-axis on graph
                                                        !(e.g. 0.5 indicates a plot height of half the x-axis LENGTH);
    REAL(WP),DIMENSION(2),OPTIONAL,INTENT(IN) :: &
        & XAXIS_LIMITS, YAXIS_LIMITS                    !(optional) targeted x-axis and y-axis limits for plot;
    INTEGER,INTENT(IN) :: LEGEND_POSITION               !legend position value options:
                                                        != 0 for no legend
                                                        != 1 is the lower left corner of the page.
                                                        != 2 is the lower right corner of the page.
                                                        != 3 is the upper right corner of the page.
                                                        != 4 is the upper left corner of the page.
                                                        != 5 is the lower left corner of the axis system.
                                                        != 6 is the lower right corner of the axis system.
                                                        != 7 is the upper right corner of the axis system.
                                                        != 8 is the upper left corner of the axis system.
    CHARACTER(LEN=*),INTENT(IN) :: METAFILE             !typically set as 'xwin', 'cons', 'pdf' or two plots vIA
                                                        !'cons, pdf', 'xwin, pdf' (one to screen, one to file)
    CHARACTER(LEN=*),INTENT(IN) :: OUT_FILE_NAME        !relative path/file name of output file (e.g. for a .pDF FILE,
                                                        !without stating file extension);

    !local variables:
    CHARACTER(LEN=:),ALLOCATABLE :: CBUFF
    CHARACTER(LEN=:),ALLOCATABLE :: META_FILE_KIND
    INTEGER,PARAMETER :: DP = KIND(1.0D0)               !use this real kind with the double precision version OF DISLIN.
    INTEGER :: I, ISTAT, IPLOT, NDIG, NDSETS, NPTS, NXL, NYL, NZL, NPLOTS
    REAL(DP) :: XAXIS_MIN, XAXIS_MAX, YAXIS_MIN, YAXIS_MAX, AXIS_RANGE, TEMP, &
        & XA, XE, XOR, XSTEP, YA, YE, YOR, YSTEP
    LOGICAL :: INVERT_XAXIS, INVERT_YAXIS
    ! the external Dislin procedures (available via linked static library):
    EXTERNAL :: METAFL, SCRMOD, DISINI, PAGFLL, SETCLR, PSFONT, CHASPC, HEIGHT, HWFONT, &
        & TEXMOD, NAME, SETSCL, GRAF, INCMRK, LABDIG, LEGINI, LEGLIN, LEGEND, LNCAP, DOT, &
        & DASH, DASHM, SOLID, PENWID, COLOR, CURVE, SETRGB, ENDGRF, DISFIN, SETFIL, FILMOD, &
        & LEGTIT, FRAME, LINESP, GETLEN, AXSLEN, PSMODE, MARKER, NOCHEK, GAXPAR
    !...............................................

    !The object xy_data contains stored data for a curve or for several curves,
    !including curve color, thickness, legend text entry, etc.

    NDSETS = SIZE(XY_DATA%PEN_WIDTH)            !number of data sets
    ALLOCATE(CHARACTER(LEN=75*MIN(NDSETS, 30)) :: CBUFF)

    SELECT CASE(METAFILE)
    CASE('cons, pdf', 'xwin, pdf')
        NPLOTS = 2
    CASE DEFAULT
        NPLOTS = 1
    END SELECT

    DO IPLOT = 1, NPLOTS
        SELECT CASE(IPLOT)
        CASE(1)
            META_FILE_KIND = METAFILE( 1:MIN(4, LEN_TRIM(METAFILE)) )
        CASE(2)
            META_FILE_KIND = 'pdf'
        END SELECT
        CALL METAFL(META_FILE_KIND)
        CALL SETFIL(TRIM(OUT_FILE_NAME)//'.'//TRIM(META_FILE_KIND))
        CALL FILMOD('delete')                       !overwrite if file already exists
        CALL SCRMOD('norev')
        CALL DISINI
        CALL PAGFLL(255)                            !set page background color to white
        IF (TRIM(META_FILE_KIND) == 'pdf') THEN
            CALL PSFONT('Helvetica')                !for 'pdf' use a postscript font; e.g. 'Times-Roman' or 'HELVETICA'
            CALL PSMODE('both')                     !allow both Greek and italic modes
        ELSE
            CALL HWFONT()                           !use a hardware font; choice depends on operating system
        ENDIF
        CALL CHASPC(-0.06_DP)                       !slightly adjust character spacing
        CALL TEXMOD('on')                           !allow for TeX-style statments in figure text
        CALL COLOR('black')                         !set text/axis/curve color by name

        !potentially modify axis system properties
        CALL GETLEN(NXL, NYL, NZL)
        NXL = CEILING(0.8_DP*NXL)
        CALL AXSLEN(NXL, CEILING(NXL*YAXIS_MOD))    !modify the aspect ratio of the axis system via y-axis scaLING

        !set axis system properties
        CALL NAME(TRIM(XLABEL), 'X')                !set x-axis label text
        CALL NAME(TRIM(YLABEL), 'Y')

        INVERT_XAXIS = .FALSE.
        IF (PRESENT(XAXIS_LIMITS)) THEN
            XAXIS_MIN = XAXIS_LIMITS(1)
            XAXIS_MAX = XAXIS_LIMITS(2)
            IF (XAXIS_MIN > XAXIS_MAX) THEN
                INVERT_XAXIS = .TRUE.
            ENDIF
        ELSE
            !set automatic scaling for x-axis and y-axis based on slightly scaled input ranges:
            XAXIS_MIN = MINVAL( [( MINVAL(XY_DATA%XVAL(1:XY_DATA%NPOINTS(I), I)), I=1,NDSETS )] )
            XAXIS_MAX = MAXVAL( [( MAXVAL(XY_DATA%XVAL(1:XY_DATA%NPOINTS(I), I)), I=1,NDSETS )] )
        ENDIF
        AXIS_RANGE = ABS(XAXIS_MAX - XAXIS_MIN)
        IF (INVERT_XAXIS) THEN
            TEMP = XAXIS_MIN
            XAXIS_MIN = XAXIS_MAX
            XAXIS_MAX = TEMP
        ENDIF
        XAXIS_MIN = XAXIS_MIN -0.008_DP*AXIS_RANGE
        XAXIS_MAX = XAXIS_MAX +0.008_DP*AXIS_RANGE

        INVERT_YAXIS = .FALSE.
        IF (PRESENT(YAXIS_LIMITS)) THEN
            YAXIS_MIN = YAXIS_LIMITS(1)
            YAXIS_MAX = YAXIS_LIMITS(2)
            IF (YAXIS_MIN > YAXIS_MAX) THEN
                INVERT_YAXIS = .TRUE.
            ENDIF
        ELSE
            YAXIS_MIN = MINVAL( [( MINVAL( XY_DATA%YVAL(1:XY_DATA%NPOINTS(I), I)), I=1,NDSETS )] )
            YAXIS_MAX = MAXVAL( [( MAXVAL( XY_DATA%YVAL(1:XY_DATA%NPOINTS(I), I)), I=1,NDSETS )] )
        ENDIF
        AXIS_RANGE = ABS(YAXIS_MAX - YAXIS_MIN)
        IF (INVERT_YAXIS) THEN
            TEMP = YAXIS_MIN
            YAXIS_MIN = YAXIS_MAX
            YAXIS_MAX = TEMP
        ENDIF
        YAXIS_MIN = YAXIS_MIN -(0.015_DP/YAXIS_MOD)*AXIS_RANGE
        YAXIS_MAX = YAXIS_MAX +(0.015_DP/YAXIS_MOD)*AXIS_RANGE

        !initialize graph axis system (using automatic scaling):
        NDIG = -2                                   !set as -2 so dislin determines number of displayed digits AUTOMATICALLY
        CALL NOCHEK()                               !suppress warning about points outside of the plotting area
        IF (INVERT_XAXIS) THEN
            CALL GAXPAR(XAXIS_MAX, XAXIS_MIN, 'noextend', 'x', XA, XE, XOR, XSTEP, NDIG)
        ELSE
            CALL GAXPAR(XAXIS_MIN, XAXIS_MAX, 'noextend', 'x', XA, XE, XOR, XSTEP, NDIG)
        ENDIF
        CALL LABDIG(NDIG, 'x')
        NDIG = -2
        IF (INVERT_YAXIS) THEN
            CALL GAXPAR(YAXIS_MAX, YAXIS_MIN, 'noextend', 'y', YA, YE, YOR, YSTEP, NDIG)
        ELSE
            CALL GAXPAR(YAXIS_MIN, YAXIS_MAX, 'noextend', 'y', YA, YE, YOR, YSTEP, NDIG)
        ENDIF
        CALL LABDIG(NDIG, 'y')
        CALL GRAF(XA, XE, XOR, XSTEP, YA, YE, YOR, YSTEP)
        CALL LEGINI(CBUFF, MIN(NDSETS, 30), 75)     !initialize legend

        !plot x--y data for all curves/symbols with the curve-specific properties:
        DO I = 1,NDSETS
            NPTS = XY_DATA%NPOINTS(I)
            !select plotting of only symbols, only curves, or both:
            SELECT CASE( TRIM(XY_DATA%PLOT_SYMB(I)) )
            CASE('curve')
                CALL INCMRK(0)
            CASE('symbols')
                CALL INCMRK(-1)
                CALL MARKER(XY_DATA%SYMBOL_ID(I))
            CASE('both')
                CALL INCMRK(1)
                CALL MARKER(XY_DATA%SYMBOL_ID(I))
            CASE DEFAULT
                CALL INCMRK(0)
            END SELECT
            !select line style:
            SELECT CASE( TRIM(XY_DATA%LINE_STYLE(I)) )
            CASE('solid')
                CALL LNCAP('long')
                CALL SOLID()
            CASE('dotted')
                CALL LNCAP('round')     !rounded line caps
                CALL DOT()
            CASE('dashed')
                CALL LNCAP('round')
                CALL DASH()
            CASE('dashed_medium')
                CALL LNCAP('cut')
                CALL DASHM()
            CASE DEFAULT                !solid line style
                CALL LNCAP('long')
                CALL SOLID()
            END SELECT
            CALL PENWID(REAL(XY_DATA%PEN_WIDTH(I), KIND=DP))    !set pen / curve width (especially for pdf outPUT)
            CALL SETRGB(XY_DATA%RGB_COLOR(1,I)/255.0_DP, XY_DATA%RGB_COLOR(2,I)/255.0_DP, &
                & XY_DATA%RGB_COLOR(3,I)/255.0_DP)              !set color by RGB value
            CALL LEGLIN(CBUFF, XY_DATA%LEGTEXT(I), I)
            CALL CURVE(REAL(XY_DATA%XVAL(1:NPTS,I), KIND=DP), REAL(XY_DATA%YVAL(1:NPTS,I), KIND=DP), NPTS)
        ENDDO

        !plot legend and finalize plot:
        IF (LEGEND_POSITION /= 0) THEN
            CALL HEIGHT(26)                             !set font size (height) for legend
            CALL COLOR('black')
            CALL PENWID(1.0_DP)
            CALL LEGTIT('')
            CALL FRAME(1)                               !set legend frame thickness
            CALL LINESP(2.0_DP)                         !modify line spacing of legend entries
            CALL LEGEND(CBUFF, LEGEND_POSITION)         !plot legend; e.g. 3 = position at upper right corner OF PAGE
        ENDIF
        CALL ENDGRF
        CALL DISFIN
    ENDDO !iplot

    DEALLOCATE(CBUFF)
    !deallocate xy_data content so the next plot is not including current data:
    DEALLOCATE( XY_DATA, STAT=ISTAT )

    END SUBROUTINE DISLIN_PLOT
    !------------------------------------------------------------

END MODULE ModLib_DislinPlots
