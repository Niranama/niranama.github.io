
MODULE ModLib_Ogpf

!-------------------------------------------------------------------------------
!    GnuPlot Interface
!-------------------------------------------------------------------------------
!    Purpose:   Object Based Interface to GnuPlot from Fortran (ogpf)
!    Platform:  Windows XP/Vista/7/10
!               (It should work on other platforms, see the finalize_plot subroutine below)
!    Language:  Fortran 2003 and 2008
!    Requires:  1. Fortran 2003 compiler (e.g gfortran 5, IVF 12.1, ...)
!                  There is only two more features needs Fortran 2008 standard
!                  execute_command_line and passing internal function as argument.
!               2. gnuplot 5 and higher (other previous version can be used
!    Author:    Mohammad Rahmani
!               Chem Eng Dep., Amirkabir Uni. of Tech
!               Tehran, Ir
!               url:    aut.ac.ir/m.rahmani
!               github: github.com/kookma
!               email:  m[dot]rahmani[at]aut[dot]ac[dot]ir
!
!
! Acknowledgement:
! Special thanks to Hagen Wierstorf (http://www.gnuplotting.org)
! For valuable codes and examples on using gnuplot
! Some examples and color palettes are provided by gnuplotting.
!
    IMPLICIT NONE

    PRIVATE

    PUBLIC ARANGE, LINSPACE, MESHGRID, WP
    PUBLIC NUM2STR

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! select precision
    !
    !     sp: kind for single precision
    !     dp: kind for double precision
    !
    !     wp: kind for working precision (set to either sp or dp)
    INTEGER, PARAMETER :: SP = KIND( 1.0 )
    INTEGER, PARAMETER :: DP = KIND( 1.0D0 )

    INTEGER, PARAMETER :: WP = DP
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ! Library information
    CHARACTER(LEN=*), PARAMETER :: MD_NAME = 'ogpf libray'
    CHARACTER(LEN=*), PARAMETER :: MD_REV  = 'Rev. 0.22 of March 9th, 2018'
    CHARACTER(LEN=*), PARAMETER :: MD_LIC  = 'Licence: MIT'

    ! ogpf Configuration parameters
    ! The terminal and font have been set for Windows operating system
    ! Correct to meet the requirements on other OS like Linux and Mac.
    CHARACTER(LEN=*), PARAMETER ::  GNUPLOT_TERM_TYPE = 'wxt'                      ! Output terminal
    CHARACTER(LEN=*), PARAMETER ::  GNUPLOT_TERM_FONT = 'verdana,10'               ! font
!    CHARACTER(LEN=*), PARAMETER ::  GNUPLOT_TERM_SIZE = '640,480'                  ! plot window SIZE
    CHARACTER(LEN=*), PARAMETER ::  GNUPLOT_TERM_SIZE = '960,840'                  ! plot window SIZE
!    CHARACTER(LEN=*), PARAMETER ::  GNUPLOT_OUTPUT_FILENAME='ogpf_temp_script.gp' ! temporary file for output
    CHARACTER(LEN=*), PARAMETER ::  GNUPLOT_OUTPUT_FILENAME='ogpf_temp_script.plt' ! temporary file for output
    ! extra configuration can be set using ogpf object

    ! module procedure
    INTERFACE NUM2STR ! convert integer, real, double precision into string
        MODULE PROCEDURE NUM2STR_I4
        MODULE PROCEDURE NUM2STR_R4
        MODULE PROCEDURE NUM2STR_R8
    END INTERFACE

    !> 0.22
    ! tplabel is a structure for gnuplot labels including
    ! title, xlabel, x2label, ylabel, ...
    INTEGER, PARAMETER, PRIVATE :: NOT_INITIALIZED = -32000
    TYPE TPLABEL
        LOGICAL                       :: HAS_LABEL = .FALSE.
        CHARACTER(LEN=:), ALLOCATABLE :: LBLTEXT
        CHARACTER(LEN=:), ALLOCATABLE :: LBLCOLOR
        CHARACTER(LEN=:), ALLOCATABLE :: LBLFONTNAME
        INTEGER                       :: LBLFONTSIZE = NOT_INITIALIZED
        INTEGER                       :: LBLROTATE   = NOT_INITIALIZED
    END TYPE TPLABEL


    TYPE, PUBLIC :: GPF
        ! the gpf class implement the object for using gnuplot from fortran in a semi-interactive mode!
        ! the fortran actually do the job and write out the commands and data in a single file and then
        ! calls the gnuplot by shell command to plot the data

        PRIVATE

        !> 0.22
        TYPE(TPLABEL) :: TPPLOTTITLE
        TYPE(TPLABEL) :: TPXLABEL
        TYPE(TPLABEL) :: TPX2LABEL
        TYPE(TPLABEL) :: TPYLABEL
        TYPE(TPLABEL) :: TPY2LABEL
        TYPE(TPLABEL) :: TPZLABEL

        CHARACTER(LEN=:), ALLOCATABLE  :: TXTOPTIONS    ! a long string to store all type of gnuplot options
        CHARACTER(LEN=:), ALLOCATABLE  :: TXTSCRIPT     ! a long string to store gnuplot script
        CHARACTER(LEN=:), ALLOCATABLE  :: TXTDATASTYLE  ! lines, points, linepoints

        LOGICAL :: HASXRANGE      = .FALSE.
        LOGICAL :: HASX2RANGE     = .FALSE.
        LOGICAL :: HASYRANGE      = .FALSE.
        LOGICAL :: HASY2RANGE     = .FALSE.
        LOGICAL :: HASZRANGE      = .FALSE.
        LOGICAL :: HASOPTIONS     = .FALSE.
        LOGICAL :: HASANIMATION   = .FALSE.
        LOGICAL :: HASFILENAME    = .FALSE.
        LOGICAL :: HASFILEOPEN    = .FALSE.

        REAL(WP)           :: XRANGE(2), YRANGE(2), ZRANGE(2)
        REAL(WP)           :: X2RANGE(2), Y2RANGE(2)
        CHARACTER(LEN=8)   :: PLOTSCALE


        ! multiplot parameters
        LOGICAL :: HASMULTIPLOT = .FALSE.
        INTEGER :: MULTIPLOT_ROWS
        INTEGER :: MULTIPLOT_COLS
        INTEGER :: MULTIPLOT_TOTAL_PLOTS

        ! animation
        INTEGER  :: PAUSE_SECONDS = 0  ! keep plot on screen for this value in seconds
        INTEGER                         :: FRAME_NUMBER   ! frame number in animation

        ! use for debugging and error handling
        CHARACTER(LEN=:), ALLOCATABLE   :: MSG      !Message from plot procedures
        INTEGER                         :: STATUS=0 !Status from plot procedures

        !
        INTEGER                         :: FILE_UNIT      ! file unit identifier
        CHARACTER(LEN=:), ALLOCATABLE   :: TXTFILENAME    ! the name of physical file
                                                          ! to write the gnuplot script


        ! ogpf preset configuration (kind of gnuplot initialization)
        LOGICAL :: PRESET_CONFIGURATION = .TRUE.


    CONTAINS

        PRIVATE

        ! local private procedures
        PROCEDURE, PASS, PRIVATE :: PRESET_GNUPLOT_CONFIG

        PROCEDURE, PASS, PRIVATE :: PLOT2D_VECTOR_VS_VECTOR
        PROCEDURE, PASS, PRIVATE :: PLOT2D_MATRIX_VS_VECTOR
        PROCEDURE, PASS, PRIVATE :: PLOT2D_MATRIX_VS_MATRIX

        PROCEDURE, PASS, PRIVATE :: SEMILOGXV
        PROCEDURE, PASS, PRIVATE :: SEMILOGXM
        PROCEDURE, PASS, PRIVATE :: SEMILOGYV
        PROCEDURE, PASS, PRIVATE :: SEMILOGYM
        PROCEDURE, PASS, PRIVATE :: LOGLOGV
        PROCEDURE, PASS, PRIVATE :: LOGLOGM

        !> 0.22
        PROCEDURE, PASS, PRIVATE :: SET_LABEL

        ! public procedures
        PROCEDURE, PASS, PUBLIC :: OPTIONS      => SET_OPTIONS
        PROCEDURE, PASS, PUBLIC :: TITLE        => SET_PLOTTITLE
        PROCEDURE, PASS, PUBLIC :: XLABEL       => SET_XLABEL
        PROCEDURE, PASS, PUBLIC :: X2LABEL      => SET_X2LABEL
        PROCEDURE, PASS, PUBLIC :: YLABEL       => SET_YLABEL
        PROCEDURE, PASS, PUBLIC :: Y2LABEL      => SET_Y2LABEL
        PROCEDURE, PASS, PUBLIC :: ZLABEL       => SET_ZLABEL
        PROCEDURE, PASS, PUBLIC :: AXIS         => SET_AXIS
        PROCEDURE, PASS, PUBLIC :: AXIS_SC      => SET_SECONDARY_AXIS
        PROCEDURE, PASS, PUBLIC :: FILENAME     => SET_FILENAME
        PROCEDURE, PASS, PUBLIC :: RESET        => RESET_TO_DEFAULTS
        PROCEDURE, PASS, PUBLIC :: PRESET       => USE_PRESET_CONFIGURATION


        PROCEDURE, PASS, PUBLIC :: MULTIPLOT  => SUB_MULTIPLOT
        GENERIC, PUBLIC         :: PLOT       => PLOT2D_VECTOR_VS_VECTOR, &
                                                 PLOT2D_MATRIX_VS_VECTOR, &
                                                 PLOT2D_MATRIX_VS_MATRIX
        GENERIC, PUBLIC         :: SEMILOGX   => SEMILOGXV, SEMILOGXM
        GENERIC, PUBLIC         :: SEMILOGY   => SEMILOGYV, SEMILOGYM
        GENERIC, PUBLIC         :: LOGLOG     => LOGLOGV, LOGLOGM

        PROCEDURE, PASS, PUBLIC :: SURF       => SPLOT  ! 3D surface plot
        PROCEDURE, PASS, PUBLIC :: LPLOT      => LPLOT3D  ! 3D line plot
        PROCEDURE, PASS, PUBLIC :: CONTOUR    => CPLOT  ! contour plot

        PROCEDURE, PASS, PUBLIC :: FPLOT      => FUNCTION_PLOT

        PROCEDURE, PASS, PUBLIC :: ADD_SCRIPT => ADDSCRIPT
        PROCEDURE, PASS, PUBLIC :: RUN_SCRIPT => RUNSCRIPT

        PROCEDURE, PASS, PUBLIC :: ANIMATION_START => SUB_ANIMATION_START
        PROCEDURE, PASS, PUBLIC :: ANIMATION_SHOW  => SUB_ANIMATION_SHOW

    END TYPE GPF


CONTAINS

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section One: Set/Get Methods for ogpf object
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    SUBROUTINE USE_PRESET_CONFIGURATION(THIS,FLAG)
        !..............................................................................
        !Set a flag to tell ogpf if the customized gnuplot configuration should
        !be used
        !..............................................................................

        CLASS(GPF):: THIS
        LOGICAL, INTENT(IN) :: FLAG

        ! default is true
        THIS%PRESET_CONFIGURATION = FLAG

    END SUBROUTINE USE_PRESET_CONFIGURATION



    SUBROUTINE SET_FILENAME(THIS,STRING)
        !..............................................................................
        !Set a file name for plot command output
        !This file can be used later by gnuplot as an script file to reproduce the plot
        !..............................................................................

        CLASS(GPF):: THIS
        CHARACTER(LEN=*), INTENT(IN) :: STRING

        THIS%TXTFILENAME = TRIM(STRING)
        THIS%HASFILENAME = .TRUE.

    END SUBROUTINE SET_FILENAME


    SUBROUTINE SET_OPTIONS(THIS,STROPT)
        !..............................................................................
        ! Set the plot options. This is a very powerfull procedure accepts many types
        ! of gnuplot command and customization
        !..............................................................................

        CLASS(GPF):: THIS
        CHARACTER(LEN=*), INTENT(IN) :: STROPT

        IF(.NOT.ALLOCATED(THIS%TXTOPTIONS))THIS%TXTOPTIONS=''
        IF (LEN_TRIM(THIS%TXTOPTIONS) == 0 ) THEN
            THIS%TXTOPTIONS = '' ! initialize string
        END IF
        IF ( LEN_TRIM(STROPT)>0 ) THEN
            THIS%TXTOPTIONS = THIS%TXTOPTIONS // SPLITSTR(STROPT)
          END IF

        THIS%HASOPTIONS=.TRUE.

    END SUBROUTINE SET_OPTIONS




    SUBROUTINE SET_AXIS(THIS,RNG)
        !..............................................................................
        !Set the axes limits in form of [xmin, xmax, ymin, ymax, zmin, zmax]
        !..............................................................................

        CLASS(GPF):: THIS
        REAL(WP), INTENT(IN) :: RNG(:)
        INTEGER :: N
        N=SIZE(RNG,DIM=1)
        SELECT CASE(N)
            CASE(2) !Only the range for x-axis has been sent
                THIS%HASXRANGE=.TRUE.
                THIS%XRANGE=RNG(1:2)
            CASE(4)
                THIS%HASXRANGE=.TRUE.
                THIS%HASYRANGE=.TRUE.
                THIS%XRANGE=RNG(1:2)
                THIS%YRANGE=RNG(3:4)
            CASE(6)
                THIS%HASXRANGE=.TRUE.
                THIS%HASYRANGE=.TRUE.
                THIS%HASZRANGE=.TRUE.
                THIS%XRANGE=RNG(1:2)
                THIS%YRANGE=RNG(3:4)
                THIS%ZRANGE=RNG(5:6)
            CASE DEFAULT
                PRINT*, 'gpf error: wrong axis range setting!'
                RETURN
        END SELECT

    END SUBROUTINE SET_AXIS


    SUBROUTINE SET_SECONDARY_AXIS(THIS,RNG)
        !..............................................................................
        !Set the secondary axes limits in form of [x2min, x2max, y2min, y2max]
        !..............................................................................

        CLASS(GPF):: THIS
        REAL(WP), INTENT(IN) :: RNG(:)
        INTEGER :: N
        N=SIZE(RNG,DIM=1)
        SELECT CASE(N)
            CASE(2) !Only the range for x2-axis has been sent
                THIS%HASX2RANGE=.TRUE.
                THIS%X2RANGE=RNG(1:2)
            CASE(4)
                THIS%HASX2RANGE=.TRUE.
                THIS%HASY2RANGE=.TRUE.
                THIS%X2RANGE=RNG(1:2)
                THIS%Y2RANGE=RNG(3:4)
            CASE DEFAULT
                PRINT*, 'gpf error: wrong axis range setting!'
                RETURN
        END SELECT

    END SUBROUTINE SET_SECONDARY_AXIS


    SUBROUTINE SET_PLOTTITLE(THIS, STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)
        !..............................................................................
        !Set the plot title
        !..............................................................................
        CLASS(GPF):: THIS
        CHARACTER(LEN=*), INTENT(IN)           :: STRING
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: TEXTCOLOR
        INTEGER, OPTIONAL                      :: FONT_SIZE
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FONT_NAME
        INTEGER, OPTIONAL                      :: ROTATE

        CALL THIS%SET_LABEL('plot_title', STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)

    END SUBROUTINE SET_PLOTTITLE


    SUBROUTINE SET_XLABEL(THIS, STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)
        !..............................................................................
        !Set the xlabel
        !..............................................................................
        CLASS(GPF):: THIS
        CHARACTER(LEN=*), INTENT(IN)           :: STRING
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: TEXTCOLOR
        INTEGER, OPTIONAL                      :: FONT_SIZE
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FONT_NAME
        INTEGER, OPTIONAL                      :: ROTATE

        CALL THIS%SET_LABEL('xlabel', STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)

    END SUBROUTINE SET_XLABEL


    SUBROUTINE SET_X2LABEL(THIS, STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)
        !..............................................................................
        !Set the x2label
        !..............................................................................
        CLASS(GPF):: THIS
        CHARACTER(LEN=*), INTENT(IN)           :: STRING
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: TEXTCOLOR
        INTEGER, OPTIONAL                      :: FONT_SIZE
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FONT_NAME
        INTEGER, OPTIONAL                      :: ROTATE

        CALL THIS%SET_LABEL('x2label', STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)

    END SUBROUTINE SET_X2LABEL


    SUBROUTINE SET_YLABEL(THIS, STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)
        !..............................................................................
        !Set the ylabel
        !..............................................................................
        CLASS(GPF):: THIS
        CHARACTER(LEN=*), INTENT(IN)           :: STRING
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: TEXTCOLOR
        INTEGER, OPTIONAL                      :: FONT_SIZE
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FONT_NAME
        INTEGER, OPTIONAL                      :: ROTATE

        CALL THIS%SET_LABEL('ylabel', STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)

    END SUBROUTINE SET_YLABEL



    SUBROUTINE SET_Y2LABEL(THIS, STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)
        !..............................................................................
        !Set the y2label
        !..............................................................................
        CLASS(GPF):: THIS
        CHARACTER(LEN=*), INTENT(IN)           :: STRING
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: TEXTCOLOR
        INTEGER, OPTIONAL                      :: FONT_SIZE
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FONT_NAME
        INTEGER, OPTIONAL                      :: ROTATE

        CALL THIS%SET_LABEL('y2label', STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)

    END SUBROUTINE SET_Y2LABEL


    SUBROUTINE SET_ZLABEL(THIS, STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)
        !..............................................................................
        !Set the zlabel
        !..............................................................................
        CLASS(GPF):: THIS
        CHARACTER(LEN=*), INTENT(IN)           :: STRING
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: TEXTCOLOR
        INTEGER, OPTIONAL                      :: FONT_SIZE
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FONT_NAME
        INTEGER, OPTIONAL                      :: ROTATE

        CALL THIS%SET_LABEL('zlabel', STRING, TEXTCOLOR, FONT_SIZE, FONT_NAME, ROTATE)

    END SUBROUTINE SET_ZLABEL


    !> 0.22

    SUBROUTINE SET_LABEL(THIS, LBLNAME, LBLTEXT, LBLCOLOR, FONT_SIZE, FONT_NAME, ROTATE)
        !..............................................................................
        ! Set the text, color, font, size and rotation for labels including
        ! title, xlabel, x2label, ylabel, ....
        !..............................................................................

        CLASS(GPF):: THIS
        CHARACTER(LEN=*), INTENT(IN)           :: LBLNAME
        CHARACTER(LEN=*), INTENT(IN)           :: LBLTEXT
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: LBLCOLOR
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FONT_NAME
        INTEGER, OPTIONAL :: FONT_SIZE
        INTEGER, OPTIONAL                      :: ROTATE

        ! local variable
        TYPE(TPLABEL) :: LABEL

        LABEL%HAS_LABEL = .TRUE.
        LABEL%LBLTEXT   = TRIM(LBLTEXT)

        IF (PRESENT(LBLCOLOR)) THEN
            LABEL%LBLCOLOR = LBLCOLOR
        END IF

        IF (PRESENT(FONT_NAME)) THEN
            LABEL%LBLFONTNAME = FONT_NAME
        ELSE
            IF(.NOT.ALLOCATED(LABEL%LBLFONTNAME))THEN
                LABEL%LBLFONTNAME = ''
            ENDIF
        END IF

        IF (PRESENT(FONT_SIZE)) THEN
            LABEL%LBLFONTSIZE = FONT_SIZE
        END IF

        IF (PRESENT(ROTATE)) THEN
            LABEL%LBLROTATE = ROTATE
        END IF

        SELECT CASE (LBLNAME)
            CASE ('xlabel')
                THIS%TPXLABEL     = LABEL
            CASE ('x2label')
                THIS%TPX2LABEL    = LABEL
            CASE ('ylabel')
                THIS%TPYLABEL     = LABEL
            CASE ('y2label')
                THIS%TPY2LABEL    = LABEL
            CASE ('zlabel')
                THIS%TPZLABEL     = LABEL
            CASE ('plot_title')
                THIS%TPPLOTTITLE  = LABEL
        END SELECT


    END SUBROUTINE SET_LABEL



    SUBROUTINE RESET_TO_DEFAULTS(THIS)
        !..............................................................................
        !Reset all ogpf properties (params to their default values
        !...............................................................................
        CLASS(GPF):: THIS

        THIS%PRESET_CONFIGURATION    = .TRUE.
        THIS%TXTFILENAME             = GNUPLOT_OUTPUT_FILENAME

        IF (ALLOCATED(THIS%TXTOPTIONS))    DEALLOCATE(THIS%TXTOPTIONS)
        IF (ALLOCATED(THIS%TXTSCRIPT))     DEALLOCATE(THIS%TXTSCRIPT)
        IF (ALLOCATED(THIS%TXTDATASTYLE))  DEALLOCATE(THIS%TXTDATASTYLE)
        IF (ALLOCATED(THIS%MSG))           DEALLOCATE(THIS%MSG)

        THIS%HASOPTIONS            = .FALSE.

        THIS%HASXRANGE             = .FALSE.
        THIS%HASX2RANGE            = .FALSE.
        THIS%HASYRANGE             = .FALSE.
        THIS%HASY2RANGE            = .FALSE.
        THIS%HASZRANGE             = .FALSE.

        THIS%PAUSE_SECONDS         = 0
        THIS%STATUS                = 0
        THIS%HASANIMATION          = .FALSE.
        THIS%HASFILEOPEN           = .FALSE.
        THIS%HASMULTIPLOT          = .FALSE.

        THIS%PLOTSCALE             = ''
        THIS%TPPLOTTITLE%HAS_LABEL =.FALSE.
        THIS%TPXLABEL%HAS_LABEL    =.FALSE.
        THIS%TPX2LABEL%HAS_LABEL   =.FALSE.
        THIS%TPYLABEL%HAS_LABEL    =.FALSE.
        THIS%TPY2LABEL%HAS_LABEL   =.FALSE.
        THIS%TPZLABEL%HAS_LABEL    =.FALSE.


    END SUBROUTINE RESET_TO_DEFAULTS


    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Two: Main Plotting Routines
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    SUBROUTINE SUB_MULTIPLOT(THIS, ROWS, COLS)
        !..............................................................................
        ! This subroutine sets flag and number of rows and columns in case
        ! of multiplot layout
        !..............................................................................

        CLASS(GPF):: THIS
        INTEGER, INTENT(IN) :: ROWS
        INTEGER, INTENT(IN) :: COLS

        ! ogpf does not support multiplot in animation mode
        IF (THIS%HASANIMATION) THEN
            PRINT*, MD_NAME // ': ogpf does not support animation in multiplot mode'
            STOP
        END IF

        ! set multiplot cols and rows
        IF (ROWS> 0 ) THEN
            THIS%MULTIPLOT_ROWS = ROWS
        ELSE

        END IF
        IF (COLS > 0 ) THEN
            THIS%MULTIPLOT_COLS = COLS
        ELSE

        END IF

        ! set the multiplot layout flag and plot numbers
        THIS%HASMULTIPLOT = .TRUE.
        THIS%MULTIPLOT_TOTAL_PLOTS = 0

        ! create the ouput file for writting gnuplot script
        CALL CREATE_OUTPUTFILE(THIS)


    END SUBROUTINE SUB_MULTIPLOT


    SUBROUTINE PLOT2D_VECTOR_VS_VECTOR(THIS, X1, Y1, LS1, AXES1, &
            X2, Y2, LS2, AXES2, &
            X3, Y3, LS3, AXES3, &
            X4, Y4, LS4, AXES4  )
        !..............................................................................
        ! This procedure plots:
        !   1. A vector against another vector (xy plot)
        !   2. A vector versus its element indices (yi plot).
        !   3. Can accept up to 4 data sets as x,y pairs!
        ! Arguments
        ! xi, yi vectors of data series,
        ! lsi a string maximum 80 characters containing the line specification,
        ! legends, ...
        ! axesi is the axes for plotting: secondary axes are x2, and y2
        !..............................................................................

        CLASS(GPF):: THIS
        ! Input vector
        REAL(WP),  INTENT(IN)                          :: X1(:)  ! vector of data for x
        REAL(WP),  INTENT(IN), OPTIONAL                :: Y1(:)  ! vector of data for y
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS1    ! line specification
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES1

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X2
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y2
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS2
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES2

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X3
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y3
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS3
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES3

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X4
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y4
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS4
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES4

        !   Local variables
        !----------------------------------------------------------------------

        INTEGER:: NX1
        INTEGER:: NY1
        INTEGER:: NX2
        INTEGER:: NY2
        INTEGER:: NX3
        INTEGER:: NY3
        INTEGER:: NX4
        INTEGER:: NY4
        INTEGER::           NUMBER_OF_PLOTS
        CHARACTER(LEN=3)::  PLOTTYPE
        INTEGER:: I
        CHARACTER(LEN=80) ::  PLTSTRING(4)  ! Four 80 characters string

        !Initialize variables
        PLOTTYPE  = ''
        PLTSTRING = ''

        !   Check the input
        NX1=SIZE(X1)
        IF ((PRESENT(Y1) )) THEN
            NY1=SIZE(Y1)
            IF (CHECKDIM(NX1,NY1)) THEN
                PLOTTYPE='xy1'
                NUMBER_OF_PLOTS=1
            ELSE
                PRINT*, MD_NAME // ':plot2d_vector_vs_vector:' // 'length of x1 and y1 does not match'
                RETURN
            END IF
        ELSE !plot only x againest its element indices
            PLOTTYPE='xi'
            NUMBER_OF_PLOTS=1
        END IF

        !Process line spec and axes set for first data set if present
        CALL PROCESS_LINESPEC(1, PLTSTRING(1), LS1, AXES1)


        IF (PRESENT(X2) .AND. PRESENT (Y2)) THEN
            NX2=SIZE(X2)
            NY2=SIZE(Y2)
            IF (CHECKDIM(NX2,NY2)) THEN
                PLOTTYPE='xy2'
                NUMBER_OF_PLOTS=2
            ELSE
                RETURN
            END IF
            !Process line spec for 2nd data set if present
            CALL PROCESS_LINESPEC(2, PLTSTRING(2), LS2, AXES2)
        END IF

        IF (PRESENT(X3) .AND. PRESENT (Y3)) THEN
            NX3=SIZE(X3)
            NY3=SIZE(Y3)
            IF (CHECKDIM(NX3,NY3)) THEN
                PLOTTYPE='xy3'
                NUMBER_OF_PLOTS=3
            ELSE
                RETURN
            END IF
            !Process line spec for 3rd data set if present
            CALL PROCESS_LINESPEC(3, PLTSTRING(3), LS3, AXES3)
        END IF

        IF (PRESENT(X4) .AND. PRESENT (Y4)) THEN
            NX4=SIZE(X4)
            NY4=SIZE(Y4)
            IF (CHECKDIM(NX4,NY4)) THEN
                PLOTTYPE='xy4'
                NUMBER_OF_PLOTS=4
            ELSE
                RETURN
            END IF
            !Process line spec for 4th data set if present
            CALL PROCESS_LINESPEC(4, PLTSTRING(4), LS4, AXES4)
        END IF


        CALL CREATE_OUTPUTFILE(THIS)

        ! Write plot title, axis labels and other annotations
        CALL PROCESSCMD(THIS)

        ! Write plot command and line styles and legend if any
        IF (NUMBER_OF_PLOTS ==1) THEN
            WRITE ( THIS%FILE_UNIT, '(a)' )  TRIM(PLTSTRING(1))
        ELSE
            WRITE ( THIS%FILE_UNIT, '(a)' )  ( TRIM(PLTSTRING(I)) // ' \' , I=1, NUMBER_OF_PLOTS-1)
            WRITE ( THIS%FILE_UNIT, '(a)' )  TRIM(PLTSTRING(NUMBER_OF_PLOTS))
        END IF
        ! Write xy data into file
        SELECT CASE (PLOTTYPE)
            CASE ('xi')
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX1,X1)
            CASE ('xy1')
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX1,X1,Y1)
            CASE ('xy2')
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX1,X1,Y1)
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX2,X2,Y2)
            CASE ('xy3')
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX1,X1,Y1)
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX2,X2,Y2)
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX3,X3,Y3)
            CASE ('xy4')
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX1,X1,Y1)
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX2,X2,Y2)
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX3,X3,Y3)
                CALL WRITE_XYDATA(THIS%FILE_UNIT,NX4,X4,Y4)
        END SELECT

        !> Rev 0.2
        ! if there is no animation finalize
        IF (.NOT. (THIS%HASANIMATION)) THEN
            CALL FINALIZE_PLOT(THIS)
        ELSE
            WRITE(THIS%FILE_UNIT, '(a, I2)') 'pause ', THIS%PAUSE_SECONDS
        END IF


        !: End of plot2D_vector_vs_vector
    END SUBROUTINE PLOT2D_VECTOR_VS_VECTOR



    SUBROUTINE  PLOT2D_MATRIX_VS_VECTOR(THIS, XV,YMAT, LSPEC)
        !..............................................................................
        ! plot2D_matrix_vs_vector accepts a vector xv and a matrix ymat and plots
        ! columns of ymat against xv. lspec is an optional array defines the line
        ! specification for each data series. If a single element array is sent for
        ! lspec then all series are plotted using the same linespec
        !..............................................................................

        IMPLICIT NONE
        CLASS(GPF):: THIS
        ! Input arrays
        REAL(WP),  INTENT(IN)                       :: XV(:)
        REAL(WP),  INTENT(IN)                       :: YMAT(:,:)
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL     :: LSPEC
        !----------------------------------------------------------------------
        !       Local variables
        INTEGER:: NX
        INTEGER:: NY
        INTEGER:: NS
        INTEGER:: NUMBER_OF_CURVES
        INTEGER:: I
        INTEGER:: J
        INTEGER:: IERR
        CHARACTER(LEN=80), ALLOCATABLE ::  PLTSTRING(:), LST(:)
        !

        !*******************************************************************************
        !   Check the input
        NX=SIZE(XV)
        NY=SIZE(YMAT,DIM=1)
        IF (.NOT. CHECKDIM(NX,NY)) THEN
            PRINT*, MD_NAME // ':plot2d_matrix_vs_vector:' // 'The length of arrays does not match'
            RETURN
        END IF
        ! create the outfile to write the gnuplot script
        CALL CREATE_OUTPUTFILE(THIS)

        ! Write titles and other annotations
        CALL PROCESSCMD(THIS)

        ! Write plot command and line styles and legend if any
        NUMBER_OF_CURVES=SIZE(YMAT,DIM=2)
        ALLOCATE(PLTSTRING(NUMBER_OF_CURVES), STAT=IERR)
        IF (IERR /=0) THEN
            PRINT*, 'allocation error'
            RETURN
        END IF

        ! assume no linespec is available
        PLTSTRING(1:NUMBER_OF_CURVES) = ''

        IF ( PRESENT(LSPEC) ) THEN

            CALL SPLITSTRING2ARRAY(LSPEC,LST,';')
            NS = SIZE(LST, DIM=1)

            IF (NS == NUMBER_OF_CURVES) THEN
                ! there is a linespec for each curve
                PLTSTRING = LST
            ELSEIF (NS < NUMBER_OF_CURVES) THEN
                ! not enough linespec
                DO I=1, NS
                    PLTSTRING(I) = LST(I)
                END DO
            ELSE ! ns > number_of curves
                PRINT*, 'ogpf: plot2d_matrix_vs_vector: wrong number of linespec'
                PRINT*, 'semicolon ";" acts as delimiter, check the linespec'
            END IF
        END IF

        IF ( PRESENT(LSPEC) ) THEN

            CALL PROCESS_LINESPEC(1,PLTSTRING(1),LST(1))
            NS=SIZE(LST)
            ! gpf will cylce through line specification, if number of specification passed
            ! is less than number of plots
            DO I=1, NUMBER_OF_CURVES
                J=MOD(I-1, NS) + 1
                CALL PROCESS_LINESPEC(I, PLTSTRING(I), LST(J))
            END DO
        ELSE !No lspec is available
            PLTSTRING(1)=' plot "-" notitle,'
            PLTSTRING(2:NUMBER_OF_CURVES-1)='"-" notitle,'
            PLTSTRING(NUMBER_OF_CURVES)='"-" notitle'
        END IF

        ! Write plot command and line styles and legend if any
        WRITE ( THIS%FILE_UNIT, '(a)' ) ( TRIM(PLTSTRING(I)) // ' \' , I=1, NUMBER_OF_CURVES-1)
        WRITE ( THIS%FILE_UNIT, '(a)' )   TRIM(PLTSTRING(NUMBER_OF_CURVES))

        ! Write data into script file
        DO J=1, NUMBER_OF_CURVES
            DO I = 1, NX
                WRITE ( THIS%FILE_UNIT, * ) XV(I),YMAT(I,J)
            END DO
            WRITE ( THIS%FILE_UNIT, '(a)' ) 'e'  !end of jth set of data
        END DO


        !> Rev 0.2
        ! if there is no animation finalize
        IF (.NOT. (THIS%HASANIMATION)) THEN
            CALL FINALIZE_PLOT(THIS)
        ELSE
            WRITE(THIS%FILE_UNIT, '(a, I2)') 'pause ', THIS%PAUSE_SECONDS
        END IF

        !Release memory
        IF (ALLOCATED(PLTSTRING)) THEN
            DEALLOCATE(PLTSTRING)
        END IF
        !: End of plot2D_matrix_vs_vector
    END SUBROUTINE  PLOT2D_MATRIX_VS_VECTOR



    SUBROUTINE  PLOT2D_MATRIX_VS_MATRIX(THIS, XMAT,YMAT, LSPEC)
        !..............................................................................
        ! plot2D_matrix_vs_matrix accepts a matrix xmat and a matrix ymat and plots
        ! columns of ymat against columns of xmat. lspec is an optional array defines
        ! the line specification for each data series. If a single element array is
        ! sent for lspec then all series are plotted using the same linespec
        !..............................................................................

        IMPLICIT NONE
        CLASS(GPF):: THIS
        ! Input arrays
        REAL(WP),  INTENT(IN)                       :: XMAT(:,:)
        REAL(WP),  INTENT(IN)                       :: YMAT(:,:)
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL     :: LSPEC
        !----------------------------------------------------------------------
        !       Local variables
        INTEGER:: MX, NX
        INTEGER:: MY, NY
        INTEGER:: NS
        INTEGER:: NUMBER_OF_CURVES
        INTEGER:: I
        INTEGER:: J
        INTEGER:: IERR
        CHARACTER(LEN=80), ALLOCATABLE ::  PLTSTRING(:), LST(:)
        !

        !*******************************************************************************
        !   Check the input
        ! check number of rows
        MX=SIZE(XMAT,DIM=1)
        MY=SIZE(YMAT,DIM=1)
        IF (.NOT. CHECKDIM(MX,MY)) THEN
            PRINT*, MD_NAME // ':plot2d_matrix_vs_matrix:' // 'The length of arrays does not match'
            RETURN
        END IF
        ! check number of rows
        NX=SIZE(XMAT,DIM=2)
        NY=SIZE(YMAT,DIM=2)
        IF (.NOT. CHECKDIM(NX,NY)) THEN
            PRINT*, 'gpf error: The number of columns are different, check xmat, ymat'
            RETURN
        END IF


        ! create the outfile to write the gnuplot script
        CALL CREATE_OUTPUTFILE(THIS)

        ! Write titles and other annotations
        CALL PROCESSCMD(THIS)

        ! Write plot command and line styles and legend if any
        NUMBER_OF_CURVES=SIZE(YMAT,DIM=2)
        ALLOCATE(PLTSTRING(NUMBER_OF_CURVES), STAT=IERR)
        IF (IERR /=0) THEN
            PRINT*, 'allocation error'
            RETURN
        END IF

        ! assume no linespec is available
        PLTSTRING(1:NUMBER_OF_CURVES) = ''

        IF ( PRESENT(LSPEC) ) THEN

            CALL SPLITSTRING2ARRAY(LSPEC,LST,';')
            NS = SIZE(LST, DIM=1)

            IF (NS == NUMBER_OF_CURVES) THEN
                ! there is a linespec for each curve
                PLTSTRING = LST
            ELSEIF (NS < NUMBER_OF_CURVES) THEN
                ! not enough linespec
                DO I=1, NS
                    PLTSTRING(I) = LST(I)
                END DO
            ELSE ! ns > number_of curves
                PRINT*, MD_NAME // ': plot2d_matrix_vs_matrix:'//' wrong number of linespec'
                PRINT*, 'semicolon ";" acts as delimiter, check the linespec'
            END IF
        END IF

        IF ( PRESENT(LSPEC) ) THEN

            CALL PROCESS_LINESPEC(1,PLTSTRING(1),LST(1))
            NS=SIZE(LST)
            ! gpf will cylce through line specification, if number of specification passed
            ! is less than number of plots
            DO I=1, NUMBER_OF_CURVES
                J=MOD(I-1, NS) + 1
                CALL PROCESS_LINESPEC(I, PLTSTRING(I), LST(J))
            END DO
        ELSE !No lspec is available
            PLTSTRING(1)=' plot "-" notitle,'
            PLTSTRING(2:NUMBER_OF_CURVES-1)='"-" notitle,'
            PLTSTRING(NUMBER_OF_CURVES)='"-" notitle'
        END IF

        ! Write plot command and line styles and legend if any
        WRITE ( THIS%FILE_UNIT, '(a)' ) ( TRIM(PLTSTRING(I)) // ' \' , I=1, NUMBER_OF_CURVES-1)
        WRITE ( THIS%FILE_UNIT, '(a)' )   TRIM(PLTSTRING(NUMBER_OF_CURVES))

        ! Write data into script file
        DO J=1, NUMBER_OF_CURVES
            DO I = 1, MX
                WRITE ( THIS%FILE_UNIT, * ) XMAT(I,J),YMAT(I,J)
            END DO
            WRITE ( THIS%FILE_UNIT, '(a)' ) 'e'  !end of jth set of data
        END DO

        !> Rev 0.2
        ! if there is no animation finalize
        IF (.NOT. (THIS%HASANIMATION)) THEN
            CALL FINALIZE_PLOT(THIS)
        ELSE
            WRITE(THIS%FILE_UNIT, '(a, I2)') 'pause ', THIS%PAUSE_SECONDS
        END IF

        !Release memory
        IF (ALLOCATED(PLTSTRING)) THEN
            DEALLOCATE(PLTSTRING)
        END IF
        !: End of plot2D_matrix_vs_vector
    END SUBROUTINE  PLOT2D_MATRIX_VS_MATRIX


    SUBROUTINE SPLOT(THIS, X, Y, Z, LSPEC, PALETTE)
        !..............................................................................
        ! splot create a surface plot
        ! datablock is used instead of  gnuplot inline file "-"
        !..............................................................................

        CLASS(GPF):: THIS
        ! Input vector
        REAL(WP),  INTENT(IN)            :: X(:,:)
        REAL(WP),  INTENT(IN), OPTIONAL  :: Y(:,:)
        REAL(WP),  INTENT(IN), OPTIONAL  :: Z(:,:)
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  LSPEC
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  PALETTE

        !   Local variables
        !----------------------------------------------------------------------
        INTEGER:: NCX
        INTEGER:: NRX
        INTEGER:: I
        INTEGER:: J
        LOGICAL:: XYZ_DATA
        CHARACTER(LEN=80)::  PLTSTRING
        CHARACTER(LEN=*), PARAMETER ::  DATABLOCK = '$xyz'

        PLTSTRING=''
        !   Check the input data
        NCX=SIZE(X,DIM=2)
        NRX=SIZE(X,DIM=1)
        IF (PRESENT(Y) .AND. PRESENT(Z)) THEN
            XYZ_DATA=.TRUE.
        ELSEIF (PRESENT(Y)) THEN
            PRINT*, "gpf error: Z matrix was not sent to 3D plot routine"
            RETURN
        ELSE
            XYZ_DATA=.FALSE.
        END IF

        ! set default line style for 3D plot, can be overwritten
        THIS%TXTDATASTYLE = 'lines'
        ! create the script file for writting gnuplot commands and data
        CALL CREATE_OUTPUTFILE(THIS)

        ! Write titles and other annotations
        CALL PROCESSCMD(THIS)

        ! Write xy data into file
        WRITE ( THIS%FILE_UNIT, '(a)' ) '#data x y z'
        ! Rev 0.20
        ! write the $xyz datablocks
        WRITE( THIS%FILE_UNIT, '(a)' )  DATABLOCK // ' << EOD'
        IF (XYZ_DATA) THEN
            DO J=1,NCX
                DO I=1, NRX
                    WRITE ( THIS%FILE_UNIT, * ) X(I,J), Y(I,J), Z(I,J)
                ENDDO
                WRITE( THIS%FILE_UNIT, '(a)' )  !put an empty line
            ENDDO
            WRITE ( THIS%FILE_UNIT, '(a)' ) 'EOD'  !end of datablock
        ELSE !only Z has been sent (i.e. single matrix data)
            DO J=1,NCX
                DO I=1, NRX
                    WRITE ( THIS%FILE_UNIT, * ) I, J, X(I,J)
                ENDDO
                WRITE( THIS%FILE_UNIT, '(a)' )  !put an empty line
            ENDDO
            WRITE ( THIS%FILE_UNIT, '(a)' ) 'EOD'  !end of datablock
        END IF


        !write the color palette into gnuplot script file
        IF (PRESENT(PALETTE)) THEN
            WRITE ( THIS%FILE_UNIT, '(a)' )  COLOR_PALETTES(PALETTE)
            WRITE ( THIS%FILE_UNIT, '(a)' )  'set pm3d' ! a conflict with lspec
        END IF


        IF ( PRESENT(LSPEC) ) THEN
            IF (HASTITLE(LSPEC)) THEN
                PLTSTRING='splot ' // DATABLOCK // ' ' // TRIM(LSPEC)
            ELSE
                PLTSTRING='splot ' // DATABLOCK // ' notitle '//TRIM(LSPEC)
            END IF
        ELSE
            PLTSTRING='splot ' // DATABLOCK // ' notitle '
        END IF

        WRITE ( THIS%FILE_UNIT, '(a)' ) TRIM(PLTSTRING)


        !> Rev 0.2: animation
        ! if there is no animation finalize
        IF (.NOT. (THIS%HASANIMATION)) THEN
            CALL FINALIZE_PLOT(THIS)
        ELSE
            WRITE(THIS%FILE_UNIT, '(a, I2)') 'pause ', THIS%PAUSE_SECONDS
        END IF

        !: End of splot
    END SUBROUTINE SPLOT


    SUBROUTINE CPLOT(THIS, X, Y, Z, LSPEC, PALETTE)
        !..............................................................................
        !   Rev 0.19
        !   cplot creates a contour plot based on the three dimensional data
        !..............................................................................

        CLASS(GPF):: THIS
        ! Input vector
        REAL(WP),  INTENT(IN)            :: X(:,:)
        REAL(WP),  INTENT(IN), OPTIONAL  :: Y(:,:)
        REAL(WP),  INTENT(IN), OPTIONAL  :: Z(:,:)
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  LSPEC
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  PALETTE

        !   Local variables
        !----------------------------------------------------------------------

        INTEGER:: NCX
        INTEGER:: NRX
        INTEGER:: I
        INTEGER:: J
        LOGICAL:: XYZ_DATA
        CHARACTER(LEN=80)::  PLTSTRING
        CHARACTER(LEN=*), PARAMETER ::  DATABLOCK  = '$xyz'
        !       character(len=*), parameter ::  cntr_table = '$xyz_contour'

        PLTSTRING=''
        !   Check the input data
        NCX=SIZE(X,DIM=2)
        NRX=SIZE(X,DIM=1)
        IF (PRESENT(Y) .AND. PRESENT(Z)) THEN
            XYZ_DATA=.TRUE.
        ELSEIF (PRESENT(Y)) THEN
            PRINT*, "gpf error: Z matrix was not sent to 3D plot routine"
            RETURN
        ELSE
            XYZ_DATA=.FALSE.
        END IF

        ! set default line style for 3D plot, can be overwritten
        THIS%TXTDATASTYLE = 'lines'
        ! create the script file for writting gnuplot commands and data
        CALL CREATE_OUTPUTFILE(THIS)

        ! Write titles and other annotations
        CALL PROCESSCMD(THIS)

        ! Write xy data into file
        WRITE ( THIS%FILE_UNIT, '(a)' ) '#data x y z'
        ! write the $xyz datablocks
        WRITE( THIS%FILE_UNIT, '(a)' )  DATABLOCK // ' << EOD'
        IF (XYZ_DATA) THEN
            DO J=1,NCX
                DO I=1, NRX
                    WRITE ( THIS%FILE_UNIT, FMT=* ) X(I,J), Y(I,J), Z(I,J)
                ENDDO
                WRITE( THIS%FILE_UNIT, '(a)' )  !put an empty line
            ENDDO
            WRITE ( THIS%FILE_UNIT, '(a)' ) 'EOD'  !end of datablock
        ELSE !only Z has been sent (i.e. single matrix data)
            DO J=1,NCX
                DO I=1, NRX
                    WRITE ( THIS%FILE_UNIT, FMT=* ) I, J, X(I,J)
                ENDDO
                WRITE( THIS%FILE_UNIT, '(a)' )  !put an empty line
            ENDDO
            WRITE ( THIS%FILE_UNIT, '(a)' ) 'EOD'  !end of datablock
        END IF


        ! create the contour lines
        WRITE ( THIS%FILE_UNIT, '(a)' ) ! empty line
        WRITE ( THIS%FILE_UNIT, '(a)' ) '# create the contour'
        WRITE ( THIS%FILE_UNIT, '(a)' ) 'set contour base'
        WRITE ( THIS%FILE_UNIT, '(a)' ) 'set cntrparam levels 14'
        WRITE ( THIS%FILE_UNIT, '(a)' ) 'unset surface'
        WRITE ( THIS%FILE_UNIT, '(a)' ) 'set view map'


        !write the color palette into gnuplot script file
        IF (PRESENT(PALETTE)) THEN
            WRITE ( THIS%FILE_UNIT, '(a)' )  COLOR_PALETTES(PALETTE)
            WRITE ( THIS%FILE_UNIT, '(a)' )  'set pm3d' ! a conflict with lspec
        END IF


        WRITE ( THIS%FILE_UNIT, '(a)' ) ! empty line

        IF ( PRESENT(LSPEC) ) THEN
            IF (HASTITLE(LSPEC)) THEN
                PLTSTRING='splot ' // DATABLOCK // ' ' // TRIM(LSPEC)
            ELSE
                PLTSTRING='splot ' // DATABLOCK // ' notitle '//TRIM(LSPEC)
            END IF
        ELSE
            PLTSTRING='splot ' // DATABLOCK // ' notitle '
        END IF

        WRITE ( THIS%FILE_UNIT, '(a)' ) TRIM(PLTSTRING)

        !> Rev 0.20
        ! if there is no animation finalize
        IF (.NOT. (THIS%HASANIMATION)) THEN
            CALL FINALIZE_PLOT(THIS)
        ELSE
            WRITE(THIS%FILE_UNIT, '(a, I2)') 'pause ', THIS%PAUSE_SECONDS
        END IF

        !: End of cplot
    END SUBROUTINE CPLOT

     SUBROUTINE LPLOT3D(THIS, X, Y, Z, LSPEC, PALETTE)
         !..............................................................................
         ! lplot3d create a line plot in 3d
         ! datablock is used instead of  gnuplot inline file "-"
         !..............................................................................

         CLASS(GPF):: THIS
         ! Input vector
         REAL(WP),  INTENT(IN)            :: X(:)
         REAL(WP),  INTENT(IN), OPTIONAL  :: Y(:)
         REAL(WP),  INTENT(IN), OPTIONAL  :: Z(:)
         CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  LSPEC
         CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  PALETTE

         !   Local variables
         !----------------------------------------------------------------------
         INTEGER:: NCX
         INTEGER:: NRX
         INTEGER:: I
         INTEGER:: J
         LOGICAL:: XYZ_DATA
         CHARACTER(LEN=80)::  PLTSTRING
         CHARACTER(LEN=*), PARAMETER ::  DATABLOCK = '$xyz'

         PLTSTRING=''
         !   Check the input data
         NRX=SIZE(X)
         IF (PRESENT(Y) .AND. PRESENT(Z)) THEN
             XYZ_DATA=.TRUE.
         ELSEIF (PRESENT(Y)) THEN
             PRINT*, "gpf error: Z matrix was not sent to 3D plot routine"
             RETURN
         ELSE
             XYZ_DATA=.FALSE.
         END IF

         ! set default line style for 3D plot, can be overwritten
         THIS%TXTDATASTYLE = 'lines'
         ! create the script file for writing gnuplot commands and data
         CALL CREATE_OUTPUTFILE(THIS)

         ! Write titles and other annotations
         CALL PROCESSCMD(THIS)

         ! Write xy data into file
         WRITE ( THIS%FILE_UNIT, '(a)' ) '#data x y z'
         ! Rev 0.20
         ! write the $xyz datablocks
         WRITE( THIS%FILE_UNIT, '(a)' )  DATABLOCK // ' << EOD'
         IF (XYZ_DATA) THEN
             DO I=1, NRX
                 WRITE ( THIS%FILE_UNIT, * ) X(I), Y(I), Z(I)
             ENDDO
             WRITE( THIS%FILE_UNIT, '(a)' )  !put an empty line
             WRITE ( THIS%FILE_UNIT, '(a)' ) 'EOD'  !end of datablock
         ELSE !only Z has been sent (i.e. single matrix data)
             DO I=1, NRX
                 WRITE ( THIS%FILE_UNIT, * ) I, X(I)
             ENDDO
             WRITE( THIS%FILE_UNIT, '(a)' )  !put an empty line
             WRITE ( THIS%FILE_UNIT, '(a)' ) 'EOD'  !end of datablock
         END IF


         !write the color palette into gnuplot script file
         IF (PRESENT(PALETTE)) THEN
             WRITE ( THIS%FILE_UNIT, '(a)' )  COLOR_PALETTES(PALETTE)
             WRITE ( THIS%FILE_UNIT, '(a)' )  'set pm3d' ! a conflict with lspec
         END IF


         IF ( PRESENT(LSPEC) ) THEN
             IF (HASTITLE(LSPEC)) THEN
                 PLTSTRING='splot ' // DATABLOCK // ' ' // TRIM(LSPEC) // 'with lines'
             ELSE
                 PLTSTRING='splot ' // DATABLOCK // ' notitle '//TRIM(LSPEC) // 'with lines'
             END IF
         ELSE
             PLTSTRING='splot ' // DATABLOCK // ' notitle with lines'
         END IF

         WRITE ( THIS%FILE_UNIT, '(a)' ) TRIM(PLTSTRING)


         !> Rev 0.2: animation
         ! if there is no animation finalize
         IF (.NOT. (THIS%HASANIMATION)) THEN
             CALL FINALIZE_PLOT(THIS)
         ELSE
             WRITE(THIS%FILE_UNIT, '(a, I2)') 'pause ', THIS%PAUSE_SECONDS
         END IF

         !: End of lplot3d
     END SUBROUTINE LPLOT3D

    SUBROUTINE FUNCTION_PLOT(THIS, FUNC,XRANGE,NP)
        !..............................................................................
        ! fplot, plot a function in the range xrange=[xmin, xamx] with np points
        ! if np is not sent, then np=50 is assumed!
        ! func is the name of function to be plotted
        !..............................................................................

        CLASS(GPF):: THIS
        INTERFACE
            FUNCTION FUNC(X)
                IMPORT :: WP
                REAL(WP), INTENT(IN) :: X
                REAL(WP) :: FUNC
            END FUNCTION FUNC
        END INTERFACE
        REAL(WP), INTENT(IN) :: XRANGE(2)
        INTEGER, OPTIONAL, INTENT(IN):: NP

        INTEGER:: N
        INTEGER:: I
        INTEGER:: ALLOC_ERR
        REAL(WP), ALLOCATABLE :: X(:)
        REAL(WP), ALLOCATABLE :: Y(:)

        IF (PRESENT(NP)) THEN
            N=NP
        ELSE
            N=50
        END IF
        ALLOCATE(X(1:N), Y(1:N), STAT=ALLOC_ERR)
        IF (ALLOC_ERR /=0) THEN
            STOP "Allocation error in fplot procedure..."
        END IF
        !Create set of xy data
        X=LINSPACE(XRANGE(1),XRANGE(2), N)
        Y=[ (FUNC(X(I)), I=1, N) ]

        CALL PLOT2D_VECTOR_VS_VECTOR(THIS,X,Y)

        ! cleanup memory
        IF (ALLOCATED(X)) DEALLOCATE(X)
        IF (ALLOCATED(Y)) DEALLOCATE(Y)


    END SUBROUTINE FUNCTION_PLOT


    SUBROUTINE SEMILOGXV(THIS, X1, Y1, LS1, AXES1, &
            X2, Y2, LS2, AXES2, &
            X3, Y3, LS3, AXES3, &
            X4, Y4, LS4, AXES4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic x1 and x2 axes
        !..............................................................................

        CLASS(GPF):: THIS
        ! Input vector
        REAL(WP),  INTENT(IN)                          :: X1(:)  ! vector of data for x
        REAL(WP),  INTENT(IN), OPTIONAL                :: Y1(:)  ! vector of data for y
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS1    ! line specification
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES1

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X2
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y2
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS2
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES2

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X3
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y3
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS3
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES3

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X4
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y4
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS4
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES4
        THIS%PLOTSCALE='semilogx'
        CALL PLOT2D_VECTOR_VS_VECTOR(THIS, &
            X1, Y1, LS1, AXES1, &
            X2, Y2, LS2, AXES2, &
            X3, Y3, LS3, AXES3, &
            X4, Y4, LS4, AXES4  )
        ! Set the plot scale as linear. It means log scale is off
        THIS%PLOTSCALE='linear'

    END SUBROUTINE SEMILOGXV


    !..............................................................................
    SUBROUTINE SEMILOGYV(THIS, X1, Y1, LS1, AXES1, &
            X2, Y2, LS2, AXES2, &
            X3, Y3, LS3, AXES3, &
            X4, Y4, LS4,AXES4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic y1 and y2 axes
        !..............................................................................

        CLASS(GPF):: THIS
        ! Input vector
        REAL(WP),  INTENT(IN)                          :: X1(:)  ! vector of data for x
        REAL(WP),  INTENT(IN), OPTIONAL                :: Y1(:)  ! vector of data for y
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS1    ! line specification
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES1

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X2
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y2
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS2
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES2

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X3
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y3
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS3
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES3

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X4
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y4
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS4
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES4

        THIS%PLOTSCALE='semilogy'
        CALL PLOT2D_VECTOR_VS_VECTOR(THIS, &
            X1, Y1, LS1, AXES1, &
            X2, Y2, LS2, AXES2, &
            X3, Y3, LS3, AXES3, &
            X4, Y4, LS4, AXES4  )
        ! Set the plot scale as linear. It means log scale is off
        THIS%PLOTSCALE='linear'


    END SUBROUTINE SEMILOGYV



    SUBROUTINE LOGLOGV(THIS, X1, Y1, LS1, AXES1, &
            X2, Y2, LS2, AXES2, &
            X3, Y3, LS3, AXES3, &
            X4, Y4, LS4, AXES4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic x1, y1, x2, y2 axes
        !..............................................................................

        CLASS(GPF):: THIS
        ! Input vector
        REAL(WP),  INTENT(IN)                          :: X1(:)  ! vector of data for x
        REAL(WP),  INTENT(IN), OPTIONAL                :: Y1(:)  ! vector of data for y
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS1    ! line specification
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES1

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X2
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y2
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS2
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES2

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X3
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y3
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS3
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES3

        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: X4
        REAL(WP),  INTENT(IN), DIMENSION(:), OPTIONAL  :: Y4
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: LS4
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL        :: AXES4


        THIS%PLOTSCALE='loglog'
        CALL PLOT2D_VECTOR_VS_VECTOR(THIS, &
            X1, Y1, LS1, AXES1, &
            X2, Y2, LS2, AXES2, &
            X3, Y3, LS3, AXES3, &
            X4, Y4, LS4, AXES4  )
        ! Set the plot scale as linear. It means log scale is off
        THIS%PLOTSCALE='linear'

    END SUBROUTINE LOGLOGV



    SUBROUTINE  SEMILOGXM(THIS, XV, YMAT, LSPEC)
        !..............................................................................
        !Plots a matrix against a vector with logarithmic x-axis
        !For more information see plot2D_matrix_vs_vector procedure
        !Everything is the same except the x-axis scale
        !..............................................................................

        IMPLICIT NONE
        CLASS(GPF)                                :: THIS
        ! Input arrays
        REAL(WP),  INTENT(IN)                     :: XV(:)
        REAL(WP),  INTENT(IN)                     :: YMAT(:,:)
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   :: LSPEC

        THIS%PLOTSCALE='semilogx'
        CALL PLOT2D_MATRIX_VS_VECTOR(THIS, XV,YMAT, LSPEC)
        ! Set the plot scale as linear. It means log scale is off
        THIS%PLOTSCALE='linear'


    END SUBROUTINE SEMILOGXM



    SUBROUTINE  SEMILOGYM(THIS, XV,YMAT, LSPEC)
        !..............................................................................
        !Plots a matrix against a vector with logarithmic y-axis
        !For more information see plot2D_matrix_vs_vector procedure
        !Everything is the same except the x-axis scale
        !..............................................................................

        IMPLICIT NONE
        CLASS(GPF)                                  :: THIS
        ! Input arrays
        REAL(WP),  INTENT(IN)                       :: XV(:)
        REAL(WP),  INTENT(IN)                       :: YMAT(:,:)
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL     :: LSPEC

        THIS%PLOTSCALE='semilogy'
        CALL PLOT2D_MATRIX_VS_VECTOR(THIS, XV,YMAT, LSPEC)
        ! Set the plot scale as linear. It means log scale is off
        THIS%PLOTSCALE='linear'


    END SUBROUTINE SEMILOGYM


    SUBROUTINE  LOGLOGM(THIS, XV,YMAT, LSPEC)
        !..............................................................................
        !Plots a matrix against a vector with logarithmic x-axis and y-axis
        !For more information see plot2D_matrix_vs_vector procedure
        !Everything is the same except the axes scale
        !..............................................................................

        IMPLICIT NONE
        CLASS(GPF)                                :: THIS
        ! Input arrays
        REAL(WP),  INTENT(IN)                     :: XV(:)
        REAL(WP),  INTENT(IN)                     :: YMAT(:,:)
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   :: LSPEC

        THIS%PLOTSCALE='loglog'
        CALL PLOT2D_MATRIX_VS_VECTOR(THIS, XV,YMAT, LSPEC)
        ! Set the plot scale as linear. It means log scale is off
        THIS%PLOTSCALE='linear'


    END SUBROUTINE LOGLOGM



    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Three: Animation Routines
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    SUBROUTINE SUB_ANIMATION_START(THIS, PAUSE_SECONDS)
        !-------------------------------------------------------------------------------
        ! sub_animation_start: set the setting to start an animation
        ! it simply set flags and open a script file to write data
        !-------------------------------------------------------------------------------
        CLASS(GPF)                    :: THIS
        INTEGER, INTENT(IN), OPTIONAL :: PAUSE_SECONDS


        ! ogpf does not support multiplot with animation at the same time
        IF (THIS%HASMULTIPLOT) THEN
            PRINT*, MD_NAME // ': does not support animation in multiplot mode!'
            STOP
        END IF


        IF (PRESENT(PAUSE_SECONDS)) THEN
            THIS%PAUSE_SECONDS = PAUSE_SECONDS
        ELSE
            THIS%PAUSE_SECONDS = 2  ! delay in second
        END IF

        THIS%FRAME_NUMBER = 0

        ! create the ouput file for writting gnuplot script
        CALL CREATE_OUTPUTFILE(THIS)
        THIS%HASFILEOPEN  = .TRUE.
        THIS%HASANIMATION = .TRUE.

    END SUBROUTINE SUB_ANIMATION_START


    SUBROUTINE SUB_ANIMATION_SHOW(THIS)
        !-------------------------------------------------------------------------------
        ! sub_animation_show: simply resets the animation flags
        ! and finalize the plotting.
        !-------------------------------------------------------------------------------

        CLASS(GPF) :: THIS

        THIS%FRAME_NUMBER = 0
        THIS%HASANIMATION = .FALSE.

        CALL FINALIZE_PLOT(THIS)

    END SUBROUTINE SUB_ANIMATION_SHOW




    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Four: Gnuplot direct scriptting
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    SUBROUTINE ADDSCRIPT(THIS,STRCMD)
        !..............................................................................
        ! addscript: accepts all type of gnuplot command as a string and store it
        ! in global txtscript to be later sent to gnuplot
        !..............................................................................

        CLASS(GPF)                   :: THIS
        CHARACTER(LEN=*), INTENT(IN) :: STRCMD

        IF (.NOT.ALLOCATED(THIS%TXTSCRIPT)) THIS%TXTSCRIPT=''
        IF (LEN_TRIM(THIS%TXTSCRIPT) == 0 ) THEN
            THIS%TXTSCRIPT = '' ! initialize string
        END IF
        IF ( LEN_TRIM(STRCMD)>0 ) THEN
            THIS%TXTSCRIPT = THIS%TXTSCRIPT // SPLITSTR(STRCMD)
        END IF

    END SUBROUTINE ADDSCRIPT



    SUBROUTINE RUNSCRIPT(THIS)
        !..............................................................................
        ! runscript sends the the script string (txtstring) into a script
        ! file to be run by gnuplot
        !..............................................................................

        CLASS(GPF):: THIS

        !REV 0.18: a dedicated subroutine is used to create the output file
        CALL CREATE_OUTPUTFILE(THIS)

        !write the script
        CALL PROCESSCMD(THIS)
        WRITE(UNIT=THIS%FILE_UNIT, FMT='(a)') THIS%TXTSCRIPT

        ! close the file and call gnuplot
        CALL FINALIZE_PLOT(THIS)

    END SUBROUTINE RUNSCRIPT



    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Five: gnuplot command processing and data writing to script file
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE PROCESS_AXES_SET(AXES_SET, AXES)
        !..............................................................................
        ! process_axesspec accepts the axes set and interpret it into
        ! a format to be sent to gnuplot.
        ! the axes set can be one of the following set
        ! x1y1, x1y2, x2y1, x2y2
        !..............................................................................

        CHARACTER(LEN=*), INTENT(IN)  :: AXES_SET
        CHARACTER(LEN=4), INTENT(OUT) :: AXES


        IF (LEN_TRIM (ADJUSTL(AXES_SET)) == 0) THEN
            AXES=''
            RETURN
        END IF

        SELECT CASE ( LCASE(TRIM (ADJUSTL (AXES_SET) ) ) )
            CASE ('x1y1')
                AXES='x1y1'
            CASE ('x1y2')
                AXES='x1y2'
            CASE ('x2y1')
                AXES='x2y1'
            CASE ('x2y2')
                AXES='x2y2'
            CASE DEFAULT ! wrong strings
                PRINT*, MD_NAME // ':process_axes_set:' // ' wrong axes set is sent.'// NEW_LINE(' ') &
                    // 'axes set can be on of: x1y1, x1y2, x2y1, x2y2'
                AXES=''
                RETURN
        END SELECT

    END SUBROUTINE PROCESS_AXES_SET



    SUBROUTINE PROCESS_LINESPEC(ORDER, LSSTRING, LSPEC, AXES_SET)
        !..............................................................................
        ! process_linespec accepts the line specification and interpret it into
        ! a format to be sent to gnuplot
        !..............................................................................

        INTEGER, INTENT(IN) :: ORDER !1 for the first data series
        CHARACTER(LEN=*), INTENT(OUT) :: LSSTRING
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: LSPEC
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: AXES_SET

        !local variables
        CHARACTER(LEN=4)  :: AXES
        CHARACTER(LEN=10) :: AXES_SETTING

        !check the axes set
        AXES_SETTING = ''
        IF ( PRESENT (AXES_SET)) THEN
            CALL PROCESS_AXES_SET(AXES_SET, AXES)
            IF (LEN(TRIM(AXES))> 0 ) THEN
                AXES_SETTING = ' axes ' // AXES
            END IF
        END IF

        SELECT CASE(ORDER)
            CASE(1)
                IF ( PRESENT(LSPEC) ) THEN
                    IF (HASTITLE(LSPEC)) THEN
                        LSSTRING='plot "-" '//TRIM(LSPEC) // AXES_SETTING
                    ELSE
                        LSSTRING='plot "-" notitle '//TRIM(LSPEC) // AXES_SETTING
                    END IF
                ELSE
                    LSSTRING='plot "-" notitle' // AXES_SETTING
                END IF
            CASE  DEFAULT !e.g. 2, 3, 4, ...
                IF (PRESENT(LSPEC)) THEN
                    IF (HASTITLE(LSPEC)) THEN
                        LSSTRING=', "-" '// TRIM(LSPEC) // AXES_SETTING
                    ELSE
                        LSSTRING=', "-" notitle '// TRIM(LSPEC) // AXES_SETTING
                    END IF
                ELSE
                    LSSTRING=', "-" notitle' // AXES_SETTING
                END IF
        END SELECT
    END SUBROUTINE PROCESS_LINESPEC



    SUBROUTINE PROCESSCMD(THIS)
        !..............................................................................
        !   This subroutine writes all the data into plot file
        !   to be read by gnuplot
        !..............................................................................

        CLASS(GPF) :: THIS

        ! write the plot style for data
        ! this is used only when 3D plots (splot, cplot) is used
        IF (ALLOCATED(THIS%TXTDATASTYLE)) THEN
            WRITE ( THIS%FILE_UNIT, '("set style data ", a)' ) THIS%TXTDATASTYLE
            WRITE ( THIS%FILE_UNIT, '(a)' )
        END IF


        ! Write options
        IF ( THIS%HASOPTIONS ) THEN
            WRITE ( THIS%FILE_UNIT, '(" ")' )
            WRITE ( THIS%FILE_UNIT, '("# options")' )
            WRITE ( THIS%FILE_UNIT, '(a)' ) THIS%TXTOPTIONS
            WRITE ( THIS%FILE_UNIT, '(a)' )
        END IF

        ! Check with plot scale: i.e linear, logx, logy, or log xy
        WRITE( THIS%FILE_UNIT, '(" ")' )
        WRITE( THIS%FILE_UNIT, '("# plot scale")' )
        SELECT CASE (THIS%PLOTSCALE)
            CASE ('semilogx')
                WRITE ( THIS%FILE_UNIT, '("set logscale  x")' )
            CASE ('semilogy')
                WRITE ( THIS%FILE_UNIT, '("set logscale  y")' )
            CASE ('loglog')
                WRITE ( THIS%FILE_UNIT, '("set logscale  xy")' )
            CASE DEFAULT !for no setting
                !pass
        END SELECT

        !!>0.22
        ! write annotation
        WRITE ( THIS%FILE_UNIT, '(" ")' )
        WRITE ( THIS%FILE_UNIT, '("# Annotation: title and labels")' )
        CALL WRITE_LABEL(THIS, 'plot_title')
        CALL WRITE_LABEL(THIS, 'xlabel'    )
        CALL WRITE_LABEL(THIS, 'x2label'   )
        CALL WRITE_LABEL(THIS, 'ylabel'    )
        CALL WRITE_LABEL(THIS, 'y2label'   )
        CALL WRITE_LABEL(THIS, 'zlabel'    )

        ! axes range
        WRITE ( THIS%FILE_UNIT, '(" ")')
        WRITE ( THIS%FILE_UNIT, '("# axes setting")')
        IF (THIS%HASXRANGE) THEN
            WRITE ( THIS%FILE_UNIT, '("set xrange [",G0,":",G0,"]")' ) THIS%XRANGE
        END IF
        IF (THIS%HASYRANGE) THEN
            WRITE ( THIS%FILE_UNIT, '("set yrange [",G0,":",G0,"]")' ) THIS%YRANGE
        END IF
        IF (THIS%HASZRANGE) THEN
            WRITE ( THIS%FILE_UNIT, '("set zrange [",G0,":",G0,"]")' ) THIS%ZRANGE
        END IF

        ! secondary axes range
        IF (THIS%HASX2RANGE) THEN
            WRITE ( THIS%FILE_UNIT, '("set x2range [",G0,":",G0,"]")' ) THIS%X2RANGE
        END IF
        IF (THIS%HASY2RANGE) THEN
            WRITE ( THIS%FILE_UNIT, '("set y2range [",G0,":",G0,"]")' ) THIS%Y2RANGE
        END IF
        ! finish by new line
        WRITE ( THIS%FILE_UNIT, '(a)' ) ! emptyline

    END SUBROUTINE PROCESSCMD



    SUBROUTINE WRITE_LABEL(THIS, LBLNAME)
        !..............................................................................
        !   This subroutine writes the labels into plot file
        !   to be read by gnuplot
        !..............................................................................


        ! write_label
        CLASS(GPF)                    :: THIS
        CHARACTER(LEN=*)              :: LBLNAME

        ! local var
        CHARACTER(LEN=:), ALLOCATABLE :: LBLSTRING
        CHARACTER(LEN=:), ALLOCATABLE :: LBLSET
        TYPE(TPLABEL)                 :: LABEL

        SELECT CASE (LBLNAME)
            CASE ('xlabel')
                IF (.NOT. (THIS%TPXLABEL%HAS_LABEL) ) THEN
                    RETURN ! there is no label
                END IF
                LBLSET = 'set xlabel "'
                LABEL = THIS%TPXLABEL
            CASE ('x2label')
                IF (.NOT. (THIS%TPX2LABEL%HAS_LABEL) ) THEN
                    RETURN ! there is no label
                END IF
                LBLSET = 'set x2label "'
                LABEL = THIS%TPX2LABEL
            CASE ('ylabel')
                IF (.NOT. (THIS%TPYLABEL%HAS_LABEL) ) THEN
                    RETURN ! there is no label
                END IF
                LBLSET = 'set ylabel "'
                LABEL = THIS%TPYLABEL
            CASE ('y2label')
                IF (.NOT. (THIS%TPY2LABEL%HAS_LABEL) ) THEN
                    RETURN ! there is no label
                END IF
                LBLSET = 'set y2label "'
                LABEL = THIS%TPY2LABEL
            CASE ('zlabel')
                IF (.NOT. (THIS%TPZLABEL%HAS_LABEL) ) THEN
                    RETURN ! there is no label
                END IF
                LBLSET = 'set zlabel "'
                LABEL = THIS%TPZLABEL
            CASE ('plot_title')
                IF (.NOT. (THIS%TPPLOTTITLE%HAS_LABEL) ) THEN
                    RETURN ! there is no label
                END IF
                LBLSET = 'set title "'
                LABEL = THIS%TPPLOTTITLE
        END SELECT

        LBLSTRING = ''
        ! if there is a label continue to set it
        LBLSTRING                  = LBLSTRING // LBLSET // TRIM(LABEL%LBLTEXT)//'"'
        IF (ALLOCATED(LABEL%LBLCOLOR)) THEN
            LBLSTRING              = LBLSTRING // ' tc "' //TRIM(LABEL%LBLCOLOR) // '"'
        END IF
        ! set font and size
        IF (ALLOCATED(THIS%TPXLABEL%LBLFONTNAME)) THEN
            LBLSTRING              = LBLSTRING // ' font "'// TRIM(LABEL%LBLFONTNAME) // ','
            IF (LABEL%LBLFONTSIZE /= NOT_INITIALIZED) THEN
                LBLSTRING          = LBLSTRING // NUM2STR(LABEL%LBLFONTSIZE) //'"'
            ELSE
                LBLSTRING          = LBLSTRING //'"'
            END IF
        ELSE ! check if only font size has been given
            IF (LABEL%LBLFONTSIZE /= NOT_INITIALIZED ) THEN
                LBLSTRING          = LBLSTRING // ' font ",' // NUM2STR(LABEL%LBLFONTSIZE) //'"'
            END IF
        END IF
        ! set rotation
        IF (LABEL%LBLROTATE       /= NOT_INITIALIZED ) THEN
            LBLSTRING              = LBLSTRING // ' rotate by ' // NUM2STR(LABEL%LBLROTATE )
        END IF


        ! write to ogpf script file
        WRITE ( THIS%FILE_UNIT, '(a)' ) LBLSTRING


    END SUBROUTINE WRITE_LABEL



    FUNCTION COLOR_PALETTES(PALETTE_NAME) RESULT(STR)
        !...............................................................................
        ! color_palettes create color palette as a
        ! string to be written into gnuplot script file
        ! the palettes credit goes to: Anna Schnider (https://github.com/aschn) and
        ! Hagen Wierstorf (https://github.com/hagenw)
        !...............................................................................
        CHARACTER(LEN=*), INTENT(IN)  :: PALETTE_NAME
        CHARACTER(LEN=:), ALLOCATABLE :: STR

        ! local variables
        CHARACTER(LEN=1)              :: STRNUMBER
        CHARACTER(LEN=11)             :: STRBLANK
        INTEGER                       :: J
        INTEGER                       :: MAXCOLORS

        ! define the color palettes
        CHARACTER(LEN=:), ALLOCATABLE :: PLTNAME
        CHARACTER(LEN=7)              :: PALETTE(9) ! palettes with maximum 9 colors

        MAXCOLORS = 8 ! default number of discrete colors
        PALETTE=''
        SELECT CASE ( LCASE(TRIM(ADJUSTL(PALETTE_NAME))) )
            CASE ('set1')
                PLTNAME='set1'
                PALETTE(1:MAXCOLORS)=[&
                    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", &
                    "#FF7F00", "#FFFF33", "#A65628", "#F781BF" ]
            CASE ('set2')
                PLTNAME='set2'
                PALETTE(1:MAXCOLORS)=[&
                    "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", &
                    "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3" ]
            CASE ('set3')
                PLTNAME='set3'
                PALETTE(1:MAXCOLORS)=[&
                    "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", &
                    "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5" ]
            CASE ('palette1')
                PLTNAME='palette1'
                PALETTE(1:MAXCOLORS)=[&
                    "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", &
                    "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC" ]
            CASE ('palette2')
                PLTNAME='palette2'
                PALETTE(1:MAXCOLORS)=[&
                    "#B3E2CD", "#FDCDAC", "#CDB5E8", "#F4CAE4", &
                    "#D6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC" ]
            CASE ('paired')
                PLTNAME='paired'
                PALETTE(1:MAXCOLORS)=[&
                    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", &
                    "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00" ]
            CASE ('dark2')
                PLTNAME='dark2'
                PALETTE(1:MAXCOLORS)=[&
                    "#1B9E77", "#D95F02", "#7570B3", "#E7298A", &
                    "#66A61E", "#E6AB02", "#A6761D", "#666666" ]
            CASE ('accent')
                PLTNAME='accent'
                PALETTE(1:MAXCOLORS)=[&
                    "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", &
                    "#386CB0", "#F0027F", "#BF5B17", "#666666" ]
            CASE ('jet')
                ! Matlab jet palette
                MAXCOLORS = 9
                PLTNAME='jet'
                PALETTE(1:MAXCOLORS)=[&
                    '#000090', '#000fff', '#0090ff', '#0fffee', &
                    '#90ff70', '#ffee00', '#ff7000', '#ee0000', '#7f0000' ]
            CASE DEFAULT
                PRINT*, MD_NAME // ": color_palettes: wrong palette name"
                PRINT*, 'gnuplot default palette will be used!'
                STR=' ' ! empty palette is returned!
                RETURN
        END SELECT

        ! generate the gnuplot palette as a single multiline string
        STR                 = '# Define the ' // PLTNAME // ' pallete' // NEW_LINE(' ')
        STR                 = STR // 'set palette defined ( \' // NEW_LINE(' ')
        STRBLANK            = '           ' ! pad certain number of paces
        DO J=1, MAXCOLORS - 1
            WRITE(UNIT      =STRNUMBER, FMT='(I1)' ) J-1
            STR             = STR // STRBLANK // STRNUMBER // ' "' // PALETTE(J) // '",\' // NEW_LINE(' ')
        END DO

        J                   =MAXCOLORS
        WRITE(STRNUMBER, FMT='(I1)') J
        STR                 = STR // STRBLANK // STRNUMBER // ' "' // PALETTE(J) // '" )' // NEW_LINE(' ')

    END FUNCTION COLOR_PALETTES



    SUBROUTINE WRITE_XYDATA(FILE_UNIT,NDATA,X,Y)
        !..............................................................................
        ! Writes set of xy data into a file
        !..............................................................................

        INTEGER,  INTENT(IN)           ::  FILE_UNIT
        INTEGER,  INTENT(IN)           ::  NDATA
        REAL(WP), INTENT(IN)           ::  X(:)
        REAL(WP), INTENT(IN), OPTIONAL ::  Y(:)

        INTEGER:: I

        ! TODO (Mohammad#1#12/22/17): The format string shall be modified to write the
        ! number in more suitable form
        ! Rev 0.18
        IF (PRESENT(Y) ) THEN !both x and y are present, data are xy set
            DO I = 1, NDATA
                WRITE ( FILE_UNIT, * ) X(I), Y(I)
            END DO
        ELSE !only x is passed, data are index-x set
            DO I = 1, NDATA
                WRITE ( FILE_UNIT, * ) X(I)
            END DO
        END IF
        WRITE ( FILE_UNIT, '(a)' ) 'e'  !end of set of data

    END SUBROUTINE WRITE_XYDATA



    SUBROUTINE CREATE_OUTPUTFILE(THIS)
        !..............................................................................
        ! Create an output file, assign a file_unit
        ! for writing the gnuplot commands
        !..............................................................................

        ! Rev 0.18
        CLASS(GPF), INTENT(INOUT)   :: THIS

        IF (THIS%HASFILEOPEN) THEN
            ! there is nothing to do, file has been already open!
            RETURN
        END IF

        !> Rev 0.2 animation

        ! animation handling
        IF (THIS%HASANIMATION ) THEN
            THIS%FRAME_NUMBER = THIS%FRAME_NUMBER + 1 ! for future use
        END IF

        ! Open the output file

        IF (.NOT. (THIS%HASFILENAME)) THEN ! check if no file has been set by user
            THIS%TXTFILENAME=GNUPLOT_OUTPUT_FILENAME
        END IF

        OPEN ( NEWUNIT = THIS%FILE_UNIT, FILE = THIS%TXTFILENAME, STATUS = 'replace', IOSTAT = THIS%STATUS )


        IF (THIS%STATUS /= 0 ) THEN
            PRINT*, "md_helperproc, create_outputfile: cannot open file for output"
            STOP
        END IF


        ! Set the gnuplot terminal, write ogpf configuration (customized setting)
        ! Can be overwritten by options

        ! write signature
        WRITE ( THIS%FILE_UNIT, '(a)' ) '# ' // MD_NAME
        WRITE ( THIS%FILE_UNIT, '(a)' ) '# ' // MD_REV
        WRITE ( THIS%FILE_UNIT, '(a)' ) '# ' // MD_LIC
        WRITE ( THIS%FILE_UNIT, '(a)' )  ! emptyline

        ! write the global settings
        WRITE ( THIS%FILE_UNIT, '(a)' ) '# gnuplot global setting'
        WRITE(UNIT=THIS%FILE_UNIT, FMT='(a)') 'set term ' // GNUPLOT_TERM_TYPE // &
            ' size ' // GNUPLOT_TERM_SIZE // ' enhanced font "' // &
            GNUPLOT_TERM_FONT // '"' // &
            ' title "' // MD_NAME // ': ' // MD_REV //'"'   ! library name and version

        ! write the preset configuration for gnuplot (ogpf customized settings)
        IF (THIS%PRESET_CONFIGURATION) THEN
            CALL THIS%PRESET_GNUPLOT_CONFIG()
        END IF
        ! write multiplot setting
        IF (THIS%HASMULTIPLOT) THEN
            WRITE(THIS%FILE_UNIT, FMT='(a, I2, a, I2)') 'set multiplot layout ', &
                THIS%MULTIPLOT_ROWS, ',',  THIS%MULTIPLOT_COLS
        END IF
        ! set flag true for file is opened
        THIS%HASFILEOPEN = .TRUE.

    END SUBROUTINE CREATE_OUTPUTFILE


    SUBROUTINE PRESET_GNUPLOT_CONFIG(THIS)
        !..............................................................................
        ! To write the preset configuration for gnuplot (ogpf customized settings)
        !..............................................................................
        CLASS(GPF) :: THIS

        WRITE(THIS%FILE_UNIT, FMT='(a)')
        WRITE(THIS%FILE_UNIT, FMT='(a)') '# ogpf extra configuration'
        WRITE(THIS%FILE_UNIT, FMT='(a)') '# -------------------------------------------'


        ! color definition
        WRITE(THIS%FILE_UNIT, FMT='(a)') '# color definitions'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set style line 1 lc rgb "#800000" lt 1 lw 2'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set style line 2 lc rgb "#ff0000" lt 1 lw 2'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set style line 3 lc rgb "#ff4500" lt 1 lw 2'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set style line 4 lc rgb "#ffa500" lt 1 lw 2'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set style line 5 lc rgb "#006400" lt 1 lw 2'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set style line 6 lc rgb "#0000ff" lt 1 lw 2'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set style line 7 lc rgb "#9400d3" lt 1 lw 2'
        WRITE(THIS%FILE_UNIT, FMT='(a)')
        ! axes setting
        WRITE(THIS%FILE_UNIT, FMT='(a)') '# Axes'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set border linewidth 1.15'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set tics nomirror'
        WRITE(THIS%FILE_UNIT, FMT='(a)')

        WRITE(THIS%FILE_UNIT, FMT='(a)') '# grid'
        WRITE(THIS%FILE_UNIT, FMT='(a)') '# Add light grid to plot'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set style line 102 lc rgb "#d6d7d9" lt 0 lw 1'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set grid back ls 102'
        WRITE(THIS%FILE_UNIT, FMT='(a)')
        ! set the plot style
        WRITE(THIS%FILE_UNIT, FMT='(a)') '# plot style'
        WRITE(THIS%FILE_UNIT, FMT='(a)') 'set style data linespoints'
        WRITE(THIS%FILE_UNIT, FMT='(a)')

        WRITE(THIS%FILE_UNIT, FMT='(a)') '# -------------------------------------------'
        WRITE(THIS%FILE_UNIT, FMT='(a)') ''


    END SUBROUTINE PRESET_GNUPLOT_CONFIG



    SUBROUTINE FINALIZE_PLOT(THIS)
        !..............................................................................
        ! To finalize the writing of gnuplot commands/data and close the output file.
        !..............................................................................
!        USE IFPORT
        CLASS(GPF) :: THIS
!        CHARACTER(LEN=128)  :: DirName
!        INTEGER             :: ISTAT

        ! check for multiplots
        IF (THIS%HASMULTIPLOT) THEN
            IF (THIS%MULTIPLOT_TOTAL_PLOTS < THIS%MULTIPLOT_ROWS * THIS%MULTIPLOT_COLS - 1 ) THEN
                ! increment the number of plots
                THIS%MULTIPLOT_TOTAL_PLOTS = THIS%MULTIPLOT_TOTAL_PLOTS + 1
                RETURN ! do not finalize plot, still there is places in multiplot
            ELSE
                ! close multiplot
                WRITE(THIS%FILE_UNIT, FMT='(a)') 'unset multiplot'
                ! reset multiplot flag
                THIS%HASMULTIPLOT = .FALSE.

            END IF
        END IF

        CLOSE ( UNIT = THIS%FILE_UNIT )   ! close the script file
        THIS%HASFILEOPEN = .FALSE.        ! reset file open flag
        THIS%HASANIMATION = .FALSE.
        ! Use shell command to run gnuplot
!        ISTAT = GETCWD (DirName)
!        CALL EXECUTE_COMMAND_LINE ('gnuplot --persist ' // TRIM(DirName) // '\' // THIS%TXTFILENAME)  !   Now plot the results
!        CALL EXECUTE_COMMAND_LINE ('gnuplot --persist ' // THIS%TXTFILENAME)  !   Now plot the results
!        CALL EXECUTE_COMMAND_LINE ('gnuplot ' // THIS%TXTFILENAME)  !   Now plot the results
        CALL EXECUTE_COMMAND_LINE ('gnuplot --persist ' // THIS%TXTFILENAME, WAIT=.FALSE.)  !   Now plot the results

    END SUBROUTINE FINALIZE_PLOT



    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Six: Utility and helper procedures
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    FUNCTION HASTITLE(STRING)
        !..............................................................................
        ! check to see if the plot title (used as legend = key)
        !..............................................................................

        CHARACTER(LEN=*), INTENT(IN) :: STRING
        LOGICAL:: HASTITLE
        INTEGER:: IDX1
        INTEGER:: IDX2

        IDX1=INDEX( LCASE(STRING),'title')     !Check if title is passed
        IDX2=INDEX(' ' // LCASE(STRING),' t ') !Check if the abbreviated title 't' is passed. Extra space is aDDED
        ! at the beginning of string to find starting 't'
        IF (IDX1 /=0 .OR. IDX2 /=0 ) THEN
            HASTITLE=.TRUE.
        ELSE
            HASTITLE=.FALSE.
        END IF

    END FUNCTION HASTITLE


    FUNCTION CHECKDIM(NX,NY)
        !..............................................................................
        ! checkdim checks the equality of dimensions of two vector
        !..............................................................................

        INTEGER, INTENT(IN):: NX
        INTEGER, INTENT(IN):: NY
        LOGICAL:: CHECKDIM
        IF (NX/=NY) THEN
            CHECKDIM=.FALSE.
        ELSE
            CHECKDIM=.TRUE.
        END IF

    END FUNCTION CHECKDIM



    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !> Section Seven: String utility Routines
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    PURE FUNCTION SPLITSTR(STR) RESULT(SPSTR)
        !..............................................................................
        !splitstr, separate a string using ";" delimiters
        !..............................................................................

        CHARACTER(LEN=*), INTENT(IN)    :: STR

        ! local variables
        CHARACTER, PARAMETER          :: DELIMITER=';'
        CHARACTER(LEN=:), ALLOCATABLE :: SPSTR
        INTEGER ::  N
        INTEGER ::  M
        INTEGER ::  K


        K=LEN_TRIM(STR) !length with removed trailing blanks
        N=SCAN(STR,DELIMITER)
        IF (N==0) THEN  ! This is a single statement
            SPSTR = ADJUSTL(STR) // NEW_LINE(' ')
            RETURN
        END IF

        ! for two or more statements separated by ;
        SPSTR = ''
        M=1
        DO WHILE (N/=0 .AND. M<K)
            IF (N/=1) THEN
                SPSTR = SPSTR // ADJUSTL(STR(M:M+N-2)) // NEW_LINE(' ')
            END IF
            M=N+M
            N=SCAN(STR(M:K),DELIMITER)
        END DO
        IF (M<K) THEN !write the last statement
            SPSTR = SPSTR// ADJUSTL(STR(M:K)) // NEW_LINE(' ')
        END IF
    END FUNCTION SPLITSTR



    SUBROUTINE SPLITSTRING2ARRAY(STRIN, STRARRAY, DELIMITER)
        !..............................................................................
        ! splitstring splits a string to an array of
        ! substrings based on a selected delimiter
        ! note:
        !    a. any facing space/blank in substrings will be removed
        !    b. two adjacent delimiter treats as an empty substring between them
        !    c. facing and trailing delimiter treats as empty substring at the fornt and end
        !..............................................................................

        CHARACTER(LEN=*),  INTENT(IN)               :: STRIN
        CHARACTER(LEN=80), ALLOCATABLE, INTENT(OUT) :: STRARRAY(:)
        CHARACTER(LEN=1),  OPTIONAL,    INTENT(IN)  :: DELIMITER

        ! local variables
        INTEGER   :: M, N
        INTEGER   :: I, IDX
        CHARACTER(LEN=LEN(STRIN)) :: STRTMP
        CHARACTER(LEN=1)  :: DELIMITER_

        ! 0. check the existance of delimiter
        IF (PRESENT(DELIMITER)) THEN
            DELIMITER_ = DELIMITER
        ELSE
            DELIMITER_ = ';'
        END IF

        ! 1. remove initial blanks if any
        STRTMP=TRIM (ADJUSTL(STRIN) )

        ! 2. count the number substrings separated by delimiter
        N = COUNT( [ (STRTMP(I:I), I=1, LEN_TRIM(STRTMP)) ] == DELIMITER_)

        ! 3. allocate the output string array
        ALLOCATE(STRARRAY(N+1))

        ! 4. extract substrings and store in array one by one
        M=1
        DO I=1, N
            IDX=INDEX(STRTMP(M:),DELIMITER_)
            STRARRAY(I) = ADJUSTL( STRTMP(M:M+IDX-2) )
            M = M + IDX
        END DO
        STRARRAY(N+1)=ADJUSTL(STRTMP(M:) )


    END SUBROUTINE SPLITSTRING2ARRAY


    FUNCTION LCASE(STRINPUT)
        !..............................................................................
        ! Return the string (strInput) in lowercase
        !..............................................................................

        CHARACTER(LEN=*), INTENT(IN) :: STRINPUT
        CHARACTER(LEN=LEN(STRINPUT)):: LCASE
        INTEGER:: I
        INTEGER:: N
        CHARACTER(1):: CHR

        DO I=1, LEN(STRINPUT)
            CHR=STRINPUT(I:I)
            N=ICHAR(CHR)
            IF (N >=65 .AND. N <= 90) THEN
                LCASE(I:I)=CHAR(N+32)
            ELSE
                LCASE(I:I)=CHR
            END IF
        END DO
    END FUNCTION LCASE


    FUNCTION NUM2STR_I4(NUMBER_IN)
        !..............................................................................
        ! num2str_int: converts integer number to string
        !..............................................................................

        INTEGER(KIND=KIND(1)), INTENT(IN)     :: NUMBER_IN
        CHARACTER(LEN=:), ALLOCATABLE   :: NUM2STR_I4

        ! local variable
        CHARACTER(LEN=RANGE(NUMBER_IN)) :: STRNM
        WRITE(UNIT=STRNM, FMT='(I0)') NUMBER_IN
        NUM2STR_I4 = TRIM(STRNM)

    END FUNCTION NUM2STR_I4

    FUNCTION NUM2STR_R4(NUMBER_IN, STRFMT)
        !..............................................................................
        ! num2str_r4: converts single precision real number to string
        ! strfmt is the optional format string
        !..............................................................................

        REAL(KIND=SP), INTENT(IN)                :: NUMBER_IN
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: STRFMT
        CHARACTER(LEN=:), ALLOCATABLE           :: NUM2STR_R4

        ! local variable
        CHARACTER(LEN=RANGE(NUMBER_IN)) :: STRNM


        IF (PRESENT(STRFMT)) THEN
            WRITE(UNIT=STRNM, FMT= '('//TRIM(STRFMT)//')' ) NUMBER_IN
        ELSE
            WRITE(UNIT=STRNM, FMT='(G0)') NUMBER_IN
        END IF

        NUM2STR_R4 = TRIM(STRNM)

    END FUNCTION NUM2STR_R4


    FUNCTION NUM2STR_R8(NUMBER_IN, STRFMT)
        !..............................................................................
        ! num2str_real: converts double precision real number to string
        ! strfmt is the optional format string
        !..............................................................................

        REAL(KIND=DP), INTENT(IN)                :: NUMBER_IN
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: STRFMT
        CHARACTER(LEN=:), ALLOCATABLE           :: NUM2STR_R8

        ! local variable
        CHARACTER(LEN=RANGE(NUMBER_IN)) :: STRNM

        IF (PRESENT(STRFMT)) THEN
            WRITE(UNIT=STRNM, FMT= '('//TRIM(STRFMT)//')' ) NUMBER_IN
        ELSE
            WRITE(UNIT=STRNM, FMT='(G0)') NUMBER_IN
        END IF

        NUM2STR_R8 = TRIM(STRNM)

    END FUNCTION NUM2STR_R8


    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Eight: Math helper function
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    FUNCTION ARANGE(XA, XB, DX)
        !..............................................................................
        !   returns a vector in the form of [xa, xa+dx, xa+2*dx, ...]
        !   the number of elements is calculated as m = n+ 1,
        !   where n= int ( (xa-xb)/dx) ).
        !   arange is similar to colon in Matlab and arange in Python!
        !
        !   NOTE:
        !    - If n calculated as zero, result is [xa]
        !    - If n calculated as Inf (dx=0), a fatal error will be raised
        !    - If n calculated as negative value (e.g xa<xb or dx<0.0), a
        !      fatal error will be raised
        !..............................................................................

        REAL(WP), INTENT(IN)            :: XA
        REAL(WP), INTENT(IN)            :: XB
        REAL(WP), INTENT(IN), OPTIONAL  :: DX
        REAL(WP), ALLOCATABLE           :: ARANGE(:)

        !   Local vars
        REAL(WP):: DXL
        INTEGER:: I
        INTEGER:: N
        INTEGER:: IERR

        ! check the presence of dx and its correctness
        IF (PRESENT(DX)) THEN
            DXL = DX
            IF ( ABS(DX) <= TINY(DX)) THEN
                PRINT*, "arange procedure: Fatal Error: wrong dx, use a dx > 0.0 "
                STOP
            END IF
        ELSE
            DXL = 1.0_WP
        END IF

        IF ( (XA < XB) .AND. (DX < 0.0_WP) ) THEN
            PRINT*, "arange procedure: Fatal Error: wrong dx, use a dx > 0.0 "
            STOP
        END IF

        N = INT( (XB-XA)/ DXL) ! n+1 is the number of elements

        ALLOCATE(ARANGE(N), STAT=IERR)

        IF (IERR /= 0) THEN
            PRINT*, "arange procedure: Fatal Error, allocation failed in arange function"
            STOP
        END IF

        ARANGE = [(XA + I*DXL, I=0, N)]

    END FUNCTION ARANGE


    FUNCTION LINSPACE(A,B,N_ELEMENTS)
        !..............................................................................
        !   returns a linearly spaced vector with n points in [a, b]
        !   if n is omitted, 100 points will be considered
        !..............................................................................

        REAL(WP), INTENT(IN)           :: A
        REAL(WP), INTENT(IN)           :: B
        INTEGER,  INTENT(IN), OPTIONAL :: N_ELEMENTS
        REAL(WP), ALLOCATABLE          :: LINSPACE(:)

        !   Local vars
        REAL(WP) :: DX
        INTEGER  :: I
        INTEGER  :: N
        INTEGER  :: IERR

        IF (PRESENT(N_ELEMENTS)) THEN
            IF (N_ELEMENTS <=1 ) THEN
                PRINT*, "linspace procedure: Error: wrong value of n_elements, use an n_elements > 1"
                STOP
            END IF
            N=N_ELEMENTS
        ELSE
            N=100
        END IF

        ALLOCATE(LINSPACE(N), STAT=IERR)
        IF (IERR /= 0) THEN
            PRINT*, "linspace procedure: Fatal Error, Allocation failed in linspace function"
            STOP
        END IF

        DX=(B-A)/REAL((N-1),WP)
        LINSPACE=[(I*DX+A, I=0,N-1)]

    END FUNCTION LINSPACE



    SUBROUTINE MESHGRID(X,Y,XGV,YGV, IERR)
        !..............................................................................
        !meshgrid generate mesh grid over a rectangular domain of [xmin xmax, ymin, ymax]
        ! Inputs:
        !     xgv, ygv are grid vectors in form of full grid data
        ! Outputs:
        !     X and Y are matrix each of size [ny by nx] contains the grid data.
        !     The coordinates of point (i,j) is [X(i,j), Y(i,j)]
        !     ierr: The error flag
        !     """
        !     # Example
        !     # call meshgrid(X, Y, [0.,1.,2.,3.],[5.,6.,7.,8.])
        !     # X
        !     # [0.0, 1.0, 2.0, 3.0,
        !     #  0.0, 1.0, 2.0, 3.0,
        !     #  0.0, 1.0, 2.0, 3.0,
        !     #  0.0, 1.0, 2.0, 3.0]
        !     #
        !     #Y
        !     #[ 5.0, 5.0, 5.0, 5.0,
        !     #  6.0, 6.0, 6.0, 6.0,
        !     #  7.0, 7.0, 7.0, 7.0,
        !     #  8.0, 8.0, 8.0, 8.0]
        !..............................................................................
        ! Rev 0.2, Feb 2018
        ! New feature added: xgv and ygv as full grid vector are accepted now

        ! Arguments
        REAL(WP), INTENT(OUT), ALLOCATABLE  :: X(:,:)
        REAL(WP), INTENT(OUT), ALLOCATABLE  :: Y(:,:)
        REAL(WP), INTENT(IN)                :: XGV(:) ! x grid vector [start, stop, step] or [start, stop]
        REAL(WP), INTENT(IN),  OPTIONAL     :: YGV(:) ! y grid vector [start, stop, step] or [start, stop]
        INTEGER,  INTENT(OUT), OPTIONAL     :: IERR   ! the error value

        ! Local variables
        INTEGER:: SV
        INTEGER:: NX
        INTEGER:: NY
        LOGICAL:: ONLY_XGV_AVAILABLE

        ! Initial setting
        ONLY_XGV_AVAILABLE  = .FALSE.
        SV=0 !Assume no error

        NX=SIZE(XGV, DIM=1)

        IF (PRESENT(YGV)) THEN
            NY = SIZE(YGV, DIM=1)
        ELSE
            ONLY_XGV_AVAILABLE=.TRUE.
            NY=NX
        END IF

        ALLOCATE(X(NY,NX),Y(NY,NX),STAT=SV)
        IF (SV /=0) THEN
            PRINT*, "allocataion erro in meshgrid"
            STOP
        END IF

        X(1,:)    = XGV
        X(2:NY,:) = SPREAD(XGV, DIM=1, NCOPIES=NY-1)

        IF (ONLY_XGV_AVAILABLE) THEN
            Y=TRANSPOSE(X)
        ELSE
            Y(:,1)    = YGV
            Y(:,2:NX) = SPREAD(YGV,DIM=2,NCOPIES=NX-1)
        END IF

        IF (PRESENT(IERR)) THEN
            IERR=SV
        END IF

    END SUBROUTINE MESHGRID


    !End of ogpf
END MODULE ModLib_Ogpf
