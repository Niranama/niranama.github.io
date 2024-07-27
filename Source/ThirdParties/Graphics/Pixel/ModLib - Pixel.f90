!>
!!##NAME
!!    M_pixel(3f) - [M_pixel::INTRO] module for drawing into a pixel array with 2D vector operations
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   Module procedures
!!
!!    use M_writegif, only : writegif
!!
!!    use :: M_pixel, only : drawchar,  rect,            rdraw2,     strlength
!!    use :: M_pixel, only : color,     mapcolor,        clear,      draw2
!!    use :: M_pixel, only : circle,    circleprecision, arc,        getviewport
!!    use :: M_pixel, only : viewport,  ortho2,          rmove2
!!    use :: M_pixel, only : line,      linewidth,       polyline2,  move2
!!    use :: M_pixel, only : move2,     draw2,           prefsize,   vinit
!!    use :: M_pixel, only : textang,   textsize,        drawstr,    getgp2
!!    use :: M_pixel, only : vflush,    page,            point2,     getdisplaysize
!!    use :: M_pixel, only : poly2,     centertext,      xcentertext, ycentertext
!!    use :: M_pixel, only : makepoly,  closepoly,       font
!!
!!    use :: M_pixel, only : state,     hershey,         justfy
!!    use :: M_pixel, only : print_ascii, print_ppm, print_p3, print_p6
!!    use :: M_pixel, only : pixel
!!    use :: M_pixel, only : hue
!!
!!    ! Differences between M_pixel and M_draw and M_draw-related procedures:
!!    !    hershey(3f) and justfy(3f) do not exist in M_draw and might be replaced
!!    !    and the same font names are not available
!!    !    print_ascii(3f) and print_ppm|p3|p6(3f) do not exist in M_draw
!!    !    state(3f) does not exist in M_draw
!!    !    viewport is in terms of pixels, not range -1.0 to 1.0
!!
!!   Module variables
!!
!!    use M_pixel, only : P_pixel, P_ColorMap, P_debug
!!
!!##DESCRIPTION
!!    M_pixel(3fm) is intended to produce simple pixel graphics composed of
!!    line drawings and polygon fills in two dimensions. It handles circles,
!!    curves, arcs, polygons, and software text. It is designed to provide a
!!    programming interface very similar to a subset of the VOGLE graphics
!!    library (M_pixel does not support objects, interactive graphics,
!!    or 3D vectors).
!!
!!    It is primarily intended to provide a simple Fortran-based set of
!!    routines that can generate simple graphics that can be written to a
!!    GIF file using the writegif(3f) routine.
!!
!!    This is a prototype under construction starting 2017-06, but is already
!!    useful. Improvements in line width, dashed lines, polygon fill and
!!    higher level graphing routines are being worked on. If anyone is
!!    interested in collaborating on the module, contact the author.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_pixel
!!    use M_pixel
!!    use M_writegif, only :  writegif
!!    use M_pixel,    only : cosd, sind
!!    implicit none
!!
!!    integer  :: i
!!    integer  :: j
!!    integer  :: icolor
!!
!!       ! initialize image
!!       call prefsize(400,400)  ! set size before starting
!!       call vinit()            ! start graphics
!!       call clear(0)           ! clear to color 0
!!
!!       ! put some colored boxes into pixmap by address
!!       ! so show how the pixel map can be edited easily with
!!       ! other routines that can manipulate a pixel array.
!!       ! The P_pixel array was created when vinit(3f) was called
!!       icolor=1
!!       do i=1,4
!!          do j=1,4
!!             P_pixel((i-1)*100+1+3:i*100-3,(j-1)*100+1+3:j*100-3)=icolor
!!             icolor=icolor+1
!!          enddo
!!       enddo
!!
!!       ! map area of virtual world to pixel array
!!       ! notice Y-axis for viewport is zero at TOP
!!          ! viewport(left, right, bottom, top)
!!       call viewport(0.0,  400.0,  400.0, 0.0)
!!       ! define the virtual world area we want to work in
!!           !ortho2(left, right, bottom,   top)
!!       call ortho2(0.0,  400.0,    0.0, 400.0)
!!       ! the drawing routines use these world units
!!
!!       ! draw polar grids
!!       call linewidth(100)
!!       call color(14)
!!       call target(200.0,200.0,200.0)
!!
!!       call linewidth(75)
!!       call color(0)
!!       call target(100.0,200.0,50.0)
!!
!!       ! draw some lines
!!       call color(1)
!!       call linewidth(200)
!!       call line(1.0,1.0,400.0,400.0)
!!
!!       call color(4)
!!       call line(350.0,200.0,350.0,300.0)
!!
!!       ! print some text
!!       call color(1)
!!       !call hershey(x,y,height,itext,theta,ntext)
!!       call linewidth(125)
!!       call hershey(40.0, 40.0,35.0,'Hello World',0.0,11)
!!       call color(7)
!!       call linewidth(25)
!!       call hershey(40.0, 80.0,35.0,'Hello World',0.0,11)
!!       call linewidth(100)
!!       call hershey(40.0,120.0,35.0,'Hello World',30.0,11)
!!
!!       call hershey( 40.0,350.0,35.0,'\COMPLEX\Hello World',0.0,20)
!!       call hershey( 40.0,310.0,35.0,'\DUPLEX\Hello World',0.0,19)
!!       call hershey( 350.0,400.0,35.0,'\ITALIC\Hello World',90.0,19)
!!       call linewidth(50)
!!       call hershey(200.0,120.0,15.0,'\SIMPLEX\Hello World',20.0,20)
!!
!!       ! change background color directly
!!       where (P_pixel.eq.0) P_pixel=9
!!       ! write standard gif file
!!       call writegif('M_pixel.3m_pixel.gif',P_pixel,P_ColorMap)
!!
!!    contains
!!
!!       subroutine target(xc,yc,rc)
!!       use M_pixel,    only : cosd, sind
!!       real     :: xc,yc,rc
!!       integer  :: i
!!       real     :: x,y
!!          do i=0,360,10
!!             x=rc*cosd(i)
!!             y=rc*sind(i)
!!             call line(xc,yc,xc+x,yc+y)
!!          enddo
!!          do i=1,int(rc),10
!!             call circle(xc,yc,real(i))
!!          enddo
!!       end subroutine target
!!    end program demo_M_pixel
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
MODULE ModLib_Pixel
!  Modify pixel data with vector drawing calls
!
!-!use M_pixel,                    only : cosd, sind
USE, INTRINSIC :: ISO_C_BINDING,   ONLY : C_SHORT, C_INT, C_FLOAT
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT8, INT16, INT32, INT64       !  1           2           4           8
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : REAL32, REAL64, REAL128         !  4           8          10
IMPLICIT NONE         !  Check all declarations
PRIVATE
!-------------------------------------------------------------------------------
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_XCENTERED=     1_C_SHORT
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_YCENTERED=     2_C_SHORT
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_LEFT=          4_C_SHORT  ! The default
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_RIGHT=         8_C_SHORT
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_TOP=          16_C_SHORT
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_BOTTOM=       32_C_SHORT ! The default
!-------------------------------------------------------------------------------
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_NORMAL=        0_C_SHORT ! The default
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_BOLD=          1_C_SHORT
!-------------------------------------------------------------------------------
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_THICK=         1_C_SHORT
INTEGER(KIND=C_SHORT),PUBLIC,PARAMETER :: D_THIN=          0_C_SHORT ! The default
!-------------------------------------------------------------------------------
INTEGER(KIND=C_INT),PUBLIC,PARAMETER   :: D_BLACK    =  0_C_INT
INTEGER(KIND=C_INT),PUBLIC,PARAMETER   :: D_RED      =  1_C_INT
INTEGER(KIND=C_INT),PUBLIC,PARAMETER   :: D_GREEN    =  2_C_INT
INTEGER(KIND=C_INT),PUBLIC,PARAMETER   :: D_YELLOW   =  3_C_INT
INTEGER(KIND=C_INT),PUBLIC,PARAMETER   :: D_BLUE     =  4_C_INT
INTEGER(KIND=C_INT),PUBLIC,PARAMETER   :: D_MAGENTA  =  5_C_INT
INTEGER(KIND=C_INT),PUBLIC,PARAMETER   :: D_CYAN     =  6_C_INT
INTEGER(KIND=C_INT),PUBLIC,PARAMETER   :: D_WHITE    =  7_C_INT
!-------------------------------------------------------------------------------
PUBLIC MATRIX
TYPE, BIND(C) :: MATRIX
   REAL(KIND=C_FLOAT),DIMENSION(4,4) :: ARRAY
END TYPE MATRIX
!==================================================================================================================================!
REAL,PARAMETER      :: PI=3.14159265358979323844
! Global Graphics State
LOGICAL,SAVE :: P_VINIT_CALLED=.FALSE.
REAL,SAVE    :: P_X=0.0, P_Y=0.0      ! current position
INTEGER,SAVE :: P_WIDTH=1             ! line width
INTEGER,SAVE :: P_COLOR_INDEX=1       ! pen color
INTEGER,SAVE :: P_NSEGS=60            ! number of line segments making up a circle
INTEGER,SAVE :: P_VIEWPORT_WIDTH=400, P_VIEWPORT_HEIGHT=400
REAL,SAVE    :: P_TEXT_HEIGHT=10.0
REAL,SAVE    :: P_TEXT_WIDTH=7.0
REAL,SAVE    :: P_TEXT_ANGLE=0.0
REAL,SAVE    :: P_TEXT_COSINE=1.0
REAL,SAVE    :: P_TEXT_SINE  =0.0
LOGICAL,SAVE :: P_X_CENTERTEXT=.FALSE.
LOGICAL,SAVE :: P_Y_CENTERTEXT=.FALSE.
CHARACTER(LEN=20) :: P_FONT='SIMPLEX'

INTEGER,PARAMETER   :: P_MAXVERTS=9999
LOGICAL,SAVE        :: P_INPOLYGON=.FALSE.
INTEGER,SAVE        :: P_POLYVERTEX=1
REAL,SAVE           :: P_POLYPOINTS(2,P_MAXVERTS)

REAL,SAVE    :: P_VIEWPORT_LEFT=0.0
REAL,SAVE    :: P_VIEWPORT_RIGHT=0.0
REAL,SAVE    :: P_VIEWPORT_BOTTOM=0.0
REAL,SAVE    :: P_VIEWPORT_TOP=0.0

REAL,SAVE    :: P_WINDOW_LEFT=0.0
REAL,SAVE    :: P_WINDOW_RIGHT=0.0
REAL,SAVE    :: P_WINDOW_BOTTOM=0.0
REAL,SAVE    :: P_WINDOW_TOP=0.0

REAL,SAVE    :: P_A, P_B, P_C, P_D ! factors for mapping between viewport coordinates and world coordinates

INTEGER,SAVE,PUBLIC,ALLOCATABLE :: P_PIXEL(:,:)
INTEGER,SAVE,PUBLIC :: P_COLORMAP(3,0:255)
LOGICAL,SAVE,PUBLIC :: P_DEBUG=.FALSE.

!data P_ColorMap(1:3,0)     / 0,0,0 /
DATA P_COLORMAP(1:3,0:16) / &
       255,255,255, &  !white
       255,  0,  0, &  !red
         0,255,  0, &  !green
       255,255,  0, &  !yellow
         0,  0,255, &  !blue
       255,  0,255, &  !magenta
         0,255,255, &  !cyan
         0,  0,  0, &  !black
         0,155,  0, &
       155,155,155, &
       155,255,255, &
       155,155,  0, &
         0,  0,155, &
       155,  0,155, &
         0,155,155, &
       100,100,100, &
       155,100,100/, &
     P_COLORMAP(1:3,17:255) / 717*255 /
!==================================================================================================================================!
! mapping
PUBLIC  :: VIEWPORT            ! define viewport into screen units
PUBLIC  :: GETVIEWPORT         ! query viewport in screen units
PUBLIC  :: ORTHO2              ! define window area in virtual world to map to viewport
PUBLIC  :: PAGE                ! define window area in virtual world as big as possible with one-to-one corresPONDENCE
!public  :: getviewport         ! returns limits of current viewport in screen coordinates
PUBLIC  :: GETDISPLAYSIZE      ! returns the width and height of the device in pixels
! draw routines
PUBLIC  :: MOVE2               ! move current position
PUBLIC  :: RMOVE2              ! relative move current position
PUBLIC  :: DRAW2               ! draw from current position to specified point
PUBLIC  :: RDRAW2              ! relative draw from current position to specified point
PUBLIC  :: LINE                ! draw line between two points
PUBLIC  :: POLYLINE2           ! draw polyline2
PUBLIC  :: POINT2              ! draw a point
! polygons
PUBLIC  :: RECT                ! draw rectangle
PUBLIC  :: CIRCLE              ! draw circle
PUBLIC  :: MAKEPOLY            ! start polygon composed of a move and draws
PUBLIC  :: CLOSEPOLY           ! end polygon started by P_makepoly(3f)
PUBLIC  :: POLY2               ! fill a polygon given an array of (x,y) points
! arcs
PUBLIC  :: ARC                 ! arc(x, y, radius, startang, endang)| Draw an arc in world units
PUBLIC  :: CIRCLEPRECISION     ! set circle precision
! text
PUBLIC  :: HERSHEY             ! draw a software text string
PUBLIC  :: JUSTFY
PUBLIC  :: FONT
PUBLIC  :: STRLENGTH           ! length of string in world coordinates
PUBLIC  :: DRAWSTR             ! draw the text in string at the current position
INTERFACE DRAWSTR
   MODULE PROCEDURE MSG_SCALAR, MSG_ONE
END INTERFACE DRAWSTR
PUBLIC  :: DRAWCHAR            ! draw a character at the current position
PUBLIC  :: TEXTSIZE            ! set text size in world units
PUBLIC  :: TEXTANG             ! set text angle
PUBLIC  :: CENTERTEXT          ! set text centering mode
PUBLIC  :: XCENTERTEXT         ! set text centering mode in X direction
PUBLIC  :: YCENTERTEXT         ! set text centering mode in Y direction
! attributes
PUBLIC  :: LINEWIDTH           ! set default line width
PUBLIC  :: MAPCOLOR            ! define a color in the color map
PUBLIC  :: COLOR               ! set current color
! print pixel array
PUBLIC  :: PRINT_P3            ! print pixel array as a P3 ppm file, replacing output file
PUBLIC  :: PRINT_P6            ! print pixel array as a P6 ppm file, replacing output file
PUBLIC  :: PRINT_PPM           ! print pixel array as a P6 ppm file, appending to existing files
PUBLIC  :: PRINT_ASCII         ! print small pixel array as ASCII text

PUBLIC  :: PIXEL               ! directly set pixel value

PUBLIC  :: STATE               ! print graphics state
PUBLIC  :: VFLUSH              ! flush graphics (NOOP)
PUBLIC  :: GETGP2              ! get current position
PUBLIC  :: CLEAR               ! set frame all to specified color index
PUBLIC  :: PREFSIZE            ! set size of pixel array to be created on next call to vinit
PUBLIC  :: VINIT               ! initialize pixel drawing module
PUBLIC  :: VEXIT               ! close down pixel drawing module
!==================================================================================================================================!
! EXTRACTED FROM M_UNITS
PUBLIC  :: COSD, SIND, D2R, POLAR_TO_CARTESIAN
! EXTRACTED FROM M_STRINGS
PUBLIC  :: I2S, LOWER
! EXTRACTED FROM M_COLOR
PUBLIC HUE                  ! converts a color's components from one color model to another
PUBLIC RGBMONO              ! convert RGB colors to a reasonable grayscale
PUBLIC CLOSEST_COLOR_NAME
PUBLIC COLOR_NAME2RGB
!----------------------------
PRIVATE HLSRGB              ! convert HLS(hue, lightness, saturation) values to RGB (red, green, blue) componeNTS
PRIVATE HVSRGB              ! given hue, saturation, value calculate red, green, & blue components
PRIVATE CMYRGB              ! given cyan,magenta, and yellow calculate red,green,blue components
!----------------------------
PRIVATE RGBHLS              ! given red,green,blue calculate hue,lightness, and saturation components
PRIVATE RGBHVS              ! given red, green, blue calculate hue, saturation and value components
PRIVATE RGBCMY              ! given red,green,blue calculate cyan,magenta, and yellow components
PRIVATE RGBYIQ              ! given RGB calculate luma, orange-blue chrominance, and  purple-green chrominance
!----------------------------
PRIVATE RGBVAL              ! internal routine to ensure a value is in the appropriate range and quadrant
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!    From: talcott!seismo!s3sun!sdcsvax!brian (Brian Kantor)
!    Subject: Hershey Fonts in Fortran 77 part 1 of 2
!    Newsgroups: mod.sources
!    Approved: jpn@panda.UUCP
!
!    Mod.sources:  Volume 4, Issue 25
!    Submitted by: seismo!s3sun!sdcsvax!brian (Brian Kantor)
!
!    The following is a fortran-77 subroutine called 'HERSHEY' which will use the
!    Public-Domain Hershey fonts to draw letters, numbers, and symbols. It is
!    in use here at UCSD in connection with several plotting packages for lettering
!    and for point plotting.
!
!    Part 2 of this distribution contains the BLOCKDATA statements which
!    form the actual fonts themselves, and a description of the format in
!    which they are stored.
!
!    I contacted the authors of this subroutine and obtained their permission to
!    distribute the subroutine. I'm in the process of writing a 'c' subroutine
!    to also use the Hershey data. I will submit that for posting when I'm
!    done.
!
!       Brian Kantor    UCSD Computer Graphics Lab
!                       c/o B-028, La Jolla, CA 92093 (619) 452-6865
!
!       decvax\         brian@sdcsvax.ucsd.edu
!       ihnp4  >---  sdcsvax  --- brian
!       ucbvax/         Kantor@Nosc
!==================================================================================================================================!
      ! offset
      INTEGER :: P_IOFF=0
      INTEGER :: P_JUST1
      INTEGER :: P_JUST2
      ! adjust
      INTEGER :: P_NCHR
      INTEGER :: P_ICHR(350)
!==================================================================================================================================!
!     From: talcott!seismo!s3sun!sdcsvax!brian (Brian Kantor)
!     Subject: Hershey Fonts in Fortran 77 part 2 of 2
!     Newsgroups: mod.sources
!     Approved: jpn@panda.UUCP
!
!     Mod.sources:  Volume 4, Issue 26
!     Submitted by: seismo!s3sun!sdcsvax!brian (Brian Kantor)
!
!
!     How it works: The subroutine and data storage assume that you are
!     using a system with 32-bit integers. The character index is used to
!     index into array 'istart'. The resulting starting index is used to
!     begin retrieval from array 'symbcd'.
!
!     Each 32 bit word in 'symbcd' contains two 16 bit fields, which in turn
!     contain three subfields:
!
!       (bit 16 - highest order bit - is zero, then)
!       3-bit-int       pencode
!       6-bit-int       delta-x
!       6-bit-int       delta-y
!
!     pencode is a drawing flag:
!       0 - end of character
!       2 - draw from current position (x,y) to (x+dx, y+dy)
!       3 - move from current position (x,y) to (x+dx, y+dy)
!       other values - undefined

INTEGER      :: J
INTEGER,SAVE :: SYMBCD(4711)
INTEGER,SAVE :: ISTART(432)
INTEGER,SAVE :: SSYMBC(128)
INTEGER,SAVE :: ISSTAR(22)
REAL,SAVE    :: WIDTH(432)

!  Data for subroutine HERSHEY providing 4 fonts, special
!  mathematical symbols, and centered symbols for data point plotting
!  Taken from Wolcott, NBS Publication
!  Modified by A.Chave, R.L.Parker, and L.Shure, IGPP/UCSD Aug 1981,Feb 1982
!
!  APPENDED FROM HERE -----
!
!  Wolcott's BLOCKDATA statement reordered for subroutine HERSHEY.
!  The new ordering is as follows:
!   The symbol numbers are:
!   1-26   UPPER CASE ROMAN SIMPLEX
!  27-52   LOWER CASE ROMAN SIMPLEX

!  53-72   SIMPLEX NUMBERS AND SYMBOLS

!  73-96   UPPER CASE GREEK SIMPLEX
!  97-120  LOWER CASE GREEK SIMPLEX

!  121-146 UPPER CASE ROMAN COMPLEX
!  147-172 LOWER CASE ROMAN COMPLEX

!  173-192 COMPLEX NUMBERS AND SYMBOLS

!  193-216 UPPER CASE GREEK COMPLEX
!  217-240 LOWER CASE GREEK COMPLEX

!  241-266 UPPER CASE ROMAN ITALIC
!  267-292 LOWER CASE ROMAN ITALIC

!  293-312 ITALIC NUMBERS AND SYMBOLS

!  313-338 UPPER CASE ROMAN DUPLEX
!  339-364 LOWER CASE ROMAN DUPLEX

!  365-384 DUPLEX NUMBERS AND SYMBOLS

!  385-432 SPECIAL MATHEMATICAL SYMBOLS
!
DATA (SYMBCD(J),  J=1,  114)/ &
     & 443556555, 443557579, 432612882,         0, 433070987, 433071584,  &
     & 323987166, 328083226, 325854871, 317404054, 317400725, 325723922,  &
     & 327657165, 323364299, 298156032, 462268125, 321889760, 309339231,  &
     & 300852123, 296493907, 298329038, 304489675, 317040204, 325527312,  &
     &         0, 433070987, 433071456, 319792797, 325953304, 327788240,  &
     & 323429900, 312845195,         0, 433070987, 433071840, 432743830,  &
     & 432383691,         0, 433070987, 433071840, 432743830,         0,  &
     & 462268125, 321889760, 309339231, 300852123, 296493907, 298329038,  &
     & 304489675, 317040204, 325527312, 327792083, 327778304, 433070987,  &
     & 462432011, 432744214,         0, 433070987,         0, 449848720,  &
     & 312911116, 306553867, 298197837, 294134546,         0, 433070987,  &
     & 462431122, 443262731,         0, 433070987, 432383627,         0,  &
     & 433070987, 433071499, 466625931, 466626443,         0, 433070987,  &
     & 433071883, 462432011,         0, 443556959, 300852123, 296493907,  &
     & 298329038, 304489675, 317040204, 325527312, 329885528, 328050397,  &
     & 321889760, 309329920, 433070987, 433071584, 323987166, 328083225,  &
     & 325822102, 317367189,         0, 443556959, 300852123, 296493907,  &
     & 298329038, 304489675, 317040204, 325527312, 329885528, 328050397,  &
     & 321889760, 309343631, 327450624, 433070987, 433071584, 323987166/

DATA (SYMBCD(J),  J =    115,   228)/ &
     & 328083226, 325854871, 317399958, 447424267,         0, 460236383,  &
     & 315630752, 300917597, 296592281, 300688471, 317367892, 323593937,  &
     & 325527116, 314942603, 300294990,         0, 441459851, 426780256,  &
     &         0, 433070993, 300360780, 310748555, 321267406, 327722784,  &
     &         0, 426779851, 460334283,         0, 428876875, 449848395,  &
     & 449849035, 470820555,         0, 430974667, 460333899,         0,  &
     & 426779862, 308655840, 309002240, 460333899, 430974688, 430286539,  &
     &         0, 443556555, 443557579, 432612882,         0, 433070987,  &
     & 433071584, 323987166, 328083226, 325854871, 317404054, 317400725,  &
     & 325723922, 327657165, 323364299, 298156032, 433070987, 433071776,  &
     &         0, 443556555, 443557579, 426092235,         0, 433070987,  &
     & 433071840, 432743830, 432383691,         0, 460333899, 430974688,  &
     & 430286539,         0, 433070987, 462432011, 432744214,         0,  &
     & 443556959, 300852123, 296493907, 298329038, 304489675, 317040204,  &
     & 325527312, 329885528, 328050397, 321889760, 309343382, 319488000,  &
     & 433070987,         0, 433070987, 462431122, 443262731,         0,  &
     & 443556555, 443557579,         0, 433070987, 433071499, 466625931,  &
     & 466626443,         0, 433070987, 433071883, 462432011,         0,  &
     & 428877472, 436938134, 428189323,         0, 443556959, 300852123/

DATA (SYMBCD(J),  J =    229,   342)/ &
     & 296493907, 298329038, 304489675, 317040204, 325527312, 329885528,  &
     & 328050397, 321889760, 309329920, 433070987, 462432011, 433071904,  &
     &         0, 433070987, 433071584, 323987166, 328083225, 325822102,  &
     & 317367189,         0, 428877014, 293974816, 324023051, 323321856,  &
     & 441459851, 426780256,         0, 428712733, 296723360, 303047775,  &
     & 307143897, 308655771, 323921503, 319825312, 313500957, 309100544,  &
     & 445654283, 441295834, 298623831, 296362898, 300459152, 315106897,  &
     & 323561172, 325822105, 321725851, 307068928, 430974667, 430286560,  &
     &         0, 447751499, 428680026, 298623957, 302621778, 310945169,  &
     & 321463955, 325756697, 330114970,         0, 430285899, 298394454,  &
     & 296559517, 303015136, 313533983, 323921626, 325789330, 317040331,  &
     &         0, 455910987, 455812568, 313304217, 302785430, 296330065,  &
     & 298263564, 306554187, 317072974,         0, 433070987, 432743448,  &
     & 307012953, 317466198, 323593873, 321332684, 312845451, 302392206,  &
     &         0, 455812568, 313304217, 302785430, 296330065, 298263564,  &
     & 306554187, 317072974,         0, 456140363, 455812568, 313304217,  &
     & 302785430, 296330065, 298263564, 306554187, 317072974,         0,  &
     & 430548563, 321562135, 317465945, 307012632, 298525523, 296264590,  &
     & 302392459, 312845772, 321323008, 445654176, 303014876, 300266265/

DATA (SYMBCD(J),  J =    343,   456)/ &
     & 309100544, 455910985, 318973381, 312616068, 302167638, 317465945,  &
     & 307012632, 298525523, 296264590, 302392459, 312845772, 321323008,  &
     & 433070987, 432710744, 309110169, 319563349, 321224704, 430973855,  &
     & 300950433, 296760217, 298156032, 435168287, 305144865, 300954649,  &
     & 302261189, 295838404,         0, 433070987, 453813135, 441034315,  &
     &         0, 433070987,         0, 432841611, 432710744, 309110169,  &
     & 319563349, 321238613, 327952281, 338471128, 344631563,         0,  &
     & 432841611, 432710744, 309110169, 319563349, 321224704, 441230360,  &
     & 298525523, 296264590, 302392459, 312845772, 321332881, 323593814,  &
     & 317465945, 307003392, 432841604, 432743448, 307012953, 317466198,  &
     & 323593873, 321332684, 312845451, 302392206,         0, 455910980,  &
     & 455812568, 313304217, 302785430, 296330065, 298263564, 306554187,  &
     & 317072974,         0, 432841611, 432645078, 304882905, 315392000,  &
     & 453715416, 311207001, 298591062, 298460179, 313075153, 319268366,  &
     & 317072651, 304456588, 296157184, 435168207, 302392459, 310752025,  &
     & 309100544, 432841615, 300295243, 310748556, 321369689, 321224704,  &
     & 428647563, 453813387,         0, 430744651, 447521867, 447522379,  &
     & 464299595,         0, 430745099, 453813067,         0, 428647563,  &
     & 453813387, 302228357, 293741252,         0, 453813067, 430745113/

DATA (SYMBCD(J),  J =    457,   570)/ &
     & 430286347,         0, 443327576, 300622740, 296264526, 298198027,  &
     & 306554124, 317171282, 325789465, 443327833, 315368918, 321332876,  &
     & 325429003,         0, 449848607, 307143705, 300622738, 296100612,  &
     & 449848864, 323954331, 321693208, 315335895, 443262294, 317335058,  &
     & 319268301, 314975499, 306553868, 300327824,         0, 426451800,  &
     & 300721177, 306980055, 311043344, 308655833, 323692116, 308651079,  &
     & 302120960, 447521945, 302785430, 296330064, 298230732, 304456907,  &
     & 312878542, 319333908, 317433177, 309175453, 307209440, 313533919,  &
     & 321814528, 451650968, 311207001, 300688342, 302654675, 443130834,  &
     & 296231758, 298198027, 308651340, 317128704, 445654175, 305079389,  &
     & 307111259, 319665691, 311206999, 298459985, 296199053, 302359753,  &
     & 310617349, 308421700, 302186496, 426418967, 298624025, 304882774,  &
     & 302588811, 436806806, 311174553, 319596183, 323626575, 314703872,  &
     & 426418967, 298624025, 304882774, 302556174, 304489611, 310748556,  &
     & 319268433, 323626713, 325985951, 319825312, 313468252, 315401750,  &
     & 323626834,         0, 437035922, 296166220, 298165259, 306619599,  &
     &         0, 437035787, 457975385, 319595928, 306848787, 300528595,  &
     & 304686225, 310781259, 314942924,         0, 426779488, 300917790,  &
     & 319141017, 293961728, 439132868, 436904912, 300328011, 308651340/

DATA (SYMBCD(J),  J =    571,   684)/ &
     & 317138514, 460105298, 319235596, 321234635, 329688975,         0,  &
     & 430744601, 300524430, 296072857, 321594900, 315139278, 302392139,  &
     &         0, 445654175, 305079389, 307111259, 319665499, 307045401,  &
     & 300655573, 304719122, 315176210, 302556048, 296166220, 300229832,  &
     & 310617349, 306324484,         0, 441230360, 298525523, 296231821,  &
     & 300295243, 308651340, 317138449, 319432151, 315368729, 307003392,  &
     & 443327435, 453813843, 323430091, 428549016, 304916377,         0,  &
     & 432645008, 300327948, 306554123, 314975758, 321431124, 319530456,  &
     & 313304281, 304882646, 298427012,         0, 462202009, 302785430,  &
     & 296330064, 298230732, 304456907, 312878542, 319333908, 317433240,  &
     & 311197696, 447521931, 428549016, 304916249,         0, 426418967,  &
     & 298624025, 304882774, 300426189, 304456907, 314975758, 323561174,  &
     & 325877760, 441197591, 298492754, 296199053, 300295243, 310748620,  &
     & 323430161, 329918295, 325887577, 317433171, 308749316,         0,  &
     & 428647321, 302753158, 318908036, 460105367, 319431561, 293806788,  &
     &         0, 458237060, 426418967, 298624025, 304882774, 302556174,  &
     & 304489675, 312845836, 323430161, 332081113,         0, 441230360,  &
     & 298492754, 296199052, 300262475, 308684111, 449422671, 314975691,  &
     & 321234636, 329754514, 332048216, 327974912, 445653835, 445654731/

DATA (SYMBCD(J),  J =    685,   798)/ &
     & 445556363, 434677265, 426091595, 451258187,         0, 435168203,  &
     & 437265419, 428877344, 326084382, 330180442, 327952087, 319501856,  &
     & 323987166, 328083226, 325854871, 319501334, 319497941, 327821138,  &
     & 329754381, 325461515, 293975574, 323659476, 327755535, 325494412,  &
     & 319127552, 460236570, 328214237, 321889696, 311436383, 300852123,  &
     & 296493907, 298329038, 304489739, 314943052, 325527312, 445654175,  &
     & 302949339, 298591123, 300426254, 306586891,         0, 435168203,  &
     & 437265419, 428877216, 321890013, 328050520, 329885456, 325527116,  &
     & 314942219, 449848863, 323921627, 327952147, 325592718, 319169931,  &
     &         0, 435168203, 437265419, 449652114, 428877600, 328017632,  &
     & 436938134, 428189451, 327722699,         0, 435168203, 437265419,  &
     & 449652114, 428877600, 328017632, 436938134, 428188875,         0,  &
     & 460236570, 328214237, 321889696, 311436383, 300852123, 296493907,  &
     & 298329038, 304489739, 314943052, 325530912, 307209245, 300786584,  &
     & 298427344, 302457996, 310752979, 325433107, 327530003, 334069760,  &
     & 435168203, 437265419, 462432011, 464529227, 428877024, 456140832,  &
     & 436938518, 428188875, 455452683,         0, 435168203, 437265419,  &
     & 428877024, 428188875,         0, 445654287, 308683851, 300262220,  &
     & 294069008, 296264592, 296203488, 308782220, 304460832, 317718528/

DATA (SYMBCD(J),  J =    799,   912)/ &
     & 435168203, 437265419, 464528403, 447457099, 445359883, 428877024,  &
     & 456140768, 428188875, 455452619,         0, 435168203, 437265419,  &
     & 428877024, 428189387, 325625483,         0, 435168203, 437265806,  &
     & 435168651, 464528779, 464529227, 466626443, 428876832, 464529504,  &
     & 428188811, 457549899,         0, 435168203, 437266189, 437200651,  &
     & 462432011, 428876832, 456140768, 428188811,         0, 445654111,  &
     & 300852123, 296461140, 298329038, 304489739, 314943052, 325527312,  &
     & 329918295, 328050397, 321889696, 311440672, 307209245, 300786583,  &
     & 298460112, 302457996, 310752651, 319170190, 325592852, 327919323,  &
     & 323921439, 315621376, 435168203, 437265419, 428877344, 326084382,  &
     & 330180441, 327919318, 319464469, 454043295, 326051612, 327984855,  &
     & 323692053, 428188875,         0, 445654111, 300852123, 296461140,  &
     & 298329038, 304489739, 314943052, 325527312, 329918295, 328050397,  &
     & 321889696, 311440672, 307209245, 300786583, 298460112, 302457996,  &
     & 310752651, 319170190, 325592852, 327919323, 323921439, 315634765,  &
     & 304555152, 310945105, 317203982, 321103494, 327362376, 329561614,  &
     & 321201800, 325297927, 329515008, 435168203, 437265419, 428877344,  &
     & 326084382, 330180442, 327952087, 319497238, 454043295, 326051612,  &
     & 328017624, 323724822, 428188875, 447423957, 319432397, 327558988/

DATA (SYMBCD(J),  J =    913,   1026)/ &
     & 331789781, 319399564, 325429067, 331786126,         0, 458139360,  &
     & 325920413, 319792480, 307241951, 296657755, 298623960, 304850389,  &
     & 321529554, 430810073, 304883158, 321562260, 325658318, 321267083,  &
     & 308651020, 298263377, 296067982,         0, 443557067, 445654283,  &
     & 430973722, 294659808, 325920416, 436577739,         0, 435168209,  &
     & 302457996, 312845771, 323364622, 329820000, 437265425, 304555212,  &
     & 312849184, 309343904, 336592896, 430974219, 433071374, 460334347,  &
     & 426779744, 451946336,         0, 433071243, 435168400, 449848459,  &
     & 449848971, 451946128, 466626187, 426779808, 460335200,         0,  &
     & 430974603, 433071819, 460333899, 426779744, 451946336, 426091595,  &
     & 451258187,         0, 430974229, 310752160, 313173323, 462431573,  &
     & 426779744, 454043552, 438674955,         0, 458236747, 460333963,  &
     & 433070938, 296756960, 430286539, 325625483,         0, 445653835,  &
     & 445654731, 445556363, 434677265, 426091595, 451258187,         0,  &
     & 435168203, 437265419, 428877344, 326084382, 330180442, 327952087,  &
     & 319501856, 323987166, 328083226, 325854871, 319501334, 319497941,  &
     & 327821138, 329754381, 325461515, 293975574, 323659476, 327755535,  &
     & 325494412, 319127552, 435168203, 437265419, 428877536, 325920416,  &
     & 428188875,         0, 445653771, 445654795, 445556427, 430319308/

DATA (SYMBCD(J),  J =    1027,   1140)/ &
     & 428189451,         0, 435168203, 437265419, 449652114, 428877600,  &
     & 328017632, 436938134, 428189451, 327722699,         0, 458236747,  &
     & 460333963, 433070938, 296756960, 430286539, 325625483,         0,  &
     & 435168203, 437265419, 462432011, 464529227, 428877024, 456140832,  &
     & 436938518, 428188875, 455452683,         0, 445654111, 300852123,  &
     & 296461140, 298329038, 304489739, 314943052, 325527312, 329918295,  &
     & 328050397, 321889696, 311440672, 307209245, 300786583, 298460112,  &
     & 302457996, 310752651, 319170190, 325592852, 327919323, 323921439,  &
     & 315634841, 306787865, 319370390, 319501461, 319455232, 435168203,  &
     & 437265419, 428877024, 428188875,         0, 435168203, 437265419,  &
     & 464528403, 447457099, 445359883, 428877024, 456140768, 428188875,  &
     & 455452619,         0, 445653835, 445654731, 445556363, 426091595,  &
     & 451258187,         0, 435168203, 437265806, 435168651, 464528779,  &
     & 464529227, 466626443, 428876832, 464529504, 428188811, 457549899,  &
     &         0, 435168203, 437266189, 437200651, 462432011, 428876832,  &
     & 456140768, 428188811,         0, 433103708, 464561948, 441197651,  &
     & 455878163, 432513866, 463972106, 433039135, 433006366, 441132566,  &
     & 441099797, 432449293, 432416524,         0, 445654111, 300852123,  &
     & 296461140, 298329038, 304489739, 314943052, 325527312, 329918295/

DATA (SYMBCD(J),  J =    1141,   1254)/ &
     & 328050397, 321889696, 311440672, 307209245, 300786583, 298460112,  &
     & 302457996, 310752651, 319170190, 325592852, 327919323, 323921439,  &
     & 315621376, 435168203, 437265419, 462432011, 464529227, 428877856,  &
     & 428188875, 455452683,         0, 435168203, 437265419, 428877344,  &
     & 326084382, 330180441, 327919318, 319464469, 454043295, 326051612,  &
     & 327984855, 323692053, 428188875,         0, 430974230, 293974816,  &
     & 309015328, 326117146, 324023116, 323367691, 325429009, 323321856,  &
     & 443557067, 445654283, 430973722, 294659808, 325920416, 436577739,  &
     &         0, 428712733, 296723360, 303047775, 307143897, 308654877,  &
     & 298820639, 307148507, 326018719, 321922528, 315598173, 311207179,  &
     & 460236383, 317695325, 436577739,         0, 445654283, 447751499,  &
     & 441295834, 298623831, 296362898, 300459152, 317204113, 325658388,  &
     & 327919321, 323823067, 307082395, 302851033, 298558356, 300491793,  &
     & 306722256, 321431186, 325723863, 323790426, 317568096, 319829067,  &
     & 319127552, 430974603, 433071819, 460333899, 426779744, 451946336,  &
     & 426091595, 451258187,         0, 447751499, 449848715, 428647258,  &
     & 300721173, 304718994, 310948698, 298623957, 302621778, 310945233,  &
     & 323561171, 327853913, 332215761, 321463955, 325756697, 332212185,  &
     & 441460320, 440772171,         0, 430384011, 306553871, 298427222/

DATA (SYMBCD(J),  J =    1255,   1368)/ &
     & 296559517, 303015136, 317728415, 328116058, 329983763, 323462667,  &
     & 327526222, 436708306, 298525594, 300852319, 309343712, 321890013,  &
     & 328017686, 325658255, 432415820, 455485196,         0, 434873302,  &
     & 298525591, 300688473, 313304536, 319530581, 321332876, 325432855,  &
     & 319235660, 325429003, 453682644, 304718738, 296231758, 298198091,  &
     & 310748556, 319239251, 300491664, 298263500, 304447488, 435168203,  &
     & 437265419, 436937880, 311207321, 321660630, 327788305, 325527116,  &
     & 314942731, 306586638, 449619480, 323692243, 325625486, 319169931,  &
     & 428876832,         0, 455812629, 321529493, 323692056, 315401433,  &
     & 302785430, 296330065, 298263564, 308651339, 319170190, 443327576,  &
     & 300622739, 298361806, 304489675,         0, 456140363, 458237579,  &
     & 455812568, 313304281, 302785430, 296330065, 298263564, 308651339,  &
     & 317072974, 443327576, 300622739, 298361806, 304489675, 449848992,  &
     & 455452491,         0, 432645779, 323659351, 319563161, 309109784,  &
     & 298525523, 296264590, 302392523, 312845836, 323434067, 321594904,  &
     & 443327576, 300622739, 298361806, 304489675,         0, 445621470,  &
     & 311338334, 313500960, 307242015, 300852171, 441459807, 302949387,  &
     & 428647705, 428188875,         0, 441230360, 300655509, 298427345,  &
     & 302523535, 310879632, 317236755, 319464919, 315368729, 307016728/

DATA (SYMBCD(J),  J =    1369,   1482)/ &
     & 300622802, 302527888, 317269462, 315373015, 319563417, 323757592,  &
     & 434676624, 296166221, 298165322, 314910281, 323236685, 298198091,  &
     & 314943050, 323233415, 321037700, 302129989, 293839624, 296035339,  &
     &         0, 435168203, 437265419, 436937880, 313304537, 323757782,  &
     & 325432793, 321660566, 323334944, 303051531, 308655563, 331710464,  &
     & 435168159, 300885023, 300954585, 300266521, 302363417, 302822155,  &
     & 308641792, 437265375, 302982239, 303051865, 304325637, 297935620,  &
     & 291676870, 293839686, 293778457, 302228421, 297939801, 304906240,  &
     & 435168203, 437265419, 458007567, 447325899, 445228683, 428876832,  &
     & 451716953, 428188875, 451258187,         0, 435168203, 437265419,  &
     & 428876832, 428188875,         0, 434938827, 437036043, 436937880,  &
     & 313304537, 323757782, 325432793, 321660566, 323335894, 330049561,  &
     & 340568408, 348858763, 474786072, 346761547, 428647449, 428188875,  &
     & 451258251, 474327627,         0, 434938827, 437036043, 436937880,  &
     & 313304537, 323757782, 325432793, 321660566, 323334937, 302822155,  &
     & 308655563, 331710464, 443327512, 298525523, 296264590, 302392523,  &
     & 312845836, 323430097, 325691030, 319563097, 309114073, 304882646,  &
     & 298427281, 300360780, 308655435, 317072974, 323528339, 321594840,  &
     & 313294848, 434938820, 437036036, 436937880, 311207321, 321660630/

DATA (SYMBCD(J),  J =    1483,   1596)/ &
     & 327788305, 325527116, 314942731, 306586638, 449619480, 323692243,  &
     & 325625486, 319169931, 428647449, 427959492,         0, 455910980,  &
     & 458008196, 455812568, 313304281, 302785430, 296330065, 298263564,  &
     & 308651339, 317072974, 443327576, 300622739, 298361806, 304489675,  &
     & 448931652,         0, 434938827, 437036043, 436839510, 309077337,  &
     & 319596120, 321627670, 317433368, 428647449, 428188875,         0,  &
     & 451651097, 319464919, 315368729, 302818200, 296461141, 298460179,  &
     & 313042384, 319271766, 298492948, 313075153, 319301133, 317072715,  &
     & 304456652, 298230607, 296067981,         0, 435168207, 302392459,  &
     & 310748556, 317142048, 302490700, 306557721, 311197696, 434938830,  &
     & 302392523, 312845836, 323433497, 302457932, 308655769, 323335897,  &
     & 325432089, 302822873, 325891723, 331710464, 430744779, 432841933,  &
     & 455910603, 426550361, 447522521,         0, 432841867, 434939022,  &
     & 449619083, 449619595, 451716750, 466396811, 426550425, 460105817,  &
     &         0, 432842315, 434939531, 458007435, 428647577, 449619737,  &
     & 428188811, 449160971,         0, 432841995, 434939149, 458007819,  &
     & 306422789, 297935684, 293774150, 297972505, 307017113, 327974912,  &
     & 453813067, 455910283, 432841557, 296527449, 430286411, 321365515,  &
     &         0, 445424728, 300622740, 296264526, 298198091, 308651340/

DATA (SYMBCD(J),  J =    1597,   1710)/ &
     & 319268498, 327886681, 445424792, 302719956, 298361742, 300295243,  &
     & 445425049, 319563350, 325527308, 329627033, 317466134, 323430092,  &
     & 329623435,         0, 451945759, 307143705, 300622738, 296100612,  &
     & 451945823, 309240921, 302719954, 298197828, 451946080, 326084382,  &
     & 328050393, 323757527, 309048928, 326051547, 323790424, 317437143,  &
     & 317400660, 323561103, 321299980, 312845515, 304489485, 300430551,  &
     & 315303444, 321463887, 319202764, 312836096, 426451800, 300721241,  &
     & 309077271, 313140560, 310780996, 428581784, 306980119, 462202582,  &
     & 323626317, 306455556, 460105366, 321529165,         0, 451683673,  &
     & 309109784, 298492754, 296199053, 300295243, 308651404, 319268434,  &
     & 321562135, 311305438, 309339425, 315663904, 323957977, 304882645,  &
     & 298394510, 300299467, 312878543, 319366678, 317465947, 311338271,  &
     & 313533920, 323944448, 455812568, 313304153, 300688342, 304751891,  &
     & 439133208, 302720148, 311014675, 300491600, 296166284, 304456971,  &
     & 314975758, 445228050, 298328974, 300295243,         0, 447751391,  &
     & 307176605, 309208475, 325953244, 319661337, 304849812, 296264527,  &
     & 298230859, 310682951, 312648964, 306324549, 449651863, 300557201,  &
     & 298296269, 304447488, 426418967, 298624089, 306979990, 304686027,  &
     & 437036120, 304817170, 298169426, 309011800, 317498969, 325854999/

DATA (SYMBCD(J),  J =    1711,   1824)/ &
     & 327821007, 318912089, 325822164, 323462596,         0, 426418967,  &
     & 298624089, 306979990, 304653390, 306586827, 437036120, 304817169,  &
     & 302457932, 308651339, 317072974, 325625620, 330082141, 328181408,  &
     & 319825310, 315499993, 321595092, 331953612, 321365649, 325723929,  &
     & 328115935, 324009984, 437035922, 296166220, 298165323, 308716815,  &
     & 439133138, 298263436, 300253184, 437035787, 439133003, 458008280,  &
     & 327952089, 321693144, 308946003, 300528723, 308880716, 314946643,  &
     & 306783500, 312845771, 321267407,         0, 430973920, 305112222,  &
     & 309208654, 323364555, 435168350, 307111438, 321267403, 327529753,  &
     & 293975321, 296058880, 439132868, 441230084, 439034896, 302425227,  &
     & 310748556, 319235729, 462202446, 321267339, 329623501, 336050009,  &
     & 323430028, 325419008, 437035915, 439133203, 300360587, 460105365,  &
     & 319338265, 325789332, 319333775, 308716620, 298169177, 304906240,  &
     & 447751391, 307176605, 309208475, 321762715, 307045401, 300655573,  &
     & 304719122, 317273499, 309142617, 302752789, 306816274, 445195281,  &
     & 298328910, 296100810, 310650183, 312648900, 304231698, 304653264,  &
     & 298263436, 302327048,         0, 443327512, 298492754, 296199053,  &
     & 300295243, 308651404, 319268434, 321562135, 317465945, 309114073,  &
     & 304882645, 298394510, 300299467, 312878543, 319366678, 317456384/

DATA (SYMBCD(J),  J =    1825,   1938)/ &
     & 443294667, 443294731, 455878219, 455878283, 428549016, 304916377,  &
     & 428549015, 304883608,         0, 432546765, 302392459, 310748620,  &
     & 321365650, 323659351, 319563161, 311207000, 300589970, 289551627,  &
     & 314975759, 321463894, 319567129, 306979861, 300491460,         0,  &
     & 464299225, 302785429, 296297295, 298230732, 304456907, 314975759,  &
     & 321463893, 319530456, 313308377, 304882645, 298394510, 300299467,  &
     & 312878543, 319366678, 317470168, 330039296, 447489163, 447489227,  &
     & 428549016, 304916249, 428549015, 304883480,         0, 426418967,  &
     & 298624089, 306979990, 302523405, 306557977, 304882774, 300426189,  &
     & 302392459, 308651404, 319235729, 325723863, 323790424, 323725012,  &
     & 457746135,         0, 441197591, 298492754, 296199053, 300295243,  &
     & 310748620, 323430161, 329918295, 325887577, 317433171, 308749316,  &
     & 430416845, 304489740, 317105807, 327726935, 325854808, 317400403,  &
     & 308716612,         0, 428647321, 302785622, 314811845, 318911385,  &
     & 300688406, 312714629, 318908036, 460105367, 319431561, 293806788,  &
     &         0, 456139972, 458237060, 426418967, 298624089, 306979990,  &
     & 304653390, 308684172, 319203024, 329888793, 304882774, 302556174,  &
     & 304489675, 314942988, 323430161, 329885657,         0, 432710679,  &
     & 309077145, 302785429, 296297295, 298197963, 304456908, 312976786/

DATA (SYMBCD(J),  J =    1939,   2052)/ &
     & 430416781, 300295244, 308716879, 447292751, 314975691, 321234636,  &
     & 329754514, 332048216, 327984856, 330016661, 447194509, 317072972,  &
     & 325494607,         0, 451945099, 451945995, 449783243, 432580049,  &
     & 419799947, 444966539,         0, 443556683, 445653899, 437266144,  &
     & 332376029, 334342040, 330016406, 460334943, 332310427, 330049303,  &
     & 323695702, 323692309, 329885521, 327624332, 314942091, 457909973,  &
     & 327788305, 325527116, 314933248, 462366558, 332408666, 330180382,  &
     & 326084192, 315630815, 305046490, 298558291, 296231821, 300295307,  &
     & 312845772, 321332880, 449848607, 307143706, 300655507, 298329037,  &
     & 302392459,         0, 443556683, 445653899, 437266016, 328181598,  &
     & 332244887, 329885391, 321299916, 308650635, 456140511, 328148827,  &
     & 330016531, 323462669, 314975435,         0, 443556683, 445653899,  &
     & 453846418, 437266400, 332212128, 439035350, 423994955, 325592587,  &
     &         0, 443556683, 445653899, 453846418, 437266400, 332212128,  &
     & 439035350, 423994443,         0, 462366558, 332408666, 330180382,  &
     & 326084192, 315630815, 305046490, 298558291, 296231821, 300295307,  &
     & 310748620, 321332946, 449848607, 307143706, 300655507, 298329037,  &
     & 302392459, 444966284, 319235730, 451487634,         0, 443556683,  &
     & 445653899, 470820491, 472917707, 437265888, 464529696, 439035734/

DATA (SYMBCD(J),  J =    2053,   2166)/ &
     & 423994443, 451258251,         0, 443556683, 445653899, 437265888,  &
     & 423994443,         0, 456140047, 308716684, 302359435, 294003406,  &
     & 292037393, 296231695, 454042831, 306619403, 447751968,         0,  &
     & 443556683, 445653899, 472917011, 451651275, 449554059, 437265888,  &
     & 464529632, 423994443, 451258187,         0, 443556683, 445653899,  &
     & 437265888, 423994955, 325625355,         0, 443556683, 443557131,  &
     & 445654349, 472917259, 472917707, 475014923, 437265696, 472918368,  &
     & 423994379, 453355467,         0, 443556683, 443557518, 443459211,  &
     & 470820491, 437265632, 464529632, 423994379,         0, 449848543,  &
     & 305046490, 298558291, 296231821, 300295243, 310748620, 321332945,  &
     & 327821144, 330147614, 326084192, 315635104, 311403677, 302851031,  &
     & 298427280, 300328011, 444966284, 319235729, 325723928, 328050398,  &
     & 321912832, 443556683, 445653899, 437266208, 334473245, 336439256,  &
     & 329983573, 304789280, 332376029, 334342040, 327886421, 423994443,  &
     &         0, 449848543, 305046490, 298558291, 296231821, 300295243,  &
     & 310748620, 321332945, 327821144, 330147614, 326084192, 315635104,  &
     & 311403677, 302851031, 298427280, 300328011, 444966284, 319235729,  &
     & 325723928, 328050398, 321926093, 300360720, 306750673, 313009550,  &
     & 314811846, 321070728, 323270030, 316941831, 321103496,         0/

DATA (SYMBCD(J),  J =    2167,   2280)/ &
     & 443556683, 445653899, 437266144, 332376029, 334342040, 330016406,  &
     & 304821984, 330278813, 332244824, 327919254, 449521173, 321529484,  &
     & 325429067, 331786126, 455747277, 327558988, 331788939, 304447488,  &
     & 464463774, 334505882, 332277598, 328181344, 313533599, 302949403,  &
     & 304915608, 321529554, 437101721, 321562260, 325658319, 323397196,  &
     & 314942603, 300295053, 296198993, 293970765, 298221568, 451945547,  &
     & 454042763, 439362458, 303048672, 332212128, 432383307,         0,  &
     & 441459669, 298361742, 300295307, 314943052, 325527313, 336606432,  &
     & 302687185, 300360716, 306557920, 315635552, 342884352, 437265483,  &
     & 439362701, 466625611, 433071392, 458237984,         0, 441459723,  &
     & 443556941, 458236939, 458237451, 460334669, 475014667, 435168672,  &
     & 468724064,         0, 439363083, 441460299, 468722379, 435168608,  &
     & 460335200, 421897163, 447063755,         0, 437265686, 304460896,  &
     & 313205899, 468723030, 433071392, 460335200, 432383307,         0,  &
     & 466625227, 468722443, 441459674, 305145824, 426092107, 325625355,  &
     &         0, 466527124, 331710464, 432973716, 298156032, 455747095,  &
     & 317465945, 309109784, 298492754, 296199053, 300295243, 308651404,  &
     & 319235665, 323692187, 321857055, 315630816, 305112094, 302949469,  &
     & 305083609, 304882645, 298394510, 300299467, 312878542, 319333974/

DATA (SYMBCD(J),  J =    2281,   2394)/ &
     & 321758750, 315621376, 428877067, 430974221, 462431499, 428877600,  &
     & 430941919,         0, 453780889, 309109784, 298525523, 296231821,  &
     & 300295307, 312845772, 443327576, 300622739, 298329037, 302392459,  &
     & 432612754,         0, 466625433, 331953040, 331887499, 331710464,  &
     & 433072025, 298398608, 331887499, 331710464, 468166479, 325592658,  &
     & 315303255, 309077080, 300655509, 298427345, 304620752, 313042322,  &
     & 321595096, 330082265,         0, 468821922, 334538786, 336701412,  &
     & 330442467, 321955359, 317597080, 310781128, 306394786, 321922588,  &
     & 315106636, 310682823, 304260036, 295838469, 293806919, 298001221,  &
     &         0, 468821922, 334538786, 336701412, 330442467, 321955359,  &
     & 317597080, 310781128, 306394786, 321922588, 315106636, 310682823,  &
     & 304260036, 295838469, 293806919, 298001221, 447587482, 302785493,  &
     & 300524560, 306652493, 317105806, 327690067, 329951000, 323823067,  &
     & 313360384, 470394833, 329787088, 321431058, 313206039, 306979864,  &
     & 298558293, 296330129, 302523536, 310945106, 319497815, 325855064,  &
     & 334211093, 336166912, 449717643, 432678804, 432383883,         0,  &
     & 449717643, 432940956, 432678804,         0, 432908045, 462267277,  &
     &         0, 451847580, 317564444, 317633428, 336213453, 314975691,  &
     & 319169997,         0, 439493700, 441590916, 479340804, 481438020/

DATA (SYMBCD(J),  J =    2395,   2508)/ &
     & 431106660, 430056836, 469903940,         0, 434807700, 300524564,  &
     & 300580864, 430744665, 317109273, 317044772, 317030400, 435299926,  &
     & 297939876, 319501156, 319468388, 345123229, 343028677, 344109956,  &
     & 344074635, 341966848, 447751327, 302916570, 298558290, 296166284,  &
     & 302359691, 312878543, 319333972, 323790493, 321889760, 313537888,  &
     & 309306460, 302851031, 298394510, 300295179, 440771852, 315074001,  &
     & 319432281, 321824287, 317731798, 319488000, 443688035, 303113184,  &
     & 300885020, 304981145, 306947093, 439460897, 303015005, 307111130,  &
     & 309077142, 298460306, 308815054, 306586699, 302294023, 304264211,  &
     & 306750607, 304522252, 300229576, 302195781, 308412416, 435299427,  &
     & 307307744, 309273756, 304981017, 302752917, 439461025, 307209309,  &
     & 302916570, 300688406, 311043090, 300426190, 302392395, 306488455,  &
     & 304264339, 302556175, 304522380, 308618440, 306390085, 300023808,  &
     & 462169818, 321758619, 311239897, 306914451, 308847952, 319301265,  &
     & 325694875, 311207126, 308913425, 313014043, 325691089, 329787344,  &
     & 338241685, 340502618, 336471966, 328181344, 315630815, 305079260,  &
     & 298656599, 296362897, 300393549, 308684171, 321234700, 331786190,  &
     & 464365331, 327722832,         0, 426321109, 325661394, 309012178,  &
     &        0, 298394766, 308651209, 306390020, 300032901, 295936842/

DATA (SYMBCD(J),  J =    2509,   2622)/ &
     & 298263570, 306881880, 317498969, 327952214, 329852686, 323364363,  &
     & 317040012, 315041231, 319235533, 455911128, 327886610, 325527180,  &
     &         0, 458008082, 317138380, 319137483, 329688975, 460105298,  &
     & 319235596, 321238546, 319464920, 313304281, 302785429, 296297295,  &
     & 298230732, 304456907, 312878543, 319370457, 304882645, 298394510,  &
     & 300285952, 441459603, 298329037, 302396640, 300528595, 302720152,  &
     & 311207321, 319563351, 323659410, 321365452, 310748299, 302392271,  &
     & 300529176, 321594962, 319268236, 310752224, 309329920, 453715477,  &
     & 321562198, 319563161, 309109784, 298492754, 296199053, 300295243,  &
     & 308651404, 319272153, 304882645, 298394510, 300285952, 462431762,  &
     & 317138380, 319137483, 329688975, 464528978, 319235596, 321238546,  &
     & 319464920, 313304281, 302785429, 296297295, 298230732, 304456907,  &
     & 312878543, 319370457, 304882645, 298394510, 300299872, 330301440,  &
     & 432546961, 313075220, 321594904, 315401433, 302785429, 296297295,  &
     & 298230732, 304456907, 314975758, 443327576, 300589970, 298263500,  &
     &         0, 456107550, 321824414, 323987040, 317728095, 311370972,  &
     & 307012555, 298033989, 451945822, 311305432, 304587787, 300163974,  &
     & 295871172, 287449605, 285418055, 289612357, 432842265,         0,  &
     & 460105163, 314844421, 304227204, 293774022, 291742472, 295936774/

DATA (SYMBCD(J),  J =    2623,   2736)/ &
     & 458007947, 312747205, 304231954, 319464920, 313304281, 302785429,  &
     & 296297295, 298230732, 304456907, 312878543, 319370457, 304882645,  &
     & 298394510, 300285952, 441459467, 443556683, 434709590, 309077337,  &
     & 317498968, 323724949, 319268364, 321238489, 321627733, 317171148,  &
     & 319137483, 329688975, 435168480,         0, 443557023, 309273887,  &
     & 309342933, 294364057, 304915608, 306881551, 302392395, 437036120,  &
     & 304784335, 300295179, 308651341, 315064320, 445654239, 311371103,  &
     & 311440149, 296461273, 307012824, 308978699, 300163974, 295871172,  &
     & 287449605, 285418055, 289612357, 439133336, 306881483, 298066758,  &
     & 291635200, 441459467, 443556683, 457975383, 323692247, 325854873,  &
     & 321693144, 308946003, 300528723, 308880716, 314946643, 306783500,  &
     & 312845771, 321267407, 435168480,         0, 441459602, 296166220,  &
     & 298165323, 308716815, 443556818, 298263436, 300266464, 309329920,  &
     & 426418967, 298624089, 306979990, 304686027, 437036120, 304817170,  &
     & 298169426, 309011800, 317498969, 325854999, 327853643, 455911127,  &
     & 325756427, 459876182, 334243929, 342665560, 348891541, 344434956,  &
     & 346405081, 346794325, 342337740, 344304075, 354855567,         0,  &
     & 426418967, 298624089, 306979990, 304686027, 437036120, 304817170,  &
     & 298169426, 309011800, 317498969, 325854999, 327853711, 323364555/

DATA (SYMBCD(J),  J =    2737,   2850)/ &
     & 455911127, 325756495, 321267339, 329623501, 336035840, 443327512,  &
     & 298492754, 296199053, 300295243, 308651404, 319268434, 321562135,  &
     & 317465945, 309114073, 304882645, 298394510, 300299467, 312878543,  &
     & 319366678, 317456384, 426418967, 298624089, 306979990, 304685892,  &
     & 437036120, 304817170, 293745746, 306881816, 315401753, 323757783,  &
     & 327853842, 325559884, 314942731, 306586703, 304690840, 325789394,  &
     & 323462668, 314946116, 302120960, 458007812, 460105028, 453584405,  &
     & 317465945, 309109784, 298492754, 296199053, 300295243, 308651340,  &
     & 317171218, 443327576, 300589970, 298263500, 438445572,         0,  &
     & 426418967, 298624089, 306979990, 304686027, 437036120, 304817170,  &
     & 298169426, 309011800, 317498969, 323757719, 321594903, 321650688,  &
     & 453748246, 321594967, 319563097, 307012568, 298558357, 300557712,  &
     & 317174678, 300590481, 317203917, 314975435, 302359372, 294036238,  &
     & 296166221,         0, 443556818, 298263436, 300262539, 310814031,  &
     & 445654034, 300360652, 302363481, 315392000, 426418967, 298624089,  &
     & 306979989, 302490637, 306557977, 304882773, 300393421, 302392459,  &
     & 310748556, 319235730, 462202514, 321332812, 323331915, 333883407,  &
     & 464299730, 323430028, 325419008, 426418967, 298624089, 306979989,  &
     & 302490637, 306557977, 304882773, 300393421, 302392459, 308651404/

DATA (SYMBCD(J),  J =    2851,   2964)/ &
     & 319235729, 325756633, 323790551,         0, 426418967, 298624089,  &
     & 306979989, 302490637, 306557977, 304882773, 300393421, 302392459,  &
     & 310748556, 319235664, 460105296, 321300108, 327526283, 335947918,  &
     & 342370580, 344762585, 344700697, 323495565, 327516160, 430613464,  &
     & 304915737, 313238868, 443327767, 311043280, 306652172, 298165067,  &
     & 294003469, 296166285, 296105168, 308716811, 317040204, 325564120,  &
     & 323725014, 327919384, 325887641, 319563158, 313140496, 310814027,  &
     &         0, 426418967, 298624089, 306979989, 302490637, 306557977,  &
     & 304882773, 300393421, 302392459, 310748556, 319235730, 464299595,  &
     & 319038853, 308421636, 297968454, 295936904, 300131206, 462202379,  &
     & 316941637, 308412416, 460105367, 319464463, 298230603, 432710615,  &
     & 304915737, 319534039, 304882968, 319530647, 432448525, 310781388,  &
     & 321303565, 310748619, 321300111,         0, 433202052, 435299268,  &
     & 433202532, 432153924,         0, 443688132, 445785348, 431105316,  &
     & 430056708,         0, 447751044, 460334340, 432711445, 430417615,  &
     &         0, 447653148, 313370012, 315532639, 309339232, 300917661,  &
     & 298689497, 304850324, 434939158, 315237842, 317203854, 310785048,  &
     & 298525524, 296297360, 302458187, 432547021, 312845705, 314811717,  &
     & 308421700, 300065671, 298066889, 302261191,         0, 441459806/

DATA (SYMBCD(J),  J =    2965,   3078)/ &
     & 307111134, 307246240, 306328725, 304686212, 308880533, 428647320,  &
     & 302818202, 294433561, 319599897, 315368985, 315434265,         0,  &
     & 434938776, 300655640, 300725197, 298197963, 302392269,         0,  &
     & 434938776, 300655640, 300725195, 298197965, 302392330, 300163975,  &
     &         0, 435168158, 300491806, 300954590, 300692429, 298197963,  &
     & 302392269,         0, 432939995, 298656603, 296625054, 300917856,  &
     & 311436767, 319759964, 321725976, 317433045, 308884768, 315598302,  &
     & 319694362, 317465942, 442934412, 308651276, 308707328, 468722507,  &
     & 441459998, 311305434, 304915417, 296592221, 298820640, 307242271,  &
     & 317662878, 330278880, 459875921, 319268365, 323331851, 331753422,  &
     & 333981522, 325648384, 468461463, 334178327, 336340953, 332179288,  &
     & 327886481, 319235468, 310748235, 298197838, 296264595, 311141785,  &
     & 317564381, 315598112, 307209309, 304981144, 311076430, 325461899,  &
     & 333817868, 335983691, 300295054, 298361811, 304788571, 307013262,  &
     & 327559051,         0, 437035992, 302752856, 302822221, 294003531,  &
     & 298188800, 437035992, 302752856, 302822219, 294003533, 298197899,  &
     & 296002247,         0, 441459807, 300528799, 300528800, 309306323,  &
     & 430351116, 296067980, 296124416, 439231643, 304948251, 302916702,  &
     & 307209568, 321922847, 330213211, 327984856, 313205973, 308913426/

DATA (SYMBCD(J),  J =    3079,   3192)/ &
     & 315176544, 326084381, 328050393, 323757591, 440837196, 306554060,  &
     & 306610176, 430482259, 298525719, 306947350, 319399570, 327755667,  &
     & 334148435, 298492950, 306914581, 319366801, 327722898, 334145495,  &
     &         0, 445784916, 310509568, 433202516, 297926656, 433202052,  &
     &         0, 435168153, 437265305, 451945881, 454043033,         0,  &
     & 323397323, 441131922, 296231758, 298197835, 430449612, 432612240,  &
     & 300360652, 296072531, 323761693, 319628888, 325854938, 321758749,  &
     & 453944922, 325844992, 437265311, 296657755, 298624024, 306980121,  &
     & 313369949, 311403680, 303038464, 464201748, 329856665, 334112399,  &
     & 432678868,         0, 454042756, 456139844, 445424664, 298525523,  &
     & 296231822, 302392523, 314943116, 327624529, 329918230, 323757529,  &
     & 311211289, 304882646, 298427280, 300360780, 308655499, 321267406,  &
     & 327722772, 325789272, 317489152, 443557017, 445654169,         0,  &
     & 306787478, 304751824, 306652240, 308946070, 441001092, 440673350,  &
     & 306324678, 306459417, 298591257, 298656537, 428647961, 445425048,  &
     & 319595930, 311210763, 298132491, 298197771, 428189195, 444966282,  &
     & 319137164, 310738944, 443556895, 298722135, 296362895, 302392523,  &
     & 312845836, 323462868, 325822108, 319792480, 309329920, 437134493,  &
     & 313533771,         0, 432907164, 300885023, 307242400, 319792734/

DATA (SYMBCD(J),  J =    3193,   3306)/ &
     & 323888794, 321660373, 296068811,         0, 435168928, 311174616,  &
     & 321627798, 325691089, 323429900, 312845451, 300295053, 296189952,  &
     & 451945298, 327759328, 317030400, 456139744, 298558424, 307012953,  &
     & 319563414, 325691089, 323429900, 312845451, 300295053, 296189952,  &
     & 458139231, 315630880, 305112028, 298558354, 300360780, 310748491,  &
     & 319170190, 325625554, 323659287, 313271576, 304849877, 298385408,  &
     & 460334155, 430974688,         0, 441459679, 298754971, 300721240,  &
     & 313239062, 323626706, 325559949, 321267083, 306553804, 298230607,  &
     & 296297364, 302720215, 317466201, 323856029, 321889696, 307232768,  &
     & 458008150, 317334803, 308913172, 298525529, 296559517, 303015136,  &
     & 311436767, 321824409, 323626575, 317072651, 306553804, 298254336,  &
     & 451847627, 432678932,         0, 432678932,         0, 466756356,  &
     &         0, 432777239, 432580625,         0, 447882466, 305112027,  &
     & 298525586, 300328009, 308487492,         0, 431104994, 305112283,  &
     & 311108882, 308716617, 300098372,         0, 441263246, 430679505,  &
     & 451650385,         0, 436609995, 298197965, 302392330, 300163975,  &
     &         0, 434545548, 300262412, 300318720, 441590919, 449979783,  &
     & 460236383, 315630752, 300917597, 296592281, 300688471, 317367892,  &
     & 323593937, 325527116, 314942603, 300294990,         0, 443556895/

DATA (SYMBCD(J),  J =    3307,   3420)/ &
     & 298722135, 296362895, 302392523, 312845836, 323462868, 325822108,  &
     & 319792480, 309343456, 305112094, 300819351, 298460111, 302425164,  &
     & 308655435, 317072909, 321365652, 323724892, 319759839, 313524224,  &
     & 437134493, 313533771, 445621515, 436577867,         0, 432939995,  &
     & 298656603, 296625054, 300917920, 315631199, 323954396, 325920408,  &
     & 317400212, 302621585, 296166219, 449848863, 321857180, 323823192,  &
     & 315303060, 430351246, 302458188, 319170189, 325530638, 312845899,  &
     & 323364558, 325582848, 432939995, 298656603, 296625054, 300917920,  &
     & 315631199, 323921562, 321660311, 309048736, 319792733, 321725976,  &
     & 315340183, 319497876, 325658319, 323397196, 314942603, 300295053,  &
     & 296198992, 298361808, 298301013, 323561103, 321299980, 314933248,  &
     & 449783179, 451945931, 451945233, 327726283, 323321856, 435168086,  &
     & 430646232, 307012953, 319563414, 325691089, 323429900, 312845451,  &
     & 300295053, 296198992, 298361808, 298300761, 317466198, 323593873,  &
     & 321332684, 312849376, 321926111, 311404128,         0, 456042012,  &
     & 321758876, 323921503, 317728032, 305112029, 298689367, 296264590,  &
     & 302392523, 312845836, 323430097, 325658261, 319530328, 311174231,  &
     & 300589970, 445654175, 302949339, 298558353, 300360780, 308655435,  &
     & 317072974, 323528338, 321562071, 313262080, 430973786, 430842782/

DATA (SYMBCD(J),  J =    3421,   3534)/ &
     & 303047840, 317630045, 323954400, 433005599, 307209693, 460334813,  &
     & 323822997, 313107728, 310752922, 313173267, 308815051,         0,  &
     & 441459679, 298754970, 300688535, 315336280, 323823261, 321889696,  &
     & 307246240, 303014877, 300753944, 306951575, 319563354, 321824287,  &
     & 315634839, 300622741, 296330063, 298230732, 306554251, 321267341,  &
     & 325560019, 323659350, 315339927, 302719957, 298427279, 300327948,  &
     & 306558347, 319170125, 323462803, 321562134, 315326464, 458008150,  &
     & 317334803, 308913172, 298525529, 296559517, 303015136, 313533983,  &
     & 323921626, 325723792, 321332684, 310748235, 300295054, 298296272,  &
     & 302490574, 443130964, 300622745, 298656733, 305112288, 447751647,  &
     & 321824410, 323626576, 319235468, 310738944, 451847627, 432678932,  &
     &         0, 432678932,         0, 466756356,         0, 432777239,  &
     & 432580625,         0, 447882466, 305112027, 298525586, 300328009,  &
     & 308487492, 443622494, 302883798, 300491789, 304424134,         0,  &
     & 431104994, 305112283, 311108882, 308716617, 300098372, 435233886,  &
     & 307078358, 308880525, 304423878,         0, 441459860, 430876119,  &
     & 451846999,         0, 434480012, 300327948, 302326728, 298024960,  &
     & 434545548, 300262412, 300318720, 441590919, 449979783, 458139228,  &
     & 323856092, 326018655, 315630752, 300917597, 296592281, 300688471/

DATA (SYMBCD(J),  J =    3535,   3648)/ &
     & 317367892, 325661531, 300721240, 317400661, 323626706, 325527116,  &
     & 314942603, 300294990, 296199056, 300393358,         0, 449848543,  &
     & 305046490, 298558291, 296231821, 300295243, 308651404, 319235729,  &
     & 325723928, 328050398, 323986976, 315635104, 311403677, 302851031,  &
     & 298427280, 300328011, 442869068, 317138513, 323626712, 325953182,  &
     & 319815680, 449717323, 454042763, 454042973, 307078170, 451847387,  &
     & 302841856, 439231643, 304948251, 302916702, 307209568, 319825631,  &
     & 328115995, 325887575, 315270291, 300458831, 291878432, 323987165,  &
     & 325953177, 319530131, 428254030, 300360972, 317072973, 323466190,  &
     & 310748619, 321267343,         0, 439231643, 304948251, 302916702,  &
     & 307209568, 319825631, 328115995, 325887511, 313210400, 323987165,  &
     & 325953177, 319534294, 313206293, 321529490, 323462733, 319169867,  &
     & 304456588, 296133391, 294134609, 298328911, 447423957, 319432274,  &
     & 321365517, 317072715,         0, 458204427, 460334411, 460333841,  &
     & 327712768, 443556758, 443557728, 443524639, 330314646, 300655768,  &
     & 313271831, 321595028, 323528270, 317072651, 304456588, 296133391,  &
     & 294134609, 298328911, 447489495, 319497812, 321431054, 314975499,  &
     &         0, 460236444, 325953308, 328115935, 321922464, 309306461,  &
     & 300753815, 296330063, 298230732, 304456971, 317072974, 323495571/

DATA (SYMBCD(J),  J =    3649,   3762)/ &
     & 321562134, 315335895, 304817108, 298399136, 311403677, 302851031,  &
     & 298427278, 300299531, 314975758, 321398356, 319488000, 437265306,  &
     & 464529181, 323822932, 308847759, 304461466, 311043217, 304587787,  &
     & 435070112, 311436893, 437200031, 311404125, 326018846, 330301440,  &
     & 447751327, 305079324, 302818391, 309011862, 323725016, 328017693,  &
     & 326084128, 313537888, 309306526, 305013849, 306947286, 449521239,  &
     & 323757786, 326018719, 319829206, 300589907, 294167310, 296100875,  &
     & 310748684, 321300111, 323561044, 319464854, 443229205, 298427217,  &
     & 296166284, 302363915, 317072909, 321365587, 319455232, 460105367,  &
     & 319464852, 308946005, 302719960, 300786717, 307209568, 319825567,  &
     & 326051612, 327952084, 323528206, 314975435, 302359436, 296166223,  &
     & 298329039, 298267733, 302752795, 305046751, 313538207, 326018776,  &
     & 323626577, 317138252, 308641792, 451847627, 432678932,         0,  &
     & 432678932,         0, 475144708,         0, 432777239, 432580625,  &
     &         0, 456271201, 307176475, 298558290, 296166281, 300098564,  &
     & 447784093, 302818262, 298361740, 300131332,         0, 443688226,  &
     & 313501082, 315303249, 308716618, 298033796, 443688225, 313402711,  &
     & 310977743, 304456583,         0, 445654292, 435070551, 456041431,  &
     &         0, 430285580, 296133516, 298165065, 291733504, 430351116/

DATA (SYMBCD(J),  J =    3763,   3876)/ &
     & 296067980, 296124416, 449979271, 460465351, 462300891, 328017755,  &
     & 330180382, 326084128, 311436383, 300852187, 302818392, 319432338,  &
     & 435004505, 319465044, 323561103, 321299980, 312845387, 298197837,  &
     & 294101776, 296264592, 296189952, 443556895, 298722135, 296362895,  &
     & 302392523, 312845836, 323462868, 325822108, 319792480, 309343327,  &
     & 300819351, 298460111, 304493581, 308684108, 319206860, 321365652,  &
     & 323724892, 317699614, 313500895, 302972928, 437134493, 313533771,  &
     & 437134363, 307111198, 310748491,         0, 432907164, 300885023,  &
     & 307242400, 319792734, 323888794, 321660373, 298169243, 300786652,  &
     & 302982303, 315598366, 321791578, 319563157, 296072076, 325461707,  &
     & 430286539,         0, 435168928, 309048288, 300918367, 456139927,  &
     & 443295064, 319530645, 325658321, 323429900, 312845451, 300295053,  &
     & 296199055, 441165143, 319497875, 449554005, 323561105, 321332620,  &
     & 457713165, 312878220, 300327823, 438707086,         0, 451847627,  &
     & 319141408, 319141408, 296232720, 451847056, 432580369, 327680000,  &
     & 435168151, 437232600, 435168864, 321893407, 321893336, 307012953,  &
     & 319563414, 325691089, 323429900, 312845451, 300295053, 296199055,  &
     & 432776151, 304883032, 319530644, 449586774, 323593873, 321332620,  &
     & 457713165, 312878220, 300327823, 438707086,         0, 454010461/

DATA (SYMBCD(J),  J =    3877,   3990)/ &
     & 323921503, 315630880, 305112028, 298558354, 300360780, 310748491,  &
     & 319170190, 325625554, 323659287, 313271576, 304849877, 456074655,  &
     & 311403614, 441426972, 300655570, 302458060, 434644045, 310781260,  &
     & 319202960, 449193550, 323528338, 321562007, 457811478, 313238807,  &
     & 304817107, 443261973, 300482560, 430974688, 304460640, 296724127,  &
     & 458236939, 304447488, 441459679, 298754971, 300721176, 306947478,  &
     & 319465044, 323561103, 321299852, 306586573, 298296210, 300557333,  &
     & 306914711, 319563353, 323856029, 321889696, 307246111, 300852187,  &
     & 302818456, 315336214, 323626706, 325559949, 321267083, 306553804,  &
     & 298230607, 296297364, 302720151, 315368985, 321758813, 319796830,  &
     & 315597983, 300888974, 304494028, 323420160, 455812564, 311010515,  &
     & 302654358, 296526682, 298755103, 309339424, 317695581, 323790484,  &
     & 321365452, 310748299, 300295054, 300360716, 455910934, 313144920,  &
     & 317367572, 308945941, 298595476, 300622745, 298656733, 307213211,  &
     & 302982367, 311403998, 321762655, 319727193, 321529359, 314979789,  &
     & 310781068, 300318720, 449750412, 317076893, 317629900, 432711637,  &
     & 334115733, 298461140,         0, 432711637, 334115733, 298461140,  &
     &         0, 466756356, 295843748, 334635844,         0, 432842713,  &
     & 334246809, 298592216, 432580561, 333984657, 298330064,         0/

DATA (SYMBCD(J),  J =    3991,   4104)/ &
     & 445785250, 303014811, 296428370, 298230793, 306390276, 312620324,  &
     & 313664738, 305112027, 298525586, 300328009, 308487492,         0,  &
     & 431104994, 305112283, 311108882, 308716617, 300098372, 297939812,  &
     & 298984482, 307209499, 313206098, 310813833, 302195588,         0,  &
     & 441459807, 308978836, 441459860, 441459935, 304784532, 430875549,  &
     & 315336151, 430876119, 430875484, 317466071, 451847581, 298558295,  &
     & 451846999, 451847644, 296493911,         0, 438707211, 300262284,  &
     & 298230734, 302457933, 304423944, 298038221, 300295180, 302425037,  &
     & 436577354, 438707208,         0, 434578317, 298197963, 302359628,  &
     & 304522254, 300364749, 300295180, 302425037,         0, 443688135,  &
     & 310621412, 311567623, 453944989, 319792480, 307241951, 296657755,  &
     & 298623960, 317335059, 321431119, 319202636, 306586637, 300365341,  &
     & 317662559, 307209182, 298754971, 300721621, 321496721, 323462733,  &
     & 319169867, 306553804, 296166350, 455550348,         0, 445653771,  &
     & 445555531, 293975325, 325429003, 445654795, 434677329, 432547472,  &
     &         0, 433070987, 435135436, 433071520, 321889950, 325986009,  &
     & 323724886, 315274207, 315598430, 323888793, 321627542, 434840982,  &
     & 321562260, 325658319, 323397196, 314942347, 434808213, 321529490,  &
     & 323462733, 314975180,         0, 462268125, 321889760, 309339231/

DATA (SYMBCD(J),  J =    4105,   4218)/ &
     & 300852123, 296493907, 298329038, 304489675, 317040204, 325527312,  &
     & 462268123, 323921502, 317695199, 305079259, 298591123, 300426317,  &
     & 308684236, 321300110, 325592848,         0, 433070987, 435135436,  &
     & 433071456, 319792797, 325953304, 327788240, 323429900, 312845195,  &
     & 435135839, 319759965, 323856088, 325691024, 321332749, 312878028,  &
     &         0, 433070987, 435135436, 433071776, 435136159, 324023254,  &
     & 313206101, 434808149, 434513548, 323335051, 323321856, 433070987,  &
     & 435135435, 298169248, 324023263, 323987104, 434840918, 313177045,  &
     & 313163776, 462268125, 321889760, 309339231, 300852123, 296493907,  &
     & 298329038, 304489675, 317040204, 325527312, 327820756, 462268123,  &
     & 323921502, 317695199, 305079325, 300786584, 298427344, 302457933,  &
     & 308684236, 321300110, 325592787, 317302228,         0, 433070987,  &
     & 433071072, 300262283, 462431968, 325429003, 462432011, 434841302,  &
     & 434808533,         0, 433070987, 300266400, 300950475,         0,  &
     & 449848720, 312911052, 304489421, 298328912, 449848800, 317203853,  &
     & 312878283, 304456652, 298230608,         0, 433070987, 300266400,  &
     & 300950475, 462431968, 300562208, 300528791, 325429003, 443262731,  &
     &         0, 433070987, 433071072, 300299212, 323364491, 432383627,  &
     &         0, 433070987, 435004363, 298169307, 314946464, 315045792/

DATA (SYMBCD(J),  J =    4219,   4332)/ &
     & 315045723, 314947419, 329623435, 466626443,         0, 433070987,  &
     & 435069899, 298169309, 327529376, 325531360, 325531360, 328214283,  &
     &         0, 443556959, 300852123, 296493907, 298329038, 304489675,  &
     & 317040204, 325527312, 329885528, 328050397, 321889760, 309343519,  &
     & 305079259, 298591123, 300426317, 310781324, 321300176, 327788312,  &
     & 325953118, 315598111,         0, 433070987, 435135435, 298169248,  &
     & 317728351, 323954396, 325887639, 321594837, 300594143, 317695582,  &
     & 323888793, 321627606, 300613632, 443556959, 300852123, 296493907,  &
     & 298329038, 304489675, 317040204, 325527312, 329885528, 328050397,  &
     & 321889760, 309343519, 305079259, 298591123, 300426317, 310781324,  &
     & 321300176, 327788312, 325953118, 315598111, 449259209, 327464334,  &
     & 317138697,         0, 433070987, 435135435, 298169248, 315631199,  &
     & 323954396, 325887639, 321594773, 300594143, 315598430, 323888793,  &
     & 321627542, 300627221, 323331787, 447391435,         0, 460236383,  &
     & 315630752, 300917597, 296592281, 300688471, 315270676, 321496721,  &
     & 323429965, 314975372, 302425038, 296171229, 321824286, 315597983,  &
     & 300884893, 298689497, 304883094, 319465107, 325625550, 321267083,  &
     & 306553804, 296157184, 441427083, 443524299, 306557728, 321922655,  &
     & 428876575, 321880064, 433070993, 300360780, 310748555, 321267406/

DATA (SYMBCD(J),  J =    4333,   4446)/ &
     & 327722784, 433071072, 300459022, 304522508, 314975821, 323430097,  &
     & 326117152,         0, 428877067, 428876640, 310851360, 326116622,  &
     & 462431499,         0, 428876939, 428876640, 306656736, 306656733,  &
     & 306558429, 327529952, 327628960, 338700046, 475014923,         0,  &
     & 430974603, 325432160, 298854091, 460334752, 296072928, 298165067,  &
     &         0, 428877014, 308651275, 428876640, 311113440, 324019414,  &
     & 460334358, 310738944, 458236747, 460333963, 430974688, 430973791,  &
     & 323990412, 325461707, 430286539,         0, 455910987, 323335769,  &
     & 323790475, 455812568, 313304217, 302785430, 296330065, 298263564,  &
     & 306554187, 317072974, 455812440, 306979863, 300622739, 298361806,  &
     & 302425228, 312878670,         0, 433070987, 300266400, 300950475,  &
     & 434840664, 309110169, 319563414, 325691089, 323429900, 314942667,  &
     & 304489422, 434840792, 315368983, 321595027, 323528270, 319202700,  &
     & 308683726,         0, 455812568, 313304217, 302785430, 296330065,  &
     & 298263564, 306554187, 317072974, 455812629, 317433176, 306979863,  &
     & 300622739, 298361806, 302425228, 312878541, 319268430,         0,  &
     & 456140363, 323335776, 324019851, 455812568, 313304217, 302785430,  &
     & 296330065, 298263564, 306554187, 317072974, 455812440, 306979863,  &
     & 300622739, 298361806, 302425228, 312878670,         0, 432612946/

DATA (SYMBCD(J),  J =    4447,   4560)/ &
     & 321562135, 317465945, 307012632, 298525523, 296264590, 302392459,  &
     & 312845772, 321336211, 319399445, 317433176, 306979863, 300622739,  &
     & 298361806, 302425228, 312878541, 319268430,         0, 447751392,  &
     & 305112092, 302359627, 447751519, 309306462, 441427036, 304460633,  &
     & 311207192, 430744408, 311164928, 458008153, 321201671, 316876101,  &
     & 308454470, 302228359, 458008202, 321103301, 312616068, 302162823,  &
     & 455812568, 313304217, 302785430, 296330065, 298263564, 306554187,  &
     & 317072974, 455812440, 306979863, 300622739, 298361806, 302425228,  &
     & 312878670,         0, 433070987, 300266400, 300950475, 434807960,  &
     & 311207385, 321660565, 323335125, 306947352, 315368983, 321562187,  &
     & 323321856, 433070943, 296690589, 300852254, 303014880, 298857375,  &
     & 298787806, 300917663, 432841611, 300266393, 300721099,         0,  &
     & 433070943, 296690589, 300852254, 303014880, 298857375, 298787806,  &
     & 300917663, 432841604, 300037017, 300721092,         0, 433070987,  &
     & 300266400, 300950475, 458008153, 300398233, 300364946, 319137419,  &
     & 443131531,         0, 433070987, 300266400, 300950475,         0,  &
     & 432841611, 300266393, 300721099, 434807960, 311207385, 321660565,  &
     & 323335125, 306947352, 315368983, 321562187, 323335829, 330049497,  &
     & 340568344, 346728779, 457877335, 334243928, 342599957, 344303947/

DATA (SYMBCD(J),  J =    4561,   4674)/ &
     &         0, 432841611, 300266393, 300721099, 434807960, 311207385,  &
     & 321660565, 323335125, 306947352, 315368983, 321562187, 323321856,  &
     & 441230360, 298525523, 296264590, 302392459, 312845772, 321332881,  &
     & 323593814, 317465945, 307016856, 302752726, 298427281, 300360717,  &
     & 306586956, 317105678, 321431123, 319497687, 313271448,         0,  &
     & 432841604, 300037017, 300721092, 434840664, 309110169, 319563414,  &
     & 325691089, 323429900, 314942667, 304489422, 434840792, 315368983,  &
     & 321595027, 323528270, 319202700, 308683726,         0, 455910980,  &
     & 323106393, 323790468, 455812568, 313304217, 302785430, 296330065,  &
     & 298263564, 306554187, 317072974, 455812440, 306979863, 300622739,  &
     & 298361806, 302425228, 312878670,         0, 432841611, 300266393,  &
     & 300721099, 434742294, 306980121, 317502419, 302687383, 311174616,  &
     & 317489152, 453715416, 311207001, 298591062, 298460179, 313042384,  &
     & 449357263, 317138316, 451323148, 304489357, 434512782, 296171030,  &
     & 317400472, 451650840, 304882583, 434906006, 300561301, 302654802,  &
     & 317236751, 319235532, 310748235, 298197838,         0, 435168203,  &
     & 302363616, 303047691, 428647641, 309080857, 294397144,         0,  &
     & 432841615, 300295243, 310748556, 321368985, 300721103, 302425228,  &
     & 310781325, 321369689, 321234571, 455911065, 323321856, 428647563/

DATA (SYMBCD(J),  J =    4675,   4711)/ &
     & 428647257, 306624025, 317498509, 453813387,         0, 430744715,  &
     & 430744473, 306656665, 306656662, 306558358, 323335577, 323434457,  &
     & 332179086, 468493963,         0, 430745099, 321237849, 298624587,  &
     & 455910937, 296072793, 298165067,         0, 428647563, 428647257,  &
     & 306624025, 317498509, 297940505, 306553796, 297926656, 451683147,  &
     & 455910348, 430745177, 430744408, 317469644, 321267275, 430286411,  &
     &        0/
!
DATA (ISTART(J),  J=1, 229)/ &
     &    1,    5,   16,   26,   34,   39,   43,   54,   58,   60,   66,   70,   73,  &
     &   78,   82,   93,  100,  112,  120,  131,  134,  140,  143,  148,  151,  154,  &
     &  296,  305,  314,  322,  331,  340,  344,  355,  360,  364,  370,  374,  376,  &
     &  385,  390,  399,  408,  417,  421,  430,  434,  439,  442,  447,  450,  455,  &
     & 3177, 3186, 3189, 3197, 3205, 3208, 3217, 3229, 3232, 3247, 3259, 3262, 3264,  &
     & 3266, 3269, 3275, 3281, 3285, 3290, 3293,  &
     &  158,  162,  173,  176,  180,  185,  189,  193,  205,  207,  211,  214,  219,  &
     &  223,  227,  238,  242,  249,  253,  256,  265,  275,  278,  287,  &
     &  459,  471,  486,  494,  506,  515,  526,  535,  549,  554,  563,  567,  577,  &
     &  584,  598,  607,  613,  623,  632,  636,  644,  655,  662,  672,  &
     &  683,  690,  710,  726,  740,  749,  757,  775,  785,  790,  799,  809,  815,  &
     &  826,  834,  855,  868,  898,  918,  935,  942,  952,  958,  967,  975,  983,  &
     & 1272, 1290, 1305, 1319, 1335, 1350, 1360, 1388, 1399, 1406, 1417, 1427, 1432,  &
     & 1450, 1461, 1478, 1494, 1509, 1519, 1535, 1542, 1553, 1559, 1568, 1576, 1585,  &
     & 3306, 3325, 3330, 3351, 3373, 3378, 3396, 3419, 3433, 3462, 3485, 3488, 3490,  &
     & 3492, 3495, 3505, 3515, 3519, 3523, 3526,  &
     &  990,  997, 1017, 1023, 1029, 1038, 1045, 1055, 1080, 1085, 1095, 1101, 1112,  &
     & 1120, 1133, 1154, 1162, 1175, 1183, 1190, 1205, 1226, 1234, 1252,  &
     & 1592, 1611, 1637, 1650, 1671, 1686, 1701, 1716, 1737, 1744, 1757, 1767, 1779/

DATA (ISTART(J),  J=230,  432)/ &
     & 1789, 1810, 1825, 1834, 1849, 1865, 1872, 1887, 1905, 1916, 1932,  &
     & 1953, 1960, 1978, 1995, 2009, 2018, 2026, 2046, 2056, 2061, 2071, 2081, 2087,  &
     & 2098, 2106, 2126, 2138, 2167, 2185, 2202, 2209, 2220, 2226, 2235, 2243, 2251,  &
     & 2522, 2540, 2556, 2568, 2587, 2600, 2617, 2637, 2651, 2663, 2678, 2693, 2701,  &
     & 2725, 2742, 2757, 2776, 2791, 2803, 2817, 2825, 2842, 2855, 2874, 2894, 2913,  &
     & 3546, 3566, 3572, 3592, 3616, 3620, 3638, 3660, 3673, 3702, 3724, 3727, 3729,  &
     & 3731, 3734, 3744, 3754, 3758, 3762, 3765,  &
     & 4074, 4082, 4102, 4121, 4136, 4146, 4154, 4176, 4185, 4189, 4199, 4208, 4214,  &
     & 4224, 4232, 4252, 4264, 4287, 4302, 4323, 4329, 4341, 4347, 4357, 4364, 4371,  &
     & 4379, 4396, 4413, 4429, 4446, 4464, 4474, 4497, 4508, 4519, 4530, 4539, 4543,  &
     & 4562, 4573, 4591, 4608, 4625, 4634, 4656, 4663, 4674, 4680, 4690, 4697, 4704,  &
     & 3784, 3803, 3809, 3825, 3846, 3853, 3876, 3904, 3909, 3941, 3969, 3976, 3980,  &
     & 3984, 3991, 4003, 4015, 4031, 4042, 4050,  &
     & 2258, 2260, 2262, 2283, 2289, 2301, 2305, 2309, 2320, 2336, 2360, 2373, 2377,  &
     & 2381, 2384, 2391, 2399, 2402, 2406, 2415, 2435, 2454, 2473, 2500,  &
     & 2927, 2932, 2937, 2942, 2964, 2977, 2983, 2990, 2997, 3012, 3027, 3051, 3056,  &
     & 3063, 3070, 3086, 3098, 3100, 3102, 3104, 3123, 3130, 3135, 3154/

DATA (WIDTH(J),  J=1, 216)/ &
     & 18., 21., 21., 21., 19., 18., 21., 22.,  8., 16., 21., 17., 24., 22., 22., 21.,  &
     & 22., 21., 20., 16., 22., 18., 24., 20., 18., 20.,  &
     & 19., 19., 18., 19., 18., 12., 19., 19.,  8., 10., 17.,  8., 30., 19., 19., 19.,  &
     & 19., 13., 17., 12., 19., 16., 22., 17., 16., 17.,  &
     & 20., 20., 20., 20., 20., 20., 20., 20., 20., 20., 26., 26., 22., 26., 14., 14.,  &
     & 16., 10., 10., 20.,  &
     & 18., 21., 17., 18., 19., 20., 22., 22.,  8., 21., 18., 24., 22., 18., 22., 22.,  &
     & 21., 18., 16., 18., 20., 20., 22., 20.,  &
     & 21., 19., 19., 18., 16., 15., 20., 21., 11., 18., 16., 21., 18., 16., 17., 22.,  &
     & 18., 20., 20., 20., 22., 18., 23., 23.,  &
     & 20., 22., 21., 22., 21., 20., 23., 24., 11., 15., 22., 18., 25., 23., 22., 22.,  &
     & 22., 22., 20., 19., 24., 20., 24., 20., 21., 20.,  &
     & 20., 21., 19., 21., 19., 13., 19., 22., 11., 11., 21., 11., 33., 22., 20., 21.,  &
     & 20., 17., 17., 15., 22., 18., 24., 20., 19., 18.,  &
     & 20., 20., 20., 20., 20., 20., 20., 20., 20., 20., 26., 26., 22., 26., 14., 14.,  &
     & 16., 10., 10., 20.,  &
     & 20., 22., 18., 20., 21., 20., 24., 22., 11., 22., 20., 25., 23., 22., 22., 24.,  &
     & 22., 21., 19., 19., 21., 20., 23., 22./

DATA (WIDTH(J),  J= 217,  432)/ &
     & 23., 21., 20., 19., 18., 18., 22., 23., 12., 20., 20., 23., 20., 17., 18., 22.,  &
     & 19., 21., 20., 20., 22., 18., 23., 23.,  &
     & 20., 24., 21., 23., 23., 22., 22., 26., 13., 18., 23., 20., 27., 25., 22., 23.,  &
     & 22., 24., 23., 21., 25., 20., 26., 22., 21., 22.,  &
     & 21., 19., 18., 21., 18., 15., 20., 21., 13., 13., 20., 12., 33., 23., 18., 21.,  &
     & 20., 17., 17., 14., 23., 20., 29., 20., 21., 20.,  &
     & 21., 21., 21., 21., 21., 21., 21., 21., 21., 21., 26., 26., 22., 26., 15., 15.,  &
     & 17., 11., 11., 21.,  &
     & 20., 20., 21., 21., 19., 18., 21., 22.,  9., 17., 21., 17., 24., 22., 22., 20.,  &
     & 22., 20., 20., 17., 22., 20., 26., 20., 19., 20.,  &
     & 20., 20., 18., 20., 18., 14., 20., 20.,  9.,  9., 19.,  9., 31., 20., 19., 20.,  &
     & 20., 14., 17., 11., 20., 16., 24., 18., 16., 18.,  &
     & 20., 20., 20., 20., 20., 20., 20., 20., 20., 20., 25., 25., 23., 25., 14., 14.,  &
     & 16., 11., 11., 19.,  &
     & 24., 24., 19., 20., 17., 24., 24., 25., 24., 24., 25., 24., 24., 22., 26., 34.,  &
     & 10., 22., 31., 19., 14., 14., 27., 22.,  &
     & 14., 14., 21., 16., 16., 10., 10., 10., 18., 24., 25., 11., 11., 11., 21., 24.,  &
     & 14., 14.,  8., 16., 14., 26., 22.,  8./

DATA (SSYMBC(J),  J=1, 120)/ &
     &            471149226, 357246358, 315959338, 336592896, 470820906,  &
     & 345320100, 357443862, 327886236, 315762474, 336920576, 470820906,  &
     & 355313115, 336920576, 470493226, 449850016, 0, 455911911, 456370649, 0,  &
     & 471149216, 336274848, 336930848, 0, 470493226, 357574048, 336920576,  &
     & 449522346, 315959958, 0, 470820906, 355641947, 336274907, 317892650, 0,  &
     & 456370208, 336279584, 351502336, 481470811, 325953253, 347256234,  &
     & 326284694, 325958294, 346929184, 357892096, 449850016, 470493226,  &
     & 455911911, 485271143, 0, 450177706, 315304598, 315949056, 470493226, 0,  &
     & 470820906, 355313115, 336935525, 336274917, 355631104, 470853600,  &
     &            336570464, 336625664, 468592477, 328181537, 330409956,  &
     & 338831587, 345024799, 342796380, 334364672, 466265814, 319563163,  &
     & 313468258, 315794984, 326444971, 341158250, 353643173, 359738078,  &
     & 357411352, 346761365, 332038144, 465905227, 312910991, 300491605,  &
     & 292332190, 290530023, 297116654, 307799411, 322611126, 341518837,  &
     & 360295345, 372714731, 380874146, 382676313, 376089682, 365406925,  &
     & 350595210, 331677696, 468592477, 328181537, 330409956, 338831587,  &
     & 345024799, 342796380, 334378847, 330344289, 466560930, 468625379,  &
     & 470722595, 472819811, 474949794, 477079777, 0, 462300964, 345123100,  &
     & 328087389, 330413981, 332511197, 334608413, 336705629, 338802845/

DATA (SSYMBC(J), J=121, 128)/ 340900061, 342982656, 470623971, 347187226, 464594973, 342964256, 334571552, 338755584/

DATA ISSTAR /1, 5, 11, 14, 17, 20, 24, 27, 30, 35, 38, 45, 50, 53, 55, 60, 63, 70, 81, 98, 113, 123/
!----------------------------------------------------------------------------------------------------------------------------------!
REAL,PARAMETER :: &
   PI_D               = 3.14159265358979323846264338327950288419716939937510D0,   &
   DEGREES_TO_RADIANS = PI_D / 180.0D+00
   INTERFACE D2R
      MODULE PROCEDURE D2R_D
      MODULE PROCEDURE D2R_R
      MODULE PROCEDURE D2R_I
   END INTERFACE
!----------------------------------------------------------------------------------------------------------------------------------!
CONTAINS
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      rect(3f) - [M_pixel:POLYGON] draw rectangle given two corners
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine rect(x1,y1, x2,y2)
!!    real,intent(in) :: x1,y1,x2,y2
!!
!!##DESCRIPTION
!!    Draw rectangle given two opposite corners.
!!
!!##OPTIONS
!!    X1,Y1  coordinates of a corner of the rectangle
!!    X2,Y2  coordinates of corner point opposite first point
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rect
!!    use M_pixel
!!    use M_writegif, only : writegif
!!    implicit none
!!       integer :: i
!!
!!       !! set up graphics area
!!       call prefsize(400,400)
!!       call vinit()
!!       call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)
!!
!!       !! draw some filled rectangles
!!       do i=95,5,-10
!!          call makepoly()
!!          call color(i/10)
!!          call rect( -1.0*i, -1.0*i, 1.0*i, 1.0*i )
!!          call closepoly()
!!       enddo
!!
!!       !! draw some rectangles
!!       call linewidth(50)
!!       call color(7)
!!       do i=5,95,5
!!          call rect( -1.0*i, -1.0*i, 1.0*i, 1.0*i )
!!       enddo
!!
!!       !! render pixel array to a file
!!       call writegif('rect.3m_pixel.gif',P_pixel,P_colormap)
!!
!!       !! display graphic assuming display(1) is available
!!       call execute_command_line('display rect.3m_pixel.gif')
!!
!!       !! wrap up graphics
!!       call vexit()
!!
!!    end program demo_rect
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE RECT(X1,Y1, X2,Y2)

! ident_1="@(#)M_pixel::rect(3f): draw line rectangle given two opposite corners"

!
!  x1,y1 ############ x2,y1
!        #          #
!        #          #
!        #          #
!  x1,y2 ############ x2,y2
!

REAL,INTENT(IN)            :: X1,Y1,X2,Y2

   CALL MOVE2(X1,Y1)
   CALL DRAW2(X1,Y2)
   CALL DRAW2(X2,Y2)
   CALL DRAW2(X2,Y1)
   CALL DRAW2(X1,Y1)

END SUBROUTINE RECT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      line(3f) - [M_pixel:DRAW] draw line between two points
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine line(x1,y1, x2,y2 )
!!    real,intent(in)            :: x1,y1,x2,y2
!!
!!##DESCRIPTION
!!    Draw line between two points using current line width and color
!!
!!##OPTIONS
!!    X1,Y1  starting point for line segment
!!    X2,Y2  end point for line segment
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE LINE(X1,Y1, X2,Y2 )

! ident_2="@(#)M_pixel::line(3f): draw line between two points applying line width and color"

REAL,INTENT(IN)  :: X1,Y1,X2,Y2

REAL             :: XX1,YY1,XX2,YY2
INTEGER          :: I
INTEGER          :: IX1,IY1,IX2,IY2

   P_X=X2                                                  ! update current position
   P_Y=Y2

   IF(P_DEBUG)WRITE(*,*)'linewidth ',P_WIDTH,';move2 ',X1,Y1,';draw2 ',X2,Y2
!-----------------------------------------------------------------------------------------------------------------------------------
! allow collecting points in a continuous polyline into a polygon as a first cut using makepoly(3f) and closepoly(3f)
! assuming all line drawing goes thru this routine, and using fixed size array
   IF(P_INPOLYGON)THEN
      IF(P_POLYVERTEX.GT.P_MAXVERTS)THEN
         WRITE(*,*)'*P_line* exceeded limit on number of points in a polygon (',P_MAXVERTS,')'
      ELSE
         IF(P_POLYVERTEX.EQ.1)THEN
            P_POLYPOINTS(1,1)=X1
            P_POLYPOINTS(2,1)=Y1
            P_POLYVERTEX=P_POLYVERTEX+1
         ENDIF
         P_POLYPOINTS(1,P_POLYVERTEX)=X2
         P_POLYPOINTS(2,P_POLYVERTEX)=Y2
         P_POLYVERTEX=P_POLYVERTEX+1
      ENDIF
      RETURN
   ENDIF
!-----------------------------------------------------------------------------------------------------------------------------------

   CALL WORLD2VIEWPORT(X1,Y1,XX1,YY1)                      ! convert from world coordinates to pixel array addRESSES
   CALL WORLD2VIEWPORT(X2,Y2,XX2,YY2)

   IX1=NINT(XX1)                                        ! change values to integers
   IY1=NINT(YY1)
   IX2=NINT(XX2)
   IY2=NINT(YY2)

   SELECT CASE(P_WIDTH)
   CASE(:1)
      CALL DRAW_LINE_SINGLE(IX1, IY1 , IX2, IY2 )             ! draw line
   CASE(2:5)
      DO I=1,P_WIDTH/2                                        ! thicken line NEEDS BETTER METHOD
         CALL DRAW_LINE_SINGLE(IX1+I, IY1  , IX2+I, IY2  )
         CALL DRAW_LINE_SINGLE(IX1  , IY1+I, IX2  , IY2+I)
      ENDDO

      DO I=1,(P_WIDTH-1)/2
         CALL DRAW_LINE_SINGLE(IX1-I, IY1  , IX2-I, IY2  )
         CALL DRAW_LINE_SINGLE(IX1  , IY1-I, IX2  , IY2-I)
      ENDDO
   CASE(6:)
      CALL PPM_DRAW_THICK_LINE(IX1, IY1, IX2, IY2)
   END SELECT

END SUBROUTINE LINE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE SWAPCOORD(P1, P2)

! ident_3="@(#)M_pixel::swapcoor(3fp): swap two coordinates (integers)"

    INTEGER, INTENT(INOUT) :: P1, P2
    INTEGER :: T
    T = P2
    P2 = P1
    P1 = T
END SUBROUTINE SWAPCOORD
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      draw_line_single(3fp) - [M_pixel:LINE] Bresenham's line algorithm
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine draw_line_single(x1,y1, x2,y2)
!!    integer,intent(in)            :: x1,y1,x2,y2
!!
!!##DESCRIPTION
!! From Wikipedia, the free encyclopedia
!!
!! The Bresenham line algorithm is an algorithm that determines which
!! points in an n-dimensional raster should be plotted in order to form
!! a close approximation to a straight line between two given points. It
!! is commonly used to draw lines on a computer screen, as it uses only
!! integer addition, subtraction and bit shifting all of which are very
!! cheap operations in standard computer architectures. It is one of the
!! earliest algorithms developed in the field of computer graphics.
!!
!! Through a minor expansion, the original algorithm for lines can also
!! be used to draw circles. Also this can be done with simple arithmetic
!! operations; quadratic or trigonometric expressions can be avoided or
!! recursively dissolved into simpler steps.
!!
!! The mentioned properties make it still an important algorithm, and it
!! is used among others in plotters, in graphics chips of modern graphics
!! cards, and in many graphics libraries. As it is so simple, it is not
!! only implemented in the firmware of such devices, but is also cast into
!! hardware of those graphics chips.
!!
!! To be precise, the label "Bresenham" is today often used for a whole
!! family of algorithms, which have actually been developed by others, later,
!! yet in succession of Bresenham and with a similar basic approach. See
!! deeper references below.
!!
!!##CONTENTS
!!
!!   * 1 The algorithm
!!   * 2 Generalization
!!   * 3 Optimization
!!   * 4 Different approach to the algorithm
!!       + 4.1 Generalized version for this approach
!!   * 5 Circle Variant
!!       + 5.1 Drawing incomplete octants
!!       + 5.2 Ellipses
!!   * 6 History
!!   * 7 Similar Algorithms
!!   * 8 References
!!   * 9 See also
!!   * 10 External links
!!
!! The common conventions that pixel coordinates increase in the down and
!! right directions and that pixel centers have integer coordinates will
!! be used. The endpoints of the line are the pixels at (x[0], y[0]) and
!! (x[1], y[1]), where the first coordinate of the pair is the column and
!! the second is the row.
!!
!! The algorithm will be initially presented only for the octant in which
!! the segment goes down and to the right (x[0]?x[1] and y[0]?y [1] ) ,
!! and its horizontal projection x[1] ? x[0] is longer than the vertical
!! projection y[1] ? y[0] (in other words, the line has a slope less
!! than 1 and greater than 0.) In this octant, for each column x between
!! x[0] and x[1], there is exactly one row y (computed by the algorithm)
!! containing a pixel of the line, while each row between y[0] and y[1]
!! contains multiple rasterized pixels.
!!
!! Bresenham's algorithm chooses the integer y corresponding to the pixel
!! center that is closest to the ideal (fractional) y for the same x; on
!! successive columns y can remain the same or increase by 1. The general
!! equation of the line through the endpoints is given by:
!!
!!     y - y_0 = \frac{y_1-y_0}{x_1-x_0} (x-x_0).
!!
!! Since we know the column, x, the pixel's row, y, is given by rounding
!! this quantity to the nearest integer:
!!
!!     \frac{y_1-y_0}{x_1-x_0} (x-x_0) + y_0.
!!
!! The slope (y[1] ? y[0]) / (x[1] ? x[0]) depends on the endpoint
!! coordinates only and can be precomputed, and the ideal y for successive
!! integer values of x can be computed starting from y[0] and repeatedly
!! adding the slope.
!!
!! In practice, the algorithm can track, instead of possibly large y values,
!! a small error value between ?0.5 and 0.5: the vertical distance between
!! the rounded and the exact y values for the current x. Each time x is
!! increased, the error is increased by the slope; if it exceeds 0.5, the
!! rasterization y is increased by 1 (the line continues on the next lower
!! row of the raster) and the error is decremented by 1.0.
!!
!! In the following pseudocode sample plot(x,y) plots a point and abs
!! returns absolute value:
!!
!!
!!  function line(x0, x1, y0, y1)
!!      int deltax := x1 - x0
!!      int deltay := y1 - y0
!!      real error := 0
!!      real deltaerr := deltay / deltax    // Assume deltax != 0 (line is not vertical)
!!      int y := y0
!!      for x from x0 to x1
!!          plot(x,y)
!!          error := error + deltaerr
!!          if abs(error) ? 0.5 then
!!              y := y + 1
!!              error := error - 1.0
!!
!!##GENERALIZATION
!!
!! This first version only handles lines that descend to the right. We
!! would of course like to be able to draw all lines. The first case is
!! allowing us to draw lines that still slope downwards but head in the
!! opposite direction. This is a simple matter of swapping the initial
!! points if x0 > x1. Trickier is determining how to draw lines that go
!! up. To do this, we check if y[0] ? y[1]; if so, we step y by -1 instead
!! of 1. Lastly, We still need to generalize the algorithm to drawing lines
!! in all directions. Up until now we have only been able to draw lines with
!! a slope less than one. To be able to draw lines with a steeper slope,
!! we take advantage of the fact that a steep line can be reflected across
!! the line y=x to obtain a line with a small slope. The effect is to switch
!! the x and y variables throughout, including switching the parameters to
!! plot. The code looks like this:
!!
!!  function line(x0, x1, y0, y1)
!!      boolean steep := abs(y1 - y0) > abs(x1 - x0)
!!      if steep then
!!          swap(x0, y0)
!!          swap(x1, y1)
!!      if x0 > x1 then
!!          swap(x0, x1)
!!          swap(y0, y1)
!!      int deltax := x1 - x0
!!      int deltay := abs(y1 - y0)
!!      real error := 0
!!      real deltaerr := deltay / deltax
!!      int ystep
!!      int y := y0
!!      if y0 < y1 then ystep := 1 else ystep := -1
!!      for x from x0 to x1
!!          if steep then plot(y,x) else plot(x,y)
!!          error := error + deltaerr
!!          if error ? 0.5 then
!!              y := y + ystep
!!              error := error - 1.0
!!
!! The function now handles all lines and implements the complete Bresenham's
!! algorithm. A more standard C code for the algorithm is shown here:
!!
!!    void Bresenham(int x1, int y1, int x2, int y2) {
!!             int slope;
!!             int dx, dy, incE, incNE, d, x, y;
!!             // Reverse lines where x1 > x2
!!             if (x1 > x2)
!!             {
!!                 Bresenham(x2, y2, x1, y1);
!!                 return;
!!             }
!!             dx = x2 - x1;
!!             dy = y2 - y1;
!!             // Adjust y-increment for negatively sloped lines
!!             if (dy < 0)
!!             {
!!                 slope = -1;
!!                 dy = -dy;
!!             }
!!             else
!!             {
!!                 slope = 1;
!!             }
!!             // Bresenham constants
!!             incE = 2 * dy;
!!             incNE = 2 * dy - 2 * dx;
!!             d = 2 * dy - dx;
!!             y = y1;
!!             // Blit
!!             for (x = x1; x <= x2; x++)
!!             {
!!                 putpixel(x, y);
!!                 if (d <= 0)
!!                 {
!!                     d += incE;
!!                 }
!!                 else
!!                 {
!!                     d += incNE;
!!                     y += slope;
!!                 }
!!             }
!!         }
!!
!!##OPTIMIZATION
!!
!! The problem with this approach is that computers operate relatively
!! slowly on fractional numbers like error and deltaerr; moreover, errors
!! can accumulate over many floating-point additions. Working with integers
!! will be both faster and more accurate. The trick we use is to multiply
!! all the fractional numbers above by deltax, which enables us to express
!! them as integers. The only problem remaining is the constant 0.5?to deal
!! with this, we change the initialization of the variable error. The new
!! program looks like this:
!!
!!  function line(x0, x1, y0, y1)
!!      boolean steep := abs(y1 - y0) > abs(x1 - x0)
!!      if steep then
!!          swap(x0, y0)
!!          swap(x1, y1)
!!      if x0 > x1 then
!!          swap(x0, x1)
!!          swap(y0, y1)
!!      int deltax := x1 - x0
!!      int deltay := abs(y1 - y0)
!!      int error := -deltax / 2
!!      int ystep
!!      int y := y0
!!      if y0 < y1 then ystep := 1 else ystep := -1
!!      for x from x0 to x1
!!          if steep then plot(y,x) else plot(x,y)
!!          error := error + deltay
!!          if error > 0 then
!!              y := y + ystep
!!              error := error - deltax
!!
!!##DIFFERENT APPROACH TO THE ALGORITHM
!!
!! A different approach to the Bresenham algorithm works more from the
!! practical side. It was published by Pitteway ^[1] and confirmed by van
!! Aken ^[2]. Again we first consider a line in the first octant, which
!! means a slope between 0 and 1. Mathematically spoken, we want to draw
!! a line from point (x[1],y[1]) to (x[2],y[2]). The intervals in the
!! two directions are dx=x[2]-x[1] and dy=y[2]-y [1], and the slope is
!! dy/dx. The line equation can be written as y=y[1]+(x-x[1])*dy/dx. In
!! this first octant, we have 0<dy<=dx.
!!
!! So, when working pixel-wise along this line, we have one "fast"
!! direction, the positive x direction, and a "slow" direction, the positive
!! y direction, where fewer steps have to be done than in the fast one. So
!! the algorithm simply goes like this: a) Always do a single pixel step
!! in the fast direction. b) Every now and then also do a step in the
!! slow direction.
!!
!! Bresenham's trick is the introduction of an error term, which deals with
!! the decision, when to do also this extra step in the slow direction. The
!! line equation is transformed into 0=dx*(y-y[1])-dy*(x-x[1]), and then
!! the null on the left side is replaced by the error term. A step by 1 in
!! the x direction (variable x) causes a decrement of the error term by
!! one times dy. If the error term gets below zero due to this, it will
!! be increased by one times dx through a step by 1 in the y direction
!! (variable y). Because of dx>=dy, this will render the error term positive
!! again in any case, at least brought back to zero.
!!
!! You realize a cross-wise subtraction of dy from the error term for any x
!! step and an addition of dx for any y step. This way, the division dy/dx
!! for the slope is dissolved into a number of more elementary operations.
!!
!! A critical issue is the initialisation of the error term. In this approach
!! here, we simply consider a line with dy=1, so with only one single step
!! in the y direction along the whole line. Of course for the best look
!! of the line, we want this step to happen right in the middle of the
!! line. This leads to initialising the error term to dx/2. (Rounding this
!! term to integers in case of odd dx is no problem.)
!!
!! This approach comes out a little different from the original, as it
!! avoids the additional factor of 2 on both sides, which has to do with
!! the initialisation.
!!
!! To generalize this algorithm for all octants, you will again have to do
!! role changes of x and y and consider the different signs of dx and dy.
!!
!! A simple implementation of this approach is not very elegant, but
!! demonstrates the principle of the algorithm fairly well.
!!
!!    REM Bresenham algorithm for a line in the first octant in Pseudo Basic
!!    dx = xend-xstart
!!    dy = yend-ystart
!!    REM in first octant, we have 0 < dy <= dx
!!
!!    REM Initialisations
!!    x = xstart
!!    y = ystart
!!    SETPIXEL x,y
!!    error = dx/2
!!
!!    REM Pixel loop: always do a step in fast direction, every now and then also one in the slow direction
!!    WHILE x < xend
!!       REM Step in fast direction
!!       x = x + 1
!!       error = error-dy
!!       IF error < 0 THEN
!!          REM Step in slow direction
!!          y = y + 1
!!          error = error + dx
!!          ENDIF
!!       SETPIXEL x,y
!!       WEND
!!
!!##GENERALIZED VERSION FOR THIS APPROACH
!!
!! This generalized version in BASIC shall be valid for all octants. For
!! this, all signs of the coordinate distances have to be considered, as
!! well as the possible role change of x and y. If these if clauses would
!! all be put into the innermost loop, which would mean a high number of
!! executions, it would considerably increase the time consumption. A more
!! efficient solution tries to put all these case differentiations into the
!! initialisation phase of the procedure before the start of the inner main
!! loop. Then the inner loop will still contain a single if clause for the
!! Bresenham error term.
!!
!! This version in BASIC introduces a number of abstractions: First the step
!! in the "fast" direction is now considered a parallel step (parallel to
!! one of the coordinate axis), and if additionally a step in the "slow"
!! direction becomes necessary, it becomes a diagonal step. For these cases
!! we can compute variable values during initialisation, in advance, which
!! contain the step widths (including signs) in the coordinate directions
!! and thus achieve the generalization for the eight octants. For example
!! the step width in perpendicular direction to a parallel step is just
!! zero. Secondly the error term is still computed like in the first octant
!! by using the absolute values of the distances. In the innermost loop,
!! no more the step in the fast direction is executed first, but the error
!! term is updated, and only after that the step widths are added to the
!! current coordinate values, depending on whether a parallel or a diagonal
!! step has to be done:
!!
!!    REM Bresenham algorithm for a line in an arbitrary octant in pseudo Basic
!!    dx = xend-xstart
!!    dy = yend-ystart
!!
!!    REM Initialisations
!!    adx = ABS(dx): ady = ABS(dy) ' Absolute values of distances
!!    sdx = SGN(dx): sdy = SGN(dy) ' Signum of distances
!!
!!    IF adx > ady THEN
!!      ' x is fast direction
!!      pdx = sdx: pdy = 0   ' pd. is parallel step
!!      ddx = sdx: ddy = sdy ' dd. is diagonal step
!!      ef  = ady: es  = adx ' error steps fast, slow
!!                 ELSE
!!      ' y is fast direction
!!      pdx = 0  : pdy = sdy ' pd. is parallel step
!!      ddx = sdx: ddy = sdy ' dd. is diagonal step
!!      ef  = adx: es  = ady ' error steps fast, slow
!!      ENDIF
!!
!!    x = xstart
!!    y = ystart
!!    SETPIXEL x,y
!!    error = es/2
!!
!!    REM Pixel loop: always a step in fast direction, every now and then also one in slow direction
!!    FOR i=1 TO es          ' es also is the count of pixels zo be drawn
!!       REM update error term
!!       error = error - ef
!!       IF error < 0 THEN
!!          error = error + es ' make error term positive (>=0) again
!!          REM step in both slow and fast direction
!!          x = x + ddx: y = y + ddy ' Diagonal step
!!                    ELSE
!!          REM step in fast direction
!!          x = x + pdx: y = y + pdy ' Parallel step
!!          ENDIF
!!       SETPIXEL x,y
!!       NEXT
!!
!!##RASTERIZATION OF A CIRCLE BY THE BRESENHAM ALGORITHM
!!
!! The approach for the Circle Variant shown here is also not originally
!! from Bresenham, see again references to Pitteway and van Aken below. The
!! algorithm starts accordingly with the circle equation x?+y?=r?. Again
!! we consider first only the first octant. Here you want to draw a curve
!! which starts at point (r,0) and then proceeds to the top left, up to
!! reaching the angle of 45?.
!!
!! The "fast" direction here is the y direction. You always do a step in
!! the positive y direction (upwards), and every now and then you also have
!! to do a step in the "slow" direction, the negative x direction.
!!
!! The frequent computations of squares in the circle equation, trigonometric
!! expressions or square roots can again be avoided by dissolving everything
!! into single steps and recursive computation of the quadratic terms from
!! the preceding ones.
!!
!! From the circle equation you get to the transformed equation
!! 0=x?+y?-r? with r? to be computed only a single time during
!! initialisation, x?=(xpreceding-1)?=xpreceding?-2*xpreceding+1 (according
!! for y), where x? (or xpreceding?) is kept as an own variable. Additionally
!! you need to add the mid point coordinates when setting a pixel. These
!! frequent integer additions do not limit the performance much, as you
!! spare those square (root) computations in the inner loop in turn. Again
!! the zero in the transformed circle equation is replaced by the error term.
!!
!! The initialization of the error term is derived from an offset of ? pixel
!! at the start. Until the intersection with the perpendicular line, this
!! leads to an accumulated value of r in the error term, so that this value
!! is used for initialisation.
!!
!! The following implementation is shown here only for the first octant,
!! and again the other octants need sign changes for x and/or y and the
!! swapping of x and y. An easy expansion for full circles, as it is possible
!! for graphics displays, but not for plotters, is added in the comments.
!!
!!
!!    REM Bresenham Algorithm for one eighth of a circle in Pseudo-Basic
!!    REM given: r, xmid, ymid
!!    REM initialisations for the first octant
!!    r2 = r*r : REM single multiplication
!!    x = r
!!    y = 0
!!    error = r
!!    SETPIXEL xmid + x, ymid + y
!!
!!    REM Pixel loop: always a step in fast direction, every now and then also in slow one
!!    WHILE y <= x
!!       REM step in fast direction (positive y direction)
!!       dy = y*2+1 : REM in Assembler implementation *2 per Shift
!!       y = y+1
!!       error = error-dy
!!       IF error<0 THEN
!!          REM step in slow direction (here the negative x direction)
!!          dx = 1-x*2 : REM in Assembler implementation *2 per Shift
!!          x = x-1
!!          error = error-dx
!!          ENDIF
!!       SETPIXEL  xmid+x, ymid+y
!!       REM If this deals with a screen and not a mechanical plotter,
!!       REM you can cover simultaneously also the other octants:
!!       REM SETPIXEL xmid-x, ymid+y
!!       REM SETPIXEL xmid-x, ymid-y
!!       REM SETPIXEL xmid+x, ymid-y
!!       REM SETPIXEL xmid+y, ymid+x
!!       REM SETPIXEL xmid-y, ymid+x
!!       REM SETPIXEL xmid-y, ymid-x
!!       REM SETPIXEL xmid+y, ymid-x
!!       WEND
!!
!! A possible implementation of the Bresenham Algorithm for a full circle
!! in C. Here another variable for recursive computation of the quadratic
!! terms is used, which corresponds with the term 2*n+1 above. It just has
!! to be increased by 2 from one step to the next:
!!
!!  void rasterCircle(int x0, int y0, int radius)
!!  {
!!    int f = 1 - radius;
!!    int ddF_x = 0;
!!    int ddF_y = -2 * radius;
!!    int x = 0;
!!    int y = radius;
!!
!!    setPixel(x0, y0 + radius);
!!    setPixel(x0, y0 - radius);
!!    setPixel(x0 + radius, y0);
!!    setPixel(x0 - radius, y0);
!!
!!    while(x < y)
!!    {
!!      if(f >= 0)
!!      {
!!        y--;
!!        ddF_y += 2;
!!        f += ddF_y;
!!      }
!!      x++;
!!      ddF_x += 2;
!!      f += ddF_x + 1;
!!      setPixel(x0 + x, y0 + y);
!!      setPixel(x0 - x, y0 + y);
!!      setPixel(x0 + x, y0 - y);
!!      setPixel(x0 - x, y0 - y);
!!      setPixel(x0 + y, y0 + x);
!!      setPixel(x0 - y, y0 + x);
!!      setPixel(x0 + y, y0 - x);
!!      setPixel(x0 - y, y0 - x);
!!    }
!!  }
!!
!! Note: There is correlation between this algorithm and the sum of first
!! N odd numbers. Which this one basically does. Sum of N odd numbers, from
!! 1 inclusive, is equal to the square of N ( N squared). See Square number.
!!
!!  So.
!!  When we compare sum of N odd numbers to this algorithm we have.
!!  ddF_y = -2 * radius       is connected to last member of of sum of N odd numbers.
!!                            This member has index equal to value of radius (integral).
!!                            Since odd number is 2*n + 1 there is 1 handled elsewhere
!!                            or it should be -2*radius - 1
!!  ddF_x = 0                 should be 1. Because difference between two consecutive odd numbers is 2.
!!                            If so f += ddF_y + 1 is f+= ddF_y. Saving one operation.
!!  f = - radius + 1          Initial error equal to half of "bigger" step.
!!                            In case of saving one addition it should be either -radius or -radius + 2.
!!  In any case there should be addition of 1 driven out of outer loop.
!!  So.
!!  f += ddF_y                Adding odd numbers from Nth to 1st.
!!  f += ddF_x                Adding odd numbers from 1st to Nth. 1 is missing because it can be moved outside of loop.
!!
!!##DRAWING INCOMPLETE OCTANTS
!!
!! The implementations above always only draw complete octants or circles. If
!! you want to draw only a certain arc from an angle ? to an angle ?, you
!! have to implement it in a way to first calculate the x and y coordinates
!! of these end points, where you inevitably have to resort to trigonometric
!! or square root computations (see Methods of computing square roots). Then
!! you run the Bresenham algorithm over the complete octant or circle
!! and set the pixels only if they fall into the wanted interval. After
!! finishing this arc, you can abort the algorithm prematurely.
!!
!!##ELLIPSES
!!
!! By scaling the drawn x and y values (and horizontal or vertical line
!! expansion, respectively) you can produce even ellipses parallel to the
!! x or y axis. For this, you use the circle algorithm with the smaller
!! ellipse axis as radius and add a value in the other direction, which
!! again is computed through another Bresenham line algorithm increasing
!! from the pole to the equator. As the ellipse has to be elongated into
!! the longer axis direction, you don't set single pixels anymore, but
!! have to draw lines (though simple ones, only horizontal or vertical)
!! from the previous to the next point.
!!
!! A general ellipse can be derived from such an axis-parallel one by
!! application of a shearing operation on it. Again you use an additional
!! Bresenham line algorithm to compute the offset increasing in one of the
!! axis directions and to let it contribute to every drawn coordinate.
!!
!!##HISTORY
!!
!! The algorithm was developed by Jack E. Bresenham in 1962 at IBM. In 2001
!! Bresenham wrote:
!!
!!     "I was working in the computation lab at IBM's San Jose development
!!     lab. A Calcomp plotter had been attached to an IBM 1401 via the
!!     1407 typewriter console. [The algorithm] was in production use by
!!     summer 1962, possibly a month or so earlier. Programs in those days
!!     were freely exchanged among corporations so Calcomp (Jim Newland and
!!     Calvin Hefte) had copies. When I returned to Stanford in Fall 1962,
!!     I put a copy in the Stanford comp center library.
!!
!!     A description of the line drawing routine was accepted for
!!     presentation at the 1963 ACM national convention in Denver,
!!     Colorado. It was a year in which no proceedings were published, only
!!     the agenda of speakers and topics in an issue of Communications of
!!     the ACM. A person from the IBM Systems Journal asked me after I made
!!     my presentation if they could publish the paper. I happily agreed,
!!     and they printed it in 1965."
!!
!! Bresenham later modified his algorithm to produce circles.
!!
!!##SIMILAR ALGORITHMS
!!
!! The principle of using an incremental error in place of division
!! operations has other applications in graphics. It is possible to use
!! this technique to calculate the U,V co-ordinates during raster scan of
!! texture mapped polygons. The voxel heightmap software-rendering engines
!! seen in some PC games also used this principle.
!!
!!##REFERENCES
!!
!!   * "The Bresenham Line-Drawing Algorithm", by Colin Flanagan
!!
!! Bresenham also published a Run-Slice (as opposed to the Run-Length) computational algorithm.
!!
!!  1. ^ Pitteway, M.L.V., "Algorithm for Drawing Ellipses or Hyperbolae with a Digital Plotter", Computer J., 10(3) November 1967, pp
!!     282-289
!!  2. ^ Van Aken, J.R., "An Efficient Ellipse Drawing Algorithm", CG&A, 4(9), September 1984, pp 24-35
!!
!!##SEE ALSO
!!
!!   * Patrick-Gilles Maillot's Thesis an extension of the Bresenham line drawing algorithm to perform 3D hidden lines removal; also
!!     published in MICAD '87 proceedings on CAD/CAM and Computer Graphics, page 591 - ISBN 2-86601-084-1.
!!
!!   * Xiaolin Wu's line algorithm, a similarly fast method of drawing lines with antialiasing.
!!
!!##EXTERNAL LINKS
!!
!!   * Analyze Bresenham's line algorithm in an online Javascript IDE
!!   * Basic Graphics Programs
!!   * The Bresenham Line-Drawing Algorithm by Colin Flanagan
!!   * National Institute of Standards and Technology page on Bresenham's algorithm
!!   * Calcomp 563 Incremental Plotter Information
!!   * Bresenham's Original Paper
!!   * Implementations in Java, C, and O Caml at the Code Codex
!!
!! Retrieved from "http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm"
SUBROUTINE DRAW_LINE_SINGLE(X1,Y1, X2,Y2 )

! ident_4="@(#)M_pixel::draw_line_single(3fp): draw line between two points in pixel array"

INTEGER,INTENT(IN)            :: X1,Y1,X2,Y2

   INTEGER                    :: XX1,YY1,XX2,YY2
   INTEGER                    :: DX, DY, ERROR, YSTEP, X, Y
   LOGICAL                    :: STEEP
   INTEGER                    :: MOSTX, MOSTY

   XX1 = X1
   YY1 = Y1
   XX2 = X2
   YY2 = Y2
   CALL IF_INIT()
   MOSTX=SIZE(P_PIXEL,DIM=1)-1
   MOSTY=SIZE(P_PIXEL,DIM=2)-1

   STEEP = (ABS(YY2 - YY1) > ABS(XX2 - XX1))
   IF ( STEEP ) THEN
      CALL SWAPCOORD(XX1, YY1)
      CALL SWAPCOORD(XX2, YY2)
   ENDIF
   IF ( XX1 > XX2 ) THEN
      CALL SWAPCOORD(XX1, XX2)
      CALL SWAPCOORD(YY1, YY2)
   ENDIF

   DX = XX2 - XX1
   DY = ABS(YY2 - YY1)
   ERROR = DX / 2
   Y = YY1

   IF ( YY1 < YY2 ) THEN
      YSTEP = 1
   ELSE
      YSTEP = -1
   ENDIF

   DO X = XX1, XX2
      IF ( STEEP ) THEN
         IF(Y.LE.MOSTX.AND.X.LE.MOSTY.AND.X.GT.0.AND.Y.GT.0) P_PIXEL(Y,X)=P_COLOR_INDEX
         IF(P_DEBUG)WRITE(*,*)'! ',P_COLOR_INDEX,Y,X
      ELSE
         IF(X.LE.MOSTX.AND.Y.LE.MOSTY.AND.X.GT.0.AND.Y.GT.0) P_PIXEL(X,Y)=P_COLOR_INDEX
         IF(P_DEBUG)WRITE(*,*)'! ',P_COLOR_INDEX,X,Y
      ENDIF
      ERROR = ERROR - DY
      IF ( ERROR < 0 ) THEN
         Y = Y + YSTEP
         ERROR = ERROR + DX
      ENDIF
   ENDDO

END SUBROUTINE DRAW_LINE_SINGLE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    hershey(3f) - [M_pixel:TEXT] draw text string as Hershey software vector fonts
!!    (LICENSE:PD
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine hershey(x,y,height,itext,theta,ntext)
!!    character(len=*),intent(in)   :: itext
!!    real,intent(in)               :: x,y
!!    real,intent(in)               :: height
!!    real,intent(in)               :: theta
!!    integer,intent(in)            :: ntext
!!
!!##OPTIONS
!!    X,Y    are the coordinates in inches from the current origin to the
!!           lower left corner of the 1st character to be plotted. If either
!!           is set to 999.0 then saved next character position is used.
!!    HEIGHT is the character height in inches
!!    ITEXT  contains the text to be plotted
!!    THETA  is the positive CCW angle W.R.T. the X-axis
!!    NTEXT  is the number of characters in itext to plot
!!           o If NTEXT.lt.-1 the pen is down to (X,Y) and a single special
!!             centered symbol is plotted. ITEXT must be from CHAR(0) to
!!             CHAR(21).
!!           o If NTEXT.eq.-1 the pen is up to (X,Y) and a single special
!!             centered symbol is plotted. ITEXT must be from CHAR(0) to
!!             CHAR(21).
!!           o if NTEXT=0 a single Simplex Roman character from ITEXT,
!!             left-justified, is plotted.
!!           o if NTEXT.gt.0 NTEXT characters from ITEXT are decoded and
!!             NCHR characters are plotted where NCHR.le.NTEXT to remove
!!             backslashes, command codes, etc.
!!
!!##DESCRIPTION
!!    FEATURES:
!!      1) Four HERSHEY letter fonts--SIMPLEX,COMPLEX,ITALIC, and DUPLEX--
!!         are provided in upper and lower case ROMAN
!!      2) Two hershey letter fonts--SIMPLEX and COMPLEX--are provided in
!!         upper and lower case GREEK
!!      3) 47 special mathematical symbols, e.g. integral sign, del... are
!!         provided
!!      4) SUPER- and SUB-scripting is possible within a character string
!!         without separate calls to HERSHEY
!!
!!    Change of font is made by enclosing the name of the font in upper
!!    case in backslashes, e.g \SIMPLEX\. Three letters suffice to
!!    specify the font. SIMPLEX is the default font on the initial call
!!    to HERSHEY. A font remains in effect until explicitly changed.
!!    SUPER- or SUB-scripting is accomplished by enclosing the expression
!!    to be SUPER- or SUB-scripted in curly brackets and preceding it by
!!    SUP or SUB. the closing curly bracket terminates the
!!    SUPER- or SUB-scripting and returns to normal character plotting.
!!    Note that SUPER- and SUB-script letters are plotted with a
!!    different character size.
!!
!!    GREEK letters are drawn by enclosing the ENGLISH name of the
!!    letter in backslashes, e.g. \ALPHA\. The case of the first letter
!!    determines the case of the GREEK letter. The closing backslash must
!!    be included.
!!
!!    Any symbol may be called by enclosing the symbol number+1000 in
!!    backslashes. This is the only way to call some symbols, especially
!!    special mathematical symbols.
!!
!!   The symbol numbers are
!!
!!     1-26    upper case ROMAN SIMPLEX
!!    27-52    lower case ROMAN SIMPLEX
!!    53-72    SIMPLEX numbers and symbols
!!    73-96    upper case GREEK SIMPLEX
!!    97-120   lower case GREEK SIMPLEX
!!    121-146  upper case ROMAN COMPLEX
!!    147-172  lower case ROMAN COMPLEX
!!    173-192  COMPLEX numbers and symbols
!!    193-216  upper case GREEK COMPLEX
!!    217-240  lower case GREEK COMPLEX
!!    241-266  upper case ROMAN ITALIC
!!    267-292  lower case ROMAN ITALIC
!!    293-312  ITALIC numbers and symbols
!!    313-338  upper case ROMAN DUPLEX
!!    339-364  lower case ROMAN DUPLEX
!!    365-384  DUPLEX numbers and symbols
!!    385-432  special mathematical symbols
!!
!!    Additional features added Feb 1982:
!!
!!    The pen may be moved back to the start point for the previous character
!!    by \BS\. This is useful, for example, in writing integral signs with
!!    limits above and below them.
!!
!!    Symbol parameters taken from N.M.Wolcott, FORTRAN IV Enhanced Character Graphics, NBS
!!
!!    A. CHAVE IGPP/UCSD Aug 1981, Modified Feb 1982 by A. Chave, R.L. Parker, and L. Shure
!!
!!    programmed in FORTRAN-77
!!
!!##EXAMPLE
!!
!!   Show all Hershey characters
!!
!!    program demo_hershey
!!    use M_pixel
!!    use M_writegif_animated, only : write_animated_gif
!!    implicit none
!!    integer,parameter :: isize=600
!!    integer,parameter :: topsym=432
!!    integer           :: movie(1:topsym,0:isize-1,0:isize-1)
!!    integer           :: i
!!    !! set up environment
!!       call prefsize(isize,isize)
!!       call vinit()
!!       call ortho2(-150.0,150.0,-150.0,150.0)
!!
!!       !! draw all characters using hershey numeric strings
!!       do i=1,topsym
!!          !! draw reference circle and crosshairs
!!          call color(0)
!!          call clear()
!!
!!          call color(4)
!!          call linewidth(100)
!!          call circle(0.0,0.0,75.0)
!!          call move2(-75.0,0.0)
!!          call draw2(75.0,0.0)
!!          call move2(0.0,-75.0)
!!          call draw2(0.0,75.0)
!!
!!          call centertext(.true.)
!!          call color(7)
!!          call linewidth(500)
!!          call textang(3.0*i)
!!          call textang(0.0)
!!          call move2(0.0,0.0)
!!          call textsize(150.0,150.0)
!!          call drawstr('\',i+1000,'\',sep='')
!!
!!          call centertext(.false.)
!!          call color(1)
!!          call move2(-120.0,120.0)
!!          call textsize(10.0,10.0)
!!          call linewidth(40)
!!          call drawstr(i+1000,' ')
!!          movie(i,:,:)=P_pixel
!!       enddo
!!       call vexit()
!!       !! write to file and display with display(1)
!!       call write_animated_gif('hershey.3m_pixel.gif',&
!!       & movie,P_colormap,delay=40)
!!       !call execute_command_line('display hershey.3m_pixel.gif')
!!    end program demo_hershey
!!
!!##AUTHOR
!!    Derived from the Longlib93 library.
!!
!!##LICENSE
!!    Public Domain
!!
!!    Longlib was written by an employee of a US government contractor and
!!    is in the public domain.
!!
!!    Changes to modernize and make more portable by John S. Urban are also
!!    placed in the public domain.
SUBROUTINE HERSHEY(X,Y,HEIGHT,ITEXT,THETA,NTEXT)

! ident_5="@(#)M_pixel::hershey(3f): draw text string as Hershey software vector fonts"

      CHARACTER(LEN=*),INTENT(IN)   :: ITEXT
      REAL,INTENT(IN)               :: X,Y
      REAL,INTENT(IN)               :: HEIGHT
      REAL,INTENT(IN)               :: THETA
      INTEGER,INTENT(IN)            :: NTEXT

      REAL                          :: OLDWID
      REAL                          :: SCALE
      CHARACTER(LEN=4096) :: TEXT
      REAL                :: RAISE(20)
      REAL,SAVE           :: XO,YO
      REAL,PARAMETER      :: SUPSUB(2)=[0.50,-0.50]
      REAL,PARAMETER      :: FACTOR=0.75
      INTEGER,PARAMETER   :: IUP=3
      INTEGER,PARAMETER   :: IDOWN=2
      REAL                :: YY, XX
      REAL                :: YOFF
      REAL                :: YI, XI
      REAL                :: SI
      REAL                :: RSCALE
      REAL                :: CO
      INTEGER :: IPEN
      INTEGER :: ISAV
      INTEGER :: IA
      INTEGER :: IB
      INTEGER :: IC
      INTEGER :: IS
      INTEGER :: IX
      INTEGER :: IY
      INTEGER :: I,K,L,N

!  P_ICHR(J) contains the symbol number of the Jth symbol or a
!  code to indicate SPACE (1000),BEGIN SUPER-SCRIPTING (1001),
!  BEGIN SUB-SCRIPTING (1002), OR END SUPER/SUB-SCRIPTING (1003),
!  OR BACK-SPACE (1004).
!  ISTART(P_ICHR(J)) contains the address in SYMBOL of the Jth
!  character. SYMBCD contains the pen instructions stored in a
!  special form. ISSTAR and SSYMBC contain addresses and pen
!  instructions for the special centered symbols. WIDTH contains
!  the widths of the characters.
!
!-----------------------------------------------------------------------------------------------------------------------------------
   INTEGER :: IXTRCT
   INTEGER :: NSTART
   INTEGER :: NBITS
   INTEGER :: IWORD
!  IXTRCT gets NBITS from IWORD starting at the NSTART bit from the right
      IXTRCT(NSTART,NBITS,IWORD)=MOD(IWORD/(2**(NSTART-NBITS)), &
     &                           2**NBITS)+((1-ISIGN(1,IWORD))/2)* &
     &                           (2**NBITS-MIN0(1,MOD(-IWORD, &
     &                           2**(NSTART-NBITS))))
!-----------------------------------------------------------------------------------------------------------------------------------
      !!write(*,*)'GOT HERE A','X=',x,'Y=',y,'HEIGHT=',height,'ITEXT=',itext,'THETA=',theta,'NTEXT=',ntext
      YOFF=0.0
      SI=SIND(THETA)
      CO=COSD(THETA)
      SCALE=HEIGHT/21.0
      IF(SCALE.EQ.0.0)RETURN
      IF(X.GE.999.0)THEN
         XI=XO
      ELSE
         XI=X
      ENDIF
      IF(Y.GE.999.0)THEN
         YI=YO
      ELSE
         YI=Y
      ENDIF
      IF(NTEXT.LT.0)THEN                                   !  plot a single special centered symbol
       IF(NTEXT.LT.-1)CALL HSTYLUS(XI,YI,IDOWN)
       IA=ICHAR(ITEXT(1:1))+1
       IF(IA.GT.SIZE(ISSTAR))THEN
          WRITE(*,*)'*hershey* error: character out of range for centered characters=',IA,ITEXT(1:1)
          IA=SIZE(ISSTAR)
       ENDIF
       IS=ISSTAR(IA)
       IB=30
          INFINITE: DO
             IPEN=IXTRCT(IB,3,SSYMBC(IS))
             IF(IPEN.EQ.0)THEN
               CALL HSTYLUS(XI,YI,IUP)
               XI=XI+20.0*CO
               YI=YI+20.0*SI
               XO=XI
               YO=YI
               RETURN
             ENDIF
             IX=IXTRCT(IB-3,6,SSYMBC(IS))
             IY=IXTRCT(IB-9,6,SSYMBC(IS))
             XX=SCALE*(IX-32)
             YY=SCALE*(IY-32)
             CALL HSTYLUS(XI+XX*CO-YY*SI,YI+XX*SI+YY*CO,IPEN)
             IB=45-IB
             IF(IB.EQ.30)IS=IS+1
          ENDDO INFINITE
      ELSEIF (NTEXT.EQ.0)THEN                               ! plot a single simplex roman character
        ISAV=P_IOFF
        P_IOFF=0
        TEXT(1:1)=ITEXT(1:1)
        CALL CHRCOD(TEXT,1)
        P_IOFF=ISAV
        IS=ISTART(P_ICHR(1))
        IB=30
        DO
           IPEN=IXTRCT(IB,3,SYMBCD(IS))
           IF(IPEN.EQ.0)THEN
             XI=XI+CO*SCALE*WIDTH(P_ICHR(1))
             YI=YI+SI*SCALE*WIDTH(P_ICHR(1))
             XO=XI
             YO=YI
             RETURN
           ENDIF
           IX=IXTRCT(IB-3,6,SYMBCD(IS))
           IY=IXTRCT(IB-9,6,SYMBCD(IS))
           XX=(IX-10)*SCALE
           YY=(IY-11)*SCALE
           CALL HSTYLUS(XI+CO*XX-SI*YY,YI+CO*YY+SI*XX,IPEN)
           IB=45-IB
           IF(IB.EQ.30)IS=IS+1
        ENDDO
      ELSE
         !  plot a character string.
         !  first find pointer array P_ichr containing the starts of characters-
         !  but only if P_just1 and P_just2  are not 1, when P_ichr is assumed
         !  correctly transmitted through common /ajust/.
        IF(P_JUST1.NE.1.OR.P_JUST2.NE.1)THEN
          N=NTEXT
          K=1
          DO I=1,N
             TEXT(I:I)=ITEXT(I:I)
             K=K+1
          ENDDO
          CALL CHRCOD(TEXT,N)
        ENDIF
        P_JUST2=2
        OLDWID=0.0
        L=1
        RSCALE=SCALE
        EACH_CHAR: DO I=1,P_NCHR                                 !  plot each character
           IC=P_ICHR(I)
           IF(IC.EQ.1000)THEN
             !  plot a space
             XI=XI+20.*RSCALE*CO
             YI=YI+20.*RSCALE*SI
             XO=XI
             YO=YI
             CALL HSTYLUS(XI,YI,IUP)
           ELSEIF ((IC.EQ.1001).OR.(IC.EQ.1002))THEN
             !  begin super-scripting or sub-scripting
             RAISE(L)=SUPSUB(IC-1000)*HEIGHT*RSCALE/SCALE
             RSCALE=FACTOR*RSCALE
             YOFF=RAISE(L)+YOFF
             L=L+1
           ELSEIF (IC.EQ.1003)THEN
             !  end super/sub-scripting
             RSCALE=RSCALE/FACTOR
             L=L-1
             YOFF=YOFF-RAISE(L)
           ELSEIF (IC.EQ.1004)THEN
             !  backspace -use the width of the previous letter in oldwid.
             XI=XI - CO*OLDWID
             YI=YI - SI*OLDWID
             XO=XI
             YO=YI
           ELSE
             ! plot a single symbol
             IS=ISTART(IC)
             IB=30
             DO
                IPEN=IXTRCT(IB,3,SYMBCD(IS))
                IF(IPEN.EQ.0)THEN
                  XI=XI+CO*RSCALE*WIDTH(IC)
                  YI=YI+SI*RSCALE*WIDTH(IC)
                  XO=XI
                  YO=YI
                  OLDWID=WIDTH(IC)*RSCALE
               CYCLE EACH_CHAR
                ENDIF
                IX=IXTRCT(IB-3,6,SYMBCD(IS))
                IY=IXTRCT(IB-9,6,SYMBCD(IS))
                XX=(IX-10)*RSCALE
                YY=(IY-11)*RSCALE+YOFF
                CALL HSTYLUS(XI+CO*XX-SI*YY,YI+CO*YY+SI*XX,IPEN)
                IB=45-IB
                IF(IB.EQ.30)IS=IS+1
             ENDDO
           ENDIF
        ENDDO EACH_CHAR
      ENDIF
END SUBROUTINE HERSHEY
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE HSTYLUS(XI,YI,IPEN)

! ident_6="@(#)M_pixel::hstylus(3fp): move to new current position(CP) or draw from CP to new position and update CP"

REAL,INTENT(IN)    :: XI,YI
INTEGER,INTENT(IN) :: IPEN
REAL               :: P_X_TMP,P_Y_TMP

   INTEGER,PARAMETER  :: IDOWN=2 !, iup=3

   IF(IPEN.EQ.IDOWN)THEN
      P_X_TMP=P_X
      P_Y_TMP=P_Y
      CALL LINE(P_X_TMP, P_Y_TMP, XI, YI )
   ELSE
      P_X=XI
      P_Y=YI
   ENDIF


END SUBROUTINE HSTYLUS
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE CHRCOD(TEXT,NTEXT)

! ident_7="@(#)M_pixel::chrcod(3fp): return symbol numbers or formatting codes for a text string"

!  Given text string in text, NTEXT characters
!  returns P_ICHR containing P_NCHR symbol numbers or codes for
!   o SPACE (1000)
!   o BEGIN SUPERSCRIPTING (1001)
!   o BEGIN SUBSCRIPTING (1002)
!   o END SUPER/SUB-SCRIPTING (1003)
!   o BACKSPACE (1004)
!   o VECTOR (1005)
!   o HAT (1006)
!  Change of font commands are decoded and executed internally
!
   CHARACTER(LEN=*),INTENT(IN) :: TEXT
   INTEGER,INTENT(IN)          :: NTEXT
   INTEGER,SAVE :: IRLU(95),IILU(95),IGLU(26)
   INTEGER :: NUMBER
   INTEGER :: NT
   INTEGER :: IGOFF
   INTEGER :: IGR
   INTEGER :: IB
   INTEGER :: IC
   INTEGER :: IG
   INTEGER :: ICO
   INTEGER :: K,L,N
!  IRLU IS A LOOK-UP TABLE FOR ROMAN CHARACTERS ARRANGED BY
!  INTEGER VALUE FOR THE ASCII CHARACTER SET WITH AN
!  OFFSET TO REMOVE THE 31 NONPRINTING CONTROL CHARACTERS.
!  IRLU RETURNS WITH THE SYMBOL NUMBER OR, IF NO SYMBOL
!  EXISTS, THE CODE FOR SPACE.
   DATA IRLU/1000,416,428,411,72,418,419,432,67,68,69,63,70, &
     &          64,71,65,53,54,55,56,57,58,59,60,61,62,414,415, &
     &          385,66,386,417,407,1,2,3,4,5,6,7,8,9,10,11,12,13, &
     &          14,15,16,17,18,19,20,21,22,23,24,25,26,409,1000, &
     &          410,408,1000,1000,27,28,29,30,31,32,33,34,35,36, &
     &          37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52, &
     &          405,427,406,424/
!  IILU IS A LOOK-UP TABLE FOR ITALIC CHARACTERS ONLY. IT IS
!  IDENTICAL TO IRLU WITH FOUR ITALIC SPECIAL SYMBOLS SUBSTITUTED
!  FOR REGULAR ONES.
   DATA IILU/1000,422,1000,411,72,418,419,1000,67,68,69,63,70, &
     &          64,71,65,53,54,55,56,57,58,59,60,61,62,420,421, &
     &          385,66,386,423,407,1,2,3,4,5,6,7,8,9,10,11,12,13, &
     &          14,15,16,17,18,19,20,21,22,23,24,25,26,409,1000, &
     &          410,1000,1000,1000,27,28,29,30,31,32,33,34,35,36, &
     &          37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52, &
     &          405,427,406,424/
!  IGLU IS A LOOK-UP TABLE FOR GREEK CHARACTERS ARRANGED BY THE
!  INTEGER VALUE OF THEIR ROMAN EXPRESSION WITH A=1, B=2, ETC.
!  AMBIGUOUS CASES GIVE 25 FOR EPSILON OR ETA, 26 FOR OMEGA OR
!  OMICRON, 27 FOR PHI,PI,OR PSI, AND 28 FOR TAU OR THETA. ADDITIONAL
!  LETTERS MUST BE CHECKED FOR THESE CASE. A VALUE OF 50 IS RETURNED
!  FOR THOSE ROMAN LETTERS WHICH HAVE NO CORRESPONDING GREEK LETTER.
   DATA IGLU/1,2,22,4,25,50,3,50,9,50,10,11,12,13,26,27,50,17,18,28,20,50,50,14,50,6/
! FINDS LENGTH OF STRING WITH BLANKS TRIMMED FROM RIGHT END.
   DO N=NTEXT,1,-1
      IF(TEXT(N:N).NE.' ')GOTO 15
   ENDDO
   P_NCHR=0
   RETURN
15 CONTINUE
   NT=N
!  SCAN TEXT CHARACTER BY CHARACTER
   K=1
   J=1
!  K IS CURRENT ADDRESS OF CHARACTER IN TEXT
!  J IS INDEX OF NEXT SYMBOL CODE IN P_ICHR
   INFINITE: DO
20 CONTINUE
      IF(K.GT.N)THEN
        P_NCHR=J-1
        RETURN
      ENDIF
      IF(TEXT(K:K).NE.'\')THEN
        !  ROMAN CHARACTER OR KEYBOARD SYMBOL
        IF(TEXT(K:K).EQ.'}')THEN
          !  CHECK FOR CLOSING CURLY BRACKET-IF FOUND, RETURN 1003
          P_ICHR(J)=1003
          J=J+1
          K=K+1
          CYCLE INFINITE
          GOTO 20
        ENDIF
        !  ICHAR RETURNS INTEGER ASCII VALUE OF CHARACTER
        !  OFFSET BY NONPRINTING CHARACTERS TO GET ENTRY IN LOOK-UP TABLE
        IC=ICHAR(TEXT(K:K))-ICHAR(' ')+1
        IF(IC.LE.0)THEN                           !  NONPRINTING CONTROL CHARACTER-ERROR RETURN
          P_ICHR(J)=1000
        ELSEIF (P_IOFF.NE.240)THEN                !  NOT ITALIC FONT
          P_ICHR(J)=IRLU(IC)
        ELSE                                      !  ITALIC FONT
          P_ICHR(J)=IILU(IC)
        ENDIF
        IF(P_ICHR(J).LT.385)P_ICHR(J)=P_ICHR(J)+P_IOFF    !  ADD OFFSET FOR FONT IF NOT A SPECIAL SYMBOL
          J=J+1
          K=K+1
          CYCLE INFINITE
          GOTO 20
        ELSE                                      !  BACKSLASH FOUND
          !  CHECK NEXT FOUR CHARACTERS FOR FOUR DIGIT NUMBER
          K=K+1
          READ(TEXT(K:K+3),'(i4)',ERR=50)NUMBER
          !  NUMBER FOUND-CHECK ITS VALIDITY
          IC=NUMBER-1000
          IF((IC.GT.0).AND.(IC.LT.433))THEN
            !  VALID SYMBOL CODE
            P_ICHR(J)=IC
          ELSEIF ((IC.GT.999).AND.(IC.LT.1004))THEN
            !  VALID COMMAND CODE
            P_ICHR(J)=IC
          ELSE
            ! NOT RECOGNIZED-ERROR RETURN
            P_ICHR(J)=1000
          ENDIF
          J=J+1
           !  MOVE BEYOND CLOSING BACKSLASH-IGNORE EXTRA CHARACTERS
           !  FUNCTION INDEX RETURNS OFFSET OF SECOND SUBSTRING IN FIRST
           !  RETURNS 0 IF SUBSTRING NOT FOUND
           L=INDEX(TEXT(K:NT),'\')
           IF(L.EQ.0)THEN
             K=NT+1
           ELSE
             K=K+L
           ENDIF
          CYCLE INFINITE
           GOTO 20
   50      CONTINUE
           !  NOT A NUMBER
           !  CHECK FOR FONT CHANGE COMMAND
         IF(TEXT(K:K+2).EQ.'SIM'.OR.TEXT(K:K+2).EQ.'sim')THEN
           !  SIMPLEX FONT
           P_IOFF=0
         ELSEIF(TEXT(K:K+1).EQ.'CO'.OR.TEXT(K:K+1).EQ.'co')THEN
           !  COMPLEX FONT
           P_IOFF=120
         ELSEIF(TEXT(K:K+1).EQ.'IT'.OR.TEXT(K:K+1).EQ.'it')THEN
           !  ITALIC FONT
           P_IOFF=240
         ELSEIF (TEXT(K:K+1).EQ.'DU'.OR.TEXT(K:K+1).EQ.'du')THEN
           !  DUPLEX FONT
           P_IOFF=312
           !  FOUND THE BACK-SPACE CODE
         ELSEIF(TEXT(K:K+1).EQ.'BS'.OR.TEXT(K:K+1).EQ.'bs') THEN
           P_ICHR(J)=1004
           J=J+1
           K=K+3
           GO TO 20
          CYCLE INFINITE
           !  CHECK FOR SUPER/SUB-SCRIPT COMMAND
         ELSEIF(TEXT(K:K+3).EQ.'SUP{'.OR.TEXT(K:K+3).EQ.'sup{')THEN
           !  BEGIN SUPERSCRIPTING
           P_ICHR(J)=1001
           J=J+1
           K=K+4
           GOTO 20
          CYCLE INFINITE
         ELSEIF (TEXT(K:K+3).EQ.'SUB{'.OR.TEXT(K:K+3).EQ.'sub{')THEN
           !  BEGIN SUBSCRIPTING
           P_ICHR(J)=1002
           J=J+1
           K=K+4
           GOTO 20
          CYCLE INFINITE
         ELSE
           !  GREEK CHARACTER OR INVALID CHARACTER
           IC=ICHAR(TEXT(K:K))
           IGOFF=MIN0(P_IOFF, 120)
           IF(P_IOFF.EQ.312)IGOFF=0
           IF((IC.GE.ICHAR('A')).AND.(IC.LE.ICHAR('Z')))THEN
             !  UPPER CASE
             IGR=72
             ICO=ICHAR('A')-1
           ELSEIF((IC.GE.ICHAR('a')).AND.(IC.LE.ICHAR('z')))THEN
             !  LOWER CASE
             IGR=96
             ICO=ICHAR('a')-1
           ELSE
             !  NOT A LETTER-ERROR RETURN
             P_ICHR(J)=1000
             J=J+1
             L=INDEX(TEXT(K:NT),'\')
             IF(L.EQ.0)THEN
               K=NT+1
             ELSE
               K=K+L
             ENDIF
             GOTO 20
          CYCLE INFINITE
           ENDIF
           !  LOOK UP THE CHARACTER
           IG=IGLU(IC-ICO)
           IF(IG.LT.25)THEN                !  UNAMBIGUOUS GREEK LETTER
             P_ICHR(J)=IG+IGR+IGOFF
           ELSEIF (IG.EQ.25)THEN           !  EPSILON OR ETA
             IB=ICHAR(TEXT(K+1:K+1))-ICO
             IF(IB.EQ.16)THEN              !  EPSILON
               P_ICHR(J)=5+IGR+IGOFF
             ELSEIF (IB.EQ.20)THEN         !  ETA
               P_ICHR(J)=7+IGR+IGOFF
             ELSE                          !  NOT A GREEK CHARACTER--ERROR RETURN
               P_ICHR(J)=1000
             ENDIF
         ELSEIF (IG.EQ.26)THEN             !  OMEGA OR OMICRON
           IB=ICHAR(TEXT(K+1:K+1))-ICO
           IF(IB.NE.13)THEN                ! NOT A GREEK CHARACTER-ERROR RETURN
             P_ICHR(J)=1000
           ELSE
             IC=ICHAR(TEXT(K+2:K+2))-ICO
             IF(IC.EQ.5)THEN               !  OMEGA
               P_ICHR(J)=24+IGR+IGOFF
             ELSEIF (IC.EQ.9)THEN          !  OMICRON
               P_ICHR(J)=15+IGR+IGOFF
             ELSE                          !  NOT A GREEK CHARACTER-ERROR RETURN
               P_ICHR(J)=1000
             ENDIF
           ENDIF
         ELSEIF (IG.EQ.27)THEN             !  PHI,PI, OR PSI
           IB=ICHAR(TEXT(K+1:K+1))-ICO
           IF(IB.EQ.8)THEN                 !  PHI
             P_ICHR(J)=21+IGR+IGOFF
           ELSEIF (IB.EQ.9)THEN            !  PI
             P_ICHR(J)=16+IGR+IGOFF
           ELSEIF (IB.EQ.19)THEN           !  PSI
             P_ICHR(J)=23+IGR+IGOFF
           ELSE                            !  NOT A GREEK CHARACTER-ERROR RETURN
             P_ICHR(J)=1000
           ENDIF
           ELSEIF (IG.EQ.28)THEN           ! TAU OR THETA
             IB=ICHAR(TEXT(K+1:K+1))-ICO
             IF(IB.EQ.1)THEN               !  TAU
               P_ICHR(J)=19+IGR+IGOFF
             ELSEIF(IB.EQ.8)THEN           !  THETA
               P_ICHR(J)=8+IGR+IGOFF
             ELSE                          !  NOT A GREEK CHARACTER-ERROR RETURN
               P_ICHR(J)=1000
             ENDIF
           ELSE                            !  NOT A GREEK CHARACTER-ERROR RETURN
             P_ICHR(J)=1000
           ENDIF
          J=J+1
        ENDIF
        L=INDEX(TEXT(K:NT),'\')
        IF(L.EQ.0)THEN
          K=NT+1
        ELSE
          K=K+L
        ENDIF
        GOTO 20
        CYCLE INFINITE
      ENDIF
      EXIT INFINITE
      ENDDO INFINITE
END SUBROUTINE CHRCOD
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    strlength(3f) - [M_pixel:TEXT] return length of string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    function strlength(string)
!!    character(len=*),intent(in)    :: string
!!
!!##DESCRIPTION
!!    Return the length of the string "STRING" in world units.
!!
!!##RETURNS
!!    STRLENGTH  length of string using current font size
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_strlength
!!    use :: M_pixel
!!    use :: M_writegif, only : writegif
!!    implicit none
!!    real    :: left
!!    real    :: baseline
!!    integer :: icolor=0
!!    real    :: texth=10.0
!!       !! set up drawing surface
!!       call prefsize(800, 400)
!!       call vinit()
!!       call viewport(0.0, 800.0, 400.0, 0.0)
!!       call ortho2(-100.0, 300.0, -100.0, 100.0)
!!       call color(7)
!!       call clear()
!!       call linewidth(30)
!!       call textsize(texth, texth)
!!       call xcentertext()
!!       call color(1)
!!
!!       baseline=85.0
!!       call move2(0.0,baseline)
!!       call drawstr('If I Can Stop One Heart')
!!       baseline= baseline-texth*1.20
!!       call move2(0.0,baseline)
!!       call drawstr('by Emily Dickinson')
!!       call centertext(.false.)
!!
!!       texth=8.5
!!       baseline=baseline-texth*1.50
!!       call textsize(texth, texth)
!!       left=-90.0
!!
!!       call nextline('If I can stop one heart from breaking,')
!!       call nextline('I shall not live in vain;')
!!       call nextline('If I can ease one life the aching,')
!!       call nextline('Or cool one pain,')
!!       call nextline('Or help one fainting robin')
!!       call nextline('Unto his nest again,')
!!       call nextline('I shall not live in vain.')
!!
!!       call writegif('strlength.3m_pixel.gif',P_pixel,P_colormap)
!!       call execute_command_line('display strlength.3m_pixel.gif')
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    real :: xx
!!    !! reduce some duplicate code; very specific to this example
!!       call color(icolor)
!!       baseline=baseline-texth*1.5    ! move down before drawing line
!!       call makepoly()
!!       xx=strlength(string)
!!       call rect(left,baseline-texth*0.3,left+xx,baseline+texth)
!!       call closepoly()
!!       call color(7)
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!       icolor=icolor+1         ! set pen color
!!    end subroutine nextline
!!
!!    end program demo_strlength
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
FUNCTION STRLENGTH(STRING)

! ident_8="@(#)M_pixel::strlength: length of string using current font size"

CHARACTER(LEN=*),INTENT(IN)    :: STRING
REAL                           :: STRLENGTH

   REAL                        :: S(4)
   !!character(len=:),allocatable :: fontstring
   !!fontstring='\'//trim(P_FONT)//'\'//trim(string)

   CALL JUSTFY(S, P_TEXT_HEIGHT, TRIM(STRING), LEN_TRIM(STRING))

!  S(1)  to the left edge of the 1st nonblank character
!  s(2)  to the center of the string, blanks removed from the ends
!  s(3)  to the right edge of the last nonblank character
!  s(4)  to the right edge of the last character of the string.

   STRLENGTH=S(4)

END FUNCTION STRLENGTH
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    justfy(3f) - [M_pixel:TEXT] return lengths used to justify a string when calling hershey
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine justfy(s, height, text, ntext)
!!    real,intent(out)               :: s(4)
!!    real,intent(in)                :: height
!!    character(len=*),intent(in)    :: text
!!    integer,intent(in)             :: ntext
!!
!!##DESCRIPTION
!!    Given the text string TEXT with NTEXT characters, height HEIGHT,
!!    this routine gives 4 distances in inches, all from the left end of
!!    the string -
!!
!!    o S(1)  to the left edge of the 1st nonblank character
!!    o S(2)  to the center of the string, blanks removed from the ends
!!    o S(3)  to the right edge of the last nonblank character
!!    o S(4)  to the right edge of the last character of the string.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE JUSTFY(S, HEIGHT, TEXT, NTEXT)

! ident_9="@(#)M_pixel::justfy(3f): calculate values for justifying Hershey fonts called by hershey(3f)"

!  Given the text string TEXT with NTEXT characters, height HEIGHT, this routine
!  gives 4 distances in inches, all from the left end of the string -
!  S(1)  to the left edge of the 1st nonblank character
!  s(2)  to the center of the string, blanks removed from the ends
!  s(3)  to the right edge of the last nonblank character
!  s(4)  to the right edge of the last character of the string.

      REAL,INTENT(OUT)               :: S(4)
      REAL,INTENT(IN)                :: HEIGHT
      CHARACTER(LEN=*),INTENT(IN)    :: TEXT
      CHARACTER(LEN=4096)            :: TEXT_LOCAL
      INTEGER,INTENT(IN)             :: NTEXT

      REAL,PARAMETER                 :: FACTOR=0.75
      INTEGER,PARAMETER              :: IPOWER(3)=[1,1,-1]
      REAL                           :: SCALE
      REAL                           :: OLDWID
      INTEGER                        :: JQUART
      INTEGER                        :: LEAD
      INTEGER                        :: I
      INTEGER                        :: L
      INTEGER                        :: NTXT
!
      TEXT_LOCAL=TEXT
      NTXT=NTEXT
      SCALE=HEIGHT/21.0
      JQUART=(NTEXT+3)/4
!  translate integer string into character variable, then get pointers
!  into the array P_ichr.
!
      CALL CHRCOD(TEXT_LOCAL,NTXT)
!
!  count leading blanks.
      DO LEAD=1,P_NCHR
         IF(P_ICHR(LEAD).NE.1000)GOTO 1110
      ENDDO
      LEAD=NTXT
 1110 CONTINUE
      S(1)=20.0*SCALE*(LEAD-1)
      S(3)=S(1)
!
!  sum the widths of the remaining text, recalling that trailing blanks
!  were lopped off by chrcod.
      OLDWID=0.0
      DO I=LEAD,P_NCHR
         L=P_ICHR(I)
         IF (L.LT.1000) THEN
           OLDWID=WIDTH(L)*SCALE
           S(3)=S(3) + OLDWID
         ENDIF
         IF(L.EQ.1000)S(3)=S(3)+20.0*SCALE
         IF(L.GE.1001.AND.L.LE.1003)SCALE=SCALE*FACTOR**IPOWER(L-1000)
         IF(L.EQ.1004)S(3)=S(3)-OLDWID
      ENDDO
!
!  add on width of surplus trailing blanks.
      S(4)=S(3)+20.0*SCALE*(NTXT-P_NCHR)
!
!  find center of nonblank text.
      S(2)=(S(1)+S(3))/2.0
      P_JUST2=1
END SUBROUTINE JUSTFY
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    polyline2(3f) - [M_pixel:DRAW] - draw an unclosed polyline in the XY plane
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine polyline2(arrx,arry)
!!
!!           integer,intent(in)          :: arrx(:)
!!           integer,intent(in),optional :: arry(:)
!!
!!##DESCRIPTION
!!        Given either a single array composed of pairs <x(i),y(i)> of
!!        values defining points or an X and Y array move to first point
!!        and draw to remaining points using current line style.
!!
!!##OPTIONS
!!        ARRX   If ARRY is present, an array of X values
!!
!!        ARRY   An optional array of Y values
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_polyline2
!!    use M_pixel
!!    use M_writegif, only : writegif
!!    implicit none
!!    integer :: transparent=0
!!    integer :: ipaws
!!       call prefsize(300,300)
!!       call vinit(' ')
!!       call ortho2(-2.0,2.0,-2.0,2.0)
!!       call color(2)
!!       call linewidth(100)
!!       call polyline2([-0.5,-0.5, -0.5,+0.5, +0.5,+0.5, +0.5,-0.5])
!!       call color(4)
!!       call polyline2( [-1,-1,+1,+1,-1] , &  ! X values
!!       & [-1,+1,+1,-1,-1] )    ! Y values
!!        ! write gif with a transparent background
!!       call writegif('polyline2.3m_pixel.gif',P_pixel,P_ColorMap,transparent)
!!       call vexit()
!!    end program demo_polyline2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE POLYLINE2(X,Y)
!-!use :: M_anything, only : anyscalar_to_real
CLASS(*),INTENT(IN)          :: X(:)
CLASS(*),INTENT(IN),OPTIONAL :: Y(:)
REAL,ALLOCATABLE             :: ARRX(:)
REAL,ALLOCATABLE             :: ARRY(:)
INTEGER                      :: I
INTEGER                      :: ISIZEX
INTEGER                      :: ISIZEY
INTEGER                      :: IPAIRS
! assuming nice data in x,y pairs
ARRX=ANYSCALAR_TO_REAL(X)
IF(PRESENT(Y))THEN    ! two arrays means X array and Y array
   ARRY=ANYSCALAR_TO_REAL(Y)
   ISIZEX=SIZE(ARRX)
   ISIZEY=SIZE(ARRY)
   IPAIRS=MIN(ISIZEX,ISIZEY)
   IF(IPAIRS.GT.0)THEN
      CALL MOVE2(ARRX(1),ARRY(1))
   ENDIF
   DO I=2,IPAIRS
      CALL DRAW2(ARRX(I),ARRY(I))
   ENDDO
ELSE                      ! one array means array is <x1,y1>, <x2,y2>, <x3,y3>, ...
   ISIZEX=SIZE(ARRX)
   ISIZEY=0
   IPAIRS=ISIZEX/2
   IF(IPAIRS.GT.0)THEN
      CALL MOVE2(ARRX(1),ARRX(2))
   ENDIF
   DO I=3,IPAIRS*2,2
      CALL DRAW2(ARRX(I),ARRX(I+1))
   ENDDO
ENDIF

END SUBROUTINE POLYLINE2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    clear(3f) - [M_pixel] clear background to current color or specified color index
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine clear(indx)
!!    integer,intent(in),optional :: indx
!!
!!##DESCRIPTION
!!    Clears the screen to the current color or to color specified
!!
!!##OPTIONS
!!    INDX   color index to set pixel array to. Optional
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_clear
!!    use :: M_pixel
!!    use :: M_writegif, only : writegif
!!    implicit none
!!    real,parameter :: x=400.0, y=400.0
!!       call prefsize(int(x), int(y)) ! set up drawing surface
!!       call vinit()
!!       call color(1)
!!       call linewidth(300)
!!       ! clear a circle and rectangle in default window and viewport
!!       call rect(0.0,0.0,x,y)
!!       call circle(x/2.0,y/2.0,x/2.0)
!!       ! now clear screen to current color
!!       call color(3)
!!       call clear()
!!       ! gif should be blank
!!       call writegif('clear.3m_pixel.gif',P_pixel,P_colormap)
!!       call execute_command_line('display clear.3m_pixel.gif')
!!       call vexit()
!!    end program demo_clear
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE CLEAR(INDX)

! ident_10="@(#)M_pixel::clear(3f): set background color all to specified color index"

INTEGER,INTENT(IN),OPTIONAL :: INDX
CALL IF_INIT()
IF(PRESENT(INDX))THEN
   P_PIXEL=INDX
ELSE
   P_PIXEL=P_COLOR_INDEX
ENDIF
END SUBROUTINE CLEAR
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    pixel(3f) - [M_pixel] set pixel to current color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    elemental impure subroutine pixel(row,column,indx)
!!    integer,intent(in)          :: row
!!    integer,intent(in)          :: column
!!    integer,intent(in),optional :: indx
!!
!!##DESCRIPTION
!!    Directly set a pixel to the current or specified color index.
!!    The ROW and COLUMN start at 1.
!!
!!##OPTIONS
!!    ROW      row number in P_pixel to set
!!
!!              0 < ROW < size(P_pixel,dim=1)-1.
!!    COLUMN   column number in P_pixel to set
!!
!!              0 < COLUMN < size(P_pixel,dim=2)-1.
!!
!!    INDX     color index to set pixel array to. Optional
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_pixel
!!    use :: M_pixel
!!    implicit none
!!       call prefsize(10,10) ! set up drawing surface
!!       call mapcolor(0,255,255,255)
!!       call mapcolor(1,255,000,000)
!!       call mapcolor(2,255,255,000)
!!       call mapcolor(3,255,000,255)
!!       call mapcolor(4,000,255,255)
!!       call mapcolor(5,000,255,000)
!!       call mapcolor(6,000,000,255)
!!       call mapcolor(7,000,000,000)
!!       call vinit()
!!       call color(0)
!!       call clear()
!!       call color(1)
!!       call pixel(1,1)
!!       call color(3)
!!       call pixel(3,3)
!!       call pixel(5,5,5)
!!       call print_ascii()
!!       call vexit()
!!    end program demo_pixel
!!
!!   Results:
!!
!!    1000000000
!!    0000000000
!!    0030000000
!!    0000000000
!!    0000500000
!!    0000000000
!!    0000000000
!!    0000000000
!!    0000000000
!!    0000000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
ELEMENTAL IMPURE SUBROUTINE PIXEL(ROW,COLUMN,INDX)

! ident_11="@(#)M_pixel::pixel(3f): set background color all to specified color index"

INTEGER,INTENT(IN)          :: ROW
INTEGER,INTENT(IN)          :: COLUMN
INTEGER,INTENT(IN),OPTIONAL :: INDX
   CALL IF_INIT()
   CHECK: BLOCK
      IF(ROW.LT.1.OR.ROW.GT.P_VIEWPORT_HEIGHT) EXIT CHECK
      IF(COLUMN.LT.1.OR.COLUMN.GT.P_VIEWPORT_WIDTH) EXIT CHECK
      IF(PRESENT(INDX))THEN
         P_PIXEL(ROW-1,COLUMN-1)=INDX
      ELSE
         P_PIXEL(ROW-1,COLUMN-1)=P_COLOR_INDEX
      ENDIF
   END BLOCK CHECK
END SUBROUTINE PIXEL
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE IF_INIT()

! ident_12="@(#)M_pixel::if_init(3f): check if pixel graphics library has been initialized"

   IF(.NOT.P_VINIT_CALLED)THEN
      WRITE(*,*)'*draw_line_single* WARNING: P_vinit(3f) was not called'
      CALL VINIT()
   ENDIF
END SUBROUTINE IF_INIT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    arc(3f) - [M_pixel:ARCS] draw an arc using current line width and color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine arc(x, y, radius, startang, endang)
!!    real,intent(in) :: x
!!    real,intent(in) :: y
!!    real,intent(in) :: radius
!!    real,intent(in) :: startang
!!    real,intent(in) :: endang
!!
!!##DESCRIPTION
!!    Draw an arc. x, y, and radius are values in world units.
!!
!!    Angles are in degrees, positive measured counterclockwise from the
!!    +X axis. The current position after the arc is drawn is at the end
!!    of the arc.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!    STARTANG   Start angle
!!    ENDANG     End angle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_arc
!!    use M_pixel
!!    use M_writegif, only : writegif
!!    implicit none
!!    integer  :: transparent=0
!!       call prefsize(600,240)
!!       call vinit()
!!       call ortho2(0.0,60.0,0.0,24.0)
!!       call linewidth(400)
!!       call color(1)
!!       call arc(16.0,12.0,12.0,90.0,270.0)
!!       call color(2)
!!       call arc(44.0,12.0,12.0,-90.0,90.0)
!!       ! write gif with a transparent background
!!       call writegif('arc.3m_pixel.gif',P_pixel,P_ColorMap,transparent)
!!       call vexit()
!!    end program demo_arc
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE ARC(X,Y,RADIUS,STARTANG,ENDANG)

! ident_13="@(#)M_pixel::arc(3f): draw a arc using current line width and color"

REAL,INTENT(IN) :: X,Y
REAL,INTENT(IN) :: RADIUS
REAL,INTENT(IN) :: STARTANG,ENDANG

   REAL               :: DELTANG
   INTEGER            :: I
   REAL               :: DX,DY,CX,CY,COSINE,SINE
   INTEGER            :: NUMSEGS

   NUMSEGS = NINT( ABS(ENDANG - STARTANG) / 360.0) * P_NSEGS
   DELTANG = (ENDANG - STARTANG) / NUMSEGS
   COSINE = COSD(DELTANG)
   SINE = SIND(DELTANG)

   ! calculates initial point on arc

   CX = X + RADIUS * COSD(STARTANG)
   CY = Y + RADIUS * SIND(STARTANG)
   CALL MOVE2(CX, CY)

   DO I=0,NUMSEGS-1
      DX = CX - X
      DY = CY - Y
      CX = X + DX * COSINE - DY * SINE
      CY = Y + DX * SINE + DY * COSINE
      CALL DRAW2(CX, CY)
   ENDDO

END SUBROUTINE ARC
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    circle(3f) - [M_pixel:ARCS] draw a circle using current line width and color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine circle(x,y,radius)
!!    real,intent(in) :: x
!!    real,intent(in) :: y
!!    real,intent(in) :: radius
!!
!!##DESCRIPTION
!!    Draw a circle using the current line width and color into the pixel
!!    array. Units are in world coordinates.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circle
!!    use M_pixel
!!    use M_writegif, only : writegif
!!    implicit none
!!       !! set up drawing surface
!!       call prefsize(400,400)
!!       call vinit()
!!       call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)
!!       call color(3)
!!       call clear()
!!       call color(4)
!!       call linewidth(200)
!!       !! draw some circles
!!       call circle(0.0, 0.0, 90.0)
!!       call color(1)
!!       call circle(0.0, 0.0, 40.0)
!!       call color(2)
!!       call circle(-25.0, 25.0, 20.0)
!!       call circle(-25.0,-25.0, 20.0)
!!       call circle( 25.0, 25.0, 20.0)
!!       call circle( 25.0,-25.0, 20.0)
!!       !! render the pixel map
!!       call writegif('circle.3m_pixel.gif',P_pixel,P_colormap)
!!       !! display the graphic assuming display(1) is available
!!       call execute_command_line('display circle.3m_pixel.gif')
!!       !! exit graphics mode
!!       call vexit()
!!    end program demo_circle
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE CIRCLE(X,Y,RADIUS)

! ident_14="@(#)M_pixel::circle(3f): draw a circle using current line width and color"

REAL,INTENT(IN) :: X
REAL,INTENT(IN) :: Y
REAL,INTENT(IN) :: RADIUS

REAL               :: DEGREES
REAL               :: INCREMENT
INTEGER            :: I
REAL               :: XX1,YY1, XX2,YY2

INCREMENT=360.0/P_NSEGS

DO I=1,P_NSEGS

   DEGREES=(I-1)*INCREMENT
   XX1=X+RADIUS*COSD(DEGREES)
   YY1=Y+RADIUS*SIND(DEGREES)

   DEGREES=I*INCREMENT
   XX2=X+RADIUS*COSD(DEGREES)
   YY2=Y+RADIUS*SIND(DEGREES)

   CALL LINE(XX1,YY1,XX2,YY2)
ENDDO

END SUBROUTINE CIRCLE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    linewidth(3f) - [M_pixel] set linewidth
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine linewidth(iwidth)
!!    integer iwidth
!!
!!##DESCRIPTION
!!    Set the current line width in units of 1/10,000 of the X size of the
!!    display surface
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_linewidth
!!    use M_pixel,    only : prefsize, vinit, ortho2, clear, P_pixel, P_colormap
!!    use M_pixel,    only : move2, draw2, vexit, color, linewidth
!!    use M_writegif, only : writegif
!!    use M_pixel,    only : d2r, polar_to_cartesian
!!    implicit none
!!    integer :: i
!!    real    :: x,y,r,a,b,theta
!!    ! The Archimedean spiral is the locus of points corresponding
!!    ! to the locations over time of a point moving away from a
!!    ! fixed point with a constant speed along a line which rotates
!!    ! with constant angular velocity.
!!    !    r=a+b*theta
!!    ! Changing the parameter a will turn the spiral,
!!    ! while b controls the distance between successive turnings.
!!       call prefsize(401,401)
!!       call vinit('')
!!       call ortho2(-150.0,150.0,-150.0,150.0)
!!       call clear()
!!       call move2(0.0,0.0)
!!       call color(2)
!!       a=0.0
!!       b=2.0
!!       do i=0,360*10,5
!!          theta=d2r(i)
!!          r=a+b*theta
!!          call polar_to_cartesian(r,theta,x,y)
!!          call linewidth(i/5/3)
!!          call draw2(x,y)
!!       enddo
!!       call writegif('linewidth.3m_pixel.gif',P_pixel,P_colormap)
!!       call vexit()
!!    end program demo_linewidth
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE LINEWIDTH(IWIDTH)

! ident_15="@(#)M_pixel::linewidth(3f): set line width for lines drawn in pixel image"

INTEGER,INTENT(IN) :: IWIDTH
   REAL            :: XWIDTH
   XWIDTH= IWIDTH*P_VIEWPORT_WIDTH /10000
   P_WIDTH=MAX(NINT(XWIDTH),1)
END SUBROUTINE LINEWIDTH
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    color(3f) - [M_pixel:COLOR] set current color index
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine color(col)
!!    integer,intent(in) :: col
!!
!!##DESCRIPTION
!!    Set the current color. The standard colors are as follows:
!!
!!       black  =  0  red      =  1  green  =  2  yellow  =  3
!!       blue   =  4  magenta  =  5  cyan   =  6  white   =  7
!!
!!##OPTION
!!     COL  A color number from 0 to 255. To define additional
!!          colors see mapcolor(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_color
!!     use M_pixel
!!     use M_writegif, only : writegif
!!     implicit none
!!     real    :: b=0.5
!!     real    :: y1,y2,ym,x1,x2
!!     real    :: width=50.0/8.0,width2
!!     integer :: i
!!        !! set up long bar as plotting area
!!        call prefsize(1000,200)
!!        call vinit()
!!        call ortho2(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!        call textsize( 3.5, 4.0)
!!        call font('DUPLEX')
!!        call centertext(.true.)
!!        call linewidth(90)
!!        y1=-5
!!        y2=5
!!        ym=0
!!        x1=-25+.05*width
!!        ! draw colored rectangle and a circle and label center of circle
!!        ! and repeat from colors 0 to 7.
!!        width2=width*0.95
!!        do i=0,7
!!           call color(i)
!!           x2=x1+width2
!!           call makepoly()
!!           call rect(x1,y1,x2,y2)
!!           call closepoly()
!!           call color(i+1)
!!           call move2((x1+x2)/2.0,ym)
!!           call drawstr(i)     ! convert number to string and draw it
!!           call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!           x1=x1+width
!!        enddo
!!        ! write plot as GIF file
!!        call writegif('color.3m_pixel.gif',P_pixel,P_colormap)
!!        call vexit()
!!        ! use system to display GIF file
!!        call execute_command_line('display color.3m_pixel.gif')
!!     end program demo_color
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE COLOR(ICOLOR)

! ident_16="@(#)M_pixel::color(3f): set current color for lines drawn in pixel image"

INTEGER,INTENT(IN) :: ICOLOR
   P_COLOR_INDEX=ICOLOR
END SUBROUTINE COLOR
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     mapcolor(3f) - [M_pixel:COLOR] set a color index using RGB values
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine mapcolor(indx, red, green, blue)
!!    integer indx, red, green, blue
!!
!!##DESCRIPTION
!!    Set the color map index indx to the color represented by (red,
!!    green, blue). rgb values are in the range of 0 to 255.
!!
!!##OPTIONS
!!    INDX    color index number, in range 0 to 255
!!    RED     red component of color being defined, in range 0 to 255
!!    GREEN   green component of color being defined, in range 0 to 255
!!    BLUE    blue component of color being defined, in range 0 to 255
!!
!!##EXAMPLE
!!
!!  Color wheel example:
!!
!!    !     good program to exercise color tables, and look at differences
!!    !     when actual output device has a color table that is dynamic,
!!    !     or only has a small color table (a frame in this program takes
!!    !     at least SLICES*RINGS colors to produce accurately).
!!    !
!!    program demo_mapcolor
!!    use M_pixel
!!    use m_pixel, only: hue
!!    use M_writegif, only : writegif
!!    use M_pixel,    only : cosd, sind
!!    use M_writegif_animated, only : write_animated_gif
!!    implicit none
!!    character(len=4096)  :: filename
!!    real                 :: lightstep
!!    integer              :: ii,iframe
!!    integer,parameter    :: SLICES=30
!!    integer,parameter    :: RINGS=  8
!!    real                 :: LIGHTNESS
!!    integer,parameter    :: BOX=1200
!!    integer              :: movie(1:19,0:box-1,0:box-1)
!!       call prefsize(BOX,BOX)
!!       call vinit(' ')
!!       call color(0)
!!       call clear()
!!       call color(7)
!!       call page(-110./2.,85./2.,-110./2.,110./2.)
!!       LIGHTNESS=100.0
!!       lightstep=-5
!!       do ii=1,19
!!          iframe=ii
!!          call color(0)
!!          call clear()
!!          call color(7)
!!          call wheel()
!!          write(filename,'("mapcolor.3_",i3.3,".gif")')int(LIGHTNESS)
!!          call writegif(filename,P_pixel,P_colormap)
!!          movie(ii,:,:)=P_pixel
!!          LIGHTNESS=LIGHTNESS+LIGHTSTEP
!!       enddo
!!       call write_animated_gif('mapcolor.3m_pixel.gif',movie,P_colormap,delay=40)
!!       call vexit()
!!    contains
!!    !=======================================================================--------
!!    subroutine wheel() ! draw an entire wheel
!!       character(len=40) :: inline
!!       real              :: hue_val
!!       integer           :: ii
!!       call textang(0.0)
!!       call color(7)
!!       call textsize(5.0,6.0)
!!       call font('times.r')
!!       call move2(0.0,103.0/2.0)
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call drawstr('COLOR WHEEL')
!!       call linewidth(0)
!!       call textsize( 2.5,2.5)
!!       call font('futura.l')
!!       call move2(0.0,90.0/2.0)
!!       write(inline,'("lightness=",f6.2)')LIGHTNESS
!!       call linewidth(30)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       call textsize(1.5,1.5)
!!       hue_val=0
!!       do ii=SLICES, 1,-1
!!          call slice(hue_val)
!!       enddo
!!       call centertext(.false.)
!!    end subroutine wheel
!!    !=======================================================================--------
!!    subroutine slice(hue_val) ! draw a slice
!!    integer           :: buffer
!!    real              :: hue_val, ang_inc
!!    character(len=40) :: inline
!!    real              :: step
!!    real              :: X1, X2, X3, X4
!!    real              :: Y1, Y2, Y3, Y4
!!    !
!!    integer           :: maxcolors, current_color
!!    integer           :: ir, ig, ib
!!    real              :: r,g,b
!!    real              :: saturation
!!    !
!!    integer           :: status
!!    integer           :: icount
!!    real              :: angle1, angle2
!!    real              :: radius1, radius2, radius3, radius4
!!    !
!!    integer,save      :: color_count=0
!!    !
!!       buffer=8
!!       ANG_INC=360.0/SLICES
!!       angle1=hue_val-ANG_INC/2
!!       angle2=angle1+ANG_INC
!!       saturation=100
!!       radius1=32
!!       radius3=radius1+4
!!       radius4=radius1+7
!!       ! draw tic from wheel to start of angle label
!!       call color(7)
!!       call linewidth(40)
!!       call move2( radius1*cosd(hue_val), radius1*sind(hue_val) )
!!       call draw2( radius3*cosd(hue_val), radius3*sind(hue_val) )
!!       ! draw degree label at tic
!!       call textang(hue_val)
!!       call move2( radius4*cosd(hue_val), radius4*sind(hue_val) )
!!       write(inline,'(i0)')nint(hue_val)
!!       call linewidth(20)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       step=radius1/(RINGS)
!!       radius2=radius1-step
!!       ! draw a chunk in a slice
!!       MAXCOLORS=(256)-buffer
!!       do icount=RINGS+1,2,-1
!!          CURRENT_COLOR=MOD(color_count,MAXCOLORS)+buffer  ! add buffer to leave base colors alone
!!          color_count=color_count+1
!!          ! fancy mapcolor
!!          call hue("hls",hue_val,LIGHTNESS,saturation,"rgb",r,g,b,status)
!!          ir=int(r*255.0/100.0+0.50)
!!          ig=int(g*255.0/100.0+0.50)
!!          ib=int(b*255.0/100.0+0.50)
!!          call mapcolor(CURRENT_COLOR,ir,ig,ib)
!!          call color(CURRENT_COLOR)
!!          !
!!          X1=cosd(angle1)*radius2
!!          Y1=sind(angle1)*radius2
!!          X2=cosd(angle1)*radius1
!!          Y2=sind(angle1)*radius1
!!          !
!!          X3=cosd(angle2)*radius2
!!          Y3=sind(angle2)*radius2
!!          X4=cosd(angle2)*radius1
!!          Y4=sind(angle2)*radius1
!!          !
!!          call makepoly()
!!          call move2(X1,Y1)
!!          call draw2(X2,Y2)
!!          call draw2(X4,Y4)
!!          call draw2(X3,Y3)
!!          call closepoly()
!!          !
!!          saturation=saturation-100.0/RINGS
!!          radius1=radius2
!!          radius2=radius1-step
!!       enddo
!!       hue_val=hue_val+ANG_INC
!!    end subroutine slice
!!    end program demo_mapcolor
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE MAPCOLOR(INDX,RED,GREEN,BLUE)

! ident_17="@(#)M_pixel::mapcolor(3f): set a color index using RGB values"

INTEGER,INTENT(IN) :: INDX
INTEGER,INTENT(IN) :: RED
INTEGER,INTENT(IN) :: GREEN
INTEGER,INTENT(IN) :: BLUE
   CHECKRANGE: BLOCK

      IF(  INDX .LT. 0 .OR. INDX  .GT. 255) EXIT CHECKRANGE
      IF(   RED .LT. 0 .OR. RED   .GT. 255) EXIT CHECKRANGE
      IF( GREEN .LT. 0 .OR. GREEN .GT. 255) EXIT CHECKRANGE
      IF(  BLUE .LT. 0 .OR. BLUE  .GT. 255) EXIT CHECKRANGE

      P_COLORMAP(:,INDX)= [RED,GREEN,BLUE]
      RETURN

   ENDBLOCK CHECKRANGE
   WRITE(*,*)'*mapcolor* value out of range. input=',INDX,RED,GREEN,BLUE
END SUBROUTINE MAPCOLOR
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     circleprecision(3f) - [M_pixel:ARCS] set number of line segments used to approximate a circle
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine circleprecision(nsegs)
!!    integer   :: nsegs
!!
!!##DESCRIPTION
!!    Set the number of line segments making up a circle. Default is
!!    currently 60. The number of segments in an arc or sector is calculated
!!    from the variable "nsegs" according to the span of the arc or sector.
!!
!!##OPTIONS
!!    NSEGS   number of line segments making up a circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circleprecision
!!    use M_pixel
!!    use M_writegif, only : writegif
!!    implicit none
!!    real              :: b=0.5
!!    real              :: y1,y2,ym,x1,x2
!!    real              :: width=50.0/8.0,width2
!!    integer,parameter :: ivals(*)=[3,5,7,10,20,30,60,100]
!!    integer           :: i
!!       !! set up long bar as plotting area
!!       call prefsize(1000,200)
!!       call vinit()
!!       call ortho2(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!       call textsize( 2.5/2.0, 3.0/2.0)
!!       call font('DUPLEX')
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call color(2)
!!       y1=-5
!!       y2=5
!!       ym=0
!!       x1=-25+.05*width
!!       ! draw colored rectangle and a circle and label center of circle repeat
!!       width2=width*0.95
!!       do i=1,size(ivals)
!!          x2=x1+width2
!!          call move2((x1+x2)/2.0,ym)
!!          call circleprecision(ivals(i))
!!          call drawstr(ivals(i))     ! convert number to string and draw it
!!          call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!          x1=x1+width
!!       enddo
!!       ! write plot as GIF file
!!       call writegif('circleprecision.3m_pixel.gif',P_pixel,P_colormap)
!!       call vexit()
!!       ! use system to display GIF file
!!       call execute_command_line('display circleprecision.3m_pixel.gif')
!!    end program demo_circleprecision
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE CIRCLEPRECISION(NSEGS)

! ident_18="@(#)M_pixel::circleprecision(3f): set number of line segments making up a circle"

INTEGER,INTENT(IN) :: NSEGS
   P_NSEGS=NSEGS
END SUBROUTINE CIRCLEPRECISION
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getviewport(3f) - [M_pixel] return viewport in screen pixel coordinates
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine getviewport(left, right, bottom, top)
!!    real,intent(out)    :: left
!!    real,intent(out)    :: right
!!    real,intent(out)    :: bottom
!!    real,intent(out)    :: top
!!
!!##DESCRIPTION
!! Returns the left, right, bottom and top limits of the current viewport
!! in screen coordinates (-1.0 to 1.0).
!!
!!     Fortran:
!!          subroutine getviewport(left, right, bottom, top)
!!          real left, right, bottom, top
!!    If a pixel array has been declared to be real :: array(600,400)
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!!
!!##OPTIONS
!!    LEFT     value for left side
!!    RIGHT    value for right side
!!    BOTTOM   value for bottom side
!!    TOP      value for top side
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE GETVIEWPORT(LEFT,RIGHT,BOTTOM,TOP)

! ident_19="@(#)M_pixel::getviewport(3f): return viewport in screen pixel coordinates"

REAL,INTENT(OUT)    :: LEFT
REAL,INTENT(OUT)    :: RIGHT
REAL,INTENT(OUT)    :: BOTTOM
REAL,INTENT(OUT)    :: TOP

   LEFT    =  P_VIEWPORT_LEFT
   RIGHT   =  P_VIEWPORT_RIGHT
   BOTTOM  =  P_VIEWPORT_BOTTOM
   TOP     =  P_VIEWPORT_TOP

END SUBROUTINE GETVIEWPORT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    viewport(3f) - [M_pixel] Specify which part of the screen to draw in.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine viewport(left, right, bottom, top)
!!    real,intent(in) :: left, right, bottom, top
!!
!!##DESCRIPTION
!!    Specify which part of the screen to draw in. Left, right, bottom,
!!    and top are real values in screen coordinates (0:n,0:m).
!!
!!    If a pixel array has been declared to be real :: array(600,400)
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!!
!!##EXAMPLE
!!
!!  Sample program
!!
!!    program demo_viewport
!!    use :: M_pixel
!!    use :: M_writegif, only : writegif
!!    implicit none
!!       call prefsize(400, 400) ! set up drawing surface
!!       call vinit()
!!       call color(7)
!!       call linewidth(40)
!!       call clear()
!!       call ortho2(-88.0, 88.0, -88.0, 88.0)
!!       ! draw the same circle, just changing viewport
!!
!!       call viewport(   0.0, 200.0,   0.0, 200.0 ); call draw_circle(1)
!!       call viewport( 200.0, 400.0,   0.0, 200.0 ); call draw_circle(2)
!!       call viewport(   0.0, 200.0, 200.0, 400.0 ); call draw_circle(3)
!!       call viewport( 200.0, 400.0, 200.0, 400.0 ); call draw_circle(4)
!!       call viewport( 250.0, 350.0, 150.0, 300.0 ); call draw_circle(5)
!!
!!       call writegif('viewport.3m_pixel.gif',P_pixel,P_colormap)
!!       !call execute_command_line('display viewport.3m_pixel.gif')
!!       call vexit()
!!    contains
!!    subroutine draw_circle(icolor)
!!    integer,intent(in) :: icolor
!!       call color(0)
!!       call rect(-88.0,-88.0,88.0,88.0)
!!       call color(icolor)
!!       call makepoly()
!!       call circle(0.0,0.0,88.0)
!!       call closepoly()
!!    end subroutine draw_circle
!!    end program demo_viewport
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE VIEWPORT(LEFT,RIGHT,BOTTOM,TOP)

! ident_20="@(#)M_pixel::viewport(3f): Specify which part of the screen to draw in."

REAL,INTENT(IN) :: LEFT, RIGHT, BOTTOM, TOP

   P_VIEWPORT_LEFT=LEFT
   P_VIEWPORT_RIGHT=RIGHT
   P_VIEWPORT_BOTTOM=BOTTOM   ! pixel row,column has (0,0) in upper left so switch top and bottom
   P_VIEWPORT_TOP=TOP
   CALL MAPPING()

END SUBROUTINE VIEWPORT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mapping(3fp) - [M_pixel] calculate conversion factors between viewport and world window
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine mapping()
!!
!!##DESCRIPTION
!!    calculate conversion factors between viewport and world window
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE MAPPING()
!-!use M_math,only : invert_4x4

! ident_21="@(#)M_pixel::mapping(3fp): calculate conversion factors between viewport and world window"

   REAL, DIMENSION(4,4) :: VIEWPORT,VIEWPORT_INV
   REAL, DIMENSION(4)   :: WINDOW, FACTORS

   VIEWPORT(1,:)=[ P_VIEWPORT_LEFT,   P_VIEWPORT_BOTTOM, 1.0, 0.0 ]
   VIEWPORT(2,:)=[-P_VIEWPORT_BOTTOM, P_VIEWPORT_LEFT,   0.0, 1.0 ]
   VIEWPORT(3,:)=[ P_VIEWPORT_RIGHT,  P_VIEWPORT_TOP,    1.0, 0.0 ]
   VIEWPORT(4,:)=[-P_VIEWPORT_TOP,    P_VIEWPORT_RIGHT,  0.0, 1.0 ]

   WINDOW=[P_WINDOW_LEFT,P_WINDOW_BOTTOM,P_WINDOW_RIGHT,P_WINDOW_TOP]

   VIEWPORT_INV=INVERT_4X4(VIEWPORT)

   FACTORS=MATMUL(VIEWPORT_INV,WINDOW)
   P_A=FACTORS(1)
   P_B=FACTORS(2)
   P_C=FACTORS(3)
   P_D=FACTORS(4)
END SUBROUTINE MAPPING
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE WORLD2VIEWPORT(XW,YW,XV,YV)

! ident_22="@(#)M_pixel::world2viewport(3fp): convert world coordinates to viewports"

REAL,INTENT(IN)  :: XW,YW
REAL,INTENT(OUT) :: XV,YV

   XV = (P_A*XW + P_B*YW - P_B*P_D - P_A*P_C)/(P_A**2 + P_B**2)
   YV = (P_B*XW - P_A*YW - P_B*P_C + P_A*P_D)/(P_A**2 + P_B**2)

END SUBROUTINE WORLD2VIEWPORT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE VIEWPORT2WORLD(XV,YV,XW,YW)

! ident_23="@(#)M_pixel::viewport2world(3fp): convert viewport to world coordinates"

REAL,INTENT(IN)  :: XV,YV
REAL,INTENT(OUT) :: XW,YW

   XW = P_A*XV + P_B*YV + P_C
   YW = P_B*XV - P_A*YV + P_D

END SUBROUTINE VIEWPORT2WORLD
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    ortho2(3f) - [M_pixel] define the area of the virtual world coordinates to map to the viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine ortho2(left, right, bottom, top)
!!    real,intent(in) :: left, right, bottom, top
!!
!!##DESCRIPTION
!!    Defines the section of the virtual world coordinates to map to the
!!    viewport. All the projection routines define a new transformation
!!    matrix, and consequently the world units. Parallel projections are
!!    defined by ortho2.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE ORTHO2(LEFT, RIGHT, BOTTOM, TOP)

! ident_24="@(#)M_pixel::ortho2(3f): define the area of the virtual world coordinates to map to the viewport"

REAL,INTENT(IN) :: LEFT, RIGHT, BOTTOM, TOP ! Define x (left, right), and y (bottom, top) clipping planes.

   P_WINDOW_LEFT=LEFT
   P_WINDOW_RIGHT=RIGHT
   P_WINDOW_BOTTOM=BOTTOM
   P_WINDOW_TOP=TOP
   CALL MAPPING()

END SUBROUTINE ORTHO2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    page(3f) - [M_pixel] define the area of the virtual world coordinates to map to the viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine page(left, right, bottom, top)
!!    real,intent(in) :: left, right, bottom, top
!!
!!##DESCRIPTION
!!    Defines the section of the virtual world coordinates to map to
!!    the viewport. Automatically use the largest viewport that provides
!!    one-to-one correspondence between the window and the viewport.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE PAGE(XSMALL,XLARGE,YSMALL,YLARGE)
!use M_journal, only : journal

! ident_25="@(#)M_pixel::page(3f): given a window size, find and set to largest accommodating viewport"

REAL,INTENT(IN) :: XSMALL
REAL,INTENT(IN) :: XLARGE
REAL,INTENT(IN) :: YSMALL
REAL,INTENT(IN) :: YLARGE

REAL :: RPS
REAL :: SPR
REAL :: TRYX
REAL :: TRYY
REAL :: VHIGH
REAL :: VWIDE
REAL :: XDELTA
REAL :: XMAX
REAL :: XMIN
REAL :: XSPLIT
REAL :: YDELTA
REAL :: YMAX
REAL :: YMIN
REAL :: YSPLIT

!
!     given a window size, and assuming a one-to-one correspondence of window
!     units (ie. an "x-unit" is as long as a "y-unit"), find the largest area
!     on the display surface that has the same aspect ratio, and set the
!     viewport to it.
!     assumes that the screen rasters are square.
!
      CALL GETDISPLAYSIZE(VWIDE,VHIGH) !get screen size in terms of raster units
!
!     the default viewport is in "screen units", and goes from top-left of 0,0
!     to bottom-right of vwide,vhigh
!     all new viewports are defined in terms of this original viewport.
!
      RPS=1.0                          ! number of rasters per screen unit
      SPR=1.0                          ! number of screen units per raster
      TRYX=VWIDE                       ! make as wide as display as a trial fit
      IF(XLARGE-XSMALL.NE.0.0)THEN
         TRYY=VWIDE*(YLARGE-YSMALL)/(XLARGE-XSMALL) ! calculate required height
      ELSE                             ! ERROR: do something desperate
         CALL JOURNAL('*P_page* window has a zero X dimension')
         TRYY=VHIGH
      ENDIF
      IF(TRYY.GT.VHIGH)THEN ! if required height too great, fit with y maximized
         TRYY=VHIGH
         IF(YLARGE-YSMALL.NE.0.0)THEN
            TRYX=VHIGH*(XLARGE-XSMALL)/(YLARGE-YSMALL)
         ELSE                          ! ERROR: do something desperate
            CALL JOURNAL('*P_page* window has a zero Y dimension')
            TRYX=VWIDE
         ENDIF
      ENDIF
!
!   tryx and tryy are now the required viewport in raster units. The raster
!   units now need converted to screen units to be used in viewport procedure
!
!   some explanation of physical viewport units is required:
!   assuming maximizing the required aspect ratio in the available drawing area,
!   and that the original viewport "origin" 0,0 stays in its original position,
!   and that the original -1,1,-1,1 viewport is the largest square that can fit
!   on the display, bottom left justified.
!   the screen coordinate system is a right-handed Cartesian coordinate system
!   with positive x to the viewer's right, positive y up.
!
!   at this point,
!    vwide=width in rasters of entire display
!    vhigh=height in rasters of entire display
!   assuming a square raster
!     tryx is desired width in rasters
!     tryy is desired height in rasters
!
      XDELTA=TRYX-2.0*RPS  ! need this many more rasters in x direction from 1,1
      YDELTA=TRYY-2.0*RPS  ! need this many more rasters in y direction from 1,1
      ! to center (to left bottom justify, make xsplit and ysplit 0)
      XSPLIT=(VWIDE-TRYX)/2.0
      YSPLIT=(VHIGH-TRYY)/2.0
      XMAX=1+XDELTA*SPR+XSPLIT*SPR
      YMAX=1+YDELTA*SPR+YSPLIT*SPR
      XMIN=-1+XSPLIT*SPR
      YMIN=-1+YSPLIT*SPR
      IF(P_DEBUG)THEN
         WRITE(*,*)'max. display area is', VWIDE, ' by ',VHIGH,' rasters'
         WRITE(*,*)'shape is ',XSMALL,XLARGE,YSMALL,YLARGE
         WRITE(*,*)'attempting to get a viewport of ',TRYX,' by ',TRYY
         WRITE(*,*)'needed more rasters, ',XDELTA,' by ',YDELTA
         WRITE(*,*)'resulted in viewport ',XMIN,XMAX,YMIN,YMAX
      ENDIF
      IF(XMIN.NE.XMAX.AND.YMIN.NE.YMAX)THEN
         CALL VIEWPORT(XMIN,XMAX,YMAX,YMIN)
      ELSE
       CALL JOURNAL('*P_page* window has zero dimension,no viewport set')
      ENDIF
      IF(XSMALL.NE.XLARGE.AND.YSMALL.NE.YLARGE)THEN
         CALL ORTHO2(XSMALL,XLARGE,YSMALL,YLARGE)
      ELSE    ! ERROR: do something desperate
        CALL JOURNAL('*P_page* window has zero dimension, no window set')
      ENDIF
END SUBROUTINE PAGE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    rmove2(3f) - [M_pixel:DRAW] relative move
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine rmove2(deltax, deltay)
!!    real,intent(in) :: deltax, deltay
!!
!!##DESCRIPTION
!!    Update current position.
!!    Relative move2. deltax and deltay are offsets in world units.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rmove2
!!      use M_pixel, only: prefsize, vinit, ortho2, clear
!!      use M_pixel, only: move2, rmove2, rdraw2, vexit
!!      use M_pixel, only: linewidth
!!      use M_pixel, only: P_pixel, P_colormap
!!      use M_writegif, only : writegif
!!      implicit none
!!      integer :: i
!!         call prefsize(500,500)
!!         call vinit()
!!         call ortho2(-110.0,110.0,-110.0,110.0)
!!         call move2(-100.0,-100.0)
!!         call linewidth(70)
!!         do i=1,20
!!            call rmove2(10.0, 0.0)
!!            call rdraw2( 0.0,10.0)
!!         enddo
!!         call writegif('rmove2.3m_pixel.gif',P_pixel,P_colormap)
!!         call  execute_command_line('display rmove2.3m_pixel.gif')
!!         call vexit()
!!      end program demo_rmove2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE RMOVE2(XDELTA,YDELTA)

! ident_26="@(#)M_pixel::rmove2(3f): relative move"

REAL,INTENT(IN) :: XDELTA
REAL,INTENT(IN) :: YDELTA

   P_X=P_X+XDELTA
   P_Y=P_Y+YDELTA

END SUBROUTINE RMOVE2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    move2(3f) - [M_pixel:DRAW] change current position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine move2(x, y)
!!    real x, y
!!
!!##DESCRIPTION
!!    Update current position.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_move2
!!      use M_pixel, only : prefsize, vinit, ortho2, clear
!!      use M_pixel, only : move2, draw2, vexit
!!      use M_pixel, only : P_pixel,P_colormap
!!      use M_writegif, only : writegif
!!      implicit none
!!         call prefsize(60,40)
!!         call vinit()
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call clear(0)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!         call writegif('move2.3m_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!      end program demo_move2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE MOVE2(X,Y)

! ident_27="@(#)M_pixel::move2(3f): move current position"

REAL,INTENT(IN) :: X,Y

   P_X=X
   P_Y=Y

END SUBROUTINE MOVE2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    rdraw2(3f) - [M_pixel:DRAW] draw from current position to given point
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine rdraw2(x, y)
!!    real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Relative draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rdraw2
!!      use M_pixel, only: vinit, prefsize, ortho2,linewidth
!!      use M_pixel, only: clear, move2, rdraw2, vexit,color
!!      use M_pixel, only: P_pixel, P_colormap
!!      use M_writegif, only : writegif
!!      implicit none
!!
!!         call prefsize(200,200)
!!         call vinit()
!!         call ortho2(-55.0, 55.0, -55.0,  55.0)
!!         call linewidth(400)
!!         call color(7)
!!         call clear()
!!
!!         call color(1)
!!         call move2(-50.0,0.0)
!!         call square(50.0)
!!
!!         call linewidth(200)
!!         call color(2)
!!         call move2(  0.0,-50.0)
!!         call square(50.0)
!!
!!         call writegif('rdraw2.3m_pixel.gif',P_pixel,P_colormap)
!!         call execute_command_line('display rdraw2.3m_pixel.gif')
!!         call vexit()
!!
!!         contains
!!
!!         subroutine square(side)
!!         real,intent(in) :: side
!!         call rdraw2( side,   0.0)
!!         call rdraw2(  0.0,  side)
!!         call rdraw2(-side,   0.0)
!!         call rdraw2(  0.0, -side)
!!         end subroutine square
!!
!!      end program demo_rdraw2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE RDRAW2(XDELTA,YDELTA)

! ident_28="@(#)M_pixel::rdraw2(3f): relative draw"

REAL,INTENT(IN) :: XDELTA
REAL,INTENT(IN) :: YDELTA
REAL            :: P_X_TMP
REAL            :: P_Y_TMP

   P_X_TMP=P_X
   P_Y_TMP=P_Y
   CALL LINE( P_X_TMP, P_Y_TMP, P_X_TMP+XDELTA, P_Y_TMP+YDELTA )

END SUBROUTINE RDRAW2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    draw2(3f) - [M_pixel:DRAW] draw from current position to given point
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine draw2(x, y)
!!    real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_draw2
!!    use M_pixel,    only : prefsize, vinit, ortho2, clear
!!    use M_pixel,    only : move2, draw2, vexit, color,linewidth
!!    use M_pixel,    only : P_pixel, P_colormap
!!    use M_writegif, only : writegif
!!    use M_pixel,    only : d2r, polar_to_cartesian
!!    !
!!    ! The Archimedean spiral is the locus of points corresponding
!!    ! to the locations over time of a point moving away from a
!!    ! fixed point with a constant speed along a line which rotates
!!    ! with constant angular velocity.
!!    !    r=A+B*theta
!!    ! Changing the parameter A will turn the spiral,
!!    ! while B controls the distance between successive turnings.
!!    !
!!    implicit none
!!    integer        :: i
!!    real           :: x,y,radius,theta
!!    real,parameter :: rotate=0.0, gap=2.0
!!       call prefsize(400,400)
!!       call vinit('')
!!       call ortho2(-150.0,150.0,-150.0,150.0)
!!       call color(5)
!!       call clear()
!!       call move2(0.0,0.0)
!!       call color(0)
!!       call linewidth(40)
!!       do i=0,360*10,5
!!          theta=d2r(i)
!!          ! equation in polar coordinates
!!          radius=rotate+gap*theta
!!          ! convert polar coordinates to cartesian
!!          call polar_to_cartesian(radius,theta,x,y)
!!          ! draw from current position to end of next segment
!!          call draw2(x,y)
!!       enddo
!!       ! write the pixel map array as a GIF image file
!!       call writegif('draw2.3m_pixel.gif',P_pixel,P_colormap)
!!       ! exit graphics mode
!!       call vexit()
!!    end program demo_draw2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE DRAW2(X,Y)

! ident_29="@(#)M_pixel::draw2(3f): draw a line from current position to specified point"

REAL,INTENT(IN) :: X
REAL,INTENT(IN) :: Y
REAL            :: P_X_TMP
REAL            :: P_Y_TMP

   P_X_TMP=P_X
   P_Y_TMP=P_Y
   CALL LINE( P_X_TMP, P_Y_TMP, X, Y )

END SUBROUTINE DRAW2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    prefsize(3f) - [M_pixel] specify size of pixel array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine prefsize(width, height)
!!    integer width, height
!!
!!##DESCRIPTION
!!    Specify the preferred width and height of the pixel array opened
!!    by the *next* vinit(3f). The pixel array is then available via
!!    the M_pixel(3fm) module as variable P_pixel. Note that the width
!!    corresponds to the number of rows in the array, and height to the
!!    number of columns.
!!
!!##OPTIONS
!!    WIDTH   width of pixel array to create when vinit(3f) is called
!!    HEIGHT  height of pixel array to create when vinit(3f) is called
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_prefsize
!!      use M_pixel, only: prefsize, vinit, ortho2, clear
!!      use M_pixel, only: move2, draw2, vexit, color
!!      use M_pixel, only : P_pixel,P_colormap
!!      use M_writegif, only : writegif
!!      implicit none
!!         ! make first file with one size
!!         call prefsize(60*2,40*2)
!!         call vinit()
!!         call picture()
!!         call writegif('prefsize.3m_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!
!!         ! make second file with another size
!!         call prefsize(60*3,40*3)
!!         call vinit()
!!         call picture()
!!         call writegif('prefsize_B.3m_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!      contains
!!      subroutine picture
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call clear(0)
!!         call color(1)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!      end subroutine picture
!!      end program demo_prefsize
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE PREFSIZE(X,Y)

! ident_30="@(#)M_pixel::prefsize(3f): specify size of pixel array"

INTEGER,INTENT(IN) :: X
INTEGER,INTENT(IN) :: Y

   P_VIEWPORT_WIDTH=X
   P_VIEWPORT_HEIGHT=Y

END SUBROUTINE PREFSIZE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    vexit(3f) - [M_pixel] exit pixel graphics mode
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine vexit()
!!
!!##DESCRIPTION
!!    Used to terminate pixel graphics mode. Does any actions required to
!!    terminate graphics mode including unallocating the module pixel array
!!    P_pixel. Required before calling vinit(3f) more than once.
!!
!!    Resets the window/terminal (must be the last ModLib_Pixel routine called).
!!
!!##OPTIONS
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_vexit
!!      use M_pixel, only: prefsize, vexit, ortho2, clear
!!      use M_pixel, only: move2, draw2, color, vinit
!!      use M_pixel, only : P_pixel,P_colormap
!!      use M_writegif, only : writegif
!!      implicit none
!!         call prefsize(60,40)
!!         call vinit()
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call clear(0)
!!         call color(1)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!         call writegif('vexit.3m_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!      end program demo_vexit
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE VEXIT()

! ident_31="@(#)M_pixel::vexit(3f): exit pixel array drawing module"

   IF(ALLOCATED(P_PIXEL))THEN
      DEALLOCATE(P_PIXEL)
   ENDIF
   P_VINIT_CALLED=.FALSE.

END SUBROUTINE VEXIT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    vinit(3f) - [M_pixel] initialize pixel graphics module
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!   subroutine vinit()
!!
!!##DESCRIPTION
!!    Initialize the pixel graphics module. The pixel array P_pixel and the
!!    colormap P_ColorMap are directly accessible after the call to allow
!!    display or printing
!!
!!##OPTIONS
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_vinit
!!      use M_pixel, only    : prefsize, vinit, ortho2, clear
!!      use M_pixel, only    : move2, draw2, vexit, color
!!      use M_pixel, only    : P_pixel, P_colormap
!!      use M_writegif, only : writegif
!!      implicit none
!!         call prefsize(60,40)
!!         call vinit()
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call clear(0)
!!         call color(1)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!         call writegif('vinit.3m_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!      end program demo_vinit
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE VINIT(STRING)

! ident_32="@(#)M_pixel::vinit(3f): initialize pixel array drawing module"

CHARACTER(LEN=*),OPTIONAL :: STRING

   P_X=0                                ! initialize current position
   P_Y=0

   IF(ALLOCATED(P_PIXEL))THEN
      DEALLOCATE(P_PIXEL)
   ENDIF
   ALLOCATE(P_PIXEL(0:P_VIEWPORT_WIDTH-1,0:P_VIEWPORT_HEIGHT-1))
   P_VINIT_CALLED=.TRUE.
   P_PIXEL=0

   P_WIDTH=1             ! line width
   P_COLOR_INDEX=1       ! pen color
   P_NSEGS=60            ! number of line segments making up a circle

!  If a pixel array has been declared to be real :: array(600,400)
!
!       o-----> X                         (right=600,top=0)
!       | #------------------------------------#
!       | |                                    |
!       | |                                    |
!       V |                                    |
!       Y |                                    |
!         #------------------------------------#
!    (left=0,bottom=400)
   P_VIEWPORT_LEFT=0.0
   P_VIEWPORT_RIGHT=REAL(P_VIEWPORT_WIDTH-1)
   P_VIEWPORT_BOTTOM=REAL(P_VIEWPORT_HEIGHT-1)
   P_VIEWPORT_TOP=0.0

   P_WINDOW_LEFT=0.0
   P_WINDOW_RIGHT=REAL(P_VIEWPORT_WIDTH)
   P_WINDOW_BOTTOM=0.0
   P_WINDOW_TOP=REAL(P_VIEWPORT_HEIGHT)
   CALL MAPPING()

   P_TEXT_HEIGHT=10.0
   P_TEXT_WIDTH=7.0
   P_TEXT_ANGLE=0.0
   P_TEXT_COSINE=1.0
   P_TEXT_SINE  =0.0
   P_X_CENTERTEXT=.FALSE.
   P_Y_CENTERTEXT=.FALSE.
   P_FONT='SIMPLEX'

   P_INPOLYGON=.FALSE.
   P_POLYVERTEX=1

END SUBROUTINE VINIT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    makepoly(3f) - [M_pixel:POLYGONS] opens polygon constructed by a series of move-draws and closed by closepoly                  |
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine makepoly()
!!
!!##DESCRIPTION
!!    MAKEPOLY(3f) opens up a polygon which will then be constructed by a
!!    series of move-draws and closed by a CLOSEPOLY(3f).
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program demo_makepoly
!!    use :: M_pixel
!!    use :: M_writegif, only : writegif
!!    use :: M_writegif_animated, only : write_animated_gif
!!    implicit none
!!    integer,parameter :: wide=640, tall=640
!!    integer :: rows, xoff, yoff, box_sz
!!    integer :: i20, i30, ncols, nrows, ilines
!!    real    :: bottom, left, sun_radius, planet_radius, planet_offset
!!    character(len=40) :: filename
!!    integer :: movie(300,0:wide-1,0:tall-1)
!!       call prefsize(wide,tall)
!!       call vinit()
!!       call ortho2(0.0, real(wide), 0.0, real(tall) )
!!       ! call linewidth(3) ! really slows down pbm driver because all lines are polygons
!!       call color(7)
!!       call clear()
!!       call color(0)
!!       rows=1
!!       box_sz=MIN(wide,tall)/rows       ! size of biggest box to use and get specified number of rows
!!       nrows = tall/box_sz              ! number of rows of objects to draw
!!       ncols = wide/box_sz              ! number of columns of objects to draw
!!       xoff = (wide - ncols * box_sz)/2 ! initial x offset to begin row at to center drawings
!!       yoff = (tall - nrows * box_sz)/2 ! initial x offset to begin column at to center drawings
!!       sun_radius = 148
!!       planet_radius = 1
!!       do ilines = 1, 300
!!          do i20 = 1, ncols
!!             left = (i20-1)*box_sz+xoff
!!             do i30 = 1, nrows
!!                bottom = (i30-1)*box_sz+yoff
!!                call color(0)
!!             call makepoly()
!!                call rect(left,bottom,left+box_sz,bottom+box_sz)
!!             call closepoly()
!!                planet_offset= sun_radius
!!                   call color(mod(ilines,15)+1)
!!                   call hypoc(left + box_sz/2.0, bottom + box_sz/2.0, &
!!                & sun_radius, planet_radius, planet_offset, &
!!                & box_sz/2.0, ilines,  &
!!                & 0.0, 0.0, 1)
!!             enddo
!!          enddo
!!          movie(ilines,:,:)=P_pixel
!!          write(filename,'("hypoc.",i0,".gif")')ilines
!!          !!call writegif(filename,P_pixel,P_colormap)
!!       enddo
!!       call write_animated_gif('makepoly.3m_pixel.gif',movie,P_colormap,delay=70)
!!       call vexit()
!!    contains
!!    !
!!    !  Make shapes using hypocycloidal curves.
!!    !
!!    subroutine hypoc(xcenter,ycenter,sunr0,planet0,offset0,radius,ilines,ang,angs,ifill)
!!    use M_pixel
!!    implicit none
!!    real,parameter     :: PI= 3.14159265358979323846264338327950288419716939937510
!!    real,intent(in)    :: xcenter, ycenter      ! center of curve
!!    real,intent(in)    :: sunr0,planet0,offset0 ! radii of sun, planet, and planet offset
!!    real,intent(in)    :: radius                ! radius to fit the shape to (no fit if radius is 0)
!!    integer,intent(in) :: ilines                ! number of points to sample along curve
!!    real,intent(in)    :: ang                   ! angle to rotate the shape by, to orientate it.
!!    real,intent(in)    :: angs                  ! angle to start sampling points at; ccw is +; 0 is East
!!    integer,intent(in) :: ifill                 ! 1 make a filled polygon, 2 make a hatched polygon
!!    integer            :: i10
!!    real               :: ang1, con1, con2, factor
!!    real               :: offset, planet, r, sunr, u
!!    real               :: xpoin, xpoin1, ypoin, ypoin1
!!       sunr=sunr0
!!       offset=offset0
!!       planet=planet0
!!       if(ilines.eq.0.0) return
!!       if(planet.eq.0.0) return
!!       if(sunr.eq.0.0)   return
!!       if(radius.ne.0.and.sunr-planet+offset.ne.0)then
!!          factor=radius/(sunr-planet+offset)
!!          sunr=factor*sunr
!!          planet=factor*planet
!!          offset=factor*offset
!!       endif
!!       u=0.0+ang
!!       con1=PI*2.*(sunr/planet)/real(ilines)
!!       con2=(1.0-planet/sunr)*u
!!       xpoin1=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!       ypoin1=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!       ang1=atan2(ypoin1,xpoin1)+angs
!!       r=sqrt(xpoin1**2+ypoin1**2)
!!       xpoin1=r*cos(ang1)+xcenter
!!       ypoin1=r*sin(ang1)+ycenter
!!       select case(ifill)
!!       case(:0)
!!       case(1:)
!!          call makepoly()
!!       end select
!!       call move2(xpoin1,ypoin1)
!!       do i10=1,ilines
!!          u=con1*i10+ang
!!          con2=(1.0-planet/sunr)*u
!!          if(con2.ge.2**24) con2=amod(con2,PI)
!!          xpoin=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!          ypoin=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!          ang1=atan2(ypoin,xpoin)+angs
!!          r=sqrt(xpoin**2+ypoin**2)
!!          xpoin=r*cos(ang1)+xcenter
!!          ypoin=r*sin(ang1)+ycenter
!!          call draw2(xpoin,ypoin)
!!       enddo
!!       call draw2(xpoin1,ypoin1)
!!       if(ifill.gt.0)then
!!         call closepoly()
!!       endif
!!    end subroutine hypoc
!!    end program demo_makepoly
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE MAKEPOLY()

! ident_33="@(#)M_pixel::makepoly(3f): opens polygon constructed by a series of move-draws and closed by closepoly"

   P_INPOLYGON=.TRUE.
   P_POLYVERTEX=1
END SUBROUTINE MAKEPOLY
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    closepoly(3f) - [M_pixel:POLYGONS] Terminates a polygon opened by makepoly(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!       subroutine closepoly()
!!
!!##DESCRIPTION
!!    Terminates a polygon opened by MAKEPOLY(3f).
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE CLOSEPOLY()

! ident_34="@(#)M_pixel::makepoly(3f): terminate a polygon opened by makepoly(3f)"

   P_INPOLYGON=.FALSE.
   CALL POLY2(P_POLYVERTEX-1,P_POLYPOINTS)
END SUBROUTINE CLOSEPOLY
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    print_ppm(3f) - [M_pixel:PRINT] print pixel array as a ppm file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine print_ppm(filename)
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!   This driver makes an P6 PPM(portable pixmap) file. Any
!!   existing file will be appended to.
!!
!!##OPTIONS
!!   FILENAME  name of output file to create or append to.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_print_ppm
!!      use M_pixel, only : prefsize,vinit,ortho2,vexit
!!      use M_pixel, only : linewidth,circle,color
!!      use M_pixel, only : print_ppm
!!      implicit none
!!         call prefsize(40,40)
!!         call vinit()
!!         call ortho2(-100.0,100.0,-100.0,100.0)
!!         call linewidth(400)
!!         call circle(0.0,0.0,45.0)
!!         call color(3)
!!         call circle(0.0,0.0,25.0)
!!         call print_ppm('demo_print.ppm')
!!         call vexit()
!!      end program demo_print_ppm
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE PRINT_PPM(FILENAME)

! ident_35="@(#)M_pixel::print_ppm(3f): print pixel array as a P6 PPM file, appending to any existing file"

CHARACTER(LEN=*),INTENT(IN) :: FILENAME
INTEGER                     :: LUN,IOS
CHARACTER(LEN=4096)         :: MESSAGE

   OPEN(NEWUNIT=LUN,FILE=FILENAME, &
      & STATUS='unknown',          & !  STATUS    =  NEW        | REPLACE     | OLD    | SCRATCH | UNKNOWN
      & ACCESS='stream',           & !  ACCESS    =  SEQUENTIAL | DIRECT      | STREAM
      & ACTION='write',            & !  ACTION    =  READ|WRITE | READWRITE
      & POSITION='append',         & !  POSITION  =  ASIS       | REWIND      | APPEND
      & FORM='unformatted',        & !  FORM      =  FORMATTED  | UNFORMATTED
      & IOSTAT=IOS,                &
      & IOMSG=MESSAGE)
   IF(IOS.NE.0)THEN
       WRITE(*,'(a)')'<ERROR>*p6out*: writing '//TRIM(FILENAME)//':'//TRIM(MESSAGE)
   ELSE
      CALL OUTPUT_PPM(LUN)
   ENDIF
END SUBROUTINE PRINT_PPM
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    print_p6(3f) - [M_pixel:PRINT] print pixel array as a ppm file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine print_p6(filename)
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!   This driver makes an P6 PPM(portable pixmap) file. Any
!!   existing file will be replaced.
!!
!!##OPTIONS
!!   FILENAME  name of output file to create.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_print_p6
!!      use M_pixel, only : prefsize,vinit,ortho2,vexit
!!      use M_pixel, only : linewidth,circle,color
!!      use M_pixel, only : print_p6
!!      implicit none
!!         call prefsize(40,40)
!!         call vinit()
!!         call ortho2(-100.0,100.0,-100.0,100.0)
!!         call linewidth(400)
!!         call circle(0.0,0.0,45.0)
!!         call color(3)
!!         call circle(0.0,0.0,25.0)
!!         call print_p6('demo_print.p6')
!!         call vexit()
!!      end program demo_print_p6
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE PRINT_P6(FILENAME)
! ident_36="@(#)M_pixel::print_p6(3f): print pixel array as a P6 PPM file, replacing any existing file"

CHARACTER(LEN=*),INTENT(IN) :: FILENAME
INTEGER                     :: LUN,IOS
CHARACTER(LEN=4096)         :: MESSAGE

   OPEN(NEWUNIT=LUN,FILE=FILENAME, &
      & STATUS='replace',          & !  STATUS    =  NEW        | REPLACE     | OLD    | SCRATCH | UNKNOWN
      & ACCESS='stream',           & !  ACCESS    =  SEQUENTIAL | DIRECT      | STREAM
      & ACTION='write',            & !  ACTION    =  READ|WRITE | READWRITE
      & POSITION='rewind',         & !  POSITION  =  ASIS       | REWIND      | APPEND
      & FORM='unformatted',        & !  FORM      =  FORMATTED  | UNFORMATTED
      & IOSTAT=IOS,                &
      & IOMSG=MESSAGE)
   IF(IOS.NE.0)THEN
       WRITE(*,'(a)')'<ERROR>*p6out*: writing '//TRIM(FILENAME)//':'//TRIM(MESSAGE)
   ELSE
      CALL OUTPUT_PPM(LUN)
   ENDIF
END SUBROUTINE PRINT_P6
!==================================================================================================================================!
SUBROUTINE OUTPUT_PPM(LUN)
! ident_37="@(#)M_pixel::output_ppm(3f): print pixel array as a PPM file"
INTEGER,INTENT(IN)  :: LUN
INTEGER             :: IOS
INTEGER             :: I, J
CHARACTER(LEN=100)  :: MESSAGE
   CALL IF_INIT()
   ASSOCIATE( XS=>SIZE(P_PIXEL,DIM=1), YS=>SIZE(P_PIXEL,DIM=2), CS=>SIZE(P_COLORMAP,DIM=2) )
    WRITE(MESSAGE,'(''P6'', 3(1X,I0))')  XS, YS , CS-1 ! HEADER
    WRITE(LUN)TRIM(MESSAGE)//' '
    IF(CS-1.GT.255)THEN
       WRITE(LUN) ((NUM2BYTES2(P_COLORMAP(1:3,P_PIXEL(I,J))),I=0,XS-1),J=0,YS-1)
    ELSE
       WRITE(LUN) ((CHAR(P_COLORMAP(1:3,P_PIXEL(I,J))),I=0,XS-1),J=0,YS-1)
    ENDIF
   END ASSOCIATE
   FLUSH(UNIT=LUN,IOSTAT=IOS)
   CLOSE(UNIT=LUN,IOSTAT=IOS)
END SUBROUTINE OUTPUT_PPM
!==================================================================================================================================!
ELEMENTAL PURE FUNCTION NUM2BYTES2(INUM) RESULT (BYT2)
INTEGER,INTENT(IN) :: INUM
CHARACTER(LEN=2)   :: BYT2
INTEGER            :: ITMP1, ITMP2
   ITMP2 = INUM / 256
   ITMP1 = INUM -(ITMP2 * 256)
   BYT2(1:1) = CHAR(ITMP1)
   BYT2(2:2) = CHAR(ITMP2)
END FUNCTION NUM2BYTES2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    print_p3(3f) - [M_pixel:PRINT] print pixel array as a ppm p3 file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine print_p3(filename)
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!   This driver makes an ASCII P3 portable pixmap file. Any existing
!!   file is replaced.
!!
!!##OPTIONS
!!   FILENAME  name of output file to create or replace
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_print_p3
!!      use M_pixel, only : prefsize,vinit,ortho2,vexit
!!      use M_pixel, only : linewidth,circle,color
!!      use M_pixel, only : print_p3
!!      implicit none
!!         call prefsize(40,40)
!!         call vinit()
!!         call ortho2(-100.0,100.0,-100.0,100.0)
!!         call linewidth(400)
!!         call circle(0.0,0.0,45.0)
!!         call color(3)
!!         call circle(0.0,0.0,25.0)
!!         call print_p3('demo_print.p3')
!!         call vexit()
!!      end program demo_print_p3
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE PRINT_P3(FILENAME)

! ident_38="@(#)M_pixel::print_p3(3f): print pixel array as a P3 PPM file"

CHARACTER(LEN=*),INTENT(IN) :: FILENAME

   INTEGER             :: IU,IOS,I,XS,YS,CS
   CHARACTER(LEN=4096) :: MESSAGE

   OPEN(FILE=TRIM(FILENAME),NEWUNIT=IU,IOSTAT=IOS,IOMSG=MESSAGE,ACTION='write')
   IF(IOS.EQ.0)THEN
      CALL IF_INIT()
      XS=SIZE(P_PIXEL,DIM=1)
      YS=SIZE(P_PIXEL,DIM=2)
      CS=SIZE(P_COLORMAP,DIM=2)
      WRITE(IU,'("P3",/,i0,1x,i0,/,i0,/,(20(i0,1x)))') XS,YS,CS,((P_COLORMAP(1:3,P_PIXEL(I,J)),I=0,XS-1),J=0,YS-1)
   ELSE
      WRITE(*,*)'*P_print_p3* ERROR: ',TRIM(MESSAGE)
   ENDIF

   FLUSH(UNIT=IU,IOSTAT=IOS)
   CLOSE(UNIT=IU,IOSTAT=IOS)

END SUBROUTINE PRINT_P3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!   print_ascii(3f) - [M_pixel:PRINT] print small pixel array as ASCII text
!!   (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine print_ascii(filename)
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!   This driver prints the pixmap as a simple ASCII array. It assumes
!!   only single-digit colors are used. It is appropriate for inspecting
!!   small pixmaps.
!!
!!##OPTIONS
!!   FILENAME  name of output file. If blank write to stdout.
!!
!!##EXAMPLE
!!
!!
!!   Sample Program:
!!
!!    program demo_print_ascii
!!    use M_pixel
!!    implicit none
!!    call prefsize(80,24)
!!       call vinit()
!!       call ortho2(0.0,80.0,0.0,24.0)
!!       call linewidth(400)
!!       call color(1)
!!       call circle(12.0,12.0,6.0)
!!       call color(2)
!!       call circle(72.0,12.0,6.0)
!!       call print_ascii()
!!       call vexit()
!!    end program demo_print_ascii
!!
!!   Results:
!!
!!    00000000000000000000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000000000000000000
!!    00000000000111000000000000000000000000000000000000000000000000000000000000000000
!!    00000000111111110000000000000000000000000000000000000000000000000000022222000000
!!    00000001111111111000000000000000000000000000000000000000000000000002222222220000
!!    00000001111001111100000000000000000000000000000000000000000000000022222222222000
!!    00000011100000011110000000000000000000000000000000000000000000000222200000222200
!!    00000111100000001111000000000000000000000000000000000000000000000222000000022200
!!    00000111000000000111000000000000000000000000000000000000000000002220000000002220
!!    00000111000000000111000000000000000000000000000000000000000000002220000000002220
!!    00000111000000000111000000000000000000000000000000000000000000002220000000002220
!!    00000111000000000111000000000000000000000000000000000000000000002220000000002220
!!    00000111100000001110000000000000000000000000000000000000000000002222000000022220
!!    00000011110000011110000000000000000000000000000000000000000000000222000000022200
!!    00000001111111111100000000000000000000000000000000000000000000000020220002202000
!!    00000000111111111000000000000000000000000000000000000000000000000002222222220000
!!    00000000011111100000000000000000000000000000000000000000000000000000222222200000
!!    00000000000000000000000000000000000000000000000000000000000000000000002220000000
!!    00000000000000000000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000000000000000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE PRINT_ASCII(FILENAME)
USE,INTRINSIC :: ISO_FORTRAN_ENV, ONLY : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT

! ident_39="@(#)M_pixel::print_ascii(3f): print pixel array as an ASCII block of text"

CHARACTER(LEN=*),INTENT(IN),OPTIONAL  :: FILENAME
CHARACTER(LEN=1024)                   :: MESSAGE
   INTEGER                            :: IU,IOS,I

   IF(PRESENT(FILENAME))THEN  ! if filename is present and not blank open specified filename else use stdout
      IF(FILENAME.EQ.'')THEN
         IU=OUTPUT_UNIT
         IOS=0
      ELSE
         OPEN(FILE=TRIM(FILENAME),NEWUNIT=IU,IOSTAT=IOS,IOMSG=MESSAGE,ACTION='write')
         IF(IOS.NE.0)THEN
            WRITE(ERROR_UNIT,'(*(a))',IOSTAT=IOS)'*P_print_ascii* OPEN ERROR:',TRIM(MESSAGE)
         ENDIF
      ENDIF
   ELSE
      IU=OUTPUT_UNIT
      IOS=0
   ENDIF

   IF(IOS.EQ.0)THEN
      CALL IF_INIT()
      DO I=0,SIZE(P_PIXEL,DIM=2)-1
         WRITE(IU,'(*(i1))',IOSTAT=IOS,IOMSG=MESSAGE)P_PIXEL(:,I)
         IF(IOS.NE.0)THEN
            WRITE(ERROR_UNIT,'(*(a))',IOSTAT=IOS)'*P_print_ascii* WRITE ERROR:',TRIM(MESSAGE)
            EXIT
         ENDIF
      ENDDO
   ENDIF

   FLUSH(UNIT=IU,IOSTAT=IOS)
   IF(IU.NE.OUTPUT_UNIT)THEN
      CLOSE(UNIT=IU,IOSTAT=IOS)
   ENDIF

END SUBROUTINE PRINT_ASCII
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      ppm - portable pixmap file format
!!
!!##DESCRIPTION
!!      The portable pixmap format is a lowest common denominator
!!      color image file format. The definition is as follows:
!!
!!      - A "magic number" for identifying the file type. A ppm
!!        file's magic number is the two characters "P3".
!!
!!      - Whitespace (blanks, TABs, CRs, LFs).
!!
!!      - A width, formatted as ASCII characters in decimal.
!!
!!      - Whitespace.
!!
!!      - A height, again in ASCII decimal.
!!
!!      - Whitespace.
!!
!!      - The maximum color-component value, again in ASCII decimal.
!!
!!      - Whitespace.
!!
!!      - Width * height pixels, each three ASCII decimal values
!!        between 0 and the specified maximum value, starting at the
!!        top-left corner of the pixmap,  proceeding in normal
!!        English reading order. The three values for each pixel
!!        represent red, green, and blue, respectively; a value of 0
!!        means that color is off, and the maximum value means that
!!        color is maxxed out.
!!
!!      - Characters from a "#" to the next end-of-line are ignored
!!        (comments).
!!
!!      - No line should be longer than 70 characters.
!!
!!      Here is an example of a small pixmap in this format:
!!      P3
!!      # feep.ppm
!!      4 4
!!      15
!!       0  0  0    0  0  0    0  0  0   15  0 15
!!       0  0  0    0 15  7    0  0  0    0  0  0
!!       0  0  0    0  0  0    0 15  7    0  0  0
!!      15  0 15    0  0  0    0  0  0    0  0  0
!!
!!      Programs that read this format should be as lenient as possible,
!!      accepting anything that looks remotely like a pixmap.
!!
!!      There is also a variant on the format, available by setting
!!      the RAWBITS option at compile time. This variant is
!!      different in the following ways:
!!
!!      - The "magic number" is "P6" instead of "P3".
!!
!!      - The pixel values are stored as plain bytes,  instead of
!!        ASCII decimal.
!!
!!      - Whitespace is not allowed in the pixels area, and only a
!!        single character of whitespace (typically a newline) is
!!        allowed after the maxval.
!!
!!      - The files are smaller and many times faster to read and
!!        write.
!!
!!      Note that this raw format can only be used for maxvals less
!!      than or equal to 255. If you use the ppm library and try to
!!      write a file with a larger maxval,  it will automatically
!!      fall back on the slower but more general plain format.
!!
!!##AUTHOR
!!      Copyright (C) 1989, 1991 by Jef Poskanzer.
!!
!!                  Last change: 27 September 1991
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    textsize(3f) - [M_pixel:TEXT] set text size in world units
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine textsize(width, height)
!!    real,intent(in) :: width
!!    real,intent(in) :: height
!!
!!##DESCRIPTION
!!    Set the maximum size of a character in the current font. Width
!!    and height are values in world units. This only applies to software
!!    text. This must be done after the font being scaled is loaded. To keep
!!    text of different sizes aligned along the same baseline note that you
!!    typically need to subtrace the descender height from the Y position.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textsize
!!    use M_pixel
!!    use M_writegif, only : writegif
!!    implicit none
!!    integer :: i,ii
!!       !! set up long bar as plotting area
!!       call prefsize(900,150)
!!       call vinit()
!!       call ortho2(-30.0, 30.0, -5.0, 5.0)
!!       call font('DUPLEX')
!!       call move2(-23.0,-4.5)
!!       call color(7)
!!       call textsize(2.0,2.0)
!!       call move2(-27.5,-3.0)
!!       call draw2( 27.5,-3.0)
!!       call move2(-27.5,-3.0)
!!       do i=1,7
!!          ii=nint((i*20)*0.30)
!!          call linewidth(nint(ii*2.35))
!!          call textsize(real(i),real(i))
!!          call color(5)
!!          call drawstr('aA')
!!       enddo
!!       ! write plot as GIF file
!!       call writegif('textsize.3m_pixel.gif',P_pixel,P_colormap)
!!       call vexit()
!!       ! use system to display GIF file
!!       call execute_command_line('display textsize.3m_pixel.gif')
!!    end program demo_textsize
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE TEXTSIZE(WIDTH,HEIGHT)

! ident_40="@(#)M_pixel::textsize(3f): set text size in world units"

REAL,INTENT(IN) :: WIDTH
REAL,INTENT(IN) :: HEIGHT

   P_TEXT_HEIGHT=HEIGHT
   P_TEXT_WIDTH=WIDTH

END SUBROUTINE TEXTSIZE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    ycentertext(3f) - [M_pixel:TEXT] set text centering mode on for drawstr(3f) and drawc(3f) in Y direction
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine ycentertext()
!!
!!##DESCRIPTION
!!    Centers text in the Y direction. The text string will be draw so
!!    that its center line is aligned with the current y position. Top
!!    justification and Bottom justification are turned off.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE YCENTERTEXT()

! ident_41="@(#)M_pixel::ycentertext(3f): set text centering mode on for drawstr(3f) and drawc(3f) in Y direction"

   P_X_CENTERTEXT=.FALSE.
   P_Y_CENTERTEXT=.TRUE.

END SUBROUTINE YCENTERTEXT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    xcentertext(3f) - [M_pixel:TEXT] set text centering mode on for drawstr(3f) and drawc(3f) in X direction
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine xcentertext()
!!
!!##DESCRIPTION
!!    Set text centering mode on in X direction. Y justification is
!!    turned off.
!!
!!    Centers text in the X direction. The text string will begin at a
!!    point to the notional left of the current position and finish at a
!!    point to the right of the current position. Left justification and
!!    Right justification are turned off.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE XCENTERTEXT()

! ident_42="@(#)M_pixel::xcentertext(3f): set text centering mode for drawstr(3f) and drawc(3f) in X direction"

   P_X_CENTERTEXT=.TRUE.
   P_Y_CENTERTEXT=.FALSE.

END SUBROUTINE XCENTERTEXT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    centertext(3f) - [M_pixel:TEXT] set text centering mode for drawstr(3f) and drawc(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine centertext(onoff)
!!    logical,intent(in) :: onoff
!!
!!##DESCRIPTION
!!    Set text centering mode on or off. Only approximate in vertical
!!    direction.
!!
!!##OPTIONS
!!    ONOFF  set centering mode on or off
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_centertext
!!    use :: M_pixel
!!    use :: M_pixel, only : cosd, sind
!!    use :: M_writegif, only : writegif
!!    implicit none
!!    real    :: x1, y1, xx, yy, ang, r
!!    integer :: i, j
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit()
!!    call ortho2(-300.0,300.0,-300.0,300.0)
!!    call textsize(8.0,8.0)
!!    call linewidth(30)
!!    x1=-150
!!    y1=-150
!!    do j=1,4
!!       select case(j)
!!       case(1);  call  xcentertext();        x1=-150;  y1=-150;  r=100
!!       case(2);  call  ycentertext();        x1=+150;  y1=-150;  r= 30
!!       case(3);  call  centertext(.true.);   x1=-150;  y1=+150;  r=100
!!       case(4);  call  centertext(.false.);  x1=+150;  y1=+150;  r= 30
!!       end select
!!       !! draw radial lines
!!       call color(1)
!!       do i=1,80
!!          call move2(x1,y1)
!!          call draw2(x1+150.0*cosd(i*12), y1+150.0*sind(i*12))
!!       enddo
!!
!!       !! draw rotated text
!!       call color(2)
!!       do i=1,30
!!          ang=i*12.0
!!          xx=x1+r*cosd(ang)
!!          yy=y1+r*sind(ang)
!!          call move2(xx,yy)
!!          call textang(ang)
!!          call color(7)
!!          call drawstr('This is angled text')
!!          call color(1)
!!       enddo
!!    enddo
!!
!!    call  writegif('centertext.3m_pixel.gif',P_pixel,P_colormap)
!!    call  execute_command_line('display centertext.3m_pixel.gif')
!!
!!    call vexit()
!!
!!    end program demo_centertext
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE CENTERTEXT(ONOFF)

! ident_43="@(#)M_pixel::centertext(3f): set text centering mode for drawstr(3f) and drawc(3f)"

LOGICAL,INTENT(IN) :: ONOFF

   P_X_CENTERTEXT=ONOFF
   P_Y_CENTERTEXT=ONOFF

END SUBROUTINE CENTERTEXT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    textang(3f) - [M_pixel:TEXT] set text angle
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine textang(ang)
!!    real,intent(in) :: ang
!!
!!##DESCRIPTION
!!    Set the text angle. This angles strings and chars. This routine only
!!    affects software text.
!!
!!##OPTIONS
!!    ANG   The angle in degrees to draw text with when using drawstr(3f).
!!          Angles are measured counterclockwise with zero degrees at the horizontal
!!          line to the right of the original.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textang
!!    use :: M_pixel
!!    use :: M_pixel, only : cosd, sind
!!    use :: M_writegif, only : writegif
!!    implicit none
!!    integer :: i
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit()
!!    call ortho2(-100.0,100.0,-100.0,100.0)
!!    call textsize(7.0,7.0)
!!    call linewidth(20)
!!    do i=1,30
!!       !! draw radial lines
!!       call color(1)
!!       call move2(0.0,0.0)
!!       call draw2(100.0*cosd(i*12),100.0*sind(i*12))
!!       !! draw rotated text
!!       call color(7)
!!       call move2(30.0*cosd(i*12),30.0*sind(i*12))
!!       call textang(i*12.0)
!!       call drawstr('angled text')
!!    enddo
!!
!!    call writegif('textang.3m_pixel.gif',P_pixel,P_colormap)
!!    call execute_command_line('display textang.3m_pixel.gif')
!!
!!    call vexit()
!!
!!    end program demo_textang
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE TEXTANG(ANG)

! ident_44="@(#)M_pixel::textang(3f): set angle in degrees to draw text at using drawstr(3f)"

REAL,INTENT(IN) :: ANG

   P_TEXT_ANGLE=ANG
   P_TEXT_COSINE=COSD(P_TEXT_ANGLE)
   P_TEXT_SINE  =SIND(P_TEXT_ANGLE)

END SUBROUTINE TEXTANG
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    font(3f) - [M_pixel:TEXT] select font style by name
!!    (LICENSE:PD)
!!
!!##SYNOPSIS:
!!  definition:
!!
!!         subroutine font(fontname)
!!         character(len=*),intent(in) :: fontname
!!
!!##DESCRIPTION
!!    Set the current font. Allowed names are
!!
!!       o futura.l  SIMPLEX
!!       o futura.m  DUPLEX
!!       o times.r   COMPLEX
!!       o times.i   ITALIC
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_font
!!    use :: M_pixel
!!    use :: M_writegif, only : writegif
!!    implicit none
!!    real    :: left
!!    real    :: baseline=80.0
!!    integer :: icolor=1
!!       !! set up drawing surface
!!       call prefsize(400, 400)
!!       call vinit()
!!       call viewport(0.0, 400.0, 400.0, 0.0)
!!       call ortho2(-100.0, 100.0, -100.0, 100.0)
!!       call color(7)
!!       call clear()
!!       call textsize(10.0, 10.0)
!!       !! place a vertical line along the edge
!!       call color(1)
!!       call move2(-90.0, -90.0)
!!       call draw2(-90.0, 90.0)
!!       !! make a centered title at top a bit bolder and bigger
!!       call xcentertext()
!!       call textsize(13.0, 13.0)
!!       call linewidth(90)
!!       left=0
!!       call nextline('Font Samples')
!!       !! print the font samples
!!       left=-90
!!       call linewidth(0)
!!       call textsize(10.0, 10.0)
!!       call centertext(.false.)
!!       icolor=icolor-1
!!       call nextline('DEFAULT (ie. futura.l)')
!!       icolor=icolor-1
!!       call nextline('now call font(3f) ...')
!!       call nextline('SIMPLEX, or futura.l')
!!       call nextline('COMPLEX, or times.r')
!!       call nextline('ITALIC, or times.i')
!!       call nextline('DUPLEX, or futura.m')
!!       call writegif('font.3m_pixel.gif',P_pixel,P_colormap)
!!       !call execute_command_line('display font.3m_pixel.gif')
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    !! reduce some duplicate code; very specific to this example
!!    integer :: iend
!!       iend=index(string,',')  ! if comma, assume font name found
!!       if(iend.ne.0)call font(string(:iend-1)) ! change font
!!       icolor=icolor+1         ! set pen color
!!       call color(icolor)
!!       baseline=baseline-20    ! move down before drawing line
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!    end subroutine nextline
!!
!!    end program demo_font
SUBROUTINE FONT(FONTNAME)

! ident_45="@(#)M_pixel::font(3f): select font style by name"

CHARACTER(LEN=*),INTENT(IN) :: FONTNAME
      SELECT CASE(FONTNAME)
      CASE ('futura.l','SIMPLEX','simplex')
        P_FONT='SIMPLEX'
      CASE ('futura.m','DUPLEX','duplex')
        P_FONT='DUPLEX'
      CASE ('times.r' ,'COMPLEX','complex')
        P_FONT='COMPLEX'
      CASE ('times.i' ,'ITALIC','italic')
        P_FONT='ITALIC'
      CASE DEFAULT
        P_FONT='SIMPLEX'
      END SELECT
END SUBROUTINE FONT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    drawchar(3f) - [M_pixel:TEXT]  Draw a character at the current position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine drawchar(ch)
!!    character(len=1),intent(in) :: ch
!!
!!##DESCRIPTION
!!    Draw a character at the current position. Uses current line color
!!    and thickness and text justification mode.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_drawchar
!!    use M_pixel
!!    use M_writegif_animated, only : write_animated_gif
!!    implicit none
!!    integer,parameter :: isize=600
!!    integer           :: movie(32:124,0:isize-1,0:isize-1)
!!    integer           :: i
!!    !! set up environment
!!    call prefsize(isize,isize)
!!    call vinit()
!!    call ortho2(-100.0,100.0,-100.0,100.0)
!!    call textsize(150.0,150.0)
!!    call centertext(.true.)
!!
!!    do i=33,124
!!       !! draw reference circle and crosshairs
!!       call linewidth(100)
!!       call color(0)
!!       call clear()
!!       call color(4)
!!       call circle(0.0,0.0,75.0)
!!       call move2(-75.0,0.0)
!!       call draw2(75.0,0.0)
!!       call move2(0.0,-75.0)
!!       call draw2(0.0,75.0)
!!       call color(7)
!!       call linewidth(200)
!!       call textang(3.0*i)
!!       call move2(0.0,0.0)
!!       call drawchar(char(i))
!!       movie(i,:,:)=P_pixel
!!    enddo
!!    call vexit()
!!    !! write to file and display with display(1)
!!    call write_animated_gif('drawchar.3m_pixel.gif',movie,P_colormap)
!!    call execute_command_line('display drawchar.3m_pixel.gif')
!!    end program demo_drawchar
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE DRAWCHAR(CH)

! ident_46="@(#)M_pixel::drawchar(3f): draw text at the current position"

CHARACTER(LEN=1),INTENT(IN) :: CH

   CALL DRAWSTR(CH)

END SUBROUTINE DRAWCHAR
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    drawstr(3f) - [M_pixel:TEXT]  Draw the text string at the current position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine drawstr(string)
!!    character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!    Draw a text string at the current position. Uses current line color
!!    and thickness and text centering mode.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!       program demo_drawstr
!!       use M_pixel
!!       use :: M_writegif, only : writegif
!!       implicit none
!!       call prefsize(400,400)
!!       call vinit()
!!       call ortho2(-1.0,1.0,-1.0,1.0)
!!       ! by default the drawing surface is
!!       ! a square ranging from -1 to 1 in both
!!       ! the X and Y axis
!!       write(*,*)D_BLACK, D_GREEN, D_RED
!!
!!       call color(D_BLACK)    ! set current color to black
!!       call clear()           ! clear to current color
!!
!!       ! SET COMMON TEXT ATTRIBUTES
!!       call color(D_GREEN)    ! we want to draw in green
!!       call circle(0.0,0.0,1.0)
!!       call font('futura.m')  ! set font
!!       call textsize(0.1,0.1) ! font size
!!
!!       ! DRAW A STRING
!!       call move2(-1.0, 0.0)
!!       call drawstr('Hello')  ! draw string at current position
!!       ! note that current position is now at end of this string
!!
!!       ! CHANGE SOME TEXT ATTRIBUTES AGAIN
!!       call linewidth(20)     ! set line width
!!       call color(D_RED)      ! change color
!!       call textang(45.0)     ! change text angle
!!
!!       call drawstr(' World!')! draw string at current position
!!       !! render pixel array to a file
!!       call writegif('drawstr.3m_pixel.gif',P_pixel,P_colormap)
!!       !! display graphic assuming display(1) is available
!!       call execute_command_line('display drawstr.3m_pixel.gif')
!!
!!       call vexit()           !  wrap up and exit graphics mode
!!
!!       end program demo_drawstr
!!   Results:
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE DRAWSTR_(STRING)
!-!use :: M_pixel, only : cosd, sind

! ident_47="@(#)M_pixel::drawstr(3f): draw text at the current position"

CHARACTER(LEN=*),INTENT(IN)  :: STRING
CHARACTER(LEN=:),ALLOCATABLE :: FONTSTRING
   REAL                     :: S(4)
   REAL                     :: XT, YT
   REAL                     :: XX, YY, LL
   !
   !   gives 4 distances in world coordinates, all from the left end of the string -
   !
   !   o S(1)  to the left edge of the 1st nonblank character
   !   o S(2)  to the center of the string, blanks removed from the ends
   !   o S(3)  to the right edge of the last nonblank character
   !   o S(4)  to the right edge of the last character of the string.
   !  XCENTER            * \
   !                   *     \
   !                 *         *
   !               *         *   \
   !             *\        *       \
   !           *    \    *          *
   !         *        \*          *
   !       *         *  \       *        X2=X1-S(2)*COSD(P_TEXT_ANGLE)
   !       \       *      \   *          Y2=Y1-S(2)*SIND(P_TEXT_ANGLE)
   !         \   *          * X1,Y1
   !           *          *
   !            \       *
   !              \   *    P_TEXT_ANGLE
   !                *  X2,Y2==================
   !
   !
   !  YCENTER            * \
   !                   *     \
   !                 *         *
   !               *         *   \
   !             *\        *       \
   !           *    \    *          *
   !         *        \* X1,Y1    *
   !       *         *  \       *        X2=X1+P_TEXT_HEIGHT/2.0*COSD(P_TEXT_ANGLE+90)
   !       \       *      \   *          Y2=Y1-P_TEXT_HEIGHT/2.0*SIND(P_TEXT_ANGLE+90)
   !         \   *          * X2,Y2      X3=
   !           *          *
   !            \       *
   !              \   *    P_TEXT_ANGLE
   !                *  =======================
   !
   !  CENTER             * \
   !                   *     \
   !                 *         *
   !               *         *   \
   !             *\        *       \
   !           *    \    *          *
   !         *        \* X1,Y1    *
   !       *         *  \       *        X2=X1+S(2)*COSD(P_TEXT_ANGLE+90)
   !       \       *      \   *          Y2=Y1-S(2)*SIND(P_TEXT_ANGLE+90)
   !         \   *          *X2,Y2
   !           *          *
   !            \       *
   !              \   *    P_TEXT_ANGLE
   !                * X3,Y3 ==================
   !
   !

   XT=P_X
   YT=P_Y
   FONTSTRING='\'//TRIM(P_FONT)//'\'//TRIM(STRING)

   IF (P_X_CENTERTEXT.OR.P_Y_CENTERTEXT)THEN
      CALL JUSTFY(S, P_TEXT_HEIGHT, TRIM(STRING), LEN_TRIM(FONTSTRING))

      IF (P_Y_CENTERTEXT)THEN
         XT=XT-P_TEXT_HEIGHT/2.0*COSD(P_TEXT_ANGLE+90)
         YT=YT-P_TEXT_HEIGHT/2.0*SIND(P_TEXT_ANGLE+90)
      ENDIF
      IF (P_X_CENTERTEXT)THEN
         XT=XT-S(2)*P_TEXT_COSINE
         YT=YT-S(2)*P_TEXT_SINE
      ENDIF

   ENDIF
   CALL HERSHEY(XT,YT,P_TEXT_HEIGHT,FONTSTRING,P_TEXT_ANGLE,LEN_TRIM(FONTSTRING))
   ! hershey(3f) appears to leave off at last vector written in the last character, so a
   ! series of calls creates a very strange string. This makes more sense.
   LL=STRLENGTH(TRIM(STRING))
   XX=XT+COSD(P_TEXT_ANGLE)*LL
   YY=YT+SIND(P_TEXT_ANGLE)*LL
   CALL MOVE2(XX,YY)

END SUBROUTINE DRAWSTR_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getgp2(3f) - [M_pixel] Gets the current graphics position in world coords.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine getgp2(x, y)
!!    real,intent(out) :: x,y
!!
!!##DESCRIPTION
!!    Gets the current graphics position in world coords.
!!
!!##RETURNS
!!    X  X coordinate of current position
!!    Y  Y coordinate of current position
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!      program demo_getgp2
!!      use M_pixel
!!      implicit none
!!      real :: X,Y
!!      call prefsize(20,20)
!!      call vinit()
!!      call ortho2(-100.0,100.0,-100.0,100.0)
!!      call move2(0.0,0.0)
!!      call draw2(96.5,98.333)
!!
!!      call getgp2(X,Y)
!!      write(*,*)'CURRENT POSITION (X,Y)=',X,Y
!!
!!      call vexit()
!!      end program demo_getgp2
!!
!!   Results
!!
!!    CURRENT POSITION (X,Y)=   96.5000000       98.3330002
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE GETGP2(X, Y)

! ident_48="@(#)M_pixel::getgp2(3f): get current graphics position"

REAL,INTENT(OUT) :: X, Y

   X=P_X
   Y=P_Y

END SUBROUTINE GETGP2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getdisplaysize(3f) - [M_pixel] Returns the width and height of the device in pixels
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine getdisplaysize(w, h)
!!    real,intent(in) :: w, h
!!
!!##DESCRIPTION
!!    Returns the width and height of the device in pixels in w and h
!!    respectively.
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
SUBROUTINE GETDISPLAYSIZE(W, H)

! ident_49="@(#)M_pixel::getdisplaysize(3f): Returns the width and height of the device in pixels"

REAL,INTENT(OUT) :: W, H

   W=P_VIEWPORT_WIDTH
   H=P_VIEWPORT_HEIGHT

END SUBROUTINE GETDISPLAYSIZE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    point2(3f) - [M_pixel:DRAW] Draw a point at x, y
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine point2(x, y)
!!    real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw a point at x, y. Points are drawn with the current color as
!!    a circle with a diameter equal to the current linewidth.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_point2
!!    use :: M_pixel
!!    use :: M_writegif, only : writegif
!!    implicit none
!!    integer :: i
!!    call vinit()
!!    call color(5)
!!    do i=1,20
!!       call linewidth(50*i)
!!       call point2(real(i*25),real(i*25))
!!    enddo
!!    call writegif('point2.3m_pixel.gif',P_pixel,P_colormap)
!!    call vexit()
!!    end program demo_point2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE POINT2(X, Y)

! ident_50="@(#)M_pixel::point2(3f): Draw a point at x, y"

REAL,INTENT(IN) :: X, Y

   CALL LINE(X,Y,X,Y)

END SUBROUTINE POINT2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    state(3f) - [M_pixel] print graphics state of M_pixel graphics module
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    recursive subroutine state(string)
!!    character(len=*),intent(in),optional :: string
!!
!!##DESCRIPTION
!!    Print the state of the M_pixel graphics module. This is primarily
!!    used in debugging during program development and is not currently in
!!    the M_draw library.
!!
!!##OPTIONS
!!    STRING  can have the following values
!!            o all
!!            o default
!!            o colormap
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_state
!!    use M_pixel
!!    implicit none
!!       call prefsize(640,400)
!!       call vinit()
!!       call state()
!!       call vexit()
!!    end program demo_state
!!
!!   Results:
!!
!!    VINIT CALLED:        T
!!    PREFSIZE: WIDTH=         640  HEIGHT=         400
!!    CURRENT POSITION: X=   0.00000000      Y=   0.00000000
!!    LINE WIDTH:                    1
!!    FONT:               SIMPLEX
!!    COLOR NUMBER:                  1
!!    CIRCLE PRECISION:             60
!!    TEXT:               HEIGHT=   10.0000000     WIDTH=   7.00000000     ANGLE=   0.00000000
!!    TEXT JUSTIFICATION: X_CENTER= F Y_CENTER= F
!!    VIEWPORT:           LEFT=   0.00000000     RIGHT=   639.000000     BOTTOM=   399.000000     TOP=   0.00000000
!!    WINDOW:             LEFT=   0.00000000     RIGHT=   640.000000     BOTTOM=   0.00000000     TOP=   400.000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
RECURSIVE SUBROUTINE STATE(STRING)

! ident_51="@(#)M_pixel::state(3f): print graphics state of M_pixel graphics module"

CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: STRING
CHARACTER(LEN=40)         :: STRING_LOCAL
INTEGER :: I

IF(PRESENT(STRING))THEN
   STRING_LOCAL=STRING
ELSE
   STRING_LOCAL='all'
ENDIF

!-----------------------------------------------------------------------------------------------------------------------------------
SELECT CASE(TRIM(STRING))
!-----------------------------------------------------------------------------------------------------------------------------------
CASE ('all')
   CALL STATE('colormap')
   CALL STATE('default')
!-----------------------------------------------------------------------------------------------------------------------------------
CASE ('colormap','color')
WRITE(*,*)'COLOR MAP:          ',NEW_LINE('n'),(I,P_COLORMAP(:,I),NEW_LINE('n'),I=0,255)
!-----------------------------------------------------------------------------------------------------------------------------------
CASE DEFAULT
WRITE(*,*)'VINIT CALLED:       ',P_VINIT_CALLED
WRITE(*,*)'PREFSIZE: WIDTH=',P_VIEWPORT_WIDTH,' HEIGHT=',P_VIEWPORT_HEIGHT
WRITE(*,*)'CURRENT POSITION: X=',P_X,' Y=',P_Y
WRITE(*,*)'LINE WIDTH:         ',P_WIDTH
WRITE(*,*)'FONT:               ',P_FONT
WRITE(*,*)'COLOR NUMBER:       ',P_COLOR_INDEX
WRITE(*,*)'CIRCLE PRECISION:   ',P_NSEGS
WRITE(*,*)'TEXT:               ','HEIGHT=',P_TEXT_HEIGHT,'WIDTH=',P_TEXT_WIDTH,'ANGLE=',P_TEXT_ANGLE
WRITE(*,*)'TEXT JUSTIFICATION: ','X_CENTER=',P_X_CENTERTEXT,'Y_CENTER=',P_Y_CENTERTEXT
WRITE(*,*)'VIEWPORT:           ','LEFT=',P_VIEWPORT_LEFT,'RIGHT=',P_VIEWPORT_RIGHT,'BOTTOM=',P_VIEWPORT_BOTTOM,'TOP=',P_VIEWPORT_TOP
WRITE(*,*)'WINDOW:             ','LEFT=',P_WINDOW_LEFT,'RIGHT=',P_WINDOW_RIGHT,'BOTTOM=',P_WINDOW_BOTTOM,'TOP=',P_WINDOW_TOP
!-----------------------------------------------------------------------------------------------------------------------------------
END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
END SUBROUTINE STATE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    poly2(3f) - [M_pixel:POLYGONS] construct a polygon from an array of points
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine poly2(n, points)
!!    integer,intent(in) :: n
!!    real,intent(in)    :: points(2, n)
!!
!!##DESCRIPTION
!!    Construct a polygon from an array of points
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_poly2
!!    use M_pixel
!!    use M_writegif, only : writegif
!!    implicit none
!!    integer :: i, j, icolor
!!    real    :: xx, yy
!!       call prefsize(512,512)
!!       call vinit()
!!       call ortho2(0.0,256.0,0.0,256.0)
!!       call linewidth(1)
!!       ! step thru a series of rectangular cells
!!       icolor=0
!!       xx=0.0
!!       do i=1,16
!!          yy=0.0
!!          do j=1,16
!!             yy=yy+16.0
!!             icolor=icolor+1
!!             call setcolor(icolor,xx,yy)
!!          enddo
!!          xx=xx+16.0
!!       enddo
!!       call writegif('poly2.3m_pixel.gif',P_pixel,P_colormap)
!!       call vexit()
!!    contains
!!
!!    subroutine setcolor(iset,xx,yy)
!!    use M_pixel,  only : i2s
!!    use M_pixel,  only : color_name2rgb
!!    integer,intent(in) :: iset
!!    real,intent(in)    :: xx,yy
!!    character(len=80)  :: echoname
!!    real               :: points(2,100)
!!    real               :: red, green, blue
!!       if(iset.gt.255)return
!!       ! determine coordinates of next square
!!       points(1:2,1)=[xx,      yy      ]
!!       points(1:2,2)=[xx,      yy+16.0 ]
!!       points(1:2,3)=[xx+16.0, yy+16.0 ]
!!       points(1:2,4)=[xx+16.0, yy      ]
!!       points(1:2,5)=[xx,      yy      ]
!!       ! get some nice RGB values to try from named colors known by M_pixel module
!!       call color_name2rgb(i2s(icolor),red,green,blue,echoname)
!!       if(echoname.eq.'Unknown') return
!!       ! set a color number to the new RGB values
!!       write(*,*)icolor, nint(red*2.55), nint(green*2.55), nint(blue*2.55),trim(echoname)
!!       call mapcolor(icolor, nint(red*2.55), nint(green*2.55), nint(blue*2.55))
!!       ! set to the new color
!!       call color(icolor)
!!       ! fill the rectangle in that color
!!       call poly2(5,points)
!!    end subroutine setcolor
!!
!!    end program demo_poly2
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
SUBROUTINE POLY2(N,POINTS)

! ident_52="@(#)M_pixel::poly2(3f): construct a polygon from an array of points"

INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: POINTS(2, N)
   REAL            :: XX, YY
   INTEGER         :: IX(N), IY(N)
   INTEGER         :: I

   DO I=1,N                                                   ! convert array from world coordinates to pixel COORDINATES
      CALL WORLD2VIEWPORT(POINTS(1,I), POINTS(2,I), XX, YY)
      IX(I)=NINT(XX)
      IY(I)=NINT(YY)
   ENDDO

   CALL PPM_SOLID_FILL(IX, IY, N)

END SUBROUTINE POLY2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE VFLUSH()

! ident_53="@(#)M_pixel::vflush(3f): flush current page"

END SUBROUTINE VFLUSH
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE PPM_DRAW_FILL_LINE(XSTART,YSTART,X,Y)

! ident_54="@(#)M_pixel::PPM_DRAW_FILL_LINE(3fp): draws a line across a graphics array"

INTEGER,INTENT(IN) :: XSTART,YSTART
INTEGER,INTENT(IN) :: X,Y
   INTEGER         :: IX,IY
   INTEGER         :: RUNCOUNT
   INTEGER         :: DX,DY
   INTEGER         :: XINC,YINC
   INTEGER         :: XPLOT,YPLOT
   INTEGER         :: MOSTX, MOSTY

   MOSTX=SIZE(P_PIXEL,DIM=1)-1
   MOSTY=SIZE(P_PIXEL,DIM=2)-1
   IF(X.LE.MOSTX.AND.Y.LE.MOSTY.AND.X.GT.0.AND.Y.GT.0) P_PIXEL(XSTART,YSTART)=P_COLOR_INDEX ! move to initial SPOT
   IX=XSTART
   IY=YSTART

   RUNCOUNT=0

   DX = ABS(IX-X)

   XINC=0
   IF (X > IX)  XINC=  1
   IF (X == IX) XINC=  0
   IF (X < IX)  XINC= -1

   DY = ABS(IY-Y)

   YINC=0
   IF (Y > IY)  YINC=  1
   IF (Y == IY) YINC=  0
   IF (Y < IY)  YINC= -1

   XPLOT = IX
   YPLOT = IY

   IF (DX>DY) THEN
      ! iterate x
      DO WHILE (XPLOT /= X)
         XPLOT = XPLOT + XINC
         RUNCOUNT = RUNCOUNT + DY
         IF (RUNCOUNT >= (DX-RUNCOUNT)) THEN
            YPLOT = YPLOT + YINC
            RUNCOUNT = RUNCOUNT - DX
         ENDIF
         IF(XPLOT.LE.MOSTX.AND.YPLOT.LE.MOSTY.AND.XPLOT.GT.0.AND.YPLOT.GT.0) P_PIXEL(XPLOT,YPLOT)=P_COLOR_INDEX
      ENDDO
   ELSE
      ! iterate y
      DO WHILE (YPLOT /= Y)
         YPLOT = YPLOT + YINC
         RUNCOUNT = RUNCOUNT + DX
         IF (RUNCOUNT >= (DY-RUNCOUNT)) THEN
            XPLOT = XPLOT + XINC
            RUNCOUNT = RUNCOUNT - DY
         ENDIF
         IF(XPLOT.LE.MOSTX.AND.YPLOT.LE.MOSTY.AND.XPLOT.GT.0.AND.YPLOT.GT.0) P_PIXEL(XPLOT,YPLOT)=P_COLOR_INDEX
      ENDDO
   ENDIF

END SUBROUTINE PPM_DRAW_FILL_LINE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE PPM_DRAW_THICK_LINE(INX1,INY1,INX2, INY2)

! ident_55="@(#)M_pixel::PPM_DRAW_THICK_LINE(3fp): draw line from current pixel graphics position to (x, y) using polygons for line thickness"

INTEGER,INTENT(IN) :: INX1,INY1,INX2,INY2
   INTEGER         :: COSINE, SINE
   REAL            :: ANGLE
   !
   !               *  P2
   !             *   \
   !           *       \
   !         *         * inx2,iny2
   !   P1  *         *    \
   !       \       *        \ P3
   !         \   *          *
   ! inx1,iny1 *          *
   !            \       *
   !              \   *
   !                * P4
   !

   ! thick lines are made from filled polygon(s). Add a circle to ends of really thick lines
   CALL PPM_ENDCAP_CIRCLE(INX1,INY1)
   CALL PPM_ENDCAP_CIRCLE(INX2,INY2)

   ANGLE=ATAN2(REAL(INY2-INY1),REAL(INX2-INX1)) + PI/2.0
   COSINE=NINT((P_WIDTH/2.0)*COS(ANGLE))
   SINE=NINT((P_WIDTH/2.0)*SIN(ANGLE))

   CALL PPM_SOLID_FILL( [INX1+COSINE, INX2+COSINE, INX2-COSINE, INX1-COSINE], [INY1+SINE, INY2+SINE, INY2-SINE, INY1-SINE], 4)

END SUBROUTINE PPM_DRAW_THICK_LINE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
FUNCTION PPM_YINTERCEPT(YSCAN, X1, Y1, X2, Y2, XINTERCEPT, YPREV)
LOGICAL :: PPM_YINTERCEPT
INTEGER :: YSCAN
INTEGER :: X1
INTEGER :: Y1
INTEGER :: X2
INTEGER :: Y2
INTEGER :: XINTERCEPT
INTEGER :: YPREV
! Determine if scan line intercepts the line segment. If it does, return the x intercept.
   INTEGER :: DELTAY, YPREVIOUS
   REAL    :: T
   YPREVIOUS = YPREV   ! the value we need to use in this pass
   YPREV = Y1          ! store the value for the next call to (probably) use
   DELTAY = Y2 - Y1
   IF ( DELTAY == 0 )THEN
      ! horizontal lines do not contribute to scan line intercepts
      YPREV=YPREVIOUS
      PPM_YINTERCEPT=.FALSE.
      RETURN
   ENDIF
   T = REAL(YSCAN - Y1) / DELTAY
   IF (T > 0.0 .AND. T <= 1.0) THEN
      ! scan line and line segment intersect but not at leading vertex
      XINTERCEPT = X1 + NINT(T*(X2 - X1))
      PPM_YINTERCEPT=.TRUE.
      RETURN
   ELSEIF ( T == 0.0 )THEN
      ! scan line and line segment intersect at leading vertex
      XINTERCEPT = X1 + NINT(T*(X2 - X1))
      IF(YPREVIOUS <= Y1 .AND. Y2 <= Y1 )THEN
         ! local maximum
         PPM_YINTERCEPT=.TRUE.
         RETURN
      ELSEIF(YPREVIOUS >= Y1 .AND. Y2 >= Y1 )THEN
         ! local minimum
         PPM_YINTERCEPT=.TRUE.
         RETURN
      ELSE
         ! ignore duplicate at vertex that is not a local maximum or minimum
         PPM_YINTERCEPT=.FALSE.
         RETURN
      ENDIF
   ENDIF
   PPM_YINTERCEPT=.FALSE.
   ! scan line and line segment did not intersect
END FUNCTION PPM_YINTERCEPT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE PPM_SOLID_FILL(X,Y,N)
!-!use M_sort, only : sort_shell

! ident_56="@(#)M_pixel::PPM_SOLID_FILL(3fp): fill polygon of n points that are in viewport coordinates"

INTEGER,INTENT(IN) :: N
INTEGER,INTENT(IN) :: X(0:N-1)
INTEGER,INTENT(IN) :: Y(0:N-1)
INTEGER,PARAMETER  :: MAX_VERTICES=9999
INTEGER            :: X1,Y1

   INTEGER :: I, J, YHORIZONTAL, XINT, XMIN, XMAX, YMAX, YMIN, XI(0:MAX_VERTICES), YPREV

   XI=-999999
   IF ( N >= MAX_VERTICES) THEN
      WRITE(*,*)"*PPM_SOLID_FILL* more than ",MAX_VERTICES," vertices in a polygon"
      RETURN
   ENDIF

   ! find clip range
   YMIN=MINVAL(Y)
   YMAX=MAXVAL(Y)
   XMIN=MINVAL(X)
   XMAX=MAXVAL(X)

   ! ensure scan lines are generated that do not cause out-of-bound problems in the y direction
   YMIN=MAX(YMIN,0)
   YMAX=MIN(YMAX,P_VIEWPORT_HEIGHT-1)

   ! For each y value, get a list of X intersections...
   YHORIZONTAL = YMAX
   DO WHILE (YHORIZONTAL >= YMIN)
      J = 0
      YPREV = Y(N-1)
      DO I = 0,N-2
         IF (PPM_YINTERCEPT(YHORIZONTAL, X(I), Y(I), X(I+1), Y(I+1), XINT, YPREV))THEN
            XI(J) = XINT
            J=J+1
         ENDIF
      ENDDO
      ! Last one.
      IF (PPM_YINTERCEPT(YHORIZONTAL, X(N-1), Y(N-1), X(0), Y(0), XINT, YPREV))THEN
         XI(J) = XINT
         J=J+1
      ENDIF

      ! odd pairs means something went wrong in figuring out whether to count vertices or not
      IF( 2 * (J/2) /= J)THEN
         IF(P_DEBUG) THEN
            WRITE(*,*)"*PPM_SOLID_FILL* Internal error: odd number of intersection points ",J
         ENDIF
      ENDIF

      CALL SORT_SHELL_INTEGERS_HL(XI(0:J-1)) ! Sort the X intersections

      ! Draw the horizontal lines
      ! should make sure within X clipping range
      DO I = 0, J-2, 2
         X1=MAX(0,MIN(XI(I),P_VIEWPORT_WIDTH-1))
         Y1=YHORIZONTAL
         CALL PPM_DRAW_FILL_LINE(X1,Y1,MAX(0, MIN(XI(I+1), P_VIEWPORT_WIDTH-1)),  YHORIZONTAL)
      ENDDO
      YHORIZONTAL = YHORIZONTAL - 1
   ENDDO
END SUBROUTINE PPM_SOLID_FILL
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE PPM_ENDCAP_CIRCLE(X, Y)

! ident_57="@(#)M_pixel::PPM_ENDCAP_CIRCLE(3fp): Draw a circle on thick line segment end point"

INTEGER,INTENT(IN) :: X
INTEGER,INTENT(IN) :: Y

   INTEGER,PARAMETER :: NSEGS=15                       ! circle precision
   REAL              :: ANGLE_STEP
   INTEGER           :: CXRAS(NSEGS), CYRAS(NSEGS)     ! array to place circle points on
   INTEGER           :: I

   ANGLE_STEP = 360.0 / NSEGS
   DO I=0,NSEGS-1
      CXRAS(I+1) = NINT(X+(P_WIDTH-1)/2.0*COSD(ANGLE_STEP*I))
      CYRAS(I+1) = NINT(Y+(P_WIDTH-1)/2.0*SIND(ANGLE_STEP*I))
   ENDDO
   CALL PPM_SOLID_FILL(CXRAS,CYRAS,NSEGS)

END SUBROUTINE PPM_ENDCAP_CIRCLE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    drawstr(3f) - [M_msg] converts any standard scalar type to a string and prints it
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine drawstr(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!
!!     class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!     character(len=*),intent(in),optional :: sep
!!     character(len=:),allocatable  :: sep_local
!!
!!##DESCRIPTION
!!    drawstr(3f) builds a space-separated string from up to twenty scalar values.
!!
!!##OPTIONS
!!    g[0-9a-j]   optional value to print the value of after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!                Optionally, all the generic values can be
!!                single-dimensioned arrays. Currently, mixing scalar
!!                arguments and array arguments is not supported.
!!
!!    sep         separator between values. Defaults to a space.
!!
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_msg
!!    use M_pixel, only : str
!!    implicit none
!!    character(len=:),allocatable :: pr
!!    character(len=:),allocatable :: frmt
!!    integer                      :: biggest
!!
!!       pr=str('HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!       write(*,'(a)')pr
!!       pr=str('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!       write(*,'(a)')pr
!!       pr=str('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!       write(*,'(a)')pr
!!       pr=str('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!       write(*,'(a)')pr
!!
!!       ! create a format on the fly
!!       biggest=huge(0)
!!       frmt=str('(*(i',int(log10(real(biggest))),':,1x))',sep='')
!!       write(*,*)'format=',frmt
!!
!!       ! although it will often work, using str(3f) in an I/O statement is not recommended
!!       ! because if an error occurs str(3f) will try to write while part of an I/O statement
!!       ! which not all compilers can handle and is currently non-standard
!!       write(*,*)str('program will now stop')
!!
!!    end program demo_msg
!!
!!  Output
!!
!!    HUGE(3f) integers 2147483647 and real 3.40282347E+38 and double 1.7976931348623157E+308
!!    real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!    doubleprecision : 1.7976931348623157E+308 0.0000000000000000 12345.678900000001 2.2250738585072014E-308
!!    complex         : (3.40282347E+38,1.17549435E-38)
!!     format=(*(i9:,1x))
!!     program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE MSG_SCALAR(GENERIC0, GENERIC1, GENERIC2, GENERIC3, GENERIC4, GENERIC5, GENERIC6, GENERIC7, GENERIC8, GENERIC9, &
                  & GENERICA, GENERICB, GENERICC, GENERICD, GENERICE, GENERICF, GENERICG, GENERICH, GENERICI, GENERICJ, &
                  & SEP)
IMPLICIT NONE

! ident_2="@(#)M_msg::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

CLASS(*),INTENT(IN),OPTIONAL  :: GENERIC0, GENERIC1, GENERIC2, GENERIC3, GENERIC4
CLASS(*),INTENT(IN),OPTIONAL  :: GENERIC5, GENERIC6, GENERIC7, GENERIC8, GENERIC9
CLASS(*),INTENT(IN),OPTIONAL  :: GENERICA, GENERICB, GENERICC, GENERICD, GENERICE
CLASS(*),INTENT(IN),OPTIONAL  :: GENERICF, GENERICG, GENERICH, GENERICI, GENERICJ
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: SEP
CHARACTER(LEN=:),ALLOCATABLE  :: SEP_LOCAL
CHARACTER(LEN=:), ALLOCATABLE :: MSG
CHARACTER(LEN=4096)           :: LINE
INTEGER                       :: ISTART
INTEGER                       :: INCREMENT
   IF(PRESENT(SEP))THEN
      INCREMENT=1+LEN(SEP)
      SEP_LOCAL=SEP
   ELSE
      SEP_LOCAL=' '
      INCREMENT=2
   ENDIF

   ISTART=1
   LINE=''
   IF(PRESENT(GENERIC0))CALL PRINT_GENERIC(GENERIC0)
   IF(PRESENT(GENERIC1))CALL PRINT_GENERIC(GENERIC1)
   IF(PRESENT(GENERIC2))CALL PRINT_GENERIC(GENERIC2)
   IF(PRESENT(GENERIC3))CALL PRINT_GENERIC(GENERIC3)
   IF(PRESENT(GENERIC4))CALL PRINT_GENERIC(GENERIC4)
   IF(PRESENT(GENERIC5))CALL PRINT_GENERIC(GENERIC5)
   IF(PRESENT(GENERIC6))CALL PRINT_GENERIC(GENERIC6)
   IF(PRESENT(GENERIC7))CALL PRINT_GENERIC(GENERIC7)
   IF(PRESENT(GENERIC8))CALL PRINT_GENERIC(GENERIC8)
   IF(PRESENT(GENERIC9))CALL PRINT_GENERIC(GENERIC9)
   IF(PRESENT(GENERICA))CALL PRINT_GENERIC(GENERICA)
   IF(PRESENT(GENERICB))CALL PRINT_GENERIC(GENERICB)
   IF(PRESENT(GENERICC))CALL PRINT_GENERIC(GENERICC)
   IF(PRESENT(GENERICD))CALL PRINT_GENERIC(GENERICD)
   IF(PRESENT(GENERICE))CALL PRINT_GENERIC(GENERICE)
   IF(PRESENT(GENERICF))CALL PRINT_GENERIC(GENERICF)
   IF(PRESENT(GENERICG))CALL PRINT_GENERIC(GENERICG)
   IF(PRESENT(GENERICH))CALL PRINT_GENERIC(GENERICH)
   IF(PRESENT(GENERICI))CALL PRINT_GENERIC(GENERICI)
   IF(PRESENT(GENERICJ))CALL PRINT_GENERIC(GENERICJ)
   MSG=TRIM(LINE)
   CALL DRAWSTR_(MSG)
CONTAINS
!===================================================================================================================================
SUBROUTINE PRINT_GENERIC(GENERIC)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
USE,INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
CLASS(*),INTENT(IN) :: GENERIC
   SELECT TYPE(GENERIC)
      TYPE IS (INTEGER(KIND=INT8));     WRITE(LINE(ISTART:),'(i0)') GENERIC
      TYPE IS (INTEGER(KIND=INT16));    WRITE(LINE(ISTART:),'(i0)') GENERIC
      TYPE IS (INTEGER(KIND=INT32));    WRITE(LINE(ISTART:),'(i0)') GENERIC
      TYPE IS (INTEGER(KIND=INT64));    WRITE(LINE(ISTART:),'(i0)') GENERIC
      TYPE IS (REAL(KIND=REAL32));      WRITE(LINE(ISTART:),'(1pg0)') GENERIC
      TYPE IS (REAL(KIND=REAL64));      WRITE(LINE(ISTART:),'(1pg0)') GENERIC
      TYPE IS (REAL(KIND=REAL128));     WRITE(LINE(ISTART:),'(1pg0)') GENERIC
      TYPE IS (LOGICAL);                WRITE(LINE(ISTART:),'(l1)') GENERIC
      TYPE IS (CHARACTER(LEN=*));       WRITE(LINE(ISTART:),'(a)') TRIM(GENERIC)
      TYPE IS (COMPLEX);                WRITE(LINE(ISTART:),'("(",1pg0,",",1pg0,")")') GENERIC
   END SELECT
   LINE=LINE(:ISTART-1)//SEP_LOCAL
   ISTART=LEN_TRIM(LINE)+INCREMENT
END SUBROUTINE PRINT_GENERIC
!===================================================================================================================================
END SUBROUTINE MSG_SCALAR
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE MSG_ONE(GENERIC0,GENERIC1, GENERIC2, GENERIC3, GENERIC4, GENERIC5, GENERIC6, GENERIC7, GENERIC8, GENERIC9,SEP)
IMPLICIT NONE

! ident_3="@(#)M_msg::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

CLASS(*),INTENT(IN)           :: GENERIC0(:)
CLASS(*),INTENT(IN),OPTIONAL  :: GENERIC1(:), GENERIC2(:), GENERIC3(:), GENERIC4(:), GENERIC5(:)
CLASS(*),INTENT(IN),OPTIONAL  :: GENERIC6(:), GENERIC7(:), GENERIC8(:), GENERIC9(:)
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: SEP
CHARACTER(LEN=:),ALLOCATABLE  :: SEP_LOCAL
CHARACTER(LEN=:), ALLOCATABLE :: MSG
CHARACTER(LEN=4096)           :: LINE
INTEGER                       :: ISTART
INTEGER                       :: INCREMENT
   IF(PRESENT(SEP))THEN
      INCREMENT=1+LEN(SEP)
      SEP_LOCAL=SEP
   ELSE
      SEP_LOCAL=' '
      INCREMENT=2
   ENDIF

   ISTART=1
   LINE=' '
   CALL PRINT_GENERIC(GENERIC0)
   IF(PRESENT(GENERIC1))CALL PRINT_GENERIC(GENERIC1)
   IF(PRESENT(GENERIC2))CALL PRINT_GENERIC(GENERIC2)
   IF(PRESENT(GENERIC3))CALL PRINT_GENERIC(GENERIC3)
   IF(PRESENT(GENERIC4))CALL PRINT_GENERIC(GENERIC4)
   IF(PRESENT(GENERIC5))CALL PRINT_GENERIC(GENERIC5)
   IF(PRESENT(GENERIC6))CALL PRINT_GENERIC(GENERIC6)
   IF(PRESENT(GENERIC7))CALL PRINT_GENERIC(GENERIC7)
   IF(PRESENT(GENERIC8))CALL PRINT_GENERIC(GENERIC8)
   IF(PRESENT(GENERIC9))CALL PRINT_GENERIC(GENERIC9)
   MSG=TRIM(LINE)
   CALL DRAWSTR_(MSG)
CONTAINS
!===================================================================================================================================
SUBROUTINE PRINT_GENERIC(GENERIC)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
USE,INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
CLASS(*),INTENT(IN),OPTIONAL :: GENERIC(:)
INTEGER :: I
   SELECT TYPE(GENERIC)
      TYPE IS (INTEGER(KIND=INT8));     WRITE(LINE(ISTART:),'("[",*(i0,1x))') GENERIC
      TYPE IS (INTEGER(KIND=INT16));    WRITE(LINE(ISTART:),'("[",*(i0,1x))') GENERIC
      TYPE IS (INTEGER(KIND=INT32));    WRITE(LINE(ISTART:),'("[",*(i0,1x))') GENERIC
      TYPE IS (INTEGER(KIND=INT64));    WRITE(LINE(ISTART:),'("[",*(i0,1x))') GENERIC
      TYPE IS (REAL(KIND=REAL32));      WRITE(LINE(ISTART:),'("[",*(1pg0,1x))') GENERIC
      TYPE IS (REAL(KIND=REAL64));      WRITE(LINE(ISTART:),'("[",*(1pg0,1x))') GENERIC
      TYPE IS (REAL(KIND=REAL128));     WRITE(LINE(ISTART:),'("[",*(1pg0,1x))') GENERIC
      !type is (real(kind=real256));     write(error_unit,'(1pg0)',advance='no') generic
      TYPE IS (LOGICAL);                WRITE(LINE(ISTART:),'("[",*(l1,1x))') GENERIC
      TYPE IS (CHARACTER(LEN=*));       WRITE(LINE(ISTART:),'("[",:*("""",a,"""",1x))') (TRIM(GENERIC(I)),I=1,SIZE(GENERIC))
      TYPE IS (COMPLEX);                WRITE(LINE(ISTART:),'("[",*("(",1pg0,",",1pg0,")",1x))') GENERIC
      CLASS DEFAULT
         STOP 'unknown type in *print_generic*'
   END SELECT
   ISTART=LEN_TRIM(LINE)+INCREMENT
   LINE=TRIM(LINE)//"]"//SEP_LOCAL
END SUBROUTINE PRINT_GENERIC
!===================================================================================================================================
END SUBROUTINE MSG_ONE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! EXTRACTED FROM OTHER MODULES
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
ELEMENTAL REAL FUNCTION COSD(ANGLE_IN_DEGREES)

! ident_58="@(#)M_pixel::cosd(3f): cos(3f) with degrees as input instead of radians"

CLASS(*),INTENT(IN) :: ANGLE_IN_DEGREES
REAL                :: ANGLE_IN_DEGREES_LOCAL
   ANGLE_IN_DEGREES_LOCAL=ANYSCALAR_TO_DOUBLE(ANGLE_IN_DEGREES)
   COSD=COS(ANGLE_IN_DEGREES_LOCAL*DEGREES_TO_RADIANS)
END FUNCTION COSD
!-----------------------------------------------------------------------------------------------------------------------------------
ELEMENTAL REAL FUNCTION SIND(ANGLE_IN_DEGREES)

! ident_59="@(#)M_pixel::sind(3f): sin(3f) with degrees as input instead of radians"

CLASS(*),INTENT(IN)  :: ANGLE_IN_DEGREES
REAL                 :: ANGLE_IN_DEGREES_LOCAL
   ANGLE_IN_DEGREES_LOCAL=ANYSCALAR_TO_DOUBLE(ANGLE_IN_DEGREES)
   SIND=SIN(ANGLE_IN_DEGREES_LOCAL*DEGREES_TO_RADIANS)
END FUNCTION SIND
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE ELEMENTAL FUNCTION ANYSCALAR_TO_REAL(VALUEIN) RESULT(R_OUT)
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : ERROR_UNIT !! ,input_unit,output_unit
IMPLICIT NONE

! ident_5="@(#)M_anything::anyscalar_to_real(3f): convert integer or real parameter of any kind to real"

CLASS(*),INTENT(IN) :: VALUEIN
REAL                :: R_OUT
REAL,PARAMETER      :: BIG=HUGE(0.0)
   SELECT TYPE(VALUEIN)
   TYPE IS (INTEGER(KIND=INT8));   R_OUT=REAL(VALUEIN)
   TYPE IS (INTEGER(KIND=INT16));  R_OUT=REAL(VALUEIN)
   TYPE IS (INTEGER(KIND=INT32));  R_OUT=REAL(VALUEIN)
   TYPE IS (INTEGER(KIND=INT64));  R_OUT=REAL(VALUEIN)
   TYPE IS (REAL(KIND=REAL32));    R_OUT=REAL(VALUEIN)
   TYPE IS (REAL(KIND=REAL64))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_real* value too large ',valuein
      !!endif
      R_OUT=REAL(VALUEIN)
   TYPE IS (REAL(KIND=REAL128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_real* value too large ',valuein
      !!endif
      R_OUT=REAL(VALUEIN)
   TYPE IS (LOGICAL);              R_OUT=MERGE(0.0D0,1.0D0,VALUEIN)
   TYPE IS (CHARACTER(LEN=*));     READ(VALUEIN,*) R_OUT
   !type is (real(kind=real128));  r_out=real(valuein)
   END SELECT
END FUNCTION ANYSCALAR_TO_REAL
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
PURE FUNCTION INVERT_4X4(A) RESULT(B)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   INTEGER,PARAMETER            :: WP=KIND(0.0)
   REAL(KIND=WP), INTENT(IN) :: A(4,4)   !! Matrix
   REAL(KIND=WP)             :: B(4,4)   !! Inverse matrix
   REAL(KIND=WP)             :: DETINV

   ! Calculate the inverse determinant of the matrix
   DETINV = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = DETINV*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = DETINV*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = DETINV*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = DETINV*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = DETINV*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = DETINV*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = DETINV*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = DETINV*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = DETINV*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = DETINV*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = DETINV*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = DETINV*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = DETINV*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = DETINV*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = DETINV*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = DETINV*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
END FUNCTION INVERT_4X4
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE JOURNAL(STRING)
CHARACTER(LEN=*),INTENT(IN) :: STRING
WRITE(*,'(g0)')TRIM(STRING)
END SUBROUTINE JOURNAL
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
FUNCTION I2S(IVALUE) RESULT(OUTSTR)

! ident_60="@(#)M_strings::i2s(3fp): private function returns string given integer value"

INTEGER,INTENT(IN)           :: IVALUE                         ! input value to convert to a string
CHARACTER(LEN=:),ALLOCATABLE :: OUTSTR                         ! output string to generate
CHARACTER(LEN=80)            :: STRING
   WRITE(STRING,'(g0)')IVALUE
   OUTSTR=TRIM(STRING)
END FUNCTION I2S
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE SORT_SHELL_INTEGERS_HL(IARRAY)
! Copyright (C) 1989,1996 John S. Urban;  all rights reserved

! ident_61="@(#)M_sort::sort_shell_integers_hl(3fp):sort integer array using Shell sort (high to low)"

INTEGER,INTENT(INOUT)      :: IARRAY(:)  ! input/output array
INTEGER                    :: N          ! number of elements in input array (iarray)
INTEGER                    :: IGAP, I, J, K, JG
   N=SIZE(IARRAY)
   IGAP=N
   INFINITE: DO
      IGAP=IGAP/2
      IF(IGAP.EQ.0) EXIT INFINITE
      K=N-IGAP
      I=1
      INNER: DO
         J=I
         INSIDE: DO
            JG=J+IGAP
            IF(IARRAY(J).GE.IARRAY(JG)) EXIT INSIDE
            CALL SWAPCOORD(IARRAY(J),IARRAY(JG))
            J=J-IGAP
            IF(J.LT.1) EXIT INSIDE
         ENDDO INSIDE
         I=I+1
         IF(I.GT.K) EXIT INNER
      ENDDO INNER
   ENDDO INFINITE
END SUBROUTINE SORT_SHELL_INTEGERS_HL
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE ELEMENTAL FUNCTION ANYSCALAR_TO_DOUBLE(VALUEIN) RESULT(D_OUT)
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : ERROR_UNIT !! ,input_unit,output_unit
IMPLICIT NONE

! ident_4="@(#)M_anything::anyscalar_to_double(3f): convert integer or real parameter of any kind to doubleprecision"

CLASS(*),INTENT(IN)       :: VALUEIN
DOUBLEPRECISION           :: D_OUT
DOUBLEPRECISION,PARAMETER :: BIG=HUGE(0.0D0)
   SELECT TYPE(VALUEIN)
   TYPE IS (INTEGER(KIND=INT8));   D_OUT=DBLE(VALUEIN)
   TYPE IS (INTEGER(KIND=INT16));  D_OUT=DBLE(VALUEIN)
   TYPE IS (INTEGER(KIND=INT32));  D_OUT=DBLE(VALUEIN)
   TYPE IS (INTEGER(KIND=INT64));  D_OUT=DBLE(VALUEIN)
   TYPE IS (REAL(KIND=REAL32));    D_OUT=DBLE(VALUEIN)
   TYPE IS (REAL(KIND=REAL64));    D_OUT=DBLE(VALUEIN)
   TYPE IS (REAL(KIND=REAL128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
      !!endif
      D_OUT=DBLE(VALUEIN)
   TYPE IS (LOGICAL);              D_OUT=MERGE(0.0D0,1.0D0,VALUEIN)
   TYPE IS (CHARACTER(LEN=*));      READ(VALUEIN,*) D_OUT
   !type is (real(kind=real128))
   !   if(valuein.gt.big)then
   !      write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
   !   endif
   !   d_out=dble(valuein)
   CLASS DEFAULT
     D_OUT=0.0D0
     !!stop '*M_anything::anyscalar_to_double: unknown type'
   END SELECT
END FUNCTION ANYSCALAR_TO_DOUBLE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

!>
!!##NAME
!!    HUE(3f) - [M_pixel:COLOR] converts a color's components from one color model to another
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine hue(modei,clr1i,clr2i,clr3i,modeo,clr1o,clr2o,clr3o,status)
!!
!!     character(len=*),intent(in) :: modei
!!     character(len=*),intent(in) :: modeo
!!     real,intent(in)             :: clr1i,clr2i,clr3i
!!     real,intent(out)            :: clr1o,clr2o,clr3o
!!     integer,intent(out)         :: status
!!
!!##DESCRIPTION
!!    Basic color models:
!!
!!     +----------------------------------------------------------+
!!     | valid values for modei and modeo as well as the          |
!!     | corresponding meanings for clr1*, clr2*, and clr3* are:  |
!!     +----------------------------------------------------------+
!!     |model| clr1         |         clr2      |         clr3    |
!!     |-----+--------------+-------------------+-----------------|
!!     |hls  |hue           |lightness          |saturation       |
!!     |-----+--------------+-------------------+-----------------|
!!     |hsl  |hue           |saturation         |lightness        |
!!     |-----+--------------+-------------------+-----------------|
!!     |hvs  |hue           |value              |saturation       |
!!     |-----+--------------+-------------------+-----------------|
!!     |hsv  |hue           |saturation         |value            |
!!     |-----+--------------+-------------------+-----------------|
!!     |rgb  |red           |green              |blue             |
!!     |-----+--------------+-------------------+-----------------|
!!     |cmy  |cyan          |magenta            |yellow           |
!!     |-----+--------------+-------------------+-----------------|
!!     |yiq  |gray scale)   |orange-blue        |purple-green     |
!!     |     |              |chrominance        |chrominance      |
!!     +----------------------------------------------------------+
!!
!!    *  lightness, value, saturation, red, green, blue, cyan, magenta, and
!!       yellow range from 0 to 100,
!!
!!       * hue ranges from 0 to 360 degrees,
!!       * y ranges from 0 to 100,
!!       * i ranges from -60 to 60,
!!       * q ranges from -52 to 52
!!
!!    The STATUS variable can signal the following conditions:
!!
!!      -1   modei = modeo, so no substantial conversion was done,
!!       1   one of the input color values was outside the allowable range,
!!       2   modei was invalid
!!       3   modeo was invalid
!!
!!##EXAMPLE
!!
!! Sample program
!!
!!     program demo_hue
!!     use M_pixel, only : hue
!!     implicit none
!!        !                      NAME       RGB(0-255)            HLS(0-100)
!!        call check_name('hls','red',      [ 100, 0,   0   ],[ 0,   50,  100 ])
!!        call check_name('hls','orange',   [ 100, 65,  0   ],[ 39,  50,  100 ])
!!        call check_name('hls','yellow',   [ 100, 100, 0   ],[ 60,  50,  100 ])
!!        call check_name('hls','green',    [ 0,   100, 0   ],[ 120, 50,  100 ])
!!        call check_name('hls','cyan',     [ 0,   100, 100 ],[ 180, 50,  100 ])
!!        call check_name('hls','blue',     [ 0,   0,   100 ],[ 240, 50,  100 ])
!!        call check_name('hls','magenta',  [ 100, 0,   100 ],[ 300, 50,  100 ])
!!        call check_name('hls','black',    [ 0,   0,   0   ],[ 0,   0,   0   ])
!!        call check_name('hls','white',    [ 100, 100, 100 ],[ 0,   100, 0   ])
!!        call check_name('hsv','black',    [ 0,   0,   0   ],[ 0,   0,   0   ])
!!        !                      NAME        RGB(0-255)            HSV(0-100)
!!        call check_name('hsv','gray50',   [ 50,  50,  50  ],[ 0,   0,   50  ])
!!        call check_name('hsv','silver',   [ 75,  75,  75  ],[ 0,   0,   75  ])
!!        call check_name('hsv','white',    [ 100, 100, 100 ],[ 0,   0,   100 ])
!!        call check_name('hsv','red4',     [ 55,  0,   0   ],[ 0,   100, 55  ])
!!        call check_name('hsv','red',      [ 100, 0,   0   ],[ 0,   100, 100 ])
!!        call check_name('hsv','olive',    [ 50,  50,  0   ],[ 60,  100, 50  ])
!!        call check_name('hsv','yellow',   [ 100, 100, 0   ],[ 60,  100, 100 ])
!!        call check_name('hsv','green',    [ 0,   100, 0   ],[ 120, 100, 100 ])
!!        call check_name('hsv','lime',     [ 0,   100, 0   ],[ 120, 100, 100 ])
!!        call check_name('hsv','teal',     [ 0,   50,  50  ],[ 180, 100, 50  ])
!!        call check_name('hsv','cyan',     [ 0,   100, 100 ],[ 180, 100, 100 ])
!!        call check_name('hsv','navy',     [ 0,   0,   50  ],[ 240, 100, 50  ])
!!        call check_name('hsv','blue',     [ 0,   0,   100 ],[ 240, 100, 100 ])
!!        call check_name('hsv','purple',   [ 63,  13,  94  ],[ 277, 87,  94  ])
!!        call check_name('hsv','magenta4', [ 55,  0,   55  ],[ 300, 100, 55  ])
!!        call check_name('hsv','magenta',  [ 100, 0,   100 ],[ 300, 100, 100 ])
!!        call check_name('hsv','maroon',   [ 69,  19,  38  ],[ 338, 73,  69  ])
!!     contains
!!     subroutine check_name(modelout,name,rgb,other)
!!     ! given a color convert to MODELOUT and compare to expected values
!!     character(len=*),intent(in)   :: name
!!     integer,intent(in)            :: rgb(3), other(3)
!!     character(len=*),intent(in)   :: modelout
!!        real                       :: val1,val2,val3
!!        integer                    :: status
!!        ! convert RGB values to MODELOUT values
!!        call hue('rgb',REAL(rgb(1)),REAL(rgb(2)),REAL(rgb(3)), &
!!        & modelout,val1,val2,val3,status)
!!           write(*,*)'COLOR '//trim(name)
!!           write(*,*)'EXPECTED '//modelout//' ====>',other
!!           write(*,*)'RETURNED '//modelout//' ====>', &
!!           & int([val1+0.5,val2+0.5,val3+0.5])
!!           write(*,*)'STATUS ==========>',status
!!     end subroutine check_name
!!     end program demo_hue
!!
!!    Results:
!!
!!     COLOR red
!!     EXPECTED hls ====>           0          50         100
!!     RETURNED hls ====>           0          50         100
!!     STATUS ==========>           0
!!     COLOR orange
!!     EXPECTED hls ====>          39          50         100
!!     RETURNED hls ====>          39          50         100
!!     STATUS ==========>           0
!!     COLOR yellow
!!     EXPECTED hls ====>          60          50         100
!!     RETURNED hls ====>          60          50         100
!!     STATUS ==========>           0
!!     COLOR green
!!     EXPECTED hls ====>         120          50         100
!!     RETURNED hls ====>         120          50         100
!!     STATUS ==========>           0
!!     COLOR cyan
!!     EXPECTED hls ====>         180          50         100
!!     RETURNED hls ====>         180          50         100
!!     STATUS ==========>           0
!!     COLOR blue
!!     EXPECTED hls ====>         240          50         100
!!     RETURNED hls ====>         240          50         100
!!     STATUS ==========>           0
!!     COLOR magenta
!!     EXPECTED hls ====>         300          50         100
!!     RETURNED hls ====>         300          50         100
!!     STATUS ==========>           0
!!     COLOR black
!!     EXPECTED hls ====>           0           0           0
!!     RETURNED hls ====>           0           0           0
!!     STATUS ==========>           0
!!     COLOR white
!!     EXPECTED hls ====>           0         100           0
!!     RETURNED hls ====>           0         100           0
!!     STATUS ==========>           0
!!     COLOR black
!!     EXPECTED hsv ====>           0           0           0
!!     RETURNED hsv ====>           0           0           0
!!     STATUS ==========>           0
!!     COLOR gray50
!!     EXPECTED hsv ====>           0           0          50
!!     RETURNED hsv ====>           0           0          50
!!     STATUS ==========>           0
!!     COLOR silver
!!     EXPECTED hsv ====>           0           0          75
!!     RETURNED hsv ====>           0           0          75
!!     STATUS ==========>           0
!!     COLOR white
!!     EXPECTED hsv ====>           0           0         100
!!     RETURNED hsv ====>           0           0         100
!!     STATUS ==========>           0
!!     COLOR red4
!!     EXPECTED hsv ====>           0         100          55
!!     RETURNED hsv ====>           0         100          55
!!     STATUS ==========>           0
!!     COLOR red
!!     EXPECTED hsv ====>           0         100         100
!!     RETURNED hsv ====>           0         100         100
!!     STATUS ==========>           0
!!     COLOR olive
!!     EXPECTED hsv ====>          60         100          50
!!     RETURNED hsv ====>          60         100          50
!!     STATUS ==========>           0
!!     COLOR yellow
!!     EXPECTED hsv ====>          60         100         100
!!     RETURNED hsv ====>          60         100         100
!!     STATUS ==========>           0
!!     COLOR green
!!     EXPECTED hsv ====>         120         100         100
!!     RETURNED hsv ====>         120         100         100
!!     STATUS ==========>           0
!!     COLOR lime
!!     EXPECTED hsv ====>         120         100         100
!!     RETURNED hsv ====>         120         100         100
!!     STATUS ==========>           0
!!     COLOR teal
!!     EXPECTED hsv ====>         180         100          50
!!     RETURNED hsv ====>         180         100          50
!!     STATUS ==========>           0
!!     COLOR cyan
!!     EXPECTED hsv ====>         180         100         100
!!     RETURNED hsv ====>         180         100         100
!!     STATUS ==========>           0
!!     COLOR navy
!!     EXPECTED hsv ====>         240         100          50
!!     RETURNED hsv ====>         240         100          50
!!     STATUS ==========>           0
!!     COLOR blue
!!     EXPECTED hsv ====>         240         100         100
!!     RETURNED hsv ====>         240         100         100
!!     STATUS ==========>           0
!!     COLOR purple
!!     EXPECTED hsv ====>         277          87          94
!!     RETURNED hsv ====>         277          86          94
!!     STATUS ==========>           0
!!     COLOR magenta4
!!     EXPECTED hsv ====>         300         100          55
!!     RETURNED hsv ====>         300         100          55
!!     STATUS ==========>           0
!!     COLOR magenta
!!     EXPECTED hsv ====>         300         100         100
!!     RETURNED hsv ====>         300         100         100
!!     STATUS ==========>           0
!!     COLOR maroon
!!     EXPECTED hsv ====>         338          73          69
!!     RETURNED hsv ====>         337          72          69
!!     STATUS ==========>           0
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE HUE(MODEI,CLR1I,CLR2I,CLR3I,MODEO,CLR1O,CLR2O,CLR3O,STATUS)
CHARACTER(LEN=*),INTENT(IN) :: MODEI
REAL,INTENT(IN)             :: CLR1I,CLR2I,CLR3I
CHARACTER(LEN=*),INTENT(IN) :: MODEO
REAL,INTENT(OUT)            :: CLR1O,CLR2O,CLR3O
INTEGER,INTENT(OUT)         :: STATUS

CHARACTER(LEN=3)            :: INPUT_COLOR_MODEL,OUTPUT_COLOR_MODEL
REAL                        :: C1, C2, C3, R, G, B
!-----------------------------------------------------------------------------------------------------------------------------------
!-- initialize the status flag.
   STATUS=0
!-- set the output colors equal to invalid values
   CLR1O=-99999.0
   CLR2O=-99999.0
   CLR3O=-99999.0
!-- ensure that the input character strings are lowercase
   INPUT_COLOR_MODEL=LOWER(MODEI)
   OUTPUT_COLOR_MODEL=LOWER(MODEO)
!-----------------------------------------------------------------------------------------------------------------------------------
!-- check for a trivial instance where the input and output model names are the same
   IF(INPUT_COLOR_MODEL .EQ. OUTPUT_COLOR_MODEL) THEN
      CLR1O=CLR1I
      CLR2O=CLR2I
      CLR3O=CLR3I
      STATUS=-1
      RETURN
   ENDIF
!-----------------------------------------------------------------------------------------------------------------------------------
!-- check for a transpose of terms, another trivial instance.
   SELECT CASE (INPUT_COLOR_MODEL)
   CASE ('hls','hsl','hvs','hsv')
      IF( INPUT_COLOR_MODEL.EQ.'hls' .AND. OUTPUT_COLOR_MODEL.EQ.'hsl'   &
    & .OR.INPUT_COLOR_MODEL.EQ.'hsl' .AND. OUTPUT_COLOR_MODEL.EQ.'hls'   &
    & .OR.INPUT_COLOR_MODEL.EQ.'hvs' .AND. OUTPUT_COLOR_MODEL.EQ.'hsv'   &
    & .OR.INPUT_COLOR_MODEL.EQ.'hsv' .AND. OUTPUT_COLOR_MODEL.EQ.'hvs') THEN
         CLR1O=CLR1I
         CLR2O=CLR3I
         CLR3O=CLR2I
         STATUS=-1
         RETURN
      ENDIF
   END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
!-- assign new variables so that the input arguments can't possibly be changed by subsequent procedures.
   C1=CLR1I
   C2=CLR2I
   C3=CLR3I
!-----------------------------------------------------------------------------------------------------------------------------------
!-- first, convert input values to rgb values.
   SELECT CASE (INPUT_COLOR_MODEL)
   CASE ('hls'); CALL HLSRGB(C1,C2,C3,R,G,B,STATUS)
   CASE ('hvs'); CALL HVSRGB(C1,C2,C3,R,G,B,STATUS)
   CASE ('hsl'); CALL HLSRGB(C1,C3,C2,R,G,B,STATUS)
   CASE ('hsv'); CALL HVSRGB(C1,C3,C2,R,G,B,STATUS)
   CASE ('cmy'); CALL CMYRGB(C1,C2,C3,R,G,B,STATUS)
   CASE ('yiq'); CALL YIQRGB(C1,C2,C3,R,G,B,STATUS)
   CASE ('rgb'); R=C1;G=C2;B=C3
   CASE DEFAULT ! unknown input model name
      STATUS=2
      RETURN
   END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
   IF(STATUS .NE. 0 )THEN
      RETURN
   ENDIF
!-----------------------------------------------------------------------------------------------------------------------------------
!-- then convert from RGB to the desired output values
!
   SELECT CASE (OUTPUT_COLOR_MODEL)
   CASE ('hls'); CALL RGBHLS(R,G,B,CLR1O,CLR2O,CLR3O,STATUS)
   CASE ('hsl'); CALL RGBHLS(R,G,B,CLR1O,CLR3O,CLR2O,STATUS)
   CASE ('hvs'); CALL RGBHVS(R,G,B,CLR1O,CLR2O,CLR3O,STATUS)
   CASE ('hsv'); CALL RGBHVS(R,G,B,CLR1O,CLR3O,CLR2O,STATUS)
   CASE ('cmy'); CALL RGBCMY(R,G,B,CLR1O,CLR2O,CLR3O,STATUS)
   CASE ('rgb'); CLR1O=R; CLR2O=G; CLR3O=B
   CASE ('yiq'); CALL RGBYIQ(R,G,B,CLR1O,CLR2O,CLR3O,STATUS)
   CASE DEFAULT ! unknown output model name
      STATUS=3
      RETURN
   END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
   IF(STATUS .NE. 0 )THEN
      RETURN
   ENDIF
!-----------------------------------------------------------------------------------------------------------------------------------
END SUBROUTINE HUE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE RGBHLS(R0,G0,B0,H,L,S,STATUS)

!     given  : r, g, b each as a value of 0 to 100
!     desired: h as a value of 0 to 360 degrees.
!     .        l and s each as a value of 0 to 100
!
REAL    :: R0,G0,B0
REAL    :: R,G,B,H,L,S
REAL    :: CLRMAX,CLRMIN,CLRDEL,CLRSUM,RR,GG,BB
INTEGER :: STATUS
   IF(R0 .LT. 0.0 .OR. R0 .GT. 100.0 ) STATUS = 1 !---- passive check for valid range of values.
   IF(G0 .LT. 0.0 .OR. G0 .GT. 100.0 ) STATUS = 1 !---- passive check for valid range of values.
   IF(B0 .LT. 0.0 .OR. B0 .GT. 100.0 ) STATUS = 1 !---- passive check for valid range of values.
   R=R0/100.0
   G=G0/100.0
   B=B0/100.0
   CLRMAX=AMAX1(R,G,B)
   CLRMIN=AMIN1(R,G,B)
   CLRDEL=CLRMAX-CLRMIN
   CLRSUM=CLRMAX+CLRMIN
   L=CLRSUM/2.0
   IF(CLRDEL.NE.0.0 ) THEN
      RR=(CLRMAX-R)/CLRDEL
      GG=(CLRMAX-G)/CLRDEL
      BB=(CLRMAX-B)/CLRDEL
      IF(L.LE.0.5) THEN
         S=CLRDEL/CLRSUM
      ELSE
         S=CLRDEL/(2.0-CLRSUM)
      ENDIF
      IF(R.EQ.CLRMAX) THEN
         H=BB-GG
      ELSE IF(G.EQ.CLRMAX) THEN
         H=2.0 +RR-BB
      ELSE IF(B.EQ.CLRMAX) THEN
         H=4.0 +GG-RR
      ENDIF
      H=H*60.0
      IF(H.LT.0.0 ) THEN
         H=H+360.0
      ENDIF
   ELSE
      S=0.0
      H=0.0
   ENDIF
   L=L*100.0
   S=S*100.0
   IF(H .LT. 0.0 ) H = 0.0   !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   IF(H .GT. 360.0 ) H = 360.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   IF(L .LT. 0.0 ) L=0.0
   IF(L .GT. 100.0 ) L = 100.0
   IF(S .LT. 0.0 ) S=0.0
   IF(S .GT. 100.0 ) S = 100.0
END SUBROUTINE RGBHLS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE RGBHVS(R0,G0,B0,H,V,S,STATUS)

! ident_62="@(#)M_pixel::rgbhvs(3fp): given red,green,blue calculate hue,saturation,value components"

!---- this procedure calculates a hue, saturation, value equivalent for a
!     color given in red, green, & blue components.
!     given  : r, g, b each as a value of 0 to 100.
!     desired: h as a value of 0 to 360 degrees.
!     .        s and v each as a value of 0 to 100.
!
REAL,INTENT(IN)  :: R0,G0,B0
REAL,INTENT(OUT) :: H,V,S
INTEGER          :: STATUS
REAL             :: R,G,B
REAL             :: CLRMAX,CLRMIN,CLRDEL,RR,GG,BB
   IF(R0 .LT. 0.0 .OR. R0 .GT. 100.0 ) STATUS = 1 !---- check for valid range of values.
   IF(G0 .LT. 0.0 .OR. G0 .GT. 100.0 ) STATUS = 1 !---- check for valid range of values.
   IF(B0 .LT. 0.0 .OR. B0 .GT. 100.0 ) STATUS = 1 !---- check for valid range of values.
   R=R0
   G=G0
   B=B0
   R=R/100.0
   G=G/100.0
   B=B/100.0
   CLRMAX=AMAX1(R,G,B)
   CLRMIN=AMIN1(R,G,B)
   CLRDEL=CLRMAX-CLRMIN
   V=CLRMAX
   IF(CLRMAX.NE.0.0 )THEN
         S=CLRDEL/CLRMAX
   ELSE
         S=0.0
   ENDIF
   IF(S.NE.0.0 )THEN
         RR=(CLRMAX-R)/CLRDEL
         GG=(CLRMAX-G)/CLRDEL
         BB=(CLRMAX-B)/CLRDEL
         IF(R.EQ.CLRMAX)THEN
            H=BB-GG
         ELSE IF(G.EQ.CLRMAX) THEN
            H=2.0 +RR-BB
         ELSE IF(B.EQ.CLRMAX) THEN
            H=4.0 +GG-RR
         ENDIF
         H=H*60.0
         IF(H.LT.0.0 ) THEN
            H=H+360.0
         ENDIF
   ENDIF
   V=V*100.0
   S=S*100.0
   IF(H .GT. 360.0 ) H = 360.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   IF(H .LT. 0.0 ) H =   0.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   IF(V .GT. 100.0 ) V = 100.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   IF(V .LT. 0.0 ) V =   0.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   IF(S .GT. 100.0 ) S = 100.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   IF(S .LT. 0.0 ) S =   0.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
END SUBROUTINE RGBHVS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE CMYRGB(C,M,Y,R,G,B,STATUS)

! ident_63="@(#)M_pixel::cmyrgb(3fp): given cyan,magenta,yellow calculate red,green,blue components"

! given  : r, g, b each as a value of 0 to 100
! desired: c, m, y each as a value of 0 to 100
REAL,INTENT(IN)   :: C,M,Y
REAL,INTENT(OUT)  :: R,G,B
INTEGER           :: STATUS
   IF(C .LT. 0.0 .OR. C .GT. 100.0 ) STATUS = 1 !---- passively check for valid range of values.
   IF(M .LT. 0.0 .OR. M .GT. 100.0 ) STATUS = 1 !---- passively check for valid range of values.
   IF(Y .LT. 0.0 .OR. Y .GT. 100.0 ) STATUS = 1 !---- passively check for valid range of values.
   R= 100.0 - C
   G= 100.0 - M
   B= 100.0 - Y
END SUBROUTINE CMYRGB
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE RGBCMY(R,G,B,C,M,Y,STATUS)

! ident_64="@(#)M_pixel::rgbcmy(3fp): given red,green,blue calculate cyan,magenta,yellow components"

!     given  : r, g, b each as a value of 0 to 100
!     desired: c, m, y each as a value of 0 to 100
REAL,INTENT(IN)  :: R,G,B
REAL,INTENT(OUT) :: C,M,Y
INTEGER          :: STATUS
   IF(R .LT. 0.0 .OR. R .GT. 100.0 ) STATUS = 1 !---- check for valid range of values.
   IF(G .LT. 0.0 .OR. G .GT. 100.0 ) STATUS = 1 !---- check for valid range of values.
   IF(B .LT. 0.0 .OR. B .GT. 100.0 ) STATUS = 1 !---- check for valid range of values.
   C = 100.0 - R
   M = 100.0 - G
   Y = 100.0 - B

END SUBROUTINE RGBCMY
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE RGBMONO(RR,RG,RB,RI,STATUS)

! ident_65="@(#)M_pixel::rgbmono(3f): convert RGB colors to a reasonable grayscale"

! monochrome devices that support intensity can have intensity calculated from the specified Red, Green, Blue
! intensities as 0.30*R + 0.59*G + 0.11*B, as in US color television systems, NTSC encoding.
! Note that most devices do not have an infinite range of monochrome intensities available.

REAL,INTENT(IN)      :: RR,RG,RB                ! red, green, blue, & intensity range from 0 to 100
REAL,INTENT(OUT)     :: RI
INTEGER,INTENT(OUT)  :: STATUS
   STATUS=0
   IF(RR .LT. 0.0 .OR. RR .GT. 100.0 ) STATUS = 1 !---- passive check for valid range of values.
   IF(RG .LT. 0.0 .OR. RG .GT. 100.0 ) STATUS = 1 !---- passive check for valid range of values.
   IF(RB .LT. 0.0 .OR. RB .GT. 100.0 ) STATUS = 1 !---- passive check for valid range of values.
   RI = 0.30*RR + 0.59*RG + 0.11*RB
END SUBROUTINE RGBMONO
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
REAL FUNCTION RGBVAL(CLR1,CLR2,H)

! ident_66="@(#)M_pixel::rgbval(3fp): ensure a value is in the appropriate range and quadrant"

REAL    :: CLR1,CLR2
REAL    :: H
REAL    :: H2
   H2=H
   DO
      IF(H2.GT.360.0 ) THEN
         H2=H2-360.0
         CYCLE
      ENDIF
      EXIT
   ENDDO

   DO
      IF( H2 .LT. 0.0 ) THEN
         H2=H2+360.0
         CYCLE
      ENDIF
      EXIT
   ENDDO

   IF(H2.LT.60.0 ) THEN
      RGBVAL=CLR1+(CLR2-CLR1)*H2/60.0
   ELSE IF(H2.LT.180.0) THEN
      RGBVAL=CLR2
   ELSE IF(H2.LT.240.0) THEN
      RGBVAL=CLR1+(CLR2-CLR1)*(240.0-H2)/60.0
   ELSE
      RGBVAL=CLR1
   ENDIF

END FUNCTION RGBVAL
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE HLSRGB(H,L,S,R,G,B,STATUS)

! ident_67="@(#)M_pixel::hlsrgb(3fp): convert HLS(hue,lightness,saturation) values to RGB components"

!     given  : hue as a value of 0 to 360 degrees.
!     .        lightness and saturation each as a value of 0 to 100.
!     desired: r, g, and b each as a value of 0 to 100.
!
REAL,INTENT(IN)   :: H,L,S
REAL,INTENT(OUT)  :: R,G,B
INTEGER           :: STATUS
REAL              :: HUE,LIGHTNESS,SATURATION
REAL              :: CLR1,CLR2
   IF(H .LT. 0.0 .OR. H .GT.360.0 ) STATUS = 1 ! passively report on bad input values
   IF(L .LT. 0.0 .OR. L .GT.100.0 ) STATUS = 1 ! passively report on bad input values
   IF(S .LT. 0.0 .OR. S .GT.100.0 ) STATUS = 1 ! passively report on bad input values
   HUE =           H
   LIGHTNESS =     L/100.0
   SATURATION =    S/100.0
   IF( SATURATION .EQ. 0.0 ) THEN
      R = LIGHTNESS
      G = LIGHTNESS
      B = LIGHTNESS
   ENDIF
   IF(LIGHTNESS .LE. 0.50) THEN
      CLR2= LIGHTNESS*( 1.0 + SATURATION )
   ELSE
      CLR2= LIGHTNESS + SATURATION - LIGHTNESS * SATURATION
   ENDIF
   CLR1= 2.0 * LIGHTNESS - CLR2
   R = RGBVAL(CLR1,CLR2,HUE+120.0)  *100.0
   G = RGBVAL(CLR1,CLR2,HUE)        *100.0
   B = RGBVAL(CLR1,CLR2,HUE-120.0)  *100.0
END SUBROUTINE HLSRGB
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE HVSRGB(H,V,S,R,G,B,STATUS)

! ident_68="@(#)M_pixel::hvsrgb(3fp): given hue,saturation,value calculate red,green,blue components"

!     given  : hue as value of 0 to 360 degrees.
!     .        saturation and value each as a value of 0 to 100.
!     desired: r, g, and b as a value of 0 to 100.
REAL,INTENT(IN)    :: H,V,S
REAL,INTENT(OUT)   :: R,G,B
INTEGER            :: STATUS
REAL               :: HUE,VALUE,SATURATION
INTEGER            :: IFLOOR
REAL               :: F,P,Q,T
   IF(H .LT. 0.0 .OR. H .GT.360.0 ) STATUS = 1 ! passively report on bad input values
   IF(V .LT. 0.0 .OR. V .GT.100.0 ) STATUS = 1 ! passively report on bad input values
   IF(S .LT. 0.0 .OR. S .GT.100.0 ) STATUS = 1 ! passively report on bad input values
   HUE=H
   VALUE=V/100.0
   SATURATION=S/100.0
!-----------------------------------------------------------------------------------------------------------------------------------
   IF(SATURATION.EQ.0.0) THEN
      R=VALUE
      G=VALUE
      B=VALUE
   ENDIF
!-----------------------------------------------------------------------------------------------------------------------------------
   IF(HUE.EQ.360.0) THEN
      HUE=0.0
   ENDIF
!-----------------------------------------------------------------------------------------------------------------------------------
   HUE=HUE/60.0
   IFLOOR=INT(HUE)
   F=HUE-IFLOOR
   P=VALUE*(1.0-SATURATION)
   Q=VALUE*(1.0-(SATURATION*F))
   T=VALUE*(1.0-(SATURATION*(1-F)))
   SELECT CASE (IFLOOR)
   CASE (0) ;R=VALUE; G=T; B=P
   CASE (1) ;R=Q; G=VALUE; B=P
   CASE (2) ;R=P; G=VALUE; B=T
   CASE (3) ;R=P; G=Q; B=VALUE
   CASE (4) ;R=T; G=P; B=VALUE
   CASE (5) ;R=VALUE; G=P; B=Q
   CASE DEFAULT
   END SELECT
   R=R*100.0
   G=G*100.0
   B=B*100.0
END SUBROUTINE HVSRGB
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE YIQRGB(Y,I,Q,R,G,B,STATUS)

! ident_69="@(#)M_pixel::yiqrgb(3fp): convert luma,orange-blue chrominance,purple-green chrominance to RGB"

REAL,INTENT(IN)  :: Y,I,Q
REAL,INTENT(OUT) :: R,G,B
INTEGER          :: STATUS
!
!----    i don't believe that this is an exhaustive test of value ranges
!        for yiq. for example yiq=(100.0,60.0,52.0) when converted to
!        rgb produces values greater than 100!?
!
      IF(I .LT. -60.0 .OR. I .GT.  60.0) STATUS = 1
      IF(Q .LT. -53.0 .OR. Q .GT.  53.0) STATUS = 1

      R = 1.0 * Y + 0.956 * I + 0.621 * Q
      G = 1.0 * Y - 0.272 * I - 0.647 * Q
      B = 1.0 * Y - 1.106 * I + 1.703 * Q
      !r= 1.0 *y + 0.94826224*i + 0.62401264*q
      !g= 1.0 *y - 0.27606635*i - 0.63981043*q
      !b= 1.0 *y - 1.1054502 *i + 1.7298578 *q
!
!-- If outside the valid range of values, truncate to allow for reasonable roundoff and then retest.
!   This should pass values essentially 0 or 100, but fail others.
!   The above formula for rgb from yiq can give answers slightly less than 0 and slightly greater than 100.0
!   The truncation should fix this.
!   The retest should then catch the instances such as yiq=(100.0,60.0,52.0) as mentioned earlier.

   R=MIN(100.0,MAX(0.0,R))
   G=MIN(100.0,MAX(0.0,G))
   B=MIN(100.0,MAX(0.0,B))

END SUBROUTINE YIQRGB
!=============================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
SUBROUTINE RGBYIQ(R,G,B,Y,I,Q,STATUS)

! ident_70="@(#)M_pixel::rgbyiq(3fp): convert RGB to luma,orange-blue chrominance,purple-green chrominance"

REAL,INTENT(IN)  :: R,G,B
REAL,INTENT(OUT) :: Y,I,Q
INTEGER          :: STATUS
   IF(R.LT.0.0 .OR. R.GT.100.0) STATUS=1
   IF(G.LT.0.0 .OR. G.GT.100.0) STATUS=1
   IF(B.LT.0.0 .OR. B.GT.100.0) STATUS=1

   Y= 0.299 * R + 0.587 * G + 0.114 * B
   I= 0.596 * R - 0.274 * G - 0.322 * B
   Q= 0.211 * R - 0.523 * G + 0.312 * B

!-- Eliminate any roundoff that exceeds the limits.
   IF(I .LT. -59.57 ) I = -59.57
   IF(I .GT.  59.57 ) I =  59.57
   IF(Q .LT. -52.26 ) Q = -52.26
   IF(Q .GT.  52.26 ) Q =  52.26
END SUBROUTINE RGBYIQ
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     closest_color_name(3f) - [M_pixel:COLOR] returns the closest name for the
!!     given RGB values.
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine closest_color_name(r,g,b,closestname)
!!
!!     real,intent(in)               :: r,g,b
!!     character(len=20),intent(out) :: closestname
!!
!!##DESCRIPTION
!!     closest_color_name() returns the closest name for the given RGB
!!     values. Most X11 Windows color names are supported.
!!
!!##OPTIONS
!!     R   red component, range of 0 to 100
!!     G   green component, range of 0 to 100
!!     B   blue component, range of 0 to 100
!!
!!##RETURNS
!!     CLOSESTNAME   name of color found closest to given RGB value</li>
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!        program demo_closest_color_name
!!        use M_pixel, only : closest_color_name
!!        implicit none
!!        character(len=100) :: string ! at least 20 characters
!!           string=' '
!!
!!           call closest_color_name(100.0,  0.0,  0.0,string)
!!           write(*,*)trim(string)
!!
!!           call closest_color_name(  0.0,100.0,  0.0,string)
!!           write(*,*)trim(string)
!!
!!           call closest_color_name(  0.0,  0.0,100.0,string)
!!           write(*,*)trim(string)
!!
!!        end program demo_closest_color_name
!!
!!    Results:
!!
!!        red
!!        green
!!        blue
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE CLOSEST_COLOR_NAME(R,G,B,CLOSESTNAME)

! ident_71="@(#)M_pixel::closest_color_name(3f): given RGB values, try to find closest named color"

REAL,INTENT(IN)               :: R,G,B
CHARACTER(LEN=*),INTENT(OUT) :: CLOSESTNAME
REAL                          :: RN,GN,BN
REAL                          :: DISTANCE, MINIMUM_DISTANCE
CHARACTER(LEN=20)             :: ECHONAME
INTEGER                       :: I
!-----------------------------------------------------------------------------------------------------------------------------------
   MINIMUM_DISTANCE=1000.0
   CLOSESTNAME='Unknown'
   INFINITE: DO I=1,1000
      CALL COLOR_NAME2RGB(I2S(I),RN,GN,BN,ECHONAME)       ! get next color
      IF(ECHONAME.EQ.'Unknown') EXIT INFINITE
      DISTANCE=SQRT( (R-RN)**2 + (G-GN)**2 + (B-BN)**2 )
      IF(DISTANCE.LT.MINIMUM_DISTANCE)THEN
         CLOSESTNAME=ECHONAME
         MINIMUM_DISTANCE=MIN(MINIMUM_DISTANCE,DISTANCE)
      ENDIF
   ENDDO INFINITE
END SUBROUTINE CLOSEST_COLOR_NAME
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     COLOR_NAME2RGB(3f) - [M_pixel:COLOR] returns the RGB values in the range 0 to 100 for a given known color name.
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine color_name2rgb(name,r,g,b,echoname)
!!
!!     character(len=20),intent(in)   :: name
!!     real,intent(out)               :: r,g,b
!!     character(len=20),intent(out)  :: echoname
!!
!!##DESCRIPTION
!!     COLOR_NAME2RGB() returns the RGB values in the range 0 to 100
!!     for a given known color name. Most X11 Windows color names are
!!     supported. If the name is not found, ECHONAME is set to "Unknown".
!!
!!##EXAMPLE
!!
!!    A sample program:
!!
!!     program demo_color_name2rgb
!!     use M_pixel, only : hue, color_name2rgb
!!     implicit none
!!     !
!!     ! list colors known to colorname2rgb(3f) & corresponding RGB values
!!     !
!!     character(len=20) :: name
!!     character(len=20) :: echoname
!!     real              :: red,green,blue
!!     integer           :: i
!!     TRYALL: do i=1,10000
!!        ! weird little thing where the color names have aliases
!!        ! that are numeric strings
!!        write(name,'(i0)')i
!!        ! get the RGB values and English name of the color
!!        call color_name2rgb(name,red,green,blue,echoname)
!!        ! the last color name is "Unknown" so the loop should exit
!!        if(echoname.eq.'Unknown')exit TRYALL
!!        ! display the English name and RGB values for the name
!!        write(*,*)echoname,int([red,green,blue])
!!     enddo TRYALL
!!     !write(*,*)'Number of colors found is ',i-1
!!     end program demo_color_name2rgb
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE COLOR_NAME2RGB(NAME,R,G,B,ECHONAME)

! ident_72="@(#)M_pixel::color_name2rgb(3f): given a color name, return rgb color values in range 0 to 100"

CHARACTER(LEN=*),INTENT(IN)            :: NAME
REAL,INTENT(OUT)                       :: R,G,B
CHARACTER(LEN=*),INTENT(OUT),OPTIONAL  :: ECHONAME
CHARACTER(LEN=20)                      :: NEWNAME
!-----------------------------------------------------------------------------------------------------------------------------------
! returns name in ECHONAME; which is usually not useful unless NAME represents an integer string.
! Note that an integer converted to a string can be used to go sequentially thru the names until NEWNAME="Unknown"
! Color names can generally be listed using showrgb(1) in GNU/Linux and Unix environments that support X11 Windows:

! A structure would normally be used for the data; but a large SELECT is easy to maintain.
! a numeric name is an alias for each color to facilitate going thru them sequentially since they are not an array.
   SELECT CASE(TRIM(LOWER(NAME)))

   CASE("1",   "snow")                  ;  NEWNAME="snow"                  ;  R=255  ;  G=250  ;  B=250
   CASE("2",   "ghostwhite")            ;  NEWNAME="ghostwhite"            ;  R=248  ;  G=248  ;  B=255
   CASE("3",   "whitesmoke")            ;  NEWNAME="whitesmoke"            ;  R=245  ;  G=245  ;  B=245
   CASE("4",   "gainsboro")             ;  NEWNAME="gainsboro"             ;  R=220  ;  G=220  ;  B=220
   CASE("5",   "floralwhite")           ;  NEWNAME="floralwhite"           ;  R=255  ;  G=250  ;  B=240
   CASE("6",   "oldlace")               ;  NEWNAME="oldlace"               ;  R=253  ;  G=245  ;  B=230
   CASE("7",   "linen")                 ;  NEWNAME="linen"                 ;  R=250  ;  G=240  ;  B=230
   CASE("8",   "antiquewhite")          ;  NEWNAME="antiquewhite"          ;  R=250  ;  G=235  ;  B=215
   CASE("9",   "papayawhip")            ;  NEWNAME="papayawhip"            ;  R=255  ;  G=239  ;  B=213
   CASE("10",  "blanchedalmond")        ;  NEWNAME="blanchedalmond"        ;  R=255  ;  G=235  ;  B=205
   CASE("11",  "bisque")                ;  NEWNAME="bisque"                ;  R=255  ;  G=228  ;  B=196
   CASE("12",  "peachpuff")             ;  NEWNAME="peachpuff"             ;  R=255  ;  G=218  ;  B=185
   CASE("13",  "navajowhite")           ;  NEWNAME="navajowhite"           ;  R=255  ;  G=222  ;  B=173
   CASE("14",  "moccasin")              ;  NEWNAME="moccasin"              ;  R=255  ;  G=228  ;  B=181
   CASE("15",  "cornsilk")              ;  NEWNAME="cornsilk"              ;  R=255  ;  G=248  ;  B=220
   CASE("16",  "ivory")                 ;  NEWNAME="ivory"                 ;  R=255  ;  G=255  ;  B=240
   CASE("17",  "lemonchiffon")          ;  NEWNAME="lemonchiffon"          ;  R=255  ;  G=250  ;  B=205
   CASE("18",  "seashell")              ;  NEWNAME="seashell"              ;  R=255  ;  G=245  ;  B=238
   CASE("19",  "honeydew")              ;  NEWNAME="honeydew"              ;  R=240  ;  G=255  ;  B=240
   CASE("20",  "mintcream")             ;  NEWNAME="mintcream"             ;  R=245  ;  G=255  ;  B=250
   CASE("21",  "azure")                 ;  NEWNAME="azure"                 ;  R=240  ;  G=255  ;  B=255
   CASE("22",  "aliceblue")             ;  NEWNAME="aliceblue"             ;  R=240  ;  G=248  ;  B=255
   CASE("23",  "lavender")              ;  NEWNAME="lavender"              ;  R=230  ;  G=230  ;  B=250
   CASE("24",  "lavenderblush")         ;  NEWNAME="lavenderblush"         ;  R=255  ;  G=240  ;  B=245
   CASE("25",  "mistyrose")             ;  NEWNAME="mistyrose"             ;  R=255  ;  G=228  ;  B=225
   CASE("26",  "white")                 ;  NEWNAME="white"                 ;  R=255  ;  G=255  ;  B=255
   CASE("27",  "black")                 ;  NEWNAME="black"                 ;  R=0    ;  G=0    ;  B=0
   CASE("28",  "darkslategray")         ;  NEWNAME="darkslategray"         ;  R=47   ;  G=79   ;  B=79
   CASE("29",  "dimgray")               ;  NEWNAME="dimgray"               ;  R=105  ;  G=105  ;  B=105
   CASE("30",  "slategray")             ;  NEWNAME="slategray"             ;  R=112  ;  G=128  ;  B=144
   CASE("31",  "lightslategray")        ;  NEWNAME="lightslategray"        ;  R=119  ;  G=136  ;  B=153
   CASE("32",  "gray")                  ;  NEWNAME="gray"                  ;  R=190  ;  G=190  ;  B=190
   CASE("33",  "lightgray")             ;  NEWNAME="lightgray"             ;  R=211  ;  G=211  ;  B=211
   CASE("34",  "midnightblue")          ;  NEWNAME="midnightblue"          ;  R=25   ;  G=25   ;  B=112
   CASE("35",  "navy")                  ;  NEWNAME="navy"                  ;  R=0    ;  G=0    ;  B=128
   CASE("36",  "navyblue")              ;  NEWNAME="navyblue"              ;  R=0    ;  G=0    ;  B=128
   CASE("37",  "cornflowerblue")        ;  NEWNAME="cornflowerblue"        ;  R=100  ;  G=149  ;  B=237
   CASE("38",  "darkslateblue")         ;  NEWNAME="darkslateblue"         ;  R=72   ;  G=61   ;  B=139
   CASE("39",  "slateblue")             ;  NEWNAME="slateblue"             ;  R=106  ;  G=90   ;  B=205
   CASE("40",  "mediumslateblue")       ;  NEWNAME="mediumslateblue"       ;  R=123  ;  G=104  ;  B=238
   CASE("41",  "lightslateblue")        ;  NEWNAME="lightslateblue"        ;  R=132  ;  G=112  ;  B=255
   CASE("42",  "mediumblue")            ;  NEWNAME="mediumblue"            ;  R=0    ;  G=0    ;  B=205
   CASE("43",  "royalblue")             ;  NEWNAME="royalblue"             ;  R=65   ;  G=105  ;  B=225
   CASE("44",  "blue")                  ;  NEWNAME="blue"                  ;  R=0    ;  G=0    ;  B=255
   CASE("45",  "dodgerblue")            ;  NEWNAME="dodgerblue"            ;  R=30   ;  G=144  ;  B=255
   CASE("46",  "deepskyblue")           ;  NEWNAME="deepskyblue"           ;  R=0    ;  G=191  ;  B=255
   CASE("47",  "skyblue")               ;  NEWNAME="skyblue"               ;  R=135  ;  G=206  ;  B=235
   CASE("48",  "lightskyblue")          ;  NEWNAME="lightskyblue"          ;  R=135  ;  G=206  ;  B=250
   CASE("49",  "steelblue")             ;  NEWNAME="steelblue"             ;  R=70   ;  G=130  ;  B=180
   CASE("50",  "lightsteelblue")        ;  NEWNAME="lightsteelblue"        ;  R=176  ;  G=196  ;  B=222
   CASE("51",  "lightblue")             ;  NEWNAME="lightblue"             ;  R=173  ;  G=216  ;  B=230
   CASE("52",  "powderblue")            ;  NEWNAME="powderblue"            ;  R=176  ;  G=224  ;  B=230
   CASE("53",  "paleturquoise")         ;  NEWNAME="paleturquoise"         ;  R=175  ;  G=238  ;  B=238
   CASE("54",  "darkturquoise")         ;  NEWNAME="darkturquoise"         ;  R=0    ;  G=206  ;  B=209
   CASE("55",  "mediumturquoise")       ;  NEWNAME="mediumturquoise"       ;  R=72   ;  G=209  ;  B=204
   CASE("56",  "turquoise")             ;  NEWNAME="turquoise"             ;  R=64   ;  G=224  ;  B=208
   CASE("57",  "cyan")                  ;  NEWNAME="cyan"                  ;  R=0    ;  G=255  ;  B=255
   CASE("58",  "lightcyan")             ;  NEWNAME="lightcyan"             ;  R=224  ;  G=255  ;  B=255
   CASE("59",  "cadetblue")             ;  NEWNAME="cadetblue"             ;  R=95   ;  G=158  ;  B=160
   CASE("60",  "mediumaquamarine")      ;  NEWNAME="mediumaquamarine"      ;  R=102  ;  G=205  ;  B=170
   CASE("61",  "aquamarine")            ;  NEWNAME="aquamarine"            ;  R=127  ;  G=255  ;  B=212
   CASE("62",  "darkgreen")             ;  NEWNAME="darkgreen"             ;  R=0    ;  G=100  ;  B=0
   CASE("63",  "darkolivegreen")        ;  NEWNAME="darkolivegreen"        ;  R=85   ;  G=107  ;  B=47
   CASE("64",  "darkseagreen")          ;  NEWNAME="darkseagreen"          ;  R=143  ;  G=188  ;  B=143
   CASE("65",  "seagreen")              ;  NEWNAME="seagreen"              ;  R=46   ;  G=139  ;  B=87
   CASE("66",  "mediumseagreen")        ;  NEWNAME="mediumseagreen"        ;  R=60   ;  G=179  ;  B=113
   CASE("67",  "lightseagreen")         ;  NEWNAME="lightseagreen"         ;  R=32   ;  G=178  ;  B=170
   CASE("68",  "palegreen")             ;  NEWNAME="palegreen"             ;  R=152  ;  G=251  ;  B=152
   CASE("69",  "springgreen")           ;  NEWNAME="springgreen"           ;  R=0    ;  G=255  ;  B=127
   CASE("70",  "lawngreen")             ;  NEWNAME="lawngreen"             ;  R=124  ;  G=252  ;  B=0
   CASE("71",  "green")                 ;  NEWNAME="green"                 ;  R=0    ;  G=255  ;  B=0
   CASE("72",  "chartreuse")            ;  NEWNAME="chartreuse"            ;  R=127  ;  G=255  ;  B=0
   CASE("73",  "mediumspringgreen")     ;  NEWNAME="mediumspringgreen"     ;  R=0    ;  G=250  ;  B=154
   CASE("74",  "greenyellow")           ;  NEWNAME="greenyellow"           ;  R=173  ;  G=255  ;  B=47
   CASE("75",  "limegreen")             ;  NEWNAME="limegreen"             ;  R=50   ;  G=205  ;  B=50
   CASE("76",  "yellowgreen")           ;  NEWNAME="yellowgreen"           ;  R=154  ;  G=205  ;  B=50
   CASE("77",  "forestgreen")           ;  NEWNAME="forestgreen"           ;  R=34   ;  G=139  ;  B=34
   CASE("78",  "olivedrab")             ;  NEWNAME="olivedrab"             ;  R=107  ;  G=142  ;  B=35
   CASE("79",  "darkkhaki")             ;  NEWNAME="darkkhaki"             ;  R=189  ;  G=183  ;  B=107
   CASE("80",  "khaki")                 ;  NEWNAME="khaki"                 ;  R=240  ;  G=230  ;  B=140
   CASE("81",  "palegoldenrod")         ;  NEWNAME="palegoldenrod"         ;  R=238  ;  G=232  ;  B=170
   CASE("82",  "lightgoldenrodyellow")  ;  NEWNAME="lightgoldenrodyellow"  ;  R=250  ;  G=250  ;  B=210
   CASE("83",  "lightyellow")           ;  NEWNAME="lightyellow"           ;  R=255  ;  G=255  ;  B=224
   CASE("84",  "yellow")                ;  NEWNAME="yellow"                ;  R=255  ;  G=255  ;  B=0
   CASE("85",  "gold")                  ;  NEWNAME="gold"                  ;  R=255  ;  G=215  ;  B=0
   CASE("86",  "lightgoldenrod")        ;  NEWNAME="lightgoldenrod"        ;  R=238  ;  G=221  ;  B=130
   CASE("87",  "goldenrod")             ;  NEWNAME="goldenrod"             ;  R=218  ;  G=165  ;  B=32
   CASE("88",  "darkgoldenrod")         ;  NEWNAME="darkgoldenrod"         ;  R=184  ;  G=134  ;  B=11
   CASE("89",  "rosybrown")             ;  NEWNAME="rosybrown"             ;  R=188  ;  G=143  ;  B=143
   CASE("90",  "indianred")             ;  NEWNAME="indianred"             ;  R=205  ;  G=92   ;  B=92
   CASE("91",  "saddlebrown")           ;  NEWNAME="saddlebrown"           ;  R=139  ;  G=69   ;  B=19
   CASE("92",  "sienna")                ;  NEWNAME="sienna"                ;  R=160  ;  G=82   ;  B=45
   CASE("93",  "peru")                  ;  NEWNAME="peru"                  ;  R=205  ;  G=133  ;  B=63
   CASE("94",  "burlywood")             ;  NEWNAME="burlywood"             ;  R=222  ;  G=184  ;  B=135
   CASE("95",  "beige")                 ;  NEWNAME="beige"                 ;  R=245  ;  G=245  ;  B=220
   CASE("96",  "wheat")                 ;  NEWNAME="wheat"                 ;  R=245  ;  G=222  ;  B=179
   CASE("97",  "sandybrown")            ;  NEWNAME="sandybrown"            ;  R=244  ;  G=164  ;  B=96
   CASE("98",  "tan")                   ;  NEWNAME="tan"                   ;  R=210  ;  G=180  ;  B=140
   CASE("99",  "chocolate")             ;  NEWNAME="chocolate"             ;  R=210  ;  G=105  ;  B=30
   CASE("100", "firebrick")             ;  NEWNAME="firebrick"             ;  R=178  ;  G=34   ;  B=34
   CASE("101", "brown")                 ;  NEWNAME="brown"                 ;  R=165  ;  G=42   ;  B=42
   CASE("102", "darksalmon")            ;  NEWNAME="darksalmon"            ;  R=233  ;  G=150  ;  B=122
   CASE("103", "salmon")                ;  NEWNAME="salmon"                ;  R=250  ;  G=128  ;  B=114
   CASE("104", "lightsalmon")           ;  NEWNAME="lightsalmon"           ;  R=255  ;  G=160  ;  B=122
   CASE("105", "orange")                ;  NEWNAME="orange"                ;  R=255  ;  G=165  ;  B=0
   CASE("106", "darkorange")            ;  NEWNAME="darkorange"            ;  R=255  ;  G=140  ;  B=0
   CASE("107", "coral")                 ;  NEWNAME="coral"                 ;  R=255  ;  G=127  ;  B=80
   CASE("108", "lightcoral")            ;  NEWNAME="lightcoral"            ;  R=240  ;  G=128  ;  B=128
   CASE("109", "tomato")                ;  NEWNAME="tomato"                ;  R=255  ;  G=99   ;  B=71
   CASE("110", "orangered")             ;  NEWNAME="orangered"             ;  R=255  ;  G=69   ;  B=0
   CASE("111", "red")                   ;  NEWNAME="red"                   ;  R=255  ;  G=0    ;  B=0
   CASE("116", "palevioletred")         ;  NEWNAME="palevioletred"         ;  R=219  ;  G=112  ;  B=147
   CASE("117", "maroon")                ;  NEWNAME="maroon"                ;  R=176  ;  G=48   ;  B=96
   CASE("118", "mediumvioletred")       ;  NEWNAME="mediumvioletred"       ;  R=199  ;  G=21   ;  B=133
   CASE("119", "violetred")             ;  NEWNAME="violetred"             ;  R=208  ;  G=32   ;  B=144
   CASE("120", "magenta")               ;  NEWNAME="magenta"               ;  R=255  ;  G=0    ;  B=255
   CASE("121", "violet")                ;  NEWNAME="violet"                ;  R=238  ;  G=130  ;  B=238
   CASE("122", "plum")                  ;  NEWNAME="plum"                  ;  R=221  ;  G=160  ;  B=221
   CASE("123", "orchid")                ;  NEWNAME="orchid"                ;  R=218  ;  G=112  ;  B=214
   CASE("124", "mediumorchid")          ;  NEWNAME="mediumorchid"          ;  R=186  ;  G=85   ;  B=211
   CASE("125", "darkorchid")            ;  NEWNAME="darkorchid"            ;  R=153  ;  G=50   ;  B=204
   CASE("126", "darkviolet")            ;  NEWNAME="darkviolet"            ;  R=148  ;  G=0    ;  B=211
   CASE("127", "blueviolet")            ;  NEWNAME="blueviolet"            ;  R=138  ;  G=43   ;  B=226
   CASE("128", "purple")                ;  NEWNAME="purple"                ;  R=160  ;  G=32   ;  B=240
   CASE("129", "mediumpurple")          ;  NEWNAME="mediumpurple"          ;  R=147  ;  G=112  ;  B=219
   CASE("130", "thistle")               ;  NEWNAME="thistle"               ;  R=216  ;  G=191  ;  B=216
   CASE("131", "snow1")                 ;  NEWNAME="snow1"                 ;  R=255  ;  G=250  ;  B=250
   CASE("132", "snow2")                 ;  NEWNAME="snow2"                 ;  R=238  ;  G=233  ;  B=233
   CASE("133", "snow3")                 ;  NEWNAME="snow3"                 ;  R=205  ;  G=201  ;  B=201
   CASE("134", "snow4")                 ;  NEWNAME="snow4"                 ;  R=139  ;  G=137  ;  B=137
   CASE("135", "seashell1")             ;  NEWNAME="seashell1"             ;  R=255  ;  G=245  ;  B=238
   CASE("136", "seashell2")             ;  NEWNAME="seashell2"             ;  R=238  ;  G=229  ;  B=222
   CASE("137", "seashell3")             ;  NEWNAME="seashell3"             ;  R=205  ;  G=197  ;  B=191
   CASE("138", "seashell4")             ;  NEWNAME="seashell4"             ;  R=139  ;  G=134  ;  B=130
   CASE("139", "antiquewhite1")         ;  NEWNAME="antiquewhite1"         ;  R=255  ;  G=239  ;  B=219
   CASE("140", "antiquewhite2")         ;  NEWNAME="antiquewhite2"         ;  R=238  ;  G=223  ;  B=204
   CASE("141", "antiquewhite3")         ;  NEWNAME="antiquewhite3"         ;  R=205  ;  G=192  ;  B=176
   CASE("142", "antiquewhite4")         ;  NEWNAME="antiquewhite4"         ;  R=139  ;  G=131  ;  B=120
   CASE("143", "bisque1")               ;  NEWNAME="bisque1"               ;  R=255  ;  G=228  ;  B=196
   CASE("144", "bisque2")               ;  NEWNAME="bisque2"               ;  R=238  ;  G=213  ;  B=183
   CASE("145", "bisque3")               ;  NEWNAME="bisque3"               ;  R=205  ;  G=183  ;  B=158
   CASE("146", "bisque4")               ;  NEWNAME="bisque4"               ;  R=139  ;  G=125  ;  B=107
   CASE("147", "peachpuff1")            ;  NEWNAME="peachpuff1"            ;  R=255  ;  G=218  ;  B=185
   CASE("148", "peachpuff2")            ;  NEWNAME="peachpuff2"            ;  R=238  ;  G=203  ;  B=173
   CASE("149", "peachpuff3")            ;  NEWNAME="peachpuff3"            ;  R=205  ;  G=175  ;  B=149
   CASE("150", "peachpuff4")            ;  NEWNAME="peachpuff4"            ;  R=139  ;  G=119  ;  B=101
   CASE("151", "navajowhite1")          ;  NEWNAME="navajowhite1"          ;  R=255  ;  G=222  ;  B=173
   CASE("152", "navajowhite2")          ;  NEWNAME="navajowhite2"          ;  R=238  ;  G=207  ;  B=161
   CASE("153", "navajowhite3")          ;  NEWNAME="navajowhite3"          ;  R=205  ;  G=179  ;  B=139
   CASE("154", "navajowhite4")          ;  NEWNAME="navajowhite4"          ;  R=139  ;  G=121  ;  B=94
   CASE("155", "lemonchiffon1")         ;  NEWNAME="lemonchiffon1"         ;  R=255  ;  G=250  ;  B=205
   CASE("156", "lemonchiffon2")         ;  NEWNAME="lemonchiffon2"         ;  R=238  ;  G=233  ;  B=191
   CASE("157", "lemonchiffon3")         ;  NEWNAME="lemonchiffon3"         ;  R=205  ;  G=201  ;  B=165
   CASE("158", "lemonchiffon4")         ;  NEWNAME="lemonchiffon4"         ;  R=139  ;  G=137  ;  B=112
   CASE("159", "cornsilk1")             ;  NEWNAME="cornsilk1"             ;  R=255  ;  G=248  ;  B=220
   CASE("160", "cornsilk2")             ;  NEWNAME="cornsilk2"             ;  R=238  ;  G=232  ;  B=205
   CASE("161", "cornsilk3")             ;  NEWNAME="cornsilk3"             ;  R=205  ;  G=200  ;  B=177
   CASE("162", "cornsilk4")             ;  NEWNAME="cornsilk4"             ;  R=139  ;  G=136  ;  B=120
   CASE("163", "ivory1")                ;  NEWNAME="ivory1"                ;  R=255  ;  G=255  ;  B=240
   CASE("164", "ivory2")                ;  NEWNAME="ivory2"                ;  R=238  ;  G=238  ;  B=224
   CASE("165", "ivory3")                ;  NEWNAME="ivory3"                ;  R=205  ;  G=205  ;  B=193
   CASE("166", "ivory4")                ;  NEWNAME="ivory4"                ;  R=139  ;  G=139  ;  B=131
   CASE("167", "honeydew1")             ;  NEWNAME="honeydew1"             ;  R=240  ;  G=255  ;  B=240
   CASE("168", "honeydew2")             ;  NEWNAME="honeydew2"             ;  R=224  ;  G=238  ;  B=224
   CASE("169", "honeydew3")             ;  NEWNAME="honeydew3"             ;  R=193  ;  G=205  ;  B=193
   CASE("170", "honeydew4")             ;  NEWNAME="honeydew4"             ;  R=131  ;  G=139  ;  B=131
   CASE("171", "lavenderblush1")        ;  NEWNAME="lavenderblush1"        ;  R=255  ;  G=240  ;  B=245
   CASE("172", "lavenderblush2")        ;  NEWNAME="lavenderblush2"        ;  R=238  ;  G=224  ;  B=229
   CASE("173", "lavenderblush3")        ;  NEWNAME="lavenderblush3"        ;  R=205  ;  G=193  ;  B=197
   CASE("174", "lavenderblush4")        ;  NEWNAME="lavenderblush4"        ;  R=139  ;  G=131  ;  B=134
   CASE("175", "mistyrose1")            ;  NEWNAME="mistyrose1"            ;  R=255  ;  G=228  ;  B=225
   CASE("176", "mistyrose2")            ;  NEWNAME="mistyrose2"            ;  R=238  ;  G=213  ;  B=210
   CASE("177", "mistyrose3")            ;  NEWNAME="mistyrose3"            ;  R=205  ;  G=183  ;  B=181
   CASE("178", "mistyrose4")            ;  NEWNAME="mistyrose4"            ;  R=139  ;  G=125  ;  B=123
   CASE("179", "azure1")                ;  NEWNAME="azure1"                ;  R=240  ;  G=255  ;  B=255
   CASE("180", "azure2")                ;  NEWNAME="azure2"                ;  R=224  ;  G=238  ;  B=238
   CASE("181", "azure3")                ;  NEWNAME="azure3"                ;  R=193  ;  G=205  ;  B=205
   CASE("182", "azure4")                ;  NEWNAME="azure4"                ;  R=131  ;  G=139  ;  B=139
   CASE("183", "slateblue1")            ;  NEWNAME="slateblue1"            ;  R=131  ;  G=111  ;  B=255
   CASE("184", "slateblue2")            ;  NEWNAME="slateblue2"            ;  R=122  ;  G=103  ;  B=238
   CASE("185", "slateblue3")            ;  NEWNAME="slateblue3"            ;  R=105  ;  G=89   ;  B=205
   CASE("186", "slateblue4")            ;  NEWNAME="slateblue4"            ;  R=71   ;  G=60   ;  B=139
   CASE("187", "royalblue1")            ;  NEWNAME="royalblue1"            ;  R=72   ;  G=118  ;  B=255
   CASE("188", "royalblue2")            ;  NEWNAME="royalblue2"            ;  R=67   ;  G=110  ;  B=238
   CASE("189", "royalblue3")            ;  NEWNAME="royalblue3"            ;  R=58   ;  G=95   ;  B=205
   CASE("190", "royalblue4")            ;  NEWNAME="royalblue4"            ;  R=39   ;  G=64   ;  B=139
   CASE("191", "blue1")                 ;  NEWNAME="blue1"                 ;  R=0    ;  G=0    ;  B=255
   CASE("192", "blue2")                 ;  NEWNAME="blue2"                 ;  R=0    ;  G=0    ;  B=238
   CASE("193", "blue3")                 ;  NEWNAME="blue3"                 ;  R=0    ;  G=0    ;  B=205
   CASE("194", "blue4")                 ;  NEWNAME="blue4"                 ;  R=0    ;  G=0    ;  B=139
   CASE("195", "dodgerblue1")           ;  NEWNAME="dodgerblue1"           ;  R=30   ;  G=144  ;  B=255
   CASE("196", "dodgerblue2")           ;  NEWNAME="dodgerblue2"           ;  R=28   ;  G=134  ;  B=238
   CASE("197", "dodgerblue3")           ;  NEWNAME="dodgerblue3"           ;  R=24   ;  G=116  ;  B=205
   CASE("198", "dodgerblue4")           ;  NEWNAME="dodgerblue4"           ;  R=16   ;  G=78   ;  B=139
   CASE("199", "steelblue1")            ;  NEWNAME="steelblue1"            ;  R=99   ;  G=184  ;  B=255
   CASE("200", "steelblue2")            ;  NEWNAME="steelblue2"            ;  R=92   ;  G=172  ;  B=238
   CASE("201", "steelblue3")            ;  NEWNAME="steelblue3"            ;  R=79   ;  G=148  ;  B=205
   CASE("202", "steelblue4")            ;  NEWNAME="steelblue4"            ;  R=54   ;  G=100  ;  B=139
   CASE("203", "deepskyblue1")          ;  NEWNAME="deepskyblue1"          ;  R=0    ;  G=191  ;  B=255
   CASE("204", "deepskyblue2")          ;  NEWNAME="deepskyblue2"          ;  R=0    ;  G=178  ;  B=238
   CASE("205", "deepskyblue3")          ;  NEWNAME="deepskyblue3"          ;  R=0    ;  G=154  ;  B=205
   CASE("206", "deepskyblue4")          ;  NEWNAME="deepskyblue4"          ;  R=0    ;  G=104  ;  B=139
   CASE("207", "skyblue1")              ;  NEWNAME="skyblue1"              ;  R=135  ;  G=206  ;  B=255
   CASE("208", "skyblue2")              ;  NEWNAME="skyblue2"              ;  R=126  ;  G=192  ;  B=238
   CASE("209", "skyblue3")              ;  NEWNAME="skyblue3"              ;  R=108  ;  G=166  ;  B=205
   CASE("210", "skyblue4")              ;  NEWNAME="skyblue4"              ;  R=74   ;  G=112  ;  B=139
   CASE("211", "lightskyblue1")         ;  NEWNAME="lightskyblue1"         ;  R=176  ;  G=226  ;  B=255
   CASE("212", "lightskyblue2")         ;  NEWNAME="lightskyblue2"         ;  R=164  ;  G=211  ;  B=238
   CASE("213", "lightskyblue3")         ;  NEWNAME="lightskyblue3"         ;  R=141  ;  G=182  ;  B=205
   CASE("214", "lightskyblue4")         ;  NEWNAME="lightskyblue4"         ;  R=96   ;  G=123  ;  B=139
   CASE("215", "slategray1")            ;  NEWNAME="slategray1"            ;  R=198  ;  G=226  ;  B=255
   CASE("216", "slategray2")            ;  NEWNAME="slategray2"            ;  R=185  ;  G=211  ;  B=238
   CASE("217", "slategray3")            ;  NEWNAME="slategray3"            ;  R=159  ;  G=182  ;  B=205
   CASE("218", "slategray4")            ;  NEWNAME="slategray4"            ;  R=108  ;  G=123  ;  B=139
   CASE("219", "lightsteelblue1")       ;  NEWNAME="lightsteelblue1"       ;  R=202  ;  G=225  ;  B=255
   CASE("220", "lightsteelblue2")       ;  NEWNAME="lightsteelblue2"       ;  R=188  ;  G=210  ;  B=238
   CASE("221", "lightsteelblue3")       ;  NEWNAME="lightsteelblue3"       ;  R=162  ;  G=181  ;  B=205
   CASE("222", "lightsteelblue4")       ;  NEWNAME="lightsteelblue4"       ;  R=110  ;  G=123  ;  B=139
   CASE("223", "lightblue1")            ;  NEWNAME="lightblue1"            ;  R=191  ;  G=239  ;  B=255
   CASE("224", "lightblue2")            ;  NEWNAME="lightblue2"            ;  R=178  ;  G=223  ;  B=238
   CASE("225", "lightblue3")            ;  NEWNAME="lightblue3"            ;  R=154  ;  G=192  ;  B=205
   CASE("226", "lightblue4")            ;  NEWNAME="lightblue4"            ;  R=104  ;  G=131  ;  B=139
   CASE("227", "lightcyan1")            ;  NEWNAME="lightcyan1"            ;  R=224  ;  G=255  ;  B=255
   CASE("228", "lightcyan2")            ;  NEWNAME="lightcyan2"            ;  R=209  ;  G=238  ;  B=238
   CASE("229", "lightcyan3")            ;  NEWNAME="lightcyan3"            ;  R=180  ;  G=205  ;  B=205
   CASE("230", "lightcyan4")            ;  NEWNAME="lightcyan4"            ;  R=122  ;  G=139  ;  B=139
   CASE("231", "paleturquoise1")        ;  NEWNAME="paleturquoise1"        ;  R=187  ;  G=255  ;  B=255
   CASE("232", "paleturquoise2")        ;  NEWNAME="paleturquoise2"        ;  R=174  ;  G=238  ;  B=238
   CASE("233", "paleturquoise3")        ;  NEWNAME="paleturquoise3"        ;  R=150  ;  G=205  ;  B=205
   CASE("234", "paleturquoise4")        ;  NEWNAME="paleturquoise4"        ;  R=102  ;  G=139  ;  B=139
   CASE("235", "cadetblue1")            ;  NEWNAME="cadetblue1"            ;  R=152  ;  G=245  ;  B=255
   CASE("236", "cadetblue2")            ;  NEWNAME="cadetblue2"            ;  R=142  ;  G=229  ;  B=238
   CASE("237", "cadetblue3")            ;  NEWNAME="cadetblue3"            ;  R=122  ;  G=197  ;  B=205
   CASE("238", "cadetblue4")            ;  NEWNAME="cadetblue4"            ;  R=83   ;  G=134  ;  B=139
   CASE("239", "turquoise1")            ;  NEWNAME="turquoise1"            ;  R=0    ;  G=245  ;  B=255
   CASE("240", "turquoise2")            ;  NEWNAME="turquoise2"            ;  R=0    ;  G=229  ;  B=238
   CASE("241", "turquoise3")            ;  NEWNAME="turquoise3"            ;  R=0    ;  G=197  ;  B=205
   CASE("242", "turquoise4")            ;  NEWNAME="turquoise4"            ;  R=0    ;  G=134  ;  B=139
   CASE("243", "cyan1")                 ;  NEWNAME="cyan1"                 ;  R=0    ;  G=255  ;  B=255
   CASE("244", "cyan2")                 ;  NEWNAME="cyan2"                 ;  R=0    ;  G=238  ;  B=238
   CASE("245", "cyan3")                 ;  NEWNAME="cyan3"                 ;  R=0    ;  G=205  ;  B=205
   CASE("246", "cyan4")                 ;  NEWNAME="cyan4"                 ;  R=0    ;  G=139  ;  B=139
   CASE("247", "darkslategray1")        ;  NEWNAME="darkslategray1"        ;  R=151  ;  G=255  ;  B=255
   CASE("248", "darkslategray2")        ;  NEWNAME="darkslategray2"        ;  R=141  ;  G=238  ;  B=238
   CASE("249", "darkslategray3")        ;  NEWNAME="darkslategray3"        ;  R=121  ;  G=205  ;  B=205
   CASE("250", "darkslategray4")        ;  NEWNAME="darkslategray4"        ;  R=82   ;  G=139  ;  B=139
   CASE("251", "aquamarine1")           ;  NEWNAME="aquamarine1"           ;  R=127  ;  G=255  ;  B=212
   CASE("252", "aquamarine2")           ;  NEWNAME="aquamarine2"           ;  R=118  ;  G=238  ;  B=198
   CASE("253", "aquamarine3")           ;  NEWNAME="aquamarine3"           ;  R=102  ;  G=205  ;  B=170
   CASE("254", "aquamarine4")           ;  NEWNAME="aquamarine4"           ;  R=69   ;  G=139  ;  B=116
   CASE("255", "darkseagreen1")         ;  NEWNAME="darkseagreen1"         ;  R=193  ;  G=255  ;  B=193
   CASE("256", "darkseagreen2")         ;  NEWNAME="darkseagreen2"         ;  R=180  ;  G=238  ;  B=180
   CASE("257", "darkseagreen3")         ;  NEWNAME="darkseagreen3"         ;  R=155  ;  G=205  ;  B=155
   CASE("258", "darkseagreen4")         ;  NEWNAME="darkseagreen4"         ;  R=105  ;  G=139  ;  B=105
   CASE("259", "seagreen1")             ;  NEWNAME="seagreen1"             ;  R=84   ;  G=255  ;  B=159
   CASE("260", "seagreen2")             ;  NEWNAME="seagreen2"             ;  R=78   ;  G=238  ;  B=148
   CASE("261", "seagreen3")             ;  NEWNAME="seagreen3"             ;  R=67   ;  G=205  ;  B=128
   CASE("262", "seagreen4")             ;  NEWNAME="seagreen4"             ;  R=46   ;  G=139  ;  B=87
   CASE("263", "palegreen1")            ;  NEWNAME="palegreen1"            ;  R=154  ;  G=255  ;  B=154
   CASE("264", "palegreen2")            ;  NEWNAME="palegreen2"            ;  R=144  ;  G=238  ;  B=144
   CASE("265", "palegreen3")            ;  NEWNAME="palegreen3"            ;  R=124  ;  G=205  ;  B=124
   CASE("266", "palegreen4")            ;  NEWNAME="palegreen4"            ;  R=84   ;  G=139  ;  B=84
   CASE("267", "springgreen1")          ;  NEWNAME="springgreen1"          ;  R=0    ;  G=255  ;  B=127
   CASE("268", "springgreen2")          ;  NEWNAME="springgreen2"          ;  R=0    ;  G=238  ;  B=118
   CASE("269", "springgreen3")          ;  NEWNAME="springgreen3"          ;  R=0    ;  G=205  ;  B=102
   CASE("270", "springgreen4")          ;  NEWNAME="springgreen4"          ;  R=0    ;  G=139  ;  B=69
   CASE("271", "green1")                ;  NEWNAME="green1"                ;  R=0    ;  G=255  ;  B=0
   CASE("272", "green2")                ;  NEWNAME="green2"                ;  R=0    ;  G=238  ;  B=0
   CASE("273", "green3")                ;  NEWNAME="green3"                ;  R=0    ;  G=205  ;  B=0
   CASE("274", "green4")                ;  NEWNAME="green4"                ;  R=0    ;  G=139  ;  B=0
   CASE("275", "chartreuse1")           ;  NEWNAME="chartreuse1"           ;  R=127  ;  G=255  ;  B=0
   CASE("276", "chartreuse2")           ;  NEWNAME="chartreuse2"           ;  R=118  ;  G=238  ;  B=0
   CASE("277", "chartreuse3")           ;  NEWNAME="chartreuse3"           ;  R=102  ;  G=205  ;  B=0
   CASE("278", "chartreuse4")           ;  NEWNAME="chartreuse4"           ;  R=69   ;  G=139  ;  B=0
   CASE("279", "olivedrab1")            ;  NEWNAME="olivedrab1"            ;  R=192  ;  G=255  ;  B=62
   CASE("280", "olivedrab2")            ;  NEWNAME="olivedrab2"            ;  R=179  ;  G=238  ;  B=58
   CASE("281", "olivedrab3")            ;  NEWNAME="olivedrab3"            ;  R=154  ;  G=205  ;  B=50
   CASE("282", "olivedrab4")            ;  NEWNAME="olivedrab4"            ;  R=105  ;  G=139  ;  B=34
   CASE("283", "darkolivegreen1")       ;  NEWNAME="darkolivegreen1"       ;  R=202  ;  G=255  ;  B=112
   CASE("284", "darkolivegreen2")       ;  NEWNAME="darkolivegreen2"       ;  R=188  ;  G=238  ;  B=104
   CASE("285", "darkolivegreen3")       ;  NEWNAME="darkolivegreen3"       ;  R=162  ;  G=205  ;  B=90
   CASE("286", "darkolivegreen4")       ;  NEWNAME="darkolivegreen4"       ;  R=110  ;  G=139  ;  B=61
   CASE("287", "khaki1")                ;  NEWNAME="khaki1"                ;  R=255  ;  G=246  ;  B=143
   CASE("288", "khaki2")                ;  NEWNAME="khaki2"                ;  R=238  ;  G=230  ;  B=133
   CASE("289", "khaki3")                ;  NEWNAME="khaki3"                ;  R=205  ;  G=198  ;  B=115
   CASE("290", "khaki4")                ;  NEWNAME="khaki4"                ;  R=139  ;  G=134  ;  B=78
   CASE("291", "lightgoldenrod1")       ;  NEWNAME="lightgoldenrod1"       ;  R=255  ;  G=236  ;  B=139
   CASE("292", "lightgoldenrod2")       ;  NEWNAME="lightgoldenrod2"       ;  R=238  ;  G=220  ;  B=130
   CASE("293", "lightgoldenrod3")       ;  NEWNAME="lightgoldenrod3"       ;  R=205  ;  G=190  ;  B=112
   CASE("294", "lightgoldenrod4")       ;  NEWNAME="lightgoldenrod4"       ;  R=139  ;  G=129  ;  B=76
   CASE("295", "lightyellow1")          ;  NEWNAME="lightyellow1"          ;  R=255  ;  G=255  ;  B=224
   CASE("296", "lightyellow2")          ;  NEWNAME="lightyellow2"          ;  R=238  ;  G=238  ;  B=209
   CASE("297", "lightyellow3")          ;  NEWNAME="lightyellow3"          ;  R=205  ;  G=205  ;  B=180
   CASE("298", "lightyellow4")          ;  NEWNAME="lightyellow4"          ;  R=139  ;  G=139  ;  B=122
   CASE("299", "yellow1")               ;  NEWNAME="yellow1"               ;  R=255  ;  G=255  ;  B=0
   CASE("300", "yellow2")               ;  NEWNAME="yellow2"               ;  R=238  ;  G=238  ;  B=0
   CASE("301", "yellow3")               ;  NEWNAME="yellow3"               ;  R=205  ;  G=205  ;  B=0
   CASE("302", "yellow4")               ;  NEWNAME="yellow4"               ;  R=139  ;  G=139  ;  B=0
   CASE("303", "gold1")                 ;  NEWNAME="gold1"                 ;  R=255  ;  G=215  ;  B=0
   CASE("304", "gold2")                 ;  NEWNAME="gold2"                 ;  R=238  ;  G=201  ;  B=0
   CASE("305", "gold3")                 ;  NEWNAME="gold3"                 ;  R=205  ;  G=173  ;  B=0
   CASE("306", "gold4")                 ;  NEWNAME="gold4"                 ;  R=139  ;  G=117  ;  B=0
   CASE("307", "goldenrod1")            ;  NEWNAME="goldenrod1"            ;  R=255  ;  G=193  ;  B=37
   CASE("308", "goldenrod2")            ;  NEWNAME="goldenrod2"            ;  R=238  ;  G=180  ;  B=34
   CASE("309", "goldenrod3")            ;  NEWNAME="goldenrod3"            ;  R=205  ;  G=155  ;  B=29
   CASE("310", "goldenrod4")            ;  NEWNAME="goldenrod4"            ;  R=139  ;  G=105  ;  B=20
   CASE("311", "darkgoldenrod1")        ;  NEWNAME="darkgoldenrod1"        ;  R=255  ;  G=185  ;  B=15
   CASE("312", "darkgoldenrod2")        ;  NEWNAME="darkgoldenrod2"        ;  R=238  ;  G=173  ;  B=14
   CASE("313", "darkgoldenrod3")        ;  NEWNAME="darkgoldenrod3"        ;  R=205  ;  G=149  ;  B=12
   CASE("314", "darkgoldenrod4")        ;  NEWNAME="darkgoldenrod4"        ;  R=139  ;  G=101  ;  B=8
   CASE("315", "rosybrown1")            ;  NEWNAME="rosybrown1"            ;  R=255  ;  G=193  ;  B=193
   CASE("316", "rosybrown2")            ;  NEWNAME="rosybrown2"            ;  R=238  ;  G=180  ;  B=180
   CASE("317", "rosybrown3")            ;  NEWNAME="rosybrown3"            ;  R=205  ;  G=155  ;  B=155
   CASE("318", "rosybrown4")            ;  NEWNAME="rosybrown4"            ;  R=139  ;  G=105  ;  B=105
   CASE("319", "indianred1")            ;  NEWNAME="indianred1"            ;  R=255  ;  G=106  ;  B=106
   CASE("320", "indianred2")            ;  NEWNAME="indianred2"            ;  R=238  ;  G=99   ;  B=99
   CASE("321", "indianred3")            ;  NEWNAME="indianred3"            ;  R=205  ;  G=85   ;  B=85
   CASE("322", "indianred4")            ;  NEWNAME="indianred4"            ;  R=139  ;  G=58   ;  B=58
   CASE("323", "sienna1")               ;  NEWNAME="sienna1"               ;  R=255  ;  G=130  ;  B=71
   CASE("324", "sienna2")               ;  NEWNAME="sienna2"               ;  R=238  ;  G=121  ;  B=66
   CASE("325", "sienna3")               ;  NEWNAME="sienna3"               ;  R=205  ;  G=104  ;  B=57
   CASE("326", "sienna4")               ;  NEWNAME="sienna4"               ;  R=139  ;  G=71   ;  B=38
   CASE("327", "burlywood1")            ;  NEWNAME="burlywood1"            ;  R=255  ;  G=211  ;  B=155
   CASE("328", "burlywood2")            ;  NEWNAME="burlywood2"            ;  R=238  ;  G=197  ;  B=145
   CASE("329", "burlywood3")            ;  NEWNAME="burlywood3"            ;  R=205  ;  G=170  ;  B=125
   CASE("330", "burlywood4")            ;  NEWNAME="burlywood4"            ;  R=139  ;  G=115  ;  B=85
   CASE("331", "wheat1")                ;  NEWNAME="wheat1"                ;  R=255  ;  G=231  ;  B=186
   CASE("332", "wheat2")                ;  NEWNAME="wheat2"                ;  R=238  ;  G=216  ;  B=174
   CASE("333", "wheat3")                ;  NEWNAME="wheat3"                ;  R=205  ;  G=186  ;  B=150
   CASE("334", "wheat4")                ;  NEWNAME="wheat4"                ;  R=139  ;  G=126  ;  B=102
   CASE("335", "tan1")                  ;  NEWNAME="tan1"                  ;  R=255  ;  G=165  ;  B=79
   CASE("336", "tan2")                  ;  NEWNAME="tan2"                  ;  R=238  ;  G=154  ;  B=73
   CASE("337", "tan3")                  ;  NEWNAME="tan3"                  ;  R=205  ;  G=133  ;  B=63
   CASE("338", "tan4")                  ;  NEWNAME="tan4"                  ;  R=139  ;  G=90   ;  B=43
   CASE("339", "chocolate1")            ;  NEWNAME="chocolate1"            ;  R=255  ;  G=127  ;  B=36
   CASE("340", "chocolate2")            ;  NEWNAME="chocolate2"            ;  R=238  ;  G=118  ;  B=33
   CASE("341", "chocolate3")            ;  NEWNAME="chocolate3"            ;  R=205  ;  G=102  ;  B=29
   CASE("342", "chocolate4")            ;  NEWNAME="chocolate4"            ;  R=139  ;  G=69   ;  B=19
   CASE("343", "firebrick1")            ;  NEWNAME="firebrick1"            ;  R=255  ;  G=48   ;  B=48
   CASE("344", "firebrick2")            ;  NEWNAME="firebrick2"            ;  R=238  ;  G=44   ;  B=44
   CASE("345", "firebrick3")            ;  NEWNAME="firebrick3"            ;  R=205  ;  G=38   ;  B=38
   CASE("346", "firebrick4")            ;  NEWNAME="firebrick4"            ;  R=139  ;  G=26   ;  B=26
   CASE("347", "brown1")                ;  NEWNAME="brown1"                ;  R=255  ;  G=64   ;  B=64
   CASE("348", "brown2")                ;  NEWNAME="brown2"                ;  R=238  ;  G=59   ;  B=59
   CASE("349", "brown3")                ;  NEWNAME="brown3"                ;  R=205  ;  G=51   ;  B=51
   CASE("350", "brown4")                ;  NEWNAME="brown4"                ;  R=139  ;  G=35   ;  B=35
   CASE("351", "salmon1")               ;  NEWNAME="salmon1"               ;  R=255  ;  G=140  ;  B=105
   CASE("352", "salmon2")               ;  NEWNAME="salmon2"               ;  R=238  ;  G=130  ;  B=98
   CASE("353", "salmon3")               ;  NEWNAME="salmon3"               ;  R=205  ;  G=112  ;  B=84
   CASE("354", "salmon4")               ;  NEWNAME="salmon4"               ;  R=139  ;  G=76   ;  B=57
   CASE("355", "lightsalmon1")          ;  NEWNAME="lightsalmon1"          ;  R=255  ;  G=160  ;  B=122
   CASE("356", "lightsalmon2")          ;  NEWNAME="lightsalmon2"          ;  R=238  ;  G=149  ;  B=114
   CASE("357", "lightsalmon3")          ;  NEWNAME="lightsalmon3"          ;  R=205  ;  G=129  ;  B=98
   CASE("358", "lightsalmon4")          ;  NEWNAME="lightsalmon4"          ;  R=139  ;  G=87   ;  B=66
   CASE("359", "orange1")               ;  NEWNAME="orange1"               ;  R=255  ;  G=165  ;  B=0
   CASE("360", "orange2")               ;  NEWNAME="orange2"               ;  R=238  ;  G=154  ;  B=0
   CASE("361", "orange3")               ;  NEWNAME="orange3"               ;  R=205  ;  G=133  ;  B=0
   CASE("362", "orange4")               ;  NEWNAME="orange4"               ;  R=139  ;  G=90   ;  B=0
   CASE("363", "darkorange1")           ;  NEWNAME="darkorange1"           ;  R=255  ;  G=127  ;  B=0
   CASE("364", "darkorange2")           ;  NEWNAME="darkorange2"           ;  R=238  ;  G=118  ;  B=0
   CASE("365", "darkorange3")           ;  NEWNAME="darkorange3"           ;  R=205  ;  G=102  ;  B=0
   CASE("366", "darkorange4")           ;  NEWNAME="darkorange4"           ;  R=139  ;  G=69   ;  B=0
   CASE("367", "coral1")                ;  NEWNAME="coral1"                ;  R=255  ;  G=114  ;  B=86
   CASE("368", "coral2")                ;  NEWNAME="coral2"                ;  R=238  ;  G=106  ;  B=80
   CASE("369", "coral3")                ;  NEWNAME="coral3"                ;  R=205  ;  G=91   ;  B=69
   CASE("370", "coral4")                ;  NEWNAME="coral4"                ;  R=139  ;  G=62   ;  B=47
   CASE("371", "tomato1")               ;  NEWNAME="tomato1"               ;  R=255  ;  G=99   ;  B=71
   CASE("372", "tomato2")               ;  NEWNAME="tomato2"               ;  R=238  ;  G=92   ;  B=66
   CASE("373", "tomato3")               ;  NEWNAME="tomato3"               ;  R=205  ;  G=79   ;  B=57
   CASE("374", "tomato4")               ;  NEWNAME="tomato4"               ;  R=139  ;  G=54   ;  B=38
   CASE("375", "orangered1")            ;  NEWNAME="orangered1"            ;  R=255  ;  G=69   ;  B=0
   CASE("376", "orangered2")            ;  NEWNAME="orangered2"            ;  R=238  ;  G=64   ;  B=0
   CASE("377", "orangered3")            ;  NEWNAME="orangered3"            ;  R=205  ;  G=55   ;  B=0
   CASE("378", "orangered4")            ;  NEWNAME="orangered4"            ;  R=139  ;  G=37   ;  B=0
   CASE("379", "red1")                  ;  NEWNAME="red1"                  ;  R=255  ;  G=0    ;  B=0
   CASE("380", "red2")                  ;  NEWNAME="red2"                  ;  R=238  ;  G=0    ;  B=0
   CASE("381", "red3")                  ;  NEWNAME="red3"                  ;  R=205  ;  G=0    ;  B=0
   CASE("382", "red4")                  ;  NEWNAME="red4"                  ;  R=139  ;  G=0    ;  B=0
   CASE("112", "hotpink")               ;  NEWNAME="hotpink"               ;  R=255  ;  G=105  ;  B=180
   CASE("113", "deeppink")              ;  NEWNAME="deeppink"              ;  R=255  ;  G=20   ;  B=147
   CASE("115", "lightpink")             ;  NEWNAME="lightpink"             ;  R=255  ;  G=182  ;  B=193
   CASE("383", "deeppink1")             ;  NEWNAME="deeppink1"             ;  R=255  ;  G=20   ;  B=147
   CASE("384", "deeppink2")             ;  NEWNAME="deeppink2"             ;  R=238  ;  G=18   ;  B=137
   CASE("385", "deeppink3")             ;  NEWNAME="deeppink3"             ;  R=205  ;  G=16   ;  B=118
   CASE("386", "deeppink4")             ;  NEWNAME="deeppink4"             ;  R=139  ;  G=10   ;  B=80
   CASE("387", "hotpink1")              ;  NEWNAME="hotpink1"              ;  R=255  ;  G=110  ;  B=180
   CASE("388", "hotpink2")              ;  NEWNAME="hotpink2"              ;  R=238  ;  G=106  ;  B=167
   CASE("389", "hotpink3")              ;  NEWNAME="hotpink3"              ;  R=205  ;  G=96   ;  B=144
   CASE("390", "hotpink4")              ;  NEWNAME="hotpink4"              ;  R=139  ;  G=58   ;  B=98
   CASE("114", "pink")                  ;  NEWNAME="pink"                  ;  R=255  ;  G=192  ;  B=203
   CASE("391", "pink1")                 ;  NEWNAME="pink1"                 ;  R=255  ;  G=181  ;  B=197
   CASE("392", "pink2")                 ;  NEWNAME="pink2"                 ;  R=238  ;  G=169  ;  B=184
   CASE("393", "pink3")                 ;  NEWNAME="pink3"                 ;  R=205  ;  G=145  ;  B=158
   CASE("394", "pink4")                 ;  NEWNAME="pink4"                 ;  R=139  ;  G=99   ;  B=108
   CASE("395", "lightpink1")            ;  NEWNAME="lightpink1"            ;  R=255  ;  G=174  ;  B=185
   CASE("396", "lightpink2")            ;  NEWNAME="lightpink2"            ;  R=238  ;  G=162  ;  B=173
   CASE("397", "lightpink3")            ;  NEWNAME="lightpink3"            ;  R=205  ;  G=140  ;  B=149
   CASE("398", "lightpink4")            ;  NEWNAME="lightpink4"            ;  R=139  ;  G=95   ;  B=101
   CASE("399", "palevioletred1")        ;  NEWNAME="palevioletred1"        ;  R=255  ;  G=130  ;  B=171
   CASE("400", "palevioletred2")        ;  NEWNAME="palevioletred2"        ;  R=238  ;  G=121  ;  B=159
   CASE("401", "palevioletred3")        ;  NEWNAME="palevioletred3"        ;  R=205  ;  G=104  ;  B=137
   CASE("402", "palevioletred4")        ;  NEWNAME="palevioletred4"        ;  R=139  ;  G=71   ;  B=93
   CASE("403", "maroon1")               ;  NEWNAME="maroon1"               ;  R=255  ;  G=52   ;  B=179
   CASE("404", "maroon2")               ;  NEWNAME="maroon2"               ;  R=238  ;  G=48   ;  B=167
   CASE("405", "maroon3")               ;  NEWNAME="maroon3"               ;  R=205  ;  G=41   ;  B=144
   CASE("406", "maroon4")               ;  NEWNAME="maroon4"               ;  R=139  ;  G=28   ;  B=98
   CASE("407", "violetred1")            ;  NEWNAME="violetred1"            ;  R=255  ;  G=62   ;  B=150
   CASE("408", "violetred2")            ;  NEWNAME="violetred2"            ;  R=238  ;  G=58   ;  B=140
   CASE("409", "violetred3")            ;  NEWNAME="violetred3"            ;  R=205  ;  G=50   ;  B=120
   CASE("410", "violetred4")            ;  NEWNAME="violetred4"            ;  R=139  ;  G=34   ;  B=82
   CASE("411", "magenta1")              ;  NEWNAME="magenta1"              ;  R=255  ;  G=0    ;  B=255
   CASE("412", "magenta2")              ;  NEWNAME="magenta2"              ;  R=238  ;  G=0    ;  B=238
   CASE("413", "magenta3")              ;  NEWNAME="magenta3"              ;  R=205  ;  G=0    ;  B=205
   CASE("414", "magenta4")              ;  NEWNAME="magenta4"              ;  R=139  ;  G=0    ;  B=139
   CASE("415", "orchid1")               ;  NEWNAME="orchid1"               ;  R=255  ;  G=131  ;  B=250
   CASE("416", "orchid2")               ;  NEWNAME="orchid2"               ;  R=238  ;  G=122  ;  B=233
   CASE("417", "orchid3")               ;  NEWNAME="orchid3"               ;  R=205  ;  G=105  ;  B=201
   CASE("418", "orchid4")               ;  NEWNAME="orchid4"               ;  R=139  ;  G=71   ;  B=137
   CASE("419", "plum1")                 ;  NEWNAME="plum1"                 ;  R=255  ;  G=187  ;  B=255
   CASE("420", "plum2")                 ;  NEWNAME="plum2"                 ;  R=238  ;  G=174  ;  B=238
   CASE("421", "plum3")                 ;  NEWNAME="plum3"                 ;  R=205  ;  G=150  ;  B=205
   CASE("422", "plum4")                 ;  NEWNAME="plum4"                 ;  R=139  ;  G=102  ;  B=139
   CASE("423", "mediumorchid1")         ;  NEWNAME="mediumorchid1"         ;  R=224  ;  G=102  ;  B=255
   CASE("424", "mediumorchid2")         ;  NEWNAME="mediumorchid2"         ;  R=209  ;  G=95   ;  B=238
   CASE("425", "mediumorchid3")         ;  NEWNAME="mediumorchid3"         ;  R=180  ;  G=82   ;  B=205
   CASE("426", "mediumorchid4")         ;  NEWNAME="mediumorchid4"         ;  R=122  ;  G=55   ;  B=139
   CASE("427", "darkorchid1")           ;  NEWNAME="darkorchid1"           ;  R=191  ;  G=62   ;  B=255
   CASE("428", "darkorchid2")           ;  NEWNAME="darkorchid2"           ;  R=178  ;  G=58   ;  B=238
   CASE("429", "darkorchid3")           ;  NEWNAME="darkorchid3"           ;  R=154  ;  G=50   ;  B=205
   CASE("430", "darkorchid4")           ;  NEWNAME="darkorchid4"           ;  R=104  ;  G=34   ;  B=139
   CASE("431", "purple1")               ;  NEWNAME="purple1"               ;  R=155  ;  G=48   ;  B=255
   CASE("432", "purple2")               ;  NEWNAME="purple2"               ;  R=145  ;  G=44   ;  B=238
   CASE("433", "purple3")               ;  NEWNAME="purple3"               ;  R=125  ;  G=38   ;  B=205
   CASE("434", "purple4")               ;  NEWNAME="purple4"               ;  R=85   ;  G=26   ;  B=139
   CASE("435", "mediumpurple1")         ;  NEWNAME="mediumpurple1"         ;  R=171  ;  G=130  ;  B=255
   CASE("436", "mediumpurple2")         ;  NEWNAME="mediumpurple2"         ;  R=159  ;  G=121  ;  B=238
   CASE("437", "mediumpurple3")         ;  NEWNAME="mediumpurple3"         ;  R=137  ;  G=104  ;  B=205
   CASE("438", "mediumpurple4")         ;  NEWNAME="mediumpurple4"         ;  R=93   ;  G=71   ;  B=139
   CASE("439", "thistle1")              ;  NEWNAME="thistle1"              ;  R=255  ;  G=225  ;  B=255
   CASE("440", "thistle2")              ;  NEWNAME="thistle2"              ;  R=238  ;  G=210  ;  B=238
   CASE("441", "thistle3")              ;  NEWNAME="thistle3"              ;  R=205  ;  G=181  ;  B=205
   CASE("442", "thistle4")              ;  NEWNAME="thistle4"              ;  R=139  ;  G=123  ;  B=139
   CASE("443", "gray0")                 ;  NEWNAME="gray0"                 ;  R=0    ;  G=0    ;  B=0
   CASE("444", "gray1")                 ;  NEWNAME="gray1"                 ;  R=3    ;  G=3    ;  B=3
   CASE("445", "gray2")                 ;  NEWNAME="gray2"                 ;  R=5    ;  G=5    ;  B=5
   CASE("446", "gray3")                 ;  NEWNAME="gray3"                 ;  R=8    ;  G=8    ;  B=8
   CASE("447", "gray4")                 ;  NEWNAME="gray4"                 ;  R=10   ;  G=10   ;  B=10
   CASE("448", "gray5")                 ;  NEWNAME="gray5"                 ;  R=13   ;  G=13   ;  B=13
   CASE("449", "gray6")                 ;  NEWNAME="gray6"                 ;  R=15   ;  G=15   ;  B=15
   CASE("450", "gray7")                 ;  NEWNAME="gray7"                 ;  R=18   ;  G=18   ;  B=18
   CASE("451", "gray8")                 ;  NEWNAME="gray8"                 ;  R=20   ;  G=20   ;  B=20
   CASE("452", "gray9")                 ;  NEWNAME="gray9"                 ;  R=23   ;  G=23   ;  B=23
   CASE("453", "gray10")                ;  NEWNAME="gray10"                ;  R=26   ;  G=26   ;  B=26
   CASE("454", "gray11")                ;  NEWNAME="gray11"                ;  R=28   ;  G=28   ;  B=28
   CASE("455", "gray12")                ;  NEWNAME="gray12"                ;  R=31   ;  G=31   ;  B=31
   CASE("456", "gray13")                ;  NEWNAME="gray13"                ;  R=33   ;  G=33   ;  B=33
   CASE("457", "gray14")                ;  NEWNAME="gray14"                ;  R=36   ;  G=36   ;  B=36
   CASE("458", "gray15")                ;  NEWNAME="gray15"                ;  R=38   ;  G=38   ;  B=38
   CASE("459", "gray16")                ;  NEWNAME="gray16"                ;  R=41   ;  G=41   ;  B=41
   CASE("460", "gray17")                ;  NEWNAME="gray17"                ;  R=43   ;  G=43   ;  B=43
   CASE("461", "gray18")                ;  NEWNAME="gray18"                ;  R=46   ;  G=46   ;  B=46
   CASE("462", "gray19")                ;  NEWNAME="gray19"                ;  R=48   ;  G=48   ;  B=48
   CASE("463", "gray20")                ;  NEWNAME="gray20"                ;  R=51   ;  G=51   ;  B=51
   CASE("464", "gray21")                ;  NEWNAME="gray21"                ;  R=54   ;  G=54   ;  B=54
   CASE("465", "gray22")                ;  NEWNAME="gray22"                ;  R=56   ;  G=56   ;  B=56
   CASE("466", "gray23")                ;  NEWNAME="gray23"                ;  R=59   ;  G=59   ;  B=59
   CASE("467", "gray24")                ;  NEWNAME="gray24"                ;  R=61   ;  G=61   ;  B=61
   CASE("468", "gray25")                ;  NEWNAME="gray25"                ;  R=64   ;  G=64   ;  B=64
   CASE("469", "gray26")                ;  NEWNAME="gray26"                ;  R=66   ;  G=66   ;  B=66
   CASE("470", "gray27")                ;  NEWNAME="gray27"                ;  R=69   ;  G=69   ;  B=69
   CASE("471", "gray28")                ;  NEWNAME="gray28"                ;  R=71   ;  G=71   ;  B=71
   CASE("472", "gray29")                ;  NEWNAME="gray29"                ;  R=74   ;  G=74   ;  B=74
   CASE("473", "gray30")                ;  NEWNAME="gray30"                ;  R=77   ;  G=77   ;  B=77
   CASE("474", "gray31")                ;  NEWNAME="gray31"                ;  R=79   ;  G=79   ;  B=79
   CASE("475", "gray32")                ;  NEWNAME="gray32"                ;  R=82   ;  G=82   ;  B=82
   CASE("476", "gray33")                ;  NEWNAME="gray33"                ;  R=84   ;  G=84   ;  B=84
   CASE("477", "gray34")                ;  NEWNAME="gray34"                ;  R=87   ;  G=87   ;  B=87
   CASE("478", "gray35")                ;  NEWNAME="gray35"                ;  R=89   ;  G=89   ;  B=89
   CASE("479", "gray36")                ;  NEWNAME="gray36"                ;  R=92   ;  G=92   ;  B=92
   CASE("480", "gray37")                ;  NEWNAME="gray37"                ;  R=94   ;  G=94   ;  B=94
   CASE("481", "gray38")                ;  NEWNAME="gray38"                ;  R=97   ;  G=97   ;  B=97
   CASE("482", "gray39")                ;  NEWNAME="gray39"                ;  R=99   ;  G=99   ;  B=99
   CASE("483", "gray40")                ;  NEWNAME="gray40"                ;  R=102  ;  G=102  ;  B=102
   CASE("484", "gray41")                ;  NEWNAME="gray41"                ;  R=105  ;  G=105  ;  B=105
   CASE("485", "gray42")                ;  NEWNAME="gray42"                ;  R=107  ;  G=107  ;  B=107
   CASE("486", "gray43")                ;  NEWNAME="gray43"                ;  R=110  ;  G=110  ;  B=110
   CASE("487", "gray44")                ;  NEWNAME="gray44"                ;  R=112  ;  G=112  ;  B=112
   CASE("488", "gray45")                ;  NEWNAME="gray45"                ;  R=115  ;  G=115  ;  B=115
   CASE("489", "gray46")                ;  NEWNAME="gray46"                ;  R=117  ;  G=117  ;  B=117
   CASE("490", "gray47")                ;  NEWNAME="gray47"                ;  R=120  ;  G=120  ;  B=120
   CASE("491", "gray48")                ;  NEWNAME="gray48"                ;  R=122  ;  G=122  ;  B=122
   CASE("492", "gray49")                ;  NEWNAME="gray49"                ;  R=125  ;  G=125  ;  B=125
   CASE("493", "gray50")                ;  NEWNAME="gray50"                ;  R=127  ;  G=127  ;  B=127
   CASE("494", "gray51")                ;  NEWNAME="gray51"                ;  R=130  ;  G=130  ;  B=130
   CASE("495", "gray52")                ;  NEWNAME="gray52"                ;  R=133  ;  G=133  ;  B=133
   CASE("496", "gray53")                ;  NEWNAME="gray53"                ;  R=135  ;  G=135  ;  B=135
   CASE("497", "gray54")                ;  NEWNAME="gray54"                ;  R=138  ;  G=138  ;  B=138
   CASE("498", "gray55")                ;  NEWNAME="gray55"                ;  R=140  ;  G=140  ;  B=140
   CASE("499", "gray56")                ;  NEWNAME="gray56"                ;  R=143  ;  G=143  ;  B=143
   CASE("500", "gray57")                ;  NEWNAME="gray57"                ;  R=145  ;  G=145  ;  B=145
   CASE("501", "gray58")                ;  NEWNAME="gray58"                ;  R=148  ;  G=148  ;  B=148
   CASE("502", "gray59")                ;  NEWNAME="gray59"                ;  R=150  ;  G=150  ;  B=150
   CASE("503", "gray60")                ;  NEWNAME="gray60"                ;  R=153  ;  G=153  ;  B=153
   CASE("504", "gray61")                ;  NEWNAME="gray61"                ;  R=156  ;  G=156  ;  B=156
   CASE("505", "gray62")                ;  NEWNAME="gray62"                ;  R=158  ;  G=158  ;  B=158
   CASE("506", "gray63")                ;  NEWNAME="gray63"                ;  R=161  ;  G=161  ;  B=161
   CASE("507", "gray64")                ;  NEWNAME="gray64"                ;  R=163  ;  G=163  ;  B=163
   CASE("508", "gray65")                ;  NEWNAME="gray65"                ;  R=166  ;  G=166  ;  B=166
   CASE("509", "gray66")                ;  NEWNAME="gray66"                ;  R=168  ;  G=168  ;  B=168
   CASE("510", "gray67")                ;  NEWNAME="gray67"                ;  R=171  ;  G=171  ;  B=171
   CASE("511", "gray68")                ;  NEWNAME="gray68"                ;  R=173  ;  G=173  ;  B=173
   CASE("512", "gray69")                ;  NEWNAME="gray69"                ;  R=176  ;  G=176  ;  B=176
   CASE("513", "gray70")                ;  NEWNAME="gray70"                ;  R=179  ;  G=179  ;  B=179
   CASE("514", "gray71")                ;  NEWNAME="gray71"                ;  R=181  ;  G=181  ;  B=181
   CASE("515", "gray72")                ;  NEWNAME="gray72"                ;  R=184  ;  G=184  ;  B=184
   CASE("516", "gray73")                ;  NEWNAME="gray73"                ;  R=186  ;  G=186  ;  B=186
   CASE("517", "gray74")                ;  NEWNAME="gray74"                ;  R=189  ;  G=189  ;  B=189
   CASE("518", "gray75")                ;  NEWNAME="gray75"                ;  R=191  ;  G=191  ;  B=191
   CASE("519", "gray76")                ;  NEWNAME="gray76"                ;  R=194  ;  G=194  ;  B=194
   CASE("520", "gray77")                ;  NEWNAME="gray77"                ;  R=196  ;  G=196  ;  B=196
   CASE("521", "gray78")                ;  NEWNAME="gray78"                ;  R=199  ;  G=199  ;  B=199
   CASE("522", "gray79")                ;  NEWNAME="gray79"                ;  R=201  ;  G=201  ;  B=201
   CASE("523", "gray80")                ;  NEWNAME="gray80"                ;  R=204  ;  G=204  ;  B=204
   CASE("524", "gray81")                ;  NEWNAME="gray81"                ;  R=207  ;  G=207  ;  B=207
   CASE("525", "gray82")                ;  NEWNAME="gray82"                ;  R=209  ;  G=209  ;  B=209
   CASE("526", "gray83")                ;  NEWNAME="gray83"                ;  R=212  ;  G=212  ;  B=212
   CASE("527", "gray84")                ;  NEWNAME="gray84"                ;  R=214  ;  G=214  ;  B=214
   CASE("528", "gray85")                ;  NEWNAME="gray85"                ;  R=217  ;  G=217  ;  B=217
   CASE("529", "gray86")                ;  NEWNAME="gray86"                ;  R=219  ;  G=219  ;  B=219
   CASE("530", "gray87")                ;  NEWNAME="gray87"                ;  R=222  ;  G=222  ;  B=222
   CASE("531", "gray88")                ;  NEWNAME="gray88"                ;  R=224  ;  G=224  ;  B=224
   CASE("532", "gray89")                ;  NEWNAME="gray89"                ;  R=227  ;  G=227  ;  B=227
   CASE("533", "gray90")                ;  NEWNAME="gray90"                ;  R=229  ;  G=229  ;  B=229
   CASE("534", "gray91")                ;  NEWNAME="gray91"                ;  R=232  ;  G=232  ;  B=232
   CASE("535", "gray92")                ;  NEWNAME="gray92"                ;  R=235  ;  G=235  ;  B=235
   CASE("536", "gray93")                ;  NEWNAME="gray93"                ;  R=237  ;  G=237  ;  B=237
   CASE("537", "gray94")                ;  NEWNAME="gray94"                ;  R=240  ;  G=240  ;  B=240
   CASE("538", "gray95")                ;  NEWNAME="gray95"                ;  R=242  ;  G=242  ;  B=242
   CASE("539", "gray96")                ;  NEWNAME="gray96"                ;  R=245  ;  G=245  ;  B=245
   CASE("540", "gray97")                ;  NEWNAME="gray97"                ;  R=247  ;  G=247  ;  B=247
   CASE("541", "gray98")                ;  NEWNAME="gray98"                ;  R=250  ;  G=250  ;  B=250
   CASE("542", "gray99")                ;  NEWNAME="gray99"                ;  R=252  ;  G=252  ;  B=252
   CASE("543", "gray100")               ;  NEWNAME="gray100"               ;  R=255  ;  G=255  ;  B=255
   CASE("544", "darkgray")              ;  NEWNAME="darkgray"              ;  R=169  ;  G=169  ;  B=169
   CASE("545", "darkblue")              ;  NEWNAME="darkblue"              ;  R=0    ;  G=0    ;  B=139
   CASE("546", "darkcyan")              ;  NEWNAME="darkcyan"              ;  R=0    ;  G=139  ;  B=139
   CASE("547", "darkmagenta")           ;  NEWNAME="darkmagenta"           ;  R=139  ;  G=0    ;  B=139
   CASE("548", "darkred")               ;  NEWNAME="darkred"               ;  R=139  ;  G=0    ;  B=0
   CASE("549", "lightgreen")            ;  NEWNAME="lightgreen"            ;  R=144  ;  G=238  ;  B=144
   CASE("550", "silver")                ;  NEWNAME="silver"                ;  R=192  ;  G=192  ;  B=192
   CASE("551", "teal")                  ;  NEWNAME="teal"                  ;  R=0    ;  G=128  ;  B=128
   CASE("552", "olive")                 ;  NEWNAME="olive"                 ;  R=128  ;  G=128  ;  B=0
   CASE("553", "lime")                  ;  NEWNAME="lime"                  ;  R=0    ;  G=255  ;  B=0
   CASE("554", "aqua")                  ;  NEWNAME="aqua"                  ;  R=0    ;  G=255  ;  B=255
   CASE("555", "fuchsia")               ;  NEWNAME="fuchsia"               ;  R=255  ;  G=0    ;  B=255

   CASE DEFAULT                         ;  NEWNAME="Unknown"               ;  R=255  ;  G=255  ;  B=255 ! unknOWN COLOR NAME

   END SELECT

   IF(PRESENT(ECHONAME)) THEN
      ECHONAME = NEWNAME
   ENDIF
   R=R/2.55; G=G/2.55; B=B/2.55 ! take values from range of 0 to 255 to 0 to 100
END SUBROUTINE COLOR_NAME2RGB
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
ELEMENTAL PURE FUNCTION LOWER(STR) RESULT (STRING)

! ident_73="@(#)M_strings::lower(3f): Changes a string to lowercase"

CHARACTER(*), INTENT(IN)     :: STR
CHARACTER(LEN(STR))          :: STRING
INTEGER                      :: I
   STRING = STR
   DO I = 1, LEN(STR)                                ! step thru each letter in the string in specified range
      SELECT CASE (STR(I:I))
      CASE ('A':'Z')
         STRING(I:I) = CHAR(IACHAR(STR(I:I))+32)     ! change letter to miniscule
      CASE DEFAULT
      END SELECT
   END DO
END FUNCTION LOWER
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     polar_to_cartesian(3f) - [M_pixel:TRIGONOMETRY] convert polar coordinates to Cartesian coordinates
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine polar_to_cartesian(radius,inclination,x,y)
!!
!!     real,intent(in) :: radius,inclination
!!     real,intent(out)  :: x,y
!!
!!##DESCRIPTION
!!     Convert polar coordinate <radius, inclination > with
!!     angles in radians to cartesian point <X,Y> using the formulas
!!
!!       x=radius*cos(inclination)
!!       y=radius*sin(inclination)
!!
!!##OPTIONS
!!    RADIUS       The radial distance from the origin (O) to the point (P)
!!    INCLINATION  The INCLINATION angle in radians between the inclination
!!                 reference direction (x-axis) and the orthogonal projection
!!                 of the line OP of the reference plane (x-y plane).
!!
!!##RESULTS
!!    X  The distance along the x-axis
!!    Y  The distance along the y-axis
!!
!!##EXAMPLES
!!
!!   examples of usage
!!
!!    program demo_polar_to_cartesian
!!    use M_pixel, only : polar_to_cartesian
!!    implicit none
!!    real    :: x,y
!!    real    :: r,i
!!    !!integer :: ios
!!
!!     !!INFINITE: do
!!     !!   write(*,advance='no')'Enter radius and inclination(in radians):'
!!     !!   read(*,*,iostat=ios) r, i
!!     !!   if(ios.ne.0)exit INFINITE
!!        call polar_to_cartesian(r,i,x,y)
!!        write(*,*)'x=',x,' y=',y,'radius=',r,'inclination=',i
!!     !!enddo INFINITE
!!    end program demo_polar_to_cartesian
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE POLAR_TO_CARTESIAN(RADIUS,INCLINATION,X,Y)
IMPLICIT NONE
! ident_74="@(#)M_pixel::polar_to_cartesian(3f): convert polar coordinates to cartesian coordinates"
REAL,INTENT(IN) :: RADIUS,INCLINATION
REAL,INTENT(OUT)  :: X,Y
   IF(RADIUS.EQ.0)THEN
      X=0.0
      Y=0.0
   ELSE
      X=RADIUS*COS(INCLINATION)
      Y=RADIUS*SIN(INCLINATION)
   ENDIF
END SUBROUTINE POLAR_TO_CARTESIAN
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2r(3f) - [M_pixel:TRIGONOMETRY] convert degrees to radians
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental real function d2r(degrees)
!!
!!     class(*),intent(in) :: radians
!!##DESCRIPTION
!!    Converts degrees to radians using the formula:
!!
!!     radians=real(degrees*acos(-1.0d0)/180.d0)
!!##OPTIONS
!!    degrees    any standard scalar value supported by anyscalar_to_real(3f).
!!               This includes REAL, INTEGER, DOUBLEPRECISION, ... .
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_d2r
!!    use M_pixel, only :  d2r
!!    implicit none
!!       write(*,*)'With REAL array input    ', &
!!        & d2r([0.0,45.0,90.0,135.0,180.0])
!!       write(*,*)'With INTEGER array input ', &
!!        & d2r([0,  45,  90,  135,  180  ])
!!       write(*,*)'With DOUBLEPRECISION     ', &
!!        & d2r(0.0d0),d2r(45.0d0),d2r(90.0d0),d2r(135.0d0),d2r(180.0d0)
!!    end program demo_d2r
!!
!!   Results
!!
!!    With REAL array input    0.00000 0.785398185 1.57079637
!!    2.35619450 3.14159274
!!    With INTEGER array input 0.00000 0.785398185 1.57079637
!!    2.35619450 3.14159274
!!    With DOUBLEPRECISION     0.00000 0.785398185 1.57079637
!!    2.35619450 3.14159274
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
ELEMENTAL REAL FUNCTION D2R_R(DEGREES)

! ident_75="@(#)M_pixel::d2r_r(3f): Convert degrees to radians"

DOUBLEPRECISION,PARAMETER :: RADIAN=57.2957795131D0 ! degrees
REAL,INTENT(IN)           :: DEGREES                ! input degrees to convert to radians
   D2R_R=DBLE(DEGREES)/RADIAN                       ! do the unit conversion
END FUNCTION D2R_R
!-----------------------------------------------------------------------------------------------------------------------------------
ELEMENTAL DOUBLEPRECISION FUNCTION D2R_D(DEGREES)

! ident_76="@(#)M_pixel::d2r_d(3f): Convert degrees to radians"

DOUBLEPRECISION,PARAMETER :: RADIAN=57.2957795131D0 ! degrees
DOUBLEPRECISION,INTENT(IN) :: DEGREES               ! input degrees to convert to radians
   D2R_D=DEGREES/RADIAN                             ! do the unit conversion
END FUNCTION D2R_D
!-----------------------------------------------------------------------------------------------------------------------------------
ELEMENTAL DOUBLEPRECISION FUNCTION D2R_I(IDEGREES)

! ident_77="@(#)M_pixel::d2r_i(3f): Convert degrees to radians"

DOUBLEPRECISION,PARAMETER :: RADIAN=57.2957795131D0 ! degrees
INTEGER,INTENT(IN) :: IDEGREES                      ! input degrees to convert to radians
   D2R_I=DBLE(IDEGREES)/RADIAN                      ! do the unit conversion
END FUNCTION D2R_I
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
END MODULE ModLib_Pixel
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
