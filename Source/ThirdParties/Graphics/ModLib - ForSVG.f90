!
! Copyright (c) 2011 Charles O'Neill
!
! Permission is hereby granted, free of charge, to any person
! obtaining a copy of this software and associated documentation
! files (the "Software"), to deal in the Software without
! restriction, including without limitation the rights to use,
! copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the
! Software is furnished to do so, subject to the following
! conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
! HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
! WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
! OTHER DEALINGS IN THE SOFTWARE.

MODULE ModLib_ForSVG
!
! Create SVG images with Fortran
!
!   Charles O'Neill 12 May 2011
!    charles.oneill@gmail.com
!    www.caselab.okstate.edu
!
! Useage:
!    1) call svgOpen(Name, CanvasLowerLeft,CanvasUpperRight, Resolution)
!    2) call svgHeader()
!    3) Place Objects
!      i) call svgLine([x,y], [x,y])
!     ii) call svgRectangle( [x,y], [x,y], [x,y], [x,y])
!    iii) call svgCircle([x,y], [x,y])
!    iii) call svgTriangle([x,y], [x,y], [x,y])
!    4) call svgFooter()
!
  IMPLICIT NONE
  INTEGER,PARAMETER :: SVGID = 245

  INTEGER,PRIVATE :: RESOLUTION
  REAL,PRIVATE :: CANVASSIZE ! x=y
  REAL,PRIVATE :: CANVASLOWLEFT(2),CANVASUPRIGHT(2)
CONTAINS

  SUBROUTINE SVGOPEN(NAME,CLL,CUP,RES)
    CHARACTER(LEN=*) :: NAME
    REAL :: CLL(2), CUP(2)
    INTEGER :: RES
    OPEN(SVGID, FILE=TRIM(NAME)//".svg", STATUS='unknown')
    WRITE(*,*) "Opening", TRIM(NAME)//".svg"
    CANVASLOWLEFT = CLL
    CANVASUPRIGHT = CUP
    CANVASSIZE = MAX( CUP(1)-CLL(1) , CUP(2)-CLL(2))
    RESOLUTION = RES
  END SUBROUTINE SVGOPEN

  FUNCTION CANVASX(X)
    REAL :: X
    REAL :: CANVASX
    CANVASX = (X-CANVASLOWLEFT(1))/CANVASSIZE*RESOLUTION
  END FUNCTION CANVASX

  FUNCTION CANVASY(Y)
    REAL :: Y
    REAL :: CANVASY
    CANVASY = (1.0-(Y-CANVASLOWLEFT(2))/CANVASSIZE)*RESOLUTION
  END FUNCTION CANVASY

  FUNCTION SCALER(R)
    REAL :: R
    REAL :: SCALER
    SCALER = R/CANVASSIZE*RESOLUTION
  END FUNCTION SCALER

  SUBROUTINE SVGHEADER()
    WRITE(SVGID,'(1X,A)') '<svg version="1.1" xmlns="http://www.w3.org/2000/svg"> '
  END SUBROUTINE

  SUBROUTINE SVGRECTANGLE(XA,XB,XC,XD)
    REAL :: XA(2), XB(2), XC(2), XD(2)
    CALL SVGLINE(XA,XB)
    CALL SVGLINE(XB,XC)
    CALL SVGLINE(XC,XD)
    CALL SVGLINE(XD,XA)
  END SUBROUTINE


  SUBROUTINE SVGTRIANGLE(XA,XB,XC)
    REAL :: XA(2), XB(2), XC(2)
    CALL SVGLINE(XA,XB)
    CALL SVGLINE(XB,XC)
    CALL SVGLINE(XC,XA)
  END SUBROUTINE

  SUBROUTINE SVGLINE(XA,XB)
    REAL :: XA(2), XB(2)
    CHARACTER(LEN=80) :: SA1,SA2, SB1,SB2
    WRITE(SA1,'( e15.8 )') CANVASX(XA(1))
    WRITE(SA2,'( e15.8 )') CANVASY(XA(2))
    WRITE(SB1,'( e15.8 )') CANVASX(XB(1))
    WRITE(SB2,'( e15.8 )') CANVASY(XB(2))
    WRITE(SVGID,'(1X,A)') '<line'
    WRITE(SVGID,'(1X,A,A,A)') 'x1="', TRIM(ADJUSTL(SA1)),'" '
    WRITE(SVGID,'(1X,A,A,A)') 'y1="', TRIM(ADJUSTL(SA2)),'" '
    WRITE(SVGID,'(1X,A,A,A)') 'x2="', TRIM(ADJUSTL(SB1)),'" '
    WRITE(SVGID,'(1X,A,A,A)') 'y2="', TRIM(ADJUSTL(SB2)),'" '
    WRITE(SVGID,'(1X,A,A,A,A)') 'style="stroke:#000000" />'
  END SUBROUTINE

  SUBROUTINE SVGCIRCLE(X,R)
    REAL :: X(2), R
    CHARACTER(LEN=80) :: STRINGA,STRINGB,STRINGC
    WRITE(STRINGA,'( e15.8 )') CANVASX(X(1))
    WRITE(STRINGB,'( e15.8 )') CANVASY(X(2))
    WRITE(STRINGC,'( e15.8 )') SCALER(R)
    WRITE(SVGID,'(1X,A,A,A,A,A,A,A)') ' <circle cx="', TRIM(ADJUSTL(STRINGA)), &
                                      '" cy="', TRIM(ADJUSTL(STRINGB)), &
                                      '" r="', TRIM(ADJUSTL(STRINGC)), &
                                      '"'
    WRITE(SVGID,'(1X,A,A,A,A)') 'style="stroke:#000000;fill:#ff0000" />'
  END SUBROUTINE

  SUBROUTINE SVGFOOTER()
    WRITE(SVGID,'(1X,A)') ' </svg>'
    CLOSE(SVGID)
  END SUBROUTINE

END MODULE ModLib_ForSVG
