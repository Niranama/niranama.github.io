MODULE ModLib_Plotter
!                  Kernel Program of PLOTTER
! Author: Masao Kodama
! E-mail: mkodama@mable.ne.jp
! Version: 2.1
! Date: March 18, 2009

REAL,PARAMETER:: FACT=2.845,PI=3.14159265359   !  1mm=2.845pt
REAL,PARAMETER:: ALNB2=3*FACT,ALNB1=ALNB2*.7   !  broken line
REAL,PARAMETER:: ALND2=1.5*FACT,ALND1=ALND2*.5 !  dotted line
REAL,PARAMETER:: ALNC4=8*FACT,ALNC1=ALNC4*.7,ALNC2=ALNC4*.8, &
                 ALNC3=ALNC4*.9, &             !  chain  line
                 CONST=80*FACT ! 80=the initial value of xcomax and ycomax
REAL:: X00D=0,Y00D=0,XCOMAX1=CONST,YCOMAX1=CONST, &
       XCOMAXD=CONST,YCOMAXD=CONST,HEIGHTD=4.,THETA_DEG=0
! x00d=x00*fact         y00d=y00*fact
! xcomax1=xcomax*fact   ycomax1=ycomax*fact
! xcomaxd=xcomax1+x00d  ycomaxd=ycomax+y00d
! heightd=the height of characters in unit mm.
! theta_deg: the slant angle in degrees.
INTEGER:: LINE_TYPED=1, &  ! line_type: the kind of lines
     ICLIPD=0,          &  ! iclipd: clip or non-clip
     NXD=0,NYD=0,       &  ! nxd, nyd: the internal position of character.
     IOPEN=-8              ! When iopen<0, an output file is not open.

    CONTAINS

SUBROUTINE PFNBEGIN(CHAR)
! Section 2.1 of the manual "Computer Graphics PLOTTER"
! Beginning Plotter
!USE plotter,ONLY: iopen
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN):: CHAR
IF(IOPEN > 0) THEN
  PRINT *,'An error in SUBROUTINE pfnbegin of Plotter'
  PRINT *,'SUBROUTINE pfnbegin has been already invoked.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
OPEN(88,STATUS='REPLACE',FILE=CHAR//'.ps')
IOPEN=2
END SUBROUTINE PFNBEGIN

SUBROUTINE PFNEND
! Section 2.2 of the manual "Computer Graphics PLOTTER"
! Ending Plotter
! Closing the Postscript file for Plotter
!USE plotter,ONLY: iopen
IMPLICIT NONE
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pfnend of Plotter'
  PRINT *,'SUBROUTINE pfnend has been already invoked,'
  PRINT *,'or SUBROUTINE pfnbegin must be invoked before SUBROUTINE pfnend.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
WRITE(88,*) ' showpage '
CLOSE(88,STATUS='KEEP')
IOPEN=-8
END SUBROUTINE PFNEND

SUBROUTINE PFSORIGIN(X00,Y00)
! Section 2.3 of the manual "Computer Graphics PLOTTER"
! Setting the origin
!USE plotter,ONLY: fact,x00d,y00d,xcomax1,ycomax1,xcomaxd,ycomaxd
IMPLICIT NONE
REAL,INTENT(IN):: X00,Y00
X00D=X00*FACT;  Y00D=Y00*FACT
XCOMAXD=XCOMAX1+X00D;  YCOMAXD=YCOMAX1+Y00D
END SUBROUTINE PFSORIGIN

SUBROUTINE PFSCOLOR(REDC,GREENC,BLUEC)
! Section 2.4 of the manual "Computer Graphics PLOTTER"
! Setting the color
! Argument redc indicates the strength of red.
! Argument greenc indicates the strength of green.
! Argument bluec indicates the strength of blue.
! The color of a combination of redc, greenc and bluec is determined by the
! additive combination of colors.
! Default values of redc, greenc and bluec are 0.
!USE plotter,ONLY: iopen
IMPLICIT NONE
REAL,INTENT(IN):: REDC,GREENC,BLUEC
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pfscolor of Plotter'
  PRINT *,'SUBROUTINE pfscolor has been already invoked,'
  PRINT *,'or SUBROUTINE pfnbegin must be invoked before SUBROUTINE pfscolor.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(MAX(REDC,GREENC,BLUEC) > 100.01) THEN
  PRINT *,'An error in SUBROUTINE pfscolor(redc,greenc,bluec) of Plotter'
  PRINT 11,'redc  =',REDC
  PRINT 11,'greenc=',GREENC
  PRINT 11,'bluec =',BLUEC
  PRINT *,'It is necessary that MAX(redc,greenc,bluec)<=100.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(MIN(REDC,GREENC,BLUEC) < -0.01) THEN
  PRINT *,'An error in SUBROUTINE pfscolor(redc,greenc,bluec) of Plotter'
  PRINT 11,'redc  =',REDC
  PRINT 11,'greenc=',GREENC
  PRINT 11,'bluec =',BLUEC
  PRINT *,'It is necessary that MIN(redc,greenc,bluec)>=0.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
11 FORMAT(1X,A,F6.1)
WRITE(88,8) REDC*0.01,GREENC*0.01,BLUEC*0.01,' setrgbcolor'
8 FORMAT(3F8.2,A)
END SUBROUTINE PFSCOLOR

SUBROUTINE PLSTYPE(LINE_TYPE)
! Section 2.5 of the manual "Computer Graphics PLOTTER"
! Setting the line type
! line_type=1: solid line     line_type=2: broken line
! line_type=3: dotted line    line_type=4: chain line
!USE plotter,ONLY: line_typed
IMPLICIT NONE
INTEGER,INTENT(IN):: LINE_TYPE
IF(LINE_TYPE<=0 .OR. LINE_TYPE>=5) THEN
  PRINT *,'An error in SUBROUTINE plstype(line_type) of Plotter'
  PRINT *,'line_type=',LINE_TYPE
  PRINT *,'The value of argument line_type is invalid.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
  ENDIF
LINE_TYPED=LINE_TYPE
END SUBROUTINE PLSTYPE

SUBROUTINE PLSWIDTH(WIDTH)
! Section 2.6 of the manual "Computer Graphics PLOTTER"
! Setting the line width
!USE plotter,ONLY: fact
IMPLICIT NONE
REAL,INTENT(IN):: WIDTH
IF(WIDTH < 0) THEN
  PRINT *,'An error in SUBROUTINE plswidth(width) of Plotter'
  PRINT '(A,F6.1)',' width=',WIDTH
  PRINT *,'The argument width must be positive.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(WIDTH > 20) THEN
  PRINT *,'An error in SUBROUTINE plswidth(width) of Plotter'
  PRINT '(A,F6.1)',' width=',WIDTH
  PRINT *,'The argument width must be less than 20.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
WRITE(88,*) WIDTH*FACT,' setlinewidth'
END SUBROUTINE PLSWIDTH

SUBROUTINE PLSFRAME(XCOMAX,YCOMAX)
! Section 2.7 of the manual "Computer Graphics PLOTTER"
! Setting the clipping frame
! The clipping frame is a rectangle.
! The vertexes of the rectangle are at (0., 0.), (xcomax, 0.), (xcomax, ycomax)
! and (0., ycomax).
!USE plotter,ONLY: fact,x00d,y00d,xcomax1,ycomax1,xcomaxd,ycomaxd
IMPLICIT NONE
REAL,INTENT(IN):: XCOMAX,YCOMAX
IF(XCOMAX <= 0) THEN
  PRINT *,'An error in SUBROUTINE plsframe(xcomax,ycomax) of Plotter'
  PRINT '(A,F6.1)',' xcomax=',XCOMAX
  PRINT *,'The argument xcomax must be positive.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(YCOMAX <= 0) THEN
  PRINT *,'An error in SUBROUTINE plsframe(xcomax,ycomax) of Plotter'
  PRINT '(A,F6.1)',' ycomax=',YCOMAX
  PRINT *,'The argument ycomax must be positive.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
XCOMAX1=XCOMAX*FACT;   YCOMAX1=YCOMAX*FACT
XCOMAXD=XCOMAX1+X00D;  YCOMAXD=YCOMAX1+Y00D
END SUBROUTINE PLSFRAME

SUBROUTINE PLSCLIP(ICLIP)
! Section 2.8 of the manual "Computer Graphics PLOTTER"
! Setting the clipping indicator
! iclip=0: non_clipping,   iclip=1: clipping
!USE plotter,ONLY: iclipd
IMPLICIT NONE
INTEGER,INTENT(IN):: ICLIP
IF(ICLIP/=0 .AND. ICLIP/=1) THEN
  PRINT *,'An error in SUBROUTINE plsclip(iclip) of Plotter'
  PRINT *,'iclip=',ICLIP
  PRINT *,'The value of iclip must be 0 or 1.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
ICLIPD=ICLIP
END SUBROUTINE PLSCLIP

SUBROUTINE PLDLINE(NN,DXX,DYY)
! Section 2.9 of the manual "Computer Graphics PLOTTER"
! Drawing a polygonal line
! The internal procedures: gpl1, gpl2, gpl3, gpl4 and gsgmnt1.
!USE plotter,ONLY: fact,line_typed,x00d,y00d,alnb1,alnb2,alnc1,alnc2,alnc3, &
!  alnc4,alnd1,alnd2,iopen
IMPLICIT NONE
REAL,INTENT(IN):: DXX(*),DYY(*)
INTEGER,INTENT(IN):: NN
REAL:: DX1(NN),DY1(NN)
INTEGER:: I
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pldline of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pldline.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(NN <= 1) THEN
  PRINT *,'An error in SUBROUTINE pldline(nn,dxx,dyy) of Plotter'
  PRINT *,'nn=',NN
  PRINT *,'The argument nn must be larger than 1.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
DO I=1,NN
  IF(ABS(DXX(I)) > 30000) THEN
    PRINT *,'An error in SUBROUTINE pldline(nn,dxx,dyy) of Plotter'
    WRITE(*,11) 'dxx(',I,')=',DXX(I)
    11 FORMAT(1X,A,I4,A,2G14.7)
    WRITE(*,*) 'The values of ABS(dxx(i)) must be less than 30000.'
    PRINT *,'Stop of execution';  PRINT *;  STOP
  ENDIF
  IF(ABS(DYY(I)) > 30000) THEN
    PRINT *,'An error in SUBROUTINE pldline(nn,dxx,dyy) of Plotter'
    WRITE(*,11) 'dyy(',I,')=',DYY(I)
    WRITE(*,*) 'The values of ABS(dyy(i)) must be less than 30000.'
    PRINT *,'Stop of execution';  PRINT *;  STOP
  ENDIF
  DX1(I)=FACT*DXX(I)+X00D;  DY1(I)=FACT*DYY(I)+Y00D
ENDDO
SELECT CASE(LINE_TYPED)
  CASE(1);  CALL GPL1   ! solid line
  CASE(2);  CALL GPL2   ! broken line
  CASE(3);  CALL GPL3   ! dotted line
  CASE(4);  CALL GPL4   ! chain line
  CASE DEFAULT;  STOP 'Error in pldline of Plotter'
END SELECT
CONTAINS

SUBROUTINE GPL1
! An internal procedure of SUBROUTINE pldline
! Solid line
! The variables used in common with the host procedure: nn, dx1 and dy1
DO I=2,NN
  CALL GSGMNT1(DX1(I-1),DY1(I-1),DX1(I),DY1(I))
ENDDO
END SUBROUTINE GPL1

SUBROUTINE GPL2
! An internal procedure of SUBROUTINE pldline
! Broken line
! The variables used in common with the host procedure: nn, dx1 and dy1
REAL:: AST0,AST1,B1,COMPX,COMPY,POSIT,X1,Y1,X2,Y2
INTEGER:: I
POSIT=0
DO I=2,NN
  AST0=0
  AST1=SQRT((DX1(I)-DX1(I-1))**2+(DY1(I)-DY1(I-1))**2)
  IF(AST1 <= 1E-8) THEN
    COMPX=0;  COMPY=0
  ELSE
    COMPX=(DX1(I)-DX1(I-1))/AST1;  COMPY=(DY1(I)-DY1(I-1))/AST1
  ENDIF
  DO
    X1=DX1(I-1)+AST0*COMPX;  Y1=DY1(I-1)+AST0*COMPY
    IF(AST1-AST0 <= ALNB2-POSIT) THEN
      IF(POSIT < ALNB1) THEN
        B1=MIN(AST1-AST0,ALNB1-POSIT)+AST0
        X2=DX1(I-1)+B1*COMPX;  Y2=DY1(I-1)+B1*COMPY
        CALL GSGMNT1(X1,Y1,X2,Y2)
      ENDIF
      POSIT=POSIT+AST1-AST0;  AST0=0;  EXIT
    ELSE
    IF(POSIT < ALNB1) THEN
      B1=AST0+ALNB1-POSIT
      X2=DX1(I-1)+B1*COMPX;  Y2=DY1(I-1)+B1*COMPY
      CALL GSGMNT1(X1,Y1,X2,Y2)
    ENDIF
    AST0=AST0+ALNB2-POSIT;  POSIT=0
    ENDIF
  ENDDO
ENDDO
END SUBROUTINE GPL2

SUBROUTINE GPL3
! An internal procedure of SUBROUTINE pldline
! Dotted line
! The variables used in common with the host procedure: nn, dx1 and dy1
REAL:: AST0,AST1,POSIT,COMPX,COMPY,B1,X1,Y1,X2,Y2
POSIT=0
DO I=2,NN
  AST0=0
  AST1=SQRT((DX1(I)-DX1(I-1))**2+(DY1(I)-DY1(I-1))**2)
  IF(AST1 <= 1E-8) THEN
    COMPX=0;  COMPY=0
  ELSE
    COMPX=(DX1(I)-DX1(I-1))/AST1;  COMPY=(DY1(I)-DY1(I-1))/AST1
  ENDIF
  DO
    X1=DX1(I-1)+AST0*COMPX;  Y1=DY1(I-1)+AST0*COMPY
    IF(AST1-AST0 <= ALND2-POSIT) THEN
      IF(POSIT < ALND1) THEN
        B1=MIN(AST1-AST0,ALND1-POSIT)+AST0
        X2=DX1(I-1)+B1*COMPX;  Y2=DY1(I-1)+B1*COMPY
        CALL GSGMNT1(X1,Y1,X2,Y2)
      ENDIF
      POSIT=POSIT+AST1-AST0;  AST0=0;  EXIT
    ELSE
      IF(POSIT < ALND1) THEN
        B1=AST0+ALND1-POSIT
        X2=DX1(I-1)+B1*COMPX;  Y2=DY1(I-1)+B1*COMPY
        CALL GSGMNT1(X1,Y1,X2,Y2)
      ENDIF
      AST0=AST0+ALND2-POSIT;  POSIT=0
    ENDIF
  ENDDO
ENDDO
END SUBROUTINE GPL3

SUBROUTINE GPL4
! An internal procedure of SUBROUTINE pldline
! Chain line
! The variables used in common with the host procedure: nn, dx1 and dy1
REAL:: AST0,AST1,B1,COMPX,COMPY,POSIT,X1,Y1,X2,Y2,X3,Y3,X4,Y4
POSIT=0
DO I=2,NN
  AST0=0
  AST1=SQRT((DX1(I)-DX1(I-1))**2+(DY1(I)-DY1(I-1))**2)
  IF(AST1 <= 1E-8) THEN
    COMPX=0;  COMPY=0
  ELSE
    COMPX=(DX1(I)-DX1(I-1))/AST1;  COMPY=(DY1(I)-DY1(I-1))/AST1
  ENDIF
  DO
    X1=DX1(I-1)+AST0*COMPX;  Y1=DY1(I-1)+AST0*COMPY
    IF(AST1-AST0 <= ALNC4-POSIT) THEN
      IF(POSIT < ALNC1) THEN
        B1=MIN(AST1-AST0,ALNC1-POSIT)+AST0
        X2=DX1(I-1)+B1*COMPX;  Y2=DY1(I-1)+B1*COMPY
        CALL GSGMNT1(X1,Y1,X2,Y2)
      ENDIF
      IF(POSIT < ALNC3) THEN
        B1=MIN(AST1-AST0,MAX(ALNC2-POSIT,0.))+AST0
        X3=DX1(I-1)+B1*COMPX;  Y3=DY1(I-1)+B1*COMPY
        B1=MIN(AST1-AST0,ALNC3-POSIT)+AST0
        X4=DX1(I-1)+B1*COMPX;  Y4=DY1(I-1)+B1*COMPY
        CALL GSGMNT1(X3,Y3,X4,Y4)
      ENDIF
      POSIT=POSIT+AST1-AST0;  AST0=0;  EXIT
    ELSE
      IF(POSIT < ALNC1) THEN
        B1=AST0+ALNC1-POSIT
        X2=DX1(I-1)+B1*COMPX;  Y2=DY1(I-1)+B1*COMPY
        CALL GSGMNT1(X1,Y1,X2,Y2)
      ENDIF
      IF(POSIT < ALNC3) THEN
        B1=MIN(AST1-AST0,MAX(ALNC2-POSIT,0.))+AST0
        X3=DX1(I-1)+B1*COMPX;  Y3=DY1(I-1)+B1*COMPY
        B1=MIN(AST1-AST0,ALNC3-POSIT)+AST0
        X4=DX1(I-1)+B1*COMPX;  Y4=DY1(I-1)+B1*COMPY
        CALL GSGMNT1(X3,Y3,X4,Y4)
      ENDIF
      AST0=AST0+ALNC4-POSIT;   POSIT=0
    ENDIF
  ENDDO
ENDDO
END SUBROUTINE GPL4

SUBROUTINE GSGMNT1(X1,Y1,X2,Y2)
! An internal procedure of SUBROUTINE pldline
! Drawing a solid segment independently of the values of iclip and line_type.
! (x1,y1), (x2,y2)=both ends of segment
! The variable used in common with the host procedure: x00d and y00d
!USE plotter,ONLY: iclipd,xcomaxd,ycomaxd
REAL,INTENT(IN):: X1,Y1,X2,Y2
REAL:: A1,X2D,Y2D,X1D,Y1D,XS,YS
INTEGER:: ICUT
X1D=X1;  Y1D=Y1;  X2D=X2;  Y2D=Y2
IF(ICLIPD == 0) THEN
  WRITE(88,101) 'newpath ',X1,Y1,' moveto ',X2,Y2,' lineto stroke'
ELSE
  ICUT=0
  IF((X00D-X1D)*(X00D-X2D) < 0) THEN
    YS=(Y2D-Y1D)*(X00D-X1D)/(X2D-X1D)+Y1D
    IF((YS-Y00D)*(YS-YCOMAXD) < 0) THEN
      IF(X00D < X1D) THEN
        A1=X2D;  X2D=X1D;  X1D=A1
        A1=Y2D;  Y2D=Y1D;  Y1D=A1
      ENDIF
      X1D=X00D;  Y1D=YS;  ICUT=1
    ENDIF
  ENDIF
  IF((XCOMAXD-X1D)*(XCOMAXD-X2D) < 0) THEN
    YS=(Y2D-Y1D)*(XCOMAXD-X1D)/(X2D-X1D)+Y1D
    IF((YS-Y00D)*(YS-YCOMAXD) < 0) THEN
      IF(XCOMAXD > X1D) THEN
        A1=X2D;  X2D=X1D;  X1D=A1
        A1=Y2D;  Y2D=Y1D;  Y1D=A1
      ENDIF
      X1D=XCOMAXD;  Y1D=YS;  ICUT=1
    ENDIF
  ENDIF
  IF((Y00D-Y1D)*(Y00D-Y2D) < 0) THEN
    XS=(X2D-X1D)*(Y00D-Y1D)/(Y2D-Y1D)+X1D
    IF((XS-X00D)*(XS-XCOMAXD) < 0) THEN
      IF(Y00D < Y1D) THEN
        A1=X2D;  X2D=X1D;  X1D=A1
        A1=Y2D;  Y2D=Y1D;  Y1D=A1
      ENDIF
      X1D=XS;  Y1D=Y00D;  ICUT=1
    ENDIF
  ENDIF
  IF((YCOMAXD-Y1D)*(YCOMAXD-Y2D) < 0) THEN
    XS=(X2D-X1D)*(YCOMAXD-Y1D)/(Y2D-Y1D)+X1D
    IF((XS-X00D)*(XS-XCOMAXD) < 0) THEN
      IF(YCOMAXD > Y1D) THEN
        A1=X2D;  X2D=X1D;  X1D=A1
        A1=Y2D;  Y2D=Y1D;  Y1D=A1
      ENDIF
      X1D=XS;  Y1D=YCOMAXD;  ICUT=1
    ENDIF
  ENDIF
  IF(ICUT==0 .AND. (X00D-X1D)*(XCOMAXD-X1D)<=0 &
    .AND. (Y00D-Y1D)*(YCOMAXD-Y1D)<=0 .AND. &
    (X00D-X2D)*(XCOMAXD-X2D)<=0 .AND. (Y00D-Y2D)*(YCOMAXD-Y2D)<=0) ICUT=1
  IF(ICUT == 1) &
      WRITE(88,101) 'newpath',X1D,Y1D,' moveto',X2D,Y2D,' lineto stroke'
ENDIF
101 FORMAT(A,2F10.2,A,2F10.2,A)
END SUBROUTINE GSGMNT1

END SUBROUTINE PLDLINE

SUBROUTINE PLDSGMNT(X1,Y1,X2,Y2)
! Section 2.10 of the manual "Computer Graphics PLOTTER"
! Drawing a segment
! (x1,y1) and (x2,y2)=both ends of the segment
! The subroutine invoked by this subroutine: pldline.
!USE plotter,ONLY: iopen
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1,X2,Y2
REAL:: DX(2),DY(2)
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pldsgmnt of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pldsgmnt.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
DX(1)=X1;  DY(1)=Y1
DX(2)=X2;  DY(2)=Y2
CALL PLDLINE(2,DX,DY)
END SUBROUTINE PLDSGMNT

SUBROUTINE PLDARROW(X1,Y1,X2,Y2)
! Section 2.11 of the manual "Computer Graphics PLOTTER"
! Drawing an arrowhead
! The arrowhead points from (x1,y1) to (x2,y2)
! The subroutine invoked by this subroutine: pldsgmnt.
!USE plotter,ONLY: iopen,line_typed
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1,X2,Y2
REAL,PARAMETER:: A1=2.,TH2=.51
REAL:: TH
INTEGER:: I
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pldarrow of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pldarrow.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(ABS(X1-X2)+ABS(Y1-Y2) <= 0) THEN
  PRINT *,'An error in SUBROUTINE pldarrow(x1,y1,x2,y2) of Plotter'
  PRINT '(A,F6.1)',' x1=x2=',X1
  PRINT '(A,F6.1)',' y1=y2=',Y1
  PRINT *,'It is necessary that ABS(x1-x2)+ABS(y1-y2)>0.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
TH=ATAN2(Y1-Y2,X1-X2)
I=LINE_TYPED;   LINE_TYPED=1
CALL PLDSGMNT(X2+A1*COS(TH+TH2),Y2+A1*SIN(TH+TH2),X2,Y2)
CALL PLDSGMNT(X2+A1*COS(TH-TH2),Y2+A1*SIN(TH-TH2),X2,Y2)
LINE_TYPED=I
END SUBROUTINE PLDARROW

SUBROUTINE PLDCRCL(X1,Y1,RR)
! Section 2.12 of the manual "Computer Graphics PLOTTER"
! Drawing a circle
! (x1,y1)=the center of the circle,    rr=the radius of the circle
!USE plotter,ONLY: iopen,fact,x00d,y00d,pi,line_typed, &
!  alnb1,alnb2,alnc1,alnc2,alnc3,alnc4,alnd1,alnd2
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1,RR
REAL:: ARR,AX1,AY1,DEG1,DEG2,DEG3,DEG4,DEG5,DEG6,DEG7,DEG8
INTEGER:: I,NN
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pldcrcl of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pldcrcl.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(RR <= 0) THEN
  PRINT *,'An error in SUBROUTINE pldcrcl(x1,y1,rr) of Plotter'
  PRINT '(A,F6.1)',' rr=',RR
  PRINT *,'The argument rr must be positive.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
AX1=FACT*X1+X00D;  AY1=FACT*Y1+Y00D;  ARR=FACT*RR
SELECT CASE(LINE_TYPED)
  CASE(1);  CALL PLDCRCLARC(X1,Y1,RR,0.,2*PI)       ! solid line
  CASE(2);  NN=NINT(2*PI*ARR/ALNB2);  NN=MAX(1,NN)  ! broken line
    ! alnb2=3*fact,  alnb1=alnb2*.7:                  broken line
    DEG2=360./NN;  DEG1=DEG2*(ALNB1/ALNB2)
    DO I=1,NN
      DEG5=DEG2*I;  DEG6=DEG5+DEG1
      WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
      100 FORMAT(A,5F9.2,A)
    ENDDO
  CASE(3);  NN=NINT(2*PI*ARR/ALND2);  NN=MAX(1,NN)  ! dotted line
    ! alnd2=1.5*fact,  alnd1=alnd2*.5:                dotted line
    DEG2=360./NN;  DEG1=DEG2*(ALND1/ALND2)
    DO I=1,NN
      DEG5=DEG2*I;  DEG6=DEG5+DEG1
      WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
    ENDDO
  CASE(4);  NN=NINT(2*PI*ARR/ALNC4);  NN=MAX(1,NN)  !  chain line
    ! alnc4=8*fact, alnc1=alnc4*.7, alnc2=alnc4*.8, alnc3=alnc4*.9:  chain line
    DEG4=360./NN;  DEG1=DEG4*(ALNC1/ALNC4);  DEG2=DEG4*(ALNC2/ALNC4)
    DEG3=DEG4*(ALNC3/ALNC4)
    DO I=1,NN
      DEG5=DEG4*I;  DEG6=DEG5+DEG1
      WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
      DEG7=DEG5+DEG2;  DEG8=DEG5+DEG3
      WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG7,DEG8,' arc stroke'
    ENDDO
  CASE DEFAULT;  STOP 'Error in pldcrcl of Plotter'
END SELECT
END SUBROUTINE PLDCRCL

SUBROUTINE PLDCRCLARC(X1,Y1,RR,PHI1,PHI2)
! Section 2.13 of the manual "Computer Graphics PLOTTER"
! Drawing a circular arc
! (x1,y1)=the center of circular arc,    rr=the radius of circular arc
! phi1=the starting angle,               phi2=the arriving angle
! It is necessary that phi1<phi2<phi1+2*pi.
!USE plotter,ONLY: iopen,fact,x00d,y00d,pi,line_typed, &
!  alnb1,alnb2,alnc1,alnc2,alnc3,alnc4,alnd1,alnd2
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1,RR,PHI1,PHI2
REAL:: ARR,AX1,AY1,ADEG1,ADEG2,DEG1,DEG,DEG2,DEG3,DEG4,DEG5,DEG6,DEG7,DEG8, &
  DEGTOTAL
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pldcrclarc of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pldcrclarc.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(RR <= 0) THEN
  PRINT *,'An error in SUBROUTINE pldcrclarc(x1,y1,rr,phi1,phi2) of Plotter'
  PRINT '(A,F6.1)',' rr=',RR
  PRINT *,'The argument rr must be positive.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(PHI2 < PHI1) THEN
  PRINT *,'An error in SUBROUTINE pldcrclarc(x1,y1,rr,phi1,phi2) of Plotter'
  PRINT '(A,F6.1)',' phi1=',PHI1
  PRINT '(A,F6.1)',' phi2=',PHI2
  PRINT *,'Angle phi2 must be larger than angle phi1.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(PHI2 > PHI1+2*PI) THEN
  PRINT *,'An error in SUBROUTINE pldcrclarc(x1,y1,rr,phi1,phi2) of Plotter'
  PRINT '(A,F6.1)',' phi1=',PHI1
  PRINT '(A,F6.1)',' phi2=',PHI2
  PRINT *,'Angle phi2 must be less than angle phi1+2*pi.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(ABS(PHI1)>4*PI .OR. ABS(PHI2)>4*PI) THEN
  PRINT *,'An error in SUBROUTINE pldcrclarc of Plotter'
  PRINT '(A,F6.1)',' phi1=',PHI1
  PRINT '(A,F6.1)',' phi2=',PHI2
  PRINT *,'It must be satisfied that ABS(phi1)<4*pi and ABS(phi2)<4*pi.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
AX1=FACT*X1+X00D;  AY1=FACT*Y1+Y00D;  ARR=FACT*RR
ADEG1=PHI1*180./PI;  ADEG2=PHI2*180./PI
SELECT CASE(LINE_TYPED)
  CASE(1)
    WRITE(88,100) 'newpath ',AX1,AY1,ARR,ADEG1,ADEG2,' arc stroke'
  CASE(2)    ! alnb2=3*fact, alnb1=alnb2*.7:     broken line
    DEGTOTAL=ADEG2-ADEG1;  DEG=0;  DEG2=(ALNB2/ARR)*180/PI
    DEG1=DEG2*(ALNB1/ALNB2)
    DO
      IF(DEGTOTAL-DEG > DEG2) THEN
        DEG5=ADEG1+DEG;  DEG6=DEG5+DEG1
        WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
      ELSE;  EXIT
      ENDIF
      DEG=DEG+DEG2
    ENDDO
    DEG5=ADEG1+DEG
    DEG6=DEG5+MIN(DEG1,DEGTOTAL-DEG)
    WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
  CASE(3)    ! alnd2=1.5*fact, alnd1=alnd2*.5:   dotted line
    DEGTOTAL=ADEG2-ADEG1;  DEG=0;  DEG2=(ALND2/ARR)*180/PI
    DEG1=DEG2*(ALND1/ALND2)
    DO
      IF(DEGTOTAL-DEG > DEG2) THEN
        DEG5=ADEG1+DEG;  DEG6=DEG5+DEG1
        WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
      ELSE;  EXIT
      ENDIF
      DEG=DEG+DEG2
    ENDDO
    DEG5=ADEG1+DEG
    DEG6=DEG5+MIN(DEG1,DEGTOTAL-DEG)
    WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
  CASE(4)
    ! alnc4=8*fact, alnc1=alnc4*.7, alnc2=alnc4*.8, alnc3=alnc4*.9: chain line
    DEGTOTAL=ADEG2-ADEG1;  DEG=0;  DEG4=(ALNC4/ARR)*180/PI;
    DEG1=DEG4*(ALNC1/ALNC4);  DEG2=DEG4*(ALNC2/ALNC4)
    DEG3=DEG4*(ALNC3/ALNC4)
    DO
      IF(DEGTOTAL-DEG > DEG4) THEN
        DEG5=ADEG1+DEG;  DEG6=DEG5+DEG1
        WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
        DEG7=DEG5+DEG2;  DEG8=DEG5+DEG3
        WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG7,DEG8,' arc stroke'
      ELSE;  EXIT
      ENDIF
      DEG=DEG+DEG4
    ENDDO
    DEG5=ADEG1+DEG
    DEG6=DEG5+MIN(DEG1,DEGTOTAL-DEG)
    WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
    IF(DEGTOTAL-DEG > DEG2) THEN
      DEG5=ADEG1+DEG+DEG2
      DEG6=ADEG1+DEG+MIN(DEG3,ADEG2-DEG)
      WRITE(88,100) 'newpath ',AX1,AY1,ARR,DEG5,DEG6,' arc stroke'
    ENDIF
  CASE DEFAULT;  STOP 'Error in pldcrclarc of Plotter'
END SELECT
100 FORMAT(A,5F9.2,A)
END SUBROUTINE PLDCRCLARC

SUBROUTINE PLDARCARROW(X1,Y1,RR,PHI1,PHI2)
! Section 2.14 of the manual "Computer Graphics PLOTTER"
! Drawing an arrowhead on a circular arc
! (x1,y1)=the center of the circular arc
! phi2=the angle of the position of the point of the arrowhead
! If phi1<=phi2, the arrowhead points anti-clockwise.
! If phi1> phi2, the arrowhead points clockwise.
! It must be satisfied that ABS(ph1)<4*pi and ABS(phi2)<4*pi.
! The subroutine invoked by this subroutine: pldarrow.
!USE plotter,ONLY: iopen,pi
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1,RR,PHI1,PHI2
REAL:: X3,Y3,PHI5
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pldarcarrow of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pldarcarrow.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(RR <= 0) THEN
  PRINT *,'An error in SUBROUTINE pldarcarrow(x1,y1,rr,phi1,phi2) of Plotter'
  PRINT '(A,F6.1)',' rr=',RR
  PRINT *,'The argument rr must be positive.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(ABS(PHI1)>4*PI .OR. ABS(PHI2)>4*PI) THEN
  PRINT *,'An error in SUBROUTINE pldarcarrow(x1,y1,rr,phi1,phi2) of Plotter'
  PRINT '(A,F6.1)',' phi1=',PHI1
  PRINT '(A,F6.1)',' phi2=',PHI2
  PRINT *,'It must be satisfied that ABS(phi1)<4*pi and ABS(phi2)<4*pi.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
X3=X1+RR*COS(PHI2);  Y3=Y1+RR*SIN(PHI2)
PHI5=PHI2-SIGN(1.5707963,PHI2-PHI1)
CALL PLDARROW(X3+COS(PHI5),Y3+SIN(PHI5),X3,Y3)
END SUBROUTINE PLDARCARROW

SUBROUTINE PLDRCTGL(X1,Y1,X2,Y2)
! Section 2.15 of the manual "Computer Graphics PLOTTER"
! Drawing a rectangle
! The vertexes of the rectangular are (x1,y1), (x2,y1), (x2,y2) and (x1,y2).
! The subroutine invoked by this subroutine: pldline.
!USE plotter,ONLY: iopen
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1,X2,Y2
REAL:: DX(5),DY(5)
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pldrctgl of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pldrctgl.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
DX(1)=X1;  DY(1)=Y1
DX(2)=X2;  DY(2)=Y1
DX(3)=X2;  DY(3)=Y2
DX(4)=X1;  DY(4)=Y2
DX(5)=X1;  DY(5)=Y1
CALL PLDLINE(5,DX,DY)
END SUBROUTINE PLDRCTGL

SUBROUTINE PLDCROSS(X1,Y1,RR)
! Section 2.16 of the manual "Computer Graphics PLOTTER"
! Drawing a cross
! (x1,y1)=the center of the cross,   rr=the radius of the cross
! The subroutine invoked by this subroutine: pldsgmnt.
!USE plotter,ONLY: iopen,line_typed
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1,RR
REAL:: A1
INTEGER:: I
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pldcross of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pldcross.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(RR <= 0) THEN
  PRINT *,'An error in SUBROUTINE pldcross(x1,y1,rr) of Plotter'
  PRINT '(A,F6.1)',' rr=',RR
  PRINT *,'The argument rr must be positive.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
I=LINE_TYPED;  LINE_TYPED=1;  A1=.7071*RR
CALL PLDSGMNT(X1+A1,Y1+A1,X1-A1,Y1-A1)
CALL PLDSGMNT(X1-A1,Y1+A1,X1+A1,Y1-A1)
LINE_TYPED=I
END SUBROUTINE PLDCROSS

SUBROUTINE PLDHATCHING(NN,DXX,DYY,SP,THETA)
! Section 2.17 of the manual "Computer Graphics PLOTTER"
! Drawing hatching
! nn=number of vertexes of the polygon,  dxx,dyy=coordinates of vertexes
! sp=the interval of the parallel lines
! theta=the slant angle of the parallel lines
! The internal procedure: sleq2.
! The subroutine invoked by this subroutine: pldsgmnt.
!USE plotter,ONLY: iopen,pi
IMPLICIT NONE
REAL,INTENT(IN):: DXX(*),DYY(*),SP,THETA
INTEGER,INTENT(IN):: NN
REAL,DIMENSION(NN):: DAA,DBB,DCC
REAL:: A,AS,FX,FY,FX1,FY1
INTEGER,PARAMETER:: IMAX=20
! imax: the maximum number of the cross points between a parallel line and
!       the polygon.
REAL:: DA(2,2),DB(2),DX1(IMAX),DY1(IMAX),DCOMP(IMAX)
INTEGER:: I,II,INFO,JJ,J1
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pldhatching of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pldhatching.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(NN <= 2) THEN
  PRINT *,'An error in SUBROUTINE pldhatching(nn,dx,dy,sp,theta) of Plotter'
  PRINT *,'nn=',NN
  PRINT *,'It is not permitted that nn<=2.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(SP < 0.) THEN
  PRINT *,'An error in SUBROUTINE pldhatching(nn,dx,dy,sp,theta) of Plotter'
  PRINT '(A,F6.1)',' sp=',SP
  PRINT *,'It is necessary that sp>0.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(ABS(THETA) > PI) THEN
  PRINT *,'An error in SUBROUTINE pldhatching(nn,dx,dy,sp,theta) of Plotter'
  PRINT '(A,F6.1)',' theta=',THETA
  PRINT *,'It is necessary that ABS(theta)<pi'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
! To obtain the equation of the polygon.
DO I=1,NN
  II=I+1;  IF(II == NN+1) II=1
  DB(1)=1.;  DA(1,1)=DXX(I);   DA(1,2)=DYY(I)
  DB(2)=1.;  DA(2,1)=DXX(II);  DA(2,2)=DYY(II)
  CALL SLEQ2(INFO)
  IF(INFO == 0) THEN
    DAA(I)=DB(1);  DBB(I)=DB(2);  DCC(I)=1.
  ELSE
    IF(ABS(DXX(I))+ABS(DYY(I)) > ABS(DXX(II))+ABS(DYY(II))) THEN
      DAA(I)=-DYY(I);   DBB(I)=DXX(I)
    ELSE
      DAA(I)=-DYY(II);  DBB(I)=DXX(II)
    ENDIF
    DCC(I)=0.
  ENDIF
ENDDO
FX=-SIN(THETA);  FY=COS(THETA);  AS=1E20
DO I=1,NN
  A=FX*DXX(I)+FY*DYY(I);  IF(A < AS) AS=A
ENDDO
FX1=COS(THETA);  FY1=SIN(THETA);  AS=AS+SP/2.
! To obtain cross points.
5 JJ=0
DO I=1,NN
  II=I+1;  IF(II == NN+1) II=1
  A=(FX*DXX(I)+FY*DYY(I)-AS)*(FX*DXX(II)+FY*DYY(II)-AS)
  IF(A < 0) THEN
    DB(1)=DCC(I);  DA(1,1)=DAA(I);  DA(1,2)=DBB(I)
    DB(2)=AS;      DA(2,1)=FX;      DA(2,2)=FY
    CALL SLEQ2(INFO)
    IF(INFO /= 0) STOP 'Error in pldhatching of Plotter'
    JJ=JJ+1
    IF(JJ > IMAX) THEN
      PRINT *,'An error in SUBROUTINE pldhatching of Plotter'
      PRINT *,'The number of cross points between the polygon and a oblique'
      WRITE(*,8) ' line is larger than ',IMAX,'.'
      WRITE(*,8) ' It must be less than ',IMAX+1,'.'
      8 FORMAT(A,I2,A)
      PRINT *,'Stop of execution';  PRINT *;  STOP
    ENDIF
    DX1(JJ)=DB(1);  DY1(JJ)=DB(2)
    DCOMP(JJ)=FX1*DX1(JJ)+FY1*DY1(JJ)
  ENDIF
ENDDO
! Arrangement of the cross points
7 J1=0
DO I=1,JJ-1
  IF(DCOMP(I+1) > DCOMP(I)) THEN
    A=DCOMP(I+1);  DCOMP(I+1)=DCOMP(I);  DCOMP(I)=A
    A=DX1(I+1);    DX1(I+1)=DX1(I);      DX1(I)=A
    A=DY1(I+1);    DY1(I+1)=DY1(I);      DY1(I)=A
    J1=J1+1
  ENDIF
ENDDO
IF(J1 >= 1) GOTO 7
DO I=1,JJ,2
  CALL PLDSGMNT(DX1(I),DY1(I),DX1(I+1),DY1(I+1))
ENDDO
AS=AS+SP
IF(JJ > 0) GOTO 5
CONTAINS

SUBROUTINE SLEQ2(INFO)
! An internal procedure of SUBROUTINE pldhatching
! Solving 2-dimensional linear simultaneous equations.
! The variables used in common with the host procedure: da(2,2) and db(2)
INTEGER,INTENT(OUT):: INFO
REAL,DIMENSION(2):: DB2
REAL:: DD
DB2=DB
DD=DA(1,1)*DA(2,2)-DA(1,2)*DA(2,1)
INFO=0; IF(ABS(DD) <= 1E-10) THEN;  INFO=10;  RETURN;  ENDIF
DB(1)=( DA(2,2)*DB2(1)-DA(1,2)*DB2(2))/DD
DB(2)=(-DA(2,1)*DB2(1)+DA(1,1)*DB2(2))/DD
END SUBROUTINE SLEQ2

END SUBROUTINE PLDHATCHING

SUBROUTINE PRPRCTGL(X1,Y1,X2,Y2)
! Section 2.18 of the manual "Computer Graphics PLOTTER"
! Painting a rectangular region
! The vertexes of the rectangular are (x1,y1), (x2,y1), (x2,y2) and (x1,y2).
! The subroutine invoked by this subroutine: prpplygn.
!USE plotter,ONLY: iopen
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1,X2,Y2
REAL:: DX(4),DY(4)
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE prprctgl of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE prprctgl.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
DX(1)=X1;  DX(2)=X2;  DX(3)=X2;  DX(4)=X1
DY(1)=Y1;  DY(2)=Y1;  DY(3)=Y2;  DY(4)=Y2
CALL PRPPLYGN(4,DX,DY)
END SUBROUTINE PRPRCTGL

SUBROUTINE PRPCRCL(X1,Y1,RR)
! Section 2.19 of the manual "Computer Graphics PLOTTER"
! Painting a circular region
! (x1,y1)=the center of the circle,  rr=the radius of the circle
!USE plotter,ONLY: iopen,fact,x00d,y00d
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1,RR
REAL:: X1D,Y1D,RRD
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE prpcrcl of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE prpcrcl.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(RR <= 0) THEN
  PRINT *,'An error in SUBROUTINE prpcrcl(x1,y1,rr) of Plotter'
  PRINT '(A,F6.1)',' rr=',RR
  PRINT *,'It is necessary that rr>0.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
X1D=FACT*X1+X00D;  Y1D=FACT*Y1+Y00D;  RRD=FACT*RR
WRITE(88,10) 'newpath ',X1D,Y1D,RRD,' 0 360 arc fill'
10 FORMAT(A,3F9.2,A)
END SUBROUTINE PRPCRCL

SUBROUTINE PRPPLYGN(NN,DXX,DYY)
! Section 2.20 of the manual "Computer Graphics PLOTTER"
! Painting a polygonal region
!USE plotter,ONLY: iopen,fact,x00d,y00d
IMPLICIT NONE
INTEGER,INTENT(IN):: NN
REAL,INTENT(IN):: DXX(*),DYY(*)
REAL:: X1D,Y1D
INTEGER:: I
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE prpplygn of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE prpplygn.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(NN <= 2) THEN
  PRINT *,'An error in SUBROUTINE prpplygn(nn,dxx,dyy) of Plotter'
  PRINT *,'nn=',NN
  PRINT *,'It is necessary that nn>=3.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
WRITE(88,*) 'newpath'
X1D=FACT*DXX(1)+X00D;  Y1D=FACT*DYY(1)+Y00D
WRITE(88,10) X1D,Y1D,' moveto'
DO I=2,NN
  X1D=FACT*DXX(I)+X00D;  Y1D=FACT*DYY(I)+Y00D
  WRITE(88,10) X1D,Y1D,' lineto'
ENDDO
10 FORMAT(2F9.2,A,2F9.2,A)
WRITE(88,*) 'fill'
END SUBROUTINE PRPPLYGN

SUBROUTINE PCSHEIGHT(HEIGHT)
! Section 2.21 of the manual "Computer Graphics PLOTTER"
! Setting the height of character patterns
!USE plotter,ONLY: iopen,heightd
IMPLICIT NONE
REAL,INTENT(IN):: HEIGHT
IF(HEIGHT <= 0) THEN
  PRINT *,'An error in SUBROUTINE pcsheight(height) of Plotter'
  PRINT '(A,F6.1)',' height=',HEIGHT
  PRINT *,'The argument height must be positive.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
IF(HEIGHT > 200) THEN
  PRINT *,'An error in SUBROUTINE pcsheight(height) of Plotter'
  PRINT '(A,F6.1)',' height=',HEIGHT
  PRINT *,'The argument height must be less than 200.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
HEIGHTD=HEIGHT                      ! heightd: size of font in unit mm
END SUBROUTINE PCSHEIGHT

SUBROUTINE PCSPSTN(NX,NY)
! Section 2.22 of the manual "Computer Graphics PLOTTER"
! Setting the internal position of character patterns
!USE plotter,ONLY: nxd,nyd
IMPLICIT NONE
INTEGER,INTENT(IN):: NX,NY
IF(NX<=-1 .OR. NX>=3) THEN
  PRINT *,'An error in SUBROUTINE pcspstn(nx,ny) of Plotter'
  PRINT *,'nx=',NX
  PRINT *,'The value of argument nx is invalid.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
NXD=NX
IF(NY<=-2 .OR. NY>=4) THEN
  PRINT *,'An error in SUBROUTINE pcspstn(nx,ny) of Plotter'
  PRINT *,'ny=',NY
  PRINT *,'The value of argument ny is invalid.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
NYD=NY
END SUBROUTINE PCSPSTN

SUBROUTINE PCSANGLE(THETA)
! Section 2.23 of the manual "Computer Graphics PLOTTER"
! Setting the slant angle of character patterns.
!USE plotter,ONLY: pi,theta_deg
IMPLICIT NONE
REAL,INTENT(IN):: THETA
IF(ABS(THETA)>2*PI) THEN
  PRINT *,'An error in SUBROUTINE pcsangle(theta) of Plotter'
  PRINT '(A,F6.1)',' theta=',THETA
  PRINT *,'It is necessary that ABS(theta)<2*pi.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
THETA_DEG=THETA*180/PI
END SUBROUTINE PCSANGLE

SUBROUTINE PCWRITE(X1,Y1,CHAR)
! Section 2.24 of the manual "Computer Graphics PLOTTER"
! Writing a character pattern.
! The internal position of the character pattern is put at (x1, y1).
! The internal procedures: writeascii, shiftkanji, iachard, indexd and
! printchar.
!USE plotter,ONLY: iopen,x00d,y00d,theta_deg,nxd,nyd,heightd,fact
IMPLICIT NONE
REAL,INTENT(IN):: X1,Y1
CHARACTER(LEN=*),INTENT(IN):: CHAR
CHARACTER(LEN=200):: CHAR1,CHAR20,CHAR3
CHARACTER(LEN=3):: CHSUF
CHARACTER(LEN=2):: CHARS
REAL,PARAMETER:: FACTSPACE=20     ! factspace=charsize/\s1}
REAL,PARAMETER:: FACTSUBF=.7      ! factsubf=charsizesub/charsize
REAL:: A1,CHARSIZE_PRNT,SP,SUFPOS,XSHIFT,YSHIFT,HEIGHTD1,CHARSIZE,CHARSIZESUB
INTEGER:: I1,I2,I3,I11,I12,ISUF,NUMSUF,KINDCHAR,KSP
REAL:: DCHARFACT(12)
DATA DCHARFACT/4*4.27,4*3.97,2*4.27,2*3.63/
CHARACTER(LEN=22):: CHARTYPE(12)
DATA CHARTYPE &
   /'Times-Roman','Times-Bold','Times-Italic','Times-BoldItalic',&
   'Helvetica','Helvetica-Bold','Helvetica-Oblique','Helvetica-BoldOblique', &
   'Symbol','Symbol','Ryumin-Light-H','GothicBBB-Medium-H'/
IF(IOPEN < 0) THEN
  PRINT *,'An error in SUBROUTINE pcwrite of Plotter'
  PRINT *,'SUBROUTINE pfnbegin must be invoked before SUBROUTINE pcwrite.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
! Making a scratch file
OPEN(UNIT=86,STATUS='SCRATCH')
REWIND 86
IF(LEN(CHAR) > 200) THEN
  PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
  PRINT *,'LEN(char)=',LEN(CHAR)
  PRINT *,'It is necessary that LEN(char)<=200.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
HEIGHTD1=HEIGHTD*FACT  ! heightd1=the height of characters in unit pt.
KINDCHAR=1
CHARSIZE=HEIGHTD*DCHARFACT(KINDCHAR) ! =character size in scalefont
CHARSIZESUB=CHARSIZE*FACTSUBF        ! =subscript size in scalefont
CALL PRINTCHAR(CHARSIZE)
! Writing the control strings and the text strings of char to file unit 86.
CHAR1=ADJUSTL(CHAR)
DO
  I2=MIN(INDEXD(INDEX(CHAR1,'\f')),INDEXD(INDEX(CHAR1,'\u')),&
     INDEXD(INDEX(CHAR1,'\l')),INDEXD(INDEX(CHAR1,'\e')),&
     INDEXD(INDEX(CHAR1,'\s')))  ! indexd: an internal function
  IF(I2 > 500) EXIT
  CHAR20=CHAR1(:I2-1);  I11=INDEX(CHAR20,'\')
  DO
    IF(I11 == 0) EXIT
    IF(VERIFY(CHAR20(I11+1:I11+3),'01234567') /= 0) THEN
      PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
      PRINT *,"char='",CHAR,"'"
      PRINT *,'The letter next \ must be f, u, l, e, s or 3 octal figures.'
      PRINT *,'Stop of execution';  PRINT *;  STOP
    ENDIF
    I12=INDEX(CHAR20(I11+1:),'\');  IF(I12 == 0) EXIT;  I11=I12+I11
  ENDDO
  IF(I2 >= 2) WRITE(86,102) CHAR1(:I2-1)//'\'  ! letters to be printed
  CHAR1=CHAR1(I2:)
  I1=INDEX(CHAR1,' ');  I3=INDEX(CHAR1,'}')
  IF(I1 < I3) THEN
    PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
    PRINT *,"char='",CHAR,"'"
    PRINT *,'There is a space in a control string.'
    PRINT *,'Stop of execution';  PRINT *;  STOP
  ENDIF
  IF(I3 == 0) THEN
    PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
    PRINT *,"char='",CHAR,"'"
    PRINT *,'Argument char lacks } in a control string.'
    PRINT *,'Stop of execution';  PRINT *;  STOP
  ENDIF
  WRITE(86,102) CHAR1(:I3-1)          !  Control string
  CHAR1=CHAR1(I3+1:)
ENDDO
I11=INDEX(CHAR1,'\')
DO
  IF(I11 == 0) EXIT
  IF(VERIFY(CHAR1(I11+1:I11+3),'01234567') /= 0) THEN
    PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
    PRINT *,"char='",CHAR,"'"
    PRINT *,'The letter next \ must be f, u, l, e, s or 3 octal figures.'
    PRINT *,'Stop of execution';  PRINT *;  STOP
  ENDIF
  I12=INDEX(CHAR1(I11+1:),'\');  IF(I12 == 0) EXIT;  I11=I12+I11
ENDDO
IF(LEN_TRIM(CHAR1) >= 1) WRITE(86,102) TRIM(CHAR1)//'\' ! Letters to be printed
ENDFILE 86;  REWIND 86
! Calculation of the length of the string = xxdis = a variable of PostScript
102 FORMAT(A,F9.2,A)
WRITE(88,102) '/xxdis 0 def '
ISUF=0    ! isuf=0: outside subscripts,  isuf=1,2: inside subscripts
NUMSUF=0  ! numsuf: the total of subscripts.
DO
  READ(86,102,END=50) CHAR3
  IF(IACHARD(CHAR3(1:2)) == -1) THEN ! processing control strings
    SELECT CASE(CHAR3(2:2))
      CASE('f')
        IF(LEN_TRIM(CHAR3) <= 2) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'There is no figure after \f.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        IF(VERIFY(CHAR3(3:),' 0123456789') /= 0) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'There is a letter other than figures after \f.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        READ(CHAR3(3:),*) KINDCHAR
        IF(KINDCHAR <=0 .OR. KINDCHAR >= 13) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'The font number is invalid.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        CHARSIZE=HEIGHTD*DCHARFACT(KINDCHAR) ! =character size in scalefont
        CHARSIZESUB=CHARSIZE*FACTSUBF        ! =sharsize of subscripts
        CHARSIZE_PRNT=CHARSIZE
        IF(ISUF >= 1) CHARSIZE_PRNT=CHARSIZESUB
        CALL PRINTCHAR(CHARSIZE_PRNT)
      CASE('s')
        IF(LEN_TRIM(CHAR3) <= 2) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'There is no figure after \s.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        IF(VERIFY(CHAR3(3:),' +-0123456789') /= 0) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'There is a letter other than figures between \s and }.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        READ(CHAR3(3:),*) KSP;  SP=(KSP/FACTSPACE)*HEIGHTD1
        SELECT CASE(ISUF)
          CASE(0)
            WRITE(88,102) '/xxdis xxdis ',SP,' add def'
          CASE(1:2)
            WRITE(88,102) '/xxdis'//CHSUF//' xxdis'//CHSUF,SP,' add def'
          CASE DEFAULT;  STOP 'Error (1) in pcwrite of Plotter'
        END SELECT
      CASE('u','l');  ISUF=ISUF+1
        IF(ISUF == 1) THEN;  NUMSUF=NUMSUF+1
          WRITE(CHARS,'(I2.2)') NUMSUF
          WRITE(88,*) '/xxdis'//CHARS//'1 0 def '
          WRITE(88,*) '/xxdis'//CHARS//'2 0 def '
        ENDIF
        WRITE(CHSUF,'(I2.2,I1.1)') NUMSUF,ISUF
        CALL PRINTCHAR(CHARSIZESUB)
        IF(LEN_TRIM(CHAR3) >= 3) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'The letter next \u or \l must be }.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
      CASE('e')
        IF(LEN_TRIM(CHAR3) >= 3) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'The letter next \e must be }.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        CALL PRINTCHAR(CHARSIZE)
        WRITE(CHARS,'(I2.2)') NUMSUF
        WRITE(88,*) ' xxdis'//CHARS//'1 xxdis'//CHARS//'2 2 copy lt &
             &{ exch } if pop xxdis add /xxdis exch def'
        IF(ISUF <= 0) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'There is not \u} nor \l} before \e}.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        IF(ISUF >= 3) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'The total number of \u} and \l} before \e}&
            & must be less than 3.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        ISUF=0
      CASE DEFAULT;  STOP 'Error (2) in pcwrite of Plotter'
    END SELECT
  ELSE    ! Processing characters to be written
    I2=INDEX(CHAR3,'\ ')
    IF(ISUF == 0) THEN
      IF(KINDCHAR <= 10) THEN
        WRITE(88,102) '/xxdis '
        CALL WRITEASCII
        WRITE(88,102) ' stringwidth pop xxdis add def'
      ELSE
        IF(MOD(I2,2) == 0) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'There is an ASCII code in the Japanese sentence.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        WRITE(88,102) '/xxdis '
        CALL SHIFTKANJI
        WRITE(88,102) ' stringwidth pop xxdis add def'
      ENDIF
    ELSE
      IF(KINDCHAR <= 10) THEN
        CALL WRITEASCII
        WRITE(88,102) ' stringwidth pop '
        WRITE(88,102) ' xxdis'//CHSUF//' add /xxdis'//CHSUF//' exch def'
      ELSE
        IF(MOD(I2,2) == 0) THEN
          PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
          PRINT *,"char='",CHAR,"'"
          PRINT *,'There is an ASCII code in the Japanese sentence.'
          PRINT *,'Stop of execution';  PRINT *;  STOP
        ENDIF
        CALL SHIFTKANJI
        WRITE(88,102) ' stringwidth pop '
        WRITE(88,102) ' xxdis'//CHSUF//' add /xxdis'//CHSUF//' exch def'
      ENDIF
    ENDIF
  ENDIF
ENDDO
50 CONTINUE
IF(ISUF /= 0) THEN
  PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
  PRINT *,"char='",CHAR,"'"
  PRINT *,'Argument char lacks the index control string \e}.'
  PRINT *,'Stop of execution';  PRINT *;  STOP
ENDIF
! Print of characters
REWIND 86;  ISUF=0;  NUMSUF=0
WRITE(88,'(2F9.2,A)') FACT*X1+X00D,FACT*Y1+Y00D,' moveto'
WRITE(88,'(F9.2,A)') THETA_DEG,' rotate'
SELECT CASE(NXD)
  CASE(0);  XSHIFT=0
  CASE(1);  XSHIFT=-.5
  CASE(2);  XSHIFT=-1
  CASE DEFAULT;  STOP 'Error (3) in pcwrite of Plotter'
END SELECT
WRITE(88,'(A,F9.2,A)') ' xxdis ',XSHIFT,' mul '
SELECT CASE(NYD)
  CASE(-1);  YSHIFT=  (7./21)*HEIGHTD1
  CASE( 0);  YSHIFT=   0
  CASE( 1);  YSHIFT= -(9./21)*HEIGHTD1
  CASE( 2);  YSHIFT=         -HEIGHTD1
  CASE( 3);  YSHIFT=-(25./21)*HEIGHTD1
  CASE DEFAULT;  STOP 'Error (4) in pcwrite of Plotter'
END SELECT
WRITE(88,'(F9.2,A)') YSHIFT,' rmoveto'
KINDCHAR=1
CHARSIZE=HEIGHTD*DCHARFACT(KINDCHAR) !=character size in scalefont
CHARSIZESUB=CHARSIZE*FACTSUBF        ! charsizesub: sharsize of subscripts
CALL PRINTCHAR(CHARSIZE)
DO
  READ(86,102,END=60) CHAR3
  IF(IACHARD(CHAR3(1:2)) == -1) THEN   ! Processing control strings
    SELECT CASE(CHAR3(2:2))
      CASE('f')
        READ(CHAR3(3:),*) KINDCHAR
        CHARSIZE=HEIGHTD*DCHARFACT(KINDCHAR) !=character size in scalefont
        CHARSIZESUB=CHARSIZE*FACTSUBF   ! charsizesub: sharsize of subscripts
        CHARSIZE_PRNT=CHARSIZE
        IF(ISUF >= 1) CHARSIZE_PRNT=CHARSIZESUB
        CALL PRINTCHAR(CHARSIZE_PRNT)
      CASE('s')
        READ(CHAR3(3:),*) KSP;  SP=(KSP/FACTSPACE)*HEIGHTD1
        WRITE(88,'(F9.2,A)') SP,' 0 rmoveto'
      CASE('u')
        IF(ISUF == 0) THEN;  NUMSUF=NUMSUF+1
        WRITE(CHARS,'(I2.2)') NUMSUF;  ENDIF
        IF(ISUF == 1) WRITE(88,102) ' xxdis'//CHSUF//' neg 0  rmoveto'
        ISUF=ISUF+1;  SUFPOS=1
        WRITE(CHSUF,'(I2.2,I1.1)') NUMSUF,ISUF
        CALL PRINTCHAR(CHARSIZESUB)
      CASE('l')
        IF(ISUF == 0) THEN
          NUMSUF=NUMSUF+1;  WRITE(CHARS,'(I2.2)') NUMSUF
        ENDIF
        IF(ISUF == 1) WRITE(88,102) ' xxdis'//CHSUF//' neg 0  rmoveto'
        ISUF=ISUF+1;   SUFPOS=0
        WRITE(CHSUF,'(I2.2,I1.1)') NUMSUF,ISUF
        CALL PRINTCHAR(CHARSIZESUB)
      CASE('e')
        CALL PRINTCHAR(CHARSIZE)
        SELECT CASE(ISUF)
          CASE(1:2)
            WRITE(88,*) ' xxdis'//CHSUF//' neg 0  rmoveto'
            WRITE(88,*) ' xxdis'//CHARS//'1 xxdis'//CHARS//'2 2 copy lt &
              &{ exch } if pop 0 rmoveto'
          CASE DEFAULT;  STOP 'Error (5) in pcwrite of Plotter'
        END SELECT
        ISUF=0
      CASE DEFAULT;  STOP 'Error (6) in pcwrite of Plotter'
    END SELECT
  ELSE ! processing characters to be printing
    IF(ISUF == 0) THEN
      IF(KINDCHAR <= 10) THEN
        CALL WRITEASCII
        WRITE(88,102) ' show'
      ELSE
        CALL SHIFTKANJI
        WRITE(88,102) ' show'
      ENDIF
    ELSE
      A1=(SUFPOS-0.3)*HEIGHTD1
      WRITE(88,102) ' 0 ',A1,' rmoveto'
      IF(KINDCHAR <= 10) THEN
        CALL WRITEASCII
        WRITE(88,102) ' show'
      ELSE
        CALL SHIFTKANJI
        WRITE(88,102) ' show'
      ENDIF
      WRITE(88,102) ' 0 ',-A1,' rmoveto'
    ENDIF
  ENDIF
ENDDO
60  CONTINUE
WRITE(88,'(F9.2,A)') -THETA_DEG,' rotate '
CLOSE(UNIT=86,STATUS='DELETE')
CONTAINS

SUBROUTINE WRITEASCII
! An internal procedure of SUBROUTINE pcwrite
! Including process of ) and (.
! The variable used in common with the host procedure: char,char3
INTEGER:: I6,II,I2
CHARACTER(LEN=200):: CHAR70
CHARACTER(LEN=2):: CHAR2
I2=INDEX(CHAR3,'\ ');  I6=1
DO II=1,I2-1
  WRITE(CHAR2,'(Z2)') ICHAR(CHAR3(II:II))
  IF(.NOT.(('20'<=CHAR2 .AND. CHAR2<='7E') .OR. &
    ('A1'<=CHAR2 .AND. CHAR2<='FE'))) THEN
    PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
    PRINT *,"char='",CHAR,"'"
    PRINT *,'There is a non-ASCII code in the English sentence.'
    PRINT *,'Stop of execution';  PRINT *;  STOP
  ENDIF
  SELECT CASE(CHAR3(II:II))
    CASE('(')
      CHAR70(I6:)='\050'//CHAR3(II+1:)
      I6=I6+4
    CASE(')')
      CHAR70(I6:)='\051'//CHAR3(II+1:)
      I6=I6+4
    CASE DEFAULT
      CHAR70(I6:I6)=CHAR3(II:II)
      I6=I6+1
  END SELECT
ENDDO
WRITE(88,102) '('//CHAR70(:I6-1)//')'
102 FORMAT(A,F9.2,A)
END SUBROUTINE WRITEASCII

SUBROUTINE SHIFTKANJI
! An internal procedure of SUBROUTINE pcwrite
! Processing kanji
! Conversion from a shift JIS code into the JIS code.
! Ensaikuropedia jyohou shori p.474
! The variable used in common with the host procedure: char3
INTEGER:: ID(200),IL,IO,IACH1,I1,I2,IK,IS1,IS2,J1,I,J2
CHARACTER(LEN=300):: CHAR300
IL=0
DO
  IO=IL*2+1
  IACH1=IACHARD(CHAR3(IO:IO+1))
  IF(IACH1 == -1) EXIT
  IF(IACH1 == -2) THEN
    PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
    PRINT *,"char='",CHAR,"'"
    PRINT *,'There is an ASCII code in the Japanese sentence.'
    PRINT *,'Stop of execution';  PRINT *;  STOP
  ENDIF
  I2=MOD(IACH1,256);  I1=IACH1/256
  IK=224;  IF(I1 >= 220) IK=352
  IS1=0;  IS2=0
  IF(I2 <= 158) THEN;  IS1=1;  IS2=95;  ENDIF
  J1=2*I1-IK-IS1;  J2=I2-126+IS2
  IF(I2 <= 158 .AND. J2 >= 96) J2=J2-1
  IF(.NOT.((J1>=33 .AND. J1<=116) .AND. (J2>=33 .AND. J2<=126))) THEN
    PRINT *,'An error in SUBROUTINE pcwrite(x1,y1,char) of Plotter'
    PRINT *,"char='",CHAR,"'"
    PRINT *,'There is a non-kanji code in the Japanese sentence.'
    PRINT *,'Stop of execution';  PRINT *;  STOP
  ENDIF
  IL=IL+1
  ID(IL)=J1*256+J2  !  16*16=256
ENDDO
WRITE(CHAR300,'(200Z5)') (ID(I),I=1,IL)
WRITE(88,'(A)') '<'//TRIM(CHAR300)//'>'
END SUBROUTINE SHIFTKANJI

FUNCTION IACHARD(CHRR2) RESULT(IACHART)
! An internal procedure of SUBROUTINE pcwrite
! Conversion from a hexadecimal number to the decimal number.
! The variable used in common with the host procedure: none
CHARACTER(LEN=2),INTENT(IN):: CHRR2
INTEGER:: IACHART,IK1,IK2
CHARACTER(LEN=4):: CHRR4
WRITE(CHRR4,'(Z4)') ICHAR(CHRR2(1:1))*256+ICHAR(CHRR2(2:2))
READ(CHRR4(1:2),'(Z2)') IK1
IF(IK1 == 92 .AND. SCAN('fules ',CHRR2(2:2)) >= 1) THEN   ! ik1=92 for '\'
  IACHART=-1;  RETURN   ! a control string
ENDIF
READ(CHRR4(3:4),'(Z2)') IK2
IF(IK2 == 92) THEN;  IACHART=-2;  RETURN;  ENDIF          ! ik2=92 for '\'
IACHART=IK1*256+IK2  !  16*16=256
END FUNCTION IACHARD

FUNCTION INDEXD(I) RESULT(INDEXT)
! An internal procedure of SUBROUTINE pcwrite
INTEGER,INTENT(IN):: I
INTEGER:: INDEXT
IF(I == 0) THEN; INDEXT=550;  ELSE;  INDEXT=I;  ENDIF
END FUNCTION INDEXD

SUBROUTINE PRINTCHAR(CHARSIZED)
! An internal procedure of SUBROUTINE pcwrite
! Printing a size and a font of characters.
! The variables used in common with the host procedure: chartype and kindchar
REAL,INTENT(IN):: CHARSIZED
IF(KINDCHAR /= 10) THEN
  WRITE(88,102) '/'//CHARTYPE(KINDCHAR)//' findfont ',CHARSIZED, &
         ' scalefont setfont '
  102 FORMAT(A,F9.2,A)
ELSE
  WRITE(88,105) '/'//CHARTYPE(KINDCHAR),' findfont [ ',CHARSIZED, &
        ' 0 ',CHARSIZED*.25,CHARSIZED,-.045*CHARSIZED,' 0 ] makefont setfont '
  105 FORMAT(A/A,1F9.2,A,3F9.2,A)
ENDIF
END SUBROUTINE PRINTCHAR

END SUBROUTINE PCWRITE

END MODULE ModLib_Plotter
