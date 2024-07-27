
MODULE ModLib_Display_Quad

!** PURPOSE OF THIS MODULE:
  ! Add-on module to DISPMODULE to display selected_real_kind(25) reals
  ! (these are probably 16 bytes and possibly quadruple precision)
  !
  ! This module is obtained by copying the section SINGLE PRECSION PROCEDURES from
  ! dispmodule.f90, replacing sngl with quad, single withe quadruple (only appears
  ! in comments) and cplx with cplq, adding a DECLARATIONS section, and defining
  ! the constant quad as selected_real_kind(25).
  !
  ! Copyright (c) 2008, Kristjan Jonasson, Dept. of Computer Science, University of
  ! Iceland (jonasson@hi.is). This software is free. For details see the file README.

!** USE STATEMENTS:
    USE ModLib_Display_Auxiliary

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: DISP                 ! MAIN ROUTINE OF PACKAGE, "PRETTY-PRINTS" VECTORS AND MATRICES
    PUBLIC  :: TOSTRING             ! CONVERT NUMBERS TO STRINGS

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    INTEGER, PARAMETER :: QUAD = SELECTED_REAL_KIND(25)

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE DISP
        MODULE PROCEDURE DISP_S_QUAD, DISP_TS_QUAD, DISP_V_QUAD
        MODULE PROCEDURE DISP_TV_QUAD, DISP_M_QUAD, DISP_TM_QUAD
        MODULE PROCEDURE DISP_S_CPLQ, DISP_TS_CPLQ, DISP_V_CPLQ
        MODULE PROCEDURE DISP_TV_CPLQ, DISP_M_CPLQ, DISP_TM_CPLQ
    END INTERFACE

    INTERFACE TOSTRING
        MODULE PROCEDURE TOSTRING_QUAD, TOSTRING_F_QUAD
        MODULE PROCEDURE TOSTRING_S_QUAD, TOSTRING_SF_QUAD
        MODULE PROCEDURE TOSTRING_CPLQ, TOSTRING_F_CPLQ
        MODULE PROCEDURE TOSTRING_S_CPLQ, TOSTRING_SF_CPLQ
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

  ! **************************** QUADRUPLE PRECISION PROCEDURES *******************************
  SUBROUTINE DISP_S_QUAD(X, FMT, ADVANCE, DIGMAX, SEP, TRIM, UNIT, ZEROAS)
    ! quadruple precision scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, TRIM, ZEROAS
    REAL(QUAD), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TS_QUAD('', X, FMT, ADVANCE, DIGMAX, SEP, 'left', TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_S_QUAD

  SUBROUTINE DISP_V_QUAD(X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! quadruple precision vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    REAL(QUAD), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    CALL DISP_TV_QUAD('', X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
  END SUBROUTINE DISP_V_QUAD

  SUBROUTINE DISP_M_QUAD(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, DIGMAX, ZEROAS)
    ! quadruple precision matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    REAL(QUAD), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX, LBOUND(:)
    CALL DISP_TM_QUAD('', X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_M_QUAD

  SUBROUTINE DISP_TS_QUAD(TITLE, X, FMT, ADVANCE, DIGMAX, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! quadruple precision scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    REAL(QUAD), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TM_QUAD(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, ADVANCE, DIGMAX, SEP=SEP, STYLE=STYLE, TRIM=TRIM, &
         UNIT=UNIT, ZEROAS=ZEROAS)
  END SUBROUTINE DISP_TS_QUAD

  SUBROUTINE DISP_TV_QUAD(TITLE, X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! quadruple precision vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    REAL(QUAD), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS, DIGMAX)
    IF (SE % ROW) THEN
      CALL DISP_QUAD(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE)
    ELSE
      CALL DISP_QUAD(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE)
    END IF
  END SUBROUTINE DISP_TV_QUAD

  SUBROUTINE DISP_TM_QUAD(TITLE, X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! quadruple precision matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    REAL(QUAD),   INTENT(IN)           :: X(:,:)     ! The matrix to be written
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT        ! Editdit descriptor to use for each matrix element (e.g. 'F5.2')
    INTEGER,      INTENT(IN), OPTIONAL :: UNIT       ! Unit to display on
    INTEGER,      INTENT(IN), OPTIONAL :: DIGMAX     ! Nbr of significant digits for largest abs value in x
    CHARACTER(*), INTENT(IN), OPTIONAL :: ADVANCE    ! 'No' to print next matrix to right of current, otherewiSE 'YES'
    CHARACTER(*), INTENT(IN), OPTIONAL :: SEP        ! Separator between matrix columns (e.g. ", ")
    CHARACTER(*), INTENT(IN), OPTIONAL :: ZEROAS     ! Zeros are replaced with this string if it is not empty
    CHARACTER(*), INTENT(IN), OPTIONAL :: STYLE      ! Style(s): See NOTE 1 below
    CHARACTER(*), INTENT(IN), OPTIONAL :: TRIM       ! 'Auto' (the default) to trim if fmt absent, 'no' for no
    !                                                ! trimming, 'yes' for trimming
    INTEGER,      INTENT(IN), OPTIONAL :: LBOUND(:)  ! Lower bounds of x
    TYPE(SETTINGS) :: SE
    !
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS=ZEROAS, DIGMAX=DIGMAX)
    CALL DISP_QUAD(TITLE, X, SE)
  END SUBROUTINE DISP_TM_QUAD

  SUBROUTINE DISP_QUAD(TITLE, X, SE)
    ! quadruple precision item
    CHARACTER(*),   INTENT(IN)    :: TITLE
    REAL(QUAD),     INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER WID(SIZE(X,2)), NBL(SIZE(X,2))
    CALL FIND_EDITDESC_QUAD(X, SE, WID, NBL) ! determine also SE % w
    CALL TOBOX_QUAD(TITLE, X, SE, WID, NBL)
  END SUBROUTINE DISP_QUAD

  SUBROUTINE TOBOX_QUAD(TITLE, X, SE, WID, NBL)
    ! Write quadruple precision matrix to box
    CHARACTER(*),   INTENT(IN)    :: TITLE   ! title
    REAL(QUAD),     INTENT(IN)    :: X(:,:)  ! item
    TYPE(SETTINGS), INTENT(INOUT) :: SE      ! settings
    INTEGER,        INTENT(INOUT) :: WID(:)  ! widths of columns
    INTEGER,        INTENT(INOUT) :: NBL(:)  ! number of blanks to trim from left
    CHARACTER(SE % W)  :: S(SIZE(X,1))
    INTEGER            :: LIN1, J, WLEFT, M, N, WIDP(SIZE(WID))
    CHARACTER, POINTER :: BOXP(:,:)
    REAL(QUAD)         :: XJ(SIZE(X,1)), H
    M = SIZE(X,1)
    N = SIZE(X,2)
    H = HUGE(X)
    CALL PREPAREBOX(TITLE, SE, M, N, WID, WIDP, LIN1, WLEFT, BOXP)
    DO J=1,N
      XJ = X(:, J)
      IF (M > 0) WRITE(S, SE % ED) XJ
      CALL REPLACE_ZERONANINF(S, SE % ZAS(1:SE % LZAS), XJ == 0, XJ /= XJ, XJ < -H, XJ > H)
      CALL COPYTOBOX(S, LIN1, WID(J), WIDP(J), NBL(J), BOXP,  WLEFT)
      IF (J<N) CALL COPYSEPTOBOX(SE % SEP(1:SE % LSEP), M, LIN1, BOXP,  WLEFT)
    ENDDO
    CALL FINISHBOX(TITLE, SE, BOXP)
  END SUBROUTINE TOBOX_QUAD

  PURE FUNCTION MAXW_QUAD(X, D) RESULT(W)
    ! Find max field width needed (F0.d editing is specified)
    REAL(QUAD), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN) :: D
    INTEGER EXPMAX, EXPMIN, W
    LOGICAL XFINITE(SIZE(X))
    REAL(QUAD) XMAX, XMIN, H
    CHARACTER(12) :: F1, S(2)
    XMIN = 0; XMAX = 0; H = HUGE(H)
    XFINITE = X == X .AND. X >= -H .AND. X <= H ! neither NaN, Inf nor -Inf
    IF (.NOT. ANY(XFINITE)) THEN
      W = 4
    ELSE
      XMAX = MAXVAL(X, MASK=XFINITE)
      XMIN = MINVAL(X, MASK=XFINITE)
      F1 = '(SS,ES9.0E4)'
      WRITE(S,F1) XMAX, XMIN
      READ(S(:)(5:9),'(I5)') EXPMAX, EXPMIN
      W = MAX(0, EXPMAX, EXPMIN) + D + 4
    END IF
    IF (.NOT. ALL(XFINITE)) W = MAX(W, 4)
  END FUNCTION MAXW_QUAD

  SUBROUTINE FIND_EDITDESC_QUAD(X, SE, WID, NBL)
    ! Determine SE % ed, SE % w (unless specified) and wid.
    ! The if-block (*) is for safety: make f wider in case xm is written ok with the
    ! ES format in fmt but overflows with F format (the feature has been tested through
    ! manual changes to the program).
    REAL(QUAD),     INTENT(IN)    :: X(:,:)         ! Item to be written
    TYPE(SETTINGS), INTENT(INOUT) :: SE             ! Settings
    INTEGER,        INTENT(OUT)   :: WID(SIZE(X,2)) ! Widths of individual columns
    INTEGER,        INTENT(OUT)   :: NBL(SIZE(X,2)) ! Blanks to trim from left of individual columns
    INTEGER :: EXPMAX, EXPMIN, WW, DD, DMX
    REAL(QUAD) XMAXV(SIZE(X,2)), XMINV(SIZE(X,2)), XP, XM, H
    CHARACTER(14) :: F1 = '(SS,ESxx.xxE4)'  ! could be ES99.89E4; default is ES14.05E4
    CHARACTER(99) S
    LOGICAL XZERO(SIZE(X,2)), XALLZ(SIZE(X,2)), XFINITE(SIZE(X,1),SIZE(X,2)), XNONN(SIZE(X,2)), XALLN(SIZE(X,2))
    !
    DMX = SE % DMX
    H = HUGE(H)
    XFINITE = X == X .AND. X >= -H .AND. X <= H ! neither NaN, Inf nor -Inf
    IF (SE % W == 0) THEN  ! Edit descriptor 'F0.d' specified
      WW = MAXW_QUAD(RESHAPE(X, (/SIZE(X)/)), SE % D)
      IF (SE % LZAS > 0 .AND. ANY(X == 0._QUAD))  WW = MAX(WW, SE % LZAS)
      CALL REPLACE_W(SE % ED, WW)
      SE % W = WW
    ELSEIF (SE % W < 0) THEN ! No edit descriptor specified
      IF (SIZE(X) == 0) THEN
        SE % W = 0
        WID = 0
        NBL = 0
        RETURN
      ENDIF
      IF (ANY(XFINITE)) THEN
        XP = MAXVAL(X, MASK=XFINITE)
        XM = MINVAL(X, MASK=XFINITE)
        WRITE(F1(7:11), '(SS,I2,".",I2.2)') DMX + 8, DMX - 1
        WRITE(S,F1) XP; READ(S(DMX+4:DMX+8),'(I5)') EXPMAX
        WRITE(S,F1) XM; READ(S(DMX+4:DMX+8),'(I5)') EXPMIN
        CALL FIND_EDITDESC_REAL(EXPMAX, EXPMIN, DMX,  SE % ED, WW, DD, XM >= 0)
        IF (.NOT. ALL(XFINITE))                     WW = MAX(WW, 4)
        IF (SE % LZAS > 0 .AND. ANY(X == 0._QUAD))  WW = MAX(WW, SE % LZAS)
        IF (SE % ED(5:5)=='F') THEN  ! (*)
          WRITE(S, SE % ED) XP; IF (S(1:1) == '*') WW = WW + 1
          WRITE(S, SE % ED) XM; IF (S(1:1) == '*') WW = WW + 1
          WRITE(SE % ED(6:10), '(SS,I2,".",I2)') WW, DD
        ENDIF
      ELSE
        WW = 4
        SE % ED = '(F4.0)'
      ENDIF
      SE % W = WW
    ENDIF
    IF (SE % TRM) THEN
      XMAXV = MAXVAL(X, 1, MASK=XFINITE)  ! max in each column
      XMINV = MINVAL(X, 1, MASK=XFINITE)  ! min
      XZERO = ANY(X == 0._QUAD, 1) ! true where column has some zeros
      XALLZ = ALL(X == 0._QUAD, 1) ! true where column has only zeros
      XNONN = ANY(X > H .OR. X < -H .OR. X /= X, 1)  ! true where column has some nonnormals (inf, -inf, nan)
      XALLN = ALL(X > H .OR. X < -H .OR. X /= X, 1)  ! true where column has only nonnormals (inf, -inf, nan)
      CALL GETWID_QUAD(XMAXV, XMINV, XZERO, XALLZ, XNONN, XALLN, SE,  WID, NBL)
    ELSE
      WID = SE % W
      NBL = 0
    ENDIF
  END SUBROUTINE FIND_EDITDESC_QUAD

  SUBROUTINE GETWID_QUAD(XMAXV, XMINV, XZERO, XALLZ, XNONN, XALLN, SE,  WID, NBL)
    ! determine length of the strings that result when writing with edit descriptor SE%ed a
    ! vector v where v(i) is xmaxv(i) or xminv(i) depending on which gives longer output
    REAL(QUAD),     INTENT(IN)  :: XMAXV(:), XMINV(:) ! max and min values in each column
    LOGICAL,        INTENT(IN)  :: XZERO(:), XALLZ(:) ! true for columns with some/all zeros
    LOGICAL,        INTENT(IN)  :: XNONN(:), XALLN(:) ! true for columns with some/all nonnormals
    TYPE(SETTINGS), INTENT(IN)  :: SE                 ! settings
    INTEGER,        INTENT(OUT) :: WID(:)             ! widths of columns
    INTEGER,        INTENT(OUT) :: NBL(:)             ! number of blanks to peel from left (w-wid)
    CHARACTER(SE % W) :: STMAX(SIZE(XMAXV)), STMIN(SIZE(XMAXV))
    INTEGER W
    W = SE % W
    WRITE(STMIN, SE % ED) XMINV
    WRITE(STMAX, SE % ED) XMAXV
    NBL = MOD(VERIFY(STMIN, ' ') + W, W + 1) ! loc. of first nonblank
    NBL = MIN(NBL, MOD(VERIFY(STMAX, ' ') + W, W + 1))
    IF (SE % GEDIT) THEN
      WID = W
    ELSE
      WID = LEN_TRIM(ADJUSTL(STMIN))
      WID = MAX(WID, LEN_TRIM(ADJUSTL(STMAX)))
    ENDIF
    IF (SE % LZAS > 0) THEN
      WID = MERGE(SE % LZAS, WID, XALLZ)
      WID = MAX(WID, MERGE(SE % LZAS, 0, XZERO))
    ENDIF
    WID = MERGE(4, WID, XALLN)
    WID = MAX(WID, MERGE(4, 0, XNONN))
    NBL = W - WID
  END SUBROUTINE GETWID_QUAD

  ! ******** TOSTRING QUADRUPLE PRECISION PROCEDURES ***********
  PURE FUNCTION WIDTHMAX_QUAD(X, FMT) RESULT(W)
    ! Maximum width of an element of x
    REAL(QUAD), INTENT(IN)   :: X(:)
    CHARACTER(*), INTENT(IN) :: FMT
    CHARACTER(NNBLK(FMT)+5)  :: FMT1
    INTEGER W, D
    LOGICAL GEDIT
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN ! illegal format, use 1
      W = 1
    ELSEIF (W == 0) THEN
      W = MAXW_QUAD(X, D)
    ENDIF
  END FUNCTION WIDTHMAX_QUAD

  PURE FUNCTION LEN_F_QUAD(X, FMT) RESULT(WTOT)
    ! Total length of returned string, vector s
    REAL(QUAD), INTENT(IN)           :: X(:)
    CHARACTER(*), INTENT(IN)         :: FMT
    CHARACTER(WIDTHMAX_QUAD(X, FMT)) :: SA(SIZE(X))
    INTEGER                          :: WTOT, W, D, WW
    LOGICAL                          :: GEDIT
    CHARACTER(NNBLK(FMT)+8)          :: FMT1  !(5 for readfmt and 3 for replace_w)
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    IF (W == 0) THEN
      WW = MAXW_QUAD(X, D)
      CALL REPLACE_W(FMT1, WW)
    ENDIF
    WRITE(SA, FMT1) X
    CALL TRIM_REAL(SA, GEDIT, W)
    WTOT = SUM(LEN_TRIM(SA)) + (SIZE(X) - 1)*(TOSSET % SEPLEN)
  END FUNCTION LEN_F_QUAD

  FUNCTION TOSTRING_S_QUAD(X) RESULT(ST)
    ! Scalar to string
    REAL(QUAD), INTENT(IN) :: X
    CHARACTER(LEN_F_QUAD((/X/), TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_QUAD((/X/), TOSSET % RFMT)
  END FUNCTION TOSTRING_S_QUAD

  FUNCTION TOSTRING_SF_QUAD(X, FMT) RESULT(ST)
    ! Scalar with specified format to string
    REAL(QUAD),   INTENT(IN) :: X
    CHARACTER(*), INTENT(IN) :: FMT
    CHARACTER(LEN_F_QUAD((/X/), FMT)) :: ST
    ST = TOSTRING_F_QUAD((/X/), FMT)
  END FUNCTION TOSTRING_SF_QUAD

  FUNCTION TOSTRING_QUAD(X) RESULT(ST)
    ! Vector to string
    REAL(QUAD), INTENT(IN) :: X(:)
    CHARACTER(LEN_F_QUAD(X, TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_QUAD(X, TOSSET % RFMT)
  END FUNCTION TOSTRING_QUAD

  FUNCTION TOSTRING_F_QUAD(X, FMT) RESULT(ST)
    ! Vector with specified format to string
    REAL(QUAD)    ,       INTENT(IN) :: X(:)
    CHARACTER(*),         INTENT(IN) :: FMT
    CHARACTER(LEN_F_QUAD(X, FMT))    :: ST
    CHARACTER(WIDTHMAX_QUAD(X, FMT)) :: SA(SIZE(X))
    CHARACTER(NNBLK(FMT)+8)          :: FMT1  !(5 for readfmt and 3 for replace_w)
    INTEGER                          :: W, D, WW
    LOGICAL                          :: GEDIT
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN
      ST = ERRORMSG
      RETURN
    ELSEIF (W == 0) THEN
      WW = MAXW_QUAD(X, D)
      CALL REPLACE_W(FMT1, WW)
    ENDIF
    WRITE(SA, FMT1) X
    CALL TRIM_REAL(SA, GEDIT, W)
    CALL TOSTRING_GET(SA, ST)
  END FUNCTION TOSTRING_F_QUAD
  ! *************************************** END OF QUADRUPLE PRECISION PROCEDURES ***************************************

  ! *************************************** QUADRUPLE PRECISION COMPLEX PROCEDURES **************************************
  SUBROUTINE DISP_S_CPLQ(X, FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP, TRIM, UNIT)
    ! quadruple precision complex scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, TRIM
    COMPLEX(QUAD), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TS_CPLQ('', X, FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP, 'left', TRIM, UNIT)
  END SUBROUTINE DISP_S_CPLQ

  SUBROUTINE DISP_V_CPLQ(X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! quadruple precision complex vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM, ORIENT
    COMPLEX(QUAD), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    CALL DISP_TV_CPLQ('', X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
  END SUBROUTINE DISP_V_CPLQ

  SUBROUTINE DISP_M_CPLQ(X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! quadruple precision complex matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM
    COMPLEX(QUAD), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX, LBOUND(:)
    CALL DISP_TM_CPLQ('', X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT)
  END SUBROUTINE DISP_M_CPLQ

  SUBROUTINE DISP_TS_CPLQ(TITLE, X, FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP, STYLE, TRIM, UNIT)
    ! quadruple precision complex scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM
    COMPLEX(QUAD), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TM_CPLQ(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP=SEP, STYLE=STYLE, &
                                                       TRIM=TRIM, UNIT=UNIT)
  END SUBROUTINE DISP_TS_CPLQ

  SUBROUTINE DISP_TV_CPLQ(TITLE, X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! quadruple precision complex vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM, ORIENT
    COMPLEX(QUAD), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    TYPE(SETTINGS) SE, SEIM
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, DIGMAX=DIGMAX)
    IF (PRESENT(FMT_IMAG)) THEN
      IF (.NOT.PRESENT(FMT)) THEN
        CALL DISP_ERRMSG('DISP: error, FMT must be present if FMT_IMAG is present'); RETURN;
      ENDIF
      CALL GET_SE(SEIM, TITLE, SHAPE(X), FMT_IMAG)
    ELSE
      SEIM = SE
    END IF
    IF (SE % ROW) THEN
      CALL DISP_CPLQ(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE, SEIM, N = SIZE(X))
    ELSE
      CALL DISP_CPLQ(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE, SEIM, N = 1)
    END IF
  END SUBROUTINE DISP_TV_CPLQ

  SUBROUTINE DISP_TM_CPLQ(TITLE, X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! quadruple precision complex matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    COMPLEX(QUAD),  INTENT(IN)         :: X(:,:)     ! The matrix to be written
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT        ! Edit descriptor for each element (real element when fmt_IMAG &
    !                                                ! is present)
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT_IMAG   ! Edit descriptor for each imaginary element
    INTEGER,      INTENT(IN), OPTIONAL :: UNIT       ! Unit to display on
    INTEGER,      INTENT(IN), OPTIONAL :: DIGMAX     ! Nbr of significant digits for largest abs value in real(X) &
    !                                                ! and aimag(x)
    CHARACTER(*), INTENT(IN), OPTIONAL :: ADVANCE    ! 'No' to print next matrix to right of current, otherewiSE 'YES'
    CHARACTER(*), INTENT(IN), OPTIONAL :: SEP        ! Separator between matrix columns (e.g. ", ")
    CHARACTER(*), INTENT(IN), OPTIONAL :: STYLE      ! Style(s): See NOTE 1 below
    CHARACTER(*), INTENT(IN), OPTIONAL :: TRIM       ! 'Auto' (the default) to trim if fmt absent, 'no' for no
    !                                                ! trimming, 'yes' for trimming
    INTEGER,      INTENT(IN), OPTIONAL :: LBOUND(:)  ! Lower bounds of x
    !
    TYPE(SETTINGS) :: SE, SEIM
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, DIGMAX=DIGMAX)
    IF (PRESENT(FMT_IMAG)) THEN
      IF (.NOT.PRESENT(FMT)) THEN
        CALL DISP_ERRMSG('DISP: error, FMT must be present if FMT_IMAG is present'); RETURN
      ENDIF
      CALL GET_SE(SEIM, TITLE, SHAPE(X), FMT_IMAG)
    ELSE
      SEIM = SE
    END IF
    CALL DISP_CPLQ(TITLE, X, SE, SEIM, N = SIZE(X,2))
  END SUBROUTINE DISP_TM_CPLQ

  SUBROUTINE DISP_CPLQ(TITLE, X, SE, SEIM, N)
    ! quadruple precision item
    CHARACTER(*),   INTENT(IN)    :: TITLE
    COMPLEX(QUAD),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE, SEIM
    INTEGER,        INTENT(IN)    :: N
    INTEGER, DIMENSION(N) :: WIDRE(N), WIDIM(N), NBLRE(N), NBLIM(N)
    CALL FIND_EDITDESC_QUAD(REAL(X), SE, WIDRE, NBLRE)         ! determine also SE % w
    CALL FIND_EDITDESC_QUAD(ABS(AIMAG(X)), SEIM, WIDIM, NBLIM) ! determine also SEim % w
    CALL TOBOX_CPLQ(TITLE, X, SE, SEIM, WIDRE, WIDIM, NBLRE, NBLIM, M = SIZE(X,1), N = SIZE(X,2))
  END SUBROUTINE DISP_CPLQ

  SUBROUTINE TOBOX_CPLQ(TITLE, X, SE, SEIM, WIDRE, WIDIM, NBLRE, NBLIM, M, N)
    ! Write quadruple precision complex matrix to box
    CHARACTER(*),   INTENT(IN)    :: TITLE
    COMPLEX(QUAD),  INTENT(IN)    :: X(:,:)
    INTEGER,        INTENT(IN)    :: M, N, WIDRE(:), WIDIM(:), NBLRE(:), NBLIM(:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE, SEIM
    CHARACTER(SE % W)   :: S(M)
    CHARACTER(SEIM % W) :: SIM(M)
    CHARACTER(3)        :: SGN(M)
    INTEGER             :: LIN1, I, J, WLEFT, WID(N), WIDP(N)
    CHARACTER, POINTER  :: BOXP(:,:)
    SE % ZAS = ''
    SEIM % ZAS = ''
    WID = WIDRE + WIDIM + 4
    CALL PREPAREBOX(TITLE, SE, M, N, WID, WIDP, LIN1, WLEFT, BOXP)
    DO J=1,N
      IF (M > 0) WRITE(S, SE % ED) (REAL(X(I,J)), I=1,M)
      CALL COPYTOBOX(S, LIN1, WIDRE(J), WIDP(J) - WIDIM(J) - 4, NBLRE(J), BOXP,  WLEFT)
      DO I=1,M
        IF (AIMAG(X(I,J)) < 0) THEN; SGN(I) = ' - '; ELSE; SGN(I) = ' + '; ENDIF
        ENDDO
      CALL COPYTOBOX(SGN, LIN1, 3, 3, 0, BOXP,  WLEFT)
      IF (M > 0) WRITE(SIM, SEIM % ED) (ABS(AIMAG(X(I,J))), I=1,M)
      CALL COPYTOBOX(SIM, LIN1, WIDIM(J), WIDIM(J), NBLIM(J), BOXP,  WLEFT)
      CALL COPYSEPTOBOX('i', M, LIN1, BOXP, WLEFT)
      IF (J<N) CALL COPYSEPTOBOX(SE % SEP(1:SE % LSEP), M, LIN1, BOXP,  WLEFT)
    ENDDO
    CALL FINISHBOX(TITLE, SE, BOXP)
  END SUBROUTINE TOBOX_CPLQ

  ! ******* TOSTRING QUADRUPLE PRECISION COMPLEX PROCEDURES ********

  PURE FUNCTION LEN_S_CPLQ(X, FMT) RESULT(WTOT)
    COMPLEX(QUAD), INTENT(IN) :: X
    CHARACTER(*), INTENT(IN)  :: FMT
    INTEGER                   :: WTOT, W, D
    LOGICAL                   :: GEDIT
    CHARACTER(NNBLK(FMT)+8)   :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    WTOT = LEN_F_QUAD((/REAL(X)/), FMT) + LEN_F_QUAD((/ABS(AIMAG(X))/), FMT) + 4
  END FUNCTION LEN_S_CPLQ

  PURE FUNCTION LEN_F_CPLQ(X, FMT) RESULT(WTOT)
    COMPLEX(QUAD), INTENT(IN) :: X(:)
    CHARACTER(*), INTENT(IN)  :: FMT
    INTEGER                   :: WTOT, W, D
    LOGICAL                   :: GEDIT
    CHARACTER(NNBLK(FMT)+8)   :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    WTOT = LEN_F_QUAD(REAL(X), FMT) + LEN_F_QUAD(ABS(AIMAG(X)), FMT) + SIZE(X)*4 - (SIZE(X) - 1)*(TOSSET % SEPLEN)
    ! subtract seplen because it has been added twice in len_f_quad
  END FUNCTION LEN_F_CPLQ
  ! *************************************** END OF QUADRUPLE PRECISION COMPLEX PROCEDURES ********************************

  FUNCTION TOSTRING_S_CPLQ(X) RESULT(ST)
    COMPLEX(QUAD), INTENT(IN)                   :: X
    CHARACTER(LEN_S_CPLQ(X, TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_CPLQ((/X/), TOSSET % RFMT)
  END FUNCTION TOSTRING_S_CPLQ

  FUNCTION TOSTRING_SF_CPLQ(X, FMT) RESULT(ST)
    COMPLEX(QUAD),  INTENT(IN)        :: X
    CHARACTER(*), INTENT(IN)          :: FMT
    CHARACTER(LEN_S_CPLQ(X, FMT)) :: ST
    ST = TOSTRING_F_CPLQ((/X/), FMT)
  END FUNCTION TOSTRING_SF_CPLQ

  FUNCTION TOSTRING_CPLQ(X) RESULT(ST)
    COMPLEX(QUAD), INTENT(IN)               :: X(:)
    CHARACTER(LEN_F_CPLQ(X, TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_CPLQ(X, TOSSET % RFMT)
  END FUNCTION TOSTRING_CPLQ

  FUNCTION TOSTRING_F_CPLQ(X, FMT) RESULT(ST)
    COMPLEX(QUAD),  INTENT(IN)                    :: X(:)
    CHARACTER(*),   INTENT(IN)                    :: FMT
    CHARACTER(LEN_F_CPLQ(X, FMT))                 :: ST
    CHARACTER(WIDTHMAX_QUAD(REAL(X), FMT))        :: SAR(SIZE(X))
    CHARACTER(WIDTHMAX_QUAD(ABS(X-REAL(X)), FMT)) :: SAI(SIZE(X))  ! x-real(x) instead of aimag(x) to enable tHE FNCTION
    CHARACTER(1)                                  :: SGN(SIZE(X))  ! to pass -stand:f95 switch of the ifort coMPILER.
    INTEGER                                       :: W, D, WR, WI, I
    LOGICAL                                       :: GEDIT
    CHARACTER(NNBLK(FMT)+8)                       :: FMT1  !(5 for readfmt and 3 for replace_w)
    REAL(QUAD)                                    :: XRE(SIZE(X)), XIM(SIZE(X)), H
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    XRE = REAL(X)
    XIM = AIMAG(X)
    H = HUGE(H)
    IF (W < 0) THEN
      ST = ERRORMSG
      RETURN
    ELSEIF (W == 0) THEN
      WR = MAXW_QUAD(XRE, D)
      WI = MAXW_QUAD(XIM, D)
      CALL REPLACE_W(FMT1, MAX(WR, WI))
    ENDIF
    WRITE(SAR, FMT1) REAL(X)
    WRITE(SAI, FMT1) ABS(AIMAG(X))
    CALL TRIM_REAL(SAR, GEDIT, W)
    CALL TRIM_REAL(SAI, GEDIT, W)
    DO I = 1,SIZE(X); IF (AIMAG(X(I)) < 0) THEN; SGN(I) = '-'; ELSE; SGN(I) = '+'; ENDIF; ENDDO
    CALL TOSTRING_GET_COMPLEX(SAR, SGN, SAI, ST)
  END FUNCTION TOSTRING_F_CPLQ

END MODULE ModLib_Display_Quad

!******************************************************************************
