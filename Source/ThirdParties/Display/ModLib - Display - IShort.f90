
MODULE ModLib_Display_IShort

!** PURPOSE OF THIS MODULE:
  ! Add-on module to DISPMODULE to display 2-byte integers
  ! (assuming that these are obtained with selected_int_kind(4))
  !
  ! This module is obtained by copying the section DEFAULT INTEGER PROCEDURES from
  ! from dispmodule.f90, replacing dint with byt2 and default integer' with 2-byte
  ! integer (only appears in comments), and adding the DECLARATIONS section below.
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
    INTEGER, PARAMETER :: BYT2 = SELECTED_INT_KIND(4)

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE DISP
        MODULE PROCEDURE DISP_S_BYT2, DISP_TS_BYT2, DISP_V_BYT2
        MODULE PROCEDURE DISP_TV_BYT2, DISP_M_BYT2, DISP_TM_BYT2
    END INTERFACE

    INTERFACE TOSTRING
        MODULE PROCEDURE TOSTRING_BYT2, TOSTRING_F_BYT2
        MODULE PROCEDURE TOSTRING_S_BYT2, TOSTRING_SF_BYT2
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

  PURE FUNCTION WIDTHMAX_BYT2(X, FMT) RESULT(W)
    ! Maximum width of string representation of an element in x
    INTEGER(BYT2), INTENT(IN)  :: X(:)
    CHARACTER(*), INTENT(IN) :: FMT
    CHARACTER(RANGE(X)+2) SX(2)
    INTEGER W, D
    LOGICAL GEDIT
    CHARACTER(NNBLK(FMT)+5) :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W<=0) THEN
      WRITE(SX, '(SS,I0)') MAXVAL(X), MINVAL(X)
      W = MAXVAL(LEN_TRIM(SX))
    END IF
  END FUNCTION WIDTHMAX_BYT2
   ! ************************************* END OF 2-BYTE INTEGER PROCEDURES ******************************************

  PURE FUNCTION LEN_F_BYT2(X, FMT) RESULT(WTOT)
    ! Total width of tostring representation of x
    INTEGER(BYT2), INTENT(IN)        :: X(:)
    CHARACTER(*), INTENT(IN)         :: FMT
    CHARACTER(WIDTHMAX_BYT2(X, FMT)) :: SA(SIZE(X))
    INTEGER                          :: WTOT, W, D
    LOGICAL                          :: GEDIT
    CHARACTER(NNBLK(FMT)+5)          :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    WRITE(SA, FMT1) X
    IF (TOSSET % TRIMB == 'YES' .OR. W == 0) SA = ADJUSTL(SA)
    WTOT = SUM(LEN_TRIM(SA)) + (SIZE(X) - 1)*(TOSSET % SEPLEN)
  END FUNCTION LEN_F_BYT2

  ! ******************************** 2-BYTE INTEGER PROCEDURES *******************************
  SUBROUTINE DISP_S_BYT2(X, FMT, ADVANCE, SEP, TRIM, UNIT, ZEROAS)
    ! 2-byte integer scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, TRIM, ZEROAS
    INTEGER(BYT2), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    CALL DISP_TS_BYT2('', X, FMT, ADVANCE, SEP, 'left', TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_S_BYT2

  SUBROUTINE DISP_V_BYT2(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! 2-byte integer vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    INTEGER(BYT2), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TV_BYT2('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
  END SUBROUTINE DISP_V_BYT2

  SUBROUTINE DISP_M_BYT2(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! 2-byte integer matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    INTEGER(BYT2), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TM_BYT2('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_M_BYT2

  SUBROUTINE DISP_TS_BYT2(TITLE, X, FMT, ADVANCE, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! 2-byte integer scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    INTEGER(BYT2), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    CALL DISP_TM_BYT2(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, ADVANCE, SEP=SEP, STYLE=STYLE, TRIM=TRIM, UNIT=UNIT, &
         ZEROAS=ZEROAS)
  END SUBROUTINE DISP_TS_BYT2

  SUBROUTINE DISP_TV_BYT2(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! 2-byte integer vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    INTEGER(BYT2), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    IF (SE % ROW) THEN
      CALL DISP_BYT2(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE)
    ELSE
      CALL DISP_BYT2(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE)
    END IF
  END SUBROUTINE DISP_TV_BYT2

  SUBROUTINE DISP_TM_BYT2(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! 2-byte integer matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    INTEGER(BYT2),INTENT(IN)           :: X(:,:)     ! The matrix to be written
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT        ! Format edit descriptor to use for each matrix element (E.G.'I4')
    INTEGER,      INTENT(IN), OPTIONAL :: UNIT       ! Unit to display on
    CHARACTER(*), INTENT(IN), OPTIONAL :: ADVANCE    ! 'No' to print next matrix to right of current, otherewiSE 'YES'
    CHARACTER(*), INTENT(IN), OPTIONAL :: SEP        ! Separator between matrix columns (e.g. ", ")
    CHARACTER(*), INTENT(IN), OPTIONAL :: ZEROAS     ! Zeros are replaced by this string
    CHARACTER(*), INTENT(IN), OPTIONAL :: STYLE      ! Style(s): See NOTE 1 below
    CHARACTER(*), INTENT(IN), OPTIONAL :: TRIM       ! 'Auto' (the default) to trim if fmt absent, 'no' for no TRIMMING,
    !                                                ! trimming, 'yes' for trimming
    INTEGER,      INTENT(IN), OPTIONAL :: LBOUND(:)  ! Lower bounds of x
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS=ZEROAS)
    CALL DISP_BYT2(TITLE, X, SE)
  END SUBROUTINE DISP_TM_BYT2

  SUBROUTINE DISP_BYT2(TITLE, X, SE)
    ! 2-byte integer item
    CHARACTER(*),   INTENT(IN)    :: TITLE
    INTEGER(BYT2),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER WID(SIZE(X,2)), NBL(SIZE(X,2))
    CALL FIND_EDITDESC_BYT2(X, SE, WID, NBL) ! determine also SE % w
    CALL TOBOX_BYT2(TITLE, X, SE, WID, NBL)
  END SUBROUTINE DISP_BYT2

  SUBROUTINE TOBOX_BYT2(TITLE, X, SE, WID, NBL)
    ! Write 2-byte integer matrix to box
    CHARACTER(*),   INTENT(IN)    :: TITLE
    INTEGER(BYT2),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER,        INTENT(INOUT) :: WID(:)
    INTEGER,        INTENT(INOUT) :: NBL(:)
    CHARACTER(SE % W)  :: S(SIZE(X,1))
    INTEGER            :: LIN1, J, WLEFT, M, N, WIDP(SIZE(WID))
    CHARACTER, POINTER :: BOXP(:,:)
    M = SIZE(X,1)
    N = SIZE(X,2)
    CALL PREPAREBOX(TITLE, SE, M, N, WID, WIDP, LIN1, WLEFT, BOXP)
    DO J=1,N
      IF (M > 0) WRITE(S, SE % ED) X(:,J)
      IF (SE % LZAS > 0) CALL REPLACE_ZERONANINF(S, SE % ZAS(1:SE % LZAS), X(:,J) == 0)
      CALL COPYTOBOX(S, LIN1, WID(J), WIDP(J), NBL(J), BOXP,  WLEFT)
      IF (J<N) CALL COPYSEPTOBOX(SE % SEP(1:SE % LSEP), M, LIN1, BOXP,  WLEFT)
    ENDDO
    CALL FINISHBOX(TITLE, SE, BOXP)
  END SUBROUTINE TOBOX_BYT2

  SUBROUTINE FIND_EDITDESC_BYT2(X, SE, WID, NBL)
    ! Determine SE % ed, SE % w (unless specified) and wid
    INTEGER(BYT2),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER,        INTENT(OUT)   :: WID(SIZE(X,2)), NBL(SIZE(X,2))
    !
    INTEGER(BYT2) XMAXV(SIZE(X,2)), XMINV(SIZE(X,2)), XP, XM
    LOGICAL XZERO(SIZE(X,2)), XALLZ(SIZE(X,2))
    CHARACTER(22) S
    INTEGER WW
    !
    IF (SE % W == 0) THEN
      XP = MAXVAL(X)
      XM = MINVAL(X)
      WRITE(S, '(SS,I0)') XP; WW = LEN_TRIM(S)
      WRITE(S, '(SS,I0)') XM; WW = MAX(WW, LEN_TRIM(S))
      SE % W = MAX(SE % LZAS, WW)
      CALL REPLACE_W(SE % ED, WW)
    ELSEIF (SE % W < 0) THEN ! obtain max-width of x
      IF (SIZE(X) == 0) THEN
        SE % ED = '()'
        SE % W = 0
        WID = 0
        RETURN
      ENDIF
      XP = MAXVAL(X)
      XM = MINVAL(X)
      WRITE(S, '(SS,I0)') XP; WW = LEN_TRIM(S)
      WRITE(S, '(SS,I0)') XM; WW = MAX(WW, LEN_TRIM(S))
      WW = MAX(SE % LZAS, WW)
      SE % ED = '(SS,Ixx)'
      WRITE(SE % ED(6:7), '(SS,I2)') WW
      SE % W = WW
    ENDIF
    IF (SE % TRM) THEN
      XMAXV = MAXVAL(X, 1) ! max in each column
      XMINV = MINVAL(X, 1) ! min
      XZERO = ANY(X == 0_BYT2, 1) ! true where column has some zeros
      XALLZ = ALL(X == 0_BYT2, 1) ! true where column has only zeros
      CALL GETWID_BYT2(XMAXV, XMINV, XZERO, XALLZ, SE,  WID, NBL)
    ELSE
      WID = SE % W
      NBL = 0
    ENDIF
  END SUBROUTINE FIND_EDITDESC_BYT2

  SUBROUTINE GETWID_BYT2(XMAXV, XMINV, XZERO, XALLZ, SE,  WID, NBL)
    INTEGER(BYT2),  INTENT(IN)  :: XMAXV(:), XMINV(:)
    LOGICAL,        INTENT(IN)  :: XZERO(:), XALLZ(:) ! True for columns with some/all zeros
    TYPE(SETTINGS), INTENT(IN)  :: SE                 ! Settings
    INTEGER,        INTENT(OUT) :: WID(:)             ! Widths of columns
    INTEGER,        INTENT(OUT) :: NBL(:)             ! n of blanks to peel from left (w-wid)
    CHARACTER(SE % W) :: STMAX(SIZE(XMAXV)), STMIN(SIZE(XMAXV))
    INTEGER W
    W = SE % W
    WRITE(STMAX, SE % ED) XMAXV
    WRITE(STMIN, SE % ED) XMINV
    NBL = MOD(VERIFY(STMIN, ' ') + W, W + 1) ! loc. of first nonblank
    NBL = MIN(NBL, MOD(VERIFY(STMAX, ' ') + W, W + 1))
    WID = W - NBL
    IF (SE % LZAS > 0) THEN
      WID = MERGE(SE % LZAS, WID, XALLZ)
      WID = MAX(WID, MERGE(SE % LZAS, 0, XZERO))
      NBL = W - WID
    ENDIF
  END SUBROUTINE GETWID_BYT2

  ! ********* 2-BYTE INTEGER TOSTRING PROCEDURES *********
  FUNCTION TOSTRING_S_BYT2(X) RESULT(ST)
    ! Scalar to string
    INTEGER(BYT2), INTENT(IN)                   :: X
    CHARACTER(LEN_F_BYT2((/X/), TOSSET % IFMT)) :: ST
    ST = TOSTRING_F_BYT2((/X/), TOSSET % IFMT)
  END FUNCTION TOSTRING_S_BYT2

  FUNCTION TOSTRING_SF_BYT2(X, FMT) RESULT(ST)
    ! Scalar with specified format to string
    INTEGER(BYT2),INTENT(IN)        :: X
    CHARACTER(*), INTENT(IN)        :: FMT
    CHARACTER(LEN_F_BYT2((/X/), FMT)) :: ST
    ST = TOSTRING_F_BYT2((/X/), FMT)
  END FUNCTION TOSTRING_SF_BYT2

  FUNCTION TOSTRING_BYT2(X) RESULT(ST)
    ! Vector to string
    INTEGER(BYT2), INTENT(IN)               :: X(:)
    CHARACTER(LEN_F_BYT2(X, TOSSET % IFMT)) :: ST
    ST = TOSTRING_F_BYT2(X, TOSSET % IFMT)
  END FUNCTION TOSTRING_BYT2

  FUNCTION TOSTRING_F_BYT2(X, FMT) RESULT(ST)
    ! Vector with specified format to string
    INTEGER(BYT2), INTENT(IN)        :: X(:)
    CHARACTER(*), INTENT(IN)         :: FMT
    CHARACTER(LEN_F_BYT2(X, FMT))    :: ST
    CHARACTER(WIDTHMAX_BYT2(X, FMT)) :: SA(SIZE(X))
    INTEGER                          :: W, D
    LOGICAL                          :: GEDIT
    CHARACTER(NNBLK(FMT)+5)          :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; ST = ERRORMSG; RETURN; ENDIF
    WRITE(SA, FMT1) X
    IF (TOSSET % TRIMB == 'YES' .OR. W == 0) SA = ADJUSTL(SA)
    CALL TOSTRING_GET(SA, ST)
  END FUNCTION TOSTRING_F_BYT2

END MODULE ModLib_Display_IShort

!******************************************************************************
