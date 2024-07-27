
MODULE ModLib_Display_Default

!** PURPOSE OF THIS MODULE:
    ! THIS IS A RENAMED MODULE OF DISPMODULE, A FORTRAN 95 MODULE
    !   FOR PRETTY-PRINTING MATRICES.
    ! Version number 1.02 6-Sept-2008
    !
    ! Copyright (c) 2008, Kristjan Jonasson, Dept. of Computer Science, University of
    ! Iceland (jonasson@hi.is). This software is free. For details see the file README.

!** USE STATEMENTS:
    USE ModLib_Display_Auxiliary

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: DISP                 ! MAIN ROUTINE OF PACKAGE, "PRETTY-PRINTS" VECTORS AND MATRICES
    PUBLIC  :: DISP_SET             ! SUBROUTINE TO CHANGE DEFAULT SETTINGS FOR DISP
    PUBLIC  :: DISP_GET             ! OBTAIN CURRENT DEFAULT SETTINGS
    PUBLIC  :: DISP_SET_FACTORY     ! CALL (WITHOUT PARAMETERS) TO RESTORE ORIGINAL DEFAULT SETTINGS
    PUBLIC  :: TOSTRING             ! CONVERT NUMBERS TO STRINGS
    PUBLIC  :: TOSTRING_SET         ! CHANGE SETTINGS FOR TOSTRING
    PUBLIC  :: TOSTRING_SET_FACTORY ! RESTORE ORIGINAL DEFAULT SETTINGS FOR TOSTRING
    !
    PUBLIC  :: DISP_SETTINGS        ! DERIVED TYPE WITH SETTINGS
    !
    PUBLIC  :: ASTERISK_UNIT        ! CONSTANT TO SPECIFY DISPLAYING ON ASTERISK UNIT (NORMALLY THE SCREEN)
    PUBLIC  :: PUTSTR_UNIT          ! CONSTANT TO SPECIFY THE USE OF SUBROUTINES PUTSTR AND PUTNL TO DISPLAY
    PUBLIC  :: NULL_UNIT            ! CONSTANT TO SPECIFY DISCARDING OF ALL DISPLAYED OUTPUT

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    INTEGER, PARAMETER :: ASTERISK_UNIT = -3  ,&
                          PUTSTR_UNIT   = -2  ,&
                          NULL_UNIT     = -1
    INTEGER, PARAMETER :: DINT = KIND(0)       ! default integer
    INTEGER, PARAMETER :: SNGL = KIND(0.0)     ! single precision (default real)
    INTEGER, PARAMETER :: DBLE = KIND(0D0)     ! double precision
    INTEGER, PARAMETER :: DLOG = KIND(.FALSE.) ! default logical

!** DERIVED TYPE DEFINITIONS
    !

!** INTERFACE DEFINITIONS:
    INTERFACE DISP_SET
        MODULE PROCEDURE DISP_SET, DISP_SET_DS
    END INTERFACE

    INTERFACE DISP
        MODULE PROCEDURE DISP_S_DINT, DISP_TS_DINT, DISP_V_DINT
        MODULE PROCEDURE DISP_TV_DINT, DISP_M_DINT, DISP_TM_DINT
        MODULE PROCEDURE DISP_S_SNGL, DISP_TS_SNGL, DISP_V_SNGL
        MODULE PROCEDURE DISP_TV_SNGL, DISP_M_SNGL, DISP_TM_SNGL
        MODULE PROCEDURE DISP_S_DBLE, DISP_TS_DBLE, DISP_V_DBLE
        MODULE PROCEDURE DISP_TV_DBLE, DISP_M_DBLE, DISP_TM_DBLE
        MODULE PROCEDURE DISP_S_CPLX, DISP_TS_CPLX, DISP_V_CPLX
        MODULE PROCEDURE DISP_TV_CPLX, DISP_M_CPLX, DISP_TM_CPLX
        MODULE PROCEDURE DISP_S_CPLD, DISP_TS_CPLD, DISP_V_CPLD
        MODULE PROCEDURE DISP_TV_CPLD, DISP_M_CPLD, DISP_TM_CPLD
        MODULE PROCEDURE DISP_S_DLOG, DISP_TS_DLOG, DISP_V_DLOG
        MODULE PROCEDURE DISP_TV_DLOG, DISP_M_DLOG, DISP_TM_DLOG
        MODULE PROCEDURE              DISP_TS_DCHR, DISP_V_DCHR
        MODULE PROCEDURE DISP_TV_DCHR, DISP_M_DCHR, DISP_TM_DCHR
    END INTERFACE

    INTERFACE TOSTRING
        MODULE PROCEDURE TOSTRING_DINT, TOSTRING_F_DINT, TOSTRING_S_DINT, TOSTRING_SF_DINT
        MODULE PROCEDURE TOSTRING_DLOG, TOSTRING_F_DLOG, TOSTRING_S_DLOG, TOSTRING_SF_DLOG
        MODULE PROCEDURE TOSTRING_SNGL, TOSTRING_F_SNGL, TOSTRING_S_SNGL, TOSTRING_SF_SNGL
        MODULE PROCEDURE TOSTRING_DBLE, TOSTRING_F_DBLE, TOSTRING_S_DBLE, TOSTRING_SF_DBLE
        MODULE PROCEDURE TOSTRING_CPLX, TOSTRING_F_CPLX, TOSTRING_S_CPLX, TOSTRING_SF_CPLX
        MODULE PROCEDURE TOSTRING_CPLD, TOSTRING_F_CPLD, TOSTRING_S_CPLD, TOSTRING_SF_CPLD
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    !

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

  ! ******************************* SETTING AND GETTING PROCEDURES *************************************
  SUBROUTINE DISP_SET(ADVANCE, DIGMAX, MATSEP, ORIENT, SEP, STYLE, UNIT, ZEROAS)
    ! Change display settings according to individual parameters
    CHARACTER(*), OPTIONAL, INTENT(IN) :: ADVANCE, SEP, MATSEP, ORIENT, STYLE, ZEROAS
    INTEGER, OPTIONAL, INTENT(IN) :: DIGMAX, UNIT
    IF (PRESENT(ADVANCE))    DEFSET % ADVANCE = UPPER(ADVANCE)
    IF (PRESENT(SEP))        DEFSET % SEP = SEP
    IF (PRESENT(SEP))        DEFSET % SEPLEN = MIN(9, LEN(SEP))
    IF (PRESENT(ZEROAS))     DEFSET % ZEROAS = ZEROAS
    IF (PRESENT(ZEROAS))     DEFSET % ZASLEN = MIN(9, LEN(ZEROAS))
    IF (PRESENT(MATSEP))     DEFSET % MATSEP = MATSEP
    IF (PRESENT(MATSEP))     DEFSET % MATSEPLEN = MIN(9, LEN(MATSEP))
    IF (PRESENT(ORIENT))     DEFSET % ORIENT = UPPER(ORIENT)
    IF (PRESENT(STYLE))      DEFSET % STYLE = STYLE
    IF (PRESENT(DIGMAX))     DEFSET % DIGMAX = DIGMAX
    IF (PRESENT(UNIT))       DEFSET % UNIT = UNIT
    CALL CHECK_SETTINGS
  END SUBROUTINE DISP_SET

  SUBROUTINE DISP_SET_FACTORY()
    ! Change display settings to the original default
    DEFSET = FACTORY_SETTINGS
  END SUBROUTINE DISP_SET_FACTORY

  SUBROUTINE AVOID_COMPILER_WARNINGS
    ! Routine that exists only to avoid compiler warnings (due to compiler bugs)
    TYPE(BOXLIST), POINTER :: BOXL_DUMMY1 => NULL(), BOXL_DUMMY2 => NULL()
    TYPE(BOXNODE), POINTER :: BOXN_DUMMY1 => NULL(), BOXN_DUMMY2 => NULL()
    TYPE(TOSTRING_SETTINGS), POINTER :: TS1 => NULL(), TS2 => NULL()
    TS1 => TS2
    TS2 => TS1
    BOXL_DUMMY2 => BOXL_DUMMY1
    BOXL_DUMMY1 => BOXL_DUMMY2
    BOXN_DUMMY2 => BOXN_DUMMY1
    BOXN_DUMMY1 => BOXN_DUMMY2
  END SUBROUTINE AVOID_COMPILER_WARNINGS

  SUBROUTINE TOSTRING_SET(SEP, RFMT, IFMT, TRIMB, TRIMZ)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SEP, RFMT, IFMT, TRIMB, TRIMZ
    IF (PRESENT(SEP))    TOSSET % SEP    = UPPER(SEP)
    IF (PRESENT(SEP))    TOSSET % SEPLEN = MIN(9, LEN(SEP))
    IF (PRESENT(RFMT))   TOSSET % RFMT   = UPPER(RFMT)
    IF (PRESENT(IFMT))   TOSSET % IFMT   = UPPER(IFMT)
    IF (PRESENT(TRIMB))  TOSSET % TRIMB  = UPPER(TRIMB)
    IF (PRESENT(TRIMZ))  TOSSET % TRIMZ  = UPPER(TRIMZ)
    CALL TOSTRING_CHECK_SETTINGS
  END SUBROUTINE TOSTRING_SET

  SUBROUTINE TOSTRING_SET_FACTORY()
    LOGICAL DUMMY
    DUMMY = .FALSE.
    IF (DUMMY) CALL AVOID_COMPILER_WARNINGS
    TOSSET = TOSFAC
  END SUBROUTINE TOSTRING_SET_FACTORY

  SUBROUTINE DISP_SET_DS(SETTINGS)
    ! Change display settings according to the structure "settings"
    TYPE(DISP_SETTINGS), INTENT(IN) :: SETTINGS
    DEFSET = SETTINGS
    CALL CHECK_SETTINGS
  END SUBROUTINE DISP_SET_DS

  FUNCTION DISP_GET() RESULT(DEFS)
    ! Return current display settings
    TYPE(DISP_SETTINGS) :: DEFS
    DEFS = DEFSET
  END FUNCTION DISP_GET

  ! ********************************* DEFAULT INTEGER PROCEDURES ****************************************
  SUBROUTINE DISP_S_DINT(X, FMT, ADVANCE, SEP, TRIM, UNIT, ZEROAS)
    ! Default integer scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, TRIM, ZEROAS
    INTEGER(DINT), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    CALL DISP_TS_DINT('', X, FMT, ADVANCE, SEP, 'left', TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_S_DINT

  SUBROUTINE DISP_V_DINT(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! Default integer vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    INTEGER(DINT), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TV_DINT('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
  END SUBROUTINE DISP_V_DINT

  SUBROUTINE DISP_M_DINT(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! Default integer matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    INTEGER(DINT), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TM_DINT('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_M_DINT

  SUBROUTINE DISP_TS_DINT(TITLE, X, FMT, ADVANCE, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! Default integer scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    INTEGER(DINT), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    CALL DISP_TM_DINT(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, ADVANCE, SEP=SEP, STYLE=STYLE, TRIM=TRIM, UNIT=UNIT, &
         ZEROAS=ZEROAS)
  END SUBROUTINE DISP_TS_DINT

  SUBROUTINE DISP_TV_DINT(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! Default integer vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    INTEGER(DINT), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    IF (SE % ROW) THEN
      CALL DISP_DINT(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE)
    ELSE
      CALL DISP_DINT(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE)
    END IF
  END SUBROUTINE DISP_TV_DINT

  SUBROUTINE DISP_TM_DINT(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! Default integer matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    INTEGER(DINT),INTENT(IN)           :: X(:,:)     ! The matrix to be written
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
    CALL DISP_DINT(TITLE, X, SE)
  END SUBROUTINE DISP_TM_DINT

  SUBROUTINE DISP_DINT(TITLE, X, SE)
    ! Default integer item
    CHARACTER(*),   INTENT(IN)    :: TITLE
    INTEGER(DINT),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER WID(SIZE(X,2)), NBL(SIZE(X,2))
    CALL FIND_EDITDESC_DINT(X, SE, WID, NBL) ! determine also SE % w
    CALL TOBOX_DINT(TITLE, X, SE, WID, NBL)
  END SUBROUTINE DISP_DINT

  SUBROUTINE TOBOX_DINT(TITLE, X, SE, WID, NBL)
    ! Write default integer matrix to box
    CHARACTER(*),   INTENT(IN)    :: TITLE
    INTEGER(DINT),  INTENT(IN)    :: X(:,:)
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
  END SUBROUTINE TOBOX_DINT

  SUBROUTINE FIND_EDITDESC_DINT(X, SE, WID, NBL)
    ! Determine SE % ed, SE % w (unless specified) and wid
    INTEGER(DINT),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER,        INTENT(OUT)   :: WID(SIZE(X,2)), NBL(SIZE(X,2))
    !
    INTEGER(DINT) XMAXV(SIZE(X,2)), XMINV(SIZE(X,2)), XP, XM
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
      XZERO = ANY(X == 0_DINT, 1) ! true where column has some zeros
      XALLZ = ALL(X == 0_DINT, 1) ! true where column has only zeros
      CALL GETWID_DINT(XMAXV, XMINV, XZERO, XALLZ, SE,  WID, NBL)
    ELSE
      WID = SE % W
      NBL = 0
    ENDIF
  END SUBROUTINE FIND_EDITDESC_DINT

  SUBROUTINE GETWID_DINT(XMAXV, XMINV, XZERO, XALLZ, SE,  WID, NBL)
    INTEGER(DINT),  INTENT(IN)  :: XMAXV(:), XMINV(:)
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
  END SUBROUTINE GETWID_DINT

  ! ********* DEFAULT INTEGER TOSTRING PROCEDURES *********

  PURE FUNCTION WIDTHMAX_DINT(X, FMT) RESULT(W)
    ! Maximum width of string representation of an element in x
    INTEGER(DINT), INTENT(IN)  :: X(:)
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
  END FUNCTION WIDTHMAX_DINT

  PURE FUNCTION LEN_F_DINT(X, FMT) RESULT(WTOT)
    ! Total width of tostring representation of x
    INTEGER(DINT), INTENT(IN)        :: X(:)
    CHARACTER(*), INTENT(IN)         :: FMT
    CHARACTER(WIDTHMAX_DINT(X, FMT)) :: SA(SIZE(X))
    INTEGER                          :: WTOT, W, D
    LOGICAL                          :: GEDIT
    CHARACTER(NNBLK(FMT)+5)          :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    WRITE(SA, FMT1) X
    IF (TOSSET % TRIMB == 'YES' .OR. W == 0) SA = ADJUSTL(SA)
    WTOT = SUM(LEN_TRIM(SA)) + (SIZE(X) - 1)*(TOSSET % SEPLEN)
  END FUNCTION LEN_F_DINT

  FUNCTION TOSTRING_S_DINT(X) RESULT(ST)
    ! Scalar to string
    INTEGER(DINT), INTENT(IN)                   :: X
    CHARACTER(LEN_F_DINT((/X/), TOSSET % IFMT)) :: ST
    ST = TOSTRING_F_DINT((/X/), TOSSET % IFMT)
  END FUNCTION TOSTRING_S_DINT

  FUNCTION TOSTRING_SF_DINT(X, FMT) RESULT(ST)
    ! Scalar with specified format to string
    INTEGER(DINT),INTENT(IN)        :: X
    CHARACTER(*), INTENT(IN)        :: FMT
    CHARACTER(LEN_F_DINT((/X/), FMT)) :: ST
    ST = TOSTRING_F_DINT((/X/), FMT)
  END FUNCTION TOSTRING_SF_DINT

  FUNCTION TOSTRING_DINT(X) RESULT(ST)
    ! Vector to string
    INTEGER(DINT), INTENT(IN)               :: X(:)
    CHARACTER(LEN_F_DINT(X, TOSSET % IFMT)) :: ST
    ST = TOSTRING_F_DINT(X, TOSSET % IFMT)
  END FUNCTION TOSTRING_DINT

  FUNCTION TOSTRING_F_DINT(X, FMT) RESULT(ST)
    ! Vector with specified format to string
    INTEGER(DINT), INTENT(IN)        :: X(:)
    CHARACTER(*), INTENT(IN)         :: FMT
    CHARACTER(LEN_F_DINT(X, FMT))    :: ST
    CHARACTER(WIDTHMAX_DINT(X, FMT)) :: SA(SIZE(X))
    INTEGER                          :: W, D
    LOGICAL                          :: GEDIT
    CHARACTER(NNBLK(FMT)+5)          :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; ST = ERRORMSG; RETURN; ENDIF
    WRITE(SA, FMT1) X
    IF (TOSSET % TRIMB == 'YES' .OR. W == 0) SA = ADJUSTL(SA)
    CALL TOSTRING_GET(SA, ST)
  END FUNCTION TOSTRING_F_DINT

  ! ************************************* END OF DEFAULT INTEGER PROCEDURES ******************************************

  ! **************************************** SINGLE PRECISION PROCEDURES *********************************************
  SUBROUTINE DISP_S_SNGL(X, FMT, ADVANCE, DIGMAX, SEP, TRIM, UNIT, ZEROAS)
    ! Single precision scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, TRIM, ZEROAS
    REAL(SNGL), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TS_SNGL('', X, FMT, ADVANCE, DIGMAX, SEP, 'left', TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_S_SNGL

  SUBROUTINE DISP_V_SNGL(X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! Single precision vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    REAL(SNGL), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    CALL DISP_TV_SNGL('', X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
  END SUBROUTINE DISP_V_SNGL

  SUBROUTINE DISP_M_SNGL(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, DIGMAX, ZEROAS)
    ! Single precision matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    REAL(SNGL), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX, LBOUND(:)
    CALL DISP_TM_SNGL('', X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_M_SNGL

  SUBROUTINE DISP_TS_SNGL(TITLE, X, FMT, ADVANCE, DIGMAX, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! Single precision scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    REAL(SNGL), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TM_SNGL(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, ADVANCE, DIGMAX, SEP=SEP, STYLE=STYLE, TRIM=TRIM, &
         UNIT=UNIT, ZEROAS=ZEROAS)
  END SUBROUTINE DISP_TS_SNGL

  SUBROUTINE DISP_TV_SNGL(TITLE, X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! Single precision vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    REAL(SNGL), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS, DIGMAX)
    IF (SE % ROW) THEN
      CALL DISP_SNGL(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE)
    ELSE
      CALL DISP_SNGL(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE)
    END IF
  END SUBROUTINE DISP_TV_SNGL

  SUBROUTINE DISP_TM_SNGL(TITLE, X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! Single precision matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    REAL(SNGL),   INTENT(IN)           :: X(:,:)     ! The matrix to be written
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
    CALL DISP_SNGL(TITLE, X, SE)
  END SUBROUTINE DISP_TM_SNGL

  SUBROUTINE DISP_SNGL(TITLE, X, SE)
    ! Single precision item
    CHARACTER(*),   INTENT(IN)    :: TITLE
    REAL(SNGL),     INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER WID(SIZE(X,2)), NBL(SIZE(X,2))
    CALL FIND_EDITDESC_SNGL(X, SE, WID, NBL) ! determine also SE % w
    CALL TOBOX_SNGL(TITLE, X, SE, WID, NBL)
  END SUBROUTINE DISP_SNGL

  SUBROUTINE TOBOX_SNGL(TITLE, X, SE, WID, NBL)
    ! Write single precision matrix to box
    CHARACTER(*),   INTENT(IN)    :: TITLE   ! title
    REAL(SNGL),     INTENT(IN)    :: X(:,:)  ! item
    TYPE(SETTINGS), INTENT(INOUT) :: SE      ! settings
    INTEGER,        INTENT(INOUT) :: WID(:)  ! widths of columns
    INTEGER,        INTENT(INOUT) :: NBL(:)  ! number of blanks to trim from left
    CHARACTER(SE % W)  :: S(SIZE(X,1))
    INTEGER            :: LIN1, J, WLEFT, M, N, WIDP(SIZE(WID))
    CHARACTER, POINTER :: BOXP(:,:)
    REAL(SNGL)         :: XJ(SIZE(X,1)), H
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
  END SUBROUTINE TOBOX_SNGL

  PURE FUNCTION MAXW_SNGL(X, D) RESULT(W)
    ! Find max field width needed (F0.d editing is specified)
    REAL(SNGL), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN) :: D
    INTEGER EXPMAX, EXPMIN, W
    LOGICAL XFINITE(SIZE(X))
    REAL(SNGL) XMAX, XMIN, H
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
  END FUNCTION MAXW_SNGL

  SUBROUTINE FIND_EDITDESC_SNGL(X, SE, WID, NBL)
    ! Determine SE % ed, SE % w (unless specified) and wid.
    ! The if-block (*) is for safety: make f wider in case xm is written ok with the
    ! ES format in fmt but overflows with F format (the feature has been tested through
    ! manual changes to the program).
    REAL(SNGL),     INTENT(IN)    :: X(:,:)         ! Item to be written
    TYPE(SETTINGS), INTENT(INOUT) :: SE             ! Settings
    INTEGER,        INTENT(OUT)   :: WID(SIZE(X,2)) ! Widths of individual columns
    INTEGER,        INTENT(OUT)   :: NBL(SIZE(X,2)) ! Blanks to trim from left of individual columns
    INTEGER :: EXPMAX, EXPMIN, WW, DD, DMX
    REAL(SNGL) XMAXV(SIZE(X,2)), XMINV(SIZE(X,2)), XP, XM, H
    CHARACTER(14) :: F1 = '(SS,ESxx.xxE4)'  ! could be ES99.89E4; default is ES14.05E4
    CHARACTER(99) S
    LOGICAL XZERO(SIZE(X,2)), XALLZ(SIZE(X,2)), XFINITE(SIZE(X,1),SIZE(X,2)), XNONN(SIZE(X,2)), XALLN(SIZE(X,2))
    !
    DMX = SE % DMX
    H = HUGE(H)
    XFINITE = X == X .AND. X >= -H .AND. X <= H ! neither NaN, Inf nor -Inf
    IF (SE % W == 0) THEN  ! Edit descriptor 'F0.d' specified
      WW = MAXW_SNGL(RESHAPE(X, (/SIZE(X)/)), SE % D)
      IF (SE % LZAS > 0 .AND. ANY(X == 0._SNGL))  WW = MAX(WW, SE % LZAS)
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
        IF (SE % LZAS > 0 .AND. ANY(X == 0._SNGL))  WW = MAX(WW, SE % LZAS)
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
      XZERO = ANY(X == 0._SNGL, 1) ! true where column has some zeros
      XALLZ = ALL(X == 0._SNGL, 1) ! true where column has only zeros
      XNONN = ANY(X > H .OR. X < -H .OR. X /= X, 1)  ! true where column has some nonnormals (inf, -inf, nan)
      XALLN = ALL(X > H .OR. X < -H .OR. X /= X, 1)  ! true where column has only nonnormals (inf, -inf, nan)
      CALL GETWID_SNGL(XMAXV, XMINV, XZERO, XALLZ, XNONN, XALLN, SE,  WID, NBL)
    ELSE
      WID = SE % W
      NBL = 0
    ENDIF
  END SUBROUTINE FIND_EDITDESC_SNGL

  SUBROUTINE GETWID_SNGL(XMAXV, XMINV, XZERO, XALLZ, XNONN, XALLN, SE,  WID, NBL)
    ! determine length of the strings that result when writing with edit descriptor SE%ed a
    ! vector v where v(i) is xmaxv(i) or xminv(i) depending on which gives longer output
    REAL(SNGL),     INTENT(IN)  :: XMAXV(:), XMINV(:) ! max and min values in each column
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
  END SUBROUTINE GETWID_SNGL

  ! ******** TOSTRING SINGLE PRECISION PROCEDURES ***********

  PURE FUNCTION WIDTHMAX_SNGL(X, FMT) RESULT(W)
    ! Maximum width of an element of x
    REAL(SNGL), INTENT(IN)   :: X(:)
    CHARACTER(*), INTENT(IN) :: FMT
    CHARACTER(NNBLK(FMT)+5)  :: FMT1
    INTEGER W, D
    LOGICAL GEDIT
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN ! illegal format, use 1
      W = 1
    ELSEIF (W == 0) THEN
      W = MAXW_SNGL(X, D)
    ENDIF
  END FUNCTION WIDTHMAX_SNGL

  PURE FUNCTION LEN_F_SNGL(X, FMT) RESULT(WTOT)
    ! Total length of returned string, vector s
    REAL(SNGL), INTENT(IN)           :: X(:)
    CHARACTER(*), INTENT(IN)         :: FMT
    CHARACTER(WIDTHMAX_SNGL(X, FMT)) :: SA(SIZE(X))
    INTEGER                          :: WTOT, W, D, WW
    LOGICAL                          :: GEDIT
    CHARACTER(NNBLK(FMT)+8)          :: FMT1  !(5 for readfmt and 3 for replace_w)
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    IF (W == 0) THEN
      WW = MAXW_SNGL(X, D)
      CALL REPLACE_W(FMT1, WW)
    ENDIF
    WRITE(SA, FMT1) X
    CALL TRIM_REAL(SA, GEDIT, W)
    WTOT = SUM(LEN_TRIM(SA)) + (SIZE(X) - 1)*(TOSSET % SEPLEN)
  END FUNCTION LEN_F_SNGL

  FUNCTION TOSTRING_S_SNGL(X) RESULT(ST)
    ! Scalar to string
    REAL(SNGL), INTENT(IN) :: X
    CHARACTER(LEN_F_SNGL((/X/), TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_SNGL((/X/), TOSSET % RFMT)
  END FUNCTION TOSTRING_S_SNGL

  FUNCTION TOSTRING_SF_SNGL(X, FMT) RESULT(ST)
    ! Scalar with specified format to string
    REAL(SNGL),   INTENT(IN) :: X
    CHARACTER(*), INTENT(IN) :: FMT
    CHARACTER(LEN_F_SNGL((/X/), FMT)) :: ST
    ST = TOSTRING_F_SNGL((/X/), FMT)
  END FUNCTION TOSTRING_SF_SNGL

  FUNCTION TOSTRING_SNGL(X) RESULT(ST)
    ! Vector to string
    REAL(SNGL), INTENT(IN) :: X(:)
    CHARACTER(LEN_F_SNGL(X, TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_SNGL(X, TOSSET % RFMT)
  END FUNCTION TOSTRING_SNGL

  FUNCTION TOSTRING_F_SNGL(X, FMT) RESULT(ST)
    ! Vector with specified format to string
    REAL(SNGL)    ,       INTENT(IN) :: X(:)
    CHARACTER(*),         INTENT(IN) :: FMT
    CHARACTER(LEN_F_SNGL(X, FMT))    :: ST
    CHARACTER(WIDTHMAX_SNGL(X, FMT)) :: SA(SIZE(X))
    CHARACTER(NNBLK(FMT)+8)          :: FMT1  !(5 for readfmt and 3 for replace_w)
    INTEGER                          :: W, D, WW
    LOGICAL                          :: GEDIT
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN
      ST = ERRORMSG
      RETURN
    ELSEIF (W == 0) THEN
      WW = MAXW_SNGL(X, D)
      CALL REPLACE_W(FMT1, WW)
    ENDIF
    WRITE(SA, FMT1) X
    CALL TRIM_REAL(SA, GEDIT, W)
    CALL TOSTRING_GET(SA, ST)
  END FUNCTION TOSTRING_F_SNGL

  ! *************************************** END OF SINGLE PRECISION PROCEDURES ***************************************

  ! *************************************** SINGLE PRECISION COMPLEX PROCEDURES **************************************
  SUBROUTINE DISP_S_CPLX(X, FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP, TRIM, UNIT)
    ! single precision complex scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, TRIM
    COMPLEX(SNGL), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TS_CPLX('', X, FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP, 'left', TRIM, UNIT)
  END SUBROUTINE DISP_S_CPLX

  SUBROUTINE DISP_V_CPLX(X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! single precision complex vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM, ORIENT
    COMPLEX(SNGL), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    CALL DISP_TV_CPLX('', X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
  END SUBROUTINE DISP_V_CPLX

  SUBROUTINE DISP_M_CPLX(X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! single precision complex matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM
    COMPLEX(SNGL), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX, LBOUND(:)
    CALL DISP_TM_CPLX('', X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT)
  END SUBROUTINE DISP_M_CPLX

  SUBROUTINE DISP_TS_CPLX(TITLE, X, FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP, STYLE, TRIM, UNIT)
    ! single precision complex scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM
    COMPLEX(SNGL), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TM_CPLX(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP=SEP, STYLE=STYLE, &
                                                       TRIM=TRIM, UNIT=UNIT)
  END SUBROUTINE DISP_TS_CPLX

  SUBROUTINE DISP_TV_CPLX(TITLE, X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! single precision complex vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM, ORIENT
    COMPLEX(SNGL), INTENT(IN) :: X(:)
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
      CALL DISP_CPLX(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE, SEIM, N = SIZE(X))
    ELSE
      CALL DISP_CPLX(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE, SEIM, N = 1)
    END IF
  END SUBROUTINE DISP_TV_CPLX

  SUBROUTINE DISP_TM_CPLX(TITLE, X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! single precision complex matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    COMPLEX(SNGL),  INTENT(IN)         :: X(:,:)     ! The matrix to be written
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
    CALL DISP_CPLX(TITLE, X, SE, SEIM, N = SIZE(X,2))
  END SUBROUTINE DISP_TM_CPLX

  SUBROUTINE DISP_CPLX(TITLE, X, SE, SEIM, N)
    ! Single precision item
    CHARACTER(*),   INTENT(IN)    :: TITLE
    COMPLEX(SNGL),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE, SEIM
    INTEGER,        INTENT(IN)    :: N
    INTEGER, DIMENSION(N) :: WIDRE(N), WIDIM(N), NBLRE(N), NBLIM(N)
    CALL FIND_EDITDESC_SNGL(REAL(X), SE, WIDRE, NBLRE)         ! determine also SE % w
    CALL FIND_EDITDESC_SNGL(ABS(AIMAG(X)), SEIM, WIDIM, NBLIM) ! determine also SEim % w
    CALL TOBOX_CPLX(TITLE, X, SE, SEIM, WIDRE, WIDIM, NBLRE, NBLIM, M = SIZE(X,1), N = SIZE(X,2))
  END SUBROUTINE DISP_CPLX

  SUBROUTINE TOBOX_CPLX(TITLE, X, SE, SEIM, WIDRE, WIDIM, NBLRE, NBLIM, M, N)
    ! Write single precision complex matrix to box
    CHARACTER(*),   INTENT(IN)    :: TITLE
    COMPLEX(SNGL),  INTENT(IN)    :: X(:,:)
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
  END SUBROUTINE TOBOX_CPLX

  ! ******* TOSTRING SINGLE PRECISION COMPLEX PROCEDURES ********

  PURE FUNCTION LEN_S_CPLX(X, FMT) RESULT(WTOT)
    COMPLEX(SNGL), INTENT(IN) :: X
    CHARACTER(*), INTENT(IN)  :: FMT
    INTEGER                   :: WTOT, W, D
    LOGICAL                   :: GEDIT
    CHARACTER(NNBLK(FMT)+8)   :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    WTOT = LEN_F_SNGL((/REAL(X)/), FMT) + LEN_F_SNGL((/ABS(AIMAG(X))/), FMT) + 4
  END FUNCTION LEN_S_CPLX

  PURE FUNCTION LEN_F_CPLX(X, FMT) RESULT(WTOT)
    COMPLEX(SNGL), INTENT(IN) :: X(:)
    CHARACTER(*), INTENT(IN)  :: FMT
    INTEGER                   :: WTOT, W, D
    LOGICAL                   :: GEDIT
    CHARACTER(NNBLK(FMT)+8)   :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    WTOT = LEN_F_SNGL(REAL(X), FMT) + LEN_F_SNGL(ABS(AIMAG(X)), FMT) + SIZE(X)*4 - (SIZE(X) - 1)*(TOSSET % SEPLEN)
    ! subtract seplen because it has been added twice in len_f_sngl
  END FUNCTION LEN_F_CPLX

  FUNCTION TOSTRING_S_CPLX(X) RESULT(ST)
    COMPLEX(SNGL), INTENT(IN)                   :: X
    CHARACTER(LEN_S_CPLX(X, TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_CPLX((/X/), TOSSET % RFMT)
  END FUNCTION TOSTRING_S_CPLX

  FUNCTION TOSTRING_SF_CPLX(X, FMT) RESULT(ST)
    COMPLEX(SNGL),  INTENT(IN)        :: X
    CHARACTER(*), INTENT(IN)          :: FMT
    CHARACTER(LEN_S_CPLX(X, FMT)) :: ST
    ST = TOSTRING_F_CPLX((/X/), FMT)
  END FUNCTION TOSTRING_SF_CPLX

  FUNCTION TOSTRING_CPLX(X) RESULT(ST)
    COMPLEX(SNGL), INTENT(IN)               :: X(:)
    CHARACTER(LEN_F_CPLX(X, TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_CPLX(X, TOSSET % RFMT)
  END FUNCTION TOSTRING_CPLX

  FUNCTION TOSTRING_F_CPLX(X, FMT) RESULT(ST)
    COMPLEX(SNGL),  INTENT(IN)                    :: X(:)
    CHARACTER(*),   INTENT(IN)                    :: FMT
    CHARACTER(LEN_F_CPLX(X, FMT))                 :: ST
    CHARACTER(WIDTHMAX_SNGL(REAL(X), FMT))        :: SAR(SIZE(X))
    CHARACTER(WIDTHMAX_SNGL(ABS(X-REAL(X)), FMT)) :: SAI(SIZE(X))  ! x-real(x) instead of aimag(x) to enable tHE FNCTION
    CHARACTER(1)                                  :: SGN(SIZE(X))  ! to pass -stand:f95 switch of the ifort coMPILER.
    INTEGER                                       :: W, D, WR, WI, I
    LOGICAL                                       :: GEDIT
    CHARACTER(NNBLK(FMT)+8)                       :: FMT1  !(5 for readfmt and 3 for replace_w)
    REAL(SNGL)                                    :: XRE(SIZE(X)), XIM(SIZE(X)), H
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    XRE = REAL(X)
    XIM = AIMAG(X)
    H = HUGE(H)
    IF (W < 0) THEN
      ST = ERRORMSG
      RETURN
    ELSEIF (W == 0) THEN
      WR = MAXW_SNGL(XRE, D)
      WI = MAXW_SNGL(XIM, D)
      CALL REPLACE_W(FMT1, MAX(WR, WI))
    ENDIF
    WRITE(SAR, FMT1) REAL(X)
    WRITE(SAI, FMT1) ABS(AIMAG(X))
    CALL TRIM_REAL(SAR, GEDIT, W)
    CALL TRIM_REAL(SAI, GEDIT, W)
    DO I = 1,SIZE(X); IF (AIMAG(X(I)) < 0) THEN; SGN(I) = '-'; ELSE; SGN(I) = '+'; ENDIF; ENDDO
    CALL TOSTRING_GET_COMPLEX(SAR, SGN, SAI, ST)
  END FUNCTION TOSTRING_F_CPLX

  ! *************************************** END OF SINGLE PRECISION COMPLEX PROCEDURES ********************************

  ! ************************************* DOUBLE PRECISION PROCEDURES (SEE NOTE 2 BELOW) ******************************
  SUBROUTINE DISP_S_DBLE(X, FMT, ADVANCE, DIGMAX, SEP, TRIM, UNIT, ZEROAS)
    ! Double precision scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, TRIM, ZEROAS
    REAL(DBLE), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TS_DBLE('', X, FMT, ADVANCE, DIGMAX, SEP, 'left', TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_S_DBLE

  SUBROUTINE DISP_V_DBLE(X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! Double precision vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    REAL(DBLE), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    CALL DISP_TV_DBLE('', X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
  END SUBROUTINE DISP_V_DBLE

  SUBROUTINE DISP_M_DBLE(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, DIGMAX, ZEROAS)
    ! Double precision matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    REAL(DBLE), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX, LBOUND(:)
    CALL DISP_TM_DBLE('', X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
  END SUBROUTINE DISP_M_DBLE

  SUBROUTINE DISP_TS_DBLE(TITLE, X, FMT, ADVANCE, DIGMAX, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! Double precision scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS
    REAL(DBLE), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TM_DBLE(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, ADVANCE, DIGMAX, SEP=SEP, STYLE=STYLE, TRIM=TRIM, &
         UNIT=UNIT, ZEROAS=ZEROAS)
  END SUBROUTINE DISP_TS_DBLE

  SUBROUTINE DISP_TV_DBLE(TITLE, X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS)
    ! Double precision vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ZEROAS, ORIENT
    REAL(DBLE), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT, ZEROAS, DIGMAX)
    IF (SE % ROW) THEN
      CALL DISP_DBLE(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE)
    ELSE
      CALL DISP_DBLE(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE)
    END IF
  END SUBROUTINE DISP_TV_DBLE

  SUBROUTINE DISP_TM_DBLE(TITLE, X, FMT, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ZEROAS)
    ! Double precision matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    REAL(DBLE),   INTENT(IN)           :: X(:,:)     ! The matrix to be written
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
    CALL DISP_DBLE(TITLE, X, SE)
  END SUBROUTINE DISP_TM_DBLE

  SUBROUTINE DISP_DBLE(TITLE, X, SE)
    ! Double precision item
    CHARACTER(*),   INTENT(IN)    :: TITLE
    REAL(DBLE),     INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER WID(SIZE(X,2)), NBL(SIZE(X,2))
    CALL FIND_EDITDESC_DBLE(X, SE, WID, NBL) ! determine also SE % w
    CALL TOBOX_DBLE(TITLE, X, SE, WID, NBL)
  END SUBROUTINE DISP_DBLE

  SUBROUTINE TOBOX_DBLE(TITLE, X, SE, WID, NBL)
    ! Write double precision matrix to box
    CHARACTER(*),   INTENT(IN)    :: TITLE   ! title
    REAL(DBLE),     INTENT(IN)    :: X(:,:)  ! item
    TYPE(SETTINGS), INTENT(INOUT) :: SE      ! settings
    INTEGER,        INTENT(INOUT) :: WID(:)  ! widths of columns
    INTEGER,        INTENT(INOUT) :: NBL(:)  ! number of blanks to trim from left
    CHARACTER(SE % W)  :: S(SIZE(X,1))
    INTEGER            :: LIN1, J, WLEFT, M, N, WIDP(SIZE(WID))
    CHARACTER, POINTER :: BOXP(:,:)
    REAL(DBLE)         :: XJ(SIZE(X,1)), H
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
  END SUBROUTINE TOBOX_DBLE

  PURE FUNCTION MAXW_DBLE(X, D) RESULT(W)
    ! Find max field width needed (F0.d editing is specified)
    REAL(DBLE), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN) :: D
    INTEGER EXPMAX, EXPMIN, W
    LOGICAL XFINITE(SIZE(X))
    REAL(DBLE) XMAX, XMIN, H
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
  END FUNCTION MAXW_DBLE

  SUBROUTINE FIND_EDITDESC_DBLE(X, SE, WID, NBL)
    ! Determine SE % ed, SE % w (unless specified) and wid.
    ! The if-block (*) is for safety: make f wider in case xm is written ok with the
    ! ES format in fmt but overflows with F format (the feature has been tested through
    ! manual changes to the program).
    REAL(DBLE),     INTENT(IN)    :: X(:,:)         ! Item to be written
    TYPE(SETTINGS), INTENT(INOUT) :: SE             ! Settings
    INTEGER,        INTENT(OUT)   :: WID(SIZE(X,2)) ! Widths of individual columns
    INTEGER,        INTENT(OUT)   :: NBL(SIZE(X,2)) ! Blanks to trim from left of individual columns
    INTEGER :: EXPMAX, EXPMIN, WW, DD, DMX
    REAL(DBLE) XMAXV(SIZE(X,2)), XMINV(SIZE(X,2)), XP, XM, H
    CHARACTER(14) :: F1 = '(SS,ESxx.xxE4)'  ! could be ES99.89E4; default is ES14.05E4
    CHARACTER(99) S
    LOGICAL XZERO(SIZE(X,2)), XALLZ(SIZE(X,2)), XFINITE(SIZE(X,1),SIZE(X,2)), XNONN(SIZE(X,2)), XALLN(SIZE(X,2))
    !
    DMX = SE % DMX
    H = HUGE(H)
    XFINITE = X == X .AND. X >= -H .AND. X <= H ! neither NaN, Inf nor -Inf
    IF (SE % W == 0) THEN  ! Edit descriptor 'F0.d' specified
      WW = MAXW_DBLE(RESHAPE(X, (/SIZE(X)/)), SE % D)
      IF (SE % LZAS > 0 .AND. ANY(X == 0._DBLE))  WW = MAX(WW, SE % LZAS)
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
        IF (SE % LZAS > 0 .AND. ANY(X == 0._DBLE))  WW = MAX(WW, SE % LZAS)
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
      XZERO = ANY(X == 0._DBLE, 1) ! true where column has some zeros
      XALLZ = ALL(X == 0._DBLE, 1) ! true where column has only zeros
      XNONN = ANY(X > H .OR. X < -H .OR. X /= X, 1)  ! true where column has some nonnormals (inf, -inf, nan)
      XALLN = ALL(X > H .OR. X < -H .OR. X /= X, 1)  ! true where column has only nonnormals (inf, -inf, nan)
      CALL GETWID_DBLE(XMAXV, XMINV, XZERO, XALLZ, XNONN, XALLN, SE,  WID, NBL)
    ELSE
      WID = SE % W
      NBL = 0
    ENDIF
  END SUBROUTINE FIND_EDITDESC_DBLE

  SUBROUTINE GETWID_DBLE(XMAXV, XMINV, XZERO, XALLZ, XNONN, XALLN, SE,  WID, NBL)
    ! determine length of the strings that result when writing with edit descriptor SE%ed a
    ! vector v where v(i) is xmaxv(i) or xminv(i) depending on which gives longer output
    REAL(DBLE),     INTENT(IN)  :: XMAXV(:), XMINV(:) ! max and min values in each column
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
  END SUBROUTINE GETWID_DBLE

  ! ******** TOSTRING DOUBLE PRECISION PROCEDURES ***********


  PURE FUNCTION WIDTHMAX_DBLE(X, FMT) RESULT(W)
    ! Maximum width of an element of x
    REAL(DBLE), INTENT(IN)   :: X(:)
    CHARACTER(*), INTENT(IN) :: FMT
    CHARACTER(NNBLK(FMT)+5)  :: FMT1
    INTEGER W, D
    LOGICAL GEDIT
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN ! illegal format, use 1
      W = 1
    ELSEIF (W == 0) THEN
      W = MAXW_DBLE(X, D)
    ENDIF
  END FUNCTION WIDTHMAX_DBLE

  PURE FUNCTION LEN_F_DBLE(X, FMT) RESULT(WTOT)
    ! Total length of returned string, vector s
    REAL(DBLE), INTENT(IN)           :: X(:)
    CHARACTER(*), INTENT(IN)         :: FMT
    CHARACTER(WIDTHMAX_DBLE(X, FMT)) :: SA(SIZE(X))
    INTEGER                          :: WTOT, W, D, WW
    LOGICAL                          :: GEDIT
    CHARACTER(NNBLK(FMT)+8)          :: FMT1  !(5 for readfmt and 3 for replace_w)
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    IF (W == 0) THEN
      WW = MAXW_DBLE(X, D)
      CALL REPLACE_W(FMT1, WW)
    ENDIF
    WRITE(SA, FMT1) X
    CALL TRIM_REAL(SA, GEDIT, W)
    WTOT = SUM(LEN_TRIM(SA)) + (SIZE(X) - 1)*(TOSSET % SEPLEN)
  END FUNCTION LEN_F_DBLE

  FUNCTION TOSTRING_S_DBLE(X) RESULT(ST)
    ! Scalar to string
    REAL(DBLE), INTENT(IN) :: X
    CHARACTER(LEN_F_DBLE((/X/), TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_DBLE((/X/), TOSSET % RFMT)
  END FUNCTION TOSTRING_S_DBLE

  FUNCTION TOSTRING_SF_DBLE(X, FMT) RESULT(ST)
    ! Scalar with specified format to string
    REAL(DBLE),   INTENT(IN) :: X
    CHARACTER(*), INTENT(IN) :: FMT
    CHARACTER(LEN_F_DBLE((/X/), FMT)) :: ST
    ST = TOSTRING_F_DBLE((/X/), FMT)
  END FUNCTION TOSTRING_SF_DBLE

  FUNCTION TOSTRING_DBLE(X) RESULT(ST)
    ! Vector to string
    REAL(DBLE), INTENT(IN) :: X(:)
    CHARACTER(LEN_F_DBLE(X, TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_DBLE(X, TOSSET % RFMT)
  END FUNCTION TOSTRING_DBLE

  FUNCTION TOSTRING_F_DBLE(X, FMT) RESULT(ST)
    ! Vector with specified format to string
    REAL(DBLE)    ,       INTENT(IN) :: X(:)
    CHARACTER(*),         INTENT(IN) :: FMT
    CHARACTER(LEN_F_DBLE(X, FMT))    :: ST
    CHARACTER(WIDTHMAX_DBLE(X, FMT)) :: SA(SIZE(X))
    CHARACTER(NNBLK(FMT)+8)          :: FMT1  !(5 for readfmt and 3 for replace_w)
    INTEGER                          :: W, D, WW
    LOGICAL                          :: GEDIT
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN
      ST = ERRORMSG
      RETURN
    ELSEIF (W == 0) THEN
      WW = MAXW_DBLE(X, D)
      CALL REPLACE_W(FMT1, WW)
    ENDIF
    WRITE(SA, FMT1) X
    CALL TRIM_REAL(SA, GEDIT, W)
    CALL TOSTRING_GET(SA, ST)
  END FUNCTION TOSTRING_F_DBLE

  ! *************************************** END OF DOUBLE PRECISION PROCEDURES ***************************************

  ! *************************************** DOUBLE PRECISION COMPLEX PROCEDURES **************************************
  SUBROUTINE DISP_S_CPLD(X, FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP, TRIM, UNIT)
    ! double precision complex scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, TRIM
    COMPLEX(DBLE), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TS_CPLD('', X, FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP, 'left', TRIM, UNIT)
  END SUBROUTINE DISP_S_CPLD

  SUBROUTINE DISP_V_CPLD(X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! double precision complex vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM, ORIENT
    COMPLEX(DBLE), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:), DIGMAX
    CALL DISP_TV_CPLD('', X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
  END SUBROUTINE DISP_V_CPLD

  SUBROUTINE DISP_M_CPLD(X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! double precision complex matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM
    COMPLEX(DBLE), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX, LBOUND(:)
    CALL DISP_TM_CPLD('', X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT)
  END SUBROUTINE DISP_M_CPLD

  SUBROUTINE DISP_TS_CPLD(TITLE, X, FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP, STYLE, TRIM, UNIT)
    ! double precision complex scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM
    COMPLEX(DBLE), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, DIGMAX
    CALL DISP_TM_CPLD(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, FMT_IMAG, ADVANCE, DIGMAX, SEP=SEP, STYLE=STYLE, &
                                                       TRIM=TRIM, UNIT=UNIT)
  END SUBROUTINE DISP_TS_CPLD

  SUBROUTINE DISP_TV_CPLD(TITLE, X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! double precision complex vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, FMT_IMAG, ADVANCE, SEP, STYLE, TRIM, ORIENT
    COMPLEX(DBLE), INTENT(IN) :: X(:)
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
      CALL DISP_CPLD(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE, SEIM, N = SIZE(X))
    ELSE
      CALL DISP_CPLD(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE, SEIM, N = 1)
    END IF
  END SUBROUTINE DISP_TV_CPLD

  SUBROUTINE DISP_TM_CPLD(TITLE, X, FMT, FMT_IMAG, ADVANCE, DIGMAX, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! double precision complex matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    COMPLEX(DBLE),  INTENT(IN)         :: X(:,:)     ! The matrix to be written
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
    CALL DISP_CPLD(TITLE, X, SE, SEIM, N = SIZE(X,2))
  END SUBROUTINE DISP_TM_CPLD

  SUBROUTINE DISP_CPLD(TITLE, X, SE, SEIM, N)
    ! Double precision item
    CHARACTER(*),   INTENT(IN)    :: TITLE
    COMPLEX(DBLE),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE, SEIM
    INTEGER,        INTENT(IN)    :: N
    INTEGER, DIMENSION(N) :: WIDRE(N), WIDIM(N), NBLRE(N), NBLIM(N)
    CALL FIND_EDITDESC_DBLE(REAL(X), SE, WIDRE, NBLRE)         ! determine also SE % w
    CALL FIND_EDITDESC_DBLE(ABS(AIMAG(X)), SEIM, WIDIM, NBLIM) ! determine also SEim % w
    CALL TOBOX_CPLD(TITLE, X, SE, SEIM, WIDRE, WIDIM, NBLRE, NBLIM, M = SIZE(X,1), N = SIZE(X,2))
  END SUBROUTINE DISP_CPLD

  SUBROUTINE TOBOX_CPLD(TITLE, X, SE, SEIM, WIDRE, WIDIM, NBLRE, NBLIM, M, N)
    ! Write double precision complex matrix to box
    CHARACTER(*),   INTENT(IN)    :: TITLE
    COMPLEX(DBLE),  INTENT(IN)    :: X(:,:)
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
  END SUBROUTINE TOBOX_CPLD

  ! ******* TOSTRING DOUBLE PRECISION COMPLEX PROCEDURES ********

  PURE FUNCTION LEN_S_CPLD(X, FMT) RESULT(WTOT)
    COMPLEX(DBLE), INTENT(IN) :: X
    CHARACTER(*), INTENT(IN)  :: FMT
    INTEGER                   :: WTOT, W, D
    LOGICAL                   :: GEDIT
    CHARACTER(NNBLK(FMT)+8)   :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    WTOT = LEN_F_DBLE((/REAL(X)/), FMT) + LEN_F_DBLE((/ABS(AIMAG(X))/), FMT) + 4
  END FUNCTION LEN_S_CPLD

  PURE FUNCTION LEN_F_CPLD(X, FMT) RESULT(WTOT)
    COMPLEX(DBLE), INTENT(IN) :: X(:)
    CHARACTER(*), INTENT(IN)  :: FMT
    INTEGER                   :: WTOT, W, D
    LOGICAL                   :: GEDIT
    CHARACTER(NNBLK(FMT)+8)   :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W < 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    WTOT = LEN_F_DBLE(REAL(X), FMT) + LEN_F_DBLE(ABS(AIMAG(X)), FMT) + SIZE(X)*4 - (SIZE(X) - 1)*(TOSSET % SEPLEN)
    ! subtract seplen because it has been added twice in len_f_dble
  END FUNCTION LEN_F_CPLD

  FUNCTION TOSTRING_S_CPLD(X) RESULT(ST)
    COMPLEX(DBLE), INTENT(IN)                   :: X
    CHARACTER(LEN_S_CPLD(X, TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_CPLD((/X/), TOSSET % RFMT)
  END FUNCTION TOSTRING_S_CPLD

  FUNCTION TOSTRING_SF_CPLD(X, FMT) RESULT(ST)
    COMPLEX(DBLE),  INTENT(IN)        :: X
    CHARACTER(*), INTENT(IN)          :: FMT
    CHARACTER(LEN_S_CPLD(X, FMT)) :: ST
    ST = TOSTRING_F_CPLD((/X/), FMT)
  END FUNCTION TOSTRING_SF_CPLD

  FUNCTION TOSTRING_CPLD(X) RESULT(ST)
    COMPLEX(DBLE), INTENT(IN)               :: X(:)
    CHARACTER(LEN_F_CPLD(X, TOSSET % RFMT)) :: ST
    ST = TOSTRING_F_CPLD(X, TOSSET % RFMT)
  END FUNCTION TOSTRING_CPLD

  FUNCTION TOSTRING_F_CPLD(X, FMT) RESULT(ST)
    COMPLEX(DBLE),  INTENT(IN)                    :: X(:)
    CHARACTER(*),   INTENT(IN)                    :: FMT
    CHARACTER(LEN_F_CPLD(X, FMT))                 :: ST
    CHARACTER(WIDTHMAX_DBLE(REAL(X), FMT))        :: SAR(SIZE(X))
    CHARACTER(WIDTHMAX_DBLE(ABS(X-REAL(X)), FMT)) :: SAI(SIZE(X))  ! x-real(x) instead of aimag(x) to enable tHE FNCTION
    CHARACTER(1)                                  :: SGN(SIZE(X))  ! to pass -stand:f95 switch of the ifort coMPILER.
    INTEGER                                       :: W, D, WR, WI, I
    LOGICAL                                       :: GEDIT
    CHARACTER(NNBLK(FMT)+8)                       :: FMT1  !(5 for readfmt and 3 for replace_w)
    REAL(DBLE)                                    :: XRE(SIZE(X)), XIM(SIZE(X)), H
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    XRE = REAL(X)
    XIM = AIMAG(X)
    H = HUGE(H)
    IF (W < 0) THEN
      ST = ERRORMSG
      RETURN
    ELSEIF (W == 0) THEN
      WR = MAXW_DBLE(XRE, D)
      WI = MAXW_DBLE(XIM, D)
      CALL REPLACE_W(FMT1, MAX(WR, WI))
    ENDIF
    WRITE(SAR, FMT1) REAL(X)
    WRITE(SAI, FMT1) ABS(AIMAG(X))
    CALL TRIM_REAL(SAR, GEDIT, W)
    CALL TRIM_REAL(SAI, GEDIT, W)
    DO I = 1,SIZE(X); IF (AIMAG(X(I)) < 0) THEN; SGN(I) = '-'; ELSE; SGN(I) = '+'; ENDIF; ENDDO
    CALL TOSTRING_GET_COMPLEX(SAR, SGN, SAI, ST)
  END FUNCTION TOSTRING_F_CPLD
  ! *************************************** END OF DOUBLE PRECISION COMPLEX PROCEDURES ********************************

  ! ********************************************** DEFAULT LOGICAL PROCEDURES *****************************************
  SUBROUTINE DISP_S_DLOG(X, FMT, ADVANCE, SEP, TRIM, UNIT)
    ! Default logical scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, TRIM
    LOGICAL(DLOG), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    CALL DISP_TS_DLOG('', X, FMT, ADVANCE, SEP, 'left', TRIM, UNIT)
  END SUBROUTINE DISP_S_DLOG

  SUBROUTINE DISP_V_DLOG(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! Default logical vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ORIENT
    LOGICAL(DLOG), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TV_DLOG('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
  END SUBROUTINE DISP_V_DLOG

  SUBROUTINE DISP_M_DLOG(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! Default logical matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM
    LOGICAL(DLOG), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TM_DLOG('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
  END SUBROUTINE DISP_M_DLOG

  SUBROUTINE DISP_TS_DLOG(TITLE, X, FMT, ADVANCE, SEP, STYLE, TRIM, UNIT)
    ! Default logical scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM
    LOGICAL(DLOG), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    CALL DISP_TM_DLOG(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, ADVANCE, SEP=SEP, STYLE=STYLE, TRIM=TRIM, UNIT=UNIT)
  END SUBROUTINE DISP_TS_DLOG

  SUBROUTINE DISP_TV_DLOG(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! Default logical vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ORIENT
    LOGICAL(DLOG), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    IF (SE % ROW) THEN
      CALL DISP_DLOG(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE)
    ELSE
      CALL DISP_DLOG(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE)
    END IF
  END SUBROUTINE DISP_TV_DLOG

  SUBROUTINE DISP_TM_DLOG(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! Default logical matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE     ! The title to use for the matrix
    LOGICAL(DLOG),INTENT(IN)           :: X(:,:)    ! The matrix to be written
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT       ! Format edit descriptor to use for each matrix element (e.G. 'L1')
    INTEGER,      INTENT(IN), OPTIONAL :: UNIT      ! Unit to display on
    CHARACTER(*), INTENT(IN), OPTIONAL :: ADVANCE   ! 'No' to print next matrix to right of current, otherewisE 'YES'
    CHARACTER(*), INTENT(IN), OPTIONAL :: SEP       ! Separator between matrix columns (e.g. ", ")
    CHARACTER(*), INTENT(IN), OPTIONAL :: STYLE     ! Style(s): See NOTE 1 below
    CHARACTER(*), INTENT(IN), OPTIONAL :: TRIM      ! 'Auto' (the default) to trim if fmt absent, 'no' for no TRIMMING,
    !                                               ! 'yes' for trimming
    INTEGER,      INTENT(IN), OPTIONAL :: LBOUND(:) ! Lower bounds of x
    TYPE(SETTINGS) :: SE
    !
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
    CALL DISP_DLOG(TITLE, X, SE)
  END SUBROUTINE DISP_TM_DLOG

  SUBROUTINE DISP_DLOG(TITLE, X, SE)
    ! Write default logical to box or unit
    CHARACTER(*),   INTENT(IN)    :: TITLE
    LOGICAL(DLOG),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER WID(SIZE(X,2)), NBL(SIZE(X,2))
    IF (SE % W <= 0 .OR. SE % TRM) THEN
      SE % ED = '(L1)'
      IF (SIZE(X) == 0) THEN
        WID = 0
      ELSE
        WID = 1
      ENDIF
      SE % W = 1
      NBL = SE % W - WID
    ELSE
      WID = SE % W
      NBL = 0
    ENDIF
    CALL TOBOX_DLOG(TITLE, X, SE, WID, NBL)
  END SUBROUTINE DISP_DLOG

  SUBROUTINE TOBOX_DLOG(TITLE, X, SE, WID, NBL)
    CHARACTER(*),   INTENT(IN)    :: TITLE
    LOGICAL(DLOG),  INTENT(IN)    :: X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    INTEGER,        INTENT(INOUT) :: WID(:)
    INTEGER,        INTENT(INOUT) :: NBL(:)
    CHARACTER(SE % W)  :: S(SIZE(X,1))
    INTEGER            :: M, N, LIN1, I, J, WLEFT, WIDP(SIZE(WID))
    CHARACTER, POINTER :: BOXP(:,:)
    M = SIZE(X,1)
    N = SIZE(X,2)
    CALL PREPAREBOX(TITLE, SE, M, N, WID, WIDP, LIN1, WLEFT, BOXP)
    DO J=1,N
      IF (M > 0) WRITE(S, SE % ED) (X(I,J), I=1,M)
      CALL COPYTOBOX(S, LIN1, WID(J), WIDP(J), NBL(J), BOXP,  WLEFT)
      IF (J<N) CALL COPYSEPTOBOX(SE % SEP(1:SE % LSEP), M, LIN1, BOXP,  WLEFT)
    ENDDO
    CALL FINISHBOX(TITLE, SE, BOXP)
  END SUBROUTINE TOBOX_DLOG

  ! ********** DEFAULT LOGICAL TOSTRING PROCEDURES *********
  PURE FUNCTION WIDTHMAX_DLOG(FMT) RESULT(W)
    CHARACTER(*), INTENT(IN) :: FMT
    INTEGER W, D
    LOGICAL GEDIT
    CHARACTER(NNBLK(FMT)+5) :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W <= 0) W = 1
  END FUNCTION WIDTHMAX_DLOG

  PURE FUNCTION LEN_F_DLOG(X, FMT) RESULT(WTOT)
    LOGICAL(DLOG), INTENT(IN)  :: X(:)
    CHARACTER(*), INTENT(IN)   :: FMT
    INTEGER                    :: WTOT, W, D
    LOGICAL                    :: GEDIT
    CHARACTER(NNBLK(FMT)+5)    :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W <= 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    IF (TOSSET % TRIMB == 'YES') WTOT = SIZE(X)
    IF (TOSSET % TRIMB == 'NO' ) WTOT = W*SIZE(X)
    WTOT = WTOT + (SIZE(X) - 1)*(TOSSET % SEPLEN)
  END FUNCTION LEN_F_DLOG

  FUNCTION TOSTRING_S_DLOG(X) RESULT(ST)
    LOGICAL(DLOG), INTENT(IN) :: X
    CHARACTER(1)            :: ST
    ST = TOSTRING_F_DLOG((/X/), 'L1')
  END FUNCTION TOSTRING_S_DLOG

  FUNCTION TOSTRING_SF_DLOG(X, FMT) RESULT(ST)
    LOGICAL(DLOG),INTENT(IN)        :: X
    CHARACTER(*), INTENT(IN)        :: FMT
    CHARACTER(LEN_F_DLOG((/X/), FMT)) :: ST
    ST = TOSTRING_F_DLOG((/X/), FMT)
  END FUNCTION TOSTRING_SF_DLOG

  FUNCTION TOSTRING_DLOG(X) RESULT(ST)
    LOGICAL(DLOG), INTENT(IN)                          :: X(:)
    CHARACTER(1 + (SIZE(X) - 1)*(1 + TOSSET % SEPLEN)) :: ST
    ST = TOSTRING_F_DLOG(X, 'L1')
  END FUNCTION TOSTRING_DLOG

  FUNCTION TOSTRING_F_DLOG(X, FMT) RESULT(ST)
    LOGICAL(DLOG), INTENT(IN)     :: X(:)
    CHARACTER(*), INTENT(IN)      :: FMT
    CHARACTER(LEN_F_DLOG(X, FMT)) :: ST
    CHARACTER(WIDTHMAX_DLOG(FMT)) :: SA(SIZE(X))
    INTEGER                       :: W, D
    LOGICAL                       :: GEDIT
    CHARACTER(NNBLK(FMT)+5)       :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W <= 0) THEN; ST = ERRORMSG; RETURN; ENDIF
    WRITE(SA, FMT1) X
    IF (TOSSET % TRIMB == 'YES') SA = ADJUSTL(SA)
    CALL TOSTRING_GET(SA, ST)
  END FUNCTION TOSTRING_F_DLOG
  ! ****************************** END OF DEFAULT LOGICAL PROCEDURES *******************************

  ! ******************************* DEFAULT CHARACTER PROCEDURES **********************************
  SUBROUTINE DISP_V_DCHR(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! Default character vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ORIENT
    CHARACTER(*), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TV_DCHR('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
  END SUBROUTINE DISP_V_DCHR

  SUBROUTINE DISP_M_DCHR(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! Default character matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM
    CHARACTER(*), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TM_DCHR('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
  END SUBROUTINE DISP_M_DCHR

  SUBROUTINE DISP_TS_DCHR(TITLE, X, FMT, ADVANCE, SEP, STYLE, TRIM, UNIT)
    ! Default character scalar with title
    CHARACTER(*), INTENT(IN), OPTIONAL :: TITLE, X, FMT, ADVANCE, SEP, STYLE, TRIM
    CHARACTER(0) EMPTY(1,0)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    EMPTY = ''
    IF (PRESENT(TITLE).AND.PRESENT(X)) THEN
      CALL DISP_NONOPT_DCHR(TITLE, X, FMT, ADVANCE, SEP=SEP, STYLE=STYLE, TRIM=TRIM, UNIT=UNIT)
    ELSEIF (PRESENT(X)) THEN
      CALL DISP_NONOPT_DCHR('', X, FMT, ADVANCE, SEP=SEP, STYLE='left', TRIM=TRIM, UNIT=UNIT)
    ELSEIF (PRESENT(TITLE)) THEN
      CALL DISP_NONOPT_DCHR('', TITLE, FMT, ADVANCE, SEP=SEP, STYLE='left', TRIM=TRIM, UNIT=UNIT)
    ELSE
      CALL DISP_TM_DCHR('', EMPTY, FMT, ADVANCE, SEP=SEP, STYLE=STYLE, TRIM=TRIM, UNIT=UNIT)
    END IF
  END SUBROUTINE DISP_TS_DCHR

  SUBROUTINE DISP_NONOPT_DCHR(TITLE, X, FMT, ADVANCE, SEP, STYLE, TRIM, UNIT)
    ! This routine exists to circumvent bug in gfortran, that made it not possible to change scalar strings
    ! to matrices with reshape in calls of disp_tm_dchr. This intermediate routine provides work-around.
    CHARACTER(*), INTENT(IN) :: TITLE, X, FMT, ADVANCE, SEP, STYLE, TRIM
    OPTIONAL FMT, ADVANCE, SEP, STYLE, TRIM
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    CHARACTER(LEN(X)) :: XM(1,1)
    XM(1,1) = X
    CALL DISP_TM_DCHR(TITLE, XM, FMT, ADVANCE, SEP=SEP, STYLE=STYLE, TRIM=TRIM, UNIT=UNIT)
  END SUBROUTINE DISP_NONOPT_DCHR

  SUBROUTINE DISP_TV_DCHR(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! Default character vector with title
    CHARACTER(*), INTENT(IN) :: TITLE, X(:)
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ORIENT
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    IF (SE % ROW) THEN
      CALL DISP_DCHR(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE)
    ELSE
      CALL DISP_DCHR(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE)
    END IF
  END SUBROUTINE DISP_TV_DCHR

  SUBROUTINE DISP_TM_DCHR(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! Default character matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    CHARACTER(*), INTENT(IN)           :: X(:,:)     ! The matrix to be written
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT        ! Format edit descriptor to use for each matrix element (E.G.'A4')
    INTEGER,      INTENT(IN), OPTIONAL :: UNIT       ! Unit to display on
    CHARACTER(*), INTENT(IN), OPTIONAL :: ADVANCE    ! 'No' to print next matrix to right of current, otherewiSE 'YES'
    CHARACTER(*), INTENT(IN), OPTIONAL :: SEP        ! Separator between matrix columns (e.g. ", ")
    CHARACTER(*), INTENT(IN), OPTIONAL :: STYLE      ! Style(s): see NOTE 1 below
    CHARACTER(*), INTENT(IN), OPTIONAL :: TRIM       ! 'Auto' (the default) to trim if fmt absent, 'no' for no
    !                                                ! trimming, 'yes' for trimming
    INTEGER,      INTENT(IN), OPTIONAL :: LBOUND(:)  ! Lower bounds of x
    !
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
    CALL DISP_DCHR(TITLE, X, SE)
  END SUBROUTINE DISP_TM_DCHR

  SUBROUTINE DISP_DCHR(TITLE, X, SE)
    ! Default character item to box
    CHARACTER(*), INTENT(IN)      :: TITLE, X(:,:)
    TYPE(SETTINGS), INTENT(INOUT) :: SE
    CHARACTER(13)                 :: EDESC
    CHARACTER, POINTER            :: BOXP(:,:)
    INTEGER                       :: M, N, J, LIN1, WLEFT, LX, W
    INTEGER, DIMENSION(SIZE(X,2)) :: WID, NBL, N1, N2, WIDP
    M = SIZE(X,1)
    N = SIZE(X,2)
    LX = LEN(X)
    W = SE % W
    IF (W <= 0) THEN
      W = LX
      IF (W < 0) THEN
        EDESC = '(A__________)'
        WRITE(EDESC(3:12), '(SS,I10)') W
        SE % ED = EDESC
      END IF
    END IF
    IF (SE % TRM .AND. SIZE(X) > 0) THEN
      N1 = MINVAL(MOD(VERIFY(X, ' ') - W - 1, W + 1), 1) + W + 1
      N2 = MAXVAL(VERIFY(X, ' ', BACK = .TRUE.), 1)
      WID = N2 - N1 + 1
      NBL = W - WID
    ELSE
      N1 = 1
      N2 = W
      WID = W
      NBL = 0
    END IF
    IF (ALL(WID == 0)) N = 0
    SE % W = W
    CALL PREPAREBOX(TITLE, SE, M, N, WID, WIDP, LIN1, WLEFT, BOXP)
    DO J=1,N
      IF (SE % TRM) THEN
        CALL COPYTOBOX(X(:,J)(N1(J):N2(J)), LIN1, WID(J), WIDP(J), NBL(J), BOXP,  WLEFT)
      ELSE
        IF (WIDP(J) > LX) CALL COPYSEPTOBOX(REPEAT(' ', WIDP(J)-LX), M, LIN1, BOXP,  WLEFT)
        CALL COPYTOBOX(X(:,J), LIN1, LX, LX, 0, BOXP,  WLEFT)
      END IF
      IF (J<N) CALL COPYSEPTOBOX(SE % SEP(1:SE % LSEP), M, LIN1, BOXP,  WLEFT)
    ENDDO
    CALL FINISHBOX(TITLE, SE, BOXP)
  END SUBROUTINE DISP_DCHR

  ! ************************* END OF DEFAULT CHARACTER PROCEDURES ********************************

  ! NOTE 1: STYLES
  !   Styles can be LEFT, ABOVE, PAD, UNDERLINE or NUMBER. Padding is by default done with hyphen
  !   characters (e.g. ---title---), but can be changed for example to asterisks with style='*PAD'.
  !   Underlining is also with hypens and can also be changed, e.g. with style='*UNDERLINE'. Lower
  !   or mixed case is acceptable: style='above' or style='Above'. It is also possible to specify
  !   both NUMBER and one of the other styles, with e.g. style='ABOVE & NUMBER'.
  !
  ! NOTE 2: DOUBLE PRECISION
  !   The double precision functions and subroutines above (the sections marked DOUBLE PRECISION
  !   PROECDURES and DOUBLE PRECISION COMPLEX PROECEDURES) are copies of the sections marked SINGLE
  !   PRECISION PROCEDURES and SINGLE PRECISION COMPLEX PROCEDURES, with the kind parameter sngl
  !   changed to dble, the procedure name suffixes _sngl and _cplx changed to _dble and _cpld, and
  !   single changed to double (only appears in comments). The add-on module DISP_R16MOD is another
  !   copy of these procedures (for quad precision).

END MODULE ModLib_Display_Default

!******************************************************************************
