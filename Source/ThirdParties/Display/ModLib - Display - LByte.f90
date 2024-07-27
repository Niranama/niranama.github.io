
MODULE ModLib_Display_LByte

!** PURPOSE OF THIS MODULE:
  ! Add-on module to DISPMODULE to display 1-byte logical items
  ! (assuming that these have kind = 1)
  !
  ! This module is obtained by copying the section DEFAULT LOGICAL PROCEDURES from
  ! dispmodule.f90, replacing dlog with log1 and 'default logical' with '1-byte
  ! logical' (only appears in comments), and adding the DECLARATIONS section below.
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
    INTEGER, PARAMETER :: LOG1 = 1  ! hopefully logical(1) is byte

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE DISP
        MODULE PROCEDURE DISP_S_LOG1, DISP_TS_LOG1, DISP_V_LOG1
        MODULE PROCEDURE DISP_TV_LOG1, DISP_M_LOG1, DISP_TM_LOG1
    END INTERFACE

    INTERFACE TOSTRING
        MODULE PROCEDURE TOSTRING_LOG1, TOSTRING_F_LOG1
        MODULE PROCEDURE TOSTRING_S_LOG1, TOSTRING_SF_LOG1
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    !

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

  ! ********************************************** 1-BYTE LOGICAL PROCEDURES *************************************************
  SUBROUTINE DISP_S_LOG1(X, FMT, ADVANCE, SEP, TRIM, UNIT)
    ! 1-byte logical scalar without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, TRIM
    LOGICAL(LOG1), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    CALL DISP_TS_LOG1('', X, FMT, ADVANCE, SEP, 'left', TRIM, UNIT)
  END SUBROUTINE DISP_S_LOG1

  SUBROUTINE DISP_V_LOG1(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! 1-byte logical vector without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ORIENT
    LOGICAL(LOG1), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TV_LOG1('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
  END SUBROUTINE DISP_V_LOG1

  SUBROUTINE DISP_M_LOG1(X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! 1-byte logical matrix without title
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM
    LOGICAL(LOG1), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    CALL DISP_TM_LOG1('', X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
  END SUBROUTINE DISP_M_LOG1

  SUBROUTINE DISP_TS_LOG1(TITLE, X, FMT, ADVANCE, SEP, STYLE, TRIM, UNIT)
    ! 1-byte logical scalar with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM
    LOGICAL(LOG1), INTENT(IN) :: X
    INTEGER, INTENT(IN), OPTIONAL :: UNIT
    CALL DISP_TM_LOG1(TITLE, RESHAPE((/X/), (/1, 1/)), FMT, ADVANCE, SEP=SEP, STYLE=STYLE, TRIM=TRIM, UNIT=UNIT)
  END SUBROUTINE DISP_TS_LOG1

  SUBROUTINE DISP_TV_LOG1(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    ! 1-byte logical vector with title
    CHARACTER(*), INTENT(IN) :: TITLE
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT, ADVANCE, SEP, STYLE, TRIM, ORIENT
    LOGICAL(LOG1), INTENT(IN) :: X(:)
    INTEGER, INTENT(IN), OPTIONAL :: UNIT, LBOUND(:)
    TYPE(SETTINGS) :: SE
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT, ORIENT)
    IF (SE % ROW) THEN
      CALL DISP_LOG1(TITLE, RESHAPE(X, (/1, SIZE(X)/)), SE)
    ELSE
      CALL DISP_LOG1(TITLE, RESHAPE(X, (/SIZE(X), 1/)), SE)
    END IF
  END SUBROUTINE DISP_TV_LOG1

  SUBROUTINE DISP_TM_LOG1(TITLE, X, FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
    ! 1-byte logical matrix with title
    CHARACTER(*), INTENT(IN)           :: TITLE      ! The title to use for the matrix
    LOGICAL(LOG1),INTENT(IN)           :: X(:,:)     ! The matrix to be written
    CHARACTER(*), INTENT(IN), OPTIONAL :: FMT        ! Format edit descriptor to use for each matrix element (E.G. 'L1')
    INTEGER,      INTENT(IN), OPTIONAL :: UNIT       ! Unit to display on
    CHARACTER(*), INTENT(IN), OPTIONAL :: ADVANCE    ! 'No' to print next matrix to right of current, otherewiSE 'YES'
    CHARACTER(*), INTENT(IN), OPTIONAL :: SEP        ! Separator between matrix columns (e.g. ", ")
    CHARACTER(*), INTENT(IN), OPTIONAL :: STYLE      ! Style(s): See NOTE 1 below
    CHARACTER(*), INTENT(IN), OPTIONAL :: TRIM       ! 'Auto' (the default) to trim if fmt absent, 'no' for no TRIMMING,
    !                                                ! 'yes' for trimming
    INTEGER,      INTENT(IN), OPTIONAL :: LBOUND(:)  ! Lower bounds of x
    TYPE(SETTINGS) :: SE
    !
    CALL GET_SE(SE, TITLE, SHAPE(X), FMT, ADVANCE, LBOUND, SEP, STYLE, TRIM, UNIT)
    CALL DISP_LOG1(TITLE, X, SE)
  END SUBROUTINE DISP_TM_LOG1

  SUBROUTINE DISP_LOG1(TITLE, X, SE)
    ! Write 1-byte logical to box or unit
    CHARACTER(*),   INTENT(IN)    :: TITLE
    LOGICAL(LOG1),  INTENT(IN)    :: X(:,:)
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
    CALL TOBOX_LOG1(TITLE, X, SE, WID, NBL)
  END SUBROUTINE DISP_LOG1

  SUBROUTINE TOBOX_LOG1(TITLE, X, SE, WID, NBL)
    CHARACTER(*),   INTENT(IN)    :: TITLE
    LOGICAL(LOG1),  INTENT(IN)    :: X(:,:)
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
  END SUBROUTINE TOBOX_LOG1

  ! ********** 1-BYTE LOGICAL TOSTRING PROCEDURES *********

  PURE FUNCTION WIDTHMAX_LOG1(FMT) RESULT(W)
    CHARACTER(*), INTENT(IN) :: FMT
    INTEGER W, D
    LOGICAL GEDIT
    CHARACTER(NNBLK(FMT)+5) :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W <= 0) W = 1
  END FUNCTION WIDTHMAX_LOG1

  PURE FUNCTION LEN_F_LOG1(X, FMT) RESULT(WTOT)
    LOGICAL(LOG1), INTENT(IN)  :: X(:)
    CHARACTER(*), INTENT(IN)   :: FMT
    INTEGER                    :: WTOT, W, D
    LOGICAL                    :: GEDIT
    CHARACTER(NNBLK(FMT)+2)    :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W <= 0) THEN; WTOT = LEN(ERRORMSG); RETURN; ENDIF
    IF (TOSSET % TRIMB == 'YES') WTOT = SIZE(X)
    IF (TOSSET % TRIMB == 'NO' ) WTOT = W*SIZE(X)
    WTOT = WTOT + (SIZE(X) - 1)*(TOSSET % SEPLEN)
  END FUNCTION LEN_F_LOG1

  FUNCTION TOSTRING_S_LOG1(X) RESULT(ST)
    LOGICAL(LOG1), INTENT(IN) :: X
    CHARACTER(1)            :: ST
    ST = TOSTRING_F_LOG1((/X/), 'L1')
  END FUNCTION TOSTRING_S_LOG1

  FUNCTION TOSTRING_SF_LOG1(X, FMT) RESULT(ST)
    LOGICAL(LOG1),INTENT(IN)        :: X
    CHARACTER(*), INTENT(IN)        :: FMT
    CHARACTER(LEN_F_LOG1((/X/), FMT)) :: ST
    ST = TOSTRING_F_LOG1((/X/), FMT)
  END FUNCTION TOSTRING_SF_LOG1

  FUNCTION TOSTRING_LOG1(X) RESULT(ST)
    LOGICAL(LOG1), INTENT(IN)                          :: X(:)
    CHARACTER(1 + (SIZE(X) - 1)*(1 + TOSSET % SEPLEN)) :: ST
    ST = TOSTRING_F_LOG1(X, 'L1')
  END FUNCTION TOSTRING_LOG1

  FUNCTION TOSTRING_F_LOG1(X, FMT) RESULT(ST)
    LOGICAL(LOG1), INTENT(IN)     :: X(:)
    CHARACTER(*), INTENT(IN)      :: FMT
    CHARACTER(LEN_F_LOG1(X, FMT)) :: ST
    CHARACTER(WIDTHMAX_LOG1(FMT)) :: SA(SIZE(X))
    INTEGER                       :: W, D
    LOGICAL                       :: GEDIT
    CHARACTER(NNBLK(FMT)+2)       :: FMT1
    CALL READFMT(FMT, FMT1, W, D, GEDIT)
    IF (W <= 0) THEN; ST = ERRORMSG; RETURN; ENDIF
    WRITE(SA, FMT1) X
    IF (TOSSET % TRIMB == 'YES') SA = ADJUSTL(SA)
    CALL TOSTRING_GET(SA, ST)
  END FUNCTION TOSTRING_F_LOG1
  ! ************************************* END OF 1-BYTE LOGICAL PROCEDURES ******************************************

END MODULE ModLib_Display_LByte

!******************************************************************************
