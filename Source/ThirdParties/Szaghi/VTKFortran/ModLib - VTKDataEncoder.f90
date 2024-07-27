!< DataArray encoder, codecs: "ascii", "base64".
MODULE ModLib_VTKDataEncoder
!< VTK file XMl writer, ascii local.
USE ModLib_BeFor64
USE ModLib_Penf

IMPLICIT NONE
PRIVATE
PUBLIC :: ENCODE_ASCII_DATAARRAY
PUBLIC :: ENCODE_BINARY_DATAARRAY

INTERFACE ENCODE_ASCII_DATAARRAY
  !< Ascii DataArray encoder.
  MODULE PROCEDURE ENCODE_ASCII_DATAARRAY1_RANK1_R8P, &
                   ENCODE_ASCII_DATAARRAY1_RANK1_R4P, &
                   ENCODE_ASCII_DATAARRAY1_RANK1_I8P, &
                   ENCODE_ASCII_DATAARRAY1_RANK1_I4P, &
                   ENCODE_ASCII_DATAARRAY1_RANK1_I2P, &
                   ENCODE_ASCII_DATAARRAY1_RANK1_I1P, &
                   ENCODE_ASCII_DATAARRAY1_RANK2_R8P, &
                   ENCODE_ASCII_DATAARRAY1_RANK2_R4P, &
                   ENCODE_ASCII_DATAARRAY1_RANK2_I8P, &
                   ENCODE_ASCII_DATAARRAY1_RANK2_I4P, &
                   ENCODE_ASCII_DATAARRAY1_RANK2_I2P, &
                   ENCODE_ASCII_DATAARRAY1_RANK2_I1P, &
                   ENCODE_ASCII_DATAARRAY1_RANK3_R8P, &
                   ENCODE_ASCII_DATAARRAY1_RANK3_R4P, &
                   ENCODE_ASCII_DATAARRAY1_RANK3_I8P, &
                   ENCODE_ASCII_DATAARRAY1_RANK3_I4P, &
                   ENCODE_ASCII_DATAARRAY1_RANK3_I2P, &
                   ENCODE_ASCII_DATAARRAY1_RANK3_I1P, &
                   ENCODE_ASCII_DATAARRAY1_RANK4_R8P, &
                   ENCODE_ASCII_DATAARRAY1_RANK4_R4P, &
                   ENCODE_ASCII_DATAARRAY1_RANK4_I8P, &
                   ENCODE_ASCII_DATAARRAY1_RANK4_I4P, &
                   ENCODE_ASCII_DATAARRAY1_RANK4_I2P, &
                   ENCODE_ASCII_DATAARRAY1_RANK4_I1P, &
                   ENCODE_ASCII_DATAARRAY3_RANK1_R8P, &
                   ENCODE_ASCII_DATAARRAY3_RANK1_R4P, &
                   ENCODE_ASCII_DATAARRAY3_RANK1_I8P, &
                   ENCODE_ASCII_DATAARRAY3_RANK1_I4P, &
                   ENCODE_ASCII_DATAARRAY3_RANK1_I2P, &
                   ENCODE_ASCII_DATAARRAY3_RANK1_I1P, &
                   ENCODE_ASCII_DATAARRAY3_RANK3_R8P, &
                   ENCODE_ASCII_DATAARRAY3_RANK3_R4P, &
                   ENCODE_ASCII_DATAARRAY3_RANK3_I8P, &
                   ENCODE_ASCII_DATAARRAY3_RANK3_I4P, &
                   ENCODE_ASCII_DATAARRAY3_RANK3_I2P, &
                   ENCODE_ASCII_DATAARRAY3_RANK3_I1P, &
                   ENCODE_ASCII_DATAARRAY6_RANK1_R8P, &
                   ENCODE_ASCII_DATAARRAY6_RANK1_R4P, &
                   ENCODE_ASCII_DATAARRAY6_RANK1_I8P, &
                   ENCODE_ASCII_DATAARRAY6_RANK1_I4P, &
                   ENCODE_ASCII_DATAARRAY6_RANK1_I2P, &
                   ENCODE_ASCII_DATAARRAY6_RANK1_I1P, &
                   ENCODE_ASCII_DATAARRAY6_RANK3_R8P, &
                   ENCODE_ASCII_DATAARRAY6_RANK3_R4P, &
                   ENCODE_ASCII_DATAARRAY6_RANK3_I8P, &
                   ENCODE_ASCII_DATAARRAY6_RANK3_I4P, &
                   ENCODE_ASCII_DATAARRAY6_RANK3_I2P, &
                   ENCODE_ASCII_DATAARRAY6_RANK3_I1P
ENDINTERFACE ENCODE_ASCII_DATAARRAY
INTERFACE ENCODE_BINARY_DATAARRAY
  !< Binary (base64) DataArray encoder.
  MODULE PROCEDURE ENCODE_BINARY_DATAARRAY1_RANK1_R8P, &
                   ENCODE_BINARY_DATAARRAY1_RANK1_R4P, &
                   ENCODE_BINARY_DATAARRAY1_RANK1_I8P, &
                   ENCODE_BINARY_DATAARRAY1_RANK1_I4P, &
                   ENCODE_BINARY_DATAARRAY1_RANK1_I2P, &
                   ENCODE_BINARY_DATAARRAY1_RANK1_I1P, &
                   ENCODE_BINARY_DATAARRAY1_RANK2_R8P, &
                   ENCODE_BINARY_DATAARRAY1_RANK2_R4P, &
                   ENCODE_BINARY_DATAARRAY1_RANK2_I8P, &
                   ENCODE_BINARY_DATAARRAY1_RANK2_I4P, &
                   ENCODE_BINARY_DATAARRAY1_RANK2_I2P, &
                   ENCODE_BINARY_DATAARRAY1_RANK2_I1P, &
                   ENCODE_BINARY_DATAARRAY1_RANK3_R8P, &
                   ENCODE_BINARY_DATAARRAY1_RANK3_R4P, &
                   ENCODE_BINARY_DATAARRAY1_RANK3_I8P, &
                   ENCODE_BINARY_DATAARRAY1_RANK3_I4P, &
                   ENCODE_BINARY_DATAARRAY1_RANK3_I2P, &
                   ENCODE_BINARY_DATAARRAY1_RANK3_I1P, &
                   ENCODE_BINARY_DATAARRAY1_RANK4_R8P, &
                   ENCODE_BINARY_DATAARRAY1_RANK4_R4P, &
                   ENCODE_BINARY_DATAARRAY1_RANK4_I8P, &
                   ENCODE_BINARY_DATAARRAY1_RANK4_I4P, &
                   ENCODE_BINARY_DATAARRAY1_RANK4_I2P, &
                   ENCODE_BINARY_DATAARRAY1_RANK4_I1P, &
                   ENCODE_BINARY_DATAARRAY3_RANK1_R8P, &
                   ENCODE_BINARY_DATAARRAY3_RANK1_R4P, &
                   ENCODE_BINARY_DATAARRAY3_RANK1_I8P, &
                   ENCODE_BINARY_DATAARRAY3_RANK1_I4P, &
                   ENCODE_BINARY_DATAARRAY3_RANK1_I2P, &
                   ENCODE_BINARY_DATAARRAY3_RANK1_I1P, &
                   ENCODE_BINARY_DATAARRAY3_RANK3_R8P, &
                   ENCODE_BINARY_DATAARRAY3_RANK3_R4P, &
                   ENCODE_BINARY_DATAARRAY3_RANK3_I8P, &
                   ENCODE_BINARY_DATAARRAY3_RANK3_I4P, &
                   ENCODE_BINARY_DATAARRAY3_RANK3_I2P, &
                   ENCODE_BINARY_DATAARRAY3_RANK3_I1P, &
                   ENCODE_BINARY_DATAARRAY6_RANK1_R8P, &
                   ENCODE_BINARY_DATAARRAY6_RANK1_R4P, &
                   ENCODE_BINARY_DATAARRAY6_RANK1_I8P, &
                   ENCODE_BINARY_DATAARRAY6_RANK1_I4P, &
                   ENCODE_BINARY_DATAARRAY6_RANK1_I2P, &
                   ENCODE_BINARY_DATAARRAY6_RANK1_I1P, &
                   ENCODE_BINARY_DATAARRAY6_RANK3_R8P, &
                   ENCODE_BINARY_DATAARRAY6_RANK3_R4P, &
                   ENCODE_BINARY_DATAARRAY6_RANK3_I8P, &
                   ENCODE_BINARY_DATAARRAY6_RANK3_I4P, &
                   ENCODE_BINARY_DATAARRAY6_RANK3_I2P, &
                   ENCODE_BINARY_DATAARRAY6_RANK3_I1P
ENDINTERFACE ENCODE_BINARY_DATAARRAY
CONTAINS
  !< ascii encoder
  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_R16P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  REAL(R16P),      INTENT(IN)   :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension size

  SIZE_N = SIZE(X,DIM=1)
  L = DR16P+1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N = 1,SIZE_N
      CODE(SP+1:SP+L) = STR(N=X(N))
      SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_R16P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_R8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  REAL(R8P),       INTENT(IN)   :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension size

  SIZE_N = SIZE(X,DIM=1)
  L = DR8P+1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N = 1,SIZE_N
      CODE(SP+1:SP+L) = STR(N=X(N))
      SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_R8P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_R4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R4P).
  REAL(R4P),       INTENT(IN)   :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension size

  SIZE_N = SIZE(X,DIM=1)
  L = DR4P+1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N = 1,SIZE_N
      CODE(SP+1:SP+L) = STR(N=X(N))
      SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_R4P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_I8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I8P).
  INTEGER(I8P),    INTENT(IN)   :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension size

  SIZE_N = SIZE(X,DIM=1)
  L = DI8P+1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N = 1,SIZE_N
      CODE(SP+1:SP+L) = STR(N=X(N))
      SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_I8P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_I4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I4P).
  INTEGER(I4P),    INTENT(IN)   :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension size

  SIZE_N = SIZE(X,DIM=1)
  L = DI4P+1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N = 1,SIZE_N
      CODE(SP+1:SP+L) = STR(N=X(N))
      SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_I4P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_I2P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I2P).
  INTEGER(I2P),    INTENT(IN)   :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension size

  SIZE_N = SIZE(X,DIM=1)
  L = DI2P+1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N = 1,SIZE_N
      CODE(SP+1:SP+L) = STR(N=X(N))
      SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_I2P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_I1P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I1P).
  INTEGER(I1P),    INTENT(IN)   :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension size

  SIZE_N = SIZE(X,DIM=1)
  L = DI1P+1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N = 1,SIZE_N
      CODE(SP+1:SP+L) = STR(N=X(N))
      SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK1_I1P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_R16P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R16P).
  REAL(R16P),      INTENT(IN)   :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1       !< Counter.
  INTEGER(I4P)                  :: N2       !< Counter.
  INTEGER(I4P)                  :: L        !< Length
  INTEGER(I4P)                  :: SP       !< String pointer
  INTEGER(I4P)                  :: SIZE_N1  !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2  !< Dimension 2 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  L = DR16P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2)
  DO N2=1, SIZE(X, DIM=2)
    DO N1=1, SIZE(X, DIM=1)-1
      CODE(SP+1:SP+L) = STR(N=X(N1, N2))//' '
      SP = SP + L
    ENDDO
    CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_R16P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_R8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R8P).
  REAL(R8P),       INTENT(IN)   :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1       !< Counter.
  INTEGER(I4P)                  :: N2       !< Counter.
  INTEGER(I4P)                  :: L        !< Length
  INTEGER(I4P)                  :: SP       !< String pointer
  INTEGER(I4P)                  :: SIZE_N1  !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2  !< Dimension 2 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  L = DR8P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2)
  DO N2=1, SIZE(X, DIM=2)
    DO N1=1, SIZE(X, DIM=1)-1
      CODE(SP+1:SP+L) = STR(N=X(N1, N2))//' '
      SP = SP + L
    ENDDO
    CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_R8P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_R4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R4P).
  REAL(R4P),       INTENT(IN)   :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1       !< Counter.
  INTEGER(I4P)                  :: N2       !< Counter.
  INTEGER(I4P)                  :: L        !< Length
  INTEGER(I4P)                  :: SP       !< String pointer
  INTEGER(I4P)                  :: SIZE_N1  !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2  !< Dimension 2 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  L = DR4P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2)
  DO N2=1, SIZE(X, DIM=2)
    DO N1=1, SIZE(X, DIM=1)-1
      CODE(SP+1:SP+L) = STR(N=X(N1, N2))//' '
      SP = SP + L
    ENDDO
    CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_R4P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_I8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I8P).
  INTEGER(I8P),    INTENT(IN)   :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1       !< Counter.
  INTEGER(I4P)                  :: N2       !< Counter.
  INTEGER(I4P)                  :: L        !< Length
  INTEGER(I4P)                  :: SP       !< String pointer
  INTEGER(I4P)                  :: SIZE_N1  !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2  !< Dimension 2 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  L = DI8P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2)
  DO N2=1, SIZE(X, DIM=2)
    DO N1=1, SIZE(X, DIM=1)-1
      CODE(SP+1:SP+L) = STR(N=X(N1, N2))//' '
      SP = SP + L
    ENDDO
    CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_I8P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_I4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I4P).
  INTEGER(I4P),    INTENT(IN)   :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1       !< Counter.
  INTEGER(I4P)                  :: N2       !< Counter.
  INTEGER(I4P)                  :: L        !< Length
  INTEGER(I4P)                  :: SP       !< String pointer
  INTEGER(I4P)                  :: SIZE_N1  !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2  !< Dimension 2 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  L = DI4P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2)
  DO N2=1, SIZE(X, DIM=2)
    DO N1=1, SIZE(X, DIM=1)-1
      CODE(SP+1:SP+L) = STR(N=X(N1, N2))//' '
      SP = SP + L
    ENDDO
    CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_I4P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_I2P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  INTEGER(I2P),    INTENT(IN)   :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1       !< Counter.
  INTEGER(I4P)                  :: N2       !< Counter.
  INTEGER(I4P)                  :: L        !< Length
  INTEGER(I4P)                  :: SP       !< String pointer
  INTEGER(I4P)                  :: SIZE_N1  !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2  !< Dimension 2 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  L = DI4P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2)
  DO N2=1, SIZE(X, DIM=2)
    DO N1=1, SIZE(X, DIM=1)-1
      CODE(SP+1:SP+L) = STR(N=X(N1, N2))//' '
      SP = SP + L
    ENDDO
    CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_I2P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_I1P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  INTEGER(I1P),    INTENT(IN)   :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1       !< Counter.
  INTEGER(I4P)                  :: N2       !< Counter.
  INTEGER(I4P)                  :: L        !< Length
  INTEGER(I4P)                  :: SP       !< String pointer
  INTEGER(I4P)                  :: SIZE_N1  !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2  !< Dimension 2 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  L = DI1P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2)
  DO N2=1, SIZE(X, DIM=2)
    DO N1=1, SIZE(X, DIM=1)-1
      CODE(SP+1:SP+L) = STR(N=X(N1, N2))//' '
      SP = SP + L
    ENDDO
    CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK2_I1P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_R16P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R16P).
  REAL(R16P),      INTENT(IN)   :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DR16P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)-1
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '
        SP = SP + 1
      ENDDO
      CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2, N3))
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_R16P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_R8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R8P).
  REAL(R8P),       INTENT(IN)   :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DR8P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)-1
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '
        SP = SP + L
      ENDDO
      CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2, N3))
      SP = SP + L
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_R8P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_R4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R4P).
  REAL(R4P),       INTENT(IN)   :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DR4P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)-1
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '
        SP = SP + L
      ENDDO
      CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2, N3))
      SP = SP + L
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_R4P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_I8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I8P).
  INTEGER(I8P),    INTENT(IN)   :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI8P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)-1
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '
        SP = SP + L
      ENDDO
      CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2, N3))
      SP = SP + L
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_I8P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_I4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I4P).
  INTEGER(I4P),    INTENT(IN)   :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI4P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)-1
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '
        SP = SP + L
      ENDDO
      CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2, N3))
      SP = SP + L
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_I4P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_I2P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I2P).
  INTEGER(I2P),    INTENT(IN)   :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI2P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)-1
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '
        SP = SP + L
      ENDDO
      CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2, N3))
      SP = SP + L
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_I2P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_I1P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I1P).
  INTEGER(I1P),    INTENT(IN)   :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI1P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)-1
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '
        SP = SP + L
      ENDDO
      CODE(SP+1:SP+L) = ' '//STR(N=X(SIZE(X, DIM=1), N2, N3))
      SP = SP + L
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK3_I1P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_R16P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R16P).
  REAL(R16P),      INTENT(IN)   :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1             !< Counter.
  INTEGER(I4P)                  :: N2             !< Counter.
  INTEGER(I4P)                  :: N3             !< Counter.
  INTEGER(I4P)                  :: N4             !< Counter.
  INTEGER(I4P)                  :: L              !< Length
  INTEGER(I4P)                  :: SP             !< String pointer
  INTEGER(I4P)                  :: SIZE_N1        !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2        !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3        !< Dimension 3 size
  INTEGER(I4P)                  :: SIZE_N4        !< Dimension 4 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)
  SIZE_N3 = SIZE(X, DIM=4)

  L = DR16P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3*SIZE_N4)
  DO N4=1, SIZE(X, DIM=4)
    DO N3=1, SIZE(X, DIM=3)
      DO N2=1, SIZE(X, DIM=2)
        DO N1=1, SIZE(X, DIM=1)
          CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3, N4))//' '
          SP = SP + L
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_R16P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_R8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R8P).
  REAL(R8P),       INTENT(IN)   :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1             !< Counter.
  INTEGER(I4P)                  :: N2             !< Counter.
  INTEGER(I4P)                  :: N3             !< Counter.
  INTEGER(I4P)                  :: N4             !< Counter.
  INTEGER(I4P)                  :: L              !< Length
  INTEGER(I4P)                  :: SP             !< String pointer
  INTEGER(I4P)                  :: SIZE_N1        !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2        !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3        !< Dimension 3 size
  INTEGER(I4P)                  :: SIZE_N4        !< Dimension 4 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)
  SIZE_N3 = SIZE(X, DIM=4)

  L = DR8P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3*SIZE_N4)
  DO N4=1, SIZE(X, DIM=4)
    DO N3=1, SIZE(X, DIM=3)
      DO N2=1, SIZE(X, DIM=2)
        DO N1=1, SIZE(X, DIM=1)
          CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3, N4))//' '
          SP = SP + L
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_R8P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_R4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R4P).
  REAL(R4P),       INTENT(IN)   :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1             !< Counter.
  INTEGER(I4P)                  :: N2             !< Counter.
  INTEGER(I4P)                  :: N3             !< Counter.
  INTEGER(I4P)                  :: N4             !< Counter.
  INTEGER(I4P)                  :: L              !< Length
  INTEGER(I4P)                  :: SP             !< String pointer
  INTEGER(I4P)                  :: SIZE_N1        !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2        !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3        !< Dimension 3 size
  INTEGER(I4P)                  :: SIZE_N4        !< Dimension 4 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)
  SIZE_N3 = SIZE(X, DIM=4)

  L = DR4P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3*SIZE_N4)
  DO N4=1, SIZE(X, DIM=4)
    DO N3=1, SIZE(X, DIM=3)
      DO N2=1, SIZE(X, DIM=2)
        DO N1=1, SIZE(X, DIM=1)
          CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3, N4))//' '
          SP = SP + L
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_R4P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_I8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I8P).
  INTEGER(I8P),    INTENT(IN)   :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1             !< Counter.
  INTEGER(I4P)                  :: N2             !< Counter.
  INTEGER(I4P)                  :: N3             !< Counter.
  INTEGER(I4P)                  :: N4             !< Counter.
  INTEGER(I4P)                  :: L              !< Length
  INTEGER(I4P)                  :: SP             !< String pointer
  INTEGER(I4P)                  :: SIZE_N1        !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2        !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3        !< Dimension 3 size
  INTEGER(I4P)                  :: SIZE_N4        !< Dimension 4 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)
  SIZE_N3 = SIZE(X, DIM=4)

  L = DI8P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3*SIZE_N4)
  DO N4=1, SIZE(X, DIM=4)
    DO N3=1, SIZE(X, DIM=3)
      DO N2=1, SIZE(X, DIM=2)
        DO N1=1, SIZE(X, DIM=1)
          CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3, N4))//' '
          SP = SP + L
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_I8P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_I4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I4P).
  INTEGER(I4P),    INTENT(IN)   :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1             !< Counter.
  INTEGER(I4P)                  :: N2             !< Counter.
  INTEGER(I4P)                  :: N3             !< Counter.
  INTEGER(I4P)                  :: N4             !< Counter.
  INTEGER(I4P)                  :: L              !< Length
  INTEGER(I4P)                  :: SP             !< String pointer
  INTEGER(I4P)                  :: SIZE_N1        !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2        !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3        !< Dimension 3 size
  INTEGER(I4P)                  :: SIZE_N4        !< Dimension 4 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)
  SIZE_N3 = SIZE(X, DIM=4)

  L = DI4P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3*SIZE_N4)
  DO N4=1, SIZE(X, DIM=4)
    DO N3=1, SIZE(X, DIM=3)
      DO N2=1, SIZE(X, DIM=2)
        DO N1=1, SIZE(X, DIM=1)
          CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3, N4))//' '
          SP = SP + L
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_I4P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_I2P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I2P).
  INTEGER(I2P),    INTENT(IN)   :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1             !< Counter.
  INTEGER(I4P)                  :: N2             !< Counter.
  INTEGER(I4P)                  :: N3             !< Counter.
  INTEGER(I4P)                  :: N4             !< Counter.
  INTEGER(I4P)                  :: L              !< Length
  INTEGER(I4P)                  :: SP             !< String pointer
  INTEGER(I4P)                  :: SIZE_N1        !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2        !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3        !< Dimension 3 size
  INTEGER(I4P)                  :: SIZE_N4        !< Dimension 4 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)
  SIZE_N3 = SIZE(X, DIM=4)

  L = DI2P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3*SIZE_N4)
  DO N4=1, SIZE(X, DIM=4)
    DO N3=1, SIZE(X, DIM=3)
      DO N2=1, SIZE(X, DIM=2)
        DO N1=1, SIZE(X, DIM=1)
          CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3, N4))//' '
          SP = SP + L
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_I2P

  FUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_I1P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I1P).
  INTEGER(I1P),    INTENT(IN)   :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1             !< Counter.
  INTEGER(I4P)                  :: N2             !< Counter.
  INTEGER(I4P)                  :: N3             !< Counter.
  INTEGER(I4P)                  :: N4             !< Counter.
  INTEGER(I4P)                  :: L              !< Length
  INTEGER(I4P)                  :: SP             !< String pointer
  INTEGER(I4P)                  :: SIZE_N1        !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2        !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3        !< Dimension 3 size
  INTEGER(I4P)                  :: SIZE_N4        !< Dimension 4 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)
  SIZE_N3 = SIZE(X, DIM=4)

  L = DI1P + 1
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3*SIZE_N4)
  DO N4=1, SIZE(X, DIM=4)
    DO N3=1, SIZE(X, DIM=3)
      DO N2=1, SIZE(X, DIM=2)
        DO N1=1, SIZE(X, DIM=1)
          CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3, N4))//' '
          SP = SP + L
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY1_RANK4_I1P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_R16P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R16P).
  REAL(R16P),      INTENT(IN)   :: X(1:) !< X component.
  REAL(R16P),      INTENT(IN)   :: Y(1:) !< Y component.
  REAL(R16P),      INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DR16P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_R16P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_R8P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R8P).
  REAL(R8P),       INTENT(IN)   :: X(1:) !< X component.
  REAL(R8P),       INTENT(IN)   :: Y(1:) !< Y component.
  REAL(R8P),       INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DR8P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_R8P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_R4P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R4P).
  REAL(R4P),       INTENT(IN)   :: X(1:) !< X component.
  REAL(R4P),       INTENT(IN)   :: Y(1:) !< Y component.
  REAL(R4P),       INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DR4P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_R4P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_I8P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I8P).
  INTEGER(I8P),    INTENT(IN)   :: X(1:) !< X component.
  INTEGER(I8P),    INTENT(IN)   :: Y(1:) !< Y component.
  INTEGER(I8P),    INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DI8P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_I8P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_I4P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I4P).
  INTEGER(I4P),    INTENT(IN)   :: X(1:) !< X component.
  INTEGER(I4P),    INTENT(IN)   :: Y(1:) !< Y component.
  INTEGER(I4P),    INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DI4P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_I4P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_I2P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I2P).
  INTEGER(I2P),    INTENT(IN)   :: X(1:) !< X component.
  INTEGER(I2P),    INTENT(IN)   :: Y(1:) !< Y component.
  INTEGER(I2P),    INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DI2P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_I2P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_I1P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I1P).
  INTEGER(I1P),    INTENT(IN)   :: X(1:) !< X component.
  INTEGER(I1P),    INTENT(IN)   :: Y(1:) !< Y component.
  INTEGER(I1P),    INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DI1P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK1_I1P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_R16P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  REAL(R16P),      INTENT(IN)   :: X(1:,1:,1:) !< X component.
  REAL(R16P),      INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  REAL(R16P),      INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DR16P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_R16P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_R8P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  REAL(R8P),       INTENT(IN)   :: X(1:,1:,1:) !< X component.
  REAL(R8P),       INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  REAL(R8P),       INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DR8P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_R8P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_R4P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  REAL(R4P),       INTENT(IN)   :: X(1:,1:,1:) !< X component.
  REAL(R4P),       INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  REAL(R4P),       INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DR4P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_R4P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_I8P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I8P).
  INTEGER(I8P),    INTENT(IN)   :: X(1:,1:,1:) !< X component.
  INTEGER(I8P),    INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  INTEGER(I8P),    INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI8P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_I8P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_I4P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I4P).
  INTEGER(I4P),    INTENT(IN)   :: X(1:,1:,1:) !< X component.
  INTEGER(I4P),    INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  INTEGER(I4P),    INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI4P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_I4P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_I2P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I2P).
  INTEGER(I2P),    INTENT(IN)   :: X(1:,1:,1:) !< X component.
  INTEGER(I2P),    INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  INTEGER(I2P),    INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI2P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_I2P

  FUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_I1P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I1P).
  INTEGER(I1P),    INTENT(IN)   :: X(1:,1:,1:) !< X component.
  INTEGER(I1P),    INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  INTEGER(I1P),    INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI1P*3 + 2
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE(X, DIM=3)
    DO N2=1, SIZE(X, DIM=2)
      DO N1=1, SIZE(X, DIM=1)
        CODE(SP+1:SP+L) = STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY3_RANK3_I1P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_R16P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R16P).
  REAL(R16P),      INTENT(IN)   :: U(1:) !< U component.
  REAL(R16P),      INTENT(IN)   :: V(1:) !< V component.
  REAL(R16P),      INTENT(IN)   :: W(1:) !< W component.
  REAL(R16P),      INTENT(IN)   :: X(1:) !< X component.
  REAL(R16P),      INTENT(IN)   :: Y(1:) !< Y component.
  REAL(R16P),      INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DR16P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=U(N))//' '//STR(N=V(N))//' '//STR(N=W(N))// &
                STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_R16P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_R8P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R8P).
  REAL(R8P),       INTENT(IN)   :: U(1:) !< U component.
  REAL(R8P),       INTENT(IN)   :: V(1:) !< V component.
  REAL(R8P),       INTENT(IN)   :: W(1:) !< W component.
  REAL(R8P),       INTENT(IN)   :: X(1:) !< X component.
  REAL(R8P),       INTENT(IN)   :: Y(1:) !< Y component.
  REAL(R8P),       INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DR8P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=U(N))//' '//STR(N=V(N))//' '//STR(N=W(N))// &
                STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_R8P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_R4P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R4P).
  REAL(R4P),       INTENT(IN)   :: U(1:) !< U component.
  REAL(R4P),       INTENT(IN)   :: V(1:) !< V component.
  REAL(R4P),       INTENT(IN)   :: W(1:) !< W component.
  REAL(R4P),       INTENT(IN)   :: X(1:) !< X component.
  REAL(R4P),       INTENT(IN)   :: Y(1:) !< Y component.
  REAL(R4P),       INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DR4P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=U(N))//' '//STR(N=V(N))//' '//STR(N=W(N))// &
                STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_R4P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_I8P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I8P).
  INTEGER(I8P),    INTENT(IN)   :: U(1:) !< U component.
  INTEGER(I8P),    INTENT(IN)   :: V(1:) !< V component.
  INTEGER(I8P),    INTENT(IN)   :: W(1:) !< W component.
  INTEGER(I8P),    INTENT(IN)   :: X(1:) !< X component.
  INTEGER(I8P),    INTENT(IN)   :: Y(1:) !< Y component.
  INTEGER(I8P),    INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DI8P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=U(N))//' '//STR(N=V(N))//' '//STR(N=W(N))// &
                STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_I8P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_I4P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I4P).
  INTEGER(I4P),    INTENT(IN)   :: U(1:) !< U component.
  INTEGER(I4P),    INTENT(IN)   :: V(1:) !< V component.
  INTEGER(I4P),    INTENT(IN)   :: W(1:) !< W component.
  INTEGER(I4P),    INTENT(IN)   :: X(1:) !< X component.
  INTEGER(I4P),    INTENT(IN)   :: Y(1:) !< Y component.
  INTEGER(I4P),    INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DI4P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=U(N))//' '//STR(N=V(N))//' '//STR(N=W(N))// &
                STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_I4P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_I2P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I2P).
  INTEGER(I2P),    INTENT(IN)   :: U(1:) !< U component.
  INTEGER(I2P),    INTENT(IN)   :: V(1:) !< V component.
  INTEGER(I2P),    INTENT(IN)   :: W(1:) !< W component.
  INTEGER(I2P),    INTENT(IN)   :: X(1:) !< X component.
  INTEGER(I2P),    INTENT(IN)   :: Y(1:) !< Y component.
  INTEGER(I2P),    INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DI2P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=U(N))//' '//STR(N=V(N))//' '//STR(N=W(N))// &
                STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_I2P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_I1P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I1P).
  INTEGER(I1P),    INTENT(IN)   :: U(1:) !< U component.
  INTEGER(I1P),    INTENT(IN)   :: V(1:) !< V component.
  INTEGER(I1P),    INTENT(IN)   :: W(1:) !< W component.
  INTEGER(I1P),    INTENT(IN)   :: X(1:) !< X component.
  INTEGER(I1P),    INTENT(IN)   :: Y(1:) !< Y component.
  INTEGER(I1P),    INTENT(IN)   :: Z(1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N     !< Counter.
  INTEGER(I4P)                  :: L     !< Length
  INTEGER(I4P)                  :: SP    !< String pointer
  INTEGER(I4P)                  :: SIZE_N!< Dimension 1 size

  SIZE_N = SIZE(X, DIM=1)
  L = DI1P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N)
  DO N=1, SIZE_N
    CODE(SP+1:SP+L) = STR(N=U(N))//' '//STR(N=V(N))//' '//STR(N=W(N))// &
                STR(N=X(N))//' '//STR(N=Y(N))//' '//STR(N=Z(N))
    SP = SP + L
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK1_I1P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_R16P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (R8P).
  REAL(R16P),      INTENT(IN)   :: U(1:,1:,1:) !< U component.
  REAL(R16P),      INTENT(IN)   :: V(1:,1:,1:) !< V component.
  REAL(R16P),      INTENT(IN)   :: W(1:,1:,1:) !< W component.
  REAL(R16P),      INTENT(IN)   :: X(1:,1:,1:) !< X component.
  REAL(R16P),      INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  REAL(R16P),      INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DR16P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE_N3
    DO N2=1, SIZE_N2
      DO N1=1, SIZE_N1
        CODE(SP+1:SP+L) = STR(N=U(N1, N2, N3))//' '//STR(N=V(N1, N2, N3))//' '//STR(N=W(N1, N2, N3))// &
          STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_R16P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_R8P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (R8P).
  REAL(R8P),       INTENT(IN)   :: U(1:,1:,1:) !< U component.
  REAL(R8P),       INTENT(IN)   :: V(1:,1:,1:) !< V component.
  REAL(R8P),       INTENT(IN)   :: W(1:,1:,1:) !< W component.
  REAL(R8P),       INTENT(IN)   :: X(1:,1:,1:) !< X component.
  REAL(R8P),       INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  REAL(R8P),       INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DR8P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE_N3
    DO N2=1, SIZE_N2
      DO N1=1, SIZE_N1
        CODE(SP+1:SP+L) = STR(N=U(N1, N2, N3))//' '//STR(N=V(N1, N2, N3))//' '//STR(N=W(N1, N2, N3))// &
          STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_R8P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_R4P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  REAL(R4P),       INTENT(IN)   :: U(1:,1:,1:) !< U component.
  REAL(R4P),       INTENT(IN)   :: V(1:,1:,1:) !< V component.
  REAL(R4P),       INTENT(IN)   :: W(1:,1:,1:) !< W component.
  REAL(R4P),       INTENT(IN)   :: X(1:,1:,1:) !< X component.
  REAL(R4P),       INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  REAL(R4P),       INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DR4P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE_N3
    DO N2=1, SIZE_N2
      DO N1=1, SIZE_N1
        CODE(SP+1:SP+L) = STR(N=U(N1, N2, N3))//' '//STR(N=V(N1, N2, N3))//' '//STR(N=W(N1, N2, N3))// &
          STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_R4P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_I8P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I8P).
  INTEGER(I8P),    INTENT(IN)   :: U(1:,1:,1:) !< U component.
  INTEGER(I8P),    INTENT(IN)   :: V(1:,1:,1:) !< V component.
  INTEGER(I8P),    INTENT(IN)   :: W(1:,1:,1:) !< W component.
  INTEGER(I8P),    INTENT(IN)   :: X(1:,1:,1:) !< X component.
  INTEGER(I8P),    INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  INTEGER(I8P),    INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI8P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE_N3
    DO N2=1, SIZE_N2
      DO N1=1, SIZE_N1
        CODE(SP+1:SP+L) = STR(N=U(N1, N2, N3))//' '//STR(N=V(N1, N2, N3))//' '//STR(N=W(N1, N2, N3))// &
          STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_I8P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_I4P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I4P).
  INTEGER(I4P),    INTENT(IN)   :: U(1:,1:,1:) !< U component.
  INTEGER(I4P),    INTENT(IN)   :: V(1:,1:,1:) !< V component.
  INTEGER(I4P),    INTENT(IN)   :: W(1:,1:,1:) !< W component.
  INTEGER(I4P),    INTENT(IN)   :: X(1:,1:,1:) !< X component.
  INTEGER(I4P),    INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  INTEGER(I4P),    INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI4P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE_N3
    DO N2=1, SIZE_N2
      DO N1=1, SIZE_N1
        CODE(SP+1:SP+L) = STR(N=U(N1, N2, N3))//' '//STR(N=V(N1, N2, N3))//' '//STR(N=W(N1, N2, N3))// &
          STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_I4P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_I2P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I2P).
  INTEGER(I2P),    INTENT(IN)   :: U(1:,1:,1:) !< U component.
  INTEGER(I2P),    INTENT(IN)   :: V(1:,1:,1:) !< V component.
  INTEGER(I2P),    INTENT(IN)   :: W(1:,1:,1:) !< W component.
  INTEGER(I2P),    INTENT(IN)   :: X(1:,1:,1:) !< X component.
  INTEGER(I2P),    INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  INTEGER(I2P),    INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI2P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE_N3
    DO N2=1, SIZE_N2
      DO N1=1, SIZE_N1
        CODE(SP+1:SP+L) = STR(N=U(N1, N2, N3))//' '//STR(N=V(N1, N2, N3))//' '//STR(N=W(N1, N2, N3))// &
          STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_I2P

  FUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_I1P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I1P).
  INTEGER(I1P),    INTENT(IN)   :: U(1:,1:,1:) !< U component.
  INTEGER(I1P),    INTENT(IN)   :: V(1:,1:,1:) !< V component.
  INTEGER(I1P),    INTENT(IN)   :: W(1:,1:,1:) !< W component.
  INTEGER(I1P),    INTENT(IN)   :: X(1:,1:,1:) !< X component.
  INTEGER(I1P),    INTENT(IN)   :: Y(1:,1:,1:) !< Y component.
  INTEGER(I1P),    INTENT(IN)   :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.
  INTEGER(I4P)                  :: L           !< Length
  INTEGER(I4P)                  :: SP          !< String pointer
  INTEGER(I4P)                  :: SIZE_N1     !< Dimension 1 size
  INTEGER(I4P)                  :: SIZE_N2     !< Dimension 2 size
  INTEGER(I4P)                  :: SIZE_N3     !< Dimension 3 size

  SIZE_N1 = SIZE(X, DIM=1)
  SIZE_N2 = SIZE(X, DIM=2)
  SIZE_N3 = SIZE(X, DIM=3)

  L = DI1P*6 + 5
  SP = 0
  CODE = REPEAT(' ',L*SIZE_N1*SIZE_N2*SIZE_N3)
  DO N3=1, SIZE_N3
    DO N2=1, SIZE_N2
      DO N1=1, SIZE_N1
        CODE(SP+1:SP+L) = STR(N=U(N1, N2, N3))//' '//STR(N=V(N1, N2, N3))//' '//STR(N=W(N1, N2, N3))// &
          STR(N=X(N1, N2, N3))//' '//STR(N=Y(N1, N2, N3))//' '//STR(N=Z(N1, N2, N3))
        SP = SP + L
      ENDDO
    ENDDO
  ENDDO
  ENDFUNCTION ENCODE_ASCII_DATAARRAY6_RANK3_I1P

  !< binary encoder
  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_R8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  REAL(R8P), INTENT(IN)         :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:) !< Packed data.
  INTEGER(I4P)                  :: NN    !< Number of elements.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(NN*BYR8P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_R8P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_R4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R4P).
  REAL(R4P), INTENT(IN)         :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:) !< Packed data.
  INTEGER(I4P)                  :: NN    !< Number of elements.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(NN*BYR4P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_R4P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_I8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I8P).
  INTEGER(I8P), INTENT(IN)      :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:) !< Packed data.
  INTEGER(I4P)                  :: NN    !< Number of elements.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(NN*BYI8P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_I8P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_I4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I4P).
  INTEGER(I4P), INTENT(IN)      :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:) !< Packed data.
  INTEGER(I4P)                  :: NN    !< Number of elements.

  NN = SIZE(X, DIM=1)
  XP = TRANSFER([INT(NN*BYI4P, I4P), RESHAPE(X, [NN])], XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_I4P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_I2P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I2P).
  INTEGER(I2P), INTENT(IN)      :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:) !< Packed data.
  INTEGER(I4P)                  :: NN    !< Number of elements.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(NN*BYI2P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_I2P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_I1P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I1P).
  INTEGER(I1P), INTENT(IN)      :: X(1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE  !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:) !< Packed data.
  INTEGER(I4P)                  :: NN    !< Number of elements.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(NN*BYI1P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK1_I1P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_R8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R8P).
  REAL(R8P), INTENT(IN)         :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)    !< Packed data.
  INTEGER(I4P)                  :: NN       !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)
  CALL PACK_DATA(A1=[INT(NN*BYR8P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_R8P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_R4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R4P).
  REAL(R4P), INTENT(IN)         :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)    !< Packed data.
  INTEGER(I4P)                  :: NN       !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)
  CALL PACK_DATA(A1=[INT(NN*BYR4P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_R4P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_I8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I8P).
  INTEGER(I8P), INTENT(IN)      :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)    !< Packed data.
  INTEGER(I4P)                  :: NN       !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)
  CALL PACK_DATA(A1=[INT(NN*BYI8P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_I8P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_I4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I4P).
  INTEGER(I4P), INTENT(IN)      :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)    !< Packed data.
  INTEGER(I4P)                  :: NN       !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)
  XP = TRANSFER([INT(NN*BYI4P, I4P), RESHAPE(X, [NN])], XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_I4P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_I2P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  INTEGER(I2P), INTENT(IN)      :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)    !< Packed data.
  INTEGER(I4P)                  :: NN       !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)
  CALL PACK_DATA(A1=[INT(NN*BYI2P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_I2P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_I1P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I1P).
  INTEGER(I1P), INTENT(IN)      :: X(1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE     !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)    !< Packed data.
  INTEGER(I4P)                  :: NN       !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)
  CALL PACK_DATA(A1=[INT(NN*BYI1P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK2_I1P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_R8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R8P).
  REAL(R8P), INTENT(IN)         :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)       !< Packed data.
  INTEGER(I4P)                  :: NN          !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)
  CALL PACK_DATA(A1=[INT(NN*BYR8P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_R8P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_R4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R4P).
  REAL(R4P), INTENT(IN)         :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)       !< Packed data.
  INTEGER(I4P)                  :: NN          !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)
  CALL PACK_DATA(A1=[INT(NN*BYR4P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_R4P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_I8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I8P).
  INTEGER(I8P), INTENT(IN)      :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)       !< Packed data.
  INTEGER(I4P)                  :: NN          !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)
  CALL PACK_DATA(A1=[INT(NN*BYI8P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_I8P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_I4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I4P).
  INTEGER(I4P), INTENT(IN)      :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)       !< Packed data.
  INTEGER(I4P)                  :: NN          !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)
  XP = TRANSFER([INT(NN*BYI4P, I4P), RESHAPE(X, [NN])], XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_I4P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_I2P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I2P).
  INTEGER(I2P), INTENT(IN)      :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)       !< Packed data.
  INTEGER(I4P)                  :: NN          !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)
  CALL PACK_DATA(A1=[INT(NN*BYI2P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_I2P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_I1P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I1P).
  INTEGER(I1P), INTENT(IN)      :: X(1:,1:,1:) !< Data variable
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)       !< Packed data.
  INTEGER(I4P)                  :: NN          !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)
  CALL PACK_DATA(A1=[INT(NN*BYI1P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK3_I1P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_R8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R8P).
  REAL(R8P), INTENT(IN)         :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)          !< Packed data.
  INTEGER(I4P)                  :: NN             !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)*SIZE(X, DIM=4)
  CALL PACK_DATA(A1=[INT(NN*BYR8P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_R8P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_R4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R4P).
  REAL(R4P), INTENT(IN)         :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)          !< Packed data.
  INTEGER(I4P)                  :: NN             !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)*SIZE(X, DIM=4)
  CALL PACK_DATA(A1=[INT(NN*BYR4P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_R4P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_I8P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I8P).
  INTEGER(I8P), INTENT(IN)      :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)          !< Packed data.
  INTEGER(I4P)                  :: NN             !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)*SIZE(X, DIM=4)
  CALL PACK_DATA(A1=[INT(NN*BYI8P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_I8P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_I4P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I4P).
  INTEGER(I4P), INTENT(IN)      :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)          !< Packed data.
  INTEGER(I4P)                  :: NN             !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)*SIZE(X, DIM=4)
  XP = TRANSFER([INT(NN*BYI4P, I4P), RESHAPE(X, [NN])], XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_I4P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_I2P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I2P).
  INTEGER(I2P), INTENT(IN)      :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)          !< Packed data.
  INTEGER(I4P)                  :: NN             !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)*SIZE(X, DIM=4)
  CALL PACK_DATA(A1=[INT(NN*BYI2P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_I2P

  FUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_I1P(X) RESULT(CODE)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I1P).
  INTEGER(I1P), INTENT(IN)      :: X(1:,1:,1:,1:) !< Data variable.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE           !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XP(:)          !< Packed data.
  INTEGER(I4P)                  :: NN             !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)*SIZE(X, DIM=4)
  CALL PACK_DATA(A1=[INT(NN*BYI1P, I4P)], A2=RESHAPE(X, [NN]), PACKED=XP)
  CALL B64_ENCODE(N=XP, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY1_RANK4_I1P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_R8P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R8P).
  REAL(R8P),    INTENT(IN)      :: X(1:)  !< X component.
  REAL(R8P),    INTENT(IN)      :: Y(1:)  !< Y component.
  REAL(R8P),    INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(3*NN*BYR8P, I4P)], A2=[(X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_R8P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_R4P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R4P).
  REAL(R4P),    INTENT(IN)      :: X(1:)  !< X component.
  REAL(R4P),    INTENT(IN)      :: Y(1:)  !< Y component.
  REAL(R4P),    INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(3*NN*BYR4P, I4P)], A2=[(X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_R4P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_I8P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I8P).
  INTEGER(I8P), INTENT(IN)      :: X(1:)  !< X component.
  INTEGER(I8P), INTENT(IN)      :: Y(1:)  !< Y component.
  INTEGER(I8P), INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(3*NN*BYI8P, I4P)], A2=[(X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_I8P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_I4P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I4P).
  INTEGER(I4P), INTENT(IN)      :: X(1:)  !< X component.
  INTEGER(I4P), INTENT(IN)      :: Y(1:)  !< Y component.
  INTEGER(I4P), INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  XYZ = TRANSFER([INT(3*NN*BYI4P, I4P), [(X(N), Y(N), Z(N), N=1, NN)]], XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_I4P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_I2P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I2P).
  INTEGER(I2P), INTENT(IN)      :: X(1:)  !< X component.
  INTEGER(I2P), INTENT(IN)      :: Y(1:)  !< Y component.
  INTEGER(I2P), INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(3*NN*BYI2P, I4P)], A2=[(X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_I2P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_I1P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I1P).
  INTEGER(I1P), INTENT(IN)      :: X(1:)  !< X component.
  INTEGER(I1P), INTENT(IN)      :: Y(1:)  !< Y component.
  INTEGER(I1P), INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(3*NN*BYI1P, I4P)], A2=[(X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK1_I1P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_R8P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  REAL(R8P),    INTENT(IN)      :: X(1:,1:,1:) !< X component.
  REAL(R8P),    INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  REAL(R8P),    INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(3*NN*BYR8P, I4P)], A2=[(((X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_R8P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_R4P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  REAL(R4P),    INTENT(IN)      :: X(1:,1:,1:) !< X component.
  REAL(R4P),    INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  REAL(R4P),    INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(3*NN*BYR4P, I4P)], A2=[(((X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_R4P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_I8P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I8P).
  INTEGER(I8P), INTENT(IN)      :: X(1:,1:,1:) !< X component.
  INTEGER(I8P), INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  INTEGER(I8P), INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(3*NN*BYI8P, I4P)], A2=[(((X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_I8P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_I4P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I4P).
  INTEGER(I4P), INTENT(IN)      :: X(1:,1:,1:) !< X component.
  INTEGER(I4P), INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  INTEGER(I4P), INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  XYZ = TRANSFER([INT(3*NN*BYI4P, I4P), [(((X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                         N2=1, NN2),  &
                                                                                         N3=1, NN3)]], XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_I4P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_I2P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I2P).
  INTEGER(I2P), INTENT(IN)      :: X(1:,1:,1:) !< X component.
  INTEGER(I2P), INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  INTEGER(I2P), INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(3*NN*BYI2P, I4P)], A2=[(((X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_I2P

  FUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_I1P(X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I1P).
  INTEGER(I1P), INTENT(IN)      :: X(1:,1:,1:) !< X component.
  INTEGER(I1P), INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  INTEGER(I1P), INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(3*NN*BYI1P, I4P)], A2=[(((X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY3_RANK3_I1P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_R8P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R8P).
  REAL(R8P),    INTENT(IN)      :: U(1:)  !< U component.
  REAL(R8P),    INTENT(IN)      :: V(1:)  !< V component.
  REAL(R8P),    INTENT(IN)      :: W(1:)  !< W component.
  REAL(R8P),    INTENT(IN)      :: X(1:)  !< X component.
  REAL(R8P),    INTENT(IN)      :: Y(1:)  !< Y component.
  REAL(R8P),    INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(6*NN*BYR8P, I4P)], A2=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_R8P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_R4P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R4P).
  REAL(R4P),    INTENT(IN)      :: U(1:)  !< U component.
  REAL(R4P),    INTENT(IN)      :: V(1:)  !< V component.
  REAL(R4P),    INTENT(IN)      :: W(1:)  !< W component.
  REAL(R4P),    INTENT(IN)      :: X(1:)  !< X component.
  REAL(R4P),    INTENT(IN)      :: Y(1:)  !< Y component.
  REAL(R4P),    INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(6*NN*BYR4P, I4P)], A2=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_R4P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_I8P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I8P).
  INTEGER(I8P), INTENT(IN)      :: U(1:)  !< U component.
  INTEGER(I8P), INTENT(IN)      :: V(1:)  !< V component.
  INTEGER(I8P), INTENT(IN)      :: W(1:)  !< W component.
  INTEGER(I8P), INTENT(IN)      :: X(1:)  !< X component.
  INTEGER(I8P), INTENT(IN)      :: Y(1:)  !< Y component.
  INTEGER(I8P), INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(6*NN*BYI8P, I4P)], A2=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_I8P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_I4P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I4P).
  INTEGER(I4P), INTENT(IN)      :: U(1:)  !< U component.
  INTEGER(I4P), INTENT(IN)      :: V(1:)  !< V component.
  INTEGER(I4P), INTENT(IN)      :: W(1:)  !< W component.
  INTEGER(I4P), INTENT(IN)      :: X(1:)  !< X component.
  INTEGER(I4P), INTENT(IN)      :: Y(1:)  !< Y component.
  INTEGER(I4P), INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  XYZ = TRANSFER([INT(6*NN*BYI4P, I4P), [(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1, NN)]], XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_I4P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_I2P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I2P).
  INTEGER(I2P), INTENT(IN)      :: U(1:)  !< U component.
  INTEGER(I2P), INTENT(IN)      :: V(1:)  !< V component.
  INTEGER(I2P), INTENT(IN)      :: W(1:)  !< W component.
  INTEGER(I2P), INTENT(IN)      :: X(1:)  !< X component.
  INTEGER(I2P), INTENT(IN)      :: Y(1:)  !< Y component.
  INTEGER(I2P), INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(6*NN*BYI2P, I4P)], A2=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_I2P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_I1P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I1P).
  INTEGER(I1P), INTENT(IN)      :: U(1:)  !< U component.
  INTEGER(I1P), INTENT(IN)      :: V(1:)  !< V component.
  INTEGER(I1P), INTENT(IN)      :: W(1:)  !< W component.
  INTEGER(I1P), INTENT(IN)      :: X(1:)  !< X component.
  INTEGER(I1P), INTENT(IN)      :: Y(1:)  !< Y component.
  INTEGER(I1P), INTENT(IN)      :: Z(1:)  !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE   !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:) !< Packed data.
  INTEGER(I4P)                  :: NN     !< Number of elements.
  INTEGER(I4P)                  :: N      !< Counter.

  NN = SIZE(X, DIM=1)
  CALL PACK_DATA(A1=[INT(6*NN*BYI1P, I4P)], A2=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1, NN)], PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ, CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK1_I1P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_R8P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (R8P).
  REAL(R8P),    INTENT(IN)      :: U(1:,1:,1:) !< U component.
  REAL(R8P),    INTENT(IN)      :: V(1:,1:,1:) !< V component.
  REAL(R8P),    INTENT(IN)      :: W(1:,1:,1:) !< W component.
  REAL(R8P),    INTENT(IN)      :: X(1:,1:,1:) !< X component.
  REAL(R8P),    INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  REAL(R8P),    INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(6*NN*BYR8P, I4P)], A2=[(((U(N1, N2, N3), V(N1, N2, N3), W(N1, N2, N3), &
                                                   X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_R8P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_R4P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (R4P).
  REAL(R4P),    INTENT(IN)      :: U(1:,1:,1:) !< U component.
  REAL(R4P),    INTENT(IN)      :: V(1:,1:,1:) !< V component.
  REAL(R4P),    INTENT(IN)      :: W(1:,1:,1:) !< W component.
  REAL(R4P),    INTENT(IN)      :: X(1:,1:,1:) !< X component.
  REAL(R4P),    INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  REAL(R4P),    INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(6*NN*BYR4P, I4P)], A2=[(((U(N1, N2, N3), V(N1, N2, N3), W(N1, N2, N3), &
                                                   X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_R4P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_I8P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I8P).
  INTEGER(I8P), INTENT(IN)      :: U(1:,1:,1:) !< U component.
  INTEGER(I8P), INTENT(IN)      :: V(1:,1:,1:) !< V component.
  INTEGER(I8P), INTENT(IN)      :: W(1:,1:,1:) !< W component.
  INTEGER(I8P), INTENT(IN)      :: X(1:,1:,1:) !< X component.
  INTEGER(I8P), INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  INTEGER(I8P), INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(6*NN*BYI8P, I4P)], A2=[(((U(N1, N2, N3), V(N1, N2, N3), W(N1, N2, N3), &
                                                   X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_I8P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_I4P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I4P).
  INTEGER(I4P), INTENT(IN)      :: U(1:,1:,1:) !< U component.
  INTEGER(I4P), INTENT(IN)      :: V(1:,1:,1:) !< V component.
  INTEGER(I4P), INTENT(IN)      :: W(1:,1:,1:) !< W component.
  INTEGER(I4P), INTENT(IN)      :: X(1:,1:,1:) !< X component.
  INTEGER(I4P), INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  INTEGER(I4P), INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  XYZ = TRANSFER([INT(6*NN*BYI4P, I4P), [(((U(N1, N2, N3), V(N1, N2, N3), W(N1, N2, N3), &
                                            X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                         N2=1, NN2),  &
                                                                                         N3=1, NN3)]], XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_I4P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_I2P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I2P).
  INTEGER(I2P), INTENT(IN)      :: U(1:,1:,1:) !< U component.
  INTEGER(I2P), INTENT(IN)      :: V(1:,1:,1:) !< V component.
  INTEGER(I2P), INTENT(IN)      :: W(1:,1:,1:) !< W component.
  INTEGER(I2P), INTENT(IN)      :: X(1:,1:,1:) !< X component.
  INTEGER(I2P), INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  INTEGER(I2P), INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(6*NN*BYI2P, I4P)], A2=[(((U(N1, N2, N3), V(N1, N2, N3), W(N1, N2, N3), &
                                                   X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_I2P

  FUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_I1P(U, V, W, X, Y, Z) RESULT(CODE)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I1P).
  INTEGER(I1P), INTENT(IN)      :: U(1:,1:,1:) !< U component.
  INTEGER(I1P), INTENT(IN)      :: V(1:,1:,1:) !< V component.
  INTEGER(I1P), INTENT(IN)      :: W(1:,1:,1:) !< W component.
  INTEGER(I1P), INTENT(IN)      :: X(1:,1:,1:) !< X component.
  INTEGER(I1P), INTENT(IN)      :: Y(1:,1:,1:) !< Y component.
  INTEGER(I1P), INTENT(IN)      :: Z(1:,1:,1:) !< Z component.
  CHARACTER(LEN=:), ALLOCATABLE :: CODE        !< Encoded base64 dataarray.
  INTEGER(I1P),     ALLOCATABLE :: XYZ(:)      !< Packed data.
  INTEGER(I4P)                  :: NN1         !< Number of elements along dim 1.
  INTEGER(I4P)                  :: NN2         !< Number of elements along dim 2.
  INTEGER(I4P)                  :: NN3         !< Number of elements along dim 3.
  INTEGER(I4P)                  :: NN          !< Number of elements.
  INTEGER(I4P)                  :: N1          !< Counter.
  INTEGER(I4P)                  :: N2          !< Counter.
  INTEGER(I4P)                  :: N3          !< Counter.

  NN1 = SIZE(X, DIM=1)
  NN2 = SIZE(X, DIM=2)
  NN3 = SIZE(X, DIM=3)
  NN = NN1*NN2*NN3
  CALL PACK_DATA(A1=[INT(6*NN*BYI1P, I4P)], A2=[(((U(N1, N2, N3), V(N1, N2, N3), W(N1, N2, N3), &
                                                   X(N1, N2, N3), Y(N1, N2, N3), Z(N1, N2, N3), N1=1, NN1),  &
                                                                                                N2=1, NN2),  &
                                                                                                N3=1, NN3)], &
                 PACKED=XYZ)
  CALL B64_ENCODE(N=XYZ,CODE=CODE)
  ENDFUNCTION ENCODE_BINARY_DATAARRAY6_RANK3_I1P
ENDMODULE ModLib_VTKDataEncoder
