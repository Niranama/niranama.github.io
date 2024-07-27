!< BeFoR64, Base64 encoding/decoding library for FoRtran poor people.

MODULE ModLib_BeFor64
!< BeFoR64, Base64 encoding/decoding library for FoRtran poor people.
USE ModLib_Penf
USE ModLib_BeFor64PackData

IMPLICIT NONE
PRIVATE
PUBLIC :: IS_B64_INITIALIZED, B64_INIT
PUBLIC :: B64_ENCODE, B64_ENCODE_UP
PUBLIC :: B64_DECODE, B64_DECODE_UP
PUBLIC :: PACK_DATA

LOGICAL       :: IS_B64_INITIALIZED=.FALSE. !< Flag for checking the initialization of the library.
CHARACTER(64) :: BASE64="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !< Base64 alphabet.

INTERFACE B64_ENCODE
  !< Encode numbers (integer and real) to base64.
  !<
  !< This is an interface for encoding integer and real numbers of any kinds into a base64 string. This interfACE CAN ENCODE BOTH
  !< scalar and array.
  !<
  !< @warning The encoded string is returned as varying length character string, `character(len=:), allocatablE:: STRING`, THUS THE
  !< compiler must support such a Fortran (2003) feature.
  !<
  !< @note Before start to encode anything the library must be initialized. The procedure `b64_init` must be cALLED AT FIRST. THE
  !< global variable `is_b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar encoding
  !<```ortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode(n=12._R8P,code=code64)
  !<```
  !<
  !<#### Array encoding
  !<```ortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode(n=[12_I4P,1_I4P],code=code64)
  !<```
  !<
  !< @note If you want to encode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `PACK_DATA`
  !< procedure.
  !<
  !< @warning The encoding of array of strings is admitted only if each string of the array has the same lengtH.
  MODULE PROCEDURE &
#ifdef _R16P
                   B64_ENCODE_R16,    B64_ENCODE_R16_A, &
#endif
                   B64_ENCODE_R8,     B64_ENCODE_R8_A,  &
                   B64_ENCODE_R4,     B64_ENCODE_R4_A,  &
                   B64_ENCODE_I8,     B64_ENCODE_I8_A,  &
                   B64_ENCODE_I4,     B64_ENCODE_I4_A,  &
                   B64_ENCODE_I2,     B64_ENCODE_I2_A,  &
                   B64_ENCODE_I1,     B64_ENCODE_I1_A,  &
                   B64_ENCODE_STRING, B64_ENCODE_STRING_A
ENDINTERFACE

INTERFACE B64_ENCODE_UP
  !< Encode unlimited polymorphic variable to base64.
  !<
  !< This is an interface for encoding both scalar and array.
  !<
  !< @warning The encoded string is returned as varying length character string, `character(len=:), allocatablE:: STRING`, THUS THE
  !< compiler must support such a Fortran (2003) feature.
  !<
  !< @note Before start to encode anything the library must be initialized. The procedure `b64_init` must be cALLED AT FIRST. THE
  !< global variable `is_b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar encoding
  !<```ortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode_up(up=12._R8P,code=code64)
  !<```
  !<
  !<#### Array encoding
  !<```ortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode_up(up=[12_I4P,1_I4P],code=code64)
  !<```
  !<
  !< @note If you want to encode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `PACK_DATA`
  !< procedure.
  !<
  !< @warning The encoding of array of strings is admitted only if each string of the array has the same lengtH.
  MODULE PROCEDURE B64_ENCODE_UP, B64_ENCODE_UP_A
ENDINTERFACE

INTERFACE B64_DECODE
  !< Decode numbers (integer and real) from base64.
  !<
  !< This is an interface for decoding integer and real numbers of any kinds from a base64 string. This interfACE CAN DECODE BOTH
  !< scalar and array.
  !<
  !< @note Before start to decode anything the library must be initialized. The procedure `b64_init` must be cALLED AT FIRST. THE
  !< global variable `is_b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar decoding
  !<```ortran
  !<real(R8P):: decoded ! scalar to be decoded
  !<...
  !<call b64_decode(code='AAAAAAAA8D8=',n=decoded)
  !<```
  !<
  !<#### Array decoding
  !<```ortran
  !<integer(I8P):: decoded(1:4) ! array to be decoded
  !<...
  !<call b64_decode(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=',n=decoded)
  !<```
  !<
  !< @note If you want to decode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `PACK_DATA`
  !< procedure.
  !<
  !< @warning The decoding of array of strings is admitted only if each string of the array has the same lengtH.
  MODULE PROCEDURE &
#ifdef _R16P
                   B64_DECODE_R16,    B64_DECODE_R16_A, &
#endif
                   B64_DECODE_R8,     B64_DECODE_R8_A,  &
                   B64_DECODE_R4,     B64_DECODE_R4_A,  &
                   B64_DECODE_I8,     B64_DECODE_I8_A,  &
                   B64_DECODE_I4,     B64_DECODE_I4_A,  &
                   B64_DECODE_I2,     B64_DECODE_I2_A,  &
                   B64_DECODE_I1,     B64_DECODE_I1_A,  &
                   B64_DECODE_STRING, B64_DECODE_STRING_A
ENDINTERFACE

INTERFACE B64_DECODE_UP
  !< Decode unlimited polymorphic variable from base64.
  !<
  !< This is an interface for decoding both scalar and array.
  !<
  !< @note Before start to decode anything the library must be initialized. The procedure `b64_init` must be cALLED AT FIRST. THE
  !< global variable `is_b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar decoding
  !<```ortran
  !<real(R8P):: decoded ! scalar to be decoded
  !<...
  !<call b64_decode_up(code='AAAAAAAA8D8=',up=decoded)
  !<```
  !<
  !<#### Array decoding
  !<```ortran
  !<integer(I8P):: decoded(1:4) ! array to be decoded
  !<...
  !<call b64_decode_up(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=',up=decoded)
  !<```
  !<
  !< @note If you want to decode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `PACK_DATA`
  !< procedure.
  !<
  !< @warning The decoding of array of strings is admitted only if each string of the array has the same lengtH.
  MODULE PROCEDURE B64_DECODE_UP, B64_DECODE_UP_A
ENDINTERFACE

CONTAINS
   SUBROUTINE B64_INIT()
   !< Initialize the BeFoR64 library.
   !<
   !< @note This procedure **must** be called before encoding/decoding anything!
   !<
   !<```fortran
   !< use befor64
   !< call b64_init
   !< print "(L1)", is_b64_initialized
   !<```
   !=> T <<<

   IF (.NOT.IS_INITIALIZED) CALL PENF_INIT
   IS_B64_INITIALIZED = .TRUE.
   ENDSUBROUTINE B64_INIT

   PURE SUBROUTINE ENCODE_BITS(BITS, PADD, CODE)
   !< Encode a bits stream (must be multiple of 24 bits) into base64 charcaters code (of length multiple of 4).
   !<
   !< The bits stream are encoded in chunks of 24 bits as the following example (in little endian order)
   !<```
   !< +--first octet--+-second octet--+--third octet--+
   !< |7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|
   !< +-----------+---+-------+-------+---+-----------+
   !< |5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|
   !< +--1.index--+--2.index--+--3.index--+--4.index--+
   !<```
   !< @note The 4 indexes are stored into 4 elements 8 bits array, thus 2 bits of each array element are not uSED.
   !<
   !< @note The number of paddings must be computed outside this procedure, into the calling scope.
   !<
   !< @warning This procedure is the backend of encoding, thus it must be never called outside the module.
   INTEGER(I1P), INTENT(IN)  :: BITS(1:)  !< Bits to be encoded.
   INTEGER(I4P), INTENT(IN)  :: PADD      !< Number of padding characters ('=').
   CHARACTER(*), INTENT(OUT) :: CODE      !< Characters code.
   INTEGER(I1P)              :: SIXB(1:4) !< 6 bits slices (stored into 8 bits integer) of 24 bits input.
   INTEGER(I8P)              :: C         !< Counter.
   INTEGER(I8P)              :: E         !< Counter.
   INTEGER(I8P)              :: NB        !< Length of bits array.

   NB=SIZE(BITS,DIM=1,KIND=I8P)
   C = 1_I8P
   DO E=1_I8P,NB,3_I8P ! loop over array elements: 3 bytes (24 bits) scanning
      SIXB = 0_I1P
         CALL MVBITS(BITS(E  ),2,6,SIXB(1),0)
         CALL MVBITS(BITS(E  ),0,2,SIXB(2),4)
      IF (E+1<=NB) THEN
         CALL MVBITS(BITS(E+1),4,4,SIXB(2),0)
         CALL MVBITS(BITS(E+1),0,4,SIXB(3),2)
      ENDIF
      IF (E+2<=NB) THEN
         CALL MVBITS(BITS(E+2),6,2,SIXB(3),0)
         CALL MVBITS(BITS(E+2),0,6,SIXB(4),0)
      ENDIF
      SIXB = SIXB + 1_I1P
      CODE(C  :C  ) = BASE64(SIXB(1):SIXB(1))
      CODE(C+1:C+1) = BASE64(SIXB(2):SIXB(2))
      CODE(C+2:C+2) = BASE64(SIXB(3):SIXB(3))
      CODE(C+3:C+3) = BASE64(SIXB(4):SIXB(4))
      C = C + 4_I8P
   ENDDO
   IF (PADD>0) CODE(LEN(CODE)-PADD+1:)=REPEAT('=',PADD)
   ENDSUBROUTINE ENCODE_BITS

   PURE SUBROUTINE DECODE_BITS(CODE, BITS)
   !< Decode a base64 string into a sequence of bits stream.
   !<
   !< The base64 string must be parsed with a strike of 4 characters and converted into a 3 bytes stream. ConsIDERING THE BASE64 CODE
   !< `QUJD` the decoding process must do
   !<```
   !< +-b64 char--+-b64 char--+-b64 char--+-b64 char--+
   !< |      Q    |      U    |      J    |      D    |
   !< +-b64 index-+-b64 index-+-b64 index-+-b64 index-+
   !< !      16   |      20   |      9    |      3    |
   !< +-6 bits----+-6 bits----+-6 bits----+-6 bits----+
   !< |0 1 0 0 0 0|0 1 0 1 0 0|0 0 1 0 0 1|0 0 0 0 1 1|
   !< +-----------+---+-------+-------+---+-----------+
   !< |0 1 0 0 0 0 0 1|0 1 0 0 0 0 1 0|0 1 0 0 0 0 1 1|
   !< +-----8 bits----+-----8 bits----+-----8 bits----+
   !<```
   !< @note The bits pattern is returned as a 1-byte element array, the dimension of witch must be computed ouTSIDE THIS PROCEDURE.
   !<
   !< @warning This procedure is the backend of decoding, thus it must be never called outside the module.
   CHARACTER(*), INTENT(IN)  :: CODE      !< Characters code.
   INTEGER(I1P), INTENT(OUT) :: BITS(1:)  !< Bits decoded.
   INTEGER(I1P)              :: SIXB(1:4) !< 6 bits slices (stored into 8 bits integer) of 24 bits input.
   INTEGER(I8P)              :: C         !< Counter.
   INTEGER(I8P)              :: E         !< Counter.
   INTEGER(I8P)              :: NB        !< Length of bits array.

   NB=SIZE(BITS,DIM=1,KIND=I8P)
   E = 1_I8P
   DO C=1_I8P,LEN(CODE),4_I8P ! loop over code characters: 3 bytes (24 bits) scanning
      SIXB = 0_I1P
      SIXB(1) = INDEX(BASE64,CODE(C  :C  )) - 1
      SIXB(2) = INDEX(BASE64,CODE(C+1:C+1)) - 1
      SIXB(3) = INDEX(BASE64,CODE(C+2:C+2)) - 1
      SIXB(4) = INDEX(BASE64,CODE(C+3:C+3)) - 1
         CALL MVBITS(SIXB(1),0,6,BITS(E  ),2) ; CALL MVBITS(SIXB(2),4,2,BITS(E  ),0)
      IF (E+1<=NB) THEN
         CALL MVBITS(SIXB(2),0,4,BITS(E+1),4) ; CALL MVBITS(SIXB(3),2,4,BITS(E+1),0)
      ENDIF
      IF (E+2<=NB) THEN
         CALL MVBITS(SIXB(3),0,2,BITS(E+2),6) ; CALL MVBITS(SIXB(4),0,6,BITS(E+2),0)
      ENDIF
      E = E + 3_I8P
   ENDDO
   ENDSUBROUTINE DECODE_BITS

   SUBROUTINE B64_ENCODE_UP(UP, CODE)
   !< Encode an unlimited polymorphic scalar to base64.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode_up(up=1._R8P, code=code64)
   !< print "(A)", code64
   !<```
   !=> AAAAAAAA8D8= <<<
   CLASS(*),                      INTENT(IN)  :: UP   !< Unlimited polymorphic variable to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE !< Encoded scalar.

   SELECT TYPE(UP)
   TYPE IS(REAL(R8P))
      CALL B64_ENCODE_R8(N=UP,CODE=CODE)
   TYPE IS(REAL(R4P))
      CALL B64_ENCODE_R4(N=UP,CODE=CODE)
   TYPE IS(INTEGER(I8P))
      CALL B64_ENCODE_I8(N=UP,CODE=CODE)
   TYPE IS(INTEGER(I4P))
      CALL B64_ENCODE_I4(N=UP,CODE=CODE)
   TYPE IS(INTEGER(I2P))
      CALL B64_ENCODE_I2(N=UP,CODE=CODE)
   TYPE IS(INTEGER(I1P))
      CALL B64_ENCODE_I1(N=UP,CODE=CODE)
   TYPE IS(CHARACTER(*))
      CALL B64_ENCODE_STRING(S=UP,CODE=CODE)
   ENDSELECT
   ENDSUBROUTINE B64_ENCODE_UP

   PURE SUBROUTINE B64_ENCODE_UP_A(UP, CODE)
   !< Encode an unlimited polymorphic array to base64.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode_up(up=[0._R4P,-32.12_R4P], code=code64)
   !< print "(A)", code64
   !<```
   !=> AAAAAOF6AMI= <<<
   CLASS(*),                      INTENT(IN)  :: UP(1:) !< Unlimited polymorphic variable to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE   !< Encoded array.

   SELECT TYPE(UP)
   TYPE IS(REAL(R8P))
      CALL B64_ENCODE_R8_A(N=UP,CODE=CODE)
   TYPE IS(REAL(R4P))
      CALL B64_ENCODE_R4_A(N=UP,CODE=CODE)
   TYPE IS(INTEGER(I8P))
      CALL B64_ENCODE_I8_A(N=UP,CODE=CODE)
   TYPE IS(INTEGER(I4P))
      CALL B64_ENCODE_I4_A(N=UP,CODE=CODE)
   TYPE IS(INTEGER(I2P))
      CALL B64_ENCODE_I2_A(N=UP,CODE=CODE)
   TYPE IS(INTEGER(I1P))
      CALL B64_ENCODE_I1_A(N=UP,CODE=CODE)
   TYPE IS(CHARACTER(*))
      CALL B64_ENCODE_STRING_A(S=UP,CODE=CODE)
   ENDSELECT
   ENDSUBROUTINE B64_ENCODE_UP_A

   SUBROUTINE B64_DECODE_UP(CODE, UP)
   !< Decode an unlimited polymorphic scalar from base64.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P) :: scalar_I4
   !< call b64_decode_up(code='5wcAAA==',up=scalar_I4)
   !< print "(L1)", scalar_I4==2023_I4P
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE !< Encoded scalar.
   CLASS(*),     INTENT(OUT) :: UP   !< Unlimited polymorphic variable to be decoded.

   SELECT TYPE(UP)
   TYPE IS(REAL(R8P))
      CALL B64_DECODE_R8(CODE=CODE,N=UP)
   TYPE IS(REAL(R4P))
      CALL B64_DECODE_R4(CODE=CODE,N=UP)
   TYPE IS(INTEGER(I8P))
      CALL B64_DECODE_I8(CODE=CODE,N=UP)
   TYPE IS(INTEGER(I4P))
      CALL B64_DECODE_I4(CODE=CODE,N=UP)
   TYPE IS(INTEGER(I2P))
      CALL B64_DECODE_I2(CODE=CODE,N=UP)
   TYPE IS(INTEGER(I1P))
      CALL B64_DECODE_I1(CODE=CODE,N=UP)
   TYPE IS(CHARACTER(*))
      CALL B64_DECODE_STRING(CODE=CODE,S=UP)
   ENDSELECT
   ENDSUBROUTINE B64_DECODE_UP

   SUBROUTINE B64_DECODE_UP_A(CODE, UP)
   !< Decode an unlimited polymorphic array from base64.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P) :: array_I8(1:4)
   !< call b64_decode_up(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=', up=array_I8)
   !< print "(L1)", str(n=array_I8)==str(n=[23_I8P,324_I8P,25456656_I8P,2_I8P])
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE   !< Encoded array.
   CLASS(*),     INTENT(OUT) :: UP(1:) !< Unlimited polymorphic variable to be decoded.

   SELECT TYPE(UP)
   TYPE IS(REAL(R8P))
      CALL B64_DECODE_R8_A(CODE=CODE,N=UP)
   TYPE IS(REAL(R4P))
      CALL B64_DECODE_R4_A(CODE=CODE,N=UP)
   TYPE IS(INTEGER(I8P))
      CALL B64_DECODE_I8_A(CODE=CODE,N=UP)
   TYPE IS(INTEGER(I4P))
      CALL B64_DECODE_I4_A(CODE=CODE,N=UP)
   TYPE IS(INTEGER(I2P))
      CALL B64_DECODE_I2_A(CODE=CODE,N=UP)
   TYPE IS(INTEGER(I1P))
      CALL B64_DECODE_I1_A(CODE=CODE,N=UP)
   TYPE IS(CHARACTER(*))
      CALL B64_DECODE_STRING_A(CODE=CODE,S=UP)
   ENDSELECT
   ENDSUBROUTINE B64_DECODE_UP_A

   PURE SUBROUTINE B64_ENCODE_R16(N, CODE)
   !< Encode scalar number to base64 (R16P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=134.231_R16P, code=code64)
   !< print "(A)", code64
   !<```
   !=> CKwcWmTHYEA= <<<
   REAL(R16P),                    INTENT(IN)  :: N       !< Number to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded scalar.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').

   ALLOCATE(NI1P(1:((BYR16P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((BYR16P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
#ifdef _R16P
   PADD = MOD((BYR16P),3_I2P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
#else
   PADD = MOD((BYR16P),3_I1P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
#endif
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_R16

   PURE SUBROUTINE B64_ENCODE_R8(N, CODE)
   !< Encode scalar number to base64 (R8P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=1._R8P, code=code64)
   !< print "(A)", code64
   !<```
   !=> AAAAAAAA8D8= <<<
   REAL(R8P),                     INTENT(IN)  :: N       !< Number to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded scalar.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').

   ALLOCATE(NI1P(1:((BYR8P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((BYR8P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((BYR8P),3_I1P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_R8

   PURE SUBROUTINE B64_ENCODE_R4(N, CODE)
   !< Encode scalar number to base64 (R4P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=0._R4P, code=code64)
   !< print "(A)", code64
   !<```
   !=> AAAAAA== <<<
   REAL(R4P),                     INTENT(IN)  :: N       !< Number to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded scalar.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').

   ALLOCATE(NI1P(1:((BYR4P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((BYR4P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((BYR4P),3_I1P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_R4

   PURE SUBROUTINE B64_ENCODE_I8(N, CODE)
   !< Encode scalar number to base64 (I8P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=23_I8P, code=code64)
   !< print "(A)", code64
   !<```
   !=> FwAAAAAAAAA= <<<
   INTEGER(I8P),                  INTENT(IN)  :: N       !< Number to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded scalar.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').

   ALLOCATE(NI1P(1:((BYI8P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((BYI8P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((BYI8P),3_I8P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_I8

   PURE SUBROUTINE B64_ENCODE_I4(N, CODE)
   !< Encode scalar number to base64 (I4P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=2023_I4P, code=code64)
   !< print "(A)", code64
   !<```
   !=> 5wcAAA== <<<
   INTEGER(I4P),                  INTENT(IN)  :: N       !< Number to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded scalar.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').

   ALLOCATE(NI1P(1:((BYI4P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((BYI4P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((BYI4P),3_I4P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_I4

   PURE SUBROUTINE B64_ENCODE_I2(N, CODE)
   !< Encode scalar number to base64 (I2P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=-203_I2P, code=code64)
   !< print "(A)", code64
   !<```
   !=> Nf8= <<<
   INTEGER(I2P),                  INTENT(IN)  :: N       !< Number to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded scalar.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').

   ALLOCATE(NI1P(1:((BYI2P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((BYI2P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((BYI2P),3_I2P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_I2

   PURE SUBROUTINE B64_ENCODE_I1(N, CODE)
   !< Encode scalar number to base64 (I1P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=120_I1P, code=code64)
   !< print "(A)", code64
   !<```
   !=> eA== <<<
   INTEGER(I1P),                  INTENT(IN)  :: N       !< Number to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded scalar.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').

   ALLOCATE(NI1P(1:((BYI1P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((BYI1P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((BYI1P),3_I1P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_I1

   PURE SUBROUTINE B64_ENCODE_STRING(S, CODE)
   !< Encode scalar string to base64.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(s='hello', code=code64)
   !< print "(A)", code64
   !<```
   !=> aGVsbG8= <<<
   CHARACTER(*),                  INTENT(IN)  :: S       !< String to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded scalar.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').
   INTEGER(I4P)                               :: BYCHS   !< Bytes of character string.

   BYCHS = BYTE_SIZE(S)
   ALLOCATE(NI1P(1:((BYCHS+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((BYCHS+2)/3)*4)
   NI1P = TRANSFER(S,NI1P)
   PADD = MOD((BYCHS),3_I4P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_STRING

   PURE SUBROUTINE B64_ENCODE_R16_A(N, CODE)
   !< Encode array numbers to base64 (R16P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=[121._R16P,2.32_R16P], code=code64)
   !< print "(A)", code64
   !<```
   !=> AAAAAABAXkCPwvUoXI8CQA== <<<
   REAL(R16P),                    INTENT(IN)  :: N(1:)   !< Array of numbers to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded array.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').
   INTEGER(I8P)                               :: NS      !< Size of n.

   NS = SIZE(N,DIM=1)
   ALLOCATE(NI1P(1:((NS*BYR16P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((NS*BYR16P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((NS*BYR16P),3_I8P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_R16_A

   PURE SUBROUTINE B64_ENCODE_R8_A(N, CODE)
   !< Encode array numbers to base64 (R8P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=[1._R8P,2._R8P], code=code64)
   !< print "(A)", code64
   !<```
   !=> AAAAAAAA8D8AAAAAAAAAQA== <<<
   REAL(R8P),                     INTENT(IN)  :: N(1:)   !< Array of numbers to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded array.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').
   INTEGER(I8P)                               :: NS      !< Size of n.

   NS = SIZE(N,DIM=1)
   ALLOCATE(NI1P(1:((NS*BYR8P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((NS*BYR8P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((NS*BYR8P),3_I8P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_R8_A

   PURE SUBROUTINE B64_ENCODE_R4_A(N, CODE)
   !< Encode array numbers to base64 (R4P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=[0._R4P,-32.12_R4P], code=code64)
   !< print "(A)", code64
   !<```
   !=> AAAAAOF6AMI= <<<
   REAL(R4P),                     INTENT(IN)  :: N(1:)   !< Array of numbers to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded array.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').
   INTEGER(I8P)                               :: NS      !< Size of n.

   NS = SIZE(N,DIM=1)
   ALLOCATE(NI1P(1:((NS*BYR4P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((NS*BYR4P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((NS*BYR4P),3_I8P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_R4_A

   PURE SUBROUTINE B64_ENCODE_I8_A(N, CODE)
   !< Encode array numbers to base64 (I8P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=[23_I8P,324_I8P,25456656_I8P,2_I8P], code=code64)
   !< print "(A)", code64
   !<```
   !=> FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA= <<<
   INTEGER(I8P),                  INTENT(IN)  :: N(1:)   !< Array of numbers to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded array.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').
   INTEGER(I8P)                               :: NS      !< Size of n.

   NS = SIZE(N,DIM=1)
   ALLOCATE(NI1P(1:((NS*BYI8P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((NS*BYI8P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((NS*BYI8P),3_I8P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_I8_A

   PURE SUBROUTINE B64_ENCODE_I4_A(N, CODE)
   !< Encode array numbers to base64 (I4P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=[2023_I4P,-24_I4P], code=code64)
   !< print "(A)", code64
   !<```
   !=> 5wcAAOj///8= <<<
   INTEGER(I4P),                  INTENT(IN)  :: N(1:)   !< Array of numbers to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded array.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').
   INTEGER(I8P)                               :: NS      !< Size of n.

   NS = SIZE(N,DIM=1)
   ALLOCATE(NI1P(1:((NS*BYI4P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((NS*BYI4P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((NS*BYI4P),3_I8P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_I4_A

   PURE SUBROUTINE B64_ENCODE_I2_A(N, CODE)
   !< Encode array numbers to base64 (I2P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=[-203_I2P,-10_I2P], code=code64)
   !< print "(A)", code64
   !<```
   !=> Nf/2/w== <<<
   INTEGER(I2P),                  INTENT(IN)  :: N(1:)   !< Array of numbers to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded array.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').
   INTEGER(I8P)                               :: NS      !< Size of n.

   NS = SIZE(N,DIM=1)
   ALLOCATE(NI1P(1:((NS*BYI2P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((NS*BYI2P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((NS*BYI2P),3_I8P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_I2_A

   PURE SUBROUTINE B64_ENCODE_I1_A(N, CODE)
   !< Encode array numbers to base64 (I1P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(n=[120_I1P,-1_I1P], code=code64)
   !< print "(A)", code64
   !<```
   !=> eP8= <<<
   INTEGER(I1P),                  INTENT(IN)  :: N(1:)   !< Array of numbers to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded array.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').
   INTEGER(I8P)                               :: NS      !< Size of n.

   NS = SIZE(N,DIM=1)
   ALLOCATE(NI1P(1:((NS*BYI1P+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((NS*BYI1P+2)/3)*4)
   NI1P = TRANSFER(N,NI1P)
   PADD = MOD((NS*BYI1P),3_I8P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_I1_A

   PURE SUBROUTINE B64_ENCODE_STRING_A(S, CODE)
   !< Encode array string to base64.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(len=:), allocatable :: code64
   !< call b64_encode(s=['hello','world'], code=code64)
   !< print "(A)", code64
   !<```
   !=> aGVsbG93b3JsZA== <<<
   CHARACTER(*),                  INTENT(IN)  :: S(1:)   !< String to be encoded.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CODE    !< Encoded scalar.
   INTEGER(I1P),     ALLOCATABLE              :: NI1P(:) !< One byte integer array containing n.
   INTEGER(I4P)                               :: PADD    !< Number of padding characters ('=').
   INTEGER(I4P)                               :: BYCHS   !< Bytes of character string.

   BYCHS = BYTE_SIZE(S(1))*SIZE(S,DIM=1)
   ALLOCATE(NI1P(1:((BYCHS+2)/3)*3)) ; NI1P = 0_I1P
   CODE = REPEAT(' ',((BYCHS+2)/3)*4)
   NI1P = TRANSFER(S,NI1P)
   PADD = MOD((BYCHS),3_I4P) ; IF (PADD>0_I4P) PADD = 3_I4P - PADD
   CALL ENCODE_BITS(BITS=NI1P,PADD=PADD,CODE=CODE)
   ENDSUBROUTINE B64_ENCODE_STRING_A

   ELEMENTAL SUBROUTINE B64_DECODE_R16(CODE, N)
   !< Decode a base64 code into a scalar number (R16P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R16P) :: scalar_R16
   !< call b64_decode(code='CKwcWmTHYEA=',n=scalar_R16)
   !< print "(L1)", scalar_R16==134.231_R16P
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded scalar.
   REAL(R16P),   INTENT(OUT) :: N       !< Number to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:BYR16P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_R16

   ELEMENTAL SUBROUTINE B64_DECODE_R8(CODE, N)
   !< Decode a base64 code into a scalar number (R8P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P) :: scalar_R8
   !< call b64_decode(code='AAAAAAAA8D8=',n=scalar_R8)
   !< print "(L1)", scalar_R8==1._R8P
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded scalar.
   REAL(R8P),    INTENT(OUT) :: N       !< Number to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:BYR8P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_R8

   ELEMENTAL SUBROUTINE B64_DECODE_R4(CODE, N)
   !< Decode a base64 code into a scalar number (R4P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P) :: scalar_R4
   !< call b64_decode(code='AAAAAA==',n=scalar_R4)
   !< print "(L1)", scalar_R4==0._R4P
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded scalar.
   REAL(R4P),    INTENT(OUT) :: N       !< Number to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:BYR4P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_R4

   ELEMENTAL SUBROUTINE B64_DECODE_I8(CODE, N)
   !< Decode a base64 code into a scalar number (I8P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P) :: scalar_I8
   !< call b64_decode(code='FwAAAAAAAAA=',n=scalar_I8)
   !< print "(L1)", scalar_I8==23_I8P
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded scalar.
   INTEGER(I8P), INTENT(OUT) :: N       !< Number to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:BYI8P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_I8

   ELEMENTAL SUBROUTINE B64_DECODE_I4(CODE, N)
   !< Decode a base64 code into a scalar number (I4P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P) :: scalar_I4
   !< call b64_decode(code='5wcAAA==',n=scalar_I4)
   !< print "(L1)", scalar_I4==2023_I4P
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded scalar.
   INTEGER(I4P), INTENT(OUT) :: N       !< Number to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:BYI4P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_I4

   ELEMENTAL SUBROUTINE B64_DECODE_I2(CODE, N)
   !< Decode a base64 code into a scalar number (I2P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P) :: scalar_I2
   !< call b64_decode(code='Nf8=',n=scalar_I2)
   !< print "(L1)", scalar_I2==-203_I2P
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded scalar.
   INTEGER(I2P), INTENT(OUT) :: N       !< Number to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:BYI2P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_I2

   ELEMENTAL SUBROUTINE B64_DECODE_I1(CODE, N)
   !< Decode a base64 code into a scalar number (I1P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P) :: scalar_I1
   !< call b64_decode(code='eA==',n=scalar_I1)
   !< print "(L1)", scalar_I1==120_I1P
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded scalar.
   INTEGER(I1P), INTENT(OUT) :: N       !< Number to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:BYI1P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_I1

   ELEMENTAL SUBROUTINE B64_DECODE_STRING(CODE, S)
   !< Decode a base64 code into a scalar string.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(:), allocatable :: code64
   !< code64 = repeat(' ',5)
   !< call b64_decode(code='aGVsbG8=',s=code64)
   !< print "(L1)", code64=='hello'
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded scalar.
   CHARACTER(*), INTENT(OUT) :: S       !< String to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:BYTE_SIZE(S))) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   S = TRANSFER(NI1P,S)
   ENDSUBROUTINE B64_DECODE_STRING

   PURE SUBROUTINE B64_DECODE_R16_A(CODE, N)
   !< Decode a base64 code into an array numbers (R16P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R16P) :: array_R16(1:2)
   !< call b64_decode(code='AAAAAABAXkCPwvUoXI8CQA==',n=array_R16)
   !< print "(L1)", str(n=array_R16)==str(n=[121._R16P,2.32_R16P])
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded array.
   REAL(R16P),   INTENT(OUT) :: N(1:)   !< Array of numbers to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:SIZE(N,DIM=1)*BYR16P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_R16_A

   PURE SUBROUTINE B64_DECODE_R8_A(CODE, N)
   !< Decode a base64 code into an array numbers (R8P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P) :: array_R8(1:2)
   !< call b64_decode(code='AAAAAAAA8D8AAAAAAAAAQA==',n=array_R8)
   !< print "(L1)", str(n=array_R8)==str(n=[1._R8P,2._R8P])
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded array.
   REAL(R8P),    INTENT(OUT) :: N(1:)   !< Array of numbers to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:SIZE(N,DIM=1)*BYR8P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_R8_A

   PURE SUBROUTINE B64_DECODE_R4_A(CODE, N)
   !< Decode a base64 code into an array numbers (R4P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P) :: array_R4(1:2)
   !< call b64_decode(code='AAAAAOF6AMI=',n=array_R4)
   !< print "(L1)", str(n=array_R4)==str(n=[0._R4P,-32.12_R4P])
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded array.
   REAL(R4P),    INTENT(OUT) :: N(1:)   !< Array of numbers to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:SIZE(N,DIM=1)*BYR4P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_R4_A

   PURE SUBROUTINE B64_DECODE_I8_A(CODE, N)
   !< Decode a base64 code into an array numbers (I8P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P) :: array_I8(1:4)
   !< call b64_decode(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=',n=array_I8)
   !< print "(L1)", str(n=array_I8)==str(n=[23_I8P,324_I8P,25456656_I8P,2_I8P])
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded array.
   INTEGER(I8P), INTENT(OUT) :: N(1:)   !< Array of numbers to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:SIZE(N,DIM=1)*BYI8P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_I8_A

   PURE SUBROUTINE B64_DECODE_I4_A(CODE, N)
   !< Decode a base64 code into an array numbers (I4P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P) :: array_I4(1:2)
   !< call b64_decode(code='5wcAAOj///8=',n=array_I4)
   !< print "(L1)", str(n=array_I4)==str(n=[2023_I4P,-24_I4P])
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded array.
   INTEGER(I4P), INTENT(OUT) :: N(1:)   !< Array of numbers to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:SIZE(N,DIM=1)*BYI4P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_I4_A

   PURE SUBROUTINE B64_DECODE_I2_A(CODE, N)
   !< Decode a base64 code into an array numbers (I2P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P) :: array_I2(1:2)
   !< call b64_decode(code='Nf/2/w==',n=array_I2)
   !< print "(L1)", str(n=array_I2)==str(n=[-203_I2P,-10_I2P])
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded array.
   INTEGER(I2P), INTENT(OUT) :: N(1:)   !< Array of numbers to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:SIZE(N,DIM=1)*BYI2P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_I2_A

   PURE SUBROUTINE B64_DECODE_I1_A(CODE, N)
   !< Decode a base64 code into an array numbers (I1P).
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P) :: array_I1(1:2)
   !< call b64_decode(code='eP8=',n=array_I1)
   !< print "(L1)", str(n=array_I1)==str(n=[120_I1P,-1_I1P])
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded array.
   INTEGER(I1P), INTENT(OUT) :: N(1:)   !< Array of numbers to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:SIZE(N,DIM=1)*BYI1P)) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   N = TRANSFER(NI1P,N)
   ENDSUBROUTINE B64_DECODE_I1_A

   PURE SUBROUTINE B64_DECODE_STRING_A(CODE, S)
   !< Decode a base64 code into an array of strings.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< character(5) :: array_s(1:2)
   !< call b64_decode(code='aGVsbG93b3JsZA==',s=array_s)
   !< print "(L1)", array_s(1)//array_s(2)=='helloworld'
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN)  :: CODE    !< Encoded scalar.
   CHARACTER(*), INTENT(OUT) :: S(1:)   !< String to be decoded.
   INTEGER(I1P), ALLOCATABLE :: NI1P(:) !< One byte integer array containing n.

   ALLOCATE(NI1P(1:BYTE_SIZE(S(1))*SIZE(S,DIM=1))) ; NI1P = 0_I1P
   CALL DECODE_BITS(CODE=CODE,BITS=NI1P)
   S = TRANSFER(NI1P,S)
   ENDSUBROUTINE B64_DECODE_STRING_A
ENDMODULE ModLib_BeFor64
