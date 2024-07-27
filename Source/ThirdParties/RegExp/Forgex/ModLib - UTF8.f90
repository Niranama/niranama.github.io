!! Fortran Regular Expression (Forgex)
!!
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!     utf8_m module is a part of Forgex.

MODULE ModLib_UTF8
   IMPLICIT NONE

   INTEGER, PARAMETER :: UTF8_CODE_MAX = 2**21-1
   INTEGER, PARAMETER :: UTF8_CODE_MIN = 32 ! = 0x21: '!'
   INTEGER, PARAMETER :: UTF8_CODE_EMPTY = 0

   INTEGER, PARAMETER :: UTF8_CHAR_SIZE = 4

CONTAINS

   ! INDEX OF UTF8
   ! This function returns the index of the end of the (multibyte) character,
   ! given the string str and the current index curr.
   PURE FUNCTION IDXUTF8 (STR, CURR) RESULT(TAIL)
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: STR
      INTEGER(INT32), INTENT(IN) :: CURR
      INTEGER(INT32)  :: TAIL
      INTEGER(INT32) :: I
      INTEGER(INT8) :: BYTE, SHIFT_3, SHIFT_4, SHIFT_5, SHIFT_6, SHIFT_7

      TAIL = CURR

      DO I = 0, 3

         BYTE = INT(ICHAR(STR(CURR+I:CURR+I)), KIND(BYTE))

         SHIFT_3 = ISHFT(BYTE, -3)
         SHIFT_4 = ISHFT(BYTE, -4)
         SHIFT_5 = ISHFT(BYTE, -5)
         SHIFT_6 = ISHFT(BYTE, -6)
         SHIFT_7 = ISHFT(BYTE, -7)

         IF (SHIFT_6 == 2) CYCLE

         IF (I == 0) THEN

            IF (SHIFT_3 == 30 ) THEN ! 11110_2
               TAIL = CURR + 4 - 1
               RETURN
            END IF

            IF (SHIFT_4 == 14) THEN ! 1110_2
               TAIL = CURR + 3 - 1
               RETURN
            END IF

            IF (SHIFT_5 == 6) THEN  ! 110_2
               TAIL = CURR + 2 - 1
               RETURN
            END IF

            IF (SHIFT_7 == 0) THEN ! 0_2
               TAIL = CURR + 1 - 1
               RETURN
            END IF

         ELSE

            IF (SHIFT_3 == 30 .OR. SHIFT_4 == 14 .OR. SHIFT_5 == 6 .OR. SHIFT_7 == 0) THEN
               TAIL = CURR + I - 1
               RETURN
            END IF

         END IF

      END DO

   END FUNCTION IDXUTF8


   FUNCTION CHAR_UTF8 (CODE) RESULT(STR)
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      IMPLICIT NONE
      INTEGER(INT32), INTENT(IN) :: CODE
      CHARACTER(:), ALLOCATABLE :: STR

      CHARACTER(:), ALLOCATABLE :: BIN
      INTEGER(INT32) :: BUF, MASK
      INTEGER(INT8) :: BYTE(4)

      STR = ''
      BUF = CODE

      BIN = '0000000000000000000000000111111' ! lower 6-bit mask
      READ(BIN, '(b32.32)') MASK

      BYTE(1) = INT(IAND(ISHFT(BUF, -18), MASK),KIND(BYTE))

      BUF = CODE
      BYTE(2) = INT(IAND(ISHFT(BUF, -12), MASK), KIND(BYTE))

      BUF = CODE
      BYTE(3) = INT(IAND(ISHFT(BUF, -6), MASK), KIND(BYTE))

      BUF = CODE
      BYTE(4) = INT(IAND(BUF, MASK), KIND(BYTE))

      IF (CODE > 2**7-1) THEN

         IF (2**16 -1 < CODE) THEN
            ! the first byte of 4-byte character
            BYTE(1) = IBSET(BYTE(1),7)
            BYTE(1) = IBSET(BYTE(1),6)
            BYTE(1) = IBSET(BYTE(1),5)
            BYTE(1) = IBSET(BYTE(1),4)
            BYTE(1) = IBCLR(BYTE(1),3)
            BYTE(2) = SET_CONTINUATION_BYTE(BYTE(2))
            BYTE(3) = SET_CONTINUATION_BYTE(BYTE(3))
            BYTE(4) = SET_CONTINUATION_BYTE(BYTE(4))

         ! the first byte of 3-byte character
         ELSE IF (2**11 - 1 < CODE) THEN
            BYTE(1) = 0
            BYTE(2) = IBSET(BYTE(2), 7)
            BYTE(2) = IBSET(BYTE(2), 6)
            BYTE(2) = IBSET(BYTE(2), 5)
            BYTE(2) = IBCLR(BYTE(2), 4)
            BYTE(3) = SET_CONTINUATION_BYTE(BYTE(3))
            BYTE(4) = SET_CONTINUATION_BYTE(BYTE(4))

         ! the first byte of 2-byte character
         ELSE IF (2**7 -1 < CODE) THEN
            BYTE(1) = 0
            BYTE(2) = 0
            BYTE(3) = IBSET(BYTE(3), 7)
            BYTE(3) = IBSET(BYTE(3), 6)
            BYTE(3) = IBCLR(BYTE(3), 5)
            BYTE(4) = SET_CONTINUATION_BYTE(BYTE(4))
         END IF

         STR = CHAR(BYTE(1))//CHAR(BYTE(2))//CHAR(BYTE(3))//CHAR(BYTE(4))
         STR = TRIM(ADJUSTL(STR))

      ELSE
         STR = CHAR(CODE)
      END IF


   CONTAINS

      FUNCTION SET_CONTINUATION_BYTE(BYTE) RESULT(RES)
         IMPLICIT NONE
         INTEGER(INT8), INTENT(IN) :: BYTE
         INTEGER(INT8) :: RES

         RES = IBSET(BYTE, 7)
         RES = IBCLR(RES, 6)

      END FUNCTION SET_CONTINUATION_BYTE

   END FUNCTION CHAR_UTF8

   ! Take a UTF-8 character as an argument and
   ! return the integer representing its Unicode code point.
   FUNCTION ICHAR_UTF8 (CHARA) RESULT(RES)
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: CHARA
      INTEGER(INT32) :: RES
      INTEGER(INT8) :: BYTE(4), SHIFT_3, SHIFT_4, SHIFT_5, SHIFT_7
      INTEGER(INT8) ::  MASK_2_BIT, MASK_3_BIT, MASK_4_BIT, MASK_5_BIT
      INTEGER(INT32) :: BUF

      CHARACTER(8) :: BINARY

      BINARY = '00111111'
      READ(BINARY, '(b8.8)') MASK_2_BIT

      BINARY = '00011111'
      READ(BINARY, '(b8.8)') MASK_3_BIT  ! for 2-byte character

      BINARY = '00001111'
      READ(BINARY, '(b8.8)') MASK_4_BIT  ! for 3-byte character

      BINARY = '00000111'
      READ(BINARY, '(b8.8)') MASK_5_BIT

      RES = 0

      IF (LEN(CHARA) > 4)  THEN
         RES = -1
         RETURN
      END IF

      BYTE(1) = INT(ICHAR(CHARA(1:1)),KIND(BYTE))
      IF (LEN(CHARA) >= 2) BYTE(2) = INT(ICHAR(CHARA(2:2)), KIND(BYTE))
      IF (LEN(CHARA) >= 3) BYTE(3) = INT(ICHAR(CHARA(3:3)), KIND(BYTE))
      IF (LEN(CHARA) >= 4) BYTE(4) = INT(ICHAR(CHARA(4:4)), KIND(BYTE))

      SHIFT_3 = ISHFT(BYTE(1), -3)
      SHIFT_4 = ISHFT(BYTE(1), -4)
      SHIFT_5 = ISHFT(BYTE(1), -5)
      SHIFT_7 = ISHFT(BYTE(1), -7)

      ! 1-byte character
      IF (SHIFT_7 == 0) THEN

         RES = BYTE(1)
         RETURN

      ! 4-byte character
      ELSE IF (SHIFT_3 == 30) THEN

         RES = IAND(BYTE(1), MASK_5_BIT)

         RES = ISHFT(RES, 6)
         BUF = IAND(BYTE(2), MASK_2_BIT)
         RES = IOR(RES, BUF)

         RES = ISHFT(RES, 6)
         BUF = IAND(BYTE(3), MASK_2_BIT)
         RES = IOR(RES, BUF)

         RES = ISHFT(RES, 6)
         BUF = IAND(BYTE(4), MASK_2_BIT)
         RES = IOR(RES, BUF)

      ! 3-byte character
      ELSE IF (SHIFT_4 == 14) THEN

         RES = IAND(BYTE(1), MASK_4_BIT)

         RES = ISHFT(RES, 6)
         BUF = IAND(BYTE(2), MASK_2_BIT)
         RES = IOR(RES, BUF)

         RES = ISHFT(RES, 6)
         BUF = IAND(BYTE(3), MASK_2_BIT)
         RES = IOR(RES, BUF)

      ! 2-byte character
      ELSE IF (SHIFT_5 == 6) THEN

         RES = IAND(BYTE(1), MASK_3_BIT)

         RES = ISHFT(RES, 6)
         BUF = IAND(BYTE(2), MASK_2_BIT)
         RES = IOR(RES, BUF)

      END IF

   END FUNCTION ICHAR_UTF8

   FUNCTION LEN_TRIM_UTF8(STR) RESULT(COUNT)
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: STR
      INTEGER :: I, INEXT, COUNT

      I = 1
      COUNT = 0
      DO WHILE(I <= LEN_TRIM(STR))
         INEXT = IDXUTF8(STR, I) + 1
         COUNT = COUNT + 1
         I = INEXT
      END DO

   END FUNCTION LEN_TRIM_UTF8


   FUNCTION LEN_UTF8(STR) RESULT(COUNT)
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: STR
      INTEGER :: I, INEXT, COUNT

      I = 1
      COUNT = 0
      DO WHILE(I <= LEN(STR))
         INEXT = IDXUTF8(STR, I) + 1
         COUNT = COUNT + 1
         I = INEXT
      END DO

   END FUNCTION LEN_UTF8


   PURE FUNCTION IS_FIRST_BYTE_OF_CHARACTER(CHARA) RESULT(RES)
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      IMPLICIT NONE
      CHARACTER(1), INTENT(IN) :: CHARA
      LOGICAL :: RES
      INTEGER(INT8) :: BYTE, SHIFT_6

      BYTE = INT(ICHAR(CHARA), KIND(BYTE))

      RES = .TRUE.

      SHIFT_6 = ISHFT(BYTE, -6)

      IF (SHIFT_6 == 2) RES = .FALSE.

   END FUNCTION IS_FIRST_BYTE_OF_CHARACTER


   SUBROUTINE IS_FIRST_BYTE_OF_CHARACTER_ARRAY (STR, ARRAY, LENGTH)
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      IMPLICIT NONE
      LOGICAL, ALLOCATABLE, INTENT(INOUT) :: ARRAY(:)
      INTEGER(INT32), INTENT(IN) :: LENGTH
      CHARACTER(LEN=LENGTH), INTENT(IN) :: STR
      INTEGER :: I

      IF (ALLOCATED(ARRAY)) DEALLOCATE(ARRAY)

      ALLOCATE(ARRAY(LENGTH), SOURCE=.FALSE.)

      DO CONCURRENT (I = 1:LENGTH)
         ARRAY(I) = IS_FIRST_BYTE_OF_CHARACTER(STR(I:I))
      END DO

   END SUBROUTINE


   FUNCTION COUNT_TOKEN(STR, TOKEN) RESULT(COUNT)
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: STR
      CHARACTER(1), INTENT(IN) :: TOKEN
      INTEGER :: COUNT, I, SIZ

      COUNT = 0
      SIZ = LEN(STR)
      DO I = 1, SIZ
         IF (STR(I:I) == TOKEN) COUNT = COUNT + 1
      END DO

   END FUNCTION COUNT_TOKEN



END MODULE ModLib_UTF8
