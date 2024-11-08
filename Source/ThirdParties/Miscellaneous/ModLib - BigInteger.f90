!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
MODULE ModLib_BigInteger

!** PURPOSE OF THIS MODULE:
    ! contains derived type and routines that perform basic operations for
    ! multiple-precision integers.

!!##DESCRIPTION
!!
!!    The module named BIG_INTEGERS defines a new data type BIG_INTEGER.
!!    This data type represents nonnegative integers up to 10**n - 1, where n
!!    is the parameter (named constant) NR_OF_DECIMAL_DIGITS. This value may
!!    be changed, but the module must then be recompiled (it is not dynamic).
!!
!!    The following operations are implemented. "b" represents a big integer,
!!    "c" a character string, and "i" an ordinary integer.
!!
!!      big (i)
!!      big (c)
!!      int (b)
!!      int (c)
!!      char (b)
!!      char (i)
!!
!!      b = i
!!      b = c
!!      i = b
!!      i = c
!!      c = b
!!      c = i
!!
!!      b ? i, i ? b, and b ? b, where ? is
!!        +, -, *, /,
!!        <, <=, >, >=, ==, or /=
!!
!!      b ** i
!!
!!      modulo (b, i)  [result is integer]
!!      modulo (i, b)  [result is big_integer]
!!      modulo (b, b)  [result is big_integer]
!!
!!      huge (b)
!!      sqrt (b)
!!
!!      call print_big (b)
!!      call random_number (b, low, high)
!!
!!   Many operations of the form b ? i, where i < base, are implemented to
!!   be efficient as special cases.
!!
!!##AUTHOR
!!   Copyright (c) 1993-2002 Unicomp, Inc.
!!
!!   Developed at Unicomp, Inc.
!!
!!   Permission to use, copy, modify, and distribute this
!!   software is freely granted, provided that this notice
!!   is preserved.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!    program demo_M_big_integer
!!    use M_big_integer
!!    type(big_integer) :: a, b
!!    a = "1234567890"
!!    b = a ** 34
!!    call print_big(b)
!!    end program demo_M_big_integer
!!
!!   Expected output:
!!
!!    18044389448552283571495419217299928002884571753268277623634094010253!!    66981452225691084844631888351415012575687866144963748904906312371105!!    80592001682129147574651845715171456148359301092015447205623057495772!!    659564027213301182232076238

   IMPLICIT NONE

   PUBLIC :: BIG
   PUBLIC :: INT
   PUBLIC :: CHAR
   PUBLIC :: PRINT_BIG
   PUBLIC :: ASSIGNMENT (=)
   PUBLIC :: OPERATOR (+)
   PUBLIC :: OPERATOR (-)
   PUBLIC :: OPERATOR (*)
   PUBLIC :: OPERATOR (/)
   PUBLIC :: OPERATOR (**)
   PUBLIC :: MODULO
   PUBLIC :: HUGE
   PUBLIC :: SQRT
   PUBLIC :: RANDOM_NUMBER
   PUBLIC :: OPERATOR (==)
   PUBLIC :: OPERATOR (/=)
   PUBLIC :: OPERATOR (<=)
   PUBLIC :: OPERATOR (<)
   PUBLIC :: OPERATOR (>=)
   PUBLIC :: OPERATOR (>)
   PUBLIC :: ToDecString

   PRIVATE :: BIG_GETS_INT, &
              BIG_INT, &
              BIG_GETS_CHAR, &
              BIG_CHAR, &
              INT_GETS_BIG, &
              INT_BIG, &
              INT_GETS_CHAR, &
              INT_CHAR, &
              CHAR_GETS_BIG, &
              CHAR_BIG, &
              CHAR_GETS_INT, &
              CHAR_INT

   PRIVATE :: BIG_PLUS_INT, &
              INT_PLUS_BIG, &
              BIG_PLUS_BIG, &
              BIG_MINUS_INT, &
              INT_MINUS_BIG, &
              BIG_MINUS_BIG, &
              BIG_TIMES_INT, &
              INT_TIMES_BIG, &
              BIG_TIMES_BIG, &
              BIG_DIV_INT, &
              INT_DIV_BIG, &
              BIG_DIV_BIG, &
              BIG_POWER_INT, &
              MODULO_BIG_INT, &
              MODULO_INT_BIG, &
              MODULO_BIG_BIG

   PRIVATE :: BIG_EQ_INT, &
              INT_EQ_BIG, &
              BIG_EQ_BIG, &
              BIG_NE_INT, &
              INT_NE_BIG, &
              BIG_NE_BIG, &
              BIG_LE_INT, &
              INT_LE_BIG, &
              BIG_LE_BIG, &
              BIG_GE_INT, &
              INT_GE_BIG, &
              BIG_GE_BIG, &
              BIG_LT_INT, &
              INT_LT_BIG, &
              BIG_LT_BIG, &
              BIG_GT_INT, &
              INT_GT_BIG, &
              BIG_GT_BIG

   PRIVATE :: HUGE_BIG, &
              BIG_BASE_TO_POWER, &
              PRINT_BIG_BASE, &
              SQRT_BIG, &
              MSD, &
              RANDOM_NUMBER_BIG

   INTRINSIC :: CHAR
   INTRINSIC :: INT
   INTRINSIC :: MODULO
   INTRINSIC :: HUGE
   INTRINSIC :: SQRT
   INTRINSIC :: RANDOM_NUMBER
   INTRINSIC :: RADIX
   INTRINSIC :: DIGITS

   ! This indicates the maximum number of decimal digits
   ! that a big integer may contain.

!   INTEGER, PARAMETER, PUBLIC :: NR_OF_DECIMAL_DIGITS = 500
   INTEGER, PARAMETER, PUBLIC :: NR_OF_DECIMAL_DIGITS = 600

   ! If the radix (returned by "radix(0)" of the integers on
   ! your system is not 2 change the following constant to
   ! the logarithm in the base 10 of the radix: log10(radix)

   REAL, PARAMETER, PRIVATE :: LOG_BASE_10_OF_RADIX = 0.30103

   INTEGER, PARAMETER, PRIVATE :: &
         D = DIGITS (0) / 2, &
         R = RADIX (0), &
         BASE = R ** D, &
         NR_OF_DIGITS = NR_OF_DECIMAL_DIGITS / (LOG_BASE_10_OF_RADIX * D) + 1

   ! The base of the number system is r ** d,
   ! so that each "digit" is 0 to r**d - 1

   TYPE, PUBLIC :: BIG_INTEGER
      PRIVATE
      INTEGER, DIMENSION (0 : NR_OF_DIGITS) :: DIGIT
   END TYPE BIG_INTEGER

   INTERFACE BIG
      MODULE PROCEDURE BIG_CHAR, &
                       BIG_INT
   END INTERFACE

   INTERFACE INT
      MODULE PROCEDURE INT_CHAR, &
                       INT_BIG
   END INTERFACE

   INTERFACE CHAR
      MODULE PROCEDURE CHAR_BIG, &
                       CHAR_INT
   END INTERFACE

   INTERFACE ASSIGNMENT (=)
      MODULE PROCEDURE BIG_GETS_INT, &
                       BIG_GETS_CHAR, &
                       INT_GETS_BIG, &
                       INT_GETS_CHAR, &
                       CHAR_GETS_BIG, &
                       CHAR_GETS_INT
   END INTERFACE

   INTERFACE OPERATOR (+)
      MODULE PROCEDURE BIG_PLUS_INT, &
                       BIG_PLUS_BIG
   END INTERFACE

   INTERFACE OPERATOR (-)
      MODULE PROCEDURE BIG_MINUS_INT, &
                       INT_MINUS_BIG, &
                       BIG_MINUS_BIG
   END INTERFACE

   INTERFACE OPERATOR (*)
      MODULE PROCEDURE BIG_TIMES_INT, &
                       INT_TIMES_BIG, &
                       BIG_TIMES_BIG
   END INTERFACE

   INTERFACE OPERATOR (/)
      MODULE PROCEDURE BIG_DIV_INT, &
                       INT_DIV_BIG, &
                       BIG_DIV_BIG
   END INTERFACE

   INTERFACE OPERATOR (**)
      MODULE PROCEDURE BIG_POWER_INT
   END INTERFACE

   INTERFACE MODULO
      MODULE PROCEDURE MODULO_BIG_INT, &
                       MODULO_INT_BIG, &
                       MODULO_BIG_BIG
   END INTERFACE

   INTERFACE OPERATOR (==)
      MODULE PROCEDURE BIG_EQ_INT, &
                       INT_EQ_BIG, &
                       BIG_EQ_BIG
   END INTERFACE

   INTERFACE OPERATOR (/=)
      MODULE PROCEDURE BIG_NE_INT, &
                       INT_NE_BIG, &
                       BIG_NE_BIG
   END INTERFACE

   INTERFACE OPERATOR (<=)
      MODULE PROCEDURE BIG_LE_INT, &
                       INT_LE_BIG, &
                       BIG_LE_BIG
   END INTERFACE

   INTERFACE OPERATOR (>=)
      MODULE PROCEDURE BIG_GE_INT, &
                       INT_GE_BIG, &
                       BIG_GE_BIG
   END INTERFACE

   INTERFACE OPERATOR (<)
      MODULE PROCEDURE BIG_LT_INT, &
                       INT_LT_BIG, &
                       BIG_LT_BIG
   END INTERFACE

   INTERFACE OPERATOR (>)
      MODULE PROCEDURE BIG_GT_INT, &
                       INT_GT_BIG, &
                       BIG_GT_BIG
   END INTERFACE

   INTERFACE HUGE
      MODULE PROCEDURE HUGE_BIG
   END INTERFACE

   INTERFACE SQRT
      MODULE PROCEDURE SQRT_BIG
   END INTERFACE

   INTERFACE RANDOM_NUMBER
      MODULE PROCEDURE RANDOM_NUMBER_BIG
   END INTERFACE

   INTERFACE ToDecString
      MODULE PROCEDURE DecString_From_BigInteger
   END INTERFACE
   
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_CHAR (C) RESULT (B)

   CHARACTER (LEN=*), INTENT (IN) :: C
   TYPE (BIG_INTEGER) :: B
   INTEGER :: TEMP_DIGIT, N

   IF (LEN (C) > NR_OF_DECIMAL_DIGITS) THEN
      B = HUGE (B)
      RETURN
   END IF
   B % DIGIT = 0
   DO N = 1, LEN (C)
      TEMP_DIGIT = INDEX ("0123456789", C (N:N)) - 1
      IF (TEMP_DIGIT < 0) THEN
         B = HUGE (B)
      END IF
      B = B * 10 + TEMP_DIGIT
   END DO

END FUNCTION BIG_CHAR
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE SUBROUTINE BIG_GETS_CHAR (B, C)

   TYPE (BIG_INTEGER), INTENT (OUT) :: B
   CHARACTER (LEN=*), INTENT (IN) :: C

   B = BIG_CHAR (TRIM (C))

END SUBROUTINE BIG_GETS_CHAR
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_INT (I) RESULT (B)

   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: B
   INTEGER :: TEMP_I, N

   IF (I < 0) THEN
      B = HUGE (B)
   END IF

   B % DIGIT = 0
   TEMP_I = I
   DO N = 0, NR_OF_DIGITS - 1
      IF (TEMP_I == 0) THEN
         RETURN
      END IF
      B % DIGIT (N) = MODULO (TEMP_I, BASE)
      TEMP_I = TEMP_I / BASE
   END DO

   IF (TEMP_I /= 0) THEN
      B = HUGE (B)
   END IF

END FUNCTION BIG_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE SUBROUTINE BIG_GETS_INT (B, I)

   TYPE (BIG_INTEGER), INTENT (OUT) :: B
   INTEGER, INTENT (IN) :: I

   B = BIG (I)

END SUBROUTINE BIG_GETS_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_CHAR (C) RESULT (I)

   CHARACTER (LEN=*), INTENT (IN) :: C
   INTEGER :: I

   I = INT (BIG (TRIM (C)))

END FUNCTION INT_CHAR
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE SUBROUTINE INT_GETS_CHAR (I, C)

   INTEGER, INTENT (OUT) :: I
   CHARACTER (LEN=*), INTENT (IN) :: C

   I = INT_CHAR (TRIM (C))

END SUBROUTINE INT_GETS_CHAR
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_BIG (B) RESULT (I)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER :: I

   IF (MSD (B) > 1) THEN
      I = HUGE (I)
   ELSE
      I = BASE * B % DIGIT (1) + B % DIGIT (0)
   END IF

END FUNCTION INT_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE SUBROUTINE INT_GETS_BIG (I, B)

   INTEGER, INTENT (OUT) :: I
   TYPE (BIG_INTEGER), INTENT (IN) :: B

   I = INT (B)

END SUBROUTINE INT_GETS_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION CHAR_BIG (B) RESULT (C)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   CHARACTER (LEN=NR_OF_DECIMAL_DIGITS+9) :: C
   TYPE (BIG_INTEGER) :: TEMP_BIG
   INTEGER :: N, REMAINDER
   CHARACTER (LEN = *), PARAMETER :: DIGIT_CHARS = "0123456789"

   TEMP_BIG = B
   C = REPEAT (" ", LEN(C)-1) // "0"
   DO N = LEN (C), 1, -1
      IF (TEMP_BIG == 0) THEN
         EXIT
      END IF
      REMAINDER = MODULO (TEMP_BIG, 10) + 1
      TEMP_BIG = TEMP_BIG / 10
      C (N:N) = DIGIT_CHARS (REMAINDER:REMAINDER)
   END DO

   C = ADJUSTL (C)

END FUNCTION CHAR_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE SUBROUTINE CHAR_GETS_BIG (C, B)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   CHARACTER (LEN=*), INTENT (OUT) :: C

   C = CHAR (B)

END SUBROUTINE CHAR_GETS_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION CHAR_INT (I) RESULT (C)

   INTEGER, INTENT (IN) :: I
   CHARACTER (LEN=NR_OF_DECIMAL_DIGITS+9) :: C

   C = BIG (I)

END FUNCTION CHAR_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE SUBROUTINE CHAR_GETS_INT (C, I)

   INTEGER, INTENT (IN) :: I
   CHARACTER (LEN=*), INTENT (OUT) :: C

   C = BIG (I)

END SUBROUTINE CHAR_GETS_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION MSD (X) RESULT (MSD_RESULT)

! Find most significant digit of x

   TYPE (BIG_INTEGER), INTENT (IN) :: X
   INTEGER :: MSD_RESULT
   INTEGER :: N

   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= 0) THEN
         MSD_RESULT = N
         RETURN
      END IF
   END DO

   MSD_RESULT = -1

END FUNCTION MSD
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_PLUS_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: BI
   INTEGER :: N, SUMM, CARRY

   IF (I < BASE) THEN
      CARRY = I
      DO N = 0, NR_OF_DIGITS - 1
         SUMM = B % DIGIT (N) + CARRY
         BI % DIGIT (N) = MODULO (SUMM, BASE)
         CARRY = SUMM / BASE
         IF (CARRY == 0) THEN
            BI % DIGIT (N+1:) = B % DIGIT (N+1:)
            RETURN
         END IF
      END DO
      IF (N==NR_OF_DIGITS) THEN
         BI = HUGE (BI)
      END IF
   ELSE
      BI = B + BIG (I)
   END IF

END FUNCTION BIG_PLUS_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_PLUS_BIG (I, B) RESULT (BI)

   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: BI

   BI = B + I

END FUNCTION INT_PLUS_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_PLUS_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   TYPE (BIG_INTEGER) :: BB
   INTEGER :: CARRY, TEMP_DIGIT, N, M

   CARRY = 0
   M = MAX (MSD (X), MSD (Y))
   DO N = 0, M
      TEMP_DIGIT = &
         X % DIGIT (N) + Y % DIGIT (N) + CARRY
      BB % DIGIT (N) = MODULO (TEMP_DIGIT, BASE)
      CARRY = TEMP_DIGIT / BASE
   END DO

   BB % DIGIT (M+1) = CARRY
   BB % DIGIT (M+2:NR_OF_DIGITS) = 0
   IF (BB % DIGIT (NR_OF_DIGITS) /= 0) THEN
      BB = HUGE (BB)
   END IF

END FUNCTION BIG_PLUS_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_MINUS_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: BI
   INTEGER :: N, BORROW, DIFF, MSDB

   BI % DIGIT = 0
   MSDB = MSD (B)
   IF (MSDB<1 .AND. B % DIGIT (0) < I) THEN
      RETURN
   END IF

   IF (I < BASE) THEN
      BORROW = I
      DO N = 0, NR_OF_DIGITS - 1
         DIFF = B % DIGIT (N) - BORROW
         BI % DIGIT (N) = MODULO (DIFF, BASE)
         BORROW = (BASE - DIFF) / BASE
         IF (BORROW == 0) THEN
            BI % DIGIT (N+1:MSDB) = B % DIGIT (N+1:MSDB)
            RETURN
         END IF
      END DO
   ELSE
      BI = B - BIG (I)
   END IF

END FUNCTION BIG_MINUS_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_MINUS_BIG (I, B) RESULT (IB)

   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: IB

   IB = BIG (I) - B

END FUNCTION INT_MINUS_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_MINUS_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   TYPE (BIG_INTEGER) :: BB
   TYPE (BIG_INTEGER) :: TEMP_BIG
   INTEGER :: N

   TEMP_BIG = X
   DO N = 0, NR_OF_DIGITS - 1
      BB % DIGIT (N) = TEMP_BIG % DIGIT (N) - Y % DIGIT (N)
      IF (BB % DIGIT (N) < 0) THEN
         BB % DIGIT (N) = BB % DIGIT (N) + BASE
         TEMP_BIG % DIGIT (N + 1) = TEMP_BIG % DIGIT (N + 1) - 1
      END IF
   END DO

   IF (TEMP_BIG % DIGIT (NR_OF_DIGITS) < 0) THEN
      BB % DIGIT = 0
   ELSE
      BB % DIGIT (NR_OF_DIGITS) = 0
   END IF

END FUNCTION BIG_MINUS_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_TIMES_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: BI
   INTEGER :: IB, PROD, CARRY

   IF (I < BASE) THEN
      BI % DIGIT = 0
      CARRY = 0
      DO IB = 0, MSD (B)
         PROD = B % DIGIT (IB) * I + CARRY
         BI % DIGIT (IB) = MODULO (PROD, BASE)
         CARRY = PROD / BASE
      END DO
      IF (IB==NR_OF_DIGITS .AND. CARRY /= 0) THEN
         BI = HUGE (BI)
      ELSE
         BI % DIGIT (IB) = CARRY
      END IF
   ELSE
      BI = B * BIG (I)
   END IF

END FUNCTION BIG_TIMES_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_TIMES_BIG (I, B) RESULT (BI)

   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: BI

   BI = B * I

END FUNCTION INT_TIMES_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_TIMES_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   TYPE (BIG_INTEGER) :: BB

   INTEGER :: IX, IY, IB, CARRY, PROD

   BB % DIGIT = 0

   DO IX = 0, MSD (X)
      CARRY = 0
      IB = IX
      DO IY = 0, MSD (Y)
         PROD = X % DIGIT (IX) * Y % DIGIT (IY) + BB % DIGIT (IB) + CARRY
         CARRY = PROD / BASE
         BB % DIGIT (IB) = MODULO (PROD, BASE)
         IF (IB == NR_OF_DIGITS) THEN
            BB = HUGE (BB)
            RETURN
         END IF
         IB = IB + 1
      END DO
      BB % DIGIT (IB) = BB % DIGIT (IB) + CARRY
   END DO

END FUNCTION BIG_TIMES_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_BASE_TO_POWER (N)  RESULT (B)

   INTEGER, INTENT (IN) :: N
   TYPE (BIG_INTEGER) :: B

   IF (N < 0) THEN
     B = 0
   ELSE IF (N >= NR_OF_DIGITS) THEN
      B = HUGE (B)
   ELSE
      B % DIGIT = 0
      B % DIGIT (N) = 1
   END IF

END FUNCTION BIG_BASE_TO_POWER
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_DIV_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: BI
   INTEGER :: N, TEMP_INT, REMAINDER

   IF (I == 0) THEN
      BI = HUGE (BI)
   ELSE IF (I < BASE) THEN
      BI % DIGIT = 0
      REMAINDER = 0
      DO N = MSD(B), 0, -1
         TEMP_INT = BASE * REMAINDER + B % DIGIT (N)
         BI % DIGIT (N) = TEMP_INT / I
         REMAINDER = MODULO (TEMP_INT, I)
      END DO
   ELSE
      BI = B / BIG (I)
   END IF

END FUNCTION BIG_DIV_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_DIV_BIG (I, B) RESULT (IB)

   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: IB

   IB = BIG (I) / B

END FUNCTION INT_DIV_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_DIV_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   TYPE (BIG_INTEGER) :: BB

   TYPE (BIG_INTEGER) :: TX, TY

   INTEGER :: MSDX, MSDY, IX, IY
   INTEGER :: V1, V2, U0, U1, U2
   INTEGER :: DD, BI, CAR, BAR, PRD

   IF (Y == 0) THEN
      BB = HUGE (BB)
      RETURN
   END IF

   MSDX = MSD(X)
   MSDY = MSD(Y)

   IF (MSDY == 0) THEN
      BB = X / Y % DIGIT (0)
      RETURN
   END IF

   BB % DIGIT = 0

   IF (MSDY < MSDY) THEN
      RETURN
   END IF

   TX = X
   TY = Y

   CAR = 0
   BAR = 0
   PRD = 0
   DD = BASE / (TY % DIGIT (MSDY) + 1)
   IF (DD /= 1) THEN
      DO IX = 0, MSDX
         TX % DIGIT (IX) = TX % DIGIT (IX) * DD + CAR
         CAR = TX % DIGIT (IX) / BASE
         TX % DIGIT (IX) = TX % DIGIT (IX) - BASE * CAR
      END DO
      TX % DIGIT (MSDX+1) = CAR
      CAR = 0
      DO IY = 0, MSDY
         TY % DIGIT (IY) = TY % DIGIT (IY) * DD + CAR
         CAR = TY % DIGIT (IY) / BASE
         TY % DIGIT (IY) = TY % DIGIT (IY) - BASE * CAR
      END DO
   END IF

   MSDX = MSDX + 1

   V1 = TY % DIGIT (MSDY)
   V2 = TY % DIGIT (MSDY-1)
   BB % DIGIT = 0

   DO MSDX = MSDX, MSDY + 1, -1

      U0 = TX % DIGIT (MSDX)
      U1 = TX % DIGIT (MSDX-1)
      U2 = TX % DIGIT (MSDX-2)

      IF (U0 == V1) THEN
         BI = BASE - 1
      ELSE
         BI = (U0*BASE + U1) / V1
      END IF

      DO
         IF (V2*BI <= (U0*BASE + U1 - BI*V1) * BASE + U2) THEN
            EXIT
         END IF
         BI = BI - 1
      END DO

      IF (BI > 0) THEN
         CAR = 0
         BAR = 0
         IX = MSDX - MSDY - 1
         DO IY = 0, MSDY
            PRD = BI * TY % DIGIT (IY) + CAR
            CAR = PRD / BASE
            PRD = PRD - BASE * CAR
            TX % DIGIT (IX) = TX % DIGIT (IX) - (PRD + BAR)
            IF (TX % DIGIT (IX) < 0) THEN
               BAR = 1
               TX % DIGIT (IX) = TX % DIGIT (IX) + BASE
            ELSE
               BAR = 0
            END IF
            IX = IX + 1
         END DO
         IF (TX % DIGIT (MSDX) < CAR + BAR) THEN
            CAR = 0
            BI = BI -1
            IX = MSDX - MSDY - 1
            DO IY = 0, MSDY
               TX % DIGIT (IX) = TX % DIGIT (IX) + TY % DIGIT (IY) + CAR
               IF (TX % DIGIT (IX) > BASE) THEN
                  CAR = 1
                  TX % DIGIT (IX) = TX % DIGIT (IX) - BASE
               ELSE
                  CAR = 0
               END IF
               IX = IX + 1
            END DO
         END IF
      END IF
      TX % DIGIT (MSDX) = 0
      BB % DIGIT (1:NR_OF_DIGITS) = BB % DIGIT (0:NR_OF_DIGITS-1)
      BB % DIGIT (0) = BI
   END DO

END FUNCTION BIG_DIV_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION MODULO_BIG_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   INTEGER :: BI
   INTEGER :: N

   IF (I == 0) THEN
      BI = HUGE (BI)
   ELSE IF (I < BASE) THEN
      BI = 0
      DO N = MSD(B), 0, -1
         BI = MODULO (BASE * BI + B % DIGIT (N), I)
      END DO
   ELSE
      BI = MODULO (B, BIG (I))
   END IF

END FUNCTION MODULO_BIG_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION MODULO_INT_BIG (II, B) RESULT (IB)

   INTEGER, INTENT (IN) :: II
   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: IB

   IB = MODULO (BIG (II), B)

END FUNCTION MODULO_INT_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION MODULO_BIG_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   TYPE (BIG_INTEGER) :: BB

   BB = X - X / Y * Y

END FUNCTION MODULO_BIG_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_EQ_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = INT (B) == I

END FUNCTION BIG_EQ_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_EQ_BIG (I, B) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = INT (B) == I

END FUNCTION INT_EQ_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_EQ_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: BB

   BB = ALL (X % DIGIT == Y % DIGIT)

END FUNCTION BIG_EQ_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_NE_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = INT (B) /= I

END FUNCTION BIG_NE_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_NE_BIG (I, B) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = INT (B) /= I

END FUNCTION INT_NE_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_NE_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: BB

   BB = ANY (X % DIGIT /= Y % DIGIT)

END FUNCTION BIG_NE_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_LE_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = INT (B) <= I

END FUNCTION BIG_LE_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_LE_BIG (I, B) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = I <= INT (B)

END FUNCTION INT_LE_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_LE_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: BB
   INTEGER :: N

   BB = .TRUE.
   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= Y % DIGIT (N)) THEN
         BB = (X % DIGIT (N) < Y % DIGIT (N))
         EXIT
      END IF
   END DO

END FUNCTION BIG_LE_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_GT_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = INT (B) > I

END FUNCTION BIG_GT_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_GT_BIG (I, B) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = I > INT (B)

END FUNCTION INT_GT_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_GT_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: BB
   INTEGER :: N

   BB = .TRUE.
   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= Y % DIGIT (N)) THEN
         BB = (X % DIGIT (N) < Y % DIGIT (N))
         EXIT
      END IF
   END DO

   BB = .NOT. BB

END FUNCTION BIG_GT_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_LT_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = INT (B) < I

END FUNCTION BIG_LT_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_LT_BIG (I, B) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = I < INT (B)

END FUNCTION INT_LT_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_LT_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: BB
   INTEGER :: N

   BB = .FALSE.
   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= Y % DIGIT (N)) THEN
         BB = (X % DIGIT (N) < Y % DIGIT (N))
         EXIT
      END IF
   END DO

END FUNCTION BIG_LT_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_GE_INT (B, I) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = INT (B) >= I

END FUNCTION BIG_GE_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION INT_GE_BIG (I, B) RESULT (BI)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: BI

   BI = I >= INT (B)

END FUNCTION INT_GE_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION BIG_GE_BIG (X, Y) RESULT (BB)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: BB
   INTEGER :: N

   BB = .FALSE.
   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= Y % DIGIT (N)) THEN
         BB = (X % DIGIT (N) < Y % DIGIT (N))
         EXIT
      END IF
   END DO

   BB = .NOT. BB

END FUNCTION BIG_GE_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION HUGE_BIG (B) RESULT (HB)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: HB

   HB % DIGIT (0) = B % DIGIT (0) ! to avoid diagnostic
   HB % DIGIT = BASE - 1
   HB % DIGIT (NR_OF_DIGITS) = 0

END FUNCTION HUGE_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
PURE FUNCTION SQRT_BIG (B) RESULT (SB)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: SB
   TYPE (BIG_INTEGER) :: OLD_SQRT_BIG, NEW_SQRT_BIG
   INTEGER :: I, N

   N = -1
   DO I = NR_OF_DIGITS, 0, -1
      IF (B % DIGIT (I) /= 0) THEN
         N = I
         EXIT
      END IF
   END DO

   IF (N == -1) THEN
      SB = 0
   ELSE IF (N == 0) THEN
      SB = INT (SQRT (REAL (B % DIGIT (0))))
   ELSE
      OLD_SQRT_BIG = 0
      IF (MODULO (N, 2) == 0) THEN
         OLD_SQRT_BIG % DIGIT (N / 2) = INT (SQRT (REAL (B % DIGIT (N))))
      ELSE
         OLD_SQRT_BIG % DIGIT ((N - 1) / 2) =  &
               INT (SQRT (REAL (BASE * B % DIGIT (N) + B % DIGIT (N-1))))
      END IF

      DO
         NEW_SQRT_BIG = (OLD_SQRT_BIG + B / OLD_SQRT_BIG) / 2
         IF (NEW_SQRT_BIG == OLD_SQRT_BIG .OR.  &
             NEW_SQRT_BIG == OLD_SQRT_BIG + 1 .OR.  &
             NEW_SQRT_BIG == 0) THEN
            EXIT
         ELSE
            OLD_SQRT_BIG = NEW_SQRT_BIG
         END IF
      END DO
      SB = OLD_SQRT_BIG
   END IF

END FUNCTION SQRT_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
RECURSIVE FUNCTION BIG_POWER_INT (B, I)  &
      RESULT (BIG_POWER_INT_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: BIG_POWER_INT_RESULT
   TYPE (BIG_INTEGER) :: TEMP_BIG

   IF (I <= 0) THEN
      BIG_POWER_INT_RESULT = 1
   ELSE
      TEMP_BIG = BIG_POWER_INT (B, I / 2)
      IF (MODULO (I, 2) == 0) THEN
         BIG_POWER_INT_RESULT = TEMP_BIG * TEMP_BIG
      ELSE
         BIG_POWER_INT_RESULT = TEMP_BIG * TEMP_BIG * B
      END IF
   END IF

END FUNCTION BIG_POWER_INT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE PRINT_BIG (B)

   TYPE (BIG_INTEGER), INTENT (IN) :: B

   WRITE (UNIT = *, FMT = "(a)", ADVANCE = "no") TRIM (CHAR (B))

END SUBROUTINE PRINT_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE PRINT_BIG_BASE (B)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER :: N

   PRINT *, "base: ", BASE
   DO N = NR_OF_DIGITS, 1, -1
      IF (B % DIGIT (N) /= 0) THEN
         EXIT
      END IF
   END DO
   PRINT "(10i9)", B % DIGIT (N:0:-1)

END SUBROUTINE PRINT_BIG_BASE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE RANDOM_NUMBER_BIG (R, LOW, HIGH)

!  Generate by linear congruence x' = ax + c mod m
!  where m is huge (b) + 1 = base ** nr_of_digits

   TYPE (BIG_INTEGER), INTENT (OUT) :: R
   TYPE (BIG_INTEGER), INTENT (IN) :: LOW, HIGH
   INTEGER :: N, I, CARRY, PROD, SUMM
   TYPE (BIG_INTEGER), SAVE :: X = BIG_INTEGER ( (/ (1, I=0,NR_OF_DIGITS-1), 0 /) )
   TYPE (BIG_INTEGER), PARAMETER :: H = BIG_INTEGER ( (/ (BASE-1, I=0,NR_OF_DIGITS-1), 0 /) )
   INTEGER, PARAMETER :: A = 16907, C = 8191

!  Multiply by a
   CARRY = 0
   DO N = 0, NR_OF_DIGITS - 1
      PROD = X % DIGIT (N) * A + CARRY
      X % DIGIT (N) = MODULO (PROD, BASE)
      CARRY = PROD / BASE
   END DO

!  Add c
   CARRY = C
   DO N = 0, NR_OF_DIGITS - 1
      SUMM = X % DIGIT (N) + CARRY
      X % DIGIT (N) = MODULO (SUMM, BASE)
      CARRY = SUMM / BASE
   END DO

   R = X / (H / (HIGH -LOW + 1)) + LOW

END SUBROUTINE RANDOM_NUMBER_BIG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
FUNCTION DecString_From_BigInteger(BigNum) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a BigInt number to a decimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE (BIG_INTEGER), INTENT(IN)  :: BigNum
    CHARACTER(LEN=:), ALLOCATABLE   :: Str
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! NA

!** FLOW
    
    Str = TRIM(CHAR(BigNum))

    RETURN

END FUNCTION DecString_From_BigInteger
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END MODULE ModLib_BigInteger
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
