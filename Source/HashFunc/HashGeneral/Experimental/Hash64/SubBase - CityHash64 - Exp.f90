
SUBMODULE (ModBase_ExperimentalHash64) SubBase_CityHash64_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the City hash algorithms
!   for 64-bit-integer output by Google Inc. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE ModBase_SIntUtil,   ONLY: LongReverseBytes    => ReverseBytes
    USE ModBase_ExperimentalHash32

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)               IAND(ToLong(X), Z'00000000000000FF')
#define MaskI32(X)              IAND(ToLong(X), Z'00000000FFFFFFFF')
#define ShiftMix(V)             IEOR(V, SHIFTR(V, 47))
#define HashLen16_3(U,V,M)      ShiftMix(IEOR(V, ShiftMix(IEOR(U, V)*M))*M)*M
#define HashLen16_2(U,V)        HashLen16_3(U, V, K_MUL)
#define Mul(L)                  K2 + SHIFTL(ToLong(L), 1)

!** MODULE PARAMETERS:
    tLong,  PARAMETER   :: K0    = ToLong(Z'C3A5C85C97CB3127')
    tLong,  PARAMETER   :: K1    = ToLong(Z'B492B66FBE98F273')
    tLong,  PARAMETER   :: K2    = ToLong(Z'9AE16A3B2F90404F')
    tLong,  PARAMETER   :: K_MUL = ToLong(Z'9DDFEA08EB382D69')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION City_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the CityHash hash algorithm by Google Inc.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-10)
    tLong,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tLong,    OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
    tLong                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tLong               :: Seed1

!** FLOW

    ! initialize
    SET_OPTION(Seed1, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    SELECT CASE (Algo)
    CASE (1)
        HashCode = City_Hash64(InpPtr, Seed1, Pack_I64_A1, Pack_I32_A1, Seed)
    CASE (2)
        HashCode = City_Hash64(InpPtr, Seed1, Pack_I64_A2, Pack_I32_A2, Seed)
    CASE (3)
        HashCode = City_Hash64(InpPtr, Seed1, Pack_I64_A3, Pack_I32_A3, Seed)
    CASE (4)
        HashCode = City_Hash64(InpPtr, Seed1, Pack_I64_A4, Pack_I32_A4, Seed)
    CASE (5)
        HashCode = City_Hash64(InpPtr, Seed1, Pack_I64_A5, Pack_I32_A5, Seed)
    CASE (6)
        HashCode = City_Hash64(InpPtr, Seed1, Pack_I64_A6, Pack_I32_A6, Seed)
    CASE (7)
        HashCode = City_Hash64(InpPtr, Seed1, Pack_I64_A7, Pack_I32_A7, Seed)
    CASE (8)
        HashCode = City_Hash64_08(InpPtr, Seed1, Pack_I32_A6, Seed)
    CASE (9)
        HashCode = City_Hash64_09(InpPtr, Seed1, Seed)
    CASE (10)
        HashCode = City_Hash64_10(InpPtr, Seed1, Seed)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION City_Hash64_Exp

!******************************************************************************

FUNCTION City_Hash64(Input, Seed1, PackLong, PackInteger, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the CityHash64 hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN) :: Input(0:)    !! input bytes
    tLong,           INTENT(IN) :: Seed1        !! seed
    PROCEDURE(Pack_I64)         :: PackLong     !! procedure to convert a byte array to 64-bit integer
    PROCEDURE(Pack_I32)         :: PackInteger  !! procedure to convert a byte array to 32-bit integer
    tLong, OPTIONAL, INTENT(IN) :: Seed2        !! optional (additional) seed
    tLong                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset
    tLong           :: X, Y, Z, Tmp
    tLong           :: VFirst, VSecond, WFirst, WSecond
    tLong           :: A, B, C, W1, X1, Y1, Z1
    tLong           :: A1, B1, C1, W2, X2, Y2, Z2
    tLong           :: A2, B2, C2, W3, X3, Y3, Z3
    tLong           :: A3, B3, C3, W4, X4, Y4, Z4

!** FLOW

#define GetU8(In, Off)      MaskI8(In(Off))
#define Pack_U32(In, Off)   MaskI32(PackInteger(In, Off))

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 16) THEN
        ! CityHash_Len0To16
        IF (Length >= 8) THEN
            BLOCK
                tLong   :: M, A, C, D
                tLong   :: First8Bytes, Last8Bytes
                First8Bytes = PackLong(Input, 0)
                Last8Bytes  = PackLong(Input, Length - 8)
                ! Hash 8 To 16 Bytes
                M = Mul(Length)
                A = First8Bytes + K2
                C = RotateRight(Last8Bytes, 37)*M + A
                D = (RotateRight(A, 25) + Last8Bytes)*M
                HashCode = HashLen16_3(C, D, M)
            END BLOCK
        ELSEIF (Length >= 4) THEN
            BLOCK
                tLong   :: M, First4Bytes, Last4Bytes
                First4Bytes = Pack_U32(Input, 0)
                Last4Bytes  = Pack_U32(Input, Length - 4)
                ! Hash 4 To 7 Bytes
                M = Mul(Length)
                HashCode = HashLen16_3(ToLong(Length) + SHIFTL(First4Bytes, 3), Last4Bytes, M)
            END BLOCK
        ELSEIF (Length > 0) THEN
            BLOCK
                tInteger    :: FirstByte, MidOrLastByte, LastByte
                tInteger    :: Y, Z
                FirstByte     = GetU8(Input, 0)
                MidOrLastByte = GetU8(Input, SHIFTA(Length, 1))
                LastByte      = GetU8(Input, Length - 1)
                ! Hash 1 To 3 Bytes
                Y = FirstByte + SHIFTL(MidOrLastByte, 8)
                Z = ToInteger(Length) + SHIFTL(LastByte, 2)
                HashCode = ShiftMix(IEOR(ToLong(Y)*K2, ToLong(Z)*K0))*K2
            END BLOCK
        ELSE
            HashCode = K2
        END IF

    ELSEIF (Length <= 32) THEN
        ! CityHash_Len17To32
        BLOCK
            tLong   :: M, A, B, C, D
            ! perform hashing
            M = Mul(Length)
            A = PackLong(Input, 0)*K1
            B = PackLong(Input, 8)
            C = PackLong(Input, Length - 8)*M
            D = PackLong(Input, Length - 16)*K2
            HashCode = HashLen16_3(RotateRight(A+B, 43) + RotateRight(C, 30) + D, \
                               A + RotateRight(B+K2, 18) + C, M)
        END BLOCK
    ELSEIF (Length <= 64) THEN
        ! CityHash_Len33To64
        BLOCK
            tLong   :: M, A, B, C, D, E, F, G, H
            tLong   :: U, V, W, X, Y, Z
            ! perform hashing
            M = Mul(Length)
            A = PackLong(Input, 0)*K2
            B = PackLong(Input, 8)
            C = PackLong(Input, Length - 24)
            D = PackLong(Input, Length - 32)
            E = PackLong(Input, 16)*K2
            F = PackLong(Input, 24)*9_kLong
            G = PackLong(Input, Length - 8)
            H = PackLong(Input, Length - 16)*M
            U = RotateRight(A+G, 43) + (RotateRight(B, 30) + C)*9_kLong
            V = IEOR(A+G, D) + F + 1_kLong
            w = LongReverseBytes((U + V)*M) + H
            X = RotateRight(E+F, 42) + C
            Y = (LongReverseBytes((V + w)*M) + G)*M
            Z = E + F + C
            A = LongReverseBytes((X + Z)*M + Y) + B
            B = ShiftMix((Z + A)*M + D + H)*M
            HashCode = B + X
        END BLOCK
    ELSE
        X = PackLong(Input, Length - 40)
        Y = PackLong(Input, Length - 16) + PackLong(Input, Length - 56)
        Z = HashLen16_2(PackLong(Input, Length - 48) + Length, \
                        PackLong(Input, Length - 24))

        ! WeakHashLen32WithSeeds
        A3 = Length
        B3 = Z
        W4 = PackLong(Input, Length - 64)
        X4 = PackLong(Input, Length - 64 + 8)
        Y4 = PackLong(Input, Length - 64 + 16)
        Z4 = PackLong(Input, Length - 64 + 24)
        A3 = A3 + W4
        B3 = RotateRight(B3 + A3 + Z4, 21)
        C3 = A3
        A3 = A3 + X4 + Y4
        B3 = B3 + RotateRight(A3, 44)
        VFirst = A3 + Z4
        VSecond = B3 + C3

        ! WeakHashLen32WithSeeds
        A2 = Y + K1
        B2 = X
        W3 = PackLong(Input, Length - 32)
        X3 = PackLong(Input, Length - 32 + 8)
        Y3 = PackLong(Input, Length - 32 + 16)
        Z3 = PackLong(Input, Length - 32 + 24)
        A2 = A2 + W3
        B2 = RotateRight(B2 + A2 + Z3, 21)
        C2 = A2
        A2 = A2 + X3 + Y3
        B2 = B2 + RotateRight(A2, 44)
        WFirst = A2 + Z3
        WSecond = B2 + C2

        X = X*K1 + PackLong(Input, 0)

        Length = IAND(Length - 1, NOT(63))
        Offset = 0
        MainLoop: DO
            X = RotateRight(X + Y + VFirst + PackLong(Input, Offset + 8), 37)*K1
            Y = RotateRight(Y + VSecond + PackLong(Input, Offset + 48), 42)*K1
            X = IEOR(X, WSecond)
            Y = Y + VFirst + PackLong(Input, Offset + 40)
            Z = RotateRight(Z + WFirst, 33)*K1

            ! WeakHashLen32WithSeeds
            A1 = VSecond*K1
            B1 = X + WFirst
            W2 = PackLong(Input, Offset)
            X2 = PackLong(Input, Offset + 8)
            Y2 = PackLong(Input, Offset + 16)
            Z2 = PackLong(Input, Offset + 24)
            A1 = A1 + W2
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + X2 + Y2
            B1 = B1 + RotateRight(A1, 44)
            VFirst = A1 + Z2
            VSecond = B1 + C1

            ! WeakHashLen32WithSeeds
            A = Z + WSecond
            B = Y + PackLong(Input, Offset + 16)
            W1 = PackLong(Input, Offset + 32)
            X1 = PackLong(Input, Offset + 32 + 8)
            Y1 = PackLong(Input, Offset + 32 + 16)
            Z1 = PackLong(Input, Offset + 32 + 24)
            A = A + W1
            B = RotateRight(B + A + Z1, 21)
            C = A
            A = A + X1 + Y1
            B = B + RotateRight(A, 44)
            WFirst = A + Z1
            WSecond = B + C

            Tmp = X
            X = Z
            Z = Tmp

            Length = Length - 64
            Offset = Offset + 64
            IF (Length == 0) EXIT MainLoop
        END DO MainLoop
        HashCode = HashLen16_2(HashLen16_2(VFirst, WFirst) + ShiftMix(Y)*K1 + Z, \
                               HashLen16_2(VSecond, WSecond) + X)
    END IF

    ! finalize with seed(s)
    IF (PRESENT(Seed2)) THEN
        HashCode = HashLen16_2(HashCode - Seed2, Seed1)
    ELSE
        HashCode = HashLen16_2(HashCode - K2, Seed1)
    END IF

    RETURN

#undef GetU8
#undef Pack_U32

END FUNCTION City_Hash64

!******************************************************************************

FUNCTION City_Hash64_08(Input, Seed1, PackInteger, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the CityHash hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN) :: Input(0:)    !! input bytes
    tLong,           INTENT(IN) :: Seed1        !! required seed
    PROCEDURE(Pack_I32)         :: PackInteger  !! procedure to convert a byte array to 32-bit integer
    tLong, OPTIONAL, INTENT(IN) :: Seed2        !! optional (additional) seed
    tLong                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: LongVal(:)
    tLong, POINTER  :: I64Val
    tIndex          :: Length, Offset
    tLong           :: X, Y, Z, Tmp
    tLong           :: VFirst, VSecond, WFirst, WSecond
    tLong           :: A, B, C, W1, X1, Y1, Z1
    tLong           :: A1, B1, C1, W2, X2, Y2, Z2
    tLong           :: A2, B2, C2, W3, X3, Y3, Z3
    tLong           :: A3, B3, C3, W4, X4, Y4, Z4
    TYPE(C_PTR)     :: CPTR

!** FLOW

#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN

        ! CityHash_Len0To16
        IF (Length >= 8) THEN
            BLOCK
                tLong   :: M, A, C, D
                tLong   :: First8Bytes, Last8Bytes
                CPTR = C_LOC(Input(Offset))
                CALL C_F_POINTER(CPTR, I64Val)
                First8Bytes = I64Val
                CPTR = C_LOC(Input(Offset + Length - 8))
                CALL C_F_POINTER(CPTR, I64Val)
                Last8Bytes  = I64Val
                ! Hash 8 To 16 Bytes
                M = Mul(Length)
                A = First8Bytes + K2
                C = RotateRight(Last8Bytes, 37)*M + A
                D = (RotateRight(A, 25) + Last8Bytes)*M
                HashCode = HashLen16_3(C, D, M)
            END BLOCK
        ELSEIF (Length >= 4) THEN
            ! set procedure pointer
            BLOCK
                tLong   :: M, First4Bytes, Last4Bytes
                First4Bytes = Pack_U32(Input, Offset)
                Last4Bytes  = Pack_U32(Input, Offset + Length - 4)
                ! Hash 4 To 7 Bytes
                M = Mul(Length)
                HashCode = HashLen16_3(ToLong(Length) + SHIFTL(First4Bytes, 3), Last4Bytes, M)
            END BLOCK
        ELSEIF (Length > 0) THEN
            BLOCK
                tInteger    :: FirstByte, MidOrLastByte, LastByte
                tInteger    :: Y, Z
                FirstByte     = MaskI8(Input(Offset))
                MidOrLastByte = MaskI8(Input(Offset + SHIFTA(Length, 1)))
                LastByte      = MaskI8(Input(Offset + Length - 1))
                ! Hash 1 To 3 Bytes
                Y = FirstByte + SHIFTL(MidOrLastByte, 8)
                Z = ToInteger(Length) + SHIFTL(LastByte, 2)
                HashCode = ShiftMix(IEOR(ToLong(Y)*K2, ToLong(Z)*K0))*K2
            END BLOCK
        ELSE
            HashCode = K2
        END IF

    ELSEIF (Length <= 32) THEN
        ! CityHash_Len17To32
        BLOCK
            tLong   :: M, A, B, C, D
            ! perform hashing
            M = Mul(Length)
            CPTR = C_LOC(Input(Offset))
            CALL C_F_POINTER(CPTR, LongVal, SHAPE=[2])
            A = LongVal(1)*K1
            B = LongVal(2)
            CPTR = C_LOC(Input(Offset + Length - 16))
            CALL C_F_POINTER(CPTR, LongVal, SHAPE=[2])
            C = LongVal(2)*M
            D = LongVal(1)*K2
            HashCode = HashLen16_3(RotateRight(A+B, 43) + RotateRight(C, 30) + D, \
                                A + RotateRight(B+K2, 18) + C, M)
        END BLOCK
    ELSEIF (Length <= 64) THEN
        ! CityHash_Len33To64
        BLOCK
            tLong   :: M, A, B, C, D, E, F, G, H
            tLong   :: U, V, W, X, Y, Z
            ! perform hashing
            M = Mul(Length)
            CPTR = C_LOC(Input(Offset))
            CALL C_F_POINTER(CPTR, LongVal, SHAPE=[4])
            A = LongVal(1)*K2
            B = LongVal(2)
            E = LongVal(3)*K2
            F = LongVal(4)*9_kLong
            CPTR = C_LOC(Input(Offset + Length - 32))
            CALL C_F_POINTER(CPTR, LongVal, SHAPE=[4])
            C = LongVal(2)
            D = LongVal(1)
            G = LongVal(4)
            H = LongVal(3)*M
            U = RotateRight(A+G, 43) + (RotateRight(B, 30) + C)*9_kLong
            V = IEOR(A+G, D) + F + 1_kLong
            w = LongReverseBytes((U + V)*M) + H
            X = RotateRight(E+F, 42) + C
            Y = (LongReverseBytes((V + w)*M) + G)*M
            Z = E + F + C
            A = LongReverseBytes((X + Z)*M + Y) + B
            B = ShiftMix((Z + A)*M + D + H)*M
            HashCode = B + X
        END BLOCK
    ELSE
        CPTR = C_LOC(Input(Offset + Length - 64))
        CALL C_F_POINTER(CPTR, LongVal, SHAPE=[8])
        X = LongVal(4)
        Y = LongVal(7) + LongVal(2)
        Z = HashLen16_2(LongVal(3) + Length, LongVal(6))

        ! This and following 3 blocks are produced by a single-click
        ! inline-function refactoring.
        ! IntelliJ IDEA ftw
        ! WeakHashLen32WithSeeds
        A3 = Length
        B3 = Z
        W4 = LongVal(1)
        X4 = LongVal(2)
        Y4 = LongVal(3)
        Z4 = LongVal(4)
        A3 = A3 + W4
        B3 = RotateRight(B3 + A3 + Z4, 21)
        C3 = A3
        A3 = A3 + X4 + Y4
        B3 = B3 + RotateRight(A3, 44)
        VFirst = A3 + Z4
        VSecond = B3 + C3

        ! WeakHashLen32WithSeeds
        A2 = Y + K1
        B2 = X
        W3 = LongVal(5)
        X3 = LongVal(6)
        Y3 = LongVal(7)
        Z3 = LongVal(8)
        A2 = A2 + W3
        B2 = RotateRight(B2 + A2 + Z3, 21)
        C2 = A2
        A2 = A2 + X3 + Y3
        B2 = B2 + RotateRight(A2, 44)
        WFirst = A2 + Z3
        WSecond = B2 + C2

        CPTR = C_LOC(Input(Offset))
        CALL C_F_POINTER(CPTR, LongVal, SHAPE=[8])
        X = X*K1 + LongVal(1)

        Length = IAND(Length - 1, NOT(63))
        MainLoop: DO
            X = RotateRight(X + Y + VFirst + LongVal(2), 37)*K1
            Y = RotateRight(Y + VSecond + LongVal(7), 42)*K1
            X = IEOR(X, WSecond)
            Y = Y + VFirst + LongVal(6)
            Z = RotateRight(Z + WFirst, 33)*K1

            ! WeakHashLen32WithSeeds
            A1 = VSecond*K1
            B1 = X + WFirst
            W2 = LongVal(1)
            X2 = LongVal(2)
            Y2 = LongVal(3)
            Z2 = LongVal(4)
            A1 = A1 + W2
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + X2 + Y2
            B1 = B1 + RotateRight(A1, 44)
            VFirst = A1 + Z2
            VSecond = B1 + C1

            ! WeakHashLen32WithSeeds
            A = Z + WSecond
            B = Y + LongVal(3)
            W1 = LongVal(5)
            X1 = LongVal(6)
            Y1 = LongVal(7)
            Z1 = LongVal(8)
            A = A + W1
            B = RotateRight(B + A + Z1, 21)
            C = A
            A = A + X1 + Y1
            B = B + RotateRight(A, 44)
            WFirst = A + Z1
            WSecond = B + C

            Tmp = X
            X = Z
            Z = Tmp

            Length = Length - 64
            Offset = Offset + 64
            IF (Length == 0) EXIT MainLoop
            CPTR = C_LOC(Input(Offset))
            CALL C_F_POINTER(CPTR, LongVal, SHAPE=[8])
        END DO MainLoop
        HashCode = HashLen16_2(HashLen16_2(VFirst, WFirst) + ShiftMix(Y)*K1 + Z, \
                                HashLen16_2(VSecond, WSecond) + X)
    END IF
    ! finalize with seed(s)
    IF (PRESENT(Seed2)) THEN
        HashCode = HashLen16_2(HashCode - Seed2, Seed1)
    ELSE
        HashCode = HashLen16_2(HashCode - K2, Seed1)
    END IF

    RETURN

#undef Pack_U32

END FUNCTION City_Hash64_08

!******************************************************************************

FUNCTION City_Hash64_09(Input, Seed1, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the CityHash hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN) :: Input(0:)    ! input bytes
    tLong,           INTENT(IN) :: Seed1        ! required seed
    tLong, OPTIONAL, INTENT(IN) :: Seed2        ! optional (additional) seed
    tLong                       :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset
    tLong           :: X, Y, Z, Tmp
    tLong           :: VFirst, VSecond, WFirst, WSecond
    tLong           :: A, B, C, W1, X1, Y1, Z1
    tLong           :: A1, B1, C1, W2, X2, Y2, Z2
    tLong           :: A2, B2, C2, W3, X3, Y3, Z3
    tLong           :: A3, B3, C3, W4, X4, Y4, Z4
    tByte           :: ByteVal(0:63)
    tLong           :: LongVal(1:8)
    EQUIVALENCE(ByteVal, LongVal)
    tInteger        :: IntVal(1:2)
    EQUIVALENCE(ByteVal, IntVal)

!** FLOW

#define Pack_U32(Val,Index)     MaskI32(Val(Index))

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! CityHash_Len0To16
        IF (Length >= 8) THEN
            BLOCK
                tLong   :: M, A, C, D
                tLong   :: First8Bytes, Last8Bytes
                tIndex  :: I
                DO I = 0, 7
                    ByteVal(I)   = Input(Offset+I)
                    ByteVal(I+8) = Input(Offset+Length-8+I)
                END DO
                First8Bytes = LongVal(1)
                Last8Bytes  = LongVal(2)
                ! Hash 8 To 16 Bytes
                M = Mul(Length)
                A = First8Bytes + K2
                C = RotateRight(Last8Bytes, 37)*M + A
                D = (RotateRight(A, 25) + Last8Bytes)*M
                HashCode = HashLen16_3(C, D, M)
            END BLOCK
        ELSEIF (Length >= 4) THEN
            ! set procedure pointer
            BLOCK
                tLong   :: M, First4Bytes, Last4Bytes
                tIndex  :: I
                DO I = 0, 3
                    ByteVal(I)   = Input(Offset+I)
                    ByteVal(I+4) = Input(Offset+Length-4+I)
                END DO
                First4Bytes = Pack_U32(IntVal, 1)
                Last4Bytes  = Pack_U32(IntVal, 2)
                ! Hash 4 To 7 Bytes
                M = Mul(Length)
                HashCode = HashLen16_3(ToLong(Length) + SHIFTL(First4Bytes, 3), Last4Bytes, M)
            END BLOCK
        ELSEIF (Length > 0) THEN
            BLOCK
                tInteger    :: FirstByte, MidOrLastByte, LastByte
                tInteger    :: Y, Z
                FirstByte     = MaskI8(Input(Offset))
                MidOrLastByte = MaskI8(Input(Offset + SHIFTA(Length, 1)))
                LastByte      = MaskI8(Input(Offset + Length - 1))
                ! Hash 1 To 3 Bytes
                Y = FirstByte + SHIFTL(MidOrLastByte, 8)
                Z = ToInteger(Length) + SHIFTL(LastByte, 2)
                HashCode = ShiftMix(IEOR(ToLong(Y)*K2, ToLong(Z)*K0))*K2
            END BLOCK
        ELSE
            HashCode = K2
        END IF
    ELSEIF (Length <= 32) THEN
        ! CityHash_Len17To32
        BLOCK
            tLong   :: M, A, B, C, D
            tIndex  :: I
            ! perform hashing
            M = Mul(Length)
            DO I = 0, 15
                ByteVal(I)    = Input(Offset+I)
                ByteVal(I+16) = Input(Offset+Length-16+I)
            END DO
            A = LongVal(1)*K1
            B = LongVal(2)
            C = LongVal(4)*M
            D = LongVal(3)*K2
            HashCode = HashLen16_3(RotateRight(A+B, 43) + RotateRight(C, 30) + D, \
                                   A + RotateRight(B+K2, 18) + C, M)
        END BLOCK
    ELSEIF (Length <= 64) THEN
        ! CityHash_Len33To64
        BLOCK
            tLong   :: M, A, B, C, D, E, F, G, H
            tLong   :: U, V, W, X, Y, Z
            tIndex  :: I
            ! perform hashing
            M = Mul(Length)
            DO I = 0, 31
                ByteVal(I)    = Input(Offset+I)
                ByteVal(I+32) = Input(Offset+Length-32+I)
            END DO
            A = LongVal(1)*K2
            B = LongVal(2)
            C = LongVal(6)
            D = LongVal(5)
            E = LongVal(3)*K2
            F = LongVal(4)*9_kLong
            G = LongVal(8)
            H = LongVal(7)*M
            U = RotateRight(A+G, 43) + (RotateRight(B, 30) + C)*9_kLong
            V = IEOR(A+G, D) + F + 1_kLong
            w = LongReverseBytes((U + V)*M) + H
            X = RotateRight(E+F, 42) + C
            Y = (LongReverseBytes((V + w)*M) + G)*M
            Z = E + F + C
            A = LongReverseBytes((X + Z)*M + Y) + B
            B = ShiftMix((Z + A)*M + D + H)*M
            HashCode = B + X
        END BLOCK
    ELSE
        ByteVal(0:63) = Input(Offset+Length-64:Offset+Length-1)
        X = LongVal(4)
        Y = LongVal(7) + LongVal(2)
        Z = HashLen16_2(LongVal(3) + Length, LongVal(6))

        ! This and following 3 blocks are produced by a single-click
        ! inline-function refactoring.
        ! IntelliJ IDEA ftw
        ! WeakHashLen32WithSeeds
        A3 = Length
        B3 = Z
        W4 = LongVal(1)
        X4 = LongVal(2)
        Y4 = LongVal(3)
        Z4 = LongVal(4)
        A3 = A3 + W4
        B3 = RotateRight(B3 + A3 + Z4, 21)
        C3 = A3
        A3 = A3 + X4 + Y4
        B3 = B3 + RotateRight(A3, 44)
        VFirst = A3 + Z4
        VSecond = B3 + C3

        ! WeakHashLen32WithSeeds
        A2 = Y + K1
        B2 = X
        W3 = LongVal(5)
        X3 = LongVal(6)
        Y3 = LongVal(7)
        Z3 = LongVal(8)
        A2 = A2 + W3
        B2 = RotateRight(B2 + A2 + Z3, 21)
        C2 = A2
        A2 = A2 + X3 + Y3
        B2 = B2 + RotateRight(A2, 44)
        WFirst = A2 + Z3
        WSecond = B2 + C2

        ByteVal(0:63) = Input(Offset:Offset+63)
        X = X*K1 + LongVal(1)

        Length = IAND(Length - 1, NOT(63))
        MainLoop: DO
            X = RotateRight(X + Y + VFirst + LongVal(2), 37)*K1
            Y = RotateRight(Y + VSecond + LongVal(7), 42)*K1
            X = IEOR(X, WSecond)
            Y = Y + VFirst + LongVal(6)
            Z = RotateRight(Z + WFirst, 33)*K1

            ! WeakHashLen32WithSeeds
            A1 = VSecond*K1
            B1 = X + WFirst
            W2 = LongVal(1)
            X2 = LongVal(2)
            Y2 = LongVal(3)
            Z2 = LongVal(4)
            A1 = A1 + W2
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + X2 + Y2
            B1 = B1 + RotateRight(A1, 44)
            VFirst = A1 + Z2
            VSecond = B1 + C1

            ! WeakHashLen32WithSeeds
            A = Z + WSecond
            B = Y + LongVal(3)
            W1 = LongVal(5)
            X1 = LongVal(6)
            Y1 = LongVal(7)
            Z1 = LongVal(8)
            A = A + W1
            B = RotateRight(B + A + Z1, 21)
            C = A
            A = A + X1 + Y1
            B = B + RotateRight(A, 44)
            WFirst = A + Z1
            WSecond = B + C

            Tmp = X
            X = Z
            Z = Tmp

            Length = Length - 64
            Offset = Offset + 64
            IF (Length == 0) EXIT MainLoop
            ByteVal(0:63) = Input(Offset:Offset+63)
        END DO MainLoop
        HashCode = HashLen16_2(HashLen16_2(VFirst, WFirst) + ShiftMix(Y)*K1 + Z, \
                                HashLen16_2(VSecond, WSecond) + X)
    END IF
    ! finalize with seed(s)
    IF (PRESENT(Seed2)) THEN
        HashCode = HashLen16_2(HashCode - Seed2, Seed1)
    ELSE
        HashCode = HashLen16_2(HashCode - K2, Seed1)
    END IF

    RETURN

#undef Pack_U32

END FUNCTION City_Hash64_09

!******************************************************************************

FUNCTION City_Hash64_10(Input, Seed1, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the CityHash hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN) :: Input(0:)    ! input bytes
    tLong,           INTENT(IN) :: Seed1        ! required seed
    tLong, OPTIONAL, INTENT(IN) :: Seed2        ! optional (additional) seed
    tLong                       :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: Long2Val(:) => NULL()
    tIndex          :: Length, Offset
    tLong           :: X, Y, Z, Tmp
    tLong           :: VFirst, VSecond, WFirst, WSecond
    tLong           :: A, B, C, W1, X1, Y1, Z1
    tLong           :: A1, B1, C1, W2, X2, Y2, Z2
    tLong           :: A2, B2, C2, W3, X3, Y3, Z3
    tLong           :: A3, B3, C3, W4, X4, Y4, Z4
    tByte           :: ByteVal(0:63)
    tLong           :: Long1Val(1:8)
    EQUIVALENCE(ByteVal, Long1Val)
    tInteger        :: IntVal(1:2)
    EQUIVALENCE(ByteVal, IntVal)
    TYPE(C_PTR)     :: CPTR

!** FLOW

#define Pack_U32(Val,Index)     MaskI32(Val(Index))

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 16) THEN
        ! CityHash_Len0To16
        IF (Length >= 8) THEN
            Len8To16: BLOCK
                tLong   :: M, A, C, D
                tLong   :: First8Bytes, Last8Bytes
                tIndex  :: I
                DO I = 0, 7
                    ByteVal(I)   = Input(I)
                    ByteVal(I+8) = Input(Length-8+I)
                END DO
                First8Bytes = Long1Val(1)
                Last8Bytes  = Long1Val(2)
                ! Hash 8 To 16 Bytes
                M = Mul(Length)
                A = First8Bytes + K2
                C = RotateRight(Last8Bytes, 37)*M + A
                D = (RotateRight(A, 25) + Last8Bytes)*M
                HashCode = HashLen16_3(C, D, M)
            END BLOCK Len8To16
        ELSEIF (Length >= 4) THEN
            ! set procedure pointer
            Len4To7: BLOCK
                tLong   :: M, First4Bytes, Last4Bytes
                tIndex  :: I
                DO I = 0, 3
                    ByteVal(I)   = Input(I)
                    ByteVal(I+4) = Input(Length-4+I)
                END DO
                First4Bytes = Pack_U32(IntVal, 1)
                Last4Bytes  = Pack_U32(IntVal, 2)
                ! Hash 4 To 7 Bytes
                M = Mul(Length)
                HashCode = HashLen16_3(ToLong(Length) + SHIFTL(First4Bytes, 3), Last4Bytes, M)
            END BLOCK Len4To7
        ELSEIF (Length > 0) THEN
            Len1To3: BLOCK
                tInteger    :: FirstByte, MidOrLastByte, LastByte
                tInteger    :: Y, Z
                FirstByte     = MaskI8(Input(0))
                MidOrLastByte = MaskI8(Input(0 + SHIFTA(Length, 1)))
                LastByte      = MaskI8(Input(0 + Length - 1))
                ! Hash 1 To 3 Bytes
                Y = FirstByte + SHIFTL(MidOrLastByte, 8)
                Z = ToInteger(Length) + SHIFTL(LastByte, 2)
                HashCode = ShiftMix(IEOR(ToLong(Y)*K2, ToLong(Z)*K0))*K2
            END BLOCK Len1To3
        ELSE
            HashCode = K2
        END IF
    ELSEIF (Length <= 32) THEN
        ! CityHash_Len17To32
        Len17To32: BLOCK
            tLong   :: M, A, B, C, D
            tIndex  :: I
            ! perform hashing
            M = Mul(Length)
            DO I = 0, 15
                ByteVal(I)    = Input(I)
                ByteVal(I+16) = Input(Length-16+I)
            END DO
            A = Long1Val(1)*K1
            B = Long1Val(2)
            C = Long1Val(4)*M
            D = Long1Val(3)*K2
            HashCode = HashLen16_3(RotateRight(A+B, 43) + RotateRight(C, 30) + D, \
                                A + RotateRight(B+K2, 18) + C, M)
        END BLOCK Len17To32
    ELSEIF (Length <= 64) THEN
        ! CityHash_Len33To64
        Len33To64: BLOCK
            tLong   :: M, A, B, C, D, E, F, G, H
            tLong   :: U, V, W, X, Y, Z
            tIndex  :: I
            ! perform hashing
            M = Mul(Length)
            DO I = 0, 31
                ByteVal(I)    = Input(I)
                ByteVal(I+32) = Input(Length-32+I)
            END DO
            A = Long1Val(1)*K2
            B = Long1Val(2)
            C = Long1Val(6)
            D = Long1Val(5)
            E = Long1Val(3)*K2
            F = Long1Val(4)*9_kLong
            G = Long1Val(8)
            H = Long1Val(7)*M
            U = RotateRight(A+G, 43) + (RotateRight(B, 30) + C)*9_kLong
            V = IEOR(A+G, D) + F + 1_kLong
            w = LongReverseBytes((U + V)*M) + H
            X = RotateRight(E+F, 42) + C
            Y = (LongReverseBytes((V + w)*M) + G)*M
            Z = E + F + C
            A = LongReverseBytes((X + Z)*M + Y) + B
            B = ShiftMix((Z + A)*M + D + H)*M
            HashCode = B + X
        END BLOCK Len33To64
    ELSEIF (Length <= 192) THEN
        ByteVal(0:63) = Input(Length-64:Length-1)
        X = Long1Val(4)
        Y = Long1Val(7) + Long1Val(2)
        Z = HashLen16_2(Long1Val(3) + Length, Long1Val(6))

        ! WeakHashLen32WithSeeds
        A3 = Length
        B3 = Z
        W4 = Long1Val(1)
        X4 = Long1Val(2)
        Y4 = Long1Val(3)
        Z4 = Long1Val(4)
        A3 = A3 + W4
        B3 = RotateRight(B3 + A3 + Z4, 21)
        C3 = A3
        A3 = A3 + X4 + Y4
        B3 = B3 + RotateRight(A3, 44)
        VFirst = A3 + Z4
        VSecond = B3 + C3

        ! WeakHashLen32WithSeeds
        A2 = Y + K1
        B2 = X
        W3 = Long1Val(5)
        X3 = Long1Val(6)
        Y3 = Long1Val(7)
        Z3 = Long1Val(8)
        A2 = A2 + W3
        B2 = RotateRight(B2 + A2 + Z3, 21)
        C2 = A2
        A2 = A2 + X3 + Y3
        B2 = B2 + RotateRight(A2, 44)
        WFirst = A2 + Z3
        WSecond = B2 + C2

        ByteVal(0:63) = Input(0:63)
        X = X*K1 + Long1Val(1)

        Length = IAND(Length - 1, NOT(63))
        Offset = 0

        MainLoop1: DO
            X = RotateRight(X + Y + VFirst + Long1Val(2), 37)*K1
            Y = RotateRight(Y + VSecond + Long1Val(7), 42)*K1
            X = IEOR(X, WSecond)
            Y = Y + VFirst + Long1Val(6)
            Z = RotateRight(Z + WFirst, 33)*K1

            ! WeakHashLen32WithSeeds
            A1 = VSecond*K1
            B1 = X + WFirst
            W2 = Long1Val(1)
            X2 = Long1Val(2)
            Y2 = Long1Val(3)
            Z2 = Long1Val(4)
            A1 = A1 + W2
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + X2 + Y2
            B1 = B1 + RotateRight(A1, 44)
            VFirst = A1 + Z2
            VSecond = B1 + C1

            ! WeakHashLen32WithSeeds
            A = Z + WSecond
            B = Y + Long1Val(3)
            W1 = Long1Val(5)
            X1 = Long1Val(6)
            Y1 = Long1Val(7)
            Z1 = Long1Val(8)
            A = A + W1
            B = RotateRight(B + A + Z1, 21)
            C = A
            A = A + X1 + Y1
            B = B + RotateRight(A, 44)
            WFirst = A + Z1
            WSecond = B + C

            Tmp = X
            X = Z
            Z = Tmp

            Length = Length - 64
            Offset = Offset + 64
            IF (Length == 0) EXIT MainLoop1
            ByteVal(0:63) = Input(Offset:Offset+63)
        END DO MainLoop1
        HashCode = HashLen16_2(HashLen16_2(VFirst, WFirst) + ShiftMix(Y)*K1 + Z, \
                                HashLen16_2(VSecond, WSecond) + X)
    ELSE
        CPTR = C_LOC(Input(Length - 64))
        CALL C_F_POINTER(CPTR, Long2Val, SHAPE=[8])
        X = Long2Val(4)
        Y = Long2Val(7) + Long2Val(2)
        Z = HashLen16_2(Long2Val(3) + Length, Long2Val(6))

        ! This and following 3 blocks are produced by a single-click
        ! inline-function refactoring.
        ! IntelliJ IDEA ftw
        ! WeakHashLen32WithSeeds
        A3 = Length
        B3 = Z
        W4 = Long2Val(1)
        X4 = Long2Val(2)
        Y4 = Long2Val(3)
        Z4 = Long2Val(4)
        A3 = A3 + W4
        B3 = RotateRight(B3 + A3 + Z4, 21)
        C3 = A3
        A3 = A3 + X4 + Y4
        B3 = B3 + RotateRight(A3, 44)
        VFirst = A3 + Z4
        VSecond = B3 + C3

        ! WeakHashLen32WithSeeds
        A2 = Y + K1
        B2 = X
        W3 = Long2Val(5)
        X3 = Long2Val(6)
        Y3 = Long2Val(7)
        Z3 = Long2Val(8)
        A2 = A2 + W3
        B2 = RotateRight(B2 + A2 + Z3, 21)
        C2 = A2
        A2 = A2 + X3 + Y3
        B2 = B2 + RotateRight(A2, 44)
        WFirst = A2 + Z3
        WSecond = B2 + C2

        CPTR = C_LOC(Input(0))
        CALL C_F_POINTER(CPTR, Long2Val, SHAPE=[8])
        X = X*K1 + Long2Val(1)

        Length = IAND(Length - 1, NOT(63))
        Offset = 0

        MainLoop2: DO
            X = RotateRight(X + Y + VFirst + Long2Val(2), 37)*K1
            Y = RotateRight(Y + VSecond + Long2Val(7), 42)*K1
            X = IEOR(X, WSecond)
            Y = Y + VFirst + Long2Val(6)
            Z = RotateRight(Z + WFirst, 33)*K1

            ! WeakHashLen32WithSeeds
            A1 = VSecond*K1
            B1 = X + WFirst
            W2 = Long2Val(1)
            X2 = Long2Val(2)
            Y2 = Long2Val(3)
            Z2 = Long2Val(4)
            A1 = A1 + W2
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + X2 + Y2
            B1 = B1 + RotateRight(A1, 44)
            VFirst = A1 + Z2
            VSecond = B1 + C1

            ! WeakHashLen32WithSeeds
            A = Z + WSecond
            B = Y + Long2Val(3)
            W1 = Long2Val(5)
            X1 = Long2Val(6)
            Y1 = Long2Val(7)
            Z1 = Long2Val(8)
            A = A + W1
            B = RotateRight(B + A + Z1, 21)
            C = A
            A = A + X1 + Y1
            B = B + RotateRight(A, 44)
            WFirst = A + Z1
            WSecond = B + C

            Tmp = X
            X = Z
            Z = Tmp

            Length = Length - 64
            Offset = Offset + 64
            IF (Length == 0) EXIT MainLoop2
            CPTR = C_LOC(Input(Offset))
            CALL C_F_POINTER(CPTR, Long2Val, SHAPE=[8])
        END DO MainLoop2
        HashCode = HashLen16_2(HashLen16_2(VFirst, WFirst) + ShiftMix(Y)*K1 + Z, \
                                HashLen16_2(VSecond, WSecond) + X)
    END IF

    ! finalize with seed(s)
    IF (PRESENT(Seed2)) THEN
        HashCode = HashLen16_2(HashCode - Seed2, Seed1)
    ELSE
        HashCode = HashLen16_2(HashCode - K2, Seed1)
    END IF

    RETURN

#undef Pack_U32

END FUNCTION City_Hash64_10

!******************************************************************************

#undef ShiftMix
#undef HashLen16_3
#undef HashLen16_2
#undef Mul

END SUBMODULE SubBase_CityHash64_Exp

!******************************************************************************
