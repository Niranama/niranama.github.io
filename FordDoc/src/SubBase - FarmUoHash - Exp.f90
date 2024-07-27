
SUBMODULE (ModBase_ExperimentalHash64) SubBase_FarmUoHash_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the FarmUo hash algorithms
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

MODULE FUNCTION FarmUo_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FarmUoHash hash algorithm by Google Inc.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-9)
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
        HashCode = FarmUo_Hash64(InpPtr, Seed1, Pack_I64_A1, Pack_I32_A1, Seed)
    CASE (2)
        HashCode = FarmUo_Hash64(InpPtr, Seed1, Pack_I64_A2, Pack_I32_A2, Seed)
    CASE (3)
        HashCode = FarmUo_Hash64(InpPtr, Seed1, Pack_I64_A3, Pack_I32_A3, Seed)
    CASE (4)
        HashCode = FarmUo_Hash64(InpPtr, Seed1, Pack_I64_A4, Pack_I32_A4, Seed)
    CASE (5)
        HashCode = FarmUo_Hash64(InpPtr, Seed1, Pack_I64_A5, Pack_I32_A5, Seed)
    CASE (6)
        HashCode = FarmUo_Hash64(InpPtr, Seed1, Pack_I64_A6, Pack_I32_A6, Seed)
    CASE (7)
        HashCode = FarmUo_Hash64(InpPtr, Seed1, Pack_I64_A7, Pack_I32_A7, Seed)
    CASE (8)
        HashCode = FarmUo_Hash64_08(InpPtr, Seed1, Pack_I32_A6, Seed)
    CASE (9)
        HashCode = FarmUo_Hash64_09(InpPtr, Seed1, Seed)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION FarmUo_Hash64_Exp

!******************************************************************************

FUNCTION FarmUo_Hash64(Input, Seed1, PackLong, PackInteger, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the FarmUoHash64 hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN) :: Input(0:)    !! input bytes
    tLong,           INTENT(IN) :: Seed1        !! seed
    PROCEDURE(Pack_I64)         :: PackLong     !! procedure to convert a byte array to 64-bit integer
    PROCEDURE(Pack_I32)         :: PackInteger  !! procedure to convert a byte array to 32-bit integer
    tLong, OPTIONAL, INTENT(IN) :: Seed2        !! optional (additional) seed
    tLong                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset, IEnd, Last64
    tLong           :: Seed0, M, X, Y, Z, U, V0, V1, W0, W1
    tLong           :: A0, A1, A2, A3, A4, A5, A6, A7, Tmp
    tLong           :: A, B, C, B1, C1, Z1, Z2

!** FLOW

#define GetU8(In, Off)          MaskI8(In(Off))
#define Pack_U32(In, Off)       MaskI32(PackInteger(In, Off))
#define FarmUoH(X, Y, M, R)     RotateRight(IEOR(Y, ShiftMix(IEOR(X, Y)*M))*M, R)*M

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 64) THEN
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
        ELSE
            ! FarmNaHash_Len33To64
            BLOCK
                tLong       :: M, A, B, C, D, E, F, G, H
                tLong       :: Y, Z
                ! perform hashing
                M = Mul(Length)
                A = PackLong(Input, 0)*K2
                B = PackLong(Input, 8)
                C = PackLong(Input, Length - 8)*M
                D = PackLong(Input, Length - 16)*K2
                Y = RotateRight(A + B, 43) + RotateRight(C, 30) + D
                Z = HashLen16_3(Y, A + RotateRight(B + K2, 18) + C, M)
                E = PackLong(Input, 16)*M
                F = PackLong(Input, 24)
                G = (Y + PackLong(Input, Length - 32))*M
                H = (Z + PackLong(Input, Length - 24))*M
                HashCode = HashLen16_3(RotateRight(E + F, 43) + RotateRight(G, 30) + H, \
                                   E + RotateRight(F + A, 18) + G, M)
            END BLOCK
        END IF

        ! finalize with seed(s)
        IF (PRESENT(Seed2)) THEN
            HashCode = HashLen16_2(HashCode - Seed2, Seed1)
        ELSE
            HashCode = HashLen16_2(HashCode - K2, Seed1)
        END IF

        RETURN

    END IF

    IF (PRESENT(Seed2)) THEN
        Seed0 = Seed2
    ELSE
        Seed0 = 0_kLong
    END IF

    ! perform hashing
    X = Seed0
    Y = Seed1*K2 + 113_kLong
    Z = ShiftMix(Y*K2)*K2
    V0 = Seed0
    V1 = Seed1
    W0 = 0_kLong
    W1 = 0_kLong
    U = X - Z
    X = X*K2
    M = K2 + IAND(U, ToLong(Z'82'))

    IEnd = SHIFTA((Length - 1), 6)*64
    Last64 = IEnd + IAND((Length - 1), 63) - 63
    Offset = 0

    DO
        A0 = PackLong(Input, Offset)
        A1 = PackLong(Input, Offset + 8)
        A2 = PackLong(Input, Offset + 16)
        A3 = PackLong(Input, Offset + 24)
        A4 = PackLong(Input, Offset + 32)
        A5 = PackLong(Input, Offset + 40)
        A6 = PackLong(Input, Offset + 48)
        A7 = PackLong(Input, Offset + 56)
        X = X + A0 + A1
        Y = Y + A2
        Z = Z + A3
        V0 = V0 + A4
        V1 = V1 + A5 + A1
        W0 = W0 + A6
        W1 = W1 +A7

        X = RotateRight(X, 26)
        X = X*9_kLong
        Y = RotateRight(Y, 29)
        Z = Z*M
        V0 = RotateRight(V0, 33)
        V1 = RotateRight(V1, 30)
        W0 = IEOR(W0, X)
        W0 = W0*9_kLong
        Z = RotateRight(Z, 32)
        Z = Z + W1
        W1 = W1 + Z
        Z = Z*9_kLong

        Tmp = U
        U = Y
        Y = Tmp

        Z = Z + A0 + A6
        V0 = V0 + A2
        V1 = V1 + A3
        W0 = W0 + A4
        W1 = W1 + A5 + A6
        X = X + A1
        Y = Y + A7

        Y = Y + V0
        V0 = V0 + X - Y
        V1 = V1 + W0
        W0 = W0 + V1
        W1 = W1 + X - Y
        X = X + W1
        W1 = RotateRight(W1, 34)

        Tmp = U
        U = Z
        Z = Tmp

        Offset = Offset + 64
        IF (Offset == IEnd) EXIT
    END DO

    Offset = Last64

    U = U*9_kLong
    V1 = RotateRight(V1, 28)
    V0 = RotateRight(V0, 20)
    W0 = W0 + ToLong(IAND(Length - 1, 63))
    U = U + Y
    Y = Y + U
    X = RotateRight(Y - X + V0 + PackLong(Input, Offset + 8), 37)*M
    Y = RotateRight(IEOR(IEOR(Y, V1), PackLong(Input, Offset + 48)), 42)*M
    X = IEOR(X, W1*9_kLong)
    Y = Y + V0 + PackLong(Input, Offset + 40)
    Z = RotateRight(Z + W0, 33)*M

    A = V1*M
    B = X + W0
    Z1 = PackLong(Input, Offset + 24)
    A = A + PackLong(Input, Offset)
    B = RotateRight(B + A + Z1, 21)
    C = A
    A = A + PackLong(Input, Offset + 8)
    A = A + PackLong(Input, Offset + 16)
    B = B + RotateRight(A, 44)
    V0 =  A + Z1
    V1 = B + C

    A1 = Z + W1
    B1 = Y + PackLong(Input, Offset + 16)
    Z2 = PackLong(Input, Offset + 32 + 24)
    A1 = A1 + PackLong(Input, Offset + 32)
    B1 = RotateRight(B1 + A1 + Z2, 21)
    C1 = A1
    A1 = A1 + PackLong(Input, Offset + 32 + 8)
    A1 = A1 + PackLong(Input, Offset + 32 + 16)
    B1 = B1 + RotateRight(A1, 44)
    W0 = A1 + Z2
    W1 = B1 + C1

    HashCode = FarmUoH(HashLen16_3(V0 + X, IEOR(W0, Y), M) + Z - U,     \
                       IEOR(FarmUoH(V1 + Y, W1 + Z, K2, 30_kInteger), X), K2, 31_kInteger)

    RETURN

#undef GetU8
#undef Pack_U32
#undef FarmUoH

END FUNCTION FarmUo_Hash64

!******************************************************************************

FUNCTION FarmUo_Hash64_08(Input, Seed1, PackInteger, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the FarmUoHash hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN) :: Input(0:)    !! input bytes
    tLong,           INTENT(IN) :: Seed1        !! required seed
    PROCEDURE(Pack_I32)         :: PackInteger  !! procedure to convert a byte array to 32-bit integer
    tLong, OPTIONAL, INTENT(IN) :: Seed2        !! optional (additional) seed
    tLong                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: LongVal(:)
    tLong, POINTER  :: I64Val
    tIndex          :: Length, Offset, IEnd, Last64
    tLong           :: Seed0, M, X, Y, Z, U, V0, V1, W0, W1
    tLong           :: A, A1, B, C, B1, C1, Tmp
    TYPE(C_PTR)     :: CPTR

!** FLOW

#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))
#define FarmUoH(X, Y, M, R)     RotateRight(IEOR(Y, ShiftMix(IEOR(X, Y)*M))*M, R)*M

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 64) THEN
        IF (Length <= 16) THEN
            ! FarmUoHash_Len0To16
            IF (Length >= 8) THEN
                BLOCK
                    tLong   :: M, A, C, D
                    tLong   :: First8Bytes, Last8Bytes
                    CPTR = C_LOC(Input(0))
                    CALL C_F_POINTER(CPTR, I64Val)
                    First8Bytes = I64Val
                    CPTR = C_LOC(Input(Length - 8))
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
                    FirstByte     = MaskI8(Input(0))
                    MidOrLastByte = MaskI8(Input(SHIFTA(Length, 1)))
                    LastByte      = MaskI8(Input(Length - 1))
                    ! Hash 1 To 3 Bytes
                    Y = FirstByte + SHIFTL(MidOrLastByte, 8)
                    Z = ToInteger(Length) + SHIFTL(LastByte, 2)
                    HashCode = ShiftMix(IEOR(ToLong(Y)*K2, ToLong(Z)*K0))*K2
                END BLOCK
            ELSE
                HashCode = K2
            END IF
        ELSEIF (Length <= 32) THEN
            ! FarmUoHash_Len17To32
            BLOCK
                tLong   :: M, A, B, C, D
                ! perform hashing
                M = Mul(Length)
                CPTR = C_LOC(Input(0))
                CALL C_F_POINTER(CPTR, LongVal, SHAPE=[2])
                A = LongVal(1)*K1
                B = LongVal(2)
                CPTR = C_LOC(Input(Length - 16))
                CALL C_F_POINTER(CPTR, LongVal, SHAPE=[2])
                C = LongVal(2)*M
                D = LongVal(1)*K2
                HashCode = HashLen16_3(RotateRight(A+B, 43) + RotateRight(C, 30) + D, \
                                    A + RotateRight(B+K2, 18) + C, M)
            END BLOCK
        ELSE
            ! FarmUoHash_Len33To64
            BLOCK
                tLong       :: M, A, B, C, D, E, F, G, H
                tLong       :: Y, Z
                ! perform hashing
                M = Mul(Length)
                CPTR = C_LOC(Input(0))
                CALL C_F_POINTER(CPTR, LongVal, SHAPE=[4])
                A = LongVal(1)*K2
                B = LongVal(2)
                E = LongVal(3)*M
                F = LongVal(4)
                CPTR = C_LOC(Input(Length - 32))
                CALL C_F_POINTER(CPTR, LongVal, SHAPE=[4])
                C = LongVal(4)*M
                D = LongVal(3)*K2
                Y = RotateRight(A + B, 43) + RotateRight(C, 30) + D
                Z = HashLen16_3(Y, A + RotateRight(B + K2, 18) + C, M)
                G = (Y + LongVal(1))*M
                H = (Z + LongVal(2))*M
                HashCode = HashLen16_3(RotateRight(E + F, 43) + RotateRight(G, 30) + H, \
                                    E + RotateRight(F + A, 18) + G, M)
            END BLOCK

        END IF

        ! finalize with seed(s)
        IF (PRESENT(Seed2)) THEN
            HashCode = HashLen16_2(HashCode - Seed2, Seed1)
        ELSE
            HashCode = HashLen16_2(HashCode - K2, Seed1)
        END IF
        RETURN

    END IF

    IF (PRESENT(Seed2)) THEN
        Seed0 = Seed2
    ELSE
        Seed0 = 0_kLong
    END IF

    ! perform hashing
    X = Seed0
    Y = Seed1*K2 + 113_kLong
    Z = ShiftMix(Y*K2)*K2
    V0 = Seed0
    V1 = Seed1
    W0 = 0_kLong
    W1 = 0_kLong
    U = X - Z
    X = X*K2
    M = K2 + IAND(U, ToLong(Z'82'))

    IEnd = SHIFTA((Length - 1), 6)*64
    Last64 = IEnd + IAND((Length - 1), 63) - 63
    Offset = 0

    DO
        CPTR = C_LOC(Input(Offset))
        CALL C_F_POINTER(CPTR, LongVal, SHAPE=[8])
        X = X + LongVal(1) + LongVal(2)
        Y = Y + LongVal(3)
        Z = Z + LongVal(4)
        V0 = V0 + LongVal(5)
        V1 = V1 + LongVal(6) + LongVal(2)
        W0 = W0 + LongVal(7)
        W1 = W1 + LongVal(8)

        X = RotateRight(X, 26)
        X = X*9_kLong
        Y = RotateRight(Y, 29)
        Z = Z*M
        V0 = RotateRight(V0, 33)
        V1 = RotateRight(V1, 30)
        W0 = IEOR(W0, X)
        W0 = W0*9_kLong
        Z = RotateRight(Z, 32)
        Z = Z + W1
        W1 = W1 + Z
        Z = Z*9_kLong

        Tmp = U
        U = Y
        Y = Tmp

        Z = Z + LongVal(1) + LongVal(7)
        V0 = V0 + LongVal(3)
        V1 = V1 + LongVal(4)
        W0 = W0 + LongVal(5)
        W1 = W1 + LongVal(6) + LongVal(7)
        X = X + LongVal(2)
        Y = Y + LongVal(8)

        Y = Y + V0
        V0 = V0 + X - Y
        V1 = V1 + W0
        W0 = W0 + V1
        W1 = W1 + X - Y
        X = X + W1
        W1 = RotateRight(W1, 34)

        Tmp = U
        U = Z
        Z = Tmp

        Offset = Offset + 64
        IF (Offset == IEnd) EXIT
    END DO

    Offset = Last64
    CPTR = C_LOC(Input(Offset))
    CALL C_F_POINTER(CPTR, LongVal, SHAPE=[8])

    U = U*9_kLong
    V1 = RotateRight(V1, 28)
    V0 = RotateRight(V0, 20)
    W0 = W0 + ToLong(IAND(Length - 1, 63))
    U = U + Y
    Y = Y + U
    X = RotateRight(Y - X + V0 + LongVal(2), 37)*M
    Y = RotateRight(IEOR(IEOR(Y, V1), LongVal(7)), 42)*M
    X = IEOR(X, W1*9_kLong)
    Y = Y + V0 + LongVal(6)
    Z = RotateRight(Z + W0, 33)*M

    A = V1*M
    B = X + W0
    A = A + LongVal(1)
    B = RotateRight(B + A + LongVal(4), 21)
    C = A
    A = A + LongVal(2)
    A = A + LongVal(3)
    B = B + RotateRight(A, 44)
    V0 =  A + LongVal(4)
    V1 = B + C

    A1 = Z + W1
    B1 = Y + LongVal(3)
    A1 = A1 + LongVal(5)
    B1 = RotateRight(B1 + A1 + LongVal(8), 21)
    C1 = A1
    A1 = A1 + LongVal(6)
    A1 = A1 + LongVal(7)
    B1 = B1 + RotateRight(A1, 44)
    W0 = A1 + LongVal(8)
    W1 = B1 + C1

    HashCode = FarmUoH(HashLen16_3(V0 + X, IEOR(W0, Y), M) + Z - U,     \
                        IEOR(FarmUoH(V1 + Y, W1 + Z, K2, 30_kInteger), X), K2, 31_kInteger)

    RETURN

#undef Pack_U32
#undef FarmUoH

END FUNCTION FarmUo_Hash64_08

!******************************************************************************

FUNCTION FarmUo_Hash64_09(Input, Seed1, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the FarmUoHash hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN) :: Input(0:)    ! input bytes
    tLong,           INTENT(IN) :: Seed1        ! required seed
    tLong, OPTIONAL, INTENT(IN) :: Seed2        ! optional (additional) seed
    tLong                       :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset, IEnd, Last64
    tLong           :: Seed0, M, X, Y, Z, U, V0, V1, W0, W1
    tLong           :: A, A1, B, C, B1, C1, Tmp
    tByte           :: ByteVal(0:63)
    tLong           :: LongVal(1:8)
    EQUIVALENCE(ByteVal, LongVal)
    tInteger        :: IntVal(1:2)
    EQUIVALENCE(ByteVal, IntVal)

!** FLOW

#define Pack_U32(Val,Index)     MaskI32(Val(Index))
#define FarmUoH(X, Y, M, R)     RotateRight(IEOR(Y, ShiftMix(IEOR(X, Y)*M))*M, R)*M

    ! initialize
        Length = SIZE(Input)

        ! perform hashing
        IF (Length <= 64) THEN
            IF (Length <= 16) THEN
                ! FarmUoHash_Len0To16
                IF (Length >= 8) THEN
                    BLOCK
                        tLong   :: M, A, C, D
                        tLong   :: First8Bytes, Last8Bytes
                        tIndex  :: I
                        DO I = 0, 7
                            ByteVal(I)   = Input(I)
                            ByteVal(I+8) = Input(Length-8+I)
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
                    BLOCK
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
                    END BLOCK
                ELSEIF (Length > 0) THEN
                    BLOCK
                        tInteger    :: FirstByte, MidOrLastByte, LastByte
                        tInteger    :: Y, Z
                        FirstByte     = MaskI8(Input(0))
                        MidOrLastByte = MaskI8(Input(SHIFTA(Length, 1)))
                        LastByte      = MaskI8(Input(Length - 1))
                        ! Hash 1 To 3 Bytes
                        Y = FirstByte + SHIFTL(MidOrLastByte, 8)
                        Z = ToInteger(Length) + SHIFTL(LastByte, 2)
                        HashCode = ShiftMix(IEOR(ToLong(Y)*K2, ToLong(Z)*K0))*K2
                    END BLOCK
                ELSE
                    HashCode = K2
                END IF
            ELSEIF (Length <= 32) THEN
                ! FarmUoHash_Len17To32
                BLOCK
                    tLong   :: M, A, B, C, D
                    tIndex  :: I
                    ! perform hashing
                    M = Mul(Length)
                    DO I = 0, 15
                        ByteVal(I)    = Input(I)
                        ByteVal(I+16) = Input(Length-16+I)
                    END DO
                    A = LongVal(1)*K1
                    B = LongVal(2)
                    C = LongVal(4)*M
                    D = LongVal(3)*K2
                    HashCode = HashLen16_3(RotateRight(A+B, 43) + RotateRight(C, 30) + D, \
                                       A + RotateRight(B+K2, 18) + C, M)
                END BLOCK
            ELSE
                ! FarmUoHash_Len33To64
                BLOCK
                    tLong       :: M, A, B, C, D, E, F, G, H
                    tLong       :: Y, Z
                    tIndex      :: I
                    ! perform hashing
                    M = Mul(Length)
                    DO I = 0, 31
                        ByteVal(I)    = Input(I)
                        ByteVal(I+32) = Input(Length-32+I)
                    END DO
                    A = LongVal(1)*K2
                    B = LongVal(2)
                    C = LongVal(8)*M
                    D = LongVal(7)*K2
                    E = LongVal(3)*M
                    F = LongVal(4)
                    Y = RotateRight(A + B, 43) + RotateRight(C, 30) + D
                    Z = HashLen16_3(Y, A + RotateRight(B + K2, 18) + C, M)
                    G = (Y + LongVal(5))*M
                    H = (Z + LongVal(6))*M
                    HashCode = HashLen16_3(RotateRight(E + F, 43) + RotateRight(G, 30) + H, \
                                       E + RotateRight(F + A, 18) + G, M)
                END BLOCK
            END IF

            ! finalize with seed(s)
            IF (PRESENT(Seed2)) THEN
                HashCode = HashLen16_2(HashCode - Seed2, Seed1)
            ELSE
                HashCode = HashLen16_2(HashCode - K2, Seed1)
            END IF
            RETURN

        END IF

        IF (PRESENT(Seed2)) THEN
            Seed0 = Seed2
        ELSE
            Seed0 = 0_kLong
        END IF

        ! perform hashing
        X = Seed0
        Y = Seed1*K2 + 113_kLong
        Z = ShiftMix(Y*K2)*K2
        V0 = Seed0
        V1 = Seed1
        W0 = 0_kLong
        W1 = 0_kLong
        U = X - Z
        X = X*K2
        M = K2 + IAND(U, ToLong(Z'82'))

        IEnd = SHIFTA((Length - 1), 6)*64
        Last64 = IEnd + IAND((Length - 1), 63) - 63
        Offset = 0

        DO
            ByteVal(0:63) = Input(Offset:Offset+63)
            X = X + LongVal(1) + LongVal(2)
            Y = Y + LongVal(3)
            Z = Z + LongVal(4)
            V0 = V0 + LongVal(5)
            V1 = V1 + LongVal(6) + LongVal(2)
            W0 = W0 + LongVal(7)
            W1 = W1 + LongVal(8)

            X = RotateRight(X, 26)
            X = X*9_kLong
            Y = RotateRight(Y, 29)
            Z = Z*M
            V0 = RotateRight(V0, 33)
            V1 = RotateRight(V1, 30)
            W0 = IEOR(W0, X)
            W0 = W0*9_kLong
            Z = RotateRight(Z, 32)
            Z = Z + W1
            W1 = W1 + Z
            Z = Z*9_kLong

            Tmp = U
            U = Y
            Y = Tmp

            Z = Z + LongVal(1) + LongVal(7)
            V0 = V0 + LongVal(3)
            V1 = V1 + LongVal(4)
            W0 = W0 + LongVal(5)
            W1 = W1 + LongVal(6) + LongVal(7)
            X = X + LongVal(2)
            Y = Y + LongVal(8)

            Y = Y + V0
            V0 = V0 + X - Y
            V1 = V1 + W0
            W0 = W0 + V1
            W1 = W1 + X - Y
            X = X + W1
            W1 = RotateRight(W1, 34)

            Tmp = U
            U = Z
            Z = Tmp

            Offset = Offset + 64
            IF (Offset == IEnd) EXIT
        END DO

        Offset = Last64
        ByteVal(0:63) = Input(Offset:Offset+63)

        U = U*9_kLong
        V1 = RotateRight(V1, 28)
        V0 = RotateRight(V0, 20)
        W0 = W0 + ToLong(IAND(Length - 1, 63))
        U = U + Y
        Y = Y + U
        X = RotateRight(Y - X + V0 + LongVal(2), 37)*M
        Y = RotateRight(IEOR(IEOR(Y, V1), LongVal(7)), 42)*M
        X = IEOR(X, W1*9_kLong)
        Y = Y + V0 + LongVal(6)
        Z = RotateRight(Z + W0, 33)*M

        A = V1*M
        B = X + W0
        A = A + LongVal(1)
        B = RotateRight(B + A + LongVal(4), 21)
        C = A
        A = A + LongVal(2)
        A = A + LongVal(3)
        B = B + RotateRight(A, 44)
        V0 =  A + LongVal(4)
        V1 = B + C

        A1 = Z + W1
        B1 = Y + LongVal(3)
        A1 = A1 + LongVal(5)
        B1 = RotateRight(B1 + A1 + LongVal(8), 21)
        C1 = A1
        A1 = A1 + LongVal(6)
        A1 = A1 + LongVal(7)
        B1 = B1 + RotateRight(A1, 44)
        W0 = A1 + LongVal(8)
        W1 = B1 + C1

        HashCode = FarmUoH(HashLen16_3(V0 + X, IEOR(W0, Y), M) + Z - U,     \
                           IEOR(FarmUoH(V1 + Y, W1 + Z, K2, 30_kInteger), X), K2, 31_kInteger)

        RETURN

#undef Pack_U32
#undef FarmUoH

END FUNCTION FarmUo_Hash64_09

!******************************************************************************

END SUBMODULE SubBase_FarmUoHash_Exp

!******************************************************************************
