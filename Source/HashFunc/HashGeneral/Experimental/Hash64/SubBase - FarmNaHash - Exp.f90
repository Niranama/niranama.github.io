
SUBMODULE (ModBase_ExperimentalHash64) SubBase_FarmNaHash_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the FarmNa hash algorithms
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

MODULE FUNCTION FarmNa_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FarmNaHash hash algorithm by Google Inc.
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
        HashCode = FarmNa_Hash64(InpPtr, Seed1, Pack_I64_A1, Pack_I32_A1, Seed)
    CASE (2)
        HashCode = FarmNa_Hash64(InpPtr, Seed1, Pack_I64_A2, Pack_I32_A2, Seed)
    CASE (3)
        HashCode = FarmNa_Hash64(InpPtr, Seed1, Pack_I64_A3, Pack_I32_A3, Seed)
    CASE (4)
        HashCode = FarmNa_Hash64(InpPtr, Seed1, Pack_I64_A4, Pack_I32_A4, Seed)
    CASE (5)
        HashCode = FarmNa_Hash64(InpPtr, Seed1, Pack_I64_A5, Pack_I32_A5, Seed)
    CASE (6)
        HashCode = FarmNa_Hash64(InpPtr, Seed1, Pack_I64_A6, Pack_I32_A6, Seed)
    CASE (7)
        HashCode = FarmNa_Hash64(InpPtr, Seed1, Pack_I64_A7, Pack_I32_A7, Seed)
    CASE (8)
        HashCode = FarmNa_Hash64_08(InpPtr, Seed1, Pack_I32_A6, Seed)
    CASE (9)
        HashCode = FarmNa_Hash64_09(InpPtr, Seed1, Seed)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION FarmNa_Hash64_Exp

!******************************************************************************

FUNCTION FarmNa_Hash64(Input, Seed1, PackLong, PackInteger, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the FarmNaHash64 hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN) :: Input(0:)    !! input bytes
    tLong,           INTENT(IN) :: Seed1        !! seed
    PROCEDURE(Pack_I64)         :: PackLong     !! procedure to convert a byte array to 64-bit integer
    PROCEDURE(Pack_I32)         :: PackInteger  !! procedure to convert a byte array to 32-bit integer
    tLong, OPTIONAL, INTENT(IN) :: Seed2        !! optional (additional) seed
    tLong                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset, IEnd, Last64
    tLong           :: X, Y, Z, Tmp, Seed, M
    tLong           :: A, B, C, V1, W1, Z1
    tLong           :: A1, B1, C1, V2, W2, Z2

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
    ELSE
        ! initialize
        Offset = 0
        Seed   = 81_kLong

        ! For strings over 64 bytes we loop.  Internal state consists of
        ! 56 bytes: v, w, x, y, and z.
        X = Seed
        !== Seed*k1 + 113 This overflows uint64 and is a compile error,
        ! so we expand the constant by hand
        Y = Seed*K1 + 113_kLong
        Z = ShiftMix(Y*K2 + 113_kLong)*K2
        V1 = 0_kLong
        V2 = 0_kLong
        W1 = 0_kLong
        W2 = 0_kLong
        X = X*K2 + PackLong(Input, 0)

        ! Set IEnd so that after the loop we have 1 to 64 bytes left to process.
        IEnd = SHIFTA((Length - 1), 6)*64
        Last64 = IEnd + IAND((Length - 1), 63) - 63

        DO
            X = RotateRight(X + Y + V1 + PackLong(Input, Offset + 8), 37)*K1
            Y = RotateRight(Y + V2 + PackLong(Input, Offset + 48), 42)*K1
            X = IEOR(X, W2)
            Y = Y + V1 + PackLong(Input, Offset + 40)
            Z = RotateRight(Z + W1, 33)*K1
            A = V2*K1
            B = X + W1
            Z1 = PackLong(Input, Offset + 24)
            A = A + PackLong(Input, Offset)
            B = RotateRight(B + A + Z1, 21)
            C = A
            A = A + PackLong(Input, Offset + 8)
            A = A + PackLong(Input, Offset + 16)
            B = B + RotateRight(A, 44)
            V1 = A + Z1
            V2 = B + C
            A1 = Z + W2
            B1 = Y + PackLong(Input, Offset + 16)
            Z2 = PackLong(Input, Offset + 32 + 24)
            A1 = A1 + PackLong(Input, Offset + 32)
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + PackLong(Input, Offset + 32 + 8)
            A1 = A1 + PackLong(Input, Offset + 32 + 16)
            B1 = B1 + RotateRight(A1, 44)
            W1 = A1 + Z2
            W2 = B1 + C1
            Tmp = Z
            Z = X
            X = Tmp
            ! update offset and check whether to terminate the loop
            Offset = Offset + 64
            IF (Offset == IEnd) EXIT
        END DO

        Offset = Last64

        M = K1 + SHIFTL(IAND(Z, ToLong(Z'FF')), 1)

        ! Make s point to the last 64 bytes of Input.
        W1 = W1 + ToLong(IAND((Length - 1), 63))
        V1 = V1 + W1
        W1 = W1 + V1
        X = RotateRight(X + Y + V1 + PackLong(Input, Offset + 8), 37)*M
        Y = RotateRight(Y + V2 + PackLong(Input, Offset + 48), 42)*M
        X = IEOR(X, W2*9_kLong)
        Y = Y + V1*9_kLong + PackLong(Input, Offset + 40)
        Z = RotateRight(Z + W1, 33)*M
        A = V2*M
        B = X + W1
        Z1 = PackLong(Input, Offset + 24)
        A = A + PackLong(Input, Offset)
        B = RotateRight(B + A + Z1, 21)
        C = A
        A = A + PackLong(Input, Offset + 8)
        A = A + PackLong(Input, Offset + 16)
        B = B + RotateRight(A, 44)
        V1 = A + Z1
        V2 = B + C
        A1 = Z + W2
        B1 = Y + PackLong(Input, Offset + 16)
        Z2 = PackLong(Input, Offset + 32 + 24)
        A1 = A1 + PackLong(Input, Offset + 32)
        B1 = RotateRight(B1 + A1 + Z2, 21)
        C1 = A1
        A1 = A1 + PackLong(Input, Offset + 32 + 8)
        A1 = A1 + PackLong(Input, Offset + 32 + 16)
        B1 = B1 + RotateRight(A1, 44)
        W1 = A1 + Z2
        W2 = B1 + C1
        Tmp = Z
        Z = X
        X = Tmp
        HashCode = HashLen16_3(HashLen16_3(V1, W1, M) + ShiftMix(Y)*K0 + Z, \
                               HashLen16_3(V2, W2, M) + X, M)
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

END FUNCTION FarmNa_Hash64

!******************************************************************************

FUNCTION FarmNa_Hash64_08(Input, Seed1, PackInteger, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the FarmNaHash hash algorithm by Google Inc.

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
    tLong           :: X, Y, Z, Tmp, Seed, M
    tLong           :: A, B, C, V1, W1, Z1
    tLong           :: A1, B1, C1, V2, W2, Z2
    TYPE(C_PTR)     :: CPTR

!** FLOW

#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 16) THEN
        ! FarmNaHash_Len0To16
        IF (Length >= 8) THEN
            BLOCK
                tLong       :: M, A, C, D
                tLong       :: First8Bytes, Last8Bytes
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
        ! FarmNaHash_Len17To32
        BLOCK
            tLong       :: M, A, B, C, D
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
    ELSEIF (Length <= 64) THEN
        ! FarmNaHash_Len33To64
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
    ELSE
        ! initialize
        Offset = 0
        Seed   = 81_kLong

        ! For strings over 64 bytes we loop.  Internal state consists of
        ! 56 bytes: v, w, x, y, and z.
        X = Seed
        !== Seed*k1 + 113 This overflows uint64 and is a compile error,
        ! so we expand the constant by hand
        Y = Seed*K1 + 113_kLong
        Z = ShiftMix(Y*K2 + 113_kLong)*K2
        V1 = 0_kLong
        V2 = 0_kLong
        W1 = 0_kLong
        W2 = 0_kLong
        CPTR = C_LOC(Input(0))
        CALL C_F_POINTER(CPTR, LongVal, SHAPE=[8])
        X = X*K2 + LongVal(1)

        ! Set IEnd so that after the loop we have 1 to 64 bytes left to process.
        IEnd = SHIFTA((Length - 1), 6)*64
        Last64 = IEnd + IAND((Length - 1), 63) - 63

        DO
            X = RotateRight(X + Y + V1 + LongVal(2), 37)*K1
            Y = RotateRight(Y + V2 + LongVal(7), 42)*K1
            X = IEOR(X, W2)
            Y = Y + V1 + LongVal(6)
            Z = RotateRight(Z + W1, 33)*K1
            A = V2*K1
            B = X + W1
            Z1 = LongVal(4)
            A = A + LongVal(1)
            B = RotateRight(B + A + Z1, 21)
            C = A
            A = A + LongVal(2)
            A = A + LongVal(3)
            B = B + RotateRight(A, 44)
            V1 = A + Z1
            V2 = B + C
            A1 = Z + W2
            B1 = Y + LongVal(3)
            Z2 = LongVal(8)
            A1 = A1 + LongVal(5)
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + LongVal(6)
            A1 = A1 + LongVal(7)
            B1 = B1 + RotateRight(A1, 44)
            W1 = A1 + Z2
            W2 = B1 + C1
            Tmp = Z
            Z = X
            X = Tmp
            ! update offset and check whether to terminate the loop
            Offset = Offset + 64
            IF (Offset == IEnd) EXIT
            CPTR = C_LOC(Input(Offset))
            CALL C_F_POINTER(CPTR, LongVal, SHAPE=[8])
        END DO

        Offset = Last64
        CPTR = C_LOC(Input(Offset))
        CALL C_F_POINTER(CPTR, LongVal, SHAPE=[8])

        M = K1 + SHIFTL(IAND(Z, ToLong(Z'FF')), 1)

        ! Make s point to the last 64 bytes of input.
        W1 = W1 + ToLong(IAND((Length - 1), 63))
        V1 = V1 + W1
        W1 = W1 + V1
        X = RotateRight(X + Y + V1 + LongVal(2), 37)*M
        Y = RotateRight(Y + V2 + LongVal(7), 42)*M
        X = IEOR(X, W2*9_kLong)
        Y = Y + V1*9_kLong + LongVal(6)
        Z = RotateRight(Z + W1, 33)*M
        A = V2*M
        B = X + W1
        Z1 = LongVal(4)
        A = A + LongVal(1)
        B = RotateRight(B + A + Z1, 21)
        C = A
        A = A + LongVal(2)
        A = A + LongVal(3)
        B = B + RotateRight(A, 44)
        V1 = A + Z1
        V2 = B + C
        A1 = Z + W2
        B1 = Y + LongVal(3)
        Z2 = LongVal(8)
        A1 = A1 + LongVal(5)
        B1 = RotateRight(B1 + A1 + Z2, 21)
        C1 = A1
        A1 = A1 + LongVal(6)
        A1 = A1 + LongVal(7)
        B1 = B1 + RotateRight(A1, 44)
        W1 = A1 + Z2
        W2 = B1 + C1
        Tmp = Z
        Z = X
        X = Tmp
        HashCode = HashLen16_3(HashLen16_3(V1, W1, M) + ShiftMix(Y)*K0 + Z, \
                                HashLen16_3(V2, W2, M) + X, M)
    END IF

    ! finalize with seed(s)
    IF (PRESENT(Seed2)) THEN
        HashCode = HashLen16_2(HashCode - Seed2, Seed1)
    ELSE
        HashCode = HashLen16_2(HashCode - K2, Seed1)
    END IF

    RETURN

#undef Pack_U32

END FUNCTION FarmNa_Hash64_08

!******************************************************************************

FUNCTION FarmNa_Hash64_09(Input, Seed1, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the FarmNaHash hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN) :: Input(0:)    ! input bytes
    tLong,           INTENT(IN) :: Seed1        ! required seed
    tLong, OPTIONAL, INTENT(IN) :: Seed2        ! optional (additional) seed
    tLong                       :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset, IEnd, Last64
    tLong           :: X, Y, Z, Tmp, Seed, M
    tLong           :: A, B, C, V1, W1, Z1
    tLong           :: A1, B1, C1, V2, W2, Z2
    tByte           :: ByteVal(0:63)
    tLong           :: LongVal(1:8)
    EQUIVALENCE(ByteVal, LongVal)
    tInteger        :: IntVal(1:2)
    EQUIVALENCE(ByteVal, IntVal)

!** FLOW

#define Pack_U32(Val,Index)     MaskI32(Val(Index))

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 16) THEN
        ! FarmNaHash_Len0To16
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
            ! set procedure pointer
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
        ! FarmNaHash_Len17To32
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
    ELSEIF (Length <= 64) THEN
        ! FarmNaHash_Len33To64
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
    ELSE
        ! initialize
        Offset = 0
        Seed   = 81_kLong

        ! For strings over 64 bytes we loop.  Internal state consists of
        ! 56 bytes: v, w, x, y, and z.
        X = Seed
        !== Seed*k1 + 113 This overflows uint64 and is a compile error,
        ! so we expand the constant by hand
        Y = Seed*K1 + 113_kLong
        Z = ShiftMix(Y*K2 + 113_kLong)*K2
        V1 = 0_kLong
        V2 = 0_kLong
        W1 = 0_kLong
        W2 = 0_kLong
        ByteVal(0:63) = Input(0:63)
        X = X*K2 + LongVal(1)

        ! Set IEnd so that after the loop we have 1 to 64 bytes left to process.
        IEnd = SHIFTA((Length - 1), 6)*64
        Last64 = IEnd + IAND((Length - 1), 63) - 63

        DO
            X = RotateRight(X + Y + V1 + LongVal(2), 37)*K1
            Y = RotateRight(Y + V2 + LongVal(7), 42)*K1
            X = IEOR(X, W2)
            Y = Y + V1 + LongVal(6)
            Z = RotateRight(Z + W1, 33)*K1
            A = V2*K1
            B = X + W1
            Z1 = LongVal(4)
            A = A + LongVal(1)
            B = RotateRight(B + A + Z1, 21)
            C = A
            A = A + LongVal(2)
            A = A + LongVal(3)
            B = B + RotateRight(A, 44)
            V1 = A + Z1
            V2 = B + C
            A1 = Z + W2
            B1 = Y + LongVal(3)
            Z2 = LongVal(8)
            A1 = A1 + LongVal(5)
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + LongVal(6)
            A1 = A1 + LongVal(7)
            B1 = B1 + RotateRight(A1, 44)
            W1 = A1 + Z2
            W2 = B1 + C1
            Tmp = Z
            Z = X
            X = Tmp
            ! update offset and check whether to terminate the loop
            Offset = Offset + 64
            IF (Offset == IEnd) EXIT
            ByteVal(0:63) = Input(Offset:Offset+63)
        END DO

        Offset = Last64
        ByteVal(0:63) = Input(Offset:Offset+63)

        M = K1 + SHIFTL(IAND(Z, ToLong(Z'FF')), 1)

        ! Make s point to the last 64 bytes of input.
        W1 = W1 + ToLong(IAND((Length - 1), 63))
        V1 = V1 + W1
        W1 = W1 + V1
        X = RotateRight(X + Y + V1 + LongVal(2), 37)*M
        Y = RotateRight(Y + V2 + LongVal(7), 42)*M
        X = IEOR(X, W2*9_kLong)
        Y = Y + V1*9_kLong + LongVal(6)
        Z = RotateRight(Z + W1, 33)*M
        A = V2*M
        B = X + W1
        Z1 = LongVal(4)
        A = A + LongVal(1)
        B = RotateRight(B + A + Z1, 21)
        C = A
        A = A + LongVal(2)
        A = A + LongVal(3)
        B = B + RotateRight(A, 44)
        V1 = A + Z1
        V2 = B + C
        A1 = Z + W2
        B1 = Y + LongVal(3)
        Z2 = LongVal(8)
        A1 = A1 + LongVal(5)
        B1 = RotateRight(B1 + A1 + Z2, 21)
        C1 = A1
        A1 = A1 + LongVal(6)
        A1 = A1 + LongVal(7)
        B1 = B1 + RotateRight(A1, 44)
        W1 = A1 + Z2
        W2 = B1 + C1
        Tmp = Z
        Z = X
        X = Tmp
        HashCode = HashLen16_3(HashLen16_3(V1, W1, M) + ShiftMix(Y)*K0 + Z, \
                                HashLen16_3(V2, W2, M) + X, M)
    END IF

    ! finalize with seed(s)
    IF (PRESENT(Seed2)) THEN
        HashCode = HashLen16_2(HashCode - Seed2, Seed1)
    ELSE
        HashCode = HashLen16_2(HashCode - K2, Seed1)
    END IF

    RETURN

#undef Pack_U32

END FUNCTION FarmNa_Hash64_09

!******************************************************************************

END SUBMODULE SubBase_FarmNaHash_Exp

!******************************************************************************
