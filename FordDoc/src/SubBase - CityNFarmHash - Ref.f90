
SUBMODULE (ModBase_ReferenceHash64) SubBase_CityNFarmHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the City and Farm hash algorithms
!   for 64-bit-integer output by Google Inc. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE ModBase_SIntUtil,   ONLY: LongReverseBytes    => ReverseBytes
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define ShiftMix(V)             IEOR(V, SHIFTR(V, 47))
#define HashLen16_3(U,V,M)      ShiftMix(IEOR(V, ShiftMix(IEOR(U, V)*M))*M)*M
#define HashLen16_2(U,V)        HashLen16_3(U, V, K_MUL)
#define Mul(L)                  K2 + SHIFTL(ToLong(L), 1)
#define FarmUoH(X, Y, M, R)     RotateRight(IEOR(Y, ShiftMix(IEOR(X, Y)*M))*M, R)*M

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

MODULE FUNCTION CityHash_I64(Input, InpSize, StartHash, RemoveSign, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the CityHash hash algorithm by Google Inc.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed1, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = City_Hash64(InpPtr, Seed1, ByteConv, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION CityHash_I64

!******************************************************************************

MODULE FUNCTION FarmNaHash_I64(Input, InpSize, StartHash, RemoveSign, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FarmNaHash hash algorithm by Google Inc.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed1, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = FarmNa_Hash64(InpPtr, Seed1, ByteConv, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION FarmNaHash_I64

!******************************************************************************

MODULE FUNCTION FarmUoHash_I64(Input, InpSize, StartHash, RemoveSign, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FarmUoHash hash algorithm by Google Inc.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed1, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = FarmUo_Hash64(InpPtr, Seed1, ByteConv, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION FarmUoHash_I64

!******************************************************************************

FUNCTION City_Hash64(Input, Seed1, BC, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the CityHash64 hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed1        !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong, OPTIONAL,     INTENT(IN) :: Seed2        !! optional (additional) seed
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset
    tLong           :: X, Y, Z, Tmp
    tLong           :: VFirst, VSecond, WFirst, WSecond
    tLong           :: A, B, C, W1, X1, Y1, Z1
    tLong           :: A1, B1, C1, W2, X2, Y2, Z2
    tLong           :: A2, B2, C2, W3, X3, Y3, Z3
    tLong           :: A3, B3, C3, W4, X4, Y4, Z4

!** FLOW

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 16) THEN
        ! CityHash_Len0To16
        IF (Length >= 8) THEN
            BLOCK
                tLong   :: M, A, C, D
                tLong   :: First8Bytes, Last8Bytes
                First8Bytes = BC%Pack_I64(Input, 0)
                Last8Bytes  = BC%Pack_I64(Input, Length - 8)
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
                First4Bytes = BC%Pack_U32(Input, 0)
                Last4Bytes  = BC%Pack_U32(Input, Length - 4)
                ! Hash 4 To 7 Bytes
                M = Mul(Length)
                HashCode = HashLen16_3(ToLong(Length) + SHIFTL(First4Bytes, 3), Last4Bytes, M)
            END BLOCK
        ELSEIF (Length > 0) THEN
            BLOCK
                tInteger    :: FirstByte, MidOrLastByte, LastByte
                tInteger    :: Y, Z
                FirstByte     = BC%Get_U8(Input, 0)
                MidOrLastByte = BC%Get_U8(Input, SHIFTA(Length, 1))
                LastByte      = BC%Get_U8(Input, Length - 1)
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
            A = BC%Pack_I64(Input, 0)*K1
            B = BC%Pack_I64(Input, 8)
            C = BC%Pack_I64(Input, Length - 8)*M
            D = BC%Pack_I64(Input, Length - 16)*K2
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
            A = BC%Pack_I64(Input, 0)*K2
            B = BC%Pack_I64(Input, 8)
            C = BC%Pack_I64(Input, Length - 24)
            D = BC%Pack_I64(Input, Length - 32)
            E = BC%Pack_I64(Input, 16)*K2
            F = BC%Pack_I64(Input, 24)*9_kLong
            G = BC%Pack_I64(Input, Length - 8)
            H = BC%Pack_I64(Input, Length - 16)*M
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
        X = BC%Pack_I64(Input, Length - 40)
        Y = BC%Pack_I64(Input, Length - 16) + BC%Pack_I64(Input, Length - 56)
        Z = HashLen16_2(BC%Pack_I64(Input, Length - 48) + Length, \
                        BC%Pack_I64(Input, Length - 24))

        ! WeakHashLen32WithSeeds
        A3 = Length
        B3 = Z
        W4 = BC%Pack_I64(Input, Length - 64)
        X4 = BC%Pack_I64(Input, Length - 64 + 8)
        Y4 = BC%Pack_I64(Input, Length - 64 + 16)
        Z4 = BC%Pack_I64(Input, Length - 64 + 24)
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
        W3 = BC%Pack_I64(Input, Length - 32)
        X3 = BC%Pack_I64(Input, Length - 32 + 8)
        Y3 = BC%Pack_I64(Input, Length - 32 + 16)
        Z3 = BC%Pack_I64(Input, Length - 32 + 24)
        A2 = A2 + W3
        B2 = RotateRight(B2 + A2 + Z3, 21)
        C2 = A2
        A2 = A2 + X3 + Y3
        B2 = B2 + RotateRight(A2, 44)
        WFirst = A2 + Z3
        WSecond = B2 + C2

        X = X*K1 + BC%Pack_I64(Input, 0)

        Length = IAND(Length - 1, NOT(63))
        Offset = 0
        MainLoop: DO
            X = RotateRight(X + Y + VFirst + BC%Pack_I64(Input, Offset + 8), 37)*K1
            Y = RotateRight(Y + VSecond + BC%Pack_I64(Input, Offset + 48), 42)*K1
            X = IEOR(X, WSecond)
            Y = Y + VFirst + BC%Pack_I64(Input, Offset + 40)
            Z = RotateRight(Z + WFirst, 33)*K1

            ! WeakHashLen32WithSeeds
            A1 = VSecond*K1
            B1 = X + WFirst
            W2 = BC%Pack_I64(Input, Offset)
            X2 = BC%Pack_I64(Input, Offset + 8)
            Y2 = BC%Pack_I64(Input, Offset + 16)
            Z2 = BC%Pack_I64(Input, Offset + 24)
            A1 = A1 + W2
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + X2 + Y2
            B1 = B1 + RotateRight(A1, 44)
            VFirst = A1 + Z2
            VSecond = B1 + C1

            ! WeakHashLen32WithSeeds
            A = Z + WSecond
            B = Y + BC%Pack_I64(Input, Offset + 16)
            W1 = BC%Pack_I64(Input, Offset + 32)
            X1 = BC%Pack_I64(Input, Offset + 32 + 8)
            Y1 = BC%Pack_I64(Input, Offset + 32 + 16)
            Z1 = BC%Pack_I64(Input, Offset + 32 + 24)
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

END FUNCTION City_Hash64

!******************************************************************************

FUNCTION FarmNa_Hash64(Input, Seed1, BC, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the FarmNaHash64 hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed1        !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong, OPTIONAL,     INTENT(IN) :: Seed2        !! optional (additional) seed
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset, IEnd, Last64
    tLong           :: X, Y, Z, Tmp, Seed, M
    tLong           :: A, B, C, V1, W1, Z1
    tLong           :: A1, B1, C1, V2, W2, Z2

!** FLOW

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 16) THEN
        ! CityHash_Len0To16
        IF (Length >= 8) THEN
            BLOCK
                tLong   :: M, A, C, D
                tLong   :: First8Bytes, Last8Bytes
                First8Bytes = BC%Pack_I64(Input, 0)
                Last8Bytes  = BC%Pack_I64(Input, Length - 8)
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
                First4Bytes = BC%Pack_U32(Input, 0)
                Last4Bytes  = BC%Pack_U32(Input, Length - 4)
                ! Hash 4 To 7 Bytes
                M = Mul(Length)
                HashCode = HashLen16_3(ToLong(Length) + SHIFTL(First4Bytes, 3), Last4Bytes, M)
            END BLOCK
        ELSEIF (Length > 0) THEN
            BLOCK
                tInteger    :: FirstByte, MidOrLastByte, LastByte
                tInteger    :: Y, Z
                FirstByte     = BC%Get_U8(Input, 0)
                MidOrLastByte = BC%Get_U8(Input, SHIFTA(Length, 1))
                LastByte      = BC%Get_U8(Input, Length - 1)
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
            A = BC%Pack_I64(Input, 0)*K1
            B = BC%Pack_I64(Input, 8)
            C = BC%Pack_I64(Input, Length - 8)*M
            D = BC%Pack_I64(Input, Length - 16)*K2
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
            A = BC%Pack_I64(Input, 0)*K2
            B = BC%Pack_I64(Input, 8)
            C = BC%Pack_I64(Input, Length - 8)*M
            D = BC%Pack_I64(Input, Length - 16)*K2
            Y = RotateRight(A + B, 43) + RotateRight(C, 30) + D
            Z = HashLen16_3(Y, A + RotateRight(B + K2, 18) + C, M)
            E = BC%Pack_I64(Input, 16)*M
            F = BC%Pack_I64(Input, 24)
            G = (Y + BC%Pack_I64(Input, Length - 32))*M
            H = (Z + BC%Pack_I64(Input, Length - 24))*M
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
        X = X*K2 + BC%Pack_I64(Input, 0)

        ! Set IEnd so that after the loop we have 1 to 64 bytes left to process.
        IEnd = SHIFTA((Length - 1), 6)*64
        Last64 = IEnd + IAND((Length - 1), 63) - 63

        DO
            X = RotateRight(X + Y + V1 + BC%Pack_I64(Input, Offset + 8), 37)*K1
            Y = RotateRight(Y + V2 + BC%Pack_I64(Input, Offset + 48), 42)*K1
            X = IEOR(X, W2)
            Y = Y + V1 + BC%Pack_I64(Input, Offset + 40)
            Z = RotateRight(Z + W1, 33)*K1
            A = V2*K1
            B = X + W1
            Z1 = BC%Pack_I64(Input, Offset + 24)
            A = A + BC%Pack_I64(Input, Offset)
            B = RotateRight(B + A + Z1, 21)
            C = A
            A = A + BC%Pack_I64(Input, Offset + 8)
            A = A + BC%Pack_I64(Input, Offset + 16)
            B = B + RotateRight(A, 44)
            V1 = A + Z1
            V2 = B + C
            A1 = Z + W2
            B1 = Y + BC%Pack_I64(Input, Offset + 16)
            Z2 = BC%Pack_I64(Input, Offset + 32 + 24)
            A1 = A1 + BC%Pack_I64(Input, Offset + 32)
            B1 = RotateRight(B1 + A1 + Z2, 21)
            C1 = A1
            A1 = A1 + BC%Pack_I64(Input, Offset + 32 + 8)
            A1 = A1 + BC%Pack_I64(Input, Offset + 32 + 16)
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
        X = RotateRight(X + Y + V1 + BC%Pack_I64(Input, Offset + 8), 37)*M
        Y = RotateRight(Y + V2 + BC%Pack_I64(Input, Offset + 48), 42)*M
        X = IEOR(X, W2*9_kLong)
        Y = Y + V1*9_kLong + BC%Pack_I64(Input, Offset + 40)
        Z = RotateRight(Z + W1, 33)*M
        A = V2*M
        B = X + W1
        Z1 = BC%Pack_I64(Input, Offset + 24)
        A = A + BC%Pack_I64(Input, Offset)
        B = RotateRight(B + A + Z1, 21)
        C = A
        A = A + BC%Pack_I64(Input, Offset + 8)
        A = A + BC%Pack_I64(Input, Offset + 16)
        B = B + RotateRight(A, 44)
        V1 = A + Z1
        V2 = B + C
        A1 = Z + W2
        B1 = Y + BC%Pack_I64(Input, Offset + 16)
        Z2 = BC%Pack_I64(Input, Offset + 32 + 24)
        A1 = A1 + BC%Pack_I64(Input, Offset + 32)
        B1 = RotateRight(B1 + A1 + Z2, 21)
        C1 = A1
        A1 = A1 + BC%Pack_I64(Input, Offset + 32 + 8)
        A1 = A1 + BC%Pack_I64(Input, Offset + 32 + 16)
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

END FUNCTION FarmNa_Hash64

!******************************************************************************

FUNCTION FarmUo_Hash64(Input, Seed1, BC, Seed2) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the FarmUoHash64 hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed1        !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong, OPTIONAL,     INTENT(IN) :: Seed2        !! optional (additional) seed
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset, IEnd, Last64
    tLong           :: Seed0, M, X, Y, Z, U, V0, V1, W0, W1
    tLong           :: A0, A1, A2, A3, A4, A5, A6, A7, Tmp
    tLong           :: A, B, C, B1, C1, Z1, Z2

!** FLOW

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
                    First8Bytes = BC%Pack_I64(Input, 0)
                    Last8Bytes  = BC%Pack_I64(Input, Length - 8)
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
                    First4Bytes = BC%Pack_U32(Input, 0)
                    Last4Bytes  = BC%Pack_U32(Input, Length - 4)
                    ! Hash 4 To 7 Bytes
                    M = Mul(Length)
                    HashCode = HashLen16_3(ToLong(Length) + SHIFTL(First4Bytes, 3), Last4Bytes, M)
                END BLOCK
            ELSEIF (Length > 0) THEN
                BLOCK
                    tInteger    :: FirstByte, MidOrLastByte, LastByte
                    tInteger    :: Y, Z
                    FirstByte     = BC%Get_U8(Input, 0)
                    MidOrLastByte = BC%Get_U8(Input, SHIFTA(Length, 1))
                    LastByte      = BC%Get_U8(Input, Length - 1)
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
                A = BC%Pack_I64(Input, 0)*K1
                B = BC%Pack_I64(Input, 8)
                C = BC%Pack_I64(Input, Length - 8)*M
                D = BC%Pack_I64(Input, Length - 16)*K2
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
                A = BC%Pack_I64(Input, 0)*K2
                B = BC%Pack_I64(Input, 8)
                C = BC%Pack_I64(Input, Length - 8)*M
                D = BC%Pack_I64(Input, Length - 16)*K2
                Y = RotateRight(A + B, 43) + RotateRight(C, 30) + D
                Z = HashLen16_3(Y, A + RotateRight(B + K2, 18) + C, M)
                E = BC%Pack_I64(Input, 16)*M
                F = BC%Pack_I64(Input, 24)
                G = (Y + BC%Pack_I64(Input, Length - 32))*M
                H = (Z + BC%Pack_I64(Input, Length - 24))*M
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
        A0 = BC%Pack_I64(Input, Offset)
        A1 = BC%Pack_I64(Input, Offset + 8)
        A2 = BC%Pack_I64(Input, Offset + 16)
        A3 = BC%Pack_I64(Input, Offset + 24)
        A4 = BC%Pack_I64(Input, Offset + 32)
        A5 = BC%Pack_I64(Input, Offset + 40)
        A6 = BC%Pack_I64(Input, Offset + 48)
        A7 = BC%Pack_I64(Input, Offset + 56)
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
    X = RotateRight(Y - X + V0 + BC%Pack_I64(Input, Offset + 8), 37)*M
    Y = RotateRight(IEOR(IEOR(Y, V1), BC%Pack_I64(Input, Offset + 48)), 42)*M
    X = IEOR(X, W1*9_kLong)
    Y = Y + V0 + BC%Pack_I64(Input, Offset + 40)
    Z = RotateRight(Z + W0, 33)*M

    A = V1*M
    B = X + W0
    Z1 = BC%Pack_I64(Input, Offset + 24)
    A = A + BC%Pack_I64(Input, Offset)
    B = RotateRight(B + A + Z1, 21)
    C = A
    A = A + BC%Pack_I64(Input, Offset + 8)
    A = A + BC%Pack_I64(Input, Offset + 16)
    B = B + RotateRight(A, 44)
    V0 =  A + Z1
    V1 = B + C

    A1 = Z + W1
    B1 = Y + BC%Pack_I64(Input, Offset + 16)
    Z2 = BC%Pack_I64(Input, Offset + 32 + 24)
    A1 = A1 + BC%Pack_I64(Input, Offset + 32)
    B1 = RotateRight(B1 + A1 + Z2, 21)
    C1 = A1
    A1 = A1 + BC%Pack_I64(Input, Offset + 32 + 8)
    A1 = A1 + BC%Pack_I64(Input, Offset + 32 + 16)
    B1 = B1 + RotateRight(A1, 44)
    W0 = A1 + Z2
    W1 = B1 + C1

    HashCode = FarmUoH(HashLen16_3(V0 + X, IEOR(W0, Y), M) + Z - U,     \
                       IEOR(FarmUoH(V1 + Y, W1 + Z, K2, 30_kInteger), X), K2, 31_kInteger)

    RETURN

END FUNCTION FarmUo_Hash64

!******************************************************************************

END SUBMODULE SubBase_CityNFarmHash_Ref

!******************************************************************************
