
SUBMODULE (ModBase_ReferenceHash32) SubBase_CityNFarmHash32_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the City and Farm hash algorithm
!   for 32-bit-integer output by Google Inc. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    USE ModBase_SIntUtil,   ONLY: ReverseBytes
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define     Permute3(A, B, C) \
    SWAP_SCALAR(A, B); \
    SWAP_SCALAR(A, C);

!** MODULE PARAMETERS:
    tInteger, PARAMETER    :: C1 = ToInteger(Z'CC9E2D51')
    tInteger, PARAMETER    :: C2 = ToInteger(Z'1B873593')
    tInteger, PARAMETER    :: C3 = ToInteger(Z'E6546B64')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION CityHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the City hash algorithm by Google Inc.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tInteger, OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tInteger                            :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tInteger            :: Seed
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = City_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION CityHash_I32

!******************************************************************************

MODULE FUNCTION FarmMkHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FarmMk hash algorithm by Google Inc.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tInteger, OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tInteger                            :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tInteger            :: Seed
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = FarmMk_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION FarmMkHash_I32

!******************************************************************************

MODULE FUNCTION FarmMkHash_I32_Recur(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FarmMk hash algorithm by Google Inc.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tInteger, OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tInteger                            :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tInteger            :: Seed
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = FarmMk_Hash32_Recur(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION FarmMkHash_I32_Recur

!******************************************************************************

FUNCTION City_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the City hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: H, G, F, Temp
    tInteger    :: A0, A1, A2, A3, A4
    tIndex      :: Iters, Length, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 24) THEN
        IF (Length <= 4) THEN
            ! Hash32Len0to4
            BLOCK
                tInteger    :: A, B, C
                tIndex      :: I
                B = Seed
                C = 9
                DO I = 0, Length-1
                    B = B*C1 +  BC%Get_U8(Input, I)
                    C = IEOR(C, B)
                END DO
                A = Length
                HashCode = C
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        ELSEIF (Length <= 12) THEN
            ! Hash32Len5to12
            BLOCK
                tInteger    :: A, B, C
                A = ToInteger(Length) + Seed
                B = ToInteger(Length)*5
                C = 9
                HashCode = B
                A = A + BC%Pack_I32(Input, 0)
                B = B + BC%Pack_I32(Input, Length - 4)
                C = C + BC%Pack_I32(Input, IAND(SHIFTA(Length, 1), 4))
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL Mur(C, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        ELSE
            ! Hash32Len13to24
            BLOCK
                tInteger    :: A, B, C, D, E, F
                A = BC%Pack_I32(Input, SHIFTA(Length, 1) - 4)
                B = BC%Pack_I32(Input, 4)
                C = BC%Pack_I32(Input, Length - 8)
                D = BC%Pack_I32(Input, SHIFTA(Length, 1))
                E = BC%Pack_I32(Input, 0)
                F = BC%Pack_I32(Input, Length - 4)
                HashCode = Seed + ToInteger(Length)
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL Mur(C, HashCode)
                CALL Mur(D, HashCode)
                CALL Mur(E, HashCode)
                CALL Mur(F, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        END IF
        RETURN
    END IF

    ! Length > 24
    H = ToInteger(Length) + Seed
    G = C1*ToInteger(Length)
    F = G
    A0 = RotateRight(BC%Pack_I32(Input, Length - 4)*C1, 17)*C2
    A1 = RotateRight(BC%Pack_I32(Input, Length - 8)*C1, 17)*C2
    A2 = RotateRight(BC%Pack_I32(Input, Length - 16)*C1, 17)*C2
    A3 = RotateRight(BC%Pack_I32(Input, Length - 12)*C1, 17)*C2
    A4 = RotateRight(BC%Pack_I32(Input, Length - 20)*C1, 17)*C2
    CALL Mix(H, A0)
    CALL Mix(H, A2)
    CALL Mix(G, A1)
    CALL Mix(G, A3)
    F = F + A4
    F = RotateRight(F, 19)
    F = F*5 + C3
    Iters = (Length - 1) / 20
    Offset = 0
    DO
        A0 = RotateRight(BC%Pack_I32(Input, Offset)*C1, 17)*C2
        A1 = BC%Pack_I32(Input, Offset + 4)
        A2 = RotateRight(BC%Pack_I32(Input, Offset + 8)*C1, 17)*C2
        A3 = RotateRight(BC%Pack_I32(Input, Offset + 12)*C1, 17)*C2
        A4 = BC%Pack_I32(Input, Offset + 16)
        H = IEOR(H, A0)
        H = RotateRight(H, 18)
        H = H*5 + C3
        F = F + A1
        F = RotateRight(F, 19)
        F = F*C1
        G = G + A2
        G = RotateRight(G, 18)
        G = G*5 + C3
        H = IEOR(H, A3 + A1)
        H = RotateRight(H, 19)
        H = H*5 + C3
        G = IEOR(G, A4)
        G = ReverseBytes(G)*5
        H = H + A4*5
        H = ReverseBytes(H)
        F = F + A0
        Permute3(F, H, G)
        Offset = Offset + 20
        Iters = Iters - 1
        IF (Iters == 0) EXIT
    END DO
    G = RotateRight(G, 11)*C1
    G = RotateRight(G, 17)*C1
    F = RotateRight(F, 11)*C1
    F = RotateRight(F, 17)*C1
    H = RotateRight(H + G, 19)
    H = H*5 + C3
    H = RotateRight(H, 17)*C1
    H = RotateRight(H + F, 19)
    H = H*5 + C3
    HashCode = RotateRight(H, 17)*C1

    RETURN

END FUNCTION City_Hash32

!******************************************************************************

FUNCTION FarmMk_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the FarmMk hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: M, N
    tIndex      :: Length

!** FLOW

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 4) THEN
        HashCode = HashLen0To4(Input, Seed, BC)
        RETURN
    ELSEIF (Length <= 12) THEN
        HashCode = HashLen5To12(Input, Seed, BC)
        RETURN
    ELSEIF (Length <= 24)  THEN
        HashCode = HashLen13To24(Input, Seed*C1, BC)
        RETURN
    END IF

    HashCode = HashLen13To24(Input(0:23), IEOR(Seed, ToInteger(Length)), BC)
    M = HashWithoutSeed(Input(24:), BC)
    
    ! Hash32WithSeed
    N = M + Seed
    CALL Mur(N, HashCode)

    RETURN

CONTAINS

    FUNCTION HashLen0To4(Input, Seed, BC) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code for length less than or equal to 4.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger,             INTENT(IN)    :: Seed         ! seed
        CLASS(ByteConverter), INTENT(IN)    :: BC           ! byte converter
        tInteger                            :: HashCode     ! single hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, X
        tIndex      :: I
        tIndex      :: Length

    !** FLOW

        Length = SIZE(Input)

        ! Hash32Len0to4
        B = Seed
        C = 9
        DO I = 0, Length-1
            B = B*C1 +  BC%Get_I8(Input, I)
            C = IEOR(C, B)
        END DO
        A = ToInteger(Length)
        HashCode = C
        CALL Mur(A, HashCode)
        CALL Mur(B, HashCode)
        CALL FMix(HashCode)

        RETURN

    END FUNCTION HashLen0To4

    !**************************************************************************

    FUNCTION HashLen5To12(Input, Seed, BC) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code for length between 5 and 12.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger,             INTENT(IN)    :: Seed         ! seed
        CLASS(ByteConverter), INTENT(IN)    :: BC           ! byte converter
        tInteger                            :: HashCode     ! single hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, X
        tIndex      :: Length

    !** FLOW

        Length = SIZE(Input)

        ! Hash32Len5to12
        A = ToInteger(Length)
        B = ToInteger(Length)*5
        C = 9
        HashCode = B + Seed
        A = A + BC%Pack_I32(Input, 0)
        B = B + BC%Pack_I32(Input, Length - 4)
        C = C + BC%Pack_I32(Input, IAND(SHIFTA(Length, 1), 4))
        CALL Mur(A, HashCode)
        CALL Mur(B, HashCode)
        CALL Mur(C, HashCode)
        HashCode = IEOR(Seed, HashCode)
        CALL FMix(HashCode)

        RETURN

    END FUNCTION HashLen5To12

    !**************************************************************************

    FUNCTION HashLen13To24(Input, Seed, BC) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code for length between 13 and 24.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger,             INTENT(IN)    :: Seed         ! seed
        CLASS(ByteConverter), INTENT(IN)    :: BC           ! byte converter
        tInteger                            :: HashCode     ! single hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, D, E, F, X
        tIndex      :: Length

    !** FLOW

        Length = SIZE(Input)

        ! Hash32Len13to24
        A = BC%Pack_I32(Input, SHIFTA(Length, 1) - 4)
        B = BC%Pack_I32(Input, 4)
        C = BC%Pack_I32(Input, Length - 8)
        D = BC%Pack_I32(Input, SHIFTA(Length, 1))
        E = BC%Pack_I32(Input, 0)
        F = BC%Pack_I32(Input, Length - 4)
        HashCode = D*C1 + ToInteger(Length) + Seed
        A = RotateRight(A, 12) + F
        CALL MurPlus(C, HashCode, A)
        A = RotateRight(A, 3) + C
        CALL MurPlus(E, HashCode, A)
        A = RotateRight(A + F, 12) + D
        B = IEOR(B, Seed)
        CALL MurPlus(B, HashCode, A)
        CALL FMix(HashCode)

        RETURN

    END FUNCTION HashLen13To24

    !**************************************************************************

    FUNCTION HashWithoutSeed(Input, BC) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code without seed.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
        CLASS(ByteConverter), INTENT(IN)    :: BC           ! byte converter
        tInteger                            :: HashCode     ! single hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: G, F, T, X
        tInteger    :: A0, A1, A2, A3, A4
        tIndex      :: Iters, Length, Offset

    !** FLOW

        ! initialize
        Length = SIZE(Input)

        ! perform hashing
        IF (Length <= 4) THEN
            HashCode = HashLen0To4(Input, 0, BC)
            RETURN
        ELSEIF (Length <= 12) THEN
            HashCode = HashLen5To12(Input, 0, BC)
            RETURN
        ELSEIF (Length <= 24)  THEN
            HashCode = HashLen13To24(Input, 0, BC)
            RETURN
        END IF

        ! Length > 24 of Hash32
        HashCode = ToInteger(Length)
        G = C1*ToInteger(Length)
        F = G
        A0 = RotateRight(BC%Pack_I32(Input, Length - 4)*C1, 17)*C2
        A1 = RotateRight(BC%Pack_I32(Input, Length - 8)*C1, 17)*C2
        A2 = RotateRight(BC%Pack_I32(Input, Length - 16)*C1, 17)*C2
        A3 = RotateRight(BC%Pack_I32(Input, Length - 12)*C1, 17)*C2
        A4 = RotateRight(BC%Pack_I32(Input, Length - 20)*C1, 17)*C2
        CALL Mix(HashCode, A0)
        CALL Mix(HashCode, A2)
        CALL Mix(G, A1)
        CALL Mix(G, A3)
        F = F + A4
        F = RotateRight(F, 19) + 113
        Iters = (Length - 1) / 20
        Offset = 0
        DO
            A0 = BC%Pack_I32(Input, Offset)
            A1 = BC%Pack_I32(Input, Offset + 4)
            A2 = BC%Pack_I32(Input, Offset + 8)
            A3 = BC%Pack_I32(Input, Offset + 12)
            A4 = BC%Pack_I32(Input, Offset + 16)
            HashCode = HashCode + A0
            G = G + A1
            F = F + A2
            CALL MurPlus(A3, HashCode, A4)
            CALL MurPlus(A2, G, A0)
            T = A1 + A4*C1
            CALL MurPlus(T, F, A3)
            F = F + G
            G = G + F
            Offset = Offset + 20
            Iters = Iters - 1
            IF (Iters == 0) EXIT
        END DO
        G = RotateRight(G, 11)*C1
        G = RotateRight(G, 17)*C1
        F = RotateRight(F, 11)*C1
        F = RotateRight(F, 17)*C1
        HashCode = RotateRight(HashCode + G, 19)
        HashCode = HashCode*5 + C3
        HashCode = RotateRight(HashCode, 17)*C1
        HashCode = RotateRight(HashCode + F, 19)
        HashCode = HashCode*5 + C3
        HashCode = RotateRight(HashCode, 17)*C1

        RETURN

    END FUNCTION HashWithoutSeed

    !**************************************************************************

END FUNCTION FarmMk_Hash32

!******************************************************************************

RECURSIVE FUNCTION FarmMk_Hash32_Recur(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the FarmMk hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: H, G, F, T
    tInteger    :: A0, A1, A2, A3, A4
    tIndex      :: Iters, Length, Remaining, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 4) THEN
        ! Hash32Len0to4
        BLOCK
            tInteger    :: A, B, C
            tIndex      :: I
            B = Seed
            C = 9
            DO I = 0, Length-1
                B = B*C1 +  BC%Get_I8(Input, I)
                C = IEOR(C, B)
            END DO
            A = ToInteger(Length)
            HashCode = C
            CALL Mur(A, HashCode)
            CALL Mur(B, HashCode)
            CALL FMix(HashCode)
        END BLOCK
        RETURN
    ELSEIF (Length <= 12) THEN
        ! Hash32Len5to12
        BLOCK
            tInteger    :: A, B, C
            A = ToInteger(Length)
            B = ToInteger(Length)*5
            C = 9
            HashCode = B + Seed
            A = A + BC%Pack_I32(Input, 0)
            B = B + BC%Pack_I32(Input, Length - 4)
            C = C + BC%Pack_I32(Input, IAND(SHIFTA(Length, 1), 4))
            CALL Mur(A, HashCode)
            CALL Mur(B, HashCode)
            CALL Mur(C, HashCode)
            HashCode = IEOR(Seed, HashCode)
            CALL FMix(HashCode)
        END BLOCK
        RETURN
    END IF
    ! Hash32Len13to24
    BLOCK
        tInteger    :: A, B, C, D, E, F
        tInteger    :: Seed1
        tIndex      :: Len2
        IF (Length <= 24) THEN
            Seed1 = Seed*C1
            Len2  = Length
        ELSE
            Seed1 = IEOR(Seed, ToInteger(Length))
            Len2  = 24
        END IF
        A = BC%Pack_I32(Input, SHIFTA(Len2, 1) - 4)
        B = BC%Pack_I32(Input, 4)
        C = BC%Pack_I32(Input, Len2 - 8)
        D = BC%Pack_I32(Input, SHIFTA(Len2, 1))
        E = BC%Pack_I32(Input, 0)
        F = BC%Pack_I32(Input, Len2 - 4)
        HashCode = D*C1 + ToInteger(Len2) + Seed1
        A = RotateRight(A, 12) + F
        CALL MurPlus(C, HashCode, A)
        A = RotateRight(A, 3) + C
        CALL MurPlus(E, HashCode, A)
        A = RotateRight(A + F, 12) + D
        B = IEOR(B, Seed1)
        CALL MurPlus(B, HashCode, A)
        CALL FMix(HashCode)
    END BLOCK
    IF (Length <= 24) RETURN

    Remaining = Length - 24
    Offset = 24
    IF (Remaining <= 24) THEN
        ! Length <= 24 of Hash32
        H = FarmMk_Hash32_Recur(Input(Offset:), 0, BC)
        ! Hash32WithSeed
        G = H + Seed
        CALL Mur(G, HashCode)
        RETURN
    END IF

    ! Length > 24 of Hash32
    H = ToInteger(Remaining)
    G = C1*ToInteger(Remaining)
    F = G
    A0 = RotateRight(BC%Pack_I32(Input, Length - 4)*C1, 17)*C2
    A1 = RotateRight(BC%Pack_I32(Input, Length - 8)*C1, 17)*C2
    A2 = RotateRight(BC%Pack_I32(Input, Length - 16)*C1, 17)*C2
    A3 = RotateRight(BC%Pack_I32(Input, Length - 12)*C1, 17)*C2
    A4 = RotateRight(BC%Pack_I32(Input, Length - 20)*C1, 17)*C2
    CALL Mix(H, A0)
    CALL Mix(H, A2)
    CALL Mix(G, A1)
    CALL Mix(G, A3)
    F = F + A4
    F = RotateRight(F, 19) + 113
    Iters = (Remaining - 1) / 20
    DO
        A0 = BC%Pack_I32(Input, Offset)
        A1 = BC%Pack_I32(Input, Offset + 4)
        A2 = BC%Pack_I32(Input, Offset + 8)
        A3 = BC%Pack_I32(Input, Offset + 12)
        A4 = BC%Pack_I32(Input, Offset + 16)
        H = H + A0
        G = G + A1
        F = F + A2
        CALL MurPlus(A3, H, A4)
        CALL MurPlus(A2, G, A0)
        T = A1 + A4*C1
        CALL MurPlus(T, F, A3)
        F = F + G
        G = G + F
        Offset = Offset + 20
        Iters = Iters - 1
        IF (Iters == 0) EXIT
    END DO
    G = RotateRight(G, 11)*C1
    G = RotateRight(G, 17)*C1
    F = RotateRight(F, 11)*C1
    F = RotateRight(F, 17)*C1
    H = RotateRight(H + G, 19)
    H = H*5 + C3
    H = RotateRight(H, 17)*C1
    H = RotateRight(H + F, 19)
    H = H*5 + C3
    H = RotateRight(H, 17)*C1
    ! Hash32WithSeed
    G = H + Seed
    CALL Mur(G, HashCode)

    RETURN

END FUNCTION FarmMk_Hash32_Recur

!******************************************************************************

SUBROUTINE FMix(H)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    H = IEOR(H, SHIFTR(H, 16))
    H = H*ToInteger(Z'85EBCA6B')
    H = IEOR(H, SHIFTR(H, 13))
    H = H*ToInteger(Z'C2B2AE35')
    H = IEOR(H, SHIFTR(H, 16))

    RETURN

END SUBROUTINE FMix

!******************************************************************************

SUBROUTINE Mix(H, A)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H
    tInteger, INTENT(IN)    :: A

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    H = IEOR(H, A)
    H = RotateRight(H, 19)
    H = H*5 + C3

    RETURN

END SUBROUTINE Mix

!******************************************************************************

SUBROUTINE Mur(A, H)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication and mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: A
    tInteger, INTENT(INOUT) :: H

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: X

!** FLOW
    
    X = A
    X = X*C1
    X = RotateRight(X, 17)
    X = X*C2
    H = IEOR(H, X)
    H = RotateRight(H, 19)
    H = H*5 + C3

    RETURN

END SUBROUTINE Mur

!******************************************************************************

SUBROUTINE MurPlus(A, H, B)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication and mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: A
    tInteger, INTENT(INOUT) :: H
    tInteger, INTENT(IN)    :: B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    CALL Mur(A, H)
    H = H + B

    RETURN

END SUBROUTINE MurPlus

!******************************************************************************

END SUBMODULE SubBase_CityNFarmHash32_Ref

!******************************************************************************
