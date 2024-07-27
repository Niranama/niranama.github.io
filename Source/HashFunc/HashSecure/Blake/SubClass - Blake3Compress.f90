
SUBMODULE (Class_Blake3 : SubClass_Blake3Private) SubClass_Blake3Compress

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains routines relating to *compress* operations of the
!   BLAKE3 message-digest algorithm. <br>

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     MAX_SIMD_DEGREE         16
#define     MAX_SIMD_DEGREE_OR_2    16
#define     BLAKE3_SIMD_DEGREE      1

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: SCHEDULE(0:15, 0:6) = RESHAPE([    &
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, &
        2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8, &
        3, 4, 10, 12, 13, 2, 7, 14, 6, 5, 9, 0, 11, 15, 8, 1, &
        10, 7, 12, 9, 14, 3, 13, 15, 4, 0, 11, 2, 5, 8, 1, 6, &
        12, 13, 9, 11, 15, 10, 14, 8, 7, 2, 5, 3, 0, 1, 6, 4, &
        9, 14, 11, 5, 8, 12, 15, 1, 13, 3, 0, 10, 2, 6, 4, 7, &
        11, 15, 5, 0, 1, 9, 8, 6, 14, 10, 2, 12, 3, 4, 7, 13], [16, 7])
    !---------------------------------------------------------------------

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Compress_Pre(State, ChainVal, MBlock, BlockLen, Counter, Flags)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform data compression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: State(0:15)                  ! state
    tInteger, INTENT(IN)    :: ChainVal(0:7)                ! chaining values
    tByte,    INTENT(IN)    :: MBlock(0:BLAKE3_BLOCK_LEN-1) ! message blocks
    tIndex,   INTENT(IN)    :: BlockLen                     ! block length
    tLong,    INTENT(IN)    :: Counter                      ! counter
    tByte,    INTENT(IN)    :: Flags                        ! flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: BlockWords(0:15)

! FLOW

    ! get input blocks
    CALL BytePackLE(MBlock, 0_kIndex, BlockWords)

    ! initialize the state values
    State(0:7)  = ChainVal(0:7)
    State(8:11) = IV(0:3)
    State(12)   = ToInteger(IAND(Counter, ToLong(Z'00000000FFFFFFFF')))
    State(13)   = ToInteger(IAND(SHIFTR(Counter, 32), ToLong(Z'00000000FFFFFFFF')))
    State(14)   = ToInteger(BlockLen)
    State(15)   = ToInteger(Flags)

    ! perform 7 rounds of core compression operations
    CALL RoundFn(State, BlockWords, 0)
    CALL RoundFn(State, BlockWords, 1)
    CALL RoundFn(State, BlockWords, 2)
    CALL RoundFn(State, BlockWords, 3)
    CALL RoundFn(State, BlockWords, 4)
    CALL RoundFn(State, BlockWords, 5)
    CALL RoundFn(State, BlockWords, 6)

    RETURN

CONTAINS

    SUBROUTINE RoundFn(State, Msg, Round)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: State(0:15)  ! state values
        tInteger, INTENT(IN)    :: Msg(0:15)    ! the message block
        tIndex,   INTENT(IN)    :: Round        ! the current round

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        ! Mix the columns
        CALL G(State, 0, 4,  8, 12, Msg(SCHEDULE( 0, Round)), Msg(SCHEDULE( 1, Round)))
        CALL G(State, 1, 5,  9, 13, Msg(SCHEDULE( 2, Round)), Msg(SCHEDULE( 3, Round)))
        CALL G(State, 2, 6, 10, 14, Msg(SCHEDULE( 4, Round)), Msg(SCHEDULE( 5, Round)))
        CALL G(State, 3, 7, 11, 15, Msg(SCHEDULE( 6, Round)), Msg(SCHEDULE( 7, Round)))

        ! Mix the rows
        CALL G(State, 0, 5, 10, 15, Msg(SCHEDULE( 8, Round)), Msg(SCHEDULE( 9, Round)))
        CALL G(State, 1, 6, 11, 12, Msg(SCHEDULE(10, Round)), Msg(SCHEDULE(11, Round)))
        CALL G(State, 2, 7,  8, 13, Msg(SCHEDULE(12, Round)), Msg(SCHEDULE(13, Round)))
        CALL G(State, 3, 4,  9, 14, Msg(SCHEDULE(14, Round)), Msg(SCHEDULE(15, Round)))

        RETURN

    END SUBROUTINE RoundFn

!**************************************************************************

    SUBROUTINE G(State, A, B, C, D, X, Y)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: State(0:15)
        tInteger, INTENT(IN)    :: A, B, C, D, X, Y

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        State(A) = State(A) + State(B) + X
        State(D) = RotateRight(IEOR(State(D), State(A)), 16)
        State(C) = State(C) + State(D)
        State(B) = RotateRight(IEOR(State(B), State(C)), 12)
        State(A) = State(A) + State(B) + Y
        State(D) = RotateRight(IEOR(State(D), State(A)), 8)
        State(C) = State(C) + State(D)
        State(B) = RotateRight(IEOR(State(B), State(C)), 7)

        RETURN

    END SUBROUTINE G

!**************************************************************************

END SUBROUTINE Compress_Pre

!******************************************************************************

MODULE SUBROUTINE Compress_Inplace(ChainVal, MBlock, BlockLen, Counter, Flags)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform data compression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: ChainVal(0:7)                ! chaining values
    tByte,    INTENT(IN)    :: MBlock(0:BLAKE3_BLOCK_LEN-1) ! message blocks
    tIndex,   INTENT(IN)    :: BlockLen                     ! block length
    tLong,    INTENT(IN)    :: Counter                      ! counter
    tByte,    INTENT(IN)    :: Flags                        ! flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: State(0:15)      ! state values
    tInteger        :: I

! FLOW

    CALL Compress_Pre(State, ChainVal, MBlock, BlockLen, Counter, Flags)

    ! adjust state values after compression
    DO I = 0, 7
        ChainVal(I)   = IEOR(State(I), State(I+8))
    END DO

    RETURN

END SUBROUTINE Compress_Inplace

!******************************************************************************

MODULE SUBROUTINE Compress_XOF(ChainVal, MBlock, BlockLen, Counter, Flags, OutBuf)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform data compression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: ChainVal(0:7)                ! chaining values
    tByte,    INTENT(IN)    :: MBlock(0:BLAKE3_BLOCK_LEN-1) ! message blocks
    tIndex,   INTENT(IN)    :: BlockLen                     ! block length
    tLong,    INTENT(IN)    :: Counter                      ! counter
    tByte,    INTENT(IN)    :: Flags                        ! flag
    tByte,    INTENT(OUT)   :: OutBuf(0:63)                 ! output buffer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: State(0:15)      ! state values
    tIndex          :: I

! FLOW

    CALL Compress_Pre(State, ChainVal, MBlock, BlockLen, Counter, Flags)

    ! adjust state values after compression
    DO I = 0, 7
        State(I)   = IEOR(State(I), State(I+8))
        State(I+8) = IEOR(State(I+8), ChainVal(I))
    END DO

    ! get output buffer
    DO I = 0, 15
        CALL ByteUnpackLE(State(I), OutBuf, I*4)
    END DO

    RETURN

END SUBROUTINE Compress_XOF

!******************************************************************************

SUBROUTINE Hash_One(Input, Blocks, Key, Counter, Flags, Flags_Start, Flags_End, Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To hash one chunk

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)
    tIndex,   INTENT(IN)    :: Blocks
    tInteger, INTENT(IN)    :: Key(0:7)
    tLong,    INTENT(IN)    :: Counter
    tByte,    INTENT(IN)    :: Flags
    tByte,    INTENT(IN)    :: Flags_Start
    tByte,    INTENT(IN)    :: Flags_End
    tByte,    INTENT(OUT)   :: Output(0:BLAKE3_OUT_LEN-1)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: CV(0:7)
    tByte       :: BlockFlags
    tIndex      :: NBlocks, CurPos, I

! FLOW

    CV(0:7) = Key(0:7)
    BlockFlags = IOR(Flags, Flags_Start)
    NBlocks = Blocks
    CurPos  = 0_kIndex

    DO WHILE (NBlocks > 0_kIndex)
        IF (NBlocks == 1_kIndex) BlockFlags = IOR(BlockFlags, Flags_End)
        CALL Compress_Inplace(CV, Input(CurPos:), BLAKE3_BLOCK_LEN, Counter, BlockFlags)
        CurPos = CurPos + BLAKE3_BLOCK_LEN
        NBlocks = NBlocks - 1_kIndex
        BlockFlags = Flags
    END DO

    DO I = 0, 7
        CALL ByteUnpackLE(CV(I), Output, I*4)
    END DO

    RETURN

END SUBROUTINE Hash_One

!******************************************************************************

SUBROUTINE Hash_Many(Input, NumInput, Blocks, Key, Counter, IncrementCounter, &
                     Flags, Flags_Start, Flags_End, Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To hash many chunks

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: NumInput
    tByte,    INTENT(IN)    :: Input(0:,0:)
    tIndex,   INTENT(IN)    :: Blocks
    tInteger, INTENT(IN)    :: Key(0:7)
    tLong,    INTENT(IN)    :: Counter
    tLogical, INTENT(IN)    :: IncrementCounter
    tByte,    INTENT(IN)    :: Flags
    tByte,    INTENT(IN)    :: Flags_Start
    tByte,    INTENT(IN)    :: Flags_End
    tByte,    INTENT(OUT)   :: Output(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NInp, InpPos, OutPos
    tLong       :: NCount

! FLOW

    NCount = Counter
    NInp = NumInput
    InpPos = 0_kIndex
    OutPos = 0_kIndex

    DO WHILE (NInp > 0_kIndex)
        CALL Hash_One(Input(:,InpPos), Blocks, Key, NCount, Flags, &
                      Flags_Start, Flags_End, Output(OutPos:))
        IF (IncrementCounter) NCount = NCount + 1
        InpPos = InpPos + 1
        NInp   = NInp - 1
        OutPos = OutPos + BLAKE3_OUT_LEN
    END DO

    RETURN

END SUBROUTINE Hash_Many

!******************************************************************************

FUNCTION Compress_Chunks_Parallel(Input, InpLen, Key, ChunkCounter, Flags, Output) &
                           RESULT(ChunkArrayLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! Use SIMD parallelism to hash up to MAX_SIMD_DEGREE chunks at the same time
    ! on a single thread. Write out the chunk chaining values and return the
    ! number of chunks hashed. These chunks are never the root and never empty
    ! those cases use a different codepath.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)
    tIndex,   INTENT(IN)    :: InpLen
    tInteger, INTENT(IN)    :: Key(0:7)
    tLong,    INTENT(IN)    :: ChunkCounter
    tByte,    INTENT(IN)    :: Flags
    tByte,    INTENT(OUT)   :: Output(0:)
    tIndex                  :: ChunkArrayLen

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER       :: NBlock = ToIndex(BLAKE3_CHUNK_LEN / BLAKE3_BLOCK_LEN)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: ChunkArray(0:BLAKE3_CHUNK_LEN-1,0:MAX_SIMD_DEGREE-1)
    tIndex      :: InpPos

! FLOW

    ! gather the input into chunk array
    InpPos = 0_kIndex
    ChunkArrayLen = 0_kIndex
    DO WHILE (InpLen - InpPos >= BLAKE3_CHUNK_LEN)
        ChunkArray(:,ChunkArrayLen) = Input(InpPos:)
        InpPos = InpPos + BLAKE3_CHUNK_LEN
        ChunkArrayLen = ChunkArrayLen + 1
    END DO

    ! hash the chunk array
    CALL Hash_Many(ChunkArray, ChunkArrayLen, NBlock, Key, ChunkCounter, TrueVal, &
                   Flags, CHUNK_START, CHUNK_END, Output)

    ! hash the remaining partial chunk, if there is one. Note that the empty
    ! chunk (meaning the empty message) is a different codepath.
    IF (InpLen > InpPos) THEN
        BLOCK
            tLong               :: Counter
            TYPE(ChunkState)    :: Chunk
            TYPE(OutNode)       :: Node
            ! execution
            Counter = ChunkCounter + ChunkArrayLen
            CALL ChunkState_Init(Chunk, Key, Flags)
            Chunk%Counter = Counter
            CALL ChunkState_Update(Chunk, Input(InpPos:), InpLen - InpPos)
            Node = ChunkState_OutNode(Chunk)
            CALL OutNode_ChainingValue(Node, Output(ChunkArrayLen*BLAKE3_OUT_LEN:))
            ChunkArrayLen = ChunkArrayLen + 1
        END BLOCK
    END IF

    RETURN

END FUNCTION Compress_Chunks_Parallel

!******************************************************************************

FUNCTION Compress_Parent_Parallel(ChildCV, NumCV, Key, Flags, Output) RESULT(ParentArrayLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! Use SIMD parallelism to hash up to MAX_SIMD_DEGREE parents at the same time
    ! on a single thread. Write out the parent chaining values and return the
    ! number of parents hashed. (If there's an odd input chaining value left over,
    ! return it as an additional output.) These parents are never the root and
    ! never empty those cases use a different codepath.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ChildCV(0:)
    tIndex,   INTENT(IN)    :: NumCV
    tInteger, INTENT(IN)    :: Key(0:7)
    tByte,    INTENT(IN)    :: Flags
    tByte,    INTENT(OUT)   :: Output(0:)
    tIndex                  :: ParentArrayLen

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: ParentArray(0:2*BLAKE3_OUT_LEN-1,0:MAX_SIMD_DEGREE_OR_2-1)
    tIndex      :: InpPos

! FLOW

    ! gather the input into parent array
    InpPos = 0_kIndex
    ParentArrayLen = 0_kIndex
    DO WHILE (NumCV - (2_kIndex*ParentArrayLen) >= 2_kIndex)
        ParentArray(:,ParentArrayLen) = ChildCV(InpPos:)
        InpPos = InpPos + 2*BLAKE3_OUT_LEN
        ParentArrayLen = ParentArrayLen + 1
    END DO

    ! hash the parent array where counter, start and end flags are zero
    CALL Hash_Many(ParentArray, ParentArrayLen, 1, Key, 0_kLong, FalseVal, &
                   IOR(Flags, PARENT), 0_kByte, 0_kByte, Output)

    ! If there's an odd child left over, it becomes an output.
    IF (NumCV > 2_kIndex*ParentArrayLen) THEN
        BLOCK
            tIndex      :: OutStart, OutEnd, InpEnd
            ! execution
            OutStart = ParentArrayLen*BLAKE3_OUT_LEN
            OutEnd   = OutStart + BLAKE3_OUT_LEN - 1
            InpEnd   = InpPos + BLAKE3_OUT_LEN - 1
            Output(OutStart:OutEnd) = ChildCV(InpPos:InpEnd)
            ParentArrayLen = ParentArrayLen + 1
        END BLOCK
    END IF

    RETURN

END FUNCTION Compress_Parent_Parallel

!******************************************************************************

RECURSIVE FUNCTION Compress_Subtree_Wide(Input, InpLen, Key, ChunkCounter, Flags, Output) &
                                  RESULT(ArrayLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! The wide helper function returns (writes out) an array of chaining values
    ! and returns the length of that array. The number of chaining values returned
    ! is the dyanmically detected SIMD degree, at most MAX_SIMD_DEGREE. Or fewer,
    ! if the input is shorter than that many chunks. The reason for maintaining a
    ! wide array of chaining values going back up the tree, is to allow the
    ! implementation to hash as many parents in parallel as possible.
    !
    ! As a special case when the SIMD degree is 1, this function will still return
    ! at least 2 outputs. This guarantees that this function doesn't perform the
    ! root compression. (If it did, it would use the wrong flags, and also we
    ! wouldn't be able to implement exendable output.) Note that this function is
    ! not used when the whole input is only 1 chunk long that's a different
    ! codepath.
    !
    ! Why not just have the caller split the input on the first update(), instead
    ! of implementing this special rule? Because we don't want to limit SIMD or
    ! multi-threading parallelism for that update().

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET, INTENT(IN)   :: Input(0:)
    tIndex,        INTENT(IN)   :: InpLen
    tInteger,      INTENT(IN)   :: Key(0:7)
    tLong,         INTENT(IN)   :: ChunkCounter
    tByte,         INTENT(IN)   :: Flags
    tByte,         INTENT(OUT)  :: Output(0:)
    tIndex                      :: ArrayLen

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:

! FLOW

    ! Note that the single chunk case does *not* bump the SIMD degree up to 2
    ! when it is 1. If this implementation adds multi-threading in the future,
    ! this gives us the option of multi-threading even the 2-chunk case, which
    ! can help performance on smaller platforms.
    IF (InpLen <= BLAKE3_SIMD_DEGREE*BLAKE3_CHUNK_LEN) THEN
        ArrayLen = Compress_Chunks_Parallel(Input, InpLen, Key, ChunkCounter, Flags, Output)
        RETURN
    END IF

BLOCK
    tByte, POINTER  :: LeftInput(:)
    tByte, POINTER  :: RightInput(:)
    tByte, POINTER  :: LeftCV(:)
    tByte, POINTER  :: RightCV(:)
    tByte, TARGET   :: CVArray(0:2*MAX_SIMD_DEGREE_OR_2*BLAKE3_OUT_LEN-1)
    tIndex          :: LeftInpLen, RightInpLen, Degree, LeftN, RightN, NumCV
    tLong           :: RightChunkCounter
    ! With more than simd_degree chunks, we need to recurse. Start by dividing
    ! the input into left and right subtrees. (Note that this is only optimal
    ! as long as the SIMD degree is a power of 2. If we ever get a SIMD degree
    ! of 3 or something, we'll need a more complicated strategy.)
    LeftInpLen  = LeftLen(InpLen)
    RightInpLen = InpLen - LeftInpLen
    LeftInput  => Input(0:LeftInpLen-1)
    RightInput => Input(LeftInpLen:)
    RightChunkCounter = ChunkCounter + (LeftInpLen / BLAKE3_CHUNK_LEN)

    ! Make space for the child outputs. Here we use MAX_SIMD_DEGREE_OR_2 to
    ! account for the special case of returning 2 outputs when the SIMD degree
    ! is 1.
    Degree = BLAKE3_SIMD_DEGREE
    IF ((LeftInpLen > BLAKE3_CHUNK_LEN).AND.(Degree == 1)) THEN
        ! The special case: We always use a degree of at least two, to make
        ! sure there are two outputs. Except, as noted above, at the chunk
        ! level, where we allow degree=1. (Note that the 1-chunk-input case is
        ! a different codepath.)
        Degree = 2
    END IF
    LeftCV  => CVArray(0:Degree*BLAKE3_OUT_LEN-1)
    RightCV => CVArray(Degree*BLAKE3_OUT_LEN:)

    ! Recurse! If this implementation adds multi-threading support in the
    ! future, this is where it will go.
    LeftN  = Compress_Subtree_Wide(LeftInput, LeftInpLen, Key, ChunkCounter, Flags, LeftCV)
    RightN = Compress_Subtree_Wide(RightInput, RightInpLen, Key, RightChunkCounter, Flags, RightCV)

    IF (LeftN == 1_kIndex) THEN
        ! The special case again. If simd_degree=1, then we'll have left_n=1 and
        ! right_n=1. Rather than compressing them into a single output, return
        ! them directly, to make sure we always have at least two outputs.
        Output(0:2*BLAKE3_OUT_LEN-1) = CVArray(0:2*BLAKE3_OUT_LEN-1)
        ArrayLen = 2_kIndex
    ELSE
        ! Otherwise, do one layer of parent node compression.
        NumCV = LeftN + RightN
        ArrayLen = Compress_Parent_Parallel(CVArray, NumCV, Key, Flags, Output)
    END IF

    NULLIFY(LeftInput, RightInput, LeftCV, RightCV)
END BLOCK

    RETURN

END FUNCTION Compress_Subtree_Wide

!******************************************************************************

MODULE SUBROUTINE Compress_Subtree2ParentNode(Input, InpLen, Key, ChunkCounter, Flags, Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! Hash a subtree with compress_subtree_wide(), and then condense the resulting
    ! list of chaining values down to a single parent node. Don't compress that
    ! last parent node, however. Instead, return its message bytes (the
    ! concatenated chaining values of its children). This is necessary when the
    ! first call to update() supplies a complete subtree, because the topmost
    ! parent node of that subtree could end up being the root. It's also necessary
    ! for extended output in the general case.
    !
    ! As with compress_subtree_wide(), this function is not used on inputs of 1
    ! chunk or less. That's a different codepath.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)   :: Input(0:)
    tIndex,   INTENT(IN)   :: InpLen
    tInteger, INTENT(IN)   :: Key(0:7)
    tLong,    INTENT(IN)   :: ChunkCounter
    tByte,    INTENT(IN)   :: Flags
    tByte,    INTENT(OUT)  :: Output(0:2*BLAKE3_OUT_LEN-1)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: CVArray(0:MAX_SIMD_DEGREE_OR_2*BLAKE3_OUT_LEN-1)
    tByte       :: OutArray(0:MAX_SIMD_DEGREE_OR_2*BLAKE3_OUT_LEN/2-1)
    tIndex      :: NumCV, OutLenM1

! FLOW

    NumCV = Compress_Subtree_Wide(Input, InpLen, Key, ChunkCounter, Flags, CVArray)
    ASSERT_MSG(NumCV <= MAX_SIMD_DEGREE_OR_2, 'Compress_Subtree2ParentNode', &
               'NumCV is greater than MAX_SIMD_DEGREE_OR_2.')

    ! If MAX_SIMD_DEGREE is greater than 2 and there's enough input,
    ! compress_subtree_wide() returns more than 2 chaining values. Condense
    ! them into 2 by forming parent nodes repeatedly.
    ! The second half of this loop condition is always true, and we just
    ! asserted it above. But GCC can't tell that it's always true, and if NDEBUG
    ! is set on platforms where MAX_SIMD_DEGREE_OR_2 == 2, GCC emits spurious
    ! warnings here. GCC 8.5 is particularly sensitive, so if you're changing
    ! this code, test it against that version.
    DO WHILE ((NumCV > 2).AND.(NumCV <= MAX_SIMD_DEGREE_OR_2))
        NumCV = Compress_Parent_Parallel(CVArray, NumCV, Key, Flags, OutArray)
        OutLenM1 = NumCV*BLAKE3_OUT_LEN - 1
        CVArray(0:OutLenM1) = OutArray(0:OutLenM1)
    END DO
    OutLenM1 = 2*BLAKE3_OUT_LEN - 1
    Output(0:OutLenM1) = CVArray(0:OutLenM1)

    RETURN

END SUBROUTINE Compress_Subtree2ParentNode

!******************************************************************************

FUNCTION LeftLen(ContentLen) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    ! Given some input larger than one chunk, return the number of bytes that
    ! should go in the left subtree. This is the largest power-of-2 number of
    ! chunks that leaves at least 1 byte for the right subtree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: ContentLen
    tIndex              :: Length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: FullChunks

! FLOW

    ! Subtract 1 to reserve at least one byte for the right side. content_len
    ! should always be greater than BLAKE3_CHUNK_LEN.
    FullChunks = (ContentLen - 1_kIndex) / BLAKE3_CHUNK_LEN
    Length = RoundDownToPowerOf2(ToLong(FullChunks))*BLAKE3_CHUNK_LEN

    RETURN

END FUNCTION LeftLen

!******************************************************************************

END SUBMODULE SubClass_Blake3Compress

#undef MAX_SIMD_DEGREE
#undef MAX_SIMD_DEGREE_OR_2
#undef BLAKE3_SIMD_DEGREE

!******************************************************************************
