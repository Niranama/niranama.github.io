
SUBMODULE (Class_Blake3) SubClass_Blake3Private

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains private implementation of the BLAKE3
!   message-digest algorithm. <br>

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! Output node of the Blake3 hash tree (Helper Object)
    TYPE OutNode
        tInteger    :: InputCV(0:BLAKE3_KEY_LEN-1)
        tByte       :: Blocks(0:BLAKE3_BLOCK_LEN-1)
        tIndex      :: BlockLen
        tLong       :: Counter
        tByte       :: Flags
    END TYPE OutNode
    
!** INTERFACE DEFINITIONS:
    ! interfaces to 'compress' operations
    INTERFACE
        !----------------------------------------------------------------------
        MODULE SUBROUTINE Compress_Inplace(ChainVal, MBlock, BlockLen, Counter, Flags)
            tInteger, INTENT(INOUT) :: ChainVal(0:7)
            tByte,    INTENT(IN)    :: MBlock(0:BLAKE3_BLOCK_LEN-1)
            tIndex,   INTENT(IN)    :: BlockLen
            tLong,    INTENT(IN)    :: Counter
            tByte,    INTENT(IN)    :: Flags
        END SUBROUTINE
        !----------------------------------------------------------------------
        MODULE SUBROUTINE Compress_XOF(ChainVal, MBlock, BlockLen, Counter, Flags, OutBuf)
            tInteger, INTENT(IN)    :: ChainVal(0:7)
            tByte,    INTENT(IN)    :: MBlock(0:BLAKE3_BLOCK_LEN-1)
            tIndex,   INTENT(IN)    :: BlockLen
            tLong,    INTENT(IN)    :: Counter
            tByte,    INTENT(IN)    :: Flags
            tByte,    INTENT(OUT)   :: OutBuf(0:63)
        END SUBROUTINE Compress_XOF
        !----------------------------------------------------------------------
        MODULE SUBROUTINE Compress_Subtree2ParentNode(Input, InpLen, Key, ChunkCounter, &
                                                      Flags, Output)
            tByte,    INTENT(IN)   :: Input(0:)
            tIndex,   INTENT(IN)   :: InpLen
            tInteger, INTENT(IN)   :: Key(0:7)
            tLong,    INTENT(IN)   :: ChunkCounter
            tByte,    INTENT(IN)   :: Flags
            tByte,    INTENT(OUT)  :: Output(0:2*BLAKE3_OUT_LEN-1)
        END SUBROUTINE
        !----------------------------------------------------------------------
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!                           Blake3 Procedures
!------------------------------------------------------------------------------

MODULE SUBROUTINE Blake3_InitBase(MD, Key, Flags)

!** PURPOSE OF THIS SUBROUTINE:
    ! To initialize Blake3 hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD
    tInteger,      INTENT(IN)       :: Key(0:BLAKE3_KEY_LEN-1)
    tByte,         INTENT(IN)       :: Flags

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    MD%Key = Key
    CALL ChunkState_Init(MD%Chunk, Key, Flags)
    MD%CVStackLen = 0_kByte

    RETURN

END SUBROUTINE Blake3_InitBase

!******************************************************************************

MODULE SUBROUTINE Blake3_Merge_CVStack(MD, TotalLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! As described in hasher_push_cv() below, we do "lazy merging", delaying
    ! merges until right before the next CV is about to be added. This is
    ! different from the reference implementation. Another difference is that we
    ! aren't always merging 1 chunk at a time. Instead, each CV might represent
    ! any power-of-two number of chunks, as long as the smaller-above-larger stack
    ! order is maintained. Instead of the "count the trailing 0-bits" algorithm
    ! described in the spec, we use a "count the total number of 1-bits" variant
    ! that doesn't require us to retain the subtree size of the CV on top of the
    ! stack. The principle is the same: each CV that should remain in the stack is
    ! represented by a 1-bit in the total number of chunks (or bytes) so far.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), TARGET, INTENT(INOUT)    :: MD
    tLong,                 INTENT(IN)       :: TotalLen

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: ParentNode(:)
    tIndex          :: PostMergeStackLen
    TYPE(OutNode)   :: Output

! FLOW

    PostMergeStackLen = POPCNT(TotalLen)
    DO WHILE (MD%CVStackLen > PostMergeStackLen)
        ParentNode => MD%CVStack((MD%CVStackLen-2)*BLAKE3_OUT_LEN:)
        Output = Parent_OutNode(ParentNode, MD%Key, MD%Chunk%Flags)
        CALL OutNode_ChainingValue(Output, ParentNode)
        MD%CVStackLen = MD%CVStackLen - 1
    END DO

    NULLIFY(ParentNode)

    RETURN

END SUBROUTINE Blake3_Merge_CVStack

!******************************************************************************

MODULE SUBROUTINE Blake3_Push_CV(MD, NewCV, ChunkCounter)

!** PURPOSE OF THIS SUBROUTINE:
    ! In reference_impl.rs, we merge the new CV with existing CVs from the stack
    ! before pushing it. We can do that because we know more input is coming, so
    ! we know none of the merges are root.
    !
    ! This setting is different. We want to feed as much input as possible to
    ! compress_subtree_wide(), without setting aside anything for the chunk_state.
    ! If the user gives us 64 KiB, we want to parallelize over all 64 KiB at once
    ! as a single subtree, if at all possible.
    !
    ! This leads to two problems:
    ! 1) This 64 KiB input might be the only call that ever gets made to update.
    !    In this case, the root node of the 64 KiB subtree would be the root node
    !    of the whole tree, and it would need to be ROOT finalized. We can't
    !    compress it until we know.
    ! 2) This 64 KiB input might complete a larger tree, whose root node is
    !    similarly going to be the the root of the whole tree. For example, maybe
    !    we have 196 KiB (that is, 128 + 64) hashed so far. We can't compress the
    !    node at the root of the 256 KiB subtree until we know how to finalize it.
    !
    ! The second problem is solved with "lazy merging". That is, when we're about
    ! to add a CV to the stack, we don't merge it with anything first, as the
    ! reference impl does. Instead we do merges using the *previous* CV that was
    ! added, which is sitting on top of the stack, and we put the new CV
    ! (unmerged) on top of the stack afterwards. This guarantees that we never
    ! merge the root node until finalize().
    !
    ! Solving the first problem requires an additional tool,
    ! compress_subtree_to_parent_node(). That function always returns the top
    ! *two* chaining values of the subtree it's compressing. We then do lazy
    ! merging with each of them separately, so that the second CV will always
    ! remain unmerged. (That also helps us support extensible output when we're
    ! hashing an input all-at-once.)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD
    tByte,         INTENT(IN)       :: NewCV(0:BLAKE3_OUT_LEN-1)
    tLong,         INTENT(IN)       :: ChunkCounter

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: IStart, IEnd

! FLOW

    CALL MD%MergeCVStack(ChunkCounter)
    IStart = MD%CVStackLen*BLAKE3_OUT_LEN
    IEnd   = IStart + BLAKE3_OUT_LEN - 1
    MD%CVStack(IStart:IEnd) = NewCV(0:BLAKE3_OUT_LEN-1)
    MD%CVStackLen = MD%CVStackLen + 1

    RETURN

END SUBROUTINE Blake3_Push_CV

!******************************************************************************

MODULE SUBROUTINE Blake3_Update(MD, BytesIn, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add input (with offset and length parameters) and update the hasher state

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD
    tByte,         INTENT(IN)       :: BytesIn(0:)
    tIndex,        INTENT(IN)       :: Offset
    tIndex,        INTENT(IN)       :: Length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte               :: ChunkCV(0:31)
    tIndex              :: CurrLen, CurrPos, Take, SubtreeLen
    tLong               :: CountSoFar, SubtreeChunks
    TYPE(OutNode)       :: Output
    TYPE(ChunkState)    :: Chunk

! FLOW

    ! initialize
    CurrLen = Length

    ! Explicitly checking for zero avoids causing UB by passing a null pointer
    ! to memcpy. This comes up in practice with things like:
    !   std::vector<uint8_t> v
    !   blake3_hasher_update(&hasher, v.data(), v.size())
    IF (CurrLen == 0) RETURN

    ! If we have some partial chunk bytes in the internal chunk_state, we need
    ! to finish that chunk first.
    CurrPos = Offset
    IF (ChunkState_Length(MD%Chunk) > 0_kIndex) THEN
        Take = BLAKE3_CHUNK_LEN - ChunkState_Length(MD%Chunk)
        IF (Take > CurrLen) Take = CurrLen
        CALL ChunkState_Update(MD%Chunk, BytesIn(CurrPos:), Take)
        CurrPos = CurrPos + Take
        CurrLen = CurrLen - Take
        ! If we've filled the current chunk and there's more coming, finalize this
        ! chunk and proceed. In this case we know it's not the root.
        IF (CurrLen > 0) THEN
            Output = ChunkState_OutNode(MD%Chunk)
            CALL OutNode_ChainingValue(Output, ChunkCV)
            CALL MD%PushCV(ChunkCV, MD%Chunk%Counter)
            CALL ChunkState_Reset(MD%Chunk, MD%Key, MD%Chunk%Counter + 1)
        ELSE
            RETURN
        END IF
    END IF

    ! Now the chunk_state is clear, and we have more input. If there's more than
    ! a single chunk (so, definitely not the root chunk), hash the largest whole
    ! subtree we can, with the full benefits of SIMD (and maybe in the future,
    ! multi-threading) parallelism. Two restrictions:
    ! - The subtree has to be a power-of-2 number of chunks. Only subtrees along
    !   the right edge can be incomplete, and we don't know where the right edge
    !   is going to be until we get to finalize().
    ! - The subtree must evenly divide the total number of chunks up until this
    !   point (if total is not 0). If the current incomplete subtree is only
    !   waiting for 1 more chunk, we can't hash a subtree of 4 chunks. We have
    !   to complete the current subtree first.
    ! Because we might need to break up the input to form powers of 2, or to
    ! evenly divide what we already have, this part runs in a loop.
    DO WHILE (CurrLen > BLAKE3_CHUNK_LEN)
        SubtreeLen = RoundDownToPowerOf2(ToLong(CurrLen))
        CountSoFar = MD%Chunk%Counter*BLAKE3_CHUNK_LEN
        ! Shrink the SubtreeLen until it evenly divides the count so far. We know
        ! that SubtreeLen itself is a power of 2, so we can use a bitmasking
        ! trick instead of an actual remainder operation. (Note that if the caller
        ! consistently passes power-of-2 inputs of the same size, as is hopefully
        ! typical, this loop condition will always fail, and SubtreeLen will
        ! always be the full length of the input.)
        !
        ! An aside: We don't have to shrink SubtreeLen quite this much. For
        !     example, if CountSoFar is 1, we could pass 2 chunks to
        ! compress_subtree_to_parent_node. Since we'll get 2 CVs back, we'll still
        ! get the right answer in the end, and we might get to use 2-way SIMD
        ! parallelism. The problem with this optimization, is that it gets us
        ! stuck always hashing 2 chunks. The total number of chunks will remain
        ! odd, and we'll never graduate to higher degrees of parallelism. See
        ! https://github.com/BLAKE3-team/BLAKE3/issues/69.
        DO WHILE (IAND(ToLong(SubtreeLen - 1_kIndex), CountSoFar) /= 0_kLong)
            SubtreeLen = SubtreeLen/2_kIndex
        END DO
        ! The shrunken SubtreeLen might now be 1 chunk long. If so, hash that one
        ! chunk by itself. Otherwise, compress the subtree into a pair of CVs.
        SubtreeChunks = SubtreeLen / BLAKE3_CHUNK_LEN
        IF (SubtreeLen <= BLAKE3_CHUNK_LEN) THEN
            CALL ChunkState_Init(Chunk, MD%Key, MD%Chunk%Flags)
            Chunk%Counter = MD%Chunk%Counter
            CALL ChunkState_Update(Chunk, BytesIn(CurrPos:), SubtreeLen)
            Output = ChunkState_OutNode(Chunk)
            CALL OutNode_ChainingValue(Output, ChunkCV)
            CALL MD%PushCV(ChunkCV, Chunk%Counter)
        ELSE
        ! This is the high-performance happy path, though getting here depends
        ! on the caller giving us a long enough input.
            BLOCK
                tByte   CVPair(0:2*BLAKE3_OUT_LEN-1)
                CALL Compress_Subtree2ParentNode(BytesIn(CurrPos:), SubtreeLen, MD%Key, &
                                                    MD%Chunk%Counter, MD%Chunk%Flags, &
                                                    CVPair)
                CALL MD%PushCV(CVPair(0:), MD%Chunk%Counter)
                CALL MD%PushCV(CVPair(BLAKE3_OUT_LEN:), &
                                    MD%Chunk%Counter+(SubtreeChunks/2))
            END BLOCK
        END IF
        MD%Chunk%Counter = MD%Chunk%Counter + SubtreeChunks
        CurrPos = CurrPos + SubtreeLen
        CurrLen = CurrLen - SubtreeLen
    END DO

    ! If there's any remaining input less than a full chunk, add it to the chunk
    ! state. In that case, also do a final merge loop to make sure the subtree
    ! stack doesn't contain any unmerged pairs. The remaining input means we
    ! know these merges are non-root. This merge loop isn't strictly necessary
    ! here, because hasher_push_ChunkCV already does its own merge loop, but it
    ! simplifies blake3_hasher_finalize below.
    IF (CurrLen > 0) THEN
        CALL ChunkState_Update(MD%Chunk, BytesIn(CurrPos:), CurrLen)
        CALL MD%MergeCVStack(MD%Chunk%Counter)
    END IF

    RETURN

END SUBROUTINE Blake3_Update

!******************************************************************************

MODULE SUBROUTINE Blake3_Finalize_Seek(MD, Seek, Output, OutLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To finalize the hasher state and return output

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD           ! 'Blake3' hasher
    tLong,         INTENT(IN)       :: Seek         ! the starting byte position in the output.
    tByte,         INTENT(INOUT)    :: Output(0:)   ! the output byte array
    tIndex,        INTENT(IN)       :: OutLen       ! the output length needed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: CVRemaining
    TYPE(OutNode)       :: Node

! FLOW

    ! Explicitly checking for zero avoids causing UB by passing a null pointer
    ! to memcpy. This comes up in practice with things like:
    !   std::vector<uint8_t> v
    !   blake3_hasher_finalize(&hasher, v.data(), v.size())
    IF (OutLen == 0_kIndex) RETURN

    ! If the subtree stack is empty, then the current chunk is the root.
    IF (MD%CVStackLen == 0_kByte) THEN
        Node = ChunkState_OutNode(MD%Chunk)
        CALL OutNode_RootBytes(Node, Seek, Output, OutLen)
        RETURN
    END IF
    ! If there are any bytes in the chunk state, finalize that chunk and do a
    ! roll-up merge between that chunk hash and every subtree in the stack. In
    ! this case, the extra merge loop at the end of blake3_hasher_update
    ! guarantees that none of the subtrees in the stack need to be merged with
    ! each other first. Otherwise, if there are no bytes in the chunk state,
    ! then the top of the stack is a chunk hash, and we start the merge from
    ! that.
    IF (ChunkState_Length(MD%Chunk) > 0) THEN
        CVRemaining = MD%CVStackLen
        Node = ChunkState_OutNode(MD%Chunk)
    ELSE
        ! There are always at least 2 CVs in the stack in this case.
        CVRemaining = MD%CVStackLen - 2_kIndex
        Node = Parent_OutNode(MD%CVStack(CVRemaining*32:), MD%Key, MD%Chunk%Flags)
    END IF
    BLOCK
        tByte       :: ParentBlock(0:BLAKE3_BLOCK_LEN-1)
        tIndex      :: IStart, IEnd
        DO WHILE (CVRemaining > 0)
            CVRemaining = CVRemaining - 1_kIndex
            IStart = CVRemaining*32
            IEnd   = IStart + 31
            ParentBlock(0:31) = MD%CVStack(IStart:IEnd)
            CALL OutNode_ChainingValue(Node, ParentBlock(32:))
            Node = Parent_OutNode(ParentBlock, MD%Key, MD%Chunk%Flags)
        END DO
    END BLOCK
    CALL OutNode_RootBytes(Node, Seek, Output, OutLen)

    RETURN

END SUBROUTINE Blake3_Finalize_Seek

!******************************************************************************

MODULE SUBROUTINE Blake3_Finalize(MD, Output, OutLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To finalize the hasher state and return output

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD
    tByte,         INTENT(OUT)      :: Output(0:)
    tIndex,        INTENT(IN)       :: OutLen

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%DoFinal(0_kLong, Output, OutLen)

    RETURN

END SUBROUTINE Blake3_Finalize

!------------------------------------------------------------------------------
!                     ChunkState and OutNode Procedures
!------------------------------------------------------------------------------

SUBROUTINE ChunkState_Init(Chunk, Key, Flags)

!** PURPOSE OF THIS SUBROUTINE:
    ! To initialize the ChunkState object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ChunkState), INTENT(INOUT) :: Chunk
    tInteger,         INTENT(IN)    :: Key(0:7)
    tByte,            INTENT(IN)    :: Flags

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na
        
! FLOW
        
    Chunk%CV(0:BLAKE3_KEY_LEN-1) = Key(0:BLAKE3_KEY_LEN-1)
    Chunk%Counter = 0_kLong
    Chunk%Buf(0:BLAKE3_BLOCK_LEN-1) = 0_kByte
    Chunk%BufLen = 0_kByte
    Chunk%BlocksCompressed = 0_kByte
    Chunk%Flags = Flags

    RETURN

END SUBROUTINE ChunkState_Init

!******************************************************************************

MODULE SUBROUTINE ChunkState_Reset(Chunk, Key, Counter)

!** PURPOSE OF THIS SUBROUTINE:
    ! To reset the ChunkState object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ChunkState), INTENT(INOUT) :: Chunk
    tInteger,         INTENT(IN)    :: Key(0:7)
    tLong,            INTENT(IN)    :: Counter

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na
        
! FLOW
        
    Chunk%CV(0:BLAKE3_KEY_LEN-1) = Key(0:BLAKE3_KEY_LEN-1)
    Chunk%Counter = Counter
    Chunk%Buf(0:BLAKE3_BLOCK_LEN-1) = 0_kByte
    Chunk%BufLen = 0_kByte
    Chunk%BlocksCompressed = 0_kByte

    RETURN

END SUBROUTINE ChunkState_Reset

!******************************************************************************

FUNCTION ChunkState_Length(Chunk) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the length of ChunkState.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ChunkState), INTENT(IN)    :: Chunk
    tIndex                          :: Length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na


! FLOW

    Length = BLAKE3_BLOCK_LEN*ToIndex(Chunk%BlocksCompressed) + ToIndex(Chunk%BufLen)

    RETURN

END FUNCTION ChunkState_Length

!******************************************************************************

FUNCTION ChunkState_FillBuf(Chunk, Input, InpLen) RESULT(Take)

!** PURPOSE OF THIS SUBROUTINE:
    ! To insert input and update the ChunkState.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ChunkState), INTENT(INOUT) :: Chunk
    tByte,            INTENT(IN)    :: Input(0:)
    tIndex,           INTENT(IN)    :: InpLen
    tIndex                          :: Take

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na
        
! FLOW
        
    Take = BLAKE3_BLOCK_LEN - ToIndex(Chunk%BufLen)
    IF (Take > InpLen) Take = InpLen
    Chunk%Buf(Chunk%BufLen:Chunk%BufLen+Take-1) = Input(0:Take-1)
    Chunk%BufLen = Chunk%BufLen + Take

    RETURN

END FUNCTION ChunkState_FillBuf

!******************************************************************************

FUNCTION ChunkState_StartFlag(Chunk) RESULT(StartFlag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the starting flag of ChunkState.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ChunkState), INTENT(IN)    :: Chunk
    tByte                           :: StartFlag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na


! FLOW

    IF (Chunk%BlocksCompressed == 0_kByte) THEN
        StartFlag = CHUNK_START
    ELSE
        StartFlag = 0_kByte
    END IF

    RETURN

END FUNCTION ChunkState_StartFlag

!******************************************************************************

SUBROUTINE ChunkState_Update(Chunk, Input, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To insert input and update the ChunkState.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ChunkState), INTENT(INOUT) :: Chunk
    tByte,            INTENT(IN)    :: Input(0:)
    tIndex,           INTENT(IN)    :: InpLen

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Take, CurLen, CurPos
        
! FLOW

    IF (Chunk%BufLen > 0_kByte) THEN
        Take = ChunkState_FillBuf(Chunk, Input, InpLen)
        CurPos = Take
        CurLen = InpLen - Take
        IF (CurLen > 0) THEN
            CALL Compress_Inplace(Chunk%CV, Chunk%Buf, BLAKE3_BLOCK_LEN, Chunk%Counter, &
                                IOR(Chunk%Flags, ChunkState_StartFlag(Chunk)))
            Chunk%BlocksCompressed = Chunk%BlocksCompressed + 1_kByte
            Chunk%BufLen = 0_kByte
            Chunk%Buf    = 0_kByte
        END IF
    ELSE
        CurPos = 0
        CurLen = InpLen
    END IF
    DO WHILE (CurLen > BLAKE3_BLOCK_LEN)
        CALL Compress_Inplace(Chunk%CV, Input(CurPos:), BLAKE3_BLOCK_LEN, Chunk%Counter, &
                                IOR(Chunk%Flags, ChunkState_StartFlag(Chunk)))
        Chunk%BlocksCompressed = Chunk%BlocksCompressed + 1_kByte
        CurPos = CurPos + BLAKE3_BLOCK_LEN
        CurLen = CurLen - BLAKE3_BLOCK_LEN
    END DO

    Take = ChunkState_FillBuf(Chunk, Input(CurPos:), CurLen)

    RETURN

END SUBROUTINE ChunkState_Update

!******************************************************************************

FUNCTION ChunkState_OutNode(Chunk) RESULT(Node)

!** PURPOSE OF THIS SUBROUTINE:
    ! To create and return OutNode.
 
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ChunkState), INTENT(INOUT) :: Chunk
    TYPE(OutNode)                   :: Node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: BlockFlags

! FLOW
        
    BlockFlags = IOR(IOR(Chunk%Flags, ChunkState_StartFlag(Chunk)), CHUNK_END)
    Node = OutNode(Chunk%CV, Chunk%Buf, Chunk%BufLen, Chunk%Counter, BlockFlags)

    RETURN

END FUNCTION ChunkState_OutNode

!******************************************************************************

SUBROUTINE OutNode_ChainingValue(Node, CV)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the chaining values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(OutNode), INTENT(IN)   :: Node
    tByte,         INTENT(OUT)  :: CV(0:31)  ! chaining values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: CV_Words(0:7)
    tIndex      :: I

! FLOW
        
    CV_Words(0:7) = Node%InputCV(0:7)
    CALL Compress_Inplace(CV_Words, Node%Blocks, Node%BlockLen, Node%Counter, Node%Flags)
    DO I = 0, 7
        CALL ByteUnpackLE(CV_Words(I), CV, I*4)
    END DO
        
    RETURN

END SUBROUTINE OutNode_ChainingValue

!******************************************************************************

SUBROUTINE OutNode_RootBytes(Node, Seek, Output, OutLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the output bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(OutNode), INTENT(IN)   :: Node
    tLong,         INTENT(IN)   :: Seek
    tByte,         INTENT(OUT)  :: Output(0:)
    tIndex,        INTENT(IN)   :: OutLen

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: OutputCounter
    tIndex      :: Offset, AvailableBytes, CopyLen, Pos, CurLen
    tByte       :: WideBuf(0:63)

! FLOW

    OutputCounter = Seek / 64_kLong
    Offset = MOD(Seek, 64_kLong)
    Pos = 0_kIndex
    CurLen = OutLen
    DO WHILE (CurLen > 0_kIndex)
        ! compress input blocks
        CALL Compress_XOF(Node%InputCV, Node%Blocks, Node%BlockLen, OutputCounter, &
                            IOR(Node%Flags, ROOT), WideBuf)
        ! transfer output
        AvailableBytes = 64_kIndex - Offset
        IF (CurLen > AvailableBytes) THEN
            CopyLen = AvailableBytes
        ELSE
            CopyLen = CurLen
        END IF
        Output(Pos:Pos+CopyLen-1) = WideBuf(Offset:Offset+CopyLen-1)
        ! update indices
        Pos = Pos + CopyLen
        CurLen = CurLen - CopyLen
        OutputCounter = OutputCounter + 1_kLong
        Offset = 0_kIndex
    END DO

    RETURN

END SUBROUTINE OutNode_RootBytes

!******************************************************************************

FUNCTION Parent_OutNode(Blocks, Key, Flags) RESULT(Node)

!** PURPOSE OF THIS SUBROUTINE:
    ! To create and return OutNode.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Blocks(0:BLAKE3_BLOCK_LEN-1)
    tInteger, INTENT(IN)    :: Key(0:7)
    tByte,    INTENT(IN)    :: Flags
    TYPE(OutNode)           :: Node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Node = OutNode(Key, Blocks, BLAKE3_BLOCK_LEN, 0_kLong, IOR(Flags, PARENT))
 
    RETURN

END FUNCTION Parent_OutNode

!------------------------------------------------------------------------------
!                           Auxiliary Procedures
!------------------------------------------------------------------------------

FUNCTION RoundDownToPowerOf2(X) RESULT(Y)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return largest power of two less than or equal to X.
    ! As a special case, returns 1 when X is 0.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: X
    tLong               :: Y

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Y = SHIFTL(1_kLong, Highest_One(IOR(X, 1_kLong)))

    RETURN

CONTAINS

    FUNCTION Highest_One(X) RESULT(Indx)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To find index of the highest set bit where X is assumed to be nonzero.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: X
        tInteger            :: Indx

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: XX

    ! FLOW

        XX = X
        Indx = 0
        IF (IAND(XX, ToLong(Z'FFFFFFFF00000000')) /= 0_kLong) THEN
            XX = SHIFTR(XX, 32)
            Indx = Indx + 32
        END IF
        IF (IAND(XX, ToLong(Z'00000000FFFF0000')) /= 0_kLong) THEN
            XX = SHIFTR(XX, 16)
            Indx = Indx + 16
        END IF
        IF (IAND(XX, ToLong(Z'000000000000FF00')) /= 0_kLong) THEN
            XX = SHIFTR(XX, 8)
            Indx = Indx + 8
        END IF
        IF (IAND(XX, ToLong(Z'00000000000000F0')) /= 0_kLong) THEN
            XX = SHIFTR(XX, 4)
            Indx = Indx + 4
        END IF
        IF (IAND(XX, ToLong(Z'000000000000000C')) /= 0_kLong) THEN
            XX = SHIFTR(XX, 2)
            Indx = Indx + 2
        END IF
        IF (IAND(XX, ToLong(Z'0000000000000002')) /= 0_kLong) THEN
            Indx = Indx + 1
        END IF

        RETURN

    END FUNCTION Highest_One

    !**************************************************************************

END FUNCTION RoundDownToPowerOf2

!******************************************************************************

END SUBMODULE SubClass_Blake3Private

!******************************************************************************
