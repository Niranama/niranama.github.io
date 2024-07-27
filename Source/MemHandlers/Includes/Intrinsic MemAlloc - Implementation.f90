
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           ALLOCATE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Generic_MemAlloc_1D(Array, Size, StartID)

!** PURPOSE OF THIS SUBROUTINE:
	!! To allocate memory of array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TypeName, AttributeName, INTENT(INOUT)  :: Array(:)     !! array
    tIndex,                  INTENT(IN)     :: Size         !! size of array
    tIndex,      OPTIONAL,   INTENT(IN)     :: StartID      !! starting index
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:
    
    ! check input validity
    IF (Size < 0) THEN
        CALL Handle_ErrLevel('Generic_MemAlloc_1D', ModName, ErrSevere, '"Size" must not be negative.')
    END IF
    ! free memory if allocated
    CALL MemFree(Array)
    ! allocate memory
    IF (PRESENT(StartID)) THEN
        ALLOCATE(Array(StartID:StartID+Size-1), STAT=AllocStat, ERRMSG=AllocMsg)
    ELSE
        ALLOCATE(Array(Size), STAT=AllocStat, ERRMSG=AllocMsg)
    END IF
    ! handle error if any
    CALL Handle_ErrAlloc('Generic_MemAlloc_1D', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE Generic_MemAlloc_1D

!**************************************************************************************

SUBROUTINE Generic_MemAlloc_2D(Array, Size1, Size2, StartID1, StartID2)

!** PURPOSE OF THIS SUBROUTINE:
	!! To allocate memory of array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TypeName, AttributeName, INTENT(INOUT)  :: Array(:,:)   !! array
    tIndex,                  INTENT(IN)     :: Size1        !! first dimension of array
    tIndex,                  INTENT(IN)     :: Size2        !! second dimension of array
    tIndex,      OPTIONAL,   INTENT(IN)     :: StartID1     !! starting index of first dimension
    tIndex,      OPTIONAL,   INTENT(IN)     :: StartID2     !! starting index of second dimension
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: BeginID1, BeginID2
    tIndex              :: EndID1, EndID2

!** FLOW:
    
    ! check input validity
    IF ((Size1 < 0).OR.(Size2 < 0)) THEN
        CALL Handle_ErrLevel('Generic_MemAlloc_2D', ModName, ErrSevere, '"Size(s)" must not be negative.')
    END IF
    ! set optional input and compute indices
    SET_OPTION(BeginID1, 1, StartID1)
    SET_OPTION(BeginID2, 1, StartID2)
    EndID1 = BeginID1 + Size1 - 1
    EndID2 = BeginID2 + Size2 - 1
    ! free memory if allocated
    CALL MemFree(Array)
    ! allocate memory
    ALLOCATE(Array(BeginID1:EndID1, BeginID2:EndID2), STAT=AllocStat, ERRMSG=AllocMsg)
    ! handle error if any
    CALL Handle_ErrAlloc('Generic_MemAlloc_2D', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE Generic_MemAlloc_2D

!**************************************************************************************

SUBROUTINE Generic_MemAlloc_3D(Array, Size1, Size2, Size3, StartID1, StartID2, StartID3)

!** PURPOSE OF THIS SUBROUTINE:
	!! To allocate memory of array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TypeName, AttributeName, INTENT(INOUT)  :: Array(:,:,:) !! array
    tIndex,                  INTENT(IN)     :: Size1        !! first dimension of array
    tIndex,                  INTENT(IN)     :: Size2        !! second dimension of array
    tIndex,                  INTENT(IN)     :: Size3        !! third dimension of array
    tIndex,      OPTIONAL,   INTENT(IN)     :: StartID1     !! starting index of first dimension
    tIndex,      OPTIONAL,   INTENT(IN)     :: StartID2     !! starting index of second dimension
    tIndex,      OPTIONAL,   INTENT(IN)     :: StartID3     !! starting index of third dimension

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: BeginID1, BeginID2, BeginID3
    tIndex              :: EndID1, EndID2, EndID3

!** FLOW:
    
    ! check input validity
    IF ((Size1 < 0).OR.(Size2 < 0).OR.(Size3 < 0)) THEN
        CALL Handle_ErrLevel('Generic_MemAlloc_3D', ModName, ErrSevere, '"Size(s)" must not be negative.')
    END IF
    ! set optional input and compute indices
    SET_OPTION(BeginID1, 1, StartID1)
    SET_OPTION(BeginID2, 1, StartID2)
    SET_OPTION(BeginID3, 1, StartID3)
    EndID1 = BeginID1 + Size1 - 1
    EndID2 = BeginID2 + Size2 - 1
    EndID3 = BeginID3 + Size3 - 1
    ! free memory if allocated
    CALL MemFree(Array)
    ! allocate memory
    ALLOCATE(Array(BeginID1:EndID1, BeginID2:EndID2, BeginID3:EndID3), STAT=AllocStat, ERRMSG=AllocMsg)
    ! handle error if any
    CALL Handle_ErrAlloc('Generic_MemAlloc_3D', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE Generic_MemAlloc_3D

!**************************************************************************************
