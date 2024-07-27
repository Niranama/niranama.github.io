
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           FREE-UP MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Generic_MemFree_1D(Array)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TypeName, AttributeName, INTENT(INOUT)  :: Array(:)
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    ! check whether the array is allocated
    IF (ArrayStatusTest(Array)) THEN
        ! deallocate the array
        DEALLOCATE(Array, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('Generic_MemFree_1D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE Generic_MemFree_1D

!**************************************************************************************

SUBROUTINE Generic_MemFree_2D(Array)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TypeName, AttributeName, INTENT(INOUT)  :: Array(:,:)
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    ! check whether the array is allocated
    IF (ArrayStatusTest(Array)) THEN
        ! deallocate the array
        DEALLOCATE(Array, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('Generic_MemFree_2D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE Generic_MemFree_2D

!**************************************************************************************

SUBROUTINE Generic_MemFree_3D(Array)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TypeName, AttributeName, INTENT(INOUT)  :: Array(:,:,:)
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    ! check whether the array is allocated
    IF (ArrayStatusTest(Array)) THEN
        ! deallocate the array
        DEALLOCATE(Array, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('Generic_MemFree_3D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE Generic_MemFree_3D

!**************************************************************************************
