
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
    tIndex              :: I

!** FLOW:

    ! check whether the array is allocated
    IF (ArrayStatusTest(Array)) THEN
        ! free memory of allocatable/pointer components
        DO I = 1, SIZE(Array)
            CALL Array(I)%MemFree()
        END DO
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
    tIndex              :: I, J

!** FLOW:

    ! check whether the array is allocated
    IF (ArrayStatusTest(Array)) THEN
        ! free memory of allocatable/pointer components
        DO I = 1, SIZE(Array, DIM=1)
            DO J = 1, SIZE(Array, DIM=2)
                CALL Array(I,J)%MemFree()
            END DO
        END DO
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
    tIndex              :: I, J, K

!** FLOW:

    ! check whether the array is allocated
    IF (ArrayStatusTest(Array)) THEN
        ! free memory of allocatable/pointer components
        DO I = 1, SIZE(Array, DIM=1)
            DO J = 1, SIZE(Array, DIM=2)
                DO K = 1, SIZE(Array, DIM=3)
                    CALL Array(I,J,K)%MemFree()
                END DO
            END DO
        END DO
        ! deallocate the array
        DEALLOCATE(Array, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('Generic_MemFree_3D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE Generic_MemFree_3D

!**************************************************************************************
