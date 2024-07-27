
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           REALLOCATE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Generic_MemResize_1D(Array, NewSize)

!** PURPOSE OF THIS SUBROUTINE:
	!! To re-allocate array and preserve its data

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TypeName, AttributeName, INTENT(INOUT)  :: Array(:)     !! array
    tIndex,                  INTENT(IN)     :: NewSize      !! new size of array
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: OldSize      ! original size of array
    tIndex                  :: StartID      ! starting index
    tIndex                  :: PSize        ! size of preserved data
    tIndex                  :: I            ! index
    TypeName, AttributeName :: Temp(:)      ! working variable

!** FLOW:
    
    ! check input validity
    IF (NewSize < 0) THEN
        CALL Handle_ErrLevel('Generic_MemResize_1D', ModName, ErrSevere, '"NewSize" must not be negative.')
    END IF
    ! determine the original size and its starting index
    IF (ArrayStatusTest(Array)) THEN
        OldSize = SIZE(Array, DIM=1)
        StartID = LBOUND(Array, DIM=1)
    ELSE
        OldSize = 0
        StartID = 1
    END IF
    ! re-allocate array and preserve its data
    IF (OldSize == 0) THEN
        ! just allocate the array
        CALL MemAlloc(Array, NewSize, StartID)
    ELSE
        ! first, allocate the temporary array
        CALL MemAlloc(Temp, NewSize, StartID)
        ! determine the preserving size
        IF (NewSize >= OldSize) THEN
            PSize = OldSize
        ELSE
            PSize = NewSize
        END IF
        ! copy data to the temporary array
        DO I = StartID, (PSize+StartID-1)
            Temp(I) = Array(I)
        END DO
        ! move data from the temporary array back to the array
        ! (this operation includes deallocate the array, reallocate it to
        !  the new size and copy data back)
        MEM_MOVE(Temp, Array)
    END IF

    RETURN

END SUBROUTINE Generic_MemResize_1D

!**************************************************************************************

SUBROUTINE Generic_MemResize_2D(Array, NewSize1, NewSize2)

!** PURPOSE OF THIS SUBROUTINE:
	!! To re-allocate array and preserve its data

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TypeName, AttributeName, INTENT(INOUT)  :: Array(:,:)   !! array
    tIndex,                  INTENT(IN)     :: NewSize1     !! new size of the first dimension
    tIndex,                  INTENT(IN)     :: NewSize2     !! new size of the second dimension
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: OldSize1     ! original size of the first dimension
    tIndex                  :: OldSize2     ! original size of the second dimension
    tIndex                  :: StartID1     ! starting index of the first dimension
    tIndex                  :: StartID2     ! starting index of the second dimension
    tIndex                  :: PSize1       ! size of preserved data of the first dimension
    tIndex                  :: PSize2       ! size of preserved data of the second dimension
    tIndex                  :: I, J         ! indices
    TypeName, AttributeName :: Temp(:,:)    ! working variable

!** FLOW:
    
    ! check input validity
    IF ((NewSize1 < 0).OR.(NewSize2 < 0)) THEN
        CALL Handle_ErrLevel('Generic_MemResize_2D', ModName, ErrSevere, '"NewSize(s)" must not be negative.')
    END IF
    ! determine the original size and its starting index
    IF (ArrayStatusTest(Array)) THEN
        OldSize1 = SIZE(Array, DIM=1)
        OldSize2 = SIZE(Array, DIM=2)
        StartID1 = LBOUND(Array, DIM=1)
        StartID2 = LBOUND(Array, DIM=2)
    ELSE
        OldSize1 = 0
        OldSize2 = 0
        StartID1 = 1
        StartID2 = 1
    END IF
    ! re-allocate array and preserve its data
    IF ((OldSize1 == 0).OR.(OldSize2 == 0)) THEN
        ! just allocate the array
        CALL MemAlloc(Array, NewSize1, NewSize2, StartID1, StartID2)
    ELSE
        ! first, allocate the temporary array
        CALL MemAlloc(Temp, NewSize1, NewSize2, StartID1, StartID2)
        ! determine the preserving sizes
        IF (NewSize1 >= OldSize1) THEN
            PSize1 = OldSize1
        ELSE
            PSize1 = NewSize1
        END IF
        IF (NewSize2 >= OldSize2) THEN
            PSize2 = OldSize2
        ELSE
            PSize2 = NewSize2
        END IF
        ! copy data to the temporary array
        DO I = StartID1, (PSize1+StartID1-1)
            DO J = StartID2, (PSize2+StartID2-1)
                Temp(I,J) = Array(I,J)
            END DO
        END DO
        ! move data from the temporary array back to the array
        ! (this operation includes deallocate the array, reallocate it to
        !  the new size and copy data back)
        MEM_MOVE(Temp, Array)
    END IF

    RETURN

END SUBROUTINE Generic_MemResize_2D

!**************************************************************************************

SUBROUTINE Generic_MemResize_3D(Array, NewSize1, NewSize2, NewSize3)

!** PURPOSE OF THIS SUBROUTINE:
	!! To re-allocate array and preserve its data

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TypeName, AttributeName, INTENT(INOUT)  :: Array(:,:,:) !! array
    tIndex,                  INTENT(IN)     :: NewSize1     !! new size of the first dimension
    tIndex,                  INTENT(IN)     :: NewSize2     !! new size of the second dimension
    tIndex,                  INTENT(IN)     :: NewSize3     !! new size of the third dimension
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: OldSize1     ! original size of the first dimension
    tIndex                  :: OldSize2     ! original size of the second dimension
    tIndex                  :: OldSize3     ! original size of the third dimension
    tIndex                  :: StartID1     ! starting index of the first dimension
    tIndex                  :: StartID2     ! starting index of the second dimension
    tIndex                  :: StartID3     ! starting index of the third dimension
    tIndex                  :: PSize1       ! size of preserved data of the first dimension
    tIndex                  :: PSize2       ! size of preserved data of the second dimension
    tIndex                  :: PSize3       ! size of preserved data of the third dimension
    tIndex                  :: I, J, K      ! indices
    TypeName, AttributeName :: Temp(:,:,:)  ! working variable

!** FLOW:
    
    ! check input validity
    IF ((NewSize1 < 0).OR.(NewSize2 < 0).OR.(NewSize3 < 0)) THEN
        CALL Handle_ErrLevel('Generic_MemResize_3D', ModName, ErrSevere, '"NewSize(s)" must not be negative.')
    END IF
    ! determine the original size and its starting index
    IF (ArrayStatusTest(Array)) THEN
        OldSize1 = SIZE(Array, DIM=1)
        OldSize2 = SIZE(Array, DIM=2)
        OldSize3 = SIZE(Array, DIM=3)
        StartID1 = LBOUND(Array, DIM=1)
        StartID2 = LBOUND(Array, DIM=2)
        StartID3 = LBOUND(Array, DIM=3)
    ELSE
        OldSize1 = 0
        OldSize2 = 0
        OldSize3 = 0
        StartID1 = 1
        StartID2 = 1
        StartID3 = 1
    END IF
    ! re-allocate array and preserve its data
    IF ((OldSize1 == 0).OR.(OldSize2 == 0).OR.(OldSize3 == 0)) THEN
        ! just allocate the array
        CALL MemAlloc(Array, NewSize1, NewSize2, NewSize3, StartID1, StartID2, StartID3)
    ELSE
        ! first, allocate the temporary array
        CALL MemAlloc(Temp, NewSize1, NewSize2, NewSize3, StartID1, StartID2, StartID3)
        ! determine the preserving sizes
        IF (NewSize1 >= OldSize1) THEN
            PSize1 = OldSize1
        ELSE
            PSize1 = NewSize1
        END IF
        IF (NewSize2 >= OldSize2) THEN
            PSize2 = OldSize2
        ELSE
            PSize2 = NewSize2
        END IF
        IF (NewSize3 >= OldSize3) THEN
            PSize3 = OldSize3
        ELSE
            PSize3 = NewSize3
        END IF
        ! copy data to the temporary array
        DO I = StartID1, (PSize1+StartID1-1)
            DO J = StartID2, (PSize2+StartID2-1)
                DO K = StartID3, (PSize3+StartID3-1)
                    Temp(I,J,K) = Array(I,J,K)
                END DO
            END DO
        END DO
        ! move data from the temporary array back to the array
        ! (this operation includes deallocate the array, reallocate it to
        !  the new size and copy data back)
        MEM_MOVE(Temp, Array)
    END IF

    RETURN

END SUBROUTINE Generic_MemResize_3D

!**************************************************************************************
