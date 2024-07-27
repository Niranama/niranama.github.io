
MODULE ModBase_PolyMemHandlers

!** PURPOSE OF THIS MODULE:
    ! contains routines dealing with polymorphism

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers
    USE ModBase_SIntUtil,           ONLY: ToChar => ToDecStrSigned
    USE ModBase_PolyCommon
    USE Class_Assignable
    USE Class_Comparable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: PolyMemFree,         PolyMemAlloc,       PolyMemResize
    PUBLIC  :: PolyMemFreePtr,      PolyMemAllocPtr
    PUBLIC  :: PolyMemFreePtr1D,    PolyMemAllocPtr1D,  PolyMemResizePtr1D
    PUBLIC  :: PolyMemFreePtr2D,    PolyMemAllocPtr2D,  PolyMemResizePtr2D
    PUBLIC  :: PolyMemFreePtr3D,    PolyMemAllocPtr3D,  PolyMemResizePtr3D
    PUBLIC  :: FreeComponents

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'ModBase_PolyMemHandlers'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        ! FreeObject is an interface procedure to free memory of
        ! a derived type with allocatable/pointer components.
        SUBROUTINE FreeComponents(This)
            IMPLICIT NONE
            CLASS(*), INTENT(INOUT) :: This
        END SUBROUTINE
    END INTERFACE

!** GENERIC SPECIFICATIONS:
    INTERFACE PolyMemFree
        MODULE PROCEDURE MemFree_Allocatable_Scalar
        MODULE PROCEDURE MemFree_Allocatable_1D
        MODULE PROCEDURE MemFree_Allocatable_2D
        MODULE PROCEDURE MemFree_Allocatable_3D
    END INTERFACE
    INTERFACE PolyMemAlloc
        MODULE PROCEDURE MemAlloc_Allocatable_Scalar
        MODULE PROCEDURE MemAlloc_Allocatable_1D
        MODULE PROCEDURE MemAlloc_Allocatable_2D
        MODULE PROCEDURE MemAlloc_Allocatable_3D
    END INTERFACE
    INTERFACE PolyMemResize
        MODULE PROCEDURE MemResize_Allocatable_1D
        MODULE PROCEDURE MemResize_Allocatable_2D
        MODULE PROCEDURE MemResize_Allocatable_3D
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           FREE-UP MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE MemFree_Allocatable_Scalar(A, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of an allocatable object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    ! check whether A is allocated
    IF (ALLOCATED(A)) THEN
        ! free memory of components of A if applicable
        IF (PRESENT(UserMemFree)) THEN
            CALL UserMemFree(A)
        ELSE
            SELECT TYPE(A)
            CLASS IS (Assignable)
                CALL A%MemFree()
            END SELECT
        END IF
        ! deallocate the array
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('MemFree_Allocatable_Scalar', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE MemFree_Allocatable_Scalar

!******************************************************************************

SUBROUTINE MemFree_Allocatable_1D(A, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of an allocatable array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(:)
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: I

!** FLOW:

    ! check whether A is allocated
    IF (ALLOCATED(A)) THEN
        ! free memory of components of A if applicable
        IF (PRESENT(UserMemFree)) THEN
            DO I = 1, SIZE(A)
                CALL UserMemFree(A(I))
            END DO
        ELSE
            SELECT TYPE(A)
            CLASS IS (Assignable)
                DO I = 1, SIZE(A)
                    CALL A(I)%MemFree()
                END DO
            END SELECT
        END IF
        ! deallocate the array
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('MemFree_Allocatable_1D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE MemFree_Allocatable_1D

!******************************************************************************

SUBROUTINE MemFree_Allocatable_2D(A, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of an allocatable array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(:,:)
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: I, J

!** FLOW:

    ! check whether A is allocated
    IF (ALLOCATED(A)) THEN
        ! free memory of components of A if applicable
        IF (PRESENT(UserMemFree)) THEN
            DO I = 1, SIZE(A, DIM=1)
                DO J = 1, SIZE(A, DIM=2)
                    CALL UserMemFree(A(I,J))
                END DO
            END DO
        ELSE
            SELECT TYPE(A)
            CLASS IS (Assignable)
                DO I = 1, SIZE(A, DIM=1)
                    DO J = 1, SIZE(A, DIM=2)
                        CALL A(I,J)%MemFree()
                    END DO
                END DO
            END SELECT
        END IF
        ! deallocate the array
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('MemFree_Allocatable_1D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE MemFree_Allocatable_2D

!******************************************************************************

SUBROUTINE MemFree_Allocatable_3D(A, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of an allocatable array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(:,:,:)
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: I, J, K

!** FLOW:

    ! check whether A is allocated
    IF (ALLOCATED(A)) THEN
        ! free memory of components of A if applicable
        IF (PRESENT(UserMemFree)) THEN
            DO I = 1, SIZE(A, DIM=1)
                DO J = 1, SIZE(A, DIM=2)
                    DO K = 1, SIZE(A, DIM=3)
                        CALL UserMemFree(A(I,J,K))
                    END DO
                END DO
            END DO
        ELSE
            SELECT TYPE(A)
            CLASS IS (Assignable)
                DO I = 1, SIZE(A, DIM=1)
                    DO J = 1, SIZE(A, DIM=2)
                        DO K = 1, SIZE(A, DIM=3)
                            CALL A(I,J,K)%MemFree()
                        END DO
                    END DO
                END DO
            END SELECT
        END IF
        ! deallocate the array
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('MemFree_Allocatable_1D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE MemFree_Allocatable_3D

!******************************************************************************

SUBROUTINE PolyMemFreePtr(A, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of a pointer object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A
    PROCEDURE(FreeComponents), OPTIONAL :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    ! check whether A is allocated
    IF (ASSOCIATED(A)) THEN
        ! free memory of components of A if applicable
        IF (PRESENT(UserMemFree)) THEN
            CALL UserMemFree(A)
        ELSE
            SELECT TYPE(A)
            CLASS IS (Assignable)
                CALL A%MemFree()
            END SELECT
        END IF
        ! deallocate the array
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('PolyMemFreePtr', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE PolyMemFreePtr

!******************************************************************************

SUBROUTINE PolyMemFreePtr1D(A, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of a pointer array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A(:)
    PROCEDURE(FreeComponents), OPTIONAL :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: I

!** FLOW:

    ! check whether A is allocated
    IF (ASSOCIATED(A)) THEN
        ! free memory of components of A if applicable
        IF (PRESENT(UserMemFree)) THEN
            DO I = 1, SIZE(A)
                CALL UserMemFree(A(I))
            END DO
        ELSE
            SELECT TYPE(A)
            CLASS IS (Assignable)
                DO I = 1, SIZE(A)
                    CALL A(I)%MemFree()
                END DO
            END SELECT
        END IF
        ! deallocate the array
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('PolyMemFreePtr1D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE PolyMemFreePtr1D

!******************************************************************************

SUBROUTINE PolyMemFreePtr2D(A, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of a pointer array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A(:,:)
    PROCEDURE(FreeComponents), OPTIONAL :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: I, J

!** FLOW:

    ! check whether A is allocated
    IF (ASSOCIATED(A)) THEN
        ! free memory of components of A if applicable
        IF (PRESENT(UserMemFree)) THEN
            DO I = 1, SIZE(A, DIM=1)
                DO J = 1, SIZE(A, DIM=2)
                    CALL UserMemFree(A(I,J))
                END DO
            END DO
        ELSE
            SELECT TYPE(A)
            CLASS IS (Assignable)
                DO I = 1, SIZE(A, DIM=1)
                    DO J = 1, SIZE(A, DIM=2)
                        CALL A(I,J)%MemFree()
                    END DO
                END DO
            END SELECT
        END IF
        ! deallocate the array
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('PolyMemFreePtr2D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE PolyMemFreePtr2D

!******************************************************************************

SUBROUTINE PolyMemFreePtr3D(A, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of a pointer array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A(:,:,:)
    PROCEDURE(FreeComponents), OPTIONAL :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: I, J, K

!** FLOW:

    ! check whether A is allocated
    IF (ASSOCIATED(A)) THEN
        ! free memory of components of A if applicable
        IF (PRESENT(UserMemFree)) THEN
            DO I = 1, SIZE(A, DIM=1)
                DO J = 1, SIZE(A, DIM=2)
                    DO K = 1, SIZE(A, DIM=3)
                        CALL UserMemFree(A(I,J,K))
                    END DO
                END DO
            END DO
        ELSE
            SELECT TYPE(A)
            CLASS IS (Assignable)
                DO I = 1, SIZE(A, DIM=1)
                    DO J = 1, SIZE(A, DIM=2)
                        DO K = 1, SIZE(A, DIM=3)
                            CALL A(I,J,K)%MemFree()
                        END DO
                    END DO
                END DO
            END SELECT
        END IF
        ! deallocate the array
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        ! handle error if any
        CALL Handle_ErrDealloc('PolyMemFreePtr3D', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE PolyMemFreePtr3D

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           ALLOCATE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE MemAlloc_Allocatable_Scalar(A, Copy, IsMold, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of an allocatable object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A        ! allocatable object
    CLASS(*),              INTENT(IN)       :: Copy     ! copy providing the concrete type of A
    tLogical,              INTENT(IN)       :: IsMold   ! true if the copy is specified as a mold
                                                        ! false if the copy is specified as a source
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    ! free memory if allocated
    CALL PolyMemFree(A, UserMemFree)

    ! allocate memory
    IF (IsMold) THEN
        ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Copy)
    ELSE
        ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Copy)
    END IF

    ! handle error if any
    CALL Handle_ErrAlloc('MemAlloc_Allocatable_Scalar', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE MemAlloc_Allocatable_Scalar

!******************************************************************************

SUBROUTINE MemAlloc_Allocatable_1D(A, Size, Copy, IsMold, StartID, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of an allocatable array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(:)     ! allocatable array
    tIndex,                INTENT(IN)       :: Size     ! size of the array
    CLASS(*),              INTENT(IN)       :: Copy     ! copy providing the concrete type of A
    tLogical,              INTENT(IN)       :: IsMold   ! true if the copy is specified as a mold
                                                        ! false if the copy is specified as a source
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID  ! starting index of the array
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: BeginID
    tIndex              :: EndID

!** FLOW:

    ! check input validity
    IF (Size < 0) THEN
        CALL Handle_ErrLevel('MemAlloc_Allocatable_1D', ModName, ErrSevere, &
                            '"Size" must not be negative.')
        RETURN
    END IF

    ! set optional input and compute index
    SET_OPTION(BeginID, 1, StartID)
    EndID = BeginID + Size - 1

    ! free memory if allocated
    CALL PolyMemFree(A, UserMemFree)

    ! allocate memory
    IF (IsMold) THEN
        ALLOCATE(A(BeginID:EndID), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Copy)
    ELSE
        ALLOCATE(A(BeginID:EndID), STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Copy)
    END IF

    ! handle error if any
    CALL Handle_ErrAlloc('MemAlloc_Allocatable_1D', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE MemAlloc_Allocatable_1D

!******************************************************************************

SUBROUTINE MemAlloc_Allocatable_2D(A, Size1, Size2, Copy, IsMold, &
                                    StartID1, StartID2, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of an allocatable array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(:,:)   ! allocatable array
    tIndex,                INTENT(IN)       :: Size1    ! first dimension of array
    tIndex,                INTENT(IN)       :: Size2    ! second dimension of array
    CLASS(*),              INTENT(IN)       :: Copy     ! copy providing the concrete type of A
    tLogical,              INTENT(IN)       :: IsMold   ! true if the copy is specified as a mold
                                                        ! false if the copy is specified as a source
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID1 ! starting index of first dimension
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID2 ! starting index of second dimension
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: BeginID1, BeginID2
    tIndex              :: EndID1, EndID2

!** FLOW:

    ! check input validity
    IF ((Size1 < 0).OR.(Size2 < 0)) THEN
        CALL Handle_ErrLevel('MemAlloc_Allocatable_2D', ModName, ErrSevere, &
                            '"Size(s)" must not be negative.')
        RETURN
    END IF

    ! set optional input and compute indices
    SET_OPTION(BeginID1, 1, StartID1)
    SET_OPTION(BeginID2, 1, StartID2)
    EndID1 = BeginID1 + Size1 - 1
    EndID2 = BeginID2 + Size2 - 1

    ! free memory if allocated
    CALL PolyMemFree(A, UserMemFree)

    ! allocate memory
    IF (IsMold) THEN
        ALLOCATE(A(BeginID1:EndID1, BeginID2:EndID2), STAT=AllocStat, &
                    ERRMSG=AllocMsg, MOLD=Copy)
    ELSE
        ALLOCATE(A(BeginID1:EndID1, BeginID2:EndID2), STAT=AllocStat, &
                    ERRMSG=AllocMsg, SOURCE=Copy)
    END IF

    ! handle error if any
    CALL Handle_ErrAlloc('MemAlloc_Allocatable_2D', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE MemAlloc_Allocatable_2D

!******************************************************************************

SUBROUTINE MemAlloc_Allocatable_3D(A, Size1, Size2, Size3, Copy, IsMold, &
                                    StartID1, StartID2, StartID3, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of an allocatable array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(:,:,:) ! allocatable array
    tIndex,                INTENT(IN)       :: Size1    ! first dimension of array
    tIndex,                INTENT(IN)       :: Size2    ! second dimension of array
    tIndex,                INTENT(IN)       :: Size3    ! third dimension of array
    CLASS(*),              INTENT(IN)       :: Copy     ! copy providing the concrete type of A
    tLogical,              INTENT(IN)       :: IsMold   ! true if the copy is specified as a mold
                                                        ! false if the copy is specified as a source
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID1 ! starting index of first dimension
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID2 ! starting index of second dimension
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID3 ! starting index of third dimension
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: BeginID1, BeginID2, BeginID3
    tIndex              :: EndID1, EndID2, EndID3

!** FLOW:

    ! check input validity
    IF ((Size1 < 0).OR.(Size2 < 0).OR.(Size3 < 0)) THEN
        CALL Handle_ErrLevel('MemAlloc_Allocatable_3D', ModName, ErrSevere, &
                            '"Size(s)" must not be negative.')
        RETURN
    END IF

    ! set optional input and compute indices
    SET_OPTION(BeginID1, 1, StartID1)
    SET_OPTION(BeginID2, 1, StartID2)
    SET_OPTION(BeginID3, 1, StartID3)
    EndID1 = BeginID1 + Size1 - 1
    EndID2 = BeginID2 + Size2 - 1
    EndID3 = BeginID3 + Size3 - 1

    ! free memory if allocated
    CALL PolyMemFree(A, UserMemFree)

    ! allocate memory
    IF (IsMold) THEN
        ALLOCATE(A(BeginID1:EndID1, BeginID2:EndID2, BeginID3:EndID3), &
                    STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Copy)
    ELSE
        ALLOCATE(A(BeginID1:EndID1, BeginID2:EndID2, BeginID3:EndID3), &
                    STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Copy)
    END IF

    ! handle error if any
    CALL Handle_ErrAlloc('MemAlloc_Allocatable_3D', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE MemAlloc_Allocatable_3D

!******************************************************************************

SUBROUTINE PolyMemAllocPtr(A, Copy, IsMold, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of an pointer object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A        ! pointer object
    CLASS(*),          INTENT(IN)       :: Copy     ! copy providing the concrete type of A
    tLogical,          INTENT(IN)       :: IsMold   ! true if the copy is specified as a mold
                                                    ! false if the copy is specified as a source
    PROCEDURE(FreeComponents), OPTIONAL :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    ! free memory if allocated
    CALL PolyMemFreePtr(A, UserMemFree)

    ! allocate memory
    IF (IsMold) THEN
        ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Copy)
    ELSE
        ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Copy)
    END IF

    ! handle error if any
    CALL Handle_ErrAlloc('PolyMemAllocPtr', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE PolyMemAllocPtr

!******************************************************************************

SUBROUTINE PolyMemAllocPtr1D(A, Size, Copy, IsMold, StartID, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of a pointer array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),   POINTER,   INTENT(INOUT)    :: A(:)     ! pointer array
    tIndex,                INTENT(IN)       :: Size     ! size of the array
    CLASS(*),              INTENT(IN)       :: Copy     ! copy providing the concrete type of A
    tLogical,              INTENT(IN)       :: IsMold   ! true if the copy is specified as a mold
                                                        ! false if the copy is specified as a source
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID  ! starting index of the array
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: BeginID
    tIndex              :: EndID

!** FLOW:

    ! check input validity
    IF (Size < 0) THEN
        CALL Handle_ErrLevel('PolyMemAllocPtr1D', ModName, ErrSevere, &
                            '"Size" must not be negative.')
        RETURN
    END IF

    ! set optional input and compute index
    SET_OPTION(BeginID, 1, StartID)
    EndID = BeginID + Size - 1

    ! free memory if allocated
    CALL PolyMemFreePtr1D(A, UserMemFree)

    ! allocate memory
    IF (IsMold) THEN
        ALLOCATE(A(BeginID:EndID), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Copy)
    ELSE
        ALLOCATE(A(BeginID:EndID), STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Copy)
    END IF

    ! handle error if any
    CALL Handle_ErrAlloc('PolyMemAllocPtr1D', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE PolyMemAllocPtr1D

!******************************************************************************

SUBROUTINE PolyMemAllocPtr2D(A, Size1, Size2, Copy, IsMold, StartID1, StartID2, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of a pointer array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),   POINTER,   INTENT(INOUT)    :: A(:,:)   ! pointer array
    tIndex,                INTENT(IN)       :: Size1    ! first dimension of array
    tIndex,                INTENT(IN)       :: Size2    ! second dimension of array
    CLASS(*),              INTENT(IN)       :: Copy     ! copy providing the concrete type of A
    tLogical,              INTENT(IN)       :: IsMold   ! true if the copy is specified as a mold
                                                        ! false if the copy is specified as a source
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID1 ! starting index of first dimension
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID2 ! starting index of second dimension
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: BeginID1, BeginID2
    tIndex              :: EndID1, EndID2

!** FLOW:

    ! check input validity
    IF ((Size1 < 0).OR.(Size2 < 0)) THEN
        CALL Handle_ErrLevel('PolyMemAllocPtr2D', ModName, ErrSevere, &
                            '"Size(s)" must not be negative.')
        RETURN
    END IF

    ! set optional input and compute indices
    SET_OPTION(BeginID1, 1, StartID1)
    SET_OPTION(BeginID2, 1, StartID2)
    EndID1 = BeginID1 + Size1 - 1
    EndID2 = BeginID2 + Size2 - 1

    ! free memory if allocated
    CALL PolyMemFreePtr2D(A, UserMemFree)

    ! allocate memory
    IF (IsMold) THEN
        ALLOCATE(A(BeginID1:EndID1, BeginID2:EndID2), STAT=AllocStat, &
                    ERRMSG=AllocMsg, MOLD=Copy)
    ELSE
        ALLOCATE(A(BeginID1:EndID1, BeginID2:EndID2), STAT=AllocStat, &
                    ERRMSG=AllocMsg, SOURCE=Copy)
    END IF

    ! handle error if any
    CALL Handle_ErrAlloc('PolyMemAllocPtr2D', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE PolyMemAllocPtr2D

!******************************************************************************

SUBROUTINE PolyMemAllocPtr3D(A, Size1, Size2, Size3, Copy, IsMold, &
                                    StartID1, StartID2, StartID3, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of a pointer array

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),   POINTER,   INTENT(INOUT)    :: A(:,:,:) ! pointer array
    tIndex,                INTENT(IN)       :: Size1    ! first dimension of array
    tIndex,                INTENT(IN)       :: Size2    ! second dimension of array
    tIndex,                INTENT(IN)       :: Size3    ! third dimension of array
    CLASS(*),              INTENT(IN)       :: Copy     ! copy providing the concrete type of A
    tLogical,              INTENT(IN)       :: IsMold   ! true if the copy is specified as a mold
                                                        ! false if the copy is specified as a source
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID1 ! starting index of first dimension
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID2 ! starting index of second dimension
    tIndex,      OPTIONAL, INTENT(IN)       :: StartID3 ! starting index of third dimension
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: BeginID1, BeginID2, BeginID3
    tIndex              :: EndID1, EndID2, EndID3

!** FLOW:

    ! check input validity
    IF ((Size1 < 0).OR.(Size2 < 0).OR.(Size3 < 0)) THEN
        CALL Handle_ErrLevel('PolyMemAllocPtr3D', ModName, ErrSevere, &
                            '"Size(s)" must not be negative.')
        RETURN
    END IF

    ! set optional input and compute indices
    SET_OPTION(BeginID1, 1, StartID1)
    SET_OPTION(BeginID2, 1, StartID2)
    SET_OPTION(BeginID3, 1, StartID3)
    EndID1 = BeginID1 + Size1 - 1
    EndID2 = BeginID2 + Size2 - 1
    EndID3 = BeginID3 + Size3 - 1

    ! free memory if allocated
    CALL PolyMemFreePtr3D(A, UserMemFree)

    ! allocate memory
    IF (IsMold) THEN
        ALLOCATE(A(BeginID1:EndID1, BeginID2:EndID2, BeginID3:EndID3), &
                    STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Copy)
    ELSE
        ALLOCATE(A(BeginID1:EndID1, BeginID2:EndID2, BeginID3:EndID3), &
                    STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Copy)
    END IF

    ! handle error if any
    CALL Handle_ErrAlloc('PolyMemAllocPtr3D', ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE PolyMemAllocPtr3D

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           RESIZE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE MemResize_Allocatable_1D(A, NewSize, CopyUserData, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To re-allocate array and preserve its data

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(:)         ! allocatable array
    tIndex,                INTENT(IN)       :: NewSize      ! new size of array
    PROCEDURE(CopyArray1D),    OPTIONAL     :: CopyUserData ! optional user-supplied routine
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree  ! optional user-supplied routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: OldSize      ! original size of array
    tIndex                  :: StartID      ! starting index
    tIndex                  :: PSize        ! size of preserved data
    CLASS(*), ALLOCATABLE   :: Temp(:)      ! working variable
    tLogical                :: ErrStat      ! error status

!** FLOW:

    ! check input validity
    IF (NewSize < 0) THEN
        CALL Handle_ErrLevel('MemResize_Allocatable_1D', ModName, ErrSevere, &
                            '"NewSize" must not be negative.')
        RETURN
    END IF

    ! determine the original size and its starting index
    IF (ALLOCATED(A)) THEN
        OldSize = SIZE(A, DIM=1)
        StartID = LBOUND(A, DIM=1)
    ELSE
        OldSize = 0
        StartID = 1
    END IF

    ! re-allocate array and preserve its data
    IF (OldSize == 0) THEN
        ! just allocate the array
        CALL PolyMemAlloc(A, NewSize, A(0), TrueVal, StartID, UserMemFree)
    ELSE
        ! first, allocate the temporary array
        CALL PolyMemAlloc(Temp, NewSize, A(1), TrueVal, StartID, UserMemFree)
        ! determine the preserving size
        IF (NewSize >= OldSize) THEN
            PSize = OldSize
        ELSE
            PSize = NewSize
        END IF
        ! copy data to the temporary array
        ErrStat = PolyCopy(Temp(StartID:PSize+StartID-1), A(StartID:PSize+StartID-1), &
                            PSize, CopyUserData)
        IF (ErrStat) THEN
            ! free up memory
            CALL PolyMemFree(Temp, UserMemFree)
            ! report error
            CALL Handle_ErrLevel('MemResize_Allocatable_1D', ModName, ErrSevere, &
                                'Cannot make a copy of data.')
            RETURN
        END IF
        ! move data from the temporary array back to the array
        CALL PolyMemFree(A, UserMemFree)
        CALL MOVE_ALLOC(Temp, A)
    END IF

    RETURN

END SUBROUTINE MemResize_Allocatable_1D

!**************************************************************************************

SUBROUTINE MemResize_Allocatable_2D(A, NewSize1, NewSize2, CopyUserData, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To re-allocate array and preserve its data

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(:,:)       ! allocatable array
    tIndex,                INTENT(IN)       :: NewSize1     ! new size of the first dimension
    tIndex,                INTENT(IN)       :: NewSize2     ! new size of the second dimension
    PROCEDURE(CopyArray2D),    OPTIONAL     :: CopyUserData ! optional user-supplied routine
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree  ! optional user-supplied routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: OldSize1     ! original size of the first dimension
    tIndex                  :: OldSize2     ! original size of the second dimension
    tIndex                  :: StartID1     ! starting index of the first dimension
    tIndex                  :: StartID2     ! starting index of the second dimension
    tIndex                  :: EndID1       ! ending index of the first dimension
    tIndex                  :: EndID2       ! ending index of the second dimension
    tIndex                  :: PSize1       ! size of preserved data of the first dimension
    tIndex                  :: PSize2       ! size of preserved data of the second dimension
    CLASS(*), ALLOCATABLE   :: Temp(:,:)    ! working variable
    tLogical                :: ErrStat      ! error status

!** FLOW:

    ! check input validity
    IF ((NewSize1 < 0).OR.(NewSize2 < 0)) THEN
        CALL Handle_ErrLevel('MemResize_Allocatable_2D', ModName, ErrSevere, &
                            '"NewSize(s)" must not be negative.')
        RETURN
    END IF

    ! determine the original size and its starting indices
    IF (ALLOCATED(A)) THEN
        OldSize1 = SIZE(A, DIM=1)
        OldSize2 = SIZE(A, DIM=2)
        StartID1 = LBOUND(A, DIM=1)
        StartID2 = LBOUND(A, DIM=2)
    ELSE
        OldSize1 = 0
        OldSize2 = 0
        StartID1 = 1
        StartID2 = 1
    END IF

    ! re-allocate array and preserve its data
    IF ((OldSize1 == 0).OR.(OldSize2 == 0)) THEN
        ! just allocate the array
        CALL PolyMemAlloc(A, NewSize1, NewSize2, A(0,0), TrueVal, &
                            StartID1, StartID2, UserMemFree)
    ELSE
        ! first, allocate the temporary array
        CALL PolyMemAlloc(Temp, NewSize1, NewSize2, A(1,1), TrueVal, &
                            StartID1, StartID2, UserMemFree)
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
        ! compute ending indices
        EndID1 = PSize1 + StartID1 - 1
        EndID2 = PSize2 + StartID2 - 1
        ! copy data to the temporary array
        ErrStat = PolyCopy(Temp(StartID1:EndID1,StartID2:EndID2), &
                            A(StartID1:EndID1,StartID2:EndID2),    &
                            PSize1, PSize2, CopyUserData)
        IF (ErrStat) THEN
            ! free up memory
            CALL PolyMemFree(Temp, UserMemFree)
            ! report error
            CALL Handle_ErrLevel('MemResize_Allocatable_2D', ModName, ErrSevere, &
                                'Cannot make a copy of data.')
            RETURN
        END IF
        ! move data from the temporary array back to the array
        CALL PolyMemFree(A, UserMemFree)
        CALL MOVE_ALLOC(Temp, A)
    END IF

    RETURN

END SUBROUTINE MemResize_Allocatable_2D

!**************************************************************************************

SUBROUTINE MemResize_Allocatable_3D(A, NewSize1, NewSize2, NewSize3, CopyUserData, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To re-allocate array and preserve its data

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(:,:,:)     ! allocatable array
    tIndex,                INTENT(IN)       :: NewSize1     ! new size of the first dimension
    tIndex,                INTENT(IN)       :: NewSize2     ! new size of the second dimension
    tIndex,                INTENT(IN)       :: NewSize3     ! new size of the third dimension
    PROCEDURE(CopyArray3D),    OPTIONAL     :: CopyUserData ! optional user-supplied routine
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree  ! optional user-supplied routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: OldSize1     ! original size of the first dimension
    tIndex                  :: OldSize2     ! original size of the second dimension
    tIndex                  :: OldSize3     ! original size of the third dimension
    tIndex                  :: StartID1     ! starting index of the first dimension
    tIndex                  :: StartID2     ! starting index of the second dimension
    tIndex                  :: StartID3     ! starting index of the third dimension
    tIndex                  :: EndID1       ! ending index of the first dimension
    tIndex                  :: EndID2       ! ending index of the second dimension
    tIndex                  :: EndID3       ! ending index of the third dimension
    tIndex                  :: PSize1       ! size of preserved data of the first dimension
    tIndex                  :: PSize2       ! size of preserved data of the second dimension
    tIndex                  :: PSize3       ! size of preserved data of the third dimension
    CLASS(*), ALLOCATABLE   :: Temp(:,:,:)  ! working variable
    tLogical                :: ErrStat      ! error status

!** FLOW:

    ! check input validity
    IF ((NewSize1 < 0).OR.(NewSize2 < 0).OR.(NewSize3 < 0)) THEN
        CALL Handle_ErrLevel('MemResize_Allocatable_3D', ModName, ErrSevere, &
                            '"NewSize(s)" must not be negative.')
        RETURN
    END IF

    ! determine the original size and its starting indices
    IF (ALLOCATED(A)) THEN
        OldSize1 = SIZE(A, DIM=1)
        OldSize2 = SIZE(A, DIM=2)
        OldSize3 = SIZE(A, DIM=3)
        StartID1 = LBOUND(A, DIM=1)
        StartID2 = LBOUND(A, DIM=2)
        StartID3 = LBOUND(A, DIM=3)
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
        CALL PolyMemAlloc(A, NewSize1, NewSize2, NewSize3, A(0,0,0), &
                            TrueVal, StartID1, StartID2, StartID3, UserMemFree)
    ELSE
        ! first, allocate the temporary array
        CALL PolyMemAlloc(Temp, NewSize1, NewSize2, NewSize3, A(1,1,1), &
                            TrueVal, StartID1, StartID2, StartID3, UserMemFree)
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
        ! compute ending indices
        EndID1 = PSize1 + StartID1 - 1
        EndID2 = PSize2 + StartID2 - 1
        EndID3 = PSize3 + StartID3 - 1
        ! copy data to the temporary array
        ErrStat = PolyCopy(Temp(StartID1:EndID1,StartID2:EndID2,StartID3:EndID3), &
                            A(StartID1:EndID1,StartID2:EndID2,StartID3:EndID3),    &
                            PSize1, PSize2, PSize3, CopyUserData)
        IF (ErrStat) THEN
            ! free up memory
            CALL PolyMemFree(Temp, UserMemFree)
            ! report error
            CALL Handle_ErrLevel('MemResize_Allocatable_3D', ModName, ErrSevere, &
                                'Cannot make a copy of data.')
            RETURN
        END IF
        ! move data from the temporary array back to the array
        CALL PolyMemFree(A, UserMemFree)
        CALL MOVE_ALLOC(Temp, A)
    END IF

    RETURN

END SUBROUTINE MemResize_Allocatable_3D

!**************************************************************************************

SUBROUTINE PolyMemResizePtr1D(A, NewSize, CopyUserData, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To re-allocate array and preserve its data

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER,      INTENT(INOUT)   :: A(:)         ! pointer array
    tIndex,                 INTENT(IN)      :: NewSize      ! new size of array
    PROCEDURE(CopyArray1D),    OPTIONAL     :: CopyUserData ! optional user-supplied routine
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree  ! optional user-supplied routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: OldSize      ! original size of array
    tIndex              :: StartID      ! starting index
    tIndex              :: PSize        ! size of preserved data
    CLASS(*), POINTER   :: Temp(:)      ! working variable
    tLogical            :: ErrStat      ! error status

!** FLOW:

    ! check input validity
    IF (NewSize < 0) THEN
        CALL Handle_ErrLevel('PolyMemResizePtr1D', ModName, ErrSevere, &
                            '"NewSize" must not be negative.')
        RETURN
    END IF

    ! determine the original size and its starting index
    IF (ASSOCIATED(A)) THEN
        OldSize = SIZE(A, DIM=1)
        StartID = LBOUND(A, DIM=1)
    ELSE
        OldSize = 0
        StartID = 1
    END IF

    ! re-allocate array and preserve its data
    IF (OldSize == 0) THEN
        ! just allocate the array
        CALL PolyMemAllocPtr1D(A, NewSize, A(0), TrueVal, StartID, UserMemFree)

    ELSE
        ! first, allocate the temporary array
        CALL PolyMemAllocPtr1D(Temp, NewSize, A(1), TrueVal, StartID, UserMemFree)
        ! determine the preserving size
        IF (NewSize >= OldSize) THEN
            PSize = OldSize
        ELSE
            PSize = NewSize
        END IF
        ! copy data to the temporary array
        ErrStat = PolyCopy(Temp(StartID:PSize+StartID-1), A(StartID:PSize+StartID-1), &
                            PSize, CopyUserData)
        IF (ErrStat) THEN
            ! free up memory
            CALL PolyMemFreePtr1D(Temp, UserMemFree)
            ! report error
            CALL Handle_ErrLevel('PolyMemResizePtr1D', ModName, ErrSevere, &
                                'Cannot make a copy of data.')
            RETURN
        END IF
        ! move data from the temporary array back to the array
        CALL PolyMemFreePtr1D(A, UserMemFree)
        A => Temp
        NULLIFY(Temp)
    END IF

    RETURN

END SUBROUTINE PolyMemResizePtr1D

!**************************************************************************************

SUBROUTINE PolyMemResizePtr2D(A, NewSize1, NewSize2, CopyUserData, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To re-allocate array and preserve its data

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER,      INTENT(INOUT)   :: A(:,:)       ! pointer array
    tIndex,                 INTENT(IN)      :: NewSize1     ! new size of the first dimension
    tIndex,                 INTENT(IN)      :: NewSize2     ! new size of the second dimension
    PROCEDURE(CopyArray2D),    OPTIONAL     :: CopyUserData ! optional user-supplied routine
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree  ! optional user-supplied routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: OldSize1     ! original size of the first dimension
    tIndex              :: OldSize2     ! original size of the second dimension
    tIndex              :: StartID1     ! starting index of the first dimension
    tIndex              :: StartID2     ! starting index of the second dimension
    tIndex              :: EndID1       ! ending index of the first dimension
    tIndex              :: EndID2       ! ending index of the second dimension
    tIndex              :: PSize1       ! size of preserved data of the first dimension
    tIndex              :: PSize2       ! size of preserved data of the second dimension
    CLASS(*), POINTER   :: Temp(:,:)    ! working variable
    tLogical            :: ErrStat      ! error status

!** FLOW:

    ! check input validity
    IF ((NewSize1 < 0).OR.(NewSize2 < 0)) THEN
        CALL Handle_ErrLevel('PolyMemResizePtr2D', ModName, ErrSevere, &
                            '"NewSize(s)" must not be negative.')
        RETURN
    END IF

    ! determine the original size and its starting indices
    IF (ASSOCIATED(A)) THEN
        OldSize1 = SIZE(A, DIM=1)
        OldSize2 = SIZE(A, DIM=2)
        StartID1 = LBOUND(A, DIM=1)
        StartID2 = LBOUND(A, DIM=2)
    ELSE
        OldSize1 = 0
        OldSize2 = 0
        StartID1 = 1
        StartID2 = 1
    END IF

    ! re-allocate array and preserve its data
    IF ((OldSize1 == 0).OR.(OldSize2 == 0)) THEN
        ! just allocate the array
        CALL PolyMemAllocPtr2D(A, NewSize1, NewSize2, A(0,0), TrueVal, &
                            StartID1, StartID2, UserMemFree)
    ELSE
        ! first, allocate the temporary array
        CALL PolyMemAllocPtr2D(Temp, NewSize1, NewSize2, A(1,1), TrueVal, &
                            StartID1, StartID2, UserMemFree)

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
        ! compute ending indices
        EndID1 = PSize1 + StartID1 - 1
        EndID2 = PSize2 + StartID2 - 1
        ! copy data to the temporary array
        ErrStat = PolyCopy(Temp(StartID1:EndID1,StartID2:EndID2), &
                            A(StartID1:EndID1,StartID2:EndID2),    &
                            PSize1, PSize2, CopyUserData)
        IF (ErrStat) THEN
            ! free up memory
            CALL PolyMemFreePtr2D(Temp, UserMemFree)
            ! report error
            CALL Handle_ErrLevel('PolyMemResizePtr2D', ModName, ErrSevere, &
                                'Cannot make a copy of data.')
            RETURN
        END IF
        ! move data from the temporary array back to the array
        CALL PolyMemFreePtr2D(A, UserMemFree)
        A => Temp
        NULLIFY(Temp)
    END IF

    RETURN

END SUBROUTINE PolyMemResizePtr2D

!**************************************************************************************

SUBROUTINE PolyMemResizePtr3D(A, NewSize1, NewSize2, NewSize3, CopyUserData, UserMemFree)

!** PURPOSE OF THIS SUBROUTINE:
    ! To re-allocate array and preserve its data

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER,      INTENT(INOUT)   :: A(:,:,:)     ! pointer array
    tIndex,                 INTENT(IN)      :: NewSize1     ! new size of the first dimension
    tIndex,                 INTENT(IN)      :: NewSize2     ! new size of the second dimension
    tIndex,                 INTENT(IN)      :: NewSize3     ! new size of the third dimension
    PROCEDURE(CopyArray3D),    OPTIONAL     :: CopyUserData ! optional user-supplied routine
    PROCEDURE(FreeComponents), OPTIONAL     :: UserMemFree  ! optional user-supplied routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: OldSize1     ! original size of the first dimension
    tIndex              :: OldSize2     ! original size of the second dimension
    tIndex              :: OldSize3     ! original size of the third dimension
    tIndex              :: StartID1     ! starting index of the first dimension
    tIndex              :: StartID2     ! starting index of the second dimension
    tIndex              :: StartID3     ! starting index of the third dimension
    tIndex              :: EndID1       ! ending index of the first dimension
    tIndex              :: EndID2       ! ending index of the second dimension
    tIndex              :: EndID3       ! ending index of the third dimension
    tIndex              :: PSize1       ! size of preserved data of the first dimension
    tIndex              :: PSize2       ! size of preserved data of the second dimension
    tIndex              :: PSize3       ! size of preserved data of the third dimension
    CLASS(*), POINTER   :: Temp(:,:,:)  ! working variable
    tLogical            :: ErrStat      ! error status

!** FLOW:

    ! check input validity
    IF ((NewSize1 < 0).OR.(NewSize2 < 0).OR.(NewSize3 < 0)) THEN
        CALL Handle_ErrLevel('PolyMemResizePtr3D', ModName, ErrSevere, &
                            '"NewSize(s)" must not be negative.')
        RETURN
    END IF

    ! determine the original size and its starting indices
    IF (ASSOCIATED(A)) THEN
        OldSize1 = SIZE(A, DIM=1)
        OldSize2 = SIZE(A, DIM=2)
        OldSize3 = SIZE(A, DIM=3)
        StartID1 = LBOUND(A, DIM=1)
        StartID2 = LBOUND(A, DIM=2)
        StartID3 = LBOUND(A, DIM=3)
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
        CALL PolyMemAllocPtr3D(A, NewSize1, NewSize2, NewSize3, A(0,0,0), &
                            TrueVal, StartID1, StartID2, StartID3, UserMemFree)
    ELSE
        ! first, allocate the temporary array
        CALL PolyMemAllocPtr3D(Temp, NewSize1, NewSize2, NewSize3, A(1,1,1), &
                            TrueVal, StartID1, StartID2, StartID3, UserMemFree)
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
        ! compute ending indices
        EndID1 = PSize1 + StartID1 - 1
        EndID2 = PSize2 + StartID2 - 1
        EndID3 = PSize3 + StartID3 - 1
        ! copy data to the temporary array
        ErrStat = PolyCopy(Temp(StartID1:EndID1,StartID2:EndID2,StartID3:EndID3), &
                            A(StartID1:EndID1,StartID2:EndID2,StartID3:EndID3),    &
                            PSize1, PSize2, PSize3, CopyUserData)
        IF (ErrStat) THEN
            ! free up memory
            CALL PolyMemFreePtr3D(Temp, UserMemFree)
            ! report error
            CALL Handle_ErrLevel('PolyMemResizePtr3D', ModName, ErrSevere, &
                                'Cannot make a copy of data.')
            RETURN
        END IF
        ! move data from the temporary array back to the array
        CALL PolyMemFreePtr3D(A, UserMemFree)
        A => Temp
        NULLIFY(Temp)
    END IF

    RETURN

END SUBROUTINE PolyMemResizePtr3D

!**************************************************************************************

END MODULE ModBase_PolyMemHandlers

!******************************************************************************
