
MODULE ModBase_PolyMemHandlers

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains memory-handling routines for unlimited polymorphic entities.
!   The specified entity can have either *ALLOCATABLE* or *POINTER* attribute and
!   its rank can be any number between 0 and 7. <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers
    USE ModBase_PolyCommon,     ONLY: CopyData

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! memory-handling interfaces
    PUBLIC  :: MemFree
    PUBLIC  :: MemAlloc
    PUBLIC  :: MemResize
    PUBLIC  :: MemFreePtr
    PUBLIC  :: MemAllocPtr
    PUBLIC  :: MemResizePtr
    ! memory-handling actual procedures
    PUBLIC  :: MemFree_Allocatable
    PUBLIC  :: MemAlloc_Allocatable
    PUBLIC  :: MemResize_Allocatable
    PUBLIC  :: MemFree_Pointer
    PUBLIC  :: MemAlloc_Pointer
    PUBLIC  :: MemResize_Pointer

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
    INTERFACE MemFree
        !^ **Subroutine Interface**: MemFree <br>
        !  **Purpose**:  To free memory of the specified argument, which is declared as
        !       an unlimited polymorphic entity with either *ALLOCATABLE* attribute. The
        !       specified argument can be a scalar argument or an array argument with
        !       any rank. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFree(A) <br>
        MODULE PROCEDURE MemFree_Allocatable
    END INTERFACE
    INTERFACE MemAlloc
        !^ **Subroutine Interface**: MemAlloc <br>
        !  **Purpose**:  To allocate memory of the specified argument, which is declared as
        !       an unlimited polymorphic entity with *ALLOCATABLE*  attribute.  The specified
        !       argument can be a scalar argument or an array argument with the rank between
        !       1 and 7.  The procedure also requires the *Prototype* argument to be used as
        !       a mold or a source depending on the present of the optional *ASize* argument. <br>
        !  **Usage**: <br>
        !   ! allocate A using Prototype as a source <br>
        !   --->    CALL MemAlloc(A, Prototype) <br>
        !   ! allocate A using Prototype as a mold with starting indices of 1 <br>
        !   --->    CALL MemAlloc(A, Prototype, ASize) <br>
        !   ! allocate A using Prototype as a mold with specified starting indices <br>
        !   --->    CALL MemAlloc(A, Prototype, ASize, StartID) <br>
        MODULE PROCEDURE MemAlloc_Allocatable
    END INTERFACE
    INTERFACE MemResize
        !^ **Subroutine Interface**: MemResize <br>
        !  **Purpose**:  To re-allocate memory of the specified argument (and preserve its data),
        !       which is declared as an unlimited polymorphic entity with *ALLOCATABLE* attribute.
        !       The specified argument must have already been allocated and it must be an array
        !       argument with the rank between 1 and 7.  The procedure requires the *NewSize*
        !       argument to specify sizes in each dimension of the entity.  It also requires a
        !       procedure to make a copy of the entity as an argument. <br>
        !  **Usage**: <br>
        !   --->    CALL MemResize(A, NewSize, CopyProc) <br>
        !  **Note**: If the concrete type of the entity is one of Fortran intrinsic types, the user
        !       can use one of the auxiliary procedures (e.g. *CopyData_Integer* or *CopyData_Real*)
        !       provided in this *ModBase_PolyCommon* module.  If the type of the entity is a derived
        !       one, the user must provide a user-written routine for that specific derived type.
        MODULE PROCEDURE MemResize_Allocatable
    END INTERFACE
    INTERFACE MemFreePtr
        !^ **Subroutine Interface**: MemFreePtr <br>
        !  **Purpose**:  To free memory of the specified argument, which is declared as
        !       an unlimited polymorphic entity with either *POINTER* attribute. The
        !       specified argument can be a scalar argument or an array argument with
        !       any rank. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFreePtr(A) <br>
        MODULE PROCEDURE MemFree_Pointer
    END INTERFACE
    INTERFACE MemAllocPtr
        !^ **Subroutine Interface**: MemAllocPtr <br>
        !  **Purpose**:  To allocate memory of the specified argument, which is declared as
        !       an unlimited polymorphic entity with *POINTER*  attribute.  The specified
        !       argument can be a scalar argument or an array argument with the rank between
        !       1 and 7.  The procedure also requires the *Prototype* argument to be used as
        !       a mold or a source depending on the present of the optional *ASize* argument. <br>
        !  **Usage**: <br>
        !   ! allocate A using Prototype as a source <br>
        !   --->    CALL MemAllocPtr(A, Prototype) <br>
        !   ! allocate A using Prototype as a mold with starting indices of 1 <br>
        !   --->    CALL MemAllocPtr(A, Prototype, ASize) <br>
        !   ! allocate A using Prototype as a mold with specified starting indices <br>
        !   --->    CALL MemAllocPtr(A, Prototype, ASize, StartID) <br>
        MODULE PROCEDURE MemAlloc_Pointer
    END INTERFACE
    INTERFACE MemResizePtr
        !^ **Subroutine Interface**: MemResize <br>
        !  **Purpose**:  To re-allocate memory of the specified argument (and preserve its data),
        !       which is declared as an unlimited polymorphic entity with *POINTER* attribute.
        !       The specified argument must have already been allocated and it must be an array
        !       argument with the rank between 1 and 7.  The procedure requires the *NewSize*
        !       argument to specify sizes in each dimension of the entity.  It also requires a
        !       procedure to make a copy of the entity as an argument. <br>
        !  **Usage**: <br>
        !   --->    CALL MemResizePtr(A, NewSize, CopyProc) <br>
        !  **Note**: If the concrete type of the entity is one of Fortran intrinsic types, the user
        !       can use one of the auxiliary procedures (e.g. *CopyData_Integer* or *CopyData_Real*)
        !       provided in this *ModBase_PolyCommon* module.  If the type of the entity is a derived
        !       one, the user must provide a user-written routine for that specific derived type.
        MODULE PROCEDURE MemResize_Pointer
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           FREE-UP MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE MemFree_Allocatable(A)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of an allocatable entity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(..)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    IF (ALLOCATED(A)) THEN
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        CALL Handle_ErrDealloc('MemFree_Allocatable', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE MemFree_Allocatable

!******************************************************************************

SUBROUTINE MemFree_Pointer(A)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of a pointer entity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A(..)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    IF (ASSOCIATED(A)) THEN
        DEALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg)
        SELECT RANK (A)
        RANK (0); NULLIFY(A)
        RANK (1); NULLIFY(A)
        RANK (2); NULLIFY(A)
        RANK (3); NULLIFY(A)
        RANK (4); NULLIFY(A)
        RANK (5); NULLIFY(A)
        RANK (6); NULLIFY(A)
        RANK (7); NULLIFY(A)
        END SELECT
        CALL Handle_ErrDealloc('MemFree_Pointer', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE MemFree_Pointer

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           ALLOCATE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE MemAlloc_Allocatable(A, Prototype, ASize, StartID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of an allocatable entity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(..)
    !^ An unlimited polymorphic allocatable entity with an assumed rank.  Currently,
    !  the procedure can handle the entity with the rank between 0 and 7.
    CLASS(*),              INTENT(IN)       :: Prototype(..)
    !^ A copy providing the concrete type of *A*. <br>
    !  If ASize is NOT present, *Prototype* is used as a source for an allocation of *A*.
    !  In this case, the actual ranks of *A* and *Prototype* MUST be the same, which
    !  can be any between 0 and 7. <br>
    !  Otherwise, *Prototype* is used as a mold for an allocation of *A*.
    !  In this case, the rank of *Prototype* can be any between 0 and 7. <br>
    tIndex,   OPTIONAL,    INTENT(IN)       :: ASize(RANK(A))
    !^ An array specifying sizes of A in each dimension.
    !  All its element values must be non-negative.
    tIndex,   OPTIONAL,    INTENT(IN)       :: StartID(RANK(A))
    !^ An array specifying starting indices of A in each dimension.
    !  If *ASize* is NOT present, the *StartID* is ignored.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tLogical            :: RankMisMatch
    tLogical            :: Handled

!** FLOW:

    ! free memory if allocated
    CALL MemFree(A)

    IF (.NOT.PRESENT(ASize)) THEN
        ! initialize error flags
        Handled = TrueVal
        RankMisMatch = FalseVal
        SELECT RANK (A)
        RANK (0)
            SELECT RANK (Prototype)
            RANK (0)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (1)
            SELECT RANK (Prototype)
            RANK (1)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (2)
            SELECT RANK (Prototype)
            RANK (2)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (3)
            SELECT RANK (Prototype)
            RANK (3)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (4)
            SELECT RANK (Prototype)
            RANK (4)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (5)
            SELECT RANK (Prototype)
            RANK (5)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (6)
            SELECT RANK (Prototype)
            RANK (6)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (7)
            SELECT RANK (Prototype)
            RANK (7)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK DEFAULT
            Handled = FalseVal
        END SELECT
        ! handle error(s)
        IF (Handled) THEN
            IF (RankMisMatch) THEN
                CALL Handle_ErrLevel('MemAlloc_Allocatable', ModName, ErrSevere, &
                    'Ranks of "A" and "Prototype" must be the same.')
            ELSE
                CALL Handle_ErrAlloc('MemAlloc_Allocatable', ModName, AllocMsg, AllocStat)
            END IF
        ELSE
            CALL Handle_ErrLevel('MemAlloc_Allocatable', ModName, ErrSevere, &
                'Currently, only handle a pointer entity with the rank between 0 and 7.')
        END IF
    ELSE
        BLOCK
            tIndex      :: I, ARank
            tIndex      :: BeginID(RANK(A)), EndID(RANK(A))
            tLogical    :: ValidSize
            ! first check whether the specified ASize is valid
            ! and if so, compute start and ending indices
            ARank = RANK(A)
            IF (ARank > 0_kIndex) THEN
                ValidSize = TrueVal
                DO I = 1_kIndex, ARank
                    IF (ASize(I) < 0_kIndex) THEN
                        ValidSize = FalseVal
                        EXIT
                    END IF
                END DO
                IF (.NOT.ValidSize) THEN
                    CALL Handle_ErrLevel('MemAlloc_Allocatable', ModName, ErrSevere, &
                        'The specified "ASize" is NOT valid.  All its values must be non-negative.')
                    RETURN
                END IF
                IF (PRESENT(StartID)) THEN
                    BeginID = StartID
                    EndID = BeginID + ASize - 1_kIndex
                ELSE
                    BeginID = 1_kIndex
                    EndID = ASize
                END IF
            END IF
            ! initialize error flags
            Handled = TrueVal
            SELECT RANK(A)
            RANK(0)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(1)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(2)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(3)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(4)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(5)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(6)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(7)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK DEFAULT
                Handled = FalseVal
            END SELECT
        END BLOCK
        IF (Handled) THEN
            CALL Handle_ErrAlloc('MemAlloc_Allocatable', ModName, AllocMsg, AllocStat)
        ELSE
            CALL Handle_ErrLevel('MemAlloc_Allocatable', ModName, ErrSevere, &
                'Currently, only handle entities with the rank between 0 and 7.')
        END IF
    END IF

    RETURN

END SUBROUTINE MemAlloc_Allocatable

!******************************************************************************

SUBROUTINE MemAlloc_Pointer(A, Prototype, ASize, StartID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of an pointer entity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A(..)
    !^ An unlimited polymorphic pointer entity with an assumed rank.  Currently,
    !  the procedure can handle the entity with the rank between 0 and 7.
    CLASS(*),          INTENT(IN)       :: Prototype(..)
    !^ A copy providing the concrete type of *A*. <br>
    !  If ASize is NOT present, *Prototype* is used as a source for an allocation of *A*.
    !  In this case, the actual ranks of *A* and *Prototype* MUST be the same, which
    !  can be any between 0 and 7. <br>
    !  Otherwise, *Prototype* is used as a mold for an allocation of *A*.
    !  In this case, the rank of *Prototype* can be any between 0 and 7. <br>
    tIndex,  OPTIONAL, INTENT(IN)       :: ASize(RANK(A))
    !^ An array specifying sizes of A in each dimension.
    tIndex,  OPTIONAL, INTENT(IN)       :: StartID(RANK(A))
    !^ An array specifying starting indices of A in each dimension.
    !  If *ASize* is NOT present, the *StartID* is ignored.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tLogical            :: RankMisMatch
    tLogical            :: Handled

!** FLOW:

    ! free memory if allocated
    CALL MemFreePtr(A)

    IF (.NOT.PRESENT(ASize)) THEN
        ! initialize error flags
        Handled = TrueVal
        RankMisMatch = FalseVal
        SELECT RANK (A)
        RANK (0)
            SELECT RANK (Prototype)
            RANK (0)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (1)
            SELECT RANK (Prototype)
            RANK (1)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (2)
            SELECT RANK (Prototype)
            RANK (2)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (3)
            SELECT RANK (Prototype)
            RANK (3)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (4)
            SELECT RANK (Prototype)
            RANK (4)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (5)
            SELECT RANK (Prototype)
            RANK (5)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (6)
            SELECT RANK (Prototype)
            RANK (6)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK (7)
            SELECT RANK (Prototype)
            RANK (7)
                ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Prototype)
            RANK DEFAULT
                RankMisMatch = TrueVal
            END SELECT
        RANK DEFAULT
            Handled = FalseVal
        END SELECT
        ! handle error(s)
        IF (Handled) THEN
            IF (RankMisMatch) THEN
                CALL Handle_ErrLevel('MemAlloc_Pointer', ModName, ErrSevere, &
                    'Ranks of "A" and "Prototype" must be the same.')
            ELSE
                CALL Handle_ErrAlloc('MemAlloc_Pointer', ModName, AllocMsg, AllocStat)
            END IF
        ELSE
            CALL Handle_ErrLevel('MemAlloc_Pointer', ModName, ErrSevere, &
                'Currently, only handle a pointer entity with the rank between 0 and 7.')
        END IF
    ELSE
        BLOCK
            tIndex      :: I, ARank
            tIndex      :: BeginID(RANK(A)), EndID(RANK(A))
            tLogical    :: ValidSize
            ! first check whether the specified ASize is valid
            ! and if so, compute start and ending indices
            ARank = RANK(A)
            IF (ARank > 0_kIndex) THEN
                ValidSize = TrueVal
                DO I = 1_kIndex, ARank
                    IF (ASize(I) < 0_kIndex) THEN
                        ValidSize = FalseVal
                        EXIT
                    END IF
                END DO
                IF (.NOT.ValidSize) THEN
                    CALL Handle_ErrLevel('MemAlloc_Pointer', ModName, ErrSevere, &
                        'The specified "ASize" is NOT valid.  All its values must be non-negative.')
                    RETURN
                END IF
                IF (PRESENT(StartID)) THEN
                    BeginID = StartID
                    EndID = BeginID + ASize - 1_kIndex
                ELSE
                    BeginID = 1_kIndex
                    EndID = ASize
                END IF
            END IF
            ! initialize error flags
            Handled = TrueVal
            SELECT RANK(A)
            RANK(0)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(1)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(2)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(3)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(4)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(5)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(6)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                             STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK(7)
                SELECT RANK(Prototype)
                RANK (0)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (1)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (2)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (3)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (4)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (5)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (6)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK (7)
                    ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                               BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                               BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
                RANK DEFAULT
                    Handled = FalseVal
                END SELECT
            RANK DEFAULT
                Handled = FalseVal
            END SELECT
        END BLOCK
        IF (Handled) THEN
            CALL Handle_ErrAlloc('MemAlloc_Pointer', ModName, AllocMsg, AllocStat)
        ELSE
            CALL Handle_ErrLevel('MemAlloc_Pointer', ModName, ErrSevere, &
                'Currently, only handle entities with the rank between 0 and 7.')
        END IF
    END IF

    RETURN

END SUBROUTINE MemAlloc_Pointer

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           RESIZE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE MemResize_Allocatable(A, NewSize, MakeCopy)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-allocate memory of an allocatable entity and preserve its data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(..)
    !^ An unlimited polymorphic allocatable entity with an assumed rank.  Currently,
    !  the procedure can handle the entity with the rank between 1 and 7.
    tIndex,                INTENT(IN)       :: NewSize(RANK(A))
    !^ An array specifying new sizes of A in each dimension.
    !  All its element values must be non-negative.
    PROCEDURE(CopyData)                     :: MakeCopy
    !^ A procedure to make a copy of A.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical            :: Handled
    tLogical            :: ZeroSize
    tIndex              :: I, ARank
    tIndex              :: BeginID(RANK(A)), EndID(RANK(A))
    tIndex              :: OldSize(RANK(A)), PSize(RANK(A))

!** FLOW:

    ! check whether A has been allocated or not.
    ! if not, it cannot be used as a mold to re-allocate its self
    IF (.NOT.ALLOCATED(A)) THEN
        CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
            '"A" has not yet been allocated.  Use the "MemAlloc" procedure instead.')
        RETURN
    END IF

    ! first check validity of A's rank and values of elements of NewSize
    ARank = RANK(A)
    IF (ARank > 0_kIndex) THEN
        BLOCK
            tLogical    :: ValidSize
            ValidSize = TrueVal
            DO I = 1_kIndex, ARank
                IF (NewSize(I) < 0_kIndex) THEN
                    ValidSize = FalseVal
                    EXIT
                END IF
            END DO
            IF (.NOT.ValidSize) THEN
                CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
                    'The specified "NewSize" is NOT valid.  All its values must be non-negative.')
                RETURN
            END IF
        END BLOCK
    ELSE
        CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
            'An entity with rank of 0 cannot be resized.')
        RETURN
    END IF

    ZeroSize = FalseVal
    SELECT RANK (A)
    RANK (1)
        OldSize(1) = SIZE(A, DIM=I, KIND=kIndex)
        BeginID(1) = LBOUND(A, DIM=I, KIND=kIndex)
        IF (OldSize(1) < 1_kIndex) ZeroSize = TrueVal
    RANK (2)
        DO I = 1_kIndex, 2_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (3)
        DO I = 1_kIndex, 3_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (4)
        DO I = 1_kIndex, 4_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (5)
        DO I = 1_kIndex, 5_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (6)
        DO I = 1_kIndex, 6_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (7)
        DO I = 1_kIndex, 4_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK DEFAULT
        Handled = FalseVal
    END SELECT

    IF (ZeroSize) THEN
        ! one or more of A's dimensions has zero size; therefore,
        ! simply allocate A to new sizes
        CALL MemAlloc(A, A, NewSize, BeginID)
    ELSE
        Handled = TrueVal
        SELECT RANK (A)
        RANK (1)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:)
                tLogical                :: ErrStat
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1)), A(BeginID(1):EndID(1)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (2)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:)
                tLogical                :: ErrStat
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (3)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:)
                tLogical                :: ErrStat
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (4)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:,:)
                tLogical                :: ErrStat
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2), &
                                       BeginID(3):EndID(3),BeginID(4):EndID(4)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2), &
                                       BeginID(3):EndID(3),BeginID(4):EndID(4)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (5)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:,:,:)
                tLogical                :: ErrStat
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (6)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:,:,:,:)
                tLogical                :: ErrStat
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (7)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:,:,:,:,:)
                tLogical                :: ErrStat
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6), &
                                       BeginID(7):EndID(7)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6), &
                                       BeginID(7):EndID(7)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK DEFAULT
            Handled = FalseVal
        END SELECT
        IF (.NOT.Handled) THEN
            CALL Handle_ErrLevel('MemResize_Allocatable', ModName, ErrSevere, &
                'Currently, only handle an entity with the rank between 1 and 7.')
        END IF
    END IF

    RETURN

END SUBROUTINE MemResize_Allocatable

!******************************************************************************

SUBROUTINE MemResize_Pointer(A, NewSize, MakeCopy)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-allocate memory of an pointer entity and preserve its data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A(..)
    !^ An unlimited polymorphic pointer entity with an assumed rank.  Currently,
    !  the procedure can handle the entity with the rank between 1 and 7.
    tIndex,            INTENT(IN)       :: NewSize(RANK(A))
    !^ An array specifying new sizes of A in each dimension.
    !  All its element values must be non-negative.
    PROCEDURE(CopyData)                 :: MakeCopy
    !^ A procedure to make a copy of A.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical            :: Handled
    tLogical            :: ZeroSize
    tIndex              :: I, ARank
    tIndex              :: BeginID(RANK(A)), EndID(RANK(A))
    tIndex              :: OldSize(RANK(A)), PSize(RANK(A))

!** FLOW:

    ! check whether A has been allocated or not.
    ! if not, it cannot be used as a mold to re-allocate its self
    IF (.NOT.ASSOCIATED(A)) THEN
        CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
            '"A" has not yet been allocated.  Use the "MemAlloc" procedure instead.')
        RETURN
    END IF

    ! first check validity of A's rank and values of elements of NewSize
    ARank = RANK(A)
    IF (ARank > 0_kIndex) THEN
        BLOCK
            tLogical    :: ValidSize
            ValidSize = TrueVal
            DO I = 1_kIndex, ARank
                IF (NewSize(I) < 0_kIndex) THEN
                    ValidSize = FalseVal
                    EXIT
                END IF
            END DO
            IF (.NOT.ValidSize) THEN
                CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
                    'The specified "NewSize" is NOT valid.  All its values must be non-negative.')
                RETURN
            END IF
        END BLOCK
    ELSE
        CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
            'An entity with rank of 0 cannot be resized.')
        RETURN
    END IF

    ZeroSize = FalseVal
    SELECT RANK (A)
    RANK (1)
        OldSize(1) = SIZE(A, DIM=I, KIND=kIndex)
        BeginID(1) = LBOUND(A, DIM=I, KIND=kIndex)
        IF (OldSize(1) < 1_kIndex) ZeroSize = TrueVal
    RANK (2)
        DO I = 1_kIndex, 2_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (3)
        DO I = 1_kIndex, 3_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (4)
        DO I = 1_kIndex, 4_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (5)
        DO I = 1_kIndex, 5_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (6)
        DO I = 1_kIndex, 6_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK (7)
        DO I = 1_kIndex, 4_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
    RANK DEFAULT
        Handled = FalseVal
    END SELECT

    IF (ZeroSize) THEN
        ! one or more of A's dimensions has zero size; therefore,
        ! simply allocate A to new sizes
        CALL MemAllocPtr(A, A, NewSize, BeginID)
    ELSE
        Handled = TrueVal
        SELECT RANK (A)
        RANK (1)
            BLOCK
                CLASS(*), POINTER   :: Temp(:)
                tLogical            :: ErrStat
                ! first, allocate the temporary array
                CALL MemAllocPtr(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1)), A(BeginID(1):EndID(1)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFreePtr(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFreePtr(A)
                A => Temp
                NULLIFY(Temp)
            END BLOCK
        RANK (2)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:)
                tLogical            :: ErrStat
                ! first, allocate the temporary array
                CALL MemAllocPtr(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFreePtr(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFreePtr(A)
                A => Temp
                NULLIFY(Temp)
            END BLOCK
        RANK (3)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:)
                tLogical            :: ErrStat
                ! first, allocate the temporary array
                CALL MemAllocPtr(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFreePtr(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFreePtr(A)
                A => Temp
                NULLIFY(Temp)
            END BLOCK
        RANK (4)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:,:)
                tLogical            :: ErrStat
                ! first, allocate the temporary array
                CALL MemAllocPtr(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2), &
                                       BeginID(3):EndID(3),BeginID(4):EndID(4)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2), &
                                       BeginID(3):EndID(3),BeginID(4):EndID(4)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFreePtr(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFreePtr(A)
                A => Temp
                NULLIFY(Temp)
            END BLOCK
        RANK (5)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:,:,:)
                tLogical            :: ErrStat
                ! first, allocate the temporary array
                CALL MemAllocPtr(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFreePtr(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFreePtr(A)
                A => Temp
                NULLIFY(Temp)
            END BLOCK
        RANK (6)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:,:,:,:)
                tLogical            :: ErrStat
                ! first, allocate the temporary array
                CALL MemAllocPtr(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFreePtr(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFreePtr(A)
                A => Temp
                NULLIFY(Temp)
            END BLOCK
        RANK (7)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:,:,:,:,:)
                tLogical            :: ErrStat
                ! first, allocate the temporary array
                CALL MemAllocPtr(Temp, A, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                ErrStat= MakeCopy(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6), &
                                       BeginID(7):EndID(7)), &
                                  A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                       BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6), &
                                       BeginID(7):EndID(7)))
                IF (ErrStat) THEN
                    ! free up memory
                    CALL MemFreePtr(Temp)
                    ! report error
                    CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFreePtr(A)
                A => Temp
                NULLIFY(Temp)
            END BLOCK
        RANK DEFAULT
            Handled = FalseVal
        END SELECT
        IF (.NOT.Handled) THEN
            CALL Handle_ErrLevel('MemResize_Pointer', ModName, ErrSevere, &
                'Currently, only handle an entity with the rank between 1 and 7.')
        END IF
    END IF

    RETURN

END SUBROUTINE MemResize_Pointer

!******************************************************************************

END MODULE ModBase_PolyMemHandlers

!******************************************************************************
