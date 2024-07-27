
MODULE ModBase_Memory_Handlers

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module collectively contains memory management routines from other modules.

!** USE STATEMENTS:
    ! intrinsic types
    USE ModBase_MemHandlers_Integer
    USE ModBase_MemHandlers_Real
    USE ModBase_MemHandlers_Complex
    USE ModBase_MemHandlers_Logical
    USE ModBase_MemHandlers_Character
    ! derived types
    USE ModBase_MemHandlers_Assignable
!    USE ModBase_MemHandlers_Comparable
!    USE ModBase_MemHandlers_Hashable
#ifndef __GFORTRAN__
    USE ModBase_MemHandlers_GenStore
#endif
!    USE ModBase_MemHandlers_KeyStore
    USE ModBase_MemHandlers_Misc

END MODULE ModBase_Memory_Handlers
