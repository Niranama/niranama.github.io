! Template - Memory Handling.f90
 
! Macros for memory handling routines. 
! Fortran preprocessor must be enabled: -fpp.

#define MOVE_POINTER_ARRAY(Temp, Array) \
    CALL MemFree(Array); \
    Array => Temp; \
    NULLIFY(Temp);

#define MOVE_ALLOCATABLE_ARRAY(Temp, Array) \
    CALL MemFree(Array); \
    CALL MOVE_ALLOC(Temp, Array);
