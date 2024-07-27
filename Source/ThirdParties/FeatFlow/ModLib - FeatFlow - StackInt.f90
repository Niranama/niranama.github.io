MODULE ModLib_FeatFlow_StackInt

  USE ModLib_FeatFlow_System
  USE ModLib_FeatFlow_GenOutput

!!##############################################################################
!!# ****************************************************************************
!!# <name> stack,T) </name>
!!# ****************************************************************************
!!#
!!# <purpose>
!!#
!!# This module header file implements a LIFO stack.
!!#
!!# Stacks are a type of container adaptor, specifically designed to
!!# operate in a LIFO context (last-in first-out), where elements are
!!# inserted and extracted only from the end of the container. Elements
!!# are pushed/popped from the "back" of the specific container, which
!!# is known as the top of the stack.
!!#
!!# -----------------------------------------------------------------------------
!!# This module is designed with greatest compatibility to the C++
!!# Standard Template Library (STL) concerning naming conventions.
!!# -----------------------------------------------------------------------------
!!#
!!# The following routines are available:
!!#
!!# 1.) stack_create (constructor in STL)
!!#     -> Creates a new empty stack
!!#
!!# 2.) stack_release (destructor in STL)
!!#     -> Releases an existing stack
!!#
!!# 3.) stack_clear (no equivalent in STL)
!!#     -> Clears an existing stack
!!#
!!# 4.) stack_empty (empty in STL)
!!#     -> Tests if the stack is empty
!!#
!!# 5.) stack_size (size in STL)
!!#     -> Returns the size of the stack
!!#
!!# 6.) stack_top (top in STL)
!!#     -> Returns the entry from the top of the stack
!!#
!!# 7.) stack_push (push in STL)
!!#     -> Pushes a new entry on top of the stack
!!#
!!# 8.) stack_pop (pop in STL)
!!#     -> Pops the entry from the top of the stack
!!#
!!# 9.) stack_contains (no equivalent in STL)
!!#     -> Checks if the stack contains a given item
!!#
!!# 10.) stack_cast
!!#      -> Casts a stack to a generic object
!!#
!!# 11.) stack_uncast
!!#      -> Casts a generic object to a stack
!!#
!!#
!!# The following operators are available:
!!#
!!# 1.) "="  assignment operator
!!#
!!# 2.) "==" Compares two stacks for equality. Two stacks are equal if
!!#          they contain the same number of elements and if they are
!!#          equal element-by-element.
!!#
!!# 3.) "/=" Compares two stacks for non-equality. Two stacks are not equal
!!#          if they contain a different number of elements or if they are
!!#          not equal element-by-element.
!!#
!!# 4.) "<"  Lexicographical ordering of two stacks.
!!#
!!# 5.) "<=" Lexicographical ordering of two stacks.
!!#
!!# 6.) ">"  Lexicographical ordering of two stacks.
!!#
!!# 7.) ">=" Lexicographical ordering of two stacks.
!!#
!!# </purpose>
!!##############################################################################

!!#include "kernel/template.h"

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: t_stackInt
  PUBLIC :: stack_create
  PUBLIC :: stack_release
  PUBLIC :: stack_clear
  PUBLIC :: stack_empty
  PUBLIC :: stack_size
  PUBLIC :: stack_top
  PUBLIC :: stack_push
  PUBLIC :: stack_pop
  PUBLIC :: stack_contains

  PUBLIC ASSIGNMENT(=)
  PUBLIC operator(==)
  PUBLIC operator(/=)
  PUBLIC operator(<)
  PUBLIC operator(<=)
  PUBLIC operator(>)
  PUBLIC operator(>=)

  INTERFACE stack_create
    MODULE PROCEDURE stack_create
  END INTERFACE

  INTERFACE stack_release
    MODULE PROCEDURE stack_release
  END INTERFACE

  INTERFACE stack_clear
    MODULE PROCEDURE stack_clear
  END INTERFACE

  INTERFACE stack_empty
    MODULE PROCEDURE stack_empty
  END INTERFACE

  INTERFACE stack_size
    MODULE PROCEDURE stack_size
  END INTERFACE

  INTERFACE stack_top
    MODULE PROCEDURE stack_top
  END INTERFACE

  INTERFACE stack_push
    MODULE PROCEDURE stack_push
  END INTERFACE

  INTERFACE stack_pop
    MODULE PROCEDURE stack_pop
  END INTERFACE

  INTERFACE stack_contains
    MODULE PROCEDURE stack_contains
  END INTERFACE

  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE stack_fassign
  END INTERFACE

  INTERFACE operator(==)
    MODULE PROCEDURE stack_eq
  END INTERFACE

  INTERFACE operator(/=)
    MODULE PROCEDURE stack_ne
  END INTERFACE

  INTERFACE operator(<)
    MODULE PROCEDURE stack_lt
  END INTERFACE

  INTERFACE operator(<=)
    MODULE PROCEDURE stack_le
  END INTERFACE

  INTERFACE operator(>)
    MODULE PROCEDURE stack_gt
  END INTERFACE

  INTERFACE operator(>=)
    MODULE PROCEDURE stack_ge
  END INTERFACE

!<types>

!<typeblock>

  ! Type block for holding a dynamic stack
  TYPE t_stackInt
    PRIVATE

    ! Size of stack
    INTEGER :: istackSize = 0

    ! Position of last stack item
    INTEGER :: istackPosition = 0

    ! Pointer to stack data
    INTEGER, DIMENSION(:), POINTER :: p_StackData => NULL()

  END TYPE

!</typeblock>

!</types>

CONTAINS

  !************************************************************************

!<subroutine>

  SUBROUTINE stack_create(rstack, isize)

!<description>
    ! Creates a stack with prescribed initial memory
!</description>

!<input>
    ! Initial stack size
    INTEGER, INTENT(in) :: isize
!</input>

!<output>
    ! Stack
    TYPE(t_stackInt), INTENT(out) :: rstack
!</output>
!</subroutine>

    ! Set stack size
    rstack%istackSize = isize

    ALLOCATE(rstack%p_StackData(rstack%istackSize))

  END SUBROUTINE

  !************************************************************************

!<subroutine>

  SUBROUTINE stack_release(rstack)

!<description>
    ! Release a stack
!</description>

!<inputoutput>
    ! Stack
    TYPE(t_stackInt), INTENT(inout) :: rstack
!</inputoutput>
!</subroutine>

    IF (ASSOCIATED(rstack%p_StackData))&
        DEALLOCATE(rstack%p_StackData)

    rstack%istackSize     = 0
    rstack%istackPosition = 0

  END SUBROUTINE

  !************************************************************************

!<subroutine>

  SUBROUTINE stack_clear(rstack)

!<description>
    ! Clear stack, i.e., reset stack pointer to zero
!</description>

!<inputoutput>
    ! Stack
    TYPE(t_stackInt), INTENT(inout) :: rstack
!</inputoutput>
!</subroutine>

    rstack%istackPosition = 0

  END SUBROUTINE

  !************************************************************************

!<function>

  PURE FUNCTION stack_Empty(rstack) RESULT(bempty)

!<description>
    ! Checks if the stack is empty
!</description>

!<input>
    ! Stack
    TYPE(t_stackInt), INTENT(in) :: rstack
!</input>

!<result>
    ! Flag: is true if the stack is empty
    LOGICAL :: bempty
!</result>
!</function>

    bempty = (rstack%istackPosition .eq. 0)

  END FUNCTION

  !************************************************************************

!<function>

  PURE FUNCTION stack_size(rstack) RESULT(isize)

!<description>
    ! Returns the stack size
!</description>

!<input>
    ! Stack
    TYPE(t_stackInt), INTENT(in) :: rstack
!</input>

!<result>
    ! Size of the stack
    INTEGER :: isize
!</result>
!</function>

    isize = rstack%istackPosition

  END FUNCTION

  !************************************************************************

!<subroutine>

  SUBROUTINE stack_push(rstack, DATA)

!<description>
    ! Add a new value to the top of the stack
!</description>

!<input>
    ! Data to be pushed onto the stack
    INTEGER, INTENT(in) :: DATA
!</input>

!<inputoutput>
    ! Stack
    TYPE(t_stackInt), INTENT(inout) :: rstack
!</inputoutput>
!</subroutine>

    ! local variable
    INTEGER, DIMENSION(:), POINTER :: p_StackData

    ! Double storage for stack if required
    IF (rstack%istackSize .eq. rstack%istackPosition) THEN
      ALLOCATE(p_StackData(rstack%istackSize))
      p_StackData = rstack%p_StackData
      DEALLOCATE(rstack%p_StackData)
      rstack%istackSize = 2*rstack%istackSize
      ALLOCATE(rstack%p_StackData(rstack%istackSize))
      rstack%p_StackData(1:SIZE(p_StackData)) = p_StackData
      DEALLOCATE(p_StackData)
    END IF

    ! Push data to stack
    rstack%istackPosition = rstack%istackPosition+1
    rstack%p_StackData(rstack%istackPosition) = DATA

  END SUBROUTINE

  !************************************************************************

!<subroutine>

  SUBROUTINE stack_top(rstack, DATA)

!<description>
    ! Return value from top of the stack
!</description>

!<input>
    ! Stack
    TYPE(t_stackInt), INTENT(in) :: rstack
!</input>

!<output>
    ! Data on top of the stack
    INTEGER, INTENT(out) :: DATA
!</output>
!</subroutine>

    IF (.NOT.stack_empty(rstack)) THEN
      DATA = rstack%p_StackData(rstack%istackPosition)
    ELSE
      CALL output_line('Stack empty!',&
          OU_CLASS_ERROR, OU_MODE_STD, 'stack_top')
      CALL sys_halt()
    END IF

  END SUBROUTINE

  !************************************************************************

!<subroutine>

  SUBROUTINE stack_pop(rstack, DATA)

!<description>
    ! Remove a value from the top of the stack
!</description>

!<inputoutput>
    ! Stack
    TYPE(t_stackInt), INTENT(inout) :: rstack
!</inputoutput>

!<output>
    ! Item removed from the top of the stack
    INTEGER, INTENT(out) :: DATA
!</output>
!</subroutine>

    CALL stack_top(rstack, DATA)
    rstack%istackPosition = rstack%istackPosition-1

  END SUBROUTINE

  !************************************************************************

!<function>

  FUNCTION stack_contains(rstack, DATA) RESULT(bresult)

!<description>
    ! Check if the stack contains the given data item
!</description>

!<input>
    ! Stack
    TYPE(t_stackInt), INTENT(in) :: rstack

    ! Item to be searched for in the stack
    INTEGER, INTENT(in) :: DATA
!</inputoutput>

!<result>
    ! True if stack contains the given data item
    LOGICAL :: bresult
!</result>
!</function>

    ! local variable
    INTEGER :: i

    bresult = .false.

    DO i = rstack%istackPosition, 1, -1
      bresult = (rstack%p_StackData(i) .eq. DATA)
      IF (bresult) RETURN
    END DO

  END FUNCTION

  !************************************************************************

!<subroutine>

  SUBROUTINE stack_fassign(rstackDest,rstackSrc)

!<description>
    ! Assigns the content of rstackSrc to rstackDest
!</description>

!<input>
    ! Source stack
    TYPE(t_stackInt), INTENT(in) :: rstackSrc
!</input>

!<output>
    ! Destination stack
    TYPE(t_stackInt), INTENT(out) :: rstackDest
!</output>
!</subroutine>

    ! local variable
    INTEGER :: i

    ! Create empty stack
    CALL stack_create(rstackDest, rstackSrc%istackSize)

    DO i = 1, rstackSrc%istackSize
      rstackDest%p_StackData(i) = rstackSrc%p_StackData(i)
    END DO

  END SUBROUTINE

  !************************************************************************

!<function>

  PURE FUNCTION stack_eq(rstack1,rstack2) RESULT(beq)

!<description>
    ! Compare two stacks for equality
!</description>

!<input>
    ! Stacks
    TYPE(t_stackInt), INTENT(in) :: rstack1,rstack2
!</input>

!<result>
    ! True if both stacks contain the same number of elements and if
    ! they are equal element-by-element
    LOGICAL :: beq
!</result>
!</function>

    ! local variable
    INTEGER :: i

    ! Initialisation
    beq = (rstack1%istackSize == rstack2%istackSize)

    ! Early return?
    IF (.NOT.beq) RETURN

    DO i = 1, rstack1%istackSize
      beq = (rstack1%p_StackData(i) == rstack2%p_StackData(i))
      IF (.NOT.beq) RETURN
    END DO

    ! If we are here, then both stacks are equal

  END FUNCTION

  !************************************************************************

!<function>

  PURE FUNCTION stack_ne(rstack1,rstack2) RESULT(bne)

!<description>
    ! Compare two stacks for non-equality
!</description>

!<input>
    ! Stacks
    TYPE(t_stackInt), INTENT(in) :: rstack1,rstack2
!</input>

!<result>
    ! True if both stacks contain a different same number of elements
    ! or if they are not equal element-by-element
    LOGICAL :: bne
!</result>
!</function>

    ! local variable
    INTEGER :: i

    ! Initialisation
    bne = (rstack1%istackSize /= rstack2%istackSize)

    ! Early return?
    IF (bne) RETURN

    DO i = 1, rstack1%istackSize
      bne = (rstack1%p_StackData(i) /= rstack2%p_StackData(i))
      IF (bne) RETURN
    END DO

    ! If we are here, then both stacks are equal

  END FUNCTION

  !************************************************************************

!<function>

  PURE FUNCTION stack_lt(rstack1,rstack2) RESULT(blt)

!<description>
    ! Checks lexicographical ordering of two stacks
!</description>

!<input>
    ! Stacks
    TYPE(t_stackInt), INTENT(in) :: rstack1,rstack2
!</input>

!<result>
    ! True if the lexicographical ordering of stack1 is smaller than
    ! that of stack2
    LOGICAL :: blt
!</result>
!</function>

    ! local variable
    INTEGER :: i

    ! Initialisation
    blt = (rstack1%istackSize < rstack2%istackSize)

    ! Early return?
    IF (.NOT.blt) RETURN

    DO i = 1, MIN(rstack1%istackSize,rstack2%istackSize)
      blt = (rstack1%p_StackData(i) < rstack2%p_StackData(i))
      IF (.NOT.blt) RETURN
    END DO

  END FUNCTION

  !************************************************************************

!<function>

  PURE FUNCTION stack_le(rstack1,rstack2) RESULT(ble)

!<description>
    ! Checks lexicographical ordering of two stacks
!</description>

!<input>
    ! Stacks
    TYPE(t_stackInt), INTENT(in) :: rstack1,rstack2
!</input>

!<result>
    ! True if the lexicographical ordering of stack1 is smaller than
    ! or equal to that of stack2
    LOGICAL :: ble
!</result>
!</function>

    ! local variable
    INTEGER :: i

    ! Initialisation
    ble = (rstack1%istackSize <= rstack2%istackSize)

    ! Early return?
    IF (.NOT.ble) RETURN

    DO i = 1, MIN(rstack1%istackSize,rstack2%istackSize)
      ble = (rstack1%p_StackData(i) <= rstack2%p_StackData(i))
      IF (.NOT.ble) RETURN
    END DO

  END FUNCTION

  !************************************************************************

!<function>

  PURE FUNCTION stack_gt(rstack1,rstack2) RESULT(bgt)

!<description>
    ! Checks lexicographical ordering of two stacks
!</description>

!<input>
    ! Stacks
    TYPE(t_stackInt), INTENT(in) :: rstack1,rstack2
!</input>

!<result>
    ! True if the lexicographical ordering of stack1 is greater than
    ! that of stack2
    LOGICAL :: bgt
!</result>
!</function>

    ! local variable
    INTEGER :: i

    ! Initialisation
    bgt = (rstack1%istackSize > rstack2%istackSize)

    ! Early return?
    IF (.NOT.bgt) RETURN

    DO i = 1, MIN(rstack1%istackSize,rstack2%istackSize)
      bgt = (rstack1%p_StackData(i) > rstack2%p_StackData(i))
      IF (.NOT.bgt) RETURN
    END DO

  END FUNCTION

  !************************************************************************

!<function>

  PURE FUNCTION stack_ge(rstack1,rstack2) RESULT(bge)

!<description>
    ! Checks lexicographical ordering of two stacks
!</description>

!<input>
    ! Stacks
    TYPE(t_stackInt), INTENT(in) :: rstack1,rstack2
!</input>

!<result>
    ! True if the lexicographical ordering of stack1 is greater than
    ! or equal to that of stack2
    LOGICAL :: bge
!</result>
!</function>

    ! local variable
    INTEGER :: i

    ! Initialisation
    bge = (rstack1%istackSize >= rstack2%istackSize)

    ! Early return?
    IF (.NOT.bge) RETURN

    DO i = 1, MIN(rstack1%istackSize,rstack2%istackSize)
      bge = (rstack1%p_StackData(i) >= rstack2%p_StackData(i))
      IF (.NOT.bge) RETURN
    END DO

  END FUNCTION

  !************************************************************************

END MODULE ModLib_FeatFlow_StackInt
