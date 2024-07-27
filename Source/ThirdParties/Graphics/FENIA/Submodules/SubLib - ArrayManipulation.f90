
SUBMODULE (ModLib_FENIA) SubLib_ArrayManipulation

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule contains routines that perform ....

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
!
MODULE SUBROUTINE ArrayExpandCharacter(ARRAY_POINTER,SCALAR_VALUE)

! ArrayExpandCharacter will take a one-dimensional array and a scalar value and
! add it at the end of the array.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! array_pointer: character, pointer (1D). The original values of the array.
! scalar_value: character, scalar. The value that will be added to the array.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! array_pointer: character, pointer (1D). The updated version of the array.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!subroutine ArrayExpandCharacter(array_pointer,scalar_value)
!use Config, only: srk
!implicit none
!character,intent(in):: scalar_value*(*)
!character,pointer:: array_pointer(:)*(*)
!end subroutine ArrayExpandCharacter
!end interface

!character:: scalar_value*(default_string_length)
!character,pointer:: array_pointer(:)*(default_string_length)
!write(*,*)"Enter as many values as you wish. Enter zero to end input."
!do
!read(*,*)scalar_value
!if (scalar_value=='0') exit
!call ArrayExpandCharacter(array_pointer,scalar_value)
!end do
!write(*,*)"list size = ",size(array_pointer)
!write(*,*)array_pointer

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,default_string_length

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: SCALAR_VALUE*(*)
CHARACTER,POINTER:: ARRAY_POINTER(:)*(*)

! Private variables:
CHARACTER,ALLOCATABLE,TARGET,SAVE:: TEMP(:)*(DEFAULT_STRING_LENGTH)
INTEGER:: ERR

! ------------------------------------------------------------------------------

! Variable initialization :
IF (.NOT.ASSOCIATED(ARRAY_POINTER)) ALLOCATE(ARRAY_POINTER(0))
IF (ALLOCATED(TEMP)) DEALLOCATE(TEMP)
ALLOCATE(TEMP(SIZE(ARRAY_POINTER)+1),STAT=ERR)

IF (ERR/=0) THEN
WRITE(*,*)"ArrayExpandCharacter"
WRITE(*,*)"ERROR: array allocation failed."
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

TEMP(SIZE(TEMP))=SCALAR_VALUE
TEMP(1:SIZE(ARRAY_POINTER))=ARRAY_POINTER

! Pointer "array_pointer" may be left uninitialized in the calling code.
IF (ASSOCIATED(ARRAY_POINTER)) NULLIFY(ARRAY_POINTER)
ARRAY_POINTER=>TEMP

END SUBROUTINE ArrayExpandCharacter

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
MODULE SUBROUTINE ArrayExpandInteger(ARRAY_POINTER,SCALAR_VALUE)

! ArrayExpandInteger will take a one-dimensional array and a scalar value and add
! it at the end of the array.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! array_pointer: integer, pointer (1D). The original values of the array.
! scalar_value: integer, scalar. The value that will be added to the array.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! array_pointer: real, pointer (1D). The updated version of the array.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!subroutine ArrayExpandInteger(array_pointer,scalar_value)
!implicit none
!integer,intent(in):: scalar_value
!integer,pointer:: array_pointer(:)
!end subroutine ArrayExpandInteger
!end interface

!integer:: scalar_value
!integer,pointer:: array_pointer(:)
!write(*,*)"Enter as many values as you wish. Enter zero to end input."
!do
!read(*,*)scalar_value
!if (scalar_value==0.0) exit
!call ArrayExpandInteger(array_pointer,scalar_value)
!end do
!write(*,*)"list size = ",size(array_pointer)
!write(*,*)array_pointer

! ------------------------------------------------------------------------------

IMPLICIT NONE

! Argument variables:
INTEGER,INTENT(IN):: SCALAR_VALUE
INTEGER,POINTER:: ARRAY_POINTER(:)

! Private variables:
INTEGER,ALLOCATABLE,TARGET,SAVE:: TEMP(:)
INTEGER:: ERR

! ------------------------------------------------------------------------------

! Variable initialization :
IF (.NOT.ASSOCIATED(ARRAY_POINTER)) ALLOCATE(ARRAY_POINTER(0))
IF (ALLOCATED(TEMP)) DEALLOCATE(TEMP)
ALLOCATE(TEMP(SIZE(ARRAY_POINTER)+1),STAT=ERR)

IF (ERR/=0) THEN
WRITE(*,*)"ArrayExpandInteger"
WRITE(*,*)"ERROR: array allocation failed."
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

TEMP(SIZE(TEMP))=SCALAR_VALUE
TEMP(1:SIZE(ARRAY_POINTER))=ARRAY_POINTER

! Pointer "array_pointer" may be left uninitialized in the calling code.
IF (ASSOCIATED(ARRAY_POINTER)) NULLIFY(ARRAY_POINTER)
ARRAY_POINTER=>TEMP

END SUBROUTINE ArrayExpandInteger

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE ArrayExpandReal(ARRAY_POINTER,SCALAR_VALUE)

! ArrayExpandReal will take a one-dimensional array and a scalar value and add
! it at the end of the array.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! array_pointer: real, pointer (1D). The original values of the array.
! scalar_value: real, scalar. The value that will be added to the array.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! array_pointer: real, pointer (1D). The updated version of the array.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!subroutine ArrayExpandReal(array_pointer,scalar_value)
!use Config, only: srk
!implicit none
!real(srk),intent(in):: scalar_value
!real(srk),pointer:: array_pointer(:)
!end subroutine ArrayExpandReal
!end interface

!real(srk):: scalar_value
!real(srk),pointer:: array_pointer(:)
!write(*,*)"Enter as many values as you wish. Enter zero to end input."
!do
!read(*,*)scalar_value
!if (scalar_value==0.0) exit
!call ArrayExpandReal(array_pointer,scalar_value)
!end do
!write(*,*)"list size = ",size(array_pointer)
!write(*,*)array_pointer

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk

IMPLICIT NONE

! Argument variables:
REAL(SRK),INTENT(IN):: SCALAR_VALUE
REAL(SRK),POINTER:: ARRAY_POINTER(:)

! Private variables:
REAL(SRK),ALLOCATABLE,TARGET,SAVE:: TEMP(:)
INTEGER:: ERR

! ------------------------------------------------------------------------------

! Variable initialization :
IF (.NOT.ASSOCIATED(ARRAY_POINTER)) ALLOCATE(ARRAY_POINTER(0))
IF (ALLOCATED(TEMP)) DEALLOCATE(TEMP)
ALLOCATE(TEMP(SIZE(ARRAY_POINTER)+1),STAT=ERR)

IF (ERR/=0) THEN
WRITE(*,*)"ArrayExpandReal"
WRITE(*,*)"ERROR: array allocation failed."
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

TEMP(SIZE(TEMP))=SCALAR_VALUE
TEMP(1:SIZE(ARRAY_POINTER))=ARRAY_POINTER

! Pointer "array_pointer" may be left uninitialized in the calling code.
IF (ASSOCIATED(ARRAY_POINTER)) NULLIFY(ARRAY_POINTER)
ARRAY_POINTER=>TEMP

END SUBROUTINE ArrayExpandReal

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE LinkedListCharacter(CHARACTER_VALUE,LIST_POINTER)

! LinkedListCharacter stores a series of given character variables in a linked
! list. The list may be collected by calling the subroutine with argument
! "list_pointer" which is also the pointer where the linked list elements are
! stored.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! No required input arguments. However, calling the subroutine with no arguments
! at all causes an error.

! INPUT (OPTIONAL):
! character_value: real, scalar. The value that will be stored in the linked
!    list.

! OUTPUT (REQUIRED):
! No required output arguments. However, calling the subroutine with no
! arguments at all causes an error.

! OUTPUT (OPTIONAL):
! list_pointer: character, pointer (1D). The pointer where the linked list
!    elements are stored.

! Example of usage:

!interface
!subroutine LinkedListCharacter(character_value,list_pointer)
!use Config, only: default_string_length
!implicit none
!character,optional,intent(in):: character_value*(*)
!character,pointer,optional:: list_pointer(:)*(*)
!end subroutine LinkedListCharacter
!end interface

!integer:: i
!character:: value*(default_string_length)
!character,pointer:: list_pointer(:)*(default_string_length)
!write(*,*)"Enter as many values as you wish. Enter zero to end input."
!do
!read(*,*)value
!if (value=='0') exit
!call LinkedListCharacter(character_value=value)
!end do
!call LinkedListCharacter(list_pointer=list_pointer)
!write(*,*)"list size = ",size(list_pointer)
!write(*,*)(trim(list_pointer(i))//' ',i=1,size(list_pointer))

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: default_string_length

IMPLICIT NONE

! Derived data types:
TYPE LINKED_VALUE
CHARACTER:: VALUE*(DEFAULT_STRING_LENGTH)
TYPE(LINKED_VALUE),POINTER:: NEXT
END TYPE LINKED_VALUE

! Argument variables:
CHARACTER,OPTIONAL,INTENT(IN):: CHARACTER_VALUE*(*)
CHARACTER,POINTER,OPTIONAL:: LIST_POINTER(:)*(*)

! Private variables:
INTEGER:: ERR
INTEGER,SAVE:: LIST_LENGTH,MAX_LEN_TRIM
TYPE(LINKED_VALUE),POINTER,SAVE:: FIRST,LAST,AUX
LOGICAL,SAVE:: FIRST_RUN=.TRUE.

! Variable initialization:
IF (PRESENT(LIST_POINTER)) THEN
IF (ASSOCIATED(LIST_POINTER)) DEALLOCATE(LIST_POINTER)
END IF

IF (PRESENT(CHARACTER_VALUE)) THEN
    IF (MAX_LEN_TRIM<LEN_TRIM(CHARACTER_VALUE)) &
        & MAX_LEN_TRIM=LEN_TRIM(CHARACTER_VALUE)
END IF

! ------------------------------------------------------------------------------

! Error control:

IF ((.NOT.(PRESENT(CHARACTER_VALUE))).AND.(.NOT.(PRESENT(LIST_POINTER)))) THEN
WRITE(*,*)"LinkedListCharacter"
WRITE(*,*)"ERROR: no arguments present."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(CHARACTER_VALUE)).AND.(PRESENT(LIST_POINTER))) THEN
WRITE(*,*)"LinkedListCharacter"
WRITE(*,*)"ERROR: only one argument is allowed per call."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (LEN(CHARACTER_VALUE)>DEFAULT_STRING_LENGTH) THEN
WRITE(*,*)"LinkedListCharacter"
WRITE(*,*)"ERROR: character value too long."
WRITE(*,*)"maximum supported length : ",DEFAULT_STRING_LENGTH
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (PRESENT(LIST_POINTER)) THEN
IF (LEN(LIST_POINTER)<MAX_LEN_TRIM) THEN
WRITE(*,*)"LinkedListCharacter"
WRITE(*,*)"ERROR: character pointer too short."
WRITE(*,*)"minimum required length : ",MAX_LEN_TRIM
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
MAX_LEN_TRIM=0
END IF

IF ((FIRST_RUN).AND.(PRESENT(LIST_POINTER))) THEN
NULLIFY(LIST_POINTER)
ALLOCATE(LIST_POINTER(0))
RETURN
END IF

! ------------------------------------------------------------------------------

INITIALIZE_LIST: IF (FIRST_RUN) THEN

    NULLIFY(FIRST,LAST,AUX)
    ALLOCATE(FIRST,STAT=ERR)
    ALLOCATE(AUX,STAT=ERR)

    IF (ERR/=0) THEN
    WRITE(*,*)"LinkedListCharacter"
    WRITE(*,*)"ERROR: pointer allocation failed."
    WRITE(*,*)"error flag : ",ERR
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

    LAST=>FIRST
    LAST%NEXT=>NULL()
    LAST%VALUE=CHARACTER_VALUE
    LIST_LENGTH=1
    FIRST_RUN=.FALSE.

ELSE INITIALIZE_LIST

    ADD_TO_LIST: IF (.NOT.PRESENT(LIST_POINTER)) THEN

        ALLOCATE(LAST%NEXT,STAT=ERR)

        IF (ERR/=0) THEN
        WRITE(*,*)"LinkedListCharacter"
        WRITE(*,*)"ERROR: memory allocation failed."
        WRITE(*,*)"error flag : ",ERR
        WRITE(*,*)"Program terminated."
        READ(*,*)
        STOP
        END IF

        LAST=>LAST%NEXT
        LAST%NEXT=>NULL()
        LAST%VALUE=CHARACTER_VALUE
        LIST_LENGTH=LIST_LENGTH+1

    ELSE ADD_TO_LIST

        NULLIFY(LIST_POINTER)
        ALLOCATE(LIST_POINTER(LIST_LENGTH))
        LIST_LENGTH=0

        DO WHILE(ASSOCIATED(FIRST))
            LIST_LENGTH=LIST_LENGTH+1
            DEALLOCATE(AUX)
            LIST_POINTER(LIST_LENGTH)=FIRST%VALUE
            AUX=>FIRST
            FIRST=>FIRST%NEXT
        END DO

        DEALLOCATE(AUX)
        FIRST_RUN=.TRUE.

    END IF ADD_TO_LIST

END IF INITIALIZE_LIST

END SUBROUTINE LinkedListCharacter

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE LinkedListInteger(NUMERICAL_VALUE,LIST_POINTER)

! LinkedListInteger stores a series of given numbers in a linked list. The list
! may be collected by calling the subroutine with argument "list_pointer" which
! is also the pointer where the linked list elements are stored.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! No required input arguments. However, calling the subroutine with no arguments
! at all causes an error.

! INPUT (OPTIONAL):
! numerical_value: integer, scalar. The value that will be stored in the linked
!    list.

! OUTPUT (REQUIRED):
! No required output arguments. However, calling the subroutine with no
! arguments at all causes an error.

! OUTPUT (OPTIONAL):
! list_pointer: integer, pointer (1D). The pointer where the linked list
!    elements are stored.

! Example of usage:

!interface
!subroutine LinkedListInteger(numerical_value,list_pointer)
!implicit none
!integer,optional,intent(in):: numerical_value
!integer,pointer,optional:: list_pointer(:)
!end subroutine LinkedListInteger
!end interface

!integer:: value
!integer,pointer:: list_pointer(:)
!write(*,*)"Enter as many values as you wish. Enter zero to end input."
!do
!read(*,*)value
!if (value==0) exit
!call LinkedListInteger(numerical_value=value)
!end do
!call LinkedListInteger(list_pointer=list_pointer)
!write(*,*)"list size = ",size(list_pointer)
!write(*,*)list_pointer

! ------------------------------------------------------------------------------

IMPLICIT NONE

! Derived data types:
TYPE LINKED_VALUE
INTEGER:: VALUE
TYPE(LINKED_VALUE),POINTER:: NEXT
END TYPE LINKED_VALUE

! Argument variables:
INTEGER,OPTIONAL,INTENT(IN):: NUMERICAL_VALUE
INTEGER,POINTER,OPTIONAL:: LIST_POINTER(:)

! Private variables:
INTEGER:: ERR
INTEGER,SAVE:: LIST_LENGTH
TYPE(LINKED_VALUE),POINTER,SAVE:: FIRST,LAST,AUX
LOGICAL,SAVE:: FIRST_RUN=.TRUE.

! Variable initialization:
IF (PRESENT(LIST_POINTER)) THEN
IF (ASSOCIATED(LIST_POINTER)) DEALLOCATE(LIST_POINTER)
END IF

! ------------------------------------------------------------------------------

! Error control:

IF ((.NOT.(PRESENT(NUMERICAL_VALUE))).AND.(.NOT.(PRESENT(LIST_POINTER)))) THEN
WRITE(*,*)"LinkedListInteger"
WRITE(*,*)"ERROR: no arguments present."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(NUMERICAL_VALUE)).AND.(PRESENT(LIST_POINTER))) THEN
WRITE(*,*)"LinkedListInteger"
WRITE(*,*)"ERROR: only one argument is allowed per call."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FIRST_RUN).AND.(PRESENT(LIST_POINTER))) THEN
NULLIFY(LIST_POINTER)
ALLOCATE(LIST_POINTER(0))
RETURN
END IF

! ------------------------------------------------------------------------------

INITIALIZE_LIST: IF (FIRST_RUN) THEN

    NULLIFY(FIRST,LAST,AUX)
    ALLOCATE(FIRST,STAT=ERR)
    ALLOCATE(AUX,STAT=ERR)

    IF (ERR/=0) THEN
    WRITE(*,*)"LinkedListInteger"
    WRITE(*,*)"ERROR: pointer allocation failed."
    WRITE(*,*)"error flag : ",ERR
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

    LAST=>FIRST
    LAST%NEXT=>NULL()
    LAST%VALUE=NUMERICAL_VALUE
    LIST_LENGTH=1
    FIRST_RUN=.FALSE.

ELSE INITIALIZE_LIST

    ADD_TO_LIST: IF (.NOT.PRESENT(LIST_POINTER)) THEN

        ALLOCATE(LAST%NEXT,STAT=ERR)

        IF (ERR/=0) THEN
        WRITE(*,*)"LinkedListInteger"
        WRITE(*,*)"ERROR: memory allocation failed."
        WRITE(*,*)"error flag : ",ERR
        WRITE(*,*)"Program terminated."
        READ(*,*)
        STOP
        END IF

        LAST=>LAST%NEXT
        LAST%NEXT=>NULL()
        LAST%VALUE=NUMERICAL_VALUE
        LIST_LENGTH=LIST_LENGTH+1

    ELSE ADD_TO_LIST

        NULLIFY(LIST_POINTER)
        ALLOCATE(LIST_POINTER(LIST_LENGTH))
        LIST_LENGTH=0

        DO WHILE(ASSOCIATED(FIRST))
            LIST_LENGTH=LIST_LENGTH+1
            DEALLOCATE(AUX)
            LIST_POINTER(LIST_LENGTH)=FIRST%VALUE
            AUX=>FIRST
            FIRST=>FIRST%NEXT
        END DO

        DEALLOCATE(AUX)
        FIRST_RUN=.TRUE.

    END IF ADD_TO_LIST

END IF INITIALIZE_LIST

END SUBROUTINE LinkedListInteger

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE LinkedListReal(NUMERICAL_VALUE,LIST_POINTER)

! LinkedListReal stores a series of given numbers in a linked list. The list
! may be collected by calling the subroutine with argument "list_pointer" which
! is also the pointer where the linked list elements are stored.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! No required input arguments. However, calling the subroutine with no arguments
! at all causes an error.

! INPUT (OPTIONAL):
! numerical_value: real, scalar. The value that will be stored in the linked
!    list.

! OUTPUT (REQUIRED):
! No required output arguments. However, calling the subroutine with no
! arguments at all causes an error.

! OUTPUT (OPTIONAL):
! list_pointer: real, pointer (1D). The pointer where the linked list elements
!    are stored.

! Example of usage:

!interface
!subroutine LinkedListReal(numerical_value,list_pointer)
!use Config, only: srk
!implicit none
!real(srk),optional,intent(in):: numerical_value
!real(srk),pointer,optional:: list_pointer(:)
!end subroutine LinkedListReal
!end interface

!real(srk):: value
!real(srk),pointer:: list_pointer(:)
!write(*,*)"Enter as many values as you wish. Enter zero to end input."
!do
!read(*,*)value
!if (value==0.0) exit
!call LinkedListReal(numerical_value=value)
!end do
!call LinkedListReal(list_pointer=list_pointer)
!write(*,*)"list size = ",size(list_pointer)
!write(*,*)list_pointer

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk

IMPLICIT NONE

! Derived data types:
TYPE LINKED_VALUE
REAL(SRK):: VALUE
TYPE(LINKED_VALUE),POINTER:: NEXT
END TYPE LINKED_VALUE

! Argument variables:
REAL(SRK),OPTIONAL,INTENT(IN):: NUMERICAL_VALUE
REAL(SRK),POINTER,OPTIONAL:: LIST_POINTER(:)

! Private variables:
INTEGER:: ERR
INTEGER,SAVE:: LIST_LENGTH
TYPE(LINKED_VALUE),POINTER,SAVE:: FIRST,LAST,AUX
LOGICAL,SAVE:: FIRST_RUN=.TRUE.

! Variable initialization:
IF (PRESENT(LIST_POINTER)) THEN
IF (ASSOCIATED(LIST_POINTER)) DEALLOCATE(LIST_POINTER)
END IF

! ------------------------------------------------------------------------------

! Error control:

IF ((.NOT.(PRESENT(NUMERICAL_VALUE))).AND.(.NOT.(PRESENT(LIST_POINTER)))) THEN
WRITE(*,*)"LinkedListReal"
WRITE(*,*)"ERROR: no arguments present."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(NUMERICAL_VALUE)).AND.(PRESENT(LIST_POINTER))) THEN
WRITE(*,*)"LinkedListReal"
WRITE(*,*)"ERROR: only one argument is allowed per call."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((FIRST_RUN).AND.(PRESENT(LIST_POINTER))) THEN
NULLIFY(LIST_POINTER)
ALLOCATE(LIST_POINTER(0))
RETURN
END IF

! ------------------------------------------------------------------------------

INITIALIZE_LIST: IF (FIRST_RUN) THEN

    NULLIFY(FIRST,LAST,AUX)
    ALLOCATE(FIRST,STAT=ERR)
    ALLOCATE(AUX,STAT=ERR)

    IF (ERR/=0) THEN
    WRITE(*,*)"LinkedListReal"
    WRITE(*,*)"ERROR: pointer allocation failed."
    WRITE(*,*)"error flag : ",ERR
    WRITE(*,*)"Program terminated."
    READ(*,*)
    STOP
    END IF

    LAST=>FIRST
    LAST%NEXT=>NULL()
    LAST%VALUE=NUMERICAL_VALUE
    LIST_LENGTH=1
    FIRST_RUN=.FALSE.

ELSE INITIALIZE_LIST

    ADD_TO_LIST: IF (.NOT.PRESENT(LIST_POINTER)) THEN

        ALLOCATE(LAST%NEXT,STAT=ERR)

        IF (ERR/=0) THEN
        WRITE(*,*)"LinkedListReal"
        WRITE(*,*)"ERROR: memory allocation failed."
        WRITE(*,*)"error flag : ",ERR
        WRITE(*,*)"Program terminated."
        READ(*,*)
        STOP
        END IF

        LAST=>LAST%NEXT
        LAST%NEXT=>NULL()
        LAST%VALUE=NUMERICAL_VALUE
        LIST_LENGTH=LIST_LENGTH+1

    ELSE ADD_TO_LIST

        NULLIFY(LIST_POINTER)
        ALLOCATE(LIST_POINTER(LIST_LENGTH))
        LIST_LENGTH=0

        DO WHILE(ASSOCIATED(FIRST))
            LIST_LENGTH=LIST_LENGTH+1
            DEALLOCATE(AUX)
            LIST_POINTER(LIST_LENGTH)=FIRST%VALUE
            AUX=>FIRST
            FIRST=>FIRST%NEXT
        END DO

        DEALLOCATE(AUX)
        FIRST_RUN=.TRUE.

    END IF ADD_TO_LIST

END IF INITIALIZE_LIST

END SUBROUTINE LinkedListReal

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


END SUBMODULE SubLib_ArrayManipulation
