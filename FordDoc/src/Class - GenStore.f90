
MODULE Class_GenStore

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *GenStore* type and its related routines.
!   The *GenStore* type is a data type that provides a generic data
!   storage.  It can be used to store various data types providing
!   that the size (in bytes) of the data to be stored is known at
!   compile time.  As a parameterized derive type, this data storage
!   type is intended to be used in conjunction with a *parameterized*
!   collection (or container).

!** USE STATEMENTS:
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF, C_NULL_PTR
    USE ModBase_Common
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! storage type
    PUBLIC :: GenStore
    ! abstract IsEqual procedure
    PUBLIC :: IsEqualGen
    ! IsEqual procedures for Fortran intrinsic types
    PUBLIC :: Int8B_IsEqual, Int16B_IsEqual, Int32B_IsEqual, Int64B_IsEqual
    PUBLIC :: RealSP_IsEqual, RealDP_IsEqual, RealQP_IsEqual
    PUBLIC :: CmpxSP_IsEqual, CmpxDP_IsEqual, CmpxQP_IsEqual
    PUBLIC :: Character_IsEqual, Logical_IsEqual

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar,   PARAMETER  :: ModName = 'Class_GenStore'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,    PARAMETER  :: MsgLen = 128
    ! parameters for complex types
    tCmpxSingle, PARAMETER  :: CmpxSP = CMPLX(1.0_kSP, 0.0_kSP, KIND=kSP)
    tCmpxDouble, PARAMETER  :: CmpxDP = CMPLX(1.0_kDP, 0.0_kDP, KIND=kDP)
    tCmpxQuad,   PARAMETER  :: CmpxQP = CMPLX(1.0_kQP, 0.0_kQP, KIND=kQP)

!** DERIVED TYPE DEFINITIONS
    !> The *GenStore* type is a generic storage type that can hold
    !  various data types.  A data content is stored as an array
    !  of 8-bit integers (bytes) where its byte size must be known
    !  at compile time (i.e. when the *GenStore* type is declared).
    TYPE :: GenStore(ByteSize)
        !% size of content in bytes
        tIndex, LEN     :: ByteSize
        !% data content stored as an array of bytes (8-bit integers)
        tByte,  PRIVATE :: Content(ByteSize)
#ifdef  __GFORTRAN__
    ! GFortran does not allow PDT with type-bound procedures.
#else
    CONTAINS
        !> **Type-Bound Subroutine**: Set <br>
        ! **Purpose**:  To store the data content in the storage. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Set(Data)
        PROCEDURE   :: Set  => GenStore_SetContent
        !> **Type-Bound Subroutine**: Get <br>
        ! **Purpose**:  To retrieve the data content from the storage. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Get(Data)
        PROCEDURE   :: Get  => GenStore_GetContent
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (.NOT.ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => GenStore_IsEqualTo
#endif
    END TYPE GenStore

!** INTERFACE DEFINITIONS:
    ! abstract interface for GenStore
    ABSTRACT INTERFACE
        !> *IsEqualGen* is an interface for a user-supplied procedure to
        ! compare whether two GenStore objects are equal to one another
        ! or not.  Flag should be set to true if the two objects are equal
        ! Otherwise, flag should be set to false.
        FUNCTION IsEqualGen(A,B) RESULT(Flag)
            IMPORT
            TYPE(GenStore(*)), INTENT(IN)   :: A
            TYPE(GenStore(*)), INTENT(IN)   :: B
            tLogical                        :: Flag
        END FUNCTION
    END INTERFACE
#ifdef  __GFORTRAN__
    ! GFortran does not allow PDT with type-bound procedures.
    INTERFACE Get
        !^ **Subroutine Interface**: Get <br>
        ! **Purpose**:  To retrieve the data content from the storage. <br>
        !  **Usage**: <br>
        !   --->    CALL Get(Store, Data)
        MODULE PROCEDURE GenStore_GetContent
    END INTERFACE
    INTERFACE Set
        !^ **Subroutine Interface**: Set <br>
        ! **Purpose**:  To store the data content in the storage. <br>
        !  **Usage**: <br>
        !   --->    CALL Set(Store, Data)
        MODULE PROCEDURE GenStore_SetContent
    END INTERFACE
    INTERFACE IsEqualTo
        !^ **Function Interface**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsEqualTo(ObjA, ObjB) <br>
        !   --->    IF (.NOT.IsEqualTo(ObjA, ObjB)) DoSomething
        MODULE PROCEDURE GenStore_IsEqualTo
    END INTERFACE
#endif

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           GENSTORE ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE GenStore_SetContent(Store, Content)

!** PURPOSE OF THIS SUBROUTINE:
    !! To store the data content in the storage.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenStore(*)), INTENT(INOUT) :: Store    !! the storage
    TYPE(*), TARGET,    INTENT(IN)    :: Content  !! data content

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to data content
    TYPE(C_PTR)     :: CPtr     ! C pointer to data content

! FLOW

    ! get a C pointer to data content
    CPtr = C_LOC(Content)

    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [Store%ByteSize])

    ! store (a copy of) the content in the storage
    Store%Content = fPtr

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE GenStore_SetContent

!******************************************************************************

SUBROUTINE GenStore_GetContent(Store, Content, ByteSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the data content from the storage.  If *ByteSize* specified,
    !   write an error message to the default log file if it is not equal to
    !   the byte size of the stored content.  If it is not specified, the user
    !   is responsible for supplying the valid content type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenStore(*)), INTENT(IN)      :: Store    !! the storage
    TYPE(*), TARGET,    INTENT(INOUT)   :: Content  !! data content
    tIndex, OPTIONAL,   INTENT(IN)      :: ByteSize !! content size in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to data content
    TYPE(C_PTR)     :: CPtr     ! C pointer to data content

! FLOW

    ! check validity of given content
    IF (PRESENT(ByteSize)) THEN
        IF (ByteSize /= Store%ByteSize) THEN
            CALL Handle_ErrLevel('GenStore_GetContent', ModName, ErrSevere, &
                                 'Type of the specified content is NOT the same as that of stored content.')
            RETURN
        END IF
    END IF

    ! get a C pointer to data content
    CPtr = C_LOC(Content)

    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [Store%ByteSize])

    ! retrieve (a copy of) the content stored in the storage
    fPtr = Store%Content

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE GenStore_GetContent

!******************************************************************************

FUNCTION GenStore_IsEqualTo(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenStore(*)), INTENT(IN)  :: AStore   !! a GenStore object
    TYPE(GenStore(*)),  INTENT(IN)  :: BStore   !! another GenStore object
    tLogical                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal

    ! return quickly if ByteSizes are not equal
    IF (AStore%ByteSize /= BStore%ByteSize) RETURN

    ! check equalities of all contents
    BLOCK
        tIndex  :: I
        DO I = 1_kIndex, AStore%ByteSize
            IF (AStore%Content(I) /= BStore%Content(I)) RETURN
        END DO
    END BLOCK

    Flag = TrueVal

    RETURN

END FUNCTION GenStore_IsEqualTo

!******************************************************************************

FUNCTION Character_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(*)), INTENT(IN)   :: AStore   !! a GenStore object
    TYPE(GenStore(*)), INTENT(IN)   :: BStore   !! another GenStore object
    tLogical                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(AStore%ByteSize)   :: AVal
    tCharLen(BStore%ByteSize)   :: BVal

! FLOW

    IF (AStore%ByteSize /= BStore%ByteSize) THEN
        ! lengths of input character strings are not equal
        Flag = FalseVal
    ELSE
#ifdef  __GFORTRAN__
        CALL Get(AStore, AVal)
        CALL Get(BStore, BVal)
#else
        CALL AStore%Get(AVal)
        CALL BStore%Get(BVal)
#endif
        Flag = (AVal == BVal)
    END IF

    RETURN

END FUNCTION Character_IsEqual

!******************************************************************************

FUNCTION Logical_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(TrueVal))), INTENT(IN)   :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(TrueVal))), INTENT(IN)   :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: AVal
    tLogical    :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal .EQV. BVal)

    RETURN

END FUNCTION Logical_IsEqual

!******************************************************************************

FUNCTION Int8B_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(1_kByte))), INTENT(IN)   :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(1_kByte))), INTENT(IN)   :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte   :: AVal
    tByte   :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION Int8B_IsEqual

!******************************************************************************

FUNCTION Int16B_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(1_kShort))), INTENT(IN)  :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(1_kShort))), INTENT(IN)  :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tShort      :: AVal
    tShort      :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION Int16B_IsEqual

!******************************************************************************

FUNCTION Int32B_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(1_kInteger))), INTENT(IN)    :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(1_kInteger))), INTENT(IN)    :: BStore   !! another GenStore object
    tLogical                                            :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: AVal
    tInteger    :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION Int32B_IsEqual

!******************************************************************************

FUNCTION Int64B_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(1_kLong))), INTENT(IN)   :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(1_kLong))), INTENT(IN)   :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong   :: AVal
    tLong   :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION Int64B_IsEqual

!******************************************************************************

FUNCTION RealSP_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(1.0_kSP))), INTENT(IN)   :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(1.0_kSP))), INTENT(IN)   :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSingle     :: AVal
    tSingle     :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION RealSP_IsEqual

!******************************************************************************

FUNCTION RealDP_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(1.0_kDP))), INTENT(IN)   :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(1.0_kDP))), INTENT(IN)   :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: AVal
    tDouble     :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION RealDP_IsEqual

!******************************************************************************

FUNCTION RealQP_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(1.0_kQP))), INTENT(IN)   :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(1.0_kQP))), INTENT(IN)   :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tQuad       :: AVal
    tQuad       :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION RealQP_IsEqual

!******************************************************************************

FUNCTION CmpxSP_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(CmpxSP))), INTENT(IN)    :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(CmpxSP))), INTENT(IN)    :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCmpxSingle :: AVal
    tCmpxSingle :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION CmpxSP_IsEqual

!******************************************************************************

FUNCTION CmpxDP_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(CmpxDP))), INTENT(IN)    :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(CmpxDP))), INTENT(IN)    :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCmpxDouble :: AVal
    tCmpxDouble :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION CmpxDP_IsEqual

!******************************************************************************

FUNCTION CmpxQP_IsEqual(AStore, BStore) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenStore(C_SIZEOF(CmpxQP))), INTENT(IN)    :: AStore   !! a GenStore object
    TYPE(GenStore(C_SIZEOF(CmpxQP))), INTENT(IN)    :: BStore   !! another GenStore object
    tLogical                                        :: Flag     !! true if the two objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCmpxQuad   :: AVal
    tCmpxQuad   :: BVal

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
    Flag = (AVal == BVal)

    RETURN

END FUNCTION CmpxQP_IsEqual

!******************************************************************************

FUNCTION Dummy_IsEqual(AVal, BVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two GenStore objects are equal or not.
    ! Note: This function illustrates the typical use of "GenStore" objects
    !       and the use of a "IsEqual" procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: AVal
    tLong, INTENT(INOUT)    :: BVal
    tLogical                :: Flag

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: ByteSize = C_SIZEOF(1_kLong)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    TYPE(GenStore(ByteSize))    :: AStore
!    TYPE(GenStore(ByteSize))    :: BStore
    TYPE(GenStore(C_SIZEOF(AVal)))  :: AStore
    TYPE(GenStore(C_SIZEOF(BVal)))  :: BStore

! FLOW

#ifdef  __GFORTRAN__
    CALL Get(AStore, AVal)
    CALL Get(BStore, BVal)
#else
    CALL AStore%Get(AVal)
    CALL BStore%Get(BVal)
#endif
!    Flag = Int8B_IsEqual(AStore, BStore)   ! This results in a compile-time error.
!    Flag = Int16B_IsEqual(AStore, BStore)  ! This results in a compile-time error.
!    Flag = Int32B_IsEqual(AStore, BStore)  ! This results in a compile-time error.
    Flag = Int64B_IsEqual(AStore, BStore)   ! This is OK.
!    Flag = RealSP_IsEqual(AStore, BStore)  ! This results in a compile-time error.
    Flag = RealDP_IsEqual(AStore, BStore)  ! This is also OK.
!    Flag = RealQP_IsEqual(AStore, BStore)  ! This results in a compile-time error.
    Flag = CmpxSP_IsEqual(AStore, BStore)  ! This is also OK.
!    Flag = CmpxDP_IsEqual(AStore, BStore)  ! This results in a compile-time error.
!    Flag = CmpxQP_IsEqual(AStore, BStore)  ! This results in a compile-time error.
    Flag = Character_IsEqual(AStore, BStore)  ! This is also OK.
!    Flag = Logical_IsEqual(AStore, BStore)  ! This results in a compile-time error.

    RETURN

END FUNCTION Dummy_IsEqual

!******************************************************************************

END MODULE Class_GenStore

!******************************************************************************
