
MODULE Class_HTabRealSP

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HTabRealSP* type, the *TabItem* type and their
!   related routines.  The *TabItem* type is a helper and private type used to
!   store a key-value pair.  The *HTabRealSP* type is a container type that
!   employs an open-addressing hash table implementation to provide common
!   operations for an unordered symbol table. <br>
!   Unlike the *list-based* and *tree-based* types, which can be used instantly
!   by inserting objects into a container, the *HTabRealSP* type requires an
!   explicit construction before using other provided operations.  There are two
!   methods provided to create the container.  The *CreateEmpty* method constructs
!   an empty table with optional multiple arguments (including an initial capacity,
!   a load factor, a probing algorithm, and a hash function used to compute
!   a hash code of a key) whereas the *Construct* method constructs a table from
!   arrays of keys and values. <br>
!   As an unordered symbol table, the *HTabRealSP* type uses the Fortran intrinsic
!   *INTEGER(KIND=kI8B)* type as the type of its stored keys and an unlimited polymorphic type
!   as the type of its stored values.  As a symbol table, the *HTabRealSP* type
!   does not allow duplicated keys.  Therefore, if an inserted key is equal to a key
!   stored in the table, an associated value of the stored key is replaced by an
!   associated value of the inserted key. <br>
!   Technically, the *HTabRealSP* type employs the open-addressing as a collision
!   resolution technique where the hash resolution is performed through probing.  It
!   provides three probing algorithms: linear probing, quadratic probing and double
!   hashing.  By default, the linear probing algorithm is used.  However, a user can
!   specify other probing algorithm during the construction of the table. <br>

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash           tIndex
!----------------------------------------------------------
#define     HashTable       HTabRealSP
#define     KeyTypeA        REAL(KIND=kSP)
#define     KeyTypeB        REAL(KIND=kSP)
#define     QueueKey        ListRealSP
#define     QueueVal        ListAnyType
!----------------------------------------------------------

!** USE STATEMENTS:
    USE ModBase_Common
    USE ISO_C_BINDING,          ONLY: C_SIZEOF
    USE ModBase_Error_Handlers
    USE ModBase_SIntUtil
    USE ModLib_PrimeNumbers
    USE ModBase_DoublyLinkedLists
#ifdef Indx32Bits
    USE ModBase_OptimalHash32,  ONLY: HashFuncOpt => Murmur3_Hash32_Opt
#else
    USE ModBase_OptimalHash64,  ONLY: HashFuncOpt => XX_Hash64_Opt
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HTabRealSP

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_HTabRealSP'
    tSInt32,   PARAMETER    :: INFVAL  = ToInteger(Z'7F800000')  ! positive infinity
    tSInt32,   PARAMETER    :: NANVAL  = ToInteger(Z'7FC00000')  ! quiet NAN
    ! Special marker token used to indicate the deletion of a key-value pair
    tSingle,   PARAMETER    :: DELKEY = TRANSFER(INFVAL, 1.0_kSingle)
    ! Special marker token used to indicate the empty of a key-value pair
    tSingle,   PARAMETER    :: NULKEY = TRANSFER(NANVAL, 1.0_kSingle)

!** INCLUDE FILE FOR DECLARATION PART**
#include "Includes/Intrinsic HashTable - Declaraction.f90"

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!** INCLUDE FILE FOR IMPLEMENTATION PART **
#include "Includes/Intrinsic HashTable - Implementation.f90"

!** UNDEFINE MACROS **
#undef tHash
#undef HashTable
#undef KeyTypeA
#undef KeyTypeB

END MODULE Class_HTabRealSP

!******************************************************************************
