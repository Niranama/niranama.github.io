
MODULE Class_StringBuilder

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *StringBuilder* type and its related routines.  The *StringBuilder* type
!   is a string type that provides a convenient and efficient mechanism for concatenating multiple
!   strings (i.e. building a string from several strings).  <br>
!   The *StringBuilder* type provides several methods that can be grouped
!   into the following categories. <br>
!   (1) Construction and Destruction.  Methods for these operations include: <br>
!   - *CreateEmpty* method - method to construct an empty builder, <br>
!   - *Construct* method - method to construct the builder from its first specified value, and <br>
!   - *Clear* method - method to remove all inserted values from the builder. <br>
!   (2) Insertion and Removal.  Methods for these operations include: <br>
!   - *Append* method - method to insert a value into the end of the builder's string, <br>
!   - *Insert* method - method to insert a value at a specified position of the builder's string, and <br>
!   - *DelLastChar* method - method to remove the last character of the builder's string. <br>
!   (3) Conversion-To-String.  Methods for these operations include: <br>
!   - *ToCharAlloc* method - method to return the builder's string as an allocatable character string, <br>
!   - *ToFvlStr* method - method to return the builder's string as a string of the *FvlStr* type, and <br>
!   - *ToCharStar* method - method to return the builder's string as a character string of assumed length. <br>
!   (4) Inquiry.  A method for this operation is: <br>
!   - *Length* method - method to inquire the current length of the builder's string. <br>
!   (5) Miscellaneous.  There are several other methods that the *StringBuilder* type are inherited from
!     its parent types (*Assignable* and *Comparable*). <br>
!   It is worth noting that values inserted into the string builder do not need to be strings.  If the type
!   of values is one of Fortran intrinsic types or the *FvlStr* type, the values are automatically converted
!   into strings.  However, if the type of values is a user-defined one, the user can supply a procedure to
!   convert values into strings. <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers
    USE ModBase_CharUtil
    USE ModBase_ChrStr
    USE Class_Assignable
    USE Class_Comparable
    USE Class_FvlStr

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: StringBuilder

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"
#include    "../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'Class_StringBuilder'
    tIndex,    PARAMETER    :: DfltInitCap = 2048_kIndex

!** DERIVED TYPE DEFINITIONS
    !> The *StringBuilder* type is string type that provides a convenient way
    !  to build a string from multiple strings.
    TYPE, EXTENDS(Comparable)  :: StringBuilder
        PRIVATE
        tIndex      :: Count    = 0_kIndex  !! the number of characters currently stored
        tIndex      :: Capacity = 0_kIndex  !! the maximum number of characters that currently can be stored
        tCharAlloc  :: StrBuf               !! string buffer
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign       => StringBuilder_CopyAssign
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone        => StringBuilder_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !  Use relational operators to compare two StringBuilder objects (i.e. use the
        !  *OPERATOR(==)* instead of the *IsEqualTo* method).
        PROCEDURE   :: IsEqualTo        => StringBuilder_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory       => StringBuilder_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName      => StringBuilder_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Comparable Type          -----
        ! ---------------------------------------------------------------------
        !> *CompareTo* is a procedure deferred by the *Comparable* type. <br>
        !  Use logical expression(s) in place of the *CompareTo* method to compare
        !  two *Comparable* objects.
        PROCEDURE   :: CompareTo        => StringBuilder_CompareTo
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: EnsureCapacity   => StringBuilder_EnsureCapacity
        PROCEDURE, PRIVATE  :: StringBuilder_Append
        PROCEDURE, PRIVATE  :: StringBuilder_AppendWPrefix
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty string builder. <br>
        !  **Usage**: <br>
        !   ! use default initial capacity  <br>
        !   --->    CALL StrBld%CreateEmpty() <br>
        !   ! specify initial capacity <br>
        !   --->    CALL StrBld%CreateEmpty(InitCap=256) <br>
        PROCEDURE   :: CreateEmpty  => StringBuilder_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a string builder with the specified value.  This method
        !       provides a convenient way to create an empty string builder and then add the
        !       first value into the builder. <br>
        !  **Usage**: <br>
        !   --->    CALL StrBld%Construct(Value) <br>
        PROCEDURE   :: Construct    => StringBuilder_Construct
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To clear the buffer string of the string builder. <br>
        !  **Usage**: <br>
        !   --->    CALL StrBld%Clear() <br>
        PROCEDURE   :: Clear        => StringBuilder_Clear
        !> **Type-Bound Subroutine**: Append <br>
        !  **Purpose**:  To append the specified value at the end of the builder where the type
        !       of value can be one of Fortran intrinsic types or the *FvlStr* type.  Optionally,
        !       a prefix and/or a suffix can be specified.  Also, optionally, if the type of value
        !       is a user-defined one, a *ToString* procedure must be specified. <br>
        !  **Usage**: <br>
        !   ! specify only the value <br>
        !   --->    CALL StrBld%Append(Val) <br>
        !   ! specify only the value and its ToString procedure<br>
        !   --->    CALL StrBld%Append(Val, ToString) <br>
        !   ! specify the value and its prefix <br>
        !   --->    CALL StrBld%Append('The computed value is', Val) <br>
        !   ! specify the value and its prefix and suffix <br>
        !   --->    CALL StrBld%Append('The speed of light in vacuum =', 299792458, 'm/s.') <br>
        GENERIC     :: Append       => StringBuilder_Append, StringBuilder_AppendWPrefix
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified value at the specified position where the type of
        !       value can be one of Fortran intrinsic types or the *FvlStr* type.  Optionally, if the
        !       type of value is a user-defined one, a *ToString* procedure must be specified. <br>
        !  **Usage**: <br>
        !   --->    CALL StrBld%Insert(Val, Pos) <br>
        !   --->    CALL StrBld%Insert(Val, Pos, ToString) <br>
        PROCEDURE   :: Insert       => StringBuilder_Insert
        !> **Type-Bound Subroutine**: DelLastChar <br>
        !  **Purpose**:  To remove the last character from the builder's string. <br>
        !  **Usage**: <br>
        !   --->    CALL StrBld%DelLastChar() <br>
        PROCEDURE   :: DelLastChar  => StringBuilder_DeleteLastChar
        !> **Type-Bound Function**: ToCharAlloc <br>
        !  **Purpose**:  To return characters currently stored in the string builder as an
        !       allocatable character string.  Optionally, a user can request to clear the
        !       builder' buffer string. <br>
        !  **Usage**: <br>
        !   --->    cStr = StrBld%ToCharAlloc() <br>
        !   --->    cStr = StrBld%ToCharAlloc(ClearBuffer=.TRUE.) <br>
        PROCEDURE   :: ToCharAlloc  => StringBuilder_ToCharAlloc
        !> **Type-Bound Function**: ToFvlStr <br>
        !  **Purpose**:  To return characters currently stored in the string builder as a
        !       string of the *FvlStr* type.  Optionally, a user can request to clear the
        !       builder' buffer string. <br>
        !  **Usage**: <br>
        !   --->    vStr = StrBld%ToFvlStr() <br>
        !   --->    vStr = StrBld%ToFvlStr(ClearBuffer=.TRUE.) <br>
        PROCEDURE   :: ToFvlStr     => StringBuilder_ToFvlStr
        !> **Type-Bound Subroutine**: ToCharStar <br>
        !  **Purpose**:  To return characters currently stored in the string builder as
        !       a character string of assumed length.  Optionally, a user can request
        !       to clear the builder' buffer string. <br>
        !  **Usage**: <br>
        !   --->    CALL StrBld%ToCharStar(cStr) <br>
        !   --->    CALL StrBld%ToCharStar(cStr, ClearBuffer=.TRUE.) <br>
        !  **Note**: In order to retrieve all the characters currently stored in the builder,
        !       the length of the specified string must be at least equal to the current length
        !       of the string being built, which can be inquired using the *Length* method.<br>
        PROCEDURE   :: ToCharStar   => StringBuilder_ToCharStar
        !> **Type-Bound Function**: Length <br>
        !  **Purpose**:  To return the current length of the string being built. <br>
        !  **Usage**: <br>
        !   --->    Length = StrBld%Length() <br>
        PROCEDURE   :: Length       => StringBuilder_GetLength
        ! ---------------------------------------------------------------------
        ! -----                 Final Procedure                           -----
        ! ---------------------------------------------------------------------
        !% To perform finalization of the *StringBuilder* object.
        FINAL       :: StringBuilder_Finalize
        ! ---------------------------------------------------------------------
    END TYPE StringBuilder

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Anytype2String(Val) RESULT(Str)
            IMPORT
            CLASS(*), INTENT(IN)    :: Val
            tCharAlloc              :: Str
        END FUNCTION
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!--------------------------------------------------------------------------------------
!                   DEFERRED PROCEDURES OF COMPARABLE CLASS
!--------------------------------------------------------------------------------------

SUBROUTINE StringBuilder_CopyAssign(DstObj, SrcObj)

!** PURPOSE OF THIS ROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(OUT)   :: DstObj   !! destination object
    CLASS(Assignable),    INTENT(IN)    :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT TYPE (SrcObj)
    TYPE IS (StringBuilder)
        ALLOCATE(DstObj%StrBuf, SOURCE=SrcObj%StrBuf)
        DstObj%Count    = SrcObj%Count
        DstObj%Capacity = SrcObj%Capacity
    CLASS DEFAULT
        CALL Handle_ErrLevel('StringBuilder_CopyAssign', ModName, ErrSevere, 'Type of SrcObj is invalid.')
    END SELECT

    RETURN

END SUBROUTINE StringBuilder_CopyAssign

!**************************************************************************************

SUBROUTINE StringBuilder_Clone(SrcObj, DstObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To clone the StringBuilder object.   This is a deferred
    !  procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder),           INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! create the StringBuilder object
    ALLOCATE(DstObj, SOURCE=SrcObj)
    
    RETURN

END SUBROUTINE StringBuilder_Clone

!**************************************************************************************

FUNCTION StringBuilder_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(IN)    :: LhsObj       !! an object
    CLASS(Assignable),    INTENT(IN)    :: RhsObj       !! another object
    tLogical                            :: Flag         !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (StringBuilder)
        IF (LhsObj%Count == RhsObj%Count) THEN
            Flag = (LhsObj%StrBuf(1:LhsObj%Count) == RhsObj%StrBuf(1:LhsObj%Count))
        ELSE
            Flag = FalseVal
        END IF
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION StringBuilder_IsEqualTo

!******************************************************************************

SUBROUTINE StringBuilder_Free(Obj)

!** PURPOSE OF THIS ROUTINE:
    !^ To free memory of the StringBuilder object.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: Obj  ! StringBuilder object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Obj%StrBuf)) DEALLOCATE(Obj%StrBuf)

    RETURN

END SUBROUTINE StringBuilder_Free

!******************************************************************************

FUNCTION StringBuilder_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS ROUTINE:
    !^ To get the name of the StringBuilder type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(IN)    :: Obj
    tCharAlloc                          :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'StringBuilder'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION StringBuilder_GetTypeName

!**************************************************************************************

FUNCTION StringBuilder_CompareTo(A, B) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To compare a StringBuilder object with a Comparable object.
    !  This is a deferred procedure inherited from the *Comparable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(IN)    :: A    !! StringBuilder object
    CLASS(Comparable),    INTENT(IN)    :: B    !! Comparable object
    tInteger                            :: Flag
    !^ output flag with value of <br>
    !   -1 if A < B, <br>
    !    0 if A == B, or <br>
    !   +1 if A > B.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT TYPE (B)
    TYPE IS (StringBuilder)
        ! compare only stored characters before the rest can be garbages.
        IF (A%StrBuf(1:A%Count) > B%StrBuf(1:B%Count)) THEN
            Flag = 1
        ELSEIF (A%StrBuf(1:A%Count) < B%StrBuf(1:B%Count)) THEN
            Flag = -1
        ELSE
            Flag = 0
        END IF
    CLASS DEFAULT
        Flag = -2
        CALL Handle_ErrLevel('StringBuilder_CompareTo', ModName, ErrSevere, 'Type of B is invalid.')
    END SELECT

    RETURN

END FUNCTION StringBuilder_CompareTo

!--------------------------------------------------------------------------------------
!                   SPECIFIC PROCEDURES OF STRING-BUILDER CLASS
!--------------------------------------------------------------------------------------

SUBROUTINE StringBuilder_CreateEmpty(StrBld, InitCap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an empty 'StringBuilder' object with optionally specified initial capacity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld  !! 'StringBuilder' object
    tIndex, OPTIONAL,     INTENT(IN)    :: InitCap !! initial capacity

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set capacity
    SET_OPTION(StrBld%Capacity, DfltInitCap, InitCap)

    ! allocate buffer
    IF (.NOT.ALLOCATED(StrBld%StrBuf)) ALLOCATE(CHARACTER(LEN=StrBld%Capacity) :: StrBld%StrBuf)
    
    ! reset the count
    StrBld%Count = 0_kIndex

    RETURN

END SUBROUTINE StringBuilder_CreateEmpty

!******************************************************************************

SUBROUTINE StringBuilder_Construct(StrBld, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a string builder with the specified value.  This routine
    !  provides a convenient way to create an empty string builder and then
    !  add the first value into the builder.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object
    CLASS(*),             INTENT(IN)    :: Value    !! value of any valid type

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL StrBld%CreateEmpty()
    CALL StrBld%Append(Value)

    RETURN

END SUBROUTINE StringBuilder_Construct

!******************************************************************************

SUBROUTINE StringBuilder_Clear(StrBld)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clear the buffer string of a 'StringBuilder' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld  !! 'StringBuilder' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    StrBld%Count = 0_kIndex

    RETURN

END SUBROUTINE StringBuilder_Clear

!******************************************************************************

SUBROUTINE StringBuilder_Finalize(StrBld)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of a 'StringBuilder' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(StringBuilder), INTENT(INOUT) :: StrBld  !! 'StringBuilder' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL StrBld%MemFree()

    RETURN

END SUBROUTINE StringBuilder_Finalize

!******************************************************************************

SUBROUTINE StringBuilder_EnsureCapacity(StrBld, StrLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make sure that a 'StringBuilder' object has enough capacity to hold
    !  character string with the specified length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object
    tIndex,               INTENT(IN)    :: StrLen   !! length of character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (StrBld%Capacity < (StrLen + StrBld%Count)) THEN
        BLOCK
            tIndex      :: NewCap
            tCharAlloc  :: TmpBuf
            NewCap = MAX(StrBld%Capacity*2_kIndex, StrBld%Capacity + StrLen)
            ! allocate temporary buffer
            ALLOCATE(CHARACTER(LEN=NewCap) :: TmpBuf)
            ! copy stored characters
            IF (StrBld%Count > 0_kIndex) TmpBuf(1:StrBld%Count) = StrBld%StrBuf(1:StrBld%Count)
            ! move allocation from TmpBuf to StrBuf
            CALL MOVE_ALLOC(TmpBuf, StrBld%StrBuf)
            ! set capacity
            StrBld%Capacity = NewCap
        END BLOCK
    END IF

    RETURN

END SUBROUTINE StringBuilder_EnsureCapacity

!******************************************************************************

SUBROUTINE StringBuilder_Append(StrBld, Value, ToString)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To append the *Value* argument at the end of the stored characters (i.e. at Count+1).
    !  The *Value* argument is automatically converted to a string if its type is valid.
    !  Otherwise, an error is reported to a default log file.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object
    CLASS(*),             INTENT(IN)    :: Value    !! value of any valid type
    PROCEDURE(Anytype2String), OPTIONAL :: ToString !! procedure to convert value to string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc,   TARGET    :: ValStr
    tCharLen(:),  POINTER   :: cStrPtr
    TYPE(FvlStr), POINTER   :: vStrPtr
    tIndex                  :: StrLen, EndID

! FLOW
    
    IF (.NOT.PRESENT(ToString)) THEN
        CALL GetPointerToValString(Value)
    ELSE
        ValStr = ToString(Value)
        cStrPtr => ValStr
    END IF
    
    IF (ASSOCIATED(cStrPtr)) THEN
        ! get string length and ensure that there is enough capacity
        StrLen = LEN(cStrPtr, KIND=kIndex)
        IF (StrLen > 0_kIndex) THEN
            CALL StrBld%EnsureCapacity(StrLen)
            ! insert characters
            EndID = StrBld%Count + StrLen
            StrBld%StrBuf(StrBld%Count+1:EndID) = cStrPtr(1:StrLen)
            StrBld%Count = EndID
        END IF
    ELSEIF (ASSOCIATED(vStrPtr)) THEN
        ! get string length and ensure that there is enough capacity
        StrLen = vStrPtr%Length()
        IF (StrLen > 0_kIndex) THEN
            CALL StrBld%EnsureCapacity(StrLen)
            ! insert characters
            EndID = StrBld%Count + StrLen
            CALL ToCharStar(vStrPtr, StrBld%StrBuf(StrBld%Count+1:EndID))
            StrBld%Count = EndID
        END IF
    END IF

    ! free memory and pointers
    NULLIFY(cStrPtr, vStrPtr)
    IF (ALLOCATED(ValStr)) DEALLOCATE(ValStr)

    RETURN
    
    CONTAINS

    SUBROUTINE GetPointerToValString(Val)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert *Val* to string.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(*), TARGET, INTENT(IN)    :: Val  !! value of any valid type

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
        
        cStrPtr => NULL()
        vStrPtr => NULL()
        
        SELECT TYPE (Val)
        TYPE IS (tCharStar)
            cStrPtr => Val
        TYPE IS (FvlStr)
            vStrPtr => Val
        TYPE IS (tInteger)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tLong)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tSingle)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tDouble)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tShort)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tByte)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tQuad)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tCmpxSingle)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tCmpxDouble)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tCmpxQuad)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tLogical)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        CLASS DEFAULT
            CALL Handle_ErrLevel('StringBuilder_Append', ModName, ErrWarning, &
                    'Type of "Value" is invalid.')
        END SELECT

        RETURN

    END SUBROUTINE GetPointerToValString

    !******************************************************************************

END SUBROUTINE StringBuilder_Append

!******************************************************************************

SUBROUTINE StringBuilder_AppendWPrefix(StrBld, Prefix, Value, Suffix, NoPreSpace, NoPostSpace, ToString)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To append the *Value* argument with a prefix text.  Optionally, a suffix text can be
    !  specified.  This routine provides a convenient way to insert a value with a prefix
    !  (and a suffix). <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object
    tCharStar,            INTENT(IN)    :: Prefix   !! prefix text
    CLASS(*),             INTENT(IN)    :: Value    !! value of any valid type
    tCharStar, OPTIONAL,  INTENT(IN)    :: Suffix   !! suffix text
    tLogical,  OPTIONAL,  INTENT(IN)    :: NoPreSpace
    !^ true if there is no space between the prefix text and the value; default is false.
    tLogical,  OPTIONAL,  INTENT(IN)    :: NoPostSpace
    !^ true if there is no space between the prefix text and the value; default is false.
    PROCEDURE(Anytype2String), OPTIONAL :: ToString !! procedure to convert value to string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: PreSpace, PostSpace

! FLOW

    ! set space flags
    PreSpace  = TrueVal
    PostSpace = TrueVal
    IF (PRESENT(NoPreSpace))  PreSpace  = .NOT.NoPreSpace
    IF (PRESENT(NoPostSpace)) PostSpace = .NOT.NoPostSpace
    
    ! append value and text(s)
    CALL StrBld%Append(Prefix)
    IF (PreSpace) CALL StrBld%Append(CHR_SPACE)
    CALL StrBld%Append(Value, ToString)
    IF (PRESENT(Suffix)) THEN
        IF (PostSpace) CALL StrBld%Append(CHR_SPACE)
        CALL StrBld%Append(Suffix)
    END IF

    RETURN

END SUBROUTINE StringBuilder_AppendWPrefix

!******************************************************************************

SUBROUTINE StringBuilder_Insert(StrBld, Value, StartID, ToString)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert the *Value* argument at the specified position.  The position must be greater
    !  than 0 and less than or equal to *Count*.  Otherwise, this is the same as appending the
    !  value.  The *Value* argument is automatically converted to a string if its type is valid.
    !  Otherwise, an error is reported to a default log file.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object
    CLASS(*),             INTENT(IN)    :: Value    !! value of any valid type
    tIndex,               INTENT(IN)    :: StartID  !! position to add the value; must be less than Count
    PROCEDURE(Anytype2String), OPTIONAL :: ToString !! procedure to convert value to string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc,   TARGET    :: ValStr
    tCharLen(:),  POINTER   :: cStrPtr
    TYPE(FvlStr), POINTER   :: vStrPtr
    tIndex                  :: StrLen, EndID
    tIndex                  :: I, J

! FLOW
    
    ! check validity of *StartID*
    IF ((StartID < 1_kIndex).OR.(StartID > StrBld%Count)) THEN
        CALL StrBld%Append(Value, ToString)
        RETURN
    END IF
    
    IF (.NOT.PRESENT(ToString)) THEN
        CALL GetPointerToValString(Value)
    ELSE
        ValStr = ToString(Value)
        cStrPtr => ValStr
    END IF

    IF (ASSOCIATED(cStrPtr)) THEN
        ! get string length and ensure that there is enough capacity
        StrLen = LEN(cStrPtr, KIND=kIndex)
        CALL StrBld%EnsureCapacity(StrLen)
        ! move characters out of the spaces to be inserted
        DO I = StrBld%Count, StartID, -1_kIndex
            J = I + StrLen
            StrBld%StrBuf(J:J) = StrBld%StrBuf(I:I)
        END DO
        ! insert characters
        EndID = StartID + StrLen - 1_kIndex
        StrBld%StrBuf(StartID:EndID) = cStrPtr(1:StrLen)
        StrBld%Count = StrBld%Count + StrLen
    ELSEIF (ASSOCIATED(vStrPtr)) THEN
        ! get string length and ensure that there is enough capacity
        StrLen = vStrPtr%Length()
        CALL StrBld%EnsureCapacity(StrLen)
        ! move characters out of the spaces to be inserted
        DO I = StrBld%Count, StartID, -1_kIndex
            J = I + StrLen
            StrBld%StrBuf(J:J) = StrBld%StrBuf(I:I)
        END DO
        ! insert characters
        EndID = StartID + StrLen - 1_kIndex
        CALL ToCharStar(vStrPtr, StrBld%StrBuf(StartID:EndID))
        StrBld%Count = StrBld%Count + StrLen
    END IF

    ! free memory and pointers
    NULLIFY(cStrPtr, vStrPtr)
    IF (ALLOCATED(ValStr)) DEALLOCATE(ValStr)

    RETURN
    
    CONTAINS

    SUBROUTINE GetPointerToValString(Val)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert *Val* to string.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(*), TARGET, INTENT(IN)    :: Val  !! value of any valid type

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
        
        cStrPtr => NULL()
        vStrPtr => NULL()
        
        SELECT TYPE (Val)
        TYPE IS (tCharStar)
            cStrPtr => Val
        TYPE IS (FvlStr)
            vStrPtr => Val
        TYPE IS (tInteger)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tLong)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tSingle)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tDouble)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tShort)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tByte)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tQuad)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tCmpxSingle)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tCmpxDouble)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tCmpxQuad)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        TYPE IS (tLogical)
            ValStr = CharString(Val)
            cStrPtr => ValStr
        CLASS DEFAULT
            CALL Handle_ErrLevel('StringBuilder_Insert', ModName, ErrWarning, &
                    'Type of "Value" is invalid.')
        END SELECT

        RETURN

    END SUBROUTINE GetPointerToValString

    !******************************************************************************

END SUBROUTINE StringBuilder_Insert

!******************************************************************************

FUNCTION StringBuilder_ToCharAlloc(StrBld, ClearBuffer) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To return characters currently stored in the string builder as
    !  an allocatable character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object
    tLogical, OPTIONAL,   INTENT(IN)    :: ClearBuffer
    !^ true if requesting to clear the builder's buffer after getting the string;
    !  default is false.
    tCharAlloc                          :: cStr     !! string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: ClearBuf

! FLOW
    
    SET_OPTION(ClearBuf, FalseVal, ClearBuffer)
    IF (StrBld%Count > 0_kIndex) THEN
        cStr = StrBld%StrBuf(1:StrBld%Count)
    ELSE
        cStr = ''
    END IF
    IF (ClearBuf) CALL StrBld%Clear()

    RETURN

END FUNCTION StringBuilder_ToCharAlloc

!******************************************************************************

FUNCTION StringBuilder_ToFvlStr(StrBld, ClearBuffer) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To return characters currently stored in the string builder as
    !  a string of the *FvlStr* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object
    tLogical, OPTIONAL,   INTENT(IN)    :: ClearBuffer
    !^ true if requesting to clear the builder's buffer after getting the string;
    !  default is false.
    TYPE(FvlStr)                        :: vStr     !! string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: ClearBuf

! FLOW
    
    SET_OPTION(ClearBuf, FalseVal, ClearBuffer)
    IF (StrBld%Count > 0_kIndex) THEN
        vStr = StrBld%StrBuf(1:StrBld%Count)
    ELSE
        vStr = ''
    END IF
    IF (ClearBuf) CALL StrBld%Clear()

    RETURN

END FUNCTION StringBuilder_ToFvlStr

!******************************************************************************

SUBROUTINE StringBuilder_ToCharStar(StrBld, cStr, ClearBuffer)

!** PURPOSE OF THIS ROUTINE:
    !^ To return characters currently stored in the string builder as
    !  a character string of assumed length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object
    tCharStar,            INTENT(OUT)   :: cStr     !! character string
    tLogical, OPTIONAL,   INTENT(IN)    :: ClearBuffer
    !^ true if requesting to clear the builder's buffer after getting the string;
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: ClearBuf
    tIndex      :: MinLen

! FLOW
    
    SET_OPTION(ClearBuf, FalseVal, ClearBuffer)
    MinLen = MIN(LEN(cStr), StrBld%Count)
    IF (MinLen > 0_kIndex) cStr(1:MinLen) = StrBld%StrBuf(1:MinLen)
    IF (ClearBuf) CALL StrBld%Clear()

    RETURN

END SUBROUTINE StringBuilder_ToCharStar

!******************************************************************************

FUNCTION StringBuilder_GetLength(StrBld) RESULT(Length)

!** PURPOSE OF THIS ROUTINE:
    !^ To return the current length of the string being built (i.e. the number
    !  of characters currently stored in the builder).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object
    tIndex                              :: Length   !! length of the string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = StrBld%Count

    RETURN

END FUNCTION StringBuilder_GetLength

!******************************************************************************

SUBROUTINE StringBuilder_DeleteLastChar(StrBld)

!** PURPOSE OF THIS ROUTINE:
    !^ To delete the last character of the string being built.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StringBuilder), INTENT(INOUT) :: StrBld   !! 'StringBuilder' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (StrBld%Count > 0_kIndex) StrBld%Count = StrBld%Count - 1_kIndex

    RETURN

END SUBROUTINE StringBuilder_DeleteLastChar

!******************************************************************************

END MODULE Class_StringBuilder
    
!******************************************************************************
