
MODULE Class_OrderedSymTable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *OrderedSymTable* type and its related routines.
!   The *OrderedSymTable* type is an abstract type representing an ordered symbol
!   table, which is a collection containing key-value pairs that keeps the keys
!   in order. <br>
!   The *OrderedSymTable* type is a subtype of the *BaseSymTable* type and thus
!   inherits all methods of the *BaseSymTable* type and all its super classes.
!   The *OrderedSymTable* type provides an expanded API that defines numerous
!   natural and useful operations involving relative key order. <br>
!   It is important to note that checking the key equality is usually sufficient
!   for an unordered symbol table.  However, for an ordered symbol table, the
!   comparison between keys should provide a total ordering on all the keys.
!   This means that although all Fortran intrinsic types (with the exception of
!   the *LOGICAL* type) can be used as a type of the key in an unordered symbol
!   table, only *CHARACTER*, *INTEGER* and *REAL* types can be used as a type of
!   the key in an ordered symbol table.  To use a derived type as a type of the
!   key, the key type should be in the *Assignable* class for an unordered symbol
!   table and in the *Comparable* class for an ordered symbol table.

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_BaseSymTable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: OrderedSymTable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_OrderedSymTable'

!** DERIVED TYPE 
    !> The *OrderedSymTable* type is an abstract collection type that
    !  defines an API for an ordered symbol table.
    TYPE, ABSTRACT, EXTENDS(BaseSymTable) :: OrderedSymTable
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *GetMinKey* is a binding name of the *GetMin* deferred procedure. <br>
        !  **Type-Bound Function**: GetMinKey <br>
        !  **Purpose**:  To get the smallest key (and optionally a value associated
        !                with it) in a symbol table.  Also, return a flag indicating
        !                whether the key is successfully retrieved or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetMinKey(Key) <br>
        !   --->    IF (.NOT.Collection%GetMinKey(Key, Value)) DoSomething
        PROCEDURE(GetMin),     DEFERRED :: GetMinKey
        !> *GetMaxKey* is a binding name of the *GetMax* deferred procedure. <br>
        !  **Type-Bound Function**: GetMaxKey <br>
        !  **Purpose**:  To get the largest key (and optionally a value associated
        !                with it) in a symbol table.  Also, return a flag indicating
        !                whether the key is successfully retrieved or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetMaxKey(Key) <br>
        !   --->    IF (.NOT.Collection%GetMaxKey(Key, Value)) DoSomething
        PROCEDURE(GetMax),     DEFERRED :: GetMaxKey
        !> *Floor* is a binding name of the *GetFloor* deferred procedure. <br>
        !  **Type-Bound Function**: Floor <br>
        !  **Purpose**:  To get the largest key (and optionally a value associated
        !                with it) in a symbol table less than or equal to the given
        !                key.  Also, return a flag indicating whether the floor key
        !                is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Floor(Key) <br>
        !   --->    IF (.NOT.Collection%Floor(Key, Value)) DoSomething
        PROCEDURE(GetFloor),   DEFERRED :: Floor
        !> *Ceiling* is a binding name of the *GetCeiling* deferred procedure. <br>
        !  **Type-Bound Function**: Ceiling <br>
        !  **Purpose**:  To get the smallest key (and optionally a value associated
        !                with it) in a symbol table greater than or equal to the given
        !                key.  Also, return a flag indicating whether the ceiling key
        !                is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Ceiling(Key) <br>
        !   --->    IF (.NOT.Collection%Ceiling(Key, Value)) DoSomething
        PROCEDURE(GetCeiling), DEFERRED :: Ceiling
        !> *GetRank* is a binding name of the *KeyRank* deferred procedure. <br>
        !  **Type-Bound Function**: Floor <br>
        !  **Purpose**:  To return the number of keys in the symbol table strictly
        !                less than the given key. <br>
        !  **Usage**: <br>
        !   --->    KeyRank = Collection%GetRank(Key)
        PROCEDURE(KeyRank),    DEFERRED :: GetRank
        !> *Select* is a binding name of the *KeySelect* deferred procedure. <br>
        !  **Type-Bound Subroutine**: Select <br>
        !  **Purpose**:  To get the key (and optionally its associated value) of the
        !                specified rank where the applicable range of rank is between
        !                0 and TableSize-1. Also, return a flag indicating whether the
        !                ranked key is successfully retrieved or not. <br>
        !   --->    Flag = Collection%Select(Rank, Key) <br>
        !   --->    IF (.NOT.Collection%Select(Rank, Key, Value)) DoSomething
        PROCEDURE(KeySelect),  DEFERRED :: Select
        !> *RemoveMin* is a binding name of the *DelMin* deferred procedure. <br>
        !  **Type-Bound Function**: RemoveMin <br>
        !  **Purpose**:  To remove the smallest key (and a value associated with it)
        !                from a symbol table.  Also, return a flag indicating
        !                whether the key is successfully removed or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%RemoveMin() <br>
        !   --->    Flag = Collection%RemoveMin(MinKey) <br>
        !   --->    Flag = Collection%RemoveMin(Value=MinVal) <br>
        !   --->    IF (.NOT.Collection%RemoveMin(MinKey, MinVal)) DoSomething
        PROCEDURE(DelMin),     DEFERRED :: RemoveMin
        !> *RemoveMax* is a binding name of the *DelMax* deferred procedure. <br>
        !  **Type-Bound Function**: RemoveMax <br>
        !  **Purpose**:  To remove the largest key (and a value associated with it)
        !                from a symbol table.  Also, return a flag indicating
        !                whether the key is successfully removed or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%RemoveMax() <br>
        !   --->    Flag = Collection%RemoveMax(MaxKey) <br>
        !   --->    Flag = Collection%RemoveMax(Value=MaxVal) <br>
        !   --->    IF (.NOT.Collection%RemoveMax(MaxKey, MaxVal)) DoSomething
        PROCEDURE(DelMax),     DEFERRED :: RemoveMax
        !> *GetRangeSize* is a binding name of the *RangeSize* deferred procedure. <br>
        !  **Type-Bound Function**: GetRangeSize <br>
        !  **Purpose**:  To return the number of keys between *KeyLo* (inclusive)
        !                and *KeyHi* (inclusive). <br>
        !  **Usage**: <br>
        !   --->    RangeSize = Collection%GetRangeSize(KeyLo, KeyHi)
        PROCEDURE(RangeSize),  DEFERRED :: GetRangeSize
        ! ---------------------------------------------------------------------
    END TYPE OrderedSymTable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *GetMin* is a deferred procedure to get the smallest key (and optionally
        !  a value associated with the key) in a symbol table.  Also, return a flag
        !  indicating whether the key is successfully retrieved or not.  If the table
        !  is empty, the flag is typically false.  Otherwise, the flag is always true.
        FUNCTION GetMin(Collection, Key, Value) RESULT(Flag)
            IMPORT
            !% symbol-table collection object
            CLASS(OrderedSymTable(*)), INTENT(INOUT)    :: Collection
            !% the smallest key to be retrieved from the collection
            CLASS(*),                  INTENT(OUT)      :: Key
            !% the value associated with the smallest key
            TYPE(*), OPTIONAL,         INTENT(INOUT)    :: Value
            !% flag indicating whether the key-value pair is found or not.
            tLogical                                    :: Flag
        END FUNCTION GetMin
        !> *GetMax* is a deferred procedure to get the largest key (and optionally
        !  a value associated with the key) in a symbol table.  Also, return a flag
        !  indicating whether the key is successfully retrieved or not.  If the table
        !  is empty, the flag is typically false.  Otherwise, the flag is always true.
        FUNCTION GetMax(Collection, Key, Value) RESULT(Flag)
            IMPORT
            !% symbol-table collection object
            CLASS(OrderedSymTable(*)), INTENT(INOUT)    :: Collection
            !% the largest key to be retrieved from the collection
            CLASS(*),                  INTENT(OUT)      :: Key
            !% the value associated with the largest key
            TYPE(*), OPTIONAL,         INTENT(INOUT)    :: Value
            !% flag indicating whether the key-value pair is found or not.
            tLogical                                    :: Flag
        END FUNCTION GetMax
        !> *GetFloor* is a deferred procedure to get the largest key (and optionally
        !  a value associated with the key) in a symbol table less than or equal to
        !  the given key.  Also, return a flag indicating whether the floor key is
        !  successfully retrieved or not.
        FUNCTION GetFloor(Collection, KeyIn, KeyOut, ValOut) RESULT(Flag)
            IMPORT
            !% symbol-table collection object
            CLASS(OrderedSymTable(*)), INTENT(INOUT)    :: Collection
            !% the specified key
            CLASS(*),                  INTENT(IN)       :: KeyIn
            !% the largest key in the table less than or equal to the given key
            CLASS(*),                  INTENT(OUT)      :: KeyOut
            !% the value associated with the largest key
            TYPE(*), OPTIONAL,         INTENT(INOUT)    :: ValOut
            !% flag indicating whether the key-value pair is found or not.
            tLogical                                    :: Flag
        END FUNCTION GetFloor
        !> *GetCeiling* is a deferred procedure to get the smallest key (and optionally
        !  a value associated with the key) in a symbol table greater than or equal to
        !  the given key.  Also, return a flag indicating whether the ceiling key is
        !  successfully retrieved or not.
        FUNCTION GetCeiling(Collection, KeyIn, KeyOut, ValOut) RESULT(Flag)
            IMPORT
            !% symbol-table collection object
            CLASS(OrderedSymTable(*)), INTENT(INOUT)    :: Collection
            !% the specified key
            CLASS(*),                  INTENT(IN)       :: KeyIn
            !% the smallest key in the table less than or equal to the given key
            CLASS(*),                  INTENT(OUT)      :: KeyOut
            !% the value associated with the smallest key
            TYPE(*), OPTIONAL,         INTENT(INOUT)    :: ValOut
            !% flag indicating whether the key-value pair is found or not.
            tLogical                                    :: Flag
        END FUNCTION GetCeiling
        !> *KeyRank* is a deferred procedure to get the number of keys in the symbol
        !  table strictly less than the given key.
        FUNCTION KeyRank(Collection, Key) RESULT(Rank)
            IMPORT
            !% symbol-table collection object
            CLASS(OrderedSymTable(*)), INTENT(INOUT)    :: Collection
            !% the specified key
            CLASS(*),                  INTENT(IN)       :: Key
            !% the number of keys less than the given key.
            tIndex                                      :: Rank
        END FUNCTION KeyRank
        !> *KeySelect* is a deferred procedure to get the key (and optionally its
        !   associated value) of the given rank.  Also, return a flag indicating
        !   whether the ranked key is successfully retrieved or not. <br>
        !  This ranked key has the property such that there are keys in the symbol
        !  table that are smaller.  In other words, this key is the (rank+1)st smallest
        !  key in the table. <br>
        !  The applicable range of rank is between 0 and TableSize-1 where the rank number
        !  is zero-based.  If the specified rank is out of range or the table is empty,
        !  the returned flag is false.  Otherwise, the returned flag is true.
        FUNCTION KeySelect(Collection, Rank, Key, Value) RESULT(Flag)
            IMPORT
            !% symbol-table collection object
            CLASS(OrderedSymTable(*)), INTENT(INOUT)    :: Collection
            !% the specified rank.
            tIndex,                    INTENT(IN)       :: Rank
            !% the key of the specified rank
            CLASS(*),                  INTENT(OUT)      :: Key
            !% the value associated with the ranked key
            TYPE(*), OPTIONAL,         INTENT(INOUT)    :: Value
            !% flag indicating whether the key-value pair is found or not.
            tLogical                                    :: Flag
        END FUNCTION KeySelect
        !> *DelMin* is a deferred procedure to remove the smallest key and a value
        !  associated with the key from a symbol table.  Also, return a flag indicating
        !  whether the key is successfully removed or not.  If the table is empty,
        !  the flag is typically false.  Otherwise, the flag is always true.
        FUNCTION DelMin(Collection, Key, Value) RESULT(Flag)
            IMPORT
            !% symbol-table collection object
            CLASS(OrderedSymTable(*)), INTENT(INOUT)    :: Collection
            !% the smallest key
            CLASS(*), OPTIONAL,        INTENT(OUT)      :: Key
            !% the value associated with the smallest key
            TYPE(*),  OPTIONAL,        INTENT(INOUT)    :: Value
            !% flag indicating whether the key is successfully removed or not
            tLogical                                    :: Flag
        END FUNCTION DelMin
        !> *DelMax* is a deferred procedure to remove the largest key and a value
        !  associated with the key from a symbol table.  Also, return a flag indicating
        !  whether the key is successfully removed or not.  If the table is empty,
        !  the flag is typically false.  Otherwise, the flag is always true.
        FUNCTION DelMax(Collection, Key, Value) RESULT(Flag)
            IMPORT
            !% symbol-table collection object
            CLASS(OrderedSymTable(*)), INTENT(INOUT)    :: Collection
            !% the largest key
            CLASS(*), OPTIONAL,        INTENT(OUT)      :: Key
            !% the value associated with the largest key
            TYPE(*),  OPTIONAL,        INTENT(INOUT)    :: Value
            !% flag indicating whether the key is successfully removed or not
            tLogical                                    :: Flag
        END FUNCTION DelMax
        !> *RangeSize* is a deferred procedure to get the number of keys in
        !  the given range (between KeyLo and KeyHi).
        FUNCTION RangeSize(Collection, KeyLo, KeyHi) RESULT(Size)
            IMPORT
            !% symbol-table collection object
            CLASS(OrderedSymTable(*)), INTENT(INOUT)    :: Collection
            !% the minimum key (inclusive)
            CLASS(*),                  INTENT(IN)       :: KeyLo
            !% the maximum key (inclusive)
            CLASS(*),                  INTENT(IN)       :: KeyHi
            !% the number of keys in the given range.
            tIndex                                      :: Size
        END FUNCTION RangeSize
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    !  na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!**************************************************************************************

END MODULE Class_OrderedSymTable

!******************************************************************************
