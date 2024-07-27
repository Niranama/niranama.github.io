
! -----------------------------------------------------------------------------
! -----                       TabItem Procedures                          -----
! -----------------------------------------------------------------------------

SUBROUTINE TabItem_Clear(Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To free memory of the item's components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem), INTENT(INOUT) :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Item%Key = NULKEY
    IF (ALLOCATED(Item%Value)) DEALLOCATE(Item%Value)

    RETURN

END SUBROUTINE TabItem_Clear

!******************************************************************************

SUBROUTINE TabItem_Deallocate(Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To free memory of the item's components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem), INTENT(INOUT) :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (ALLOCATED(Item%Key))   DEALLOCATE(Item%Key)
    IF (ALLOCATED(Item%Value)) DEALLOCATE(Item%Value)

    RETURN

END SUBROUTINE TabItem_Deallocate

!******************************************************************************

SUBROUTINE TabItem_SetKeyValue(Item, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set value of the specified item.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem),     INTENT(INOUT)   :: Item
    KeyTypeB, OPTIONAL, INTENT(IN)      :: Key
    CLASS(*), OPTIONAL, INTENT(IN)      :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (PRESENT(Key)) THEN
        Item%Key = Key
    END IF
    IF (PRESENT(Value)) THEN
        IF (ALLOCATED(Item%Value)) DEALLOCATE(Item%Value)
        ALLOCATE(Item%Value, SOURCE=Value)
    END IF

    RETURN

END SUBROUTINE TabItem_SetKeyValue

!******************************************************************************

SUBROUTINE TabItem_GetValue(Item, Value)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get value of the specified item.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem),                  INTENT(INOUT)  :: Item
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=Item%Value)

    RETURN

END SUBROUTINE TabItem_GetValue

! -----------------------------------------------------------------------------
! -----                        Common Procedures                          -----
! -----------------------------------------------------------------------------

SUBROUTINE HashTable_CreateEmpty(Table, InitCap, LoadFactor, ProbAlgo, HashCalc)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To create an empty hash table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTable object
    CLASS(HashTable),   INTENT(INOUT)   :: Table
    !% initial capacity of the hash table
    tIndex,   OPTIONAL, INTENT(IN)      :: InitCap
    !% load factor
    tDouble,  OPTIONAL, INTENT(IN)      :: LoadFactor
    !% probing algorithm (1 = Linear, 2 = Quadratic, 3 = Double Hashing)
    tInteger, OPTIONAL, INTENT(IN)      :: ProbAlgo
    !> hash function to compute the hash value of the key;
    !  if not present, use default hash function.
    PROCEDURE(HashFunc), OPTIONAL       :: HashCalc

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! determine initial capacity
    IF (PRESENT(InitCap)) THEN
        IF (InitCap < 0_kIndex) THEN
            Table%Capacity = DefaultCapacity
        ELSE
            Table%Capacity = MAX(DefaultCapacity, InitCap)
        END IF
    ELSE
        Table%Capacity = DefaultCapacity
    END IF

    ! determine load factor
    IF (PRESENT(LoadFactor)) THEN
        IF (InitCap < 0.0_kDouble) THEN
            Table%LoadFactor = DefaultLoadFactor
        ELSE
            Table%LoadFactor = LoadFactor
        END IF
    ELSE
        Table%LoadFactor = DefaultLoadFactor
    END IF

    ! determine probing algorithm
    IF (PRESENT(ProbAlgo)) THEN
        SELECT CASE (ProbAlgo)
        CASE (1:3)
            Table%ProbAlgo = ProbAlgo
        CASE DEFAULT
            Table%ProbAlgo = LinearProbing
        END SELECT
    ELSE
        Table%ProbAlgo = LinearProbing
    END IF

    ! set hash function pointer
    IF (PRESENT(HashCalc)) THEN
        ! use supplied function
        Table%HashCalc => HashCalc
    ELSE
        ! use default algorithm
        Table%HashCalc => HashFuncOpt
    END IF

    ! adjust the capacity according to the probing algorithm
    CALL AdjustCapacity(Table)

    ! compute threshold
    Table%Threshold = ToIndex(Table%Capacity*Table%LoadFactor)

    ! allocate memory of item storages
    ALLOCATE(Table%Items(0:Table%Capacity-1))

    ! set key to null
    BLOCK
        tIndex      :: I
        DO I = 0_kIndex, Table%Capacity-1_kIndex
            Table%Items(I)%Key = NULKEY
        END DO
    END BLOCK

    RETURN

END SUBROUTINE HashTable_CreateEmpty

!******************************************************************************

SUBROUTINE HashTable_CreateByArray(Table, N, Keys, Values, LoadFactor, ProbAlgo, HashCalc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a table from an array of key-value pairs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTable object
    CLASS(HashTable),   INTENT(INOUT)   :: Table
    !% number of key-value pairs
    tIndex,             INTENT(IN)      :: N
    !% the keys to be added to the table
    KeyTypeB,           INTENT(IN)      :: Keys(N)
    !% the associated values to be added to the table
    CLASS(*),           INTENT(IN)      :: Values(N)
    !% load factor
    tDouble,  OPTIONAL, INTENT(IN)      :: LoadFactor
    !% probing algorithm (1 = Linear, 2 = Quadratic, 3 = Double Hashing)
    tInteger, OPTIONAL, INTENT(IN)      :: ProbAlgo
    !> hash function to compute the hash value of the key;
    !  if not present, use default hash function.
    PROCEDURE(HashFunc), OPTIONAL       :: HashCalc

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! create empty symbol table with capacity twice of the key size
    CALL Table%CreateEmpty(N*2_kIndex, LoadFactor, ProbAlgo, HashCalc)

    ! add key-value pairs to the table
    DO I = 1_kIndex, N
        CALL Table%Insert(Keys(I), Values(I))
    END DO

    RETURN

END SUBROUTINE HashTable_CreateByArray

!******************************************************************************

SUBROUTINE HashTable_ClearItems(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To free components of the items from the table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW:

    DO I = 0_kIndex, Table%Capacity-1_kIndex
        CALL TabItem_Clear(Table%Items(I))
    END DO
    Table%KeyCount = 0_kIndex
    Table%UsedBuckets = 0_kIndex
    Table%ModCount = Table%ModCount + 1_kIndex

    RETURN

END SUBROUTINE HashTable_ClearItems

!******************************************************************************

SUBROUTINE HashTable_Destroy(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! first free items' components and then free of the items themselves
    IF (ALLOCATED(Table%Items)) THEN
        BLOCK
            tIndex      :: I
            DO I = 0_kIndex, Table%Capacity-1_kIndex
                CALL TabItem_Deallocate(Table%Items(I))
            END DO
        END BLOCK
        DEALLOCATE(Table%Items)
    END IF

    ! reset all components
    Table%Capacity = DefaultCapacity
    Table%Indx = 0_kIndex
    Table%KeyLeft = 0_kIndex
    Table%IterModCount = 0_kIndex
    Table%HashCalc => NULL()
    Table%LoadFactor = DefaultLoadFactor
    Table%Threshold = 0_kIndex
    Table%ModCount = 0_kIndex
    Table%UsedBuckets = 0_kIndex
    Table%KeyCount = 0_kIndex
    Table%ProbAlgo = LinearProbing
    Table%HashIndx = 0_kIndex

    RETURN

END SUBROUTINE HashTable_Destroy

!******************************************************************************

SUBROUTINE HashTable_Finalize(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To perform finalization of the HashTable object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    CALL Table%Destruct()

    RETURN

END SUBROUTINE HashTable_Finalize

!******************************************************************************

FUNCTION HashTable_GetSize(Table) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of keys currently in the hash table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(IN)    :: Table    !! HashTable object
    tIndex                          :: Size     !! the number of keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Table%KeyCount

    RETURN

END FUNCTION HashTable_GetSize

!******************************************************************************

FUNCTION HashTable_IsEmpty(Table) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the hash table is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(IN)    :: Table    !! HashTable object
    tLogical                        :: Flag     !! true if the table is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (Table%KeyCount == 0_kIndex)

    RETURN

END FUNCTION HashTable_IsEmpty

!******************************************************************************

SUBROUTINE HashTable_Resize(Table, MoreCap)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To resize the capacity of the hash table according the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    tLogical,         INTENT(IN)    :: MoreCap
    !^ true if increasing the capacity; otherwise, decreasing the capacity

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabItem), ALLOCATABLE  :: OldItems(:)
    tIndex                      :: OldCap, I

!** FLOW:

    OldCap = Table%Capacity
    IF (MoreCap) THEN
        CALL IncreaseCapacity(Table)
    ELSE
        ! halving the capacity
        Table%Capacity = Table%Capacity/2_kIndex
    END IF
    CALL AdjustCapacity(Table)

    ! update threshold
    Table%Threshold = ToIndex(Table%Capacity*Table%LoadFactor)

    ! move currently stored objects to temporary variable
    CALL MOVE_ALLOC(Table%Items, OldItems)

    ! allocate working items to new capacity
    ALLOCATE(Table%Items(0:Table%Capacity-1_kIndex))

    ! Reset the key count and buckets used since we are about to
    ! re-insert all the keys into the hash-table.
    Table%KeyCount = 0_kIndex
    Table%UsedBuckets = 0_kIndex

    ! loop over the temporary lists to move stored objects (Items)
    ! back to the working lists of the hash table
    DO I = 0_kIndex, OldCap-1_kIndex
        IF ((OldItems(I)%Key /= NULKEY).AND.(OldItems(I)%Key /= DELKEY)) THEN
            ! re-insert the key and its associated value
            CALL Table%Insert(OldItems(I)%Key, OldItems(I)%Value)
        END IF
        CALL TabItem_Clear(OldItems(I))
    END DO

    ! free temporary variable
    DEALLOCATE(OldItems)

    RETURN

END SUBROUTINE HashTable_Resize

!******************************************************************************

SUBROUTINE HashTable_Insert(Table, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key-value pair into the hash table.  If the specified key
    !  is already stored in the table, replace the old value with the
    !  new one.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    KeyTypeB,         INTENT(IN)    :: Key      !! key to be inserted
    CLASS(*),         INTENT(IN)    :: Value    !! associated value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I, J
    tIndex      :: KeySize

! FLOW

    ! resize the capacity if needed
    IF (Table%UsedBuckets >= Table%Threshold) THEN
        CALL Table%Resize(MoreCap=TrueVal)
    END IF

    ! set up the probing if needed
    CALL SetupProbing(Table, Key)

    ! compute the hash code and offset
    KeySize  = Bytes_Char*LEN(Key)
    HashCode = Table%HashCalc(Key, KeySize, HashSeed, RemoveSign=TrueVal)
    Offset   = ComputeIndex(HashCode, Table%Capacity)

    ! initialize indices
    I = Offset
    J = -1_kIndex
    Indx = 1_kIndex
    DO
        IF (Table%Items(I)%Key == DELKEY) THEN
            ! The current bucket was previously deleted
            IF (J == -1_kIndex) J = I
        ELSEIF (Table%Items(I)%Key /= NULKEY) THEN
            ! The current bucket already contains a key
            IF (Table%Items(I)%Key == Key) THEN
                ! The key we're trying to insert already exists in the hash-table,
                ! so update its value with the most recent value
                IF (J == -1_kIndex) THEN
                    CALL TabItem_SetKeyValue(Table%Items(I), Value=Value)
                ELSE
                    CALL TabItem_SetKeyValue(Table%Items(I), Key=DELKEY)
                    CALL TabItem_SetKeyValue(Table%Items(J), Key, Value)
                END IF
                Table%ModCount = Table%ModCount + 1_kIndex
                EXIT
            END IF
        ELSE
            ! The current bucket is null so an insertion/update can occur
            IF (J == -1_kIndex) THEN
                ! No previously encountered deleted buckets
                Table%UsedBuckets = Table%UsedBuckets + 1_kIndex
                CALL TabItem_SetKeyValue(Table%Items(I), Key, Value)
            ELSE
                ! Previously seen deleted bucket. Instead of inserting
                ! the new element at i where the null element is insert
                ! it where the deleted token was found.
                CALL TabItem_SetKeyValue(Table%Items(J), Key, Value)
            END IF
            Table%KeyCount = Table%KeyCount + 1_kIndex
            Table%ModCount = Table%ModCount + 1_kIndex
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Table, Indx), Table%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    RETURN

END SUBROUTINE HashTable_Insert

!******************************************************************************

FUNCTION HashTable_Remove(Table, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified key (and its associated value) from a symbol
    !  table.  Also, return a flag indicating whether the key-value pair is
    !  successfully removed or not.  Optionally, retrieve the associated
    !  value if the key exists in the table.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable),                INTENT(INOUT)  :: Table    !! HashTable object
    KeyTypeB,                        INTENT(IN)     :: Key      !! key to be removed
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I
    tIndex      :: KeySize

! FLOW

    IF (Table%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! set up the probing if needed
    CALL SetupProbing(Table, Key)

    ! compute the hash code and offset
    KeySize  = Bytes_Char*LEN(Key)
    HashCode = Table%HashCalc(Key, KeySize, HashSeed, RemoveSign=TrueVal)
    Offset   = ComputeIndex(HashCode, Table%Capacity)

    ! initialize indices
    I = Offset
    Indx = 1_kIndex
    ! Starting at the original hash probe until we find a spot where our key is
    ! or we hit a null element in which case our element does not exist.
    DO
        IF (Table%Items(I)%Key == DELKEY) THEN
            ! ignore deleted bucket so do nothing here
        ELSEIF (Table%Items(I)%Key == NULKEY) THEN
            ! the key was not found
            Flag = FalseVal
            EXIT
        ELSEIF (Table%Items(I)%Key == Key) THEN
            ! found the key we want to remove
            Table%KeyCount = Table%KeyCount - 1_kIndex
            Table%ModCount = Table%ModCount + 1_kIndex
            CALL TabItem_GetValue(Table%Items(I), Value)
            CALL TabItem_SetKeyValue(Table%Items(I), Key=DELKEY)
            Flag = TrueVal
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Table, Indx), Table%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    ! halve the hash table capacity if it is 12.5% full or less
    IF ((Table%KeyCount > 0_kIndex).AND.(Table%KeyCount <= Table%Capacity/8)) THEN
        CALL Table%Resize(MoreCap=FalseVal)
    END IF

    RETURN

END FUNCTION HashTable_Remove

!******************************************************************************

FUNCTION HashTable_Contain(Table, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    KeyTypeB,         INTENT(IN)    :: Key      !! key to be looked for
    !% flag indicating whether the specified key is found or not.
    tLogical                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Table%IsEmpty()) THEN
        Found = FalseVal
    ELSE
        ! find the key
        Found = Table%FindKey(Key)
    END IF

    RETURN

END FUNCTION HashTable_Contain

!******************************************************************************

FUNCTION HashTable_GetValue(Table, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the associated value of the specified key.  Also, return
    !  a flag indicating whether the value is successfully retrieved or not. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable),      INTENT(INOUT)    :: Table    !! HashTable object
    KeyTypeB,              INTENT(IN)       :: Key      !! key to be looked for
    CLASS(*), ALLOCATABLE, INTENT(OUT)      :: Value    !! associated value
    !% flag indicating whether the value is successfully retrieved or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Table%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        ! find the key and get its associated value
        Flag = Table%FindKey(Key, Value)
    END IF

    RETURN

END FUNCTION HashTable_GetValue

!******************************************************************************

FUNCTION HashTable_FindKey(Table, Key, Value) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.
    !  Optionally, retrieve the associated value if the key is found. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable),                INTENT(INOUT)  :: Table    !! HashTable object
    KeyTypeB,                        INTENT(IN)     :: Key      !! key to be looked for
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !% flag indicating whether the specified key is found or not.
    tLogical                                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I, J
    tIndex      :: KeySize

! FLOW

    IF (Table%IsEmpty()) THEN
        Found = FalseVal
        RETURN
    END IF

    ! set up the probing if needed
    CALL SetupProbing(Table, Key)

    ! compute the hash code and offset
    KeySize  = Bytes_Char*LEN(Key)
    HashCode = Table%HashCalc(Key, KeySize, HashSeed, RemoveSign=TrueVal)
    Offset   = ComputeIndex(HashCode, Table%Capacity)

    ! initialize indices
    I = Offset
    J = -1_kIndex
    Indx = 1_kIndex
    ! Start at the original hash value and probe until we find a spot where our key
    ! is or hit a null element in which case our element does not exist.
    DO
        IF (Table%Items(I)%Key == DELKEY) THEN
            ! Ignore deleted buckets, but record where the first index
            ! of a deleted bucket is found to perform lazy relocation later.
            IF (J == -1_kIndex) J = I
        ELSEIF (Table%Items(I)%Key /= NULKEY) THEN
            ! found the key we want to remove
            IF (J /= -1_kIndex) THEN
                ! If J /= -1 this means we previously encountered a deleted cell.
                ! We can perform an optimization by swapping the entries in cells
                ! I and J so that the next time we search for this key it will be
                ! found faster. This is called lazy deletion/relocation.
                CALL TabItem_SetKeyValue(Table%Items(J), Table%Items(I)%Key, Table%Items(I)%Value)
                CALL TabItem_SetKeyValue(Table%Items(I), Key=DELKEY)
                CALL TabItem_GetValue(Table%Items(J), Value)
            ELSE
                CALL TabItem_GetValue(Table%Items(I), Value)
            END IF
            Found = TrueVal
            EXIT
        ELSE
            ! the key was not found
            Found = FalseVal
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Table, Indx), Table%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    RETURN

END FUNCTION HashTable_FindKey

!******************************************************************************

FUNCTION HashTable_Move2FirstPair(Table, Key, Value) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a hash table.   For the hash table,
    !  which is an unordered symbol table, the starting pair is the first pair found
    !  in the non-empty bucket.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable),                INTENT(INOUT)  :: Table    !! HashTable object
    !% the first key as output if requested (and available)
    KeyTypeA,              OPTIONAL, INTENT(OUT)    :: Key
    !% the first value as output if requested (and available)
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(INOUT)  :: Value
    !> a flag indicating whether the table contains no pair data or not <br>
    ! - true if the table is empty. <br>
    ! - otherwise the first pair data is available.
    tLogical                                        :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Table%IsEmpty()) THEN
        IsEmpty = TrueVal
        RETURN
    ELSE
        IsEmpty = FalseVal
    END IF

    ! initialize iteration-related components
    Table%Indx = 0_kIndex
    Table%KeyLeft = Table%KeyCount
    Table%IterModCount = Table%ModCount

    ! start iteration by looking for the first non-empty slot
    DO WHILE ((Table%Items(Table%Indx)%Key == NULKEY).OR.(Table%Items(Table%Indx)%Key == DELKEY))
        Table%Indx = Table%Indx + 1_kIndex
    END DO

    ! update KeyLelf
    Table%KeyLeft = Table%KeyLeft - 1_kIndex

    ! get key if requested
    IF (PRESENT(Key)) Key = Table%Items(Table%Indx)%Key

    ! get value if requested
    IF (PRESENT(Value)) CALL TabItem_GetValue(Table%Items(Table%Indx), Value)

    RETURN

END FUNCTION HashTable_Move2FirstPair

!******************************************************************************

FUNCTION HashTable_Move2NextPair(Table, Key, Value) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a symbol table.  For the *HashTable*, which
    !  is an unordered symbol table,  the next pair is a pair inserted in the first
    !  non-empty bucket after the previous one.  <br>
    !  The routine will report an error if an alteration to stored item(s) (either
    !  by an insertion or a removal) has been occurred during current iteration.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable),                INTENT(INOUT)  :: Table    !! HashTable object
    !% the next key as output if requested (and available)
    KeyTypeA,              OPTIONAL, INTENT(OUT)    :: Key
    !% the next value as output if requested (and available)
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(INOUT)  :: Value
    !> a flag indicating whether the move to the end of the table occurs or not <br>
    ! - true if next pair data is NOT available. <br>
    ! - otherwise next pair data is available.
    tLogical                                        :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Table%IterModCount /= Table%ModCount) THEN
        CALL Handle_ErrLevel('HashTable_Move2NextPair', ModName, ErrWarning, &
                 "Must re-start the iteration because the stored items have been altered.")
        RETURN
    END IF

    ! check for empty table
    IF (Table%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    ELSEIF (Table%KeyLeft == 0_kIndex) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    ! update Indx and set flag
    Table%Indx = Table%Indx + 1
    IsTheEnd = FalseVal

    ! start iteration by looking for the next non-empty slot
    DO WHILE ((Table%Items(Table%Indx)%Key == NULKEY).OR.(Table%Items(Table%Indx)%Key == DELKEY))
        Table%Indx = Table%Indx + 1_kIndex
    END DO

    ! update KeyLelf
    Table%KeyLeft = Table%KeyLeft - 1_kIndex

    ! get key if requested
    IF (PRESENT(Key)) Key = Table%Items(Table%Indx)%Key

    ! get value if requested
    IF (PRESENT(Value)) CALL TabItem_GetValue(Table%Items(Table%Indx), Value)

    RETURN

END FUNCTION HashTable_Move2NextPair

!******************************************************************************

SUBROUTINE HashTable_GetAllKeys(Table, KeyQ, ValueQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return all keys in the table and optionally all associated values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable),          INTENT(INOUT)    :: Table    !! HashTable object
    TYPE(QueueKey),            INTENT(OUT)      :: KeyQ     !! key queue
    TYPE(QueueVal),  OPTIONAL, INTENT(OUT)      :: ValueQ   !! value queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    DO I = 0_kIndex, Table%Capacity-1_kIndex
        IF ((Table%Items(I)%Key /= DELKEY).AND.(Table%Items(I)%Key /= NULKEY)) THEN
            CALL KeyQ%EnQueue(Table%Items(I)%Key)
            IF (PRESENT(ValueQ)) CALL ValueQ%EnQueue(Table%Items(I)%Value)
        END IF
    END DO

    RETURN

END SUBROUTINE HashTable_GetAllKeys

! -----------------------------------------------------------------------------
! -----             Procedures Dependent On Probing Algorithm             -----
! -----------------------------------------------------------------------------

SUBROUTINE SetupProbing(Table, Key)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set up the probing according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    KeyTypeB,         INTENT(IN)    :: Key      !! key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Table%ProbAlgo)
    CASE (LinearProbing)
        ! no setup required
    CASE (QuadraticProbing)
        ! no setup required
    CASE (DoubleHashing)
        BLOCK
            tIndex  :: HashCode
            tIndex  :: KeySize
            ! Cache second hash value.
            KeySize  = Bytes_Char*LEN(Key)
            HashCode = Table%HashCalc(Key, KeySize, HashSeed, RemoveSign=TrueVal)
            Table%HashIndx = ComputeIndex(HashCode, Table%Capacity)
            ! Fail safe to avoid infinite loop
            IF (Table%HashIndx == 0_kIndex) Table%HashIndx = 1_kIndex
        END BLOCK
    END SELECT

    RETURN

END SUBROUTINE SetupProbing

!******************************************************************************

FUNCTION Probe(Table, IdIn) RESULT(IdOut)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To look for the next available bucket according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    tIndex,           INTENT(IN)    :: IdIn     !! starting index for the probing
    tIndex                          :: IdOut    !! index of available bucket

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Table%ProbAlgo)
    CASE (LinearProbing)
        IdOut = LinearConstant*IdIn
    CASE (QuadraticProbing)
        ! Quadratic probing function (x**2 + x) / 2
        IdOut = SHIFTR(IdIn*IdIn + IdIn, 1)
    CASE (DoubleHashing)
        IdOut = Table%HashIndx*IdIn
    END SELECT

    RETURN

END FUNCTION Probe

!******************************************************************************

SUBROUTINE AdjustCapacity(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To adjust capacity according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Table%ProbAlgo)
    CASE (LinearProbing)
        ! Adjust the capacity so that the linear constant and
        ! the table capacity are relatively prime.
        DO WHILE (ComputeGCD(LinearConstant, Table%Capacity) /= 1_kIndex)
            Table%Capacity = Table%Capacity + 1_kIndex
        END DO
    CASE (QuadraticProbing)
        ! Adjust the capacity of the hash table to be a power of two.
        BLOCK
            tIndex      :: Pow2
            Pow2 = HighestOneBit(Table%Capacity)
            IF (Table%Capacity /= Pow2 ) THEN
                CALL IncreaseCapacity(Table)
            END IF
        END BLOCK
    CASE (DoubleHashing)
        ! Adjust the capacity until it is a prime number. The reason for
        ! doing this is to help ensure that the GCD(hash, capacity) = 1 when
        ! probing so that all the cells can be reached.
        IF (.NOT.Is_Prime(Table%Capacity)) THEN
            Table%Capacity = Next_Prime(Table%Capacity)
        END IF
    END SELECT

    RETURN

END SUBROUTINE AdjustCapacity

!******************************************************************************

SUBROUTINE IncreaseCapacity(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To increase capacity according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Table%ProbAlgo)
    CASE (LinearProbing)
        ! doubling the capacity
        Table%Capacity = Table%Capacity*2_kIndex + 1_kIndex
    CASE (QuadraticProbing)
        ! increase the capacity of the hash table to the next power of two.
        Table%Capacity = SHIFTL(HighestOneBit(Table%Capacity), 1)
    CASE (DoubleHashing)
        ! doubling the capacity
        Table%Capacity = Table%Capacity*2_kIndex + 1_kIndex
    END SELECT

    RETURN

END SUBROUTINE IncreaseCapacity

! -----------------------------------------------------------------------------
! -----                     Auxiliary Procedures                          -----
! -----------------------------------------------------------------------------

FUNCTION ComputeIndex(HashCode, Capacity) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the index of working items of the hash table for
    !  the specified hash code.  Returns value between 0 and
    !  Capacity-1 (assumes Capacity is a power of 2).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tHash,  INTENT(IN)  :: HashCode
    tIndex, INTENT(IN)  :: Capacity
    tIndex              :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! Note: the sign of the hash code must have already been removed.
    Indx = IAND(HashCode, Capacity-1_kIndex)

    RETURN

END FUNCTION ComputeIndex

!******************************************************************************

RECURSIVE FUNCTION ComputeGCD(A, B) RESULT(C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the greatest common denominator of A and B.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: A, B
    tIndex              :: C

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (B == 0_kIndex) THEN
        C = A
    ELSE
        C = ComputeGCD(B, MOD(A, B))
    END IF

    RETURN

END FUNCTION ComputeGCD

!******************************************************************************
