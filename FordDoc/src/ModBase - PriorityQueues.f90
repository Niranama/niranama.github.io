
MODULE ModBase_PriorityQueues

!^  **PURPOSE OF THIS MODULE**: <br>
!   This module collectively contains *priority-queue-based* types from other modules.
!   It is provided so that a user can refer to this module instead of referring to
!   several individual modules when various types are needed concurrently. <br>
!   <br>
!   **Overview**: <br>
!   These priority-queue-based types are container types that employ a binary heap
!   implementation (except the *PQBasic* type, which employs an elementary implementation)
!   to provide common operations for a priority queue.  They can be used as a max-priority
!   queue or a min-priority queue.  By default, each type represents the max-priority
!   queue but a user can specify the *MinPQ* argument to true so that it can be employed
!   as the min-priority queue instead. <br>
!   Each individual type can be used to store keys for a specific type of keys.
!   The available type of stored keys is one of Fortran intrinsic *comparable*
!   types (i.e. CHARACTER, INTEGER and REAL) or a derived type in the *Comparable*
!   class (i.e. the *Comparable* type or its subtypes). <br>
!   Specifically, available priority-queue-based container types include: <br>
!   - the <a href="../module/class_pqcharacter.html#type-pqcharacter">PQCharacter</a> type
!     for character string key type, <br>
!   - the <a href="../module/class_pqinteger1b.html#type-pqinteger1b">PQInteger1B</a> type
!     for 1-byte (or 8-bit) integer key type, <br>
!   - the <a href="../module/class_pqinteger2b.html#type-pqinteger2b">PQInteger2B</a> type
!     for 2-byte (or 16-bit) integer key type, <br>
!   - the <a href="../module/class_pqinteger4b.html#type-pqinteger4b">PQInteger4B</a> type
!     for 4-byte (or 32-bit) integer key type, <br>
!   - the <a href="../module/class_pqinteger8b.html#type-pqinteger8b">PQInteger8B</a> type
!     for 8-byte (or 64-bit) integer key type, <br>
!   - the <a href="../module/class_pqrealsp.html#type-pqrealsp">PQRealSP</a> type
!     for single-precision real key type, <br>
!   - the <a href="../module/class_pqrealdp.html#type-pqrealdp">PQRealDP</a> type
!     for double-precision real key type, <br>
!   - the <a href="../module/class_pqrealqp.html#type-pqrealqp">PQRealQP</a> type
!     for quadruple-precision real key type, <br>
!   - the <a href="../module/class_pqheap.html#type-pqheap">PQHeap</a> type for type
!     of key in *Comparable* class, and <br>
!   - the <a href="../module/class_pqbasic.html#type-pqbasic">PQBasic</a> type for type
!     of key in *Comparable* class. <br>
!   Each priority-queue-based container type represents a priority queue where various common
!   operations are provided and can be categorized as follows. <br>
!   (1) Construction and Destruction.  Methods for these operations include: <br>
!   - *CreateEmpty* method - method to construct an empty container, <br>
!   - *Construct* method - method to construct a container from an array of keys, and <br>
!   - *Destruct* method - method to destruct a container by removing all keys from
!       the container as well as free memory storage occupied by the container. <br>
!   (2) Insertion and Removal.  Methods for these operations include: <br>
!   - *Insert* method - method to insert a new key into a container, and <br>
!   - *Remove* method - method to remove a top-priority key from a container. <br>
!   (3) Retrieval.  A method for this operation is: <br>
!   - *Peek* method - method to retrieve a top-priority key from a container. <br>
!   (4) Inquiry.  Methods for this operation include: <br>
!   - *IsEmpty* method - method to check whether the container is empty or not, and <br>
!   - *GetSize* method - method to get the container size (number of key-value pairs stored). <br>

!** USE STATEMENTS:
    USE Class_PQCharacter
    USE Class_PQInteger1B
    USE Class_PQInteger2B
    USE Class_PQInteger4B
    USE Class_PQInteger8B
    USE Class_PQRealSP
    USE Class_PQRealDP
    USE Class_PQRealQP
!    USE Class_PQComparable

END MODULE ModBase_PriorityQueues