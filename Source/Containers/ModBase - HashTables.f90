
MODULE ModBase_HashTables

!^  **PURPOSE OF THIS MODULE**: <br>
!   This module collectively contains *hash-table-based* types from other modules. It
!   is provided so that a user can refer to this module instead of referring to several
!   individual modules when various types are needed concurrently. <br>
!   <br>
!   **Overview**: <br>
!   These hash-table-based types are container types that employ a hash table implementation
!   to provide common operations for an unordered symbol table.  Technically, all of them
!   employ the open-addressing as a collision resolution technique where the hash resolution
!   is performed through probing.  They provide three probing algorithms: linear probing,
!   quadratic probing and double hashing.  By default, the linear probing algorithm is used.
!   However, a user can specify other probing algorithm during a construction of a table. <br>
!   Each individual type can be used to store key-value pairs for a specific type of keys where
!   the type of stored keys is one of Fortran intrinsic *hashable* types (i.e. CHARACTER, INTEGER
!   and REAL) or a derived type in the *Hashable* class (i.e. the *Hashable* type or its subtypes).
!   Most of the hash-table-based types (except the *HTabHashable* type) utilize an unlimited
!   polymorphic type to store values; therefore, inserted values can have any types. Unlike other
!   hash-table-based types, the *HTabHashable* type uses the *Hashable* type to represent both key
!   and value, and it requires only one argument (instead of two) when inserting or retrieving the
!   key and its associated value.  Therefore, its application programming interface (API) is slightly
!   different from the API of other hash-table-based types.  <br>
!   Available hash-table-based container types include: <br>
!   - the <a href="../module/class_htabcharacter.html#type-htabcharacter">HTabCharacter</a> type
!     for character string key type, <br>
!   - the <a href="../module/class_htabinteger1b.html#type-htabinteger1b">HTabInteger1B</a> type
!     for 1-byte (or 8-bit) integer key type, <br>
!   - the <a href="../module/class_htabinteger2b.html#type-htabinteger2b">HTabInteger2B</a> type
!     for 2-byte (or 16-bit) integer key type, <br>
!   - the <a href="../module/class_htabinteger4b.html#type-htabinteger4b">HTabInteger4B</a> type
!     for 4-byte (or 32-bit) integer key type, <br>
!   - the <a href="../module/class_htabinteger8b.html#type-htabinteger8b">HTabInteger8B</a> type
!     for 8-byte (or 64-bit) integer key type, <br>
!   - the <a href="../module/class_htabrealsp.html#type-htabrealsp">HTabRealSP</a> type
!     for single-precision real key type, <br>
!   - the <a href="../module/class_htabrealdp.html#type-htabrealdp">HTabRealDP</a> type
!     for double-precision real key type, <br>
!   - the <a href="../module/class_htabrealqp.html#type-htabrealqp">HTabRealQP</a> type
!     for quadruple-precision real key type, <br>
!   - the <a href="../module/class_htabhashable.html#type-htabhashable">HTabHashable</a>
!     type for type of key-value pair in *Hashable* class. <br>
!   Each hash-table-based container type represents an unordered symbol table where various common
!   operations are provided and can be categorized as follows. <br>
!   (1) Construction and Destruction.  Methods for these operations include: <br>
!   - *CreateEmpty* method - method to construct an empty container, <br>
!   - *Construct* method - method to construct a container from arrays of keys and values, and <br>
!   - *Destruct* method - method to destruct a container by removing all key-value pairs from
!       the container as well as free memory storage occupied by the container. <br>
!   (2) Insertion and Removal.  Methods for these operations include: <br>
!   - *Insert* method - method to insert a key and its associated value into a container, <br>
!   - *Remove* method - method to remove a key (and its associated value) from a container, and <br>
!   - *Clear* method - method to remove all key-value pairs from a container. <br>
!   (3) Retrieval.  A method for this operation is: <br>
!   - *GetKeys* method - method to retrieve all keys and optionally all their associated values. <br>
!   (4) Inquiry.  Methods for this operation include: <br>
!   - *IsEmpty* method - method to check whether the container is empty or not, <br>
!   - *Contain* method - method to check whether the specified key is in the container or not, and <br>
!   - *GetSize* method - method to get the container size (number of key-value pairs stored). <br>
!   (5) Iteration.  Methods for this operation include: <br>
!   - *StartFirst* method - method to start a forward iteration over key-value pairs, and <br>
!   - *MoveForward* method - method to move forward to the next key-value pair. <br>

!** USE STATEMENTS:
    USE Class_HTabCharacter
    USE Class_HTabInteger1B
    USE Class_HTabInteger2B
    USE Class_HTabInteger4B
    USE Class_HTabInteger8B
    USE Class_HTabRealSP
    USE Class_HTabRealDP
    USE Class_HTabRealQP
    USE Class_HTabHashable

END MODULE ModBase_HashTables
