# XpfGeneral

***XpfGeneral*** is an experimenal general-purpose package written in Fortran.   It is intended to be a part of the so-call eXPerimental Fortran Library (XPFLib), which will also contain mathemetics  and engineering packages.  The XpfGeneral package contains a large collection of general-purpose modules that can be grouped into smaller packages for specific functionings.  These smaller packages are listed as follows.

- *Base* package consists of various basic and utility modules  that are used in other packages.

- *CharConv* package consists of a module (and its supporting submodules) that provides various routines to perform a conversion between a (decimal) string and a number.

- *Collection* package consist of a number of derived-types (i.e. classes) that represent ordered and unordered collections including stack, queue, deque, list, ordered and unordered symbol tables.  The collections in this package are generic containers that can be used with various data types.  The package also contains several intrusive container classes that can be employed with any data types.

- *Container* package consist of a number of derived-types (i.e. classes) that represent various containers including dynamic arrays, linked lists, trees, hash tables and priority queues.  Unlike those classes in the *Collection* package, most of the classes in this package represent a container that can be used for a specific *intrinsic* data type.  The dynamic-array and linked-list classes support all intrinsic data types whereas the tree, hash-table and priority-queue classes support all intrinsic *comparable* data types.

- *HashGeneral* package consists of a large number of modules that implement many non-secure hash function algorithms.

- *HashSecure* package consists of a large number of modules that implement many cryptographic hash function algorithms.

- *HighPrecision* package consists of four derived data types that represent integers with precisions that are higher than those of intrinsic integers.  The *ApInt32* and *ApInt32* types represent arbitrary-precision signed integers while the *SInt128* and *UInt128* types represent 128-bit *signed* and *unsigned* integers, respectively.

- *InputNOutput* package consists of various modules for input/output operations.

- *MemHandler* package consists of various modules for memory handling operations.

- *RandGen* package consists of a large number of modules that implement many pseudo-random number generation algorithms.

- *SortNRank* package consists of a number of modules/submodules that implement many sorting and ranking algorithms.

- *String* package consists of various modules that provide a number of operations/functionings that are specific to *character* and *string* data types.

- *ThirdParty* package consists of a large number of modules compiled from useful third-party packages.  This package provides additional/complementary (sometimes similar/redundant) features to those in-house packages.

## Building XpfGeneral

*Visual Studio* project solutions are provided in the *VSIFort* and *VSIfx* folders.   A project solution in one of those folders can be used to build the *XpfGeneral* package using either the Intel IFORT compiler or the Intel IFX compiler.   To build the *XpfGeneral* package using a GFortran compiler, a *CodeBlock* workspace is provided in the *CBFort* folder.  It should be noted that those *Visual Studio* project solutions can only be used in the Windows platforms whereas the *CodeBlock* workspace can be used in both Windows and Linux platforms.

## Documentation

The [FORD](https://github.com/Fortran-FOSS-Programmers/ford) program can be used to automatically generate the documentation of the *XpfGeneral* package via `ford FordGen.md`.


