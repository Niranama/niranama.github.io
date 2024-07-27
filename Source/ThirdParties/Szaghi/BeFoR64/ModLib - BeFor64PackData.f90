!< KISS library for packing heterogeneous data into single (homogeneous) packed one.
!
MODULE ModLib_BeFor64PackData
!< KISS library for packing heterogeneous data into single (homogeneous) packed one.
USE ModLib_Penf

IMPLICIT NONE
PRIVATE
PUBLIC :: PACK_DATA

INTERFACE PACK_DATA
  !< Pack different kinds of data into single I1P array.
  !<
  !< This is useful for encoding different (heterogeneous) kinds variables into a single (homogeneous) stream OF BITS.
  !< @note This procedure exploits the `transfer` builtin function, that from the standard (2003+) is defined AS
  !< `TRANSFER(SOURCE, MOLD [, SIZE])`. Data object having a physical representation identical to that of `SOURCE` BUT WITH THE TYPE
  !< and type parameters of `MOLD`. The result is of the same type and type parameters as `MOLD`.
  !< If `MOLD` is an array and `SIZE` is absent, the result is an array and of rank one. Its size is as small AS POSSIBLE SUCH
  !< that its physical representation is not shorter than that of `SOURCE`.
  !<
  !< Presently, the following combinations are available:
  !<
  !<* [ ] Arrays-Arrays:
  !<    * [X] real(any)-real(any);
  !<    * [X] real(any)-integer(any);
  !<    * [X] integer(any)-integer(any);
  !<    * [X] integer(any)-real(any);
  !<    * [ ] real(any)-character;
  !<    * [ ] character-real(any);
  !<    * [ ] integer(any)-character;
  !<    * [ ] character-integer(any);
  !<* [ ] Scalars-Scalars:
  !<    * [ ] real(any)-real(any);
  !<    * [ ] real(any)-integer(any);
  !<    * [ ] integer(any)-integer(any);
  !<    * [ ] integer(any)-real(any);
  !<    * [ ] real(any)-character;
  !<    * [ ] character-real(any);
  !<    * [ ] integer(any)-character;
  !<    * [ ] character-integer(any);
  !<
  !<### Examples of usage
  !<
  !<#### Packing two real arrays, one with kind R8P and one with R4P
  !<```ortran
  !<real(R8P)::                 array_r8(1:12)
  !<real(R4P)::                 array_r4(-1:5)
  !<integer(I1P), allocatable:: rpack
  !<...
  !<call pack_data(a1=array_r8,a2=array_r4,packed=rpack)
  !<```
  !<#### Packing two arrays, one real with kind R4P and one integer with I4P
  !<```ortran
  !<real(R4P)::                 array_r4(2)
  !<integer(I4P)::              array_i4(0:2)
  !<integer(I1P), allocatable:: rpack
  !<...
  !<call pack_data(a1=array_r4,a2=array_i4,packed=rpack)
  !<```
  MODULE PROCEDURE PACK_DATA_R8_R4, PACK_DATA_R8_I8, PACK_DATA_R8_I4, PACK_DATA_R8_I2, PACK_DATA_R8_I1, &
                   PACK_DATA_R4_R8, PACK_DATA_R4_I8, PACK_DATA_R4_I4, PACK_DATA_R4_I2, PACK_DATA_R4_I1, &
                   PACK_DATA_I8_R8, PACK_DATA_I8_R4, PACK_DATA_I8_I4, PACK_DATA_I8_I2, PACK_DATA_I8_I1, &
                   PACK_DATA_I4_R8, PACK_DATA_I4_R4, PACK_DATA_I4_I8, PACK_DATA_I4_I2, PACK_DATA_I4_I1, &
                   PACK_DATA_I2_R8, PACK_DATA_I2_R4, PACK_DATA_I2_I8, PACK_DATA_I2_I4, PACK_DATA_I2_I1, &
                   PACK_DATA_I1_R8, PACK_DATA_I1_R4, PACK_DATA_I1_I8, PACK_DATA_I1_I4, PACK_DATA_I1_I2
ENDINTERFACE

CONTAINS
   PURE SUBROUTINE PACK_DATA_R8_R4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   REAL(R8P),                 INTENT(IN)    :: A1(1:)    !< Firs data stream.
   REAL(R4P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R8_R4

   PURE SUBROUTINE PACK_DATA_R8_I8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   REAL(R8P),                 INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I8P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R8_I8

   PURE SUBROUTINE PACK_DATA_R8_I4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   REAL(R8P),                 INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I4P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R8_I4

   PURE SUBROUTINE PACK_DATA_R8_I2(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   REAL(R8P),                 INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I2P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R8_I2

   PURE SUBROUTINE PACK_DATA_R8_I1(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   REAL(R8P),                 INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I1P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R8_I1

   PURE SUBROUTINE PACK_DATA_R4_R8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   REAL(R4P),                 INTENT(IN)    :: A1(1:)    !< Firs data stream.
   REAL(R8P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R4_R8

   PURE SUBROUTINE PACK_DATA_R4_I8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   REAL(R4P),                 INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I8P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R4_I8

   PURE SUBROUTINE PACK_DATA_R4_I4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   REAL(R4P),                 INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I4P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R4_I4

   PURE SUBROUTINE PACK_DATA_R4_I2(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   REAL(R4P),                 INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I2P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R4_I2

   PURE SUBROUTINE PACK_DATA_R4_I1(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   REAL(R4P),                 INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I1P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_R4_I1

   PURE SUBROUTINE PACK_DATA_I8_R8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   INTEGER(I8P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   REAL(R8P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I8_R8

   PURE SUBROUTINE PACK_DATA_I8_R4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   INTEGER(I8P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   REAL(R4P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I8_R4

   PURE SUBROUTINE PACK_DATA_I8_I4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   INTEGER(I8P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I4P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I8_I4

   PURE SUBROUTINE PACK_DATA_I8_I2(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   INTEGER(I8P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I2P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I8_I2

   PURE SUBROUTINE PACK_DATA_I8_I1(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   INTEGER(I8P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I1P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I8_I1

   PURE SUBROUTINE PACK_DATA_I4_R8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   INTEGER(I4P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   REAL(R8P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I4_R8

   PURE SUBROUTINE PACK_DATA_I4_R4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   INTEGER(I4P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   REAL(R4P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I4_R4

   PURE SUBROUTINE PACK_DATA_I4_I8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   INTEGER(I4P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I8P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I4_I8

   PURE SUBROUTINE PACK_DATA_I4_I2(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   INTEGER(I4P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I2P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I4_I2

   PURE SUBROUTINE PACK_DATA_I4_I1(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   INTEGER(I4P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I1P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I4_I1

   PURE SUBROUTINE PACK_DATA_I2_R8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   INTEGER(I2P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   REAL(R8P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I2_R8

   PURE SUBROUTINE PACK_DATA_I2_R4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   INTEGER(I2P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   REAL(R4P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I2_R4

   PURE SUBROUTINE PACK_DATA_I2_I8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(3)
   !<```
   !=> 1 <<<
   INTEGER(I2P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I8P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I2_I8

   PURE SUBROUTINE PACK_DATA_I2_I4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(3)
   !<```
   !=> 1 <<<
   INTEGER(I2P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I4P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I2_I4

   PURE SUBROUTINE PACK_DATA_I2_I1(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(3)
   !<```
   !=> 1 <<<
   INTEGER(I2P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I1P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I2_I1

   PURE SUBROUTINE PACK_DATA_I1_R8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   INTEGER(I1P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   REAL(R8P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I1_R8

   PURE SUBROUTINE PACK_DATA_I1_R4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   INTEGER(I1P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   REAL(R4P),                 INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I1_R4

   PURE SUBROUTINE PACK_DATA_I1_I8(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(2)
   !<```
   !=> 1 <<<
   INTEGER(I1P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I8P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I1_I8

   PURE SUBROUTINE PACK_DATA_I1_I4(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(2)
   !<```
   !=> 1 <<<
   INTEGER(I1P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I4P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I1_I4

   PURE SUBROUTINE PACK_DATA_I1_I2(A1, A2, PACKED)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(2)
   !<```
   !=> 1 <<<
   INTEGER(I1P),              INTENT(IN)    :: A1(1:)    !< First data stream.
   INTEGER(I2P),              INTENT(IN)    :: A2(1:)    !< Second data stream.
   INTEGER(I1P), ALLOCATABLE, INTENT(INOUT) :: PACKED(:) !< Packed data into I1P array.
   INTEGER(I1P), ALLOCATABLE                :: P1(:)     !< Temporary packed data of first stream.
   INTEGER(I1P), ALLOCATABLE                :: P2(:)     !< Temporary packed data of second stream.

   P1 = TRANSFER(A1,P1)
   P2 = TRANSFER(A2,P2)
   PACKED = [P1,P2]
   ENDSUBROUTINE PACK_DATA_I1_I2
ENDMODULE ModLib_BeFor64PackData
