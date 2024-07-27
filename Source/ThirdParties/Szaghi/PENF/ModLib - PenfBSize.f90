!< PENF bit/byte size functions.

MODULE ModLib_PenfBSize
!< PENF bit/byte size functions.
USE ModLib_PenfGlobal

IMPLICIT NONE
PRIVATE
SAVE
PUBLIC :: BIT_SIZE, BYTE_SIZE

INTERFACE BIT_SIZE
  !< Overloading of the intrinsic *bit_size* function for computing the number of bits of (also) real and charACTER VARIABLES.
  MODULE PROCEDURE                &
                   BIT_SIZE_R16P, &
                   BIT_SIZE_R8P,  &
                   BIT_SIZE_R4P,  &
                   BIT_SIZE_CHR
ENDINTERFACE

INTERFACE BYTE_SIZE
  !< Compute the number of bytes of a variable.
  MODULE PROCEDURE                 &
                   BYTE_SIZE_I8P,  &
                   BYTE_SIZE_I4P,  &
                   BYTE_SIZE_I2P,  &
                   BYTE_SIZE_I1P,  &
                   BYTE_SIZE_R16P, &
                   BYTE_SIZE_R8P,  &
                   BYTE_SIZE_R4P,  &
                   BYTE_SIZE_CHR
ENDINTERFACE

CONTAINS
   ELEMENTAL FUNCTION BIT_SIZE_R16P(I) RESULT(BITS)
   !< Compute the number of bits of a real variable.
   !<
   !<```fortran
   !< use penf
   !< print FI2P, bit_size(1._R16P)
   !<```
   !=> 128 <<<
   REAL(R16P), INTENT(IN) :: I       !< Real variable whose number of bits must be computed.
   INTEGER(I2P)           :: BITS    !< Number of bits of r.
   INTEGER(I1P)           :: MOLD(1) !< "Molding" dummy variable for bits counting.

   BITS = SIZE(TRANSFER(I, MOLD), DIM=1, KIND=I2P) * 8_I2P
   ENDFUNCTION BIT_SIZE_R16P

   ELEMENTAL FUNCTION BIT_SIZE_R8P(I) RESULT(BITS)
   !< Compute the number of bits of a real variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, bit_size(1._R8P)
   !<```
   !=> 64 <<<
   REAL(R8P), INTENT(IN) :: I       !< Real variable whose number of bits must be computed.
   INTEGER(I1P)          :: BITS    !< Number of bits of r.
   INTEGER(I1P)          :: MOLD(1) !< "Molding" dummy variable for bits counting.

   BITS = SIZE(TRANSFER(I, MOLD), DIM=1, KIND=I1P) * 8_I1P
   ENDFUNCTION BIT_SIZE_R8P

   ELEMENTAL FUNCTION BIT_SIZE_R4P(I) RESULT(BITS)
   !< Compute the number of bits of a real variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, bit_size(1._R4P)
   !<```
   !=> 32 <<<
   REAL(R4P), INTENT(IN) :: I       !< Real variable whose number of bits must be computed.
   INTEGER(I1P)          :: BITS    !< Number of bits of r.
   INTEGER(I1P)          :: MOLD(1) !< "Molding" dummy variable for bits counting.

   BITS = SIZE(TRANSFER(I, MOLD), DIM=1, KIND=I1P) * 8_I1P
   ENDFUNCTION BIT_SIZE_R4P

   ELEMENTAL FUNCTION BIT_SIZE_CHR(I) RESULT(BITS)
   !< Compute the number of bits of a character variable.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, bit_size('ab')
   !<```
   !=> 16 <<<
   CHARACTER(*), INTENT(IN) :: I       !< Character variable whose number of bits must be computed.
   INTEGER(I4P)             :: BITS    !< Number of bits of c.
   INTEGER(I1P)             :: MOLD(1) !< "Molding" dummy variable for bits counting.

   BITS = SIZE(TRANSFER(I, MOLD), DIM=1, KIND=I4P) * 8_I4P
   ENDFUNCTION BIT_SIZE_CHR

   ELEMENTAL FUNCTION BYTE_SIZE_R16P(I) RESULT(BYTES)
   !< Compute the number of bytes of a real variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, byte_size(1._R16P)
   !<```
   !=> 16 <<<
   REAL(R16P), INTENT(IN) :: I     !< Real variable whose number of bytes must be computed.
   INTEGER(I1P)           :: BYTES !< Number of bytes of r.

   BYTES = BIT_SIZE(I) / 8_I1P
   ENDFUNCTION BYTE_SIZE_R16P

   ELEMENTAL FUNCTION BYTE_SIZE_R8P(I) RESULT(BYTES)
   !< Compute the number of bytes of a real variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, byte_size(1._R8P)
   !<```
   !=> 8 <<<
   REAL(R8P), INTENT(IN) :: I     !< Real variable whose number of bytes must be computed.
   INTEGER(I1P)          :: BYTES !< Number of bytes of r.

   BYTES = BIT_SIZE(I) / 8_I1P
   ENDFUNCTION BYTE_SIZE_R8P

   ELEMENTAL FUNCTION BYTE_SIZE_R4P(I) RESULT(BYTES)
   !< Compute the number of bytes of a real variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, byte_size(1._R4P)
   !<```
   !=> 4 <<<
   REAL(R4P), INTENT(IN) :: I     !< Real variable whose number of bytes must be computed.
   INTEGER(I1P)          :: BYTES !< Number of bytes of r.

   BYTES = BIT_SIZE(I) / 8_I1P
   ENDFUNCTION BYTE_SIZE_R4P

   ELEMENTAL FUNCTION BYTE_SIZE_CHR(I) RESULT(BYTES)
   !< Compute the number of bytes of a character variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, byte_size('ab')
   !<```
   !=> 2 <<<
   CHARACTER(*), INTENT(IN) :: I     !< Character variable whose number of bytes must be computed.
   INTEGER(I4P)             :: BYTES !< Number of bytes of c.

   BYTES = BIT_SIZE(I) / 8_I4P
   ENDFUNCTION BYTE_SIZE_CHR

   ELEMENTAL FUNCTION BYTE_SIZE_I8P(I) RESULT(BYTES)
   !< Compute the number of bytes of an integer variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, byte_size(1_I8P)
   !<```
   !=> 8 <<<
   INTEGER(I8P), INTENT(IN) :: I     !< Integer variable whose number of bytes must be computed.
   INTEGER(I1P)             :: BYTES !< Number of bytes of i.

   BYTES = BIT_SIZE(I) / 8_I1P
   ENDFUNCTION BYTE_SIZE_I8P

   ELEMENTAL FUNCTION BYTE_SIZE_I4P(I) RESULT(BYTES)
   !< Compute the number of bytes of an integer variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, byte_size(1_I4P)
   !<```
   !=> 4 <<<
   INTEGER(I4P), INTENT(IN) :: I     !< Integer variable whose number of bytes must be computed.
   INTEGER(I1P)             :: BYTES !< Number of bytes of i.

   BYTES = BIT_SIZE(I) / 8_I1P
   ENDFUNCTION BYTE_SIZE_I4P

   ELEMENTAL FUNCTION BYTE_SIZE_I2P(I) RESULT(BYTES)
   !< Compute the number of bytes of an integer variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, byte_size(1_I2P)
   !<```
   !=> 2 <<<
   INTEGER(I2P), INTENT(IN) :: I     !< Integer variable whose number of bytes must be computed.
   INTEGER(I1P)             :: BYTES !< Number of bytes of i.

   BYTES = BIT_SIZE(I) / 8_I1P
   ENDFUNCTION BYTE_SIZE_I2P

   ELEMENTAL FUNCTION BYTE_SIZE_I1P(I) RESULT(BYTES)
   !< Compute the number of bytes of an integer variable.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, byte_size(1_I1P)
   !<```
   !=> 1 <<<
   INTEGER(I1P), INTENT(IN) :: I     !< Integer variable whose number of bytes must be computed.
   INTEGER(I1P)             :: BYTES !< Number of bytes of i.

   BYTES = BIT_SIZE(I) / 8_I1P
   ENDFUNCTION BYTE_SIZE_I1P
ENDMODULE ModLib_PenfBSize
