!< Portability Environment for Fortran poor people.

MODULE ModLib_Penf
!< Portability Environment for Fortran poor people.
USE ModLib_PenfGlobal
#ifdef __INTEL_COMPILER
USE ModLib_PenfBSize
#else
USE ModLib_PenfBSize, ONLY : BIT_SIZE, BYTE_SIZE
#endif
USE ModLib_PenfStringify, ONLY : STR_ASCII, STR_UCS4, STR, STRZ, CTON, BSTR, BCTON

IMPLICIT NONE
PRIVATE
SAVE
! global parameters and variables
PUBLIC :: ENDIANL, ENDIANB, ENDIAN, IS_INITIALIZED
PUBLIC :: ASCII, UCS4, CK
PUBLIC :: R16P, FR16P, DR16P, MINR16P, MAXR16P, BIR16P, BYR16P, SMALLR16P, ZEROR16P
PUBLIC :: R8P,  FR8P,  DR8P,  MINR8P,  MAXR8P,  BIR8P,  BYR8P,  SMALLR8P,  ZEROR8P
PUBLIC :: R4P,  FR4P,  DR4P,  MINR4P,  MAXR4P,  BIR4P,  BYR4P,  SMALLR4P,  ZEROR4P
PUBLIC :: R_P,  FR_P,  DR_P,  MINR_P,  MAXR_P,  BIR_P,  BYR_P,  SMALLR_P,  ZEROR_P
PUBLIC :: I8P,  FI8P,  DI8P,  MINI8P,  MAXI8P,  BII8P,  BYI8P
PUBLIC :: I4P,  FI4P,  DI4P,  MINI4P,  MAXI4P,  BII4P,  BYI4P
PUBLIC :: I2P,  FI2P,  DI2P,  MINI2P,  MAXI2P,  BII2P,  BYI2P
PUBLIC :: I1P,  FI1P,  DI1P,  MINI1P,  MAXI1P,  BII1P,  BYI1P
PUBLIC :: I_P,  FI_P,  DI_P,  MINI_P,  MAXI_P,  BII_P,  BYI_P
PUBLIC :: CHARACTER_KINDS_LIST, REAL_KINDS_LIST, REAL_FORMATS_LIST
PUBLIC :: INTEGER_KINDS_LIST, INTEGER_FORMATS_LIST
! bit/byte size functions
PUBLIC :: BIT_SIZE, BYTE_SIZE
! stringify facility
PUBLIC :: STR_ASCII, STR_UCS4
PUBLIC :: STR, STRZ, CTON
PUBLIC :: BSTR, BCTON
! miscellanea facility
PUBLIC :: CHECK_ENDIAN
PUBLIC :: DIGIT
PUBLIC :: PENF_INIT
PUBLIC :: PENF_PRINT

INTEGER, PROTECTED :: ENDIAN         = ENDIANL !< Bit ordering: Little endian (endianL), or Big endian (endianB).
LOGICAL, PROTECTED :: IS_INITIALIZED = .FALSE. !< Check the initialization of some variables that must be initIALIZED.

#ifdef __GFORTRAN__
! work-around for strange gfortran bug...
INTERFACE BIT_SIZE
  !< Overloading of the intrinsic *bit_size* function for computing the number of bits of (also) real and charACTER VARIABLES.
ENDINTERFACE
#endif

INTERFACE DIGIT
  !< Compute the number of digits in decimal base of the input integer.
  MODULE PROCEDURE DIGIT_I8, DIGIT_I4, DIGIT_I2, DIGIT_I1
ENDINTERFACE

CONTAINS
   ! public procedures
   SUBROUTINE CHECK_ENDIAN()
   !< Check the type of bit ordering (big or little endian) of the running architecture.
   !<
   !> @note The result is stored into the *endian* global variable.
   !<
   !<```fortran
   !< use penf
   !< call check_endian
   !< print *, endian
   !<```
   !=> 1 <<<
   IF (IS_LITTLE_ENDIAN()) THEN
      ENDIAN = ENDIANL
   ELSE
      ENDIAN = ENDIANB
   ENDIF
   CONTAINS
      PURE FUNCTION IS_LITTLE_ENDIAN() RESULT(IS_LITTLE)
      !< Check if the type of the bit ordering of the running architecture is little endian.
      LOGICAL      :: IS_LITTLE !< Logical output: true is the running architecture uses little endian orderinG, FALSE OTHERWISE.
      INTEGER(I1P) :: INT1(1:4) !< One byte integer array for casting 4 bytes integer.

      INT1 = TRANSFER(1_I4P, INT1)
      IS_LITTLE = (INT1(1)==1_I1P)
      ENDFUNCTION IS_LITTLE_ENDIAN
   ENDSUBROUTINE CHECK_ENDIAN

   SUBROUTINE PENF_INIT()
   !< Initialize PENF's variables that are not initialized into the definition specification.
   !<
   !<```fortran
   !< use penf
   !< call penf_init
   !< print FI1P, BYR4P
   !<```
   !=> 4 <<<

   CALL CHECK_ENDIAN
   BIR16P = BIT_SIZE(MAXR16P) ; BYR16P = BIR16P / 8_I2P
   BIR8P  = BIT_SIZE(MAXR8P)  ; BYR8P  = BIR8P  / 8_I1P
   BIR4P  = BIT_SIZE(MAXR4P)  ; BYR4P  = BIR4P  / 8_I1P
   BIR_P  = BIT_SIZE(MAXR_P)  ; BYR_P  = BIR_P  / 8_I1P
   IS_INITIALIZED = .TRUE.
   ENDSUBROUTINE PENF_INIT

   SUBROUTINE PENF_PRINT(UNIT, PREF, IOSTAT, IOMSG)
   !< Print to the specified unit the PENF's environment data.
   !<
   !<```fortran
   !< use penf
   !< integer :: u
   !< open(newunit=u, status='scratch')
   !< call penf_print(u)
   !< close(u)
   !< print "(A)", 'done'
   !<```
   !=> done <<<
   INTEGER(I4P), INTENT(IN)            :: UNIT    !< Logic unit.
   CHARACTER(*), INTENT(IN),  OPTIONAL :: PREF    !< Prefixing string.
   INTEGER(I4P), INTENT(OUT), OPTIONAL :: IOSTAT  !< IO error.
   CHARACTER(*), INTENT(OUT), OPTIONAL :: IOMSG   !< IO error message.
   CHARACTER(LEN=:), ALLOCATABLE       :: PREFD   !< Prefixing string.
   INTEGER(I4P)                        :: IOSTATD !< IO error.
   CHARACTER(500)                      :: IOMSGD  !< Temporary variable for IO error message.

   IF (.NOT.IS_INITIALIZED) CALL PENF_INIT
   PREFD = '' ; IF (PRESENT(PREF)) PREFD = PREF
   IF (ENDIAN==ENDIANL) THEN
     WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)PREFD//'This architecture has LITTLE Endian bit ordering'
   ELSE
     WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)PREFD//'This architecture has BIG Endian bit ordering'
   ENDIF
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'Character kind:'
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  ASCII: '//STR(N=ASCII)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  UCS4:  '//STR(N=UCS4)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  CK:    '//STR(N=CK)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'Reals kind, format and characters number:'
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R16P: '//STR(N=R16P)//','//FR16P//','//STR(N=DR16P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R8P:  '//STR(N=R8P )//','//FR8P //','//STR(N=DR8P )
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R4P:  '//STR(N=R4P )//','//FR4P //','//STR(N=DR4P )
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R_P:  '//STR(N=R_P )//','//FR_P //','//STR(N=DR_P )
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'Integers kind, format and characters number:'
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I8P:  '//STR(N=I8P)//','//FI8P //','//STR(N=DI8P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I4P:  '//STR(N=I4P)//','//FI4P //','//STR(N=DI4P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I2P:  '//STR(N=I2P)//','//FI2P //','//STR(N=DI2P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I1P:  '//STR(N=I1P)//','//FI1P //','//STR(N=DI1P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'Reals minimum and maximum values:'
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R16P: '//STR(N=MINR16P)//','//STR(N=MAXR16P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R8P:  '//STR(N=MINR8P )//','//STR(N=MAXR8P )
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R4P:  '//STR(N=MINR4P )//','//STR(N=MAXR4P )
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R_P:  '//STR(N=MINR_P )//','//STR(N=MAXR_P )
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'Integergs minimum and maximum values:'
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I8P:  '//STR(N=MINI8P )//','//STR(N=MAXI8P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I4P:  '//STR(N=MINI4P )//','//STR(N=MAXI4P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I2P:  '//STR(N=MINI2P )//','//STR(N=MAXI2P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I1P:  '//STR(N=MINI1P )//','//STR(N=MAXI1P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'Reals bits/bytes sizes:'
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R16P: '//STR(N=BIR16P)//'/'//STR(N=BYR16P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R8P:  '//STR(N=BIR8P )//'/'//STR(N=BYR8P )
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R4P:  '//STR(N=BIR4P )//'/'//STR(N=BYR4P )
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  R_P:  '//STR(N=BIR_P )//'/'//STR(N=BYR_P )
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'Integers bits/bytes sizes:'
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I8P:  '//STR(N=BII8P)//'/'//STR(N=BYI8P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I4P:  '//STR(N=BII4P)//'/'//STR(N=BYI4P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I2P:  '//STR(N=BII2P)//'/'//STR(N=BYI2P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  I1P:  '//STR(N=BII1P)//'/'//STR(N=BYI1P)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'Smallest reals'
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  smallR16P: '//STR(SMALLR16P, .TRUE.)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  smallR8P:  '//STR(SMALLR8P,  .TRUE.)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  smallR4P:  '//STR(SMALLR4P,  .TRUE.)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  smallR_P:  '//STR(SMALLR_P,  .TRUE.)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'Machine zero'
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  ZeroR16P: '//STR(ZEROR16P, .TRUE.)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  ZeroR8P:  '//STR(ZEROR8P,  .TRUE.)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  ZeroR4P:  '//STR(ZEROR4P,  .TRUE.)
   WRITE(UNIT=UNIT,FMT='(A)',IOSTAT=IOSTATD,IOMSG=IOMSGD)  PREFD//'  ZeroR_P:  '//STR(ZEROR_P,  .TRUE.)
   IF (PRESENT(IOSTAT)) IOSTAT = IOSTATD
   IF (PRESENT(IOMSG))  IOMSG  = IOMSGD
   ENDSUBROUTINE PENF_PRINT

   ! private procedures
   ELEMENTAL FUNCTION DIGIT_I8(N) RESULT(DIGIT)
   !< Compute the number of digits in decimal base of the input integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, digit(100_I8P)
   !<```
   !=> 3 <<<
   INTEGER(I8P), INTENT(IN) :: N     !< Input integer.
   CHARACTER(DI8P)          :: STR   !< Returned string containing input number plus padding zeros.
   INTEGER(I4P)             :: DIGIT !< Number of digits.

   WRITE(STR, FI8P) ABS(N)        ! Casting of n to string.
   DIGIT = LEN_TRIM(ADJUSTL(STR)) ! Calculating the digits number of n.
   ENDFUNCTION DIGIT_I8

   ELEMENTAL FUNCTION DIGIT_I4(N) RESULT(DIGIT)
   !< Compute the number of digits in decimal base of the input integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, digit(100_I4P)
   !<```
   !=> 3 <<<
   INTEGER(I4P), INTENT(IN) :: N     !< Input integer.
   CHARACTER(DI4P)          :: STR   !< Returned string containing input number plus padding zeros.
   INTEGER(I4P)             :: DIGIT !< Number of digits.

   WRITE(STR, FI4P) ABS(N)        ! Casting of n to string.
   DIGIT = LEN_TRIM(ADJUSTL(STR)) ! Calculating the digits number of n.
   ENDFUNCTION DIGIT_I4

   ELEMENTAL FUNCTION DIGIT_I2(N) RESULT(DIGIT)
   !< Compute the number of digits in decimal base of the input integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, digit(100_I2P)
   !<```
   !=> 3 <<<
   INTEGER(I2P), INTENT(IN) :: N     !< Input integer.
   CHARACTER(DI2P)          :: STR   !< Returned string containing input number plus padding zeros.
   INTEGER(I4P)             :: DIGIT !< Number of digits.

   WRITE(STR, FI2P) ABS(N)        ! Casting of n to string.
   DIGIT = LEN_TRIM(ADJUSTL(STR)) ! Calculating the digits number of n.
   ENDFUNCTION DIGIT_I2

   ELEMENTAL FUNCTION DIGIT_I1(N) RESULT(DIGIT)
   !< Compute the number of digits in decimal base of the input integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, digit(100_I1P)
   !<```
   !=> 3 <<<
   INTEGER(I1P), INTENT(IN) :: N     !< Input integer.
   CHARACTER(DI1P)          :: STR   !< Returned string containing input number plus padding zeros.
   INTEGER(I4P)             :: DIGIT !< Number of digits.

   WRITE(STR, FI1P) ABS(N)        ! Casting of n to string.
   DIGIT = LEN_TRIM(ADJUSTL(STR)) ! Calculating the digits number of n.
   ENDFUNCTION DIGIT_I1
ENDMODULE ModLib_Penf
