!< PENF global parameters and variables.

MODULE ModLib_PenfGlobal
!< PENF global parameters and variables.
!<
!< @note All module defined entities are public.

IMPLICIT NONE
PUBLIC
SAVE

INTEGER, PARAMETER :: ENDIANL = 1 !< Little endian parameter.
INTEGER, PARAMETER :: ENDIANB = 0 !< Big endian parameter.

! portable kind parameters
#ifdef _ASCII_SUPPORTED
INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('ascii')     !< ASCII character set kind.
#else
INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('default')   !< ASCII character set kind defined as default sET.
#endif
#ifdef _UCS4_SUPPORTED
INTEGER, PARAMETER :: UCS4  = SELECTED_CHAR_KIND('iso_10646') !< Unicode character set kind.
#else
INTEGER, PARAMETER :: UCS4  = SELECTED_CHAR_KIND('default')   !< Unicode character set kind defined as default SET.
#endif
#if defined _CK_IS_DEFAULT
INTEGER, PARAMETER :: CK  = SELECTED_CHAR_KIND('default')     !< Default kind character.
#elif defined _CK_IS_ASCII
INTEGER, PARAMETER :: CK  = ASCII                             !< Default kind character.
#elif defined _CK_IS_UCS4
INTEGER, PARAMETER :: CK  = UCS4                              !< Default kind character.
#else
INTEGER, PARAMETER :: CK  = SELECTED_CHAR_KIND('default')     !< Default kind character.
#endif

INTEGER, PARAMETER :: R16P = SELECTED_REAL_KIND(33,4931) !< 33 digits, range \([10^{-4931}, 10^{+4931} - 1]\); 128 BITS.
INTEGER, PARAMETER :: R8P  = SELECTED_REAL_KIND(15,307)  !< 15 digits, range \([10^{-307} , 10^{+307}  - 1]\); 64 BITS.
INTEGER, PARAMETER :: R4P  = SELECTED_REAL_KIND(6,37)    !< 6  digits, range \([10^{-37}  , 10^{+37}   - 1]\); 32 BITS.
#if defined _R_P_IS_R16P
INTEGER, PARAMETER :: R_P  = R16P                        !< Default real precision.
#elif defined _R_P_IS_R8P
INTEGER, PARAMETER :: R_P  = R8P                         !< Default real precision.
#elif defined _R_P_IS_R4P
INTEGER, PARAMETER :: R_P  = R4P                         !< Default real precision.
#else
INTEGER, PARAMETER :: R_P  = R8P                         !< Default real precision.
#endif

INTEGER, PARAMETER :: I8P = SELECTED_INT_KIND(18) !< Range \([-2^{63},+2^{63} - 1]\), 19 digits plus sign; 64 BITS.
INTEGER, PARAMETER :: I4P = SELECTED_INT_KIND(9)  !< Range \([-2^{31},+2^{31} - 1]\), 10 digits plus sign; 32 BITS.
INTEGER, PARAMETER :: I2P = SELECTED_INT_KIND(4)  !< Range \([-2^{15},+2^{15} - 1]\), 5  digits plus sign; 16 BITS.
INTEGER, PARAMETER :: I1P = SELECTED_INT_KIND(2)  !< Range \([-2^{7} ,+2^{7}  - 1]\), 3  digits plus sign; 8  BITS.
INTEGER, PARAMETER :: I_P = I4P                   !< Default integer precision.

! format parameters
CHARACTER(*), PARAMETER :: FR16P = '(E42.33E4)' !< Output format for kind=R16P real.
CHARACTER(*), PARAMETER :: FR8P  = '(E23.15E3)' !< Output format for kind=R8P real.
CHARACTER(*), PARAMETER :: FR4P  = '(E13.6E2)'  !< Output format for kind=R4P real.
#if defined _R_P_IS_R16P
CHARACTER(*), PARAMETER :: FR_P  = FR16P        !< Output format for kind=R_P real.
#elif defined _R_P_IS_R8P
CHARACTER(*), PARAMETER :: FR_P  = FR8P         !< Output format for kind=R_P real.
#elif defined _R_P_IS_R4P
CHARACTER(*), PARAMETER :: FR_P  = FR4P         !< Output format for kind=R_P real.
#else
CHARACTER(*), PARAMETER :: FR_P  = FR8P         !< Output format for kind=R_P real.
#endif

CHARACTER(*), PARAMETER :: FI8P   = '(I20)'    !< Output format for kind=I8P integer.
CHARACTER(*), PARAMETER :: FI8PZP = '(I20.19)' !< Output format for kind=I8P integer with zero prefixing.
CHARACTER(*), PARAMETER :: FI4P   = '(I11)'    !< Output format for kind=I4P integer.
CHARACTER(*), PARAMETER :: FI4PZP = '(I11.10)' !< Output format for kind=I4P integer with zero prefixing.
CHARACTER(*), PARAMETER :: FI2P   = '(I6)'     !< Output format for kind=I2P integer.
CHARACTER(*), PARAMETER :: FI2PZP = '(I6.5)'   !< Output format for kind=I2P integer with zero prefixing.
CHARACTER(*), PARAMETER :: FI1P   = '(I4)'     !< Output format for kind=I1P integer.
CHARACTER(*), PARAMETER :: FI1PZP = '(I4.3)'   !< Output format for kind=I1P integer with zero prefixing.
CHARACTER(*), PARAMETER :: FI_P   = FI4P       !< Output format for kind=I_P integer.
CHARACTER(*), PARAMETER :: FI_PZP = FI4PZP     !< Output format for kind=I_P integer with zero prefixing.

! length (number of digits) of formatted numbers
INTEGER, PARAMETER :: DR16P = 42    !< Number of digits of output format FR16P.
INTEGER, PARAMETER :: DR8P  = 23    !< Number of digits of output format FR8P.
INTEGER, PARAMETER :: DR4P  = 13    !< Number of digits of output format FR4P.
#if defined _R_P_IS_R16P
INTEGER, PARAMETER :: DR_P  = DR16P !< Number of digits of output format FR_P.
#elif defined _R_P_IS_R8P
INTEGER, PARAMETER :: DR_P  = DR8P  !< Number of digits of output format FR_P.
#elif defined _R_P_IS_R4P
INTEGER, PARAMETER :: DR_P  = DR4P  !< Number of digits of output format FR_P.
#else
INTEGER, PARAMETER :: DR_P  = DR8P  !< Number of digits of output format FR_P.
#endif

INTEGER, PARAMETER :: DI8P  = 20   !< Number of digits of output format I8P.
INTEGER, PARAMETER :: DI4P  = 11   !< Number of digits of output format I4P.
INTEGER, PARAMETER :: DI2P  = 6    !< Number of digits of output format I2P.
INTEGER, PARAMETER :: DI1P  = 4    !< Number of digits of output format I1P.
INTEGER, PARAMETER :: DI_P  = DI4P !< Number of digits of output format I_P.

! list of kinds
INTEGER,      PARAMETER :: CHARACTER_KINDS_LIST(1:3) = [ASCII, UCS4, CK]                        !< List of chaRACTER KINDS.
INTEGER,      PARAMETER :: REAL_KINDS_LIST(1:4)      = [R16P, R8P, R4P, R_P]                    !< List of reaL KINDS.
CHARACTER(*), PARAMETER :: REAL_FORMATS_LIST(1:4)    = [FR16P, FR8P, FR4P//' ', FR_P]           !< List of reaL FORMATS.
INTEGER,      PARAMETER :: INTEGER_KINDS_LIST(1:5)   = [I8P, I4P, I2P, I1P,I_P]                 !< List of intEGER KINDS.
CHARACTER(*), PARAMETER :: INTEGER_FORMATS_LIST(1:5) = [FI8P, FI4P, FI2P//' ', FI1P//' ', FI_P] !< List of intEGER FORMATS.

! minimum and maximum (representable) values
REAL(R16P),   PARAMETER :: MINR16P = -HUGE(1._R16P) !< Minimum value of kind=R16P real.
REAL(R16P),   PARAMETER :: MAXR16P =  HUGE(1._R16P) !< Maximum value of kind=R16P real.
REAL(R8P),    PARAMETER :: MINR8P  = -HUGE(1._R8P ) !< Minimum value of kind=R8P real.
REAL(R8P),    PARAMETER :: MAXR8P  =  HUGE(1._R8P ) !< Maximum value of kind=R8P real.
REAL(R4P),    PARAMETER :: MINR4P  = -HUGE(1._R4P ) !< Minimum value of kind=R4P real.
REAL(R4P),    PARAMETER :: MAXR4P  =  HUGE(1._R4P ) !< Maximum value of kind=R4P real.
REAL(R_P),    PARAMETER :: MINR_P  = -HUGE(1._R_P ) !< Minimum value of kind=R_P real.
REAL(R_P),    PARAMETER :: MAXR_P  =  HUGE(1._R_P ) !< Maximum value of kind=R_P real.
INTEGER(I8P), PARAMETER :: MINI8P  = -HUGE(1_I8P)   !< Minimum value of kind=I8P integer.
INTEGER(I4P), PARAMETER :: MINI4P  = -HUGE(1_I4P)   !< Minimum value of kind=I4P integer.
INTEGER(I2P), PARAMETER :: MINI2P  = -HUGE(1_I2P)   !< Minimum value of kind=I2P integer.
INTEGER(I1P), PARAMETER :: MINI1P  = -HUGE(1_I1P)   !< Minimum value of kind=I1P integer.
INTEGER(I_P), PARAMETER :: MINI_P  = -HUGE(1_I_P)   !< Minimum value of kind=I_P integer.
INTEGER(I8P), PARAMETER :: MAXI8P  =  HUGE(1_I8P)   !< Maximum value of kind=I8P integer.
INTEGER(I4P), PARAMETER :: MAXI4P  =  HUGE(1_I4P)   !< Maximum value of kind=I4P integer.
INTEGER(I2P), PARAMETER :: MAXI2P  =  HUGE(1_I2P)   !< Maximum value of kind=I2P integer.
INTEGER(I1P), PARAMETER :: MAXI1P  =  HUGE(1_I1P)   !< Maximum value of kind=I1P integer.
INTEGER(I_P), PARAMETER :: MAXI_P  =  HUGE(1_I_P)   !< Maximum value of kind=I_P integer.

! real smallest (representable) values
REAL(R16P), PARAMETER :: SMALLR16P = TINY(1._R16P) !< Smallest representable value of kind=R16P real.
REAL(R8P),  PARAMETER :: SMALLR8P  = TINY(1._R8P ) !< Smallest representable value of kind=R8P real.
REAL(R4P),  PARAMETER :: SMALLR4P  = TINY(1._R4P ) !< Smallest representable value of kind=R4P real.
REAL(R_P),  PARAMETER :: SMALLR_P  = TINY(1._R_P ) !< Smallest representable value of kind=R_P real.

! smallest real representable difference by the running calculator
REAL(R16P), PARAMETER :: ZEROR16P = NEAREST(1._R16P, 1._R16P) - &
                                    NEAREST(1._R16P,-1._R16P) !< Smallest representable difference of kind=R16P REAL.
REAL(R8P),  PARAMETER :: ZEROR8P  = NEAREST(1._R8P, 1._R8P) - &
                                    NEAREST(1._R8P,-1._R8P)   !< Smallest representable difference of kind=R8P REAL.
REAL(R4P),  PARAMETER :: ZEROR4P  = NEAREST(1._R4P, 1._R4P) - &
                                    NEAREST(1._R4P,-1._R4P)   !< Smallest representable difference of kind=R4P REAL.
REAL(R_P),  PARAMETER :: ZEROR_P  = NEAREST(1._R_P, 1._R_P) - &
                                    NEAREST(1._R_P,-1._R_P)   !< Smallest representable difference of kind=R_P REAL.

! bits/bytes memory requirements (real variables must be computed at runtime)
INTEGER(I2P)            :: BIR16P                         !< Number of bits of kind=R16P real.
INTEGER(I1P)            :: BIR8P                          !< Number of bits of kind=R8P real.
INTEGER(I1P)            :: BIR4P                          !< Number of bits of kind=R4P real.
INTEGER(I1P)            :: BIR_P                          !< Number of bits of kind=R_P real.
INTEGER(I2P)            :: BYR16P                         !< Number of bytes of kind=R16P real.
INTEGER(I1P)            :: BYR8P                          !< Number of bytes of kind=R8P real.
INTEGER(I1P)            :: BYR4P                          !< Number of bytes of kind=R4P real.
INTEGER(I1P)            :: BYR_P                          !< Number of bytes of kind=R_P real.
INTEGER(I8P), PARAMETER :: BII8P = BIT_SIZE(MAXI8P)       !< Number of bits of kind=I8P integer.
INTEGER(I4P), PARAMETER :: BII4P = BIT_SIZE(MAXI4P)       !< Number of bits of kind=I4P integer.
INTEGER(I2P), PARAMETER :: BII2P = BIT_SIZE(MAXI2P)       !< Number of bits of kind=I2P integer.
INTEGER(I1P), PARAMETER :: BII1P = BIT_SIZE(MAXI1P)       !< Number of bits of kind=I1P integer.
INTEGER(I_P), PARAMETER :: BII_P = BIT_SIZE(MAXI_P)       !< Number of bits of kind=I_P integer.
INTEGER(I8P), PARAMETER :: BYI8P = BIT_SIZE(MAXI8P)/8_I8P !< Number of bytes of kind=I8P integer.
INTEGER(I4P), PARAMETER :: BYI4P = BIT_SIZE(MAXI4P)/8_I4P !< Number of bytes of kind=I4P integer.
INTEGER(I2P), PARAMETER :: BYI2P = BIT_SIZE(MAXI2P)/8_I2P !< Number of bytes of kind=I2P integer.
INTEGER(I1P), PARAMETER :: BYI1P = BIT_SIZE(MAXI1P)/8_I1P !< Number of bytes of kind=I1P integer.
INTEGER(I_P), PARAMETER :: BYI_P = BIT_SIZE(MAXI_P)/8_I_P !< Number of bytes of kind=I_P integer.
ENDMODULE ModLib_PenfGlobal
