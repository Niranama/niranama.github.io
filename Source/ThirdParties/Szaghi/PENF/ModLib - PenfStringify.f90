!< PENF string-to-number (and viceversa) facility.

MODULE ModLib_PenfStringify
!< PENF string-to-number (and viceversa) facility.
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : STDERR=>ERROR_UNIT
USE ModLib_PenfBSize
USE ModLib_PenfGlobal

IMPLICIT NONE
PRIVATE
SAVE
PUBLIC :: STR_ASCII, STR_UCS4
PUBLIC :: STR, STRZ, CTON
PUBLIC :: BSTR, BCTON

INTERFACE STR_ASCII
   !< Convert string of any kind to ASCII string.
   MODULE PROCEDURE STR_ASCII_DEFAULT
#if defined _ASCII_SUPPORTED && defined _ASCII_NEQ_DEFAULT
   MODULE PROCEDURE STR_ASCII_ASCII
#endif
#ifdef _UCS4_SUPPORTED
   MODULE PROCEDURE STR_ASCII_UCS4
#endif
ENDINTERFACE

INTERFACE STR_UCS4
   !< Convert string of any kind to UCS4 string.
   MODULE PROCEDURE STR_UCS4_DEFAULT
#if defined _ASCII_SUPPORTED && defined _ASCII_NEQ_DEFAULT
   MODULE PROCEDURE STR_UCS4_ASCII
#endif
#ifdef _UCS4_SUPPORTED
   MODULE PROCEDURE STR_UCS4_UCS4
#endif
ENDINTERFACE

INTERFACE STR
  !< Convert number (real and integer) to string (number to string type casting).
  MODULE PROCEDURE                       &
                   STRF_R16P,STR_R16P,   &
                   STRF_R8P ,STR_R8P,    &
                   STRF_R4P ,STR_R4P,    &
                   STRF_I8P ,STR_I8P,    &
                   STRF_I4P ,STR_I4P,    &
                   STRF_I2P ,STR_I2P,    &
                   STRF_I1P ,STR_I1P,    &
                             STR_BOL,    &
                             STR_A_R16P, &
                             STR_A_R8P,  &
                             STR_A_R4P,  &
                             STR_A_I8P,  &
                             STR_A_I4P,  &
                             STR_A_I2P,  &
                             STR_A_I1P
ENDINTERFACE

INTERFACE STRZ
  !< Convert integer, to string, prefixing with the right number of zeros (integer to string type casting with ZERO PADDING).
  MODULE PROCEDURE STRZ_I8P, STRZ_I4P, STRZ_I2P, STRZ_I1P
ENDINTERFACE

INTERFACE CTON
  !< Convert string to number (real and integer, string to number type casting).
  MODULE PROCEDURE            &
                   CTOR_R16P, &
                   CTOR_R8P,  &
                   CTOR_R4P,  &
                   CTOI_I8P,  &
                   CTOI_I4P,  &
                   CTOI_I2P,  &
                   CTOI_I1P
ENDINTERFACE

INTERFACE BSTR
  !< Convert number (real and integer) to bit-string (number to bit-string type casting).
  MODULE PROCEDURE            &
                   BSTR_R16P, &
                   BSTR_R8P,  &
                   BSTR_R4P,  &
                   BSTR_I8P,  &
                   BSTR_I4P,  &
                   BSTR_I2P,  &
                   BSTR_I1P
ENDINTERFACE

INTERFACE BCTON
  !< Convert bit-string to number (real and integer, bit-string to number type casting).
  MODULE PROCEDURE             &
                   BCTOR_R16P, &
                   BCTOR_R8P,  &
                   BCTOR_R4P,  &
                   BCTOI_I8P,  &
                   BCTOI_I4P,  &
                   BCTOI_I2P,  &
                   BCTOI_I1P
ENDINTERFACE

CONTAINS
   PURE FUNCTION STR_ASCII_DEFAULT(INPUT) RESULT(OUTPUT)
   !< Convert string of default kind to ASCII string.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=ASCII), allocatable :: string
   !< string = str_ascii('I was DEFAULT kind, but now I am ASCII')
   !< print "(A)", string
   !<```
   !=> I was DEFAULT kind, but now I am ASCII <<<
   CHARACTER(LEN=*), INTENT(IN)              :: INPUT  !< Input string of default kind.
   CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: OUTPUT !< Output string of ASCII kind.

   OUTPUT = INPUT
   ENDFUNCTION STR_ASCII_DEFAULT

   PURE FUNCTION STR_ASCII_ASCII(INPUT) RESULT(OUTPUT)
   !< Convert string of ASCII kind to ASCII string, just for convenience in sanitize strings.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=ASCII), allocatable :: string
   !< string = str_ascii('I was ASCII kind and I am still ASCII')
   !< print "(A)", string
   !<```
   !=> I was ASCII kind and I am still ASCII <<<
   CHARACTER(LEN=*, KIND=ASCII), INTENT(IN)  :: INPUT  !< Input string of ASCII kind.
   CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: OUTPUT !< Output string of ASCII kind.

   OUTPUT = INPUT
   ENDFUNCTION STR_ASCII_ASCII

   PURE FUNCTION STR_ASCII_UCS4(INPUT) RESULT(OUTPUT)
   !< Convert string of UCS4 kind to ASCII string.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=ASCII), allocatable :: string
   !< string = str_ascii(UCS4_'I was UCS4 kind, but now I am ASCII')
   !< print "(A)", string
   !<```
   !=> I was UCS4 kind, but now I am ASCII <<<
   CHARACTER(LEN=*, KIND=UCS4), INTENT(IN)   :: INPUT  !< Input string of UCS4 kind.
   CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: OUTPUT !< Output string of ASCII kind.

   OUTPUT = INPUT
   ENDFUNCTION STR_ASCII_UCS4

   PURE FUNCTION STR_UCS4_DEFAULT(INPUT) RESULT(OUTPUT)
   !< Convert string of default kind to UCS4 string.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=UCS4), allocatable :: string
   !< string = str_ascii('I was DEFAULT kind, but now I am UCS4')
   !< print "(A)", string
   !<```
   !=> I was DEFAULT kind, but now I am UCS4 <<<
   CHARACTER(LEN=*), INTENT(IN)             :: INPUT  !< Input string of default kind.
   CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: OUTPUT !< Output string of UCS4 kind.

   OUTPUT = INPUT
   ENDFUNCTION STR_UCS4_DEFAULT

   PURE FUNCTION STR_UCS4_ASCII(INPUT) RESULT(OUTPUT)
   !< Convert string of ASCII kind to UCS4 string.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=UCS4), allocatable :: string
   !< string = str_ascii(ASCII_'I was ASCII kind, but now I am UCS4')
   !< print "(A)", string
   !<```
   !=> I was ASCII kind, but now I am UCS4 <<<
   CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: INPUT  !< Input string of ASCII kind.
   CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: OUTPUT !< Output string of UCS4 kind.

   OUTPUT = INPUT
   ENDFUNCTION STR_UCS4_ASCII

   PURE FUNCTION STR_UCS4_UCS4(INPUT) RESULT(OUTPUT)
   !< Convert string of UCS4 kind to UCS4 string, just for convenience in sanitize strings.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=UCS4), allocatable :: string
   !< string = str_ascii(UCS4_'I was UCS4 kind and I am still UCS4')
   !< print "(A)", string
   !<```
   !=> I was UCS4 kind and I am still UCS4 <<<
   CHARACTER(LEN=*, KIND=UCS4), INTENT(IN)  :: INPUT  !< Input string of UCS4 kind.
   CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: OUTPUT !< Output string of UCS4 kind.

   OUTPUT = INPUT
   ENDFUNCTION STR_UCS4_UCS4

   ELEMENTAL FUNCTION STRF_R16P(FM, N) RESULT(STR)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FR16P, n=1._R16P)
   !<```
   !=> 0.100000000000000000000000000000000E+0001 <<<
   CHARACTER(*), INTENT(IN) :: FM  !< Format different from the standard for the kind.
   REAL(R16P),   INTENT(IN) :: N   !< Real to be converted.
   CHARACTER(DR16P)         :: STR !< Returned string containing input number.

   WRITE(STR, TRIM(FM)) N
   ENDFUNCTION STRF_R16P

   ELEMENTAL FUNCTION STRF_R8P(FM, N) RESULT(STR)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FR8P, n=1._R8P)
   !<```
   !=> 0.100000000000000E+001 <<<
   CHARACTER(*), INTENT(IN) :: FM  !< Format different from the standard for the kind.
   REAL(R8P),    INTENT(IN) :: N   !< Real to be converted.
   CHARACTER(DR8P)          :: STR !< Returned string containing input number.

   WRITE(STR, TRIM(FM)) N
   ENDFUNCTION STRF_R8P

   ELEMENTAL FUNCTION STRF_R4P(FM, N) RESULT(STR)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FR4P, n=1._R4P)
   !<```
   !=> 0.100000E+01 <<<
   CHARACTER(*), INTENT(IN) :: FM  !< Format different from the standard for the kind.
   REAL(R4P),    INTENT(IN) :: N   !< Real to be converted.
   CHARACTER(DR4P)          :: STR !< Returned string containing input number.

   WRITE(STR, TRIM(FM)) N
   ENDFUNCTION STRF_R4P

   ELEMENTAL FUNCTION STRF_I8P(FM, N) RESULT(STR)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FI8P, n=1_I8P)
   !<```
   !=> 1 <<<
   CHARACTER(*), INTENT(IN) :: FM  !< Format different from the standard for the kind.
   INTEGER(I8P), INTENT(IN) :: N   !< Integer to be converted.
   CHARACTER(DI8P)          :: STR !< Returned string containing input number.

   WRITE(STR, TRIM(FM)) N
   ENDFUNCTION STRF_I8P

   ELEMENTAL FUNCTION STRF_I4P(FM, N) RESULT(STR)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FI4P, n=1_I4P)
   !<```
   !=> 1 <<<
   CHARACTER(*), INTENT(IN) :: FM  !< Format different from the standard for the kind.
   INTEGER(I4P), INTENT(IN) :: N   !< Integer to be converted.
   CHARACTER(DI4P)          :: STR !< Returned string containing input number.

   WRITE(STR, TRIM(FM)) N
   ENDFUNCTION STRF_I4P

   ELEMENTAL FUNCTION STRF_I2P(FM, N) RESULT(STR)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FI2P, n=1_I2P)
   !<```
   !=> 1 <<<
   CHARACTER(*), INTENT(IN) :: FM  !< Format different from the standard for the kind.
   INTEGER(I2P), INTENT(IN) :: N   !< Integer to be converted.
   CHARACTER(DI2P)          :: STR !< Returned string containing input number.

   WRITE(STR, TRIM(FM)) N
   ENDFUNCTION STRF_I2P

   ELEMENTAL FUNCTION STRF_I1P(FM, N) RESULT(STR)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FI1P, n=1_I1P)
   !<```
   !=> 1 <<<
   CHARACTER(*), INTENT(IN) :: FM  !< Format different from the standard for the kind.
   INTEGER(I1P), INTENT(IN) :: N   !< Integer to be converted.
   CHARACTER(DI1P)          :: STR !< Returned string containing input number.

   WRITE(STR, TRIM(FM)) N
   ENDFUNCTION STRF_I1P

   ELEMENTAL FUNCTION STR_R16P(N, NO_SIGN, COMPACT) RESULT(STR)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R16P)
   !<```
   !=> -0.100000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R16P, no_sign=.true.)
   !<```
   !=> 0.100000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R16P, compact=.true.)
   !<```
   !=> -0.1E+1 <<<
   REAL(R16P), INTENT(IN)           :: N       !< Real to be converted.
   LOGICAL,    INTENT(IN), OPTIONAL :: NO_SIGN !< Flag for leaving out the sign.
   LOGICAL,    INTENT(IN), OPTIONAL :: COMPACT !< Flag for *compacting* string encoding.
   CHARACTER(DR16P)                 :: STR     !< Returned string containing input number.

   WRITE(STR, FR16P) N               ! Casting of n to string.
   IF (N>0._R16P) STR(1:1)='+'       ! Prefixing plus if n>0.
   IF (PRESENT(NO_SIGN)) STR=STR(2:) ! Leaving out the sign.
   IF (PRESENT(COMPACT)) THEN
     IF (COMPACT) CALL COMPACT_REAL_STRING(STRING=STR)
   ENDIF
   ENDFUNCTION STR_R16P

   ELEMENTAL FUNCTION STR_R8P(N, NO_SIGN, COMPACT) RESULT(STR)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R8P)
   !<```
   !=> -0.100000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R8P, no_sign=.true.)
   !<```
   !=> 0.100000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R8P, compact=.true.)
   !<```
   !=> -0.1E+1 <<<
   REAL(R8P), INTENT(IN)           :: N       !< Real to be converted.
   LOGICAL,   INTENT(IN), OPTIONAL :: NO_SIGN !< Flag for leaving out the sign.
   LOGICAL,   INTENT(IN), OPTIONAL :: COMPACT !< Flag for *compacting* string encoding.
   CHARACTER(DR8P)                 :: STR     !< Returned string containing input number.

   WRITE(STR, FR8P) N                ! Casting of n to string.
   IF (N>0._R8P) STR(1:1)='+'        ! Prefixing plus if n>0.
   IF (PRESENT(NO_SIGN)) STR=STR(2:) ! Leaving out the sign.
   IF (PRESENT(COMPACT)) THEN
     IF (COMPACT) CALL COMPACT_REAL_STRING(STRING=STR)
   ENDIF
   ENDFUNCTION STR_R8P

   ELEMENTAL FUNCTION STR_R4P(N, NO_SIGN, COMPACT) RESULT(STR)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R4P)
   !<```
   !=> -0.100000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R4P, no_sign=.true.)
   !<```
   !=> 0.100000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R4P, compact=.true.)
   !<```
   !=> -0.1E+1 <<<
   REAL(R4P), INTENT(IN)           :: N       !< Real to be converted.
   LOGICAL,   INTENT(IN), OPTIONAL :: NO_SIGN !< Flag for leaving out the sign.
   LOGICAL,   INTENT(IN), OPTIONAL :: COMPACT !< Flag for *compacting* string encoding.
   CHARACTER(DR4P)                 :: STR     !< Returned string containing input number.

   WRITE(STR, FR4P) N                ! Casting of n to string.
   IF (N>0._R4P) STR(1:1)='+'        ! Prefixing plus if n>0.
   IF (PRESENT(NO_SIGN)) STR=STR(2:) ! Leaving out the sign.
   IF (PRESENT(COMPACT)) THEN
     IF (COMPACT) CALL COMPACT_REAL_STRING(STRING=STR)
   ENDIF
   ENDFUNCTION STR_R4P

   ELEMENTAL FUNCTION STR_I8P(N, NO_SIGN) RESULT(STR)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I8P)
   !<```
   !=> -1 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I8P, no_sign=.true.)
   !<```
   !=> 1 <<<
   INTEGER(I8P), INTENT(IN)           :: N       !< Integer to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN !< Flag for leaving out the sign.
   CHARACTER(DI8P)                    :: STR     !< Returned string containing input number plus padding zeros.

   WRITE(STR, FI8P) N                ! Casting of n to string.
   STR = ADJUSTL(TRIM(STR))          ! Removing white spaces.
   IF (N>=0_I8P) STR='+'//TRIM(STR)  ! Prefixing plus if n>0.
   IF (PRESENT(NO_SIGN)) STR=STR(2:) ! Leaving out the sign.
   ENDFUNCTION STR_I8P

   ELEMENTAL FUNCTION STR_I4P(N, NO_SIGN) RESULT(STR)
   !< Converting integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I4P)
   !<```
   !=> -1 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I4P, no_sign=.true.)
   !<```
   !=> 1 <<<
   INTEGER(I4P), INTENT(IN)           :: N       !< Integer to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN !< Flag for leaving out the sign.
   CHARACTER(DI4P)                    :: STR     !< Returned string containing input number plus padding zeros.

   WRITE(STR, FI4P) N                ! Casting of n to string.
   STR = ADJUSTL(TRIM(STR))          ! Removing white spaces.
   IF (N>=0_I4P) STR='+'//TRIM(STR)  ! Prefixing plus if n>0.
   IF (PRESENT(NO_SIGN)) STR=STR(2:) ! Leaving out the sign.
   ENDFUNCTION STR_I4P

   ELEMENTAL FUNCTION STR_I2P(N, NO_SIGN) RESULT(STR)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I2P)
   !<```
   !=> -1 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I2P, no_sign=.true.)
   !<```
   !=> 1 <<<
   INTEGER(I2P), INTENT(IN)           :: N       !< Integer to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN !< Flag for leaving out the sign.
   CHARACTER(DI2P)                    :: STR     !< Returned string containing input number plus padding zeros.

   WRITE(STR, FI2P) N                ! Casting of n to string.
   STR = ADJUSTL(TRIM(STR))          ! Removing white spaces.
   IF (N>=0_I2P) STR='+'//TRIM(STR)  ! Prefixing plus if n>0.
   IF (PRESENT(NO_SIGN)) STR=STR(2:) ! Leaving out the sign.
   ENDFUNCTION STR_I2P

   ELEMENTAL FUNCTION STR_I1P(N, NO_SIGN) RESULT(STR)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I1P)
   !<```
   !=> -1 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I1P, no_sign=.true.)
   !<```
   !=> 1 <<<
   INTEGER(I1P), INTENT(IN)           :: N       !< Integer to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN !< Flag for leaving out the sign.
   CHARACTER(DI1P)                    :: STR     !< Returned string containing input number plus padding zeros.

   WRITE(STR, FI1P) N                ! Casting of n to string.
   STR = ADJUSTL(TRIM(STR))          ! Removing white spaces.
   IF (N>=0_I1P) STR='+'//TRIM(STR)  ! Prefixing plus if n>0.
   IF (PRESENT(NO_SIGN)) STR=STR(2:) ! Leaving out the sign.
   ENDFUNCTION STR_I1P

   ELEMENTAL FUNCTION STR_BOL(N) RESULT(STR)
   !< Convert logical to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=.true.)
   !<```
   !=> T <<<
   LOGICAL, INTENT(IN):: N   !< Logical to be converted.
   CHARACTER(1)::        STR !< Returned string containing input number plus padding zeros.

   WRITE(STR, '(L1)') N
   ENDFUNCTION STR_BOL

   PURE FUNCTION STR_A_R16P(N, NO_SIGN, SEPARATOR, DELIMITERS, COMPACT) RESULT(STR)
   !< Converting real array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, -2._R16P])
   !<```
   !=> +0.100000000000000000000000000000000E+0001,-0.200000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, 2._R16P], no_sign=.true.)
   !<```
   !=> 0.100000000000000000000000000000000E+0001,0.200000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, -2._R16P], separator='|')
   !<```
   !=> +0.100000000000000000000000000000000E+0001|-0.200000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, -2._R16P], delimiters=['(', ')'])
   !<```
   !=> (+0.100000000000000000000000000000000E+0001,-0.200000000000000000000000000000000E+0001) <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, -2._R16P], compact=.true.)
   !<```
   !=> +0.1E+1,-0.2E+1 <<<
   REAL(R16P),   INTENT(IN)           :: N(:)            !< Real array to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN         !< Flag for leaving out the sign.
   CHARACTER(1), INTENT(IN), OPTIONAL :: SEPARATOR       !< Eventual separator of array values.
   CHARACTER(*), INTENT(IN), OPTIONAL :: DELIMITERS(1:2) !< Eventual delimiters of array values.
   LOGICAL,      INTENT(IN), OPTIONAL :: COMPACT         !< Flag for *compacting* string encoding.
   CHARACTER(LEN=:), ALLOCATABLE      :: STR             !< Returned string containing input number.
   CHARACTER(DR16P)                   :: STRN            !< String containing of element of input array number.
   CHARACTER(LEN=1)                   :: SEP             !< Array values separator
   INTEGER                            :: I               !< Counter.

   STR = ''
   SEP = ','
   IF(PRESENT(SEPARATOR)) SEP = SEPARATOR
   DO I=1,SIZE(N)
     STRN = STR_R16P(NO_SIGN=NO_SIGN, COMPACT=COMPACT, N=N(I))
     STR = STR//SEP//TRIM(STRN)
   ENDDO
   STR = TRIM(STR(2:))
   IF (PRESENT(DELIMITERS)) STR = DELIMITERS(1)//STR//DELIMITERS(2)
   ENDFUNCTION STR_A_R16P

   PURE FUNCTION STR_A_R8P(N, NO_SIGN, SEPARATOR, DELIMITERS, COMPACT) RESULT(STR)
   !< Convert real array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, -2._R8P])
   !<```
   !=> +0.100000000000000E+001,-0.200000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, 2._R8P], no_sign=.true.)
   !<```
   !=> 0.100000000000000E+001,0.200000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, -2._R8P], separator='|')
   !<```
   !=> +0.100000000000000E+001|-0.200000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, -2._R8P], delimiters=['(', ')'])
   !<```
   !=> (+0.100000000000000E+001,-0.200000000000000E+001) <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, -2._R8P], compact=.true.)
   !<```
   !=> +0.1E+1,-0.2E+1 <<<
   REAL(R8P),    INTENT(IN)           :: N(:)            !< Real array to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN         !< Flag for leaving out the sign.
   CHARACTER(1), INTENT(IN), OPTIONAL :: SEPARATOR       !< Eventual separator of array values.
   CHARACTER(*), INTENT(IN), OPTIONAL :: DELIMITERS(1:2) !< Eventual delimiters of array values.
   LOGICAL,      INTENT(IN), OPTIONAL :: COMPACT         !< Flag for *compacting* string encoding.
   CHARACTER(LEN=:), ALLOCATABLE      :: STR             !< Returned string containing input number.
   CHARACTER(DR8P)                    :: STRN            !< String containing of element of input array number.
   CHARACTER(LEN=1)                   :: SEP             !< Array values separator
   INTEGER                            :: I               !< Counter.

   STR = ''
   SEP = ','
   IF(PRESENT(SEPARATOR)) SEP = SEPARATOR
   DO I=1,SIZE(N)
     STRN = STR_R8P(NO_SIGN=NO_SIGN, COMPACT=COMPACT, N=N(I))
     STR = STR//SEP//TRIM(STRN)
   ENDDO
   STR = TRIM(STR(2:))
   IF (PRESENT(DELIMITERS)) STR = DELIMITERS(1)//STR//DELIMITERS(2)
   ENDFUNCTION STR_A_R8P

   PURE FUNCTION STR_A_R4P(N, NO_SIGN, SEPARATOR, DELIMITERS, COMPACT) RESULT(STR)
   !< Convert real array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, -2._R4P])
   !<```
   !=> +0.100000E+01,-0.200000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, 2._R4P], no_sign=.true.)
   !<```
   !=> 0.100000E+01,0.200000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, -2._R4P], separator='|')
   !<```
   !=> +0.100000E+01|-0.200000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, -2._R4P], delimiters=['(', ')'])
   !<```
   !=> (+0.100000E+01,-0.200000E+01) <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, -2._R4P], compact=.true.)
   !<```
   !=> +0.1E+1,-0.2E+1 <<<
   REAL(R4P),    INTENT(IN)           :: N(:)            !< Real array to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN         !< Flag for leaving out the sign.
   CHARACTER(1), INTENT(IN), OPTIONAL :: SEPARATOR       !< Eventual separator of array values.
   CHARACTER(*), INTENT(IN), OPTIONAL :: DELIMITERS(1:2) !< Eventual delimiters of array values.
   LOGICAL,      INTENT(IN), OPTIONAL :: COMPACT         !< Flag for *compacting* string encoding.
   CHARACTER(LEN=:), ALLOCATABLE      :: STR             !< Returned string containing input number.
   CHARACTER(DR4P)                    :: STRN            !< String containing of element of input array number.
   CHARACTER(LEN=1)                   :: SEP             !< Array values separator
   INTEGER                            :: I               !< Counter.

   STR = ''
   SEP = ','
   IF(PRESENT(SEPARATOR)) SEP = SEPARATOR
   DO I=1,SIZE(N)
     STRN = STR_R4P(NO_SIGN=NO_SIGN, COMPACT=COMPACT, N=N(I))
     STR = STR//SEP//TRIM(STRN)
   ENDDO
   STR = TRIM(STR(2:))
   IF (PRESENT(DELIMITERS)) STR = DELIMITERS(1)//STR//DELIMITERS(2)
   ENDFUNCTION STR_A_R4P

   PURE FUNCTION STR_A_I8P(N, NO_SIGN, SEPARATOR, DELIMITERS) RESULT(STR)
   !< Convert integer array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I8P, -2_I8P])
   !<```
   !=> +1,-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I8P, 2_I8P], no_sign=.true.)
   !<```
   !=> 1,2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I8P, -2_I8P], separator='|')
   !<```
   !=> +1|-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I8P, -2_I8P], delimiters=['(', ')'])
   !<```
   !=> (+1,-2) <<<
   INTEGER(I8P), INTENT(IN)           :: N(:)            !< Integer array to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN         !< Flag for leaving out the sign.
   CHARACTER(1), INTENT(IN), OPTIONAL :: SEPARATOR       !< Eventual separator of array values.
   CHARACTER(*), INTENT(IN), OPTIONAL :: DELIMITERS(1:2) !< Eventual delimiters of array values.
   CHARACTER(LEN=:), ALLOCATABLE      :: STR             !< Returned string containing input number.
   CHARACTER(DI8P)                    :: STRN            !< String containing of element of input array number.
   CHARACTER(LEN=1)                   :: SEP             !< Array values separator
   INTEGER                            :: I               !< Counter.

   STR = ''
   SEP = ','
   IF(PRESENT(SEPARATOR)) SEP = SEPARATOR
   IF (PRESENT(NO_SIGN)) THEN
     DO I=1,SIZE(N)
       STRN = STR_I8P(NO_SIGN=NO_SIGN, N=N(I))
       STR = STR//SEP//TRIM(STRN)
     ENDDO
   ELSE
     DO I=1,SIZE(N)
       STRN = STR_I8P(N=N(I))
       STR = STR//SEP//TRIM(STRN)
     ENDDO
   ENDIF
   STR = TRIM(STR(2:))
   IF (PRESENT(DELIMITERS)) STR = DELIMITERS(1)//STR//DELIMITERS(2)
   ENDFUNCTION STR_A_I8P

   PURE FUNCTION STR_A_I4P(N, NO_SIGN, SEPARATOR, DELIMITERS) RESULT(STR)
   !< Convert integer array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I4P, -2_I4P])
   !<```
   !=> +1,-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I4P, 2_I4P], no_sign=.true.)
   !<```
   !=> 1,2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I4P, -2_I4P], separator='|')
   !<```
   !=> +1|-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I4P, -2_I4P], delimiters=['(', ')'])
   !<```
   !=> (+1,-2) <<<
   INTEGER(I4P), INTENT(IN)           :: N(:)            !< Integer array to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN         !< Flag for leaving out the sign.
   CHARACTER(1), INTENT(IN), OPTIONAL :: SEPARATOR       !< Eventual separator of array values.
   CHARACTER(*), INTENT(IN), OPTIONAL :: DELIMITERS(1:2) !< Eventual delimiters of array values.
   CHARACTER(LEN=:), ALLOCATABLE      :: STR             !< Returned string containing input number.
   CHARACTER(DI4P)                    :: STRN            !< String containing of element of input array number.
   CHARACTER(LEN=1)                   :: SEP             !< Array values separator
   INTEGER                            :: I               !< Counter.

   STR = ''
   SEP = ','
   IF(PRESENT(SEPARATOR)) SEP = SEPARATOR
   IF (PRESENT(NO_SIGN)) THEN
     DO I=1,SIZE(N)
       STRN = STR_I4P(NO_SIGN=NO_SIGN, N=N(I))
       STR = STR//SEP//TRIM(STRN)
     ENDDO
   ELSE
     DO I=1,SIZE(N)
       STRN = STR_I4P(N=N(I))
       STR = STR//SEP//TRIM(STRN)
     ENDDO
   ENDIF
   STR = TRIM(STR(2:))
   IF (PRESENT(DELIMITERS)) STR = DELIMITERS(1)//STR//DELIMITERS(2)
   ENDFUNCTION STR_A_I4P

   PURE FUNCTION STR_A_I2P(N, NO_SIGN, SEPARATOR, DELIMITERS) RESULT(STR)
   !< Convert integer array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I2P, -2_I2P])
   !<```
   !=> +1,-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I2P, 2_I2P], no_sign=.true.)
   !<```
   !=> 1,2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I2P, -2_I2P], separator='|')
   !<```
   !=> +1|-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I2P, -2_I2P], delimiters=['(', ')'])
   !<```
   !=> (+1,-2) <<<
   INTEGER(I2P), INTENT(IN)           :: N(:)            !< Integer array to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN         !< Flag for leaving out the sign.
   CHARACTER(1), INTENT(IN), OPTIONAL :: SEPARATOR       !< Eventual separator of array values.
   CHARACTER(*), INTENT(IN), OPTIONAL :: DELIMITERS(1:2) !< Eventual delimiters of array values.
   CHARACTER(LEN=:), ALLOCATABLE      :: STR             !< Returned string containing input number.
   CHARACTER(DI2P)                    :: STRN            !< String containing of element of input array number.
   CHARACTER(LEN=1)                   :: SEP             !< Array values separator
   INTEGER                            :: I               !< Counter.

   STR = ''
   SEP = ','
   IF(PRESENT(SEPARATOR)) SEP = SEPARATOR
   IF (PRESENT(NO_SIGN)) THEN
     DO I=1,SIZE(N)
       STRN = STR_I2P(NO_SIGN=NO_SIGN, N=N(I))
       STR = STR//SEP//TRIM(STRN)
     ENDDO
   ELSE
     DO I=1,SIZE(N)
       STRN = STR_I2P(N=N(I))
       STR = STR//SEP//TRIM(STRN)
     ENDDO
   ENDIF
   STR = TRIM(STR(2:))
   IF (PRESENT(DELIMITERS)) STR = DELIMITERS(1)//STR//DELIMITERS(2)
   ENDFUNCTION STR_A_I2P

   PURE FUNCTION STR_A_I1P(N, NO_SIGN, SEPARATOR, DELIMITERS) RESULT(STR)
   !< Convert integer array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I1P, -2_I1P])
   !<```
   !=> +1,-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I1P, 2_I1P], no_sign=.true.)
   !<```
   !=> 1,2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I1P, -2_I1P], separator='|')
   !<```
   !=> +1|-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I1P, -2_I1P], delimiters=['(', ')'])
   !<```
   !=> (+1,-2) <<<
   INTEGER(I1P), INTENT(IN)           :: N(:)            !< Integer array to be converted.
   LOGICAL,      INTENT(IN), OPTIONAL :: NO_SIGN         !< Flag for leaving out the sign.
   CHARACTER(1), INTENT(IN), OPTIONAL :: SEPARATOR       !< Eventual separator of array values.
   CHARACTER(*), INTENT(IN), OPTIONAL :: DELIMITERS(1:2) !< Eventual delimiters of array values.
   CHARACTER(LEN=:), ALLOCATABLE      :: STR             !< Returned string containing input number.
   CHARACTER(DI1P)                    :: STRN            !< String containing of element of input array number.
   CHARACTER(LEN=1)                   :: SEP             !< Array values separator
   INTEGER                            :: I               !< Counter.

   STR = ''
   SEP = ','
   IF(PRESENT(SEPARATOR)) SEP = SEPARATOR
   IF (PRESENT(NO_SIGN)) THEN
     DO I=1,SIZE(N)
       STRN = STR_I1P(NO_SIGN=NO_SIGN, N=N(I))
       STR = STR//SEP//TRIM(STRN)
     ENDDO
   ELSE
     DO I=1,SIZE(N)
       STRN = STR_I1P(N=N(I))
       STR = STR//SEP//TRIM(STRN)
     ENDDO
   ENDIF
   STR = TRIM(STR(2:))
   IF (PRESENT(DELIMITERS)) STR = DELIMITERS(1)//STR//DELIMITERS(2)
   ENDFUNCTION STR_A_I1P

   PURE SUBROUTINE COMPACT_REAL_STRING(STRING)
   !< author: Izaak Beekman
   !< date: 02/24/2015
   !<
   !< Compact a string representing a real number, so that the same value is displayed with fewer characters.
   !<
   !< @note No need to add doctest: this is tested by a lot of doctests of other TBPs.
   CHARACTER(LEN=*),INTENT(INOUT) :: STRING      !< string representation of a real number.
   CHARACTER(LEN=LEN(STRING))     :: SIGNIFICAND !< Significand characters.
   CHARACTER(LEN=LEN(STRING))     :: EXPNT       !< Exponent characters.
   CHARACTER(LEN=2)               :: SEPARATOR   !< Separator characters.
   INTEGER(I4P)                   :: EXP_START   !< Start position of exponent.
   INTEGER(I4P)                   :: DECIMAL_POS !< Decimal positions.
   INTEGER(I4P)                   :: SIG_TRIM    !< Signature trim.
   INTEGER(I4P)                   :: EXP_TRIM    !< Exponent trim.
   INTEGER(I4P)                   :: I           !< counter

   STRING = ADJUSTL(STRING)
   EXP_START = SCAN(STRING, 'eEdD')
   IF (EXP_START == 0) EXP_START = SCAN(STRING, '-+', BACK=.TRUE.)
   DECIMAL_POS = SCAN(STRING, '.')
   IF (EXP_START /= 0) SEPARATOR = STRING(EXP_START:EXP_START)
   IF ( EXP_START < DECIMAL_POS ) THEN ! possibly signed, exponent-less float
     SIGNIFICAND = STRING
     SIG_TRIM = LEN(TRIM(SIGNIFICAND))
     DO I = LEN(TRIM(SIGNIFICAND)), DECIMAL_POS+2, -1 ! look from right to left at 0s, but save one after the DECIMAL PLACE
       IF (SIGNIFICAND(I:I) == '0') THEN
         SIG_TRIM = I-1
       ELSE
         EXIT
       ENDIF
     ENDDO
     STRING = TRIM(SIGNIFICAND(1:SIG_TRIM))
   ELSEIF (EXP_START > DECIMAL_POS) THEN ! float has exponent
     SIGNIFICAND = STRING(1:EXP_START-1)
     SIG_TRIM = LEN(TRIM(SIGNIFICAND))
     DO I = LEN(TRIM(SIGNIFICAND)),DECIMAL_POS+2,-1 ! look from right to left at 0s
       IF (SIGNIFICAND(I:I) == '0') THEN
         SIG_TRIM = I-1
       ELSE
         EXIT
       ENDIF
     ENDDO
     EXPNT = ADJUSTL(STRING(EXP_START+1:))
     IF (EXPNT(1:1) == '+' .OR. EXPNT(1:1) == '-') THEN
       SEPARATOR = TRIM(ADJUSTL(SEPARATOR))//EXPNT(1:1)
       EXP_START = EXP_START + 1
       EXPNT     = ADJUSTL(STRING(EXP_START+1:))
     ENDIF
     EXP_TRIM = 1
     DO I = 1,(LEN(TRIM(EXPNT))-1) ! look at exponent leading zeros saving last
       IF (EXPNT(I:I) == '0') THEN
         EXP_TRIM = I+1
       ELSE
         EXIT
       ENDIF
     ENDDO
     STRING = TRIM(ADJUSTL(SIGNIFICAND(1:SIG_TRIM)))// &
              TRIM(ADJUSTL(SEPARATOR))// &
              TRIM(ADJUSTL(EXPNT(EXP_TRIM:)))
   !else ! mal-formed real, BUT this code should be unreachable
   ENDIF
   ENDSUBROUTINE COMPACT_REAL_STRING

   ELEMENTAL FUNCTION STRZ_I8P(N, NZ_PAD) RESULT(STR)
   !< Converting integer to string, prefixing with the right number of zeros.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I8P)
   !<```
   !=> 0000000000000000001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I8P, nz_pad=5)
   !<```
   !=> 00001 <<<
   INTEGER(I8P), INTENT(IN)           :: N      !< Integer to be converted.
   INTEGER(I4P), INTENT(IN), OPTIONAL :: NZ_PAD !< Number of zeros padding.
   CHARACTER(DI8P)                    :: STR    !< Returned string containing input number plus padding zeros.

   WRITE(STR,FI8PZP) N                              ! Casting of n to string.
   STR=STR(2:)                                      ! Leaving out the sign.
   IF (PRESENT(NZ_PAD)) STR=STR(DI8P-NZ_PAD:DI8P-1) ! Leaving out the extra zeros padding
   ENDFUNCTION STRZ_I8P

   ELEMENTAL FUNCTION STRZ_I4P(N, NZ_PAD) RESULT(STR)
   !< Convert integer to string, prefixing with the right number of zeros.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I4P)
   !<```
   !=> 0000000001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I4P, nz_pad=5)
   !<```
   !=> 00001 <<<
   INTEGER(I4P), INTENT(IN)           :: N      !< Integer to be converted.
   INTEGER(I4P), INTENT(IN), OPTIONAL :: NZ_PAD !< Number of zeros padding.
   CHARACTER(DI4P)                    :: STR    !< Returned string containing input number plus padding zeros.

   WRITE(STR,FI4PZP) N                              ! Casting of n to string.
   STR=STR(2:)                                      ! Leaving out the sign.
   IF (PRESENT(NZ_PAD)) STR=STR(DI4P-NZ_PAD:DI4P-1) ! Leaving out the extra zeros padding
   ENDFUNCTION STRZ_I4P

   ELEMENTAL FUNCTION STRZ_I2P(N, NZ_PAD) RESULT(STR)
   !< Convert integer to string, prefixing with the right number of zeros.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I2P)
   !<```
   !=> 00001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I2P, nz_pad=3)
   !<```
   !=> 001 <<<
   INTEGER(I2P), INTENT(IN)           :: N      !< Integer to be converted.
   INTEGER(I4P), INTENT(IN), OPTIONAL :: NZ_PAD !< Number of zeros padding.
   CHARACTER(DI2P)                    :: STR    !< Returned string containing input number plus padding zeros.

   WRITE(STR,FI2PZP) N                              ! Casting of n to string.
   STR=STR(2:)                                      ! Leaving out the sign.
   IF (PRESENT(NZ_PAD)) STR=STR(DI2P-NZ_PAD:DI2P-1) ! Leaving out the extra zeros padding
   ENDFUNCTION STRZ_I2P

   ELEMENTAL FUNCTION STRZ_I1P(N, NZ_PAD) RESULT(STR)
   !< Convert integer to string, prefixing with the right number of zeros.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I1P)
   !<```
   !=> 001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I1P, nz_pad=3)
   !<```
   !=> 001 <<<
   INTEGER(I1P), INTENT(IN)           :: N      !< Integer to be converted.
   INTEGER(I4P), INTENT(IN), OPTIONAL :: NZ_PAD !< Number of zeros padding.
   CHARACTER(DI1P)                    :: STR    !< Returned string containing input number plus padding zeros.

   WRITE(STR,FI1PZP) N                              ! Casting of n to string.
   STR=STR(2:)                                      ! Leaving out the sign.
   IF (PRESENT(NZ_PAD)) STR=STR(DI1P-NZ_PAD:DI1P-1) ! Leaving out the extra zeros padding
   ENDFUNCTION STRZ_I1P

   FUNCTION CTOR_R16P(STR, KND, PREF, ERROR) RESULT(N)
   !< Convert string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR16P, cton(str='-1.0', knd=1._R16P)
   !<```
   !=> -0.100000000000000000000000000000000E+0001 <<<
   CHARACTER(*),           INTENT(IN)  :: STR   !< String containing input number.
   REAL(R16P),             INTENT(IN)  :: KND   !< Number kind.
   CHARACTER(*), OPTIONAL, INTENT(IN)  :: PREF  !< Prefixing string.
   INTEGER(I4P), OPTIONAL, INTENT(OUT) :: ERROR !< Error trapping flag: 0 no errors, >0 error occurs.
   REAL(R16P)                          :: N     !< Number returned.
   INTEGER(I4P)                        :: ERR   !< Error trapping flag: 0 no errors, >0 error occurs.
   CHARACTER(LEN=:), ALLOCATABLE       :: PREFD !< Prefixing string.

   READ(STR, *, IOSTAT=ERR) N ! Casting of str to n.
   IF (ERR/=0) THEN
     PREFD = '' ; IF (PRESENT(PREF)) PREFD = PREF
     WRITE(STDERR, '(A,I1,A)') PREFD//' Error: conversion of string "'//STR//'" to real failed! real(', KIND(KND), ')'
   ENDIF
   IF (PRESENT(ERROR)) ERROR = ERR
   ENDFUNCTION CTOR_R16P

   FUNCTION CTOR_R8P(STR, KND, PREF, ERROR) RESULT(N)
   !< Convert string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR8P, cton(str='-1.0', knd=1._R8P)
   !<```
   !=> -0.100000000000000E+001 <<<
   CHARACTER(*),           INTENT(IN)  :: STR   !< String containing input number.
   REAL(R8P),              INTENT(IN)  :: KND   !< Number kind.
   CHARACTER(*), OPTIONAL, INTENT(IN)  :: PREF  !< Prefixing string.
   INTEGER(I4P), OPTIONAL, INTENT(OUT) :: ERROR !< Error trapping flag: 0 no errors, >0 error occurs.
   REAL(R8P)                           :: N     !< Number returned.
   INTEGER(I4P)                        :: ERR   !< Error trapping flag: 0 no errors, >0 error occurs.
   CHARACTER(LEN=:), ALLOCATABLE       :: PREFD !< Prefixing string.

   READ(STR, *, IOSTAT=ERR) N ! Casting of str to n.
   IF (ERR/=0) THEN
     PREFD = '' ; IF (PRESENT(PREF)) PREFD = PREF
     WRITE(STDERR, '(A,I1,A)') PREFD//' Error: conversion of string "'//STR//'" to real failed! real(', KIND(KND), ')'
   ENDIF
   IF (PRESENT(ERROR)) ERROR = ERR
   ENDFUNCTION CTOR_R8P

   FUNCTION CTOR_R4P(STR, KND, PREF, ERROR) RESULT(N)
   !< Convert string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR4P, cton(str='-1.0', knd=1._R4P)
   !<```
   !=> -0.100000E+01 <<<
   CHARACTER(*),           INTENT(IN)  :: STR   !< String containing input number.
   REAL(R4P),              INTENT(IN)  :: KND   !< Number kind.
   CHARACTER(*), OPTIONAL, INTENT(IN)  :: PREF  !< Prefixing string.
   INTEGER(I4P), OPTIONAL, INTENT(OUT) :: ERROR !< Error trapping flag: 0 no errors, >0 error occurs.
   REAL(R4P)                           :: N     !< Number returned.
   INTEGER(I4P)                        :: ERR   !< Error trapping flag: 0 no errors, >0 error occurs.
   CHARACTER(LEN=:), ALLOCATABLE       :: PREFD !< Prefixing string.

   READ(STR, *, IOSTAT=ERR) N ! Casting of str to n.
   IF (ERR/=0) THEN
     PREFD = '' ; IF (PRESENT(PREF)) PREFD = PREF
     WRITE(STDERR, '(A,I1,A)') PREFD//' Error: conversion of string "'//STR//'" to real failed! real(', KIND(KND), ')'
   ENDIF
   IF (PRESENT(ERROR)) ERROR = ERR
   ENDFUNCTION CTOR_R4P

   FUNCTION CTOI_I8P(STR, KND, PREF, ERROR) RESULT(N)
   !< Convert string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI8P, cton(str='-1', knd=1_I8P)
   !<```
   !=> -1 <<<
   CHARACTER(*),           INTENT(IN)  :: STR   !< String containing input number.
   INTEGER(I8P),           INTENT(IN)  :: KND   !< Number kind.
   CHARACTER(*), OPTIONAL, INTENT(IN)  :: PREF  !< Prefixing string.
   INTEGER(I4P), OPTIONAL, INTENT(OUT) :: ERROR !< Error trapping flag: 0 no errors, >0 error occurs.
   INTEGER(I8P)                        :: N     !< Number returned.
   INTEGER(I4P)                        :: ERR   !< Error trapping flag: 0 no errors, >0 error occurs.
   CHARACTER(LEN=:), ALLOCATABLE       :: PREFD !< Prefixing string.

   READ(STR, *, IOSTAT=ERR) N ! Casting of str to n.
   IF (ERR/=0) THEN
     PREFD = '' ; IF (PRESENT(PREF)) PREFD = PREF
     WRITE(STDERR, '(A,I1,A)') PREFD//' Error: conversion of string "'//STR//'" to integer failed! integer(', KIND(KND), ')'
   ENDIF
   IF (PRESENT(ERROR)) ERROR = ERR
   ENDFUNCTION CTOI_I8P

   FUNCTION CTOI_I4P(STR, KND, PREF, ERROR) RESULT(N)
   !< Convert string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, cton(str='-1', knd=1_I4P)
   !<```
   !=> -1 <<<
   CHARACTER(*),           INTENT(IN)  :: STR   !< String containing input number.
   INTEGER(I4P),           INTENT(IN)  :: KND   !< Number kind.
   CHARACTER(*), OPTIONAL, INTENT(IN)  :: PREF  !< Prefixing string.
   INTEGER(I4P), OPTIONAL, INTENT(OUT) :: ERROR !< Error trapping flag: 0 no errors, >0 error occurs.
   INTEGER(I4P)                        :: N     !< Number returned.
   INTEGER(I4P)                        :: ERR   !< Error trapping flag: 0 no errors, >0 error occurs.
   CHARACTER(LEN=:), ALLOCATABLE       :: PREFD !< Prefixing string.

   READ(STR, *, IOSTAT=ERR) N ! Casting of str to n.
   IF (ERR/=0) THEN
     PREFD = '' ; IF (PRESENT(PREF)) PREFD = PREF
     WRITE(STDERR, '(A,I1,A)') PREFD//' Error: conversion of string "'//STR//'" to integer failed! integer(', KIND(KND), ')'
   ENDIF
   IF (PRESENT(ERROR)) ERROR = ERR
   ENDFUNCTION CTOI_I4P

   FUNCTION CTOI_I2P(STR, KND, PREF, ERROR) RESULT(N)
   !< Convert string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI2P, cton(str='-1', knd=1_I2P)
   !<```
   !=> -1 <<<
   CHARACTER(*),           INTENT(IN)  :: STR   !< String containing input number.
   INTEGER(I2P),           INTENT(IN)  :: KND   !< Number kind.
   CHARACTER(*), OPTIONAL, INTENT(IN)  :: PREF  !< Prefixing string.
   INTEGER(I4P), OPTIONAL, INTENT(OUT) :: ERROR !< Error trapping flag: 0 no errors, >0 error occurs.
   INTEGER(I2P)                        :: N     !< Number returned.
   INTEGER(I4P)                        :: ERR   !< Error trapping flag: 0 no errors, >0 error occurs.
   CHARACTER(LEN=:), ALLOCATABLE       :: PREFD !< Prefixing string.

   READ(STR, *, IOSTAT=ERR) N ! Casting of str to n.
   IF (ERR/=0) THEN
     PREFD = '' ; IF (PRESENT(PREF)) PREFD = PREF
     WRITE(STDERR, '(A,I1,A)') PREFD//' Error: conversion of string "'//STR//'" to integer failed! integer(', KIND(KND), ')'
   ENDIF
   IF (PRESENT(ERROR)) ERROR = ERR
   ENDFUNCTION CTOI_I2P

   FUNCTION CTOI_I1P(STR, KND, PREF, ERROR) RESULT(N)
   !< Convert string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, cton(str='-1', knd=1_I1P)
   !<```
   !=> -1 <<<
   CHARACTER(*),           INTENT(IN)  :: STR   !< String containing input number.
   INTEGER(I1P),           INTENT(IN)  :: KND   !< Number kind.
   CHARACTER(*), OPTIONAL, INTENT(IN)  :: PREF  !< Prefixing string.
   INTEGER(I4P), OPTIONAL, INTENT(OUT) :: ERROR !< Error trapping flag: 0 no errors, >0 error occurs.
   INTEGER(I1P)                        :: N     !< Number returned.
   INTEGER(I4P)                        :: ERR   !< Error trapping flag: 0 no errors, >0 error occurs.
   CHARACTER(LEN=:), ALLOCATABLE       :: PREFD !< Prefixing string.

   READ(STR, *, IOSTAT=ERR) N ! Casting of str to n.
   IF (ERR/=0) THEN
     PREFD = '' ; IF (PRESENT(PREF)) PREFD = PREF
     WRITE(STDERR, '(A,I1,A)') PREFD//' Error: conversion of string "'//STR//'" to integer failed! integer(', KIND(KND), ')'
   ENDIF
   IF (PRESENT(ERROR)) ERROR = ERR
   ENDFUNCTION CTOI_I1P

   ELEMENTAL FUNCTION BSTR_R16P(N) RESULT(BSTR)
   !< Convert real to string of bits.
   !<
   !< @note It is assumed that R16P is represented by means of 128 bits, but this is not ensured in all architECTURES.
   !<
   !<```fortran
   !< use penf
   !< character(128) :: b
   !< b = bstr(n=1._R16P)
   !< print "(A)", b(17:)
   !<```
   !=> 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111111100111111 <<<
   REAL(R16P), INTENT(IN) :: N          !< Real to be converted.
   CHARACTER(128)         :: BSTR       !< Returned bit-string containing input number.
   INTEGER(I1P)           :: BUFFER(16) !< Transfer buffer.

   BUFFER = TRANSFER(N, BUFFER)
   WRITE(BSTR, '(16B8.8)') BUFFER
   ENDFUNCTION BSTR_R16P

   ELEMENTAL FUNCTION BSTR_R8P(N) RESULT(BSTR)
   !< Convert real to string of bits.
   !<
   !< @note It is assumed that R8P is represented by means of 64 bits, but this is not ensured in all architecTURES.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1._R8P)
   !<```
   !=> 0000000000000000000000000000000000000000000000001111000000111111 <<<
   REAL(R8P), INTENT(IN) :: N         !< Real to be converted.
   CHARACTER(64)         :: BSTR      !< Returned bit-string containing input number.
   INTEGER(I1P)          :: BUFFER(8) !< Transfer buffer.

   BUFFER = TRANSFER(N, BUFFER)
   WRITE(BSTR, '(8B8.8)') BUFFER
   ENDFUNCTION BSTR_R8P

   ELEMENTAL FUNCTION BSTR_R4P(N) RESULT(BSTR)
   !< Convert real to string of bits.
   !<
   !< @note It is assumed that R4P is represented by means of 32 bits, but this is not ensured in all architecTURES.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1._R4P)
   !<```
   !=> 00000000000000001000000000111111 <<<
   REAL(R4P), INTENT(IN) :: N         !< Real to be converted.
   CHARACTER(32)         :: BSTR      !< Returned bit-string containing input number.
   INTEGER(I1P)          :: BUFFER(4) !< Transfer buffer.

   BUFFER = TRANSFER(N, BUFFER)
   WRITE(BSTR, '(4B8.8)') BUFFER
   ENDFUNCTION BSTR_R4P

   ELEMENTAL FUNCTION BSTR_I8P(N) RESULT(BSTR)
   !< Convert integer to string of bits.
   !<
   !< @note It is assumed that I8P is represented by means of 64 bits, but this is not ensured in all architecTURES.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1_I8P)
   !<```
   !=> 0000000000000000000000000000000000000000000000000000000000000001 <<<
   INTEGER(I8P), INTENT(IN) :: N    !< Real to be converted.
   CHARACTER(64)            :: BSTR !< Returned bit-string containing input number.

   WRITE(BSTR, '(B64.64)') N
   ENDFUNCTION BSTR_I8P

   ELEMENTAL FUNCTION BSTR_I4P(N) RESULT(BSTR)
   !< Convert integer to string of bits.
   !<
   !< @note It is assumed that I4P is represented by means of 32 bits, but this is not ensured in all architecTURES.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1_I4P)
   !<```
   !=> 00000000000000000000000000000001 <<<
   INTEGER(I4P), INTENT(IN) :: N    !< Real to be converted.
   CHARACTER(32)            :: BSTR !< Returned bit-string containing input number.

   WRITE(BSTR, '(B32.32)') N
   ENDFUNCTION BSTR_I4P

   ELEMENTAL FUNCTION BSTR_I2P(N) RESULT(BSTR)
   !< Convert integer to string of bits.
   !<
   !< @note It is assumed that I2P is represented by means of 16 bits, but this is not ensured in all architecTURES.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1_I2P)
   !<```
   !=> 0000000000000001 <<<
   INTEGER(I2P), INTENT(IN) :: N    !< Real to be converted.
   CHARACTER(16)            :: BSTR !< Returned bit-string containing input number.

   WRITE(BSTR, '(B16.16)') N
   ENDFUNCTION BSTR_I2P

   ELEMENTAL FUNCTION BSTR_I1P(N) RESULT(BSTR)
   !< Convert integer to string of bits.
   !<
   !< @note It is assumed that I1P is represented by means of 8 bits, but this is not ensured in all architectURES.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1_I1P)
   !<```
   !=> 00000001 <<<
   INTEGER(I1P), INTENT(IN) :: N    !< Real to be converted.
   CHARACTER(8)             :: BSTR !< Returned bit-string containing input number.

   WRITE(BSTR, '(B8.8)') N
   ENDFUNCTION BSTR_I1P

   ELEMENTAL FUNCTION BCTOR_R16P(BSTR, KND) RESULT(N)
   !< Convert bit-string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR16P, bcton('00000000000000000000000000000000000000000000000000000000000000000000000000000'//&
   !<                    '000000000000000000000000000000000001111111100111111', knd=1._R16P)
   !<```
   !=> 0.100000000000000000000000000000000E+0001 <<<
   CHARACTER(*), INTENT(IN) :: BSTR       !< String containing input number.
   REAL(R16P),   INTENT(IN) :: KND        !< Number kind.
   REAL(R16P)               :: N          !< Number returned.
   INTEGER(I1P)             :: BUFFER(16) !< Transfer buffer.

   READ(BSTR, '(16B8.8)') BUFFER
   N = TRANSFER(BUFFER, N)
   ENDFUNCTION BCTOR_R16P

   ELEMENTAL FUNCTION BCTOR_R8P(BSTR, KND) RESULT(N)
   !< Convert bit-string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR8P, bcton('0000000000000000000000000000000000000000000000001111000000111111', knd=1._R8P)
   !<```
   !=> 0.100000000000000E+001 <<<
   CHARACTER(*), INTENT(IN) :: BSTR      !< String containing input number.
   REAL(R8P),    INTENT(IN) :: KND       !< Number kind.
   REAL(R8P)                :: N         !< Number returned.
   INTEGER(I1P)             :: BUFFER(8) !< Transfer buffer.

   READ(BSTR, '(8B8.8)') BUFFER
   N = TRANSFER(BUFFER, N)
   ENDFUNCTION BCTOR_R8P

   ELEMENTAL FUNCTION BCTOR_R4P(BSTR, KND) RESULT(N)
   !< Convert bit-string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR4P, bcton('00000000000000001000000000111111', knd=1._R4P)
   !<```
   !=> 0.100000E+01 <<<
   CHARACTER(*), INTENT(IN) :: BSTR      !< String containing input number.
   REAL(R4P),    INTENT(IN) :: KND       !< Number kind.
   REAL(R4P)                :: N         !< Number returned.
   INTEGER(I1P)             :: BUFFER(4) !< Transfer buffer.

   READ(BSTR, '(4B8.8)') BUFFER
   N = TRANSFER(BUFFER, N)
   ENDFUNCTION BCTOR_R4P

   ELEMENTAL FUNCTION BCTOI_I8P(BSTR, KND) RESULT(N)
   !< Convert bit-string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI8P, bcton('0000000000000000000000000000000000000000000000000000000000000001', knd=1_I8P)
   !<```
   !=> 1 <<<
   CHARACTER(*), INTENT(IN) :: BSTR !< String containing input number.
   INTEGER(I8P), INTENT(IN) :: KND  !< Number kind.
   INTEGER(I8P)             :: N    !< Number returned.

   READ(BSTR,'(B'//TRIM(STR(BIT_SIZE(KND), .TRUE.))//'.'//TRIM(STR(BIT_SIZE(KND), .TRUE.))//')') N
   ENDFUNCTION BCTOI_I8P

   ELEMENTAL FUNCTION BCTOI_I4P(BSTR, KND) RESULT(N)
   !< Convert bit-string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, bcton('00000000000000000000000000000001', knd=1_I4P)
   !<```
   !=> 1 <<<
   CHARACTER(*), INTENT(IN) :: BSTR !< String containing input number.
   INTEGER(I4P), INTENT(IN) :: KND  !< Number kind.
   INTEGER(I4P)             :: N    !< Number returned.

   READ(BSTR,'(B'//TRIM(STR(BIT_SIZE(KND), .TRUE.))//'.'//TRIM(STR(BIT_SIZE(KND), .TRUE.))//')') N
   ENDFUNCTION BCTOI_I4P

   ELEMENTAL FUNCTION BCTOI_I2P(BSTR, KND) RESULT(N)
   !< Convert bit-string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI2P, bcton('0000000000000001', knd=1_I2P)
   !<```
   !=> 1 <<<
   CHARACTER(*), INTENT(IN) :: BSTR !< String containing input number.
   INTEGER(I2P), INTENT(IN) :: KND  !< Number kind.
   INTEGER(I2P)             :: N    !< Number returned.

   READ(BSTR,'(B'//TRIM(STR(BIT_SIZE(KND), .TRUE.))//'.'//TRIM(STR(BIT_SIZE(KND), .TRUE.))//')') N
   ENDFUNCTION BCTOI_I2P

   ELEMENTAL FUNCTION BCTOI_I1P(BSTR, KND) RESULT(N)
   !< Convert bit-string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, bcton('00000001', knd=1_I1P)
   !<```
   !=> 1 <<<
   CHARACTER(*), INTENT(IN) :: BSTR !< String containing input number.
   INTEGER(I1P), INTENT(IN) :: KND  !< Number kind.
   INTEGER(I1P)             :: N    !< Number returned.

   READ(BSTR,'(B'//TRIM(STR(BIT_SIZE(KND), .TRUE.))//'.'//TRIM(STR(BIT_SIZE(KND), .TRUE.))//')') N
   ENDFUNCTION BCTOI_I1P
ENDMODULE ModLib_PenfStringify
