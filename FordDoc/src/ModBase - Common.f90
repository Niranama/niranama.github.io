
MODULE ModBase_Common

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains parameters and derived types commonly used.

!** USE STATEMENTS:
    USE, INTRINSIC :: ISO_FORTRAN_ENV,  ONLY: INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
    USE, INTRINSIC :: ISO_C_BINDING,    ONLY: C_PTR, C_NULL_PTR

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all parameters and derived types which are placed in
                    ! this data-only module should be available to other modules and routines.

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! symbolic names for kind types of 8-, 4-, 2-, and 1-byte integers:
    INTEGER,  PARAMETER :: kI8B = INT64     !! kind for 64-bit or 8-byte integer
    INTEGER,  PARAMETER :: kI4B = INT32     !! kind for 32-bit or 4-byte integer
    INTEGER,  PARAMETER :: kI2B = INT16     !! kind for 16-bit or 2-byte integer
    INTEGER,  PARAMETER :: kI1B = INT8      !! kind for  8-bit or 1-byte integer
    ! symbolic names for alternative kind types of 8-, 16-, 32-, and 64-bit integers:
    INTEGER,  PARAMETER :: kInt8    = kI1B  !! alternative kind for  8-bit integer
    INTEGER,  PARAMETER :: kInt16   = kI2B  !! alternative kind for 16-bit integer
    INTEGER,  PARAMETER :: kInt32   = kI4B  !! alternative kind for 32-bit integer
    INTEGER,  PARAMETER :: kInt64   = kI8B  !! alternative kind for 64-bit integer
    INTEGER,  PARAMETER :: kByte    = kI1B  !! alternative kind for  8-bit integer
    INTEGER,  PARAMETER :: kShort   = kI2B  !! alternative kind for 16-bit integer
    INTEGER,  PARAMETER :: kInteger = kI4B  !! alternative kind for 32-bit integer
    INTEGER,  PARAMETER :: kLong    = kI8B  !! alternative kind for 64-bit integer
    ! symbolic names for kind types of quadruple-, double-, and single-precision reals:
    INTEGER,  PARAMETER :: kQP = REAL128    !! kind for 128-bit floating point (real) number
    INTEGER,  PARAMETER :: kDP = REAL64     !! kind for  64-bit floating point (real) number
    INTEGER,  PARAMETER :: kSP = REAL32     !! kind for  32-bit floating point (real) number
    INTEGER,  PARAMETER :: kQuad   = kQP    !! alternative kind for 128-bit real number
    INTEGER,  PARAMETER :: kDouble = kDP    !! alternative kind for  64-bit real number
    INTEGER,  PARAMETER :: kSingle = kSP    !! alternative kind for  32-bit real number
    ! true and false values
    LOGICAL,  PARAMETER :: TrueVal  = .TRUE.
    LOGICAL,  PARAMETER :: FalseVal = .FALSE.
    ! kinds of numeric precision
#ifdef Indx64Bits
    INTEGER,  PARAMETER :: kIndex = kInt64          !! kind of indices
#else
    INTEGER,  PARAMETER :: kIndex = kInt32          !! kind of indices
#endif
    INTEGER,  PARAMETER :: kIP    = kIndex          !! kind of indices
    INTEGER,  PARAMETER :: kFloat = kRP             !! kind of default reals
    INTEGER,  PARAMETER :: kFP    = kFloat          !! kind of default floating point numbers
    ! angle unit flag
    INTEGER,  PARAMETER :: Degree = 1
    INTEGER,  PARAMETER :: Radian = 2
    ! commonly used numeric constants (i.e. whole real number)
    tReal,    PARAMETER :: Zero         = 0.0_kFloat
    tReal,    PARAMETER :: One          = 1.0_kFloat
    tReal,    PARAMETER :: Two          = 2.0_kFloat
    tReal,    PARAMETER :: Three        = 3.0_kFloat
    tReal,    PARAMETER :: Four         = 4.0_kFloat
    tReal,    PARAMETER :: Five         = 5.0_kFloat
    tReal,    PARAMETER :: Six          = 6.0_kFloat
    tReal,    PARAMETER :: Seven        = 7.0_kFloat
    tReal,    PARAMETER :: Eight        = 8.0_kFloat
    tReal,    PARAMETER :: Nine         = 9.0_kFloat
    tReal,    PARAMETER :: Ten          = 10.0_kFloat
    tReal,    PARAMETER :: Hundred      = 100.0_kFloat
    tReal,    PARAMETER :: Thousand     = 1000.0_kFloat
    tReal,    PARAMETER :: Million      = 1000000.0_kFloat
    tReal,    PARAMETER :: Billion      = 1000000000.0_kFloat
    ! common fractions
    tReal,    PARAMETER :: Quater       = 0.25_kFloat
    tReal,    PARAMETER :: Half         = 0.5_kFloat
    tReal,    PARAMETER :: ThreeQuater  = 0.75_kFloat
    tReal,    PARAMETER :: OneThird     = One/Three
    tReal,    PARAMETER :: TwoThird     = Two/Three
    ! frequently used mathematical constants (with precision to spare):
    tReal,    PARAMETER :: Pi           = 3.141592653589793238462643383279502884197_kFloat
    tReal,    PARAMETER :: PiOvr2       = Half * Pi
    tReal,    PARAMETER :: Pi3Ovr2      = 1.5_kFloat * Pi
    tReal,    PARAMETER :: TwoPi        = Two * Pi
    ! defined tolerance value
    tReal,    PARAMETER :: Zero01       = 1.0E-1_kFloat
    tReal,    PARAMETER :: Zero02       = 1.0E-2_kFloat
    tReal,    PARAMETER :: Zero03       = 1.0E-3_kFloat
    tReal,    PARAMETER :: Zero04       = 1.0E-4_kFloat
    tReal,    PARAMETER :: Zero05       = 1.0E-5_kFloat
    tReal,    PARAMETER :: Zero06       = 1.0E-6_kFloat
    tReal,    PARAMETER :: Zero07       = 1.0E-7_kFloat
    tReal,    PARAMETER :: Zero08       = 1.0E-8_kFloat
    tReal,    PARAMETER :: Zero09       = 1.0E-9_kFloat
    tReal,    PARAMETER :: Zero10       = 1.0E-10_kFloat
    tReal,    PARAMETER :: Zero11       = 1.0E-11_kFloat
    tReal,    PARAMETER :: Zero12       = 1.0E-12_kFloat
    tReal,    PARAMETER :: Zero13       = 1.0E-13_kFloat
    tReal,    PARAMETER :: Zero14       = 1.0E-14_kFloat
    tReal,    PARAMETER :: Zero15       = 1.0E-15_kFloat
    tReal,    PARAMETER :: Zero16       = 1.0E-16_kFloat
    tReal,    PARAMETER :: Zero17       = 1.0E-17_kFloat
    ! machine dependent parameters
    tReal,    PARAMETER :: MachineEps   = EPSILON(One)      !! machine epsilon 
    tReal,    PARAMETER :: Small        = TINY(One)         !! the smallest positive number
    tReal,    PARAMETER :: Large        = HUGE(One)         !! the largest positive number
    tReal,    PARAMETER :: SqrtEps      = SQRT(MachineEps)  !! square root of MachineEps
    ! huge (maximum) numbers for intrinsic types used to prevent overflow
    tByte,    PARAMETER :: Huge_I1B = HUGE(1_kI1B)  !! = 127
    tShort,   PARAMETER :: Huge_I2B = HUGE(1_kI2B)  !! = 32767
    tInteger, PARAMETER :: Huge_I4B = HUGE(1_kI4B)  !! = 2147483647
    tLong,    PARAMETER :: Huge_I8B = HUGE(1_kI8B)  !! = 9223372036854775807
    tSingle,  PARAMETER :: Huge_RSP = HUGE(1.0_kSP) !! = 3.4028235E+38
    tDouble,  PARAMETER :: Huge_RDP = HUGE(1.0_kDP) !! = 1.797693134862316E+308
    tQuad,    PARAMETER :: Huge_RQP = HUGE(1.0_kQP) !! = 1.189731495357231765085759326628007E+4932
    ! tiny (positive minimum) numbers for floating point types used to prevent underflow (normal range)
    tSingle,  PARAMETER :: Tiny_RSP = TINY(1.0_kSP)      !! = 1.1754944E-38
    tDouble,  PARAMETER :: Tiny_RDP = TINY(1.0_kDP)      !! = 2.225073858507201E-308
    tQuad,    PARAMETER :: Tiny_RQP = TINY(1.0_kQP)      !! = 3.362103143112093506262677817321753E-4932
    ! machine epsilon numbers for floating point types used to check accuracy tolerance
    tSingle,  PARAMETER :: Eps_RSP = EPSILON(1.0_kSP)    !! = 1.1920929E-07
    tDouble,  PARAMETER :: Eps_RDP = EPSILON(1.0_kDP)    !! = 2.220446049250313E-016
    tQuad,    PARAMETER :: Eps_RQP = EPSILON(1.0_kQP)    !! = 1.925929944387235853055977942584927E-0034
    ! miscellaneous
    tLogical, PARAMETER :: IsLittleEndian = (1_kI2B == TRANSFER([1_kI1B, 0_kI1B], 0_kI2B))
    tCharParam          :: LibName = "XPFLIB"

!** DERIVED TYPE DEFINITIONS
    !> an equation type that can be used to parse strings/texts expressing
    !  mathematical expressions to a 'Function Parser' so that a system of
    !  equations can be evaluated at runtime.
    TYPE Equation
        !> number of equations
        tIndex                      :: NEQ
        !> number of variables
        tIndex                      :: NVR
        !> texts expressing equations        
        tCharLen(:), ALLOCATABLE    :: EQText(:)
        !> texts expressing variable names
        tCharLen(:), ALLOCATABLE    :: VarText(:)
        !> values of variables
        tReal,       ALLOCATABLE    :: Values(:)
    END TYPE Equation
    !> a user parameter type used to modernize legacy code
    TYPE UserParam
        !> number of real parameters
        tIndex                  :: NR
        !> number of integer parameters
        tIndex                  :: NI
        !> real parameters
        tReal,    ALLOCATABLE   :: RPar(:)
        !> integer parameters
        tInteger, ALLOCATABLE   :: IPar(:)
    END TYPE UserParam
    !> a workspace type used to modernize legacy code
    TYPE WorkSpace
        !> number of real workspace variables
        tIndex                  :: LRW
        !> number of integer workspace variables
        tIndex                  :: LIW
        !> real workspace variables
        tReal,    ALLOCATABLE   :: RVar(:)
        !> integer workspace variables
        tInteger, ALLOCATABLE   :: IVar(:)
    END TYPE WorkSpace
    !> a share parameter type used to modernize common block of legacy code
    TYPE SharePar(NR,NI)
        !> number of real parameters
        tIndex, LEN     :: NR
        !> number integer parameters
        tIndex, LEN     :: NI
        !> real parameters
        tReal           :: RPar(NR)
        !> integer parameters
        tIndex          :: IPar(NI)
    END TYPE SharePar
    !> a saved variable type used to modernize legacy code
    TYPE, EXTENDS(SharePar) :: SaveVar(NL)
        !> number of logical parameters
        tIndex, LEN     :: NL
        !> number of logical parameters
        tLogical        :: LPar(NL)
    END TYPE SaveVar
    !> container type that utilize 'c' pointer (C_PTR) type
    TYPE, BIND(C) :: Container
        !> storage
        cPointer        :: Store = C_NULL_PTR
    END TYPE Container

!** MODULE VARIABLE DECLARATIONS:
    tLogical    :: Is_FMLib_Initialized = FalseVal  !! global variable used with FMLib library
    tLogical    :: Is_MPFun_Initialized = FalseVal  !! global variable used with MPFun library

END MODULE ModBase_Common

!******************************************************************************
