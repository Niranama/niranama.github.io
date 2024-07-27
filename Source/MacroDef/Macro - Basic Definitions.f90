!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++            Basic Macro Definitions.f90                 +++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Fortran preprocessor must be enabled: -fpp.

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!--- alternative definitions for library configuration ---

!+++ definition for target compilations +++
#define ReleaseMode
!#define DebugMode

!#define App32Bits
#define App64Bits

!+++ definition for real data type used for floating point numbers +++
!#define AppSingle
#define AppDouble
!#define AppQuadruple

!+++ definition for integer data type used for indexing +++
#define Indx32Bits
!#define Indx64Bits

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!--- type definitions for (signed) INTEGER ---
#define tSInt8          INTEGER(KIND=kI1B)
#define tSInt16         INTEGER(KIND=kI2B)
#define tSInt32         INTEGER(KIND=kI4B)
#define tSInt64         INTEGER(KIND=kI8B)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!--- type definitions for (unsigned) INTEGER ---
#define tUInt8          tSInt8
#define tUInt16         tSInt16
#define tUInt32         tSInt32
#define tUInt64         tSInt64
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!--- type definitions for REAL ---
#define tSingle         REAL(KIND=kSP)
#define tDouble         REAL(KIND=kDP)
#define tQuad           REAL(KIND=kQP)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!--- type definitions for COMPLEX ---
#define tCmpxSingle     COMPLEX(KIND=kSP)
#define tCmpxDouble     COMPLEX(KIND=kDP)
#define tCmpxQuad       COMPLEX(KIND=kQP)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!--- type definitions for LOGICAL ---
!#define tBool           LOGICAL(KIND=1)
#define tLogical        LOGICAL
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!--- type definitions for CHARACTER ---
#define tChar           CHARACTER(LEN=1)
#define tCharStar       CHARACTER(LEN=*)
#define tCharAlloc      CHARACTER(LEN=:), ALLOCATABLE
#define tCharLen(N)     CHARACTER(LEN=N)
#define tCharParam      CHARACTER(LEN=*), PARAMETER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!--- type/kind definitions for indices ---
#ifdef Indx64Bits
#   define tIndex          tUInt64
#else
#   define tIndex          tUInt32
#endif
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!--- type/kind definitions for typical real and complex variables ---
#ifdef App64Bits
!.............................................................
#   ifdef AppQuadruple
#       define tReal           tQuad
#       define tCmpx        tCmpxQuad
#       define kRP             kQP
#   else
#       define tReal           tDouble
#       define tCmpx        tCmpxDouble
#       define kRP             kDP
#   endif
!.............................................................
#else
!.............................................................
#   ifdef AppDouble
#       define tReal           tDouble
#       define tCmpx        tCmpxDouble
#       define kRP             kDP
#   else
#       define tReal           tSingle
#       define tCmpx        tCmpxSingle
#       define kRP             kSP
#   endif
!.............................................................
#endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! type definitions for Fortran and C Interoperability
#define cInt8           INTEGER(KIND=C_INT8_T )
#define cInt16          INTEGER(KIND=C_INT16_T)
#define cInt32          INTEGER(KIND=C_INT32_T)
#define cInt64          INTEGER(KIND=C_INT64_T)
#define cSingle         REAL(KIND=C_FLOAT)
#define cDouble         REAL(KIND=C_DOUBLE)
#define cQuad           REAL(KIND=C_LONG_DOUBLE)
#define cCmpxSingle     COMPLEX(KIND=C_FLOAT_COMPLEX)
#define cCmpxDouble     COMPLEX(KIND=C_DOUBLE_COMPLEX)
#define cCmpxQuad       COMPLEX(KIND=C_LONG_DOUBLE_COMPLEX)
#define cChar           CHARACTER(LEN=1,KIND=C_CHAR)
#define cPointer        TYPE(C_PTR)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Macros for integer conversion routines. 
#define ToInt8(X)       INT(X, KIND=kI1B)
#define ToInt16(X)      INT(X, KIND=kI2B)
#define ToInt32(X)      INT(X, KIND=kI4B)
#define ToInt64(X)      INT(X, KIND=kI8B)
#define ToIndex(X)      INT(X, KIND=kIndex)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Macros for real conversion routines. 
#define ToSingle(X)     REAL(X, KIND=kSP)
#define ToDouble(X)     REAL(X, KIND=kDP)
#define ToQuad(X)       REAL(X, KIND=kQP)
#define ToReal(X)       REAL(X, KIND=kRP)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Important Note:
!   The following type and kind definitions and conversions
!   are typically used in modules and submodules that do not
!   necessarily need the differentiations between signed and
!   unsigned integers
!

!--- type definitions for (signed) INTEGER ---
#define tByte           tSInt8
#define tShort          tSInt16
#define tInteger        tSInt32
#define tLong           tSInt64

!--- intrinsic conversions for (signed) INTEGER ---
#define ToByte          ToInt8
#define ToShort         ToInt16
#define ToInteger       ToInt32
#define ToLong          ToInt64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
