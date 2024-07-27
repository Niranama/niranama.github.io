
MODULE ModBase_MathUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various routines relating to mathematic operations.

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE ModBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: IdentityMatrix
    PUBLIC :: TransposeMatrix
    PUBLIC :: RandomGen
    PUBLIC :: LOG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly
    
!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE LOG
        !^ **Function Interface**: LOG <br>
        ! **Purpose**:  To compute the logarithm of the input for a specified base.. <br>
        !  **Usage**: <br>
        !   --->    LogVal = LOG(InVal, Base) <br>
        MODULE PROCEDURE Logarithm
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

PURE FUNCTION IdentityMatrix(M) RESULT(IDMat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create identity matrix.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: M                !! number of row (and column)
    tReal               :: IDMat(M,M)       !! M by M array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I    ! index

!** FLOW:

    ! initialize all elements to zero
    IDMat = Zero
    
    ! set the diagonal elements to one
    FORALL (I=1:M) IDMat(I,I) = One
    
! alternative implementations
!    DO I = 1,M
!        IDMat(I,I) = One
!    END DO

    RETURN

END FUNCTION IdentityMatrix

!******************************************************************************

PURE FUNCTION TransposeMatrix(M,N,Matrix) RESULT(TransMat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To transpose matrix.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: M                !! number of row of input matrix
    tIndex, INTENT(IN)  :: N                !! number of column of input matrix
    tReal,  INTENT(IN)  :: Matrix(M,N)      !! M by N array, the input matrix
    tReal               :: TransMat(N,M)    !! N by M array, the transpose matrix

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex     :: I, J

!** FLOW:

    FORALL (I=1:N, J=1:M) TransMat(I,J) = Matrix(J,I)
    
! alternative implementations
!    DO I = 1, N
!        DO J = 1, M
!            TransMat(I,J) = Matrix(J,I)
!        END DO
!    END DO
!    DO CONCURRENT (I=1:N, J=1:M)
!        TransMat(I,J) = Matrix(J,I)
!    END DO

    RETURN

END FUNCTION TransposeMatrix

!******************************************************************************

ELEMENTAL FUNCTION Logarithm(Input, Base) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the logarithm of the input for a specified base.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tReal, INTENT(IN)   :: Input    !! input value
    tReal, INTENT(IN)   :: Base     !! base
    tReal               :: Output   !! logarithmic value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Input > Zero) THEN
        Output = LOG(Input)/LOG(Base)
    ELSE
        Output = IEEE_VALUE(Zero, IEEE_SIGNALING_NAN)
    END IF

    RETURN

END FUNCTION Logarithm

!******************************************************************************

FUNCTION RandomGen() RESULT(RNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate random number.

!** METHODOLOGY:
!   'Minimal' random number generator of Park and Miller combined with a Marsaglia shift
!   sequence. Returns a uniform random deviate between 0.0 and 1.0 (exclusive of the endpoint
!   values). This fully portable, scalar generator has the 'traditional' (not Fortran 90) calling
!   sequence with a random deviate as the returned function value: call with idum a negative
!   integer to initialize; thereafter, do not alter idum except to reinitialize. The period of this
!   generator is about 3.1**1018.

!** REFERENCE:
!   Press et.al. 1992.  Numerical Recipe.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tReal       :: RNum     !! generated random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: IA = 16807
    tInteger, PARAMETER :: IM = 2147483647
    tInteger, PARAMETER :: IQ = 127773
    tInteger, PARAMETER :: IR = 2836

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: int_time
    tInteger, SAVE  :: idum
    tInteger, SAVE  :: ix=-1,iy=-1,k
    tReal,    SAVE  :: am
    tLogical, SAVE  :: FirstTime = TrueVal

! FLOW

    IF (FirstTime) THEN
        FirstTime = FalseVal
        CALL SYSTEM_CLOCK(Count=int_time)
        idum      = -int_time
    END IF

    IF (idum <= 0 .OR. iy < 0) THEN                    ! Initialize.
        am   = NEAREST(1.0,-1.0)/IM
        iy   = IOR(IEOR(888889999,ABS(idum)),1)
        ix   = IEOR(777755555,ABS(idum))
        idum = ABS(idum)+1                            ! Set idum positive.
    END IF
    ix = IEOR(ix,ISHFT(ix,13))                        ! Marsaglia shift sequence with period 2**32 - 1.
    ix = IEOR(ix,ISHFT(ix,-17))
    ix = IEOR(ix,ISHFT(ix,5))
    k  = iy/IQ                                        ! Park-Miller sequence by Schrage's method,
                                                    ! period 2**31 - 2.
    iy = IA*(iy-k*IQ)-IR*k
    IF (iy < 0) THEN
        iy=iy+IM
    END IF
    RNum = am*IOR(IAND(IM,IEOR(ix,iy)),1)            ! Combine the two generators with masking to
                                                    ! ensure nonzero value.
    RETURN

END FUNCTION RandomGen

!******************************************************************************

END MODULE ModBase_MathUtil

!******************************************************************************
