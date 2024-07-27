!*****************************************************************************

FUNCTION MPMASK13 (B)

!  Revision date:  31 May 2021

!  AUTHOR:
!     David H. Bailey
!     Lawrence Berkeley National Lab (retired) and University of California, Davis
!     Email: dhbailey@lbl.gov

!  COPYRIGHT AND DISCLAIMER:
!    All software in this package (c) 2021 David H. Bailey.
!    By downloading or using this software you agree to the copyright, disclaimer
!    and license agreement in the accompanying file DISCLAIMER.txt.

!  PURPOSE OF THIS ROUTINE:
!    This convoluted-looking code tests whether the DP value B has more than 40
!    significant bits. It actually returns the absolute value of B, with lower 13
!    bits zeroed out. This function must be compiled separately, with lower
!    optimization, since compiling with -fast with gfortran, for instance, defeats
!    the test.

USE ModLib_MPFUNA
IMPLICIT NONE

REAL (MPRKND) B, B13X, MPMASK13, T1
PARAMETER (B13X = 2.D0**13)
T1 = B13X * ABS (B)
MPMASK13 = ABS (ABS (B) + T1) - ABS (T1)
RETURN
END

FUNCTION MPMASK23 (B)

!  PURPOSE OF THIS ROUTINE:
!    This convoluted-looking code tests whether the QP (IEEE quad) value B has more
!    than 90 significant bits. It actually returns the absolute value of B, with
!    lower 23 bits zeroed out. This function must be compiled separately, with lower
!    optimization, since compiling with -fast with gfortran, for instance, defeats
!    the test.

USE ModLib_MPFUNA
IMPLICIT NONE

REAL (MPRKND) B23X
PARAMETER (B23X = 2.D0**23)
REAL (MAX (MPRKND2, KIND (1.0))) B, MPMASK23, T1
T1 = B23X * ABS (B)
MPMASK23 = ABS (ABS (B) + T1) - ABS (T1)
RETURN
END
