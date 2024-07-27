!*****************************************************************************

!  MPFUN20-Fort: A thread-safe arbitrary precision computation package
!  Precision level declaration module (module MPFUNF)

!  Revision date:  31 May 2021

!  AUTHOR:
!    David H. Bailey
!    Lawrence Berkeley National Lab (retired) and University of California, Davis
!    Email: dhbailey@lbl.gov

!  COPYRIGHT AND DISCLAIMER:
!    All software in this package (c) 2021 David H. Bailey.
!    By downloading or using this software you agree to the copyright, disclaimer
!    and license agreement in the accompanying file DISCLAIMER.txt.

!  PURPOSE OF PACKAGE:
!    This package permits one to perform floating-point computations (real and
!    complex) to arbitrarily high numeric precision, by making only relatively
!    minor changes to existing Fortran-90 programs.  All basic arithmetic
!    operations and transcendental functions are supported, together with several
!    special functions.

!    In addition to fast execution times, one key feature of this package is a
!    100% THREAD-SAFE design, which means that user-level applications can be
!    easily converted for parallel execution, say using a threaded parallel
!    environment such as OpenMP.

!  DOCUMENTATION:
!    A detailed description of this package, and instructions for compiling
!    and testing this program on various specific systems are included in the
!    README file accompanying this package, and, in more detail, in the
!    following technical paper:

!    David H. Bailey, "MPFUN2020: A new thread-safe arbitrary precision package,"
!    available at http://www.davidhbailey.com/dhbpapers/mpfun2020.pdf.

!  DESCRIPTION OF THIS MODULE (MPFUNF):
!    This module defines the default standard precision level in digits (mpipl)
!    and default medium precision level in digits (mpiplm), and the equivalent
!    precision levels in words (mpwds and mpwdsm), which are calculated below as:
!       mpwds = int (mpipl / mpdpw + 2)
!       mpwdsm = int (mpiplm / mpdpw + 2)
!    (mpdpw is the approx. number of digits per word, set in module MPFUNA).
!    These precision levels are the maximum working precision levels for all
!    operations that use module MPFUNG and MPFUNH.

MODULE ModLib_MPFUNF
USE ModLib_MPFUNA
USE ModLib_MPFUNB
USE ModLib_MPFUNC
USE ModLib_MPFUND
USE ModLib_MPFUNE
IMPLICIT NONE
INTEGER, PUBLIC:: MPIPL, MPIPLM

!  *** Set the default standard and medium precision levels (in digits) here.

PARAMETER (MPIPL = 2500, MPIPLM = 250)

!----------------------------------------------------------------------------

!  *** Do not change the following code (in normal usage).

INTEGER, PUBLIC:: MPWDS, MPWDS6, MPWDSM, MPWDSM6
PARAMETER (MPWDS = INT (MPIPL / MPDPW + 2.D0), MPWDS6 = MPWDS + 6, &
  MPWDSM = INT (MPIPLM / MPDPW + 2.D0), MPWDSM6 = MPWDSM + 6)

END MODULE ModLib_MPFUNF
