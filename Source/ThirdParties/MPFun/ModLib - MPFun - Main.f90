!*****************************************************************************

!  MPFUN20: A thread-safe Fortran arbitrary precision computation package
!  Main module (module MPMODULE) -- references all other modules for user.

!  Revision date:  31 May 2021

!  AUTHOR:
!     David H. Bailey
!     Lawrence Berkeley National Lab (retired) and University of California, Davis
!     Email: dhbailey@lbl.gov

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

!  DESCRIPTION OF THIS MODULE (MPMODULE):
!    This module links all lower-level modules and is the connection between
!    user codes and the lower modules.  See documentation for details. It also
!    declares numerous internal names from lower-level modules as private.

MODULE ModLib_MPFun

USE ModLib_MPFUNA
USE ModLib_MPFUNB
USE ModLib_MPFUNC
USE ModLib_MPFUND
USE ModLib_MPFUNE
USE ModLib_MPFUNF
USE ModLib_MPFUNG
USE ModLib_MPFUNH

!   Private subroutine names in module MPFUNB:

PRIVATE &
  MPADD, MPCABS, MPCADD, MPCDIV, MPCEQ, MPCMUL, MPCNPWR, MPCONJG, MPCSQRT, &
  MPCSUB, MPCPR, MPDIV, MPDIVD, MPDIVD40, MPDMC, MPDMC40, MPEQ, MPINFR, MPMDC, &
  MPMUL, MPMULD, MPMULD40, MPNINT, MPNORM, MPNPWR, MPNRTR, MPOUTW, &
  MPRANDR, MPROUN, MPSQRT, MPSUB, MPMQC,  MPQMC, MPQMC90, MPFFTCR, MPFFTRC, &
  MPFFT1, MPFFT2, MPFFT3, MPINIFFT, MPLCONV, MPMULX

!   Private subroutine names in module MPFUNC:

PRIVATE &
  MPCTOMP, MPEFORMAT, MPFFORMAT, MPINP, MPOUT

!   Private subroutine names in module MPFUND:

PRIVATE &
  MPAGMR, MPANG, MPCAGM, MPCEXP,  MPCLOG, MPCPOWCC, MPCPOWCR, MPCPOWRC, &
  MPCSSHR, MPCSSNR, MPEGAMMAQ, MPEXP, MPINITRAN, MPLOG, MPLOG2Q, MPPIQ, MPPOWER

!   Private subroutine names in module MPFUNE:

PRIVATE &
  MPBERNER,  MPBESSELJR, MPERFR, MPERFCR, MPGAMMAR, MPINCGAMMAR, MPZETAR, &
  MPZETAEMR

END MODULE ModLib_MPFun

